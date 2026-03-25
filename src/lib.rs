#![allow(dead_code)]

use crate::pseudo_terminal::{GridSize, PseudoTerminal};
use crate::text::TextVertex;
use crate::vte::{AnsiColor, Intensity, NamedColor, Position, StandardColor, Vte, VteEvent};
use bytemuck::{Pod, Zeroable};
use compact_str::CompactString;
use crossbeam_channel::{Receiver, Sender};
mod text;
use image::GenericImageView;
use rayon::iter::{IntoParallelIterator as _, ParallelIterator as _};
use std::collections::VecDeque;
use std::io::{self, Read, Write};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::Instant;
use text::{FontAtlas, SDFTextRenderer, generate_atlas};
use vte::MouseMode;
use wgpu::{
    ColorTargetState, CompositeAlphaMode, CurrentSurfaceTexture, DeviceDescriptor, FragmentState,
    Instance, LoadOp, MultisampleState, Operations, PipelineCompilationOptions,
    PipelineLayoutDescriptor, PresentMode, PrimitiveState, RenderPassColorAttachment,
    RenderPassDescriptor, RenderPipeline, RenderPipelineDescriptor, RequestAdapterOptions,
    ShaderModuleDescriptor, ShaderSource, SurfaceConfiguration, TextureFormat, TextureSampleType,
    TextureUsages, TextureViewDescriptor, TextureViewDimension, VertexAttribute,
    VertexBufferLayout, VertexFormat, VertexState, VertexStepMode,
};
use winit::application::ApplicationHandler;
use winit::dpi::{LogicalSize, PhysicalPosition};
use winit::event::{ElementState, MouseScrollDelta, WindowEvent};
use winit::event_loop::{ActiveEventLoop, EventLoop, EventLoopProxy};
use winit::keyboard::{Key, ModifiersState};
use winit::window::{Window, WindowAttributes, WindowId};

mod bloom;
mod config;
mod convert;
#[cfg(target_os = "macos")]
mod macos_transparency;
mod pseudo_terminal;
mod shell;
mod vte;

use crate::bloom::BloomRenderer;
use crate::config::Config;

const HISTORY_ROWS: u32 = 1000;
const CELL_WIDTH: f32 = 9.0;
const CELL_HEIGHT: f32 = 18.0;
const FONT_SIZE: f32 = 15.0;
pub const FONT_DATA: &[u8] = include_bytes!("../assets/fonts/FiraCode.ttc");
static LINEAR_LUT: std::sync::OnceLock<[f32; 256]> = std::sync::OnceLock::new();

fn get_linear_lut() -> &'static [f32; 256] {
    LINEAR_LUT.get_or_init(|| {
        let mut lut = [0.0; 256];
        for (i, v) in lut.iter_mut().enumerate() {
            *v = i as f32 / 255.0;
        }
        lut
    })
}

const BG_SHADER: &str = r#"
struct VertexInput {
    @location(0) pos: vec2<f32>,
    @location(1) color: vec4<f32>,
};

struct VertexOutput {
    @builtin(position) pos: vec4<f32>,
    @location(0) color: vec4<f32>,
};

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.pos = vec4<f32>(in.pos, 0.0, 1.0);
    out.color = in.color;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return in.color;
}
"#;

const IMAGE_SHADER: &str = r#"
struct VertexInput {
    @location(0) pos: vec2<f32>,
    @location(1) uv: vec2<f32>,
};

struct VertexOutput {
    @builtin(position) pos: vec4<f32>,
    @location(0) uv: vec2<f32>,
};

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.pos = vec4<f32>(in.pos, 0.0, 1.0);
    out.uv = in.uv;
    return out;
}

@group(0) @binding(0) var image_tex: texture_2d<f32>;
@group(0) @binding(1) var image_sampler: sampler;

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return textureSample(image_tex, image_sampler, in.uv);
}
"#;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct CursorPos {
    x: u32,
    y: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct Rgba {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}

impl Rgba {
    const WHITE: Self = Self::rgb(255, 255, 255);
    const BLACK: Self = Self::rgb(0, 0, 0);

    const fn rgb(r: u8, g: u8, b: u8) -> Self {
        Self { r, g, b, a: 255 }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct CellStyle {
    fg: Rgba,
    bg: Option<Rgba>,
    bold: bool,
    italic: bool,
}

impl Default for CellStyle {
    fn default() -> Self {
        Self {
            fg: Rgba::WHITE,
            bg: None,
            bold: false,
            italic: false,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Cell {
    ch: char,
    style: CellStyle,
}

#[derive(Clone, Debug)]
struct AnimatedCursor {
    position: (f32, f32),
    target: (f32, f32),
    last_tick: Instant,
    trail: VecDeque<CursorTrailSample>,
}

#[derive(Clone, Copy, Debug)]
struct CursorTrailSample {
    position: (f32, f32),
    born_at: Instant,
}

impl AnimatedCursor {
    fn new(position: CursorPos) -> Self {
        let now = Instant::now();
        Self {
            position: (position.x as f32, position.y as f32),
            target: (position.x as f32, position.y as f32),
            last_tick: now,
            trail: VecDeque::new(),
        }
    }

    fn set_target(&mut self, target: CursorPos) {
        let tx = target.x as f32;
        let ty = target.y as f32;
        if (self.target.0 - tx).abs() > 0.001 || (self.target.1 - ty).abs() > 0.001 {
            self.target = (tx, ty);
        }
    }

    fn snap(&mut self) {
        self.position = self.target;
        self.trail.clear();
    }

    fn update(&mut self) -> bool {
        let now = Instant::now();
        let dt = now.saturating_duration_since(self.last_tick);
        self.last_tick = now;

        let mut changed = false;
        let smoothing = 1.0 - (-dt.as_secs_f32() * 21.0).exp();

        let dx = self.target.0 - self.position.0;
        let dy = self.target.1 - self.position.1;

        if dx.abs() > 0.001 || dy.abs() > 0.001 {
            self.trail.push_front(CursorTrailSample {
                position: self.position,
                born_at: now,
            });
            while self.trail.len() > 10 {
                self.trail.pop_back();
            }
            self.position.0 += dx * smoothing;
            self.position.1 += dy * smoothing;
            changed = true;
        } else {
            // Snap to target if very close
            self.position = self.target;
        }

        while let Some(sample) = self.trail.back() {
            if now.duration_since(sample.born_at).as_millis() > 160 {
                self.trail.pop_back();
                changed = true;
            } else {
                break;
            }
        }

        changed || !self.trail.is_empty()
    }
}

struct ImageSprite {
    texture: wgpu::Texture,
    bind_group: wgpu::BindGroup,
    pipeline: RenderPipeline,
    vertex_buffer: wgpu::Buffer,
    vertex_count: u32,
    width: u32,
    height: u32,
    x: f32,
    y: f32,
}

impl Default for Cell {
    fn default() -> Self {
        Self {
            ch: ' ',
            style: CellStyle::default(),
        }
    }
}

#[derive(Debug)]
struct TerminalModel {
    cols: u32,
    viewport_rows: u32,
    row_start: usize,
    cells: Vec<Cell>,
    cursor: CursorPos,
    style: CellStyle,
    saved_cursor: Option<CursorPos>,
    scroll_offset: i32,
    image_logged: bool,
    mouse_mode: MouseMode,
    mouse_protocol_sgr: bool,
    focus_reporting: bool,
    cursor_hidden: bool,
    dirty_rows: Vec<bool>,
    screen_top: u32,
    margin_top: Option<u32>,
    margin_bottom: Option<u32>,
    is_alt_buffer: bool,
}

impl TerminalModel {
    fn new(cols: u32, viewport_rows: u32) -> Self {
        let cols = cols.max(1);
        let viewport_rows = viewport_rows.clamp(1, HISTORY_ROWS);
        Self {
            cols,
            viewport_rows,
            row_start: 0,
            cells: vec![Cell::default(); (cols * HISTORY_ROWS) as usize],
            cursor: CursorPos { x: 0, y: 0 },
            style: CellStyle::default(),
            saved_cursor: None,
            scroll_offset: 0,
            image_logged: false,
            mouse_mode: MouseMode::None,
            mouse_protocol_sgr: false,
            focus_reporting: false,
            cursor_hidden: false,
            dirty_rows: vec![true; HISTORY_ROWS as usize],
            screen_top: 0,
            margin_top: None,
            margin_bottom: None,
            is_alt_buffer: false,
        }
    }

    fn resize(&mut self, cols: u32, viewport_rows: u32) {
        let new_cols = cols.max(1);
        let viewport_rows = viewport_rows.clamp(1, HISTORY_ROWS);
        if new_cols != self.cols {
            let old_cols = self.cols;
            let old_row_start = self.row_start;
            let old_cells = std::mem::take(&mut self.cells);
            self.cells = vec![Cell::default(); (new_cols * HISTORY_ROWS) as usize];

            for y in 0..HISTORY_ROWS {
                for x in 0..old_cols.min(new_cols) {
                    let old_physical_row = (old_row_start + y as usize) % HISTORY_ROWS as usize;
                    let old_idx = old_physical_row * old_cols as usize + x as usize;
                    let new_physical_row = (self.row_start + y as usize) % HISTORY_ROWS as usize;
                    let new_idx = new_physical_row * new_cols as usize + x as usize;
                    self.cells[new_idx] = old_cells[old_idx];
                }
            }
            self.cols = new_cols;
            self.cursor.x = self.cursor.x.min(self.cols.saturating_sub(1));
        }

        self.viewport_rows = viewport_rows;
        self.screen_top = self
            .screen_top
            .min(HISTORY_ROWS.saturating_sub(viewport_rows));
        self.scroll_offset = self.scroll_offset.clamp(0, self.screen_top as i32);
    }

    fn index(&self, y: u32, x: u32) -> usize {
        let row = (self.row_start + y as usize) % HISTORY_ROWS as usize;
        row * self.cols as usize + x as usize
    }

    fn get(&self, y: u32, x: u32) -> Cell {
        self.cells[self.index(y, x)]
    }

    fn set(&mut self, y: u32, x: u32, cell: Cell) {
        let idx = self.index(y, x);
        if self.cells[idx].ch != cell.ch || self.cells[idx].style != cell.style {
            self.cells[idx] = cell;
            let physical_row = (self.row_start + y as usize) % HISTORY_ROWS as usize;
            self.dirty_rows[physical_row] = true;
        }
    }

    fn clear_all(&mut self) {
        let style = self.clear_style();
        self.cells.fill(Cell { ch: ' ', style });
        self.dirty_rows.fill(true);
    }

    fn clear_style(&self) -> CellStyle {
        CellStyle {
            fg: Rgba::WHITE,
            bg: self.style.bg,
            bold: false,
            italic: false,
        }
    }

    fn clear_cell(&mut self, y: u32, x: u32) {
        let style = self.clear_style();
        self.set(y, x, Cell { ch: ' ', style });
    }

    fn clear_row(&mut self, y: u32) {
        let style = self.clear_style();
        for x in 0..self.cols {
            self.set(y, x, Cell { ch: ' ', style });
        }
        let physical_row = (self.row_start + y as usize) % HISTORY_ROWS as usize;
        self.dirty_rows[physical_row] = true;
    }

    fn scroll_up_one(&mut self) {
        tracing::debug!(row_start = self.row_start, "global_scroll_up_one");
        self.row_start = (self.row_start + 1) % HISTORY_ROWS as usize;
        self.clear_row(HISTORY_ROWS - 1);
        self.cursor.y = HISTORY_ROWS - 1;
    }

    fn ensure_cursor_visible(&mut self) {
        while self.cursor.y >= HISTORY_ROWS {
            self.scroll_up_one();
        }

        if self.cursor.y < self.screen_top {
            self.screen_top = self.cursor.y;
        } else if self.cursor.y >= self.screen_top + self.viewport_rows {
            if self.is_alt_buffer {
                // In alt buffer, we DON'T grow screen_top.
                // We should have scrolled instead.
                self.cursor.y = self.screen_top + self.viewport_rows - 1;
            } else {
                self.screen_top = self.cursor.y - (self.viewport_rows - 1);
            }
        }
    }

    fn screen_top(&self) -> u32 {
        self.screen_top
    }

    fn current_screen_top(&self) -> u32 {
        self.screen_top()
    }

    fn current_view_top(&self) -> u32 {
        let screen_top = self.screen_top as i32;
        (screen_top - self.scroll_offset).max(0) as u32
    }

    fn apply_vte_event(
        &mut self,
        event: VteEvent,
        writer: &Sender<Vec<u8>>,
        on_image: &mut dyn FnMut(CompactString),
    ) {
        let current_screen_top = self.current_screen_top();
        tracing::debug!(?event, "vte_event");
        match event {
            VteEvent::Echo(character) => {
                if character == '\t' {
                    let next_tab = (self.cursor.x / 8 + 1) * 8;
                    self.move_right(next_tab.saturating_sub(self.cursor.x));
                    self.ensure_cursor_visible();
                    return;
                }

                self.ensure_cursor_visible();
                self.set(
                    self.cursor.y,
                    self.cursor.x,
                    Cell {
                        ch: character,
                        style: self.style,
                    },
                );
                self.move_right(1);
            }
            VteEvent::Backspace => {
                self.cursor.x = self.cursor.x.saturating_sub(1);
                self.clear_cell(self.cursor.y, self.cursor.x);
            }
            VteEvent::Goto(Position { x, y }) => {
                let x = x.saturating_sub(1).min(self.cols.saturating_sub(1));
                let y = current_screen_top + y.saturating_sub(1);
                self.cursor = CursorPos { x, y };
                self.ensure_cursor_visible();
            }
            VteEvent::GotoX(x) => {
                self.cursor.x = x.min(self.cols.saturating_sub(1));
            }
            VteEvent::GotoY(y) => {
                self.cursor.y = current_screen_top + y.saturating_sub(1);
                self.ensure_cursor_visible();
            }
            VteEvent::SaveCursorPosition => {
                self.saved_cursor = Some(self.cursor);
            }
            VteEvent::RestoreCursorPosition => {
                if let Some(cursor) = self.saved_cursor {
                    self.cursor = cursor;
                    self.ensure_cursor_visible();
                }
            }
            VteEvent::LineUp(rows) => {
                self.move_up(rows);
                self.cursor.x = 0;
            }
            VteEvent::LineDown(rows) => {
                self.move_down(rows);
                self.cursor.x = 0;
            }
            VteEvent::MoveUp(rows) => self.move_up(rows),
            VteEvent::MoveDown(rows) => self.move_down(rows),
            VteEvent::MoveLeft(cols) => {
                self.cursor.x = self.cursor.x.saturating_sub(cols);
            }
            VteEvent::MoveRight(cols) => self.move_right(cols),
            VteEvent::ReportCursorPosition => {
                let row_in_view = self.cursor.y as i32 - current_screen_top as i32 + 1;
                let col_in_view = self.cursor.x + 1;
                let response = format!("\x1b[{row_in_view};{col_in_view}R");
                let _ = writer.send(response.into_bytes());
            }
            VteEvent::ReportDeviceAttributes => {
                let _ = writer.send(b"\x1b[?1;2c".to_vec());
            }
            VteEvent::ReportVersion => {
                let _ = writer.send(b"\x1b[>1;256;0c".to_vec());
            }
            VteEvent::Reset => self.style = CellStyle::default(),
            VteEvent::Bold => self.style.bold = true,
            VteEvent::Italic => self.style.italic = true,
            VteEvent::Foreground(color) => self.style.fg = ansi_to_rgb(color),
            VteEvent::ResetForeground => self.style.fg = CellStyle::default().fg,
            VteEvent::Background(color) => self.style.bg = Some(ansi_to_rgb(color)),
            VteEvent::ResetBackground => self.style.bg = None,
            VteEvent::ClearLeft => {
                let y = self.cursor.y;
                for x in 0..self.cursor.x {
                    self.clear_cell(y, x);
                }
            }
            VteEvent::ClearRight => {
                let y = self.cursor.y;
                for x in self.cursor.x..self.cols {
                    self.clear_cell(y, x);
                }
            }
            VteEvent::ClearLine => {
                self.clear_row(self.cursor.y);
            }
            VteEvent::ClearUp => {
                let st = self.current_screen_top();
                for y in st..self.cursor.y {
                    self.clear_row(y);
                }
                for x in 0..self.cursor.x {
                    self.clear_cell(self.cursor.y, x);
                }
            }
            VteEvent::ClearDown => {
                let st = self.current_screen_top();
                let bottom = (st + self.viewport_rows).min(HISTORY_ROWS);
                for x in self.cursor.x..self.cols {
                    self.clear_cell(self.cursor.y, x);
                }
                for y in (self.cursor.y + 1)..bottom {
                    self.clear_row(y);
                }
            }
            VteEvent::ClearAll | VteEvent::ClearEverything => {
                self.clear_all();
                self.cursor = CursorPos {
                    x: 0,
                    y: current_screen_top,
                };
            }
            VteEvent::Image(image) => {
                on_image(image);
            }
            VteEvent::EnableMouseMode(mode) => match mode {
                MouseMode::Sgr => self.mouse_protocol_sgr = true,
                _ => self.mouse_mode = mode,
            },
            VteEvent::DisableMouseMode(mode) => match mode {
                MouseMode::Sgr => self.mouse_protocol_sgr = false,
                _ => {
                    if self.mouse_mode == mode {
                        self.mouse_mode = MouseMode::None;
                    }
                }
            },
            VteEvent::EnableAlternativeBuffer => {
                self.is_alt_buffer = true;
                self.clear_all();
                self.cursor_hidden = false;
                self.screen_top = 0;
                self.cursor = CursorPos { x: 0, y: 0 };
            }
            VteEvent::DisableAlternativeBuffer => {
                self.is_alt_buffer = false;
                self.clear_all();
                self.cursor_hidden = false;
                self.screen_top = 0;
                self.cursor = CursorPos { x: 0, y: 0 };
            }
            VteEvent::EnableFocusReporting => self.focus_reporting = true,
            VteEvent::DisableFocusReporting => self.focus_reporting = false,
            VteEvent::ShowCursor => self.cursor_hidden = false,
            VteEvent::HideCursor => self.cursor_hidden = true,
            VteEvent::InsertLine(n) => self.insert_line(n),
            VteEvent::DeleteLine(n) => self.delete_line(n),
            VteEvent::InsertCharacter(n) => self.insert_character(n),
            VteEvent::DeleteCharacter(n) => self.delete_character(n),
            VteEvent::EraseCharacter(n) => self.erase_character(n),
            VteEvent::FullReset => {
                self.clear_all();
                self.style = CellStyle::default();
                self.cursor_hidden = false;
                self.cursor = CursorPos { x: 0, y: 0 };
                self.screen_top = 0;
            }
            VteEvent::SetMargin { top, bottom } => {
                let st = self.current_screen_top();
                let top_val = top.unwrap_or(0);
                let bottom_val = bottom.unwrap_or(0);

                let t = if top_val == 0 { 1 } else { top_val }.saturating_sub(1);
                let b = if bottom_val == 0 {
                    self.viewport_rows
                } else {
                    bottom_val
                }
                .saturating_sub(1);
                let b = b.min(self.viewport_rows.saturating_sub(1));

                if t < b {
                    self.margin_top = Some(t);
                    self.margin_bottom = Some(b);
                } else {
                    self.margin_top = None;
                    self.margin_bottom = None;
                }
                self.cursor.x = 0;
                self.cursor.y = st + self.margin_top.unwrap_or(0);
            }
            VteEvent::Index => self.move_down(1),
            VteEvent::ReverseIndex => {
                let screen_top = self.current_screen_top();
                let cy_rel = self.cursor.y.saturating_sub(screen_top);
                let mt = self.margin_top.unwrap_or(0);
                if cy_rel == mt {
                    let mb = self.margin_bottom.unwrap_or(self.viewport_rows - 1);
                    self.scroll_region_down(1, mt as usize, mb as usize);
                } else {
                    self.cursor.y = self.cursor.y.saturating_sub(1);
                }
            }
            VteEvent::NextLine => {
                self.cursor.x = 0;
                self.move_down(1);
            }
            _ => {}
        }

        self.ensure_cursor_visible();
        self.scroll_offset = self.scroll_offset.clamp(0, self.screen_top as i32);
    }

    fn move_up(&mut self, rows: u32) {
        self.cursor.y = self.cursor.y.saturating_sub(rows.max(1));
    }

    fn move_down(&mut self, rows: u32) {
        let rows = rows.max(1);
        let screen_top = self.current_screen_top();
        let cy_rel = self.cursor.y.saturating_sub(screen_top);

        let mt = self.margin_top.unwrap_or(0);
        let mb = self.margin_bottom.unwrap_or(self.viewport_rows - 1);

        if cy_rel == mb {
            // Scroll region up by rows instead of pushing cursor down
            self.scroll_region_up(rows as usize, mt as usize, mb as usize);
            return;
        } else if cy_rel >= mt && cy_rel < mb && cy_rel + rows >= mb {
            let to_move = mb - cy_rel;
            let to_scroll = rows - to_move;
            self.cursor.y += to_move;
            if to_scroll > 0 {
                self.scroll_region_up(to_scroll as usize, mt as usize, mb as usize);
            }
            return;
        }

        self.cursor.y = self.cursor.y.saturating_add(rows);
        self.ensure_cursor_visible();
    }

    fn scroll_region_up(&mut self, n: usize, mt: usize, mb: usize) {
        tracing::debug!(n, mt, mb, "scroll_region_up");
        let screen_top = self.current_screen_top() as usize;
        let start_y = screen_top + mt;
        let end_y = screen_top + mb + 1;
        for y in start_y..end_y {
            let src_y = y + n;
            if src_y < end_y {
                for x in 0..self.cols {
                    let cell = self.get(src_y as u32, x);
                    self.set(y as u32, x, cell);
                }
            } else {
                self.clear_row(y as u32);
            }
            // Force dirty even if cell content didn't change —
            // the row has shifted position and its cached geometry is stale
            let physical = (self.row_start + y) % HISTORY_ROWS as usize;
            self.dirty_rows[physical] = true;
        }
    }

    fn scroll_region_down(&mut self, n: usize, mt: usize, mb: usize) {
        let screen_top = self.current_screen_top() as usize;
        let start_y = screen_top + mt;
        let end_y = screen_top + mb + 1;

        for y in (start_y..end_y).rev() {
            if y >= start_y + n {
                let src_y = y - n;
                for x in 0..self.cols {
                    let cell = self.get(src_y as u32, x);
                    self.set(y as u32, x, cell);
                }
            } else {
                self.clear_row(y as u32);
            }
            let physical = (self.row_start + y) % HISTORY_ROWS as usize;
            self.dirty_rows[physical] = true;
        }
    }

    fn move_right(&mut self, cols: u32) {
        self.cursor.x = self.cursor.x.saturating_add(cols.max(1));
        if self.cursor.x >= self.cols {
            self.cursor.y = self.cursor.y.saturating_add(self.cursor.x / self.cols);
            self.cursor.x %= self.cols;
            self.ensure_cursor_visible();
        }
    }

    fn insert_line(&mut self, n: u32) {
        let n = n.max(1) as usize;
        let screen_top = self.current_screen_top() as usize;
        let current_y = self.cursor.y as usize;

        let mt = self.margin_top.unwrap_or(0) as usize;
        let mb = self.margin_bottom.unwrap_or(self.viewport_rows - 1) as usize;

        let start_y = screen_top + mt;
        let limit_y = screen_top + mb + 1;

        if current_y < start_y || current_y >= limit_y {
            return;
        }

        for y in (current_y..limit_y).rev() {
            let target_y = y + n;
            if target_y < limit_y {
                for x in 0..self.cols {
                    let cell = self.get(y as u32, x);
                    self.set(target_y as u32, x, cell);
                }
                let phys = (self.row_start + target_y) % HISTORY_ROWS as usize;
                self.dirty_rows[phys] = true;
            }
            if y < current_y + n {
                self.clear_row(y as u32);
            }
            let phys = (self.row_start + y) % HISTORY_ROWS as usize;
            self.dirty_rows[phys] = true;
        }
    }

    fn delete_line(&mut self, n: u32) {
        let n = n.max(1) as usize;
        let screen_top = self.current_screen_top() as usize;
        let current_y = self.cursor.y as usize;

        let mt = self.margin_top.unwrap_or(0) as usize;
        let mb = self.margin_bottom.unwrap_or(self.viewport_rows - 1) as usize;

        let start_y = screen_top + mt;
        let limit_y = screen_top + mb + 1;

        if current_y < start_y || current_y >= limit_y {
            return;
        }

        for y in current_y..limit_y {
            let src_y = y + n;
            if src_y < limit_y {
                for x in 0..self.cols {
                    let cell = self.get(src_y as u32, x);
                    self.set(y as u32, x, cell);
                }
            } else {
                self.clear_row(y as u32);
            }
            let phys = (self.row_start + y) % HISTORY_ROWS as usize;
            self.dirty_rows[phys] = true;
        }
    }

    fn insert_character(&mut self, n: u32) {
        let n = n.max(1) as usize;
        let y = self.cursor.y;
        let x = self.cursor.x as usize;
        for col in (x..self.cols as usize).rev() {
            let target_col = col + n;
            if target_col < self.cols as usize {
                let cell = self.get(y, col as u32);
                self.set(y, target_col as u32, cell);
            }
        }
        for col in x..(x + n).min(self.cols as usize) {
            self.clear_cell(y, col as u32);
        }
    }

    fn delete_character(&mut self, n: u32) {
        let n = n.max(1) as usize;
        let y = self.cursor.y;
        let x = self.cursor.x as usize;
        for col in x..self.cols as usize {
            let src_col = col + n;
            if src_col < self.cols as usize {
                let cell = self.get(y, src_col as u32);
                self.set(y, col as u32, cell);
            } else {
                self.clear_cell(y, col as u32);
            }
        }
    }

    fn erase_character(&mut self, n: u32) {
        let n = n.max(1);
        for x in self.cursor.x..(self.cursor.x + n).min(self.cols) {
            self.clear_cell(self.cursor.y, x);
        }
    }
}

#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable)]
struct BgVertex {
    pos: [f32; 2],
    color: [f32; 4],
}

struct BackgroundRenderer {
    pipeline: RenderPipeline,
    vertex_buffer: wgpu::Buffer,
    vertex_capacity: usize,
    vertex_count: u32,
}

#[repr(C)]
#[derive(Clone, Copy, Pod, Zeroable)]
struct ImageVertex {
    pos: [f32; 2],
    uv: [f32; 2],
}

impl BackgroundRenderer {
    fn new(device: &wgpu::Device, format: TextureFormat) -> Self {
        let shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("terminal-bg-shader"),
            source: ShaderSource::Wgsl(BG_SHADER.into()),
        });

        let pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("terminal-bg-layout"),
            bind_group_layouts: &[],
            immediate_size: 0,
        });

        let pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("terminal-bg-pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                compilation_options: PipelineCompilationOptions::default(),
                buffers: &[VertexBufferLayout {
                    array_stride: std::mem::size_of::<BgVertex>() as u64,
                    step_mode: VertexStepMode::Vertex,
                    attributes: &[
                        VertexAttribute {
                            format: VertexFormat::Float32x2,
                            offset: 0,
                            shader_location: 0,
                        },
                        VertexAttribute {
                            format: VertexFormat::Float32x4,
                            offset: 8,
                            shader_location: 1,
                        },
                    ],
                }],
            },
            primitive: PrimitiveState::default(),
            depth_stencil: None,
            multisample: MultisampleState::default(),
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                compilation_options: PipelineCompilationOptions::default(),
                targets: &[Some(ColorTargetState {
                    format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            multiview_mask: None,
            cache: None,
        });

        let vertex_capacity = 1024;
        let vertex_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("terminal-bg-vertices"),
            size: (vertex_capacity * std::mem::size_of::<BgVertex>()) as u64,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        Self {
            pipeline,
            vertex_buffer,
            vertex_capacity,
            vertex_count: 0,
        }
    }

    fn prepare(&mut self, device: &wgpu::Device, queue: &wgpu::Queue, vertices: &[BgVertex]) {
        self.vertex_count = vertices.len() as u32;
        if vertices.is_empty() {
            return;
        }

        if vertices.len() > self.vertex_capacity {
            self.vertex_capacity = vertices.len().next_power_of_two();
            self.vertex_buffer = device.create_buffer(&wgpu::BufferDescriptor {
                label: Some("terminal-bg-vertices"),
                size: (self.vertex_capacity * std::mem::size_of::<BgVertex>()) as u64,
                usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
                mapped_at_creation: false,
            });
        }

        queue.write_buffer(&self.vertex_buffer, 0, bytemuck::cast_slice(vertices));
    }

    fn render<'a>(&'a self, pass: &mut wgpu::RenderPass<'a>) {
        if self.vertex_count == 0 {
            return;
        }
        pass.set_pipeline(&self.pipeline);
        pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
        pass.draw(0..self.vertex_count, 0..1);
    }
}

impl ImageSprite {
    fn new(
        device: &wgpu::Device,
        queue: &wgpu::Queue,
        format: TextureFormat,
        rgba: &[u8],
        width: u32,
        height: u32,
        x: f32,
        y: f32,
        surface_width: f32,
        surface_height: f32,
    ) -> io::Result<Self> {
        let texture = device.create_texture(&wgpu::TextureDescriptor {
            label: Some("terminal-image-texture"),
            size: wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
            mip_level_count: 1,
            sample_count: 1,
            dimension: wgpu::TextureDimension::D2,
            format: TextureFormat::Rgba8UnormSrgb,
            usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
            view_formats: &[],
        });

        queue.write_texture(
            texture.as_image_copy(),
            rgba,
            wgpu::TexelCopyBufferLayout {
                offset: 0,
                bytes_per_row: Some(4 * width),
                rows_per_image: Some(height),
            },
            wgpu::Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
        );

        let sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: Some("terminal-image-sampler"),
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            mipmap_filter: wgpu::MipmapFilterMode::Nearest,
            ..Default::default()
        });

        let bind_group_layout = device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
            label: Some("terminal-image-bind-layout"),
            entries: &[
                wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Texture {
                        sample_type: TextureSampleType::Float { filterable: true },
                        view_dimension: TextureViewDimension::D2,
                        multisampled: false,
                    },
                    count: None,
                },
                wgpu::BindGroupLayoutEntry {
                    binding: 1,
                    visibility: wgpu::ShaderStages::FRAGMENT,
                    ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                    count: None,
                },
            ],
        });

        let pipeline_layout = device.create_pipeline_layout(&PipelineLayoutDescriptor {
            label: Some("terminal-image-pipeline-layout"),
            bind_group_layouts: &[Some(&bind_group_layout)],
            immediate_size: 0,
        });

        let shader = device.create_shader_module(ShaderModuleDescriptor {
            label: Some("terminal-image-shader"),
            source: ShaderSource::Wgsl(IMAGE_SHADER.into()),
        });

        let pipeline = device.create_render_pipeline(&RenderPipelineDescriptor {
            label: Some("terminal-image-pipeline"),
            layout: Some(&pipeline_layout),
            vertex: VertexState {
                module: &shader,
                entry_point: Some("vs_main"),
                compilation_options: PipelineCompilationOptions::default(),
                buffers: &[VertexBufferLayout {
                    array_stride: std::mem::size_of::<ImageVertex>() as u64,
                    step_mode: VertexStepMode::Vertex,
                    attributes: &[
                        VertexAttribute {
                            format: VertexFormat::Float32x2,
                            offset: 0,
                            shader_location: 0,
                        },
                        VertexAttribute {
                            format: VertexFormat::Float32x2,
                            offset: 8,
                            shader_location: 1,
                        },
                    ],
                }],
            },
            primitive: PrimitiveState::default(),
            depth_stencil: None,
            multisample: MultisampleState::default(),
            fragment: Some(FragmentState {
                module: &shader,
                entry_point: Some("fs_main"),
                compilation_options: PipelineCompilationOptions::default(),
                targets: &[Some(ColorTargetState {
                    format,
                    blend: Some(wgpu::BlendState::ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
            }),
            multiview_mask: None,
            cache: None,
        });

        let view = texture.create_view(&TextureViewDescriptor::default());
        let bind_group = device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("terminal-image-bind-group"),
            layout: &bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(&sampler),
                },
            ],
        });

        let vertex_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("terminal-image-vertex-buffer"),
            size: (6 * std::mem::size_of::<ImageVertex>()) as u64,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let mut sprite = Self {
            texture,
            bind_group,
            pipeline,
            vertex_buffer,
            vertex_count: 6,
            width,
            height,
            x,
            y,
        };
        sprite.update_vertices(queue, surface_width, surface_height);
        Ok(sprite)
    }

    fn update_vertices(&mut self, queue: &wgpu::Queue, surface_width: f32, surface_height: f32) {
        let x0 = (self.x / surface_width) * 2.0 - 1.0;
        let x1 = ((self.x + self.width as f32) / surface_width) * 2.0 - 1.0;
        let y0 = 1.0 - (self.y / surface_height) * 2.0;
        let y1 = 1.0 - ((self.y + self.height as f32) / surface_height) * 2.0;

        let vertices = [
            ImageVertex {
                pos: [x0, y0],
                uv: [0.0, 0.0],
            },
            ImageVertex {
                pos: [x1, y0],
                uv: [1.0, 0.0],
            },
            ImageVertex {
                pos: [x1, y1],
                uv: [1.0, 1.0],
            },
            ImageVertex {
                pos: [x0, y0],
                uv: [0.0, 0.0],
            },
            ImageVertex {
                pos: [x1, y1],
                uv: [1.0, 1.0],
            },
            ImageVertex {
                pos: [x0, y1],
                uv: [0.0, 1.0],
            },
        ];

        queue.write_buffer(&self.vertex_buffer, 0, bytemuck::cast_slice(&vertices));
    }

    fn render<'a>(&'a self, pass: &mut wgpu::RenderPass<'a>) {
        pass.set_pipeline(&self.pipeline);
        pass.set_bind_group(0, &self.bind_group, &[]);
        pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));
        pass.draw(0..self.vertex_count, 0..1);
    }
}

struct WindowState {
    window: Arc<dyn Window>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    surface: wgpu::Surface<'static>,
    surface_config: SurfaceConfiguration,
    font_atlas: FontAtlas,
    text_renderer: SDFTextRenderer,
    bg_renderer: BackgroundRenderer,
    cursor_renderer: BackgroundRenderer,
    scale_factor: f64,
    images: Vec<ImageSprite>,
    cursor: AnimatedCursor,
    content_dirty: bool,
    cached_cursor_x_px: f32,
    row_cache: Vec<RowCache>,

    terminal: TerminalModel,
    terminal_pty: PseudoTerminal,
    reader: Receiver<VteEvent>,
    writer: Sender<Vec<u8>>,

    last_mouse_pos: (u32, u32),
    modifiers: ModifiersState,
    is_mouse_down: bool,
    face: ttf_parser::Face<'static>,

    config: Config,
    bloom_renderer: BloomRenderer,
    main_scene_texture: Option<wgpu::Texture>,
    main_scene_view: Option<wgpu::TextureView>,
}

impl WindowState {
    async fn new(
        window: Arc<dyn Window>,
        proxy: EventLoopProxy,
        redraw_pending: Arc<AtomicBool>,
    ) -> io::Result<Self> {
        let size = window.surface_size();
        let scale_factor = window.scale_factor();

        let mut instance_descriptor = wgpu::InstanceDescriptor::new_without_display_handle();
        instance_descriptor.backends = wgpu::Backends::all();
        let instance = Instance::new(instance_descriptor);
        let surface = instance
            .create_surface(window.clone())
            .map_err(|e| io::Error::other(format!("create_surface failed: {e}")))?;

        let adapter = instance
            .request_adapter(&RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::HighPerformance,
                compatible_surface: Some(&surface),
                force_fallback_adapter: false,
            })
            .await
            .map_err(|e| io::Error::other(format!("request_adapter failed: {e}")))?;

        let (device, queue) = adapter
            .request_device(&DeviceDescriptor {
                required_features: wgpu::Features::empty(),
                required_limits: adapter.limits(),
                ..Default::default()
            })
            .await
            .map_err(|e| io::Error::other(format!("request_device failed: {e}")))?;

        let caps = surface.get_capabilities(&adapter);
        let format = caps
            .formats
            .iter()
            .copied()
            .find(|f| matches!(f, TextureFormat::Rgba8Unorm | TextureFormat::Bgra8Unorm))
            .or_else(|| caps.formats.iter().copied().find(|f| f.is_srgb()))
            .or_else(|| caps.formats.first().copied())
            .ok_or_else(|| io::Error::other("no surface format available"))?;
        let present_mode = PresentMode::AutoVsync;
        let alpha_mode = if caps
            .alpha_modes
            .contains(&CompositeAlphaMode::PostMultiplied)
        {
            CompositeAlphaMode::PostMultiplied
        } else if caps.alpha_modes.contains(&CompositeAlphaMode::Opaque) {
            CompositeAlphaMode::Opaque
        } else {
            caps.alpha_modes[0]
        };

        let limits = device.limits();
        let width = size.width.clamp(1, limits.max_texture_dimension_2d);
        let height = size.height.clamp(1, limits.max_texture_dimension_2d);
        let grid = grid_from_pixels(width, height, scale_factor);

        let config = Config::load();
        let surface_config = {
            let mut surface_config = SurfaceConfiguration {
                usage: TextureUsages::RENDER_ATTACHMENT,
                format,
                width,
                height,
                present_mode,
                alpha_mode,
                view_formats: vec![],
                desired_maximum_frame_latency: 2,
            };

            if config.transparency.enabled {
                surface_config.alpha_mode = CompositeAlphaMode::PostMultiplied;
            }
            surface_config
        };

        surface.configure(&device, &surface_config);
        let font_atlas = generate_atlas(&device, &queue, FONT_DATA);
        let text_renderer = SDFTextRenderer::new(&device, format);
        let bg_renderer = BackgroundRenderer::new(&device, format);
        let cursor_renderer = BackgroundRenderer::new(&device, format);

        let mut terminal_pty = PseudoTerminal::new(grid)?;
        let mut command = shell::default();
        terminal_pty.spawn(&mut command)?;

        let bloom_renderer = BloomRenderer::new(&device, format);

        let (main_scene_texture, main_scene_view) = if config.bloom.enabled {
            let tex = device.create_texture(&wgpu::TextureDescriptor {
                label: Some("Main Scene Texture"),
                size: wgpu::Extent3d {
                    width,
                    height,
                    depth_or_array_layers: 1,
                },
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                format,
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT
                    | wgpu::TextureUsages::TEXTURE_BINDING,
                view_formats: &[],
            });
            let view = tex.create_view(&wgpu::TextureViewDescriptor::default());
            (Some(tex), Some(view))
        } else {
            (None, None)
        };

        #[cfg(target_os = "macos")]
        if config.transparency.enabled {
            use winit::raw_window_handle::{HasWindowHandle, RawWindowHandle};
            if let Ok(handle) = window.window_handle()
                && let RawWindowHandle::AppKit(appkit) = handle.as_raw()
            {
                let ptr = appkit.ns_view.as_ptr();
                unsafe {
                    macos_transparency::make_window_transparent(ptr);
                }
            }
        }

        let (reader_tx, reader_rx) = crossbeam_channel::unbounded::<VteEvent>();
        let mut pty_reader = terminal_pty.reader()?;
        thread::spawn(move || {
            let mut parser = Vte::new(Handler(reader_tx));
            let mut bytes = [0u8; 4096];
            loop {
                match pty_reader.read(&mut bytes) {
                    Ok(0) => {
                        if !redraw_pending.swap(true, Ordering::AcqRel) {
                            proxy.wake_up();
                        }
                        break;
                    }
                    Ok(amount) => {
                        parser.process(&bytes[..amount]);
                        if !redraw_pending.swap(true, Ordering::AcqRel) {
                            proxy.wake_up();
                        }
                    }
                    Err(_) => {
                        if !redraw_pending.swap(true, Ordering::AcqRel) {
                            proxy.wake_up();
                        }
                        break;
                    }
                }
            }
        });

        let (writer_tx, writer_rx) = crossbeam_channel::unbounded::<Vec<u8>>();
        let mut pty_writer = terminal_pty.writer()?;
        thread::spawn(move || {
            for payload in writer_rx {
                let _ = pty_writer.write_all(&payload);
                let _ = pty_writer.flush();
            }
        });

        let face = ttf_parser::Face::parse(FONT_DATA, 4)
            .unwrap_or_else(|_| ttf_parser::Face::parse(FONT_DATA, 0).unwrap());

        Ok(Self {
            window,
            device,
            queue,
            surface,
            surface_config,
            font_atlas,
            text_renderer,
            bg_renderer,
            cursor_renderer,
            scale_factor,
            images: Vec::new(),
            cursor: AnimatedCursor::new(CursorPos { x: 0, y: 0 }),
            content_dirty: true,
            cached_cursor_x_px: 0.0,
            terminal: TerminalModel::new(grid.cols, grid.rows),
            terminal_pty,
            reader: reader_rx,
            writer: writer_tx,
            last_mouse_pos: (0, 0),
            modifiers: ModifiersState::default(),
            is_mouse_down: false,
            face,
            row_cache: (0..HISTORY_ROWS).map(|_| RowCache::default()).collect(),
            config,
            bloom_renderer,
            main_scene_texture,
            main_scene_view,
        })
    }

    fn resize(&mut self, width: u32, height: u32) {
        if width == 0 || height == 0 {
            return;
        }

        let limits = self.device.limits();
        let width = width.clamp(1, limits.max_texture_dimension_2d);
        let height = height.clamp(1, limits.max_texture_dimension_2d);

        self.surface_config.width = width;
        self.surface_config.height = height;
        self.surface.configure(&self.device, &self.surface_config);

        self.scale_factor = self.window.scale_factor();
        let grid = grid_from_pixels(width, height, self.scale_factor);

        self.terminal.resize(grid.cols, grid.rows);
        if let Err(e) = self.terminal_pty.resize(grid) {
            eprintln!("Failed to resize PTY: {}", e);
        }

        if self.config.bloom.enabled {
            let tex = self.device.create_texture(&wgpu::TextureDescriptor {
                label: Some("Main Scene Texture"),
                size: wgpu::Extent3d {
                    width,
                    height,
                    depth_or_array_layers: 1,
                },
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                format: self.surface_config.format,
                usage: wgpu::TextureUsages::RENDER_ATTACHMENT
                    | wgpu::TextureUsages::TEXTURE_BINDING,
                view_formats: &[],
            });
            let view = tex.create_view(&wgpu::TextureViewDescriptor::default());
            self.main_scene_texture = Some(tex);
            self.main_scene_view = Some(view);
        }

        self.content_dirty = true;
    }

    fn handle_keyboard(&mut self, event: &winit::event::KeyEvent) {
        if event.state != ElementState::Pressed {
            return;
        }

        let ctrl = self.modifiers.control_key();
        if ctrl
            && let Key::Character(c) = &event.logical_key
            && let Some(c) = c.chars().next()
            && c.is_ascii_alphabetic()
        {
            let code = (c.to_ascii_uppercase() as u8) - b'@';
            let _ = self.writer.send(vec![code]);
            return;
        }

        if let Some(payload) =
            convert::convert_key(&event.logical_key.as_ref(), event.text.as_deref())
        {
            let _ = self.writer.send(payload.as_bytes().to_vec());
        }
    }

    fn handle_mouse_wheel(&mut self, delta: MouseScrollDelta) {
        if self.terminal.mouse_mode != MouseMode::None {
            let btn = match delta {
                MouseScrollDelta::LineDelta(_, y) => {
                    if y > 0.0 {
                        64
                    } else {
                        65
                    }
                }
                MouseScrollDelta::PixelDelta(pos) => {
                    if pos.y > 0.0 {
                        64
                    } else {
                        65
                    }
                }
            };
            let (x, y) = self.last_mouse_pos;
            self.report_mouse(btn, x, y, true);
            return;
        }

        let previous = self.terminal.scroll_offset;
        let lines = match delta {
            MouseScrollDelta::LineDelta(_, y) => y as i32,
            MouseScrollDelta::PixelDelta(pos) => {
                let (_, cell_height, _) = scaled_metrics(self.scale_factor);
                (pos.y / cell_height as f64) as i32
            }
        };
        self.terminal.scroll_offset =
            (self.terminal.scroll_offset + lines).clamp(0, self.terminal.screen_top() as i32);
        if self.terminal.scroll_offset != previous {
            self.content_dirty = true;
        }
    }

    fn handle_cursor_moved(&mut self, pos: PhysicalPosition<f64>) {
        let (cell_width, cell_height, _) = scaled_metrics(self.scale_factor);
        let x = (pos.x as f32 / cell_width).floor() as u32;
        let y = (pos.y as f32 / cell_height).floor() as u32;
        self.last_mouse_pos = (x, y);

        let is_any = self.terminal.mouse_mode == MouseMode::AnyMotion;
        let is_btn = self.terminal.mouse_mode == MouseMode::ButtonMotion && self.is_mouse_down;

        if is_any || is_btn {
            self.report_mouse(32, x, y, true);
        }
    }

    fn handle_mouse_input(
        &mut self,
        state: ElementState,
        button: winit::event::MouseButton,
        pos: PhysicalPosition<f64>,
    ) {
        let (cell_width, cell_height, _) = scaled_metrics(self.scale_factor);
        let x = (pos.x as f32 / cell_width).floor() as u32;
        let y = (pos.y as f32 / cell_height).floor() as u32;
        self.last_mouse_pos = (x, y);

        self.is_mouse_down = state == ElementState::Pressed;

        let btn = match button {
            winit::event::MouseButton::Left => 0,
            winit::event::MouseButton::Middle => 1,
            winit::event::MouseButton::Right => 2,
            _ => return,
        };
        self.report_mouse(btn, x, y, state == ElementState::Pressed);
    }

    fn report_mouse(&mut self, button: u32, x: u32, y: u32, pressed: bool) {
        if self.terminal.mouse_mode == MouseMode::None {
            return;
        }

        let x = x.min(self.terminal.cols - 1);
        let y = y.min(self.terminal.viewport_rows - 1);

        let mut b = button;
        if self.modifiers.shift_key() {
            b |= 4;
        }
        if self.modifiers.alt_key() {
            b |= 8;
        }
        if self.modifiers.control_key() {
            b |= 16;
        }

        if self.terminal.mouse_protocol_sgr {
            let suffix = if pressed { 'M' } else { 'm' };
            let report = format!("\x1b[<{};{};{}{}", b, x + 1, y + 1, suffix);
            let _ = self.writer.send(report.into_bytes());
        } else {
            // Standard X11 mouse reporting
            if x < 223 && y < 223 {
                let mut data = Vec::with_capacity(6);
                data.extend_from_slice(b"\x1b[M");
                let b_val = if pressed { b } else { 3 | (b & 0x1c) };
                data.push((b_val as u8).wrapping_add(32));
                data.push((x as u8).wrapping_add(33));
                data.push((y as u8).wrapping_add(33));
                let _ = self.writer.send(data);
            }
        }
    }

    fn handle_focus(&mut self, focused: bool) {
        if self.terminal.focus_reporting {
            let escape = if focused { b"\x1b[I" } else { b"\x1b[O" };
            let _ = self.writer.send(escape.to_vec());
        }
    }

    fn pump_terminal(&mut self) -> bool {
        let mut changed = false;
        for event in self.reader.try_iter() {
            changed = true;
            let device = &self.device;
            let queue = &self.queue;
            let format = self.surface_config.format;
            let surface_width = self.surface_config.width as f32;
            let surface_height = self.surface_config.height as f32;
            let scale_factor = self.scale_factor;
            let terminal = &mut self.terminal;
            let images = &mut self.images;
            let cursor_x = terminal.cursor.x;
            let cursor_y = terminal.cursor.y;
            terminal.apply_vte_event(event, &self.writer, &mut |image| {
                let Ok(image_data) =
                    base64::Engine::decode(&base64::engine::general_purpose::STANDARD, image)
                else {
                    return;
                };
                let Ok(decoded) = image::load_from_memory(&image_data) else {
                    return;
                };
                let rgba = decoded.to_rgba8();
                let (width, height) = decoded.dimensions();
                let x = cursor_x as f32 * CELL_WIDTH * scale_factor as f32;
                let y = cursor_y as f32 * CELL_HEIGHT * scale_factor as f32;
                if let Ok(sprite) = ImageSprite::new(
                    device,
                    queue,
                    format,
                    &rgba,
                    width,
                    height,
                    x,
                    y,
                    surface_width,
                    surface_height,
                ) {
                    images.push(sprite);
                }
            });
        }
        self.cursor.set_target(self.terminal.cursor);
        if self.terminal.cursor_hidden {
            self.cursor.snap();
        }
        changed
    }

    fn redraw(&mut self) {
        if self.pump_terminal() {
            self.content_dirty = true;
        }

        if self.content_dirty {
            let row_start = self.terminal.row_start;
            let screen_top = self.terminal.screen_top;
            tracing::debug!(row_start, screen_top, "content_dirty redraw trigger");
        }

        let (cell_width, cell_height, font_size) = scaled_metrics(self.scale_factor);
        let view_top = self.terminal.current_view_top() as usize;
        let view_rows = self.terminal.viewport_rows as usize;

        if self.content_dirty {
            let row_start = self.terminal.row_start;
            let (sw, sh) = (
                self.surface_config.width as f32,
                self.surface_config.height as f32,
            );

            let mut dirty_rows_to_generate = Vec::new();
            for row_idx in 0..view_rows {
                let y = view_top + row_idx;
                let physical_row = (y + row_start) % HISTORY_ROWS as usize;
                if self.terminal.dirty_rows[physical_row] {
                    dirty_rows_to_generate.push(physical_row);
                    self.terminal.dirty_rows[physical_row] = false;
                }
            }

            if !dirty_rows_to_generate.is_empty() {
                let font_atlas = &self.font_atlas;
                let face = &self.face;
                let lut = get_linear_lut();
                let cols = self.terminal.cols;

                let new_caches: Vec<(usize, RowCache)> = dirty_rows_to_generate
                    .into_par_iter()
                    .map(|physical_row| {
                        let mut cache = RowCache::default();
                        let row_data = &self.terminal.cells
                            [physical_row * cols as usize..(physical_row + 1) * cols as usize];

                        // Vertical position in cache is always 0.0 (relative to row)
                        let y_px = 0.0;

                        for col in 0..cols {
                            let cell = &row_data[col as usize];
                            let x = col as f32 * cell_width;
                            let mut ch = cell.ch;

                            if let Some(bg) = cell.style.bg {
                                append_bg_quad(
                                    &mut cache.bg,
                                    sw,
                                    sh,
                                    x,
                                    y_px,
                                    cell_width,
                                    cell_height,
                                    bg,
                                    lut,
                                );
                            }

                            if is_custom_block(ch)
                                && append_custom_block(
                                    &mut cache.bg,
                                    sw,
                                    sh,
                                    x,
                                    y_px,
                                    cell_width,
                                    cell_height,
                                    ch,
                                    cell.style.fg,
                                    lut,
                                )
                            {
                                ch = ' ';
                            }

                            if ch != ' ' {
                                let fg = cell.style.fg;
                                let color = [
                                    lut[fg.r as usize],
                                    lut[fg.g as usize],
                                    lut[fg.b as usize],
                                    fg.a as f32 / 255.0,
                                ];
                                SDFTextRenderer::layout_char(
                                    font_atlas,
                                    face,
                                    ch,
                                    x,
                                    y_px,
                                    cell_width,
                                    cell_height,
                                    font_size,
                                    sw,
                                    sh,
                                    color,
                                    &mut cache.text,
                                );
                            }
                        }
                        (physical_row, cache)
                    })
                    .collect();

                for (idx, cache) in new_caches {
                    self.row_cache[idx] = cache;
                }
            }

            let mut bg_vertices = Vec::new();
            let mut text_vertices = Vec::new();

            for row_idx in 0..view_rows {
                let y = view_top + row_idx;
                let physical_row = (y + row_start) % HISTORY_ROWS as usize;

                // Calculate position relative to viewport top
                let py_offset_ndc = (row_idx as f32 * cell_height / sh) * 2.0;

                for v in &self.row_cache[physical_row].bg {
                    let mut v = *v;
                    v.pos[1] -= py_offset_ndc;
                    bg_vertices.push(v);
                }
                for v in &self.row_cache[physical_row].text {
                    let mut v = *v;
                    v.pos[1] -= py_offset_ndc;
                    text_vertices.push(v);
                }
            }

            self.bg_renderer
                .prepare(&self.device, &self.queue, &bg_vertices);
            self.text_renderer
                .prepare(&self.device, &self.queue, &text_vertices);
            self.content_dirty = false;
        }

        let frame = match self.surface.get_current_texture() {
            CurrentSurfaceTexture::Success(f) | CurrentSurfaceTexture::Suboptimal(f) => f,
            CurrentSurfaceTexture::Outdated | CurrentSurfaceTexture::Lost => {
                self.surface.configure(&self.device, &self.surface_config);
                return;
            }
            _ => return,
        };
        let view = frame.texture.create_view(&TextureViewDescriptor::default());
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("terminal-encoder"),
            });

        let scene_view = if self.config.bloom.enabled {
            self.main_scene_view.as_ref().unwrap()
        } else {
            &view
        };

        let clear_color = wgpu::Color::TRANSPARENT;

        {
            let mut pass = encoder.begin_render_pass(&RenderPassDescriptor {
                label: Some("terminal-pass"),
                color_attachments: &[Some(RenderPassColorAttachment {
                    view: scene_view,
                    depth_slice: None,
                    resolve_target: None,
                    ops: Operations {
                        load: LoadOp::Clear(clear_color),
                        store: wgpu::StoreOp::Store,
                    },
                })],
                depth_stencil_attachment: None,
                timestamp_writes: None,
                occlusion_query_set: None,
                multiview_mask: None,
            });
            self.bg_renderer.render(&mut pass);
            self.text_renderer.render(&self.font_atlas, &mut pass);
            for image in &self.images {
                image.render(&mut pass);
            }

            let (_, cell_height, _) = scaled_metrics(self.scale_factor);
            let mut cursor_vertices = Vec::new();
            let cursor_samples: Vec<((f32, f32), u8)> =
                std::iter::once((self.cursor.position, 220))
                    .chain(self.cursor.trail.iter().enumerate().map(|(idx, sample)| {
                        let alpha = match idx {
                            0 => 160,
                            1 => 120,
                            2 => 90,
                            3 => 70,
                            _ => 40,
                        };
                        (sample.position, alpha)
                    }))
                    .collect();

            for (index, (pos, alpha)) in cursor_samples.iter().enumerate() {
                if pos.1 < view_top as f32 || pos.1 >= (view_top + view_rows) as f32 {
                    continue;
                }

                let progress = if index == 0 { 0.0 } else { index as f32 / 10.0 };
                let width = 10.0_f32;
                let inset = (width * (0.10 + progress * 0.10)).min(width * 0.35);
                let x = (pos.0 * cell_width - inset * 0.25).max(0.0);
                let y = (pos.1 - view_top as f32) * cell_height + inset * 0.15;

                let w = width - inset * 0.5;
                let h = cell_height - inset * 0.25;
                // Cursor color is always white, so we can use a fixed LUT value or direct conversion
                let lut = get_linear_lut();
                let alpha_base = *alpha as f32 / 255.0;
                let now = self.cursor.last_tick.elapsed().as_secs_f32();
                let pulse = (now * 4.0).sin() * 0.2 + 0.8;
                let final_alpha = (alpha_base * pulse).clamp(0.0, 1.0);

                append_bg_quad(
                    &mut cursor_vertices,
                    self.surface_config.width as f32,
                    self.surface_config.height as f32,
                    x,
                    y,
                    w,
                    h,
                    Rgba {
                        r: 0,
                        g: 255,
                        b: 255,
                        a: (final_alpha * 255.0) as u8,
                    },
                    lut,
                );
            }

            self.cursor_renderer
                .prepare(&self.device, &self.queue, &cursor_vertices);
            if !self.terminal.cursor_hidden {
                self.cursor_renderer.render(&mut pass);
            }
        }

        if self.config.bloom.enabled {
            self.bloom_renderer.render(
                &self.device,
                &self.queue,
                &mut encoder,
                self.main_scene_view.as_ref().unwrap(),
                &view,
                self.surface_config.width,
                self.surface_config.height,
                &self.config.bloom,
                if self.config.transparency.enabled {
                    self.config.transparency.opacity
                } else {
                    1.0
                },
            );
        }

        self.queue.submit(Some(encoder.finish()));
        frame.present();
        if self.cursor.update() {
            self.window.request_redraw();
        }
    }

    fn row_spans(&self, y: u32) -> Vec<(String, Rgba)> {
        let mut spans: Vec<(String, Rgba)> = Vec::new();
        let mut current_style: Option<CellStyle> = None;

        for x in 0..self.terminal.cols {
            let cell = self.terminal.get(y, x);
            let display_ch = if is_custom_block(cell.ch) {
                ' '
            } else {
                cell.ch
            };

            if current_style == Some(cell.style) {
                if let Some((text, _)) = spans.last_mut() {
                    text.push(display_ch);
                }
            } else {
                current_style = Some(cell.style);
                spans.push((display_ch.to_string(), cell.style.fg));
            }
        }

        if spans.is_empty() {
            spans.push((" ".to_string(), Rgba::WHITE));
        }
        spans
    }
}

struct App {
    state: Option<WindowState>,
    proxy: EventLoopProxy,
    redraw_pending: Arc<AtomicBool>,
}

impl ApplicationHandler for App {
    fn can_create_surfaces(&mut self, event_loop: &dyn ActiveEventLoop) {
        if self.state.is_some() {
            return;
        }

        let attributes = WindowAttributes::default()
            .with_title("milkshake-terminal")
            .with_surface_size(LogicalSize::new(1200.0, 720.0))
            .with_transparent(true);
        let window = Arc::from(event_loop.create_window(attributes).expect("create window"));

        match pollster::block_on(WindowState::new(
            window,
            self.proxy.clone(),
            self.redraw_pending.clone(),
        )) {
            Ok(mut state) => {
                let size = state.window.surface_size();
                state.resize(size.width, size.height);
                self.state = Some(state);
            }
            Err(err) => {
                eprintln!("failed to initialize terminal: {err}");
                event_loop.exit();
            }
        }
    }

    fn proxy_wake_up(&mut self, _event_loop: &dyn ActiveEventLoop) {
        if let Some(state) = &self.state {
            state.window.request_redraw();
        }
    }

    fn about_to_wait(&mut self, _event_loop: &dyn ActiveEventLoop) {}

    fn window_event(
        &mut self,
        event_loop: &dyn ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        let Some(state) = &mut self.state else {
            return;
        };

        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }
            WindowEvent::SurfaceResized(size) => {
                state.resize(size.width, size.height);
                state.window.request_redraw();
            }
            WindowEvent::MouseWheel { delta, .. } => {
                state.handle_mouse_wheel(delta);
                state.window.request_redraw();
            }
            WindowEvent::KeyboardInput { event, .. } => {
                state.handle_keyboard(&event);
                state.window.request_redraw();
            }
            WindowEvent::ModifiersChanged(modifiers) => {
                state.modifiers = modifiers.state();
            }
            WindowEvent::Focused(focused) => {
                state.handle_focus(focused);
            }
            WindowEvent::PointerMoved { position, .. } => {
                state.handle_cursor_moved(position);
            }
            WindowEvent::PointerButton {
                state: btn_state,
                button: winit::event::ButtonSource::Mouse(mouse_btn),
                position,
                ..
            } => {
                state.handle_mouse_input(btn_state, mouse_btn, position);
            }
            WindowEvent::RedrawRequested => {
                state.redraw();
                self.redraw_pending.store(false, Ordering::Release);
            }
            _ => {}
        }
    }
}

pub fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();
    let event_loop = EventLoop::new().expect("create event loop");
    let proxy = event_loop.create_proxy();
    let app = App {
        state: None,
        proxy,
        redraw_pending: Arc::new(AtomicBool::new(false)),
    };
    event_loop.run_app(app).expect("run app");
}

struct Handler(Sender<VteEvent>);

impl vte::VteHandler for Handler {
    fn vte_event(&mut self, event: VteEvent) {
        let _ = self.0.send(event);
    }
}

fn grid_from_pixels(width: u32, height: u32, scale_factor: f64) -> GridSize {
    let (cell_width, cell_height, _) = scaled_metrics(scale_factor);
    GridSize {
        cols: ((width as f32 / cell_width).floor() as u32).max(1),
        rows: ((height as f32 / cell_height).floor() as u32).max(1),
        width,
        height,
    }
}

fn scaled_metrics(scale_factor: f64) -> (f32, f32, f32) {
    let scale = scale_factor.max(1.0) as f32;
    (CELL_WIDTH * scale, CELL_HEIGHT * scale, FONT_SIZE * scale)
}

fn lerp_u32(current: u32, target: u32, t: f32) -> u32 {
    let current = current as f32;
    let target = target as f32;
    (current + (target - current) * t).round().max(0.0) as u32
}

pub fn is_custom_block(ch: char) -> bool {
    let u = ch as u32;
    if (0x2800..=0x28FF).contains(&u) {
        return true;
    }
    matches!(
        ch,
        '▀' | '▄'
            | '█'
            | '▌'
            | '▐'
            | '│'
            | '─'
            | '┌'
            | '┐'
            | '└'
            | '┘'
            | '├'
            | '┤'
            | '┬'
            | '┴'
            | '┼'
    )
}

fn append_custom_block(
    out: &mut Vec<BgVertex>,
    width: f32,
    height: f32,
    x: f32,
    y: f32,
    cell_width: f32,
    cell_height: f32,
    ch: char,
    color: Rgba,
    lut: &[f32; 256],
) -> bool {
    let u = ch as u32;
    // Braille: 0x2800..=0x28FF
    if (0x2800..=0x28FF).contains(&u) {
        let offset = u - 0x2800;
        let dot_w = cell_width / 2.0;
        let dot_h = cell_height / 4.0;
        let margin_x = dot_w * 0.15;
        let margin_y = dot_h * 0.15;

        let mut add_dot = |dx: f32, dy: f32| {
            append_bg_quad(
                out,
                width,
                height,
                x + dx * dot_w + margin_x,
                y + dy * dot_h + margin_y,
                dot_w - margin_x * 2.0,
                dot_h - margin_y * 2.0,
                color,
                lut,
            );
        };

        if (offset & 0x1) != 0 {
            add_dot(0.0, 0.0);
        }
        if (offset & 0x2) != 0 {
            add_dot(0.0, 1.0);
        }
        if (offset & 0x4) != 0 {
            add_dot(0.0, 2.0);
        }
        if (offset & 0x8) != 0 {
            add_dot(1.0, 0.0);
        }
        if (offset & 0x10) != 0 {
            add_dot(1.0, 1.0);
        }
        if (offset & 0x20) != 0 {
            add_dot(1.0, 2.0);
        }
        if (offset & 0x40) != 0 {
            add_dot(0.0, 3.0);
        }
        if (offset & 0x80) != 0 {
            add_dot(1.0, 3.0);
        }
        return true;
    }

    let mut draw = |x_px: f32, y_px: f32, w_px: f32, h_px: f32| {
        append_bg_quad(
            out,
            width,
            height,
            x + x_px,
            y + y_px,
            w_px,
            h_px,
            color,
            lut,
        );
    };

    match ch {
        '▀' => draw(0.0, 0.0, cell_width, cell_height / 2.0),
        '▄' => draw(0.0, cell_height / 2.0, cell_width, cell_height / 2.0),
        '█' => draw(0.0, 0.0, cell_width, cell_height),
        '▌' => draw(0.0, 0.0, cell_width / 2.0, cell_height),
        '▐' => draw(cell_width / 2.0, 0.0, cell_width / 2.0, cell_height),

        '│' => draw(cell_width / 2.0 - 0.5, 0.0, 1.0, cell_height),
        '─' => draw(0.0, cell_height / 2.0 - 0.5, cell_width, 1.0),
        '┌' => {
            draw(
                cell_width / 2.0 - 0.5,
                cell_height / 2.0 - 0.5,
                1.0,
                cell_height / 2.0 + 0.5,
            );
            draw(
                cell_width / 2.0 - 0.5,
                cell_height / 2.0 - 0.5,
                cell_width / 2.0 + 0.5,
                1.0,
            );
        }
        '┐' => {
            draw(
                cell_width / 2.0 - 0.5,
                cell_height / 2.0 - 0.5,
                1.0,
                cell_height / 2.0 + 0.5,
            );
            draw(0.0, cell_height / 2.0 - 0.5, cell_width / 2.0 + 0.5, 1.0);
        }
        '└' => {
            draw(cell_width / 2.0 - 0.5, 0.0, 1.0, cell_height / 2.0 + 0.5);
            draw(
                cell_width / 2.0 - 0.5,
                cell_height / 2.0 - 0.5,
                cell_width / 2.0 + 0.5,
                1.0,
            );
        }
        '┘' => {
            draw(cell_width / 2.0 - 0.5, 0.0, 1.0, cell_height / 2.0 + 0.5);
            draw(0.0, cell_height / 2.0 - 0.5, cell_width / 2.0 + 0.5, 1.0);
        }
        '├' => {
            draw(cell_width / 2.0 - 0.5, 0.0, 1.0, cell_height);
            draw(
                cell_width / 2.0 - 0.5,
                cell_height / 2.0 - 0.5,
                cell_width / 2.0 + 0.5,
                1.0,
            );
        }
        '┤' => {
            draw(cell_width / 2.0 - 0.5, 0.0, 1.0, cell_height);
            draw(0.0, cell_height / 2.0 - 0.5, cell_width / 2.0 + 0.5, 1.0);
        }
        '┬' => {
            draw(
                cell_width / 2.0 - 0.5,
                cell_height / 2.0 - 0.5,
                1.0,
                cell_height / 2.0 + 0.5,
            );
            draw(0.0, cell_height / 2.0 - 0.5, cell_width, 1.0);
        }
        '┴' => {
            draw(cell_width / 2.0 - 0.5, 0.0, 1.0, cell_height / 2.0 + 0.5);
            draw(0.0, cell_height / 2.0 - 0.5, cell_width, 1.0);
        }
        '┼' => {
            draw(cell_width / 2.0 - 0.5, 0.0, 1.0, cell_height);
            draw(0.0, cell_height / 2.0 - 0.5, cell_width, 1.0);
        }
        _ => return false,
    }
    true
}

fn append_bg_quad(
    out: &mut Vec<BgVertex>,
    width: f32,
    height: f32,
    x: f32,
    y: f32,
    w: f32,
    h: f32,
    color: Rgba,
    lut: &[f32; 256],
) {
    let color = [
        lut[color.r as usize],
        lut[color.g as usize],
        lut[color.b as usize],
        color.a as f32 / 255.0,
    ];

    let x0 = (x / width) * 2.0 - 1.0;
    let x1 = ((x + w) / width) * 2.0 - 1.0;
    let y0 = 1.0 - (y / height) * 2.0;
    let y1 = 1.0 - ((y + h) / height) * 2.0;

    out.extend_from_slice(&[
        BgVertex {
            pos: [x0, y0],
            color,
        },
        BgVertex {
            pos: [x1, y0],
            color,
        },
        BgVertex {
            pos: [x1, y1],
            color,
        },
        BgVertex {
            pos: [x0, y0],
            color,
        },
        BgVertex {
            pos: [x1, y1],
            color,
        },
        BgVertex {
            pos: [x0, y1],
            color,
        },
    ]);
}

static ANSI_TABLE: [Rgba; 16] = [
    Rgba::rgb(40, 44, 52),    // Black (One Dark variant)
    Rgba::rgb(224, 108, 117), // Red
    Rgba::rgb(152, 195, 121), // Green
    Rgba::rgb(229, 192, 123), // Yellow
    Rgba::rgb(97, 175, 239),  // Blue
    Rgba::rgb(198, 120, 221), // Magenta
    Rgba::rgb(86, 182, 194),  // Cyan
    Rgba::rgb(171, 178, 191), // White
    Rgba::rgb(92, 99, 112),   // Bright Black
    Rgba::rgb(255, 125, 125), // Bright Red
    Rgba::rgb(165, 255, 140), // Bright Green
    Rgba::rgb(255, 220, 150), // Bright Yellow
    Rgba::rgb(120, 195, 255), // Bright Blue
    Rgba::rgb(220, 150, 255), // Bright Magenta
    Rgba::rgb(110, 240, 255), // Bright Cyan
    Rgba::rgb(255, 255, 255), // Bright White
];

fn ansi_to_rgb(color: AnsiColor) -> Rgba {
    match color {
        AnsiColor::Standard(StandardColor { color, intensity }) => {
            let base = match color {
                NamedColor::Black => 0,
                NamedColor::Red => 1,
                NamedColor::Green => 2,
                NamedColor::Yellow => 3,
                NamedColor::Blue => 4,
                NamedColor::Magenta => 5,
                NamedColor::Cyan => 6,
                NamedColor::White => 7,
            };
            let idx = base + if intensity == Intensity::Bright { 8 } else { 0 };
            ANSI_TABLE[idx]
        }
        AnsiColor::Index(index) => index_to_color(index),
        AnsiColor::Rgb(r, g, b) => Rgba::rgb(r, g, b),
    }
}

fn index_to_color(index: u8) -> Rgba {
    match index {
        0..=15 => ANSI_TABLE[index as usize],
        16..=231 => {
            let idx = index - 16;
            let r = (idx / 36) % 6;
            let g = (idx / 6) % 6;
            let b = idx % 6;
            let r = if r == 0 { 0 } else { r * 40 + 55 };
            let g = if g == 0 { 0 } else { g * 40 + 55 };
            let b = if b == 0 { 0 } else { b * 40 + 55 };
            Rgba::rgb(r, g, b)
        }
        232..=255 => {
            let value = (index - 232) * 10 + 8;
            Rgba::rgb(value, value, value)
        }
    }
}

#[derive(Default, Clone)]
struct RowCache {
    text: Vec<TextVertex>,
    bg: Vec<BgVertex>,
}
