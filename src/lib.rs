#![allow(dead_code)]
use crate::vte::AnsiColor;

use self::vte::{Vte, VteEvent};
use bevy::asset::{RenderAssetUsages, embedded_asset};
use bevy::color::Gray;
use bevy::color::palettes::basic;
use bevy::input::keyboard::KeyboardInput;
use bevy::input::mouse::MouseWheel;
use bevy::log::{debug, info};
use bevy::prelude::*;
use bevy::window::WindowResized;
use compact_str::CompactString;
use crossbeam_channel::{Receiver, Sender};
use pseudo_terminal::PseudoTerminal;
use std::io::{Read, Write};
use std::process::Command;
use std::{io, mem, thread};
use vte::{Intensity, StandardColor};

mod convert;
mod font;
mod pseudo_terminal;
mod shell;
mod vte;

#[derive(Clone, Copy, Component, Debug, Default, Reflect)]
#[reflect(Component, Debug, Default)]
#[require(Node)]
pub struct Terminal;

#[derive(Clone, Copy, Component, Debug, Default, Reflect)]
#[reflect(Component, Debug, Default)]
#[require(Node, Text)]
pub struct TerminalCell;

#[derive(Component, Debug)]
#[require(Terminal)]
pub struct TerminalCommand(pub Command);

#[derive(Clone, Debug, Reflect, Component)]
pub struct TerminalFonts {
    pub regular: Handle<Font>,
    pub regular_italic: Handle<Font>,
    pub bold: Handle<Font>,
    pub bold_italic: Handle<Font>,
}

#[derive(Clone, Copy, Debug)]
struct TerminalStyle {
    foreground: Color,
    background: Color,
    bold: bool,
    italic: bool,
}

impl Default for TerminalStyle {
    fn default() -> Self {
        Self {
            foreground: Color::WHITE,
            background: Color::NONE,
            bold: false,
            italic: false,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TerminalState {
    cursor_position: UVec2,
    style: TerminalStyle,
    size: UVec2,
}

impl Default for TerminalState {
    fn default() -> Self {
        Self {
            cursor_position: UVec2::ZERO,
            style: Default::default(),
            size: UVec2::new(80, 24),
        }
    }
}

impl TerminalState {
    pub fn cursor_position(&self) -> UVec2 {
        self.cursor_position
    }

    pub fn cursor_offset(&self) -> usize {
        ((self.cursor_position.y * self.size.x) + self.cursor_position.x) as usize
    }

    pub fn move_up(&mut self, rows: u32) {
        debug_assert!(rows >= 1);

        self.cursor_position.y = self.cursor_position.y.saturating_sub(rows);
    }

    pub fn move_down(&mut self, rows: u32) {
        debug_assert!(rows >= 1);

        self.cursor_position.y = self.cursor_position.y.saturating_add(rows);
    }

    pub fn move_left(&mut self, columns: u32) {
        debug_assert!(columns >= 1);

        if self.cursor_position.x >= columns {
            self.cursor_position.x -= columns;
        } else {
            let remaining = columns - self.cursor_position.x;
            let rows_to_move = remaining.div_ceil(self.size.x);
            if self.cursor_position.y >= rows_to_move {
                self.cursor_position.y -= rows_to_move;
                self.cursor_position.x =
                    (self.cursor_position.x + rows_to_move * self.size.x) - columns;
            } else {
                self.cursor_position = UVec2::ZERO;
            }
        }
    }

    pub fn move_right(&mut self, columns: u32) {
        debug_assert!(columns >= 1);

        self.cursor_position.x += columns;
        if self.cursor_position.x >= self.size.x {
            self.cursor_position.y += self.cursor_position.x / self.size.x;
            self.cursor_position.x %= self.size.x;
        }
    }

    pub fn goto(&mut self, position: UVec2) {
        self.cursor_position.x = position.x.min(self.size.x - 1);
        self.cursor_position.y = position.y; // Allow y to go beyond for scrolling detection
    }

    pub fn goto_x(&mut self, x: u32) {
        self.cursor_position.x = x.min(self.size.x - 1);
    }

    pub fn goto_y(&mut self, y: u32) {
        self.cursor_position.y = y;
    }

    pub fn line_up(&mut self, rows: u32) {
        self.move_up(rows);
        self.goto_x(0);
    }

    pub fn line_down(&mut self, rows: u32) {
        self.move_down(rows);
        self.goto_x(0);
    }

    pub fn style(&self) -> TerminalStyle {
        self.style
    }

    pub fn set_bold(&mut self) {
        self.style.bold = true;
    }

    pub fn set_italic(&mut self) {
        self.style.italic = true;
    }

    pub fn reset(&mut self) {
        mem::take(&mut self.style);
    }
}

#[derive(Debug, Component)]
pub struct InternalTerminalState {
    cells: Vec<Entity>,
    rows: Vec<Entity>,
    pseudo_terminal: PseudoTerminal,
    writer: Sender<CompactString>,
    reader: Receiver<VteEvent>,
    state: TerminalState,
    scroll_offset: i32,
    saved_cursor: Option<UVec2>,
    viewport_height: u32,
}

pub struct TerminalPlugin;

impl Plugin for TerminalPlugin {
    fn build(&self, app: &mut App) {
        embedded_asset!(app, "../assets/fonts/RobotoMono-SemiBold.ttf");
        embedded_asset!(app, "../assets/fonts/RobotoMono-SemiBoldItalic.ttf");
        embedded_asset!(app, "../assets/fonts/RobotoMono-Bold.ttf");
        embedded_asset!(app, "../assets/fonts/RobotoMono-BoldItalic.ttf");
    }
}

#[bevy_main]
pub fn main() {
    App::new()
        .insert_resource(ClearColor(Color::BLACK))
        .add_plugins((
            DefaultPlugins.set(WindowPlugin {
                primary_window: Some(Window {
                    #[cfg(target_os = "android")]
                    resizable: false,
                    #[cfg(target_os = "android")]
                    mode: WindowMode::BorderlessFullscreen(MonitorSelection::Primary),
                    recognize_rotation_gesture: true,
                    ..default()
                }),
                ..default()
            }),
            TerminalPlugin,
        ))
        .add_systems(Startup, setup)
        .add_systems(Update, (setup_terminal, update, rotate_cubes))
        .run();
}

fn rotate_cubes(mut query: Query<&mut Transform, With<Cube>>, time: ResMut<Time>) {
    for mut transform in query.iter_mut() {
        transform.rotate_local_x(time.delta_secs());
    }
}

fn setup(asset_server: Res<AssetServer>, mut commands: Commands) {
    commands.spawn((
        Camera3d::default(),
        #[cfg(target_os = "android")]
        Msaa::Off,
    ));

    commands.spawn((
        Node {
            display: Display::Flex,
            flex_direction: FlexDirection::Column,
            height: Val::Percent(100.0),
            width: Val::Percent(100.0),
            ..default()
        },
        Terminal,
        TerminalCommand(shell::default()),
        font::default(&asset_server),
    ));
}

struct Handler(Sender<VteEvent>);

impl vte::VteHandler for Handler {
    fn vte_event(&mut self, event: VteEvent) {
        self.0.send(event).unwrap();
    }
}

pub fn setup_terminal(
    mut commands: Commands,
    mut query: Query<(Entity, &mut TerminalCommand), Without<InternalTerminalState>>,
) {
    for (entity, mut command) in query.iter_mut() {
        let mut pseudo_terminal = PseudoTerminal::new(UVec2::new(80, 24)).unwrap();

        pseudo_terminal.configure_command(&mut command.0).unwrap();

        let reader = {
            let (sender, receiver) = crossbeam_channel::unbounded::<VteEvent>();
            let mut control = pseudo_terminal.control.clone();

            thread::spawn(move || {
                let mut vte = Vte::new(Handler(sender));
                let mut buf = [0; 1024];

                while let Ok(amount) = control.read(&mut buf) {
                    if amount == 0 {
                        info!("Reader thread: EOF reached");
                        break;
                    }
                    info!(
                        "Reader thread: read {} bytes: {:?}",
                        amount,
                        String::from_utf8_lossy(&buf[..amount])
                    );
                    vte.process(&buf[..amount]);
                }
            });

            receiver
        };

        let writer = {
            let (sender, receiver) = crossbeam_channel::unbounded::<CompactString>();
            let mut control = pseudo_terminal.control.clone();

            thread::spawn(move || {
                for text in receiver.iter() {
                    info!("Writing to PTY: {:?}", text);
                    control.write_all(text.as_bytes())?;
                }

                io::Result::Ok(())
            });

            sender
        };

        let result = command.0.spawn();

        debug!("spawn command: {command:?} {result:?}");

        let mut rows = Vec::with_capacity(1000);
        commands.entity(entity).with_children(|builder| {
            for _ in 0..1000 {
                rows.push(
                    builder
                        .spawn(Node {
                            display: Display::None,
                            grid_template_columns: RepeatedGridTrack::px(80, 10.0),
                            grid_template_rows: RepeatedGridTrack::px(1, 18.0),
                            width: Val::Percent(100.0),
                            height: Val::Px(18.0),
                            ..default()
                        })
                        .id(),
                );
            }
        });

        let internal_terminal_state = InternalTerminalState {
            cells: vec![Entity::PLACEHOLDER; 80 * 1000],
            rows,
            pseudo_terminal,
            reader,
            writer,
            state: TerminalState {
                size: UVec2::new(80, 1000), // Logical size is 1000 rows
                ..default()
            },
            scroll_offset: 0,
            saved_cursor: None,
            viewport_height: 24,
        };

        commands.entity(entity).insert(internal_terminal_state);
    }
}

#[derive(Clone, Copy, Component, Debug, Default, Eq, PartialEq, Reflect)]
pub struct Cube;

static TABLE: [Srgba; 16] = [
    // Normal
    basic::BLACK,
    basic::RED,
    basic::GREEN,
    basic::YELLOW,
    basic::BLUE,
    basic::FUCHSIA,
    basic::AQUA,
    basic::WHITE,
    // Bright
    Srgba::rgb(0.3, 0.3, 0.3),
    Srgba::rgb(1.0, 0.3, 0.3),
    Srgba::rgb(0.3, 1.0, 0.3),
    Srgba::rgb(1.0, 1.0, 0.3),
    Srgba::rgb(0.3, 0.3, 1.0),
    Srgba::rgb(1.0, 0.3, 1.0),
    Srgba::rgb(0.3, 1.0, 1.0),
    Srgba::WHITE,
];

fn index_to_color(index: u8) -> Srgba {
    match index {
        0..=15 => TABLE[index as usize],
        16..=231 => {
            let index = index - 16;
            let r = (index / 36) % 6;
            let g = (index / 6) % 6;
            let b = index % 6;
            let r = if r == 0 { 0 } else { r * 40 + 55 };
            let g = if g == 0 { 0 } else { g * 40 + 55 };
            let b = if b == 0 { 0 } else { b * 40 + 55 };
            Srgba::rgb_u8(r, g, b)
        }
        232..=255 => {
            let val = (index - 232) * 10 + 8;
            Srgba::rgb_u8(val, val, val)
        }
    }
}

fn update(
    mut commands: Commands,
    mut images: ResMut<Assets<Image>>,
    _meshes: ResMut<Assets<Mesh>>,
    _materials: ResMut<Assets<StandardMaterial>>,
    mut keyboard_input: MessageReader<KeyboardInput>,
    mut mouse_wheel_input: MessageReader<MouseWheel>,
    mut resize_events: MessageReader<WindowResized>,
    mut query: Query<(Entity, &mut InternalTerminalState)>,
    terminal_fonts: Query<&TerminalFonts>,
    touch_input: Res<Touches>,
    mut cell_query: Query<(
        &mut BackgroundColor,
        &mut Text,
        &mut TextColor,
        &mut TextFont,
        Option<&mut TextLayout>,
    )>,
    mut node_query: Query<&mut Node>,
) {
    let mut last_resize = None;
    for event in resize_events.read() {
        last_resize = Some(event);
    }

    if let Some(event) = last_resize {
        let cols = (event.width / 10.0) as u32;
        let rows_count = (event.height / 18.0) as u32;
        info!(
            "Window resized to {}x{}, terminal grid {}x{}",
            event.width, event.height, cols, rows_count
        );

        for (_, mut internal_state) in query.iter_mut() {
            let old_cols = internal_state.state.size.x;
            if old_cols != cols {
                // Reflow cells
                let mut new_cells = vec![Entity::PLACEHOLDER; (cols * 1000) as usize];
                for y in 0..1000 {
                    for x in 0..old_cols.min(cols) {
                        new_cells[(y * cols + x) as usize] =
                            internal_state.cells[(y * old_cols + x) as usize];
                    }
                }
                internal_state.cells = new_cells;
            }

            internal_state.state.size.x = cols;
            internal_state.viewport_height = rows_count;
            // We keep logical 1000 rows for now, but PTY needs to know visible rows
            let _ = internal_state
                .pseudo_terminal
                .resize(UVec2::new(cols, rows_count));

            // Update all row nodes to have the new grid template columns
            let target_grid: Vec<RepeatedGridTrack> =
                vec![RepeatedGridTrack::px(cols as u16, 10.0)];
            for row_entity in &internal_state.rows {
                if let Ok(node) = node_query.get(*row_entity) {
                    if node.grid_template_columns != target_grid {
                        if let Ok(mut node_mut) = node_query.get_mut(*row_entity) {
                            node_mut.grid_template_columns = target_grid.clone();
                        }
                    }
                }
            }
        }
    }

    for (terminal_entity, mut internal_state) in query.iter_mut() {
        let InternalTerminalState {
            cells,
            rows,
            reader,
            writer,
            state,
            scroll_offset,
            saved_cursor,
            viewport_height,
            ..
        } = &mut *internal_state;

        let viewport_height = *viewport_height;

        let mut check_scroll = |state: &mut TerminalState,
                                cells: &mut Vec<Entity>,
                                rows: &mut Vec<Entity>,
                                commands: &mut Commands| {
            while state.cursor_position.y >= 1000 {
                cells.rotate_left(state.size.x as usize);
                let last_row_start = ((state.size.y - 1) * state.size.x) as usize;
                (last_row_start..cells.len()).for_each(|i| {
                    cells[i] = Entity::PLACEHOLDER;
                });
                let row = rows.remove(0);
                commands.entity(row).despawn_children();
                commands.entity(terminal_entity).add_child(row);
                rows.push(row);
                state.cursor_position.y -= 1;
            }
        };

        for event in reader.try_iter() {
            let current_screen_top =
                (state.cursor_position.y as i32 - (viewport_height as i32 - 1)).max(0) as u32;

            info!("VTE Event: {:?}", event);
            match event {
                VteEvent::Echo(character) => {
                    info!(
                        "Echoing: {:?} (U+{:04X}) at {:?}",
                        character, character as u32, state.cursor_position
                    );
                    if character == '\t' {
                        let next_tab = (state.cursor_position.x / 8 + 1) * 8;
                        state.move_right(next_tab - state.cursor_position.x);
                        check_scroll(state, cells, rows, &mut commands);
                        continue;
                    }

                    check_scroll(state, cells, rows, &mut commands);

                    let Some(cell_entity) = cells.get_mut(state.cursor_offset()) else {
                        continue;
                    };

                    if *cell_entity == Entity::PLACEHOLDER {
                        let row_entity = rows[state.cursor_position.y as usize];
                        commands.entity(row_entity).with_children(|builder| {
                            let terminal_fonts = terminal_fonts.get(terminal_entity).unwrap();
                            *cell_entity = new_cell(state, terminal_fonts, builder, character);
                        });
                    } else {
                        let terminal_fonts = terminal_fonts.get(terminal_entity).unwrap();
                        set_cell(
                            state,
                            terminal_fonts,
                            character,
                            *cell_entity,
                            &mut cell_query,
                            &mut commands,
                        );
                    }

                    state.move_right(1);
                }
                VteEvent::Backspace => {
                    let old_y = state.cursor_position.y;
                    state.cursor_position.x = state.cursor_position.x.saturating_sub(1);
                    let row_entity = rows[old_y as usize];
                    let Some(entity) = cells.get_mut(state.cursor_offset()) else {
                        continue;
                    };
                    let entity = mem::replace(entity, Entity::PLACEHOLDER);
                    if entity != Entity::PLACEHOLDER {
                        commands.entity(row_entity).detach_children(&[entity]);
                        commands.entity(entity).despawn();
                    }
                }

                VteEvent::Goto(new_position) => {
                    // Position is 1-based and relative to screen top
                    let x = (new_position.x).saturating_sub(1).min(state.size.x - 1);
                    let y = current_screen_top + (new_position.y).saturating_sub(1);
                    state.goto(UVec2::new(x, y));
                    check_scroll(state, cells, rows, &mut commands);
                }
                VteEvent::GotoX(x) => {
                    state.cursor_position.x = x.min(state.size.x - 1);
                }
                VteEvent::GotoY(y) => {
                    state.cursor_position.y = current_screen_top + y.saturating_sub(1);
                    check_scroll(state, cells, rows, &mut commands);
                }

                VteEvent::SaveCursorPosition => {
                    info!("Saving cursor position: {:?}", state.cursor_position);
                    *saved_cursor = Some(state.cursor_position);
                }
                VteEvent::RestoreCursorPosition => {
                    if let Some(pos) = *saved_cursor {
                        info!("Restoring cursor position to: {:?}", pos);
                        state.cursor_position = pos;
                        check_scroll(state, cells, rows, &mut commands);
                    }
                }

                VteEvent::LineUp(rows_to_move) => state.line_up(rows_to_move),
                VteEvent::LineDown(rows_to_move) => {
                    state.move_down(rows_to_move);
                    state.goto_x(0);
                    check_scroll(state, cells, rows, &mut commands);
                }

                VteEvent::MoveUp(rows_to_move) => state.move_up(rows_to_move),
                VteEvent::MoveDown(rows_to_move) => {
                    state.move_down(rows_to_move);
                    check_scroll(state, cells, rows, &mut commands);
                }
                VteEvent::MoveLeft(columns) => {
                    state.cursor_position.x = state.cursor_position.x.saturating_sub(columns);
                }
                VteEvent::MoveRight(columns) => {
                    state.move_right(columns);
                    check_scroll(state, cells, rows, &mut commands);
                }

                VteEvent::Image(image) => {
                    let Ok(image_data) =
                        base64::Engine::decode(&base64::engine::general_purpose::STANDARD, image)
                    else {
                        continue;
                    };

                    let mut reader = image::ImageReader::new(std::io::Cursor::new(image_data));
                    let Ok(reader) = reader.with_guessed_format() else {
                        continue;
                    };
                    let Ok(image) = reader.decode() else {
                        continue;
                    };

                    let image_handle = images.add(Image::from_dynamic(
                        image,
                        true,
                        RenderAssetUsages::MAIN_WORLD | RenderAssetUsages::RENDER_WORLD,
                    ));

                    let (x, y) = (state.cursor_position.x, state.cursor_position.y);

                    // Clear a 10x10 area for the image
                    for j in y..(y + 10).min(state.size.y) {
                        let row_ent = rows[j as usize];
                        for i in x..(x + 10).min(state.size.x) {
                            if let Some(entity) = cells.get_mut((j * state.size.x + i) as usize) {
                                let entity = mem::replace(entity, Entity::PLACEHOLDER);
                                if entity != Entity::PLACEHOLDER {
                                    commands.entity(row_ent).detach_children(&[entity]);
                                    commands.entity(entity).despawn();
                                }
                            }
                        }
                    }

                    let row_entity = rows[state.cursor_position.y as usize];

                    commands.entity(row_entity).with_children(|builder| {
                        let grid_column =
                            GridPlacement::start((state.cursor_position.x + 1) as i16);

                        let cell_entity = builder
                            .spawn((
                                Node {
                                    grid_column,
                                    width: Val::Px(100.0),
                                    height: Val::Px(100.0),
                                    ..default()
                                },
                                TerminalCell,
                            ))
                            .with_children(|builder| {
                                builder.spawn(ImageNode::new(image_handle));
                            })
                            .id();

                        if let Some(c) = cells.get_mut(state.cursor_offset()) {
                            *c = cell_entity;
                        }
                    });

                    state.move_right(10);
                    check_scroll(state, cells, rows, &mut commands);
                }
                VteEvent::ReportCursorPosition => {
                    let row_in_view =
                        state.cursor_position.y as i32 - current_screen_top as i32 + 1;
                    let col_in_view = state.cursor_position.x + 1;
                    let response = format!("\x1b[{row_in_view};{col_in_view}R");
                    info!("Reporting cursor position: {:?}", response);
                    writer.send(response.into()).unwrap();
                }

                VteEvent::Reset => {
                    info!("VTE Reset state");
                    state.reset();
                }

                VteEvent::Bold => state.set_bold(),
                VteEvent::Italic => state.set_italic(),
                VteEvent::Foreground(color) => {
                    let color = match color {
                        AnsiColor::Standard(StandardColor { color, intensity }) => {
                            let idx =
                                color as usize + if intensity == Intensity::Bright { 8 } else { 0 };
                            TABLE[idx]
                        }
                        AnsiColor::Index(index) => index_to_color(index),
                        AnsiColor::Rgb(r, g, b) => Srgba::rgb_u8(r, g, b),
                    };
                    state.style.foreground = color.into();
                }
                VteEvent::ResetForeground => {
                    state.style.foreground = TerminalStyle::default().foreground;
                }
                VteEvent::Background(color) => {
                    let color = match color {
                        AnsiColor::Standard(StandardColor { color, intensity }) => {
                            let idx =
                                color as usize + if intensity == Intensity::Bright { 8 } else { 0 };
                            TABLE[idx]
                        }
                        AnsiColor::Index(index) => index_to_color(index),
                        AnsiColor::Rgb(r, g, b) => Srgba::rgb_u8(r, g, b),
                    };
                    state.style.background = color.into();
                }
                VteEvent::ResetBackground => {
                    state.style.background = TerminalStyle::default().background;
                }

                VteEvent::ClearLeft => {
                    let (x, y) = (state.cursor_position.x, state.cursor_position.y);
                    let row_entity = rows[y as usize];
                    for i in 0..x {
                        if let Some(entity) = cells.get_mut((y * state.size.x + i) as usize) {
                            let entity = mem::replace(entity, Entity::PLACEHOLDER);
                            if entity != Entity::PLACEHOLDER {
                                commands.entity(row_entity).detach_children(&[entity]);
                                commands.entity(entity).despawn();
                            }
                        }
                    }
                }
                VteEvent::ClearRight => {
                    let (x, y) = (state.cursor_position.x, state.cursor_position.y);
                    let row_entity = rows[y as usize];
                    for i in x..state.size.x {
                        if let Some(entity) = cells.get_mut((y * state.size.x + i) as usize) {
                            let entity = mem::replace(entity, Entity::PLACEHOLDER);
                            if entity != Entity::PLACEHOLDER {
                                commands.entity(row_entity).detach_children(&[entity]);
                                commands.entity(entity).despawn();
                            }
                        }
                    }
                }
                VteEvent::ClearLine => {
                    let y = state.cursor_position.y;
                    let row_entity = rows[y as usize];
                    for i in 0..state.size.x {
                        if let Some(entity) = cells.get_mut((y * state.size.x + i) as usize) {
                            let entity = mem::replace(entity, Entity::PLACEHOLDER);
                            if entity != Entity::PLACEHOLDER {
                                commands.entity(row_entity).detach_children(&[entity]);
                                commands.entity(entity).despawn();
                            }
                        }
                    }
                }
                VteEvent::ClearUp => {
                    let (x, y) = (state.cursor_position.x, state.cursor_position.y);
                    // Clear all rows above
                    for row_idx in 0..y {
                        let row_ent = rows[row_idx as usize];
                        for i in 0..state.size.x {
                            if let Some(entity) =
                                cells.get_mut((row_idx * state.size.x + i) as usize)
                            {
                                let entity = mem::replace(entity, Entity::PLACEHOLDER);
                                if entity != Entity::PLACEHOLDER {
                                    commands.entity(row_ent).detach_children(&[entity]);
                                    commands.entity(entity).despawn();
                                }
                            }
                        }
                    }
                    // Clear left on current line
                    let row_ent = rows[y as usize];
                    for i in 0..x {
                        if let Some(entity) = cells.get_mut((y * state.size.x + i) as usize) {
                            let entity = mem::replace(entity, Entity::PLACEHOLDER);
                            if entity != Entity::PLACEHOLDER {
                                commands.entity(row_ent).detach_children(&[entity]);
                                commands.entity(entity).despawn();
                            }
                        }
                    }
                }
                VteEvent::ClearDown => {
                    let (x, y) = (state.cursor_position.x, state.cursor_position.y);
                    // Clear right on current line
                    let row_ent = rows[y as usize];
                    for i in x..state.size.x {
                        if let Some(entity) = cells.get_mut((y * state.size.x + i) as usize) {
                            let entity = mem::replace(entity, Entity::PLACEHOLDER);
                            if entity != Entity::PLACEHOLDER {
                                commands.entity(row_ent).detach_children(&[entity]);
                                commands.entity(entity).despawn();
                            }
                        }
                    }
                    // Clear all rows below
                    for row_idx in (y + 1)..state.size.y {
                        let row_ent = rows[row_idx as usize];
                        for i in 0..state.size.x {
                            if let Some(entity) =
                                cells.get_mut((row_idx * state.size.x + i) as usize)
                            {
                                let entity = mem::replace(entity, Entity::PLACEHOLDER);
                                if entity != Entity::PLACEHOLDER {
                                    commands.entity(row_ent).detach_children(&[entity]);
                                    commands.entity(entity).despawn();
                                }
                            }
                        }
                    }
                }
                VteEvent::ClearAll | VteEvent::ClearEverything => {
                    for row_idx in 0..state.size.y {
                        let row_ent = rows[row_idx as usize];
                        for i in 0..state.size.x {
                            if let Some(entity) =
                                cells.get_mut((row_idx * state.size.x + i) as usize)
                            {
                                let entity = mem::replace(entity, Entity::PLACEHOLDER);
                                if entity != Entity::PLACEHOLDER {
                                    commands.entity(row_ent).detach_children(&[entity]);
                                    commands.entity(entity).despawn();
                                }
                            }
                        }
                    }
                    state.goto(UVec2::new(0, current_screen_top));
                }
                _ => {}
            }
        }

        let screen_top =
            (state.cursor_position.y as i32 - (viewport_height as i32 - 1)).max(0) as u32;

        // Handle mouse wheel scrolling
        for event in mouse_wheel_input.read() {
            let dy = event.y;
            *scroll_offset = (*scroll_offset + dy as i32).clamp(0, screen_top as i32);
        }

        // Update row visibility based on viewport and scroll offset
        let view_top = (screen_top as i32 - *scroll_offset).max(0);
        let view_bottom = view_top + (viewport_height as i32 - 1);

        for (i, row_entity) in rows.iter().enumerate() {
            let target_display = if i as i32 >= view_top && i as i32 <= view_bottom {
                Display::Grid
            } else {
                Display::None
            };

            if let Ok(node) = node_query.get(*row_entity) {
                if node.display != target_display {
                    if let Ok(mut node_mut) = node_query.get_mut(*row_entity) {
                        node_mut.display = target_display;
                    }
                }
            }
        }

        if touch_input.any_just_pressed() {
            debug!("process touch event: {touch_input:?}");

            #[cfg(target_os = "android")]
            bevy::window::ANDROID_APP
                .get()
                .unwrap()
                .show_soft_input(true);
        }

        for event in keyboard_input.read() {
            info!("Keyboard Event: {:?}", event);

            if !event.state.is_pressed() {
                continue;
            }

            if let Some(string) = convert::convert_key(&event.logical_key) {
                info!("Sending to writer: {:?}", string);
                writer.send(string).unwrap();
            }
        }
    }
}

fn new_cell(
    terminal_state: &mut TerminalState,
    terminal_fonts: &TerminalFonts,
    builder: &mut ChildSpawnerCommands<'_>,
    character: char,
) -> Entity {
    let grid_column = GridPlacement::start((terminal_state.cursor_position.x + 1) as i16);

    let font = match (terminal_state.style.bold, terminal_state.style.italic) {
        (true, true) => &terminal_fonts.bold_italic,
        (true, false) => &terminal_fonts.bold,
        (false, true) => &terminal_fonts.regular_italic,
        (false, false) => &terminal_fonts.regular,
    };

    builder
        .spawn((
            BackgroundColor(terminal_state.style.background),
            Node {
                grid_column,
                ..default()
            },
            TerminalCell,
            Text::new(character),
            TextLayout::default(),
            TextColor(if terminal_state.style.foreground == Color::NONE {
                Color::WHITE
            } else {
                terminal_state.style.foreground
            }),
            TextFont {
                font: font.clone(),
                font_size: 14.0,
                ..default()
            },
        ))
        .id()
}

fn set_cell(
    terminal_state: &mut TerminalState,
    terminal_fonts: &TerminalFonts,
    character: char,
    cell_entity: Entity,
    cell_query: &mut Query<(
        &mut BackgroundColor,
        &mut Text,
        &mut TextColor,
        &mut TextFont,
        Option<&mut TextLayout>,
    )>,
    commands: &mut Commands,
) {
    let result = cell_query.get_mut(cell_entity);

    if let Ok((mut background_color, mut text, mut text_color, mut text_font, layout)) = result {
        let font = match (terminal_state.style.bold, terminal_state.style.italic) {
            (true, true) => &terminal_fonts.bold_italic,
            (true, false) => &terminal_fonts.bold,
            (false, true) => &terminal_fonts.regular_italic,
            (false, false) => &terminal_fonts.regular,
        };

        background_color.0 = terminal_state.style.background;
        text.0 = character.to_string();
        text_color.0 = if terminal_state.style.foreground == Color::NONE {
            Color::WHITE
        } else {
            terminal_state.style.foreground
        };
        *text_font = TextFont {
            font: font.clone(),
            font_size: 14.0,
            ..default()
        };
        if layout.is_none() {
            commands.entity(cell_entity).insert(TextLayout::default());
        }
    }
}
