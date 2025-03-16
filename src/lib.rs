use self::vte::{Vte, VteEvent};
use bevy::asset::{embedded_asset, RenderAssetUsages};
use bevy::color::palettes::basic;
use bevy::color::Gray;
use bevy::input::keyboard::KeyboardInput;
use bevy::prelude::*;
use bevy::render::camera::RenderTarget;
use bevy::render::render_resource::{Extent3d, TextureDimension, TextureFormat, TextureUsages};
use bevy::render::view::RenderLayers;
use bevy::window::CompositeAlphaMode;
use compact_str::CompactString;
use config::TerminalConfig;
use crossbeam_channel::{Receiver, Sender};
use pseudo_terminal::PseudoTerminal;
use std::io::{Read, Write};
use std::process::Command;
use std::{io, mem, thread};
use vte::{Intensity, NamedColor, StandardColor};

mod config;
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

#[derive(Clone, Copy, Debug, Default)]
struct TerminalState {
    cursor_position: UVec2,
    style: TerminalStyle,
}

impl TerminalState {
    pub fn cursor_position(&self) -> UVec2 {
        self.cursor_position
    }

    pub fn cursor_offset(&self, width: u32) -> usize {
        ((self.cursor_position.y * width) + self.cursor_position.x) as usize
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

        self.cursor_position.x = self.cursor_position.x.saturating_sub(columns);
    }

    pub fn move_right(&mut self, columns: u32) {
        debug_assert!(columns >= 1);

        self.cursor_position.x = self.cursor_position.x.saturating_add(columns);
    }

    pub fn goto(&mut self, position: UVec2) {
        self.cursor_position = position;
    }

    pub fn goto_x(&mut self, x: u32) {
        self.cursor_position.x = x;
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
    pseudo_terminal: PseudoTerminal,
    writer: Sender<CompactString>,
    reader: Receiver<VteEvent>,
    state: TerminalState,
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
    let terminal_config = TerminalConfig::read().unwrap();
    let alpha = terminal_config.window.opacity.unwrap_or(1.0);

    App::new()
        .insert_resource(ClearColor(Color::srgba(0.0, 0.0, 0.0, alpha)))
        .add_plugins((
            DefaultPlugins.set(WindowPlugin {
                primary_window: Some(Window {
                    #[cfg(target_os = "android")]
                    resizable: false,
                    transparent: terminal_config.window.opacity.is_some(),
                    composite_alpha_mode: CompositeAlphaMode::PreMultiplied,
                    #[cfg(target_os = "android")]
                    mode: WindowMode::BorderlessFullscreen(MonitorSelection::Primary),
                    recognize_rotation_gesture: true,
                    ..default()
                }),
                ..default()
            }),
            TerminalPlugin,
        ))
        .insert_resource(terminal_config)
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
            display: Display::Grid,
            grid_template_columns: RepeatedGridTrack::px(400, 10.0),
            grid_template_rows: RepeatedGridTrack::px(100, 18.0),
            height: Val::Percent(1000.0),
            width: Val::Percent(1000.0),
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
        let mut pseudo_terminal = PseudoTerminal::new(UVec2::new(400, 100)).unwrap();

        pseudo_terminal.configure_command(&mut command.0).unwrap();

        let reader = {
            let (sender, receiver) = crossbeam_channel::unbounded::<VteEvent>();
            let mut control = pseudo_terminal.control.clone();

            thread::spawn(move || {
                let mut vte = Vte::new(Handler(sender));
                let mut buf = [0; 1024];

                while let Ok(amount) = control.read(&mut buf) {
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
                    control.write_all(text.as_bytes())?;
                }

                io::Result::Ok(())
            });

            sender
        };

        let result = command.0.spawn();

        debug!("spawn command: {command:?} {result:?}");

        let internal_terminal_state = InternalTerminalState {
            cells: vec![Entity::PLACEHOLDER; 400 * 100],
            pseudo_terminal,
            reader,
            writer,
            state: default(),
        };

        commands.entity(entity).insert(internal_terminal_state);
    }
}

#[derive(Clone, Copy, Component, Debug, Default, Eq, PartialEq, Reflect)]
pub struct Cube;

static TABLE: [Srgba; 8] = [
    basic::BLACK,
    basic::RED,
    basic::GREEN,
    basic::YELLOW,
    basic::BLUE,
    basic::FUCHSIA,
    basic::AQUA,
    basic::WHITE,
];

fn update(
    mut commands: Commands,
    mut images: ResMut<Assets<Image>>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    mut keyboard_input: EventReader<KeyboardInput>,
    mut query: Query<(Entity, &mut InternalTerminalState)>,
    terminal_fonts: Query<&TerminalFonts>,
    touch_input: Res<Touches>,
    mut cell_query: Query<(
        &mut BackgroundColor,
        &mut Text,
        &mut TextColor,
        &mut TextFont,
    )>,
) {
    for (entity, mut state) in query.iter_mut() {
        let InternalTerminalState {
            cells,
            reader,
            writer,
            state,
            ..
        } = &mut *state;

        for event in reader.try_iter() {
            debug!("process vte event: {event:?}");

            match event {
                VteEvent::Echo(character) => {
                    let Some(cell_entity) = cells.get_mut(state.cursor_offset(400)) else {
                        continue;
                    };

                    if *cell_entity == Entity::PLACEHOLDER {
                        commands.entity(entity).with_children(|builder| {
                            let terminal_fonts =
                                terminal_fonts.get(builder.parent_entity()).unwrap();

                            *cell_entity = new_cell(state, terminal_fonts, builder, character);
                        });
                    } else {
                        let terminal_fonts = terminal_fonts.get(entity).unwrap();

                        set_cell(
                            state,
                            terminal_fonts,
                            character,
                            *cell_entity,
                            &mut cell_query,
                        );
                    }

                    state.move_right(1);
                }
                VteEvent::Backspace => {
                    state.move_left(1);

                    let Some(entity) = cells.get_mut(state.cursor_offset(400)) else {
                        continue;
                    };

                    let entity = mem::replace(entity, Entity::PLACEHOLDER);

                    if entity != Entity::PLACEHOLDER {
                        commands.entity(entity).despawn_recursive();
                    }
                }

                VteEvent::Goto(new_position) => state.goto(new_position),
                VteEvent::GotoX(x) => state.goto_x(x),
                VteEvent::GotoY(y) => state.goto_y(y),

                VteEvent::LineUp(rows) => state.line_up(rows),
                VteEvent::LineDown(rows) => state.line_down(rows),

                VteEvent::MoveUp(rows) => state.move_up(rows),
                VteEvent::MoveDown(rows) => state.move_down(rows),
                VteEvent::MoveLeft(columns) => state.move_left(columns),
                VteEvent::MoveRight(columns) => state.move_right(columns),

                VteEvent::Reset => state.reset(),
                VteEvent::Bold => state.set_bold(),
                VteEvent::Italic => state.set_italic(),
                VteEvent::Foreground(color) => {
                    info!("{color:?}");
                    if let vte::AnsiColor::Standard(StandardColor { color, intensity }) = color {
                        let mut color = if color == NamedColor::Black {
                            Srgba::gray(0.5)
                        } else {
                            TABLE[color as usize]
                        };

                        match intensity {
                            Intensity::Dim => color = color.darker(0.1),
                            Intensity::Bright => color = color.lighter(0.1),
                            _ => {}
                        };

                        state.style.foreground = color.into();
                    }
                }
                VteEvent::ResetForeground => {
                    state.style.foreground = TerminalStyle::default().foreground;
                }
                VteEvent::Background(color) => {
                    info!("{color:?}");
                    if let vte::AnsiColor::Standard(StandardColor { color, intensity }) = color {
                        let mut color = if color == NamedColor::Black {
                            Srgba::gray(0.5)
                        } else {
                            TABLE[color as usize]
                        };

                        match intensity {
                            Intensity::Dim => color = color.darker(0.1),
                            Intensity::Bright => color = color.lighter(0.1),
                            _ => {}
                        };

                        state.style.background = color.into();
                    }
                }
                VteEvent::ResetBackground => {
                    state.style.foreground = TerminalStyle::default().foreground;
                }
                VteEvent::Image(image) => {
                    let image =
                        base64::Engine::decode(&base64::engine::general_purpose::STANDARD, image)
                            .unwrap();

                    let mut reader = image::ImageReader::new(std::io::Cursor::new(image));

                    reader = reader.with_guessed_format().unwrap();

                    let image = reader.decode().unwrap();

                    let image_handle = images.add(Image::from_dynamic(
                        image,
                        true,
                        RenderAssetUsages::MAIN_WORLD | RenderAssetUsages::RENDER_WORLD,
                    ));

                    let (x, y) = state.cursor_position().into();

                    for i in x..(x + 10) {
                        for j in y..(y + 10) {
                            let Some(entity) = cells.get_mut(((j * 400) + i) as usize) else {
                                continue;
                            };

                            let entity = mem::replace(entity, Entity::PLACEHOLDER);

                            if entity != Entity::PLACEHOLDER {
                                commands.entity(entity).despawn_recursive();
                            }
                        }
                    }

                    let Some(cell_entity) = cells.get_mut(state.cursor_offset(400)) else {
                        continue;
                    };

                    {
                        let entity = mem::replace(cell_entity, Entity::PLACEHOLDER);

                        if entity != Entity::PLACEHOLDER {
                            commands.entity(entity).despawn_recursive();
                        }
                    }

                    commands.entity(entity).with_children(|builder| {
                        let [grid_column, grid_row] = (state.cursor_position() + UVec2::ONE)
                            .to_array()
                            .map(|axis| GridPlacement::start(axis as i16));

                        let size = Extent3d {
                            width: 100,
                            height: 100,
                            depth_or_array_layers: 1,
                        };

                        *cell_entity = builder
                            .spawn((
                                //BackgroundColor(state.style.background),
                                Node {
                                    grid_column,
                                    grid_row,
                                    width: Val::Px(100.0),
                                    height: Val::Px(100.0),
                                    ..default()
                                },
                                TerminalCell,
                            ))
                            .with_children(|builder| {
                                builder.spawn((
                                    Cube,
                                    Mesh3d(meshes.add(Cuboid::default())),
                                    MeshMaterial3d(materials.add(StandardMaterial {
                                        base_color: Color::WHITE,
                                        base_color_texture: Some(image_handle.clone()),
                                        emissive: Color::WHITE.into(),
                                        emissive_texture: Some(image_handle.clone()),
                                        ..default()
                                    })),
                                    RenderLayers::layer(1),
                                    Transform::from_xyz(0.0, 0.0, -100.0)
                                        .with_scale(Vec3::splat(50.0)),
                                ));

                                let mut image = Image::new_fill(
                                    size,
                                    TextureDimension::D2,
                                    &[0; 4],
                                    TextureFormat::Bgra8UnormSrgb,
                                    RenderAssetUsages::MAIN_WORLD | RenderAssetUsages::RENDER_WORLD,
                                );

                                image.texture_descriptor.usage = TextureUsages::TEXTURE_BINDING
                                    | TextureUsages::COPY_DST
                                    | TextureUsages::RENDER_ATTACHMENT;

                                let image_handle = images.add(image);

                                builder.spawn((
                                    Camera3d::default(),
                                    Camera {
                                        clear_color: ClearColorConfig::Custom(Color::NONE),
                                        order: -1,
                                        target: RenderTarget::Image(image_handle.clone()),
                                        ..default()
                                    },
                                    RenderLayers::layer(1),
                                    Transform::from_xyz(0.0, 0.0, 0.0),
                                    #[cfg(target_os = "android")]
                                    Msaa::Off,
                                ));

                                builder.spawn(UiImage::new(image_handle));
                            })
                            .id();
                    });

                    state.move_right(10);
                }
                VteEvent::ReportCursorPosition => {
                    let (column, row) = (state.cursor_position() + UVec2::ONE).into();

                    writer.send(format!("\x1b[{row};{column}R").into()).unwrap();
                }
                VteEvent::ClearLeft => {
                    let (x, y) = state.cursor_position().into();

                    for i in 0..x {
                        let Some(entity) = cells.get_mut(((y * 400) + i) as usize) else {
                            continue;
                        };

                        let entity = mem::replace(entity, Entity::PLACEHOLDER);

                        if entity != Entity::PLACEHOLDER {
                            commands.entity(entity).despawn_recursive();
                        }
                    }
                }
                VteEvent::ClearRight => {
                    let (x, y) = state.cursor_position().into();

                    for i in x..400 {
                        let Some(entity) = cells.get_mut(((y * 400) + i) as usize) else {
                            continue;
                        };

                        let entity = mem::replace(entity, Entity::PLACEHOLDER);

                        if entity != Entity::PLACEHOLDER {
                            commands.entity(entity).despawn_recursive();
                        }
                    }
                }
                VteEvent::ClearLine => {
                    let (_x, y) = state.cursor_position().into();

                    for i in 0..400 {
                        let Some(entity) = cells.get_mut(((y * 400) + i) as usize) else {
                            continue;
                        };

                        let entity = mem::replace(entity, Entity::PLACEHOLDER);

                        if entity != Entity::PLACEHOLDER {
                            commands.entity(entity).despawn_recursive();
                        }
                    }
                }
                VteEvent::ClearUp => {
                    let (x, y) = state.cursor_position().into();

                    for i in 0..x {
                        for j in 0..y {
                            let Some(entity) = cells.get_mut(((j * 400) + i) as usize) else {
                                continue;
                            };

                            let entity = mem::replace(entity, Entity::PLACEHOLDER);

                            if entity != Entity::PLACEHOLDER {
                                commands.entity(entity).despawn_recursive();
                            }
                        }
                    }
                }
                VteEvent::ClearDown => {
                    let (x, y) = state.cursor_position().into();

                    for i in x..400 {
                        for j in y..400 {
                            let Some(entity) = cells.get_mut(((j * 400) + i) as usize) else {
                                continue;
                            };

                            let entity = mem::replace(entity, Entity::PLACEHOLDER);

                            if entity != Entity::PLACEHOLDER {
                                commands.entity(entity).despawn_recursive();
                            }
                        }
                    }
                }
                VteEvent::ClearAll | VteEvent::ClearEverything => {
                    for i in 0..400 {
                        for j in 0..400 {
                            let Some(entity) = cells.get_mut(((j * 400) + i) as usize) else {
                                continue;
                            };

                            let entity = mem::replace(entity, Entity::PLACEHOLDER);

                            if entity != Entity::PLACEHOLDER {
                                commands.entity(entity).despawn_recursive();
                            }
                        }
                    }
                }
                _ => {}
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
            debug!("process keyboard event: {event:?}");

            if !event.state.is_pressed() {
                continue;
            }

            if let Some(string) = convert::convert_key(&event.logical_key) {
                writer.send(string).unwrap();
            }
        }
    }
}

fn new_cell(
    terminal_state: &mut TerminalState,
    terminal_fonts: &TerminalFonts,
    builder: &mut ChildBuilder<'_>,
    character: char,
) -> Entity {
    let [grid_column, grid_row] = (terminal_state.cursor_position() + UVec2::ONE)
        .to_array()
        .map(|axis| GridPlacement::start(axis as i16));

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
                grid_row,
                ..default()
            },
            TerminalCell,
            Text::new(character),
            TextColor(terminal_state.style.foreground),
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
    )>,
) {
    let Ok((mut background_color, mut text, mut text_color, mut text_font)) =
        cell_query.get_mut(cell_entity)
    else {
        return;
    };

    let font = match (terminal_state.style.bold, terminal_state.style.italic) {
        (true, true) => &terminal_fonts.bold_italic,
        (true, false) => &terminal_fonts.bold,
        (false, true) => &terminal_fonts.regular_italic,
        (false, false) => &terminal_fonts.regular,
    };

    background_color.0 = terminal_state.style.background;
    text.0 = character.to_string();
    text_color.0 = terminal_state.style.foreground;
    *text_font = TextFont {
        font: font.clone(),
        font_size: 14.0,
        ..default()
    };
}
