use bevy::math::UVec2;
use compact_str::CompactString;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum NamedColor {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Intensity {
    Normal,
    Dim,
    Bright,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct StandardColor {
    pub color: NamedColor,
    pub intensity: Intensity,
}

impl NamedColor {
    pub fn new(color: u16) -> Option<Self> {
        let color = match color {
            0 => Self::Black,
            1 => Self::Red,
            2 => Self::Green,
            3 => Self::Yellow,
            4 => Self::Blue,
            5 => Self::Magenta,
            6 => Self::Cyan,
            7 => Self::White,
            _ => return None,
        };

        Some(color)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AnsiColor {
    Standard(StandardColor),
    Index(u8),
    Rgb(u8, u8, u8),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum VteEvent {
    Echo(char),
    Backspace,
    Goto(UVec2),
    GotoX(u32),
    GotoY(u32),
    LineUp(u32),
    LineDown(u32),
    MoveUp(u32),
    MoveDown(u32),
    MoveLeft(u32),
    MoveRight(u32),
    SaveCursorPosition,
    RestoreCursorPosition,
    EnableAlternativeBuffer,
    DisableAlternativeBuffer,
    EnableBracketedPaste,
    DisableBracketedPaste,
    ReportCursorPosition,
    Reset,
    Bold,
    Dim,
    Italic,
    Underline,
    Foreground(AnsiColor),
    ResetForeground,
    Background(AnsiColor),
    ResetBackground,
    SetTitle(CompactString),
    RemoveTitle,
    Image(CompactString),
    ClearLeft,
    ClearRight,
    ClearLine,
    ClearUp,
    ClearDown,
    ClearAll,
    ClearEverything,
}

pub trait VteHandler {
    fn vte_event(&mut self, event: VteEvent) {
        let _event = event;
    }
}

struct Performer<T: VteHandler> {
    state: T,
    is_alt_charset: bool,
}

pub struct Vte<T: VteHandler> {
    parser: vte::Parser<1024>,
    performer: Performer<T>,
}

impl<T: VteHandler> Vte<T> {
    pub fn new(handler: T) -> Self {
        let parser = vte::Parser::new();
        let performer = Performer::new(handler);

        Self { parser, performer }
    }

    pub fn process(&mut self, bytes: &[u8]) {
        self.parser.advance(&mut self.performer, bytes);
    }
}

impl<T: VteHandler> Performer<T> {
    pub fn new(state: T) -> Self {
        Self {
            state,
            is_alt_charset: false,
        }
    }

    pub fn sgr_flat(&mut self, params: Vec<u16>) {
        if params.is_empty() {
            self.state.vte_event(VteEvent::Reset);
            return;
        }

        let mut i = 0;
        while i < params.len() {
            let param = params[i];
            match param {
                0 => self.state.vte_event(VteEvent::Reset),

                1 => self.state.vte_event(VteEvent::Bold),
                2 => self.state.vte_event(VteEvent::Dim),
                3 => self.state.vte_event(VteEvent::Italic),
                4 => self.state.vte_event(VteEvent::Underline),

                30..=37 => {
                    if let Some(color) = NamedColor::new(param - 30) {
                        self.state
                            .vte_event(VteEvent::Foreground(AnsiColor::Standard(StandardColor {
                                color,
                                intensity: Intensity::Normal,
                            })));
                    }
                }

                38 => {
                    i += 1;
                    if let Some(mode) = params.get(i) {
                        match mode {
                            5 => {
                                i += 1;
                                if let Some(index) = params.get(i) {
                                    self.state.vte_event(VteEvent::Foreground(AnsiColor::Index(
                                        *index as u8,
                                    )));
                                }
                            }
                            2 => {
                                i += 1;
                                let r = params.get(i).copied().unwrap_or(0);
                                i += 1;
                                let g = params.get(i).copied().unwrap_or(0);
                                i += 1;
                                let b = params.get(i).copied().unwrap_or(0);
                                self.state.vte_event(VteEvent::Foreground(AnsiColor::Rgb(
                                    r as u8, g as u8, b as u8,
                                )));
                            }
                            _ => {}
                        }
                    }
                }

                39 => self.state.vte_event(VteEvent::ResetForeground),

                40..=47 => {
                    if let Some(color) = NamedColor::new(param - 40) {
                        self.state
                            .vte_event(VteEvent::Background(AnsiColor::Standard(StandardColor {
                                color,
                                intensity: Intensity::Normal,
                            })));
                    }
                }

                48 => {
                    i += 1;
                    if let Some(mode) = params.get(i) {
                        match mode {
                            5 => {
                                i += 1;
                                if let Some(index) = params.get(i) {
                                    self.state.vte_event(VteEvent::Background(AnsiColor::Index(
                                        *index as u8,
                                    )));
                                }
                            }
                            2 => {
                                i += 1;
                                let r = params.get(i).copied().unwrap_or(0);
                                i += 1;
                                let g = params.get(i).copied().unwrap_or(0);
                                i += 1;
                                let b = params.get(i).copied().unwrap_or(0);
                                self.state.vte_event(VteEvent::Background(AnsiColor::Rgb(
                                    r as u8, g as u8, b as u8,
                                )));
                            }
                            _ => {}
                        }
                    }
                }

                49 => self.state.vte_event(VteEvent::ResetBackground),

                90..=97 => {
                    if let Some(color) = NamedColor::new(param - 90) {
                        self.state
                            .vte_event(VteEvent::Foreground(AnsiColor::Standard(StandardColor {
                                color,
                                intensity: Intensity::Bright,
                            })));
                    }
                }

                100..=107 => {
                    if let Some(color) = NamedColor::new(param - 100) {
                        self.state
                            .vte_event(VteEvent::Background(AnsiColor::Standard(StandardColor {
                                color,
                                intensity: Intensity::Bright,
                            })));
                    }
                }

                _ => {
                    bevy::prelude::info!("ignored SGR: {param}");
                }
            }
            i += 1;
        }
    }
}

impl<T: VteHandler> vte::Perform for Performer<T> {
    fn print(&mut self, character: char) {
        let character = if self.is_alt_charset {
            match character {
                '_' => ' ',
                'a' => '▒',
                'b' => '␉',
                'c' => '␌',
                'd' => '␍',
                'e' => '␊',
                'f' => '°',
                'g' => '±',
                'h' => '␤',
                'i' => '␋',
                'j' => '┘',
                'k' => '┐',
                'l' => '┌',
                'm' => '└',
                'n' => '┼',
                'o' => '⎺',
                'p' => '⎻',
                'q' => '─',
                'r' => '⎼',
                's' => '⎽',
                't' => '├',
                'u' => '┤',
                'v' => '┴',
                'w' => '┬',
                'x' => '│',
                'y' => '≤',
                'z' => '≥',
                '{' => 'π',
                '|' => '≠',
                '}' => '£',
                '~' => '·',
                _ => character,
            }
        } else {
            // Comprehensive fallback for box drawing if font support is questionable
            match character {
                '╭' => '┌',
                '╮' => '┐',
                '╯' => '┘',
                '╰' => '└',
                // If they still don't show, maybe mapping to very common ones helps
                '━' => '─',
                '┃' => '│',
                '┍' => '┌',
                '┑' => '┐',
                '┕' => '└',
                '┙' => '┘',
                _ => character,
            }
        };
        self.state.vte_event(VteEvent::Echo(character));
    }

    fn execute(&mut self, byte: u8) {
        match byte {
            b'\t' => self.state.vte_event(VteEvent::Echo('\t')),
            b'\x08' => self.state.vte_event(VteEvent::Backspace),
            b'\r' => self.state.vte_event(VteEvent::GotoX(0)),
            b'\n' | b'\x0b' | b'\x0c' => self.state.vte_event(VteEvent::MoveDown(1)),
            0x0E => self.is_alt_charset = true,
            0x0F => self.is_alt_charset = false,
            _ => {
                bevy::prelude::info!("VTE execute: 0x{byte:02x}");
            }
        }
    }

    fn osc_dispatch(&mut self, params: &[&[u8]], _bell_terminated: bool) {
        let Some(param) = params.first() else {
            return;
        };

        match *param {
            b"0" => match params.get(1) {
                Some(title) => self
                    .state
                    .vte_event(VteEvent::SetTitle(CompactString::from_utf8_lossy(title))),
                None => self.state.vte_event(VteEvent::RemoveTitle),
            },
            b"1337" => {
                let Some(image) = params.last() else {
                    return;
                };

                let Some(image) = image.split(|byte| *byte == b':').next_back() else {
                    return;
                };

                self.state
                    .vte_event(VteEvent::Image(CompactString::from_utf8_lossy(image)));
            }
            _ => {
                bevy::prelude::info!("VTE OSC: {params:?}");
            }
        }
    }

    fn esc_dispatch(&mut self, intermediates: &[u8], _ignore: bool, byte: u8) {
        bevy::prelude::info!("VTE ESC: intermediates={intermediates:?} byte=0x{byte:02x}");
        match (intermediates.first(), byte) {
            (None, b'7') => self.state.vte_event(VteEvent::SaveCursorPosition),
            (None, b'8') => self.state.vte_event(VteEvent::RestoreCursorPosition),
            (Some(b'('), b'0') => self.is_alt_charset = true,
            (Some(b'('), b'B') => self.is_alt_charset = false,
            (Some(b')'), b'0') => self.is_alt_charset = true,
            (Some(b')'), b'B') => self.is_alt_charset = false,
            _ => {}
        }
    }

    fn csi_dispatch(
        &mut self,
        params: &vte::Params,
        intermediates: &[u8],
        _ignore: bool,
        action: char,
    ) {
        bevy::prelude::info!(
            "CSI: action={action:?} params={params:?} intermediates={intermediates:?}"
        );

        let mut flat_params = Vec::new();
        for p in params.iter() {
            for &subp in p {
                flat_params.push(subp);
            }
        }
        let mut iter = flat_params.into_iter();

        match action {
            'A' => self.state.vte_event(VteEvent::MoveUp(next_axis(&mut iter))),
            'B' => self
                .state
                .vte_event(VteEvent::MoveDown(next_axis(&mut iter))),
            'C' => self
                .state
                .vte_event(VteEvent::MoveRight(next_axis(&mut iter))),
            'D' => self
                .state
                .vte_event(VteEvent::MoveLeft(next_axis(&mut iter))),

            'E' => self
                .state
                .vte_event(VteEvent::LineDown(next_axis(&mut iter))),
            'F' => self.state.vte_event(VteEvent::LineUp(next_axis(&mut iter))),

            'G' => self
                .state
                .vte_event(VteEvent::GotoX(next_axis(&mut iter) - 1)),
            'H' | 'f' => self
                .state
                .vte_event(VteEvent::Goto(next_position(&mut iter))),

            'm' => {
                let p_vec = iter.collect();
                self.sgr_flat(p_vec);
            }
            'n' => {
                if let Some(6) = next(&mut iter) {
                    self.state.vte_event(VteEvent::ReportCursorPosition)
                }
            }

            'J' => match next(&mut iter) {
                Some(0) | None => self.state.vte_event(VteEvent::ClearDown),
                Some(1) => self.state.vte_event(VteEvent::ClearUp),
                Some(2) => self.state.vte_event(VteEvent::ClearAll),
                Some(3) => self.state.vte_event(VteEvent::ClearEverything),
                _ => {}
            },

            'K' => match next(&mut iter) {
                Some(0) | None => self.state.vte_event(VteEvent::ClearRight),
                Some(1) => self.state.vte_event(VteEvent::ClearLeft),
                Some(2) => self.state.vte_event(VteEvent::ClearLine),
                _ => {}
            },

            's' => self.state.vte_event(VteEvent::SaveCursorPosition),
            'u' => self.state.vte_event(VteEvent::RestoreCursorPosition),
            'h' | 'l' => {
                bevy::prelude::info!("Private mode: {action} with params {params:?}");
            }
            _ => {
                bevy::prelude::info!(
                    "uncaught CSI: \\x1b[{}{action}",
                    params
                        .iter()
                        .map(|p| p
                            .iter()
                            .map(|v| v.to_string())
                            .collect::<Vec<_>>()
                            .join(":"))
                        .collect::<Vec<_>>()
                        .join(";")
                );
            }
        }
    }
}

fn next(iter: &mut impl Iterator<Item = u16>) -> Option<u16> {
    iter.next()
}

fn next_axis(iter: &mut impl Iterator<Item = u16>) -> u32 {
    next(iter).unwrap_or(1).max(1).into()
}

fn next_position(iter: &mut impl Iterator<Item = u16>) -> UVec2 {
    let y = next_axis(iter);
    let x = next_axis(iter);

    UVec2::new(x, y)
}
