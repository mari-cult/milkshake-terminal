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
        Self { state }
    }

    pub fn sgr(&mut self, iter: &mut vte::ParamsIter<'_>) {
        let Some(param) = next(iter) else {
            return;
        };

        match param {
            0 => self.state.vte_event(VteEvent::Reset),

            1 => self.state.vte_event(VteEvent::Bold),
            2 => self.state.vte_event(VteEvent::Dim),
            3 => self.state.vte_event(VteEvent::Italic),
            4 => self.state.vte_event(VteEvent::Underline),

            30..=37 => self
                .state
                .vte_event(VteEvent::Foreground(AnsiColor::Standard(StandardColor {
                    color: NamedColor::new(param - 30).unwrap(),
                    intensity: Intensity::Normal,
                }))),

            39 => self.state.vte_event(VteEvent::ResetForeground),

            40..=47 => self
                .state
                .vte_event(VteEvent::Background(AnsiColor::Standard(StandardColor {
                    color: NamedColor::new(param - 40).unwrap(),
                    intensity: Intensity::Normal,
                }))),

            49 => self.state.vte_event(VteEvent::ResetBackground),

            90..=97 => self
                .state
                .vte_event(VteEvent::Foreground(AnsiColor::Standard(StandardColor {
                    color: NamedColor::new(param - 90).unwrap(),
                    intensity: Intensity::Normal,
                }))),

            100..=107 => self
                .state
                .vte_event(VteEvent::Background(AnsiColor::Standard(StandardColor {
                    color: NamedColor::new(param - 100).unwrap(),
                    intensity: Intensity::Normal,
                }))),

            _ => {
                bevy::prelude::error!("uncaught SGR: {param}");
            }
        }
    }
}

impl<T: VteHandler> vte::Perform for Performer<T> {
    fn print(&mut self, character: char) {
        self.state.vte_event(VteEvent::Echo(character));
    }

    fn execute(&mut self, byte: u8) {
        match byte {
            b'\t' => self.state.vte_event(VteEvent::Echo('\t')),
            b'\x08' => self.state.vte_event(VteEvent::Backspace),
            b'\r' => self.state.vte_event(VteEvent::GotoX(0)),
            b'\n' => self.state.vte_event(VteEvent::LineDown(1)),
            _ => {}
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

                let Some(image) = image.split(|byte| *byte == b':').last() else {
                    return;
                };

                self.state
                    .vte_event(VteEvent::Image(CompactString::from_utf8_lossy(image)));
            }
            _ => {}
        }
    }

    fn csi_dispatch(
        &mut self,
        params: &vte::Params,
        _intermediates: &[u8],
        _ignore: bool,
        action: char,
    ) {
        let iter = &mut params.iter();

        match action {
            'A' => self.state.vte_event(VteEvent::MoveUp(next_axis(iter))),
            'B' => self.state.vte_event(VteEvent::MoveDown(next_axis(iter))),
            'C' => self.state.vte_event(VteEvent::MoveRight(next_axis(iter))),
            'D' => self.state.vte_event(VteEvent::MoveLeft(next_axis(iter))),

            'E' => self.state.vte_event(VteEvent::LineDown(next_axis(iter))),
            'F' => self.state.vte_event(VteEvent::LineUp(next_axis(iter))),

            'G' => self.state.vte_event(VteEvent::GotoX(next_axis(iter) - 1)),
            'H' | 'f' => self.state.vte_event(VteEvent::Goto(next_position(iter))),

            'm' => self.sgr(iter),
            'n' => {
                if let Some(6) = next(iter) {
                    self.state.vte_event(VteEvent::ReportCursorPosition)
                }
            }

            'J' => match next(iter) {
                Some(0) | None => self.state.vte_event(VteEvent::ClearDown),
                Some(1) => self.state.vte_event(VteEvent::ClearUp),
                Some(2) => self.state.vte_event(VteEvent::ClearAll),
                Some(3) => self.state.vte_event(VteEvent::ClearEverything),
                _ => {}
            },

            'K' => match next(iter) {
                Some(0) | None => self.state.vte_event(VteEvent::ClearRight),
                Some(1) => self.state.vte_event(VteEvent::ClearLeft),
                Some(2) => self.state.vte_event(VteEvent::ClearLine),
                _ => {}
            },

            's' => self.state.vte_event(VteEvent::SaveCursorPosition),
            'u' => self.state.vte_event(VteEvent::RestoreCursorPosition),
            _ => {
                bevy::prelude::error!(
                    "uncaught CSI: \\x1b[{}{action}",
                    iter.map(|params| params
                        .iter()
                        .map(|param| param.to_string())
                        .collect::<Vec<_>>()
                        .join("@"))
                        .collect::<Vec<_>>()
                        .join(";")
                );
            }
        }
    }
}

fn next(iter: &mut vte::ParamsIter<'_>) -> Option<u16> {
    iter.next().and_then(|params| params.first().copied())
}

fn next_axis(iter: &mut vte::ParamsIter<'_>) -> u32 {
    next(iter).unwrap_or(1).max(1).into()
}

fn next_position(iter: &mut vte::ParamsIter<'_>) -> UVec2 {
    let y = next_axis(iter);
    let x = next_axis(iter);

    UVec2::new(x, y)
}
