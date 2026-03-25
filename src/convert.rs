use compact_str::CompactString;
use winit::keyboard::{Key, NamedKey};

pub fn convert_key(key: &Key<&str>, text: Option<&str>) -> Option<CompactString> {
    let string = match key {
        Key::Named(NamedKey::ArrowUp) => "\x1bOA",
        Key::Named(NamedKey::ArrowDown) => "\x1bOB",
        Key::Named(NamedKey::ArrowRight) => "\x1bOC",
        Key::Named(NamedKey::ArrowLeft) => "\x1bOD",
        Key::Named(NamedKey::Backspace) => "\x7f",
        Key::Named(NamedKey::Enter) => "\r",
        Key::Named(NamedKey::Tab) => "\t",
        Key::Character(character) => character,
        _ => text?,
    };

    Some(string.into())
}
