use bevy::input::keyboard::Key;
use compact_str::CompactString;

pub fn convert_key(key: &Key) -> Option<CompactString> {
    let string = match key {
        Key::Character(string) => string.as_str(),
        Key::Backspace => "\x7f",
        Key::Enter => "\r",
        Key::Space => " ",
        Key::ArrowUp => "\x1bOA",
        Key::ArrowDown => "\x1bOB",
        Key::ArrowRight => "\x1bOC",
        Key::ArrowLeft => "\x1bOD",
        _ => return None,
    };

    Some(string.into())
}
