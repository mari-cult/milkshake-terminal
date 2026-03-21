use std::ffi::OsString;
use std::process::Command;

#[cfg(all(unix, not(target_os = "android")))]
use std::{
    env,
    ffi::{CStr, OsStr},
};

#[cfg(target_os = "android")]
fn program() -> OsString {
    "/system/bin/sh".into()
}

#[cfg(all(unix, not(target_os = "android")))]
fn program() -> OsString {
    if let Some(shell) = env::var_os("SHELL") {
        return shell;
    }

    unsafe {
        let entry = libc::getpwuid(libc::getuid());

        if !entry.is_null() {
            let bytes = CStr::from_ptr((*entry).pw_shell);

            return OsStr::from_encoded_bytes_unchecked(bytes.to_bytes()).into();
        }
    }

    "/bin/sh".into()
}

#[cfg(not(unix))]
fn program() -> OsString {
    "powershell.exe".into()
}

pub fn default() -> Command {
    Command::new(program())
}
