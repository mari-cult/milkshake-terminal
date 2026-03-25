use std::fs::File;
use std::io;
use std::process::Command;

#[derive(Clone, Copy, Debug)]
pub struct GridSize {
    pub cols: u32,
    pub rows: u32,
    pub width: u32,
    pub height: u32,
}

pub struct PseudoTerminal {
    inner: platform::PlatformPty,
}

impl PseudoTerminal {
    pub fn new(size: GridSize) -> io::Result<Self> {
        Ok(Self {
            inner: platform::PlatformPty::new(size)?,
        })
    }

    pub fn spawn(&mut self, command: &mut Command) -> io::Result<()> {
        self.inner.spawn(command)
    }

    pub fn resize(&self, size: GridSize) -> io::Result<()> {
        self.inner.resize(size)
    }

    pub fn reader(&self) -> io::Result<File> {
        self.inner.reader()
    }

    pub fn writer(&self) -> io::Result<File> {
        self.inner.writer()
    }
}

#[cfg(unix)]
mod platform {
    use super::GridSize;
    use rustix::process;
    use rustix::termios::Winsize;
    use std::fs::File;
    use std::io;
    use std::os::fd::{AsRawFd, BorrowedFd, OwnedFd, RawFd};
    use std::os::unix::process::CommandExt;
    use std::process::Command;
    use std::sync::Arc;

    pub struct PlatformPty {
        control: Arc<File>,
        user: OwnedFd,
        size: GridSize,
    }

    impl PlatformPty {
        pub fn new(size: GridSize) -> io::Result<Self> {
            let pty_size = Winsize {
                ws_col: size.cols as u16,
                ws_row: size.rows as u16,
                ws_xpixel: size.width as u16,
                ws_ypixel: size.height as u16,
            };

            let pty = rustix_openpty::openpty(None, Some(&pty_size))?;
            let control = Arc::new(File::from(pty.controller));
            let user = pty.user;

            // Configure tty for line-based interaction and newline mapping.
            use rustix::termios::*;
            if let Ok(mut attrs) = tcgetattr(&user) {
                attrs.local_modes |=
                    LocalModes::ECHO | LocalModes::ICANON | LocalModes::ECHOE | LocalModes::ECHOK;
                attrs.input_modes |= InputModes::ICRNL;
                let _ = tcsetattr(&user, OptionalActions::Now, &attrs);
            }

            Ok(Self {
                control,
                user,
                size,
            })
        }

        pub fn spawn(&mut self, command: &mut Command) -> io::Result<()> {
            command
                .env("TERM", "xterm-256color")
                .env("COLORTERM", "truecolor")
                .env("LANG", "en_US.UTF-8")
                .stdin(self.user.try_clone()?)
                .stdout(self.user.try_clone()?)
                .stderr(self.user.try_clone()?);

            unsafe {
                let user_fd = self.user.as_raw_fd();
                command.pre_exec(move || set_controlling_terminal(user_fd));
            }

            let _ = command.spawn()?;
            Ok(())
        }

        pub fn resize(&self, size: GridSize) -> io::Result<()> {
            let pty_size = Winsize {
                ws_col: size.cols as u16,
                ws_row: size.rows as u16,
                ws_xpixel: size.width as u16,
                ws_ypixel: size.height as u16,
            };

            rustix::termios::tcsetwinsize(&self.user, pty_size)?;
            Ok(())
        }

        pub fn reader(&self) -> io::Result<File> {
            self.control.try_clone()
        }

        pub fn writer(&self) -> io::Result<File> {
            self.control.try_clone()
        }
    }

    fn set_controlling_terminal(user: RawFd) -> io::Result<()> {
        process::setsid()?;
        process::ioctl_tiocsctty(unsafe { BorrowedFd::borrow_raw(user) })?;

        for fd in 3..1000 {
            unsafe {
                libc::close(fd);
            }
        }

        Ok(())
    }
}

#[cfg(windows)]
mod platform {
    use super::GridSize;
    use std::ffi::{OsStr, c_void};
    use std::fs::File;
    use std::mem::{size_of, zeroed};
    use std::os::windows::ffi::OsStrExt;
    use std::os::windows::io::FromRawHandle;
    use std::process::Command;
    use std::{io, ptr};
    use windows_sys::Win32::Foundation::{
        CloseHandle, GetLastError, HANDLE, INVALID_HANDLE_VALUE, S_OK,
    };
    use windows_sys::Win32::Security::SECURITY_ATTRIBUTES;
    use windows_sys::Win32::System::Console::{
        ClosePseudoConsole, CreatePseudoConsole, HPCON, ResizePseudoConsole,
    };
    use windows_sys::Win32::System::Pipes::CreatePipe;
    use windows_sys::Win32::System::Threading::{
        CreateProcessW, DeleteProcThreadAttributeList, EXTENDED_STARTUPINFO_PRESENT,
        InitializeProcThreadAttributeList, LPPROC_THREAD_ATTRIBUTE_LIST,
        PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE, PROCESS_INFORMATION, STARTUPINFOEXW,
        UpdateProcThreadAttribute,
    };
    use windows_sys::Win32::System::WindowsProgramming::INFINITE;

    pub struct PlatformPty {
        hpc: HPCON,
        input_writer: File,
        output_reader: File,
        process_handle: HANDLE,
        thread_handle: HANDLE,
    }

    impl PlatformPty {
        pub fn new(size: GridSize) -> io::Result<Self> {
            unsafe {
                let mut app_to_pty_read: HANDLE = 0;
                let mut app_to_pty_write: HANDLE = 0;
                let mut pty_to_app_read: HANDLE = 0;
                let mut pty_to_app_write: HANDLE = 0;

                let mut security = SECURITY_ATTRIBUTES {
                    nLength: size_of::<SECURITY_ATTRIBUTES>() as u32,
                    lpSecurityDescriptor: ptr::null_mut(),
                    bInheritHandle: 1,
                };

                if CreatePipe(&mut app_to_pty_read, &mut app_to_pty_write, &security, 0) == 0 {
                    return Err(last_error("CreatePipe app->pty"));
                }
                if CreatePipe(&mut pty_to_app_read, &mut pty_to_app_write, &security, 0) == 0 {
                    let _ = CloseHandle(app_to_pty_read);
                    let _ = CloseHandle(app_to_pty_write);
                    return Err(last_error("CreatePipe pty->app"));
                }

                let mut hpc: HPCON = 0;
                let hr = CreatePseudoConsole(
                    coord(size),
                    app_to_pty_read,
                    pty_to_app_write,
                    0,
                    &mut hpc,
                );
                let _ = CloseHandle(app_to_pty_read);
                let _ = CloseHandle(pty_to_app_write);
                if hr != S_OK {
                    let _ = CloseHandle(app_to_pty_write);
                    let _ = CloseHandle(pty_to_app_read);
                    return Err(io::Error::other(format!(
                        "CreatePseudoConsole failed: HRESULT 0x{hr:08x}"
                    )));
                }

                let input_writer = File::from_raw_handle(app_to_pty_write as *mut c_void);
                let output_reader = File::from_raw_handle(pty_to_app_read as *mut c_void);

                Ok(Self {
                    hpc,
                    input_writer,
                    output_reader,
                    process_handle: INVALID_HANDLE_VALUE,
                    thread_handle: INVALID_HANDLE_VALUE,
                })
            }
        }

        pub fn spawn(&mut self, command: &mut Command) -> io::Result<()> {
            unsafe {
                let mut attr_list_size = 0usize;
                let _ = InitializeProcThreadAttributeList(
                    ptr::null_mut(),
                    1,
                    0,
                    &mut attr_list_size as *mut usize,
                );
                let mut attr_storage = vec![0u8; attr_list_size];
                let attr_list = attr_storage.as_mut_ptr() as LPPROC_THREAD_ATTRIBUTE_LIST;

                if InitializeProcThreadAttributeList(attr_list, 1, 0, &mut attr_list_size) == 0 {
                    return Err(last_error("InitializeProcThreadAttributeList"));
                }

                if UpdateProcThreadAttribute(
                    attr_list,
                    0,
                    PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE as usize,
                    &self.hpc as *const _ as *const c_void,
                    size_of::<HPCON>(),
                    ptr::null_mut(),
                    ptr::null(),
                ) == 0
                {
                    DeleteProcThreadAttributeList(attr_list);
                    return Err(last_error("UpdateProcThreadAttribute"));
                }

                let mut startup: STARTUPINFOEXW = zeroed();
                startup.StartupInfo.cb = size_of::<STARTUPINFOEXW>() as u32;
                startup.lpAttributeList = attr_list;

                let mut process_info: PROCESS_INFORMATION = zeroed();
                let mut command_line = command_line(command);

                let created = CreateProcessW(
                    ptr::null(),
                    command_line.as_mut_ptr(),
                    ptr::null(),
                    ptr::null(),
                    0,
                    EXTENDED_STARTUPINFO_PRESENT,
                    ptr::null(),
                    ptr::null(),
                    &mut startup.StartupInfo,
                    &mut process_info,
                );

                DeleteProcThreadAttributeList(attr_list);

                if created == 0 {
                    return Err(last_error("CreateProcessW"));
                }

                self.process_handle = process_info.hProcess;
                self.thread_handle = process_info.hThread;
                Ok(())
            }
        }

        pub fn resize(&self, size: GridSize) -> io::Result<()> {
            unsafe {
                let hr = ResizePseudoConsole(self.hpc, coord(size));
                if hr != S_OK {
                    return Err(io::Error::other(format!(
                        "ResizePseudoConsole failed: HRESULT 0x{hr:08x}"
                    )));
                }
            }
            Ok(())
        }

        pub fn reader(&self) -> io::Result<File> {
            self.output_reader.try_clone()
        }

        pub fn writer(&self) -> io::Result<File> {
            self.input_writer.try_clone()
        }
    }

    impl Drop for PlatformPty {
        fn drop(&mut self) {
            unsafe {
                if self.thread_handle != INVALID_HANDLE_VALUE {
                    let _ = CloseHandle(self.thread_handle);
                }
                if self.process_handle != INVALID_HANDLE_VALUE {
                    let _ = CloseHandle(self.process_handle);
                }
                if self.hpc != 0 {
                    ClosePseudoConsole(self.hpc);
                }
                let _ = INFINITE;
            }
        }
    }

    fn last_error(context: &str) -> io::Error {
        let code = unsafe { GetLastError() };
        io::Error::other(format!("{context} failed: Win32 {code}"))
    }

    fn coord(size: GridSize) -> windows_sys::Win32::System::Console::COORD {
        windows_sys::Win32::System::Console::COORD {
            X: size.cols as i16,
            Y: size.rows as i16,
        }
    }

    fn command_line(command: &Command) -> Vec<u16> {
        let mut line = quote_arg(command.get_program());
        for arg in command.get_args() {
            line.push(' ');
            line.push_str(&quote_arg(arg));
        }

        OsStr::new(&line)
            .encode_wide()
            .chain(std::iter::once(0))
            .collect()
    }

    fn quote_arg(arg: &OsStr) -> String {
        let raw = arg.to_string_lossy();
        if raw.is_empty() || raw.contains([' ', '\t', '"']) {
            let escaped = raw.replace('"', "\\\"");
            format!("\"{escaped}\"")
        } else {
            raw.into_owned()
        }
    }
}
