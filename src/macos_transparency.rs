//! macOS-specific helpers for window transparency.
//!
//! Uses raw Objective-C runtime FFI to set NSWindow + CAMetalLayer to transparent.

#![allow(clippy::missing_safety_doc)]

use std::ffi::c_void;

// On ARM64, objc_msgSend must NOT be declared as variadic — the calling
// convention differs. Instead we declare it with no parameters and transmute
// to the correct signature at each call-site.
#[link(name = "objc", kind = "dylib")]
unsafe extern "C" {
    fn objc_msgSend();
    fn sel_registerName(name: *const u8) -> *mut c_void;
    fn objc_getClass(name: *const u8) -> *mut c_void;
}

type MsgSend0 = unsafe extern "C" fn(*mut c_void, *mut c_void) -> *mut c_void;
type MsgSendBool = unsafe extern "C" fn(*mut c_void, *mut c_void, i32) -> *mut c_void;
type MsgSendObj = unsafe extern "C" fn(*mut c_void, *mut c_void, *mut c_void) -> *mut c_void;

/// Make the given window fully transparent so the desktop shows through
/// any pixels with alpha < 1.
///
/// # Safety
/// `ns_view` must be a valid pointer to an `NSView` that belongs to an `NSWindow`.
pub unsafe fn make_window_transparent(ns_view: *mut c_void) {
    unsafe {
        let send0: MsgSend0 = std::mem::transmute(objc_msgSend as *const c_void);
        let send_bool: MsgSendBool = std::mem::transmute(objc_msgSend as *const c_void);
        let send_obj: MsgSendObj = std::mem::transmute(objc_msgSend as *const c_void);

        // NSView -> NSWindow
        let sel_window = sel_registerName(c"window".as_ptr() as *const _);
        let ns_window = send0(ns_view, sel_window);
        if ns_window.is_null() {
            return;
        }

        // [window setOpaque:NO]
        let sel_set_opaque = sel_registerName(c"setOpaque:".as_ptr() as *const _);
        send_bool(ns_window, sel_set_opaque, 0);

        // [window setBackgroundColor:[NSColor clearColor]]
        let cls_nscolor = objc_getClass(c"NSColor".as_ptr() as *const _);
        let sel_clear = sel_registerName(c"clearColor".as_ptr() as *const _);
        let clear = send0(cls_nscolor, sel_clear);
        let sel_set_bg = sel_registerName(c"setBackgroundColor:".as_ptr() as *const _);
        send_obj(ns_window, sel_set_bg, clear);

        // [window setHasShadow:NO]
        let sel_shadow = sel_registerName(c"setHasShadow:".as_ptr() as *const _);
        send_bool(ns_window, sel_shadow, 0);

        // Make the view's layer (CAMetalLayer) non-opaque too.
        let sel_layer = sel_registerName(c"layer".as_ptr() as *const _);
        let layer = send0(ns_view, sel_layer);
        if !layer.is_null() {
            let sel_set_opaque_layer = sel_registerName(c"setOpaque:".as_ptr() as *const _);
            send_bool(layer, sel_set_opaque_layer, 0);
        }
    }
}
