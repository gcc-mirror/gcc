use std::convert::TryInto;
use std::ffi::c_uchar;
use std::fmt;
use std::slice::from_raw_parts;
use std::str::from_utf8;

extern "C" {
    fn FFIString__new(data: *const c_uchar, len: u64) -> FFIString;
    fn FFIString__drop(string: *mut FFIString);
}

#[repr(C)]
#[derive(Debug)]
pub struct FFIString {
    data: *const c_uchar,
    len: u64,
}

impl<S> From<S> for FFIString
where
    S: AsRef<str>,
{
    fn from(s: S) -> Self {
        unsafe { FFIString__new(s.as_ref().as_ptr(), s.as_ref().len() as u64) }
    }
}

impl Clone for FFIString {
    fn clone(&self) -> Self {
        FFIString::from(&self.to_string())
    }
}

impl Drop for FFIString {
    fn drop(&mut self) {
        unsafe {
            FFIString__drop(self as *mut FFIString);
        }
    }
}

impl fmt::Display for FFIString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(
            from_utf8(unsafe {
                from_raw_parts(self.data, self.len.try_into().map_err(|_| fmt::Error)?)
            })
            .map_err(|_| fmt::Error)?,
        )
    }
}
