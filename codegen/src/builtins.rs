use std::ffi::{c_char, CStr};

pub unsafe extern "C" fn print(string: *const c_char) -> i32 {
    let cstr = CStr::from_ptr(string);
    print!("{}", cstr.to_str().unwrap());
    0
}
