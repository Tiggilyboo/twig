use std::ffi::{c_char, CStr};

pub unsafe extern "C" fn print_string(string: *const c_char) {
    let cstr = CStr::from_ptr(string);
    print!("{}", cstr.to_str().unwrap())
}
