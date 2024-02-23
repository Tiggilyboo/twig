use codegen;
use parser;

macro_rules! run_test {
    ($output:ty, $code:expr, $($input:expr, $input_ty:ty),+) => {
        if let Some(parsed) = parser::parse($code) {
            match codegen::compile(&parsed, true) {
                Err(err) => {
                    assert!(false, "{:?}", err);
                    None
                },
                Ok(Some(pointer)) => {
                    let v: $output = unsafe {
                        let code_fn = std::mem::transmute::<_, fn($($input_ty),*) -> $output>(pointer);
                        code_fn($($input),*)
                    };
                    Some(v)
                }
                _ => {
                    assert!(false, "unable to compile test");
                    None
                }
            }
        } else {
            assert!(false, "unable to parse test");
            None
        }
    };
}

#[test]
fn declare_simple_main() {
    let v: Option<()> = run_test!((), "(i:main 42)", (), ());
    assert!(v.is_some())
}

#[test]
fn declare_main_output_i32() {
    let v: Option<i32> = run_test!(i32, "(i:main, i:argc 42)", (), ());
    if let Some(value) = v {
        assert_eq!(value, 42);
    } else {
        assert!(false, "42 = {:?}", v);
    }
}

#[test]
fn main_returns_argc() {
    let args = vec!["123".into()];
    let argc = args.len() as i32;
    let v: Option<i32> = run_test!(
        i32,
        "(i:main, i:argc, (s):argv argc)",
        argc,
        i32,
        args.clone(),
        Vec<String>
    );
    if let Some(value) = v {
        assert_eq!(value, args.len() as i32);
    } else {
        assert!(false, "1 = {:?}", v);
    }
}

#[test]
fn builtin_print() {
    let v: Option<i32> = run_test!(
        i32,
        "(i:main (print \"Hello World\"))",
        vec![b"Hello World\0".as_ptr()],
        Vec<*const u8>
    );
    if let Some(value) = v {
        assert_eq!(value, 0);
    } else {
        assert!(false, "0 = {:?}", v);
    }
}
