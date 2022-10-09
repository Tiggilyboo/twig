(module
  (type (;0;) (func (param i32)))
  (type (;1;) (func (param i32 i32) (result i32)))
  (type (;2;) (func (param i32 i32 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "args_get" (func $wasi::lib_generated::wasi_snapshot_preview1::args_get::h2ad5cc186363b7d4 (type 1)))
  (import "wasi_snapshot_preview1" "args_sizes_get" (func $args_sizes_get (type 1)))
  (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (type 2)))
  (import "wasi_snapshot_preview1" "environ_get" (func $environ_get (type 1)))
  (import "wasi_snapshot_preview1" "environ_sizes_get" (func $environ_sizes_get (type 1)))
  (import "wasi_snapshot_preview1" "proc_exit" (func $proc_exit (type 0)))
  (import "wasi_snapshot_preview1" "args_get" (func $args_get (type 1)))
  (func $main (export "main")(param i32 i32) (result i32)
    (local i32)
    local.get 0
    i32.const 0
    return
  )
)
