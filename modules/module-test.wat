(module
  (type (;0;) (func))
  (type (;1;) (func (param i32)))
  (type (;2;) (func (param i32) (result i64)))
  (type (;3;) (func (param i32 i32)))
  (type (;4;) (func (param i32) (result i32)))
  (type (;5;) (func (param i32 i32) (result i32)))
  (type (;6;) (func (param i32 i32 i32)))
  (type (;7;) (func (param i32 i32 i32) (result i32)))
  (type (;8;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;9;) (func (param i32 i32 i32 i32)))
  (type (;10;) (func (result i32)))
  (type (;11;) (func (param i32 i32 i32 i32 i32)))
  (type (;12;) (func (param i32 i32 i32 i32 i32) (result i32)))
  (type (;13;) (func (param i32 i32 i32 i32 i32 i32 i32)))
  (type (;14;) (func (param i32 i32 i32 i32 i32 i32) (result i32)))
  (type (;15;) (func (param i32 i32 i32 i32 i32 i32 i32) (result i32)))
  (type (;16;) (func (param i64 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "args_get" (func $_ZN4wasi13lib_generated22wasi_snapshot_preview18args_get17hedb4461d1984109aE (type 5)))
  (import "wasi_snapshot_preview1" "args_sizes_get" (func $_ZN4wasi13lib_generated22wasi_snapshot_preview114args_sizes_get17h26cfdfead02b25dbE (type 5)))
  (import "wasi_snapshot_preview1" "fd_write" (func $_ZN4wasi13lib_generated22wasi_snapshot_preview18fd_write17h1706d154ab4fc644E (type 8)))
  (import "wasi_snapshot_preview1" "environ_get" (func $__imported_wasi_snapshot_preview1_environ_get (type 5)))
  (import "wasi_snapshot_preview1" "environ_sizes_get" (func $__imported_wasi_snapshot_preview1_environ_sizes_get (type 5)))
  (import "wasi_snapshot_preview1" "proc_exit" (func $__imported_wasi_snapshot_preview1_proc_exit (type 1)))
  (func $__wasm_call_ctors (type 0)
    call $__wasilibc_initialize_environ_eagerly)
  (func $_start (type 0)
    (local i32)
    block  ;; label = @1
      call $__original_main
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      call $exit
      unreachable
    end)
  (func $_ZN5alloc7raw_vec11finish_grow17h5aa984bc9120b3b5E.llvm.11031354072167798738 (type 9) (param i32 i32 i32 i32)
    (local i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 2
        i32.eqz
        br_if 0 (;@2;)
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 1
                  i32.const 0
                  i32.lt_s
                  br_if 0 (;@7;)
                  local.get 3
                  i32.load offset=8
                  i32.eqz
                  br_if 2 (;@5;)
                  local.get 3
                  i32.load offset=4
                  local.tee 4
                  br_if 1 (;@6;)
                  local.get 1
                  br_if 3 (;@4;)
                  local.get 2
                  local.set 3
                  br 4 (;@3;)
                end
                local.get 0
                i32.const 8
                i32.add
                i32.const 0
                i32.store
                br 5 (;@1;)
              end
              local.get 3
              i32.load
              local.get 4
              local.get 2
              local.get 1
              call $__rust_realloc
              local.set 3
              br 2 (;@3;)
            end
            local.get 1
            br_if 0 (;@4;)
            local.get 2
            local.set 3
            br 1 (;@3;)
          end
          local.get 1
          local.get 2
          call $__rust_alloc
          local.set 3
        end
        block  ;; label = @3
          local.get 3
          i32.eqz
          br_if 0 (;@3;)
          local.get 0
          local.get 3
          i32.store offset=4
          local.get 0
          i32.const 8
          i32.add
          local.get 1
          i32.store
          local.get 0
          i32.const 0
          i32.store
          return
        end
        local.get 0
        local.get 1
        i32.store offset=4
        local.get 0
        i32.const 8
        i32.add
        local.get 2
        i32.store
        br 1 (;@1;)
      end
      local.get 0
      local.get 1
      i32.store offset=4
      local.get 0
      i32.const 8
      i32.add
      i32.const 0
      i32.store
    end
    local.get 0
    i32.const 1
    i32.store)
  (func $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$7reserve21do_reserve_and_handle17h13f2e65da1914c47E (type 6) (param i32 i32 i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        local.get 2
        i32.add
        local.tee 2
        local.get 1
        i32.lt_u
        br_if 0 (;@2;)
        local.get 0
        i32.const 4
        i32.add
        i32.load
        local.tee 1
        i32.const 1
        i32.shl
        local.tee 4
        local.get 2
        local.get 4
        local.get 2
        i32.gt_u
        select
        local.tee 2
        i32.const 4
        local.get 2
        i32.const 4
        i32.gt_u
        select
        local.tee 2
        i32.const 12
        i32.mul
        local.set 4
        local.get 2
        i32.const 178956971
        i32.lt_u
        i32.const 2
        i32.shl
        local.set 5
        block  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.eqz
            br_if 0 (;@4;)
            local.get 3
            i32.const 4
            i32.store offset=24
            local.get 3
            local.get 1
            i32.const 12
            i32.mul
            i32.store offset=20
            local.get 3
            local.get 0
            i32.load
            i32.store offset=16
            br 1 (;@3;)
          end
          local.get 3
          i32.const 0
          i32.store offset=24
        end
        local.get 3
        local.get 4
        local.get 5
        local.get 3
        i32.const 16
        i32.add
        call $_ZN5alloc7raw_vec11finish_grow17h5aa984bc9120b3b5E.llvm.11031354072167798738
        local.get 3
        i32.load offset=4
        local.set 1
        block  ;; label = @3
          local.get 3
          i32.load
          br_if 0 (;@3;)
          local.get 0
          local.get 1
          i32.store
          local.get 0
          i32.const 4
          i32.add
          local.get 2
          i32.store
          br 2 (;@1;)
        end
        local.get 3
        i32.const 8
        i32.add
        i32.load
        local.tee 0
        i32.const -2147483647
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        local.get 0
        call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
        unreachable
      end
      call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
      unreachable
    end
    local.get 3
    i32.const 32
    i32.add
    global.set $__stack_pointer)
  (func $_ZN98_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$alloc..vec..spec_from_iter..SpecFromIter$LT$T$C$I$GT$$GT$9from_iter17hcf842fc43ecf5a34E (type 3) (param i32 i32)
    (local i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 80
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store
    local.get 2
    i32.const 64
    i32.add
    local.get 2
    call $_ZN73_$LT$std..env..Args$u20$as$u20$core..iter..traits..iterator..Iterator$GT$4next17hc5401505321c279dE
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 2
            i32.load offset=64
            br_if 0 (;@4;)
            local.get 0
            i32.const 0
            i32.store offset=8
            local.get 0
            i64.const 4
            i64.store align=4
            local.get 2
            i32.load offset=12
            local.tee 3
            local.get 2
            i32.load offset=8
            local.tee 1
            i32.sub
            i32.const 12
            i32.div_u
            local.set 4
            block  ;; label = @5
              local.get 3
              local.get 1
              i32.eq
              br_if 0 (;@5;)
              local.get 4
              i32.const 12
              i32.mul
              local.set 3
              loop  ;; label = @6
                block  ;; label = @7
                  local.get 1
                  i32.const 4
                  i32.add
                  i32.load
                  local.tee 4
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 1
                  i32.load
                  local.get 4
                  i32.const 1
                  call $__rust_dealloc
                end
                local.get 1
                i32.const 12
                i32.add
                local.set 1
                local.get 3
                i32.const -12
                i32.add
                local.tee 3
                br_if 0 (;@6;)
              end
            end
            local.get 2
            i32.load offset=4
            local.tee 1
            i32.eqz
            br_if 1 (;@3;)
            local.get 2
            i32.load
            local.get 1
            i32.const 12
            i32.mul
            i32.const 4
            call $__rust_dealloc
            br 1 (;@3;)
          end
          local.get 2
          i32.const 32
          i32.add
          local.get 2
          call $_ZN73_$LT$std..env..Args$u20$as$u20$core..iter..traits..iterator..Iterator$GT$9size_hint17hf8693f4f8fbdba08E
          local.get 2
          i32.load offset=32
          i32.const 1
          i32.add
          local.tee 1
          i32.const -1
          local.get 1
          select
          local.tee 1
          i32.const 4
          local.get 1
          i32.const 4
          i32.gt_u
          select
          local.tee 1
          i32.const 178956970
          i32.gt_u
          br_if 2 (;@1;)
          local.get 1
          i32.const 12
          i32.mul
          local.tee 3
          i32.const 0
          i32.lt_s
          br_if 2 (;@1;)
          local.get 1
          i32.const 178956971
          i32.lt_u
          i32.const 2
          i32.shl
          local.set 4
          block  ;; label = @4
            block  ;; label = @5
              local.get 3
              br_if 0 (;@5;)
              local.get 4
              local.set 5
              br 1 (;@4;)
            end
            local.get 3
            local.get 4
            call $__rust_alloc
            local.set 5
          end
          local.get 5
          i32.eqz
          br_if 1 (;@2;)
          local.get 5
          local.get 2
          i64.load offset=64
          i64.store align=4
          local.get 5
          i32.const 8
          i32.add
          local.get 2
          i32.const 64
          i32.add
          i32.const 8
          i32.add
          i32.load
          i32.store
          local.get 2
          i32.const 1
          i32.store offset=24
          local.get 2
          local.get 1
          i32.store offset=20
          local.get 2
          local.get 5
          i32.store offset=16
          local.get 2
          i32.const 32
          i32.add
          i32.const 8
          i32.add
          local.get 2
          i32.const 8
          i32.add
          i64.load
          i64.store
          local.get 2
          local.get 2
          i64.load
          i64.store offset=32
          local.get 2
          i32.const 48
          i32.add
          local.get 2
          i32.const 32
          i32.add
          call $_ZN73_$LT$std..env..Args$u20$as$u20$core..iter..traits..iterator..Iterator$GT$4next17hc5401505321c279dE
          block  ;; label = @4
            local.get 2
            i32.load offset=48
            i32.eqz
            br_if 0 (;@4;)
            i32.const 12
            local.set 3
            i32.const 1
            local.set 1
            loop  ;; label = @5
              block  ;; label = @6
                local.get 1
                local.get 2
                i32.load offset=20
                i32.ne
                br_if 0 (;@6;)
                local.get 2
                i32.const 64
                i32.add
                local.get 2
                i32.const 32
                i32.add
                call $_ZN73_$LT$std..env..Args$u20$as$u20$core..iter..traits..iterator..Iterator$GT$9size_hint17hf8693f4f8fbdba08E
                local.get 2
                i32.const 16
                i32.add
                local.get 1
                local.get 2
                i32.load offset=64
                i32.const 1
                i32.add
                local.tee 4
                i32.const -1
                local.get 4
                select
                call $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$7reserve21do_reserve_and_handle17h13f2e65da1914c47E
                local.get 2
                i32.load offset=16
                local.set 5
              end
              local.get 5
              local.get 3
              i32.add
              local.tee 4
              local.get 2
              i64.load offset=48
              i64.store align=4
              local.get 4
              i32.const 8
              i32.add
              local.get 2
              i32.const 48
              i32.add
              i32.const 8
              i32.add
              i32.load
              i32.store
              local.get 2
              local.get 1
              i32.const 1
              i32.add
              local.tee 1
              i32.store offset=24
              local.get 3
              i32.const 12
              i32.add
              local.set 3
              local.get 2
              i32.const 48
              i32.add
              local.get 2
              i32.const 32
              i32.add
              call $_ZN73_$LT$std..env..Args$u20$as$u20$core..iter..traits..iterator..Iterator$GT$4next17hc5401505321c279dE
              local.get 2
              i32.load offset=48
              br_if 0 (;@5;)
            end
          end
          local.get 2
          i32.load offset=44
          local.tee 3
          local.get 2
          i32.load offset=40
          local.tee 1
          i32.sub
          i32.const 12
          i32.div_u
          local.set 4
          block  ;; label = @4
            local.get 3
            local.get 1
            i32.eq
            br_if 0 (;@4;)
            local.get 4
            i32.const 12
            i32.mul
            local.set 3
            loop  ;; label = @5
              block  ;; label = @6
                local.get 1
                i32.const 4
                i32.add
                i32.load
                local.tee 4
                i32.eqz
                br_if 0 (;@6;)
                local.get 1
                i32.load
                local.get 4
                i32.const 1
                call $__rust_dealloc
              end
              local.get 1
              i32.const 12
              i32.add
              local.set 1
              local.get 3
              i32.const -12
              i32.add
              local.tee 3
              br_if 0 (;@5;)
            end
          end
          block  ;; label = @4
            local.get 2
            i32.load offset=36
            local.tee 1
            i32.eqz
            br_if 0 (;@4;)
            local.get 2
            i32.load offset=32
            local.get 1
            i32.const 12
            i32.mul
            i32.const 4
            call $__rust_dealloc
          end
          local.get 0
          local.get 2
          i64.load offset=16
          i64.store align=4
          local.get 0
          i32.const 8
          i32.add
          local.get 2
          i32.const 16
          i32.add
          i32.const 8
          i32.add
          i32.load
          i32.store
        end
        local.get 2
        i32.const 80
        i32.add
        global.set $__stack_pointer
        return
      end
      local.get 3
      local.get 4
      call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
      unreachable
    end
    call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
    unreachable)
  (func $_ZN3std10sys_common9backtrace28__rust_begin_short_backtrace17h318dfdeae81ad4d2E (type 1) (param i32)
    local.get 0
    call_indirect (type 0))
  (func $_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17h787271d5e3e8cf44E.llvm.8739220772287655740 (type 4) (param i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    local.get 0
    i32.load
    call $_ZN3std10sys_common9backtrace28__rust_begin_short_backtrace17h318dfdeae81ad4d2E
    local.get 1
    i32.const 0
    i32.store8 offset=15
    local.get 1
    i32.const 15
    i32.add
    call $_ZN3std3sys4wasi7process8ExitCode6as_i3217hd5ea6c8911056b09E
    local.set 0
    local.get 1
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17ha8bed69553fd5b4cE.llvm.8739220772287655740 (type 4) (param i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    local.get 0
    i32.load
    call $_ZN3std10sys_common9backtrace28__rust_begin_short_backtrace17h318dfdeae81ad4d2E
    local.get 1
    i32.const 0
    i32.store8 offset=15
    local.get 1
    i32.const 15
    i32.add
    call $_ZN3std3sys4wasi7process8ExitCode6as_i3217hd5ea6c8911056b09E
    local.set 0
    local.get 1
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core3ptr85drop_in_place$LT$std..rt..lang_start$LT$$LP$$RP$$GT$..$u7b$$u7b$closure$u7d$$u7d$$GT$17h45d796ff621e665dE.llvm.8739220772287655740 (type 1) (param i32))
  (func $_ZN11module_test4main17h758981acc4b6c11eE (type 0)
    (local i32 i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 64
    i32.sub
    local.tee 0
    global.set $__stack_pointer
    local.get 0
    i32.const 16
    i32.add
    call $_ZN3std3env4args17h99c9c61d3ce5a89bE
    local.get 0
    i32.const 32
    i32.add
    i32.const 8
    i32.add
    local.get 0
    i32.const 16
    i32.add
    i32.const 8
    i32.add
    i64.load
    i64.store
    local.get 0
    local.get 0
    i64.load offset=16
    i64.store offset=32
    local.get 0
    local.get 0
    i32.const 32
    i32.add
    call $_ZN98_$LT$alloc..vec..Vec$LT$T$GT$$u20$as$u20$alloc..vec..spec_from_iter..SpecFromIter$LT$T$C$I$GT$$GT$9from_iter17hcf842fc43ecf5a34E
    local.get 0
    i32.const 52
    i32.add
    i32.const 1
    i32.store
    local.get 0
    i64.const 2
    i64.store offset=36 align=4
    local.get 0
    i32.const 1048612
    i32.store offset=32
    local.get 0
    i32.const 4
    i32.store offset=20
    local.get 0
    local.get 0
    i32.load offset=8
    local.tee 1
    i32.store offset=60
    local.get 0
    local.get 0
    i32.const 16
    i32.add
    i32.store offset=48
    local.get 0
    local.get 0
    i32.const 60
    i32.add
    i32.store offset=16
    local.get 0
    i32.const 32
    i32.add
    call $_ZN3std2io5stdio6_print17hb5031aec3871df70E
    local.get 0
    i32.load
    local.set 2
    block  ;; label = @1
      local.get 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 1
      i32.const 12
      i32.mul
      local.set 3
      local.get 2
      local.set 1
      loop  ;; label = @2
        block  ;; label = @3
          local.get 1
          i32.const 4
          i32.add
          i32.load
          local.tee 4
          i32.eqz
          br_if 0 (;@3;)
          local.get 1
          i32.load
          local.get 4
          i32.const 1
          call $__rust_dealloc
        end
        local.get 1
        i32.const 12
        i32.add
        local.set 1
        local.get 3
        i32.const -12
        i32.add
        local.tee 3
        br_if 0 (;@2;)
      end
    end
    block  ;; label = @1
      local.get 0
      i32.load offset=4
      local.tee 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      local.get 1
      i32.const 12
      i32.mul
      i32.const 4
      call $__rust_dealloc
    end
    local.get 0
    i32.const 64
    i32.add
    global.set $__stack_pointer)
  (func $__original_main (type 10) (result i32)
    (local i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 0
    global.set $__stack_pointer
    local.get 0
    i32.const 5
    i32.store offset=12
    local.get 0
    i32.const 12
    i32.add
    i32.const 1048576
    i32.const 0
    i32.const 0
    i32.const 2
    call $_ZN3std2rt19lang_start_internal17h1bb05e5e1cc430efE
    local.set 1
    local.get 0
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $main (type 5) (param i32 i32) (result i32)
    call $__original_main)
  (func $__rust_alloc (type 5) (param i32 i32) (result i32)
    (local i32)
    local.get 0
    local.get 1
    call $__rdl_alloc
    local.set 2
    local.get 2
    return)
  (func $__rust_dealloc (type 6) (param i32 i32 i32)
    local.get 0
    local.get 1
    local.get 2
    call $__rdl_dealloc
    return)
  (func $__rust_realloc (type 8) (param i32 i32 i32 i32) (result i32)
    (local i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    call $__rdl_realloc
    local.set 4
    local.get 4
    return)
  (func $__rust_alloc_error_handler (type 3) (param i32 i32)
    local.get 0
    local.get 1
    call $__rg_oom
    return)
  (func $_ZN36_$LT$T$u20$as$u20$core..any..Any$GT$7type_id17h19307b269d853256E (type 2) (param i32) (result i64)
    i64.const -4464899901473429205)
  (func $_ZN36_$LT$T$u20$as$u20$core..any..Any$GT$7type_id17ha23ad085a7a28f08E (type 2) (param i32) (result i64)
    i64.const 5100591953695616150)
  (func $_ZN36_$LT$T$u20$as$u20$core..any..Any$GT$7type_id17hed1fcd7df172fc15E (type 2) (param i32) (result i64)
    i64.const 8378048562631854315)
  (func $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h35d5d312331238e0E (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    i32.load
    local.get 1
    call $_ZN64_$LT$alloc..ffi..c_str..NulError$u20$as$u20$core..fmt..Debug$GT$3fmt17hae0afb8cc3412d06E)
  (func $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h453278982654c107E (type 5) (param i32 i32) (result i32)
    local.get 1
    i32.const 1048671
    i32.const 2
    call $_ZN4core3fmt9Formatter3pad17h66985344eb075eb9E)
  (func $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h67628d985ca90d43E (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.get 1
    call $_ZN43_$LT$bool$u20$as$u20$core..fmt..Display$GT$3fmt17h1b70f6e38dbe21bfE)
  (func $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h7ebf73f0383d863dE (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.set 0
    block  ;; label = @1
      local.get 1
      call $_ZN4core3fmt9Formatter15debug_lower_hex17h3f763578aa6ee5baE
      br_if 0 (;@1;)
      block  ;; label = @2
        local.get 1
        call $_ZN4core3fmt9Formatter15debug_upper_hex17h96706c7c15afea4cE
        br_if 0 (;@2;)
        local.get 0
        local.get 1
        call $_ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$3fmt17h438b5cc0efea7d46E
        return
      end
      local.get 0
      local.get 1
      call $_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..UpperHex$u20$for$u20$i32$GT$3fmt17h4207c064850a1e9eE
      return
    end
    local.get 0
    local.get 1
    call $_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i32$GT$3fmt17hbb0d214f9fde181dE)
  (func $_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17ha5b118b1fc2155ccE (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.get 0
    i32.load offset=4
    local.get 1
    call $_ZN42_$LT$str$u20$as$u20$core..fmt..Display$GT$3fmt17h8fa90afa7600905bE)
  (func $_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17hd4d32f1a7d58f0d8E (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.get 1
    call $_ZN70_$LT$core..panic..location..Location$u20$as$u20$core..fmt..Display$GT$3fmt17h7a2d773550b3a249E)
  (func $_ZN4core3fmt5Write10write_char17h058d020d3ee2181eE (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    i32.const 0
    i32.store offset=12
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 128
        i32.lt_u
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 1
          i32.const 2048
          i32.lt_u
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 1
            i32.const 65536
            i32.lt_u
            br_if 0 (;@4;)
            local.get 2
            local.get 1
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=15
            local.get 2
            local.get 1
            i32.const 6
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=14
            local.get 2
            local.get 1
            i32.const 12
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=13
            local.get 2
            local.get 1
            i32.const 18
            i32.shr_u
            i32.const 7
            i32.and
            i32.const 240
            i32.or
            i32.store8 offset=12
            i32.const 4
            local.set 1
            br 3 (;@1;)
          end
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=14
          local.get 2
          local.get 1
          i32.const 12
          i32.shr_u
          i32.const 224
          i32.or
          i32.store8 offset=12
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=13
          i32.const 3
          local.set 1
          br 2 (;@1;)
        end
        local.get 2
        local.get 1
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=13
        local.get 2
        local.get 1
        i32.const 6
        i32.shr_u
        i32.const 192
        i32.or
        i32.store8 offset=12
        i32.const 2
        local.set 1
        br 1 (;@1;)
      end
      local.get 2
      local.get 1
      i32.store8 offset=12
      i32.const 1
      local.set 1
    end
    local.get 0
    local.get 2
    i32.const 12
    i32.add
    local.get 1
    call $_ZN80_$LT$std..io..Write..write_fmt..Adapter$LT$T$GT$$u20$as$u20$core..fmt..Write$GT$9write_str17h9cbe34749d4b82e2E
    local.set 1
    local.get 2
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN80_$LT$std..io..Write..write_fmt..Adapter$LT$T$GT$$u20$as$u20$core..fmt..Write$GT$9write_str17h9cbe34749d4b82e2E (type 7) (param i32 i32 i32) (result i32)
    (local i32 i32 i64 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    i32.const 0
    local.set 4
    block  ;; label = @1
      local.get 2
      i32.eqz
      br_if 0 (;@1;)
      i32.const 1050664
      i64.extend_i32_u
      i64.const 32
      i64.shl
      i64.const 2
      i64.or
      local.set 5
      loop  ;; label = @2
        local.get 3
        local.get 2
        i32.store offset=12
        local.get 3
        local.get 1
        i32.store offset=8
        local.get 3
        i32.const 16
        i32.add
        i32.const 2
        local.get 3
        i32.const 8
        i32.add
        i32.const 1
        call $_ZN4wasi13lib_generated8fd_write17hb23bdba73a8f4709E
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 3
                i32.load16_u offset=16
                br_if 0 (;@6;)
                local.get 3
                i32.load offset=20
                local.tee 6
                i32.eqz
                br_if 1 (;@5;)
                local.get 2
                local.get 6
                i32.lt_u
                br_if 2 (;@4;)
                local.get 2
                local.get 6
                i32.sub
                local.set 2
                local.get 1
                local.get 6
                i32.add
                local.set 1
                br 3 (;@3;)
              end
              local.get 3
              local.get 3
              i32.load16_u offset=18
              i32.store16 offset=30
              local.get 3
              i32.const 30
              i32.add
              call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
              local.tee 6
              i32.const 65535
              i32.and
              call $_ZN3std3sys4wasi17decode_error_kind17h27b067431089650bE
              i32.const 255
              i32.and
              i32.const 35
              i32.eq
              br_if 2 (;@3;)
              local.get 6
              i64.extend_i32_u
              i64.const 65535
              i64.and
              i64.const 32
              i64.shl
              local.set 5
            end
            local.get 5
            i64.const 255
            i64.and
            i64.const 4
            i64.eq
            br_if 3 (;@1;)
            block  ;; label = @5
              local.get 0
              i32.load8_u offset=4
              i32.const 3
              i32.ne
              br_if 0 (;@5;)
              local.get 0
              i32.const 8
              i32.add
              i32.load
              local.tee 2
              i32.load
              local.get 2
              i32.load offset=4
              i32.load
              call_indirect (type 1)
              block  ;; label = @6
                local.get 2
                i32.load offset=4
                local.tee 1
                i32.const 4
                i32.add
                i32.load
                local.tee 6
                i32.eqz
                br_if 0 (;@6;)
                local.get 2
                i32.load
                local.get 6
                local.get 1
                i32.const 8
                i32.add
                i32.load
                call $__rust_dealloc
              end
              local.get 2
              i32.const 12
              i32.const 4
              call $__rust_dealloc
            end
            local.get 0
            local.get 5
            i64.store offset=4 align=4
            i32.const 1
            local.set 4
            br 3 (;@1;)
          end
          local.get 6
          local.get 2
          i32.const 1050620
          call $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E
          unreachable
        end
        local.get 2
        br_if 0 (;@2;)
      end
    end
    local.get 3
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 4)
  (func $_ZN4core3fmt5Write10write_char17h612343524e950315E (type 5) (param i32 i32) (result i32)
    (local i32 i64 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    i32.const 0
    i32.store offset=4
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 128
        i32.lt_u
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 1
          i32.const 2048
          i32.lt_u
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 1
            i32.const 65536
            i32.lt_u
            br_if 0 (;@4;)
            local.get 2
            local.get 1
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=7
            local.get 2
            local.get 1
            i32.const 6
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=6
            local.get 2
            local.get 1
            i32.const 12
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=5
            local.get 2
            local.get 1
            i32.const 18
            i32.shr_u
            i32.const 7
            i32.and
            i32.const 240
            i32.or
            i32.store8 offset=4
            i32.const 4
            local.set 1
            br 3 (;@1;)
          end
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=6
          local.get 2
          local.get 1
          i32.const 12
          i32.shr_u
          i32.const 224
          i32.or
          i32.store8 offset=4
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=5
          i32.const 3
          local.set 1
          br 2 (;@1;)
        end
        local.get 2
        local.get 1
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=5
        local.get 2
        local.get 1
        i32.const 6
        i32.shr_u
        i32.const 192
        i32.or
        i32.store8 offset=4
        i32.const 2
        local.set 1
        br 1 (;@1;)
      end
      local.get 2
      local.get 1
      i32.store8 offset=4
      i32.const 1
      local.set 1
    end
    local.get 2
    i32.const 8
    i32.add
    local.get 0
    i32.load
    local.get 2
    i32.const 4
    i32.add
    local.get 1
    call $_ZN61_$LT$std..io..stdio..StdoutLock$u20$as$u20$std..io..Write$GT$9write_all17h7430178078d1718bE
    block  ;; label = @1
      local.get 2
      i32.load8_u offset=8
      local.tee 1
      i32.const 4
      i32.eq
      br_if 0 (;@1;)
      local.get 2
      i64.load offset=8
      local.set 3
      block  ;; label = @2
        local.get 0
        i32.load8_u offset=4
        i32.const 3
        i32.ne
        br_if 0 (;@2;)
        local.get 0
        i32.const 8
        i32.add
        i32.load
        local.tee 4
        i32.load
        local.get 4
        i32.load offset=4
        i32.load
        call_indirect (type 1)
        block  ;; label = @3
          local.get 4
          i32.load offset=4
          local.tee 5
          i32.const 4
          i32.add
          i32.load
          local.tee 6
          i32.eqz
          br_if 0 (;@3;)
          local.get 4
          i32.load
          local.get 6
          local.get 5
          i32.const 8
          i32.add
          i32.load
          call $__rust_dealloc
        end
        local.get 4
        i32.const 12
        i32.const 4
        call $__rust_dealloc
      end
      local.get 0
      local.get 3
      i64.store offset=4 align=4
    end
    local.get 2
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 1
    i32.const 4
    i32.ne)
  (func $_ZN61_$LT$std..io..stdio..StdoutLock$u20$as$u20$std..io..Write$GT$9write_all17h7430178078d1718bE (type 9) (param i32 i32 i32 i32)
    (local i32 i32 i32 i32 i32 i32 i64)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 4
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.load
            local.tee 1
            i32.load offset=8
            br_if 0 (;@4;)
            local.get 1
            i32.const -1
            i32.store offset=8
            local.get 4
            i32.const 10
            local.get 2
            local.get 3
            call $_ZN4core5slice6memchr7memrchr17h93afa4aa8611e14aE
            local.get 1
            i32.const 12
            i32.add
            local.set 5
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 4
                  i32.load
                  br_if 0 (;@7;)
                  block  ;; label = @8
                    block  ;; label = @9
                      local.get 1
                      i32.const 20
                      i32.add
                      i32.load
                      local.tee 6
                      br_if 0 (;@9;)
                      i32.const 0
                      local.set 6
                      br 1 (;@8;)
                    end
                    local.get 6
                    local.get 5
                    i32.load
                    i32.add
                    i32.const -1
                    i32.add
                    i32.load8_u
                    i32.const 10
                    i32.ne
                    br_if 0 (;@8;)
                    local.get 4
                    i32.const 8
                    i32.add
                    local.get 5
                    call $_ZN3std2io8buffered9bufwriter18BufWriter$LT$W$GT$9flush_buf17he5398a73dc5def27E
                    local.get 4
                    i32.load8_u offset=8
                    local.tee 6
                    i32.const 4
                    i32.ne
                    br_if 3 (;@5;)
                    local.get 1
                    i32.const 20
                    i32.add
                    i32.load
                    local.set 6
                  end
                  local.get 1
                  i32.const 16
                  i32.add
                  i32.load
                  local.get 6
                  i32.sub
                  local.get 3
                  i32.gt_u
                  br_if 1 (;@6;)
                  local.get 0
                  local.get 5
                  local.get 2
                  local.get 3
                  call $_ZN3std2io8buffered9bufwriter18BufWriter$LT$W$GT$14write_all_cold17ha8846756f7f5598dE
                  br 6 (;@1;)
                end
                local.get 4
                i32.load offset=4
                i32.const 1
                i32.add
                local.tee 6
                local.get 3
                i32.gt_u
                br_if 3 (;@3;)
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        local.get 1
                        i32.const 20
                        i32.add
                        i32.load
                        local.tee 7
                        br_if 0 (;@10;)
                        local.get 4
                        i32.const 8
                        i32.add
                        local.get 2
                        local.get 6
                        call $_ZN3std2io5Write9write_all17h80bc75e0ee5e76e4E
                        local.get 4
                        i32.load8_u offset=8
                        local.tee 7
                        i32.const 4
                        i32.eq
                        br_if 1 (;@9;)
                        block  ;; label = @11
                          block  ;; label = @12
                            local.get 7
                            br_table 0 (;@12;) 1 (;@11;) 0 (;@12;) 0 (;@12;) 0 (;@12;)
                          end
                          local.get 4
                          i32.load offset=12
                          local.set 8
                        end
                        i32.const 1052468
                        call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
                        local.set 9
                        local.get 7
                        br_if 1 (;@9;)
                        local.get 8
                        local.get 9
                        i32.const 65535
                        i32.and
                        i32.ne
                        br_if 1 (;@9;)
                        i32.const 0
                        local.set 7
                        br 2 (;@8;)
                      end
                      block  ;; label = @10
                        block  ;; label = @11
                          local.get 1
                          i32.const 16
                          i32.add
                          i32.load
                          local.get 7
                          i32.sub
                          local.get 6
                          i32.le_u
                          br_if 0 (;@11;)
                          local.get 1
                          i32.load offset=12
                          local.get 7
                          i32.add
                          local.get 2
                          local.get 6
                          call $memcpy
                          drop
                          local.get 1
                          i32.const 20
                          i32.add
                          local.get 7
                          local.get 6
                          i32.add
                          i32.store
                          br 1 (;@10;)
                        end
                        local.get 4
                        i32.const 8
                        i32.add
                        local.get 5
                        local.get 2
                        local.get 6
                        call $_ZN3std2io8buffered9bufwriter18BufWriter$LT$W$GT$14write_all_cold17ha8846756f7f5598dE
                        local.get 4
                        i32.load8_u offset=8
                        local.tee 7
                        i32.const 4
                        i32.ne
                        br_if 8 (;@2;)
                      end
                      local.get 4
                      i32.const 8
                      i32.add
                      local.get 5
                      call $_ZN3std2io8buffered9bufwriter18BufWriter$LT$W$GT$9flush_buf17he5398a73dc5def27E
                      block  ;; label = @10
                        local.get 4
                        i32.load8_u offset=8
                        local.tee 7
                        i32.const 4
                        i32.ne
                        br_if 0 (;@10;)
                        local.get 1
                        i32.const 20
                        i32.add
                        i32.load
                        local.set 7
                        br 2 (;@8;)
                      end
                      local.get 0
                      local.get 4
                      i32.load offset=9 align=1
                      i32.store offset=1 align=1
                      local.get 0
                      i32.const 4
                      i32.add
                      local.get 4
                      i32.load offset=12 align=1
                      i32.store align=1
                      local.get 0
                      local.get 7
                      i32.store8
                      br 8 (;@1;)
                    end
                    i32.const 0
                    local.set 7
                    local.get 4
                    i64.load offset=8
                    local.tee 10
                    i32.wrap_i64
                    local.tee 8
                    i32.const 255
                    i32.and
                    i32.const 4
                    i32.ne
                    br_if 1 (;@7;)
                  end
                  local.get 2
                  local.get 6
                  i32.add
                  local.set 2
                  block  ;; label = @8
                    local.get 1
                    i32.const 16
                    i32.add
                    i32.load
                    local.get 7
                    i32.sub
                    local.get 3
                    local.get 6
                    i32.sub
                    local.tee 3
                    i32.gt_u
                    br_if 0 (;@8;)
                    local.get 0
                    local.get 5
                    local.get 2
                    local.get 3
                    call $_ZN3std2io8buffered9bufwriter18BufWriter$LT$W$GT$14write_all_cold17ha8846756f7f5598dE
                    br 7 (;@1;)
                  end
                  local.get 1
                  i32.load offset=12
                  local.get 7
                  i32.add
                  local.get 2
                  local.get 3
                  call $memcpy
                  drop
                  local.get 0
                  i32.const 4
                  i32.store8
                  local.get 1
                  i32.const 20
                  i32.add
                  local.get 7
                  local.get 3
                  i32.add
                  i32.store
                  br 6 (;@1;)
                end
                local.get 0
                local.get 8
                i32.store8
                local.get 0
                i32.const 7
                i32.add
                local.get 10
                i64.const 56
                i64.shr_u
                i64.store8
                local.get 0
                i32.const 5
                i32.add
                local.get 10
                i64.const 40
                i64.shr_u
                i64.store16 align=1
                local.get 0
                local.get 10
                i64.const 8
                i64.shr_u
                i64.store32 offset=1 align=1
                br 5 (;@1;)
              end
              local.get 1
              i32.load offset=12
              local.get 6
              i32.add
              local.get 2
              local.get 3
              call $memcpy
              drop
              local.get 0
              i32.const 4
              i32.store8
              local.get 1
              i32.const 20
              i32.add
              local.get 6
              local.get 3
              i32.add
              i32.store
              br 4 (;@1;)
            end
            local.get 0
            local.get 4
            i32.load offset=9 align=1
            i32.store offset=1 align=1
            local.get 0
            i32.const 4
            i32.add
            local.get 4
            i32.load offset=12 align=1
            i32.store align=1
            local.get 0
            local.get 6
            i32.store8
            br 3 (;@1;)
          end
          i32.const 1048772
          i32.const 16
          local.get 4
          i32.const 8
          i32.add
          i32.const 1048788
          i32.const 1050516
          call $_ZN4core6result13unwrap_failed17h0b834de6e1e67b38E
          unreachable
        end
        i32.const 1048805
        i32.const 35
        i32.const 1049668
        call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
        unreachable
      end
      local.get 0
      local.get 4
      i32.load offset=9 align=1
      i32.store offset=1 align=1
      local.get 0
      i32.const 4
      i32.add
      local.get 4
      i32.load offset=12 align=1
      i32.store align=1
      local.get 0
      local.get 7
      i32.store8
    end
    local.get 1
    local.get 1
    i32.load offset=8
    i32.const 1
    i32.add
    i32.store offset=8
    local.get 4
    i32.const 16
    i32.add
    global.set $__stack_pointer)
  (func $_ZN3std2io5Write9write_all17h80bc75e0ee5e76e4E (type 6) (param i32 i32 i32)
    (local i32 i64 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 2
        i32.eqz
        br_if 0 (;@2;)
        i32.const 1050664
        i64.extend_i32_u
        i64.const 32
        i64.shl
        i64.const 2
        i64.or
        local.set 4
        loop  ;; label = @3
          local.get 3
          local.get 2
          i32.store offset=12
          local.get 3
          local.get 1
          i32.store offset=8
          local.get 3
          i32.const 16
          i32.add
          i32.const 1
          local.get 3
          i32.const 8
          i32.add
          i32.const 1
          call $_ZN4wasi13lib_generated8fd_write17hb23bdba73a8f4709E
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 3
                  i32.load16_u offset=16
                  br_if 0 (;@7;)
                  local.get 3
                  i32.load offset=20
                  local.tee 5
                  i32.eqz
                  br_if 1 (;@6;)
                  local.get 2
                  local.get 5
                  i32.lt_u
                  br_if 2 (;@5;)
                  local.get 2
                  local.get 5
                  i32.sub
                  local.set 2
                  local.get 1
                  local.get 5
                  i32.add
                  local.set 1
                  br 3 (;@4;)
                end
                local.get 3
                local.get 3
                i32.load16_u offset=18
                i32.store16 offset=30
                local.get 3
                i32.const 30
                i32.add
                call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
                local.tee 5
                i32.const 65535
                i32.and
                call $_ZN3std3sys4wasi17decode_error_kind17h27b067431089650bE
                i32.const 255
                i32.and
                i32.const 35
                i32.eq
                br_if 2 (;@4;)
                local.get 5
                i64.extend_i32_u
                i64.const 65535
                i64.and
                i64.const 32
                i64.shl
                local.set 4
              end
              local.get 0
              local.get 4
              i64.store align=4
              br 4 (;@1;)
            end
            local.get 5
            local.get 2
            i32.const 1050620
            call $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E
            unreachable
          end
          local.get 2
          br_if 0 (;@3;)
        end
      end
      local.get 0
      i32.const 4
      i32.store8
    end
    local.get 3
    i32.const 32
    i32.add
    global.set $__stack_pointer)
  (func $_ZN4core3fmt5Write10write_char17h97088a3d641899fdE (type 5) (param i32 i32) (result i32)
    (local i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    i32.const 0
    i32.store offset=12
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 128
        i32.lt_u
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 1
          i32.const 2048
          i32.lt_u
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 1
            i32.const 65536
            i32.lt_u
            br_if 0 (;@4;)
            local.get 2
            local.get 1
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=15
            local.get 2
            local.get 1
            i32.const 6
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=14
            local.get 2
            local.get 1
            i32.const 12
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=13
            local.get 2
            local.get 1
            i32.const 18
            i32.shr_u
            i32.const 7
            i32.and
            i32.const 240
            i32.or
            i32.store8 offset=12
            i32.const 4
            local.set 3
            br 3 (;@1;)
          end
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=14
          local.get 2
          local.get 1
          i32.const 12
          i32.shr_u
          i32.const 224
          i32.or
          i32.store8 offset=12
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=13
          i32.const 3
          local.set 3
          br 2 (;@1;)
        end
        local.get 2
        local.get 1
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=13
        local.get 2
        local.get 1
        i32.const 6
        i32.shr_u
        i32.const 192
        i32.or
        i32.store8 offset=12
        i32.const 2
        local.set 3
        br 1 (;@1;)
      end
      local.get 2
      local.get 1
      i32.store8 offset=12
      i32.const 1
      local.set 3
    end
    block  ;; label = @1
      local.get 0
      i32.load
      local.tee 1
      i32.const 4
      i32.add
      i32.load
      local.get 1
      i32.load offset=8
      local.tee 0
      i32.sub
      local.get 3
      i32.ge_u
      br_if 0 (;@1;)
      local.get 1
      local.get 0
      local.get 3
      call $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$7reserve21do_reserve_and_handle17ha33bee7b1e5ebd92E
      local.get 1
      i32.load offset=8
      local.set 0
    end
    local.get 1
    i32.load
    local.get 0
    i32.add
    local.get 2
    i32.const 12
    i32.add
    local.get 3
    call $memcpy
    drop
    local.get 1
    local.get 0
    local.get 3
    i32.add
    i32.store offset=8
    local.get 2
    i32.const 16
    i32.add
    global.set $__stack_pointer
    i32.const 0)
  (func $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$7reserve21do_reserve_and_handle17ha33bee7b1e5ebd92E (type 6) (param i32 i32 i32)
    (local i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        local.get 2
        i32.add
        local.tee 2
        local.get 1
        i32.lt_u
        br_if 0 (;@2;)
        local.get 0
        i32.const 4
        i32.add
        i32.load
        local.tee 1
        i32.const 1
        i32.shl
        local.tee 4
        local.get 2
        local.get 4
        local.get 2
        i32.gt_u
        select
        local.tee 2
        i32.const 8
        local.get 2
        i32.const 8
        i32.gt_u
        select
        local.tee 2
        i32.const -1
        i32.xor
        i32.const 31
        i32.shr_u
        local.set 4
        block  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.eqz
            br_if 0 (;@4;)
            local.get 3
            i32.const 1
            i32.store offset=24
            local.get 3
            local.get 1
            i32.store offset=20
            local.get 3
            local.get 0
            i32.load
            i32.store offset=16
            br 1 (;@3;)
          end
          local.get 3
          i32.const 0
          i32.store offset=24
        end
        local.get 3
        local.get 2
        local.get 4
        local.get 3
        i32.const 16
        i32.add
        call $_ZN5alloc7raw_vec11finish_grow17h659a95ac63274bc4E
        local.get 3
        i32.load offset=4
        local.set 1
        block  ;; label = @3
          local.get 3
          i32.load
          br_if 0 (;@3;)
          local.get 0
          local.get 1
          i32.store
          local.get 0
          i32.const 4
          i32.add
          local.get 2
          i32.store
          br 2 (;@1;)
        end
        local.get 3
        i32.const 8
        i32.add
        i32.load
        local.tee 0
        i32.const -2147483647
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        local.get 0
        call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
        unreachable
      end
      call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
      unreachable
    end
    local.get 3
    i32.const 32
    i32.add
    global.set $__stack_pointer)
  (func $_ZN4core3fmt5Write9write_fmt17h0e875200992553d8E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 0
    i32.store offset=4
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 2
    i32.const 4
    i32.add
    i32.const 1048676
    local.get 2
    i32.const 8
    i32.add
    call $_ZN4core3fmt5write17hb15fae9d0358166aE
    local.set 1
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN4core3fmt5Write9write_fmt17h59edcb38a9a5b8b6E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 0
    i32.store offset=4
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 2
    i32.const 4
    i32.add
    i32.const 1048724
    local.get 2
    i32.const 8
    i32.add
    call $_ZN4core3fmt5write17hb15fae9d0358166aE
    local.set 1
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN4core3fmt5Write9write_fmt17hc95f6c38645c7207E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 0
    i32.store offset=4
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 2
    i32.const 4
    i32.add
    i32.const 1048748
    local.get 2
    i32.const 8
    i32.add
    call $_ZN4core3fmt5write17hb15fae9d0358166aE
    local.set 1
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN3std9panicking12default_hook17h15ece0a870cadf43E (type 1) (param i32)
    (local i32 i32 i32 i64 i32)
    global.get $__stack_pointer
    i32.const 96
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    i32.const 1
    local.set 2
    block  ;; label = @1
      i32.const 0
      i32.load offset=1059436
      i32.const 1
      i32.gt_u
      br_if 0 (;@1;)
      call $_ZN3std5panic19get_backtrace_style17h0c1afb8971b741b4E
      i32.const 255
      i32.and
      local.set 2
    end
    local.get 1
    local.get 2
    i32.store8 offset=19
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            call $_ZN4core5panic10panic_info9PanicInfo8location17he55e718ad5bfc141E
            local.tee 2
            i32.eqz
            br_if 0 (;@4;)
            local.get 1
            local.get 2
            i32.store offset=20
            local.get 1
            i32.const 8
            i32.add
            local.get 0
            call $_ZN4core5panic10panic_info9PanicInfo7payload17hfd9d0ca104403304E
            i32.const 12
            local.set 3
            local.get 1
            i32.load offset=8
            local.tee 2
            local.get 1
            i32.load offset=12
            i32.const 12
            i32.add
            i32.load
            call_indirect (type 2)
            local.set 4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    local.get 2
                    i32.eqz
                    br_if 0 (;@8;)
                    local.get 4
                    i64.const 8378048562631854315
                    i64.eq
                    br_if 1 (;@7;)
                  end
                  local.get 1
                  local.get 0
                  call $_ZN4core5panic10panic_info9PanicInfo7payload17hfd9d0ca104403304E
                  i32.const 1051576
                  local.set 0
                  local.get 1
                  i32.load
                  local.tee 2
                  local.get 1
                  i32.load offset=4
                  i32.const 12
                  i32.add
                  i32.load
                  call_indirect (type 2)
                  local.set 4
                  local.get 2
                  i32.eqz
                  br_if 2 (;@5;)
                  local.get 4
                  i64.const 5100591953695616150
                  i64.ne
                  br_if 2 (;@5;)
                  local.get 2
                  i32.const 8
                  i32.add
                  local.set 0
                  br 1 (;@6;)
                end
                local.get 2
                i32.const 4
                i32.add
                local.set 0
              end
              local.get 0
              i32.load
              local.set 3
              local.get 2
              i32.load
              local.set 0
            end
            local.get 1
            local.get 3
            i32.store offset=28
            local.get 1
            local.get 0
            i32.store offset=24
            i32.const 9
            local.set 0
            i32.const 1051588
            local.set 3
            block  ;; label = @5
              call $_ZN3std10sys_common11thread_info14current_thread17h2a6a795d0e8df1beE
              local.tee 2
              i32.eqz
              br_if 0 (;@5;)
              local.get 2
              i32.const 16
              i32.add
              i32.load
              local.tee 5
              i32.eqz
              br_if 0 (;@5;)
              local.get 2
              i32.const 20
              i32.add
              i32.load
              i32.const -1
              i32.add
              local.set 0
              local.get 5
              local.set 3
            end
            local.get 1
            local.get 0
            i32.store offset=36
            local.get 1
            local.get 3
            i32.store offset=32
            local.get 1
            local.get 1
            i32.const 19
            i32.add
            i32.store offset=52
            local.get 1
            local.get 1
            i32.const 20
            i32.add
            i32.store offset=48
            local.get 1
            local.get 1
            i32.const 24
            i32.add
            i32.store offset=44
            local.get 1
            local.get 1
            i32.const 32
            i32.add
            i32.store offset=40
            i32.const 0
            i32.load8_u offset=1059336
            i32.eqz
            br_if 2 (;@2;)
            i32.const 0
            i32.const 1
            i32.store8 offset=1059336
            block  ;; label = @5
              i32.const 0
              i32.load8_u offset=1059416
              br_if 0 (;@5;)
              i32.const 0
              i32.const 1
              i32.store8 offset=1059416
              i32.const 0
              i32.const 0
              i32.store offset=1059420
              br 3 (;@2;)
            end
            i32.const 0
            i32.load offset=1059420
            local.set 0
            i32.const 0
            i32.const 0
            i32.store offset=1059420
            local.get 0
            i32.eqz
            br_if 2 (;@2;)
            local.get 0
            i32.load8_u offset=8
            local.set 3
            local.get 0
            i32.const 1
            i32.store8 offset=8
            local.get 1
            local.get 3
            i32.const 1
            i32.and
            local.tee 3
            i32.store8 offset=63
            local.get 3
            br_if 1 (;@3;)
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  i32.const 0
                  i32.load offset=1059404
                  i32.const 2147483647
                  i32.and
                  i32.eqz
                  br_if 0 (;@7;)
                  call $_ZN3std9panicking11panic_count17is_zero_slow_path17hea85e77ac3ac7cbaE
                  local.set 3
                  local.get 1
                  i32.const 40
                  i32.add
                  local.get 0
                  i32.const 12
                  i32.add
                  i32.const 6
                  call $_ZN3std9panicking12default_hook28_$u7b$$u7b$closure$u7d$$u7d$17h142c39c8f696cd66E
                  local.get 3
                  br_if 1 (;@6;)
                  br 2 (;@5;)
                end
                local.get 1
                i32.const 40
                i32.add
                local.get 0
                i32.const 12
                i32.add
                i32.const 6
                call $_ZN3std9panicking12default_hook28_$u7b$$u7b$closure$u7d$$u7d$17h142c39c8f696cd66E
              end
              i32.const 0
              i32.load offset=1059404
              i32.const 2147483647
              i32.and
              i32.eqz
              br_if 0 (;@5;)
              call $_ZN3std9panicking11panic_count17is_zero_slow_path17hea85e77ac3ac7cbaE
              br_if 0 (;@5;)
              local.get 0
              i32.const 1
              i32.store8 offset=9
            end
            i32.const 0
            i32.const 1
            i32.store8 offset=1059336
            local.get 0
            i32.const 0
            i32.store8 offset=8
            block  ;; label = @5
              i32.const 0
              i32.load8_u offset=1059416
              br_if 0 (;@5;)
              i32.const 0
              local.get 0
              i32.store offset=1059420
              i32.const 0
              i32.const 1
              i32.store8 offset=1059416
              br 4 (;@1;)
            end
            i32.const 0
            i32.load offset=1059420
            local.set 3
            i32.const 0
            local.get 0
            i32.store offset=1059420
            local.get 3
            i32.eqz
            br_if 3 (;@1;)
            local.get 3
            local.get 3
            i32.load
            local.tee 0
            i32.const -1
            i32.add
            i32.store
            local.get 0
            i32.const 1
            i32.ne
            br_if 3 (;@1;)
            local.get 3
            call $_ZN5alloc4sync12Arc$LT$T$GT$9drop_slow17h3851a9e5c952beabE
            br 3 (;@1;)
          end
          i32.const 1048872
          i32.const 43
          i32.const 1051600
          call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
          unreachable
        end
        local.get 1
        i32.const 0
        i32.store offset=84
        local.get 1
        i32.const 1048772
        i32.store offset=80
        local.get 1
        i64.const 1
        i64.store offset=68 align=4
        local.get 1
        i32.const 1052244
        i32.store offset=64
        local.get 1
        i32.const 63
        i32.add
        local.get 1
        i32.const 64
        i32.add
        call $_ZN4core9panicking13assert_failed17had14dc5ddcedac37E
        unreachable
      end
      local.get 1
      i32.const 40
      i32.add
      local.get 1
      i32.const 88
      i32.add
      i32.const 7
      call $_ZN3std9panicking12default_hook28_$u7b$$u7b$closure$u7d$$u7d$17h142c39c8f696cd66E
    end
    block  ;; label = @1
      local.get 2
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      local.get 2
      i32.load
      local.tee 0
      i32.const -1
      i32.add
      i32.store
      local.get 0
      i32.const 1
      i32.ne
      br_if 0 (;@1;)
      local.get 2
      call $_ZN5alloc4sync12Arc$LT$T$GT$9drop_slow17h9861dbe28414a55fE
    end
    local.get 1
    i32.const 96
    i32.add
    global.set $__stack_pointer)
  (func $_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h0236a7ae6988917eE (type 3) (param i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 0
    i32.load
    i32.store offset=12
    local.get 2
    i32.const 12
    i32.add
    local.get 2
    call $_ZN3std4sync4once4Once9call_once28_$u7b$$u7b$closure$u7d$$u7d$17hfe44afed107c6e44E
    local.get 2
    i32.const 16
    i32.add
    global.set $__stack_pointer)
  (func $_ZN3std4sync4once4Once9call_once28_$u7b$$u7b$closure$u7d$$u7d$17hfe44afed107c6e44E (type 3) (param i32 i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 0
    i32.load
    local.tee 0
    i32.load8_u
    local.set 3
    local.get 0
    i32.const 0
    i32.store8
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 3
          i32.const 1
          i32.and
          i32.eqz
          br_if 0 (;@3;)
          local.get 2
          i32.const 0
          i32.store8 offset=15
          block  ;; label = @4
            i32.const 0
            i32.load offset=1059340
            i32.const 3
            i32.eq
            br_if 0 (;@4;)
            local.get 2
            i32.const 15
            i32.add
            call $_ZN3std4sync9once_lock17OnceLock$LT$T$GT$10initialize17ha52c4cb6401dc2a7E
          end
          block  ;; label = @4
            local.get 2
            i32.load8_u offset=15
            br_if 0 (;@4;)
            block  ;; label = @5
              block  ;; label = @6
                i32.const 0
                i32.load offset=1059344
                i32.const 1059424
                i32.eq
                br_if 0 (;@6;)
                i32.const 0
                i32.load8_u offset=1059372
                local.set 3
                i32.const 1
                local.set 0
                i32.const 0
                i32.const 1
                i32.store8 offset=1059372
                local.get 3
                i32.const 1
                i32.and
                br_if 2 (;@4;)
                i32.const 0
                i32.const 1059424
                i32.store offset=1059344
                br 1 (;@5;)
              end
              i32.const 0
              i32.load offset=1059348
              i32.const 1
              i32.add
              local.tee 0
              i32.eqz
              br_if 3 (;@2;)
            end
            i32.const 0
            local.get 0
            i32.store offset=1059348
            i32.const 0
            i32.load offset=1059352
            br_if 3 (;@1;)
            i32.const 0
            i32.const -1
            i32.store offset=1059352
            block  ;; label = @5
              i32.const 0
              i32.load8_u offset=1059368
              br_if 0 (;@5;)
              local.get 2
              i32.const 16
              i32.add
              i32.const 1059356
              call $_ZN3std2io8buffered9bufwriter18BufWriter$LT$W$GT$9flush_buf17he5398a73dc5def27E
              local.get 2
              i32.load8_u offset=16
              i32.const 3
              i32.ne
              br_if 0 (;@5;)
              local.get 2
              i32.load offset=20
              local.tee 0
              i32.load
              local.get 0
              i32.load offset=4
              i32.load
              call_indirect (type 1)
              block  ;; label = @6
                local.get 0
                i32.load offset=4
                local.tee 3
                i32.const 4
                i32.add
                i32.load
                local.tee 4
                i32.eqz
                br_if 0 (;@6;)
                local.get 0
                i32.load
                local.get 4
                local.get 3
                i32.const 8
                i32.add
                i32.load
                call $__rust_dealloc
              end
              local.get 0
              i32.const 12
              i32.const 4
              call $__rust_dealloc
            end
            block  ;; label = @5
              i32.const 0
              i32.load offset=1059360
              local.tee 0
              i32.eqz
              br_if 0 (;@5;)
              i32.const 0
              i32.load offset=1059356
              local.get 0
              i32.const 1
              call $__rust_dealloc
            end
            i32.const 0
            i64.const 0
            i64.store offset=1059360 align=4
            i32.const 0
            i32.const 1
            i32.store offset=1059356
            i32.const 0
            i32.const 0
            i32.load offset=1059352
            i32.const 1
            i32.add
            i32.store offset=1059352
            i32.const 0
            i32.const 0
            i32.load offset=1059348
            i32.const -1
            i32.add
            local.tee 0
            i32.store offset=1059348
            i32.const 0
            i32.const 0
            i32.store8 offset=1059368
            local.get 0
            br_if 0 (;@4;)
            i32.const 0
            i32.const 0
            i32.store8 offset=1059372
            i32.const 0
            i32.const 0
            i32.store offset=1059344
          end
          local.get 2
          i32.const 32
          i32.add
          global.set $__stack_pointer
          return
        end
        i32.const 1048872
        i32.const 43
        i32.const 1050868
        call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
        unreachable
      end
      i32.const 1051240
      i32.const 38
      i32.const 1051316
      call $_ZN4core6option13expect_failed17hc52795d163105200E
      unreachable
    end
    i32.const 1048772
    i32.const 16
    local.get 2
    i32.const 24
    i32.add
    i32.const 1048788
    i32.const 1050500
    call $_ZN4core6result13unwrap_failed17h0b834de6e1e67b38E
    unreachable)
  (func $_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h2af9265a14d6f21aE (type 3) (param i32 i32)
    (local i32)
    local.get 0
    i32.load
    local.tee 0
    i32.load
    local.set 2
    local.get 0
    i32.const 0
    i32.store
    block  ;; label = @1
      local.get 2
      br_if 0 (;@1;)
      i32.const 1048872
      i32.const 43
      i32.const 1050940
      call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
      unreachable
    end
    local.get 0
    i32.load offset=4
    local.set 0
    local.get 2
    i32.const 1
    i32.store8
    local.get 0
    i64.const 0
    i64.store offset=16 align=4
    local.get 0
    i64.const 4294967296
    i64.store offset=8 align=4
    local.get 0
    i64.const 0
    i64.store align=4
    local.get 0
    i32.const 24
    i32.add
    i32.const 0
    i32.store8
    local.get 0
    i32.const 0
    i32.store8 offset=28)
  (func $_ZN4core9panicking13assert_failed17had14dc5ddcedac37E (type 3) (param i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    i32.const 1048804
    i32.store offset=4
    local.get 2
    local.get 0
    i32.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    i32.const 0
    local.get 2
    i32.const 1048840
    local.get 2
    i32.const 4
    i32.add
    i32.const 1048840
    local.get 2
    i32.const 8
    i32.add
    i32.const 1052308
    call $_ZN4core9panicking19assert_failed_inner17h354c863259f57591E
    unreachable)
  (func $_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17hb0cf4eba839d9459E (type 3) (param i32 i32)
    (local i32)
    local.get 0
    i32.load
    local.tee 2
    i32.load
    local.set 0
    local.get 2
    i32.const 0
    i32.store
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.eqz
        br_if 0 (;@2;)
        i32.const 1024
        i32.const 1
        call $__rust_alloc
        local.tee 2
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        i32.const 0
        i32.store8 offset=28
        local.get 0
        i32.const 0
        i32.store8 offset=24
        local.get 0
        i64.const 1024
        i64.store offset=16 align=4
        local.get 0
        local.get 2
        i32.store offset=12
        local.get 0
        i32.const 0
        i32.store offset=8
        local.get 0
        i64.const 0
        i64.store align=4
        return
      end
      i32.const 1048872
      i32.const 43
      i32.const 1050940
      call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
      unreachable
    end
    i32.const 1024
    i32.const 1
    call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
    unreachable)
  (func $_ZN4core3ptr100drop_in_place$LT$$RF$mut$u20$std..io..Write..write_fmt..Adapter$LT$alloc..vec..Vec$LT$u8$GT$$GT$$GT$17h5828119147893f65E (type 1) (param i32))
  (func $_ZN4core3ptr103drop_in_place$LT$std..sync..poison..PoisonError$LT$std..sync..mutex..MutexGuard$LT$$LP$$RP$$GT$$GT$$GT$17h5b5482fc05b44f80E (type 1) (param i32)
    (local i32)
    local.get 0
    i32.load
    local.set 1
    block  ;; label = @1
      local.get 0
      i32.const 4
      i32.add
      i32.load8_u
      br_if 0 (;@1;)
      i32.const 0
      i32.load offset=1059404
      i32.const 2147483647
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      call $_ZN3std9panicking11panic_count17is_zero_slow_path17hea85e77ac3ac7cbaE
      br_if 0 (;@1;)
      local.get 1
      i32.const 1
      i32.store8 offset=1
    end
    local.get 1
    i32.const 0
    i32.store8)
  (func $_ZN3std9panicking11panic_count17is_zero_slow_path17hea85e77ac3ac7cbaE (type 10) (result i32)
    i32.const 0
    i32.load offset=1059436
    i32.eqz)
  (func $_ZN4core3ptr205drop_in_place$LT$$LT$alloc..boxed..Box$LT$dyn$u20$core..error..Error$u2b$core..marker..Send$u2b$core..marker..Sync$GT$$u20$as$u20$core..convert..From$LT$alloc..string..String$GT$$GT$..from..StringError$GT$17h285fd2a0e5d852c2E (type 1) (param i32)
    (local i32)
    block  ;; label = @1
      local.get 0
      i32.const 4
      i32.add
      i32.load
      local.tee 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.load
      local.get 1
      i32.const 1
      call $__rust_dealloc
    end)
  (func $_ZN5alloc4sync12Arc$LT$T$GT$9drop_slow17h9861dbe28414a55fE (type 1) (param i32)
    (local i32)
    block  ;; label = @1
      local.get 0
      i32.const 16
      i32.add
      i32.load
      local.tee 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 1
      i32.const 0
      i32.store8
      local.get 0
      i32.const 20
      i32.add
      i32.load
      local.tee 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.load offset=16
      local.get 1
      i32.const 1
      call $__rust_dealloc
    end
    block  ;; label = @1
      local.get 0
      i32.const -1
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      local.get 0
      i32.load offset=4
      local.tee 1
      i32.const -1
      i32.add
      i32.store offset=4
      local.get 1
      i32.const 1
      i32.ne
      br_if 0 (;@1;)
      local.get 0
      i32.const 32
      i32.const 8
      call $__rust_dealloc
    end)
  (func $_ZN4core3ptr70drop_in_place$LT$std..panicking..begin_panic_handler..PanicPayload$GT$17h1eda4e4f8bd9f813E (type 1) (param i32)
    (local i32)
    block  ;; label = @1
      local.get 0
      i32.load offset=4
      local.tee 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const 8
      i32.add
      i32.load
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 1
      local.get 0
      i32.const 1
      call $__rust_dealloc
    end)
  (func $_ZN4core3ptr81drop_in_place$LT$core..result..Result$LT$$LP$$RP$$C$std..io..error..Error$GT$$GT$17h7ea204518b820d95E (type 1) (param i32)
    (local i32 i32 i32)
    block  ;; label = @1
      local.get 0
      i32.load8_u
      i32.const 3
      i32.ne
      br_if 0 (;@1;)
      local.get 0
      i32.load offset=4
      local.tee 1
      i32.load
      local.get 1
      i32.load offset=4
      i32.load
      call_indirect (type 1)
      block  ;; label = @2
        local.get 1
        i32.load offset=4
        local.tee 2
        i32.const 4
        i32.add
        i32.load
        local.tee 3
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        i32.load
        local.get 3
        local.get 2
        i32.const 8
        i32.add
        i32.load
        call $__rust_dealloc
      end
      local.get 0
      i32.load offset=4
      i32.const 12
      i32.const 4
      call $__rust_dealloc
    end)
  (func $_ZN4core3ptr88drop_in_place$LT$std..io..Write..write_fmt..Adapter$LT$alloc..vec..Vec$LT$u8$GT$$GT$$GT$17h418268d2ec1c31e1E (type 1) (param i32)
    (local i32 i32 i32)
    block  ;; label = @1
      local.get 0
      i32.load8_u offset=4
      i32.const 3
      i32.ne
      br_if 0 (;@1;)
      local.get 0
      i32.const 8
      i32.add
      i32.load
      local.tee 1
      i32.load
      local.get 1
      i32.load offset=4
      i32.load
      call_indirect (type 1)
      block  ;; label = @2
        local.get 1
        i32.load offset=4
        local.tee 2
        i32.const 4
        i32.add
        i32.load
        local.tee 3
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        i32.load
        local.get 3
        local.get 2
        i32.const 8
        i32.add
        i32.load
        call $__rust_dealloc
      end
      local.get 0
      i32.load offset=8
      i32.const 12
      i32.const 4
      call $__rust_dealloc
    end)
  (func $_ZN4core9panicking13assert_failed17hef7b799fcd44f104E (type 6) (param i32 i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    i32.const 1051072
    i32.store offset=4
    local.get 3
    local.get 0
    i32.store
    local.get 3
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 3
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 3
    local.get 1
    i64.load align=4
    i64.store offset=8
    i32.const 0
    local.get 3
    i32.const 1048856
    local.get 3
    i32.const 4
    i32.add
    i32.const 1048856
    local.get 3
    i32.const 8
    i32.add
    local.get 2
    call $_ZN4core9panicking19assert_failed_inner17h354c863259f57591E
    unreachable)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$10write_char17h04c2b0631800289eE (type 5) (param i32 i32) (result i32)
    (local i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 0
    i32.load
    local.set 0
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.const 128
            i32.lt_u
            br_if 0 (;@4;)
            local.get 2
            i32.const 0
            i32.store offset=12
            local.get 1
            i32.const 2048
            i32.ge_u
            br_if 1 (;@3;)
            local.get 2
            local.get 1
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=13
            local.get 2
            local.get 1
            i32.const 6
            i32.shr_u
            i32.const 192
            i32.or
            i32.store8 offset=12
            i32.const 2
            local.set 1
            br 2 (;@2;)
          end
          block  ;; label = @4
            local.get 0
            i32.load offset=8
            local.tee 3
            local.get 0
            i32.load offset=4
            i32.ne
            br_if 0 (;@4;)
            local.get 0
            local.get 3
            call $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$16reserve_for_push17h0a228214ded733fcE
            local.get 0
            i32.load offset=8
            local.set 3
          end
          local.get 0
          local.get 3
          i32.const 1
          i32.add
          i32.store offset=8
          local.get 0
          i32.load
          local.get 3
          i32.add
          local.get 1
          i32.store8
          br 2 (;@1;)
        end
        block  ;; label = @3
          local.get 1
          i32.const 65536
          i32.lt_u
          br_if 0 (;@3;)
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=15
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=14
          local.get 2
          local.get 1
          i32.const 12
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=13
          local.get 2
          local.get 1
          i32.const 18
          i32.shr_u
          i32.const 7
          i32.and
          i32.const 240
          i32.or
          i32.store8 offset=12
          i32.const 4
          local.set 1
          br 1 (;@2;)
        end
        local.get 2
        local.get 1
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=14
        local.get 2
        local.get 1
        i32.const 12
        i32.shr_u
        i32.const 224
        i32.or
        i32.store8 offset=12
        local.get 2
        local.get 1
        i32.const 6
        i32.shr_u
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=13
        i32.const 3
        local.set 1
      end
      block  ;; label = @2
        local.get 0
        i32.const 4
        i32.add
        i32.load
        local.get 0
        i32.load offset=8
        local.tee 3
        i32.sub
        local.get 1
        i32.ge_u
        br_if 0 (;@2;)
        local.get 0
        local.get 3
        local.get 1
        call $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$7reserve21do_reserve_and_handle17ha33bee7b1e5ebd92E
        local.get 0
        i32.load offset=8
        local.set 3
      end
      local.get 0
      i32.load
      local.get 3
      i32.add
      local.get 2
      i32.const 12
      i32.add
      local.get 1
      call $memcpy
      drop
      local.get 0
      local.get 3
      local.get 1
      i32.add
      i32.store offset=8
    end
    local.get 2
    i32.const 16
    i32.add
    global.set $__stack_pointer
    i32.const 0)
  (func $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$16reserve_for_push17h0a228214ded733fcE (type 3) (param i32 i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 1
        i32.add
        local.tee 1
        i32.eqz
        br_if 0 (;@2;)
        local.get 0
        i32.const 4
        i32.add
        i32.load
        local.tee 3
        i32.const 1
        i32.shl
        local.tee 4
        local.get 1
        local.get 4
        local.get 1
        i32.gt_u
        select
        local.tee 1
        i32.const 8
        local.get 1
        i32.const 8
        i32.gt_u
        select
        local.tee 1
        i32.const -1
        i32.xor
        i32.const 31
        i32.shr_u
        local.set 4
        block  ;; label = @3
          block  ;; label = @4
            local.get 3
            i32.eqz
            br_if 0 (;@4;)
            local.get 2
            i32.const 1
            i32.store offset=24
            local.get 2
            local.get 3
            i32.store offset=20
            local.get 2
            local.get 0
            i32.load
            i32.store offset=16
            br 1 (;@3;)
          end
          local.get 2
          i32.const 0
          i32.store offset=24
        end
        local.get 2
        local.get 1
        local.get 4
        local.get 2
        i32.const 16
        i32.add
        call $_ZN5alloc7raw_vec11finish_grow17h659a95ac63274bc4E
        local.get 2
        i32.load offset=4
        local.set 3
        block  ;; label = @3
          local.get 2
          i32.load
          br_if 0 (;@3;)
          local.get 0
          local.get 3
          i32.store
          local.get 0
          i32.const 4
          i32.add
          local.get 1
          i32.store
          br 2 (;@1;)
        end
        local.get 2
        i32.const 8
        i32.add
        i32.load
        local.tee 0
        i32.const -2147483647
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        local.get 0
        call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
        unreachable
      end
      call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
      unreachable
    end
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$10write_char17h1afccb88c028ec33E (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.get 1
    call $_ZN4core3fmt5Write10write_char17h612343524e950315E)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$10write_char17h55d67e40e509f4c4E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 0
    i32.load
    local.set 0
    local.get 2
    i32.const 0
    i32.store offset=12
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 128
        i32.lt_u
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 1
          i32.const 2048
          i32.lt_u
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 1
            i32.const 65536
            i32.lt_u
            br_if 0 (;@4;)
            local.get 2
            local.get 1
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=15
            local.get 2
            local.get 1
            i32.const 6
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=14
            local.get 2
            local.get 1
            i32.const 12
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=13
            local.get 2
            local.get 1
            i32.const 18
            i32.shr_u
            i32.const 7
            i32.and
            i32.const 240
            i32.or
            i32.store8 offset=12
            i32.const 4
            local.set 1
            br 3 (;@1;)
          end
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=14
          local.get 2
          local.get 1
          i32.const 12
          i32.shr_u
          i32.const 224
          i32.or
          i32.store8 offset=12
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=13
          i32.const 3
          local.set 1
          br 2 (;@1;)
        end
        local.get 2
        local.get 1
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=13
        local.get 2
        local.get 1
        i32.const 6
        i32.shr_u
        i32.const 192
        i32.or
        i32.store8 offset=12
        i32.const 2
        local.set 1
        br 1 (;@1;)
      end
      local.get 2
      local.get 1
      i32.store8 offset=12
      i32.const 1
      local.set 1
    end
    local.get 0
    local.get 2
    i32.const 12
    i32.add
    local.get 1
    call $_ZN80_$LT$std..io..Write..write_fmt..Adapter$LT$T$GT$$u20$as$u20$core..fmt..Write$GT$9write_str17h9cbe34749d4b82e2E
    local.set 1
    local.get 2
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$10write_char17h651caa0da3d206a5E (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.get 1
    call $_ZN4core3fmt5Write10write_char17h97088a3d641899fdE)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_fmt17h540a0dde17fe2f09E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 0
    i32.load
    i32.store offset=4
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 2
    i32.const 4
    i32.add
    i32.const 1048676
    local.get 2
    i32.const 8
    i32.add
    call $_ZN4core3fmt5write17hb15fae9d0358166aE
    local.set 1
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_fmt17h7edb6a52b8fc0202E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 0
    i32.load
    i32.store offset=4
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 2
    i32.const 4
    i32.add
    i32.const 1048700
    local.get 2
    i32.const 8
    i32.add
    call $_ZN4core3fmt5write17hb15fae9d0358166aE
    local.set 1
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_fmt17h9618ee3043cc2dfeE (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 0
    i32.load
    i32.store offset=4
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 2
    i32.const 4
    i32.add
    i32.const 1048724
    local.get 2
    i32.const 8
    i32.add
    call $_ZN4core3fmt5write17hb15fae9d0358166aE
    local.set 1
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_fmt17ha0cd0a34539708e6E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 0
    i32.load
    i32.store offset=4
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 2
    i32.const 4
    i32.add
    i32.const 1048748
    local.get 2
    i32.const 8
    i32.add
    call $_ZN4core3fmt5write17hb15fae9d0358166aE
    local.set 1
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_str17h6440f892aa9617a1E (type 7) (param i32 i32 i32) (result i32)
    (local i32 i64 i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    i32.const 8
    i32.add
    local.get 0
    i32.load
    local.tee 0
    i32.load
    local.get 1
    local.get 2
    call $_ZN61_$LT$std..io..stdio..StdoutLock$u20$as$u20$std..io..Write$GT$9write_all17h7430178078d1718bE
    block  ;; label = @1
      local.get 3
      i32.load8_u offset=8
      local.tee 2
      i32.const 4
      i32.eq
      br_if 0 (;@1;)
      local.get 3
      i64.load offset=8
      local.set 4
      block  ;; label = @2
        local.get 0
        i32.load8_u offset=4
        i32.const 3
        i32.ne
        br_if 0 (;@2;)
        local.get 0
        i32.const 8
        i32.add
        i32.load
        local.tee 1
        i32.load
        local.get 1
        i32.load offset=4
        i32.load
        call_indirect (type 1)
        block  ;; label = @3
          local.get 1
          i32.load offset=4
          local.tee 5
          i32.const 4
          i32.add
          i32.load
          local.tee 6
          i32.eqz
          br_if 0 (;@3;)
          local.get 1
          i32.load
          local.get 6
          local.get 5
          i32.const 8
          i32.add
          i32.load
          call $__rust_dealloc
        end
        local.get 1
        i32.const 12
        i32.const 4
        call $__rust_dealloc
      end
      local.get 0
      local.get 4
      i64.store offset=4 align=4
    end
    local.get 3
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 2
    i32.const 4
    i32.ne)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_str17h7184d3b710c1fad9E (type 7) (param i32 i32 i32) (result i32)
    (local i32)
    block  ;; label = @1
      local.get 0
      i32.load
      i32.load
      local.tee 0
      i32.const 4
      i32.add
      i32.load
      local.get 0
      i32.load offset=8
      local.tee 3
      i32.sub
      local.get 2
      i32.ge_u
      br_if 0 (;@1;)
      local.get 0
      local.get 3
      local.get 2
      call $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$7reserve21do_reserve_and_handle17ha33bee7b1e5ebd92E
      local.get 0
      i32.load offset=8
      local.set 3
    end
    local.get 0
    i32.load
    local.get 3
    i32.add
    local.get 1
    local.get 2
    call $memcpy
    drop
    local.get 0
    local.get 3
    local.get 2
    i32.add
    i32.store offset=8
    i32.const 0)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_str17ha5c0e59281498a78E (type 7) (param i32 i32 i32) (result i32)
    local.get 0
    i32.load
    local.get 1
    local.get 2
    call $_ZN80_$LT$std..io..Write..write_fmt..Adapter$LT$T$GT$$u20$as$u20$core..fmt..Write$GT$9write_str17h9cbe34749d4b82e2E)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_str17haa5048c1ee515ba8E (type 7) (param i32 i32 i32) (result i32)
    (local i32)
    block  ;; label = @1
      local.get 0
      i32.load
      local.tee 0
      i32.const 4
      i32.add
      i32.load
      local.get 0
      i32.load offset=8
      local.tee 3
      i32.sub
      local.get 2
      i32.ge_u
      br_if 0 (;@1;)
      local.get 0
      local.get 3
      local.get 2
      call $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$7reserve21do_reserve_and_handle17ha33bee7b1e5ebd92E
      local.get 0
      i32.load offset=8
      local.set 3
    end
    local.get 0
    i32.load
    local.get 3
    i32.add
    local.get 1
    local.get 2
    call $memcpy
    drop
    local.get 0
    local.get 3
    local.get 2
    i32.add
    i32.store offset=8
    i32.const 0)
  (func $_ZN5alloc4sync12Arc$LT$T$GT$9drop_slow17h3851a9e5c952beabE (type 1) (param i32)
    (local i32)
    block  ;; label = @1
      local.get 0
      i32.const 16
      i32.add
      i32.load
      local.tee 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const 12
      i32.add
      i32.load
      local.get 1
      i32.const 1
      call $__rust_dealloc
    end
    block  ;; label = @1
      local.get 0
      i32.const -1
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      local.get 0
      i32.load offset=4
      local.tee 1
      i32.const -1
      i32.add
      i32.store offset=4
      local.get 1
      i32.const 1
      i32.ne
      br_if 0 (;@1;)
      local.get 0
      i32.const 24
      i32.const 4
      call $__rust_dealloc
    end)
  (func $_ZN5alloc7raw_vec11finish_grow17h659a95ac63274bc4E (type 9) (param i32 i32 i32 i32)
    (local i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 2
        i32.eqz
        br_if 0 (;@2;)
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 1
                  i32.const 0
                  i32.lt_s
                  br_if 0 (;@7;)
                  local.get 3
                  i32.load offset=8
                  i32.eqz
                  br_if 2 (;@5;)
                  local.get 3
                  i32.load offset=4
                  local.tee 4
                  br_if 1 (;@6;)
                  local.get 1
                  br_if 3 (;@4;)
                  local.get 2
                  local.set 3
                  br 4 (;@3;)
                end
                local.get 0
                i32.const 8
                i32.add
                i32.const 0
                i32.store
                br 5 (;@1;)
              end
              local.get 3
              i32.load
              local.get 4
              local.get 2
              local.get 1
              call $__rust_realloc
              local.set 3
              br 2 (;@3;)
            end
            local.get 1
            br_if 0 (;@4;)
            local.get 2
            local.set 3
            br 1 (;@3;)
          end
          local.get 1
          local.get 2
          call $__rust_alloc
          local.set 3
        end
        block  ;; label = @3
          local.get 3
          i32.eqz
          br_if 0 (;@3;)
          local.get 0
          local.get 3
          i32.store offset=4
          local.get 0
          i32.const 8
          i32.add
          local.get 1
          i32.store
          local.get 0
          i32.const 0
          i32.store
          return
        end
        local.get 0
        local.get 1
        i32.store offset=4
        local.get 0
        i32.const 8
        i32.add
        local.get 2
        i32.store
        br 1 (;@1;)
      end
      local.get 0
      local.get 1
      i32.store offset=4
      local.get 0
      i32.const 8
      i32.add
      i32.const 0
      i32.store
    end
    local.get 0
    i32.const 1
    i32.store)
  (func $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$16reserve_for_push17h7fbb5eb56ac0a9b2E (type 3) (param i32 i32)
    (local i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 1
        i32.add
        local.tee 1
        i32.eqz
        br_if 0 (;@2;)
        local.get 0
        i32.const 4
        i32.add
        i32.load
        local.tee 3
        i32.const 1
        i32.shl
        local.tee 4
        local.get 1
        local.get 4
        local.get 1
        i32.gt_u
        select
        local.tee 1
        i32.const 4
        local.get 1
        i32.const 4
        i32.gt_u
        select
        local.tee 1
        i32.const 12
        i32.mul
        local.set 4
        local.get 1
        i32.const 178956971
        i32.lt_u
        i32.const 2
        i32.shl
        local.set 5
        block  ;; label = @3
          block  ;; label = @4
            local.get 3
            i32.eqz
            br_if 0 (;@4;)
            local.get 2
            i32.const 4
            i32.store offset=24
            local.get 2
            local.get 3
            i32.const 12
            i32.mul
            i32.store offset=20
            local.get 2
            local.get 0
            i32.load
            i32.store offset=16
            br 1 (;@3;)
          end
          local.get 2
          i32.const 0
          i32.store offset=24
        end
        local.get 2
        local.get 4
        local.get 5
        local.get 2
        i32.const 16
        i32.add
        call $_ZN5alloc7raw_vec11finish_grow17h659a95ac63274bc4E
        local.get 2
        i32.load offset=4
        local.set 3
        block  ;; label = @3
          local.get 2
          i32.load
          br_if 0 (;@3;)
          local.get 0
          local.get 3
          i32.store
          local.get 0
          i32.const 4
          i32.add
          local.get 1
          i32.store
          br 2 (;@1;)
        end
        local.get 2
        i32.const 8
        i32.add
        i32.load
        local.tee 0
        i32.const -2147483647
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        local.get 0
        call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
        unreachable
      end
      call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
      unreachable
    end
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer)
  (func $_ZN60_$LT$alloc..string..String$u20$as$u20$core..fmt..Display$GT$3fmt17h5d51ffd1ebc9bb46E (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.get 0
    i32.const 8
    i32.add
    i32.load
    local.get 1
    call $_ZN42_$LT$str$u20$as$u20$core..fmt..Display$GT$3fmt17h8fa90afa7600905bE)
  (func $_ZN70_$LT$core..result..Result$LT$T$C$E$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17hf0c62b57a5afc3a8E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load
        br_if 0 (;@2;)
        local.get 2
        local.get 0
        i32.store offset=8
        local.get 1
        i32.const 1049068
        i32.const 2
        local.get 2
        i32.const 8
        i32.add
        i32.const 1049072
        call $_ZN4core3fmt9Formatter25debug_tuple_field1_finish17he0f5b2c5c9cb6b55E
        local.set 0
        br 1 (;@1;)
      end
      local.get 2
      local.get 0
      i32.store offset=12
      local.get 1
      i32.const 1049048
      i32.const 3
      local.get 2
      i32.const 12
      i32.add
      i32.const 1049052
      call $_ZN4core3fmt9Formatter25debug_tuple_field1_finish17he0f5b2c5c9cb6b55E
      local.set 0
    end
    local.get 2
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN3std4sync4once4Once10call_inner17hc3d47dfe4dcd2319E (type 11) (param i32 i32 i32 i32 i32)
    (local i32 i32 i32 i32 i32 i64 i64 i64)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 5
    global.set $__stack_pointer
    local.get 5
    i32.const 8
    i32.add
    i32.const 2
    i32.or
    local.set 6
    local.get 0
    i32.load
    local.set 7
    loop  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            block  ;; label = @13
                              block  ;; label = @14
                                block  ;; label = @15
                                  local.get 7
                                  local.tee 8
                                  br_table 1 (;@14;) 0 (;@15;) 3 (;@12;) 2 (;@13;) 3 (;@12;)
                                end
                                local.get 1
                                i32.eqz
                                br_if 3 (;@11;)
                              end
                              local.get 0
                              i32.const 2
                              local.get 0
                              i32.load
                              local.tee 7
                              local.get 7
                              local.get 8
                              i32.eq
                              local.tee 9
                              select
                              i32.store
                              local.get 9
                              i32.eqz
                              br_if 12 (;@1;)
                              local.get 5
                              local.get 8
                              i32.const 1
                              i32.eq
                              i32.store8 offset=12
                              local.get 5
                              i32.const 3
                              i32.store offset=8
                              local.get 2
                              local.get 5
                              i32.const 8
                              i32.add
                              local.get 3
                              i32.load offset=16
                              call_indirect (type 3)
                              local.get 0
                              i32.load
                              local.set 8
                              local.get 0
                              local.get 5
                              i32.load offset=8
                              i32.store
                              local.get 5
                              local.get 8
                              i32.const 3
                              i32.and
                              local.tee 7
                              i32.store
                              local.get 7
                              i32.const 2
                              i32.ne
                              br_if 8 (;@5;)
                              local.get 8
                              i32.const -2
                              i32.add
                              local.tee 7
                              i32.eqz
                              br_if 0 (;@13;)
                              loop  ;; label = @14
                                local.get 7
                                i32.load
                                local.set 8
                                local.get 7
                                i32.const 0
                                i32.store
                                local.get 8
                                i32.eqz
                                br_if 10 (;@4;)
                                local.get 7
                                i32.load offset=4
                                local.set 9
                                local.get 7
                                i32.const 1
                                i32.store8 offset=8
                                local.get 8
                                i32.const 24
                                i32.add
                                call $_ZN3std10sys_common13thread_parker7generic6Parker6unpark17he3c6a9cd9bbb8bceE
                                local.get 8
                                local.get 8
                                i32.load
                                local.tee 7
                                i32.const -1
                                i32.add
                                i32.store
                                block  ;; label = @15
                                  local.get 7
                                  i32.const 1
                                  i32.ne
                                  br_if 0 (;@15;)
                                  local.get 8
                                  call $_ZN5alloc4sync12Arc$LT$T$GT$9drop_slow17h9861dbe28414a55fE
                                end
                                local.get 9
                                local.set 7
                                local.get 9
                                br_if 0 (;@14;)
                              end
                            end
                            local.get 5
                            i32.const 32
                            i32.add
                            global.set $__stack_pointer
                            return
                          end
                          block  ;; label = @12
                            local.get 8
                            i32.const 3
                            i32.and
                            i32.const 2
                            i32.ne
                            br_if 0 (;@12;)
                            loop  ;; label = @13
                              local.get 8
                              local.set 9
                              i32.const 0
                              i32.load offset=1059428
                              br_if 3 (;@10;)
                              i32.const 0
                              i32.const -1
                              i32.store offset=1059428
                              block  ;; label = @14
                                i32.const 0
                                i32.load offset=1059432
                                local.tee 7
                                br_if 0 (;@14;)
                                i32.const 32
                                i32.const 8
                                call $__rust_alloc
                                local.tee 7
                                i32.eqz
                                br_if 5 (;@9;)
                                local.get 7
                                i64.const 4294967297
                                i64.store
                                local.get 7
                                i32.const 16
                                i32.add
                                i32.const 0
                                i32.store
                                i32.const 0
                                i64.load offset=1059408
                                local.set 10
                                loop  ;; label = @15
                                  local.get 10
                                  i64.const 1
                                  i64.add
                                  local.tee 11
                                  i64.eqz
                                  br_if 7 (;@8;)
                                  i32.const 0
                                  local.get 11
                                  i32.const 0
                                  i64.load offset=1059408
                                  local.tee 12
                                  local.get 12
                                  local.get 10
                                  i64.eq
                                  local.tee 8
                                  select
                                  i64.store offset=1059408
                                  local.get 12
                                  local.set 10
                                  local.get 8
                                  i32.eqz
                                  br_if 0 (;@15;)
                                end
                                local.get 7
                                i32.const 0
                                i32.store16 offset=28
                                local.get 7
                                local.get 11
                                i64.store offset=8
                                i32.const 0
                                local.get 7
                                i32.store offset=1059432
                                local.get 7
                                i32.const 24
                                i32.add
                                i32.const 0
                                i32.store
                              end
                              local.get 7
                              local.get 7
                              i32.load
                              local.tee 8
                              i32.const 1
                              i32.add
                              i32.store
                              local.get 8
                              i32.const -1
                              i32.le_s
                              br_if 6 (;@7;)
                              i32.const 0
                              i32.const 0
                              i32.load offset=1059428
                              i32.const 1
                              i32.add
                              i32.store offset=1059428
                              local.get 0
                              local.get 6
                              local.get 0
                              i32.load
                              local.tee 8
                              local.get 8
                              local.get 9
                              i32.eq
                              select
                              i32.store
                              local.get 5
                              i32.const 0
                              i32.store8 offset=16
                              local.get 5
                              local.get 7
                              i32.store offset=8
                              local.get 5
                              local.get 9
                              i32.const -4
                              i32.and
                              i32.store offset=12
                              block  ;; label = @14
                                local.get 8
                                local.get 9
                                i32.ne
                                br_if 0 (;@14;)
                                local.get 5
                                i32.load8_u offset=16
                                i32.eqz
                                br_if 8 (;@6;)
                                br 11 (;@3;)
                              end
                              block  ;; label = @14
                                local.get 5
                                i32.load offset=8
                                local.tee 7
                                i32.eqz
                                br_if 0 (;@14;)
                                local.get 7
                                local.get 7
                                i32.load
                                local.tee 9
                                i32.const -1
                                i32.add
                                i32.store
                                local.get 9
                                i32.const 1
                                i32.ne
                                br_if 0 (;@14;)
                                local.get 5
                                i32.load offset=8
                                call $_ZN5alloc4sync12Arc$LT$T$GT$9drop_slow17h9861dbe28414a55fE
                              end
                              local.get 8
                              i32.const 3
                              i32.and
                              i32.const 2
                              i32.eq
                              br_if 0 (;@13;)
                              br 11 (;@2;)
                            end
                          end
                          i32.const 1050956
                          i32.const 64
                          local.get 4
                          call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
                          unreachable
                        end
                        local.get 5
                        i32.const 28
                        i32.add
                        i32.const 0
                        i32.store
                        local.get 5
                        i32.const 1048772
                        i32.store offset=24
                        local.get 5
                        i64.const 1
                        i64.store offset=12 align=4
                        local.get 5
                        i32.const 1051064
                        i32.store offset=8
                        local.get 5
                        i32.const 8
                        i32.add
                        local.get 4
                        call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
                        unreachable
                      end
                      i32.const 1048772
                      i32.const 16
                      local.get 5
                      i32.const 1048788
                      i32.const 1051376
                      call $_ZN4core6result13unwrap_failed17h0b834de6e1e67b38E
                      unreachable
                    end
                    i32.const 32
                    i32.const 8
                    call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
                    unreachable
                  end
                  call $_ZN3std6thread8ThreadId3new9exhausted17hdd290814ab833751E
                  unreachable
                end
                unreachable
                unreachable
              end
              loop  ;; label = @6
                call $_ZN3std6thread4park17h76ac8dec4e939cfcE
                local.get 5
                i32.load8_u offset=16
                i32.eqz
                br_if 0 (;@6;)
                br 3 (;@3;)
              end
            end
            local.get 5
            i32.const 0
            i32.store offset=8
            local.get 5
            local.get 5
            i32.const 8
            i32.add
            i32.const 1051076
            call $_ZN4core9panicking13assert_failed17hef7b799fcd44f104E
            unreachable
          end
          i32.const 1048872
          i32.const 43
          i32.const 1051092
          call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
          unreachable
        end
        local.get 5
        i32.load offset=8
        local.tee 8
        i32.eqz
        br_if 0 (;@2;)
        local.get 8
        local.get 8
        i32.load
        local.tee 7
        i32.const -1
        i32.add
        i32.store
        local.get 7
        i32.const 1
        i32.ne
        br_if 0 (;@2;)
        local.get 5
        i32.load offset=8
        call $_ZN5alloc4sync12Arc$LT$T$GT$9drop_slow17h9861dbe28414a55fE
        local.get 0
        i32.load
        local.set 7
        br 1 (;@1;)
      end
      local.get 0
      i32.load
      local.set 7
      br 0 (;@1;)
    end)
  (func $_ZN3std2rt19lang_start_internal17h1bb05e5e1cc430efE (type 12) (param i32 i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i64 i64 i64)
    global.get $__stack_pointer
    i32.const 112
    i32.sub
    local.tee 5
    global.set $__stack_pointer
    local.get 5
    i32.const 8
    i32.add
    i32.const 1049088
    i32.const 4
    call $_ZN72_$LT$$RF$str$u20$as$u20$alloc..ffi..c_str..CString..new..SpecNewImpl$GT$13spec_new_impl17h8276bc7cde01c1feE
    block  ;; label = @1
      block  ;; label = @2
        local.get 5
        i32.load offset=12
        br_if 0 (;@2;)
        local.get 5
        i32.const 20
        i32.add
        i32.load
        local.set 6
        local.get 5
        i32.load offset=16
        local.set 7
        i32.const 32
        i32.const 8
        call $__rust_alloc
        local.tee 8
        i32.eqz
        br_if 1 (;@1;)
        local.get 8
        local.get 6
        i32.store offset=20
        local.get 8
        i64.const 4294967297
        i64.store
        local.get 8
        i32.const 16
        i32.add
        local.get 7
        i32.store
        i32.const 0
        i64.load offset=1059408
        local.set 9
        block  ;; label = @3
          loop  ;; label = @4
            local.get 9
            i64.const 1
            i64.add
            local.tee 10
            i64.eqz
            br_if 1 (;@3;)
            i32.const 0
            local.get 10
            i32.const 0
            i64.load offset=1059408
            local.tee 11
            local.get 11
            local.get 9
            i64.eq
            local.tee 6
            select
            i64.store offset=1059408
            local.get 11
            local.set 9
            local.get 6
            i32.eqz
            br_if 0 (;@4;)
          end
          local.get 8
          i32.const 0
          i32.store16 offset=28
          local.get 8
          local.get 10
          i64.store offset=8
          local.get 8
          i32.const 24
          i32.add
          i32.const 0
          i32.store
          local.get 8
          call $_ZN3std10sys_common11thread_info3set17hc2d5dae25e871048E
          local.get 0
          local.get 1
          i32.const 20
          i32.add
          i32.load
          call_indirect (type 4)
          local.set 6
          block  ;; label = @4
            i32.const 0
            i32.load offset=1059332
            i32.const 3
            i32.eq
            br_if 0 (;@4;)
            local.get 5
            i32.const 1
            i32.store8 offset=40
            local.get 5
            local.get 5
            i32.const 40
            i32.add
            i32.store offset=72
            i32.const 1059332
            i32.const 0
            local.get 5
            i32.const 72
            i32.add
            i32.const 1050820
            i32.const 1049204
            call $_ZN3std4sync4once4Once10call_inner17hc3d47dfe4dcd2319E
          end
          local.get 5
          i32.const 112
          i32.add
          global.set $__stack_pointer
          local.get 6
          return
        end
        call $_ZN3std6thread8ThreadId3new9exhausted17hdd290814ab833751E
        unreachable
      end
      local.get 5
      local.get 5
      i32.const 8
      i32.add
      i32.store offset=28
      local.get 5
      i32.const 40
      i32.add
      i32.const 20
      i32.add
      i32.const 1
      i32.store
      local.get 5
      i32.const 72
      i32.add
      i32.const 20
      i32.add
      i32.const 1
      i32.store
      local.get 5
      i64.const 2
      i64.store offset=44 align=4
      local.get 5
      i32.const 1049116
      i32.store offset=40
      local.get 5
      i32.const 8
      i32.store offset=68
      local.get 5
      i64.const 1
      i64.store offset=76 align=4
      local.get 5
      i32.const 1049172
      i32.store offset=72
      local.get 5
      i32.const 9
      i32.store offset=100
      local.get 5
      local.get 5
      i32.const 64
      i32.add
      i32.store offset=56
      local.get 5
      local.get 5
      i32.const 72
      i32.add
      i32.store offset=64
      local.get 5
      local.get 5
      i32.const 96
      i32.add
      i32.store offset=88
      local.get 5
      local.get 5
      i32.const 28
      i32.add
      i32.store offset=96
      local.get 5
      i32.const 32
      i32.add
      local.get 5
      i32.const 104
      i32.add
      local.get 5
      i32.const 40
      i32.add
      call $_ZN3std2io5Write9write_fmt17h05a3bcb077746284E
      local.get 5
      i32.const 32
      i32.add
      call $_ZN4core3ptr81drop_in_place$LT$core..result..Result$LT$$LP$$RP$$C$std..io..error..Error$GT$$GT$17h7ea204518b820d95E
      call $_ZN3std3sys4wasi14abort_internal17h410a405cf1d1c758E
      unreachable
    end
    i32.const 32
    i32.const 8
    call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
    unreachable)
  (func $_ZN3std10sys_common11thread_info3set17hc2d5dae25e871048E (type 1) (param i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 64
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        i32.const 0
        i32.load offset=1059428
        br_if 0 (;@2;)
        i32.const 0
        i32.const -1
        i32.store offset=1059428
        i32.const 0
        i32.load offset=1059432
        br_if 1 (;@1;)
        i32.const 0
        local.get 0
        i32.store offset=1059432
        i32.const 0
        i32.const 0
        i32.store offset=1059428
        local.get 1
        i32.const 64
        i32.add
        global.set $__stack_pointer
        return
      end
      i32.const 1048772
      i32.const 16
      local.get 1
      i32.const 40
      i32.add
      i32.const 1048788
      i32.const 1051392
      call $_ZN4core6result13unwrap_failed17h0b834de6e1e67b38E
      unreachable
    end
    local.get 1
    i32.const 8
    i32.add
    i32.const 20
    i32.add
    i32.const 1
    i32.store
    local.get 1
    i32.const 40
    i32.add
    i32.const 20
    i32.add
    i32.const 0
    i32.store
    local.get 1
    i64.const 2
    i64.store offset=12 align=4
    local.get 1
    i32.const 1049116
    i32.store offset=8
    local.get 1
    i32.const 8
    i32.store offset=36
    local.get 1
    i32.const 1048772
    i32.store offset=56
    local.get 1
    i64.const 1
    i64.store offset=44 align=4
    local.get 1
    i32.const 1051448
    i32.store offset=40
    local.get 1
    local.get 1
    i32.const 32
    i32.add
    i32.store offset=24
    local.get 1
    local.get 1
    i32.const 40
    i32.add
    i32.store offset=32
    local.get 1
    local.get 1
    i32.const 40
    i32.add
    local.get 1
    i32.const 8
    i32.add
    call $_ZN3std2io5Write9write_fmt17h05a3bcb077746284E
    local.get 1
    call $_ZN4core3ptr81drop_in_place$LT$core..result..Result$LT$$LP$$RP$$C$std..io..error..Error$GT$$GT$17h7ea204518b820d95E
    call $_ZN3std3sys4wasi14abort_internal17h410a405cf1d1c758E
    unreachable)
  (func $_ZN3std6thread8ThreadId3new9exhausted17hdd290814ab833751E (type 0)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 0
    global.set $__stack_pointer
    local.get 0
    i32.const 28
    i32.add
    i32.const 0
    i32.store
    local.get 0
    i32.const 1048772
    i32.store offset=24
    local.get 0
    i64.const 1
    i64.store offset=12 align=4
    local.get 0
    i32.const 1049416
    i32.store offset=8
    local.get 0
    i32.const 8
    i32.add
    i32.const 1049424
    call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
    unreachable)
  (func $_ZN3std2io5Write9write_fmt17h05a3bcb077746284E (type 6) (param i32 i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    i32.const 4
    i32.store8 offset=12
    local.get 3
    local.get 1
    i32.store offset=8
    local.get 3
    i32.const 24
    i32.add
    i32.const 16
    i32.add
    local.get 2
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 3
    i32.const 24
    i32.add
    i32.const 8
    i32.add
    local.get 2
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 3
    local.get 2
    i64.load align=4
    i64.store offset=24
    block  ;; label = @1
      block  ;; label = @2
        local.get 3
        i32.const 8
        i32.add
        i32.const 1050676
        local.get 3
        i32.const 24
        i32.add
        call $_ZN4core3fmt5write17hb15fae9d0358166aE
        i32.eqz
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 3
          i32.load8_u offset=12
          i32.const 4
          i32.ne
          br_if 0 (;@3;)
          local.get 0
          i32.const 1050716
          i32.store offset=4
          local.get 0
          i32.const 2
          i32.store
          br 2 (;@1;)
        end
        local.get 0
        local.get 3
        i64.load offset=12 align=4
        i64.store align=4
        br 1 (;@1;)
      end
      local.get 0
      i32.const 4
      i32.store8
      local.get 3
      i32.load8_u offset=12
      i32.const 3
      i32.ne
      br_if 0 (;@1;)
      local.get 3
      i32.const 8
      i32.add
      i32.const 8
      i32.add
      i32.load
      local.tee 2
      i32.load
      local.get 2
      i32.load offset=4
      i32.load
      call_indirect (type 1)
      block  ;; label = @2
        local.get 2
        i32.load offset=4
        local.tee 0
        i32.const 4
        i32.add
        i32.load
        local.tee 1
        i32.eqz
        br_if 0 (;@2;)
        local.get 2
        i32.load
        local.get 1
        local.get 0
        i32.const 8
        i32.add
        i32.load
        call $__rust_dealloc
      end
      local.get 3
      i32.load offset=16
      i32.const 12
      i32.const 4
      call $__rust_dealloc
    end
    local.get 3
    i32.const 48
    i32.add
    global.set $__stack_pointer)
  (func $_ZN3std3sys4wasi14abort_internal17h410a405cf1d1c758E (type 0)
    call $abort
    unreachable)
  (func $_ZN3std10sys_common13thread_parker7generic6Parker6unpark17he3c6a9cd9bbb8bceE (type 1) (param i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    local.get 0
    i32.load
    local.set 2
    local.get 0
    i32.const 2
    i32.store
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 2
          br_table 2 (;@1;) 1 (;@2;) 2 (;@1;) 0 (;@3;)
        end
        local.get 1
        i32.const 28
        i32.add
        i32.const 0
        i32.store
        local.get 1
        i32.const 1048772
        i32.store offset=24
        local.get 1
        i64.const 1
        i64.store offset=12 align=4
        local.get 1
        i32.const 1052720
        i32.store offset=8
        local.get 1
        i32.const 8
        i32.add
        i32.const 1052728
        call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
        unreachable
      end
      local.get 0
      i32.load8_u offset=4
      local.set 2
      local.get 0
      i32.const 1
      i32.store8 offset=4
      local.get 1
      local.get 2
      i32.const 1
      i32.and
      local.tee 2
      i32.store8 offset=7
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 2
            br_if 0 (;@4;)
            local.get 0
            i32.const 4
            i32.add
            local.set 2
            i32.const 0
            local.set 3
            block  ;; label = @5
              block  ;; label = @6
                i32.const 0
                i32.load offset=1059404
                i32.const 2147483647
                i32.and
                i32.eqz
                br_if 0 (;@6;)
                call $_ZN3std9panicking11panic_count17is_zero_slow_path17hea85e77ac3ac7cbaE
                local.set 3
                block  ;; label = @7
                  local.get 0
                  i32.load8_u offset=5
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 3
                  i32.const 1
                  i32.xor
                  local.set 3
                  br 2 (;@5;)
                end
                local.get 3
                i32.eqz
                br_if 4 (;@2;)
                br 3 (;@3;)
              end
              local.get 0
              i32.load8_u offset=5
              i32.eqz
              br_if 2 (;@3;)
            end
            local.get 1
            local.get 3
            i32.store8 offset=12
            local.get 1
            local.get 2
            i32.store offset=8
            i32.const 1048628
            i32.const 43
            local.get 1
            i32.const 8
            i32.add
            i32.const 1052504
            i32.const 1052744
            call $_ZN4core6result13unwrap_failed17h0b834de6e1e67b38E
            unreachable
          end
          local.get 1
          i32.const 0
          i32.store offset=28
          local.get 1
          i32.const 1048772
          i32.store offset=24
          local.get 1
          i64.const 1
          i64.store offset=12 align=4
          local.get 1
          i32.const 1052244
          i32.store offset=8
          local.get 1
          i32.const 7
          i32.add
          local.get 1
          i32.const 8
          i32.add
          call $_ZN4core9panicking13assert_failed17had14dc5ddcedac37E
          unreachable
        end
        i32.const 0
        i32.load offset=1059404
        i32.const 2147483647
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        call $_ZN3std9panicking11panic_count17is_zero_slow_path17hea85e77ac3ac7cbaE
        br_if 0 (;@2;)
        local.get 0
        i32.const 1
        i32.store8 offset=5
      end
      local.get 2
      i32.const 0
      i32.store8
    end
    local.get 1
    i32.const 32
    i32.add
    global.set $__stack_pointer)
  (func $_ZN3std10sys_common11thread_info14current_thread17h2a6a795d0e8df1beE (type 10) (result i32)
    (local i32 i32 i64 i64 i64 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 0
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            i32.const 0
            i32.load offset=1059428
            br_if 0 (;@4;)
            i32.const 0
            i32.const -1
            i32.store offset=1059428
            block  ;; label = @5
              i32.const 0
              i32.load offset=1059432
              local.tee 1
              br_if 0 (;@5;)
              i32.const 32
              i32.const 8
              call $__rust_alloc
              local.tee 1
              i32.eqz
              br_if 2 (;@3;)
              local.get 1
              i64.const 4294967297
              i64.store
              local.get 1
              i32.const 16
              i32.add
              i32.const 0
              i32.store
              i32.const 0
              i64.load offset=1059408
              local.set 2
              loop  ;; label = @6
                local.get 2
                i64.const 1
                i64.add
                local.tee 3
                i64.eqz
                br_if 4 (;@2;)
                i32.const 0
                local.get 3
                i32.const 0
                i64.load offset=1059408
                local.tee 4
                local.get 4
                local.get 2
                i64.eq
                local.tee 5
                select
                i64.store offset=1059408
                local.get 4
                local.set 2
                local.get 5
                i32.eqz
                br_if 0 (;@6;)
              end
              local.get 1
              i32.const 0
              i32.store16 offset=28
              local.get 1
              local.get 3
              i64.store offset=8
              i32.const 0
              local.get 1
              i32.store offset=1059432
              local.get 1
              i32.const 24
              i32.add
              i32.const 0
              i32.store
            end
            local.get 1
            local.get 1
            i32.load
            local.tee 5
            i32.const 1
            i32.add
            i32.store
            local.get 5
            i32.const -1
            i32.le_s
            br_if 3 (;@1;)
            i32.const 0
            i32.const 0
            i32.load offset=1059428
            i32.const 1
            i32.add
            i32.store offset=1059428
            local.get 0
            i32.const 16
            i32.add
            global.set $__stack_pointer
            local.get 1
            return
          end
          i32.const 1048772
          i32.const 16
          local.get 0
          i32.const 8
          i32.add
          i32.const 1048788
          i32.const 1051376
          call $_ZN4core6result13unwrap_failed17h0b834de6e1e67b38E
          unreachable
        end
        i32.const 32
        i32.const 8
        call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
        unreachable
      end
      call $_ZN3std6thread8ThreadId3new9exhausted17hdd290814ab833751E
      unreachable
    end
    unreachable
    unreachable)
  (func $_ZN3std6thread4park17h76ac8dec4e939cfcE (type 0)
    (local i32 i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 0
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                call $_ZN3std10sys_common11thread_info14current_thread17h2a6a795d0e8df1beE
                local.tee 1
                i32.eqz
                br_if 0 (;@6;)
                local.get 1
                i32.const 24
                i32.add
                local.tee 2
                i32.const 0
                local.get 2
                i32.load
                local.tee 2
                local.get 2
                i32.const 2
                i32.eq
                local.tee 2
                select
                i32.store
                block  ;; label = @7
                  local.get 2
                  br_if 0 (;@7;)
                  local.get 1
                  i32.const 28
                  i32.add
                  local.tee 3
                  i32.load8_u
                  local.set 2
                  local.get 3
                  i32.const 1
                  i32.store8
                  local.get 0
                  local.get 2
                  i32.const 1
                  i32.and
                  local.tee 2
                  i32.store8 offset=4
                  local.get 2
                  br_if 2 (;@5;)
                  i32.const 0
                  local.set 4
                  block  ;; label = @8
                    i32.const 0
                    i32.load offset=1059404
                    i32.const 2147483647
                    i32.and
                    i32.eqz
                    br_if 0 (;@8;)
                    call $_ZN3std9panicking11panic_count17is_zero_slow_path17hea85e77ac3ac7cbaE
                    i32.const 1
                    i32.xor
                    local.set 4
                  end
                  local.get 1
                  i32.load8_u offset=29
                  br_if 3 (;@4;)
                  local.get 1
                  local.get 1
                  i32.load offset=24
                  local.tee 2
                  i32.const 1
                  local.get 2
                  select
                  i32.store offset=24
                  local.get 2
                  i32.eqz
                  br_if 6 (;@1;)
                  local.get 2
                  i32.const 2
                  i32.ne
                  br_if 4 (;@3;)
                  local.get 1
                  i32.load offset=24
                  local.set 2
                  local.get 1
                  i32.const 0
                  i32.store offset=24
                  local.get 0
                  local.get 2
                  i32.store offset=4
                  local.get 2
                  i32.const 2
                  i32.ne
                  br_if 5 (;@2;)
                  block  ;; label = @8
                    local.get 4
                    br_if 0 (;@8;)
                    i32.const 0
                    i32.load offset=1059404
                    i32.const 2147483647
                    i32.and
                    i32.eqz
                    br_if 0 (;@8;)
                    call $_ZN3std9panicking11panic_count17is_zero_slow_path17hea85e77ac3ac7cbaE
                    br_if 0 (;@8;)
                    local.get 1
                    i32.const 1
                    i32.store8 offset=29
                  end
                  local.get 3
                  i32.const 0
                  i32.store8
                end
                local.get 1
                local.get 1
                i32.load
                local.tee 2
                i32.const -1
                i32.add
                i32.store
                block  ;; label = @7
                  local.get 2
                  i32.const 1
                  i32.ne
                  br_if 0 (;@7;)
                  local.get 1
                  call $_ZN5alloc4sync12Arc$LT$T$GT$9drop_slow17h9861dbe28414a55fE
                end
                local.get 0
                i32.const 32
                i32.add
                global.set $__stack_pointer
                return
              end
              i32.const 1049220
              i32.const 94
              i32.const 1049344
              call $_ZN4core6option13expect_failed17hc52795d163105200E
              unreachable
            end
            local.get 0
            i32.const 0
            i32.store offset=28
            local.get 0
            i32.const 1048772
            i32.store offset=24
            local.get 0
            i64.const 1
            i64.store offset=12 align=4
            local.get 0
            i32.const 1052244
            i32.store offset=8
            local.get 0
            i32.const 4
            i32.add
            local.get 0
            i32.const 8
            i32.add
            call $_ZN4core9panicking13assert_failed17had14dc5ddcedac37E
            unreachable
          end
          local.get 0
          local.get 4
          i32.store8 offset=12
          local.get 0
          local.get 3
          i32.store offset=8
          i32.const 1048628
          i32.const 43
          local.get 0
          i32.const 8
          i32.add
          i32.const 1052504
          i32.const 1052572
          call $_ZN4core6result13unwrap_failed17h0b834de6e1e67b38E
          unreachable
        end
        local.get 0
        i32.const 28
        i32.add
        i32.const 0
        i32.store
        local.get 0
        i32.const 1048772
        i32.store offset=24
        local.get 0
        i64.const 1
        i64.store offset=12 align=4
        local.get 0
        i32.const 1052612
        i32.store offset=8
        local.get 0
        i32.const 8
        i32.add
        i32.const 1052620
        call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
        unreachable
      end
      local.get 0
      i32.const 0
      i32.store offset=28
      local.get 0
      i32.const 1048772
      i32.store offset=24
      local.get 0
      i64.const 1
      i64.store offset=12 align=4
      local.get 0
      i32.const 1052668
      i32.store offset=8
      local.get 0
      i32.const 4
      i32.add
      local.get 0
      i32.const 8
      i32.add
      i32.const 1052676
      call $_ZN4core9panicking13assert_failed17hef7b799fcd44f104E
      unreachable
    end
    local.get 0
    i32.const 28
    i32.add
    i32.const 0
    i32.store
    local.get 0
    i32.const 1048772
    i32.store offset=24
    local.get 0
    i64.const 1
    i64.store offset=12 align=4
    local.get 0
    i32.const 1052132
    i32.store offset=8
    local.get 0
    i32.const 8
    i32.add
    i32.const 1052196
    call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
    unreachable)
  (func $_ZN3std3env11current_dir17h8ee6faca341aee6dE (type 1) (param i32)
    (local i32 i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    i32.const 512
    local.set 2
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            i32.const 512
            i32.const 1
            call $__rust_alloc
            local.tee 3
            i32.eqz
            br_if 0 (;@4;)
            local.get 1
            i32.const 512
            i32.store offset=4
            local.get 1
            local.get 3
            i32.store
            local.get 3
            i32.const 512
            call $getcwd
            br_if 1 (;@3;)
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  i32.const 0
                  i32.load offset=1059936
                  local.tee 2
                  i32.const 68
                  i32.ne
                  br_if 0 (;@7;)
                  i32.const 512
                  local.set 2
                  br 1 (;@6;)
                end
                local.get 0
                i64.const 0
                i64.store align=4
                local.get 0
                i32.const 8
                i32.add
                local.get 2
                i32.store
                i32.const 512
                local.set 2
                br 1 (;@5;)
              end
              loop  ;; label = @6
                local.get 1
                local.get 2
                i32.store offset=8
                local.get 1
                local.get 2
                i32.const 1
                call $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$7reserve21do_reserve_and_handle17ha33bee7b1e5ebd92E
                local.get 1
                i32.load
                local.tee 3
                local.get 1
                i32.load offset=4
                local.tee 2
                call $getcwd
                br_if 3 (;@3;)
                i32.const 0
                i32.load offset=1059936
                local.tee 4
                i32.const 68
                i32.eq
                br_if 0 (;@6;)
              end
              local.get 0
              i64.const 0
              i64.store align=4
              local.get 0
              i32.const 8
              i32.add
              local.get 4
              i32.store
              local.get 2
              i32.eqz
              br_if 3 (;@2;)
            end
            local.get 3
            local.get 2
            i32.const 1
            call $__rust_dealloc
            br 2 (;@2;)
          end
          i32.const 512
          i32.const 1
          call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
          unreachable
        end
        local.get 1
        local.get 3
        call $strlen
        local.tee 4
        i32.store offset=8
        block  ;; label = @3
          local.get 2
          local.get 4
          i32.le_u
          br_if 0 (;@3;)
          i32.const 1
          local.set 5
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 4
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 4
                  i32.const -1
                  i32.gt_s
                  br_if 2 (;@5;)
                  local.get 4
                  i32.const 1
                  call $__rust_alloc
                  local.tee 5
                  i32.eqz
                  br_if 1 (;@6;)
                  local.get 5
                  local.get 3
                  local.get 4
                  call $memcpy
                  drop
                end
                local.get 3
                local.get 2
                i32.const 1
                call $__rust_dealloc
                br 2 (;@4;)
              end
              call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
              unreachable
            end
            local.get 3
            local.get 2
            i32.const 1
            local.get 4
            call $__rust_realloc
            local.tee 5
            i32.eqz
            br_if 3 (;@1;)
          end
          local.get 1
          local.get 4
          i32.store offset=4
          local.get 1
          local.get 5
          i32.store
        end
        local.get 0
        local.get 1
        i64.load
        i64.store align=4
        local.get 0
        i32.const 8
        i32.add
        local.get 1
        i32.const 8
        i32.add
        i32.load
        i32.store
      end
      local.get 1
      i32.const 16
      i32.add
      global.set $__stack_pointer
      return
    end
    local.get 4
    i32.const 1
    call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
    unreachable)
  (func $_ZN3std3env7_var_os17ha4645996074a0c32E (type 6) (param i32 i32 i32)
    (local i32 i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    local.get 1
    local.get 2
    call $_ZN81_$LT$$RF$$u5b$u8$u5d$$u20$as$u20$alloc..ffi..c_str..CString..new..SpecNewImpl$GT$13spec_new_impl17h9ddb1e973ffe1a2cE
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 3
            i32.load offset=4
            local.tee 2
            i32.eqz
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 3
              i32.load offset=8
              local.tee 1
              i32.eqz
              br_if 0 (;@5;)
              local.get 2
              local.get 1
              i32.const 1
              call $__rust_dealloc
            end
            local.get 0
            i32.const 0
            i32.store
            br 1 (;@3;)
          end
          local.get 3
          i32.const 12
          i32.add
          i32.load
          local.set 4
          block  ;; label = @4
            block  ;; label = @5
              local.get 3
              i32.load offset=8
              local.tee 1
              call $getenv
              local.tee 5
              i32.eqz
              br_if 0 (;@5;)
              block  ;; label = @6
                block  ;; label = @7
                  local.get 5
                  call $strlen
                  local.tee 2
                  br_if 0 (;@7;)
                  i32.const 1
                  local.set 6
                  br 1 (;@6;)
                end
                local.get 2
                i32.const -1
                i32.gt_s
                local.tee 7
                i32.eqz
                br_if 4 (;@2;)
                local.get 2
                local.get 7
                call $__rust_alloc
                local.tee 6
                i32.eqz
                br_if 5 (;@1;)
              end
              local.get 6
              local.get 5
              local.get 2
              call $memcpy
              local.set 5
              local.get 0
              local.get 2
              i32.store offset=8
              local.get 0
              local.get 2
              i32.store offset=4
              local.get 0
              local.get 5
              i32.store
              br 1 (;@4;)
            end
            local.get 0
            i32.const 0
            i32.store
          end
          local.get 1
          i32.const 0
          i32.store8
          local.get 4
          i32.eqz
          br_if 0 (;@3;)
          local.get 1
          local.get 4
          i32.const 1
          call $__rust_dealloc
        end
        local.get 3
        i32.const 16
        i32.add
        global.set $__stack_pointer
        return
      end
      call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
      unreachable
    end
    local.get 2
    local.get 7
    call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
    unreachable)
  (func $_ZN60_$LT$std..io..error..Error$u20$as$u20$core..fmt..Display$GT$3fmt17hb0563df3d544e4f8E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 64
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 0
              i32.load8_u
              br_table 0 (;@5;) 1 (;@4;) 2 (;@3;) 3 (;@2;) 0 (;@5;)
            end
            local.get 2
            local.get 0
            i32.load offset=4
            local.tee 0
            i32.store offset=4
            local.get 2
            i32.const 8
            i32.add
            local.get 0
            call $_ZN3std3sys4wasi2os12error_string17h7019cb2a4ed0fff2E
            local.get 2
            i32.const 60
            i32.add
            i32.const 2
            i32.store
            local.get 2
            i32.const 36
            i32.add
            i32.const 10
            i32.store
            local.get 2
            i64.const 3
            i64.store offset=44 align=4
            local.get 2
            i32.const 1050448
            i32.store offset=40
            local.get 2
            i32.const 11
            i32.store offset=28
            local.get 2
            local.get 2
            i32.const 24
            i32.add
            i32.store offset=56
            local.get 2
            local.get 2
            i32.const 4
            i32.add
            i32.store offset=32
            local.get 2
            local.get 2
            i32.const 8
            i32.add
            i32.store offset=24
            local.get 1
            local.get 2
            i32.const 40
            i32.add
            call $_ZN4core3fmt9Formatter9write_fmt17h6645fe2bd1b938a8E
            local.set 0
            local.get 2
            i32.load offset=12
            local.tee 1
            i32.eqz
            br_if 3 (;@1;)
            local.get 2
            i32.load offset=8
            local.get 1
            i32.const 1
            call $__rust_dealloc
            br 3 (;@1;)
          end
          local.get 0
          i32.load8_u offset=1
          local.set 0
          local.get 2
          i32.const 60
          i32.add
          i32.const 1
          i32.store
          local.get 2
          i64.const 1
          i64.store offset=44 align=4
          local.get 2
          i32.const 1049612
          i32.store offset=40
          local.get 2
          i32.const 12
          i32.store offset=12
          local.get 2
          local.get 0
          i32.const 32
          i32.xor
          i32.const 63
          i32.and
          i32.const 2
          i32.shl
          local.tee 0
          i32.const 1052760
          i32.add
          i32.load
          i32.store offset=28
          local.get 2
          local.get 0
          i32.const 1053016
          i32.add
          i32.load
          i32.store offset=24
          local.get 2
          local.get 2
          i32.const 8
          i32.add
          i32.store offset=56
          local.get 2
          local.get 2
          i32.const 24
          i32.add
          i32.store offset=8
          local.get 1
          local.get 2
          i32.const 40
          i32.add
          call $_ZN4core3fmt9Formatter9write_fmt17h6645fe2bd1b938a8E
          local.set 0
          br 2 (;@1;)
        end
        local.get 0
        i32.load offset=4
        local.tee 0
        i32.load
        local.get 0
        i32.load offset=4
        local.get 1
        call $_ZN42_$LT$str$u20$as$u20$core..fmt..Display$GT$3fmt17h8fa90afa7600905bE
        local.set 0
        br 1 (;@1;)
      end
      local.get 0
      i32.load offset=4
      local.tee 0
      i32.load
      local.get 1
      local.get 0
      i32.const 4
      i32.add
      i32.load
      i32.load offset=16
      call_indirect (type 5)
      local.set 0
    end
    local.get 2
    i32.const 64
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN3std3env4args17h99c9c61d3ce5a89bE (type 1) (param i32)
    local.get 0
    call $_ZN3std3env7args_os17he725a206944ad5d1E)
  (func $_ZN3std3env7args_os17he725a206944ad5d1E (type 1) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i64)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    local.get 1
    i32.const 16
    i32.add
    call $_ZN4wasi13lib_generated14args_sizes_get17hf5a1e66d6f51bb94E
    i32.const 4
    local.set 2
    i32.const 0
    local.set 3
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 1
                i32.load16_u offset=16
                br_if 0 (;@6;)
                local.get 1
                i32.const 24
                i32.add
                i32.load
                local.set 4
                block  ;; label = @7
                  block  ;; label = @8
                    local.get 1
                    i32.load offset=20
                    local.tee 5
                    br_if 0 (;@8;)
                    i32.const 4
                    local.set 6
                    br 1 (;@7;)
                  end
                  local.get 5
                  i32.const 536870911
                  i32.gt_u
                  br_if 5 (;@2;)
                  local.get 5
                  i32.const 2
                  i32.shl
                  local.tee 2
                  i32.const 0
                  i32.lt_s
                  br_if 5 (;@2;)
                  local.get 5
                  i32.const 536870912
                  i32.lt_u
                  i32.const 2
                  i32.shl
                  local.set 7
                  block  ;; label = @8
                    block  ;; label = @9
                      local.get 2
                      br_if 0 (;@9;)
                      local.get 7
                      local.set 6
                      br 1 (;@8;)
                    end
                    local.get 2
                    local.get 7
                    call $__rust_alloc
                    local.set 6
                  end
                  local.get 6
                  i32.eqz
                  br_if 3 (;@4;)
                end
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            block  ;; label = @13
                              local.get 4
                              i32.eqz
                              br_if 0 (;@13;)
                              local.get 4
                              i32.const -1
                              i32.gt_s
                              local.tee 2
                              i32.eqz
                              br_if 11 (;@2;)
                              local.get 4
                              local.get 2
                              call $__rust_alloc
                              local.tee 8
                              br_if 1 (;@12;)
                              local.get 4
                              local.get 2
                              call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
                              unreachable
                            end
                            i32.const 1
                            local.set 8
                            local.get 1
                            local.get 6
                            i32.const 1
                            call $_ZN4wasi13lib_generated8args_get17hb5bb6f40771f3aa1E
                            local.get 1
                            i32.load16_u
                            i32.eqz
                            br_if 1 (;@11;)
                            br 3 (;@9;)
                          end
                          local.get 1
                          i32.const 8
                          i32.add
                          local.get 6
                          local.get 8
                          call $_ZN4wasi13lib_generated8args_get17hb5bb6f40771f3aa1E
                          local.get 1
                          i32.load16_u offset=8
                          br_if 1 (;@10;)
                        end
                        block  ;; label = @11
                          block  ;; label = @12
                            local.get 5
                            br_if 0 (;@12;)
                            local.get 1
                            i32.const 0
                            i32.store offset=24
                            local.get 1
                            i64.const 4
                            i64.store offset=16
                            i32.const 4
                            local.set 2
                            br 1 (;@11;)
                          end
                          local.get 5
                          i32.const 178956970
                          i32.gt_u
                          br_if 9 (;@2;)
                          local.get 5
                          i32.const 12
                          i32.mul
                          local.tee 2
                          i32.const 0
                          i32.lt_s
                          br_if 9 (;@2;)
                          local.get 5
                          i32.const 178956971
                          i32.lt_u
                          i32.const 2
                          i32.shl
                          local.set 7
                          block  ;; label = @12
                            block  ;; label = @13
                              local.get 2
                              br_if 0 (;@13;)
                              local.get 7
                              local.set 9
                              br 1 (;@12;)
                            end
                            local.get 2
                            local.get 7
                            call $__rust_alloc
                            local.set 9
                          end
                          local.get 9
                          i32.eqz
                          br_if 8 (;@3;)
                          i32.const 0
                          local.set 10
                          local.get 1
                          i32.const 0
                          i32.store offset=24
                          local.get 1
                          local.get 9
                          i32.store offset=16
                          local.get 1
                          local.get 5
                          i32.store offset=20
                          local.get 5
                          i32.const 2
                          i32.shl
                          local.set 11
                          i32.const 0
                          local.set 7
                          loop  ;; label = @12
                            i32.const 1
                            local.set 12
                            block  ;; label = @13
                              local.get 6
                              local.get 10
                              i32.add
                              i32.load
                              local.tee 13
                              call $strlen
                              local.tee 2
                              i32.eqz
                              br_if 0 (;@13;)
                              local.get 2
                              i32.const -1
                              i32.le_s
                              br_if 11 (;@2;)
                              local.get 2
                              i32.const 1
                              call $__rust_alloc
                              local.tee 12
                              i32.eqz
                              br_if 12 (;@1;)
                            end
                            local.get 12
                            local.get 13
                            local.get 2
                            call $memcpy
                            local.set 13
                            block  ;; label = @13
                              local.get 7
                              local.get 1
                              i32.load offset=20
                              i32.ne
                              br_if 0 (;@13;)
                              local.get 1
                              i32.const 16
                              i32.add
                              local.get 7
                              call $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$16reserve_for_push17h7fbb5eb56ac0a9b2E
                              local.get 1
                              i32.load offset=16
                              local.set 9
                              local.get 1
                              i32.load offset=24
                              local.set 7
                            end
                            local.get 9
                            local.get 7
                            i32.const 12
                            i32.mul
                            i32.add
                            local.tee 12
                            local.get 2
                            i32.store offset=8
                            local.get 12
                            local.get 2
                            i32.store offset=4
                            local.get 12
                            local.get 13
                            i32.store
                            local.get 1
                            local.get 7
                            i32.const 1
                            i32.add
                            local.tee 7
                            i32.store offset=24
                            local.get 11
                            local.get 10
                            i32.const 4
                            i32.add
                            local.tee 10
                            i32.ne
                            br_if 0 (;@12;)
                          end
                          local.get 6
                          local.get 5
                          i32.const 2
                          i32.shl
                          i32.const 4
                          call $__rust_dealloc
                          local.get 1
                          i32.load offset=16
                          local.set 2
                        end
                        local.get 1
                        i64.load offset=20 align=4
                        local.set 14
                        block  ;; label = @11
                          local.get 4
                          i32.eqz
                          br_if 0 (;@11;)
                          local.get 8
                          local.get 4
                          i32.const 1
                          call $__rust_dealloc
                        end
                        local.get 2
                        i32.eqz
                        br_if 2 (;@8;)
                        local.get 14
                        i64.const 32
                        i64.shr_u
                        i32.wrap_i64
                        local.set 3
                        local.get 14
                        i32.wrap_i64
                        local.set 7
                        br 5 (;@5;)
                      end
                      local.get 8
                      local.get 4
                      i32.const 1
                      call $__rust_dealloc
                    end
                    local.get 5
                    br_if 1 (;@7;)
                  end
                  i32.const 4
                  local.set 2
                  br 1 (;@6;)
                end
                i32.const 4
                local.set 2
                local.get 6
                local.get 5
                i32.const 2
                i32.shl
                i32.const 4
                call $__rust_dealloc
              end
              i32.const 0
              local.set 7
            end
            local.get 0
            local.get 2
            i32.store offset=8
            local.get 0
            local.get 7
            i32.store offset=4
            local.get 0
            local.get 2
            i32.store
            local.get 0
            local.get 2
            local.get 3
            i32.const 12
            i32.mul
            i32.add
            i32.store offset=12
            local.get 1
            i32.const 32
            i32.add
            global.set $__stack_pointer
            return
          end
          local.get 2
          local.get 7
          call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
          unreachable
        end
        local.get 2
        local.get 7
        call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
        unreachable
      end
      call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
      unreachable
    end
    local.get 2
    i32.const 1
    call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
    unreachable)
  (func $_ZN73_$LT$std..env..Args$u20$as$u20$core..iter..traits..iterator..Iterator$GT$4next17hc5401505321c279dE (type 3) (param i32 i32)
    (local i32 i32 i32 i64)
    global.get $__stack_pointer
    i32.const 64
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.load offset=8
            local.tee 3
            local.get 1
            i32.load offset=12
            i32.eq
            br_if 0 (;@4;)
            local.get 1
            local.get 3
            i32.const 12
            i32.add
            i32.store offset=8
            local.get 3
            i32.load
            local.tee 1
            br_if 1 (;@3;)
          end
          local.get 0
          i32.const 0
          i32.store
          br 1 (;@2;)
        end
        local.get 3
        i32.load offset=4
        local.set 4
        local.get 2
        i32.const 40
        i32.add
        local.get 1
        local.get 3
        i32.load offset=8
        local.tee 3
        call $_ZN4core3str8converts9from_utf817he9ed2de3b80ea750E
        block  ;; label = @3
          local.get 2
          i32.load offset=40
          i32.eqz
          br_if 0 (;@3;)
          local.get 2
          i64.load offset=44 align=4
          local.tee 5
          i64.const 1095216660480
          i64.and
          i64.const 8589934592
          i64.ne
          br_if 2 (;@1;)
        end
        local.get 2
        i32.const 24
        i32.add
        i32.const 8
        i32.add
        local.get 4
        i32.store
        local.get 2
        i32.const 40
        i32.add
        i32.const 8
        i32.add
        local.get 3
        i32.store
        local.get 2
        local.get 1
        i32.store offset=28
        local.get 2
        local.get 2
        i64.load offset=28 align=4
        local.tee 5
        i64.store offset=40
        local.get 0
        i32.const 8
        i32.add
        local.get 3
        i32.store
        local.get 0
        local.get 5
        i64.store align=4
      end
      local.get 2
      i32.const 64
      i32.add
      global.set $__stack_pointer
      return
    end
    local.get 2
    local.get 5
    i64.store offset=52 align=4
    local.get 2
    local.get 3
    i32.store offset=48
    local.get 2
    local.get 4
    i32.store offset=44
    local.get 2
    local.get 1
    i32.store offset=40
    local.get 2
    i32.const 24
    i32.add
    i32.const 4
    i32.or
    local.get 2
    i32.const 40
    i32.add
    call $_ZN5alloc6string13FromUtf8Error10into_bytes17ha1e23acff317451fE
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 2
    i32.const 36
    i32.add
    i32.load
    local.tee 1
    i32.store
    local.get 2
    local.get 2
    i64.load offset=28 align=4
    local.tee 5
    i64.store offset=8
    local.get 2
    i32.const 40
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.store
    local.get 2
    local.get 5
    i64.store offset=40
    i32.const 1048628
    i32.const 43
    local.get 2
    i32.const 40
    i32.add
    i32.const 1049492
    i32.const 1049476
    call $_ZN4core6result13unwrap_failed17h0b834de6e1e67b38E
    unreachable)
  (func $_ZN73_$LT$std..env..Args$u20$as$u20$core..iter..traits..iterator..Iterator$GT$9size_hint17hf8693f4f8fbdba08E (type 3) (param i32 i32)
    local.get 0
    i32.const 1
    i32.store offset=4
    local.get 0
    i32.const 8
    i32.add
    local.get 1
    i32.const 12
    i32.add
    i32.load
    local.get 1
    i32.const 8
    i32.add
    i32.load
    i32.sub
    i32.const 12
    i32.div_u
    local.tee 1
    i32.store
    local.get 0
    local.get 1
    i32.store)
  (func $_ZN3std3sys4wasi17decode_error_kind17h27b067431089650bE (type 4) (param i32) (result i32)
    (local i32)
    i32.const 40
    local.set 1
    block  ;; label = @1
      local.get 0
      i32.const 65535
      i32.gt_u
      br_if 0 (;@1;)
      i32.const 2
      local.set 1
      i32.const 1052470
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 3
      local.set 1
      i32.const 1052472
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 1
      local.set 1
      i32.const 1052474
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 1052476
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 11
      local.set 1
      i32.const 1052478
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 7
      local.set 1
      i32.const 1052480
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 6
      local.set 1
      i32.const 1052482
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 9
      local.set 1
      i32.const 1052484
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 8
      local.set 1
      i32.const 1052486
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 0
      local.set 1
      i32.const 1052488
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 35
      local.set 1
      i32.const 1052490
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 20
      local.set 1
      i32.const 1052492
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 22
      local.set 1
      i32.const 1052494
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 12
      local.set 1
      i32.const 1052496
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 13
      local.set 1
      i32.const 1052498
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 36
      local.set 1
      i32.const 1052500
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      br_if 0 (;@1;)
      i32.const 38
      i32.const 40
      i32.const 1052502
      call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
      i32.const 65535
      i32.and
      local.get 0
      i32.eq
      select
      local.set 1
    end
    local.get 1)
  (func $_ZN3std2io8buffered9bufwriter18BufWriter$LT$W$GT$9flush_buf17he5398a73dc5def27E (type 3) (param i32 i32)
    (local i32 i32 i32 i64 i32 i32 i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 1
          i32.const 8
          i32.add
          i32.load
          local.tee 3
          br_if 0 (;@3;)
          local.get 0
          i32.const 4
          i32.store8
          br 1 (;@2;)
        end
        local.get 1
        i32.load
        local.set 4
        i32.const 1049544
        i64.extend_i32_u
        i64.const 32
        i64.shl
        i64.const 2
        i64.or
        local.set 5
        i32.const 0
        local.set 6
        loop  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 3
              local.get 6
              i32.lt_u
              br_if 0 (;@5;)
              local.get 2
              local.get 3
              local.get 6
              i32.sub
              local.tee 7
              i32.store offset=12
              local.get 2
              local.get 4
              local.get 6
              i32.add
              local.tee 8
              i32.store offset=8
              local.get 2
              i32.const 16
              i32.add
              i32.const 1
              local.get 2
              i32.const 8
              i32.add
              i32.const 1
              call $_ZN4wasi13lib_generated8fd_write17hb23bdba73a8f4709E
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      local.get 2
                      i32.load16_u offset=16
                      br_if 0 (;@9;)
                      local.get 2
                      i32.load offset=20
                      local.set 9
                      br 1 (;@8;)
                    end
                    local.get 2
                    local.get 2
                    i32.load16_u offset=18
                    i32.store16 offset=30
                    local.get 7
                    local.set 9
                    local.get 2
                    i32.const 30
                    i32.add
                    call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
                    local.tee 10
                    i32.const 65535
                    i32.and
                    local.tee 11
                    i32.const 1052468
                    call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
                    i32.const 65535
                    i32.and
                    i32.ne
                    br_if 1 (;@7;)
                  end
                  local.get 1
                  i32.const 0
                  i32.store8 offset=12
                  local.get 9
                  i32.eqz
                  br_if 1 (;@6;)
                  local.get 9
                  local.get 6
                  i32.add
                  local.set 6
                  br 3 (;@4;)
                end
                local.get 1
                i32.const 0
                i32.store8 offset=12
                local.get 11
                call $_ZN3std3sys4wasi17decode_error_kind17h27b067431089650bE
                i32.const 255
                i32.and
                i32.const 35
                i32.eq
                br_if 2 (;@4;)
                local.get 10
                i64.extend_i32_u
                i64.const 65535
                i64.and
                i64.const 32
                i64.shl
                local.set 5
              end
              local.get 0
              local.get 5
              i64.store align=4
              local.get 6
              i32.eqz
              br_if 3 (;@2;)
              local.get 1
              i32.const 8
              i32.add
              local.tee 9
              i32.const 0
              i32.store
              local.get 3
              local.get 6
              i32.eq
              br_if 3 (;@2;)
              local.get 4
              local.get 8
              local.get 7
              call $memmove
              drop
              local.get 9
              local.get 7
              i32.store
              br 3 (;@2;)
            end
            local.get 6
            local.get 3
            i32.const 1049596
            call $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E
            unreachable
          end
          local.get 3
          local.get 6
          i32.gt_u
          br_if 0 (;@3;)
        end
        local.get 0
        i32.const 4
        i32.store8
        local.get 6
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        local.get 6
        i32.lt_u
        br_if 1 (;@1;)
        local.get 1
        i32.const 8
        i32.add
        local.tee 9
        i32.const 0
        i32.store
        local.get 3
        local.get 6
        i32.eq
        br_if 0 (;@2;)
        local.get 1
        i32.load
        local.tee 7
        local.get 7
        local.get 6
        i32.add
        local.get 3
        local.get 6
        i32.sub
        local.tee 6
        call $memmove
        drop
        local.get 9
        local.get 6
        i32.store
      end
      local.get 2
      i32.const 32
      i32.add
      global.set $__stack_pointer
      return
    end
    local.get 6
    local.get 3
    i32.const 1049032
    call $_ZN4core5slice5index24slice_end_index_len_fail17h8cef88544c34b3bfE
    unreachable)
  (func $_ZN3std2io8buffered9bufwriter18BufWriter$LT$W$GT$14write_all_cold17ha8846756f7f5598dE (type 9) (param i32 i32 i32 i32)
    (local i32 i32 i64)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 4
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.load offset=4
        local.tee 5
        local.get 1
        i32.load offset=8
        i32.sub
        local.get 3
        i32.ge_u
        br_if 0 (;@2;)
        local.get 4
        i32.const 8
        i32.add
        local.get 1
        call $_ZN3std2io8buffered9bufwriter18BufWriter$LT$W$GT$9flush_buf17he5398a73dc5def27E
        block  ;; label = @3
          local.get 4
          i32.load8_u offset=8
          local.tee 5
          i32.const 4
          i32.ne
          br_if 0 (;@3;)
          local.get 1
          i32.load offset=4
          local.set 5
          br 1 (;@2;)
        end
        local.get 4
        local.get 4
        i32.load offset=12 align=1
        i32.store offset=3 align=1
        local.get 4
        local.get 4
        i32.load offset=9 align=1
        i32.store
        local.get 0
        i32.const 4
        i32.add
        local.get 4
        i32.load offset=3 align=1
        i32.store align=1
        local.get 0
        local.get 4
        i32.load
        i32.store offset=1 align=1
        local.get 0
        local.get 5
        i32.store8
        br 1 (;@1;)
      end
      block  ;; label = @2
        local.get 5
        local.get 3
        i32.le_u
        br_if 0 (;@2;)
        local.get 1
        i32.load
        local.get 1
        i32.load offset=8
        local.tee 5
        i32.add
        local.get 2
        local.get 3
        call $memcpy
        drop
        local.get 0
        i32.const 4
        i32.store8
        local.get 1
        local.get 5
        local.get 3
        i32.add
        i32.store offset=8
        br 1 (;@1;)
      end
      local.get 1
      i32.const 1
      i32.store8 offset=12
      local.get 4
      i32.const 8
      i32.add
      local.get 2
      local.get 3
      call $_ZN3std2io5Write9write_all17h80bc75e0ee5e76e4E
      block  ;; label = @2
        block  ;; label = @3
          local.get 4
          i32.load8_u offset=8
          local.tee 3
          i32.const 4
          i32.eq
          br_if 0 (;@3;)
          block  ;; label = @4
            block  ;; label = @5
              local.get 3
              br_table 0 (;@5;) 1 (;@4;) 0 (;@5;) 0 (;@5;) 0 (;@5;)
            end
            local.get 4
            i32.load offset=12
            local.set 5
          end
          i32.const 1052468
          call $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE
          local.set 2
          local.get 3
          br_if 0 (;@3;)
          i64.const 4
          local.set 6
          local.get 5
          local.get 2
          i32.const 65535
          i32.and
          i32.eq
          br_if 1 (;@2;)
        end
        local.get 4
        i64.load offset=8
        local.set 6
      end
      local.get 0
      local.get 6
      i64.store align=4
      local.get 1
      i32.const 0
      i32.store8 offset=12
    end
    local.get 4
    i32.const 16
    i32.add
    global.set $__stack_pointer)
  (func $_ZN3std3sys4wasi2os12error_string17h7019cb2a4ed0fff2E (type 3) (param i32 i32)
    (local i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 1056
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 1
          local.get 2
          i32.const 0
          i32.const 1024
          call $memset
          local.tee 2
          i32.const 1024
          call $strerror_r
          i32.const 0
          i32.lt_s
          br_if 0 (;@3;)
          local.get 2
          i32.const 1024
          i32.add
          local.get 2
          local.get 2
          call $strlen
          call $_ZN4core3str8converts9from_utf817he9ed2de3b80ea750E
          block  ;; label = @4
            local.get 2
            i32.load offset=1024
            br_if 0 (;@4;)
            local.get 2
            i32.load offset=1028
            local.set 3
            block  ;; label = @5
              block  ;; label = @6
                local.get 2
                i32.const 1032
                i32.add
                i32.load
                local.tee 1
                br_if 0 (;@6;)
                i32.const 1
                local.set 4
                br 1 (;@5;)
              end
              local.get 1
              i32.const -1
              i32.gt_s
              local.tee 5
              i32.eqz
              br_if 3 (;@2;)
              local.get 1
              local.get 5
              call $__rust_alloc
              local.tee 4
              i32.eqz
              br_if 4 (;@1;)
            end
            local.get 4
            local.get 3
            local.get 1
            call $memcpy
            local.set 3
            local.get 0
            local.get 1
            i32.store offset=8
            local.get 0
            local.get 1
            i32.store offset=4
            local.get 0
            local.get 3
            i32.store
            local.get 2
            i32.const 1056
            i32.add
            global.set $__stack_pointer
            return
          end
          local.get 2
          local.get 2
          i64.load offset=1028 align=4
          i64.store offset=1048
          i32.const 1048628
          i32.const 43
          local.get 2
          i32.const 1048
          i32.add
          i32.const 1052436
          i32.const 1052452
          call $_ZN4core6result13unwrap_failed17h0b834de6e1e67b38E
          unreachable
        end
        local.get 2
        i32.const 1044
        i32.add
        i32.const 0
        i32.store
        local.get 2
        i32.const 1048772
        i32.store offset=1040
        local.get 2
        i64.const 1
        i64.store offset=1028 align=4
        local.get 2
        i32.const 1052380
        i32.store offset=1024
        local.get 2
        i32.const 1024
        i32.add
        i32.const 1052420
        call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
        unreachable
      end
      call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
      unreachable
    end
    local.get 1
    local.get 5
    call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
    unreachable)
  (func $_ZN3std4sync9once_lock17OnceLock$LT$T$GT$10initialize17h51ade438ffc532e2E (type 0)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 0
    global.set $__stack_pointer
    block  ;; label = @1
      i32.const 0
      i32.load offset=1059340
      i32.const 3
      i32.eq
      br_if 0 (;@1;)
      local.get 0
      i32.const 1059344
      i32.store offset=8
      local.get 0
      local.get 0
      i32.const 24
      i32.add
      i32.store offset=12
      local.get 0
      local.get 0
      i32.const 8
      i32.add
      i32.store offset=20
      i32.const 1059340
      i32.const 1
      local.get 0
      i32.const 20
      i32.add
      i32.const 1050920
      i32.const 1050904
      call $_ZN3std4sync4once4Once10call_inner17hc3d47dfe4dcd2319E
    end
    local.get 0
    i32.const 32
    i32.add
    global.set $__stack_pointer)
  (func $_ZN61_$LT$$RF$std..io..stdio..Stdout$u20$as$u20$std..io..Write$GT$9write_fmt17hcc04f377b3022566E (type 6) (param i32 i32 i32)
    (local i32 i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.load
            i32.load
            local.tee 1
            i32.load
            i32.const 1059424
            i32.eq
            br_if 0 (;@4;)
            local.get 1
            i32.load8_u offset=28
            local.set 4
            local.get 1
            i32.const 1
            i32.store8 offset=28
            local.get 3
            local.get 4
            i32.const 1
            i32.and
            local.tee 4
            i32.store8 offset=8
            local.get 4
            br_if 2 (;@2;)
            local.get 1
            i32.const 1
            i32.store offset=4
            local.get 1
            i32.const 1059424
            i32.store
            br 1 (;@3;)
          end
          local.get 1
          i32.load offset=4
          i32.const 1
          i32.add
          local.tee 4
          i32.eqz
          br_if 2 (;@1;)
          local.get 1
          local.get 4
          i32.store offset=4
        end
        local.get 3
        local.get 1
        i32.store offset=4
        local.get 3
        i32.const 4
        i32.store8 offset=12
        local.get 3
        local.get 3
        i32.const 4
        i32.add
        i32.store offset=8
        local.get 3
        i32.const 24
        i32.add
        i32.const 16
        i32.add
        local.get 2
        i32.const 16
        i32.add
        i64.load align=4
        i64.store
        local.get 3
        i32.const 24
        i32.add
        i32.const 8
        i32.add
        local.get 2
        i32.const 8
        i32.add
        i64.load align=4
        i64.store
        local.get 3
        local.get 2
        i64.load align=4
        i64.store offset=24
        block  ;; label = @3
          block  ;; label = @4
            local.get 3
            i32.const 8
            i32.add
            i32.const 1050728
            local.get 3
            i32.const 24
            i32.add
            call $_ZN4core3fmt5write17hb15fae9d0358166aE
            i32.eqz
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 3
              i32.load8_u offset=12
              i32.const 4
              i32.ne
              br_if 0 (;@5;)
              local.get 0
              i32.const 1050716
              i32.store offset=4
              local.get 0
              i32.const 2
              i32.store
              br 2 (;@3;)
            end
            local.get 0
            local.get 3
            i64.load offset=12 align=4
            i64.store align=4
            br 1 (;@3;)
          end
          local.get 0
          i32.const 4
          i32.store8
          local.get 3
          i32.load8_u offset=12
          i32.const 3
          i32.ne
          br_if 0 (;@3;)
          local.get 3
          i32.const 8
          i32.add
          i32.const 8
          i32.add
          i32.load
          local.tee 1
          i32.load
          local.get 1
          i32.load offset=4
          i32.load
          call_indirect (type 1)
          block  ;; label = @4
            local.get 1
            i32.load offset=4
            local.tee 2
            i32.const 4
            i32.add
            i32.load
            local.tee 0
            i32.eqz
            br_if 0 (;@4;)
            local.get 1
            i32.load
            local.get 0
            local.get 2
            i32.const 8
            i32.add
            i32.load
            call $__rust_dealloc
          end
          local.get 3
          i32.load offset=16
          i32.const 12
          i32.const 4
          call $__rust_dealloc
        end
        local.get 3
        i32.load offset=4
        local.tee 1
        local.get 1
        i32.load offset=4
        i32.const -1
        i32.add
        local.tee 2
        i32.store offset=4
        block  ;; label = @3
          local.get 2
          br_if 0 (;@3;)
          local.get 1
          i32.const 0
          i32.store8 offset=28
          local.get 1
          i32.const 0
          i32.store
        end
        local.get 3
        i32.const 48
        i32.add
        global.set $__stack_pointer
        return
      end
      local.get 3
      i32.const 0
      i32.store offset=44
      local.get 3
      i32.const 1048772
      i32.store offset=40
      local.get 3
      i64.const 1
      i64.store offset=28 align=4
      local.get 3
      i32.const 1052244
      i32.store offset=24
      local.get 3
      i32.const 8
      i32.add
      local.get 3
      i32.const 24
      i32.add
      call $_ZN4core9panicking13assert_failed17had14dc5ddcedac37E
      unreachable
    end
    i32.const 1051240
    i32.const 38
    i32.const 1051316
    call $_ZN4core6option13expect_failed17hc52795d163105200E
    unreachable)
  (func $_ZN3std2io5stdio6_print17hb5031aec3871df70E (type 1) (param i32)
    (local i32 i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 96
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    local.get 1
    i32.const 16
    i32.add
    local.get 0
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 1
    i32.const 8
    i32.add
    local.get 0
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 1
    local.get 0
    i64.load align=4
    i64.store
    local.get 1
    i32.const 6
    i32.store offset=28
    local.get 1
    i32.const 1050588
    i32.store offset=24
    block  ;; label = @1
      block  ;; label = @2
        i32.const 0
        i32.load8_u offset=1059336
        i32.eqz
        br_if 0 (;@2;)
        block  ;; label = @3
          i32.const 0
          i32.load8_u offset=1059416
          br_if 0 (;@3;)
          i32.const 0
          i32.const 1
          i32.store8 offset=1059416
          i32.const 0
          i32.const 0
          i32.store offset=1059420
          br 1 (;@2;)
        end
        i32.const 0
        i32.load offset=1059420
        local.set 0
        i32.const 0
        i32.const 0
        i32.store offset=1059420
        local.get 0
        i32.eqz
        br_if 0 (;@2;)
        local.get 0
        i32.load8_u offset=8
        local.set 2
        i32.const 1
        local.set 3
        local.get 0
        i32.const 1
        i32.store8 offset=8
        local.get 1
        local.get 2
        i32.const 1
        i32.and
        local.tee 2
        i32.store8 offset=56
        block  ;; label = @3
          local.get 2
          br_if 0 (;@3;)
          block  ;; label = @4
            i32.const 0
            i32.load offset=1059404
            i32.const 2147483647
            i32.and
            i32.eqz
            br_if 0 (;@4;)
            call $_ZN3std9panicking11panic_count17is_zero_slow_path17hea85e77ac3ac7cbaE
            local.set 3
          end
          local.get 1
          i32.const 4
          i32.store8 offset=60
          local.get 1
          local.get 0
          i32.const 12
          i32.add
          i32.store offset=56
          local.get 1
          i32.const 72
          i32.add
          i32.const 16
          i32.add
          local.get 1
          i32.const 16
          i32.add
          i64.load
          i64.store
          local.get 1
          i32.const 72
          i32.add
          i32.const 8
          i32.add
          local.get 1
          i32.const 8
          i32.add
          i64.load
          i64.store
          local.get 1
          local.get 1
          i64.load
          i64.store offset=72
          local.get 1
          i32.const 56
          i32.add
          i32.const 1050752
          local.get 1
          i32.const 72
          i32.add
          call $_ZN4core3fmt5write17hb15fae9d0358166aE
          local.set 2
          local.get 1
          i32.load8_u offset=60
          local.set 4
          block  ;; label = @4
            block  ;; label = @5
              local.get 2
              i32.eqz
              br_if 0 (;@5;)
              local.get 4
              i32.const 255
              i32.and
              i32.const 4
              i32.eq
              br_if 1 (;@4;)
              local.get 1
              i32.load8_u offset=60
              i32.const 3
              i32.ne
              br_if 1 (;@4;)
              local.get 1
              i32.const 64
              i32.add
              i32.load
              local.tee 2
              i32.load
              local.get 2
              i32.load offset=4
              i32.load
              call_indirect (type 1)
              block  ;; label = @6
                local.get 2
                i32.load offset=4
                local.tee 4
                i32.const 4
                i32.add
                i32.load
                local.tee 5
                i32.eqz
                br_if 0 (;@6;)
                local.get 2
                i32.load
                local.get 5
                local.get 4
                i32.const 8
                i32.add
                i32.load
                call $__rust_dealloc
              end
              local.get 2
              i32.const 12
              i32.const 4
              call $__rust_dealloc
              br 1 (;@4;)
            end
            local.get 4
            i32.const 255
            i32.and
            i32.const 3
            i32.ne
            br_if 0 (;@4;)
            local.get 1
            i32.const 56
            i32.add
            i32.const 8
            i32.add
            i32.load
            local.tee 2
            i32.load
            local.get 2
            i32.load offset=4
            i32.load
            call_indirect (type 1)
            block  ;; label = @5
              local.get 2
              i32.load offset=4
              local.tee 4
              i32.const 4
              i32.add
              i32.load
              local.tee 5
              i32.eqz
              br_if 0 (;@5;)
              local.get 2
              i32.load
              local.get 5
              local.get 4
              i32.const 8
              i32.add
              i32.load
              call $__rust_dealloc
            end
            local.get 1
            i32.load offset=64
            i32.const 12
            i32.const 4
            call $__rust_dealloc
          end
          block  ;; label = @4
            local.get 3
            i32.eqz
            br_if 0 (;@4;)
            i32.const 0
            i32.load offset=1059404
            i32.const 2147483647
            i32.and
            i32.eqz
            br_if 0 (;@4;)
            call $_ZN3std9panicking11panic_count17is_zero_slow_path17hea85e77ac3ac7cbaE
            br_if 0 (;@4;)
            local.get 0
            i32.const 1
            i32.store8 offset=9
          end
          local.get 0
          i32.const 0
          i32.store8 offset=8
          i32.const 0
          i32.load offset=1059420
          local.set 3
          i32.const 0
          local.get 0
          i32.store offset=1059420
          local.get 3
          i32.eqz
          br_if 2 (;@1;)
          local.get 3
          local.get 3
          i32.load
          local.tee 0
          i32.const -1
          i32.add
          i32.store
          local.get 0
          i32.const 1
          i32.ne
          br_if 2 (;@1;)
          local.get 3
          call $_ZN5alloc4sync12Arc$LT$T$GT$9drop_slow17h3851a9e5c952beabE
          br 2 (;@1;)
        end
        local.get 1
        i32.const 0
        i32.store offset=92
        local.get 1
        i32.const 1048772
        i32.store offset=88
        local.get 1
        i64.const 1
        i64.store offset=76 align=4
        local.get 1
        i32.const 1052244
        i32.store offset=72
        local.get 1
        i32.const 56
        i32.add
        local.get 1
        i32.const 72
        i32.add
        call $_ZN4core9panicking13assert_failed17had14dc5ddcedac37E
        unreachable
      end
      block  ;; label = @2
        i32.const 0
        i32.load offset=1059340
        i32.const 3
        i32.eq
        br_if 0 (;@2;)
        call $_ZN3std4sync9once_lock17OnceLock$LT$T$GT$10initialize17h51ade438ffc532e2E
      end
      local.get 1
      i32.const 1059344
      i32.store offset=44
      local.get 1
      local.get 1
      i32.const 44
      i32.add
      i32.store offset=56
      local.get 1
      i32.const 72
      i32.add
      i32.const 16
      i32.add
      local.get 1
      i32.const 16
      i32.add
      i64.load
      i64.store
      local.get 1
      i32.const 72
      i32.add
      i32.const 8
      i32.add
      local.get 1
      i32.const 8
      i32.add
      i64.load
      i64.store
      local.get 1
      local.get 1
      i64.load
      i64.store offset=72
      local.get 1
      i32.const 32
      i32.add
      local.get 1
      i32.const 56
      i32.add
      local.get 1
      i32.const 72
      i32.add
      call $_ZN61_$LT$$RF$std..io..stdio..Stdout$u20$as$u20$std..io..Write$GT$9write_fmt17hcc04f377b3022566E
      local.get 1
      i32.load8_u offset=32
      i32.const 4
      i32.eq
      br_if 0 (;@1;)
      local.get 1
      local.get 1
      i64.load offset=32
      i64.store offset=48
      local.get 1
      i32.const 92
      i32.add
      i32.const 2
      i32.store
      local.get 1
      i32.const 68
      i32.add
      i32.const 13
      i32.store
      local.get 1
      i64.const 2
      i64.store offset=76 align=4
      local.get 1
      i32.const 1050556
      i32.store offset=72
      local.get 1
      i32.const 12
      i32.store offset=60
      local.get 1
      local.get 1
      i32.const 56
      i32.add
      i32.store offset=88
      local.get 1
      local.get 1
      i32.const 48
      i32.add
      i32.store offset=64
      local.get 1
      local.get 1
      i32.const 24
      i32.add
      i32.store offset=56
      local.get 1
      i32.const 72
      i32.add
      i32.const 1050572
      call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
      unreachable
    end
    local.get 1
    i32.const 96
    i32.add
    global.set $__stack_pointer)
  (func $_ZN3std2io5Write9write_fmt17hc24349e17874757dE (type 6) (param i32 i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    i32.const 4
    i32.store8 offset=12
    local.get 3
    local.get 1
    i32.store offset=8
    local.get 3
    i32.const 24
    i32.add
    i32.const 16
    i32.add
    local.get 2
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 3
    i32.const 24
    i32.add
    i32.const 8
    i32.add
    local.get 2
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 3
    local.get 2
    i64.load align=4
    i64.store offset=24
    block  ;; label = @1
      block  ;; label = @2
        local.get 3
        i32.const 8
        i32.add
        i32.const 1050752
        local.get 3
        i32.const 24
        i32.add
        call $_ZN4core3fmt5write17hb15fae9d0358166aE
        i32.eqz
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 3
          i32.load8_u offset=12
          i32.const 4
          i32.ne
          br_if 0 (;@3;)
          local.get 0
          i32.const 1050716
          i32.store offset=4
          local.get 0
          i32.const 2
          i32.store
          br 2 (;@1;)
        end
        local.get 0
        local.get 3
        i64.load offset=12 align=4
        i64.store align=4
        br 1 (;@1;)
      end
      local.get 0
      i32.const 4
      i32.store8
      local.get 3
      i32.load8_u offset=12
      i32.const 3
      i32.ne
      br_if 0 (;@1;)
      local.get 3
      i32.const 8
      i32.add
      i32.const 8
      i32.add
      i32.load
      local.tee 2
      i32.load
      local.get 2
      i32.load offset=4
      i32.load
      call_indirect (type 1)
      block  ;; label = @2
        local.get 2
        i32.load offset=4
        local.tee 0
        i32.const 4
        i32.add
        i32.load
        local.tee 1
        i32.eqz
        br_if 0 (;@2;)
        local.get 2
        i32.load
        local.get 1
        local.get 0
        i32.const 8
        i32.add
        i32.load
        call $__rust_dealloc
      end
      local.get 3
      i32.load offset=16
      i32.const 12
      i32.const 4
      call $__rust_dealloc
    end
    local.get 3
    i32.const 48
    i32.add
    global.set $__stack_pointer)
  (func $_ZN80_$LT$std..io..Write..write_fmt..Adapter$LT$T$GT$$u20$as$u20$core..fmt..Write$GT$9write_str17h6415def010a9be9cE (type 7) (param i32 i32 i32) (result i32)
    (local i32 i64 i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    i32.const 8
    i32.add
    local.get 0
    i32.load
    local.get 1
    local.get 2
    call $_ZN61_$LT$std..io..stdio..StdoutLock$u20$as$u20$std..io..Write$GT$9write_all17h7430178078d1718bE
    block  ;; label = @1
      local.get 3
      i32.load8_u offset=8
      local.tee 2
      i32.const 4
      i32.eq
      br_if 0 (;@1;)
      local.get 3
      i64.load offset=8
      local.set 4
      block  ;; label = @2
        local.get 0
        i32.load8_u offset=4
        i32.const 3
        i32.ne
        br_if 0 (;@2;)
        local.get 0
        i32.const 8
        i32.add
        i32.load
        local.tee 1
        i32.load
        local.get 1
        i32.load offset=4
        i32.load
        call_indirect (type 1)
        block  ;; label = @3
          local.get 1
          i32.load offset=4
          local.tee 5
          i32.const 4
          i32.add
          i32.load
          local.tee 6
          i32.eqz
          br_if 0 (;@3;)
          local.get 1
          i32.load
          local.get 6
          local.get 5
          i32.const 8
          i32.add
          i32.load
          call $__rust_dealloc
        end
        local.get 1
        i32.const 12
        i32.const 4
        call $__rust_dealloc
      end
      local.get 0
      local.get 4
      i64.store offset=4 align=4
    end
    local.get 3
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 2
    i32.const 4
    i32.ne)
  (func $_ZN80_$LT$std..io..Write..write_fmt..Adapter$LT$T$GT$$u20$as$u20$core..fmt..Write$GT$9write_str17h9aa964bb9beacebdE (type 7) (param i32 i32 i32) (result i32)
    (local i32)
    block  ;; label = @1
      local.get 0
      i32.load
      local.tee 0
      i32.const 4
      i32.add
      i32.load
      local.get 0
      i32.load offset=8
      local.tee 3
      i32.sub
      local.get 2
      i32.ge_u
      br_if 0 (;@1;)
      local.get 0
      local.get 3
      local.get 2
      call $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$7reserve21do_reserve_and_handle17ha33bee7b1e5ebd92E
      local.get 0
      i32.load offset=8
      local.set 3
    end
    local.get 0
    i32.load
    local.get 3
    i32.add
    local.get 1
    local.get 2
    call $memcpy
    drop
    local.get 0
    local.get 3
    local.get 2
    i32.add
    i32.store offset=8
    i32.const 0)
  (func $_ZN3std5panic19get_backtrace_style17h0c1afb8971b741b4E (type 10) (result i32)
    (local i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 0
    global.set $__stack_pointer
    i32.const 0
    local.set 1
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              i32.const 0
              i32.load offset=1059376
              br_table 3 (;@2;) 4 (;@1;) 1 (;@4;) 2 (;@3;) 0 (;@5;)
            end
            i32.const 1048915
            i32.const 40
            i32.const 1050800
            call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
            unreachable
          end
          i32.const 1
          local.set 1
          br 2 (;@1;)
        end
        i32.const 2
        local.set 1
        br 1 (;@1;)
      end
      local.get 0
      i32.const 1049440
      i32.const 14
      call $_ZN3std3env7_var_os17ha4645996074a0c32E
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          i32.load
          local.tee 1
          i32.eqz
          br_if 0 (;@3;)
          i32.const 0
          local.set 2
          local.get 0
          i32.load offset=4
          local.set 3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 0
                i32.load offset=8
                i32.const -1
                i32.add
                br_table 0 (;@6;) 2 (;@4;) 2 (;@4;) 1 (;@5;) 2 (;@4;)
              end
              i32.const -2
              i32.const 0
              local.get 1
              i32.load8_u
              i32.const 48
              i32.eq
              select
              local.set 2
              br 1 (;@4;)
            end
            local.get 1
            i32.const 1050816
            i32.const 4
            call $memcmp
            i32.eqz
            local.set 2
          end
          block  ;; label = @4
            local.get 3
            i32.eqz
            br_if 0 (;@4;)
            local.get 1
            local.get 3
            i32.const 1
            call $__rust_dealloc
          end
          i32.const 1
          local.set 3
          i32.const 0
          local.set 1
          block  ;; label = @4
            local.get 2
            i32.const 3
            i32.and
            br_table 2 (;@2;) 0 (;@4;) 1 (;@3;) 2 (;@2;)
          end
          i32.const 2
          local.set 3
          i32.const 1
          local.set 1
          br 1 (;@2;)
        end
        i32.const 3
        local.set 3
        i32.const 2
        local.set 1
      end
      i32.const 0
      local.get 3
      i32.store offset=1059376
    end
    local.get 0
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN55_$LT$std..path..PathBuf$u20$as$u20$core..fmt..Debug$GT$3fmt17hd047898216b3195bE (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    i32.const 8
    i32.add
    local.get 0
    i32.load
    local.get 0
    i32.const 8
    i32.add
    i32.load
    call $_ZN4core3str5lossy10Utf8Chunks3new17h4e8088422ddc37e0E
    local.get 2
    local.get 2
    i64.load offset=8
    i64.store offset=24
    local.get 2
    local.get 2
    i32.const 24
    i32.add
    call $_ZN4core3str5lossy10Utf8Chunks5debug17h6c56fa00eceb1f17E
    local.get 2
    local.get 2
    i64.load
    i64.store offset=16
    local.get 2
    i32.const 16
    i32.add
    local.get 1
    call $_ZN60_$LT$core..str..lossy..Debug$u20$as$u20$core..fmt..Debug$GT$3fmt17h56048af2e252c929E
    local.set 0
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN3std7process5abort17hde0fb92edffbc784E (type 0)
    call $_ZN3std3sys4wasi14abort_internal17h410a405cf1d1c758E
    unreachable)
  (func $_ZN3std4sync9once_lock17OnceLock$LT$T$GT$10initialize17ha52c4cb6401dc2a7E (type 1) (param i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    block  ;; label = @1
      i32.const 0
      i32.load offset=1059340
      i32.const 3
      i32.eq
      br_if 0 (;@1;)
      local.get 1
      i32.const 1059344
      i32.store offset=12
      local.get 1
      local.get 0
      i32.store offset=8
      local.get 1
      local.get 1
      i32.const 24
      i32.add
      i32.store offset=16
      local.get 1
      local.get 1
      i32.const 8
      i32.add
      i32.store offset=20
      i32.const 1059340
      i32.const 1
      local.get 1
      i32.const 20
      i32.add
      i32.const 1050884
      i32.const 1050904
      call $_ZN3std4sync4once4Once10call_inner17hc3d47dfe4dcd2319E
    end
    local.get 1
    i32.const 32
    i32.add
    global.set $__stack_pointer)
  (func $_ZN3std4sync4once4Once15call_once_force28_$u7b$$u7b$closure$u7d$$u7d$17h140d3c4ba437bc42E (type 3) (param i32 i32)
    (local i32)
    local.get 0
    i32.load
    local.tee 0
    i32.load
    local.set 2
    local.get 0
    i32.const 0
    i32.store
    block  ;; label = @1
      local.get 2
      br_if 0 (;@1;)
      i32.const 1048872
      i32.const 43
      i32.const 1050940
      call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
      unreachable
    end
    local.get 0
    i32.load offset=4
    local.set 0
    local.get 2
    i32.const 1
    i32.store8
    local.get 0
    i64.const 0
    i64.store offset=16 align=4
    local.get 0
    i64.const 4294967296
    i64.store offset=8 align=4
    local.get 0
    i64.const 0
    i64.store align=4
    local.get 0
    i32.const 24
    i32.add
    i32.const 0
    i32.store8
    local.get 0
    i32.const 0
    i32.store8 offset=28)
  (func $_ZN3std4sync4once4Once15call_once_force28_$u7b$$u7b$closure$u7d$$u7d$17ha5ace6a88633e7feE (type 3) (param i32 i32)
    (local i32)
    local.get 0
    i32.load
    local.tee 2
    i32.load
    local.set 0
    local.get 2
    i32.const 0
    i32.store
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.eqz
        br_if 0 (;@2;)
        i32.const 1024
        i32.const 1
        call $__rust_alloc
        local.tee 2
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        i32.const 0
        i32.store8 offset=28
        local.get 0
        i32.const 0
        i32.store8 offset=24
        local.get 0
        i64.const 1024
        i64.store offset=16 align=4
        local.get 0
        local.get 2
        i32.store offset=12
        local.get 0
        i32.const 0
        i32.store offset=8
        local.get 0
        i64.const 0
        i64.store align=4
        return
      end
      i32.const 1048872
      i32.const 43
      i32.const 1050940
      call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
      unreachable
    end
    i32.const 1024
    i32.const 1
    call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
    unreachable)
  (func $_ZN76_$LT$std..sync..poison..PoisonError$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h5d1b804931a59b0fE (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    i32.const 8
    i32.add
    local.get 1
    i32.const 1051108
    i32.const 11
    call $_ZN4core3fmt9Formatter12debug_struct17ha3f5ef7f2b046eb7E
    local.get 2
    i32.const 8
    i32.add
    call $_ZN4core3fmt8builders11DebugStruct21finish_non_exhaustive17h729a9bc3887ef43dE
    local.set 1
    local.get 2
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN91_$LT$std..sys_common..backtrace.._print..DisplayBacktrace$u20$as$u20$core..fmt..Display$GT$3fmt17hb243f3efbed6872fE (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 0
    i32.load8_u
    local.set 3
    local.get 2
    i32.const 8
    i32.add
    call $_ZN3std3env11current_dir17h8ee6faca341aee6dE
    block  ;; label = @1
      block  ;; label = @2
        local.get 2
        i32.load offset=8
        local.tee 0
        i32.eqz
        br_if 0 (;@2;)
        local.get 2
        i32.load offset=12
        local.set 4
        br 1 (;@1;)
      end
      local.get 2
      i32.load8_u offset=12
      i32.const 3
      i32.ne
      br_if 0 (;@1;)
      local.get 2
      i32.const 8
      i32.add
      i32.const 8
      i32.add
      i32.load
      local.tee 4
      i32.load
      local.get 4
      i32.load offset=4
      i32.load
      call_indirect (type 1)
      block  ;; label = @2
        local.get 4
        i32.load offset=4
        local.tee 5
        i32.const 4
        i32.add
        i32.load
        local.tee 6
        i32.eqz
        br_if 0 (;@2;)
        local.get 4
        i32.load
        local.get 6
        local.get 5
        i32.const 8
        i32.add
        i32.load
        call $__rust_dealloc
      end
      local.get 4
      i32.const 12
      i32.const 4
      call $__rust_dealloc
    end
    local.get 2
    i32.const 28
    i32.add
    i32.const 0
    i32.store
    local.get 2
    i32.const 1048772
    i32.store offset=24
    local.get 2
    i64.const 1
    i64.store offset=12 align=4
    local.get 2
    i32.const 1051136
    i32.store offset=8
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 1
            local.get 2
            i32.const 8
            i32.add
            call $_ZN4core3fmt9Formatter9write_fmt17h6645fe2bd1b938a8E
            br_if 0 (;@4;)
            local.get 3
            i32.const 255
            i32.and
            br_if 1 (;@3;)
            local.get 2
            i32.const 28
            i32.add
            i32.const 0
            i32.store
            local.get 2
            i32.const 1048772
            i32.store offset=24
            local.get 2
            i64.const 1
            i64.store offset=12 align=4
            local.get 2
            i32.const 1051232
            i32.store offset=8
            local.get 1
            local.get 2
            i32.const 8
            i32.add
            call $_ZN4core3fmt9Formatter9write_fmt17h6645fe2bd1b938a8E
            i32.eqz
            br_if 1 (;@3;)
          end
          i32.const 1
          local.set 1
          local.get 0
          i32.eqz
          br_if 2 (;@1;)
          local.get 4
          i32.eqz
          br_if 2 (;@1;)
          br 1 (;@2;)
        end
        i32.const 0
        local.set 1
        local.get 0
        i32.eqz
        br_if 1 (;@1;)
        local.get 4
        i32.eqz
        br_if 1 (;@1;)
      end
      local.get 0
      local.get 4
      i32.const 1
      call $__rust_dealloc
    end
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN3std10sys_common9backtrace26__rust_end_short_backtrace17h0cd229888db2b8aeE (type 1) (param i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    local.get 1
    i32.const 8
    i32.add
    local.get 0
    i32.const 8
    i32.add
    i32.load
    i32.store
    local.get 1
    local.get 0
    i64.load align=4
    i64.store
    local.get 1
    call $_ZN3std9panicking19begin_panic_handler28_$u7b$$u7b$closure$u7d$$u7d$17h340ba300319b487dE
    unreachable)
  (func $_ZN3std9panicking19begin_panic_handler28_$u7b$$u7b$closure$u7d$$u7d$17h340ba300319b487dE (type 1) (param i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    local.get 0
    i32.load
    local.tee 2
    i32.const 20
    i32.add
    i32.load
    local.set 3
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 2
            i32.load offset=4
            br_table 0 (;@4;) 1 (;@3;) 3 (;@1;)
          end
          local.get 3
          br_if 2 (;@1;)
          i32.const 1048772
          local.set 2
          i32.const 0
          local.set 3
          br 1 (;@2;)
        end
        local.get 3
        br_if 1 (;@1;)
        local.get 2
        i32.load
        local.tee 2
        i32.load offset=4
        local.set 3
        local.get 2
        i32.load
        local.set 2
      end
      local.get 1
      local.get 3
      i32.store offset=4
      local.get 1
      local.get 2
      i32.store
      local.get 1
      i32.const 1051848
      local.get 0
      i32.load offset=4
      local.tee 2
      call $_ZN4core5panic10panic_info9PanicInfo7message17hbe3048ebb862958cE
      local.get 0
      i32.load offset=8
      local.get 2
      call $_ZN4core5panic10panic_info9PanicInfo10can_unwind17hb60e365aaef4a33bE
      call $_ZN3std9panicking20rust_panic_with_hook17h0a5a34832cbe3154E
      unreachable
    end
    local.get 1
    i32.const 0
    i32.store offset=4
    local.get 1
    local.get 2
    i32.store
    local.get 1
    i32.const 1051828
    local.get 0
    i32.load offset=4
    local.tee 2
    call $_ZN4core5panic10panic_info9PanicInfo7message17hbe3048ebb862958cE
    local.get 0
    i32.load offset=8
    local.get 2
    call $_ZN4core5panic10panic_info9PanicInfo10can_unwind17hb60e365aaef4a33bE
    call $_ZN3std9panicking20rust_panic_with_hook17h0a5a34832cbe3154E
    unreachable)
  (func $_ZN3std5alloc24default_alloc_error_hook17hc7fd4e8fd00ec2b2E (type 3) (param i32 i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 64
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      i32.const 0
      i32.load8_u offset=1059328
      br_if 0 (;@1;)
      local.get 2
      i32.const 4
      i32.store offset=4
      local.get 2
      local.get 0
      i32.store offset=12
      local.get 2
      local.get 2
      i32.const 12
      i32.add
      i32.store
      local.get 2
      i32.const 4
      i32.store8 offset=20
      local.get 2
      local.get 2
      i32.const 56
      i32.add
      i32.store offset=16
      local.get 2
      i32.const 1
      i32.store offset=52
      local.get 2
      i64.const 2
      i64.store offset=36 align=4
      local.get 2
      i32.const 1051492
      i32.store offset=32
      local.get 2
      local.get 2
      i32.store offset=48
      local.get 2
      i32.const 16
      i32.add
      i32.const 1050676
      local.get 2
      i32.const 32
      i32.add
      call $_ZN4core3fmt5write17hb15fae9d0358166aE
      local.set 0
      local.get 2
      i32.load8_u offset=20
      local.set 3
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          i32.eqz
          br_if 0 (;@3;)
          local.get 3
          i32.const 255
          i32.and
          i32.const 4
          i32.eq
          br_if 1 (;@2;)
          local.get 2
          i32.load8_u offset=20
          i32.const 3
          i32.ne
          br_if 1 (;@2;)
          local.get 2
          i32.const 24
          i32.add
          i32.load
          local.tee 0
          i32.load
          local.get 0
          i32.load offset=4
          i32.load
          call_indirect (type 1)
          block  ;; label = @4
            local.get 0
            i32.load offset=4
            local.tee 3
            i32.const 4
            i32.add
            i32.load
            local.tee 4
            i32.eqz
            br_if 0 (;@4;)
            local.get 0
            i32.load
            local.get 4
            local.get 3
            i32.const 8
            i32.add
            i32.load
            call $__rust_dealloc
          end
          local.get 0
          i32.const 12
          i32.const 4
          call $__rust_dealloc
          br 1 (;@2;)
        end
        local.get 3
        i32.const 255
        i32.and
        i32.const 3
        i32.ne
        br_if 0 (;@2;)
        local.get 2
        i32.const 16
        i32.add
        i32.const 8
        i32.add
        i32.load
        local.tee 0
        i32.load
        local.get 0
        i32.load offset=4
        i32.load
        call_indirect (type 1)
        block  ;; label = @3
          local.get 0
          i32.load offset=4
          local.tee 3
          i32.const 4
          i32.add
          i32.load
          local.tee 4
          i32.eqz
          br_if 0 (;@3;)
          local.get 0
          i32.load
          local.get 4
          local.get 3
          i32.const 8
          i32.add
          i32.load
          call $__rust_dealloc
        end
        local.get 2
        i32.load offset=24
        i32.const 12
        i32.const 4
        call $__rust_dealloc
      end
      local.get 2
      i32.const 64
      i32.add
      global.set $__stack_pointer
      return
    end
    local.get 2
    i32.const 52
    i32.add
    i32.const 1
    i32.store
    local.get 2
    i64.const 2
    i64.store offset=36 align=4
    local.get 2
    i32.const 1051492
    i32.store offset=32
    local.get 2
    i32.const 4
    i32.store offset=20
    local.get 2
    local.get 0
    i32.store
    local.get 2
    local.get 2
    i32.const 16
    i32.add
    i32.store offset=48
    local.get 2
    local.get 2
    i32.store offset=16
    local.get 2
    i32.const 32
    i32.add
    i32.const 1051532
    call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
    unreachable)
  (func $rust_oom (type 3) (param i32 i32)
    (local i32)
    local.get 0
    local.get 1
    i32.const 0
    i32.load offset=1059384
    local.tee 2
    i32.const 14
    local.get 2
    select
    call_indirect (type 3)
    call $_ZN3std7process5abort17hde0fb92edffbc784E
    unreachable)
  (func $__rdl_alloc (type 5) (param i32 i32) (result i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 8
        i32.gt_u
        br_if 0 (;@2;)
        local.get 1
        local.get 0
        i32.le_u
        br_if 1 (;@1;)
      end
      local.get 1
      local.get 0
      call $aligned_alloc
      return
    end
    local.get 0
    call $malloc)
  (func $__rdl_dealloc (type 6) (param i32 i32 i32)
    local.get 0
    call $free)
  (func $__rdl_realloc (type 8) (param i32 i32 i32 i32) (result i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 2
        i32.const 8
        i32.gt_u
        br_if 0 (;@2;)
        local.get 2
        local.get 3
        i32.le_u
        br_if 1 (;@1;)
      end
      block  ;; label = @2
        local.get 2
        local.get 3
        call $aligned_alloc
        local.tee 2
        br_if 0 (;@2;)
        i32.const 0
        return
      end
      local.get 2
      local.get 0
      local.get 1
      local.get 3
      local.get 1
      local.get 3
      i32.lt_u
      select
      call $memcpy
      local.set 3
      local.get 0
      call $free
      local.get 3
      return
    end
    local.get 0
    local.get 3
    call $realloc)
  (func $_ZN3std9panicking12default_hook28_$u7b$$u7b$closure$u7d$$u7d$17h142c39c8f696cd66E (type 6) (param i32 i32 i32)
    (local i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 64
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    i32.const 20
    i32.add
    i32.const 3
    i32.store
    local.get 3
    i32.const 32
    i32.add
    i32.const 20
    i32.add
    i32.const 15
    i32.store
    local.get 3
    i32.const 44
    i32.add
    i32.const 12
    i32.store
    local.get 3
    i64.const 4
    i64.store offset=4 align=4
    local.get 3
    i32.const 1051644
    i32.store
    local.get 3
    i32.const 12
    i32.store offset=36
    local.get 3
    local.get 0
    i32.load offset=8
    i32.store offset=48
    local.get 3
    local.get 0
    i32.load offset=4
    i32.store offset=40
    local.get 3
    local.get 0
    i32.load
    i32.store offset=32
    local.get 3
    local.get 3
    i32.const 32
    i32.add
    i32.store offset=16
    local.get 3
    i32.const 24
    i32.add
    local.get 1
    local.get 3
    local.get 2
    call_indirect (type 6)
    block  ;; label = @1
      local.get 3
      i32.load8_u offset=24
      i32.const 3
      i32.ne
      br_if 0 (;@1;)
      local.get 3
      i32.load offset=28
      local.tee 4
      i32.load
      local.get 4
      i32.load offset=4
      i32.load
      call_indirect (type 1)
      block  ;; label = @2
        local.get 4
        i32.load offset=4
        local.tee 5
        i32.const 4
        i32.add
        i32.load
        local.tee 6
        i32.eqz
        br_if 0 (;@2;)
        local.get 4
        i32.load
        local.get 6
        local.get 5
        i32.const 8
        i32.add
        i32.load
        call $__rust_dealloc
      end
      local.get 4
      i32.const 12
      i32.const 4
      call $__rust_dealloc
    end
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          i32.load offset=12
          i32.load8_u
          local.tee 0
          i32.const 3
          i32.eq
          br_if 0 (;@3;)
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 0
                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;) 0 (;@6;)
              end
              i32.const 0
              i32.load8_u offset=1059380
              local.set 0
              i32.const 0
              i32.const 1
              i32.store8 offset=1059380
              local.get 3
              local.get 0
              i32.store8
              local.get 0
              br_if 3 (;@2;)
              local.get 3
              i32.const 52
              i32.add
              i32.const 1
              i32.store
              local.get 3
              i64.const 1
              i64.store offset=36 align=4
              local.get 3
              i32.const 1049612
              i32.store offset=32
              local.get 3
              i32.const 16
              i32.store offset=4
              local.get 3
              i32.const 0
              i32.store8 offset=63
              local.get 3
              local.get 3
              i32.store offset=48
              local.get 3
              local.get 3
              i32.const 63
              i32.add
              i32.store
              local.get 3
              i32.const 24
              i32.add
              local.get 1
              local.get 3
              i32.const 32
              i32.add
              local.get 2
              call_indirect (type 6)
              i32.const 0
              i32.const 0
              i32.store8 offset=1059380
              local.get 3
              i32.load8_u offset=24
              i32.const 3
              i32.ne
              br_if 2 (;@3;)
              local.get 3
              i32.load offset=28
              local.tee 0
              i32.load
              local.get 0
              i32.load offset=4
              i32.load
              call_indirect (type 1)
              block  ;; label = @6
                local.get 0
                i32.load offset=4
                local.tee 1
                i32.const 4
                i32.add
                i32.load
                local.tee 2
                i32.eqz
                br_if 0 (;@6;)
                local.get 0
                i32.load
                local.get 2
                local.get 1
                i32.const 8
                i32.add
                i32.load
                call $__rust_dealloc
              end
              local.get 0
              i32.const 12
              i32.const 4
              call $__rust_dealloc
              br 2 (;@3;)
            end
            i32.const 0
            i32.load8_u offset=1059380
            local.set 0
            i32.const 0
            i32.const 1
            i32.store8 offset=1059380
            local.get 3
            local.get 0
            i32.store8
            local.get 0
            br_if 3 (;@1;)
            local.get 3
            i32.const 52
            i32.add
            i32.const 1
            i32.store
            local.get 3
            i64.const 1
            i64.store offset=36 align=4
            local.get 3
            i32.const 1049612
            i32.store offset=32
            local.get 3
            i32.const 16
            i32.store offset=4
            local.get 3
            i32.const 1
            i32.store8 offset=63
            local.get 3
            local.get 3
            i32.store offset=48
            local.get 3
            local.get 3
            i32.const 63
            i32.add
            i32.store
            local.get 3
            i32.const 24
            i32.add
            local.get 1
            local.get 3
            i32.const 32
            i32.add
            local.get 2
            call_indirect (type 6)
            i32.const 0
            i32.const 0
            i32.store8 offset=1059380
            local.get 3
            i32.load8_u offset=24
            i32.const 3
            i32.ne
            br_if 1 (;@3;)
            local.get 3
            i32.load offset=28
            local.tee 0
            i32.load
            local.get 0
            i32.load offset=4
            i32.load
            call_indirect (type 1)
            block  ;; label = @5
              local.get 0
              i32.load offset=4
              local.tee 1
              i32.const 4
              i32.add
              i32.load
              local.tee 2
              i32.eqz
              br_if 0 (;@5;)
              local.get 0
              i32.load
              local.get 2
              local.get 1
              i32.const 8
              i32.add
              i32.load
              call $__rust_dealloc
            end
            local.get 0
            i32.const 12
            i32.const 4
            call $__rust_dealloc
            br 1 (;@3;)
          end
          i32.const 0
          i32.load8_u offset=1059316
          local.set 0
          i32.const 0
          i32.const 0
          i32.store8 offset=1059316
          local.get 0
          i32.eqz
          br_if 0 (;@3;)
          local.get 3
          i32.const 52
          i32.add
          i32.const 0
          i32.store
          local.get 3
          i32.const 1048772
          i32.store offset=48
          local.get 3
          i64.const 1
          i64.store offset=36 align=4
          local.get 3
          i32.const 1051756
          i32.store offset=32
          local.get 3
          local.get 1
          local.get 3
          i32.const 32
          i32.add
          local.get 2
          call_indirect (type 6)
          local.get 3
          i32.load8_u
          i32.const 3
          i32.ne
          br_if 0 (;@3;)
          local.get 3
          i32.load offset=4
          local.tee 0
          i32.load
          local.get 0
          i32.load offset=4
          i32.load
          call_indirect (type 1)
          block  ;; label = @4
            local.get 0
            i32.load offset=4
            local.tee 1
            i32.const 4
            i32.add
            i32.load
            local.tee 2
            i32.eqz
            br_if 0 (;@4;)
            local.get 0
            i32.load
            local.get 2
            local.get 1
            i32.const 8
            i32.add
            i32.load
            call $__rust_dealloc
          end
          local.get 0
          i32.const 12
          i32.const 4
          call $__rust_dealloc
        end
        local.get 3
        i32.const 64
        i32.add
        global.set $__stack_pointer
        return
      end
      local.get 3
      i32.const 0
      i32.store offset=52
      local.get 3
      i32.const 1048772
      i32.store offset=48
      local.get 3
      i64.const 1
      i64.store offset=36 align=4
      local.get 3
      i32.const 1052244
      i32.store offset=32
      local.get 3
      local.get 3
      i32.const 32
      i32.add
      call $_ZN4core9panicking13assert_failed17had14dc5ddcedac37E
      unreachable
    end
    local.get 3
    i32.const 0
    i32.store offset=52
    local.get 3
    i32.const 1048772
    i32.store offset=48
    local.get 3
    i64.const 1
    i64.store offset=36 align=4
    local.get 3
    i32.const 1052244
    i32.store offset=32
    local.get 3
    local.get 3
    i32.const 32
    i32.add
    call $_ZN4core9panicking13assert_failed17had14dc5ddcedac37E
    unreachable)
  (func $rust_begin_unwind (type 1) (param i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        call $_ZN4core5panic10panic_info9PanicInfo8location17he55e718ad5bfc141E
        local.tee 2
        i32.eqz
        br_if 0 (;@2;)
        local.get 0
        call $_ZN4core5panic10panic_info9PanicInfo7message17hbe3048ebb862958cE
        local.tee 3
        i32.eqz
        br_if 1 (;@1;)
        local.get 1
        local.get 2
        i32.store offset=8
        local.get 1
        local.get 0
        i32.store offset=4
        local.get 1
        local.get 3
        i32.store
        local.get 1
        call $_ZN3std10sys_common9backtrace26__rust_end_short_backtrace17h0cd229888db2b8aeE
        unreachable
      end
      i32.const 1048872
      i32.const 43
      i32.const 1051780
      call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
      unreachable
    end
    i32.const 1048872
    i32.const 43
    i32.const 1051764
    call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
    unreachable)
  (func $_ZN90_$LT$std..panicking..begin_panic_handler..PanicPayload$u20$as$u20$core..panic..BoxMeUp$GT$8take_box17h1b61cc9dffa5ac7fE (type 3) (param i32 i32)
    (local i32 i32 i32 i32 i64)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 1
    i32.const 4
    i32.add
    local.set 3
    block  ;; label = @1
      local.get 1
      i32.load offset=4
      br_if 0 (;@1;)
      local.get 1
      i32.load
      local.set 4
      local.get 2
      i32.const 8
      i32.add
      i32.const 8
      i32.add
      local.tee 5
      i32.const 0
      i32.store
      local.get 2
      i64.const 1
      i64.store offset=8
      local.get 2
      local.get 2
      i32.const 8
      i32.add
      i32.store offset=20
      local.get 2
      i32.const 24
      i32.add
      i32.const 16
      i32.add
      local.get 4
      i32.const 16
      i32.add
      i64.load align=4
      i64.store
      local.get 2
      i32.const 24
      i32.add
      i32.const 8
      i32.add
      local.get 4
      i32.const 8
      i32.add
      i64.load align=4
      i64.store
      local.get 2
      local.get 4
      i64.load align=4
      i64.store offset=24
      local.get 2
      i32.const 20
      i32.add
      i32.const 1048700
      local.get 2
      i32.const 24
      i32.add
      call $_ZN4core3fmt5write17hb15fae9d0358166aE
      drop
      local.get 3
      i32.const 8
      i32.add
      local.get 5
      i32.load
      i32.store
      local.get 3
      local.get 2
      i64.load offset=8
      i64.store align=4
    end
    local.get 2
    i32.const 24
    i32.add
    i32.const 8
    i32.add
    local.tee 4
    local.get 3
    i32.const 8
    i32.add
    i32.load
    i32.store
    local.get 1
    i32.const 12
    i32.add
    i32.const 0
    i32.store
    local.get 3
    i64.load align=4
    local.set 6
    local.get 1
    i64.const 1
    i64.store offset=4 align=4
    local.get 2
    local.get 6
    i64.store offset=24
    block  ;; label = @1
      i32.const 12
      i32.const 4
      call $__rust_alloc
      local.tee 1
      br_if 0 (;@1;)
      i32.const 12
      i32.const 4
      call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
      unreachable
    end
    local.get 1
    local.get 2
    i64.load offset=24
    i64.store align=4
    local.get 1
    i32.const 8
    i32.add
    local.get 4
    i32.load
    i32.store
    local.get 0
    i32.const 1051796
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store
    local.get 2
    i32.const 48
    i32.add
    global.set $__stack_pointer)
  (func $_ZN90_$LT$std..panicking..begin_panic_handler..PanicPayload$u20$as$u20$core..panic..BoxMeUp$GT$3get17hfdcf489f42573da3E (type 3) (param i32 i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 1
    i32.const 4
    i32.add
    local.set 3
    block  ;; label = @1
      local.get 1
      i32.load offset=4
      br_if 0 (;@1;)
      local.get 1
      i32.load
      local.set 1
      local.get 2
      i32.const 8
      i32.add
      i32.const 8
      i32.add
      local.tee 4
      i32.const 0
      i32.store
      local.get 2
      i64.const 1
      i64.store offset=8
      local.get 2
      local.get 2
      i32.const 8
      i32.add
      i32.store offset=20
      local.get 2
      i32.const 24
      i32.add
      i32.const 16
      i32.add
      local.get 1
      i32.const 16
      i32.add
      i64.load align=4
      i64.store
      local.get 2
      i32.const 24
      i32.add
      i32.const 8
      i32.add
      local.get 1
      i32.const 8
      i32.add
      i64.load align=4
      i64.store
      local.get 2
      local.get 1
      i64.load align=4
      i64.store offset=24
      local.get 2
      i32.const 20
      i32.add
      i32.const 1048700
      local.get 2
      i32.const 24
      i32.add
      call $_ZN4core3fmt5write17hb15fae9d0358166aE
      drop
      local.get 3
      i32.const 8
      i32.add
      local.get 4
      i32.load
      i32.store
      local.get 3
      local.get 2
      i64.load offset=8
      i64.store align=4
    end
    local.get 0
    i32.const 1051796
    i32.store offset=4
    local.get 0
    local.get 3
    i32.store
    local.get 2
    i32.const 48
    i32.add
    global.set $__stack_pointer)
  (func $_ZN93_$LT$std..panicking..begin_panic_handler..StrPanicPayload$u20$as$u20$core..panic..BoxMeUp$GT$8take_box17h899afaedeaeca761E (type 3) (param i32 i32)
    (local i32 i32)
    local.get 1
    i32.load offset=4
    local.set 2
    local.get 1
    i32.load
    local.set 3
    block  ;; label = @1
      i32.const 8
      i32.const 4
      call $__rust_alloc
      local.tee 1
      br_if 0 (;@1;)
      i32.const 8
      i32.const 4
      call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
      unreachable
    end
    local.get 1
    local.get 2
    i32.store offset=4
    local.get 1
    local.get 3
    i32.store
    local.get 0
    i32.const 1051812
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store)
  (func $_ZN93_$LT$std..panicking..begin_panic_handler..StrPanicPayload$u20$as$u20$core..panic..BoxMeUp$GT$3get17h44d9e73cb503261eE (type 3) (param i32 i32)
    local.get 0
    i32.const 1051812
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store)
  (func $_ZN3std9panicking20rust_panic_with_hook17h0a5a34832cbe3154E (type 11) (param i32 i32 i32 i32 i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 112
    i32.sub
    local.tee 5
    global.set $__stack_pointer
    i32.const 0
    i32.const 0
    i32.load offset=1059404
    local.tee 6
    i32.const 1
    i32.add
    i32.store offset=1059404
    i32.const 0
    i32.const 0
    i32.load offset=1059436
    i32.const 1
    i32.add
    local.tee 7
    i32.store offset=1059436
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 6
              i32.const 0
              i32.lt_s
              br_if 0 (;@5;)
              local.get 7
              i32.const 2
              i32.gt_u
              br_if 0 (;@5;)
              local.get 5
              local.get 4
              i32.store8 offset=32
              local.get 5
              local.get 3
              i32.store offset=28
              local.get 5
              local.get 2
              i32.store offset=24
              local.get 5
              i32.const 1051928
              i32.store offset=20
              local.get 5
              i32.const 1048772
              i32.store offset=16
              i32.const 0
              i32.load offset=1059388
              local.tee 6
              i32.const -1
              i32.le_s
              br_if 3 (;@2;)
              i32.const 0
              local.get 6
              i32.const 1
              i32.add
              i32.store offset=1059388
              i32.const 0
              i32.load offset=1059396
              local.set 6
              local.get 5
              i32.const 8
              i32.add
              local.get 0
              local.get 1
              i32.load offset=16
              call_indirect (type 3)
              local.get 5
              local.get 5
              i64.load offset=8
              i64.store offset=16
              local.get 6
              br_if 1 (;@4;)
              local.get 5
              i32.const 16
              i32.add
              call $_ZN3std9panicking12default_hook17h15ece0a870cadf43E
              br 2 (;@3;)
            end
            block  ;; label = @5
              block  ;; label = @6
                local.get 7
                i32.const 2
                i32.gt_u
                br_if 0 (;@6;)
                local.get 5
                local.get 4
                i32.store8 offset=64
                local.get 5
                local.get 3
                i32.store offset=60
                local.get 5
                local.get 2
                i32.store offset=56
                local.get 5
                i32.const 1051928
                i32.store offset=52
                local.get 5
                i32.const 1048772
                i32.store offset=48
                local.get 5
                i32.const 17
                i32.store offset=76
                local.get 5
                local.get 5
                i32.const 48
                i32.add
                i32.store offset=72
                local.get 5
                i32.const 4
                i32.store8 offset=20
                local.get 5
                local.get 5
                i32.const 104
                i32.add
                i32.store offset=16
                local.get 5
                i32.const 1
                i32.store offset=100
                local.get 5
                i64.const 2
                i64.store offset=84 align=4
                local.get 5
                i32.const 1051996
                i32.store offset=80
                local.get 5
                local.get 5
                i32.const 72
                i32.add
                i32.store offset=96
                local.get 5
                i32.const 16
                i32.add
                i32.const 1050676
                local.get 5
                i32.const 80
                i32.add
                call $_ZN4core3fmt5write17hb15fae9d0358166aE
                local.set 7
                local.get 5
                i32.load8_u offset=20
                local.set 6
                block  ;; label = @7
                  local.get 7
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 6
                  i32.const 255
                  i32.and
                  i32.const 4
                  i32.eq
                  br_if 2 (;@5;)
                  local.get 5
                  i32.load8_u offset=20
                  i32.const 3
                  i32.ne
                  br_if 2 (;@5;)
                  local.get 5
                  i32.const 24
                  i32.add
                  i32.load
                  local.tee 5
                  i32.load
                  local.get 5
                  i32.load offset=4
                  i32.load
                  call_indirect (type 1)
                  block  ;; label = @8
                    local.get 5
                    i32.load offset=4
                    local.tee 7
                    i32.const 4
                    i32.add
                    i32.load
                    local.tee 6
                    i32.eqz
                    br_if 0 (;@8;)
                    local.get 5
                    i32.load
                    local.get 6
                    local.get 7
                    i32.const 8
                    i32.add
                    i32.load
                    call $__rust_dealloc
                  end
                  local.get 5
                  i32.const 12
                  i32.const 4
                  call $__rust_dealloc
                  call $_ZN3std3sys4wasi14abort_internal17h410a405cf1d1c758E
                  unreachable
                end
                local.get 6
                i32.const 255
                i32.and
                i32.const 3
                i32.ne
                br_if 1 (;@5;)
                local.get 5
                i32.const 16
                i32.add
                i32.const 8
                i32.add
                i32.load
                local.tee 7
                i32.load
                local.get 7
                i32.load offset=4
                i32.load
                call_indirect (type 1)
                block  ;; label = @7
                  local.get 7
                  i32.load offset=4
                  local.tee 6
                  i32.const 4
                  i32.add
                  i32.load
                  local.tee 4
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 7
                  i32.load
                  local.get 4
                  local.get 6
                  i32.const 8
                  i32.add
                  i32.load
                  call $__rust_dealloc
                end
                local.get 5
                i32.load offset=24
                i32.const 12
                i32.const 4
                call $__rust_dealloc
                call $_ZN3std3sys4wasi14abort_internal17h410a405cf1d1c758E
                unreachable
              end
              local.get 5
              i32.const 4
              i32.store8 offset=52
              local.get 5
              local.get 5
              i32.const 104
              i32.add
              i32.store offset=48
              local.get 5
              i32.const 0
              i32.store offset=100
              local.get 5
              i32.const 1048772
              i32.store offset=96
              local.get 5
              i64.const 1
              i64.store offset=84 align=4
              local.get 5
              i32.const 1051920
              i32.store offset=80
              local.get 5
              i32.const 48
              i32.add
              i32.const 1050676
              local.get 5
              i32.const 80
              i32.add
              call $_ZN4core3fmt5write17hb15fae9d0358166aE
              local.set 7
              local.get 5
              i32.load8_u offset=52
              local.set 6
              block  ;; label = @6
                local.get 7
                i32.eqz
                br_if 0 (;@6;)
                local.get 6
                i32.const 255
                i32.and
                i32.const 4
                i32.eq
                br_if 1 (;@5;)
                local.get 5
                i32.load8_u offset=52
                i32.const 3
                i32.ne
                br_if 1 (;@5;)
                local.get 5
                i32.const 56
                i32.add
                i32.load
                local.tee 5
                i32.load
                local.get 5
                i32.load offset=4
                i32.load
                call_indirect (type 1)
                block  ;; label = @7
                  local.get 5
                  i32.load offset=4
                  local.tee 7
                  i32.const 4
                  i32.add
                  i32.load
                  local.tee 6
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 5
                  i32.load
                  local.get 6
                  local.get 7
                  i32.const 8
                  i32.add
                  i32.load
                  call $__rust_dealloc
                end
                local.get 5
                i32.const 12
                i32.const 4
                call $__rust_dealloc
                call $_ZN3std3sys4wasi14abort_internal17h410a405cf1d1c758E
                unreachable
              end
              local.get 6
              i32.const 255
              i32.and
              i32.const 3
              i32.ne
              br_if 0 (;@5;)
              local.get 5
              i32.const 48
              i32.add
              i32.const 8
              i32.add
              i32.load
              local.tee 7
              i32.load
              local.get 7
              i32.load offset=4
              i32.load
              call_indirect (type 1)
              block  ;; label = @6
                local.get 7
                i32.load offset=4
                local.tee 6
                i32.const 4
                i32.add
                i32.load
                local.tee 4
                i32.eqz
                br_if 0 (;@6;)
                local.get 7
                i32.load
                local.get 4
                local.get 6
                i32.const 8
                i32.add
                i32.load
                call $__rust_dealloc
              end
              local.get 5
              i32.load offset=56
              i32.const 12
              i32.const 4
              call $__rust_dealloc
            end
            call $_ZN3std3sys4wasi14abort_internal17h410a405cf1d1c758E
            unreachable
          end
          i32.const 0
          i32.load offset=1059396
          local.get 5
          i32.const 16
          i32.add
          i32.const 0
          i32.load offset=1059400
          i32.load offset=20
          call_indirect (type 3)
        end
        i32.const 0
        i32.const 0
        i32.load offset=1059388
        i32.const -1
        i32.add
        i32.store offset=1059388
        block  ;; label = @3
          local.get 7
          i32.const 1
          i32.gt_u
          br_if 0 (;@3;)
          local.get 4
          br_if 2 (;@1;)
        end
        local.get 5
        i32.const 100
        i32.add
        i32.const 0
        i32.store
        local.get 5
        i32.const 1048772
        i32.store offset=96
        local.get 5
        i64.const 1
        i64.store offset=84 align=4
        local.get 5
        i32.const 1052056
        i32.store offset=80
        local.get 5
        i32.const 48
        i32.add
        local.get 5
        i32.const 104
        i32.add
        local.get 5
        i32.const 80
        i32.add
        call $_ZN3std2io5Write9write_fmt17h05a3bcb077746284E
        local.get 5
        i32.const 48
        i32.add
        call $_ZN4core3ptr81drop_in_place$LT$core..result..Result$LT$$LP$$RP$$C$std..io..error..Error$GT$$GT$17h7ea204518b820d95E
        call $_ZN3std3sys4wasi14abort_internal17h410a405cf1d1c758E
        unreachable
      end
      local.get 5
      i32.const 48
      i32.add
      i32.const 20
      i32.add
      i32.const 1
      i32.store
      local.get 5
      i32.const 80
      i32.add
      i32.const 20
      i32.add
      i32.const 0
      i32.store
      local.get 5
      i64.const 2
      i64.store offset=52 align=4
      local.get 5
      i32.const 1049116
      i32.store offset=48
      local.get 5
      i32.const 8
      i32.store offset=76
      local.get 5
      i32.const 1048772
      i32.store offset=96
      local.get 5
      i64.const 1
      i64.store offset=84 align=4
      local.get 5
      i32.const 1052352
      i32.store offset=80
      local.get 5
      local.get 5
      i32.const 72
      i32.add
      i32.store offset=64
      local.get 5
      local.get 5
      i32.const 80
      i32.add
      i32.store offset=72
      local.get 5
      i32.const 40
      i32.add
      local.get 5
      i32.const 104
      i32.add
      local.get 5
      i32.const 48
      i32.add
      call $_ZN3std2io5Write9write_fmt17h05a3bcb077746284E
      local.get 5
      i32.const 40
      i32.add
      call $_ZN4core3ptr81drop_in_place$LT$core..result..Result$LT$$LP$$RP$$C$std..io..error..Error$GT$$GT$17h7ea204518b820d95E
      call $_ZN3std3sys4wasi14abort_internal17h410a405cf1d1c758E
      unreachable
    end
    local.get 0
    local.get 1
    call $rust_panic
    unreachable)
  (func $rust_panic (type 3) (param i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 96
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 1
    i32.store offset=4
    local.get 2
    local.get 0
    i32.store
    local.get 2
    local.get 2
    call $__rust_start_panic
    i32.store offset=12
    local.get 2
    i32.const 24
    i32.add
    i32.const 20
    i32.add
    i32.const 1
    i32.store
    local.get 2
    i32.const 56
    i32.add
    i32.const 20
    i32.add
    i32.const 1
    i32.store
    local.get 2
    i64.const 2
    i64.store offset=28 align=4
    local.get 2
    i32.const 1049116
    i32.store offset=24
    local.get 2
    i32.const 8
    i32.store offset=52
    local.get 2
    i64.const 1
    i64.store offset=60 align=4
    local.get 2
    i32.const 1052096
    i32.store offset=56
    local.get 2
    i32.const 4
    i32.store offset=84
    local.get 2
    local.get 2
    i32.const 48
    i32.add
    i32.store offset=40
    local.get 2
    local.get 2
    i32.const 56
    i32.add
    i32.store offset=48
    local.get 2
    local.get 2
    i32.const 80
    i32.add
    i32.store offset=72
    local.get 2
    local.get 2
    i32.const 12
    i32.add
    i32.store offset=80
    local.get 2
    i32.const 16
    i32.add
    local.get 2
    i32.const 88
    i32.add
    local.get 2
    i32.const 24
    i32.add
    call $_ZN3std2io5Write9write_fmt17h05a3bcb077746284E
    local.get 2
    i32.const 16
    i32.add
    call $_ZN4core3ptr81drop_in_place$LT$core..result..Result$LT$$LP$$RP$$C$std..io..error..Error$GT$$GT$17h7ea204518b820d95E
    call $_ZN3std3sys4wasi14abort_internal17h410a405cf1d1c758E
    unreachable)
  (func $_ZN3std3sys4wasi7process8ExitCode6as_i3217hd5ea6c8911056b09E (type 4) (param i32) (result i32)
    local.get 0
    i32.load8_u)
  (func $__rust_start_panic (type 4) (param i32) (result i32)
    unreachable
    unreachable)
  (func $_ZN4wasi13lib_generated5Errno3raw17h027903615a3d664fE (type 4) (param i32) (result i32)
    local.get 0
    i32.load16_u)
  (func $_ZN4wasi13lib_generated8args_get17hb5bb6f40771f3aa1E (type 6) (param i32 i32 i32)
    local.get 0
    local.get 1
    local.get 2
    call $_ZN4wasi13lib_generated22wasi_snapshot_preview18args_get17hedb4461d1984109aE
    local.tee 2
    i32.store16 offset=2
    local.get 0
    local.get 2
    i32.const 0
    i32.ne
    i32.store16)
  (func $_ZN4wasi13lib_generated14args_sizes_get17hf5a1e66d6f51bb94E (type 1) (param i32)
    (local i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 8
        i32.add
        local.get 1
        i32.const 12
        i32.add
        call $_ZN4wasi13lib_generated22wasi_snapshot_preview114args_sizes_get17h26cfdfead02b25dbE
        local.tee 2
        br_if 0 (;@2;)
        local.get 0
        local.get 1
        i32.load offset=8
        i32.store offset=4
        local.get 0
        i32.const 8
        i32.add
        local.get 1
        i32.load offset=12
        i32.store
        i32.const 0
        local.set 2
        br 1 (;@1;)
      end
      local.get 0
      local.get 2
      i32.store16 offset=2
      i32.const 1
      local.set 2
    end
    local.get 0
    local.get 2
    i32.store16
    local.get 1
    i32.const 16
    i32.add
    global.set $__stack_pointer)
  (func $_ZN4wasi13lib_generated8fd_write17hb23bdba73a8f4709E (type 9) (param i32 i32 i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 4
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        local.get 2
        local.get 3
        local.get 4
        i32.const 12
        i32.add
        call $_ZN4wasi13lib_generated22wasi_snapshot_preview18fd_write17h1706d154ab4fc644E
        local.tee 3
        br_if 0 (;@2;)
        local.get 0
        local.get 4
        i32.load offset=12
        i32.store offset=4
        i32.const 0
        local.set 3
        br 1 (;@1;)
      end
      local.get 0
      local.get 3
      i32.store16 offset=2
      i32.const 1
      local.set 3
    end
    local.get 0
    local.get 3
    i32.store16
    local.get 4
    i32.const 16
    i32.add
    global.set $__stack_pointer)
  (func $malloc (type 4) (param i32) (result i32)
    local.get 0
    call $dlmalloc)
  (func $dlmalloc (type 4) (param i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    block  ;; label = @1
      i32.const 0
      i32.load offset=1059464
      br_if 0 (;@1;)
      i32.const 0
      call $sbrk
      i32.const 1059984
      i32.sub
      local.tee 2
      i32.const 89
      i32.lt_u
      br_if 0 (;@1;)
      i32.const 0
      local.set 3
      block  ;; label = @2
        i32.const 0
        i32.load offset=1059912
        local.tee 4
        br_if 0 (;@2;)
        i32.const 0
        i64.const -1
        i64.store offset=1059924 align=4
        i32.const 0
        i64.const 281474976776192
        i64.store offset=1059916 align=4
        i32.const 0
        local.get 1
        i32.const 8
        i32.add
        i32.const -16
        i32.and
        i32.const 1431655768
        i32.xor
        local.tee 4
        i32.store offset=1059912
        i32.const 0
        i32.const 0
        i32.store offset=1059932
        i32.const 0
        i32.const 0
        i32.store offset=1059884
      end
      i32.const 0
      local.get 2
      i32.store offset=1059892
      i32.const 0
      i32.const 1059984
      i32.store offset=1059888
      i32.const 0
      i32.const 1059984
      i32.store offset=1059456
      i32.const 0
      local.get 4
      i32.store offset=1059476
      i32.const 0
      i32.const -1
      i32.store offset=1059472
      loop  ;; label = @2
        local.get 3
        i32.const 1059500
        i32.add
        local.get 3
        i32.const 1059488
        i32.add
        local.tee 4
        i32.store
        local.get 4
        local.get 3
        i32.const 1059480
        i32.add
        local.tee 5
        i32.store
        local.get 3
        i32.const 1059492
        i32.add
        local.get 5
        i32.store
        local.get 3
        i32.const 1059508
        i32.add
        local.get 3
        i32.const 1059496
        i32.add
        local.tee 5
        i32.store
        local.get 5
        local.get 4
        i32.store
        local.get 3
        i32.const 1059516
        i32.add
        local.get 3
        i32.const 1059504
        i32.add
        local.tee 4
        i32.store
        local.get 4
        local.get 5
        i32.store
        local.get 3
        i32.const 1059512
        i32.add
        local.get 4
        i32.store
        local.get 3
        i32.const 32
        i32.add
        local.tee 3
        i32.const 256
        i32.ne
        br_if 0 (;@2;)
      end
      i32.const 1059984
      i32.const -8
      i32.const 1059984
      i32.sub
      i32.const 15
      i32.and
      i32.const 0
      i32.const 1059984
      i32.const 8
      i32.add
      i32.const 15
      i32.and
      select
      local.tee 3
      i32.add
      local.tee 4
      i32.const 4
      i32.add
      local.get 2
      i32.const -56
      i32.add
      local.tee 5
      local.get 3
      i32.sub
      local.tee 3
      i32.const 1
      i32.or
      i32.store
      i32.const 0
      i32.const 0
      i32.load offset=1059928
      i32.store offset=1059468
      i32.const 0
      local.get 3
      i32.store offset=1059452
      i32.const 0
      local.get 4
      i32.store offset=1059464
      i32.const 1059984
      local.get 5
      i32.add
      i32.const 56
      i32.store offset=4
    end
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            local.get 0
                            i32.const 236
                            i32.gt_u
                            br_if 0 (;@12;)
                            block  ;; label = @13
                              i32.const 0
                              i32.load offset=1059440
                              local.tee 6
                              i32.const 16
                              local.get 0
                              i32.const 19
                              i32.add
                              i32.const -16
                              i32.and
                              local.get 0
                              i32.const 11
                              i32.lt_u
                              select
                              local.tee 2
                              i32.const 3
                              i32.shr_u
                              local.tee 4
                              i32.shr_u
                              local.tee 3
                              i32.const 3
                              i32.and
                              i32.eqz
                              br_if 0 (;@13;)
                              local.get 3
                              i32.const 1
                              i32.and
                              local.get 4
                              i32.or
                              i32.const 1
                              i32.xor
                              local.tee 5
                              i32.const 3
                              i32.shl
                              local.tee 0
                              i32.const 1059488
                              i32.add
                              i32.load
                              local.tee 4
                              i32.const 8
                              i32.add
                              local.set 3
                              block  ;; label = @14
                                block  ;; label = @15
                                  local.get 4
                                  i32.load offset=8
                                  local.tee 2
                                  local.get 0
                                  i32.const 1059480
                                  i32.add
                                  local.tee 0
                                  i32.ne
                                  br_if 0 (;@15;)
                                  i32.const 0
                                  local.get 6
                                  i32.const -2
                                  local.get 5
                                  i32.rotl
                                  i32.and
                                  i32.store offset=1059440
                                  br 1 (;@14;)
                                end
                                local.get 0
                                local.get 2
                                i32.store offset=8
                                local.get 2
                                local.get 0
                                i32.store offset=12
                              end
                              local.get 4
                              local.get 5
                              i32.const 3
                              i32.shl
                              local.tee 5
                              i32.const 3
                              i32.or
                              i32.store offset=4
                              local.get 4
                              local.get 5
                              i32.add
                              local.tee 4
                              local.get 4
                              i32.load offset=4
                              i32.const 1
                              i32.or
                              i32.store offset=4
                              br 12 (;@1;)
                            end
                            local.get 2
                            i32.const 0
                            i32.load offset=1059448
                            local.tee 7
                            i32.le_u
                            br_if 1 (;@11;)
                            block  ;; label = @13
                              local.get 3
                              i32.eqz
                              br_if 0 (;@13;)
                              block  ;; label = @14
                                block  ;; label = @15
                                  local.get 3
                                  local.get 4
                                  i32.shl
                                  i32.const 2
                                  local.get 4
                                  i32.shl
                                  local.tee 3
                                  i32.const 0
                                  local.get 3
                                  i32.sub
                                  i32.or
                                  i32.and
                                  local.tee 3
                                  i32.const 0
                                  local.get 3
                                  i32.sub
                                  i32.and
                                  i32.const -1
                                  i32.add
                                  local.tee 3
                                  local.get 3
                                  i32.const 12
                                  i32.shr_u
                                  i32.const 16
                                  i32.and
                                  local.tee 3
                                  i32.shr_u
                                  local.tee 4
                                  i32.const 5
                                  i32.shr_u
                                  i32.const 8
                                  i32.and
                                  local.tee 5
                                  local.get 3
                                  i32.or
                                  local.get 4
                                  local.get 5
                                  i32.shr_u
                                  local.tee 3
                                  i32.const 2
                                  i32.shr_u
                                  i32.const 4
                                  i32.and
                                  local.tee 4
                                  i32.or
                                  local.get 3
                                  local.get 4
                                  i32.shr_u
                                  local.tee 3
                                  i32.const 1
                                  i32.shr_u
                                  i32.const 2
                                  i32.and
                                  local.tee 4
                                  i32.or
                                  local.get 3
                                  local.get 4
                                  i32.shr_u
                                  local.tee 3
                                  i32.const 1
                                  i32.shr_u
                                  i32.const 1
                                  i32.and
                                  local.tee 4
                                  i32.or
                                  local.get 3
                                  local.get 4
                                  i32.shr_u
                                  i32.add
                                  local.tee 5
                                  i32.const 3
                                  i32.shl
                                  local.tee 0
                                  i32.const 1059488
                                  i32.add
                                  i32.load
                                  local.tee 4
                                  i32.load offset=8
                                  local.tee 3
                                  local.get 0
                                  i32.const 1059480
                                  i32.add
                                  local.tee 0
                                  i32.ne
                                  br_if 0 (;@15;)
                                  i32.const 0
                                  local.get 6
                                  i32.const -2
                                  local.get 5
                                  i32.rotl
                                  i32.and
                                  local.tee 6
                                  i32.store offset=1059440
                                  br 1 (;@14;)
                                end
                                local.get 0
                                local.get 3
                                i32.store offset=8
                                local.get 3
                                local.get 0
                                i32.store offset=12
                              end
                              local.get 4
                              i32.const 8
                              i32.add
                              local.set 3
                              local.get 4
                              local.get 2
                              i32.const 3
                              i32.or
                              i32.store offset=4
                              local.get 4
                              local.get 5
                              i32.const 3
                              i32.shl
                              local.tee 5
                              i32.add
                              local.get 5
                              local.get 2
                              i32.sub
                              local.tee 5
                              i32.store
                              local.get 4
                              local.get 2
                              i32.add
                              local.tee 0
                              local.get 5
                              i32.const 1
                              i32.or
                              i32.store offset=4
                              block  ;; label = @14
                                local.get 7
                                i32.eqz
                                br_if 0 (;@14;)
                                local.get 7
                                i32.const 3
                                i32.shr_u
                                local.tee 8
                                i32.const 3
                                i32.shl
                                i32.const 1059480
                                i32.add
                                local.set 2
                                i32.const 0
                                i32.load offset=1059460
                                local.set 4
                                block  ;; label = @15
                                  block  ;; label = @16
                                    local.get 6
                                    i32.const 1
                                    local.get 8
                                    i32.shl
                                    local.tee 8
                                    i32.and
                                    br_if 0 (;@16;)
                                    i32.const 0
                                    local.get 6
                                    local.get 8
                                    i32.or
                                    i32.store offset=1059440
                                    local.get 2
                                    local.set 8
                                    br 1 (;@15;)
                                  end
                                  local.get 2
                                  i32.load offset=8
                                  local.set 8
                                end
                                local.get 8
                                local.get 4
                                i32.store offset=12
                                local.get 2
                                local.get 4
                                i32.store offset=8
                                local.get 4
                                local.get 2
                                i32.store offset=12
                                local.get 4
                                local.get 8
                                i32.store offset=8
                              end
                              i32.const 0
                              local.get 0
                              i32.store offset=1059460
                              i32.const 0
                              local.get 5
                              i32.store offset=1059448
                              br 12 (;@1;)
                            end
                            i32.const 0
                            i32.load offset=1059444
                            local.tee 9
                            i32.eqz
                            br_if 1 (;@11;)
                            local.get 9
                            i32.const 0
                            local.get 9
                            i32.sub
                            i32.and
                            i32.const -1
                            i32.add
                            local.tee 3
                            local.get 3
                            i32.const 12
                            i32.shr_u
                            i32.const 16
                            i32.and
                            local.tee 3
                            i32.shr_u
                            local.tee 4
                            i32.const 5
                            i32.shr_u
                            i32.const 8
                            i32.and
                            local.tee 5
                            local.get 3
                            i32.or
                            local.get 4
                            local.get 5
                            i32.shr_u
                            local.tee 3
                            i32.const 2
                            i32.shr_u
                            i32.const 4
                            i32.and
                            local.tee 4
                            i32.or
                            local.get 3
                            local.get 4
                            i32.shr_u
                            local.tee 3
                            i32.const 1
                            i32.shr_u
                            i32.const 2
                            i32.and
                            local.tee 4
                            i32.or
                            local.get 3
                            local.get 4
                            i32.shr_u
                            local.tee 3
                            i32.const 1
                            i32.shr_u
                            i32.const 1
                            i32.and
                            local.tee 4
                            i32.or
                            local.get 3
                            local.get 4
                            i32.shr_u
                            i32.add
                            i32.const 2
                            i32.shl
                            i32.const 1059744
                            i32.add
                            i32.load
                            local.tee 0
                            i32.load offset=4
                            i32.const -8
                            i32.and
                            local.get 2
                            i32.sub
                            local.set 4
                            local.get 0
                            local.set 5
                            block  ;; label = @13
                              loop  ;; label = @14
                                block  ;; label = @15
                                  local.get 5
                                  i32.load offset=16
                                  local.tee 3
                                  br_if 0 (;@15;)
                                  local.get 5
                                  i32.const 20
                                  i32.add
                                  i32.load
                                  local.tee 3
                                  i32.eqz
                                  br_if 2 (;@13;)
                                end
                                local.get 3
                                i32.load offset=4
                                i32.const -8
                                i32.and
                                local.get 2
                                i32.sub
                                local.tee 5
                                local.get 4
                                local.get 5
                                local.get 4
                                i32.lt_u
                                local.tee 5
                                select
                                local.set 4
                                local.get 3
                                local.get 0
                                local.get 5
                                select
                                local.set 0
                                local.get 3
                                local.set 5
                                br 0 (;@14;)
                              end
                            end
                            local.get 0
                            i32.load offset=24
                            local.set 10
                            block  ;; label = @13
                              local.get 0
                              i32.load offset=12
                              local.tee 8
                              local.get 0
                              i32.eq
                              br_if 0 (;@13;)
                              i32.const 0
                              i32.load offset=1059456
                              local.get 0
                              i32.load offset=8
                              local.tee 3
                              i32.gt_u
                              drop
                              local.get 8
                              local.get 3
                              i32.store offset=8
                              local.get 3
                              local.get 8
                              i32.store offset=12
                              br 11 (;@2;)
                            end
                            block  ;; label = @13
                              local.get 0
                              i32.const 20
                              i32.add
                              local.tee 5
                              i32.load
                              local.tee 3
                              br_if 0 (;@13;)
                              local.get 0
                              i32.load offset=16
                              local.tee 3
                              i32.eqz
                              br_if 3 (;@10;)
                              local.get 0
                              i32.const 16
                              i32.add
                              local.set 5
                            end
                            loop  ;; label = @13
                              local.get 5
                              local.set 11
                              local.get 3
                              local.tee 8
                              i32.const 20
                              i32.add
                              local.tee 5
                              i32.load
                              local.tee 3
                              br_if 0 (;@13;)
                              local.get 8
                              i32.const 16
                              i32.add
                              local.set 5
                              local.get 8
                              i32.load offset=16
                              local.tee 3
                              br_if 0 (;@13;)
                            end
                            local.get 11
                            i32.const 0
                            i32.store
                            br 10 (;@2;)
                          end
                          i32.const -1
                          local.set 2
                          local.get 0
                          i32.const -65
                          i32.gt_u
                          br_if 0 (;@11;)
                          local.get 0
                          i32.const 19
                          i32.add
                          local.tee 3
                          i32.const -16
                          i32.and
                          local.set 2
                          i32.const 0
                          i32.load offset=1059444
                          local.tee 7
                          i32.eqz
                          br_if 0 (;@11;)
                          i32.const 0
                          local.set 11
                          block  ;; label = @12
                            local.get 2
                            i32.const 256
                            i32.lt_u
                            br_if 0 (;@12;)
                            i32.const 31
                            local.set 11
                            local.get 2
                            i32.const 16777215
                            i32.gt_u
                            br_if 0 (;@12;)
                            local.get 3
                            i32.const 8
                            i32.shr_u
                            local.tee 3
                            local.get 3
                            i32.const 1048320
                            i32.add
                            i32.const 16
                            i32.shr_u
                            i32.const 8
                            i32.and
                            local.tee 3
                            i32.shl
                            local.tee 4
                            local.get 4
                            i32.const 520192
                            i32.add
                            i32.const 16
                            i32.shr_u
                            i32.const 4
                            i32.and
                            local.tee 4
                            i32.shl
                            local.tee 5
                            local.get 5
                            i32.const 245760
                            i32.add
                            i32.const 16
                            i32.shr_u
                            i32.const 2
                            i32.and
                            local.tee 5
                            i32.shl
                            i32.const 15
                            i32.shr_u
                            local.get 3
                            local.get 4
                            i32.or
                            local.get 5
                            i32.or
                            i32.sub
                            local.tee 3
                            i32.const 1
                            i32.shl
                            local.get 2
                            local.get 3
                            i32.const 21
                            i32.add
                            i32.shr_u
                            i32.const 1
                            i32.and
                            i32.or
                            i32.const 28
                            i32.add
                            local.set 11
                          end
                          i32.const 0
                          local.get 2
                          i32.sub
                          local.set 4
                          block  ;; label = @12
                            block  ;; label = @13
                              block  ;; label = @14
                                block  ;; label = @15
                                  local.get 11
                                  i32.const 2
                                  i32.shl
                                  i32.const 1059744
                                  i32.add
                                  i32.load
                                  local.tee 5
                                  br_if 0 (;@15;)
                                  i32.const 0
                                  local.set 3
                                  i32.const 0
                                  local.set 8
                                  br 1 (;@14;)
                                end
                                i32.const 0
                                local.set 3
                                local.get 2
                                i32.const 0
                                i32.const 25
                                local.get 11
                                i32.const 1
                                i32.shr_u
                                i32.sub
                                local.get 11
                                i32.const 31
                                i32.eq
                                select
                                i32.shl
                                local.set 0
                                i32.const 0
                                local.set 8
                                loop  ;; label = @15
                                  block  ;; label = @16
                                    local.get 5
                                    i32.load offset=4
                                    i32.const -8
                                    i32.and
                                    local.get 2
                                    i32.sub
                                    local.tee 6
                                    local.get 4
                                    i32.ge_u
                                    br_if 0 (;@16;)
                                    local.get 6
                                    local.set 4
                                    local.get 5
                                    local.set 8
                                    local.get 6
                                    br_if 0 (;@16;)
                                    i32.const 0
                                    local.set 4
                                    local.get 5
                                    local.set 8
                                    local.get 5
                                    local.set 3
                                    br 3 (;@13;)
                                  end
                                  local.get 3
                                  local.get 5
                                  i32.const 20
                                  i32.add
                                  i32.load
                                  local.tee 6
                                  local.get 6
                                  local.get 5
                                  local.get 0
                                  i32.const 29
                                  i32.shr_u
                                  i32.const 4
                                  i32.and
                                  i32.add
                                  i32.const 16
                                  i32.add
                                  i32.load
                                  local.tee 5
                                  i32.eq
                                  select
                                  local.get 3
                                  local.get 6
                                  select
                                  local.set 3
                                  local.get 0
                                  i32.const 1
                                  i32.shl
                                  local.set 0
                                  local.get 5
                                  br_if 0 (;@15;)
                                end
                              end
                              block  ;; label = @14
                                local.get 3
                                local.get 8
                                i32.or
                                br_if 0 (;@14;)
                                i32.const 0
                                local.set 8
                                i32.const 2
                                local.get 11
                                i32.shl
                                local.tee 3
                                i32.const 0
                                local.get 3
                                i32.sub
                                i32.or
                                local.get 7
                                i32.and
                                local.tee 3
                                i32.eqz
                                br_if 3 (;@11;)
                                local.get 3
                                i32.const 0
                                local.get 3
                                i32.sub
                                i32.and
                                i32.const -1
                                i32.add
                                local.tee 3
                                local.get 3
                                i32.const 12
                                i32.shr_u
                                i32.const 16
                                i32.and
                                local.tee 3
                                i32.shr_u
                                local.tee 5
                                i32.const 5
                                i32.shr_u
                                i32.const 8
                                i32.and
                                local.tee 0
                                local.get 3
                                i32.or
                                local.get 5
                                local.get 0
                                i32.shr_u
                                local.tee 3
                                i32.const 2
                                i32.shr_u
                                i32.const 4
                                i32.and
                                local.tee 5
                                i32.or
                                local.get 3
                                local.get 5
                                i32.shr_u
                                local.tee 3
                                i32.const 1
                                i32.shr_u
                                i32.const 2
                                i32.and
                                local.tee 5
                                i32.or
                                local.get 3
                                local.get 5
                                i32.shr_u
                                local.tee 3
                                i32.const 1
                                i32.shr_u
                                i32.const 1
                                i32.and
                                local.tee 5
                                i32.or
                                local.get 3
                                local.get 5
                                i32.shr_u
                                i32.add
                                i32.const 2
                                i32.shl
                                i32.const 1059744
                                i32.add
                                i32.load
                                local.set 3
                              end
                              local.get 3
                              i32.eqz
                              br_if 1 (;@12;)
                            end
                            loop  ;; label = @13
                              local.get 3
                              i32.load offset=4
                              i32.const -8
                              i32.and
                              local.get 2
                              i32.sub
                              local.tee 6
                              local.get 4
                              i32.lt_u
                              local.set 0
                              block  ;; label = @14
                                local.get 3
                                i32.load offset=16
                                local.tee 5
                                br_if 0 (;@14;)
                                local.get 3
                                i32.const 20
                                i32.add
                                i32.load
                                local.set 5
                              end
                              local.get 6
                              local.get 4
                              local.get 0
                              select
                              local.set 4
                              local.get 3
                              local.get 8
                              local.get 0
                              select
                              local.set 8
                              local.get 5
                              local.set 3
                              local.get 5
                              br_if 0 (;@13;)
                            end
                          end
                          local.get 8
                          i32.eqz
                          br_if 0 (;@11;)
                          local.get 4
                          i32.const 0
                          i32.load offset=1059448
                          local.get 2
                          i32.sub
                          i32.ge_u
                          br_if 0 (;@11;)
                          local.get 8
                          i32.load offset=24
                          local.set 11
                          block  ;; label = @12
                            local.get 8
                            i32.load offset=12
                            local.tee 0
                            local.get 8
                            i32.eq
                            br_if 0 (;@12;)
                            i32.const 0
                            i32.load offset=1059456
                            local.get 8
                            i32.load offset=8
                            local.tee 3
                            i32.gt_u
                            drop
                            local.get 0
                            local.get 3
                            i32.store offset=8
                            local.get 3
                            local.get 0
                            i32.store offset=12
                            br 9 (;@3;)
                          end
                          block  ;; label = @12
                            local.get 8
                            i32.const 20
                            i32.add
                            local.tee 5
                            i32.load
                            local.tee 3
                            br_if 0 (;@12;)
                            local.get 8
                            i32.load offset=16
                            local.tee 3
                            i32.eqz
                            br_if 3 (;@9;)
                            local.get 8
                            i32.const 16
                            i32.add
                            local.set 5
                          end
                          loop  ;; label = @12
                            local.get 5
                            local.set 6
                            local.get 3
                            local.tee 0
                            i32.const 20
                            i32.add
                            local.tee 5
                            i32.load
                            local.tee 3
                            br_if 0 (;@12;)
                            local.get 0
                            i32.const 16
                            i32.add
                            local.set 5
                            local.get 0
                            i32.load offset=16
                            local.tee 3
                            br_if 0 (;@12;)
                          end
                          local.get 6
                          i32.const 0
                          i32.store
                          br 8 (;@3;)
                        end
                        block  ;; label = @11
                          i32.const 0
                          i32.load offset=1059448
                          local.tee 3
                          local.get 2
                          i32.lt_u
                          br_if 0 (;@11;)
                          i32.const 0
                          i32.load offset=1059460
                          local.set 4
                          block  ;; label = @12
                            block  ;; label = @13
                              local.get 3
                              local.get 2
                              i32.sub
                              local.tee 5
                              i32.const 16
                              i32.lt_u
                              br_if 0 (;@13;)
                              local.get 4
                              local.get 2
                              i32.add
                              local.tee 0
                              local.get 5
                              i32.const 1
                              i32.or
                              i32.store offset=4
                              i32.const 0
                              local.get 5
                              i32.store offset=1059448
                              i32.const 0
                              local.get 0
                              i32.store offset=1059460
                              local.get 4
                              local.get 3
                              i32.add
                              local.get 5
                              i32.store
                              local.get 4
                              local.get 2
                              i32.const 3
                              i32.or
                              i32.store offset=4
                              br 1 (;@12;)
                            end
                            local.get 4
                            local.get 3
                            i32.const 3
                            i32.or
                            i32.store offset=4
                            local.get 4
                            local.get 3
                            i32.add
                            local.tee 3
                            local.get 3
                            i32.load offset=4
                            i32.const 1
                            i32.or
                            i32.store offset=4
                            i32.const 0
                            i32.const 0
                            i32.store offset=1059460
                            i32.const 0
                            i32.const 0
                            i32.store offset=1059448
                          end
                          local.get 4
                          i32.const 8
                          i32.add
                          local.set 3
                          br 10 (;@1;)
                        end
                        block  ;; label = @11
                          i32.const 0
                          i32.load offset=1059452
                          local.tee 0
                          local.get 2
                          i32.le_u
                          br_if 0 (;@11;)
                          i32.const 0
                          i32.load offset=1059464
                          local.tee 3
                          local.get 2
                          i32.add
                          local.tee 4
                          local.get 0
                          local.get 2
                          i32.sub
                          local.tee 5
                          i32.const 1
                          i32.or
                          i32.store offset=4
                          i32.const 0
                          local.get 5
                          i32.store offset=1059452
                          i32.const 0
                          local.get 4
                          i32.store offset=1059464
                          local.get 3
                          local.get 2
                          i32.const 3
                          i32.or
                          i32.store offset=4
                          local.get 3
                          i32.const 8
                          i32.add
                          local.set 3
                          br 10 (;@1;)
                        end
                        block  ;; label = @11
                          block  ;; label = @12
                            i32.const 0
                            i32.load offset=1059912
                            i32.eqz
                            br_if 0 (;@12;)
                            i32.const 0
                            i32.load offset=1059920
                            local.set 4
                            br 1 (;@11;)
                          end
                          i32.const 0
                          i64.const -1
                          i64.store offset=1059924 align=4
                          i32.const 0
                          i64.const 281474976776192
                          i64.store offset=1059916 align=4
                          i32.const 0
                          local.get 1
                          i32.const 12
                          i32.add
                          i32.const -16
                          i32.and
                          i32.const 1431655768
                          i32.xor
                          i32.store offset=1059912
                          i32.const 0
                          i32.const 0
                          i32.store offset=1059932
                          i32.const 0
                          i32.const 0
                          i32.store offset=1059884
                          i32.const 65536
                          local.set 4
                        end
                        i32.const 0
                        local.set 3
                        block  ;; label = @11
                          local.get 4
                          local.get 2
                          i32.const 71
                          i32.add
                          local.tee 7
                          i32.add
                          local.tee 6
                          i32.const 0
                          local.get 4
                          i32.sub
                          local.tee 11
                          i32.and
                          local.tee 8
                          local.get 2
                          i32.gt_u
                          br_if 0 (;@11;)
                          i32.const 0
                          i32.const 48
                          i32.store offset=1059936
                          br 10 (;@1;)
                        end
                        block  ;; label = @11
                          i32.const 0
                          i32.load offset=1059880
                          local.tee 3
                          i32.eqz
                          br_if 0 (;@11;)
                          block  ;; label = @12
                            i32.const 0
                            i32.load offset=1059872
                            local.tee 4
                            local.get 8
                            i32.add
                            local.tee 5
                            local.get 4
                            i32.le_u
                            br_if 0 (;@12;)
                            local.get 5
                            local.get 3
                            i32.le_u
                            br_if 1 (;@11;)
                          end
                          i32.const 0
                          local.set 3
                          i32.const 0
                          i32.const 48
                          i32.store offset=1059936
                          br 10 (;@1;)
                        end
                        i32.const 0
                        i32.load8_u offset=1059884
                        i32.const 4
                        i32.and
                        br_if 4 (;@6;)
                        block  ;; label = @11
                          block  ;; label = @12
                            block  ;; label = @13
                              i32.const 0
                              i32.load offset=1059464
                              local.tee 4
                              i32.eqz
                              br_if 0 (;@13;)
                              i32.const 1059888
                              local.set 3
                              loop  ;; label = @14
                                block  ;; label = @15
                                  local.get 3
                                  i32.load
                                  local.tee 5
                                  local.get 4
                                  i32.gt_u
                                  br_if 0 (;@15;)
                                  local.get 5
                                  local.get 3
                                  i32.load offset=4
                                  i32.add
                                  local.get 4
                                  i32.gt_u
                                  br_if 3 (;@12;)
                                end
                                local.get 3
                                i32.load offset=8
                                local.tee 3
                                br_if 0 (;@14;)
                              end
                            end
                            i32.const 0
                            call $sbrk
                            local.tee 0
                            i32.const -1
                            i32.eq
                            br_if 5 (;@7;)
                            local.get 8
                            local.set 6
                            block  ;; label = @13
                              i32.const 0
                              i32.load offset=1059916
                              local.tee 3
                              i32.const -1
                              i32.add
                              local.tee 4
                              local.get 0
                              i32.and
                              i32.eqz
                              br_if 0 (;@13;)
                              local.get 8
                              local.get 0
                              i32.sub
                              local.get 4
                              local.get 0
                              i32.add
                              i32.const 0
                              local.get 3
                              i32.sub
                              i32.and
                              i32.add
                              local.set 6
                            end
                            local.get 6
                            local.get 2
                            i32.le_u
                            br_if 5 (;@7;)
                            local.get 6
                            i32.const 2147483646
                            i32.gt_u
                            br_if 5 (;@7;)
                            block  ;; label = @13
                              i32.const 0
                              i32.load offset=1059880
                              local.tee 3
                              i32.eqz
                              br_if 0 (;@13;)
                              i32.const 0
                              i32.load offset=1059872
                              local.tee 4
                              local.get 6
                              i32.add
                              local.tee 5
                              local.get 4
                              i32.le_u
                              br_if 6 (;@7;)
                              local.get 5
                              local.get 3
                              i32.gt_u
                              br_if 6 (;@7;)
                            end
                            local.get 6
                            call $sbrk
                            local.tee 3
                            local.get 0
                            i32.ne
                            br_if 1 (;@11;)
                            br 7 (;@5;)
                          end
                          local.get 6
                          local.get 0
                          i32.sub
                          local.get 11
                          i32.and
                          local.tee 6
                          i32.const 2147483646
                          i32.gt_u
                          br_if 4 (;@7;)
                          local.get 6
                          call $sbrk
                          local.tee 0
                          local.get 3
                          i32.load
                          local.get 3
                          i32.load offset=4
                          i32.add
                          i32.eq
                          br_if 3 (;@8;)
                          local.get 0
                          local.set 3
                        end
                        block  ;; label = @11
                          local.get 3
                          i32.const -1
                          i32.eq
                          br_if 0 (;@11;)
                          local.get 2
                          i32.const 72
                          i32.add
                          local.get 6
                          i32.le_u
                          br_if 0 (;@11;)
                          block  ;; label = @12
                            local.get 7
                            local.get 6
                            i32.sub
                            i32.const 0
                            i32.load offset=1059920
                            local.tee 4
                            i32.add
                            i32.const 0
                            local.get 4
                            i32.sub
                            i32.and
                            local.tee 4
                            i32.const 2147483646
                            i32.le_u
                            br_if 0 (;@12;)
                            local.get 3
                            local.set 0
                            br 7 (;@5;)
                          end
                          block  ;; label = @12
                            local.get 4
                            call $sbrk
                            i32.const -1
                            i32.eq
                            br_if 0 (;@12;)
                            local.get 4
                            local.get 6
                            i32.add
                            local.set 6
                            local.get 3
                            local.set 0
                            br 7 (;@5;)
                          end
                          i32.const 0
                          local.get 6
                          i32.sub
                          call $sbrk
                          drop
                          br 4 (;@7;)
                        end
                        local.get 3
                        local.set 0
                        local.get 3
                        i32.const -1
                        i32.ne
                        br_if 5 (;@5;)
                        br 3 (;@7;)
                      end
                      i32.const 0
                      local.set 8
                      br 7 (;@2;)
                    end
                    i32.const 0
                    local.set 0
                    br 5 (;@3;)
                  end
                  local.get 0
                  i32.const -1
                  i32.ne
                  br_if 2 (;@5;)
                end
                i32.const 0
                i32.const 0
                i32.load offset=1059884
                i32.const 4
                i32.or
                i32.store offset=1059884
              end
              local.get 8
              i32.const 2147483646
              i32.gt_u
              br_if 1 (;@4;)
              local.get 8
              call $sbrk
              local.set 0
              i32.const 0
              call $sbrk
              local.set 3
              local.get 0
              i32.const -1
              i32.eq
              br_if 1 (;@4;)
              local.get 3
              i32.const -1
              i32.eq
              br_if 1 (;@4;)
              local.get 0
              local.get 3
              i32.ge_u
              br_if 1 (;@4;)
              local.get 3
              local.get 0
              i32.sub
              local.tee 6
              local.get 2
              i32.const 56
              i32.add
              i32.le_u
              br_if 1 (;@4;)
            end
            i32.const 0
            i32.const 0
            i32.load offset=1059872
            local.get 6
            i32.add
            local.tee 3
            i32.store offset=1059872
            block  ;; label = @5
              local.get 3
              i32.const 0
              i32.load offset=1059876
              i32.le_u
              br_if 0 (;@5;)
              i32.const 0
              local.get 3
              i32.store offset=1059876
            end
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    i32.const 0
                    i32.load offset=1059464
                    local.tee 4
                    i32.eqz
                    br_if 0 (;@8;)
                    i32.const 1059888
                    local.set 3
                    loop  ;; label = @9
                      local.get 0
                      local.get 3
                      i32.load
                      local.tee 5
                      local.get 3
                      i32.load offset=4
                      local.tee 8
                      i32.add
                      i32.eq
                      br_if 2 (;@7;)
                      local.get 3
                      i32.load offset=8
                      local.tee 3
                      br_if 0 (;@9;)
                      br 3 (;@6;)
                    end
                  end
                  block  ;; label = @8
                    block  ;; label = @9
                      i32.const 0
                      i32.load offset=1059456
                      local.tee 3
                      i32.eqz
                      br_if 0 (;@9;)
                      local.get 0
                      local.get 3
                      i32.ge_u
                      br_if 1 (;@8;)
                    end
                    i32.const 0
                    local.get 0
                    i32.store offset=1059456
                  end
                  i32.const 0
                  local.set 3
                  i32.const 0
                  local.get 6
                  i32.store offset=1059892
                  i32.const 0
                  local.get 0
                  i32.store offset=1059888
                  i32.const 0
                  i32.const -1
                  i32.store offset=1059472
                  i32.const 0
                  i32.const 0
                  i32.load offset=1059912
                  i32.store offset=1059476
                  i32.const 0
                  i32.const 0
                  i32.store offset=1059900
                  loop  ;; label = @8
                    local.get 3
                    i32.const 1059500
                    i32.add
                    local.get 3
                    i32.const 1059488
                    i32.add
                    local.tee 4
                    i32.store
                    local.get 4
                    local.get 3
                    i32.const 1059480
                    i32.add
                    local.tee 5
                    i32.store
                    local.get 3
                    i32.const 1059492
                    i32.add
                    local.get 5
                    i32.store
                    local.get 3
                    i32.const 1059508
                    i32.add
                    local.get 3
                    i32.const 1059496
                    i32.add
                    local.tee 5
                    i32.store
                    local.get 5
                    local.get 4
                    i32.store
                    local.get 3
                    i32.const 1059516
                    i32.add
                    local.get 3
                    i32.const 1059504
                    i32.add
                    local.tee 4
                    i32.store
                    local.get 4
                    local.get 5
                    i32.store
                    local.get 3
                    i32.const 1059512
                    i32.add
                    local.get 4
                    i32.store
                    local.get 3
                    i32.const 32
                    i32.add
                    local.tee 3
                    i32.const 256
                    i32.ne
                    br_if 0 (;@8;)
                  end
                  local.get 0
                  i32.const -8
                  local.get 0
                  i32.sub
                  i32.const 15
                  i32.and
                  i32.const 0
                  local.get 0
                  i32.const 8
                  i32.add
                  i32.const 15
                  i32.and
                  select
                  local.tee 3
                  i32.add
                  local.tee 4
                  local.get 6
                  i32.const -56
                  i32.add
                  local.tee 5
                  local.get 3
                  i32.sub
                  local.tee 3
                  i32.const 1
                  i32.or
                  i32.store offset=4
                  i32.const 0
                  i32.const 0
                  i32.load offset=1059928
                  i32.store offset=1059468
                  i32.const 0
                  local.get 3
                  i32.store offset=1059452
                  i32.const 0
                  local.get 4
                  i32.store offset=1059464
                  local.get 0
                  local.get 5
                  i32.add
                  i32.const 56
                  i32.store offset=4
                  br 2 (;@5;)
                end
                local.get 3
                i32.load8_u offset=12
                i32.const 8
                i32.and
                br_if 0 (;@6;)
                local.get 5
                local.get 4
                i32.gt_u
                br_if 0 (;@6;)
                local.get 0
                local.get 4
                i32.le_u
                br_if 0 (;@6;)
                local.get 4
                i32.const -8
                local.get 4
                i32.sub
                i32.const 15
                i32.and
                i32.const 0
                local.get 4
                i32.const 8
                i32.add
                i32.const 15
                i32.and
                select
                local.tee 5
                i32.add
                local.tee 0
                i32.const 0
                i32.load offset=1059452
                local.get 6
                i32.add
                local.tee 11
                local.get 5
                i32.sub
                local.tee 5
                i32.const 1
                i32.or
                i32.store offset=4
                local.get 3
                local.get 8
                local.get 6
                i32.add
                i32.store offset=4
                i32.const 0
                i32.const 0
                i32.load offset=1059928
                i32.store offset=1059468
                i32.const 0
                local.get 5
                i32.store offset=1059452
                i32.const 0
                local.get 0
                i32.store offset=1059464
                local.get 4
                local.get 11
                i32.add
                i32.const 56
                i32.store offset=4
                br 1 (;@5;)
              end
              block  ;; label = @6
                local.get 0
                i32.const 0
                i32.load offset=1059456
                local.tee 8
                i32.ge_u
                br_if 0 (;@6;)
                i32.const 0
                local.get 0
                i32.store offset=1059456
                local.get 0
                local.set 8
              end
              local.get 0
              local.get 6
              i32.add
              local.set 5
              i32.const 1059888
              local.set 3
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            loop  ;; label = @13
                              local.get 3
                              i32.load
                              local.get 5
                              i32.eq
                              br_if 1 (;@12;)
                              local.get 3
                              i32.load offset=8
                              local.tee 3
                              br_if 0 (;@13;)
                              br 2 (;@11;)
                            end
                          end
                          local.get 3
                          i32.load8_u offset=12
                          i32.const 8
                          i32.and
                          i32.eqz
                          br_if 1 (;@10;)
                        end
                        i32.const 1059888
                        local.set 3
                        loop  ;; label = @11
                          block  ;; label = @12
                            local.get 3
                            i32.load
                            local.tee 5
                            local.get 4
                            i32.gt_u
                            br_if 0 (;@12;)
                            local.get 5
                            local.get 3
                            i32.load offset=4
                            i32.add
                            local.tee 5
                            local.get 4
                            i32.gt_u
                            br_if 3 (;@9;)
                          end
                          local.get 3
                          i32.load offset=8
                          local.set 3
                          br 0 (;@11;)
                        end
                      end
                      local.get 3
                      local.get 0
                      i32.store
                      local.get 3
                      local.get 3
                      i32.load offset=4
                      local.get 6
                      i32.add
                      i32.store offset=4
                      local.get 0
                      i32.const -8
                      local.get 0
                      i32.sub
                      i32.const 15
                      i32.and
                      i32.const 0
                      local.get 0
                      i32.const 8
                      i32.add
                      i32.const 15
                      i32.and
                      select
                      i32.add
                      local.tee 11
                      local.get 2
                      i32.const 3
                      i32.or
                      i32.store offset=4
                      local.get 5
                      i32.const -8
                      local.get 5
                      i32.sub
                      i32.const 15
                      i32.and
                      i32.const 0
                      local.get 5
                      i32.const 8
                      i32.add
                      i32.const 15
                      i32.and
                      select
                      i32.add
                      local.tee 6
                      local.get 11
                      local.get 2
                      i32.add
                      local.tee 2
                      i32.sub
                      local.set 5
                      block  ;; label = @10
                        local.get 4
                        local.get 6
                        i32.ne
                        br_if 0 (;@10;)
                        i32.const 0
                        local.get 2
                        i32.store offset=1059464
                        i32.const 0
                        i32.const 0
                        i32.load offset=1059452
                        local.get 5
                        i32.add
                        local.tee 3
                        i32.store offset=1059452
                        local.get 2
                        local.get 3
                        i32.const 1
                        i32.or
                        i32.store offset=4
                        br 3 (;@7;)
                      end
                      block  ;; label = @10
                        i32.const 0
                        i32.load offset=1059460
                        local.get 6
                        i32.ne
                        br_if 0 (;@10;)
                        i32.const 0
                        local.get 2
                        i32.store offset=1059460
                        i32.const 0
                        i32.const 0
                        i32.load offset=1059448
                        local.get 5
                        i32.add
                        local.tee 3
                        i32.store offset=1059448
                        local.get 2
                        local.get 3
                        i32.const 1
                        i32.or
                        i32.store offset=4
                        local.get 2
                        local.get 3
                        i32.add
                        local.get 3
                        i32.store
                        br 3 (;@7;)
                      end
                      block  ;; label = @10
                        local.get 6
                        i32.load offset=4
                        local.tee 3
                        i32.const 3
                        i32.and
                        i32.const 1
                        i32.ne
                        br_if 0 (;@10;)
                        local.get 3
                        i32.const -8
                        i32.and
                        local.set 7
                        block  ;; label = @11
                          block  ;; label = @12
                            local.get 3
                            i32.const 255
                            i32.gt_u
                            br_if 0 (;@12;)
                            local.get 6
                            i32.load offset=8
                            local.tee 4
                            local.get 3
                            i32.const 3
                            i32.shr_u
                            local.tee 8
                            i32.const 3
                            i32.shl
                            i32.const 1059480
                            i32.add
                            local.tee 0
                            i32.eq
                            drop
                            block  ;; label = @13
                              local.get 6
                              i32.load offset=12
                              local.tee 3
                              local.get 4
                              i32.ne
                              br_if 0 (;@13;)
                              i32.const 0
                              i32.const 0
                              i32.load offset=1059440
                              i32.const -2
                              local.get 8
                              i32.rotl
                              i32.and
                              i32.store offset=1059440
                              br 2 (;@11;)
                            end
                            local.get 3
                            local.get 0
                            i32.eq
                            drop
                            local.get 3
                            local.get 4
                            i32.store offset=8
                            local.get 4
                            local.get 3
                            i32.store offset=12
                            br 1 (;@11;)
                          end
                          local.get 6
                          i32.load offset=24
                          local.set 9
                          block  ;; label = @12
                            block  ;; label = @13
                              local.get 6
                              i32.load offset=12
                              local.tee 0
                              local.get 6
                              i32.eq
                              br_if 0 (;@13;)
                              local.get 8
                              local.get 6
                              i32.load offset=8
                              local.tee 3
                              i32.gt_u
                              drop
                              local.get 0
                              local.get 3
                              i32.store offset=8
                              local.get 3
                              local.get 0
                              i32.store offset=12
                              br 1 (;@12;)
                            end
                            block  ;; label = @13
                              local.get 6
                              i32.const 20
                              i32.add
                              local.tee 3
                              i32.load
                              local.tee 4
                              br_if 0 (;@13;)
                              local.get 6
                              i32.const 16
                              i32.add
                              local.tee 3
                              i32.load
                              local.tee 4
                              br_if 0 (;@13;)
                              i32.const 0
                              local.set 0
                              br 1 (;@12;)
                            end
                            loop  ;; label = @13
                              local.get 3
                              local.set 8
                              local.get 4
                              local.tee 0
                              i32.const 20
                              i32.add
                              local.tee 3
                              i32.load
                              local.tee 4
                              br_if 0 (;@13;)
                              local.get 0
                              i32.const 16
                              i32.add
                              local.set 3
                              local.get 0
                              i32.load offset=16
                              local.tee 4
                              br_if 0 (;@13;)
                            end
                            local.get 8
                            i32.const 0
                            i32.store
                          end
                          local.get 9
                          i32.eqz
                          br_if 0 (;@11;)
                          block  ;; label = @12
                            block  ;; label = @13
                              local.get 6
                              i32.load offset=28
                              local.tee 4
                              i32.const 2
                              i32.shl
                              i32.const 1059744
                              i32.add
                              local.tee 3
                              i32.load
                              local.get 6
                              i32.ne
                              br_if 0 (;@13;)
                              local.get 3
                              local.get 0
                              i32.store
                              local.get 0
                              br_if 1 (;@12;)
                              i32.const 0
                              i32.const 0
                              i32.load offset=1059444
                              i32.const -2
                              local.get 4
                              i32.rotl
                              i32.and
                              i32.store offset=1059444
                              br 2 (;@11;)
                            end
                            local.get 9
                            i32.const 16
                            i32.const 20
                            local.get 9
                            i32.load offset=16
                            local.get 6
                            i32.eq
                            select
                            i32.add
                            local.get 0
                            i32.store
                            local.get 0
                            i32.eqz
                            br_if 1 (;@11;)
                          end
                          local.get 0
                          local.get 9
                          i32.store offset=24
                          block  ;; label = @12
                            local.get 6
                            i32.load offset=16
                            local.tee 3
                            i32.eqz
                            br_if 0 (;@12;)
                            local.get 0
                            local.get 3
                            i32.store offset=16
                            local.get 3
                            local.get 0
                            i32.store offset=24
                          end
                          local.get 6
                          i32.load offset=20
                          local.tee 3
                          i32.eqz
                          br_if 0 (;@11;)
                          local.get 0
                          i32.const 20
                          i32.add
                          local.get 3
                          i32.store
                          local.get 3
                          local.get 0
                          i32.store offset=24
                        end
                        local.get 7
                        local.get 5
                        i32.add
                        local.set 5
                        local.get 6
                        local.get 7
                        i32.add
                        local.set 6
                      end
                      local.get 6
                      local.get 6
                      i32.load offset=4
                      i32.const -2
                      i32.and
                      i32.store offset=4
                      local.get 2
                      local.get 5
                      i32.add
                      local.get 5
                      i32.store
                      local.get 2
                      local.get 5
                      i32.const 1
                      i32.or
                      i32.store offset=4
                      block  ;; label = @10
                        local.get 5
                        i32.const 255
                        i32.gt_u
                        br_if 0 (;@10;)
                        local.get 5
                        i32.const 3
                        i32.shr_u
                        local.tee 4
                        i32.const 3
                        i32.shl
                        i32.const 1059480
                        i32.add
                        local.set 3
                        block  ;; label = @11
                          block  ;; label = @12
                            i32.const 0
                            i32.load offset=1059440
                            local.tee 5
                            i32.const 1
                            local.get 4
                            i32.shl
                            local.tee 4
                            i32.and
                            br_if 0 (;@12;)
                            i32.const 0
                            local.get 5
                            local.get 4
                            i32.or
                            i32.store offset=1059440
                            local.get 3
                            local.set 4
                            br 1 (;@11;)
                          end
                          local.get 3
                          i32.load offset=8
                          local.set 4
                        end
                        local.get 4
                        local.get 2
                        i32.store offset=12
                        local.get 3
                        local.get 2
                        i32.store offset=8
                        local.get 2
                        local.get 3
                        i32.store offset=12
                        local.get 2
                        local.get 4
                        i32.store offset=8
                        br 3 (;@7;)
                      end
                      i32.const 31
                      local.set 3
                      block  ;; label = @10
                        local.get 5
                        i32.const 16777215
                        i32.gt_u
                        br_if 0 (;@10;)
                        local.get 5
                        i32.const 8
                        i32.shr_u
                        local.tee 3
                        local.get 3
                        i32.const 1048320
                        i32.add
                        i32.const 16
                        i32.shr_u
                        i32.const 8
                        i32.and
                        local.tee 3
                        i32.shl
                        local.tee 4
                        local.get 4
                        i32.const 520192
                        i32.add
                        i32.const 16
                        i32.shr_u
                        i32.const 4
                        i32.and
                        local.tee 4
                        i32.shl
                        local.tee 0
                        local.get 0
                        i32.const 245760
                        i32.add
                        i32.const 16
                        i32.shr_u
                        i32.const 2
                        i32.and
                        local.tee 0
                        i32.shl
                        i32.const 15
                        i32.shr_u
                        local.get 3
                        local.get 4
                        i32.or
                        local.get 0
                        i32.or
                        i32.sub
                        local.tee 3
                        i32.const 1
                        i32.shl
                        local.get 5
                        local.get 3
                        i32.const 21
                        i32.add
                        i32.shr_u
                        i32.const 1
                        i32.and
                        i32.or
                        i32.const 28
                        i32.add
                        local.set 3
                      end
                      local.get 2
                      local.get 3
                      i32.store offset=28
                      local.get 2
                      i64.const 0
                      i64.store offset=16 align=4
                      local.get 3
                      i32.const 2
                      i32.shl
                      i32.const 1059744
                      i32.add
                      local.set 4
                      block  ;; label = @10
                        i32.const 0
                        i32.load offset=1059444
                        local.tee 0
                        i32.const 1
                        local.get 3
                        i32.shl
                        local.tee 8
                        i32.and
                        br_if 0 (;@10;)
                        local.get 4
                        local.get 2
                        i32.store
                        i32.const 0
                        local.get 0
                        local.get 8
                        i32.or
                        i32.store offset=1059444
                        local.get 2
                        local.get 4
                        i32.store offset=24
                        local.get 2
                        local.get 2
                        i32.store offset=8
                        local.get 2
                        local.get 2
                        i32.store offset=12
                        br 3 (;@7;)
                      end
                      local.get 5
                      i32.const 0
                      i32.const 25
                      local.get 3
                      i32.const 1
                      i32.shr_u
                      i32.sub
                      local.get 3
                      i32.const 31
                      i32.eq
                      select
                      i32.shl
                      local.set 3
                      local.get 4
                      i32.load
                      local.set 0
                      loop  ;; label = @10
                        local.get 0
                        local.tee 4
                        i32.load offset=4
                        i32.const -8
                        i32.and
                        local.get 5
                        i32.eq
                        br_if 2 (;@8;)
                        local.get 3
                        i32.const 29
                        i32.shr_u
                        local.set 0
                        local.get 3
                        i32.const 1
                        i32.shl
                        local.set 3
                        local.get 4
                        local.get 0
                        i32.const 4
                        i32.and
                        i32.add
                        i32.const 16
                        i32.add
                        local.tee 8
                        i32.load
                        local.tee 0
                        br_if 0 (;@10;)
                      end
                      local.get 8
                      local.get 2
                      i32.store
                      local.get 2
                      local.get 4
                      i32.store offset=24
                      local.get 2
                      local.get 2
                      i32.store offset=12
                      local.get 2
                      local.get 2
                      i32.store offset=8
                      br 2 (;@7;)
                    end
                    local.get 0
                    i32.const -8
                    local.get 0
                    i32.sub
                    i32.const 15
                    i32.and
                    i32.const 0
                    local.get 0
                    i32.const 8
                    i32.add
                    i32.const 15
                    i32.and
                    select
                    local.tee 3
                    i32.add
                    local.tee 11
                    local.get 6
                    i32.const -56
                    i32.add
                    local.tee 8
                    local.get 3
                    i32.sub
                    local.tee 3
                    i32.const 1
                    i32.or
                    i32.store offset=4
                    local.get 0
                    local.get 8
                    i32.add
                    i32.const 56
                    i32.store offset=4
                    local.get 4
                    local.get 5
                    i32.const 55
                    local.get 5
                    i32.sub
                    i32.const 15
                    i32.and
                    i32.const 0
                    local.get 5
                    i32.const -55
                    i32.add
                    i32.const 15
                    i32.and
                    select
                    i32.add
                    i32.const -63
                    i32.add
                    local.tee 8
                    local.get 8
                    local.get 4
                    i32.const 16
                    i32.add
                    i32.lt_u
                    select
                    local.tee 8
                    i32.const 35
                    i32.store offset=4
                    i32.const 0
                    i32.const 0
                    i32.load offset=1059928
                    i32.store offset=1059468
                    i32.const 0
                    local.get 3
                    i32.store offset=1059452
                    i32.const 0
                    local.get 11
                    i32.store offset=1059464
                    local.get 8
                    i32.const 16
                    i32.add
                    i32.const 0
                    i64.load offset=1059896 align=4
                    i64.store align=4
                    local.get 8
                    i32.const 0
                    i64.load offset=1059888 align=4
                    i64.store offset=8 align=4
                    i32.const 0
                    local.get 8
                    i32.const 8
                    i32.add
                    i32.store offset=1059896
                    i32.const 0
                    local.get 6
                    i32.store offset=1059892
                    i32.const 0
                    local.get 0
                    i32.store offset=1059888
                    i32.const 0
                    i32.const 0
                    i32.store offset=1059900
                    local.get 8
                    i32.const 36
                    i32.add
                    local.set 3
                    loop  ;; label = @9
                      local.get 3
                      i32.const 7
                      i32.store
                      local.get 5
                      local.get 3
                      i32.const 4
                      i32.add
                      local.tee 3
                      i32.gt_u
                      br_if 0 (;@9;)
                    end
                    local.get 8
                    local.get 4
                    i32.eq
                    br_if 3 (;@5;)
                    local.get 8
                    local.get 8
                    i32.load offset=4
                    i32.const -2
                    i32.and
                    i32.store offset=4
                    local.get 8
                    local.get 8
                    local.get 4
                    i32.sub
                    local.tee 6
                    i32.store
                    local.get 4
                    local.get 6
                    i32.const 1
                    i32.or
                    i32.store offset=4
                    block  ;; label = @9
                      local.get 6
                      i32.const 255
                      i32.gt_u
                      br_if 0 (;@9;)
                      local.get 6
                      i32.const 3
                      i32.shr_u
                      local.tee 5
                      i32.const 3
                      i32.shl
                      i32.const 1059480
                      i32.add
                      local.set 3
                      block  ;; label = @10
                        block  ;; label = @11
                          i32.const 0
                          i32.load offset=1059440
                          local.tee 0
                          i32.const 1
                          local.get 5
                          i32.shl
                          local.tee 5
                          i32.and
                          br_if 0 (;@11;)
                          i32.const 0
                          local.get 0
                          local.get 5
                          i32.or
                          i32.store offset=1059440
                          local.get 3
                          local.set 5
                          br 1 (;@10;)
                        end
                        local.get 3
                        i32.load offset=8
                        local.set 5
                      end
                      local.get 5
                      local.get 4
                      i32.store offset=12
                      local.get 3
                      local.get 4
                      i32.store offset=8
                      local.get 4
                      local.get 3
                      i32.store offset=12
                      local.get 4
                      local.get 5
                      i32.store offset=8
                      br 4 (;@5;)
                    end
                    i32.const 31
                    local.set 3
                    block  ;; label = @9
                      local.get 6
                      i32.const 16777215
                      i32.gt_u
                      br_if 0 (;@9;)
                      local.get 6
                      i32.const 8
                      i32.shr_u
                      local.tee 3
                      local.get 3
                      i32.const 1048320
                      i32.add
                      i32.const 16
                      i32.shr_u
                      i32.const 8
                      i32.and
                      local.tee 3
                      i32.shl
                      local.tee 5
                      local.get 5
                      i32.const 520192
                      i32.add
                      i32.const 16
                      i32.shr_u
                      i32.const 4
                      i32.and
                      local.tee 5
                      i32.shl
                      local.tee 0
                      local.get 0
                      i32.const 245760
                      i32.add
                      i32.const 16
                      i32.shr_u
                      i32.const 2
                      i32.and
                      local.tee 0
                      i32.shl
                      i32.const 15
                      i32.shr_u
                      local.get 3
                      local.get 5
                      i32.or
                      local.get 0
                      i32.or
                      i32.sub
                      local.tee 3
                      i32.const 1
                      i32.shl
                      local.get 6
                      local.get 3
                      i32.const 21
                      i32.add
                      i32.shr_u
                      i32.const 1
                      i32.and
                      i32.or
                      i32.const 28
                      i32.add
                      local.set 3
                    end
                    local.get 4
                    i64.const 0
                    i64.store offset=16 align=4
                    local.get 4
                    i32.const 28
                    i32.add
                    local.get 3
                    i32.store
                    local.get 3
                    i32.const 2
                    i32.shl
                    i32.const 1059744
                    i32.add
                    local.set 5
                    block  ;; label = @9
                      i32.const 0
                      i32.load offset=1059444
                      local.tee 0
                      i32.const 1
                      local.get 3
                      i32.shl
                      local.tee 8
                      i32.and
                      br_if 0 (;@9;)
                      local.get 5
                      local.get 4
                      i32.store
                      i32.const 0
                      local.get 0
                      local.get 8
                      i32.or
                      i32.store offset=1059444
                      local.get 4
                      i32.const 24
                      i32.add
                      local.get 5
                      i32.store
                      local.get 4
                      local.get 4
                      i32.store offset=8
                      local.get 4
                      local.get 4
                      i32.store offset=12
                      br 4 (;@5;)
                    end
                    local.get 6
                    i32.const 0
                    i32.const 25
                    local.get 3
                    i32.const 1
                    i32.shr_u
                    i32.sub
                    local.get 3
                    i32.const 31
                    i32.eq
                    select
                    i32.shl
                    local.set 3
                    local.get 5
                    i32.load
                    local.set 0
                    loop  ;; label = @9
                      local.get 0
                      local.tee 5
                      i32.load offset=4
                      i32.const -8
                      i32.and
                      local.get 6
                      i32.eq
                      br_if 3 (;@6;)
                      local.get 3
                      i32.const 29
                      i32.shr_u
                      local.set 0
                      local.get 3
                      i32.const 1
                      i32.shl
                      local.set 3
                      local.get 5
                      local.get 0
                      i32.const 4
                      i32.and
                      i32.add
                      i32.const 16
                      i32.add
                      local.tee 8
                      i32.load
                      local.tee 0
                      br_if 0 (;@9;)
                    end
                    local.get 8
                    local.get 4
                    i32.store
                    local.get 4
                    i32.const 24
                    i32.add
                    local.get 5
                    i32.store
                    local.get 4
                    local.get 4
                    i32.store offset=12
                    local.get 4
                    local.get 4
                    i32.store offset=8
                    br 3 (;@5;)
                  end
                  local.get 4
                  i32.load offset=8
                  local.tee 3
                  local.get 2
                  i32.store offset=12
                  local.get 4
                  local.get 2
                  i32.store offset=8
                  local.get 2
                  i32.const 0
                  i32.store offset=24
                  local.get 2
                  local.get 4
                  i32.store offset=12
                  local.get 2
                  local.get 3
                  i32.store offset=8
                end
                local.get 11
                i32.const 8
                i32.add
                local.set 3
                br 5 (;@1;)
              end
              local.get 5
              i32.load offset=8
              local.tee 3
              local.get 4
              i32.store offset=12
              local.get 5
              local.get 4
              i32.store offset=8
              local.get 4
              i32.const 24
              i32.add
              i32.const 0
              i32.store
              local.get 4
              local.get 5
              i32.store offset=12
              local.get 4
              local.get 3
              i32.store offset=8
            end
            i32.const 0
            i32.load offset=1059452
            local.tee 3
            local.get 2
            i32.le_u
            br_if 0 (;@4;)
            i32.const 0
            i32.load offset=1059464
            local.tee 4
            local.get 2
            i32.add
            local.tee 5
            local.get 3
            local.get 2
            i32.sub
            local.tee 3
            i32.const 1
            i32.or
            i32.store offset=4
            i32.const 0
            local.get 3
            i32.store offset=1059452
            i32.const 0
            local.get 5
            i32.store offset=1059464
            local.get 4
            local.get 2
            i32.const 3
            i32.or
            i32.store offset=4
            local.get 4
            i32.const 8
            i32.add
            local.set 3
            br 3 (;@1;)
          end
          i32.const 0
          local.set 3
          i32.const 0
          i32.const 48
          i32.store offset=1059936
          br 2 (;@1;)
        end
        block  ;; label = @3
          local.get 11
          i32.eqz
          br_if 0 (;@3;)
          block  ;; label = @4
            block  ;; label = @5
              local.get 8
              local.get 8
              i32.load offset=28
              local.tee 5
              i32.const 2
              i32.shl
              i32.const 1059744
              i32.add
              local.tee 3
              i32.load
              i32.ne
              br_if 0 (;@5;)
              local.get 3
              local.get 0
              i32.store
              local.get 0
              br_if 1 (;@4;)
              i32.const 0
              local.get 7
              i32.const -2
              local.get 5
              i32.rotl
              i32.and
              local.tee 7
              i32.store offset=1059444
              br 2 (;@3;)
            end
            local.get 11
            i32.const 16
            i32.const 20
            local.get 11
            i32.load offset=16
            local.get 8
            i32.eq
            select
            i32.add
            local.get 0
            i32.store
            local.get 0
            i32.eqz
            br_if 1 (;@3;)
          end
          local.get 0
          local.get 11
          i32.store offset=24
          block  ;; label = @4
            local.get 8
            i32.load offset=16
            local.tee 3
            i32.eqz
            br_if 0 (;@4;)
            local.get 0
            local.get 3
            i32.store offset=16
            local.get 3
            local.get 0
            i32.store offset=24
          end
          local.get 8
          i32.const 20
          i32.add
          i32.load
          local.tee 3
          i32.eqz
          br_if 0 (;@3;)
          local.get 0
          i32.const 20
          i32.add
          local.get 3
          i32.store
          local.get 3
          local.get 0
          i32.store offset=24
        end
        block  ;; label = @3
          block  ;; label = @4
            local.get 4
            i32.const 15
            i32.gt_u
            br_if 0 (;@4;)
            local.get 8
            local.get 4
            local.get 2
            i32.add
            local.tee 3
            i32.const 3
            i32.or
            i32.store offset=4
            local.get 8
            local.get 3
            i32.add
            local.tee 3
            local.get 3
            i32.load offset=4
            i32.const 1
            i32.or
            i32.store offset=4
            br 1 (;@3;)
          end
          local.get 8
          local.get 2
          i32.add
          local.tee 0
          local.get 4
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 8
          local.get 2
          i32.const 3
          i32.or
          i32.store offset=4
          local.get 0
          local.get 4
          i32.add
          local.get 4
          i32.store
          block  ;; label = @4
            local.get 4
            i32.const 255
            i32.gt_u
            br_if 0 (;@4;)
            local.get 4
            i32.const 3
            i32.shr_u
            local.tee 4
            i32.const 3
            i32.shl
            i32.const 1059480
            i32.add
            local.set 3
            block  ;; label = @5
              block  ;; label = @6
                i32.const 0
                i32.load offset=1059440
                local.tee 5
                i32.const 1
                local.get 4
                i32.shl
                local.tee 4
                i32.and
                br_if 0 (;@6;)
                i32.const 0
                local.get 5
                local.get 4
                i32.or
                i32.store offset=1059440
                local.get 3
                local.set 4
                br 1 (;@5;)
              end
              local.get 3
              i32.load offset=8
              local.set 4
            end
            local.get 4
            local.get 0
            i32.store offset=12
            local.get 3
            local.get 0
            i32.store offset=8
            local.get 0
            local.get 3
            i32.store offset=12
            local.get 0
            local.get 4
            i32.store offset=8
            br 1 (;@3;)
          end
          i32.const 31
          local.set 3
          block  ;; label = @4
            local.get 4
            i32.const 16777215
            i32.gt_u
            br_if 0 (;@4;)
            local.get 4
            i32.const 8
            i32.shr_u
            local.tee 3
            local.get 3
            i32.const 1048320
            i32.add
            i32.const 16
            i32.shr_u
            i32.const 8
            i32.and
            local.tee 3
            i32.shl
            local.tee 5
            local.get 5
            i32.const 520192
            i32.add
            i32.const 16
            i32.shr_u
            i32.const 4
            i32.and
            local.tee 5
            i32.shl
            local.tee 2
            local.get 2
            i32.const 245760
            i32.add
            i32.const 16
            i32.shr_u
            i32.const 2
            i32.and
            local.tee 2
            i32.shl
            i32.const 15
            i32.shr_u
            local.get 3
            local.get 5
            i32.or
            local.get 2
            i32.or
            i32.sub
            local.tee 3
            i32.const 1
            i32.shl
            local.get 4
            local.get 3
            i32.const 21
            i32.add
            i32.shr_u
            i32.const 1
            i32.and
            i32.or
            i32.const 28
            i32.add
            local.set 3
          end
          local.get 0
          local.get 3
          i32.store offset=28
          local.get 0
          i64.const 0
          i64.store offset=16 align=4
          local.get 3
          i32.const 2
          i32.shl
          i32.const 1059744
          i32.add
          local.set 5
          block  ;; label = @4
            local.get 7
            i32.const 1
            local.get 3
            i32.shl
            local.tee 2
            i32.and
            br_if 0 (;@4;)
            local.get 5
            local.get 0
            i32.store
            i32.const 0
            local.get 7
            local.get 2
            i32.or
            i32.store offset=1059444
            local.get 0
            local.get 5
            i32.store offset=24
            local.get 0
            local.get 0
            i32.store offset=8
            local.get 0
            local.get 0
            i32.store offset=12
            br 1 (;@3;)
          end
          local.get 4
          i32.const 0
          i32.const 25
          local.get 3
          i32.const 1
          i32.shr_u
          i32.sub
          local.get 3
          i32.const 31
          i32.eq
          select
          i32.shl
          local.set 3
          local.get 5
          i32.load
          local.set 2
          block  ;; label = @4
            loop  ;; label = @5
              local.get 2
              local.tee 5
              i32.load offset=4
              i32.const -8
              i32.and
              local.get 4
              i32.eq
              br_if 1 (;@4;)
              local.get 3
              i32.const 29
              i32.shr_u
              local.set 2
              local.get 3
              i32.const 1
              i32.shl
              local.set 3
              local.get 5
              local.get 2
              i32.const 4
              i32.and
              i32.add
              i32.const 16
              i32.add
              local.tee 6
              i32.load
              local.tee 2
              br_if 0 (;@5;)
            end
            local.get 6
            local.get 0
            i32.store
            local.get 0
            local.get 5
            i32.store offset=24
            local.get 0
            local.get 0
            i32.store offset=12
            local.get 0
            local.get 0
            i32.store offset=8
            br 1 (;@3;)
          end
          local.get 5
          i32.load offset=8
          local.tee 3
          local.get 0
          i32.store offset=12
          local.get 5
          local.get 0
          i32.store offset=8
          local.get 0
          i32.const 0
          i32.store offset=24
          local.get 0
          local.get 5
          i32.store offset=12
          local.get 0
          local.get 3
          i32.store offset=8
        end
        local.get 8
        i32.const 8
        i32.add
        local.set 3
        br 1 (;@1;)
      end
      block  ;; label = @2
        local.get 10
        i32.eqz
        br_if 0 (;@2;)
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            local.get 0
            i32.load offset=28
            local.tee 5
            i32.const 2
            i32.shl
            i32.const 1059744
            i32.add
            local.tee 3
            i32.load
            i32.ne
            br_if 0 (;@4;)
            local.get 3
            local.get 8
            i32.store
            local.get 8
            br_if 1 (;@3;)
            i32.const 0
            local.get 9
            i32.const -2
            local.get 5
            i32.rotl
            i32.and
            i32.store offset=1059444
            br 2 (;@2;)
          end
          local.get 10
          i32.const 16
          i32.const 20
          local.get 10
          i32.load offset=16
          local.get 0
          i32.eq
          select
          i32.add
          local.get 8
          i32.store
          local.get 8
          i32.eqz
          br_if 1 (;@2;)
        end
        local.get 8
        local.get 10
        i32.store offset=24
        block  ;; label = @3
          local.get 0
          i32.load offset=16
          local.tee 3
          i32.eqz
          br_if 0 (;@3;)
          local.get 8
          local.get 3
          i32.store offset=16
          local.get 3
          local.get 8
          i32.store offset=24
        end
        local.get 0
        i32.const 20
        i32.add
        i32.load
        local.tee 3
        i32.eqz
        br_if 0 (;@2;)
        local.get 8
        i32.const 20
        i32.add
        local.get 3
        i32.store
        local.get 3
        local.get 8
        i32.store offset=24
      end
      block  ;; label = @2
        block  ;; label = @3
          local.get 4
          i32.const 15
          i32.gt_u
          br_if 0 (;@3;)
          local.get 0
          local.get 4
          local.get 2
          i32.add
          local.tee 3
          i32.const 3
          i32.or
          i32.store offset=4
          local.get 0
          local.get 3
          i32.add
          local.tee 3
          local.get 3
          i32.load offset=4
          i32.const 1
          i32.or
          i32.store offset=4
          br 1 (;@2;)
        end
        local.get 0
        local.get 2
        i32.add
        local.tee 5
        local.get 4
        i32.const 1
        i32.or
        i32.store offset=4
        local.get 0
        local.get 2
        i32.const 3
        i32.or
        i32.store offset=4
        local.get 5
        local.get 4
        i32.add
        local.get 4
        i32.store
        block  ;; label = @3
          local.get 7
          i32.eqz
          br_if 0 (;@3;)
          local.get 7
          i32.const 3
          i32.shr_u
          local.tee 8
          i32.const 3
          i32.shl
          i32.const 1059480
          i32.add
          local.set 2
          i32.const 0
          i32.load offset=1059460
          local.set 3
          block  ;; label = @4
            block  ;; label = @5
              i32.const 1
              local.get 8
              i32.shl
              local.tee 8
              local.get 6
              i32.and
              br_if 0 (;@5;)
              i32.const 0
              local.get 8
              local.get 6
              i32.or
              i32.store offset=1059440
              local.get 2
              local.set 8
              br 1 (;@4;)
            end
            local.get 2
            i32.load offset=8
            local.set 8
          end
          local.get 8
          local.get 3
          i32.store offset=12
          local.get 2
          local.get 3
          i32.store offset=8
          local.get 3
          local.get 2
          i32.store offset=12
          local.get 3
          local.get 8
          i32.store offset=8
        end
        i32.const 0
        local.get 5
        i32.store offset=1059460
        i32.const 0
        local.get 4
        i32.store offset=1059448
      end
      local.get 0
      i32.const 8
      i32.add
      local.set 3
    end
    local.get 1
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 3)
  (func $free (type 1) (param i32)
    local.get 0
    call $dlfree)
  (func $dlfree (type 1) (param i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      local.get 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const -8
      i32.add
      local.tee 1
      local.get 0
      i32.const -4
      i32.add
      i32.load
      local.tee 2
      i32.const -8
      i32.and
      local.tee 0
      i32.add
      local.set 3
      block  ;; label = @2
        local.get 2
        i32.const 1
        i32.and
        br_if 0 (;@2;)
        local.get 2
        i32.const 3
        i32.and
        i32.eqz
        br_if 1 (;@1;)
        local.get 1
        local.get 1
        i32.load
        local.tee 2
        i32.sub
        local.tee 1
        i32.const 0
        i32.load offset=1059456
        local.tee 4
        i32.lt_u
        br_if 1 (;@1;)
        local.get 2
        local.get 0
        i32.add
        local.set 0
        block  ;; label = @3
          i32.const 0
          i32.load offset=1059460
          local.get 1
          i32.eq
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 2
            i32.const 255
            i32.gt_u
            br_if 0 (;@4;)
            local.get 1
            i32.load offset=8
            local.tee 4
            local.get 2
            i32.const 3
            i32.shr_u
            local.tee 5
            i32.const 3
            i32.shl
            i32.const 1059480
            i32.add
            local.tee 6
            i32.eq
            drop
            block  ;; label = @5
              local.get 1
              i32.load offset=12
              local.tee 2
              local.get 4
              i32.ne
              br_if 0 (;@5;)
              i32.const 0
              i32.const 0
              i32.load offset=1059440
              i32.const -2
              local.get 5
              i32.rotl
              i32.and
              i32.store offset=1059440
              br 3 (;@2;)
            end
            local.get 2
            local.get 6
            i32.eq
            drop
            local.get 2
            local.get 4
            i32.store offset=8
            local.get 4
            local.get 2
            i32.store offset=12
            br 2 (;@2;)
          end
          local.get 1
          i32.load offset=24
          local.set 7
          block  ;; label = @4
            block  ;; label = @5
              local.get 1
              i32.load offset=12
              local.tee 6
              local.get 1
              i32.eq
              br_if 0 (;@5;)
              local.get 4
              local.get 1
              i32.load offset=8
              local.tee 2
              i32.gt_u
              drop
              local.get 6
              local.get 2
              i32.store offset=8
              local.get 2
              local.get 6
              i32.store offset=12
              br 1 (;@4;)
            end
            block  ;; label = @5
              local.get 1
              i32.const 20
              i32.add
              local.tee 2
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              local.get 1
              i32.const 16
              i32.add
              local.tee 2
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              i32.const 0
              local.set 6
              br 1 (;@4;)
            end
            loop  ;; label = @5
              local.get 2
              local.set 5
              local.get 4
              local.tee 6
              i32.const 20
              i32.add
              local.tee 2
              i32.load
              local.tee 4
              br_if 0 (;@5;)
              local.get 6
              i32.const 16
              i32.add
              local.set 2
              local.get 6
              i32.load offset=16
              local.tee 4
              br_if 0 (;@5;)
            end
            local.get 5
            i32.const 0
            i32.store
          end
          local.get 7
          i32.eqz
          br_if 1 (;@2;)
          block  ;; label = @4
            block  ;; label = @5
              local.get 1
              i32.load offset=28
              local.tee 4
              i32.const 2
              i32.shl
              i32.const 1059744
              i32.add
              local.tee 2
              i32.load
              local.get 1
              i32.ne
              br_if 0 (;@5;)
              local.get 2
              local.get 6
              i32.store
              local.get 6
              br_if 1 (;@4;)
              i32.const 0
              i32.const 0
              i32.load offset=1059444
              i32.const -2
              local.get 4
              i32.rotl
              i32.and
              i32.store offset=1059444
              br 3 (;@2;)
            end
            local.get 7
            i32.const 16
            i32.const 20
            local.get 7
            i32.load offset=16
            local.get 1
            i32.eq
            select
            i32.add
            local.get 6
            i32.store
            local.get 6
            i32.eqz
            br_if 2 (;@2;)
          end
          local.get 6
          local.get 7
          i32.store offset=24
          block  ;; label = @4
            local.get 1
            i32.load offset=16
            local.tee 2
            i32.eqz
            br_if 0 (;@4;)
            local.get 6
            local.get 2
            i32.store offset=16
            local.get 2
            local.get 6
            i32.store offset=24
          end
          local.get 1
          i32.load offset=20
          local.tee 2
          i32.eqz
          br_if 1 (;@2;)
          local.get 6
          i32.const 20
          i32.add
          local.get 2
          i32.store
          local.get 2
          local.get 6
          i32.store offset=24
          br 1 (;@2;)
        end
        local.get 3
        i32.load offset=4
        local.tee 2
        i32.const 3
        i32.and
        i32.const 3
        i32.ne
        br_if 0 (;@2;)
        local.get 3
        local.get 2
        i32.const -2
        i32.and
        i32.store offset=4
        i32.const 0
        local.get 0
        i32.store offset=1059448
        local.get 1
        local.get 0
        i32.add
        local.get 0
        i32.store
        local.get 1
        local.get 0
        i32.const 1
        i32.or
        i32.store offset=4
        return
      end
      local.get 3
      local.get 1
      i32.le_u
      br_if 0 (;@1;)
      local.get 3
      i32.load offset=4
      local.tee 2
      i32.const 1
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      block  ;; label = @2
        block  ;; label = @3
          local.get 2
          i32.const 2
          i32.and
          br_if 0 (;@3;)
          block  ;; label = @4
            i32.const 0
            i32.load offset=1059464
            local.get 3
            i32.ne
            br_if 0 (;@4;)
            i32.const 0
            local.get 1
            i32.store offset=1059464
            i32.const 0
            i32.const 0
            i32.load offset=1059452
            local.get 0
            i32.add
            local.tee 0
            i32.store offset=1059452
            local.get 1
            local.get 0
            i32.const 1
            i32.or
            i32.store offset=4
            local.get 1
            i32.const 0
            i32.load offset=1059460
            i32.ne
            br_if 3 (;@1;)
            i32.const 0
            i32.const 0
            i32.store offset=1059448
            i32.const 0
            i32.const 0
            i32.store offset=1059460
            return
          end
          block  ;; label = @4
            i32.const 0
            i32.load offset=1059460
            local.get 3
            i32.ne
            br_if 0 (;@4;)
            i32.const 0
            local.get 1
            i32.store offset=1059460
            i32.const 0
            i32.const 0
            i32.load offset=1059448
            local.get 0
            i32.add
            local.tee 0
            i32.store offset=1059448
            local.get 1
            local.get 0
            i32.const 1
            i32.or
            i32.store offset=4
            local.get 1
            local.get 0
            i32.add
            local.get 0
            i32.store
            return
          end
          local.get 2
          i32.const -8
          i32.and
          local.get 0
          i32.add
          local.set 0
          block  ;; label = @4
            block  ;; label = @5
              local.get 2
              i32.const 255
              i32.gt_u
              br_if 0 (;@5;)
              local.get 3
              i32.load offset=8
              local.tee 4
              local.get 2
              i32.const 3
              i32.shr_u
              local.tee 5
              i32.const 3
              i32.shl
              i32.const 1059480
              i32.add
              local.tee 6
              i32.eq
              drop
              block  ;; label = @6
                local.get 3
                i32.load offset=12
                local.tee 2
                local.get 4
                i32.ne
                br_if 0 (;@6;)
                i32.const 0
                i32.const 0
                i32.load offset=1059440
                i32.const -2
                local.get 5
                i32.rotl
                i32.and
                i32.store offset=1059440
                br 2 (;@4;)
              end
              local.get 2
              local.get 6
              i32.eq
              drop
              local.get 2
              local.get 4
              i32.store offset=8
              local.get 4
              local.get 2
              i32.store offset=12
              br 1 (;@4;)
            end
            local.get 3
            i32.load offset=24
            local.set 7
            block  ;; label = @5
              block  ;; label = @6
                local.get 3
                i32.load offset=12
                local.tee 6
                local.get 3
                i32.eq
                br_if 0 (;@6;)
                i32.const 0
                i32.load offset=1059456
                local.get 3
                i32.load offset=8
                local.tee 2
                i32.gt_u
                drop
                local.get 6
                local.get 2
                i32.store offset=8
                local.get 2
                local.get 6
                i32.store offset=12
                br 1 (;@5;)
              end
              block  ;; label = @6
                local.get 3
                i32.const 20
                i32.add
                local.tee 2
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                local.get 3
                i32.const 16
                i32.add
                local.tee 2
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                i32.const 0
                local.set 6
                br 1 (;@5;)
              end
              loop  ;; label = @6
                local.get 2
                local.set 5
                local.get 4
                local.tee 6
                i32.const 20
                i32.add
                local.tee 2
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                local.get 6
                i32.const 16
                i32.add
                local.set 2
                local.get 6
                i32.load offset=16
                local.tee 4
                br_if 0 (;@6;)
              end
              local.get 5
              i32.const 0
              i32.store
            end
            local.get 7
            i32.eqz
            br_if 0 (;@4;)
            block  ;; label = @5
              block  ;; label = @6
                local.get 3
                i32.load offset=28
                local.tee 4
                i32.const 2
                i32.shl
                i32.const 1059744
                i32.add
                local.tee 2
                i32.load
                local.get 3
                i32.ne
                br_if 0 (;@6;)
                local.get 2
                local.get 6
                i32.store
                local.get 6
                br_if 1 (;@5;)
                i32.const 0
                i32.const 0
                i32.load offset=1059444
                i32.const -2
                local.get 4
                i32.rotl
                i32.and
                i32.store offset=1059444
                br 2 (;@4;)
              end
              local.get 7
              i32.const 16
              i32.const 20
              local.get 7
              i32.load offset=16
              local.get 3
              i32.eq
              select
              i32.add
              local.get 6
              i32.store
              local.get 6
              i32.eqz
              br_if 1 (;@4;)
            end
            local.get 6
            local.get 7
            i32.store offset=24
            block  ;; label = @5
              local.get 3
              i32.load offset=16
              local.tee 2
              i32.eqz
              br_if 0 (;@5;)
              local.get 6
              local.get 2
              i32.store offset=16
              local.get 2
              local.get 6
              i32.store offset=24
            end
            local.get 3
            i32.load offset=20
            local.tee 2
            i32.eqz
            br_if 0 (;@4;)
            local.get 6
            i32.const 20
            i32.add
            local.get 2
            i32.store
            local.get 2
            local.get 6
            i32.store offset=24
          end
          local.get 1
          local.get 0
          i32.add
          local.get 0
          i32.store
          local.get 1
          local.get 0
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 1
          i32.const 0
          i32.load offset=1059460
          i32.ne
          br_if 1 (;@2;)
          i32.const 0
          local.get 0
          i32.store offset=1059448
          return
        end
        local.get 3
        local.get 2
        i32.const -2
        i32.and
        i32.store offset=4
        local.get 1
        local.get 0
        i32.add
        local.get 0
        i32.store
        local.get 1
        local.get 0
        i32.const 1
        i32.or
        i32.store offset=4
      end
      block  ;; label = @2
        local.get 0
        i32.const 255
        i32.gt_u
        br_if 0 (;@2;)
        local.get 0
        i32.const 3
        i32.shr_u
        local.tee 2
        i32.const 3
        i32.shl
        i32.const 1059480
        i32.add
        local.set 0
        block  ;; label = @3
          block  ;; label = @4
            i32.const 0
            i32.load offset=1059440
            local.tee 4
            i32.const 1
            local.get 2
            i32.shl
            local.tee 2
            i32.and
            br_if 0 (;@4;)
            i32.const 0
            local.get 4
            local.get 2
            i32.or
            i32.store offset=1059440
            local.get 0
            local.set 2
            br 1 (;@3;)
          end
          local.get 0
          i32.load offset=8
          local.set 2
        end
        local.get 2
        local.get 1
        i32.store offset=12
        local.get 0
        local.get 1
        i32.store offset=8
        local.get 1
        local.get 0
        i32.store offset=12
        local.get 1
        local.get 2
        i32.store offset=8
        return
      end
      i32.const 31
      local.set 2
      block  ;; label = @2
        local.get 0
        i32.const 16777215
        i32.gt_u
        br_if 0 (;@2;)
        local.get 0
        i32.const 8
        i32.shr_u
        local.tee 2
        local.get 2
        i32.const 1048320
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 8
        i32.and
        local.tee 2
        i32.shl
        local.tee 4
        local.get 4
        i32.const 520192
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 4
        i32.and
        local.tee 4
        i32.shl
        local.tee 6
        local.get 6
        i32.const 245760
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 2
        i32.and
        local.tee 6
        i32.shl
        i32.const 15
        i32.shr_u
        local.get 2
        local.get 4
        i32.or
        local.get 6
        i32.or
        i32.sub
        local.tee 2
        i32.const 1
        i32.shl
        local.get 0
        local.get 2
        i32.const 21
        i32.add
        i32.shr_u
        i32.const 1
        i32.and
        i32.or
        i32.const 28
        i32.add
        local.set 2
      end
      local.get 1
      i64.const 0
      i64.store offset=16 align=4
      local.get 1
      i32.const 28
      i32.add
      local.get 2
      i32.store
      local.get 2
      i32.const 2
      i32.shl
      i32.const 1059744
      i32.add
      local.set 4
      block  ;; label = @2
        block  ;; label = @3
          i32.const 0
          i32.load offset=1059444
          local.tee 6
          i32.const 1
          local.get 2
          i32.shl
          local.tee 3
          i32.and
          br_if 0 (;@3;)
          local.get 4
          local.get 1
          i32.store
          i32.const 0
          local.get 6
          local.get 3
          i32.or
          i32.store offset=1059444
          local.get 1
          i32.const 24
          i32.add
          local.get 4
          i32.store
          local.get 1
          local.get 1
          i32.store offset=8
          local.get 1
          local.get 1
          i32.store offset=12
          br 1 (;@2;)
        end
        local.get 0
        i32.const 0
        i32.const 25
        local.get 2
        i32.const 1
        i32.shr_u
        i32.sub
        local.get 2
        i32.const 31
        i32.eq
        select
        i32.shl
        local.set 2
        local.get 4
        i32.load
        local.set 6
        block  ;; label = @3
          loop  ;; label = @4
            local.get 6
            local.tee 4
            i32.load offset=4
            i32.const -8
            i32.and
            local.get 0
            i32.eq
            br_if 1 (;@3;)
            local.get 2
            i32.const 29
            i32.shr_u
            local.set 6
            local.get 2
            i32.const 1
            i32.shl
            local.set 2
            local.get 4
            local.get 6
            i32.const 4
            i32.and
            i32.add
            i32.const 16
            i32.add
            local.tee 3
            i32.load
            local.tee 6
            br_if 0 (;@4;)
          end
          local.get 3
          local.get 1
          i32.store
          local.get 1
          i32.const 24
          i32.add
          local.get 4
          i32.store
          local.get 1
          local.get 1
          i32.store offset=12
          local.get 1
          local.get 1
          i32.store offset=8
          br 1 (;@2;)
        end
        local.get 4
        i32.load offset=8
        local.tee 0
        local.get 1
        i32.store offset=12
        local.get 4
        local.get 1
        i32.store offset=8
        local.get 1
        i32.const 24
        i32.add
        i32.const 0
        i32.store
        local.get 1
        local.get 4
        i32.store offset=12
        local.get 1
        local.get 0
        i32.store offset=8
      end
      i32.const 0
      i32.const 0
      i32.load offset=1059472
      i32.const -1
      i32.add
      local.tee 1
      i32.const -1
      local.get 1
      select
      i32.store offset=1059472
    end)
  (func $calloc (type 5) (param i32 i32) (result i32)
    (local i32 i64)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        br_if 0 (;@2;)
        i32.const 0
        local.set 2
        br 1 (;@1;)
      end
      local.get 0
      i64.extend_i32_u
      local.get 1
      i64.extend_i32_u
      i64.mul
      local.tee 3
      i32.wrap_i64
      local.set 2
      local.get 1
      local.get 0
      i32.or
      i32.const 65536
      i32.lt_u
      br_if 0 (;@1;)
      i32.const -1
      local.get 2
      local.get 3
      i64.const 32
      i64.shr_u
      i32.wrap_i64
      i32.const 0
      i32.ne
      select
      local.set 2
    end
    block  ;; label = @1
      local.get 2
      call $dlmalloc
      local.tee 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const -4
      i32.add
      i32.load8_u
      i32.const 3
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const 0
      local.get 2
      call $memset
      drop
    end
    local.get 0)
  (func $realloc (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      local.get 0
      br_if 0 (;@1;)
      local.get 1
      call $dlmalloc
      return
    end
    block  ;; label = @1
      local.get 1
      i32.const -64
      i32.lt_u
      br_if 0 (;@1;)
      i32.const 0
      i32.const 48
      i32.store offset=1059936
      i32.const 0
      return
    end
    i32.const 16
    local.get 1
    i32.const 19
    i32.add
    i32.const -16
    i32.and
    local.get 1
    i32.const 11
    i32.lt_u
    select
    local.set 2
    local.get 0
    i32.const -4
    i32.add
    local.tee 3
    i32.load
    local.tee 4
    i32.const -8
    i32.and
    local.set 5
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 4
          i32.const 3
          i32.and
          br_if 0 (;@3;)
          local.get 2
          i32.const 256
          i32.lt_u
          br_if 1 (;@2;)
          local.get 5
          local.get 2
          i32.const 4
          i32.or
          i32.lt_u
          br_if 1 (;@2;)
          local.get 5
          local.get 2
          i32.sub
          i32.const 0
          i32.load offset=1059920
          i32.const 1
          i32.shl
          i32.le_u
          br_if 2 (;@1;)
          br 1 (;@2;)
        end
        local.get 0
        i32.const -8
        i32.add
        local.tee 6
        local.get 5
        i32.add
        local.set 7
        block  ;; label = @3
          local.get 5
          local.get 2
          i32.lt_u
          br_if 0 (;@3;)
          local.get 5
          local.get 2
          i32.sub
          local.tee 1
          i32.const 16
          i32.lt_u
          br_if 2 (;@1;)
          local.get 3
          local.get 2
          local.get 4
          i32.const 1
          i32.and
          i32.or
          i32.const 2
          i32.or
          i32.store
          local.get 6
          local.get 2
          i32.add
          local.tee 2
          local.get 1
          i32.const 3
          i32.or
          i32.store offset=4
          local.get 7
          local.get 7
          i32.load offset=4
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 2
          local.get 1
          call $dispose_chunk
          local.get 0
          return
        end
        block  ;; label = @3
          i32.const 0
          i32.load offset=1059464
          local.get 7
          i32.ne
          br_if 0 (;@3;)
          i32.const 0
          i32.load offset=1059452
          local.get 5
          i32.add
          local.tee 5
          local.get 2
          i32.le_u
          br_if 1 (;@2;)
          local.get 3
          local.get 2
          local.get 4
          i32.const 1
          i32.and
          i32.or
          i32.const 2
          i32.or
          i32.store
          i32.const 0
          local.get 6
          local.get 2
          i32.add
          local.tee 1
          i32.store offset=1059464
          i32.const 0
          local.get 5
          local.get 2
          i32.sub
          local.tee 2
          i32.store offset=1059452
          local.get 1
          local.get 2
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 0
          return
        end
        block  ;; label = @3
          i32.const 0
          i32.load offset=1059460
          local.get 7
          i32.ne
          br_if 0 (;@3;)
          i32.const 0
          i32.load offset=1059448
          local.get 5
          i32.add
          local.tee 5
          local.get 2
          i32.lt_u
          br_if 1 (;@2;)
          block  ;; label = @4
            block  ;; label = @5
              local.get 5
              local.get 2
              i32.sub
              local.tee 1
              i32.const 16
              i32.lt_u
              br_if 0 (;@5;)
              local.get 3
              local.get 2
              local.get 4
              i32.const 1
              i32.and
              i32.or
              i32.const 2
              i32.or
              i32.store
              local.get 6
              local.get 2
              i32.add
              local.tee 2
              local.get 1
              i32.const 1
              i32.or
              i32.store offset=4
              local.get 6
              local.get 5
              i32.add
              local.tee 5
              local.get 1
              i32.store
              local.get 5
              local.get 5
              i32.load offset=4
              i32.const -2
              i32.and
              i32.store offset=4
              br 1 (;@4;)
            end
            local.get 3
            local.get 4
            i32.const 1
            i32.and
            local.get 5
            i32.or
            i32.const 2
            i32.or
            i32.store
            local.get 6
            local.get 5
            i32.add
            local.tee 1
            local.get 1
            i32.load offset=4
            i32.const 1
            i32.or
            i32.store offset=4
            i32.const 0
            local.set 1
            i32.const 0
            local.set 2
          end
          i32.const 0
          local.get 2
          i32.store offset=1059460
          i32.const 0
          local.get 1
          i32.store offset=1059448
          local.get 0
          return
        end
        local.get 7
        i32.load offset=4
        local.tee 8
        i32.const 2
        i32.and
        br_if 0 (;@2;)
        local.get 8
        i32.const -8
        i32.and
        local.get 5
        i32.add
        local.tee 9
        local.get 2
        i32.lt_u
        br_if 0 (;@2;)
        local.get 9
        local.get 2
        i32.sub
        local.set 10
        block  ;; label = @3
          block  ;; label = @4
            local.get 8
            i32.const 255
            i32.gt_u
            br_if 0 (;@4;)
            local.get 7
            i32.load offset=8
            local.tee 1
            local.get 8
            i32.const 3
            i32.shr_u
            local.tee 11
            i32.const 3
            i32.shl
            i32.const 1059480
            i32.add
            local.tee 8
            i32.eq
            drop
            block  ;; label = @5
              local.get 7
              i32.load offset=12
              local.tee 5
              local.get 1
              i32.ne
              br_if 0 (;@5;)
              i32.const 0
              i32.const 0
              i32.load offset=1059440
              i32.const -2
              local.get 11
              i32.rotl
              i32.and
              i32.store offset=1059440
              br 2 (;@3;)
            end
            local.get 5
            local.get 8
            i32.eq
            drop
            local.get 5
            local.get 1
            i32.store offset=8
            local.get 1
            local.get 5
            i32.store offset=12
            br 1 (;@3;)
          end
          local.get 7
          i32.load offset=24
          local.set 12
          block  ;; label = @4
            block  ;; label = @5
              local.get 7
              i32.load offset=12
              local.tee 8
              local.get 7
              i32.eq
              br_if 0 (;@5;)
              i32.const 0
              i32.load offset=1059456
              local.get 7
              i32.load offset=8
              local.tee 1
              i32.gt_u
              drop
              local.get 8
              local.get 1
              i32.store offset=8
              local.get 1
              local.get 8
              i32.store offset=12
              br 1 (;@4;)
            end
            block  ;; label = @5
              local.get 7
              i32.const 20
              i32.add
              local.tee 1
              i32.load
              local.tee 5
              br_if 0 (;@5;)
              local.get 7
              i32.const 16
              i32.add
              local.tee 1
              i32.load
              local.tee 5
              br_if 0 (;@5;)
              i32.const 0
              local.set 8
              br 1 (;@4;)
            end
            loop  ;; label = @5
              local.get 1
              local.set 11
              local.get 5
              local.tee 8
              i32.const 20
              i32.add
              local.tee 1
              i32.load
              local.tee 5
              br_if 0 (;@5;)
              local.get 8
              i32.const 16
              i32.add
              local.set 1
              local.get 8
              i32.load offset=16
              local.tee 5
              br_if 0 (;@5;)
            end
            local.get 11
            i32.const 0
            i32.store
          end
          local.get 12
          i32.eqz
          br_if 0 (;@3;)
          block  ;; label = @4
            block  ;; label = @5
              local.get 7
              i32.load offset=28
              local.tee 5
              i32.const 2
              i32.shl
              i32.const 1059744
              i32.add
              local.tee 1
              i32.load
              local.get 7
              i32.ne
              br_if 0 (;@5;)
              local.get 1
              local.get 8
              i32.store
              local.get 8
              br_if 1 (;@4;)
              i32.const 0
              i32.const 0
              i32.load offset=1059444
              i32.const -2
              local.get 5
              i32.rotl
              i32.and
              i32.store offset=1059444
              br 2 (;@3;)
            end
            local.get 12
            i32.const 16
            i32.const 20
            local.get 12
            i32.load offset=16
            local.get 7
            i32.eq
            select
            i32.add
            local.get 8
            i32.store
            local.get 8
            i32.eqz
            br_if 1 (;@3;)
          end
          local.get 8
          local.get 12
          i32.store offset=24
          block  ;; label = @4
            local.get 7
            i32.load offset=16
            local.tee 1
            i32.eqz
            br_if 0 (;@4;)
            local.get 8
            local.get 1
            i32.store offset=16
            local.get 1
            local.get 8
            i32.store offset=24
          end
          local.get 7
          i32.load offset=20
          local.tee 1
          i32.eqz
          br_if 0 (;@3;)
          local.get 8
          i32.const 20
          i32.add
          local.get 1
          i32.store
          local.get 1
          local.get 8
          i32.store offset=24
        end
        block  ;; label = @3
          local.get 10
          i32.const 15
          i32.gt_u
          br_if 0 (;@3;)
          local.get 3
          local.get 4
          i32.const 1
          i32.and
          local.get 9
          i32.or
          i32.const 2
          i32.or
          i32.store
          local.get 6
          local.get 9
          i32.add
          local.tee 1
          local.get 1
          i32.load offset=4
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 0
          return
        end
        local.get 3
        local.get 2
        local.get 4
        i32.const 1
        i32.and
        i32.or
        i32.const 2
        i32.or
        i32.store
        local.get 6
        local.get 2
        i32.add
        local.tee 1
        local.get 10
        i32.const 3
        i32.or
        i32.store offset=4
        local.get 6
        local.get 9
        i32.add
        local.tee 2
        local.get 2
        i32.load offset=4
        i32.const 1
        i32.or
        i32.store offset=4
        local.get 1
        local.get 10
        call $dispose_chunk
        local.get 0
        return
      end
      block  ;; label = @2
        local.get 1
        call $dlmalloc
        local.tee 2
        br_if 0 (;@2;)
        i32.const 0
        return
      end
      local.get 2
      local.get 0
      i32.const -4
      i32.const -8
      local.get 3
      i32.load
      local.tee 5
      i32.const 3
      i32.and
      select
      local.get 5
      i32.const -8
      i32.and
      i32.add
      local.tee 5
      local.get 1
      local.get 5
      local.get 1
      i32.lt_u
      select
      call $memcpy
      local.set 1
      local.get 0
      call $dlfree
      local.get 1
      local.set 0
    end
    local.get 0)
  (func $dispose_chunk (type 3) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32)
    local.get 0
    local.get 1
    i32.add
    local.set 2
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load offset=4
        local.tee 3
        i32.const 1
        i32.and
        br_if 0 (;@2;)
        local.get 3
        i32.const 3
        i32.and
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        i32.load
        local.tee 3
        local.get 1
        i32.add
        local.set 1
        block  ;; label = @3
          block  ;; label = @4
            i32.const 0
            i32.load offset=1059460
            local.get 0
            local.get 3
            i32.sub
            local.tee 0
            i32.eq
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 3
              i32.const 255
              i32.gt_u
              br_if 0 (;@5;)
              local.get 0
              i32.load offset=8
              local.tee 4
              local.get 3
              i32.const 3
              i32.shr_u
              local.tee 5
              i32.const 3
              i32.shl
              i32.const 1059480
              i32.add
              local.tee 6
              i32.eq
              drop
              local.get 0
              i32.load offset=12
              local.tee 3
              local.get 4
              i32.ne
              br_if 2 (;@3;)
              i32.const 0
              i32.const 0
              i32.load offset=1059440
              i32.const -2
              local.get 5
              i32.rotl
              i32.and
              i32.store offset=1059440
              br 3 (;@2;)
            end
            local.get 0
            i32.load offset=24
            local.set 7
            block  ;; label = @5
              block  ;; label = @6
                local.get 0
                i32.load offset=12
                local.tee 6
                local.get 0
                i32.eq
                br_if 0 (;@6;)
                i32.const 0
                i32.load offset=1059456
                local.get 0
                i32.load offset=8
                local.tee 3
                i32.gt_u
                drop
                local.get 6
                local.get 3
                i32.store offset=8
                local.get 3
                local.get 6
                i32.store offset=12
                br 1 (;@5;)
              end
              block  ;; label = @6
                local.get 0
                i32.const 20
                i32.add
                local.tee 3
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                local.get 0
                i32.const 16
                i32.add
                local.tee 3
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                i32.const 0
                local.set 6
                br 1 (;@5;)
              end
              loop  ;; label = @6
                local.get 3
                local.set 5
                local.get 4
                local.tee 6
                i32.const 20
                i32.add
                local.tee 3
                i32.load
                local.tee 4
                br_if 0 (;@6;)
                local.get 6
                i32.const 16
                i32.add
                local.set 3
                local.get 6
                i32.load offset=16
                local.tee 4
                br_if 0 (;@6;)
              end
              local.get 5
              i32.const 0
              i32.store
            end
            local.get 7
            i32.eqz
            br_if 2 (;@2;)
            block  ;; label = @5
              block  ;; label = @6
                local.get 0
                i32.load offset=28
                local.tee 4
                i32.const 2
                i32.shl
                i32.const 1059744
                i32.add
                local.tee 3
                i32.load
                local.get 0
                i32.ne
                br_if 0 (;@6;)
                local.get 3
                local.get 6
                i32.store
                local.get 6
                br_if 1 (;@5;)
                i32.const 0
                i32.const 0
                i32.load offset=1059444
                i32.const -2
                local.get 4
                i32.rotl
                i32.and
                i32.store offset=1059444
                br 4 (;@2;)
              end
              local.get 7
              i32.const 16
              i32.const 20
              local.get 7
              i32.load offset=16
              local.get 0
              i32.eq
              select
              i32.add
              local.get 6
              i32.store
              local.get 6
              i32.eqz
              br_if 3 (;@2;)
            end
            local.get 6
            local.get 7
            i32.store offset=24
            block  ;; label = @5
              local.get 0
              i32.load offset=16
              local.tee 3
              i32.eqz
              br_if 0 (;@5;)
              local.get 6
              local.get 3
              i32.store offset=16
              local.get 3
              local.get 6
              i32.store offset=24
            end
            local.get 0
            i32.load offset=20
            local.tee 3
            i32.eqz
            br_if 2 (;@2;)
            local.get 6
            i32.const 20
            i32.add
            local.get 3
            i32.store
            local.get 3
            local.get 6
            i32.store offset=24
            br 2 (;@2;)
          end
          local.get 2
          i32.load offset=4
          local.tee 3
          i32.const 3
          i32.and
          i32.const 3
          i32.ne
          br_if 1 (;@2;)
          local.get 2
          local.get 3
          i32.const -2
          i32.and
          i32.store offset=4
          i32.const 0
          local.get 1
          i32.store offset=1059448
          local.get 2
          local.get 1
          i32.store
          local.get 0
          local.get 1
          i32.const 1
          i32.or
          i32.store offset=4
          return
        end
        local.get 3
        local.get 6
        i32.eq
        drop
        local.get 3
        local.get 4
        i32.store offset=8
        local.get 4
        local.get 3
        i32.store offset=12
      end
      block  ;; label = @2
        block  ;; label = @3
          local.get 2
          i32.load offset=4
          local.tee 3
          i32.const 2
          i32.and
          br_if 0 (;@3;)
          block  ;; label = @4
            i32.const 0
            i32.load offset=1059464
            local.get 2
            i32.ne
            br_if 0 (;@4;)
            i32.const 0
            local.get 0
            i32.store offset=1059464
            i32.const 0
            i32.const 0
            i32.load offset=1059452
            local.get 1
            i32.add
            local.tee 1
            i32.store offset=1059452
            local.get 0
            local.get 1
            i32.const 1
            i32.or
            i32.store offset=4
            local.get 0
            i32.const 0
            i32.load offset=1059460
            i32.ne
            br_if 3 (;@1;)
            i32.const 0
            i32.const 0
            i32.store offset=1059448
            i32.const 0
            i32.const 0
            i32.store offset=1059460
            return
          end
          block  ;; label = @4
            i32.const 0
            i32.load offset=1059460
            local.get 2
            i32.ne
            br_if 0 (;@4;)
            i32.const 0
            local.get 0
            i32.store offset=1059460
            i32.const 0
            i32.const 0
            i32.load offset=1059448
            local.get 1
            i32.add
            local.tee 1
            i32.store offset=1059448
            local.get 0
            local.get 1
            i32.const 1
            i32.or
            i32.store offset=4
            local.get 0
            local.get 1
            i32.add
            local.get 1
            i32.store
            return
          end
          local.get 3
          i32.const -8
          i32.and
          local.get 1
          i32.add
          local.set 1
          block  ;; label = @4
            block  ;; label = @5
              local.get 3
              i32.const 255
              i32.gt_u
              br_if 0 (;@5;)
              local.get 2
              i32.load offset=8
              local.tee 4
              local.get 3
              i32.const 3
              i32.shr_u
              local.tee 5
              i32.const 3
              i32.shl
              i32.const 1059480
              i32.add
              local.tee 6
              i32.eq
              drop
              block  ;; label = @6
                local.get 2
                i32.load offset=12
                local.tee 3
                local.get 4
                i32.ne
                br_if 0 (;@6;)
                i32.const 0
                i32.const 0
                i32.load offset=1059440
                i32.const -2
                local.get 5
                i32.rotl
                i32.and
                i32.store offset=1059440
                br 2 (;@4;)
              end
              local.get 3
              local.get 6
              i32.eq
              drop
              local.get 3
              local.get 4
              i32.store offset=8
              local.get 4
              local.get 3
              i32.store offset=12
              br 1 (;@4;)
            end
            local.get 2
            i32.load offset=24
            local.set 7
            block  ;; label = @5
              block  ;; label = @6
                local.get 2
                i32.load offset=12
                local.tee 6
                local.get 2
                i32.eq
                br_if 0 (;@6;)
                i32.const 0
                i32.load offset=1059456
                local.get 2
                i32.load offset=8
                local.tee 3
                i32.gt_u
                drop
                local.get 6
                local.get 3
                i32.store offset=8
                local.get 3
                local.get 6
                i32.store offset=12
                br 1 (;@5;)
              end
              block  ;; label = @6
                local.get 2
                i32.const 20
                i32.add
                local.tee 4
                i32.load
                local.tee 3
                br_if 0 (;@6;)
                local.get 2
                i32.const 16
                i32.add
                local.tee 4
                i32.load
                local.tee 3
                br_if 0 (;@6;)
                i32.const 0
                local.set 6
                br 1 (;@5;)
              end
              loop  ;; label = @6
                local.get 4
                local.set 5
                local.get 3
                local.tee 6
                i32.const 20
                i32.add
                local.tee 4
                i32.load
                local.tee 3
                br_if 0 (;@6;)
                local.get 6
                i32.const 16
                i32.add
                local.set 4
                local.get 6
                i32.load offset=16
                local.tee 3
                br_if 0 (;@6;)
              end
              local.get 5
              i32.const 0
              i32.store
            end
            local.get 7
            i32.eqz
            br_if 0 (;@4;)
            block  ;; label = @5
              block  ;; label = @6
                local.get 2
                i32.load offset=28
                local.tee 4
                i32.const 2
                i32.shl
                i32.const 1059744
                i32.add
                local.tee 3
                i32.load
                local.get 2
                i32.ne
                br_if 0 (;@6;)
                local.get 3
                local.get 6
                i32.store
                local.get 6
                br_if 1 (;@5;)
                i32.const 0
                i32.const 0
                i32.load offset=1059444
                i32.const -2
                local.get 4
                i32.rotl
                i32.and
                i32.store offset=1059444
                br 2 (;@4;)
              end
              local.get 7
              i32.const 16
              i32.const 20
              local.get 7
              i32.load offset=16
              local.get 2
              i32.eq
              select
              i32.add
              local.get 6
              i32.store
              local.get 6
              i32.eqz
              br_if 1 (;@4;)
            end
            local.get 6
            local.get 7
            i32.store offset=24
            block  ;; label = @5
              local.get 2
              i32.load offset=16
              local.tee 3
              i32.eqz
              br_if 0 (;@5;)
              local.get 6
              local.get 3
              i32.store offset=16
              local.get 3
              local.get 6
              i32.store offset=24
            end
            local.get 2
            i32.load offset=20
            local.tee 3
            i32.eqz
            br_if 0 (;@4;)
            local.get 6
            i32.const 20
            i32.add
            local.get 3
            i32.store
            local.get 3
            local.get 6
            i32.store offset=24
          end
          local.get 0
          local.get 1
          i32.add
          local.get 1
          i32.store
          local.get 0
          local.get 1
          i32.const 1
          i32.or
          i32.store offset=4
          local.get 0
          i32.const 0
          i32.load offset=1059460
          i32.ne
          br_if 1 (;@2;)
          i32.const 0
          local.get 1
          i32.store offset=1059448
          return
        end
        local.get 2
        local.get 3
        i32.const -2
        i32.and
        i32.store offset=4
        local.get 0
        local.get 1
        i32.add
        local.get 1
        i32.store
        local.get 0
        local.get 1
        i32.const 1
        i32.or
        i32.store offset=4
      end
      block  ;; label = @2
        local.get 1
        i32.const 255
        i32.gt_u
        br_if 0 (;@2;)
        local.get 1
        i32.const 3
        i32.shr_u
        local.tee 3
        i32.const 3
        i32.shl
        i32.const 1059480
        i32.add
        local.set 1
        block  ;; label = @3
          block  ;; label = @4
            i32.const 0
            i32.load offset=1059440
            local.tee 4
            i32.const 1
            local.get 3
            i32.shl
            local.tee 3
            i32.and
            br_if 0 (;@4;)
            i32.const 0
            local.get 4
            local.get 3
            i32.or
            i32.store offset=1059440
            local.get 1
            local.set 3
            br 1 (;@3;)
          end
          local.get 1
          i32.load offset=8
          local.set 3
        end
        local.get 3
        local.get 0
        i32.store offset=12
        local.get 1
        local.get 0
        i32.store offset=8
        local.get 0
        local.get 1
        i32.store offset=12
        local.get 0
        local.get 3
        i32.store offset=8
        return
      end
      i32.const 31
      local.set 3
      block  ;; label = @2
        local.get 1
        i32.const 16777215
        i32.gt_u
        br_if 0 (;@2;)
        local.get 1
        i32.const 8
        i32.shr_u
        local.tee 3
        local.get 3
        i32.const 1048320
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 8
        i32.and
        local.tee 3
        i32.shl
        local.tee 4
        local.get 4
        i32.const 520192
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 4
        i32.and
        local.tee 4
        i32.shl
        local.tee 6
        local.get 6
        i32.const 245760
        i32.add
        i32.const 16
        i32.shr_u
        i32.const 2
        i32.and
        local.tee 6
        i32.shl
        i32.const 15
        i32.shr_u
        local.get 3
        local.get 4
        i32.or
        local.get 6
        i32.or
        i32.sub
        local.tee 3
        i32.const 1
        i32.shl
        local.get 1
        local.get 3
        i32.const 21
        i32.add
        i32.shr_u
        i32.const 1
        i32.and
        i32.or
        i32.const 28
        i32.add
        local.set 3
      end
      local.get 0
      i64.const 0
      i64.store offset=16 align=4
      local.get 0
      i32.const 28
      i32.add
      local.get 3
      i32.store
      local.get 3
      i32.const 2
      i32.shl
      i32.const 1059744
      i32.add
      local.set 4
      block  ;; label = @2
        i32.const 0
        i32.load offset=1059444
        local.tee 6
        i32.const 1
        local.get 3
        i32.shl
        local.tee 2
        i32.and
        br_if 0 (;@2;)
        local.get 4
        local.get 0
        i32.store
        i32.const 0
        local.get 6
        local.get 2
        i32.or
        i32.store offset=1059444
        local.get 0
        i32.const 24
        i32.add
        local.get 4
        i32.store
        local.get 0
        local.get 0
        i32.store offset=8
        local.get 0
        local.get 0
        i32.store offset=12
        return
      end
      local.get 1
      i32.const 0
      i32.const 25
      local.get 3
      i32.const 1
      i32.shr_u
      i32.sub
      local.get 3
      i32.const 31
      i32.eq
      select
      i32.shl
      local.set 3
      local.get 4
      i32.load
      local.set 6
      block  ;; label = @2
        loop  ;; label = @3
          local.get 6
          local.tee 4
          i32.load offset=4
          i32.const -8
          i32.and
          local.get 1
          i32.eq
          br_if 1 (;@2;)
          local.get 3
          i32.const 29
          i32.shr_u
          local.set 6
          local.get 3
          i32.const 1
          i32.shl
          local.set 3
          local.get 4
          local.get 6
          i32.const 4
          i32.and
          i32.add
          i32.const 16
          i32.add
          local.tee 2
          i32.load
          local.tee 6
          br_if 0 (;@3;)
        end
        local.get 2
        local.get 0
        i32.store
        local.get 0
        i32.const 24
        i32.add
        local.get 4
        i32.store
        local.get 0
        local.get 0
        i32.store offset=12
        local.get 0
        local.get 0
        i32.store offset=8
        return
      end
      local.get 4
      i32.load offset=8
      local.tee 1
      local.get 0
      i32.store offset=12
      local.get 4
      local.get 0
      i32.store offset=8
      local.get 0
      i32.const 24
      i32.add
      i32.const 0
      i32.store
      local.get 0
      local.get 4
      i32.store offset=12
      local.get 0
      local.get 1
      i32.store offset=8
    end)
  (func $internal_memalign (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.const 16
        local.get 0
        i32.const 16
        i32.gt_u
        select
        local.tee 2
        local.get 2
        i32.const -1
        i32.add
        i32.and
        br_if 0 (;@2;)
        local.get 2
        local.set 0
        br 1 (;@1;)
      end
      i32.const 32
      local.set 3
      loop  ;; label = @2
        local.get 3
        local.tee 0
        i32.const 1
        i32.shl
        local.set 3
        local.get 0
        local.get 2
        i32.lt_u
        br_if 0 (;@2;)
      end
    end
    block  ;; label = @1
      i32.const -64
      local.get 0
      i32.sub
      local.get 1
      i32.gt_u
      br_if 0 (;@1;)
      i32.const 0
      i32.const 48
      i32.store offset=1059936
      i32.const 0
      return
    end
    block  ;; label = @1
      local.get 0
      i32.const 16
      local.get 1
      i32.const 19
      i32.add
      i32.const -16
      i32.and
      local.get 1
      i32.const 11
      i32.lt_u
      select
      local.tee 1
      i32.add
      i32.const 12
      i32.add
      call $dlmalloc
      local.tee 3
      br_if 0 (;@1;)
      i32.const 0
      return
    end
    local.get 3
    i32.const -8
    i32.add
    local.set 2
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.const -1
        i32.add
        local.get 3
        i32.and
        br_if 0 (;@2;)
        local.get 2
        local.set 0
        br 1 (;@1;)
      end
      local.get 3
      i32.const -4
      i32.add
      local.tee 4
      i32.load
      local.tee 5
      i32.const -8
      i32.and
      local.get 3
      local.get 0
      i32.add
      i32.const -1
      i32.add
      i32.const 0
      local.get 0
      i32.sub
      i32.and
      i32.const -8
      i32.add
      local.tee 3
      i32.const 0
      local.get 0
      local.get 3
      local.get 2
      i32.sub
      i32.const 15
      i32.gt_u
      select
      i32.add
      local.tee 0
      local.get 2
      i32.sub
      local.tee 3
      i32.sub
      local.set 6
      block  ;; label = @2
        local.get 5
        i32.const 3
        i32.and
        br_if 0 (;@2;)
        local.get 0
        local.get 6
        i32.store offset=4
        local.get 0
        local.get 2
        i32.load
        local.get 3
        i32.add
        i32.store
        br 1 (;@1;)
      end
      local.get 0
      local.get 6
      local.get 0
      i32.load offset=4
      i32.const 1
      i32.and
      i32.or
      i32.const 2
      i32.or
      i32.store offset=4
      local.get 0
      local.get 6
      i32.add
      local.tee 6
      local.get 6
      i32.load offset=4
      i32.const 1
      i32.or
      i32.store offset=4
      local.get 4
      local.get 3
      local.get 4
      i32.load
      i32.const 1
      i32.and
      i32.or
      i32.const 2
      i32.or
      i32.store
      local.get 2
      local.get 3
      i32.add
      local.tee 6
      local.get 6
      i32.load offset=4
      i32.const 1
      i32.or
      i32.store offset=4
      local.get 2
      local.get 3
      call $dispose_chunk
    end
    block  ;; label = @1
      local.get 0
      i32.load offset=4
      local.tee 3
      i32.const 3
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      local.get 3
      i32.const -8
      i32.and
      local.tee 2
      local.get 1
      i32.const 16
      i32.add
      i32.le_u
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      local.get 3
      i32.const 1
      i32.and
      i32.or
      i32.const 2
      i32.or
      i32.store offset=4
      local.get 0
      local.get 1
      i32.add
      local.tee 3
      local.get 2
      local.get 1
      i32.sub
      local.tee 1
      i32.const 3
      i32.or
      i32.store offset=4
      local.get 0
      local.get 2
      i32.add
      local.tee 2
      local.get 2
      i32.load offset=4
      i32.const 1
      i32.or
      i32.store offset=4
      local.get 3
      local.get 1
      call $dispose_chunk
    end
    local.get 0
    i32.const 8
    i32.add)
  (func $aligned_alloc (type 5) (param i32 i32) (result i32)
    block  ;; label = @1
      local.get 0
      i32.const 16
      i32.gt_u
      br_if 0 (;@1;)
      local.get 1
      call $dlmalloc
      return
    end
    local.get 0
    local.get 1
    call $internal_memalign)
  (func $_Exit (type 1) (param i32)
    local.get 0
    call $__wasi_proc_exit
    unreachable)
  (func $sbrk (type 4) (param i32) (result i32)
    block  ;; label = @1
      local.get 0
      br_if 0 (;@1;)
      memory.size
      i32.const 16
      i32.shl
      return
    end
    block  ;; label = @1
      local.get 0
      i32.const 65535
      i32.and
      br_if 0 (;@1;)
      local.get 0
      i32.const -1
      i32.le_s
      br_if 0 (;@1;)
      block  ;; label = @2
        local.get 0
        i32.const 16
        i32.shr_u
        memory.grow
        local.tee 0
        i32.const -1
        i32.ne
        br_if 0 (;@2;)
        i32.const 0
        i32.const 48
        i32.store offset=1059936
        i32.const -1
        return
      end
      local.get 0
      i32.const 16
      i32.shl
      return
    end
    call $abort
    unreachable)
  (func $__wasi_environ_get (type 5) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_environ_get
    i32.const 65535
    i32.and)
  (func $__wasi_environ_sizes_get (type 5) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__imported_wasi_snapshot_preview1_environ_sizes_get
    i32.const 65535
    i32.and)
  (func $__wasi_proc_exit (type 1) (param i32)
    local.get 0
    call $__imported_wasi_snapshot_preview1_proc_exit
    unreachable)
  (func $getcwd (type 5) (param i32 i32) (result i32)
    (local i32)
    i32.const 0
    i32.load offset=1059320
    local.set 2
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        br_if 0 (;@2;)
        local.get 2
        call $strdup
        local.tee 0
        br_if 1 (;@1;)
        i32.const 0
        i32.const 48
        i32.store offset=1059936
        i32.const 0
        return
      end
      block  ;; label = @2
        local.get 2
        call $strlen
        i32.const 1
        i32.add
        local.get 1
        i32.gt_u
        br_if 0 (;@2;)
        local.get 0
        local.get 2
        call $strcpy
        return
      end
      i32.const 0
      local.set 0
      i32.const 0
      i32.const 68
      i32.store offset=1059936
    end
    local.get 0)
  (func $__wasilibc_initialize_environ_eagerly (type 0)
    call $__wasilibc_initialize_environ)
  (func $__wasilibc_ensure_environ (type 0)
    block  ;; label = @1
      i32.const 0
      i32.load offset=1059940
      i32.const -1
      i32.ne
      br_if 0 (;@1;)
      call $__wasilibc_initialize_environ
    end)
  (func $__wasilibc_initialize_environ (type 0)
    (local i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 0
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          i32.const 12
          i32.add
          local.get 0
          i32.const 8
          i32.add
          call $__wasi_environ_sizes_get
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 0
            i32.load offset=12
            local.tee 1
            br_if 0 (;@4;)
            i32.const 0
            i32.const 1059944
            i32.store offset=1059940
            br 3 (;@1;)
          end
          block  ;; label = @4
            block  ;; label = @5
              local.get 1
              i32.const 1
              i32.add
              local.tee 2
              local.get 1
              i32.lt_u
              br_if 0 (;@5;)
              local.get 0
              i32.load offset=8
              call $malloc
              local.tee 3
              i32.eqz
              br_if 0 (;@5;)
              local.get 2
              i32.const 4
              call $calloc
              local.tee 1
              br_if 1 (;@4;)
              local.get 3
              call $free
            end
            i32.const 70
            call $_Exit
            unreachable
          end
          local.get 1
          local.get 3
          call $__wasi_environ_get
          i32.eqz
          br_if 1 (;@2;)
          local.get 3
          call $free
          local.get 1
          call $free
        end
        i32.const 71
        call $_Exit
        unreachable
      end
      i32.const 0
      local.get 1
      i32.store offset=1059940
    end
    local.get 0
    i32.const 16
    i32.add
    global.set $__stack_pointer)
  (func $abort (type 0)
    unreachable
    unreachable)
  (func $dummy (type 0))
  (func $__wasm_call_dtors (type 0)
    call $dummy
    call $dummy)
  (func $exit (type 1) (param i32)
    call $dummy
    call $dummy
    local.get 0
    call $_Exit
    unreachable)
  (func $getenv (type 4) (param i32) (result i32)
    (local i32 i32 i32 i32)
    call $__wasilibc_ensure_environ
    block  ;; label = @1
      local.get 0
      i32.const 61
      call $__strchrnul
      local.get 0
      i32.sub
      local.tee 1
      br_if 0 (;@1;)
      i32.const 0
      return
    end
    i32.const 0
    local.set 2
    block  ;; label = @1
      local.get 0
      local.get 1
      i32.add
      i32.load8_u
      br_if 0 (;@1;)
      i32.const 0
      i32.load offset=1059940
      local.tee 3
      i32.eqz
      br_if 0 (;@1;)
      local.get 3
      i32.load
      local.tee 4
      i32.eqz
      br_if 0 (;@1;)
      local.get 3
      i32.const 4
      i32.add
      local.set 3
      block  ;; label = @2
        loop  ;; label = @3
          block  ;; label = @4
            local.get 0
            local.get 4
            local.get 1
            call $strncmp
            br_if 0 (;@4;)
            local.get 4
            local.get 1
            i32.add
            local.tee 4
            i32.load8_u
            i32.const 61
            i32.eq
            br_if 2 (;@2;)
          end
          local.get 3
          i32.load
          local.set 4
          local.get 3
          i32.const 4
          i32.add
          local.set 3
          local.get 4
          br_if 0 (;@3;)
          br 2 (;@1;)
        end
      end
      local.get 4
      i32.const 1
      i32.add
      local.set 2
    end
    local.get 2)
  (func $memcmp (type 7) (param i32 i32 i32) (result i32)
    (local i32 i32 i32)
    i32.const 0
    local.set 3
    block  ;; label = @1
      local.get 2
      i32.eqz
      br_if 0 (;@1;)
      block  ;; label = @2
        loop  ;; label = @3
          local.get 0
          i32.load8_u
          local.tee 4
          local.get 1
          i32.load8_u
          local.tee 5
          i32.ne
          br_if 1 (;@2;)
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 0
          i32.const 1
          i32.add
          local.set 0
          local.get 2
          i32.const -1
          i32.add
          local.tee 2
          br_if 0 (;@3;)
          br 2 (;@1;)
        end
      end
      local.get 4
      local.get 5
      i32.sub
      local.set 3
    end
    local.get 3)
  (func $strdup (type 4) (param i32) (result i32)
    (local i32 i32)
    block  ;; label = @1
      local.get 0
      call $strlen
      i32.const 1
      i32.add
      local.tee 1
      call $malloc
      local.tee 2
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      local.get 0
      local.get 1
      call $memcpy
      drop
    end
    local.get 2)
  (func $memmove (type 7) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32)
    block  ;; label = @1
      local.get 0
      local.get 1
      i32.eq
      br_if 0 (;@1;)
      block  ;; label = @2
        local.get 1
        local.get 0
        local.get 2
        i32.add
        local.tee 3
        i32.sub
        i32.const 0
        local.get 2
        i32.const 1
        i32.shl
        i32.sub
        i32.gt_u
        br_if 0 (;@2;)
        local.get 0
        local.get 1
        local.get 2
        call $memcpy
        drop
        br 1 (;@1;)
      end
      local.get 1
      local.get 0
      i32.xor
      i32.const 3
      i32.and
      local.set 4
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            local.get 1
            i32.ge_u
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 4
              i32.eqz
              br_if 0 (;@5;)
              local.get 2
              local.set 4
              local.get 0
              local.set 3
              br 3 (;@2;)
            end
            block  ;; label = @5
              local.get 0
              i32.const 3
              i32.and
              br_if 0 (;@5;)
              local.get 2
              local.set 4
              local.get 0
              local.set 3
              br 2 (;@3;)
            end
            local.get 2
            i32.eqz
            br_if 3 (;@1;)
            local.get 0
            local.get 1
            i32.load8_u
            i32.store8
            local.get 2
            i32.const -1
            i32.add
            local.set 4
            block  ;; label = @5
              local.get 0
              i32.const 1
              i32.add
              local.tee 3
              i32.const 3
              i32.and
              br_if 0 (;@5;)
              local.get 1
              i32.const 1
              i32.add
              local.set 1
              br 2 (;@3;)
            end
            local.get 4
            i32.eqz
            br_if 3 (;@1;)
            local.get 0
            local.get 1
            i32.load8_u offset=1
            i32.store8 offset=1
            local.get 2
            i32.const -2
            i32.add
            local.set 4
            block  ;; label = @5
              local.get 0
              i32.const 2
              i32.add
              local.tee 3
              i32.const 3
              i32.and
              br_if 0 (;@5;)
              local.get 1
              i32.const 2
              i32.add
              local.set 1
              br 2 (;@3;)
            end
            local.get 4
            i32.eqz
            br_if 3 (;@1;)
            local.get 0
            local.get 1
            i32.load8_u offset=2
            i32.store8 offset=2
            local.get 2
            i32.const -3
            i32.add
            local.set 4
            block  ;; label = @5
              local.get 0
              i32.const 3
              i32.add
              local.tee 3
              i32.const 3
              i32.and
              br_if 0 (;@5;)
              local.get 1
              i32.const 3
              i32.add
              local.set 1
              br 2 (;@3;)
            end
            local.get 4
            i32.eqz
            br_if 3 (;@1;)
            local.get 0
            local.get 1
            i32.load8_u offset=3
            i32.store8 offset=3
            local.get 0
            i32.const 4
            i32.add
            local.set 3
            local.get 1
            i32.const 4
            i32.add
            local.set 1
            local.get 2
            i32.const -4
            i32.add
            local.set 4
            br 1 (;@3;)
          end
          block  ;; label = @4
            local.get 4
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 3
              i32.const 3
              i32.and
              i32.eqz
              br_if 0 (;@5;)
              local.get 2
              i32.eqz
              br_if 4 (;@1;)
              local.get 0
              local.get 2
              i32.const -1
              i32.add
              local.tee 3
              i32.add
              local.tee 4
              local.get 1
              local.get 3
              i32.add
              i32.load8_u
              i32.store8
              block  ;; label = @6
                local.get 4
                i32.const 3
                i32.and
                br_if 0 (;@6;)
                local.get 3
                local.set 2
                br 1 (;@5;)
              end
              local.get 3
              i32.eqz
              br_if 4 (;@1;)
              local.get 0
              local.get 2
              i32.const -2
              i32.add
              local.tee 3
              i32.add
              local.tee 4
              local.get 1
              local.get 3
              i32.add
              i32.load8_u
              i32.store8
              block  ;; label = @6
                local.get 4
                i32.const 3
                i32.and
                br_if 0 (;@6;)
                local.get 3
                local.set 2
                br 1 (;@5;)
              end
              local.get 3
              i32.eqz
              br_if 4 (;@1;)
              local.get 0
              local.get 2
              i32.const -3
              i32.add
              local.tee 3
              i32.add
              local.tee 4
              local.get 1
              local.get 3
              i32.add
              i32.load8_u
              i32.store8
              block  ;; label = @6
                local.get 4
                i32.const 3
                i32.and
                br_if 0 (;@6;)
                local.get 3
                local.set 2
                br 1 (;@5;)
              end
              local.get 3
              i32.eqz
              br_if 4 (;@1;)
              local.get 0
              local.get 2
              i32.const -4
              i32.add
              local.tee 2
              i32.add
              local.get 1
              local.get 2
              i32.add
              i32.load8_u
              i32.store8
            end
            local.get 2
            i32.const 4
            i32.lt_u
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 2
              i32.const -4
              i32.add
              local.tee 5
              i32.const 2
              i32.shr_u
              i32.const 1
              i32.add
              i32.const 3
              i32.and
              local.tee 3
              i32.eqz
              br_if 0 (;@5;)
              local.get 1
              i32.const -4
              i32.add
              local.set 4
              local.get 0
              i32.const -4
              i32.add
              local.set 6
              loop  ;; label = @6
                local.get 6
                local.get 2
                i32.add
                local.get 4
                local.get 2
                i32.add
                i32.load
                i32.store
                local.get 2
                i32.const -4
                i32.add
                local.set 2
                local.get 3
                i32.const -1
                i32.add
                local.tee 3
                br_if 0 (;@6;)
              end
            end
            local.get 5
            i32.const 12
            i32.lt_u
            br_if 0 (;@4;)
            local.get 1
            i32.const -16
            i32.add
            local.set 6
            local.get 0
            i32.const -16
            i32.add
            local.set 5
            loop  ;; label = @5
              local.get 5
              local.get 2
              i32.add
              local.tee 3
              i32.const 12
              i32.add
              local.get 6
              local.get 2
              i32.add
              local.tee 4
              i32.const 12
              i32.add
              i32.load
              i32.store
              local.get 3
              i32.const 8
              i32.add
              local.get 4
              i32.const 8
              i32.add
              i32.load
              i32.store
              local.get 3
              i32.const 4
              i32.add
              local.get 4
              i32.const 4
              i32.add
              i32.load
              i32.store
              local.get 3
              local.get 4
              i32.load
              i32.store
              local.get 2
              i32.const -16
              i32.add
              local.tee 2
              i32.const 3
              i32.gt_u
              br_if 0 (;@5;)
            end
          end
          local.get 2
          i32.eqz
          br_if 2 (;@1;)
          local.get 2
          i32.const -1
          i32.add
          local.set 5
          block  ;; label = @4
            local.get 2
            i32.const 3
            i32.and
            local.tee 3
            i32.eqz
            br_if 0 (;@4;)
            local.get 1
            i32.const -1
            i32.add
            local.set 4
            local.get 0
            i32.const -1
            i32.add
            local.set 6
            loop  ;; label = @5
              local.get 6
              local.get 2
              i32.add
              local.get 4
              local.get 2
              i32.add
              i32.load8_u
              i32.store8
              local.get 2
              i32.const -1
              i32.add
              local.set 2
              local.get 3
              i32.const -1
              i32.add
              local.tee 3
              br_if 0 (;@5;)
            end
          end
          local.get 5
          i32.const 3
          i32.lt_u
          br_if 2 (;@1;)
          local.get 1
          i32.const -4
          i32.add
          local.set 4
          local.get 0
          i32.const -4
          i32.add
          local.set 6
          loop  ;; label = @4
            local.get 6
            local.get 2
            i32.add
            local.tee 1
            i32.const 3
            i32.add
            local.get 4
            local.get 2
            i32.add
            local.tee 3
            i32.const 3
            i32.add
            i32.load8_u
            i32.store8
            local.get 1
            i32.const 2
            i32.add
            local.get 3
            i32.const 2
            i32.add
            i32.load8_u
            i32.store8
            local.get 1
            i32.const 1
            i32.add
            local.get 3
            i32.const 1
            i32.add
            i32.load8_u
            i32.store8
            local.get 1
            local.get 3
            i32.load8_u
            i32.store8
            local.get 2
            i32.const -4
            i32.add
            local.tee 2
            br_if 0 (;@4;)
            br 3 (;@1;)
          end
        end
        local.get 4
        i32.const 4
        i32.lt_u
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 4
          i32.const -4
          i32.add
          local.tee 6
          i32.const 2
          i32.shr_u
          i32.const 1
          i32.add
          i32.const 7
          i32.and
          local.tee 2
          i32.eqz
          br_if 0 (;@3;)
          loop  ;; label = @4
            local.get 3
            local.get 1
            i32.load
            i32.store
            local.get 1
            i32.const 4
            i32.add
            local.set 1
            local.get 3
            i32.const 4
            i32.add
            local.set 3
            local.get 4
            i32.const -4
            i32.add
            local.set 4
            local.get 2
            i32.const -1
            i32.add
            local.tee 2
            br_if 0 (;@4;)
          end
        end
        local.get 6
        i32.const 28
        i32.lt_u
        br_if 0 (;@2;)
        loop  ;; label = @3
          local.get 3
          local.get 1
          i32.load
          i32.store
          local.get 3
          i32.const 4
          i32.add
          local.get 1
          i32.const 4
          i32.add
          i32.load
          i32.store
          local.get 3
          i32.const 8
          i32.add
          local.get 1
          i32.const 8
          i32.add
          i32.load
          i32.store
          local.get 3
          i32.const 12
          i32.add
          local.get 1
          i32.const 12
          i32.add
          i32.load
          i32.store
          local.get 3
          i32.const 16
          i32.add
          local.get 1
          i32.const 16
          i32.add
          i32.load
          i32.store
          local.get 3
          i32.const 20
          i32.add
          local.get 1
          i32.const 20
          i32.add
          i32.load
          i32.store
          local.get 3
          i32.const 24
          i32.add
          local.get 1
          i32.const 24
          i32.add
          i32.load
          i32.store
          local.get 3
          i32.const 28
          i32.add
          local.get 1
          i32.const 28
          i32.add
          i32.load
          i32.store
          local.get 3
          i32.const 32
          i32.add
          local.set 3
          local.get 1
          i32.const 32
          i32.add
          local.set 1
          local.get 4
          i32.const -32
          i32.add
          local.tee 4
          i32.const 3
          i32.gt_u
          br_if 0 (;@3;)
        end
      end
      local.get 4
      i32.eqz
      br_if 0 (;@1;)
      local.get 4
      i32.const -1
      i32.add
      local.set 6
      block  ;; label = @2
        local.get 4
        i32.const 7
        i32.and
        local.tee 2
        i32.eqz
        br_if 0 (;@2;)
        loop  ;; label = @3
          local.get 3
          local.get 1
          i32.load8_u
          i32.store8
          local.get 4
          i32.const -1
          i32.add
          local.set 4
          local.get 3
          i32.const 1
          i32.add
          local.set 3
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 2
          i32.const -1
          i32.add
          local.tee 2
          br_if 0 (;@3;)
        end
      end
      local.get 6
      i32.const 7
      i32.lt_u
      br_if 0 (;@1;)
      loop  ;; label = @2
        local.get 3
        local.get 1
        i32.load8_u
        i32.store8
        local.get 3
        i32.const 1
        i32.add
        local.get 1
        i32.const 1
        i32.add
        i32.load8_u
        i32.store8
        local.get 3
        i32.const 2
        i32.add
        local.get 1
        i32.const 2
        i32.add
        i32.load8_u
        i32.store8
        local.get 3
        i32.const 3
        i32.add
        local.get 1
        i32.const 3
        i32.add
        i32.load8_u
        i32.store8
        local.get 3
        i32.const 4
        i32.add
        local.get 1
        i32.const 4
        i32.add
        i32.load8_u
        i32.store8
        local.get 3
        i32.const 5
        i32.add
        local.get 1
        i32.const 5
        i32.add
        i32.load8_u
        i32.store8
        local.get 3
        i32.const 6
        i32.add
        local.get 1
        i32.const 6
        i32.add
        i32.load8_u
        i32.store8
        local.get 3
        i32.const 7
        i32.add
        local.get 1
        i32.const 7
        i32.add
        i32.load8_u
        i32.store8
        local.get 3
        i32.const 8
        i32.add
        local.set 3
        local.get 1
        i32.const 8
        i32.add
        local.set 1
        local.get 4
        i32.const -8
        i32.add
        local.tee 4
        br_if 0 (;@2;)
      end
    end
    local.get 0)
  (func $__stpcpy (type 5) (param i32 i32) (result i32)
    (local i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 1
          local.get 0
          i32.xor
          i32.const 3
          i32.and
          i32.eqz
          br_if 0 (;@3;)
          local.get 0
          local.set 2
          br 1 (;@2;)
        end
        block  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.const 3
            i32.and
            br_if 0 (;@4;)
            local.get 0
            local.set 2
            br 1 (;@3;)
          end
          local.get 0
          local.get 1
          i32.load8_u
          local.tee 2
          i32.store8
          block  ;; label = @4
            local.get 2
            br_if 0 (;@4;)
            local.get 0
            return
          end
          local.get 0
          i32.const 1
          i32.add
          local.set 2
          block  ;; label = @4
            local.get 1
            i32.const 1
            i32.add
            local.tee 3
            i32.const 3
            i32.and
            br_if 0 (;@4;)
            local.get 3
            local.set 1
            br 1 (;@3;)
          end
          local.get 2
          local.get 3
          i32.load8_u
          local.tee 3
          i32.store8
          local.get 3
          i32.eqz
          br_if 2 (;@1;)
          local.get 0
          i32.const 2
          i32.add
          local.set 2
          block  ;; label = @4
            local.get 1
            i32.const 2
            i32.add
            local.tee 3
            i32.const 3
            i32.and
            br_if 0 (;@4;)
            local.get 3
            local.set 1
            br 1 (;@3;)
          end
          local.get 2
          local.get 3
          i32.load8_u
          local.tee 3
          i32.store8
          local.get 3
          i32.eqz
          br_if 2 (;@1;)
          local.get 0
          i32.const 3
          i32.add
          local.set 2
          block  ;; label = @4
            local.get 1
            i32.const 3
            i32.add
            local.tee 3
            i32.const 3
            i32.and
            br_if 0 (;@4;)
            local.get 3
            local.set 1
            br 1 (;@3;)
          end
          local.get 2
          local.get 3
          i32.load8_u
          local.tee 3
          i32.store8
          local.get 3
          i32.eqz
          br_if 2 (;@1;)
          local.get 0
          i32.const 4
          i32.add
          local.set 2
          local.get 1
          i32.const 4
          i32.add
          local.set 1
        end
        local.get 1
        i32.load
        local.tee 0
        i32.const -1
        i32.xor
        local.get 0
        i32.const -16843009
        i32.add
        i32.and
        i32.const -2139062144
        i32.and
        br_if 0 (;@2;)
        loop  ;; label = @3
          local.get 2
          local.get 0
          i32.store
          local.get 2
          i32.const 4
          i32.add
          local.set 2
          local.get 1
          i32.const 4
          i32.add
          local.tee 1
          i32.load
          local.tee 0
          i32.const -1
          i32.xor
          local.get 0
          i32.const -16843009
          i32.add
          i32.and
          i32.const -2139062144
          i32.and
          i32.eqz
          br_if 0 (;@3;)
        end
      end
      local.get 2
      local.get 1
      i32.load8_u
      local.tee 0
      i32.store8
      local.get 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 1
      i32.const 1
      i32.add
      local.set 1
      loop  ;; label = @2
        local.get 2
        i32.const 1
        i32.add
        local.tee 2
        local.get 1
        i32.load8_u
        local.tee 0
        i32.store8
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        local.get 0
        br_if 0 (;@2;)
      end
    end
    local.get 2)
  (func $strcpy (type 5) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $__stpcpy
    drop
    local.get 0)
  (func $memcpy (type 7) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 3
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        local.get 2
        i32.eqz
        br_if 0 (;@2;)
        local.get 0
        local.get 1
        i32.load8_u
        i32.store8
        local.get 2
        i32.const -1
        i32.add
        local.set 3
        local.get 0
        i32.const 1
        i32.add
        local.set 4
        local.get 1
        i32.const 1
        i32.add
        local.tee 5
        i32.const 3
        i32.and
        i32.eqz
        br_if 1 (;@1;)
        local.get 3
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        local.get 1
        i32.load8_u offset=1
        i32.store8 offset=1
        local.get 2
        i32.const -2
        i32.add
        local.set 3
        local.get 0
        i32.const 2
        i32.add
        local.set 4
        local.get 1
        i32.const 2
        i32.add
        local.tee 5
        i32.const 3
        i32.and
        i32.eqz
        br_if 1 (;@1;)
        local.get 3
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        local.get 1
        i32.load8_u offset=2
        i32.store8 offset=2
        local.get 2
        i32.const -3
        i32.add
        local.set 3
        local.get 0
        i32.const 3
        i32.add
        local.set 4
        local.get 1
        i32.const 3
        i32.add
        local.tee 5
        i32.const 3
        i32.and
        i32.eqz
        br_if 1 (;@1;)
        local.get 3
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        local.get 1
        i32.load8_u offset=3
        i32.store8 offset=3
        local.get 2
        i32.const -4
        i32.add
        local.set 3
        local.get 0
        i32.const 4
        i32.add
        local.set 4
        local.get 1
        i32.const 4
        i32.add
        local.set 5
        br 1 (;@1;)
      end
      local.get 2
      local.set 3
      local.get 0
      local.set 4
      local.get 1
      local.set 5
    end
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 4
          i32.const 3
          i32.and
          local.tee 1
          br_if 0 (;@3;)
          block  ;; label = @4
            block  ;; label = @5
              local.get 3
              i32.const 16
              i32.lt_u
              br_if 0 (;@5;)
              block  ;; label = @6
                local.get 3
                i32.const -16
                i32.add
                local.tee 1
                i32.const 16
                i32.and
                br_if 0 (;@6;)
                local.get 4
                local.get 5
                i64.load align=4
                i64.store align=4
                local.get 4
                local.get 5
                i64.load offset=8 align=4
                i64.store offset=8 align=4
                local.get 4
                i32.const 16
                i32.add
                local.set 4
                local.get 5
                i32.const 16
                i32.add
                local.set 5
                local.get 1
                local.set 3
              end
              local.get 1
              i32.const 16
              i32.lt_u
              br_if 1 (;@4;)
              loop  ;; label = @6
                local.get 4
                local.get 5
                i64.load align=4
                i64.store align=4
                local.get 4
                i32.const 8
                i32.add
                local.get 5
                i32.const 8
                i32.add
                i64.load align=4
                i64.store align=4
                local.get 4
                i32.const 16
                i32.add
                local.get 5
                i32.const 16
                i32.add
                i64.load align=4
                i64.store align=4
                local.get 4
                i32.const 24
                i32.add
                local.get 5
                i32.const 24
                i32.add
                i64.load align=4
                i64.store align=4
                local.get 4
                i32.const 32
                i32.add
                local.set 4
                local.get 5
                i32.const 32
                i32.add
                local.set 5
                local.get 3
                i32.const -32
                i32.add
                local.tee 3
                i32.const 15
                i32.gt_u
                br_if 0 (;@6;)
              end
            end
            local.get 3
            local.set 1
          end
          block  ;; label = @4
            local.get 1
            i32.const 8
            i32.and
            i32.eqz
            br_if 0 (;@4;)
            local.get 4
            local.get 5
            i64.load align=4
            i64.store align=4
            local.get 5
            i32.const 8
            i32.add
            local.set 5
            local.get 4
            i32.const 8
            i32.add
            local.set 4
          end
          block  ;; label = @4
            local.get 1
            i32.const 4
            i32.and
            i32.eqz
            br_if 0 (;@4;)
            local.get 4
            local.get 5
            i32.load
            i32.store
            local.get 5
            i32.const 4
            i32.add
            local.set 5
            local.get 4
            i32.const 4
            i32.add
            local.set 4
          end
          block  ;; label = @4
            local.get 1
            i32.const 2
            i32.and
            i32.eqz
            br_if 0 (;@4;)
            local.get 4
            local.get 5
            i32.load16_u align=1
            i32.store16 align=1
            local.get 4
            i32.const 2
            i32.add
            local.set 4
            local.get 5
            i32.const 2
            i32.add
            local.set 5
          end
          local.get 1
          i32.const 1
          i32.and
          br_if 1 (;@2;)
          br 2 (;@1;)
        end
        block  ;; label = @3
          local.get 3
          i32.const 32
          i32.lt_u
          br_if 0 (;@3;)
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 1
                i32.const -1
                i32.add
                br_table 0 (;@6;) 1 (;@5;) 2 (;@4;) 3 (;@3;)
              end
              local.get 4
              local.get 5
              i32.load
              local.tee 6
              i32.store8
              local.get 4
              local.get 6
              i32.const 16
              i32.shr_u
              i32.store8 offset=2
              local.get 4
              local.get 6
              i32.const 8
              i32.shr_u
              i32.store8 offset=1
              local.get 3
              i32.const -3
              i32.add
              local.set 3
              local.get 4
              i32.const 3
              i32.add
              local.set 7
              i32.const 0
              local.set 1
              loop  ;; label = @6
                local.get 7
                local.get 1
                i32.add
                local.tee 4
                local.get 5
                local.get 1
                i32.add
                local.tee 2
                i32.const 4
                i32.add
                i32.load
                local.tee 8
                i32.const 8
                i32.shl
                local.get 6
                i32.const 24
                i32.shr_u
                i32.or
                i32.store
                local.get 4
                i32.const 4
                i32.add
                local.get 2
                i32.const 8
                i32.add
                i32.load
                local.tee 6
                i32.const 8
                i32.shl
                local.get 8
                i32.const 24
                i32.shr_u
                i32.or
                i32.store
                local.get 4
                i32.const 8
                i32.add
                local.get 2
                i32.const 12
                i32.add
                i32.load
                local.tee 8
                i32.const 8
                i32.shl
                local.get 6
                i32.const 24
                i32.shr_u
                i32.or
                i32.store
                local.get 4
                i32.const 12
                i32.add
                local.get 2
                i32.const 16
                i32.add
                i32.load
                local.tee 6
                i32.const 8
                i32.shl
                local.get 8
                i32.const 24
                i32.shr_u
                i32.or
                i32.store
                local.get 1
                i32.const 16
                i32.add
                local.set 1
                local.get 3
                i32.const -16
                i32.add
                local.tee 3
                i32.const 16
                i32.gt_u
                br_if 0 (;@6;)
              end
              local.get 7
              local.get 1
              i32.add
              local.set 4
              local.get 5
              local.get 1
              i32.add
              i32.const 3
              i32.add
              local.set 5
              br 2 (;@3;)
            end
            local.get 4
            local.get 5
            i32.load
            local.tee 6
            i32.store16 align=1
            local.get 3
            i32.const -2
            i32.add
            local.set 3
            local.get 4
            i32.const 2
            i32.add
            local.set 7
            i32.const 0
            local.set 1
            loop  ;; label = @5
              local.get 7
              local.get 1
              i32.add
              local.tee 4
              local.get 5
              local.get 1
              i32.add
              local.tee 2
              i32.const 4
              i32.add
              i32.load
              local.tee 8
              i32.const 16
              i32.shl
              local.get 6
              i32.const 16
              i32.shr_u
              i32.or
              i32.store
              local.get 4
              i32.const 4
              i32.add
              local.get 2
              i32.const 8
              i32.add
              i32.load
              local.tee 6
              i32.const 16
              i32.shl
              local.get 8
              i32.const 16
              i32.shr_u
              i32.or
              i32.store
              local.get 4
              i32.const 8
              i32.add
              local.get 2
              i32.const 12
              i32.add
              i32.load
              local.tee 8
              i32.const 16
              i32.shl
              local.get 6
              i32.const 16
              i32.shr_u
              i32.or
              i32.store
              local.get 4
              i32.const 12
              i32.add
              local.get 2
              i32.const 16
              i32.add
              i32.load
              local.tee 6
              i32.const 16
              i32.shl
              local.get 8
              i32.const 16
              i32.shr_u
              i32.or
              i32.store
              local.get 1
              i32.const 16
              i32.add
              local.set 1
              local.get 3
              i32.const -16
              i32.add
              local.tee 3
              i32.const 17
              i32.gt_u
              br_if 0 (;@5;)
            end
            local.get 7
            local.get 1
            i32.add
            local.set 4
            local.get 5
            local.get 1
            i32.add
            i32.const 2
            i32.add
            local.set 5
            br 1 (;@3;)
          end
          local.get 4
          local.get 5
          i32.load
          local.tee 6
          i32.store8
          local.get 3
          i32.const -1
          i32.add
          local.set 3
          local.get 4
          i32.const 1
          i32.add
          local.set 7
          i32.const 0
          local.set 1
          loop  ;; label = @4
            local.get 7
            local.get 1
            i32.add
            local.tee 4
            local.get 5
            local.get 1
            i32.add
            local.tee 2
            i32.const 4
            i32.add
            i32.load
            local.tee 8
            i32.const 24
            i32.shl
            local.get 6
            i32.const 8
            i32.shr_u
            i32.or
            i32.store
            local.get 4
            i32.const 4
            i32.add
            local.get 2
            i32.const 8
            i32.add
            i32.load
            local.tee 6
            i32.const 24
            i32.shl
            local.get 8
            i32.const 8
            i32.shr_u
            i32.or
            i32.store
            local.get 4
            i32.const 8
            i32.add
            local.get 2
            i32.const 12
            i32.add
            i32.load
            local.tee 8
            i32.const 24
            i32.shl
            local.get 6
            i32.const 8
            i32.shr_u
            i32.or
            i32.store
            local.get 4
            i32.const 12
            i32.add
            local.get 2
            i32.const 16
            i32.add
            i32.load
            local.tee 6
            i32.const 24
            i32.shl
            local.get 8
            i32.const 8
            i32.shr_u
            i32.or
            i32.store
            local.get 1
            i32.const 16
            i32.add
            local.set 1
            local.get 3
            i32.const -16
            i32.add
            local.tee 3
            i32.const 18
            i32.gt_u
            br_if 0 (;@4;)
          end
          local.get 7
          local.get 1
          i32.add
          local.set 4
          local.get 5
          local.get 1
          i32.add
          i32.const 1
          i32.add
          local.set 5
        end
        block  ;; label = @3
          local.get 3
          i32.const 16
          i32.and
          i32.eqz
          br_if 0 (;@3;)
          local.get 4
          local.get 5
          i32.load8_u
          i32.store8
          local.get 4
          local.get 5
          i32.load offset=1 align=1
          i32.store offset=1 align=1
          local.get 4
          local.get 5
          i64.load offset=5 align=1
          i64.store offset=5 align=1
          local.get 4
          local.get 5
          i32.load16_u offset=13 align=1
          i32.store16 offset=13 align=1
          local.get 4
          local.get 5
          i32.load8_u offset=15
          i32.store8 offset=15
          local.get 4
          i32.const 16
          i32.add
          local.set 4
          local.get 5
          i32.const 16
          i32.add
          local.set 5
        end
        block  ;; label = @3
          local.get 3
          i32.const 8
          i32.and
          i32.eqz
          br_if 0 (;@3;)
          local.get 4
          local.get 5
          i64.load align=1
          i64.store align=1
          local.get 4
          i32.const 8
          i32.add
          local.set 4
          local.get 5
          i32.const 8
          i32.add
          local.set 5
        end
        block  ;; label = @3
          local.get 3
          i32.const 4
          i32.and
          i32.eqz
          br_if 0 (;@3;)
          local.get 4
          local.get 5
          i32.load align=1
          i32.store align=1
          local.get 4
          i32.const 4
          i32.add
          local.set 4
          local.get 5
          i32.const 4
          i32.add
          local.set 5
        end
        block  ;; label = @3
          local.get 3
          i32.const 2
          i32.and
          i32.eqz
          br_if 0 (;@3;)
          local.get 4
          local.get 5
          i32.load16_u align=1
          i32.store16 align=1
          local.get 4
          i32.const 2
          i32.add
          local.set 4
          local.get 5
          i32.const 2
          i32.add
          local.set 5
        end
        local.get 3
        i32.const 1
        i32.and
        i32.eqz
        br_if 1 (;@1;)
      end
      local.get 4
      local.get 5
      i32.load8_u
      i32.store8
    end
    local.get 0)
  (func $__strchrnul (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.const 255
            i32.and
            local.tee 2
            i32.eqz
            br_if 0 (;@4;)
            local.get 0
            i32.const 3
            i32.and
            i32.eqz
            br_if 2 (;@2;)
            block  ;; label = @5
              local.get 0
              i32.load8_u
              local.tee 3
              br_if 0 (;@5;)
              local.get 0
              return
            end
            local.get 3
            local.get 1
            i32.const 255
            i32.and
            i32.ne
            br_if 1 (;@3;)
            local.get 0
            return
          end
          local.get 0
          local.get 0
          call $strlen
          i32.add
          return
        end
        block  ;; label = @3
          local.get 0
          i32.const 1
          i32.add
          local.tee 3
          i32.const 3
          i32.and
          br_if 0 (;@3;)
          local.get 3
          local.set 0
          br 1 (;@2;)
        end
        local.get 3
        i32.load8_u
        local.tee 4
        i32.eqz
        br_if 1 (;@1;)
        local.get 4
        local.get 1
        i32.const 255
        i32.and
        i32.eq
        br_if 1 (;@1;)
        block  ;; label = @3
          local.get 0
          i32.const 2
          i32.add
          local.tee 3
          i32.const 3
          i32.and
          br_if 0 (;@3;)
          local.get 3
          local.set 0
          br 1 (;@2;)
        end
        local.get 3
        i32.load8_u
        local.tee 4
        i32.eqz
        br_if 1 (;@1;)
        local.get 4
        local.get 1
        i32.const 255
        i32.and
        i32.eq
        br_if 1 (;@1;)
        block  ;; label = @3
          local.get 0
          i32.const 3
          i32.add
          local.tee 3
          i32.const 3
          i32.and
          br_if 0 (;@3;)
          local.get 3
          local.set 0
          br 1 (;@2;)
        end
        local.get 3
        i32.load8_u
        local.tee 4
        i32.eqz
        br_if 1 (;@1;)
        local.get 4
        local.get 1
        i32.const 255
        i32.and
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        i32.const 4
        i32.add
        local.set 0
      end
      block  ;; label = @2
        local.get 0
        i32.load
        local.tee 3
        i32.const -1
        i32.xor
        local.get 3
        i32.const -16843009
        i32.add
        i32.and
        i32.const -2139062144
        i32.and
        br_if 0 (;@2;)
        local.get 2
        i32.const 16843009
        i32.mul
        local.set 2
        loop  ;; label = @3
          local.get 3
          local.get 2
          i32.xor
          local.tee 3
          i32.const -1
          i32.xor
          local.get 3
          i32.const -16843009
          i32.add
          i32.and
          i32.const -2139062144
          i32.and
          br_if 1 (;@2;)
          local.get 0
          i32.const 4
          i32.add
          local.tee 0
          i32.load
          local.tee 3
          i32.const -1
          i32.xor
          local.get 3
          i32.const -16843009
          i32.add
          i32.and
          i32.const -2139062144
          i32.and
          i32.eqz
          br_if 0 (;@3;)
        end
      end
      local.get 0
      i32.const -1
      i32.add
      local.set 3
      loop  ;; label = @2
        local.get 3
        i32.const 1
        i32.add
        local.tee 3
        i32.load8_u
        local.tee 0
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        local.get 1
        i32.const 255
        i32.and
        i32.ne
        br_if 0 (;@2;)
      end
    end
    local.get 3)
  (func $strlen (type 4) (param i32) (result i32)
    (local i32 i32)
    local.get 0
    local.set 1
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.const 3
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        local.get 0
        local.set 1
        local.get 0
        i32.load8_u
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        i32.const 1
        i32.add
        local.tee 1
        i32.const 3
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        i32.load8_u
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        i32.const 2
        i32.add
        local.tee 1
        i32.const 3
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        i32.load8_u
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        i32.const 3
        i32.add
        local.tee 1
        i32.const 3
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        i32.load8_u
        i32.eqz
        br_if 1 (;@1;)
        local.get 0
        i32.const 4
        i32.add
        local.set 1
      end
      local.get 1
      i32.const -4
      i32.add
      local.set 1
      loop  ;; label = @2
        local.get 1
        i32.const 4
        i32.add
        local.tee 1
        i32.load
        local.tee 2
        i32.const -1
        i32.xor
        local.get 2
        i32.const -16843009
        i32.add
        i32.and
        i32.const -2139062144
        i32.and
        i32.eqz
        br_if 0 (;@2;)
      end
      local.get 2
      i32.const 255
      i32.and
      i32.eqz
      br_if 0 (;@1;)
      loop  ;; label = @2
        local.get 1
        i32.const 1
        i32.add
        local.tee 1
        i32.load8_u
        br_if 0 (;@2;)
      end
    end
    local.get 1
    local.get 0
    i32.sub)
  (func $strncmp (type 7) (param i32 i32 i32) (result i32)
    (local i32 i32 i32)
    block  ;; label = @1
      local.get 2
      br_if 0 (;@1;)
      i32.const 0
      return
    end
    i32.const 0
    local.set 3
    block  ;; label = @1
      local.get 0
      i32.load8_u
      local.tee 4
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const 1
      i32.add
      local.set 0
      local.get 2
      i32.const -1
      i32.add
      local.set 2
      loop  ;; label = @2
        block  ;; label = @3
          local.get 1
          i32.load8_u
          local.tee 5
          br_if 0 (;@3;)
          local.get 4
          local.set 3
          br 2 (;@1;)
        end
        block  ;; label = @3
          local.get 2
          br_if 0 (;@3;)
          local.get 4
          local.set 3
          br 2 (;@1;)
        end
        block  ;; label = @3
          local.get 4
          i32.const 255
          i32.and
          local.get 5
          i32.eq
          br_if 0 (;@3;)
          local.get 4
          local.set 3
          br 2 (;@1;)
        end
        local.get 2
        i32.const -1
        i32.add
        local.set 2
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        local.get 0
        i32.load8_u
        local.set 4
        local.get 0
        i32.const 1
        i32.add
        local.set 0
        local.get 4
        br_if 0 (;@2;)
      end
    end
    local.get 3
    i32.const 255
    i32.and
    local.get 1
    i32.load8_u
    i32.sub)
  (func $strerror (type 4) (param i32) (result i32)
    (local i32)
    block  ;; label = @1
      i32.const 0
      i32.load offset=1059972
      local.tee 1
      br_if 0 (;@1;)
      i32.const 1059948
      local.set 1
      i32.const 0
      i32.const 1059948
      i32.store offset=1059972
    end
    i32.const 0
    local.get 0
    local.get 0
    i32.const 76
    i32.gt_u
    select
    i32.const 1
    i32.shl
    i32.const 1054832
    i32.add
    i32.load16_u
    i32.const 1053274
    i32.add
    local.get 1
    i32.load offset=20
    call $__lctrans)
  (func $strerror_r (type 7) (param i32 i32 i32) (result i32)
    (local i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        call $strerror
        local.tee 0
        call $strlen
        local.tee 3
        local.get 2
        i32.lt_u
        br_if 0 (;@2;)
        i32.const 68
        local.set 3
        local.get 2
        i32.eqz
        br_if 1 (;@1;)
        local.get 1
        local.get 0
        local.get 2
        i32.const -1
        i32.add
        local.tee 2
        call $memcpy
        local.get 2
        i32.add
        i32.const 0
        i32.store8
        i32.const 68
        return
      end
      local.get 1
      local.get 0
      local.get 3
      i32.const 1
      i32.add
      call $memcpy
      drop
      i32.const 0
      local.set 3
    end
    local.get 3)
  (func $memset (type 7) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i64)
    block  ;; label = @1
      local.get 2
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      i32.store8
      local.get 2
      local.get 0
      i32.add
      local.tee 3
      i32.const -1
      i32.add
      local.get 1
      i32.store8
      local.get 2
      i32.const 3
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      i32.store8 offset=2
      local.get 0
      local.get 1
      i32.store8 offset=1
      local.get 3
      i32.const -3
      i32.add
      local.get 1
      i32.store8
      local.get 3
      i32.const -2
      i32.add
      local.get 1
      i32.store8
      local.get 2
      i32.const 7
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      local.get 1
      i32.store8 offset=3
      local.get 3
      i32.const -4
      i32.add
      local.get 1
      i32.store8
      local.get 2
      i32.const 9
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      i32.const 0
      local.get 0
      i32.sub
      i32.const 3
      i32.and
      local.tee 4
      i32.add
      local.tee 3
      local.get 1
      i32.const 255
      i32.and
      i32.const 16843009
      i32.mul
      local.tee 1
      i32.store
      local.get 3
      local.get 2
      local.get 4
      i32.sub
      i32.const -4
      i32.and
      local.tee 4
      i32.add
      local.tee 2
      i32.const -4
      i32.add
      local.get 1
      i32.store
      local.get 4
      i32.const 9
      i32.lt_u
      br_if 0 (;@1;)
      local.get 3
      local.get 1
      i32.store offset=8
      local.get 3
      local.get 1
      i32.store offset=4
      local.get 2
      i32.const -8
      i32.add
      local.get 1
      i32.store
      local.get 2
      i32.const -12
      i32.add
      local.get 1
      i32.store
      local.get 4
      i32.const 25
      i32.lt_u
      br_if 0 (;@1;)
      local.get 3
      local.get 1
      i32.store offset=24
      local.get 3
      local.get 1
      i32.store offset=20
      local.get 3
      local.get 1
      i32.store offset=16
      local.get 3
      local.get 1
      i32.store offset=12
      local.get 2
      i32.const -16
      i32.add
      local.get 1
      i32.store
      local.get 2
      i32.const -20
      i32.add
      local.get 1
      i32.store
      local.get 2
      i32.const -24
      i32.add
      local.get 1
      i32.store
      local.get 2
      i32.const -28
      i32.add
      local.get 1
      i32.store
      local.get 4
      local.get 3
      i32.const 4
      i32.and
      i32.const 24
      i32.or
      local.tee 5
      i32.sub
      local.tee 2
      i32.const 32
      i32.lt_u
      br_if 0 (;@1;)
      local.get 1
      i64.extend_i32_u
      i64.const 4294967297
      i64.mul
      local.set 6
      local.get 3
      local.get 5
      i32.add
      local.set 1
      loop  ;; label = @2
        local.get 1
        local.get 6
        i64.store
        local.get 1
        i32.const 24
        i32.add
        local.get 6
        i64.store
        local.get 1
        i32.const 16
        i32.add
        local.get 6
        i64.store
        local.get 1
        i32.const 8
        i32.add
        local.get 6
        i64.store
        local.get 1
        i32.const 32
        i32.add
        local.set 1
        local.get 2
        i32.const -32
        i32.add
        local.tee 2
        i32.const 31
        i32.gt_u
        br_if 0 (;@2;)
      end
    end
    local.get 0)
  (func $dummy.1 (type 5) (param i32 i32) (result i32)
    local.get 0)
  (func $__lctrans (type 5) (param i32 i32) (result i32)
    local.get 0
    local.get 1
    call $dummy.1)
  (func $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h0c574378139f8ff5E (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.set 0
    block  ;; label = @1
      local.get 1
      call $_ZN4core3fmt9Formatter15debug_lower_hex17h3f763578aa6ee5baE
      br_if 0 (;@1;)
      block  ;; label = @2
        local.get 1
        call $_ZN4core3fmt9Formatter15debug_upper_hex17h96706c7c15afea4cE
        br_if 0 (;@2;)
        local.get 0
        local.get 1
        call $_ZN4core3fmt3num3imp51_$LT$impl$u20$core..fmt..Display$u20$for$u20$u8$GT$3fmt17h508a1bd06037f818E
        return
      end
      local.get 0
      local.get 1
      call $_ZN4core3fmt3num52_$LT$impl$u20$core..fmt..UpperHex$u20$for$u20$i8$GT$3fmt17h7caa790059f60235E
      return
    end
    local.get 0
    local.get 1
    call $_ZN4core3fmt3num52_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i8$GT$3fmt17hc48681d90ac04511E)
  (func $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h1bfd36f5b722dde7E (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.set 0
    block  ;; label = @1
      local.get 1
      call $_ZN4core3fmt9Formatter15debug_lower_hex17h3f763578aa6ee5baE
      br_if 0 (;@1;)
      block  ;; label = @2
        local.get 1
        call $_ZN4core3fmt9Formatter15debug_upper_hex17h96706c7c15afea4cE
        br_if 0 (;@2;)
        local.get 0
        local.get 1
        call $_ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$3fmt17h438b5cc0efea7d46E
        return
      end
      local.get 0
      local.get 1
      call $_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..UpperHex$u20$for$u20$i32$GT$3fmt17h4207c064850a1e9eE
      return
    end
    local.get 0
    local.get 1
    call $_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i32$GT$3fmt17hbb0d214f9fde181dE)
  (func $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h4e2b50f3537886b4E (type 5) (param i32 i32) (result i32)
    (local i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 0
    i32.load
    local.tee 0
    i32.const 8
    i32.add
    i32.load
    local.set 3
    local.get 0
    i32.load
    local.set 0
    local.get 2
    local.get 1
    call $_ZN4core3fmt9Formatter10debug_list17ha46657161459126cE
    block  ;; label = @1
      local.get 3
      i32.eqz
      br_if 0 (;@1;)
      loop  ;; label = @2
        local.get 2
        local.get 0
        i32.store offset=12
        local.get 2
        local.get 2
        i32.const 12
        i32.add
        i32.const 1054988
        call $_ZN4core3fmt8builders8DebugSet5entry17hb3bdf118faaef184E
        drop
        local.get 0
        i32.const 1
        i32.add
        local.set 0
        local.get 3
        i32.const -1
        i32.add
        local.tee 3
        br_if 0 (;@2;)
      end
    end
    local.get 2
    call $_ZN4core3fmt8builders9DebugList6finish17h7ba20995ee25f20eE
    local.set 0
    local.get 2
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core3ptr27drop_in_place$LT$$RF$u8$GT$17h010384b463d094f9E (type 1) (param i32))
  (func $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$7reserve21do_reserve_and_handle17hb8e1de121cbbb1a6E (type 6) (param i32 i32 i32)
    (local i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        local.get 2
        i32.add
        local.tee 2
        local.get 1
        i32.lt_u
        br_if 0 (;@2;)
        local.get 0
        i32.const 4
        i32.add
        i32.load
        local.tee 1
        i32.const 1
        i32.shl
        local.tee 4
        local.get 2
        local.get 4
        local.get 2
        i32.gt_u
        select
        local.tee 2
        i32.const 8
        local.get 2
        i32.const 8
        i32.gt_u
        select
        local.tee 2
        i32.const -1
        i32.xor
        i32.const 31
        i32.shr_u
        local.set 4
        block  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.eqz
            br_if 0 (;@4;)
            local.get 3
            i32.const 1
            i32.store offset=24
            local.get 3
            local.get 1
            i32.store offset=20
            local.get 3
            local.get 0
            i32.load
            i32.store offset=16
            br 1 (;@3;)
          end
          local.get 3
          i32.const 0
          i32.store offset=24
        end
        local.get 3
        local.get 2
        local.get 4
        local.get 3
        i32.const 16
        i32.add
        call $_ZN5alloc7raw_vec11finish_grow17heb588c00c87eb036E
        local.get 3
        i32.load offset=4
        local.set 1
        block  ;; label = @3
          local.get 3
          i32.load
          br_if 0 (;@3;)
          local.get 0
          local.get 1
          i32.store
          local.get 0
          i32.const 4
          i32.add
          local.get 2
          i32.store
          br 2 (;@1;)
        end
        local.get 3
        i32.const 8
        i32.add
        i32.load
        local.tee 0
        i32.const -2147483647
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        local.get 0
        call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
        unreachable
      end
      call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
      unreachable
    end
    local.get 3
    i32.const 32
    i32.add
    global.set $__stack_pointer)
  (func $_ZN5alloc7raw_vec11finish_grow17heb588c00c87eb036E (type 9) (param i32 i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 2
        i32.eqz
        br_if 0 (;@2;)
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    local.get 1
                    i32.const 0
                    i32.lt_s
                    br_if 0 (;@8;)
                    local.get 3
                    i32.load offset=8
                    br_if 1 (;@7;)
                    local.get 1
                    br_if 2 (;@6;)
                    i32.const 1
                    local.set 2
                    br 4 (;@4;)
                  end
                  local.get 0
                  i32.const 8
                  i32.add
                  i32.const 0
                  i32.store
                  br 6 (;@1;)
                end
                block  ;; label = @7
                  local.get 3
                  i32.load offset=4
                  local.tee 2
                  br_if 0 (;@7;)
                  block  ;; label = @8
                    local.get 1
                    br_if 0 (;@8;)
                    i32.const 1
                    local.set 2
                    br 4 (;@4;)
                  end
                  local.get 1
                  i32.const 1
                  call $__rust_alloc
                  local.set 2
                  br 2 (;@5;)
                end
                local.get 3
                i32.load
                local.get 2
                i32.const 1
                local.get 1
                call $__rust_realloc
                local.set 2
                br 1 (;@5;)
              end
              local.get 1
              i32.const 1
              call $__rust_alloc
              local.set 2
            end
            local.get 2
            i32.eqz
            br_if 1 (;@3;)
          end
          local.get 0
          local.get 2
          i32.store offset=4
          local.get 0
          i32.const 8
          i32.add
          local.get 1
          i32.store
          local.get 0
          i32.const 0
          i32.store
          return
        end
        local.get 0
        local.get 1
        i32.store offset=4
        local.get 0
        i32.const 8
        i32.add
        i32.const 1
        i32.store
        local.get 0
        i32.const 1
        i32.store
        return
      end
      local.get 0
      local.get 1
      i32.store offset=4
      local.get 0
      i32.const 8
      i32.add
      i32.const 0
      i32.store
    end
    local.get 0
    i32.const 1
    i32.store)
  (func $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE (type 3) (param i32 i32)
    local.get 0
    local.get 1
    call $_ZN5alloc5alloc18handle_alloc_error8rt_error17h8a8b32f0fcc6ac2fE
    unreachable)
  (func $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E (type 0)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 0
    global.set $__stack_pointer
    local.get 0
    i32.const 28
    i32.add
    i32.const 0
    i32.store
    local.get 0
    i32.const 1055004
    i32.store offset=24
    local.get 0
    i64.const 1
    i64.store offset=12 align=4
    local.get 0
    i32.const 1055052
    i32.store offset=8
    local.get 0
    i32.const 8
    i32.add
    i32.const 1055060
    call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
    unreachable)
  (func $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$16reserve_for_push17h3d6107aae07ed92bE (type 3) (param i32 i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 1
        i32.add
        local.tee 1
        i32.eqz
        br_if 0 (;@2;)
        local.get 0
        i32.const 4
        i32.add
        i32.load
        local.tee 3
        i32.const 1
        i32.shl
        local.tee 4
        local.get 1
        local.get 4
        local.get 1
        i32.gt_u
        select
        local.tee 1
        i32.const 8
        local.get 1
        i32.const 8
        i32.gt_u
        select
        local.tee 1
        i32.const -1
        i32.xor
        i32.const 31
        i32.shr_u
        local.set 4
        block  ;; label = @3
          block  ;; label = @4
            local.get 3
            i32.eqz
            br_if 0 (;@4;)
            local.get 2
            i32.const 1
            i32.store offset=24
            local.get 2
            local.get 3
            i32.store offset=20
            local.get 2
            local.get 0
            i32.load
            i32.store offset=16
            br 1 (;@3;)
          end
          local.get 2
          i32.const 0
          i32.store offset=24
        end
        local.get 2
        local.get 1
        local.get 4
        local.get 2
        i32.const 16
        i32.add
        call $_ZN5alloc7raw_vec11finish_grow17heb588c00c87eb036E
        local.get 2
        i32.load offset=4
        local.set 3
        block  ;; label = @3
          local.get 2
          i32.load
          br_if 0 (;@3;)
          local.get 0
          local.get 3
          i32.store
          local.get 0
          i32.const 4
          i32.add
          local.get 1
          i32.store
          br 2 (;@1;)
        end
        local.get 2
        i32.const 8
        i32.add
        i32.load
        local.tee 0
        i32.const -2147483647
        i32.eq
        br_if 1 (;@1;)
        local.get 0
        i32.eqz
        br_if 0 (;@2;)
        local.get 3
        local.get 0
        call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
        unreachable
      end
      call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
      unreachable
    end
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer)
  (func $_ZN5alloc5alloc18handle_alloc_error8rt_error17h8a8b32f0fcc6ac2fE (type 3) (param i32 i32)
    local.get 0
    local.get 1
    call $__rust_alloc_error_handler
    unreachable)
  (func $__rg_oom (type 3) (param i32 i32)
    local.get 0
    local.get 1
    call $rust_oom
    unreachable)
  (func $_ZN81_$LT$$RF$$u5b$u8$u5d$$u20$as$u20$alloc..ffi..c_str..CString..new..SpecNewImpl$GT$13spec_new_impl17h9ddb1e973ffe1a2cE (type 6) (param i32 i32 i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 2
              i32.const 1
              i32.add
              local.tee 4
              i32.eqz
              br_if 0 (;@5;)
              local.get 4
              i32.const -1
              i32.le_s
              br_if 1 (;@4;)
              local.get 4
              i32.const 1
              call $__rust_alloc
              local.tee 5
              i32.eqz
              br_if 2 (;@3;)
              local.get 3
              i32.const 0
              i32.store offset=24
              local.get 3
              local.get 4
              i32.store offset=20
              local.get 3
              local.get 5
              i32.store offset=16
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    local.get 4
                    br_if 0 (;@8;)
                    local.get 3
                    i32.const 16
                    i32.add
                    i32.const 0
                    local.get 2
                    call $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$7reserve21do_reserve_and_handle17hb8e1de121cbbb1a6E
                    local.get 3
                    i32.load offset=16
                    local.get 3
                    i32.load offset=24
                    local.tee 4
                    i32.add
                    local.get 1
                    local.get 2
                    call $memcpy
                    drop
                    local.get 3
                    local.get 4
                    local.get 2
                    i32.add
                    i32.store offset=24
                    br 1 (;@7;)
                  end
                  local.get 5
                  local.get 1
                  local.get 2
                  call $memcpy
                  drop
                  local.get 3
                  local.get 2
                  i32.store offset=24
                  local.get 2
                  i32.const 8
                  i32.lt_u
                  br_if 1 (;@6;)
                end
                local.get 3
                i32.const 8
                i32.add
                i32.const 0
                local.get 1
                local.get 2
                call $_ZN4core5slice6memchr14memchr_aligned17ha15e5d65ea28ac67E
                local.get 3
                i32.load offset=12
                local.set 5
                local.get 3
                i32.load offset=8
                local.set 4
                br 5 (;@1;)
              end
              block  ;; label = @6
                local.get 2
                br_if 0 (;@6;)
                i32.const 0
                local.set 5
                i32.const 0
                local.set 4
                br 5 (;@1;)
              end
              block  ;; label = @6
                local.get 1
                i32.load8_u
                br_if 0 (;@6;)
                i32.const 1
                local.set 4
                i32.const 0
                local.set 5
                br 5 (;@1;)
              end
              i32.const 1
              local.set 4
              local.get 2
              i32.const 1
              i32.eq
              br_if 3 (;@2;)
              block  ;; label = @6
                local.get 1
                i32.load8_u offset=1
                br_if 0 (;@6;)
                i32.const 1
                local.set 5
                br 5 (;@1;)
              end
              i32.const 2
              local.set 5
              local.get 2
              i32.const 2
              i32.eq
              br_if 3 (;@2;)
              local.get 1
              i32.load8_u offset=2
              i32.eqz
              br_if 4 (;@1;)
              i32.const 3
              local.set 5
              local.get 2
              i32.const 3
              i32.eq
              br_if 3 (;@2;)
              local.get 1
              i32.load8_u offset=3
              i32.eqz
              br_if 4 (;@1;)
              i32.const 4
              local.set 5
              local.get 2
              i32.const 4
              i32.eq
              br_if 3 (;@2;)
              local.get 1
              i32.load8_u offset=4
              i32.eqz
              br_if 4 (;@1;)
              i32.const 5
              local.set 5
              local.get 2
              i32.const 5
              i32.eq
              br_if 3 (;@2;)
              local.get 1
              i32.load8_u offset=5
              i32.eqz
              br_if 4 (;@1;)
              local.get 2
              i32.const 6
              i32.eq
              br_if 3 (;@2;)
              local.get 2
              i32.const 6
              local.get 1
              i32.load8_u offset=6
              local.tee 4
              select
              local.set 5
              local.get 4
              i32.eqz
              local.set 4
              br 4 (;@1;)
            end
            i32.const 1055076
            i32.const 43
            i32.const 1055152
            call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
            unreachable
          end
          call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
          unreachable
        end
        local.get 4
        i32.const 1
        call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
        unreachable
      end
      local.get 2
      local.set 5
      i32.const 0
      local.set 4
    end
    block  ;; label = @1
      block  ;; label = @2
        local.get 4
        br_if 0 (;@2;)
        local.get 3
        i32.const 32
        i32.add
        i32.const 8
        i32.add
        local.get 3
        i32.const 16
        i32.add
        i32.const 8
        i32.add
        i32.load
        i32.store
        local.get 3
        local.get 3
        i64.load offset=16
        i64.store offset=32
        local.get 3
        local.get 3
        i32.const 32
        i32.add
        call $_ZN5alloc3ffi5c_str7CString19_from_vec_unchecked17h6a44b172131b4656E
        local.get 0
        local.get 3
        i64.load
        i64.store offset=8 align=4
        local.get 0
        i32.const 0
        i32.store offset=4
        br 1 (;@1;)
      end
      local.get 0
      local.get 3
      i64.load offset=16
      i64.store offset=4 align=4
      local.get 0
      local.get 5
      i32.store
      local.get 0
      i32.const 12
      i32.add
      local.get 3
      i32.const 24
      i32.add
      i32.load
      i32.store
    end
    local.get 3
    i32.const 48
    i32.add
    global.set $__stack_pointer)
  (func $_ZN5alloc3ffi5c_str7CString19_from_vec_unchecked17h6a44b172131b4656E (type 3) (param i32 i32)
    (local i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 1
              i32.const 4
              i32.add
              i32.load
              local.tee 3
              local.get 1
              i32.load offset=8
              local.tee 4
              i32.ne
              br_if 0 (;@5;)
              local.get 4
              i32.const 1
              i32.add
              local.tee 3
              i32.eqz
              br_if 2 (;@3;)
              local.get 3
              i32.const -1
              i32.xor
              i32.const 31
              i32.shr_u
              local.set 5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 4
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 2
                  i32.const 1
                  i32.store offset=24
                  local.get 2
                  local.get 4
                  i32.store offset=20
                  local.get 2
                  local.get 1
                  i32.load
                  i32.store offset=16
                  br 1 (;@6;)
                end
                local.get 2
                i32.const 0
                i32.store offset=24
              end
              local.get 2
              local.get 3
              local.get 5
              local.get 2
              i32.const 16
              i32.add
              call $_ZN5alloc7raw_vec11finish_grow17heb588c00c87eb036E
              local.get 2
              i32.load offset=4
              local.set 5
              local.get 2
              i32.load
              br_if 1 (;@4;)
              local.get 1
              local.get 5
              i32.store
              local.get 1
              i32.const 4
              i32.add
              local.get 3
              i32.store
            end
            local.get 4
            local.get 3
            i32.ne
            br_if 3 (;@1;)
            br 2 (;@2;)
          end
          local.get 2
          i32.const 8
          i32.add
          i32.load
          local.tee 3
          i32.const -2147483647
          i32.eq
          br_if 1 (;@2;)
          local.get 3
          i32.eqz
          br_if 0 (;@3;)
          local.get 5
          local.get 3
          call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
          unreachable
        end
        call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
        unreachable
      end
      local.get 1
      local.get 4
      call $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$16reserve_for_push17h3d6107aae07ed92bE
      local.get 1
      i32.const 4
      i32.add
      i32.load
      local.set 3
      local.get 1
      i32.load offset=8
      local.set 4
    end
    local.get 1
    local.get 4
    i32.const 1
    i32.add
    local.tee 5
    i32.store offset=8
    local.get 1
    i32.load
    local.tee 1
    local.get 4
    i32.add
    i32.const 0
    i32.store8
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 3
          local.get 5
          i32.gt_u
          br_if 0 (;@3;)
          local.get 1
          local.set 4
          br 1 (;@2;)
        end
        block  ;; label = @3
          local.get 5
          br_if 0 (;@3;)
          i32.const 1
          local.set 4
          local.get 1
          local.get 3
          i32.const 1
          call $__rust_dealloc
          br 1 (;@2;)
        end
        local.get 1
        local.get 3
        i32.const 1
        local.get 5
        call $__rust_realloc
        local.tee 4
        i32.eqz
        br_if 1 (;@1;)
      end
      local.get 0
      local.get 5
      i32.store offset=4
      local.get 0
      local.get 4
      i32.store
      local.get 2
      i32.const 32
      i32.add
      global.set $__stack_pointer
      return
    end
    local.get 5
    i32.const 1
    call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
    unreachable)
  (func $_ZN72_$LT$$RF$str$u20$as$u20$alloc..ffi..c_str..CString..new..SpecNewImpl$GT$13spec_new_impl17h8276bc7cde01c1feE (type 6) (param i32 i32 i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 2
              i32.const 1
              i32.add
              local.tee 4
              i32.eqz
              br_if 0 (;@5;)
              local.get 4
              i32.const -1
              i32.le_s
              br_if 1 (;@4;)
              local.get 4
              i32.const 1
              call $__rust_alloc
              local.tee 5
              i32.eqz
              br_if 2 (;@3;)
              local.get 3
              i32.const 0
              i32.store offset=24
              local.get 3
              local.get 4
              i32.store offset=20
              local.get 3
              local.get 5
              i32.store offset=16
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    local.get 4
                    br_if 0 (;@8;)
                    local.get 3
                    i32.const 16
                    i32.add
                    i32.const 0
                    local.get 2
                    call $_ZN5alloc7raw_vec19RawVec$LT$T$C$A$GT$7reserve21do_reserve_and_handle17hb8e1de121cbbb1a6E
                    local.get 3
                    i32.load offset=16
                    local.get 3
                    i32.load offset=24
                    local.tee 4
                    i32.add
                    local.get 1
                    local.get 2
                    call $memcpy
                    drop
                    local.get 3
                    local.get 4
                    local.get 2
                    i32.add
                    i32.store offset=24
                    br 1 (;@7;)
                  end
                  local.get 5
                  local.get 1
                  local.get 2
                  call $memcpy
                  drop
                  local.get 3
                  local.get 2
                  i32.store offset=24
                  local.get 2
                  i32.const 8
                  i32.lt_u
                  br_if 1 (;@6;)
                end
                local.get 3
                i32.const 8
                i32.add
                i32.const 0
                local.get 1
                local.get 2
                call $_ZN4core5slice6memchr14memchr_aligned17ha15e5d65ea28ac67E
                local.get 3
                i32.load offset=12
                local.set 5
                local.get 3
                i32.load offset=8
                local.set 4
                br 5 (;@1;)
              end
              block  ;; label = @6
                local.get 2
                br_if 0 (;@6;)
                i32.const 0
                local.set 5
                i32.const 0
                local.set 4
                br 5 (;@1;)
              end
              block  ;; label = @6
                local.get 1
                i32.load8_u
                br_if 0 (;@6;)
                i32.const 1
                local.set 4
                i32.const 0
                local.set 5
                br 5 (;@1;)
              end
              i32.const 1
              local.set 4
              local.get 2
              i32.const 1
              i32.eq
              br_if 3 (;@2;)
              block  ;; label = @6
                local.get 1
                i32.load8_u offset=1
                br_if 0 (;@6;)
                i32.const 1
                local.set 5
                br 5 (;@1;)
              end
              i32.const 2
              local.set 5
              local.get 2
              i32.const 2
              i32.eq
              br_if 3 (;@2;)
              local.get 1
              i32.load8_u offset=2
              i32.eqz
              br_if 4 (;@1;)
              i32.const 3
              local.set 5
              local.get 2
              i32.const 3
              i32.eq
              br_if 3 (;@2;)
              local.get 1
              i32.load8_u offset=3
              i32.eqz
              br_if 4 (;@1;)
              i32.const 4
              local.set 5
              local.get 2
              i32.const 4
              i32.eq
              br_if 3 (;@2;)
              local.get 1
              i32.load8_u offset=4
              i32.eqz
              br_if 4 (;@1;)
              i32.const 5
              local.set 5
              local.get 2
              i32.const 5
              i32.eq
              br_if 3 (;@2;)
              local.get 1
              i32.load8_u offset=5
              i32.eqz
              br_if 4 (;@1;)
              local.get 2
              i32.const 6
              i32.eq
              br_if 3 (;@2;)
              local.get 2
              i32.const 6
              local.get 1
              i32.load8_u offset=6
              local.tee 4
              select
              local.set 5
              local.get 4
              i32.eqz
              local.set 4
              br 4 (;@1;)
            end
            i32.const 1055076
            i32.const 43
            i32.const 1055168
            call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
            unreachable
          end
          call $_ZN5alloc7raw_vec17capacity_overflow17h77c070162cb1a895E
          unreachable
        end
        local.get 4
        i32.const 1
        call $_ZN5alloc5alloc18handle_alloc_error17h92cb162d32dc6dcdE
        unreachable
      end
      local.get 2
      local.set 5
      i32.const 0
      local.set 4
    end
    block  ;; label = @1
      block  ;; label = @2
        local.get 4
        br_if 0 (;@2;)
        local.get 3
        i32.const 32
        i32.add
        i32.const 8
        i32.add
        local.get 3
        i32.const 16
        i32.add
        i32.const 8
        i32.add
        i32.load
        i32.store
        local.get 3
        local.get 3
        i64.load offset=16
        i64.store offset=32
        local.get 3
        local.get 3
        i32.const 32
        i32.add
        call $_ZN5alloc3ffi5c_str7CString19_from_vec_unchecked17h6a44b172131b4656E
        local.get 0
        local.get 3
        i64.load
        i64.store offset=8 align=4
        local.get 0
        i32.const 0
        i32.store offset=4
        br 1 (;@1;)
      end
      local.get 0
      local.get 3
      i64.load offset=16
      i64.store offset=4 align=4
      local.get 0
      local.get 5
      i32.store
      local.get 0
      i32.const 12
      i32.add
      local.get 3
      i32.const 24
      i32.add
      i32.load
      i32.store
    end
    local.get 3
    i32.const 48
    i32.add
    global.set $__stack_pointer)
  (func $_ZN5alloc6string13FromUtf8Error10into_bytes17ha1e23acff317451fE (type 3) (param i32 i32)
    local.get 0
    local.get 1
    i64.load align=4
    i64.store align=4
    local.get 0
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i32.load
    i32.store)
  (func $_ZN64_$LT$alloc..ffi..c_str..NulError$u20$as$u20$core..fmt..Debug$GT$3fmt17hae0afb8cc3412d06E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 0
    i32.store offset=8
    local.get 2
    local.get 0
    i32.const 4
    i32.add
    i32.store offset=12
    local.get 1
    i32.const 1055184
    i32.const 8
    local.get 2
    i32.const 8
    i32.add
    i32.const 1055192
    local.get 2
    i32.const 12
    i32.add
    i32.const 1055208
    call $_ZN4core3fmt9Formatter25debug_tuple_field2_finish17h2a3eb19364bc5cdcE
    local.set 0
    local.get 2
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core3ops8function6FnOnce9call_once17hed399a7e342e5203E (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    drop
    loop (result i32)  ;; label = @1
      br 0 (;@1;)
    end)
  (func $_ZN4core3ptr102drop_in_place$LT$$RF$core..iter..adapters..copied..Copied$LT$core..slice..iter..Iter$LT$u8$GT$$GT$$GT$17ha3f154a6a3733eb4E (type 1) (param i32))
  (func $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E (type 3) (param i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    i32.const 1
    i32.store8 offset=24
    local.get 2
    local.get 1
    i32.store offset=20
    local.get 2
    local.get 0
    i32.store offset=16
    local.get 2
    i32.const 1055432
    i32.store offset=12
    local.get 2
    i32.const 1055224
    i32.store offset=8
    local.get 2
    i32.const 8
    i32.add
    call $rust_begin_unwind
    unreachable)
  (func $_ZN4core9panicking18panic_bounds_check17h2da60dc8be6aa455E (type 6) (param i32 i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    local.get 1
    i32.store offset=4
    local.get 3
    local.get 0
    i32.store
    local.get 3
    i32.const 28
    i32.add
    i32.const 2
    i32.store
    local.get 3
    i32.const 44
    i32.add
    i32.const 4
    i32.store
    local.get 3
    i64.const 2
    i64.store offset=12 align=4
    local.get 3
    i32.const 1055348
    i32.store offset=8
    local.get 3
    i32.const 4
    i32.store offset=36
    local.get 3
    local.get 3
    i32.const 32
    i32.add
    i32.store offset=24
    local.get 3
    local.get 3
    i32.store offset=40
    local.get 3
    local.get 3
    i32.const 4
    i32.add
    i32.store offset=32
    local.get 3
    i32.const 8
    i32.add
    local.get 2
    call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
    unreachable)
  (func $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E (type 6) (param i32 i32 i32)
    local.get 0
    local.get 1
    local.get 2
    call $_ZN4core5slice5index29slice_start_index_len_fail_rt17hea3fab39cd6dbb96E
    unreachable)
  (func $_ZN4core5slice5index24slice_end_index_len_fail17h8cef88544c34b3bfE (type 6) (param i32 i32 i32)
    local.get 0
    local.get 1
    local.get 2
    call $_ZN4core5slice5index27slice_end_index_len_fail_rt17ha5458ee76f940e9bE
    unreachable)
  (func $_ZN4core3fmt9Formatter3pad17h66985344eb075eb9E (type 7) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32)
    local.get 0
    i32.load offset=16
    local.set 3
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          i32.load offset=8
          local.tee 4
          i32.const 1
          i32.eq
          br_if 0 (;@3;)
          local.get 3
          i32.const 1
          i32.ne
          br_if 1 (;@2;)
        end
        block  ;; label = @3
          local.get 3
          i32.const 1
          i32.ne
          br_if 0 (;@3;)
          local.get 1
          local.get 2
          i32.add
          local.set 5
          local.get 0
          i32.const 20
          i32.add
          i32.load
          i32.const 1
          i32.add
          local.set 6
          i32.const 0
          local.set 7
          local.get 1
          local.set 8
          block  ;; label = @4
            loop  ;; label = @5
              local.get 8
              local.set 3
              local.get 6
              i32.const -1
              i32.add
              local.tee 6
              i32.eqz
              br_if 1 (;@4;)
              local.get 3
              local.get 5
              i32.eq
              br_if 2 (;@3;)
              block  ;; label = @6
                block  ;; label = @7
                  local.get 3
                  i32.load8_s
                  local.tee 9
                  i32.const -1
                  i32.le_s
                  br_if 0 (;@7;)
                  local.get 3
                  i32.const 1
                  i32.add
                  local.set 8
                  local.get 9
                  i32.const 255
                  i32.and
                  local.set 9
                  br 1 (;@6;)
                end
                local.get 3
                i32.load8_u offset=1
                i32.const 63
                i32.and
                local.set 8
                local.get 9
                i32.const 31
                i32.and
                local.set 10
                block  ;; label = @7
                  local.get 9
                  i32.const -33
                  i32.gt_u
                  br_if 0 (;@7;)
                  local.get 10
                  i32.const 6
                  i32.shl
                  local.get 8
                  i32.or
                  local.set 9
                  local.get 3
                  i32.const 2
                  i32.add
                  local.set 8
                  br 1 (;@6;)
                end
                local.get 8
                i32.const 6
                i32.shl
                local.get 3
                i32.load8_u offset=2
                i32.const 63
                i32.and
                i32.or
                local.set 8
                block  ;; label = @7
                  local.get 9
                  i32.const -16
                  i32.ge_u
                  br_if 0 (;@7;)
                  local.get 8
                  local.get 10
                  i32.const 12
                  i32.shl
                  i32.or
                  local.set 9
                  local.get 3
                  i32.const 3
                  i32.add
                  local.set 8
                  br 1 (;@6;)
                end
                local.get 8
                i32.const 6
                i32.shl
                local.get 3
                i32.load8_u offset=3
                i32.const 63
                i32.and
                i32.or
                local.get 10
                i32.const 18
                i32.shl
                i32.const 1835008
                i32.and
                i32.or
                local.tee 9
                i32.const 1114112
                i32.eq
                br_if 3 (;@3;)
                local.get 3
                i32.const 4
                i32.add
                local.set 8
              end
              local.get 7
              local.get 3
              i32.sub
              local.get 8
              i32.add
              local.set 7
              local.get 9
              i32.const 1114112
              i32.ne
              br_if 0 (;@5;)
              br 2 (;@3;)
            end
          end
          local.get 3
          local.get 5
          i32.eq
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 3
            i32.load8_s
            local.tee 8
            i32.const -1
            i32.gt_s
            br_if 0 (;@4;)
            local.get 8
            i32.const -32
            i32.lt_u
            br_if 0 (;@4;)
            local.get 8
            i32.const -16
            i32.lt_u
            br_if 0 (;@4;)
            local.get 3
            i32.load8_u offset=2
            i32.const 63
            i32.and
            i32.const 6
            i32.shl
            local.get 3
            i32.load8_u offset=1
            i32.const 63
            i32.and
            i32.const 12
            i32.shl
            i32.or
            local.get 3
            i32.load8_u offset=3
            i32.const 63
            i32.and
            i32.or
            local.get 8
            i32.const 255
            i32.and
            i32.const 18
            i32.shl
            i32.const 1835008
            i32.and
            i32.or
            i32.const 1114112
            i32.eq
            br_if 1 (;@3;)
          end
          block  ;; label = @4
            block  ;; label = @5
              local.get 7
              i32.eqz
              br_if 0 (;@5;)
              block  ;; label = @6
                local.get 7
                local.get 2
                i32.lt_u
                br_if 0 (;@6;)
                i32.const 0
                local.set 3
                local.get 7
                local.get 2
                i32.eq
                br_if 1 (;@5;)
                br 2 (;@4;)
              end
              i32.const 0
              local.set 3
              local.get 1
              local.get 7
              i32.add
              i32.load8_s
              i32.const -64
              i32.lt_s
              br_if 1 (;@4;)
            end
            local.get 1
            local.set 3
          end
          local.get 7
          local.get 2
          local.get 3
          select
          local.set 2
          local.get 3
          local.get 1
          local.get 3
          select
          local.set 1
        end
        block  ;; label = @3
          local.get 4
          br_if 0 (;@3;)
          local.get 0
          i32.load offset=24
          local.get 1
          local.get 2
          local.get 0
          i32.const 28
          i32.add
          i32.load
          i32.load offset=12
          call_indirect (type 7)
          return
        end
        local.get 0
        i32.const 12
        i32.add
        i32.load
        local.set 7
        block  ;; label = @3
          block  ;; label = @4
            local.get 2
            i32.const 16
            i32.lt_u
            br_if 0 (;@4;)
            local.get 1
            local.get 2
            call $_ZN4core3str5count14do_count_chars17hc6a12b510bd99c00E
            local.set 8
            br 1 (;@3;)
          end
          block  ;; label = @4
            local.get 2
            br_if 0 (;@4;)
            i32.const 0
            local.set 8
            br 1 (;@3;)
          end
          local.get 2
          i32.const 3
          i32.and
          local.set 9
          block  ;; label = @4
            block  ;; label = @5
              local.get 2
              i32.const -1
              i32.add
              i32.const 3
              i32.ge_u
              br_if 0 (;@5;)
              i32.const 0
              local.set 8
              local.get 1
              local.set 3
              br 1 (;@4;)
            end
            local.get 2
            i32.const -4
            i32.and
            local.set 6
            i32.const 0
            local.set 8
            local.get 1
            local.set 3
            loop  ;; label = @5
              local.get 8
              local.get 3
              i32.load8_s
              i32.const -65
              i32.gt_s
              i32.add
              local.get 3
              i32.load8_s offset=1
              i32.const -65
              i32.gt_s
              i32.add
              local.get 3
              i32.load8_s offset=2
              i32.const -65
              i32.gt_s
              i32.add
              local.get 3
              i32.load8_s offset=3
              i32.const -65
              i32.gt_s
              i32.add
              local.set 8
              local.get 3
              i32.const 4
              i32.add
              local.set 3
              local.get 6
              i32.const -4
              i32.add
              local.tee 6
              br_if 0 (;@5;)
            end
          end
          local.get 9
          i32.eqz
          br_if 0 (;@3;)
          loop  ;; label = @4
            local.get 8
            local.get 3
            i32.load8_s
            i32.const -65
            i32.gt_s
            i32.add
            local.set 8
            local.get 3
            i32.const 1
            i32.add
            local.set 3
            local.get 9
            i32.const -1
            i32.add
            local.tee 9
            br_if 0 (;@4;)
          end
        end
        block  ;; label = @3
          local.get 7
          local.get 8
          i32.le_u
          br_if 0 (;@3;)
          local.get 7
          local.get 8
          i32.sub
          local.tee 8
          local.set 7
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                i32.const 0
                local.get 0
                i32.load8_u offset=32
                local.tee 3
                local.get 3
                i32.const 3
                i32.eq
                select
                i32.const 3
                i32.and
                local.tee 3
                br_table 2 (;@4;) 0 (;@6;) 1 (;@5;) 2 (;@4;)
              end
              i32.const 0
              local.set 7
              local.get 8
              local.set 3
              br 1 (;@4;)
            end
            local.get 8
            i32.const 1
            i32.shr_u
            local.set 3
            local.get 8
            i32.const 1
            i32.add
            i32.const 1
            i32.shr_u
            local.set 7
          end
          local.get 3
          i32.const 1
          i32.add
          local.set 3
          local.get 0
          i32.const 28
          i32.add
          i32.load
          local.set 9
          local.get 0
          i32.const 24
          i32.add
          i32.load
          local.set 6
          local.get 0
          i32.load offset=4
          local.set 8
          block  ;; label = @4
            loop  ;; label = @5
              local.get 3
              i32.const -1
              i32.add
              local.tee 3
              i32.eqz
              br_if 1 (;@4;)
              local.get 6
              local.get 8
              local.get 9
              i32.load offset=16
              call_indirect (type 5)
              i32.eqz
              br_if 0 (;@5;)
            end
            i32.const 1
            return
          end
          i32.const 1
          local.set 3
          local.get 8
          i32.const 1114112
          i32.eq
          br_if 2 (;@1;)
          local.get 6
          local.get 1
          local.get 2
          local.get 9
          i32.load offset=12
          call_indirect (type 7)
          br_if 2 (;@1;)
          i32.const 0
          local.set 3
          loop  ;; label = @4
            block  ;; label = @5
              local.get 7
              local.get 3
              i32.ne
              br_if 0 (;@5;)
              local.get 7
              local.get 7
              i32.lt_u
              return
            end
            local.get 3
            i32.const 1
            i32.add
            local.set 3
            local.get 6
            local.get 8
            local.get 9
            i32.load offset=16
            call_indirect (type 5)
            i32.eqz
            br_if 0 (;@4;)
          end
          local.get 3
          i32.const -1
          i32.add
          local.get 7
          i32.lt_u
          return
        end
        local.get 0
        i32.load offset=24
        local.get 1
        local.get 2
        local.get 0
        i32.const 28
        i32.add
        i32.load
        i32.load offset=12
        call_indirect (type 7)
        return
      end
      local.get 0
      i32.load offset=24
      local.get 1
      local.get 2
      local.get 0
      i32.const 28
      i32.add
      i32.load
      i32.load offset=12
      call_indirect (type 7)
      local.set 3
    end
    local.get 3)
  (func $_ZN4core9panicking5panic17h107cfc2f46b7acacE (type 6) (param i32 i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    i32.const 20
    i32.add
    i32.const 0
    i32.store
    local.get 3
    i32.const 1055224
    i32.store offset=16
    local.get 3
    i64.const 1
    i64.store offset=4 align=4
    local.get 3
    local.get 1
    i32.store offset=28
    local.get 3
    local.get 0
    i32.store offset=24
    local.get 3
    local.get 3
    i32.const 24
    i32.add
    i32.store
    local.get 3
    local.get 2
    call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
    unreachable)
  (func $_ZN4core5slice5index22slice_index_order_fail17h6f9c6cfade4b2695E (type 6) (param i32 i32 i32)
    local.get 0
    local.get 1
    local.get 2
    call $_ZN4core5slice5index25slice_index_order_fail_rt17hd4b3fe4810f4fc3bE
    unreachable)
  (func $_ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$3fmt17h438b5cc0efea7d46E (type 5) (param i32 i32) (result i32)
    local.get 0
    i64.load32_u
    i32.const 1
    local.get 1
    call $_ZN4core3fmt3num3imp7fmt_u6417h5750e7de7c1be42fE)
  (func $_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h9ea99fd84ac4386dE (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 128
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 1
              i32.load
              local.tee 3
              i32.const 16
              i32.and
              br_if 0 (;@5;)
              local.get 3
              i32.const 32
              i32.and
              br_if 1 (;@4;)
              local.get 0
              i64.load32_u
              i32.const 1
              local.get 1
              call $_ZN4core3fmt3num3imp7fmt_u6417h5750e7de7c1be42fE
              local.set 0
              br 4 (;@1;)
            end
            local.get 0
            i32.load
            local.set 0
            i32.const 0
            local.set 3
            loop  ;; label = @5
              local.get 2
              local.get 3
              i32.add
              i32.const 127
              i32.add
              i32.const 48
              i32.const 87
              local.get 0
              i32.const 15
              i32.and
              local.tee 4
              i32.const 10
              i32.lt_u
              select
              local.get 4
              i32.add
              i32.store8
              local.get 3
              i32.const -1
              i32.add
              local.set 3
              local.get 0
              i32.const 15
              i32.gt_u
              local.set 4
              local.get 0
              i32.const 4
              i32.shr_u
              local.set 0
              local.get 4
              br_if 0 (;@5;)
            end
            local.get 3
            i32.const 128
            i32.add
            local.tee 0
            i32.const 129
            i32.ge_u
            br_if 1 (;@3;)
            local.get 1
            i32.const 1
            i32.const 1055732
            i32.const 2
            local.get 2
            local.get 3
            i32.add
            i32.const 128
            i32.add
            i32.const 0
            local.get 3
            i32.sub
            call $_ZN4core3fmt9Formatter12pad_integral17h8d163aacc395a3d9E
            local.set 0
            br 3 (;@1;)
          end
          local.get 0
          i32.load
          local.set 0
          i32.const 0
          local.set 3
          loop  ;; label = @4
            local.get 2
            local.get 3
            i32.add
            i32.const 127
            i32.add
            i32.const 48
            i32.const 55
            local.get 0
            i32.const 15
            i32.and
            local.tee 4
            i32.const 10
            i32.lt_u
            select
            local.get 4
            i32.add
            i32.store8
            local.get 3
            i32.const -1
            i32.add
            local.set 3
            local.get 0
            i32.const 15
            i32.gt_u
            local.set 4
            local.get 0
            i32.const 4
            i32.shr_u
            local.set 0
            local.get 4
            br_if 0 (;@4;)
          end
          local.get 3
          i32.const 128
          i32.add
          local.tee 0
          i32.const 129
          i32.ge_u
          br_if 1 (;@2;)
          local.get 1
          i32.const 1
          i32.const 1055732
          i32.const 2
          local.get 2
          local.get 3
          i32.add
          i32.const 128
          i32.add
          i32.const 0
          local.get 3
          i32.sub
          call $_ZN4core3fmt9Formatter12pad_integral17h8d163aacc395a3d9E
          local.set 0
          br 2 (;@1;)
        end
        local.get 0
        i32.const 128
        i32.const 1055716
        call $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E
        unreachable
      end
      local.get 0
      i32.const 128
      i32.const 1055716
      call $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E
      unreachable
    end
    local.get 2
    i32.const 128
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core3fmt5write17hb15fae9d0358166aE (type 7) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    i32.const 36
    i32.add
    local.get 1
    i32.store
    local.get 3
    i32.const 3
    i32.store8 offset=40
    local.get 3
    i64.const 137438953472
    i64.store offset=8
    local.get 3
    local.get 0
    i32.store offset=32
    i32.const 0
    local.set 4
    local.get 3
    i32.const 0
    i32.store offset=24
    local.get 3
    i32.const 0
    i32.store offset=16
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 2
            i32.load offset=8
            local.tee 5
            br_if 0 (;@4;)
            local.get 2
            i32.const 20
            i32.add
            i32.load
            local.tee 0
            i32.eqz
            br_if 1 (;@3;)
            local.get 2
            i32.load offset=16
            local.set 1
            local.get 0
            i32.const 3
            i32.shl
            local.set 6
            local.get 0
            i32.const -1
            i32.add
            i32.const 536870911
            i32.and
            i32.const 1
            i32.add
            local.set 4
            local.get 2
            i32.load
            local.set 0
            loop  ;; label = @5
              block  ;; label = @6
                local.get 0
                i32.const 4
                i32.add
                i32.load
                local.tee 7
                i32.eqz
                br_if 0 (;@6;)
                local.get 3
                i32.load offset=32
                local.get 0
                i32.load
                local.get 7
                local.get 3
                i32.load offset=36
                i32.load offset=12
                call_indirect (type 7)
                br_if 4 (;@2;)
              end
              local.get 1
              i32.load
              local.get 3
              i32.const 8
              i32.add
              local.get 1
              i32.const 4
              i32.add
              i32.load
              call_indirect (type 5)
              br_if 3 (;@2;)
              local.get 1
              i32.const 8
              i32.add
              local.set 1
              local.get 0
              i32.const 8
              i32.add
              local.set 0
              local.get 6
              i32.const -8
              i32.add
              local.tee 6
              br_if 0 (;@5;)
              br 2 (;@3;)
            end
          end
          local.get 2
          i32.const 12
          i32.add
          i32.load
          local.tee 1
          i32.eqz
          br_if 0 (;@3;)
          local.get 1
          i32.const 5
          i32.shl
          local.set 8
          local.get 1
          i32.const -1
          i32.add
          i32.const 134217727
          i32.and
          i32.const 1
          i32.add
          local.set 4
          local.get 2
          i32.load
          local.set 0
          i32.const 0
          local.set 6
          loop  ;; label = @4
            block  ;; label = @5
              local.get 0
              i32.const 4
              i32.add
              i32.load
              local.tee 1
              i32.eqz
              br_if 0 (;@5;)
              local.get 3
              i32.load offset=32
              local.get 0
              i32.load
              local.get 1
              local.get 3
              i32.load offset=36
              i32.load offset=12
              call_indirect (type 7)
              br_if 3 (;@2;)
            end
            local.get 3
            local.get 5
            local.get 6
            i32.add
            local.tee 1
            i32.const 28
            i32.add
            i32.load8_u
            i32.store8 offset=40
            local.get 3
            local.get 1
            i32.const 4
            i32.add
            i64.load align=4
            i64.const 32
            i64.rotl
            i64.store offset=8
            local.get 1
            i32.const 24
            i32.add
            i32.load
            local.set 9
            local.get 2
            i32.load offset=16
            local.set 10
            i32.const 0
            local.set 11
            i32.const 0
            local.set 7
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 1
                  i32.const 20
                  i32.add
                  i32.load
                  br_table 1 (;@6;) 0 (;@7;) 2 (;@5;) 1 (;@6;)
                end
                local.get 9
                i32.const 3
                i32.shl
                local.set 12
                i32.const 0
                local.set 7
                local.get 10
                local.get 12
                i32.add
                local.tee 12
                i32.const 4
                i32.add
                i32.load
                i32.const 69
                i32.ne
                br_if 1 (;@5;)
                local.get 12
                i32.load
                i32.load
                local.set 9
              end
              i32.const 1
              local.set 7
            end
            local.get 3
            local.get 9
            i32.store offset=20
            local.get 3
            local.get 7
            i32.store offset=16
            local.get 1
            i32.const 16
            i32.add
            i32.load
            local.set 7
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 1
                  i32.const 12
                  i32.add
                  i32.load
                  br_table 1 (;@6;) 0 (;@7;) 2 (;@5;) 1 (;@6;)
                end
                local.get 7
                i32.const 3
                i32.shl
                local.set 9
                local.get 10
                local.get 9
                i32.add
                local.tee 9
                i32.const 4
                i32.add
                i32.load
                i32.const 69
                i32.ne
                br_if 1 (;@5;)
                local.get 9
                i32.load
                i32.load
                local.set 7
              end
              i32.const 1
              local.set 11
            end
            local.get 3
            local.get 7
            i32.store offset=28
            local.get 3
            local.get 11
            i32.store offset=24
            local.get 10
            local.get 1
            i32.load
            i32.const 3
            i32.shl
            i32.add
            local.tee 1
            i32.load
            local.get 3
            i32.const 8
            i32.add
            local.get 1
            i32.load offset=4
            call_indirect (type 5)
            br_if 2 (;@2;)
            local.get 0
            i32.const 8
            i32.add
            local.set 0
            local.get 8
            local.get 6
            i32.const 32
            i32.add
            local.tee 6
            i32.ne
            br_if 0 (;@4;)
          end
        end
        block  ;; label = @3
          local.get 4
          local.get 2
          i32.load offset=4
          i32.ge_u
          br_if 0 (;@3;)
          local.get 3
          i32.load offset=32
          local.get 2
          i32.load
          local.get 4
          i32.const 3
          i32.shl
          i32.add
          local.tee 1
          i32.load
          local.get 1
          i32.load offset=4
          local.get 3
          i32.load offset=36
          i32.load offset=12
          call_indirect (type 7)
          br_if 1 (;@2;)
        end
        i32.const 0
        local.set 1
        br 1 (;@1;)
      end
      i32.const 1
      local.set 1
    end
    local.get 3
    i32.const 48
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN71_$LT$core..ops..range..Range$LT$Idx$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17hdc112323110892d3E (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    i32.const 1
    local.set 3
    block  ;; label = @1
      local.get 0
      local.get 1
      call $_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h9ea99fd84ac4386dE
      br_if 0 (;@1;)
      local.get 1
      i32.const 28
      i32.add
      i32.load
      local.set 4
      local.get 1
      i32.load offset=24
      local.set 5
      local.get 2
      i32.const 0
      i32.store offset=28
      local.get 2
      i32.const 1055224
      i32.store offset=24
      local.get 2
      i64.const 1
      i64.store offset=12 align=4
      local.get 2
      i32.const 1055272
      i32.store offset=8
      local.get 5
      local.get 4
      local.get 2
      i32.const 8
      i32.add
      call $_ZN4core3fmt5write17hb15fae9d0358166aE
      br_if 0 (;@1;)
      local.get 0
      i32.const 4
      i32.add
      local.get 1
      call $_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h9ea99fd84ac4386dE
      local.set 3
    end
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 3)
  (func $_ZN36_$LT$T$u20$as$u20$core..any..Any$GT$7type_id17h6faeb6b1367efc9eE (type 2) (param i32) (result i64)
    i64.const -4464899901473429205)
  (func $_ZN63_$LT$core..cell..BorrowMutError$u20$as$u20$core..fmt..Debug$GT$3fmt17h76796ccdb5fb4613E (type 5) (param i32 i32) (result i32)
    local.get 1
    i32.load offset=24
    i32.const 1055280
    i32.const 14
    local.get 1
    i32.const 28
    i32.add
    i32.load
    i32.load offset=12
    call_indirect (type 7))
  (func $_ZN4core5slice6memchr14memchr_aligned17ha15e5d65ea28ac67E (type 9) (param i32 i32 i32 i32)
    (local i32 i32 i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 2
            i32.const 3
            i32.add
            i32.const -4
            i32.and
            local.tee 4
            local.get 2
            i32.eq
            br_if 0 (;@4;)
            local.get 4
            local.get 2
            i32.sub
            local.tee 4
            local.get 3
            local.get 4
            local.get 3
            i32.lt_u
            select
            local.tee 4
            i32.eqz
            br_if 0 (;@4;)
            i32.const 0
            local.set 5
            local.get 1
            i32.const 255
            i32.and
            local.set 6
            i32.const 1
            local.set 7
            loop  ;; label = @5
              local.get 2
              local.get 5
              i32.add
              i32.load8_u
              local.get 6
              i32.eq
              br_if 4 (;@1;)
              local.get 4
              local.get 5
              i32.const 1
              i32.add
              local.tee 5
              i32.ne
              br_if 0 (;@5;)
            end
            local.get 4
            local.get 3
            i32.const -8
            i32.add
            local.tee 8
            i32.gt_u
            br_if 2 (;@2;)
            br 1 (;@3;)
          end
          local.get 3
          i32.const -8
          i32.add
          local.set 8
          i32.const 0
          local.set 4
        end
        local.get 1
        i32.const 255
        i32.and
        i32.const 16843009
        i32.mul
        local.set 5
        block  ;; label = @3
          loop  ;; label = @4
            local.get 2
            local.get 4
            i32.add
            local.tee 7
            i32.load
            local.get 5
            i32.xor
            local.tee 6
            i32.const -1
            i32.xor
            local.get 6
            i32.const -16843009
            i32.add
            i32.and
            i32.const -2139062144
            i32.and
            br_if 1 (;@3;)
            local.get 7
            i32.const 4
            i32.add
            i32.load
            local.get 5
            i32.xor
            local.tee 6
            i32.const -1
            i32.xor
            local.get 6
            i32.const -16843009
            i32.add
            i32.and
            i32.const -2139062144
            i32.and
            br_if 1 (;@3;)
            local.get 4
            i32.const 8
            i32.add
            local.tee 4
            local.get 8
            i32.le_u
            br_if 0 (;@4;)
          end
        end
        local.get 4
        local.get 3
        i32.le_u
        br_if 0 (;@2;)
        local.get 4
        local.get 3
        i32.const 1056004
        call $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E
        unreachable
      end
      i32.const 0
      local.set 7
      block  ;; label = @2
        local.get 4
        local.get 3
        i32.eq
        br_if 0 (;@2;)
        local.get 1
        i32.const 255
        i32.and
        local.set 5
        loop  ;; label = @3
          block  ;; label = @4
            local.get 2
            local.get 4
            i32.add
            i32.load8_u
            local.get 5
            i32.ne
            br_if 0 (;@4;)
            local.get 4
            local.set 5
            i32.const 1
            local.set 7
            br 3 (;@1;)
          end
          local.get 3
          local.get 4
          i32.const 1
          i32.add
          local.tee 4
          i32.ne
          br_if 0 (;@3;)
        end
      end
      local.get 3
      local.set 5
    end
    local.get 0
    local.get 5
    i32.store offset=4
    local.get 0
    local.get 7
    i32.store)
  (func $_ZN4core3str8converts9from_utf817he9ed2de3b80ea750E (type 6) (param i32 i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      local.get 2
      i32.eqz
      br_if 0 (;@1;)
      i32.const 0
      local.get 2
      i32.const -7
      i32.add
      local.tee 3
      local.get 3
      local.get 2
      i32.gt_u
      select
      local.set 4
      local.get 1
      i32.const 3
      i32.add
      i32.const -4
      i32.and
      local.get 1
      i32.sub
      local.set 5
      i32.const 0
      local.set 3
      loop  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            local.get 1
                            local.get 3
                            i32.add
                            i32.load8_u
                            local.tee 6
                            i32.const 24
                            i32.shl
                            i32.const 24
                            i32.shr_s
                            local.tee 7
                            i32.const 0
                            i32.lt_s
                            br_if 0 (;@12;)
                            local.get 5
                            i32.const -1
                            i32.eq
                            br_if 1 (;@11;)
                            local.get 5
                            local.get 3
                            i32.sub
                            i32.const 3
                            i32.and
                            br_if 1 (;@11;)
                            local.get 3
                            local.get 4
                            i32.lt_u
                            br_if 2 (;@10;)
                            br 8 (;@4;)
                          end
                          i32.const 1
                          local.set 8
                          i32.const 1
                          local.set 9
                          block  ;; label = @12
                            block  ;; label = @13
                              block  ;; label = @14
                                block  ;; label = @15
                                  block  ;; label = @16
                                    block  ;; label = @17
                                      block  ;; label = @18
                                        block  ;; label = @19
                                          local.get 6
                                          i32.const 1056204
                                          i32.add
                                          i32.load8_u
                                          i32.const -2
                                          i32.add
                                          br_table 0 (;@19;) 1 (;@18;) 2 (;@17;) 14 (;@5;)
                                        end
                                        local.get 3
                                        i32.const 1
                                        i32.add
                                        local.tee 6
                                        local.get 2
                                        i32.lt_u
                                        br_if 6 (;@12;)
                                        i32.const 0
                                        local.set 9
                                        br 13 (;@5;)
                                      end
                                      i32.const 1
                                      local.set 8
                                      i32.const 0
                                      local.set 9
                                      local.get 3
                                      i32.const 1
                                      i32.add
                                      local.tee 10
                                      local.get 2
                                      i32.ge_u
                                      br_if 12 (;@5;)
                                      local.get 1
                                      local.get 10
                                      i32.add
                                      i32.load8_s
                                      local.set 10
                                      local.get 6
                                      i32.const -224
                                      i32.add
                                      br_table 1 (;@16;) 3 (;@14;) 3 (;@14;) 3 (;@14;) 3 (;@14;) 3 (;@14;) 3 (;@14;) 3 (;@14;) 3 (;@14;) 3 (;@14;) 3 (;@14;) 3 (;@14;) 3 (;@14;) 2 (;@15;) 3 (;@14;)
                                    end
                                    i32.const 1
                                    local.set 8
                                    block  ;; label = @17
                                      local.get 3
                                      i32.const 1
                                      i32.add
                                      local.tee 9
                                      local.get 2
                                      i32.lt_u
                                      br_if 0 (;@17;)
                                      i32.const 0
                                      local.set 9
                                      br 12 (;@5;)
                                    end
                                    local.get 1
                                    local.get 9
                                    i32.add
                                    i32.load8_s
                                    local.set 10
                                    block  ;; label = @17
                                      block  ;; label = @18
                                        block  ;; label = @19
                                          local.get 6
                                          i32.const -240
                                          i32.add
                                          br_table 1 (;@18;) 0 (;@19;) 0 (;@19;) 0 (;@19;) 2 (;@17;) 0 (;@19;)
                                        end
                                        i32.const 1
                                        local.set 8
                                        block  ;; label = @19
                                          local.get 7
                                          i32.const 15
                                          i32.add
                                          i32.const 255
                                          i32.and
                                          i32.const 2
                                          i32.le_u
                                          br_if 0 (;@19;)
                                          i32.const 1
                                          local.set 9
                                          br 14 (;@5;)
                                        end
                                        local.get 10
                                        i32.const -1
                                        i32.le_s
                                        br_if 9 (;@9;)
                                        i32.const 1
                                        local.set 9
                                        br 13 (;@5;)
                                      end
                                      local.get 10
                                      i32.const 112
                                      i32.add
                                      i32.const 255
                                      i32.and
                                      i32.const 48
                                      i32.lt_u
                                      br_if 9 (;@8;)
                                      br 11 (;@6;)
                                    end
                                    local.get 10
                                    i32.const -113
                                    i32.gt_s
                                    br_if 10 (;@6;)
                                    br 8 (;@8;)
                                  end
                                  local.get 10
                                  i32.const -32
                                  i32.and
                                  i32.const -96
                                  i32.ne
                                  br_if 9 (;@6;)
                                  br 2 (;@13;)
                                end
                                local.get 10
                                i32.const -96
                                i32.ge_s
                                br_if 8 (;@6;)
                                br 1 (;@13;)
                              end
                              block  ;; label = @14
                                block  ;; label = @15
                                  local.get 7
                                  i32.const 31
                                  i32.add
                                  i32.const 255
                                  i32.and
                                  i32.const 12
                                  i32.lt_u
                                  br_if 0 (;@15;)
                                  i32.const 1
                                  local.set 8
                                  block  ;; label = @16
                                    local.get 7
                                    i32.const -2
                                    i32.and
                                    i32.const -18
                                    i32.eq
                                    br_if 0 (;@16;)
                                    i32.const 1
                                    local.set 9
                                    br 11 (;@5;)
                                  end
                                  local.get 10
                                  i32.const -1
                                  i32.le_s
                                  br_if 1 (;@14;)
                                  i32.const 1
                                  local.set 9
                                  br 10 (;@5;)
                                end
                                local.get 10
                                i32.const -65
                                i32.gt_s
                                br_if 8 (;@6;)
                                br 1 (;@13;)
                              end
                              i32.const 1
                              local.set 9
                              local.get 10
                              i32.const -64
                              i32.ge_u
                              br_if 8 (;@5;)
                            end
                            i32.const 0
                            local.set 9
                            local.get 3
                            i32.const 2
                            i32.add
                            local.tee 6
                            local.get 2
                            i32.ge_u
                            br_if 7 (;@5;)
                            local.get 1
                            local.get 6
                            i32.add
                            i32.load8_s
                            i32.const -65
                            i32.le_s
                            br_if 5 (;@7;)
                            i32.const 1
                            local.set 9
                            i32.const 2
                            local.set 8
                            br 7 (;@5;)
                          end
                          local.get 1
                          local.get 6
                          i32.add
                          i32.load8_s
                          i32.const -65
                          i32.gt_s
                          br_if 5 (;@6;)
                          br 4 (;@7;)
                        end
                        local.get 3
                        i32.const 1
                        i32.add
                        local.set 3
                        br 7 (;@3;)
                      end
                      loop  ;; label = @10
                        local.get 1
                        local.get 3
                        i32.add
                        local.tee 6
                        i32.load
                        i32.const -2139062144
                        i32.and
                        br_if 6 (;@4;)
                        local.get 6
                        i32.const 4
                        i32.add
                        i32.load
                        i32.const -2139062144
                        i32.and
                        br_if 6 (;@4;)
                        local.get 3
                        i32.const 8
                        i32.add
                        local.tee 3
                        local.get 4
                        i32.ge_u
                        br_if 6 (;@4;)
                        br 0 (;@10;)
                      end
                    end
                    i32.const 1
                    local.set 9
                    local.get 10
                    i32.const -64
                    i32.ge_u
                    br_if 3 (;@5;)
                  end
                  block  ;; label = @8
                    local.get 3
                    i32.const 2
                    i32.add
                    local.tee 6
                    local.get 2
                    i32.lt_u
                    br_if 0 (;@8;)
                    i32.const 0
                    local.set 9
                    br 3 (;@5;)
                  end
                  block  ;; label = @8
                    local.get 1
                    local.get 6
                    i32.add
                    i32.load8_s
                    i32.const -65
                    i32.le_s
                    br_if 0 (;@8;)
                    i32.const 2
                    local.set 8
                    i32.const 1
                    local.set 9
                    br 3 (;@5;)
                  end
                  i32.const 0
                  local.set 9
                  local.get 3
                  i32.const 3
                  i32.add
                  local.tee 6
                  local.get 2
                  i32.ge_u
                  br_if 2 (;@5;)
                  local.get 1
                  local.get 6
                  i32.add
                  i32.load8_s
                  i32.const -65
                  i32.le_s
                  br_if 0 (;@7;)
                  i32.const 3
                  local.set 8
                  i32.const 1
                  local.set 9
                  br 2 (;@5;)
                end
                local.get 6
                i32.const 1
                i32.add
                local.set 3
                br 3 (;@3;)
              end
              i32.const 1
              local.set 8
              i32.const 1
              local.set 9
            end
            local.get 0
            local.get 3
            i32.store offset=4
            local.get 0
            i32.const 9
            i32.add
            local.get 8
            i32.store8
            local.get 0
            i32.const 8
            i32.add
            local.get 9
            i32.store8
            local.get 0
            i32.const 1
            i32.store
            return
          end
          local.get 3
          local.get 2
          i32.ge_u
          br_if 0 (;@3;)
          loop  ;; label = @4
            local.get 1
            local.get 3
            i32.add
            i32.load8_s
            i32.const 0
            i32.lt_s
            br_if 1 (;@3;)
            local.get 2
            local.get 3
            i32.const 1
            i32.add
            local.tee 3
            i32.ne
            br_if 0 (;@4;)
            br 3 (;@1;)
          end
        end
        local.get 3
        local.get 2
        i32.lt_u
        br_if 0 (;@2;)
      end
    end
    local.get 0
    local.get 1
    i32.store offset=4
    local.get 0
    i32.const 8
    i32.add
    local.get 2
    i32.store
    local.get 0
    i32.const 0
    i32.store)
  (func $_ZN4core3fmt8builders11DebugStruct5field17h6758fe7581d6cfb8E (type 12) (param i32 i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i64 i64)
    global.get $__stack_pointer
    i32.const 64
    i32.sub
    local.tee 5
    global.set $__stack_pointer
    i32.const 1
    local.set 6
    block  ;; label = @1
      local.get 0
      i32.load8_u offset=4
      br_if 0 (;@1;)
      local.get 0
      i32.load8_u offset=5
      local.set 7
      block  ;; label = @2
        local.get 0
        i32.load
        local.tee 8
        i32.load
        local.tee 9
        i32.const 4
        i32.and
        br_if 0 (;@2;)
        i32.const 1
        local.set 6
        local.get 8
        i32.load offset=24
        i32.const 1055641
        i32.const 1055643
        local.get 7
        i32.const 255
        i32.and
        local.tee 7
        select
        i32.const 2
        i32.const 3
        local.get 7
        select
        local.get 8
        i32.const 28
        i32.add
        i32.load
        i32.load offset=12
        call_indirect (type 7)
        br_if 1 (;@1;)
        i32.const 1
        local.set 6
        local.get 8
        i32.load offset=24
        local.get 1
        local.get 2
        local.get 8
        i32.load offset=28
        i32.load offset=12
        call_indirect (type 7)
        br_if 1 (;@1;)
        i32.const 1
        local.set 6
        local.get 8
        i32.load offset=24
        i32.const 1055588
        i32.const 2
        local.get 8
        i32.load offset=28
        i32.load offset=12
        call_indirect (type 7)
        br_if 1 (;@1;)
        local.get 3
        local.get 8
        local.get 4
        i32.load offset=12
        call_indirect (type 5)
        local.set 6
        br 1 (;@1;)
      end
      block  ;; label = @2
        local.get 7
        i32.const 255
        i32.and
        br_if 0 (;@2;)
        i32.const 1
        local.set 6
        local.get 8
        i32.load offset=24
        i32.const 1055636
        i32.const 3
        local.get 8
        i32.const 28
        i32.add
        i32.load
        i32.load offset=12
        call_indirect (type 7)
        br_if 1 (;@1;)
        local.get 8
        i32.load
        local.set 9
      end
      i32.const 1
      local.set 6
      local.get 5
      i32.const 1
      i32.store8 offset=23
      local.get 5
      i32.const 52
      i32.add
      i32.const 1055608
      i32.store
      local.get 5
      local.get 9
      i32.store offset=24
      local.get 5
      local.get 8
      i64.load offset=24 align=4
      i64.store offset=8
      local.get 5
      local.get 5
      i32.const 23
      i32.add
      i32.store offset=16
      local.get 8
      i64.load offset=8 align=4
      local.set 10
      local.get 8
      i64.load offset=16 align=4
      local.set 11
      local.get 5
      local.get 8
      i32.load8_u offset=32
      i32.store8 offset=56
      local.get 5
      local.get 8
      i32.load offset=4
      i32.store offset=28
      local.get 5
      local.get 11
      i64.store offset=40
      local.get 5
      local.get 10
      i64.store offset=32
      local.get 5
      local.get 5
      i32.const 8
      i32.add
      i32.store offset=48
      local.get 5
      i32.const 8
      i32.add
      local.get 1
      local.get 2
      call $_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hf0e70d48679f35b2E
      br_if 0 (;@1;)
      local.get 5
      i32.const 8
      i32.add
      i32.const 1055588
      i32.const 2
      call $_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hf0e70d48679f35b2E
      br_if 0 (;@1;)
      local.get 3
      local.get 5
      i32.const 24
      i32.add
      local.get 4
      i32.load offset=12
      call_indirect (type 5)
      br_if 0 (;@1;)
      local.get 5
      i32.load offset=48
      i32.const 1055639
      i32.const 2
      local.get 5
      i32.load offset=52
      i32.load offset=12
      call_indirect (type 7)
      local.set 6
    end
    local.get 0
    i32.const 1
    i32.store8 offset=5
    local.get 0
    local.get 6
    i32.store8 offset=4
    local.get 5
    i32.const 64
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core6option13expect_failed17hc52795d163105200E (type 6) (param i32 i32 i32)
    local.get 0
    local.get 1
    local.get 2
    call $_ZN4core9panicking9panic_str17h8aa7e8c542e5727bE
    unreachable)
  (func $_ZN4core9panicking9panic_str17h8aa7e8c542e5727bE (type 6) (param i32 i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    local.get 1
    i32.store offset=12
    local.get 3
    local.get 0
    i32.store offset=8
    local.get 3
    i32.const 8
    i32.add
    local.get 2
    call $_ZN4core9panicking13panic_display17h8048e74e95fcae8aE
    unreachable)
  (func $_ZN70_$LT$core..panic..location..Location$u20$as$u20$core..fmt..Display$GT$3fmt17h7a2d773550b3a249E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get 2
    i32.const 12
    i32.add
    i32.const 4
    i32.store
    local.get 2
    i32.const 70
    i32.store offset=4
    local.get 2
    local.get 0
    i32.store
    local.get 2
    local.get 0
    i32.const 12
    i32.add
    i32.store offset=16
    local.get 2
    local.get 0
    i32.const 8
    i32.add
    i32.store offset=8
    local.get 1
    i32.const 28
    i32.add
    i32.load
    local.set 0
    local.get 1
    i32.load offset=24
    local.set 1
    local.get 2
    i32.const 3
    i32.store offset=44
    local.get 2
    i64.const 3
    i64.store offset=28 align=4
    local.get 2
    i32.const 1055368
    i32.store offset=24
    local.get 2
    local.get 2
    i32.store offset=40
    local.get 1
    local.get 0
    local.get 2
    i32.const 24
    i32.add
    call $_ZN4core3fmt5write17hb15fae9d0358166aE
    local.set 0
    local.get 2
    i32.const 48
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h6994d0e2455e539bE (type 5) (param i32 i32) (result i32)
    local.get 1
    local.get 0
    i32.load
    local.get 0
    i32.load offset=4
    call $_ZN4core3fmt9Formatter3pad17h66985344eb075eb9E)
  (func $_ZN4core5panic10panic_info9PanicInfo7payload17hfd9d0ca104403304E (type 3) (param i32 i32)
    local.get 0
    local.get 1
    i64.load align=4
    i64.store)
  (func $_ZN4core5panic10panic_info9PanicInfo7message17hbe3048ebb862958cE (type 4) (param i32) (result i32)
    local.get 0
    i32.load offset=8)
  (func $_ZN4core5panic10panic_info9PanicInfo8location17he55e718ad5bfc141E (type 4) (param i32) (result i32)
    local.get 0
    i32.load offset=12)
  (func $_ZN4core5panic10panic_info9PanicInfo10can_unwind17hb60e365aaef4a33bE (type 4) (param i32) (result i32)
    local.get 0
    i32.load8_u offset=16)
  (func $_ZN73_$LT$core..panic..panic_info..PanicInfo$u20$as$u20$core..fmt..Display$GT$3fmt17ha7d3bd671660d3d8E (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 64
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    i32.const 1
    local.set 3
    block  ;; label = @1
      local.get 1
      i32.load offset=24
      local.tee 4
      i32.const 1055392
      i32.const 12
      local.get 1
      i32.const 28
      i32.add
      i32.load
      local.tee 1
      i32.load offset=12
      call_indirect (type 7)
      br_if 0 (;@1;)
      block  ;; label = @2
        block  ;; label = @3
          local.get 0
          i32.load offset=8
          local.tee 3
          i32.eqz
          br_if 0 (;@3;)
          local.get 2
          local.get 3
          i32.store offset=12
          local.get 2
          i32.const 71
          i32.store offset=20
          local.get 2
          local.get 2
          i32.const 12
          i32.add
          i32.store offset=16
          i32.const 1
          local.set 3
          local.get 2
          i32.const 1
          i32.store offset=60
          local.get 2
          i64.const 2
          i64.store offset=44 align=4
          local.get 2
          i32.const 1055408
          i32.store offset=40
          local.get 2
          local.get 2
          i32.const 16
          i32.add
          i32.store offset=56
          local.get 4
          local.get 1
          local.get 2
          i32.const 40
          i32.add
          call $_ZN4core3fmt5write17hb15fae9d0358166aE
          i32.eqz
          br_if 1 (;@2;)
          br 2 (;@1;)
        end
        local.get 0
        i32.load
        local.tee 3
        local.get 0
        i32.load offset=4
        i32.const 12
        i32.add
        i32.load
        call_indirect (type 2)
        i64.const 8378048562631854315
        i64.ne
        br_if 0 (;@2;)
        local.get 2
        local.get 3
        i32.store offset=12
        local.get 2
        i32.const 72
        i32.store offset=20
        local.get 2
        local.get 2
        i32.const 12
        i32.add
        i32.store offset=16
        i32.const 1
        local.set 3
        local.get 2
        i32.const 1
        i32.store offset=60
        local.get 2
        i64.const 2
        i64.store offset=44 align=4
        local.get 2
        i32.const 1055408
        i32.store offset=40
        local.get 2
        local.get 2
        i32.const 16
        i32.add
        i32.store offset=56
        local.get 4
        local.get 1
        local.get 2
        i32.const 40
        i32.add
        call $_ZN4core3fmt5write17hb15fae9d0358166aE
        br_if 1 (;@1;)
      end
      local.get 0
      i32.load offset=12
      local.set 3
      local.get 2
      i32.const 36
      i32.add
      i32.const 4
      i32.store
      local.get 2
      i32.const 16
      i32.add
      i32.const 12
      i32.add
      i32.const 4
      i32.store
      local.get 2
      local.get 3
      i32.const 12
      i32.add
      i32.store offset=32
      local.get 2
      local.get 3
      i32.const 8
      i32.add
      i32.store offset=24
      local.get 2
      i32.const 70
      i32.store offset=20
      local.get 2
      local.get 3
      i32.store offset=16
      local.get 2
      i32.const 3
      i32.store offset=60
      local.get 2
      i64.const 3
      i64.store offset=44 align=4
      local.get 2
      i32.const 1055368
      i32.store offset=40
      local.get 2
      local.get 2
      i32.const 16
      i32.add
      i32.store offset=56
      local.get 4
      local.get 1
      local.get 2
      i32.const 40
      i32.add
      call $_ZN4core3fmt5write17hb15fae9d0358166aE
      local.set 3
    end
    local.get 2
    i32.const 64
    i32.add
    global.set $__stack_pointer
    local.get 3)
  (func $_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17hd4a025d34140f1f3E (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 1
    i32.const 28
    i32.add
    i32.load
    local.set 3
    local.get 1
    i32.load offset=24
    local.set 4
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 0
    i32.load
    local.tee 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 4
    local.get 3
    local.get 2
    i32.const 8
    i32.add
    call $_ZN4core3fmt5write17hb15fae9d0358166aE
    local.set 1
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h5cd1e1056906e4c0E (type 5) (param i32 i32) (result i32)
    local.get 1
    local.get 0
    i32.load
    local.tee 0
    i32.load
    local.get 0
    i32.load offset=4
    call $_ZN4core3fmt9Formatter3pad17h66985344eb075eb9E)
  (func $_ZN4core9panicking13panic_display17h8048e74e95fcae8aE (type 3) (param i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    i32.const 20
    i32.add
    i32.const 1
    i32.store
    local.get 2
    i64.const 1
    i64.store offset=4 align=4
    local.get 2
    i32.const 1055424
    i32.store
    local.get 2
    i32.const 70
    i32.store offset=28
    local.get 2
    local.get 0
    i32.store offset=24
    local.get 2
    local.get 2
    i32.const 24
    i32.add
    i32.store offset=16
    local.get 2
    local.get 1
    call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
    unreachable)
  (func $_ZN4core9panicking19assert_failed_inner17h354c863259f57591E (type 13) (param i32 i32 i32 i32 i32 i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 112
    i32.sub
    local.tee 7
    global.set $__stack_pointer
    local.get 7
    local.get 2
    i32.store offset=12
    local.get 7
    local.get 1
    i32.store offset=8
    local.get 7
    local.get 4
    i32.store offset=20
    local.get 7
    local.get 3
    i32.store offset=16
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            i32.const 255
            i32.and
            br_table 0 (;@4;) 1 (;@3;) 2 (;@2;) 0 (;@4;)
          end
          local.get 7
          i32.const 1055457
          i32.store offset=24
          i32.const 2
          local.set 2
          br 2 (;@1;)
        end
        local.get 7
        i32.const 1055455
        i32.store offset=24
        i32.const 2
        local.set 2
        br 1 (;@1;)
      end
      local.get 7
      i32.const 1055448
      i32.store offset=24
      i32.const 7
      local.set 2
    end
    local.get 7
    local.get 2
    i32.store offset=28
    block  ;; label = @1
      local.get 5
      i32.load
      br_if 0 (;@1;)
      local.get 7
      i32.const 56
      i32.add
      i32.const 20
      i32.add
      i32.const 73
      i32.store
      local.get 7
      i32.const 68
      i32.add
      i32.const 73
      i32.store
      local.get 7
      i32.const 88
      i32.add
      i32.const 20
      i32.add
      i32.const 3
      i32.store
      local.get 7
      i64.const 4
      i64.store offset=92 align=4
      local.get 7
      i32.const 1055556
      i32.store offset=88
      local.get 7
      i32.const 70
      i32.store offset=60
      local.get 7
      local.get 7
      i32.const 56
      i32.add
      i32.store offset=104
      local.get 7
      local.get 7
      i32.const 16
      i32.add
      i32.store offset=72
      local.get 7
      local.get 7
      i32.const 8
      i32.add
      i32.store offset=64
      local.get 7
      local.get 7
      i32.const 24
      i32.add
      i32.store offset=56
      local.get 7
      i32.const 88
      i32.add
      local.get 6
      call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
      unreachable
    end
    local.get 7
    i32.const 32
    i32.add
    i32.const 16
    i32.add
    local.get 5
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 7
    i32.const 32
    i32.add
    i32.const 8
    i32.add
    local.get 5
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 7
    local.get 5
    i64.load align=4
    i64.store offset=32
    local.get 7
    i32.const 88
    i32.add
    i32.const 20
    i32.add
    i32.const 4
    i32.store
    local.get 7
    i32.const 84
    i32.add
    i32.const 8
    i32.store
    local.get 7
    i32.const 56
    i32.add
    i32.const 20
    i32.add
    i32.const 73
    i32.store
    local.get 7
    i32.const 68
    i32.add
    i32.const 73
    i32.store
    local.get 7
    i64.const 4
    i64.store offset=92 align=4
    local.get 7
    i32.const 1055520
    i32.store offset=88
    local.get 7
    i32.const 70
    i32.store offset=60
    local.get 7
    local.get 7
    i32.const 56
    i32.add
    i32.store offset=104
    local.get 7
    local.get 7
    i32.const 32
    i32.add
    i32.store offset=80
    local.get 7
    local.get 7
    i32.const 16
    i32.add
    i32.store offset=72
    local.get 7
    local.get 7
    i32.const 8
    i32.add
    i32.store offset=64
    local.get 7
    local.get 7
    i32.const 24
    i32.add
    i32.store offset=56
    local.get 7
    i32.const 88
    i32.add
    local.get 6
    call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
    unreachable)
  (func $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17ha544352130619abaE (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.get 1
    local.get 0
    i32.load offset=4
    i32.load offset=12
    call_indirect (type 5))
  (func $_ZN59_$LT$core..fmt..Arguments$u20$as$u20$core..fmt..Display$GT$3fmt17hda659a7548139618E (type 5) (param i32 i32) (result i32)
    (local i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 1
    i32.const 28
    i32.add
    i32.load
    local.set 3
    local.get 1
    i32.load offset=24
    local.set 1
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 0
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 0
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 0
    i64.load align=4
    i64.store offset=8
    local.get 1
    local.get 3
    local.get 2
    i32.const 8
    i32.add
    call $_ZN4core3fmt5write17hb15fae9d0358166aE
    local.set 0
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core6result13unwrap_failed17h0b834de6e1e67b38E (type 11) (param i32 i32 i32 i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 64
    i32.sub
    local.tee 5
    global.set $__stack_pointer
    local.get 5
    local.get 1
    i32.store offset=12
    local.get 5
    local.get 0
    i32.store offset=8
    local.get 5
    local.get 3
    i32.store offset=20
    local.get 5
    local.get 2
    i32.store offset=16
    local.get 5
    i32.const 44
    i32.add
    i32.const 2
    i32.store
    local.get 5
    i32.const 60
    i32.add
    i32.const 73
    i32.store
    local.get 5
    i64.const 2
    i64.store offset=28 align=4
    local.get 5
    i32.const 1055592
    i32.store offset=24
    local.get 5
    i32.const 70
    i32.store offset=52
    local.get 5
    local.get 5
    i32.const 48
    i32.add
    i32.store offset=40
    local.get 5
    local.get 5
    i32.const 16
    i32.add
    i32.store offset=56
    local.get 5
    local.get 5
    i32.const 8
    i32.add
    i32.store offset=48
    local.get 5
    i32.const 24
    i32.add
    local.get 4
    call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
    unreachable)
  (func $_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hf0e70d48679f35b2E (type 7) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    i32.const 10
    i32.store offset=40
    local.get 3
    i64.const 4294967306
    i64.store offset=32
    local.get 3
    local.get 2
    i32.store offset=28
    i32.const 0
    local.set 4
    local.get 3
    i32.const 0
    i32.store offset=24
    local.get 3
    local.get 2
    i32.store offset=20
    local.get 3
    local.get 1
    i32.store offset=16
    local.get 3
    local.get 2
    i32.store offset=12
    local.get 3
    i32.const 0
    i32.store offset=8
    local.get 0
    i32.load offset=4
    local.set 5
    local.get 0
    i32.load
    local.set 6
    local.get 0
    i32.load offset=8
    local.set 7
    i32.const 0
    local.set 8
    i32.const 0
    local.set 9
    block  ;; label = @1
      loop  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 9
            i32.const 255
            i32.and
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 8
              local.get 2
              i32.gt_u
              br_if 0 (;@5;)
              loop  ;; label = @6
                local.get 1
                local.get 8
                i32.add
                local.set 10
                block  ;; label = @7
                  block  ;; label = @8
                    local.get 2
                    local.get 8
                    i32.sub
                    local.tee 11
                    i32.const 8
                    i32.lt_u
                    br_if 0 (;@8;)
                    local.get 3
                    i32.const 10
                    local.get 10
                    local.get 11
                    call $_ZN4core5slice6memchr14memchr_aligned17ha15e5d65ea28ac67E
                    local.get 3
                    i32.load offset=4
                    local.set 0
                    local.get 3
                    i32.load
                    local.set 10
                    br 1 (;@7;)
                  end
                  i32.const 0
                  local.set 0
                  block  ;; label = @8
                    local.get 11
                    br_if 0 (;@8;)
                    i32.const 0
                    local.set 10
                    br 1 (;@7;)
                  end
                  loop  ;; label = @8
                    block  ;; label = @9
                      local.get 10
                      local.get 0
                      i32.add
                      i32.load8_u
                      i32.const 10
                      i32.ne
                      br_if 0 (;@9;)
                      i32.const 1
                      local.set 10
                      br 2 (;@7;)
                    end
                    local.get 11
                    local.get 0
                    i32.const 1
                    i32.add
                    local.tee 0
                    i32.ne
                    br_if 0 (;@8;)
                  end
                  i32.const 0
                  local.set 10
                  local.get 11
                  local.set 0
                end
                block  ;; label = @7
                  local.get 10
                  i32.const 1
                  i32.eq
                  br_if 0 (;@7;)
                  local.get 2
                  local.set 8
                  br 2 (;@5;)
                end
                block  ;; label = @7
                  local.get 8
                  local.get 0
                  i32.add
                  local.tee 0
                  i32.const 1
                  i32.add
                  local.tee 8
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 8
                  local.get 2
                  i32.gt_u
                  br_if 0 (;@7;)
                  local.get 1
                  local.get 0
                  i32.add
                  i32.load8_u
                  i32.const 10
                  i32.ne
                  br_if 0 (;@7;)
                  i32.const 0
                  local.set 9
                  local.get 8
                  local.set 12
                  local.get 8
                  local.set 0
                  br 4 (;@3;)
                end
                local.get 8
                local.get 2
                i32.le_u
                br_if 0 (;@6;)
              end
            end
            i32.const 1
            local.set 9
            local.get 4
            local.set 12
            local.get 2
            local.set 0
            local.get 4
            local.get 2
            i32.ne
            br_if 1 (;@3;)
          end
          i32.const 0
          local.set 0
          br 2 (;@1;)
        end
        block  ;; label = @3
          block  ;; label = @4
            local.get 7
            i32.load8_u
            i32.eqz
            br_if 0 (;@4;)
            local.get 6
            i32.const 1055632
            i32.const 4
            local.get 5
            i32.load offset=12
            call_indirect (type 7)
            br_if 1 (;@3;)
          end
          local.get 1
          local.get 4
          i32.add
          local.set 11
          local.get 0
          local.get 4
          i32.sub
          local.set 10
          i32.const 0
          local.set 13
          block  ;; label = @4
            local.get 0
            local.get 4
            i32.eq
            br_if 0 (;@4;)
            local.get 10
            local.get 11
            i32.add
            i32.const -1
            i32.add
            i32.load8_u
            i32.const 10
            i32.eq
            local.set 13
          end
          local.get 7
          local.get 13
          i32.store8
          local.get 12
          local.set 4
          local.get 6
          local.get 11
          local.get 10
          local.get 5
          i32.load offset=12
          call_indirect (type 7)
          i32.eqz
          br_if 1 (;@2;)
        end
      end
      i32.const 1
      local.set 0
    end
    local.get 3
    i32.const 48
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core3fmt8builders11DebugStruct21finish_non_exhaustive17h729a9bc3887ef43dE (type 4) (param i32) (result i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 1
    global.set $__stack_pointer
    i32.const 1
    local.set 2
    block  ;; label = @1
      local.get 0
      i32.load8_u offset=4
      br_if 0 (;@1;)
      local.get 0
      i32.load
      local.set 3
      block  ;; label = @2
        local.get 0
        i32.const 5
        i32.add
        i32.load8_u
        br_if 0 (;@2;)
        local.get 3
        i32.load offset=24
        i32.const 1055656
        i32.const 7
        local.get 3
        i32.const 28
        i32.add
        i32.load
        i32.load offset=12
        call_indirect (type 7)
        local.set 2
        br 1 (;@1;)
      end
      block  ;; label = @2
        local.get 3
        i32.load8_u
        i32.const 4
        i32.and
        br_if 0 (;@2;)
        local.get 3
        i32.load offset=24
        i32.const 1055650
        i32.const 6
        local.get 3
        i32.const 28
        i32.add
        i32.load
        i32.load offset=12
        call_indirect (type 7)
        local.set 2
        br 1 (;@1;)
      end
      i32.const 1
      local.set 2
      local.get 1
      i32.const 1
      i32.store8 offset=15
      local.get 1
      local.get 3
      i64.load offset=24 align=4
      i64.store
      local.get 1
      local.get 1
      i32.const 15
      i32.add
      i32.store offset=8
      local.get 1
      i32.const 1055646
      i32.const 3
      call $_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hf0e70d48679f35b2E
      br_if 0 (;@1;)
      local.get 3
      i32.load offset=24
      i32.const 1055649
      i32.const 1
      local.get 3
      i32.load offset=28
      i32.load offset=12
      call_indirect (type 7)
      local.set 2
    end
    local.get 0
    local.get 2
    i32.store8 offset=4
    local.get 1
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 2)
  (func $_ZN4core3fmt8builders10DebugTuple5field17hcf01775d6abf5a8bE (type 7) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i64 i64)
    global.get $__stack_pointer
    i32.const 64
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load8_u offset=8
        i32.eqz
        br_if 0 (;@2;)
        local.get 0
        i32.load offset=4
        local.set 4
        i32.const 1
        local.set 5
        br 1 (;@1;)
      end
      local.get 0
      i32.load offset=4
      local.set 4
      block  ;; label = @2
        local.get 0
        i32.load
        local.tee 6
        i32.load
        local.tee 7
        i32.const 4
        i32.and
        br_if 0 (;@2;)
        i32.const 1
        local.set 5
        local.get 6
        i32.load offset=24
        i32.const 1055641
        i32.const 1055667
        local.get 4
        select
        i32.const 2
        i32.const 1
        local.get 4
        select
        local.get 6
        i32.const 28
        i32.add
        i32.load
        i32.load offset=12
        call_indirect (type 7)
        br_if 1 (;@1;)
        local.get 1
        local.get 6
        local.get 2
        i32.load offset=12
        call_indirect (type 5)
        local.set 5
        br 1 (;@1;)
      end
      block  ;; label = @2
        local.get 4
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 6
          i32.load offset=24
          i32.const 1055665
          i32.const 2
          local.get 6
          i32.const 28
          i32.add
          i32.load
          i32.load offset=12
          call_indirect (type 7)
          i32.eqz
          br_if 0 (;@3;)
          i32.const 1
          local.set 5
          i32.const 0
          local.set 4
          br 2 (;@1;)
        end
        local.get 6
        i32.load
        local.set 7
      end
      i32.const 1
      local.set 5
      local.get 3
      i32.const 1
      i32.store8 offset=23
      local.get 3
      i32.const 52
      i32.add
      i32.const 1055608
      i32.store
      local.get 3
      local.get 7
      i32.store offset=24
      local.get 3
      local.get 6
      i64.load offset=24 align=4
      i64.store offset=8
      local.get 3
      local.get 3
      i32.const 23
      i32.add
      i32.store offset=16
      local.get 6
      i64.load offset=8 align=4
      local.set 8
      local.get 6
      i64.load offset=16 align=4
      local.set 9
      local.get 3
      local.get 6
      i32.load8_u offset=32
      i32.store8 offset=56
      local.get 3
      local.get 6
      i32.load offset=4
      i32.store offset=28
      local.get 3
      local.get 9
      i64.store offset=40
      local.get 3
      local.get 8
      i64.store offset=32
      local.get 3
      local.get 3
      i32.const 8
      i32.add
      i32.store offset=48
      local.get 1
      local.get 3
      i32.const 24
      i32.add
      local.get 2
      i32.load offset=12
      call_indirect (type 5)
      br_if 0 (;@1;)
      local.get 3
      i32.load offset=48
      i32.const 1055639
      i32.const 2
      local.get 3
      i32.load offset=52
      i32.load offset=12
      call_indirect (type 7)
      local.set 5
    end
    local.get 0
    local.get 5
    i32.store8 offset=8
    local.get 0
    local.get 4
    i32.const 1
    i32.add
    i32.store offset=4
    local.get 3
    i32.const 64
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core3fmt8builders10DebugInner5entry17haf73aba3bce8c309E (type 6) (param i32 i32 i32)
    (local i32 i32 i32 i32 i64 i64)
    global.get $__stack_pointer
    i32.const 64
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    i32.const 1
    local.set 4
    block  ;; label = @1
      local.get 0
      i32.load8_u offset=4
      br_if 0 (;@1;)
      local.get 0
      i32.load8_u offset=5
      local.set 4
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 0
              i32.load
              local.tee 5
              i32.load
              local.tee 6
              i32.const 4
              i32.and
              br_if 0 (;@5;)
              local.get 4
              i32.const 255
              i32.and
              br_if 1 (;@4;)
              br 3 (;@2;)
            end
            local.get 4
            i32.const 255
            i32.and
            br_if 1 (;@3;)
            i32.const 1
            local.set 4
            local.get 5
            i32.load offset=24
            i32.const 1055669
            i32.const 1
            local.get 5
            i32.const 28
            i32.add
            i32.load
            i32.load offset=12
            call_indirect (type 7)
            br_if 3 (;@1;)
            local.get 5
            i32.load
            local.set 6
            br 1 (;@3;)
          end
          i32.const 1
          local.set 4
          local.get 5
          i32.load offset=24
          i32.const 1055641
          i32.const 2
          local.get 5
          i32.const 28
          i32.add
          i32.load
          i32.load offset=12
          call_indirect (type 7)
          i32.eqz
          br_if 1 (;@2;)
          br 2 (;@1;)
        end
        i32.const 1
        local.set 4
        local.get 3
        i32.const 1
        i32.store8 offset=23
        local.get 3
        i32.const 52
        i32.add
        i32.const 1055608
        i32.store
        local.get 3
        local.get 6
        i32.store offset=24
        local.get 3
        local.get 5
        i64.load offset=24 align=4
        i64.store offset=8
        local.get 3
        local.get 3
        i32.const 23
        i32.add
        i32.store offset=16
        local.get 5
        i64.load offset=8 align=4
        local.set 7
        local.get 5
        i64.load offset=16 align=4
        local.set 8
        local.get 3
        local.get 5
        i32.load8_u offset=32
        i32.store8 offset=56
        local.get 3
        local.get 5
        i32.load offset=4
        i32.store offset=28
        local.get 3
        local.get 8
        i64.store offset=40
        local.get 3
        local.get 7
        i64.store offset=32
        local.get 3
        local.get 3
        i32.const 8
        i32.add
        i32.store offset=48
        local.get 1
        local.get 3
        i32.const 24
        i32.add
        local.get 2
        i32.load offset=12
        call_indirect (type 5)
        br_if 1 (;@1;)
        local.get 3
        i32.load offset=48
        i32.const 1055639
        i32.const 2
        local.get 3
        i32.load offset=52
        i32.load offset=12
        call_indirect (type 7)
        local.set 4
        br 1 (;@1;)
      end
      local.get 1
      local.get 5
      local.get 2
      i32.load offset=12
      call_indirect (type 5)
      local.set 4
    end
    local.get 0
    i32.const 1
    i32.store8 offset=5
    local.get 0
    local.get 4
    i32.store8 offset=4
    local.get 3
    i32.const 64
    i32.add
    global.set $__stack_pointer)
  (func $_ZN4core3fmt8builders8DebugSet5entry17hb3bdf118faaef184E (type 7) (param i32 i32 i32) (result i32)
    local.get 0
    local.get 1
    local.get 2
    call $_ZN4core3fmt8builders10DebugInner5entry17haf73aba3bce8c309E
    local.get 0)
  (func $_ZN4core3fmt8builders9DebugList6finish17h7ba20995ee25f20eE (type 4) (param i32) (result i32)
    (local i32)
    i32.const 1
    local.set 1
    block  ;; label = @1
      local.get 0
      i32.load8_u offset=4
      br_if 0 (;@1;)
      local.get 0
      i32.load
      local.tee 0
      i32.const 24
      i32.add
      i32.load
      i32.const 1055688
      i32.const 1
      local.get 0
      i32.const 28
      i32.add
      i32.load
      i32.load offset=12
      call_indirect (type 7)
      local.set 1
    end
    local.get 1)
  (func $_ZN4core3fmt9Formatter12pad_integral17h8d163aacc395a3d9E (type 14) (param i32 i32 i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.eqz
        br_if 0 (;@2;)
        i32.const 43
        i32.const 1114112
        local.get 0
        i32.load
        local.tee 6
        i32.const 1
        i32.and
        local.tee 1
        select
        local.set 7
        local.get 1
        local.get 5
        i32.add
        local.set 8
        br 1 (;@1;)
      end
      local.get 5
      i32.const 1
      i32.add
      local.set 8
      local.get 0
      i32.load
      local.set 6
      i32.const 45
      local.set 7
    end
    block  ;; label = @1
      block  ;; label = @2
        local.get 6
        i32.const 4
        i32.and
        br_if 0 (;@2;)
        i32.const 0
        local.set 2
        br 1 (;@1;)
      end
      block  ;; label = @2
        block  ;; label = @3
          local.get 3
          i32.const 16
          i32.lt_u
          br_if 0 (;@3;)
          local.get 2
          local.get 3
          call $_ZN4core3str5count14do_count_chars17hc6a12b510bd99c00E
          local.set 9
          br 1 (;@2;)
        end
        block  ;; label = @3
          local.get 3
          br_if 0 (;@3;)
          i32.const 0
          local.set 9
          br 1 (;@2;)
        end
        local.get 3
        i32.const 3
        i32.and
        local.set 10
        block  ;; label = @3
          block  ;; label = @4
            local.get 3
            i32.const -1
            i32.add
            i32.const 3
            i32.ge_u
            br_if 0 (;@4;)
            i32.const 0
            local.set 9
            local.get 2
            local.set 1
            br 1 (;@3;)
          end
          local.get 3
          i32.const -4
          i32.and
          local.set 11
          i32.const 0
          local.set 9
          local.get 2
          local.set 1
          loop  ;; label = @4
            local.get 9
            local.get 1
            i32.load8_s
            i32.const -65
            i32.gt_s
            i32.add
            local.get 1
            i32.load8_s offset=1
            i32.const -65
            i32.gt_s
            i32.add
            local.get 1
            i32.load8_s offset=2
            i32.const -65
            i32.gt_s
            i32.add
            local.get 1
            i32.load8_s offset=3
            i32.const -65
            i32.gt_s
            i32.add
            local.set 9
            local.get 1
            i32.const 4
            i32.add
            local.set 1
            local.get 11
            i32.const -4
            i32.add
            local.tee 11
            br_if 0 (;@4;)
          end
        end
        local.get 10
        i32.eqz
        br_if 0 (;@2;)
        loop  ;; label = @3
          local.get 9
          local.get 1
          i32.load8_s
          i32.const -65
          i32.gt_s
          i32.add
          local.set 9
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 10
          i32.const -1
          i32.add
          local.tee 10
          br_if 0 (;@3;)
        end
      end
      local.get 9
      local.get 8
      i32.add
      local.set 8
    end
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load offset=8
        br_if 0 (;@2;)
        i32.const 1
        local.set 1
        local.get 0
        i32.const 24
        i32.add
        i32.load
        local.tee 9
        local.get 0
        i32.const 28
        i32.add
        i32.load
        local.tee 0
        local.get 7
        local.get 2
        local.get 3
        call $_ZN4core3fmt9Formatter12pad_integral12write_prefix17h94cb7822dccdd843E
        br_if 1 (;@1;)
        local.get 9
        local.get 4
        local.get 5
        local.get 0
        i32.load offset=12
        call_indirect (type 7)
        return
      end
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 0
                i32.const 12
                i32.add
                i32.load
                local.tee 11
                local.get 8
                i32.le_u
                br_if 0 (;@6;)
                local.get 6
                i32.const 8
                i32.and
                br_if 4 (;@2;)
                local.get 11
                local.get 8
                i32.sub
                local.tee 9
                local.set 8
                i32.const 1
                local.get 0
                i32.load8_u offset=32
                local.tee 1
                local.get 1
                i32.const 3
                i32.eq
                select
                i32.const 3
                i32.and
                local.tee 1
                br_table 3 (;@3;) 1 (;@5;) 2 (;@4;) 3 (;@3;)
              end
              i32.const 1
              local.set 1
              local.get 0
              i32.const 24
              i32.add
              i32.load
              local.tee 9
              local.get 0
              i32.const 28
              i32.add
              i32.load
              local.tee 0
              local.get 7
              local.get 2
              local.get 3
              call $_ZN4core3fmt9Formatter12pad_integral12write_prefix17h94cb7822dccdd843E
              br_if 4 (;@1;)
              local.get 9
              local.get 4
              local.get 5
              local.get 0
              i32.load offset=12
              call_indirect (type 7)
              return
            end
            i32.const 0
            local.set 8
            local.get 9
            local.set 1
            br 1 (;@3;)
          end
          local.get 9
          i32.const 1
          i32.shr_u
          local.set 1
          local.get 9
          i32.const 1
          i32.add
          i32.const 1
          i32.shr_u
          local.set 8
        end
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        local.get 0
        i32.const 28
        i32.add
        i32.load
        local.set 10
        local.get 0
        i32.const 24
        i32.add
        i32.load
        local.set 11
        local.get 0
        i32.load offset=4
        local.set 9
        block  ;; label = @3
          loop  ;; label = @4
            local.get 1
            i32.const -1
            i32.add
            local.tee 1
            i32.eqz
            br_if 1 (;@3;)
            local.get 11
            local.get 9
            local.get 10
            i32.load offset=16
            call_indirect (type 5)
            i32.eqz
            br_if 0 (;@4;)
          end
          i32.const 1
          return
        end
        i32.const 1
        local.set 1
        local.get 9
        i32.const 1114112
        i32.eq
        br_if 1 (;@1;)
        local.get 11
        local.get 10
        local.get 7
        local.get 2
        local.get 3
        call $_ZN4core3fmt9Formatter12pad_integral12write_prefix17h94cb7822dccdd843E
        br_if 1 (;@1;)
        local.get 11
        local.get 4
        local.get 5
        local.get 10
        i32.load offset=12
        call_indirect (type 7)
        br_if 1 (;@1;)
        i32.const 0
        local.set 1
        block  ;; label = @3
          loop  ;; label = @4
            block  ;; label = @5
              local.get 8
              local.get 1
              i32.ne
              br_if 0 (;@5;)
              local.get 8
              local.set 1
              br 2 (;@3;)
            end
            local.get 1
            i32.const 1
            i32.add
            local.set 1
            local.get 11
            local.get 9
            local.get 10
            i32.load offset=16
            call_indirect (type 5)
            i32.eqz
            br_if 0 (;@4;)
          end
          local.get 1
          i32.const -1
          i32.add
          local.set 1
        end
        local.get 1
        local.get 8
        i32.lt_u
        local.set 1
        br 1 (;@1;)
      end
      local.get 0
      i32.load offset=4
      local.set 6
      local.get 0
      i32.const 48
      i32.store offset=4
      local.get 0
      i32.load8_u offset=32
      local.set 12
      i32.const 1
      local.set 1
      local.get 0
      i32.const 1
      i32.store8 offset=32
      local.get 0
      i32.const 24
      i32.add
      i32.load
      local.tee 9
      local.get 0
      i32.const 28
      i32.add
      i32.load
      local.tee 10
      local.get 7
      local.get 2
      local.get 3
      call $_ZN4core3fmt9Formatter12pad_integral12write_prefix17h94cb7822dccdd843E
      br_if 0 (;@1;)
      local.get 11
      local.get 8
      i32.sub
      i32.const 1
      i32.add
      local.set 1
      block  ;; label = @2
        loop  ;; label = @3
          local.get 1
          i32.const -1
          i32.add
          local.tee 1
          i32.eqz
          br_if 1 (;@2;)
          local.get 9
          i32.const 48
          local.get 10
          i32.load offset=16
          call_indirect (type 5)
          i32.eqz
          br_if 0 (;@3;)
        end
        i32.const 1
        return
      end
      i32.const 1
      local.set 1
      local.get 9
      local.get 4
      local.get 5
      local.get 10
      i32.load offset=12
      call_indirect (type 7)
      br_if 0 (;@1;)
      local.get 0
      local.get 12
      i32.store8 offset=32
      local.get 0
      local.get 6
      i32.store offset=4
      i32.const 0
      return
    end
    local.get 1)
  (func $_ZN4core3fmt5Write10write_char17h265cab5073dbf68fE (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    i32.const 0
    i32.store offset=12
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 128
        i32.lt_u
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 1
          i32.const 2048
          i32.lt_u
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 1
            i32.const 65536
            i32.lt_u
            br_if 0 (;@4;)
            local.get 2
            local.get 1
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=15
            local.get 2
            local.get 1
            i32.const 6
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=14
            local.get 2
            local.get 1
            i32.const 12
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=13
            local.get 2
            local.get 1
            i32.const 18
            i32.shr_u
            i32.const 7
            i32.and
            i32.const 240
            i32.or
            i32.store8 offset=12
            i32.const 4
            local.set 1
            br 3 (;@1;)
          end
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=14
          local.get 2
          local.get 1
          i32.const 12
          i32.shr_u
          i32.const 224
          i32.or
          i32.store8 offset=12
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=13
          i32.const 3
          local.set 1
          br 2 (;@1;)
        end
        local.get 2
        local.get 1
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=13
        local.get 2
        local.get 1
        i32.const 6
        i32.shr_u
        i32.const 192
        i32.or
        i32.store8 offset=12
        i32.const 2
        local.set 1
        br 1 (;@1;)
      end
      local.get 2
      local.get 1
      i32.store8 offset=12
      i32.const 1
      local.set 1
    end
    local.get 0
    local.get 2
    i32.const 12
    i32.add
    local.get 1
    call $_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hf0e70d48679f35b2E
    local.set 1
    local.get 2
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN4core3fmt5Write9write_fmt17h1486cdb96f6c78f2E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 0
    i32.store offset=4
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 2
    i32.const 4
    i32.add
    i32.const 1055936
    local.get 2
    i32.const 8
    i32.add
    call $_ZN4core3fmt5write17hb15fae9d0358166aE
    local.set 1
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_str17had2d839cf514036aE (type 7) (param i32 i32 i32) (result i32)
    local.get 0
    i32.load
    local.get 1
    local.get 2
    call $_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hf0e70d48679f35b2E)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$10write_char17h471402e56c237cb0E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 0
    i32.load
    local.set 0
    local.get 2
    i32.const 0
    i32.store offset=12
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.const 128
        i32.lt_u
        br_if 0 (;@2;)
        block  ;; label = @3
          local.get 1
          i32.const 2048
          i32.lt_u
          br_if 0 (;@3;)
          block  ;; label = @4
            local.get 1
            i32.const 65536
            i32.lt_u
            br_if 0 (;@4;)
            local.get 2
            local.get 1
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=15
            local.get 2
            local.get 1
            i32.const 6
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=14
            local.get 2
            local.get 1
            i32.const 12
            i32.shr_u
            i32.const 63
            i32.and
            i32.const 128
            i32.or
            i32.store8 offset=13
            local.get 2
            local.get 1
            i32.const 18
            i32.shr_u
            i32.const 7
            i32.and
            i32.const 240
            i32.or
            i32.store8 offset=12
            i32.const 4
            local.set 1
            br 3 (;@1;)
          end
          local.get 2
          local.get 1
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=14
          local.get 2
          local.get 1
          i32.const 12
          i32.shr_u
          i32.const 224
          i32.or
          i32.store8 offset=12
          local.get 2
          local.get 1
          i32.const 6
          i32.shr_u
          i32.const 63
          i32.and
          i32.const 128
          i32.or
          i32.store8 offset=13
          i32.const 3
          local.set 1
          br 2 (;@1;)
        end
        local.get 2
        local.get 1
        i32.const 63
        i32.and
        i32.const 128
        i32.or
        i32.store8 offset=13
        local.get 2
        local.get 1
        i32.const 6
        i32.shr_u
        i32.const 192
        i32.or
        i32.store8 offset=12
        i32.const 2
        local.set 1
        br 1 (;@1;)
      end
      local.get 2
      local.get 1
      i32.store8 offset=12
      i32.const 1
      local.set 1
    end
    local.get 0
    local.get 2
    i32.const 12
    i32.add
    local.get 1
    call $_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hf0e70d48679f35b2E
    local.set 1
    local.get 2
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_fmt17h795910deba94c650E (type 5) (param i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 0
    i32.load
    i32.store offset=4
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 2
    i32.const 4
    i32.add
    i32.const 1055936
    local.get 2
    i32.const 8
    i32.add
    call $_ZN4core3fmt5write17hb15fae9d0358166aE
    local.set 1
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN4core3str5count14do_count_chars17hc6a12b510bd99c00E (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.const 3
        i32.add
        i32.const -4
        i32.and
        local.tee 2
        local.get 0
        i32.sub
        local.tee 3
        local.get 1
        i32.gt_u
        br_if 0 (;@2;)
        local.get 3
        i32.const 4
        i32.gt_u
        br_if 0 (;@2;)
        local.get 1
        local.get 3
        i32.sub
        local.tee 4
        i32.const 4
        i32.lt_u
        br_if 0 (;@2;)
        local.get 4
        i32.const 3
        i32.and
        local.set 5
        i32.const 0
        local.set 6
        i32.const 0
        local.set 1
        block  ;; label = @3
          local.get 2
          local.get 0
          i32.eq
          br_if 0 (;@3;)
          local.get 3
          i32.const 3
          i32.and
          local.set 7
          block  ;; label = @4
            block  ;; label = @5
              local.get 2
              local.get 0
              i32.const -1
              i32.xor
              i32.add
              i32.const 3
              i32.ge_u
              br_if 0 (;@5;)
              i32.const 0
              local.set 1
              local.get 0
              local.set 2
              br 1 (;@4;)
            end
            local.get 3
            i32.const -4
            i32.and
            local.set 8
            i32.const 0
            local.set 1
            local.get 0
            local.set 2
            loop  ;; label = @5
              local.get 1
              local.get 2
              i32.load8_s
              i32.const -65
              i32.gt_s
              i32.add
              local.get 2
              i32.load8_s offset=1
              i32.const -65
              i32.gt_s
              i32.add
              local.get 2
              i32.load8_s offset=2
              i32.const -65
              i32.gt_s
              i32.add
              local.get 2
              i32.load8_s offset=3
              i32.const -65
              i32.gt_s
              i32.add
              local.set 1
              local.get 2
              i32.const 4
              i32.add
              local.set 2
              local.get 8
              i32.const -4
              i32.add
              local.tee 8
              br_if 0 (;@5;)
            end
          end
          local.get 7
          i32.eqz
          br_if 0 (;@3;)
          loop  ;; label = @4
            local.get 1
            local.get 2
            i32.load8_s
            i32.const -65
            i32.gt_s
            i32.add
            local.set 1
            local.get 2
            i32.const 1
            i32.add
            local.set 2
            local.get 7
            i32.const -1
            i32.add
            local.tee 7
            br_if 0 (;@4;)
          end
        end
        local.get 0
        local.get 3
        i32.add
        local.set 0
        block  ;; label = @3
          local.get 5
          i32.eqz
          br_if 0 (;@3;)
          local.get 0
          local.get 4
          i32.const -4
          i32.and
          i32.add
          local.tee 2
          i32.load8_s
          i32.const -65
          i32.gt_s
          local.set 6
          local.get 5
          i32.const 1
          i32.eq
          br_if 0 (;@3;)
          local.get 6
          local.get 2
          i32.load8_s offset=1
          i32.const -65
          i32.gt_s
          i32.add
          local.set 6
          local.get 5
          i32.const 2
          i32.eq
          br_if 0 (;@3;)
          local.get 6
          local.get 2
          i32.load8_s offset=2
          i32.const -65
          i32.gt_s
          i32.add
          local.set 6
        end
        local.get 4
        i32.const 2
        i32.shr_u
        local.set 3
        local.get 6
        local.get 1
        i32.add
        local.set 7
        loop  ;; label = @3
          local.get 0
          local.set 6
          local.get 3
          i32.eqz
          br_if 2 (;@1;)
          local.get 3
          i32.const 192
          local.get 3
          i32.const 192
          i32.lt_u
          select
          local.tee 4
          i32.const 3
          i32.and
          local.set 5
          local.get 4
          i32.const 2
          i32.shl
          local.set 9
          block  ;; label = @4
            block  ;; label = @5
              local.get 4
              i32.const 252
              i32.and
              local.tee 10
              br_if 0 (;@5;)
              i32.const 0
              local.set 2
              br 1 (;@4;)
            end
            local.get 6
            local.get 10
            i32.const 2
            i32.shl
            i32.add
            local.set 8
            i32.const 0
            local.set 2
            local.get 6
            local.set 0
            loop  ;; label = @5
              local.get 0
              i32.eqz
              br_if 1 (;@4;)
              local.get 0
              i32.const 12
              i32.add
              i32.load
              local.tee 1
              i32.const -1
              i32.xor
              i32.const 7
              i32.shr_u
              local.get 1
              i32.const 6
              i32.shr_u
              i32.or
              i32.const 16843009
              i32.and
              local.get 0
              i32.const 8
              i32.add
              i32.load
              local.tee 1
              i32.const -1
              i32.xor
              i32.const 7
              i32.shr_u
              local.get 1
              i32.const 6
              i32.shr_u
              i32.or
              i32.const 16843009
              i32.and
              local.get 0
              i32.const 4
              i32.add
              i32.load
              local.tee 1
              i32.const -1
              i32.xor
              i32.const 7
              i32.shr_u
              local.get 1
              i32.const 6
              i32.shr_u
              i32.or
              i32.const 16843009
              i32.and
              local.get 0
              i32.load
              local.tee 1
              i32.const -1
              i32.xor
              i32.const 7
              i32.shr_u
              local.get 1
              i32.const 6
              i32.shr_u
              i32.or
              i32.const 16843009
              i32.and
              local.get 2
              i32.add
              i32.add
              i32.add
              i32.add
              local.set 2
              local.get 0
              i32.const 16
              i32.add
              local.tee 0
              local.get 8
              i32.ne
              br_if 0 (;@5;)
            end
          end
          local.get 3
          local.get 4
          i32.sub
          local.set 3
          local.get 6
          local.get 9
          i32.add
          local.set 0
          local.get 2
          i32.const 8
          i32.shr_u
          i32.const 16711935
          i32.and
          local.get 2
          i32.const 16711935
          i32.and
          i32.add
          i32.const 65537
          i32.mul
          i32.const 16
          i32.shr_u
          local.get 7
          i32.add
          local.set 7
          local.get 5
          i32.eqz
          br_if 0 (;@3;)
        end
        block  ;; label = @3
          block  ;; label = @4
            local.get 6
            br_if 0 (;@4;)
            i32.const 0
            local.set 0
            br 1 (;@3;)
          end
          local.get 6
          local.get 10
          i32.const 2
          i32.shl
          i32.add
          local.tee 2
          i32.load
          local.tee 0
          i32.const -1
          i32.xor
          i32.const 7
          i32.shr_u
          local.get 0
          i32.const 6
          i32.shr_u
          i32.or
          i32.const 16843009
          i32.and
          local.set 0
          local.get 5
          i32.const 1
          i32.eq
          br_if 0 (;@3;)
          local.get 2
          i32.load offset=4
          local.tee 1
          i32.const -1
          i32.xor
          i32.const 7
          i32.shr_u
          local.get 1
          i32.const 6
          i32.shr_u
          i32.or
          i32.const 16843009
          i32.and
          local.get 0
          i32.add
          local.set 0
          local.get 5
          i32.const 2
          i32.eq
          br_if 0 (;@3;)
          local.get 2
          i32.load offset=8
          local.tee 2
          i32.const -1
          i32.xor
          i32.const 7
          i32.shr_u
          local.get 2
          i32.const 6
          i32.shr_u
          i32.or
          i32.const 16843009
          i32.and
          local.get 0
          i32.add
          local.set 0
        end
        local.get 0
        i32.const 8
        i32.shr_u
        i32.const 459007
        i32.and
        local.get 0
        i32.const 16711935
        i32.and
        i32.add
        i32.const 65537
        i32.mul
        i32.const 16
        i32.shr_u
        local.get 7
        i32.add
        return
      end
      block  ;; label = @2
        local.get 1
        br_if 0 (;@2;)
        i32.const 0
        return
      end
      local.get 1
      i32.const 3
      i32.and
      local.set 2
      block  ;; label = @2
        block  ;; label = @3
          local.get 1
          i32.const -1
          i32.add
          i32.const 3
          i32.ge_u
          br_if 0 (;@3;)
          i32.const 0
          local.set 7
          br 1 (;@2;)
        end
        local.get 1
        i32.const -4
        i32.and
        local.set 1
        i32.const 0
        local.set 7
        loop  ;; label = @3
          local.get 7
          local.get 0
          i32.load8_s
          i32.const -65
          i32.gt_s
          i32.add
          local.get 0
          i32.load8_s offset=1
          i32.const -65
          i32.gt_s
          i32.add
          local.get 0
          i32.load8_s offset=2
          i32.const -65
          i32.gt_s
          i32.add
          local.get 0
          i32.load8_s offset=3
          i32.const -65
          i32.gt_s
          i32.add
          local.set 7
          local.get 0
          i32.const 4
          i32.add
          local.set 0
          local.get 1
          i32.const -4
          i32.add
          local.tee 1
          br_if 0 (;@3;)
        end
      end
      local.get 2
      i32.eqz
      br_if 0 (;@1;)
      loop  ;; label = @2
        local.get 7
        local.get 0
        i32.load8_s
        i32.const -65
        i32.gt_s
        i32.add
        local.set 7
        local.get 0
        i32.const 1
        i32.add
        local.set 0
        local.get 2
        i32.const -1
        i32.add
        local.tee 2
        br_if 0 (;@2;)
      end
    end
    local.get 7)
  (func $_ZN4core3fmt9Formatter12pad_integral12write_prefix17h94cb7822dccdd843E (type 12) (param i32 i32 i32 i32 i32) (result i32)
    (local i32)
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 2
          i32.const 1114112
          i32.eq
          br_if 0 (;@3;)
          i32.const 1
          local.set 5
          local.get 0
          local.get 2
          local.get 1
          i32.load offset=16
          call_indirect (type 5)
          br_if 1 (;@2;)
        end
        local.get 3
        br_if 1 (;@1;)
        i32.const 0
        local.set 5
      end
      local.get 5
      return
    end
    local.get 0
    local.get 3
    local.get 4
    local.get 1
    i32.load offset=12
    call_indirect (type 7))
  (func $_ZN4core3str16slice_error_fail17h0b3eac3cd2cbe8e7E (type 11) (param i32 i32 i32 i32 i32)
    local.get 0
    local.get 1
    local.get 2
    local.get 3
    local.get 4
    call $_ZN4core3str19slice_error_fail_rt17hc0186f1dc2c6e9eaE
    unreachable)
  (func $_ZN4core3fmt9Formatter9write_fmt17h6645fe2bd1b938a8E (type 5) (param i32 i32) (result i32)
    (local i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 0
    i32.const 28
    i32.add
    i32.load
    local.set 3
    local.get 0
    i32.load offset=24
    local.set 0
    local.get 2
    i32.const 8
    i32.add
    i32.const 16
    i32.add
    local.get 1
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    i32.const 8
    i32.add
    i32.const 8
    i32.add
    local.get 1
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 1
    i64.load align=4
    i64.store offset=8
    local.get 0
    local.get 3
    local.get 2
    i32.const 8
    i32.add
    call $_ZN4core3fmt5write17hb15fae9d0358166aE
    local.set 1
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN4core3fmt9Formatter15debug_lower_hex17h3f763578aa6ee5baE (type 4) (param i32) (result i32)
    local.get 0
    i32.load8_u
    i32.const 16
    i32.and
    i32.const 4
    i32.shr_u)
  (func $_ZN4core3fmt9Formatter15debug_upper_hex17h96706c7c15afea4cE (type 4) (param i32) (result i32)
    local.get 0
    i32.load8_u
    i32.const 32
    i32.and
    i32.const 5
    i32.shr_u)
  (func $_ZN4core3fmt9Formatter12debug_struct17ha3f5ef7f2b046eb7E (type 9) (param i32 i32 i32 i32)
    local.get 1
    i32.load offset=24
    local.get 2
    local.get 3
    local.get 1
    i32.const 28
    i32.add
    i32.load
    i32.load offset=12
    call_indirect (type 7)
    local.set 3
    local.get 0
    i32.const 0
    i32.store8 offset=5
    local.get 0
    local.get 3
    i32.store8 offset=4
    local.get 0
    local.get 1
    i32.store)
  (func $_ZN4core3fmt9Formatter25debug_tuple_field1_finish17he0f5b2c5c9cb6b55E (type 12) (param i32 i32 i32 i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 5
    global.set $__stack_pointer
    local.get 5
    local.get 0
    i32.load offset=24
    local.get 1
    local.get 2
    local.get 0
    i32.const 28
    i32.add
    i32.load
    i32.load offset=12
    call_indirect (type 7)
    i32.store8 offset=8
    local.get 5
    local.get 0
    i32.store
    local.get 5
    local.get 2
    i32.eqz
    i32.store8 offset=9
    local.get 5
    i32.const 0
    i32.store offset=4
    local.get 5
    local.get 3
    local.get 4
    call $_ZN4core3fmt8builders10DebugTuple5field17hcf01775d6abf5a8bE
    local.set 1
    local.get 5
    i32.load8_u offset=8
    local.set 0
    block  ;; label = @1
      local.get 5
      i32.load offset=4
      local.tee 4
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const 255
      i32.and
      local.set 2
      i32.const 1
      local.set 0
      local.get 2
      br_if 0 (;@1;)
      local.get 1
      i32.load
      local.set 2
      block  ;; label = @2
        local.get 4
        i32.const 1
        i32.ne
        br_if 0 (;@2;)
        local.get 5
        i32.load8_u offset=9
        i32.const 255
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        local.get 2
        i32.load8_u
        i32.const 4
        i32.and
        br_if 0 (;@2;)
        i32.const 1
        local.set 0
        local.get 2
        i32.load offset=24
        i32.const 1055668
        i32.const 1
        local.get 2
        i32.const 28
        i32.add
        i32.load
        i32.load offset=12
        call_indirect (type 7)
        br_if 1 (;@1;)
      end
      local.get 2
      i32.load offset=24
      i32.const 1055267
      i32.const 1
      local.get 2
      i32.const 28
      i32.add
      i32.load
      i32.load offset=12
      call_indirect (type 7)
      local.set 0
    end
    local.get 5
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 0
    i32.const 255
    i32.and
    i32.const 0
    i32.ne)
  (func $_ZN4core3fmt9Formatter25debug_tuple_field2_finish17h2a3eb19364bc5cdcE (type 15) (param i32 i32 i32 i32 i32 i32 i32) (result i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 7
    global.set $__stack_pointer
    local.get 7
    local.get 0
    i32.load offset=24
    local.get 1
    local.get 2
    local.get 0
    i32.const 28
    i32.add
    i32.load
    i32.load offset=12
    call_indirect (type 7)
    i32.store8 offset=8
    local.get 7
    local.get 0
    i32.store
    local.get 7
    local.get 2
    i32.eqz
    i32.store8 offset=9
    local.get 7
    i32.const 0
    i32.store offset=4
    local.get 7
    local.get 3
    local.get 4
    call $_ZN4core3fmt8builders10DebugTuple5field17hcf01775d6abf5a8bE
    local.get 5
    local.get 6
    call $_ZN4core3fmt8builders10DebugTuple5field17hcf01775d6abf5a8bE
    local.set 1
    local.get 7
    i32.load8_u offset=8
    local.set 0
    block  ;; label = @1
      local.get 7
      i32.load offset=4
      local.tee 6
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const 255
      i32.and
      local.set 2
      i32.const 1
      local.set 0
      local.get 2
      br_if 0 (;@1;)
      local.get 1
      i32.load
      local.set 2
      block  ;; label = @2
        local.get 6
        i32.const 1
        i32.ne
        br_if 0 (;@2;)
        local.get 7
        i32.load8_u offset=9
        i32.const 255
        i32.and
        i32.eqz
        br_if 0 (;@2;)
        local.get 2
        i32.load8_u
        i32.const 4
        i32.and
        br_if 0 (;@2;)
        i32.const 1
        local.set 0
        local.get 2
        i32.load offset=24
        i32.const 1055668
        i32.const 1
        local.get 2
        i32.const 28
        i32.add
        i32.load
        i32.load offset=12
        call_indirect (type 7)
        br_if 1 (;@1;)
      end
      local.get 2
      i32.load offset=24
      i32.const 1055267
      i32.const 1
      local.get 2
      i32.const 28
      i32.add
      i32.load
      i32.load offset=12
      call_indirect (type 7)
      local.set 0
    end
    local.get 7
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 0
    i32.const 255
    i32.and
    i32.const 0
    i32.ne)
  (func $_ZN4core3fmt9Formatter10debug_list17ha46657161459126cE (type 3) (param i32 i32)
    (local i32)
    local.get 1
    i32.load offset=24
    i32.const 1055670
    i32.const 1
    local.get 1
    i32.const 28
    i32.add
    i32.load
    i32.load offset=12
    call_indirect (type 7)
    local.set 2
    local.get 0
    i32.const 0
    i32.store8 offset=5
    local.get 0
    local.get 2
    i32.store8 offset=4
    local.get 0
    local.get 1
    i32.store)
  (func $_ZN43_$LT$bool$u20$as$u20$core..fmt..Display$GT$3fmt17h1b70f6e38dbe21bfE (type 5) (param i32 i32) (result i32)
    block  ;; label = @1
      local.get 0
      i32.load8_u
      br_if 0 (;@1;)
      local.get 1
      i32.const 1055964
      i32.const 5
      call $_ZN4core3fmt9Formatter3pad17h66985344eb075eb9E
      return
    end
    local.get 1
    i32.const 1055960
    i32.const 4
    call $_ZN4core3fmt9Formatter3pad17h66985344eb075eb9E)
  (func $_ZN4core7unicode12unicode_data15grapheme_extend6lookup17hfa729d8e04b0f6eeE (type 4) (param i32) (result i32)
    (local i32 i32 i32 i32 i32)
    local.get 0
    i32.const 11
    i32.shl
    local.set 1
    i32.const 0
    local.set 2
    i32.const 33
    local.set 3
    i32.const 33
    local.set 4
    block  ;; label = @1
      block  ;; label = @2
        loop  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              i32.const -1
              local.get 3
              i32.const 1
              i32.shr_u
              local.get 2
              i32.add
              local.tee 5
              i32.const 2
              i32.shl
              i32.const 1058440
              i32.add
              i32.load
              i32.const 11
              i32.shl
              local.tee 3
              local.get 1
              i32.ne
              local.get 3
              local.get 1
              i32.lt_u
              select
              local.tee 3
              i32.const 1
              i32.ne
              br_if 0 (;@5;)
              local.get 5
              local.set 4
              br 1 (;@4;)
            end
            local.get 3
            i32.const 255
            i32.and
            i32.const 255
            i32.ne
            br_if 2 (;@2;)
            local.get 5
            i32.const 1
            i32.add
            local.set 2
          end
          local.get 4
          local.get 2
          i32.sub
          local.set 3
          local.get 4
          local.get 2
          i32.gt_u
          br_if 0 (;@3;)
          br 2 (;@1;)
        end
      end
      local.get 5
      i32.const 1
      i32.add
      local.set 2
    end
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          local.get 2
          i32.const 32
          i32.gt_u
          br_if 0 (;@3;)
          local.get 2
          i32.const 2
          i32.shl
          local.set 1
          i32.const 727
          local.set 4
          block  ;; label = @4
            local.get 2
            i32.const 32
            i32.eq
            br_if 0 (;@4;)
            local.get 1
            i32.const 1058444
            i32.add
            i32.load
            i32.const 21
            i32.shr_u
            local.set 4
          end
          i32.const 0
          local.set 3
          block  ;; label = @4
            local.get 2
            i32.const -1
            i32.add
            local.tee 5
            local.get 2
            i32.gt_u
            br_if 0 (;@4;)
            local.get 5
            i32.const 33
            i32.ge_u
            br_if 2 (;@2;)
            local.get 5
            i32.const 2
            i32.shl
            i32.const 1058440
            i32.add
            i32.load
            i32.const 2097151
            i32.and
            local.set 3
          end
          local.get 4
          local.get 1
          i32.const 1058440
          i32.add
          i32.load
          i32.const 21
          i32.shr_u
          local.tee 2
          i32.const -1
          i32.xor
          i32.add
          i32.eqz
          br_if 2 (;@1;)
          local.get 0
          local.get 3
          i32.sub
          local.set 3
          local.get 2
          i32.const 727
          local.get 2
          i32.const 727
          i32.gt_u
          select
          local.set 1
          local.get 4
          i32.const -1
          i32.add
          local.set 5
          i32.const 0
          local.set 4
          loop  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 1
                local.get 2
                i32.eq
                br_if 0 (;@6;)
                local.get 4
                local.get 2
                i32.const 1058572
                i32.add
                i32.load8_u
                i32.add
                local.tee 4
                local.get 3
                i32.le_u
                br_if 1 (;@5;)
                br 5 (;@1;)
              end
              local.get 1
              i32.const 727
              i32.const 1059300
              call $_ZN4core9panicking18panic_bounds_check17h2da60dc8be6aa455E
              unreachable
            end
            local.get 5
            local.get 2
            i32.const 1
            i32.add
            local.tee 2
            i32.ne
            br_if 0 (;@4;)
          end
          local.get 5
          local.set 2
          br 2 (;@1;)
        end
        local.get 2
        i32.const 33
        i32.const 1059300
        call $_ZN4core9panicking18panic_bounds_check17h2da60dc8be6aa455E
        unreachable
      end
      local.get 5
      i32.const 33
      i32.const 1058352
      call $_ZN4core9panicking18panic_bounds_check17h2da60dc8be6aa455E
      unreachable
    end
    local.get 2
    i32.const 1
    i32.and)
  (func $_ZN4core7unicode9printable12is_printable17heb774253d28758deE (type 4) (param i32) (result i32)
    (local i32)
    block  ;; label = @1
      local.get 0
      i32.const 32
      i32.ge_u
      br_if 0 (;@1;)
      i32.const 0
      return
    end
    i32.const 1
    local.set 1
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.const 127
        i32.lt_u
        br_if 0 (;@2;)
        local.get 0
        i32.const 65536
        i32.lt_u
        br_if 1 (;@1;)
        block  ;; label = @3
          block  ;; label = @4
            local.get 0
            i32.const 131072
            i32.lt_u
            br_if 0 (;@4;)
            block  ;; label = @5
              local.get 0
              i32.const -205744
              i32.add
              i32.const 712016
              i32.ge_u
              br_if 0 (;@5;)
              i32.const 0
              return
            end
            block  ;; label = @5
              local.get 0
              i32.const -201547
              i32.add
              i32.const 5
              i32.ge_u
              br_if 0 (;@5;)
              i32.const 0
              return
            end
            block  ;; label = @5
              local.get 0
              i32.const -195102
              i32.add
              i32.const 1506
              i32.ge_u
              br_if 0 (;@5;)
              i32.const 0
              return
            end
            block  ;; label = @5
              local.get 0
              i32.const -191457
              i32.add
              i32.const 3103
              i32.ge_u
              br_if 0 (;@5;)
              i32.const 0
              return
            end
            block  ;; label = @5
              local.get 0
              i32.const -183970
              i32.add
              i32.const 14
              i32.ge_u
              br_if 0 (;@5;)
              i32.const 0
              return
            end
            block  ;; label = @5
              local.get 0
              i32.const -2
              i32.and
              i32.const 178206
              i32.ne
              br_if 0 (;@5;)
              i32.const 0
              return
            end
            local.get 0
            i32.const -32
            i32.and
            i32.const 173792
            i32.ne
            br_if 1 (;@3;)
            i32.const 0
            return
          end
          local.get 0
          i32.const 1057578
          i32.const 44
          i32.const 1057666
          i32.const 196
          i32.const 1057862
          i32.const 450
          call $_ZN4core7unicode9printable5check17h47620c40433dc151E
          return
        end
        i32.const 0
        local.set 1
        local.get 0
        i32.const -177978
        i32.add
        i32.const 6
        i32.lt_u
        br_if 0 (;@2;)
        local.get 0
        i32.const -1114112
        i32.add
        i32.const -196112
        i32.lt_u
        local.set 1
      end
      local.get 1
      return
    end
    local.get 0
    i32.const 1056908
    i32.const 40
    i32.const 1056988
    i32.const 287
    i32.const 1057275
    i32.const 303
    call $_ZN4core7unicode9printable5check17h47620c40433dc151E)
  (func $_ZN42_$LT$str$u20$as$u20$core..fmt..Display$GT$3fmt17h8fa90afa7600905bE (type 7) (param i32 i32 i32) (result i32)
    local.get 2
    local.get 0
    local.get 1
    call $_ZN4core3fmt9Formatter3pad17h66985344eb075eb9E)
  (func $_ZN41_$LT$char$u20$as$u20$core..fmt..Debug$GT$3fmt17h0b06e1679f51847fE (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    i32.const 1
    local.set 2
    block  ;; label = @1
      local.get 1
      i32.load offset=24
      local.tee 3
      i32.const 39
      local.get 1
      i32.const 28
      i32.add
      i32.load
      i32.load offset=16
      local.tee 4
      call_indirect (type 5)
      br_if 0 (;@1;)
      i32.const 1114114
      local.set 1
      i32.const 48
      local.set 5
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        local.get 0
                        i32.load
                        local.tee 2
                        br_table 8 (;@2;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 2 (;@8;) 4 (;@6;) 1 (;@9;) 1 (;@9;) 3 (;@7;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 1 (;@9;) 5 (;@5;) 0 (;@10;)
                      end
                      local.get 2
                      i32.const 92
                      i32.eq
                      br_if 4 (;@5;)
                    end
                    local.get 2
                    call $_ZN4core7unicode12unicode_data15grapheme_extend6lookup17hfa729d8e04b0f6eeE
                    i32.eqz
                    br_if 4 (;@4;)
                    local.get 2
                    i32.const 1
                    i32.or
                    i32.clz
                    i32.const 2
                    i32.shr_u
                    i32.const 7
                    i32.xor
                    local.set 5
                    br 5 (;@3;)
                  end
                  i32.const 116
                  local.set 5
                  br 5 (;@2;)
                end
                i32.const 114
                local.set 5
                br 4 (;@2;)
              end
              i32.const 110
              local.set 5
              br 3 (;@2;)
            end
            local.get 2
            local.set 5
            br 2 (;@2;)
          end
          i32.const 1114113
          local.set 1
          block  ;; label = @4
            local.get 2
            call $_ZN4core7unicode9printable12is_printable17heb774253d28758deE
            i32.eqz
            br_if 0 (;@4;)
            local.get 2
            local.set 5
            br 2 (;@2;)
          end
          local.get 2
          i32.const 1
          i32.or
          i32.clz
          i32.const 2
          i32.shr_u
          i32.const 7
          i32.xor
          local.set 5
        end
        local.get 2
        local.set 1
      end
      i32.const 5
      local.set 6
      loop  ;; label = @2
        local.get 6
        local.set 7
        local.get 1
        local.set 0
        i32.const 1114113
        local.set 1
        i32.const 92
        local.set 2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    local.get 0
                    i32.const -1114112
                    i32.add
                    local.tee 8
                    i32.const 3
                    local.get 8
                    i32.const 3
                    i32.lt_u
                    select
                    br_table 2 (;@6;) 1 (;@7;) 5 (;@3;) 0 (;@8;) 2 (;@6;)
                  end
                  i32.const 0
                  local.set 6
                  i32.const 125
                  local.set 2
                  local.get 0
                  local.set 1
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        local.get 7
                        i32.const 255
                        i32.and
                        br_table 4 (;@6;) 7 (;@3;) 5 (;@5;) 0 (;@10;) 1 (;@9;) 2 (;@8;) 4 (;@6;)
                      end
                      i32.const 2
                      local.set 6
                      i32.const 123
                      local.set 2
                      br 5 (;@4;)
                    end
                    i32.const 3
                    local.set 6
                    i32.const 117
                    local.set 2
                    br 4 (;@4;)
                  end
                  i32.const 4
                  local.set 6
                  i32.const 92
                  local.set 2
                  br 3 (;@4;)
                end
                i32.const 1114112
                local.set 1
                local.get 5
                local.set 2
                local.get 7
                local.set 6
                local.get 5
                i32.const 1114112
                i32.ne
                br_if 3 (;@3;)
              end
              local.get 3
              i32.const 39
              local.get 4
              call_indirect (type 5)
              local.set 2
              br 4 (;@1;)
            end
            local.get 7
            i32.const 1
            local.get 5
            select
            local.set 6
            i32.const 48
            i32.const 87
            local.get 0
            local.get 5
            i32.const 2
            i32.shl
            i32.shr_u
            i32.const 15
            i32.and
            local.tee 1
            i32.const 10
            i32.lt_u
            select
            local.get 1
            i32.add
            local.set 2
            local.get 5
            i32.const -1
            i32.add
            i32.const 0
            local.get 5
            select
            local.set 5
          end
          local.get 0
          local.set 1
        end
        local.get 3
        local.get 2
        local.get 4
        call_indirect (type 5)
        i32.eqz
        br_if 0 (;@2;)
      end
      i32.const 1
      return
    end
    local.get 2)
  (func $_ZN4core5slice6memchr7memrchr17h93afa4aa8611e14aE (type 9) (param i32 i32 i32 i32)
    (local i32 i32 i32 i32 i32)
    local.get 3
    i32.const 0
    local.get 3
    local.get 2
    i32.const 3
    i32.add
    i32.const -4
    i32.and
    local.get 2
    i32.sub
    local.tee 4
    i32.sub
    i32.const 7
    i32.and
    local.get 3
    local.get 4
    i32.lt_u
    select
    local.tee 5
    i32.sub
    local.set 6
    block  ;; label = @1
      block  ;; label = @2
        local.get 3
        local.get 5
        i32.lt_u
        br_if 0 (;@2;)
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 5
              i32.eqz
              br_if 0 (;@5;)
              local.get 2
              local.get 3
              i32.add
              local.tee 5
              local.get 2
              local.get 6
              i32.add
              local.tee 7
              i32.sub
              local.set 8
              block  ;; label = @6
                local.get 5
                i32.const -1
                i32.add
                local.tee 5
                i32.load8_u
                local.get 1
                i32.const 255
                i32.and
                i32.ne
                br_if 0 (;@6;)
                local.get 8
                i32.const -1
                i32.add
                local.get 6
                i32.add
                local.set 4
                br 2 (;@4;)
              end
              local.get 7
              local.get 5
              i32.eq
              br_if 0 (;@5;)
              block  ;; label = @6
                local.get 5
                i32.const -1
                i32.add
                local.tee 5
                i32.load8_u
                local.get 1
                i32.const 255
                i32.and
                i32.ne
                br_if 0 (;@6;)
                local.get 8
                i32.const -2
                i32.add
                local.get 6
                i32.add
                local.set 4
                br 2 (;@4;)
              end
              local.get 7
              local.get 5
              i32.eq
              br_if 0 (;@5;)
              block  ;; label = @6
                local.get 5
                i32.const -1
                i32.add
                local.tee 5
                i32.load8_u
                local.get 1
                i32.const 255
                i32.and
                i32.ne
                br_if 0 (;@6;)
                local.get 8
                i32.const -3
                i32.add
                local.get 6
                i32.add
                local.set 4
                br 2 (;@4;)
              end
              local.get 7
              local.get 5
              i32.eq
              br_if 0 (;@5;)
              block  ;; label = @6
                local.get 5
                i32.const -1
                i32.add
                local.tee 5
                i32.load8_u
                local.get 1
                i32.const 255
                i32.and
                i32.ne
                br_if 0 (;@6;)
                local.get 8
                i32.const -4
                i32.add
                local.get 6
                i32.add
                local.set 4
                br 2 (;@4;)
              end
              local.get 7
              local.get 5
              i32.eq
              br_if 0 (;@5;)
              block  ;; label = @6
                local.get 5
                i32.const -1
                i32.add
                local.tee 5
                i32.load8_u
                local.get 1
                i32.const 255
                i32.and
                i32.ne
                br_if 0 (;@6;)
                local.get 8
                i32.const -5
                i32.add
                local.get 6
                i32.add
                local.set 4
                br 2 (;@4;)
              end
              local.get 7
              local.get 5
              i32.eq
              br_if 0 (;@5;)
              block  ;; label = @6
                local.get 5
                i32.const -1
                i32.add
                local.tee 5
                i32.load8_u
                local.get 1
                i32.const 255
                i32.and
                i32.ne
                br_if 0 (;@6;)
                local.get 8
                i32.const -6
                i32.add
                local.get 6
                i32.add
                local.set 4
                br 2 (;@4;)
              end
              local.get 7
              local.get 5
              i32.eq
              br_if 0 (;@5;)
              block  ;; label = @6
                local.get 5
                i32.const -1
                i32.add
                local.tee 5
                i32.load8_u
                local.get 1
                i32.const 255
                i32.and
                i32.ne
                br_if 0 (;@6;)
                local.get 8
                i32.const -7
                i32.add
                local.get 6
                i32.add
                local.set 4
                br 2 (;@4;)
              end
              local.get 7
              local.get 5
              i32.eq
              br_if 0 (;@5;)
              local.get 8
              i32.const -8
              i32.add
              local.get 6
              i32.add
              local.set 4
              br 1 (;@4;)
            end
            local.get 4
            local.get 3
            local.get 3
            local.get 4
            i32.gt_u
            select
            local.set 7
            local.get 1
            i32.const 255
            i32.and
            i32.const 16843009
            i32.mul
            local.set 5
            block  ;; label = @5
              loop  ;; label = @6
                local.get 6
                local.tee 4
                local.get 7
                i32.le_u
                br_if 1 (;@5;)
                local.get 2
                local.get 4
                i32.add
                local.tee 8
                i32.const -8
                i32.add
                i32.load
                local.get 5
                i32.xor
                local.tee 6
                i32.const -1
                i32.xor
                local.get 6
                i32.const -16843009
                i32.add
                i32.and
                i32.const -2139062144
                i32.and
                br_if 1 (;@5;)
                local.get 4
                i32.const -8
                i32.add
                local.set 6
                local.get 8
                i32.const -4
                i32.add
                i32.load
                local.get 5
                i32.xor
                local.tee 8
                i32.const -1
                i32.xor
                local.get 8
                i32.const -16843009
                i32.add
                i32.and
                i32.const -2139062144
                i32.and
                i32.eqz
                br_if 0 (;@6;)
              end
            end
            local.get 4
            local.get 3
            i32.gt_u
            br_if 3 (;@1;)
            local.get 2
            i32.const -1
            i32.add
            local.set 5
            local.get 1
            i32.const 255
            i32.and
            local.set 8
            loop  ;; label = @5
              block  ;; label = @6
                local.get 4
                br_if 0 (;@6;)
                i32.const 0
                local.set 6
                br 3 (;@3;)
              end
              local.get 5
              local.get 4
              i32.add
              local.set 6
              local.get 4
              i32.const -1
              i32.add
              local.set 4
              local.get 6
              i32.load8_u
              local.get 8
              i32.ne
              br_if 0 (;@5;)
            end
          end
          i32.const 1
          local.set 6
        end
        local.get 0
        local.get 4
        i32.store offset=4
        local.get 0
        local.get 6
        i32.store
        return
      end
      local.get 6
      local.get 3
      i32.const 1056020
      call $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E
      unreachable
    end
    local.get 4
    local.get 3
    i32.const 1056036
    call $_ZN4core5slice5index24slice_end_index_len_fail17h8cef88544c34b3bfE
    unreachable)
  (func $_ZN4core5slice5index29slice_start_index_len_fail_rt17hea3fab39cd6dbb96E (type 6) (param i32 i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    local.get 1
    i32.store offset=4
    local.get 3
    local.get 0
    i32.store
    local.get 3
    i32.const 28
    i32.add
    i32.const 2
    i32.store
    local.get 3
    i32.const 44
    i32.add
    i32.const 4
    i32.store
    local.get 3
    i64.const 2
    i64.store offset=12 align=4
    local.get 3
    i32.const 1056104
    i32.store offset=8
    local.get 3
    i32.const 4
    i32.store offset=36
    local.get 3
    local.get 3
    i32.const 32
    i32.add
    i32.store offset=24
    local.get 3
    local.get 3
    i32.const 4
    i32.add
    i32.store offset=40
    local.get 3
    local.get 3
    i32.store offset=32
    local.get 3
    i32.const 8
    i32.add
    local.get 2
    call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
    unreachable)
  (func $_ZN4core5slice5index27slice_end_index_len_fail_rt17ha5458ee76f940e9bE (type 6) (param i32 i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    local.get 1
    i32.store offset=4
    local.get 3
    local.get 0
    i32.store
    local.get 3
    i32.const 28
    i32.add
    i32.const 2
    i32.store
    local.get 3
    i32.const 44
    i32.add
    i32.const 4
    i32.store
    local.get 3
    i64.const 2
    i64.store offset=12 align=4
    local.get 3
    i32.const 1056136
    i32.store offset=8
    local.get 3
    i32.const 4
    i32.store offset=36
    local.get 3
    local.get 3
    i32.const 32
    i32.add
    i32.store offset=24
    local.get 3
    local.get 3
    i32.const 4
    i32.add
    i32.store offset=40
    local.get 3
    local.get 3
    i32.store offset=32
    local.get 3
    i32.const 8
    i32.add
    local.get 2
    call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
    unreachable)
  (func $_ZN4core5slice5index25slice_index_order_fail_rt17hd4b3fe4810f4fc3bE (type 6) (param i32 i32 i32)
    (local i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    local.get 3
    local.get 1
    i32.store offset=4
    local.get 3
    local.get 0
    i32.store
    local.get 3
    i32.const 28
    i32.add
    i32.const 2
    i32.store
    local.get 3
    i32.const 44
    i32.add
    i32.const 4
    i32.store
    local.get 3
    i64.const 2
    i64.store offset=12 align=4
    local.get 3
    i32.const 1056188
    i32.store offset=8
    local.get 3
    i32.const 4
    i32.store offset=36
    local.get 3
    local.get 3
    i32.const 32
    i32.add
    i32.store offset=24
    local.get 3
    local.get 3
    i32.const 4
    i32.add
    i32.store offset=40
    local.get 3
    local.get 3
    i32.store offset=32
    local.get 3
    i32.const 8
    i32.add
    local.get 2
    call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
    unreachable)
  (func $_ZN4core3fmt3num3imp51_$LT$impl$u20$core..fmt..Display$u20$for$u20$u8$GT$3fmt17h508a1bd06037f818E (type 5) (param i32 i32) (result i32)
    local.get 0
    i64.load8_u
    i32.const 1
    local.get 1
    call $_ZN4core3fmt3num3imp7fmt_u6417h5750e7de7c1be42fE)
  (func $_ZN60_$LT$core..str..lossy..Debug$u20$as$u20$core..fmt..Debug$GT$3fmt17h56048af2e252c929E (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 64
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    i32.const 1
    local.set 3
    block  ;; label = @1
      block  ;; label = @2
        local.get 1
        i32.load offset=24
        local.tee 4
        i32.const 34
        local.get 1
        i32.const 28
        i32.add
        i32.load
        local.tee 5
        i32.load offset=16
        local.tee 6
        call_indirect (type 5)
        br_if 0 (;@2;)
        local.get 2
        local.get 0
        i64.load align=4
        i64.store
        local.get 2
        i32.const 8
        i32.add
        local.get 2
        call $_ZN87_$LT$core..str..lossy..Utf8Chunks$u20$as$u20$core..iter..traits..iterator..Iterator$GT$4next17h03729e82afdd847bE
        block  ;; label = @3
          local.get 2
          i32.load offset=8
          local.tee 7
          i32.eqz
          br_if 0 (;@3;)
          loop  ;; label = @4
            local.get 2
            i32.load offset=20
            local.set 8
            local.get 2
            i32.load offset=16
            local.set 9
            i32.const 0
            local.set 3
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 2
                  i32.load offset=12
                  local.tee 10
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 7
                  local.get 10
                  i32.add
                  local.set 11
                  i32.const 0
                  local.set 3
                  i32.const 0
                  local.set 12
                  local.get 7
                  local.set 13
                  block  ;; label = @8
                    loop  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          local.get 13
                          i32.load8_s
                          local.tee 1
                          i32.const -1
                          i32.le_s
                          br_if 0 (;@11;)
                          local.get 13
                          i32.const 1
                          i32.add
                          local.set 14
                          local.get 1
                          i32.const 255
                          i32.and
                          local.set 15
                          br 1 (;@10;)
                        end
                        local.get 13
                        i32.load8_u offset=1
                        i32.const 63
                        i32.and
                        local.set 16
                        local.get 1
                        i32.const 31
                        i32.and
                        local.set 0
                        block  ;; label = @11
                          local.get 1
                          i32.const -33
                          i32.gt_u
                          br_if 0 (;@11;)
                          local.get 0
                          i32.const 6
                          i32.shl
                          local.get 16
                          i32.or
                          local.set 15
                          local.get 13
                          i32.const 2
                          i32.add
                          local.set 14
                          br 1 (;@10;)
                        end
                        local.get 16
                        i32.const 6
                        i32.shl
                        local.get 13
                        i32.load8_u offset=2
                        i32.const 63
                        i32.and
                        i32.or
                        local.set 16
                        local.get 13
                        i32.const 3
                        i32.add
                        local.set 14
                        block  ;; label = @11
                          local.get 1
                          i32.const -16
                          i32.ge_u
                          br_if 0 (;@11;)
                          local.get 16
                          local.get 0
                          i32.const 12
                          i32.shl
                          i32.or
                          local.set 15
                          br 1 (;@10;)
                        end
                        local.get 16
                        i32.const 6
                        i32.shl
                        local.get 14
                        i32.load8_u
                        i32.const 63
                        i32.and
                        i32.or
                        local.get 0
                        i32.const 18
                        i32.shl
                        i32.const 1835008
                        i32.and
                        i32.or
                        local.tee 15
                        i32.const 1114112
                        i32.eq
                        br_if 2 (;@8;)
                        local.get 13
                        i32.const 4
                        i32.add
                        local.set 14
                      end
                      i32.const 48
                      local.set 17
                      i32.const 1114114
                      local.set 1
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            block  ;; label = @13
                              block  ;; label = @14
                                block  ;; label = @15
                                  block  ;; label = @16
                                    block  ;; label = @17
                                      block  ;; label = @18
                                        local.get 15
                                        br_table 8 (;@10;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 2 (;@16;) 4 (;@14;) 1 (;@17;) 1 (;@17;) 3 (;@15;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 5 (;@13;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 1 (;@17;) 5 (;@13;) 0 (;@18;)
                                      end
                                      local.get 15
                                      i32.const 92
                                      i32.eq
                                      br_if 4 (;@13;)
                                    end
                                    local.get 15
                                    call $_ZN4core7unicode12unicode_data15grapheme_extend6lookup17hfa729d8e04b0f6eeE
                                    i32.eqz
                                    br_if 4 (;@12;)
                                    local.get 15
                                    i32.const 1
                                    i32.or
                                    i32.clz
                                    i32.const 2
                                    i32.shr_u
                                    i32.const 7
                                    i32.xor
                                    local.set 17
                                    br 5 (;@11;)
                                  end
                                  i32.const 116
                                  local.set 17
                                  br 5 (;@10;)
                                end
                                i32.const 114
                                local.set 17
                                br 4 (;@10;)
                              end
                              i32.const 110
                              local.set 17
                              br 3 (;@10;)
                            end
                            local.get 15
                            local.set 17
                            br 2 (;@10;)
                          end
                          i32.const 1114113
                          local.set 1
                          local.get 15
                          local.set 17
                          local.get 15
                          call $_ZN4core7unicode9printable12is_printable17heb774253d28758deE
                          br_if 1 (;@10;)
                          local.get 15
                          i32.const 1
                          i32.or
                          i32.clz
                          i32.const 2
                          i32.shr_u
                          i32.const 7
                          i32.xor
                          local.set 17
                        end
                        local.get 15
                        local.set 1
                      end
                      block  ;; label = @10
                        block  ;; label = @11
                          local.get 1
                          i32.const -1114112
                          i32.add
                          local.tee 0
                          i32.const 3
                          local.get 0
                          i32.const 3
                          i32.lt_u
                          select
                          i32.const 1
                          i32.eq
                          br_if 0 (;@11;)
                          local.get 12
                          local.get 3
                          i32.lt_u
                          br_if 1 (;@10;)
                          block  ;; label = @12
                            local.get 3
                            i32.eqz
                            br_if 0 (;@12;)
                            block  ;; label = @13
                              local.get 10
                              local.get 3
                              i32.gt_u
                              br_if 0 (;@13;)
                              local.get 10
                              local.get 3
                              i32.eq
                              br_if 1 (;@12;)
                              br 3 (;@10;)
                            end
                            local.get 7
                            local.get 3
                            i32.add
                            i32.load8_s
                            i32.const -64
                            i32.lt_s
                            br_if 2 (;@10;)
                          end
                          block  ;; label = @12
                            local.get 12
                            i32.eqz
                            br_if 0 (;@12;)
                            block  ;; label = @13
                              local.get 12
                              local.get 10
                              i32.lt_u
                              br_if 0 (;@13;)
                              local.get 12
                              local.get 10
                              i32.ne
                              br_if 3 (;@10;)
                              br 1 (;@12;)
                            end
                            local.get 7
                            local.get 12
                            i32.add
                            i32.load8_s
                            i32.const -65
                            i32.le_s
                            br_if 2 (;@10;)
                          end
                          local.get 4
                          local.get 7
                          local.get 3
                          i32.add
                          local.get 12
                          local.get 3
                          i32.sub
                          local.get 5
                          i32.load offset=12
                          call_indirect (type 7)
                          br_if 5 (;@6;)
                          i32.const 5
                          local.set 18
                          loop  ;; label = @12
                            local.get 18
                            local.set 19
                            local.get 1
                            local.set 0
                            i32.const 1114113
                            local.set 1
                            i32.const 92
                            local.set 3
                            block  ;; label = @13
                              block  ;; label = @14
                                block  ;; label = @15
                                  block  ;; label = @16
                                    block  ;; label = @17
                                      block  ;; label = @18
                                        local.get 0
                                        i32.const -1114112
                                        i32.add
                                        local.tee 16
                                        i32.const 3
                                        local.get 16
                                        i32.const 3
                                        i32.lt_u
                                        select
                                        br_table 2 (;@16;) 1 (;@17;) 5 (;@13;) 0 (;@18;) 2 (;@16;)
                                      end
                                      i32.const 0
                                      local.set 18
                                      i32.const 125
                                      local.set 3
                                      local.get 0
                                      local.set 1
                                      block  ;; label = @18
                                        block  ;; label = @19
                                          block  ;; label = @20
                                            local.get 19
                                            i32.const 255
                                            i32.and
                                            br_table 4 (;@16;) 7 (;@13;) 5 (;@15;) 0 (;@20;) 1 (;@19;) 2 (;@18;) 4 (;@16;)
                                          end
                                          i32.const 2
                                          local.set 18
                                          i32.const 123
                                          local.set 3
                                          br 5 (;@14;)
                                        end
                                        i32.const 3
                                        local.set 18
                                        i32.const 117
                                        local.set 3
                                        br 4 (;@14;)
                                      end
                                      i32.const 4
                                      local.set 18
                                      i32.const 92
                                      local.set 3
                                      br 3 (;@14;)
                                    end
                                    i32.const 1114112
                                    local.set 1
                                    local.get 17
                                    local.set 3
                                    local.get 19
                                    local.set 18
                                    local.get 17
                                    i32.const 1114112
                                    i32.ne
                                    br_if 3 (;@13;)
                                  end
                                  i32.const 1
                                  local.set 1
                                  block  ;; label = @16
                                    local.get 15
                                    i32.const 128
                                    i32.lt_u
                                    br_if 0 (;@16;)
                                    i32.const 2
                                    local.set 1
                                    local.get 15
                                    i32.const 2048
                                    i32.lt_u
                                    br_if 0 (;@16;)
                                    i32.const 3
                                    i32.const 4
                                    local.get 15
                                    i32.const 65536
                                    i32.lt_u
                                    select
                                    local.set 1
                                  end
                                  local.get 1
                                  local.get 12
                                  i32.add
                                  local.set 3
                                  br 4 (;@11;)
                                end
                                local.get 19
                                i32.const 1
                                local.get 17
                                select
                                local.set 18
                                i32.const 48
                                i32.const 87
                                local.get 0
                                local.get 17
                                i32.const 2
                                i32.shl
                                i32.shr_u
                                i32.const 15
                                i32.and
                                local.tee 1
                                i32.const 10
                                i32.lt_u
                                select
                                local.get 1
                                i32.add
                                local.set 3
                                local.get 17
                                i32.const -1
                                i32.add
                                i32.const 0
                                local.get 17
                                select
                                local.set 17
                              end
                              local.get 0
                              local.set 1
                            end
                            local.get 4
                            local.get 3
                            local.get 6
                            call_indirect (type 5)
                            i32.eqz
                            br_if 0 (;@12;)
                            br 6 (;@6;)
                          end
                        end
                        local.get 12
                        local.get 13
                        i32.sub
                        local.get 14
                        i32.add
                        local.set 12
                        local.get 14
                        local.set 13
                        local.get 14
                        local.get 11
                        i32.ne
                        br_if 1 (;@9;)
                        br 2 (;@8;)
                      end
                    end
                    local.get 7
                    local.get 10
                    local.get 3
                    local.get 12
                    i32.const 1056492
                    call $_ZN4core3str16slice_error_fail17h0b3eac3cd2cbe8e7E
                    unreachable
                  end
                  block  ;; label = @8
                    local.get 3
                    br_if 0 (;@8;)
                    i32.const 0
                    local.set 3
                    br 1 (;@7;)
                  end
                  block  ;; label = @8
                    local.get 10
                    local.get 3
                    i32.gt_u
                    br_if 0 (;@8;)
                    local.get 10
                    local.get 3
                    i32.eq
                    br_if 1 (;@7;)
                    br 7 (;@1;)
                  end
                  local.get 7
                  local.get 3
                  i32.add
                  i32.load8_s
                  i32.const -65
                  i32.le_s
                  br_if 6 (;@1;)
                end
                local.get 4
                local.get 7
                local.get 3
                i32.add
                local.get 10
                local.get 3
                i32.sub
                local.get 5
                i32.load offset=12
                call_indirect (type 7)
                br_if 0 (;@6;)
                local.get 8
                i32.eqz
                br_if 1 (;@5;)
                loop  ;; label = @7
                  local.get 2
                  local.get 9
                  i32.load8_u
                  i32.store8 offset=31
                  local.get 2
                  i32.const 74
                  i32.store offset=36
                  local.get 2
                  local.get 2
                  i32.const 31
                  i32.add
                  i32.store offset=32
                  local.get 2
                  i32.const 1
                  i32.store offset=60
                  local.get 2
                  i32.const 1
                  i32.store offset=52
                  local.get 2
                  i32.const 1056536
                  i32.store offset=48
                  local.get 2
                  i32.const 1
                  i32.store offset=44
                  local.get 2
                  i32.const 1056528
                  i32.store offset=40
                  local.get 2
                  local.get 2
                  i32.const 32
                  i32.add
                  i32.store offset=56
                  local.get 4
                  local.get 5
                  local.get 2
                  i32.const 40
                  i32.add
                  call $_ZN4core3fmt5write17hb15fae9d0358166aE
                  br_if 1 (;@6;)
                  local.get 9
                  i32.const 1
                  i32.add
                  local.set 9
                  local.get 8
                  i32.const -1
                  i32.add
                  local.tee 8
                  i32.eqz
                  br_if 2 (;@5;)
                  br 0 (;@7;)
                end
              end
              i32.const 1
              local.set 3
              br 3 (;@2;)
            end
            local.get 2
            i32.const 8
            i32.add
            local.get 2
            call $_ZN87_$LT$core..str..lossy..Utf8Chunks$u20$as$u20$core..iter..traits..iterator..Iterator$GT$4next17h03729e82afdd847bE
            local.get 2
            i32.load offset=8
            local.tee 7
            br_if 0 (;@4;)
          end
        end
        local.get 4
        i32.const 34
        local.get 6
        call_indirect (type 5)
        local.set 3
      end
      local.get 2
      i32.const 64
      i32.add
      global.set $__stack_pointer
      local.get 3
      return
    end
    local.get 7
    local.get 10
    local.get 3
    local.get 10
    i32.const 1056508
    call $_ZN4core3str16slice_error_fail17h0b3eac3cd2cbe8e7E
    unreachable)
  (func $_ZN87_$LT$core..str..lossy..Utf8Chunks$u20$as$u20$core..iter..traits..iterator..Iterator$GT$4next17h03729e82afdd847bE (type 3) (param i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    block  ;; label = @1
      local.get 1
      i32.load offset=4
      local.tee 2
      i32.eqz
      br_if 0 (;@1;)
      local.get 1
      i32.load
      local.set 3
      i32.const 0
      local.set 4
      block  ;; label = @2
        loop  ;; label = @3
          local.get 4
          i32.const 1
          i32.add
          local.set 5
          block  ;; label = @4
            block  ;; label = @5
              local.get 3
              local.get 4
              i32.add
              i32.load8_u
              local.tee 6
              i32.const 24
              i32.shl
              i32.const 24
              i32.shr_s
              local.tee 7
              i32.const 0
              i32.lt_s
              br_if 0 (;@5;)
              local.get 5
              local.set 4
              br 1 (;@4;)
            end
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          local.get 6
                          i32.const 1056204
                          i32.add
                          i32.load8_u
                          i32.const -2
                          i32.add
                          br_table 0 (;@11;) 1 (;@10;) 2 (;@9;) 9 (;@2;)
                        end
                        local.get 3
                        local.get 5
                        i32.add
                        i32.const 1055294
                        local.get 5
                        local.get 2
                        i32.lt_u
                        select
                        i32.load8_u
                        i32.const 192
                        i32.and
                        i32.const 128
                        i32.ne
                        br_if 8 (;@2;)
                        local.get 4
                        i32.const 2
                        i32.add
                        local.set 4
                        br 6 (;@4;)
                      end
                      local.get 3
                      local.get 5
                      i32.add
                      i32.const 1055294
                      local.get 5
                      local.get 2
                      i32.lt_u
                      select
                      i32.load8_s
                      local.set 8
                      local.get 6
                      i32.const -224
                      i32.add
                      br_table 1 (;@8;) 3 (;@6;) 3 (;@6;) 3 (;@6;) 3 (;@6;) 3 (;@6;) 3 (;@6;) 3 (;@6;) 3 (;@6;) 3 (;@6;) 3 (;@6;) 3 (;@6;) 3 (;@6;) 2 (;@7;) 3 (;@6;)
                    end
                    local.get 3
                    local.get 5
                    i32.add
                    i32.const 1055294
                    local.get 5
                    local.get 2
                    i32.lt_u
                    select
                    i32.load8_s
                    local.set 8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            local.get 6
                            i32.const -240
                            i32.add
                            br_table 1 (;@11;) 0 (;@12;) 0 (;@12;) 0 (;@12;) 2 (;@10;) 0 (;@12;)
                          end
                          local.get 7
                          i32.const 15
                          i32.add
                          i32.const 255
                          i32.and
                          i32.const 2
                          i32.gt_u
                          br_if 9 (;@2;)
                          local.get 8
                          i32.const -1
                          i32.gt_s
                          br_if 9 (;@2;)
                          local.get 8
                          i32.const -64
                          i32.ge_u
                          br_if 9 (;@2;)
                          br 2 (;@9;)
                        end
                        local.get 8
                        i32.const 112
                        i32.add
                        i32.const 255
                        i32.and
                        i32.const 48
                        i32.ge_u
                        br_if 8 (;@2;)
                        br 1 (;@9;)
                      end
                      local.get 8
                      i32.const -113
                      i32.gt_s
                      br_if 7 (;@2;)
                    end
                    local.get 3
                    local.get 4
                    i32.const 2
                    i32.add
                    local.tee 5
                    i32.add
                    i32.const 1055294
                    local.get 5
                    local.get 2
                    i32.lt_u
                    select
                    i32.load8_u
                    i32.const 192
                    i32.and
                    i32.const 128
                    i32.ne
                    br_if 6 (;@2;)
                    local.get 3
                    local.get 4
                    i32.const 3
                    i32.add
                    local.tee 5
                    i32.add
                    i32.const 1055294
                    local.get 5
                    local.get 2
                    i32.lt_u
                    select
                    i32.load8_u
                    i32.const 192
                    i32.and
                    i32.const 128
                    i32.ne
                    br_if 6 (;@2;)
                    local.get 4
                    i32.const 4
                    i32.add
                    local.set 4
                    br 4 (;@4;)
                  end
                  local.get 8
                  i32.const -32
                  i32.and
                  i32.const -96
                  i32.ne
                  br_if 5 (;@2;)
                  br 2 (;@5;)
                end
                local.get 8
                i32.const -96
                i32.ge_s
                br_if 4 (;@2;)
                br 1 (;@5;)
              end
              block  ;; label = @6
                local.get 7
                i32.const 31
                i32.add
                i32.const 255
                i32.and
                i32.const 12
                i32.lt_u
                br_if 0 (;@6;)
                local.get 7
                i32.const -2
                i32.and
                i32.const -18
                i32.ne
                br_if 4 (;@2;)
                local.get 8
                i32.const -1
                i32.gt_s
                br_if 4 (;@2;)
                local.get 8
                i32.const -64
                i32.ge_u
                br_if 4 (;@2;)
                br 1 (;@5;)
              end
              local.get 8
              i32.const -65
              i32.gt_s
              br_if 3 (;@2;)
            end
            local.get 3
            local.get 4
            i32.const 2
            i32.add
            local.tee 5
            i32.add
            i32.const 1055294
            local.get 5
            local.get 2
            i32.lt_u
            select
            i32.load8_u
            i32.const 192
            i32.and
            i32.const 128
            i32.ne
            br_if 2 (;@2;)
            local.get 4
            i32.const 3
            i32.add
            local.set 4
          end
          local.get 4
          local.set 5
          local.get 4
          local.get 2
          i32.lt_u
          br_if 0 (;@3;)
        end
      end
      local.get 0
      local.get 4
      i32.store offset=4
      local.get 0
      local.get 3
      i32.store
      local.get 1
      local.get 2
      local.get 5
      i32.sub
      i32.store offset=4
      local.get 1
      local.get 3
      local.get 5
      i32.add
      i32.store
      local.get 0
      local.get 5
      local.get 4
      i32.sub
      i32.store offset=12
      local.get 0
      local.get 3
      local.get 4
      i32.add
      i32.store offset=8
      return
    end
    local.get 0
    i32.const 0
    i32.store)
  (func $_ZN4core3fmt3num52_$LT$impl$u20$core..fmt..UpperHex$u20$for$u20$i8$GT$3fmt17h7caa790059f60235E (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 128
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 0
    i32.load8_u
    local.set 3
    i32.const 0
    local.set 0
    loop  ;; label = @1
      local.get 2
      local.get 0
      i32.add
      i32.const 127
      i32.add
      i32.const 48
      i32.const 55
      local.get 3
      i32.const 15
      i32.and
      local.tee 4
      i32.const 10
      i32.lt_u
      select
      local.get 4
      i32.add
      i32.store8
      local.get 0
      i32.const -1
      i32.add
      local.set 0
      local.get 3
      i32.const 255
      i32.and
      local.tee 4
      i32.const 4
      i32.shr_u
      local.set 3
      local.get 4
      i32.const 15
      i32.gt_u
      br_if 0 (;@1;)
    end
    block  ;; label = @1
      local.get 0
      i32.const 128
      i32.add
      local.tee 3
      i32.const 129
      i32.lt_u
      br_if 0 (;@1;)
      local.get 3
      i32.const 128
      i32.const 1055716
      call $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E
      unreachable
    end
    local.get 1
    i32.const 1
    i32.const 1055732
    i32.const 2
    local.get 2
    local.get 0
    i32.add
    i32.const 128
    i32.add
    i32.const 0
    local.get 0
    i32.sub
    call $_ZN4core3fmt9Formatter12pad_integral17h8d163aacc395a3d9E
    local.set 0
    local.get 2
    i32.const 128
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core3str5lossy10Utf8Chunks3new17h4e8088422ddc37e0E (type 6) (param i32 i32 i32)
    local.get 0
    local.get 2
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store)
  (func $_ZN4core3str5lossy10Utf8Chunks5debug17h6c56fa00eceb1f17E (type 3) (param i32 i32)
    local.get 0
    local.get 1
    i64.load align=4
    i64.store)
  (func $_ZN4core3str19slice_error_fail_rt17hc0186f1dc2c6e9eaE (type 11) (param i32 i32 i32 i32 i32)
    (local i32 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 112
    i32.sub
    local.tee 5
    global.set $__stack_pointer
    local.get 5
    local.get 3
    i32.store offset=12
    local.get 5
    local.get 2
    i32.store offset=8
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  local.get 1
                  i32.const 257
                  i32.lt_u
                  br_if 0 (;@7;)
                  i32.const 0
                  local.set 6
                  loop  ;; label = @8
                    local.get 0
                    local.get 6
                    i32.add
                    local.set 7
                    local.get 6
                    i32.const -1
                    i32.add
                    local.tee 8
                    local.set 6
                    local.get 7
                    i32.const 256
                    i32.add
                    i32.load8_s
                    i32.const -65
                    i32.le_s
                    br_if 0 (;@8;)
                  end
                  local.get 8
                  i32.const 257
                  i32.add
                  local.tee 6
                  local.get 1
                  i32.lt_u
                  br_if 2 (;@5;)
                  local.get 1
                  i32.const -257
                  i32.add
                  local.get 8
                  i32.ne
                  br_if 4 (;@3;)
                  local.get 5
                  local.get 6
                  i32.store offset=20
                  br 1 (;@6;)
                end
                local.get 5
                local.get 1
                i32.store offset=20
              end
              local.get 5
              local.get 0
              i32.store offset=16
              i32.const 0
              local.set 6
              i32.const 1055224
              local.set 7
              br 1 (;@4;)
            end
            local.get 0
            local.get 8
            i32.add
            i32.const 257
            i32.add
            i32.load8_s
            i32.const -65
            i32.le_s
            br_if 1 (;@3;)
            local.get 5
            local.get 6
            i32.store offset=20
            local.get 5
            local.get 0
            i32.store offset=16
            i32.const 5
            local.set 6
            i32.const 1056568
            local.set 7
          end
          local.get 5
          local.get 6
          i32.store offset=28
          local.get 5
          local.get 7
          i32.store offset=24
          block  ;; label = @4
            block  ;; label = @5
              local.get 2
              local.get 1
              i32.gt_u
              local.tee 6
              br_if 0 (;@5;)
              local.get 3
              local.get 1
              i32.gt_u
              br_if 0 (;@5;)
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      local.get 2
                      local.get 3
                      i32.gt_u
                      br_if 0 (;@9;)
                      block  ;; label = @10
                        block  ;; label = @11
                          local.get 2
                          i32.eqz
                          br_if 0 (;@11;)
                          block  ;; label = @12
                            local.get 2
                            local.get 1
                            i32.lt_u
                            br_if 0 (;@12;)
                            local.get 2
                            local.get 1
                            i32.eq
                            br_if 1 (;@11;)
                            br 2 (;@10;)
                          end
                          local.get 0
                          local.get 2
                          i32.add
                          i32.load8_s
                          i32.const -64
                          i32.lt_s
                          br_if 1 (;@10;)
                        end
                        local.get 3
                        local.set 2
                      end
                      local.get 5
                      local.get 2
                      i32.store offset=32
                      local.get 1
                      local.set 6
                      block  ;; label = @10
                        local.get 2
                        local.get 1
                        i32.ge_u
                        br_if 0 (;@10;)
                        local.get 2
                        i32.const 1
                        i32.add
                        local.tee 6
                        i32.const 0
                        local.get 2
                        i32.const -3
                        i32.add
                        local.tee 7
                        local.get 7
                        local.get 2
                        i32.gt_u
                        select
                        local.tee 7
                        i32.lt_u
                        br_if 6 (;@4;)
                        local.get 0
                        local.get 6
                        i32.add
                        local.get 0
                        local.get 7
                        i32.add
                        i32.sub
                        local.set 6
                        loop  ;; label = @11
                          local.get 6
                          i32.const -1
                          i32.add
                          local.set 6
                          local.get 0
                          local.get 2
                          i32.add
                          local.set 7
                          local.get 2
                          i32.const -1
                          i32.add
                          local.tee 8
                          local.set 2
                          local.get 7
                          i32.load8_s
                          i32.const -64
                          i32.lt_s
                          br_if 0 (;@11;)
                        end
                        local.get 8
                        i32.const 1
                        i32.add
                        local.set 6
                      end
                      block  ;; label = @10
                        local.get 6
                        i32.eqz
                        br_if 0 (;@10;)
                        block  ;; label = @11
                          local.get 6
                          local.get 1
                          i32.lt_u
                          br_if 0 (;@11;)
                          local.get 6
                          local.get 1
                          i32.eq
                          br_if 1 (;@10;)
                          br 10 (;@1;)
                        end
                        local.get 0
                        local.get 6
                        i32.add
                        i32.load8_s
                        i32.const -65
                        i32.le_s
                        br_if 9 (;@1;)
                      end
                      local.get 6
                      local.get 1
                      i32.eq
                      br_if 7 (;@2;)
                      block  ;; label = @10
                        block  ;; label = @11
                          local.get 0
                          local.get 6
                          i32.add
                          local.tee 0
                          i32.load8_s
                          local.tee 7
                          i32.const -1
                          i32.gt_s
                          br_if 0 (;@11;)
                          local.get 0
                          i32.load8_u offset=1
                          i32.const 63
                          i32.and
                          local.set 2
                          local.get 7
                          i32.const 31
                          i32.and
                          local.set 8
                          local.get 7
                          i32.const -33
                          i32.gt_u
                          br_if 1 (;@10;)
                          local.get 8
                          i32.const 6
                          i32.shl
                          local.get 2
                          i32.or
                          local.set 0
                          br 4 (;@7;)
                        end
                        local.get 5
                        local.get 7
                        i32.const 255
                        i32.and
                        i32.store offset=36
                        i32.const 1
                        local.set 7
                        br 4 (;@6;)
                      end
                      local.get 2
                      i32.const 6
                      i32.shl
                      local.get 0
                      i32.load8_u offset=2
                      i32.const 63
                      i32.and
                      i32.or
                      local.set 2
                      local.get 7
                      i32.const -16
                      i32.ge_u
                      br_if 1 (;@8;)
                      local.get 2
                      local.get 8
                      i32.const 12
                      i32.shl
                      i32.or
                      local.set 0
                      br 2 (;@7;)
                    end
                    local.get 5
                    i32.const 100
                    i32.add
                    i32.const 70
                    i32.store
                    local.get 5
                    i32.const 72
                    i32.add
                    i32.const 20
                    i32.add
                    i32.const 70
                    i32.store
                    local.get 5
                    i32.const 84
                    i32.add
                    i32.const 4
                    i32.store
                    local.get 5
                    i32.const 48
                    i32.add
                    i32.const 20
                    i32.add
                    i32.const 4
                    i32.store
                    local.get 5
                    i64.const 4
                    i64.store offset=52 align=4
                    local.get 5
                    i32.const 1056668
                    i32.store offset=48
                    local.get 5
                    i32.const 4
                    i32.store offset=76
                    local.get 5
                    local.get 5
                    i32.const 72
                    i32.add
                    i32.store offset=64
                    local.get 5
                    local.get 5
                    i32.const 24
                    i32.add
                    i32.store offset=96
                    local.get 5
                    local.get 5
                    i32.const 16
                    i32.add
                    i32.store offset=88
                    local.get 5
                    local.get 5
                    i32.const 12
                    i32.add
                    i32.store offset=80
                    local.get 5
                    local.get 5
                    i32.const 8
                    i32.add
                    i32.store offset=72
                    local.get 5
                    i32.const 48
                    i32.add
                    local.get 4
                    call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
                    unreachable
                  end
                  local.get 2
                  i32.const 6
                  i32.shl
                  local.get 0
                  i32.load8_u offset=3
                  i32.const 63
                  i32.and
                  i32.or
                  local.get 8
                  i32.const 18
                  i32.shl
                  i32.const 1835008
                  i32.and
                  i32.or
                  local.tee 0
                  i32.const 1114112
                  i32.eq
                  br_if 5 (;@2;)
                end
                local.get 5
                local.get 0
                i32.store offset=36
                i32.const 1
                local.set 7
                local.get 0
                i32.const 128
                i32.lt_u
                br_if 0 (;@6;)
                i32.const 2
                local.set 7
                local.get 0
                i32.const 2048
                i32.lt_u
                br_if 0 (;@6;)
                i32.const 3
                i32.const 4
                local.get 0
                i32.const 65536
                i32.lt_u
                select
                local.set 7
              end
              local.get 5
              local.get 6
              i32.store offset=40
              local.get 5
              local.get 7
              local.get 6
              i32.add
              i32.store offset=44
              local.get 5
              i32.const 48
              i32.add
              i32.const 20
              i32.add
              i32.const 5
              i32.store
              local.get 5
              i32.const 108
              i32.add
              i32.const 70
              i32.store
              local.get 5
              i32.const 100
              i32.add
              i32.const 70
              i32.store
              local.get 5
              i32.const 72
              i32.add
              i32.const 20
              i32.add
              i32.const 75
              i32.store
              local.get 5
              i32.const 84
              i32.add
              i32.const 76
              i32.store
              local.get 5
              i64.const 5
              i64.store offset=52 align=4
              local.get 5
              i32.const 1056752
              i32.store offset=48
              local.get 5
              i32.const 4
              i32.store offset=76
              local.get 5
              local.get 5
              i32.const 72
              i32.add
              i32.store offset=64
              local.get 5
              local.get 5
              i32.const 24
              i32.add
              i32.store offset=104
              local.get 5
              local.get 5
              i32.const 16
              i32.add
              i32.store offset=96
              local.get 5
              local.get 5
              i32.const 40
              i32.add
              i32.store offset=88
              local.get 5
              local.get 5
              i32.const 36
              i32.add
              i32.store offset=80
              local.get 5
              local.get 5
              i32.const 32
              i32.add
              i32.store offset=72
              local.get 5
              i32.const 48
              i32.add
              local.get 4
              call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
              unreachable
            end
            local.get 5
            local.get 2
            local.get 3
            local.get 6
            select
            i32.store offset=40
            local.get 5
            i32.const 48
            i32.add
            i32.const 20
            i32.add
            i32.const 3
            i32.store
            local.get 5
            i32.const 72
            i32.add
            i32.const 20
            i32.add
            i32.const 70
            i32.store
            local.get 5
            i32.const 84
            i32.add
            i32.const 70
            i32.store
            local.get 5
            i64.const 3
            i64.store offset=52 align=4
            local.get 5
            i32.const 1056608
            i32.store offset=48
            local.get 5
            i32.const 4
            i32.store offset=76
            local.get 5
            local.get 5
            i32.const 72
            i32.add
            i32.store offset=64
            local.get 5
            local.get 5
            i32.const 24
            i32.add
            i32.store offset=88
            local.get 5
            local.get 5
            i32.const 16
            i32.add
            i32.store offset=80
            local.get 5
            local.get 5
            i32.const 40
            i32.add
            i32.store offset=72
            local.get 5
            i32.const 48
            i32.add
            local.get 4
            call $_ZN4core9panicking9panic_fmt17h3c0e4583c47aeb49E
            unreachable
          end
          local.get 7
          local.get 6
          i32.const 1056820
          call $_ZN4core5slice5index22slice_index_order_fail17h6f9c6cfade4b2695E
          unreachable
        end
        local.get 0
        local.get 1
        i32.const 0
        local.get 6
        local.get 4
        call $_ZN4core3str16slice_error_fail17h0b3eac3cd2cbe8e7E
        unreachable
      end
      i32.const 1055224
      i32.const 43
      local.get 4
      call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
      unreachable
    end
    local.get 0
    local.get 1
    local.get 6
    local.get 1
    local.get 4
    call $_ZN4core3str16slice_error_fail17h0b3eac3cd2cbe8e7E
    unreachable)
  (func $_ZN4core7unicode9printable5check17h47620c40433dc151E (type 15) (param i32 i32 i32 i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    i32.const 1
    local.set 7
    block  ;; label = @1
      block  ;; label = @2
        local.get 2
        i32.eqz
        br_if 0 (;@2;)
        local.get 1
        local.get 2
        i32.const 1
        i32.shl
        i32.add
        local.set 8
        local.get 0
        i32.const 65280
        i32.and
        i32.const 8
        i32.shr_u
        local.set 9
        i32.const 0
        local.set 10
        local.get 0
        i32.const 255
        i32.and
        local.set 11
        loop  ;; label = @3
          local.get 1
          i32.const 2
          i32.add
          local.set 12
          local.get 10
          local.get 1
          i32.load8_u offset=1
          local.tee 2
          i32.add
          local.set 13
          block  ;; label = @4
            local.get 1
            i32.load8_u
            local.tee 1
            local.get 9
            i32.eq
            br_if 0 (;@4;)
            local.get 1
            local.get 9
            i32.gt_u
            br_if 2 (;@2;)
            local.get 13
            local.set 10
            local.get 12
            local.set 1
            local.get 12
            local.get 8
            i32.eq
            br_if 2 (;@2;)
            br 1 (;@3;)
          end
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                local.get 13
                local.get 10
                i32.lt_u
                br_if 0 (;@6;)
                local.get 13
                local.get 4
                i32.gt_u
                br_if 1 (;@5;)
                local.get 3
                local.get 10
                i32.add
                local.set 1
                loop  ;; label = @7
                  local.get 2
                  i32.eqz
                  br_if 3 (;@4;)
                  local.get 2
                  i32.const -1
                  i32.add
                  local.set 2
                  local.get 1
                  i32.load8_u
                  local.set 10
                  local.get 1
                  i32.const 1
                  i32.add
                  local.set 1
                  local.get 10
                  local.get 11
                  i32.ne
                  br_if 0 (;@7;)
                end
                i32.const 0
                local.set 7
                br 5 (;@1;)
              end
              local.get 10
              local.get 13
              i32.const 1056876
              call $_ZN4core5slice5index22slice_index_order_fail17h6f9c6cfade4b2695E
              unreachable
            end
            local.get 13
            local.get 4
            i32.const 1056876
            call $_ZN4core5slice5index24slice_end_index_len_fail17h8cef88544c34b3bfE
            unreachable
          end
          local.get 13
          local.set 10
          local.get 12
          local.set 1
          local.get 12
          local.get 8
          i32.ne
          br_if 0 (;@3;)
        end
      end
      local.get 6
      i32.eqz
      br_if 0 (;@1;)
      local.get 5
      local.get 6
      i32.add
      local.set 11
      local.get 0
      i32.const 65535
      i32.and
      local.set 1
      i32.const 1
      local.set 7
      block  ;; label = @2
        loop  ;; label = @3
          local.get 5
          i32.const 1
          i32.add
          local.set 10
          block  ;; label = @4
            block  ;; label = @5
              local.get 5
              i32.load8_u
              local.tee 2
              i32.const 24
              i32.shl
              i32.const 24
              i32.shr_s
              local.tee 13
              i32.const 0
              i32.lt_s
              br_if 0 (;@5;)
              local.get 10
              local.set 5
              br 1 (;@4;)
            end
            local.get 10
            local.get 11
            i32.eq
            br_if 2 (;@2;)
            local.get 13
            i32.const 127
            i32.and
            i32.const 8
            i32.shl
            local.get 5
            i32.load8_u offset=1
            i32.or
            local.set 2
            local.get 5
            i32.const 2
            i32.add
            local.set 5
          end
          local.get 1
          local.get 2
          i32.sub
          local.tee 1
          i32.const 0
          i32.lt_s
          br_if 2 (;@1;)
          local.get 7
          i32.const 1
          i32.xor
          local.set 7
          local.get 5
          local.get 11
          i32.ne
          br_if 0 (;@3;)
          br 2 (;@1;)
        end
      end
      i32.const 1055224
      i32.const 43
      i32.const 1056892
      call $_ZN4core9panicking5panic17h107cfc2f46b7acacE
      unreachable
    end
    local.get 7
    i32.const 1
    i32.and)
  (func $_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i32$GT$3fmt17hbb0d214f9fde181dE (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 128
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 0
    i32.load
    local.set 0
    i32.const 0
    local.set 3
    loop  ;; label = @1
      local.get 2
      local.get 3
      i32.add
      i32.const 127
      i32.add
      i32.const 48
      i32.const 87
      local.get 0
      i32.const 15
      i32.and
      local.tee 4
      i32.const 10
      i32.lt_u
      select
      local.get 4
      i32.add
      i32.store8
      local.get 3
      i32.const -1
      i32.add
      local.set 3
      local.get 0
      i32.const 15
      i32.gt_u
      local.set 4
      local.get 0
      i32.const 4
      i32.shr_u
      local.set 0
      local.get 4
      br_if 0 (;@1;)
    end
    block  ;; label = @1
      local.get 3
      i32.const 128
      i32.add
      local.tee 0
      i32.const 129
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      i32.const 128
      i32.const 1055716
      call $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E
      unreachable
    end
    local.get 1
    i32.const 1
    i32.const 1055732
    i32.const 2
    local.get 2
    local.get 3
    i32.add
    i32.const 128
    i32.add
    i32.const 0
    local.get 3
    i32.sub
    call $_ZN4core3fmt9Formatter12pad_integral17h8d163aacc395a3d9E
    local.set 0
    local.get 2
    i32.const 128
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core3fmt3num52_$LT$impl$u20$core..fmt..LowerHex$u20$for$u20$i8$GT$3fmt17hc48681d90ac04511E (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 128
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 0
    i32.load8_u
    local.set 3
    i32.const 0
    local.set 0
    loop  ;; label = @1
      local.get 2
      local.get 0
      i32.add
      i32.const 127
      i32.add
      i32.const 48
      i32.const 87
      local.get 3
      i32.const 15
      i32.and
      local.tee 4
      i32.const 10
      i32.lt_u
      select
      local.get 4
      i32.add
      i32.store8
      local.get 0
      i32.const -1
      i32.add
      local.set 0
      local.get 3
      i32.const 255
      i32.and
      local.tee 4
      i32.const 4
      i32.shr_u
      local.set 3
      local.get 4
      i32.const 15
      i32.gt_u
      br_if 0 (;@1;)
    end
    block  ;; label = @1
      local.get 0
      i32.const 128
      i32.add
      local.tee 3
      i32.const 129
      i32.lt_u
      br_if 0 (;@1;)
      local.get 3
      i32.const 128
      i32.const 1055716
      call $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E
      unreachable
    end
    local.get 1
    i32.const 1
    i32.const 1055732
    i32.const 2
    local.get 2
    local.get 0
    i32.add
    i32.const 128
    i32.add
    i32.const 0
    local.get 0
    i32.sub
    call $_ZN4core3fmt9Formatter12pad_integral17h8d163aacc395a3d9E
    local.set 0
    local.get 2
    i32.const 128
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core3fmt3num49_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u8$GT$3fmt17h3e95ce674bf251aaE (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 128
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            block  ;; label = @5
              local.get 1
              i32.load
              local.tee 3
              i32.const 16
              i32.and
              br_if 0 (;@5;)
              local.get 3
              i32.const 32
              i32.and
              br_if 1 (;@4;)
              local.get 0
              i64.extend_i32_u
              i64.const 255
              i64.and
              i32.const 1
              local.get 1
              call $_ZN4core3fmt3num3imp7fmt_u6417h5750e7de7c1be42fE
              local.set 0
              br 4 (;@1;)
            end
            i32.const 0
            local.set 3
            loop  ;; label = @5
              local.get 2
              local.get 3
              i32.add
              i32.const 127
              i32.add
              i32.const 48
              i32.const 87
              local.get 0
              i32.const 15
              i32.and
              local.tee 4
              i32.const 10
              i32.lt_u
              select
              local.get 4
              i32.add
              i32.store8
              local.get 3
              i32.const -1
              i32.add
              local.set 3
              local.get 0
              i32.const 255
              i32.and
              local.tee 4
              i32.const 4
              i32.shr_u
              local.set 0
              local.get 4
              i32.const 15
              i32.gt_u
              br_if 0 (;@5;)
            end
            local.get 3
            i32.const 128
            i32.add
            local.tee 0
            i32.const 129
            i32.ge_u
            br_if 1 (;@3;)
            local.get 1
            i32.const 1
            i32.const 1055732
            i32.const 2
            local.get 2
            local.get 3
            i32.add
            i32.const 128
            i32.add
            i32.const 0
            local.get 3
            i32.sub
            call $_ZN4core3fmt9Formatter12pad_integral17h8d163aacc395a3d9E
            local.set 0
            br 3 (;@1;)
          end
          i32.const 0
          local.set 3
          loop  ;; label = @4
            local.get 2
            local.get 3
            i32.add
            i32.const 127
            i32.add
            i32.const 48
            i32.const 55
            local.get 0
            i32.const 15
            i32.and
            local.tee 4
            i32.const 10
            i32.lt_u
            select
            local.get 4
            i32.add
            i32.store8
            local.get 3
            i32.const -1
            i32.add
            local.set 3
            local.get 0
            i32.const 255
            i32.and
            local.tee 4
            i32.const 4
            i32.shr_u
            local.set 0
            local.get 4
            i32.const 15
            i32.gt_u
            br_if 0 (;@4;)
          end
          local.get 3
          i32.const 128
          i32.add
          local.tee 0
          i32.const 129
          i32.ge_u
          br_if 1 (;@2;)
          local.get 1
          i32.const 1
          i32.const 1055732
          i32.const 2
          local.get 2
          local.get 3
          i32.add
          i32.const 128
          i32.add
          i32.const 0
          local.get 3
          i32.sub
          call $_ZN4core3fmt9Formatter12pad_integral17h8d163aacc395a3d9E
          local.set 0
          br 2 (;@1;)
        end
        local.get 0
        i32.const 128
        i32.const 1055716
        call $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E
        unreachable
      end
      local.get 0
      i32.const 128
      i32.const 1055716
      call $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E
      unreachable
    end
    local.get 2
    i32.const 128
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core3fmt3num3imp7fmt_u6417h5750e7de7c1be42fE (type 16) (param i64 i32 i32) (result i32)
    (local i32 i32 i64 i32 i32 i32)
    global.get $__stack_pointer
    i32.const 48
    i32.sub
    local.tee 3
    global.set $__stack_pointer
    i32.const 39
    local.set 4
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i64.const 10000
        i64.ge_u
        br_if 0 (;@2;)
        local.get 0
        local.set 5
        br 1 (;@1;)
      end
      i32.const 39
      local.set 4
      loop  ;; label = @2
        local.get 3
        i32.const 9
        i32.add
        local.get 4
        i32.add
        local.tee 6
        i32.const -4
        i32.add
        local.get 0
        local.get 0
        i64.const 10000
        i64.div_u
        local.tee 5
        i64.const 10000
        i64.mul
        i64.sub
        i32.wrap_i64
        local.tee 7
        i32.const 65535
        i32.and
        i32.const 100
        i32.div_u
        local.tee 8
        i32.const 1
        i32.shl
        i32.const 1055734
        i32.add
        i32.load16_u align=1
        i32.store16 align=1
        local.get 6
        i32.const -2
        i32.add
        local.get 7
        local.get 8
        i32.const 100
        i32.mul
        i32.sub
        i32.const 65535
        i32.and
        i32.const 1
        i32.shl
        i32.const 1055734
        i32.add
        i32.load16_u align=1
        i32.store16 align=1
        local.get 4
        i32.const -4
        i32.add
        local.set 4
        local.get 0
        i64.const 99999999
        i64.gt_u
        local.set 6
        local.get 5
        local.set 0
        local.get 6
        br_if 0 (;@2;)
      end
    end
    block  ;; label = @1
      local.get 5
      i32.wrap_i64
      local.tee 6
      i32.const 99
      i32.le_u
      br_if 0 (;@1;)
      local.get 3
      i32.const 9
      i32.add
      local.get 4
      i32.const -2
      i32.add
      local.tee 4
      i32.add
      local.get 5
      i32.wrap_i64
      local.tee 6
      local.get 6
      i32.const 65535
      i32.and
      i32.const 100
      i32.div_u
      local.tee 6
      i32.const 100
      i32.mul
      i32.sub
      i32.const 65535
      i32.and
      i32.const 1
      i32.shl
      i32.const 1055734
      i32.add
      i32.load16_u align=1
      i32.store16 align=1
    end
    block  ;; label = @1
      block  ;; label = @2
        local.get 6
        i32.const 10
        i32.lt_u
        br_if 0 (;@2;)
        local.get 3
        i32.const 9
        i32.add
        local.get 4
        i32.const -2
        i32.add
        local.tee 4
        i32.add
        local.get 6
        i32.const 1
        i32.shl
        i32.const 1055734
        i32.add
        i32.load16_u align=1
        i32.store16 align=1
        br 1 (;@1;)
      end
      local.get 3
      i32.const 9
      i32.add
      local.get 4
      i32.const -1
      i32.add
      local.tee 4
      i32.add
      local.get 6
      i32.const 48
      i32.add
      i32.store8
    end
    local.get 2
    local.get 1
    i32.const 1055224
    i32.const 0
    local.get 3
    i32.const 9
    i32.add
    local.get 4
    i32.add
    i32.const 39
    local.get 4
    i32.sub
    call $_ZN4core3fmt9Formatter12pad_integral17h8d163aacc395a3d9E
    local.set 4
    local.get 3
    i32.const 48
    i32.add
    global.set $__stack_pointer
    local.get 4)
  (func $_ZN4core3fmt3num53_$LT$impl$u20$core..fmt..UpperHex$u20$for$u20$i32$GT$3fmt17h4207c064850a1e9eE (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 128
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 0
    i32.load
    local.set 0
    i32.const 0
    local.set 3
    loop  ;; label = @1
      local.get 2
      local.get 3
      i32.add
      i32.const 127
      i32.add
      i32.const 48
      i32.const 55
      local.get 0
      i32.const 15
      i32.and
      local.tee 4
      i32.const 10
      i32.lt_u
      select
      local.get 4
      i32.add
      i32.store8
      local.get 3
      i32.const -1
      i32.add
      local.set 3
      local.get 0
      i32.const 15
      i32.gt_u
      local.set 4
      local.get 0
      i32.const 4
      i32.shr_u
      local.set 0
      local.get 4
      br_if 0 (;@1;)
    end
    block  ;; label = @1
      local.get 3
      i32.const 128
      i32.add
      local.tee 0
      i32.const 129
      i32.lt_u
      br_if 0 (;@1;)
      local.get 0
      i32.const 128
      i32.const 1055716
      call $_ZN4core5slice5index26slice_start_index_len_fail17hcf76f422b609e9b4E
      unreachable
    end
    local.get 1
    i32.const 1
    i32.const 1055732
    i32.const 2
    local.get 2
    local.get 3
    i32.add
    i32.const 128
    i32.add
    i32.const 0
    local.get 3
    i32.sub
    call $_ZN4core3fmt9Formatter12pad_integral17h8d163aacc395a3d9E
    local.set 0
    local.get 2
    i32.const 128
    i32.add
    global.set $__stack_pointer
    local.get 0)
  (func $_ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$i32$GT$3fmt17he255841eede72becE (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.tee 0
    i64.extend_i32_u
    i64.const 0
    local.get 0
    i64.extend_i32_s
    i64.sub
    local.get 0
    i32.const -1
    i32.gt_s
    local.tee 0
    select
    local.get 0
    local.get 1
    call $_ZN4core3fmt3num3imp7fmt_u6417h5750e7de7c1be42fE)
  (func $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h371a32e5a2315df4E (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    i32.load8_u
    local.get 1
    call $_ZN4core3fmt3num49_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u8$GT$3fmt17h3e95ce674bf251aaE)
  (func $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h950b2864e75b254fE (type 5) (param i32 i32) (result i32)
    local.get 0
    i32.load
    local.get 1
    call $_ZN4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$u32$GT$3fmt17h9ea99fd84ac4386dE)
  (func $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17hb5acf96057c0046cE (type 5) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get $__stack_pointer
    i32.const 32
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load
        local.tee 3
        i32.load8_u
        br_if 0 (;@2;)
        local.get 1
        i32.load offset=24
        i32.const 1058372
        i32.const 4
        local.get 1
        i32.const 28
        i32.add
        i32.load
        i32.load offset=12
        call_indirect (type 7)
        local.set 1
        br 1 (;@1;)
      end
      i32.const 1
      local.set 0
      local.get 2
      local.get 3
      i32.const 1
      i32.add
      i32.store offset=12
      local.get 2
      local.get 1
      i32.load offset=24
      i32.const 1058368
      i32.const 4
      local.get 1
      i32.const 28
      i32.add
      i32.load
      i32.load offset=12
      call_indirect (type 7)
      i32.store8 offset=24
      local.get 2
      local.get 1
      i32.store offset=16
      local.get 2
      i32.const 0
      i32.store8 offset=25
      local.get 2
      i32.const 0
      i32.store offset=20
      local.get 2
      i32.const 16
      i32.add
      local.get 2
      i32.const 12
      i32.add
      i32.const 1055672
      call $_ZN4core3fmt8builders10DebugTuple5field17hcf01775d6abf5a8bE
      local.set 3
      local.get 2
      i32.load8_u offset=24
      local.set 1
      block  ;; label = @2
        block  ;; label = @3
          local.get 2
          i32.load offset=20
          local.tee 4
          br_if 0 (;@3;)
          local.get 1
          local.set 0
          br 1 (;@2;)
        end
        local.get 1
        i32.const 255
        i32.and
        br_if 0 (;@2;)
        local.get 3
        i32.load
        local.set 1
        block  ;; label = @3
          local.get 4
          i32.const 1
          i32.ne
          br_if 0 (;@3;)
          local.get 2
          i32.load8_u offset=25
          i32.const 255
          i32.and
          i32.eqz
          br_if 0 (;@3;)
          local.get 1
          i32.load8_u
          i32.const 4
          i32.and
          br_if 0 (;@3;)
          i32.const 1
          local.set 0
          local.get 1
          i32.load offset=24
          i32.const 1055668
          i32.const 1
          local.get 1
          i32.const 28
          i32.add
          i32.load
          i32.load offset=12
          call_indirect (type 7)
          br_if 1 (;@2;)
        end
        local.get 1
        i32.load offset=24
        i32.const 1055267
        i32.const 1
        local.get 1
        i32.const 28
        i32.add
        i32.load
        i32.load offset=12
        call_indirect (type 7)
        local.set 0
      end
      local.get 0
      i32.const 255
      i32.and
      i32.const 0
      i32.ne
      local.set 1
    end
    local.get 2
    i32.const 32
    i32.add
    global.set $__stack_pointer
    local.get 1)
  (func $_ZN64_$LT$core..str..error..Utf8Error$u20$as$u20$core..fmt..Debug$GT$3fmt17hc92a992c621fc5c4E (type 5) (param i32 i32) (result i32)
    (local i32 i32)
    global.get $__stack_pointer
    i32.const 16
    i32.sub
    local.tee 2
    global.set $__stack_pointer
    local.get 2
    local.get 0
    i32.store
    local.get 2
    local.get 0
    i32.const 4
    i32.add
    i32.store offset=4
    local.get 1
    i32.load offset=24
    i32.const 1058392
    i32.const 9
    local.get 1
    i32.const 28
    i32.add
    i32.load
    i32.load offset=12
    call_indirect (type 7)
    local.set 0
    local.get 2
    i32.const 0
    i32.store8 offset=13
    local.get 2
    local.get 0
    i32.store8 offset=12
    local.get 2
    local.get 1
    i32.store offset=8
    local.get 2
    i32.const 8
    i32.add
    i32.const 1058401
    i32.const 11
    local.get 2
    i32.const 1058376
    call $_ZN4core3fmt8builders11DebugStruct5field17h6758fe7581d6cfb8E
    i32.const 1058412
    i32.const 9
    local.get 2
    i32.const 4
    i32.add
    i32.const 1058424
    call $_ZN4core3fmt8builders11DebugStruct5field17h6758fe7581d6cfb8E
    local.set 0
    local.get 2
    i32.load8_u offset=12
    local.set 1
    block  ;; label = @1
      local.get 2
      i32.load8_u offset=13
      i32.eqz
      br_if 0 (;@1;)
      local.get 1
      i32.const 255
      i32.and
      local.set 3
      i32.const 1
      local.set 1
      local.get 3
      br_if 0 (;@1;)
      block  ;; label = @2
        local.get 0
        i32.load
        local.tee 1
        i32.load8_u
        i32.const 4
        i32.and
        br_if 0 (;@2;)
        local.get 1
        i32.load offset=24
        i32.const 1055663
        i32.const 2
        local.get 1
        i32.const 28
        i32.add
        i32.load
        i32.load offset=12
        call_indirect (type 7)
        local.set 1
        br 1 (;@1;)
      end
      local.get 1
      i32.load offset=24
      i32.const 1055649
      i32.const 1
      local.get 1
      i32.const 28
      i32.add
      i32.load
      i32.load offset=12
      call_indirect (type 7)
      local.set 1
    end
    local.get 2
    i32.const 16
    i32.add
    global.set $__stack_pointer
    local.get 1
    i32.const 255
    i32.and
    i32.const 0
    i32.ne)
  (func $_start.command_export (type 0)
    call $__wasm_call_ctors
    call $_start
    call $__wasm_call_dtors)
  (func $main.command_export (type 5) (param i32 i32) (result i32)
    call $__wasm_call_ctors
    local.get 0
    local.get 1
    call $main
    call $__wasm_call_dtors)
  (table (;0;) 88 88 funcref)
  (memory (;0;) 17)
  (global $__stack_pointer (mut i32) (i32.const 1048576))
  (export "memory" (memory 0))
  (export "_start" (func $_start.command_export))
  (export "main" (func $main.command_export))
  (elem (;0;) (i32.const 1) func $_ZN4core3ptr85drop_in_place$LT$std..rt..lang_start$LT$$LP$$RP$$GT$..$u7b$$u7b$closure$u7d$$u7d$$GT$17h45d796ff621e665dE.llvm.8739220772287655740 $_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17ha8bed69553fd5b4cE.llvm.8739220772287655740 $_ZN3std2rt10lang_start28_$u7b$$u7b$closure$u7d$$u7d$17h787271d5e3e8cf44E.llvm.8739220772287655740 $_ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$u32$GT$3fmt17h438b5cc0efea7d46E $_ZN11module_test4main17h758981acc4b6c11eE $_ZN3std2io5Write9write_fmt17hc24349e17874757dE $_ZN3std2io5Write9write_fmt17h05a3bcb077746284E $_ZN59_$LT$core..fmt..Arguments$u20$as$u20$core..fmt..Display$GT$3fmt17hda659a7548139618E $_ZN70_$LT$core..result..Result$LT$T$C$E$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17hf0c62b57a5afc3a8E $_ZN4core3fmt3num3imp52_$LT$impl$u20$core..fmt..Display$u20$for$u20$i32$GT$3fmt17he255841eede72becE $_ZN60_$LT$alloc..string..String$u20$as$u20$core..fmt..Display$GT$3fmt17h5d51ffd1ebc9bb46E $_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17ha5b118b1fc2155ccE $_ZN60_$LT$std..io..error..Error$u20$as$u20$core..fmt..Display$GT$3fmt17hb0563df3d544e4f8E $_ZN3std5alloc24default_alloc_error_hook17hc7fd4e8fd00ec2b2E $_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17hd4d32f1a7d58f0d8E $_ZN91_$LT$std..sys_common..backtrace.._print..DisplayBacktrace$u20$as$u20$core..fmt..Display$GT$3fmt17hb243f3efbed6872fE $_ZN73_$LT$core..panic..panic_info..PanicInfo$u20$as$u20$core..fmt..Display$GT$3fmt17ha7d3bd671660d3d8E $_ZN4core3ptr100drop_in_place$LT$$RF$mut$u20$std..io..Write..write_fmt..Adapter$LT$alloc..vec..Vec$LT$u8$GT$$GT$$GT$17h5828119147893f65E $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_str17ha5c0e59281498a78E $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$10write_char17h55d67e40e509f4c4E $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_fmt17h540a0dde17fe2f09E $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_str17haa5048c1ee515ba8E $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$10write_char17h04c2b0631800289eE $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_fmt17h7edb6a52b8fc0202E $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_str17h7184d3b710c1fad9E $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$10write_char17h651caa0da3d206a5E $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_fmt17h9618ee3043cc2dfeE $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_str17h6440f892aa9617a1E $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$10write_char17h1afccb88c028ec33E $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_fmt17ha0cd0a34539708e6E $_ZN63_$LT$core..cell..BorrowMutError$u20$as$u20$core..fmt..Debug$GT$3fmt17h76796ccdb5fb4613E $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h67628d985ca90d43E $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h7ebf73f0383d863dE $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h35d5d312331238e0E $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h453278982654c107E $_ZN4core3ptr205drop_in_place$LT$$LT$alloc..boxed..Box$LT$dyn$u20$core..error..Error$u2b$core..marker..Send$u2b$core..marker..Sync$GT$$u20$as$u20$core..convert..From$LT$alloc..string..String$GT$$GT$..from..StringError$GT$17h285fd2a0e5d852c2E $_ZN55_$LT$std..path..PathBuf$u20$as$u20$core..fmt..Debug$GT$3fmt17hd047898216b3195bE $_ZN4core3ptr88drop_in_place$LT$std..io..Write..write_fmt..Adapter$LT$alloc..vec..Vec$LT$u8$GT$$GT$$GT$17h418268d2ec1c31e1E $_ZN80_$LT$std..io..Write..write_fmt..Adapter$LT$T$GT$$u20$as$u20$core..fmt..Write$GT$9write_str17h9cbe34749d4b82e2E $_ZN4core3fmt5Write10write_char17h058d020d3ee2181eE $_ZN4core3fmt5Write9write_fmt17h0e875200992553d8E $_ZN80_$LT$std..io..Write..write_fmt..Adapter$LT$T$GT$$u20$as$u20$core..fmt..Write$GT$9write_str17h6415def010a9be9cE $_ZN4core3fmt5Write10write_char17h612343524e950315E $_ZN4core3fmt5Write9write_fmt17hc95f6c38645c7207E $_ZN80_$LT$std..io..Write..write_fmt..Adapter$LT$T$GT$$u20$as$u20$core..fmt..Write$GT$9write_str17h9aa964bb9beacebdE $_ZN4core3fmt5Write10write_char17h97088a3d641899fdE $_ZN4core3fmt5Write9write_fmt17h59edcb38a9a5b8b6E $_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h0236a7ae6988917eE $_ZN3std4sync4once4Once9call_once28_$u7b$$u7b$closure$u7d$$u7d$17hfe44afed107c6e44E $_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17h2af9265a14d6f21aE $_ZN3std4sync4once4Once15call_once_force28_$u7b$$u7b$closure$u7d$$u7d$17h140d3c4ba437bc42E $_ZN4core3ops8function6FnOnce40call_once$u7b$$u7b$vtable.shim$u7d$$u7d$17hb0cf4eba839d9459E $_ZN3std4sync4once4Once15call_once_force28_$u7b$$u7b$closure$u7d$$u7d$17ha5ace6a88633e7feE $_ZN36_$LT$T$u20$as$u20$core..any..Any$GT$7type_id17ha23ad085a7a28f08E $_ZN36_$LT$T$u20$as$u20$core..any..Any$GT$7type_id17hed1fcd7df172fc15E $_ZN4core3ptr70drop_in_place$LT$std..panicking..begin_panic_handler..PanicPayload$GT$17h1eda4e4f8bd9f813E $_ZN90_$LT$std..panicking..begin_panic_handler..PanicPayload$u20$as$u20$core..panic..BoxMeUp$GT$8take_box17h1b61cc9dffa5ac7fE $_ZN90_$LT$std..panicking..begin_panic_handler..PanicPayload$u20$as$u20$core..panic..BoxMeUp$GT$3get17hfdcf489f42573da3E $_ZN93_$LT$std..panicking..begin_panic_handler..StrPanicPayload$u20$as$u20$core..panic..BoxMeUp$GT$8take_box17h899afaedeaeca761E $_ZN93_$LT$std..panicking..begin_panic_handler..StrPanicPayload$u20$as$u20$core..panic..BoxMeUp$GT$3get17h44d9e73cb503261eE $_ZN36_$LT$T$u20$as$u20$core..any..Any$GT$7type_id17h19307b269d853256E $_ZN64_$LT$core..str..error..Utf8Error$u20$as$u20$core..fmt..Debug$GT$3fmt17hc92a992c621fc5c4E $_ZN4core3ptr103drop_in_place$LT$std..sync..poison..PoisonError$LT$std..sync..mutex..MutexGuard$LT$$LP$$RP$$GT$$GT$$GT$17h5b5482fc05b44f80E $_ZN76_$LT$std..sync..poison..PoisonError$LT$T$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17h5d1b804931a59b0fE $_ZN4core3ptr27drop_in_place$LT$$RF$u8$GT$17h010384b463d094f9E $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h0c574378139f8ff5E $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h1bfd36f5b722dde7E $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h4e2b50f3537886b4E $_ZN4core3ops8function6FnOnce9call_once17hed399a7e342e5203E $_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h6994d0e2455e539bE $_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17hd4a025d34140f1f3E $_ZN44_$LT$$RF$T$u20$as$u20$core..fmt..Display$GT$3fmt17h5cd1e1056906e4c0E $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17ha544352130619abaE $_ZN4core3fmt3num52_$LT$impl$u20$core..fmt..UpperHex$u20$for$u20$i8$GT$3fmt17h7caa790059f60235E $_ZN71_$LT$core..ops..range..Range$LT$Idx$GT$$u20$as$u20$core..fmt..Debug$GT$3fmt17hdc112323110892d3E $_ZN41_$LT$char$u20$as$u20$core..fmt..Debug$GT$3fmt17h0b06e1679f51847fE $_ZN4core3ptr102drop_in_place$LT$$RF$core..iter..adapters..copied..Copied$LT$core..slice..iter..Iter$LT$u8$GT$$GT$$GT$17ha3f154a6a3733eb4E $_ZN36_$LT$T$u20$as$u20$core..any..Any$GT$7type_id17h6faeb6b1367efc9eE $_ZN68_$LT$core..fmt..builders..PadAdapter$u20$as$u20$core..fmt..Write$GT$9write_str17hf0e70d48679f35b2E $_ZN4core3fmt5Write10write_char17h265cab5073dbf68fE $_ZN4core3fmt5Write9write_fmt17h1486cdb96f6c78f2E $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h371a32e5a2315df4E $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_str17had2d839cf514036aE $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$10write_char17h471402e56c237cb0E $_ZN50_$LT$$RF$mut$u20$W$u20$as$u20$core..fmt..Write$GT$9write_fmt17h795910deba94c650E $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17h950b2864e75b254fE $_ZN42_$LT$$RF$T$u20$as$u20$core..fmt..Debug$GT$3fmt17hb5acf96057c0046cE)
  (data $.rodata (i32.const 1048576) "\01\00\00\00\04\00\00\00\04\00\00\00\02\00\00\00\03\00\00\00\03\00\00\00# args: \0a\00\00\00\18\00\10\00\08\00\00\00 \00\10\00\01\00\00\00called `Result::unwrap()` on an `Err` value()\00\00\00\12\00\00\00\04\00\00\00\04\00\00\00\13\00\00\00\14\00\00\00\15\00\00\00\12\00\00\00\04\00\00\00\04\00\00\00\16\00\00\00\17\00\00\00\18\00\00\00\12\00\00\00\04\00\00\00\04\00\00\00\19\00\00\00\1a\00\00\00\1b\00\00\00\12\00\00\00\04\00\00\00\04\00\00\00\1c\00\00\00\1d\00\00\00\1e\00\00\00already borrowed\12\00\00\00\00\00\00\00\01\00\00\00\1f\00\00\00\00assertion failed: mid <= self.len()\12\00\00\00\04\00\00\00\04\00\00\00 \00\00\00\12\00\00\00\04\00\00\00\04\00\00\00!\00\00\00called `Option::unwrap()` on a `None` valueinternal error: entered unreachable code/rustc/8ce3204af9463db3192ea1eb31c45c2f6d4b5ae6/library/alloc/src/vec/mod.rs\00{\01\10\00L\00\00\00_\07\00\00$\00\00\00Err\00\12\00\00\00\04\00\00\00\04\00\00\00\22\00\00\00Ok\00\00\12\00\00\00\04\00\00\00\04\00\00\00#\00\00\00mainfatal runtime error: \0a\00\00\04\02\10\00\15\00\00\00\19\02\10\00\01\00\00\00unwrap failed: CString::new(\22main\22) = \00\00,\02\10\00&\00\00\00library/std/src/rt.rs\00\00\00\5c\02\10\00\15\00\00\00r\00\00\00\0d\00\00\00use of std::thread::current() is not possible after the thread's local data has been destroyedlibrary/std/src/thread/mod.rs\00\e2\02\10\00\1d\00\00\00\b2\02\00\00\05\00\00\00failed to generate unique thread ID: bitspace exhausted\00\10\03\10\007\00\00\00\e2\02\10\00\1d\00\00\00\17\04\00\00\0d\00\00\00RUST_BACKTRACElibrary/std/src/env.rsn\03\10\00\16\00\00\00%\03\00\00#\00\00\00$\00\00\00\0c\00\00\00\04\00\00\00%\00\00\00failed to write the buffered data\00\00\00\a4\03\10\00!\00\00\00\17\00\00\00library/std/src/io/buffered/bufwriter.rs\d4\03\10\00(\00\00\00\8d\00\00\00\12\00\00\00\c4\00\10\00\00\00\00\00library/std/src/io/buffered/linewritershim.rs\00\00\00\14\04\10\00-\00\00\00\01\01\00\00)\00\00\00uncategorized errorother errorout of memoryunexpected end of fileunsupportedoperation interruptedargument list too longinvalid filenametoo many linkscross-device link or renamedeadlockexecutable file busyresource busyfile too largefilesystem quota exceededseek on unseekable fileno storage spacewrite zerotimed outinvalid datainvalid input parameterstale network file handlefilesystem loop or indirection limit (e.g. symlink loop)read-only filesystem or storage mediumdirectory not emptyis a directorynot a directoryoperation would blockentity already existsbroken pipenetwork downaddress not availableaddress in usenot connectedconnection abortednetwork unreachablehost unreachableconnection resetconnection refusedpermission deniedentity not found (os error )\00\00\00\c4\00\10\00\00\00\00\00A\07\10\00\0b\00\00\00L\07\10\00\01\00\00\00library/std/src/io/stdio.rs\00h\07\10\00\1b\00\00\00r\02\00\00\13\00\00\00h\07\10\00\1b\00\00\00\df\02\00\00\14\00\00\00failed printing to : \00\00\00\a4\07\10\00\13\00\00\00\b7\07\10\00\02\00\00\00h\07\10\00\1b\00\00\00\fa\03\00\00\09\00\00\00stdoutlibrary/std/src/io/mod.rs\00\e2\07\10\00\19\00\00\00\0a\06\00\00!\00\00\00failed to write whole buffer\0c\08\10\00\1c\00\00\00\17\00\00\00&\00\00\00\0c\00\00\00\04\00\00\00'\00\00\00(\00\00\00)\00\00\00formatter error\00L\08\10\00\0f\00\00\00(\00\00\00&\00\00\00\0c\00\00\00\04\00\00\00*\00\00\00+\00\00\00,\00\00\00&\00\00\00\0c\00\00\00\04\00\00\00-\00\00\00.\00\00\00/\00\00\00library/std/src/panic.rs\98\08\10\00\18\00\00\00\f0\00\00\00\12\00\00\00full\12\00\00\00\04\00\00\00\04\00\00\000\00\00\001\00\00\00library/std/src/sync/once.rs\d8\08\10\00\1c\00\00\00\14\01\00\00)\00\00\00\12\00\00\00\04\00\00\00\04\00\00\002\00\00\003\00\00\00\d8\08\10\00\1c\00\00\00N\01\00\00\0e\00\00\00\12\00\00\00\04\00\00\00\04\00\00\004\00\00\005\00\00\00\d8\08\10\00\1c\00\00\00N\01\00\00(\00\00\00assertion failed: state_and_queue.addr() & STATE_MASK == RUNNINGOnce instance has previously been poisoned\00\00\8c\09\10\00*\00\00\00\02\00\00\00\d8\08\10\00\1c\00\00\00\ff\01\00\00\09\00\00\00\d8\08\10\00\1c\00\00\00\0c\02\00\00\1e\00\00\00PoisonErrorstack backtrace:\0a\ef\09\10\00\11\00\00\00note: Some details are omitted, run with `RUST_BACKTRACE=full` for a verbose backtrace.\0a\08\0a\10\00X\00\00\00lock count overflow in reentrant mutexlibrary/std/src/sys_common/remutex.rs\00\8e\0a\10\00%\00\00\00\8f\00\00\00\22\00\00\00library/std/src/sys_common/thread_info.rs\00\00\00\c4\0a\10\00)\00\00\00\16\00\00\003\00\00\00\c4\0a\10\00)\00\00\00+\00\00\00+\00\00\00assertion failed: thread_info.is_none()\00\10\0b\10\00'\00\00\00memory allocation of  bytes failed\0a\00@\0b\10\00\15\00\00\00U\0b\10\00\0e\00\00\00library/std/src/alloc.rst\0b\10\00\18\00\00\00U\01\00\00\09\00\00\00library/std/src/panicking.rsBox<dyn Any><unnamed>\00\00\00\9c\0b\10\00\1c\00\00\00\f5\00\00\00\14\00\00\00thread '' panicked at '', \00\00\e0\0b\10\00\08\00\00\00\e8\0b\10\00\0f\00\00\00\f7\0b\10\00\03\00\00\00\19\02\10\00\01\00\00\00note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace\0a\00\00\1c\0c\10\00N\00\00\00\9c\0b\10\00\1c\00\00\00+\02\00\00\0f\00\00\00\9c\0b\10\00\1c\00\00\00*\02\00\00\0f\00\00\00$\00\00\00\0c\00\00\00\04\00\00\006\00\00\00\12\00\00\00\08\00\00\00\04\00\00\007\00\00\008\00\00\00\10\00\00\00\04\00\00\009\00\00\00:\00\00\00\12\00\00\00\08\00\00\00\04\00\00\00;\00\00\00<\00\00\00thread panicked while processing panic. aborting.\0a\00\00\dc\0c\10\002\00\00\00\12\00\00\00\00\00\00\00\01\00\00\00=\00\00\00\0apanicked after panic::always_abort(), aborting.\0a\00\00\00\c4\00\10\00\00\00\00\00(\0d\10\001\00\00\00thread panicked while panicking. aborting.\0a\00l\0d\10\00+\00\00\00failed to initiate panic, error \a0\0d\10\00 \00\00\00condvar wait not supported\00\00\c8\0d\10\00\1a\00\00\00library/std/src/sys/wasi/../unsupported/locks/condvar.rs\ec\0d\10\008\00\00\00\16\00\00\00\09\00\00\00cannot recursively acquire mutex4\0e\10\00 \00\00\00library/std/src/sys/wasi/../unsupported/locks/mutex.rs\00\00\5c\0e\10\006\00\00\00\16\00\00\00\09\00\00\00rwlock locked for writing\00\00\00\a4\0e\10\00\19\00\00\00strerror_r failure\00\00\c8\0e\10\00\12\00\00\00library/std/src/sys/wasi/os.rs\00\00\e4\0e\10\00\1e\00\00\00/\00\00\00\0d\00\00\00\12\00\00\00\08\00\00\00\04\00\00\00>\00\00\00\e4\0e\10\00\1e\00\00\001\00\00\00\09\00\00\00\08\00\0e\00\0f\00?\00\02\00@\005\00\0d\00\04\00\03\00,\00\1b\00\1c\00I\00\14\00\06\004\000\00?\00\00\00\08\00\00\00\04\00\00\00@\00\00\00library/std/src/sys_common/thread_parker/generic.rs\00h\0f\10\003\00\00\00'\00\00\00\15\00\00\00inconsistent park state\00\ac\0f\10\00\17\00\00\00h\0f\10\003\00\00\005\00\00\00\17\00\00\00park state changed unexpectedly\00\dc\0f\10\00\1f\00\00\00h\0f\10\003\00\00\002\00\00\00\11\00\00\00inconsistent state in unpark\14\10\10\00\1c\00\00\00h\0f\10\003\00\00\00l\00\00\00\12\00\00\00h\0f\10\003\00\00\00z\00\00\00\0e\00\00\00\0e\00\00\00\10\00\00\00\16\00\00\00\15\00\00\00\0b\00\00\00\16\00\00\00\0d\00\00\00\0b\00\00\00\13\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\10\00\00\00\11\00\00\00\12\00\00\00\10\00\00\00\10\00\00\00\13\00\00\00\12\00\00\00\0d\00\00\00\0e\00\00\00\15\00\00\00\0c\00\00\00\0b\00\00\00\15\00\00\00\15\00\00\00\0f\00\00\00\0e\00\00\00\13\00\00\00&\00\00\008\00\00\00\19\00\00\00\17\00\00\00\0c\00\00\00\09\00\00\00\0a\00\00\00\10\00\00\00\17\00\00\00\19\00\00\00\0e\00\00\00\0d\00\00\00\14\00\00\00\08\00\00\00\1b\00\00\00\db\04\10\00\cb\04\10\00\b5\04\10\00\a0\04\10\00\95\04\10\00\7f\04\10\00r\04\10\00g\04\10\00T\04\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\001\07\10\00 \07\10\00\0e\07\10\00\fe\06\10\00\ee\06\10\00\db\06\10\00\c9\06\10\00\bc\06\10\00\ae\06\10\00\99\06\10\00\8d\06\10\00\82\06\10\00m\06\10\00X\06\10\00I\06\10\00;\06\10\00(\06\10\00\02\06\10\00\ca\05\10\00\b1\05\10\00\9a\05\10\00\8e\05\10\00\85\05\10\00{\05\10\00k\05\10\00T\05\10\00;\05\10\00-\05\10\00 \05\10\00\0c\05\10\00\04\05\10\00\e9\04\10\00/\00Success\00Illegal byte sequence\00Domain error\00Result not representable\00Not a tty\00Permission denied\00Operation not permitted\00No such file or directory\00No such process\00File exists\00Value too large for data type\00No space left on device\00Out of memory\00Resource busy\00Interrupted system call\00Resource temporarily unavailable\00Invalid seek\00Cross-device link\00Read-only file system\00Directory not empty\00Connection reset by peer\00Operation timed out\00Connection refused\00Host is unreachable\00Address in use\00Broken pipe\00I/O error\00No such device or address\00No such device\00Not a directory\00Is a directory\00Text file busy\00Exec format error\00Invalid argument\00Argument list too long\00Symbolic link loop\00Filename too long\00Too many open files in system\00No file descriptors available\00Bad file descriptor\00No child process\00Bad address\00File too large\00Too many links\00No locks available\00Resource deadlock would occur\00State not recoverable\00Previous owner died\00Operation canceled\00Function not implemented\00No message of desired type\00Identifier removed\00Link has been severed\00Protocol error\00Bad message\00Not a socket\00Destination address required\00Message too large\00Protocol wrong type for socket\00Protocol not available\00Protocol not supported\00Not supported\00Address family not supported by protocol\00Address not available\00Network is down\00Network unreachable\00Connection reset by network\00Connection aborted\00No buffer space available\00Socket is connected\00Socket not connected\00Operation already in progress\00Operation in progress\00Stale file handle\00Quota exceeded\00Multihop attempted\00Capabilities insufficient\00\00\00\00\00\00\00\00\00u\02N\00\d6\01\e2\04\b9\04\18\01\8e\05\ed\02\16\04\f2\00\97\03\01\038\05\af\01\82\01O\03/\04\1e\00\d4\05\a2\00\12\03\1e\03\c2\01\de\03\08\00\ac\05\00\01d\02\f1\01e\054\02\8c\02\cf\02-\03L\04\e3\05\9f\02\f8\04\1c\05\08\05\b1\02K\05\15\02x\00R\02<\03\f1\03\e4\00\c3\03}\04\cc\00\aa\03y\05$\02n\01m\03\22\04\ab\04D\00\fb\01\ae\00\83\03`\00\e5\01\07\04\94\04^\04+\00X\019\01\92\00\c2\05\9b\01C\02F\01\f6\05\00\00A\00\00\00\04\00\00\00\04\00\00\00B\00\00\00library/alloc/src/raw_vec.rscapacity overflow\00\00\008\19\10\00\11\00\00\00\1c\19\10\00\1c\00\00\00\06\02\00\00\05\00\00\00called `Option::unwrap()` on a `None` valuelibrary/alloc/src/ffi/c_str.rs\00\00\00\8f\19\10\00\1e\00\00\00,\01\00\00\11\00\00\00\8f\19\10\00\1e\00\00\002\01\00\00\11\00\00\00NulErrorA\00\00\00\04\00\00\00\04\00\00\00C\00\00\00A\00\00\00\04\00\00\00\04\00\00\00D\00\00\00called `Option::unwrap()` on a `None` value)..\00\00$\1a\10\00\02\00\00\00BorrowMutError\00index out of bounds: the len is  but the index is \00\00\00?\1a\10\00 \00\00\00_\1a\10\00\12\00\00\00:\00\00\00\f8\19\10\00\00\00\00\00\84\1a\10\00\01\00\00\00\84\1a\10\00\01\00\00\00panicked at '', \ac\1a\10\00\01\00\00\00\ad\1a\10\00\03\00\00\00\f8\19\10\00\00\00\00\00M\00\00\00\00\00\00\00\01\00\00\00N\00\00\00matches!===assertion failed: `(left  right)`\0a  left: ``,\0a right: ``: \00\00\00\e3\1a\10\00\19\00\00\00\fc\1a\10\00\12\00\00\00\0e\1b\10\00\0c\00\00\00\1a\1b\10\00\03\00\00\00`\00\00\00\e3\1a\10\00\19\00\00\00\fc\1a\10\00\12\00\00\00\0e\1b\10\00\0c\00\00\00@\1b\10\00\01\00\00\00: \00\00\f8\19\10\00\00\00\00\00d\1b\10\00\02\00\00\00M\00\00\00\0c\00\00\00\04\00\00\00O\00\00\00P\00\00\00Q\00\00\00     {\0a,\0a,  { ..\0a}, .. } { .. } }(\0a(,\0a[\00M\00\00\00\04\00\00\00\04\00\00\00R\00\00\00]library/core/src/fmt/num.rs\c9\1b\10\00\1b\00\00\00e\00\00\00\14\00\00\000x00010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899\00\00M\00\00\00\04\00\00\00\04\00\00\00S\00\00\00T\00\00\00U\00\00\00truefalselibrary/core/src/slice/memchr.rs\00\00\00\e1\1c\10\00 \00\00\00h\00\00\00'\00\00\00\e1\1c\10\00 \00\00\00\82\00\00\00\1a\00\00\00\e1\1c\10\00 \00\00\00\9e\00\00\00\05\00\00\00range start index  out of range for slice of length 4\1d\10\00\12\00\00\00F\1d\10\00\22\00\00\00range end index x\1d\10\00\10\00\00\00F\1d\10\00\22\00\00\00slice index starts at  but ends at \00\98\1d\10\00\16\00\00\00\ae\1d\10\00\0d\00\00\00\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\01\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\00\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\02\03\03\03\03\03\03\03\03\03\03\03\03\03\03\03\03\04\04\04\04\04\00\00\00\00\00\00\00\00\00\00\00library/core/src/str/lossy.rs\00\00\00\cc\1e\10\00\1d\00\00\00[\00\00\00&\00\00\00\cc\1e\10\00\1d\00\00\00b\00\00\00\1e\00\00\00\5cx\00\00\0c\1f\10\00\02\00\00\00\00\00\00\00 \00\00\00\08\00\00\00\02\00\00\00\00\00\00\00\00\00\00\00\02\00\00\00\03\00\00\00[...]byte index  is out of bounds of `\00\00=\1f\10\00\0b\00\00\00H\1f\10\00\16\00\00\00@\1b\10\00\01\00\00\00begin <= end ( <= ) when slicing `\00\00x\1f\10\00\0e\00\00\00\86\1f\10\00\04\00\00\00\8a\1f\10\00\10\00\00\00@\1b\10\00\01\00\00\00 is not a char boundary; it is inside  (bytes ) of `=\1f\10\00\0b\00\00\00\bc\1f\10\00&\00\00\00\e2\1f\10\00\08\00\00\00\ea\1f\10\00\06\00\00\00@\1b\10\00\01\00\00\00library/core/src/str/mod.rs\00\18 \10\00\1b\00\00\00\07\01\00\00\1d\00\00\00library/core/src/unicode/printable.rs\00\00\00D \10\00%\00\00\00\0a\00\00\00\1c\00\00\00D \10\00%\00\00\00\1a\00\00\00(\00\00\00\00\01\03\05\05\06\06\02\07\06\08\07\09\11\0a\1c\0b\19\0c\1a\0d\10\0e\0c\0f\04\10\03\12\12\13\09\16\01\17\04\18\01\19\03\1a\07\1b\01\1c\02\1f\16 \03+\03-\0b.\010\031\022\01\a7\02\a9\02\aa\04\ab\08\fa\02\fb\05\fd\02\fe\03\ff\09\adxy\8b\8d\a20WX\8b\8c\90\1c\dd\0e\0fKL\fb\fc./?\5c]_\e2\84\8d\8e\91\92\a9\b1\ba\bb\c5\c6\c9\ca\de\e4\e5\ff\00\04\11\12)147:;=IJ]\84\8e\92\a9\b1\b4\ba\bb\c6\ca\ce\cf\e4\e5\00\04\0d\0e\11\12)14:;EFIJ^de\84\91\9b\9d\c9\ce\cf\0d\11):;EIW[\5c^_de\8d\91\a9\b4\ba\bb\c5\c9\df\e4\e5\f0\0d\11EIde\80\84\b2\bc\be\bf\d5\d7\f0\f1\83\85\8b\a4\a6\be\bf\c5\c7\cf\da\dbH\98\bd\cd\c6\ce\cfINOWY^_\89\8e\8f\b1\b6\b7\bf\c1\c6\c7\d7\11\16\17[\5c\f6\f7\fe\ff\80mq\de\df\0e\1fno\1c\1d_}~\ae\af\7f\bb\bc\16\17\1e\1fFGNOXZ\5c^~\7f\b5\c5\d4\d5\dc\f0\f1\f5rs\8ftu\96&./\a7\af\b7\bf\c7\cf\d7\df\9a@\97\980\8f\1f\d2\d4\ce\ffNOZ[\07\08\0f\10'/\ee\efno7=?BE\90\91Sgu\c8\c9\d0\d1\d8\d9\e7\fe\ff\00 _\22\82\df\04\82D\08\1b\04\06\11\81\ac\0e\80\ab\05\1f\09\81\1b\03\19\08\01\04/\044\04\07\03\01\07\06\07\11\0aP\0f\12\07U\07\03\04\1c\0a\09\03\08\03\07\03\02\03\03\03\0c\04\05\03\0b\06\01\0e\15\05N\07\1b\07W\07\02\06\17\0cP\04C\03-\03\01\04\11\06\0f\0c:\04\1d%_ m\04j%\80\c8\05\82\b0\03\1a\06\82\fd\03Y\07\16\09\18\09\14\0c\14\0cj\06\0a\06\1a\06Y\07+\05F\0a,\04\0c\04\01\031\0b,\04\1a\06\0b\03\80\ac\06\0a\06/1M\03\80\a4\08<\03\0f\03<\078\08+\05\82\ff\11\18\08/\11-\03!\0f!\0f\80\8c\04\82\97\19\0b\15\88\94\05/\05;\07\02\0e\18\09\80\be\22t\0c\80\d6\1a\0c\05\80\ff\05\80\df\0c\f2\9d\037\09\81\5c\14\80\b8\08\80\cb\05\0a\18;\03\0a\068\08F\08\0c\06t\0b\1e\03Z\04Y\09\80\83\18\1c\0a\16\09L\04\80\8a\06\ab\a4\0c\17\041\a1\04\81\da&\07\0c\05\05\80\a6\10\81\f5\07\01 *\06L\04\80\8d\04\80\be\03\1b\03\0f\0d\00\06\01\01\03\01\04\02\05\07\07\02\08\08\09\02\0a\05\0b\02\0e\04\10\01\11\02\12\05\13\11\14\01\15\02\17\02\19\0d\1c\05\1d\08\1f\01$\01j\04k\02\af\03\b1\02\bc\02\cf\02\d1\02\d4\0c\d5\09\d6\02\d7\02\da\01\e0\05\e1\02\e7\04\e8\02\ee \f0\04\f8\02\fa\03\fb\01\0c';>NO\8f\9e\9e\9f{\8b\93\96\a2\b2\ba\86\b1\06\07\096=>V\f3\d0\d1\04\14\1867VW\7f\aa\ae\af\bd5\e0\12\87\89\8e\9e\04\0d\0e\11\12)14:EFIJNOde\5c\b6\b7\1b\1c\07\08\0a\0b\14\1769:\a8\a9\d8\d9\097\90\91\a8\07\0a;>fi\8f\92\11o_\bf\ee\efZb\f4\fc\ffST\9a\9b./'(U\9d\a0\a1\a3\a4\a7\a8\ad\ba\bc\c4\06\0b\0c\15\1d:?EQ\a6\a7\cc\cd\a0\07\19\1a\22%>?\e7\ec\ef\ff\c5\c6\04 #%&(38:HJLPSUVXZ\5c^`cefksx}\7f\8a\a4\aa\af\b0\c0\d0\ae\afno\be\93^\22{\05\03\04-\03f\03\01/.\80\82\1d\031\0f\1c\04$\09\1e\05+\05D\04\0e*\80\aa\06$\04$\04(\084\0bNC\817\09\16\0a\08\18;E9\03c\08\090\16\05!\03\1b\05\01@8\04K\05/\04\0a\07\09\07@ '\04\0c\096\03:\05\1a\07\04\0c\07PI73\0d3\07.\08\0a\81&RK+\08*\16\1a&\1c\14\17\09N\04$\09D\0d\19\07\0a\06H\08'\09u\0bB>*\06;\05\0a\06Q\06\01\05\10\03\05\80\8bb\1eH\08\0a\80\a6^\22E\0b\0a\06\0d\13:\06\0a6,\04\17\80\b9<dS\0cH\09\0aFE\1bH\08S\0dI\07\0a\80\f6F\0a\1d\03GI7\03\0e\08\0a\069\07\0a\816\19\07;\03\1cV\01\0f2\0d\83\9bfu\0b\80\c4\8aLc\0d\840\10\16\8f\aa\82G\a1\b9\829\07*\04\5c\06&\0aF\0a(\05\13\82\b0[eK\049\07\11@\05\0b\02\0e\97\f8\08\84\d6*\09\a2\e7\813\0f\01\1d\06\0e\04\08\81\8c\89\04k\05\0d\03\09\07\10\92`G\09t<\80\f6\0as\08p\15Fz\14\0c\14\0cW\09\19\80\87\81G\03\85B\0f\15\84P\1f\06\06\80\d5+\05>!\01p-\03\1a\04\02\81@\1f\11:\05\01\81\d0*\82\e6\80\f7)L\04\0a\04\02\83\11DL=\80\c2<\06\01\04U\05\1b4\02\81\0e,\04d\0cV\0a\80\ae8\1d\0d,\04\09\07\02\0e\06\80\9a\83\d8\04\11\03\0d\03w\04_\06\0c\04\01\0f\0c\048\08\0a\06(\08\22N\81T\0c\1d\03\09\076\08\0e\04\09\07\09\07\80\cb%\0a\84\06library/core/src/unicode/unicode_data.rs\08&\10\00(\00\00\00W\00\00\00>\00\00\00SomeNoneM\00\00\00\04\00\00\00\04\00\00\00V\00\00\00Utf8Errorvalid_up_toerror_len\00\00\00M\00\00\00\04\00\00\00\04\00\00\00W\00\00\00\00\03\00\00\83\04 \00\91\05`\00]\13\a0\00\12\17 \1f\0c `\1f\ef,\a0+*0 ,o\a6\e0,\02\a8`-\1e\fb`.\00\fe 6\9e\ff`6\fd\01\e16\01\0a!7$\0d\e17\ab\0ea9/\18\a190\1caH\f3\1e\a1L@4aP\f0j\a1QOo!R\9d\bc\a1R\00\cfaSe\d1\a1S\00\da!T\00\e0\e1U\ae\e2aW\ec\e4!Y\d0\e8\a1Y \00\eeY\f0\01\7fZ\00p\00\07\00-\01\01\01\02\01\02\01\01H\0b0\15\10\01e\07\02\06\02\02\01\04#\01\1e\1b[\0b:\09\09\01\18\04\01\09\01\03\01\05+\03<\08*\18\01 7\01\01\01\04\08\04\01\03\07\0a\02\1d\01:\01\01\01\02\04\08\01\09\01\0a\02\1a\01\02\029\01\04\02\04\02\02\03\03\01\1e\02\03\01\0b\029\01\04\05\01\02\04\01\14\02\16\06\01\01:\01\01\02\01\04\08\01\07\03\0a\02\1e\01;\01\01\01\0c\01\09\01(\01\03\017\01\01\03\05\03\01\04\07\02\0b\02\1d\01:\01\02\01\02\01\03\01\05\02\07\02\0b\02\1c\029\02\01\01\02\04\08\01\09\01\0a\02\1d\01H\01\04\01\02\03\01\01\08\01Q\01\02\07\0c\08b\01\02\09\0b\07I\02\1b\01\01\01\01\017\0e\01\05\01\02\05\0b\01$\09\01f\04\01\06\01\02\02\02\19\02\04\03\10\04\0d\01\02\02\06\01\0f\01\00\03\00\03\1d\02\1e\02\1e\02@\02\01\07\08\01\02\0b\09\01-\03\01\01u\02\22\01v\03\04\02\09\01\06\03\db\02\02\01:\01\01\07\01\01\01\01\02\08\06\0a\02\010\1f1\040\07\01\01\05\01(\09\0c\02 \04\02\02\01\038\01\01\02\03\01\01\03:\08\02\02\98\03\01\0d\01\07\04\01\06\01\03\02\c6@\00\01\c3!\00\03\8d\01` \00\06i\02\00\04\01\0a \02P\02\00\01\03\01\04\01\19\02\05\01\97\02\1a\12\0d\01&\08\19\0b.\030\01\02\04\02\02'\01C\06\02\02\02\02\0c\01\08\01/\013\01\01\03\02\02\05\02\01\01*\02\08\01\ee\01\02\01\04\01\00\01\00\10\10\10\00\02\00\01\e2\01\95\05\00\03\01\02\05\04(\03\04\01\a5\02\00\04\00\02P\03F\0b1\04{\016\0f)\01\02\02\0a\031\04\02\02\07\01=\03$\05\01\08>\01\0c\024\09\0a\04\02\01_\03\02\01\01\02\06\01\02\01\9d\01\03\08\15\029\02\01\01\01\01\16\01\0e\07\03\05\c3\08\02\03\01\01\17\01Q\01\02\06\01\01\02\01\01\02\01\02\eb\01\02\04\06\02\01\02\1b\02U\08\02\01\01\02j\01\01\01\02\06\01\01e\03\02\04\01\05\00\09\01\02\f5\01\0a\02\01\01\04\01\90\04\02\02\04\01 \0a(\06\02\04\08\01\09\06\02\03.\0d\01\02\00\07\01\06\01\01R\16\02\07\01\02\01\02z\06\03\01\01\02\01\07\01\01H\02\03\01\01\01\00\02\0b\024\05\05\01\01\01\00\01\06\0f\00\05;\07\00\01?\04Q\01\00\02\00.\02\17\00\01\01\03\04\05\08\08\02\07\1e\04\94\03\007\042\08\01\0e\01\16\05\01\0f\00\07\01\11\02\07\01\02\01\05d\01\a0\07\00\01=\04\00\04\00\07m\07\00`\80\f0\00\00\08&\10\00(\00\00\00?\01\00\00\09\00\00\00")
  (data $.data (i32.const 1059316) "\01\00\00\00X\12\10\00"))
