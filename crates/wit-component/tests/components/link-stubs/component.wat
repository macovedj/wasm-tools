(component
  (type (;0;)
    (instance
      (type (;0;) (func (param "v" s32) (result s32)))
      (export (;0;) "foo" (func (type 0)))
    )
  )
  (import "test:test/test" (instance (;0;) (type 0)))
  (core module (;0;)
    (table (;0;) 2 funcref)
    (memory (;0;) 17)
    (global (;0;) (mut i32) i32.const 1048576)
    (global (;1;) i32 i32.const 1048592)
    (global (;2;) i32 i32.const 1)
    (global (;3;) i32 i32.const 1048592)
    (global (;4;) i32 i32.const 2)
    (global (;5;) (mut i32) i32.const 1048592)
    (global (;6;) (mut i32) i32.const 1114112)
    (export "__stack_pointer" (global 0))
    (export "foo:memory_base" (global 1))
    (export "foo:table_base" (global 2))
    (export "wit-component:stubs:memory_base" (global 3))
    (export "wit-component:stubs:table_base" (global 4))
    (export "__heap_base" (global 5))
    (export "__heap_end" (global 6))
    (export "__indirect_function_table" (table 0))
    (export "memory" (memory 0))
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module (;1;)
    (type (;0;) (func (param i32) (result i32)))
    (type (;1;) (func (param i32) (result i32)))
    (func (;0;) (type 0) (param i32) (result i32)
      unreachable
    )
    (func (;1;) (type 1) (param i32) (result i32)
      unreachable
    )
    (export "bar" (func 0))
    (export "foo" (func 1))
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core module (;2;)
    (@dylink.0
      (mem-info (memory 0 4))
    )
    (type (;0;) (func (param i32) (result i32)))
    (import "test:test/test" "foo" (func $import_foo (;0;) (type 0)))
    (import "env" "foo" (func $import_foo2 (;1;) (type 0)))
    (import "env" "bar" (func $import_bar (;2;) (type 0)))
    (func $foo (;3;) (type 0) (param i32) (result i32)
      unreachable
    )
    (export "test:test/test#foo" (func $foo))
  )
  (core module (;3;)
    (type (;0;) (func))
    (type (;1;) (func (param i32)))
    (import "env" "memory" (memory (;0;) 0))
    (import "env" "__indirect_function_table" (table (;0;) 0 funcref))
    (func (;0;) (type 0))
    (start 0)
    (elem (;0;) (i32.const 1) func)
    (elem (;1;) (i32.const 2) func)
    (data (;0;) (i32.const 1048576) "\00\00\00\00\00\00\10\00")
    (@producers
      (processed-by "wit-component" "$CARGO_PKG_VERSION")
    )
  )
  (core instance (;0;) (instantiate 0))
  (alias core export 0 "memory" (core memory (;0;)))
  (alias core export 0 "__heap_base" (core global (;0;)))
  (alias core export 0 "__heap_end" (core global (;1;)))
  (core instance (;1;)
    (export "__heap_base" (global 0))
    (export "__heap_end" (global 1))
  )
  (core instance (;2;))
  (alias core export 0 "memory" (core memory (;1;)))
  (alias core export 0 "__indirect_function_table" (core table (;0;)))
  (alias core export 0 "__stack_pointer" (core global (;2;)))
  (alias core export 0 "wit-component:stubs:memory_base" (core global (;3;)))
  (alias core export 0 "wit-component:stubs:table_base" (core global (;4;)))
  (core instance (;3;)
    (export "memory" (memory 1))
    (export "__indirect_function_table" (table 0))
    (export "__stack_pointer" (global 2))
    (export "__memory_base" (global 3))
    (export "__table_base" (global 4))
  )
  (core instance (;4;) (instantiate 1
      (with "GOT.mem" (instance 1))
      (with "GOT.func" (instance 2))
      (with "env" (instance 3))
    )
  )
  (alias core export 0 "__heap_base" (core global (;5;)))
  (alias core export 0 "__heap_end" (core global (;6;)))
  (core instance (;5;)
    (export "__heap_base" (global 5))
    (export "__heap_end" (global 6))
  )
  (core instance (;6;))
  (alias core export 0 "memory" (core memory (;2;)))
  (alias core export 0 "__indirect_function_table" (core table (;1;)))
  (alias core export 0 "__stack_pointer" (core global (;7;)))
  (alias core export 0 "foo:memory_base" (core global (;8;)))
  (alias core export 0 "foo:table_base" (core global (;9;)))
  (alias core export 4 "bar" (core func (;0;)))
  (alias core export 4 "foo" (core func (;1;)))
  (core instance (;7;)
    (export "memory" (memory 2))
    (export "__indirect_function_table" (table 1))
    (export "__stack_pointer" (global 7))
    (export "__memory_base" (global 8))
    (export "__table_base" (global 9))
    (export "bar" (func 0))
    (export "foo" (func 1))
  )
  (alias export 0 "foo" (func (;0;)))
  (core func (;2;) (canon lower (func 0)))
  (core instance (;8;)
    (export "foo" (func 2))
  )
  (core instance (;9;) (instantiate 2
      (with "GOT.mem" (instance 5))
      (with "GOT.func" (instance 6))
      (with "env" (instance 7))
      (with "test:test/test" (instance 8))
    )
  )
  (core instance (;10;) (instantiate 3
      (with "env" (instance 0))
      (with "foo" (instance 9))
      (with "wit-component:stubs" (instance 4))
    )
  )
  (type (;1;) (func (param "v" s32) (result s32)))
  (alias core export 9 "test:test/test#foo" (core func (;3;)))
  (func (;1;) (type 1) (canon lift (core func 3)))
  (component (;0;)
    (type (;0;) (func (param "v" s32) (result s32)))
    (import "import-func-foo" (func (;0;) (type 0)))
    (type (;1;) (func (param "v" s32) (result s32)))
    (export (;1;) "foo" (func 0) (func (type 1)))
  )
  (instance (;1;) (instantiate 0
      (with "import-func-foo" (func 1))
    )
  )
  (export (;2;) "test:test/test" (instance 1))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
