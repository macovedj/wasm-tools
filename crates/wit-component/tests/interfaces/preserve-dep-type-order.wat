(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (enum "b"))
          (export (;1;) "a" (type (eq 0)))
          (type (;2;) (record (field "f" u8)))
          (export (;3;) "ty" (type (eq 2)))
        )
      )
      (import (interface "foo:dep/foo") (instance (;0;) (type 0)))
      (alias export 0 "ty" (type (;1;)))
      (type (;2;)
        (instance
          (alias outer 1 1 (type (;0;)))
          (export (;1;) "ty" (type (eq 0)))
        )
      )
      (export (;1;) (interface "foo:foo/foo") (instance (type 2)))
      (type (;3;)
        (component
          (type (;0;)
            (instance
              (type (;0;) (enum "b"))
              (export (;1;) "a" (type (eq 0)))
              (type (;2;) (record (field "f" u8)))
              (export (;3;) "ty" (type (eq 2)))
            )
          )
          (import (interface "foo:dep/foo") (instance (;0;) (type 0)))
        )
      )
      (export (;0;) (interface "foo:foo/bar") (component (type 3)))
    )
  )
  (export (;1;) (interface "foo:foo/wit") (type 0))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)