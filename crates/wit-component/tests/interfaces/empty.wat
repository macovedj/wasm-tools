(component
  (type (;0;)
    (component
      (type (;0;)
        (instance)
      )
      (export (;0;) "foo:empty/empty" (instance (type 0)))
    )
  )
  (export (;1;) "empty" (type 0))
  (type (;2;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance)
          )
          (import "foo:empty/empty" (instance (;0;) (type 0)))
          (type (;1;)
            (instance)
          )
          (import "empty" (instance (;1;) (type 1)))
          (type (;2;)
            (instance)
          )
          (export (;2;) "foo:empty/empty" (instance (type 2)))
          (type (;3;)
            (instance)
          )
          (export (;3;) "empty2" (instance (type 3)))
        )
      )
      (export (;0;) "foo:empty/empty-world" (component (type 0)))
    )
  )
  (export (;3;) "empty-world" (type 2))
  (type (;4;)
    (component
      (type (;0;)
        (component)
      )
      (export (;0;) "foo:empty/actually-empty-world" (component (type 0)))
    )
  )
  (export (;5;) "actually-empty-world" (type 4))
  (@custom "package-docs" "\00{}")
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
