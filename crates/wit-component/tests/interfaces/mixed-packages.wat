(component
  (component $another:thing (;0;)
    (@custom "package-docs" "\01{\22interfaces\22:{\22something\22:{\22docs\22:\22documenting an interface\22,\22types\22:{\22my-record\22:{\22stability\22:{\22stable\22:{\22since\22:\221.2.3\22}}}}}}}")
    (type (;0;)
      (component
        (type (;0;)
          (instance
            (type (;0;) (record (field "foo" string)))
            (export (;1;) "my-record" (type (eq 0)))
          )
        )
        (export (;0;) "another:thing/something" (instance (type 0)))
      )
    )
    (export (;1;) "something" (type 0))
    (type (;2;)
      (component
        (type (;0;)
          (component
            (type (;0;)
              (instance
                (type (;0;) (record (field "foo" string)))
                (export (;1;) "my-record" (type (eq 0)))
              )
            )
            (export (;0;) "another:thing/something" (instance (type 0)))
          )
        )
        (export (;0;) "another:thing/another-world" (component (type 0)))
      )
    )
    (export (;3;) "another-world" (type 2))
  )
  (component $third:pkg (;1;)
    (@custom "package-docs" "\01{\22interfaces\22:{\22things\22:{\22stability\22:{\22stable\22:{\22since\22:\221.2.3\22}},\22types\22:{\22other-record\22:{\22docs\22:\22documenting a type\22}}}}}")
    (type (;0;)
      (component
        (type (;0;)
          (instance
            (type (;0;) (record (field "foo" string)))
            (export (;1;) "other-record" (type (eq 0)))
          )
        )
        (export (;0;) "third:pkg/things" (instance (type 0)))
      )
    )
    (export (;1;) "things" (type 0))
  )
  (type (;0;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance
              (type (;0;) (record (field "foo" string)))
              (export (;1;) "my-record" (type (eq 0)))
            )
          )
          (import "another:thing/something" (instance (;0;) (type 0)))
          (type (;1;)
            (instance
              (type (;0;) (record (field "foo" string)))
              (export (;1;) "other-record" (type (eq 0)))
            )
          )
          (import "third:pkg/things" (instance (;1;) (type 1)))
          (type (;2;)
            (instance
              (type (;0;) (record (field "foo" string)))
              (export (;1;) "my-record" (type (eq 0)))
            )
          )
          (export (;2;) "another:thing/something" (instance (type 2)))
        )
      )
      (export (;0;) "foo:bar/this-world" (component (type 0)))
    )
  )
  (export (;1;) "this-world" (type 0))
  (@custom "package-docs" "\00{}")
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
