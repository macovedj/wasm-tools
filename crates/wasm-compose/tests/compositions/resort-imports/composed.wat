(component
  (type (;0;)
    (instance
      (type (;0;) u32)
      (export (;1;) "t" (type (eq 0)))
    )
  )
  (import "dep0" (instance (;0;) (type 0)))
  (type (;1;)
    (instance
      (type (;0;) u32)
      (export (;1;) "t" (type (eq 0)))
    )
  )
  (import "dep1" (instance (;1;) (type 1)))
  (alias export 0 "t" (type (;2;)))
  (alias export 1 "t" (type (;3;)))
  (type (;4;)
    (instance
      (alias outer 1 2 (type (;0;)))
      (type (;1;) (func (result 0)))
      (export (;0;) "f2" (func (type 1)))
      (alias outer 1 3 (type (;2;)))
      (type (;3;) (func (result 2)))
      (export (;1;) "f1" (func (type 3)))
    )
  )
  (import "dep2" (instance (;2;) (type 4)))
  (component (;0;)
    (type (;0;)
      (instance)
    )
    (import "b" (instance (;0;) (type 0)))
    (type (;1;)
      (instance)
    )
    (import "a" (instance (;1;) (type 1)))
  )
  (component (;1;)
    (type (;0;)
      (instance
        (type (;0;) u32)
        (export (;1;) "t" (type (eq 0)))
      )
    )
    (import "dep0" (instance $dep0 (;0;) (type 0)))
    (alias export $dep0 "t" (type $t-dep0 (;1;)))
    (type (;2;)
      (instance
        (alias outer 1 $t-dep0 (type (;0;)))
        (type (;1;) (func (result 0)))
        (export (;0;) "f2" (func (type 1)))
      )
    )
    (import "dep2" (instance (;1;) (type 2)))
  )
  (component (;2;)
    (type (;0;)
      (instance
        (type (;0;) u32)
        (export (;1;) "t" (type (eq 0)))
      )
    )
    (import "dep1" (instance $dep1 (;0;) (type 0)))
    (alias export $dep1 "t" (type $t-dep1 (;1;)))
    (type (;2;)
      (instance
        (alias outer 1 $t-dep1 (type (;0;)))
        (type (;1;) (func (result 0)))
        (export (;0;) "f1" (func (type 1)))
      )
    )
    (import "dep2" (instance (;1;) (type 2)))
  )
  (instance (;3;) (instantiate 2
      (with "dep1" (instance 1))
      (with "dep2" (instance 2))
    )
  )
  (instance (;4;) (instantiate 1
      (with "dep0" (instance 0))
      (with "dep2" (instance 2))
    )
  )
  (instance (;5;) (instantiate 0
      (with "b" (instance 4))
      (with "a" (instance 3))
    )
  )
)
