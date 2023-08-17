(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (list u8))
          (type (;1;) (record (field "a1" u32) (field "a2" u64) (field "a3" s32) (field "a4" s64) (field "b" string) (field "c" 0)))
          (export (;2;) "other-record" (type (eq 1)))
          (type (;3;) (record (field "x" string) (field "y" 2) (field "c1" u32) (field "c2" u64) (field "c3" s32) (field "c4" s64)))
          (export (;4;) "some-record" (type (eq 3)))
          (type (;5;) (variant (case "a") (case "b" u32) (case "c" string)))
          (export (;6;) "other-variant" (type (eq 5)))
          (type (;7;) (list 6))
          (type (;8;) (variant (case "a" string) (case "b") (case "c" u32) (case "d" 7)))
          (export (;9;) "some-variant" (type (eq 8)))
          (type (;10;) (tuple string u8 s8 u16 s16 u32 s32 u64 s64 float32 float64 char))
          (type (;11;) (list 10))
          (export (;12;) "load-store-all-sizes" (type (eq 11)))
          (type (;13;) (func (param "x" 0)))
          (export (;0;) "list-u8-param" (func (type 13)))
          (type (;14;) (list u16))
          (type (;15;) (func (param "x" 14)))
          (export (;1;) "list-u16-param" (func (type 15)))
          (type (;16;) (list u32))
          (type (;17;) (func (param "x" 16)))
          (export (;2;) "list-u32-param" (func (type 17)))
          (type (;18;) (list u64))
          (type (;19;) (func (param "x" 18)))
          (export (;3;) "list-u64-param" (func (type 19)))
          (type (;20;) (list s8))
          (type (;21;) (func (param "x" 20)))
          (export (;4;) "list-s8-param" (func (type 21)))
          (type (;22;) (list s16))
          (type (;23;) (func (param "x" 22)))
          (export (;5;) "list-s16-param" (func (type 23)))
          (type (;24;) (list s32))
          (type (;25;) (func (param "x" 24)))
          (export (;6;) "list-s32-param" (func (type 25)))
          (type (;26;) (list s64))
          (type (;27;) (func (param "x" 26)))
          (export (;7;) "list-s64-param" (func (type 27)))
          (type (;28;) (list float32))
          (type (;29;) (func (param "x" 28)))
          (export (;8;) "list-float32-param" (func (type 29)))
          (type (;30;) (list float64))
          (type (;31;) (func (param "x" 30)))
          (export (;9;) "list-float64-param" (func (type 31)))
          (type (;32;) (func (result 0)))
          (export (;10;) "list-u8-ret" (func (type 32)))
          (type (;33;) (func (result 14)))
          (export (;11;) "list-u16-ret" (func (type 33)))
          (type (;34;) (func (result 16)))
          (export (;12;) "list-u32-ret" (func (type 34)))
          (type (;35;) (func (result 18)))
          (export (;13;) "list-u64-ret" (func (type 35)))
          (type (;36;) (func (result 20)))
          (export (;14;) "list-s8-ret" (func (type 36)))
          (type (;37;) (func (result 22)))
          (export (;15;) "list-s16-ret" (func (type 37)))
          (type (;38;) (func (result 24)))
          (export (;16;) "list-s32-ret" (func (type 38)))
          (type (;39;) (func (result 26)))
          (export (;17;) "list-s64-ret" (func (type 39)))
          (type (;40;) (func (result 28)))
          (export (;18;) "list-float32-ret" (func (type 40)))
          (type (;41;) (func (result 30)))
          (export (;19;) "list-float64-ret" (func (type 41)))
          (type (;42;) (tuple u8 s8))
          (type (;43;) (list 42))
          (type (;44;) (tuple s64 u32))
          (type (;45;) (list 44))
          (type (;46;) (func (param "x" 43) (result 45)))
          (export (;20;) "tuple-list" (func (type 46)))
          (type (;47;) (list string))
          (type (;48;) (func (param "a" 47)))
          (export (;21;) "string-list-arg" (func (type 48)))
          (type (;49;) (func (result 47)))
          (export (;22;) "string-list-ret" (func (type 49)))
          (type (;50;) (tuple u8 string))
          (type (;51;) (list 50))
          (type (;52;) (tuple string u8))
          (type (;53;) (list 52))
          (type (;54;) (func (param "x" 51) (result 53)))
          (export (;23;) "tuple-string-list" (func (type 54)))
          (type (;55;) (func (param "x" 47) (result 47)))
          (export (;24;) "string-list" (func (type 55)))
          (type (;56;) (list 4))
          (type (;57;) (list 2))
          (type (;58;) (func (param "x" 56) (result 57)))
          (export (;25;) "record-list" (func (type 58)))
          (type (;59;) (list 9))
          (type (;60;) (func (param "x" 59) (result 7)))
          (export (;26;) "variant-list" (func (type 60)))
          (type (;61;) (func (param "a" 12) (result 12)))
          (export (;27;) "load-store-everything" (func (type 61)))
        )
      )
      (export (;0;) (interface "foo:foo/lists") (instance (type 0)))
      (type (;1;)
        (component
          (type (;0;)
            (instance
              (type (;0;) (list u8))
              (type (;1;) (record (field "a1" u32) (field "a2" u64) (field "a3" s32) (field "a4" s64) (field "b" string) (field "c" 0)))
              (export (;2;) "other-record" (type (eq 1)))
              (type (;3;) (record (field "x" string) (field "y" 2) (field "c1" u32) (field "c2" u64) (field "c3" s32) (field "c4" s64)))
              (export (;4;) "some-record" (type (eq 3)))
              (type (;5;) (variant (case "a") (case "b" u32) (case "c" string)))
              (export (;6;) "other-variant" (type (eq 5)))
              (type (;7;) (list 6))
              (type (;8;) (variant (case "a" string) (case "b") (case "c" u32) (case "d" 7)))
              (export (;9;) "some-variant" (type (eq 8)))
              (type (;10;) (tuple string u8 s8 u16 s16 u32 s32 u64 s64 float32 float64 char))
              (type (;11;) (list 10))
              (export (;12;) "load-store-all-sizes" (type (eq 11)))
              (type (;13;) (func (param "x" 0)))
              (export (;0;) "list-u8-param" (func (type 13)))
              (type (;14;) (list u16))
              (type (;15;) (func (param "x" 14)))
              (export (;1;) "list-u16-param" (func (type 15)))
              (type (;16;) (list u32))
              (type (;17;) (func (param "x" 16)))
              (export (;2;) "list-u32-param" (func (type 17)))
              (type (;18;) (list u64))
              (type (;19;) (func (param "x" 18)))
              (export (;3;) "list-u64-param" (func (type 19)))
              (type (;20;) (list s8))
              (type (;21;) (func (param "x" 20)))
              (export (;4;) "list-s8-param" (func (type 21)))
              (type (;22;) (list s16))
              (type (;23;) (func (param "x" 22)))
              (export (;5;) "list-s16-param" (func (type 23)))
              (type (;24;) (list s32))
              (type (;25;) (func (param "x" 24)))
              (export (;6;) "list-s32-param" (func (type 25)))
              (type (;26;) (list s64))
              (type (;27;) (func (param "x" 26)))
              (export (;7;) "list-s64-param" (func (type 27)))
              (type (;28;) (list float32))
              (type (;29;) (func (param "x" 28)))
              (export (;8;) "list-float32-param" (func (type 29)))
              (type (;30;) (list float64))
              (type (;31;) (func (param "x" 30)))
              (export (;9;) "list-float64-param" (func (type 31)))
              (type (;32;) (func (result 0)))
              (export (;10;) "list-u8-ret" (func (type 32)))
              (type (;33;) (func (result 14)))
              (export (;11;) "list-u16-ret" (func (type 33)))
              (type (;34;) (func (result 16)))
              (export (;12;) "list-u32-ret" (func (type 34)))
              (type (;35;) (func (result 18)))
              (export (;13;) "list-u64-ret" (func (type 35)))
              (type (;36;) (func (result 20)))
              (export (;14;) "list-s8-ret" (func (type 36)))
              (type (;37;) (func (result 22)))
              (export (;15;) "list-s16-ret" (func (type 37)))
              (type (;38;) (func (result 24)))
              (export (;16;) "list-s32-ret" (func (type 38)))
              (type (;39;) (func (result 26)))
              (export (;17;) "list-s64-ret" (func (type 39)))
              (type (;40;) (func (result 28)))
              (export (;18;) "list-float32-ret" (func (type 40)))
              (type (;41;) (func (result 30)))
              (export (;19;) "list-float64-ret" (func (type 41)))
              (type (;42;) (tuple u8 s8))
              (type (;43;) (list 42))
              (type (;44;) (tuple s64 u32))
              (type (;45;) (list 44))
              (type (;46;) (func (param "x" 43) (result 45)))
              (export (;20;) "tuple-list" (func (type 46)))
              (type (;47;) (list string))
              (type (;48;) (func (param "a" 47)))
              (export (;21;) "string-list-arg" (func (type 48)))
              (type (;49;) (func (result 47)))
              (export (;22;) "string-list-ret" (func (type 49)))
              (type (;50;) (tuple u8 string))
              (type (;51;) (list 50))
              (type (;52;) (tuple string u8))
              (type (;53;) (list 52))
              (type (;54;) (func (param "x" 51) (result 53)))
              (export (;23;) "tuple-string-list" (func (type 54)))
              (type (;55;) (func (param "x" 47) (result 47)))
              (export (;24;) "string-list" (func (type 55)))
              (type (;56;) (list 4))
              (type (;57;) (list 2))
              (type (;58;) (func (param "x" 56) (result 57)))
              (export (;25;) "record-list" (func (type 58)))
              (type (;59;) (list 9))
              (type (;60;) (func (param "x" 59) (result 7)))
              (export (;26;) "variant-list" (func (type 60)))
              (type (;61;) (func (param "a" 12) (result 12)))
              (export (;27;) "load-store-everything" (func (type 61)))
            )
          )
          (import (interface "foo:foo/lists") (instance (;0;) (type 0)))
        )
      )
      (export (;0;) (interface "foo:foo/lists-world") (component (type 1)))
    )
  )
  (export (;1;) (interface "foo:foo/wit") (type 0))
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)