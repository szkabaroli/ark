(module
  (import "second" "bar" (func $second.bar))
  (export "main" (func $func))
  (func $func
    (call $second.bar)
  )
)