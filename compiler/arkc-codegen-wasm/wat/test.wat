(module
  (memory (export "memory") 1) ;; 1 page = 64KB
  (global $size (mut i32) (i32.const 0))       ;; Current number of elements
  (global $capacity (mut i32) (i32.const 4))   ;; Initial capacity
  (global $data_ptr (mut i32) (i32.const 0))   ;; Pointer to heap data

  ;; Initialize vector (allocate initial memory)
  (func $init
    (global.set $data_ptr (memory.grow (i32.const 1))) ;; Allocate 64KB (1 page)
  )
  
  ;; Push value into the vector
  (func $push (param $value i32)
    (local $index i32)
    (local $ptr i32)
    
    ;; Check if size == capacity
    (if (i32.eq (global.get $size) (global.get $capacity))
      (then
        ;; Double the capacity
        (global.set $capacity (i32.mul (global.get $capacity) (i32.const 2)))
      )
    )
    
    ;; Calculate pointer address: data_ptr + size * 4
    (local.set $index (global.get $size))
    (local.set $ptr (i32.add (global.get $data_ptr) (i32.mul (local.get $index) (i32.const 4))))
    
    ;; Store the value
    (i32.store (local.get $ptr) (local.get $value))
    
    ;; Increment size
    (global.set $size (i32.add (global.get $size) (i32.const 1)))
  )

  ;; Get value at index
  (func $get (param $index i32) (result i32)
    (local $ptr i32)
    (local.set $ptr (i32.add (global.get $data_ptr) (i32.mul (local.get $index) (i32.const 4))))
    (i32.load (local.get $ptr))
  )

  (export "init" (func $init))
  (export "push" (func $push))
  (export "get" (func $get))
  (export "size" (global.get $size))
  (export "capacity" (global.get $capacity))
)
