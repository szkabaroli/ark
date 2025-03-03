(module
  (type $grow_memory (func))
  
  (memory (export "memory") 1) ;; 1 page (64KiB) memory
  
  (global $heap_ptr (mut i32) (i32.const 8)) ;; Start heap at 8 (first 8 bytes reserved)
  (global $heap_end (mut i32) (i32.const 65536)) ;; End of first page (64KiB)
  (global $freelist (mut i32) (i32.const 0)) ;; Head of free list (nullptr)
  
  ;; Grow memory by 64KiB
  (func $grow_memory (type $grow_memory)
    (memory.grow (i32.const 1))
    (if (i32.eq (i32.const -1))
      (then
        unreachable ;; OOM panic
      )
    )
    (global.get $heap_end)
    (i32.const 65536)
    i32.add
    global.set $heap_end
  )

  ;; Allocate memory
  (func $allocate (export "allocate") (param $size i32) (result i32)
    (local $ptr i32)
    (local $prev i32)
    (local $block_size i32)
    (local $new_ptr i32)

    ;; Align size to 8 bytes
    (local.get $size)
    (i32.const 7)
    (i32.add)
    (i32.const -8)
    (i32.and)
    local.set $size

    ;; Traverse freelist
    (global.get $freelist) ;; Start at head
    local.set $ptr
    (local.set $prev (i32.const 0))

    (block $found
      (loop $search
        (if (i32.eqz (local.get $ptr)) (then (br $found))) ;; End of list?

        ;; Get block size
        (local.get $ptr)
        (i32.load (local.get $ptr))
        local.set $block_size

        ;; Check if block fits
        (if (i32.ge_u (local.get $block_size) (local.get $size))
          (then
            ;; Unlink from list
            (if (i32.eqz (local.get $prev))
              (then
                (local.get $ptr)
                (i32.load offset=4)
                global.set $freelist
              )
              (else
                (local.get $prev)
                (local.get $ptr)
                (i32.load offset=4)
                i32.store offset=4
              )
            )
            (br $found)
          )
        )

        ;; Move to next block
        (local.get $ptr)
        (i32.load offset=4)
        local.set $prev
        local.set $ptr

        (br $search)
      )
    )

    ;; If no block found, bump allocate                        
    (if (i32.eqz (local.get $ptr))
      (then
        (block $retry
          (loop $alloc
            (global.get $heap_ptr)
            local.set $ptr

            (global.get $heap_ptr)
            (local.get $size)
            i32.add
            local.set $new_ptr

            (if (i32.ge_u (local.get $new_ptr) (global.get $heap_end))
              (then
                call $grow_memory
                (br $retry)
              )
            )

            (global.set $heap_ptr (local.get $new_ptr))
            (br $alloc)
          )
        )
      )
    )

    local.get $ptr
  )

  ;; Free memory
  (func $free (export "free") (param $ptr i32) (param $size i32)
    ;; Store block size
    (local.get $ptr)
    (local.get $size)
    i32.store

    ;; Link to freelist head
    (local.get $ptr)
    (global.get $freelist)
    i32.store offset=4

    ;; Update freelist head
    (global.set $freelist (local.get $ptr))
  )

  ;; Store value at address
  (func $store_i32 (export "store_i32") (param $addr i32) (param $value i32)
    (i32.store (local.get $addr) (local.get $value))
  )

  ;; Load value from address
  (func $load_i32 (export "load_i32") (param $addr i32) (result i32)
    (i32.load (local.get $addr))
  )
)