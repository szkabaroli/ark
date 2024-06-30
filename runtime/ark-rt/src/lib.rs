#[inline]
pub fn arkrt_print() {
    println!("{}", "goodbye world")
}

pub mod ark {
    use std::ffi;

    #[repr(C)]
    pub struct HeapObject {
        strong: usize,
        weak: usize,
        value: *mut ffi::c_void,
    }

    impl HeapObject {
        fn new(val: ffi::c_void) -> Self {
            Self {
                strong: 1,
                weak: 0,
                value: Box::into_raw(Box::new(val))
            }
        }
    }

    #[repr(C)]
    pub struct WeakReference {

    }

    /// Allocates a new heap object.  The returned memory is
    /// uninitialized outside of the heap-object header.  The object
    /// has an initial retain count of 1, and its metadata is set to
    /// the given value.
    ///
    /// At some point "soon after return", it will become an
    /// invariant that metadata->getSize(returnValue) will equal
    /// requiredSize.
    ///
    /// Either aborts or throws a swift exception if the allocation fails.
    ///
    /// * `required_size` - the required size of the allocation,
    ///   including the header
    /// * `required_alignment_mask` - the required alignment of the allocation;
    ///   always one less than a power of 2 that's at least alignof(void*)
    /// * return never null
    ///
    /// POSSIBILITIES: The argument order is fair game.  It may be useful
    /// to have a variant which guarantees zero-initialized memory.
    #[inline]
    pub fn alloc_object(
        obj: *const ffi::c_void,
        required_size: usize,
        required_alignment_mask: usize,
    ) -> *const HeapObject {
        std::ptr::null()
    }

    /// Atomically increments the retain count of an object.
    ///
    /// * `object` - may be null, in which case this is a no-op
    ///
    /// * `return` - we return the object because this enables tail call
    /// optimization and the argument register to be live through the call on
    /// architectures whose argument and return register is the same register.
    ///
    /// POSSIBILITIES: We may end up wanting a bunch of different variants:
    ///  - the general version which correctly handles null values, swift
    ///     objects, and ObjC objects
    ///    - a variant that assumes that its operand is a swift object
    ///      - a variant that can safely use non-atomic operations
    ///      - maybe a variant that can assume a non-null object
    /// It may also prove worthwhile to have this use a custom CC
    /// which preserves a larger set of registers.
    #[inline]
    pub fn retain(object: *const HeapObject) -> *const HeapObject {
        std::ptr::null()
    }

    #[inline]
    pub fn release(obj: *const ffi::c_void) {}
}
