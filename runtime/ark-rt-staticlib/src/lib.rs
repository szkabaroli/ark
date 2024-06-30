// Dummy lib in order to convert `ark_rt` library into static (.a) library.
 
#![allow(unused_imports)]
use ark_rt;

#[no_mangle]
pub unsafe extern "C" fn native_print_invoke() {
    ark_rt::arkrt_print()
}