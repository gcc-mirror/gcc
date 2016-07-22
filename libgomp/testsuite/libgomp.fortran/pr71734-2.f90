! { dg-do run { target avx_runtime } }
! { dg-additional-options "-msse2" }
! The same as simd4.f90, but compiled with -msse2.  we run it only on
! AVX machine where simd4.f90 is compiled with -mavx.

include 'simd4.f90'
