! { dg-do run { target avx_runtime } }
! { dg-additional-options "-msse2" }
! The same as simd3.f90, but compiled with -msse2.  we run it only on
! AVX machine where simd3.f90 is compiled with -mavx.

include 'simd3.f90'
