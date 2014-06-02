! { dg-do run { target vect_simd_clones } }
! { dg-options "-fno-inline" }
! { dg-additional-sources declare-simd-3.f90 }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

module declare_simd_2_mod
  contains
    real function foo (a, b, c)
      !$omp declare simd (foo) simdlen (4) uniform (a) linear (b : 5)
      double precision, value :: a
      real, value :: c
      !$omp declare simd (foo)
      integer, value :: b
      foo = a + b * c
    end function foo
end module declare_simd_2_mod

  interface
    subroutine bar ()
    end subroutine bar
  end interface

  call bar ()
end
