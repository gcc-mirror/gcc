! PR fortran/84418
! { dg-do run { target vect_simd_clones } }
! { dg-options "-fno-inline" }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

  real :: a(1024), b(1024), c(1024)
  integer :: i
  do i = 1, 1024
    a(i) = 0.5 * i
    b(i) = 1.5 * i
  end do
  !$omp simd
  do i = 1, 1024
    c(i) = foo (a(i), b(i))
  end do
  do i = 1, 1024
    if (c(i).ne.(2 * i)) stop 1
  end do
contains
  real function foo (x, y)
    real :: x, y
    !$omp declare simd linear (ref (x, y)) ! { dg-warning "Specification of the list items as arguments to the modifiers at \\(1\\) is deprecated since OpenMP 5.2 \\\[-Wdeprecated-openmp\\\]" }
    foo = x + y
  end function
end
