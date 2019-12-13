! Don't compile this anywhere, it is just auxiliary
! file compiled together with declare-simd-2.f90
! to verify inter-CU module handling of omp declare simd.
! { dg-do compile { target { lp64 && { ! lp64 } } } }

subroutine bar
  use declare_simd_2_mod
  real :: b(128)
  integer :: i

  !$omp simd
  do i = 1, 128
    b(i) = i * 2.0
  end do
  !$omp simd
  do i = 1, 128
    b(i) = foo (7.0_8, 5 * i, b(i))
  end do
  do i = 1, 128
    if (b(i).ne.(7.0 + 10.0 * i * i)) stop 1
  end do
end subroutine bar
