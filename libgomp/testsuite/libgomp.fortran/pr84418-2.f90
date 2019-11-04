! PR fortran/84418
! { dg-do run { target vect_simd_clones } }
! { dg-options "-fno-inline" }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

  type p
    integer :: i, j
  end type
  type(p) :: a(1024)
  integer :: b(4,1024), c(1024)
  integer :: i
  do i = 1, 1024
    a(i)%i = 2 * i
    a(i)%j = 3 * i
    b(1,i) = 4 * i
    b(2,i) = 5 * i
    b(3,i) = 6 * i
    b(4,i) = 7 * i
  end do
  !$omp simd
  do i = 1, 1024
    c(i) = foo (a(i), b(:,i))
  end do
  do i = 1, 1024
    if (c(i).ne.(6 * i)) stop 1
  end do
contains  
  function foo (x, y)
    type (p) :: x
    integer :: y(4), foo
    !$omp declare simd linear (ref (x, y))
    foo = x%i + y(1)
  end function
end
