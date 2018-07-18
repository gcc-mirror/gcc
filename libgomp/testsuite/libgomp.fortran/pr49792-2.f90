! PR fortran/49792
! { dg-do run }
! { dg-options "-std=f2003 -fall-intrinsics" }

subroutine reverse(n, a)
  integer :: n
  real(kind=8) :: a(n)
!$omp parallel workshare
  a(:) = a(n:1:-1)
!$omp end parallel workshare
end subroutine reverse

program pr49792
  integer :: b(16)
  integer, allocatable :: a(:)
  b = 1
!$omp parallel workshare
  a = b
!$omp end parallel workshare
  if (size(a).ne.size(b)) STOP 1
  if (any (a.ne.b)) STOP 2
end program pr49792
