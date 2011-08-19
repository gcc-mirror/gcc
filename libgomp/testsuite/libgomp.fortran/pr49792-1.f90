! PR fortran/49792
! { dg-do run }

subroutine reverse(n, a)
  integer :: n
  real(kind=8) :: a(n)
!$omp parallel workshare
  a(:) = a(n:1:-1)
!$omp end parallel workshare
end subroutine reverse

program pr49792
  real(kind=8) :: a(16) = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
  real(kind=8) :: b(16)
  b(:) = a(16:1:-1)
  call reverse (16,a)
  if (any (a.ne.b)) call abort
end program pr49792
