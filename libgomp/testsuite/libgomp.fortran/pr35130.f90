! PR middle-end/35130

program pr35130
  implicit none
  real, dimension(20) :: a
  integer :: k
  a(:) = 0.0
!$omp parallel do private(k)
  do k=1,size(a)
    call inner(k)
  end do
!$omp end parallel do
  if (any (a.ne.42)) call abort
contains
 subroutine inner(i)
   implicit none
   integer :: i
   a(i) = 42
 end subroutine inner
end program pr35130
