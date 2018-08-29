! PR fortran/63938
! { dg-do run }

program pr63938_2
  type t
    integer :: x
  end type
  integer :: i
  type(t) :: x
  x%x = 0
!$omp parallel do
  do i = 1, 1000
    !$omp atomic
    x%x = x%x + 1
  end do
!$omp end parallel do
  if (x%x .ne. 1000) STOP 1
end program pr63938_2
