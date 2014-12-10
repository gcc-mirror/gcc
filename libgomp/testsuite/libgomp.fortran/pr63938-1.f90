! PR fortran/63938
! { dg-do run }

program pr63938_1
  integer :: i, x(1)
  x(1) = 0
!$omp parallel do
  do i = 1, 1000
    !$omp atomic
    x(1) = x(1) + 1
  end do
!$omp end parallel do
  if (x(1) .ne. 1000) call abort
end program pr63938_1
