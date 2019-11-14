! PR fortran/46753
! { dg-do run }

  integer :: i, j
  j = 0
!$omp parallel do reduction(+:j)
  do i = 2147483636, 2147483646
    j = j + 1
  end do
  if (j.ne.11) stop 1
  j = 0
!$omp parallel do reduction(+:j)
  do i = -2147483637, -2147483647, -1
    j = j + 1
  end do
  if (j.ne.11) stop 2
end
