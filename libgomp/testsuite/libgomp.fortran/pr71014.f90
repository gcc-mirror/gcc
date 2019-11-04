! PR fortran/71014
! { dg-do run }
! { dg-additional-options "-O0" }

program pr71014
  implicit none
  integer :: i, j
  integer, parameter :: t = 100*101/2
  integer :: s(16)
  s(:) = 0
!$omp parallel do
  do j = 1, 16
    associate (k => j)
      do i = 1, 100
        s(j) = s(j) + i
      end do
    end associate
  end do
  if (any(s /= t)) stop 1
end program pr71014
