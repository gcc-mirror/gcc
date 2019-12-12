! PR fortran/88463
! { dg-do compile { target { ! *-*-* } } }

module pr88463_1
  integer, parameter :: c = 1
  real, parameter :: d(4) = (/ 2, 3, 4, 5 /)
end module pr88463_1

program pr88463
  use pr88463_1
  use pr88463_2
  integer :: i
  real :: j(4)
  !$omp parallel default(none) private (i, j)
    i = a + b(1) + b(4) + c + d(1) + d(4)
    j(1:4) = b(1:4)
    j(1:4) = d(1:4)
  !$omp end parallel
end program pr88463
