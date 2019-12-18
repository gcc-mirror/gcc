! { dg-do run }

program main
  use iso_c_binding, only: c_sizeof
  use openacc, only: acc_is_present
  implicit none
  integer i
  integer, parameter :: n = 100
  real*4 b(n), c(n)
  real :: d(n), e(n)
  common /BLOCK/ d, e

  !$acc enter data create(b) create(d)

  if (.not. acc_is_present(b, c_sizeof(b))) stop 1
  if (.not. acc_is_present(d, c_sizeof(d))) stop 2
#if !ACC_MEM_SHARED
  if (acc_is_present(c, 1) .or. acc_is_present(c, c_sizeof(c))) stop 3
  if (acc_is_present(e, 1) .or. acc_is_present(e, c_sizeof(d))) stop 4
#endif

  !$acc parallel loop no_create(b) no_create(c) no_create(/BLOCK/)
  do i = 1, n
     b(i) = i
     d(i) = -i
  end do
  !$acc end parallel loop

  if (.not. acc_is_present(b, c_sizeof(b))) stop 5
  if (.not. acc_is_present(d, c_sizeof(d))) stop 6
#if !ACC_MEM_SHARED
  if (acc_is_present(c, 1) .or. acc_is_present(c, c_sizeof(c))) stop 7
  if (acc_is_present(e, 1) .or. acc_is_present(e, c_sizeof(e))) stop 8
#endif

  !$acc exit data copyout(b) copyout(d)
  if (any(abs(b - [(real(i), i = 1, n)]) > 10*epsilon(b))) stop 9
  if (any(abs(d - [(real(-i), i = 1, n)]) > 10*epsilon(d))) stop 10
end program main
