! { dg-do run }

program main
  use openacc
  implicit none

  integer :: i, j, n

  j = 0
  n = 1000000

  !$acc parallel async (0) copy (j)
    do i = 1, 1000000
      j = j + 1
    end do
  !$acc end parallel

  call acc_wait_async (0, 1)

  ! Test unseen async-argument.
  if (acc_async_test (2) .neqv. .TRUE.) stop 1
  call acc_wait_async (2, 1)

  call acc_wait (1)

  if (acc_async_test (0) .neqv. .TRUE.) stop 2
  if (acc_async_test (1) .neqv. .TRUE.) stop 3
  if (acc_async_test (2) .neqv. .TRUE.) stop 4

end program
