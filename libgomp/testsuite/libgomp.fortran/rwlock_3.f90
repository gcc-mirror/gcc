! { dg-do run }
! Find or create the same unit number in concurrency,
! at beginning, threads cannot find the unit in cache or unit list,
! then threads will acquire the write lock to insert unit.
! This test case is used to ensure that no duplicate unit number will be
! inserted into cache nor unit list when same unit was accessed in concurrency.
program main
  use omp_lib
  implicit none
  integer:: i
  !$omp parallel private (i)
    do i = 1, 100
      open (10, file='tst.dat', asynchronous="yes")
      ! Delete the unit number from cache and unit list to stress write lock.
      close (10, status="delete")
    end do
  !$omp end parallel
end program
