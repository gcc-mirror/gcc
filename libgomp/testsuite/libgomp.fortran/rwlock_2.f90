! { dg-do run }
! Insert a unit into cache at the beginning, then start multiple
! threads to access the same unit concurrency, unit will be found in unit cache during the read lock phase.
! This test case is used to test the read lock when access unit cache and list.
program main
  use omp_lib
  implicit none
  integer:: thread_id, total_threads, i, j
  total_threads = omp_get_max_threads ()
  open (10, file='tst.dat', asynchronous="yes")
  !$omp parallel private (thread_id, i, j)
    do i = 1, 100
      thread_id = omp_get_thread_num ()
      do j = 1, 100
        write (10, *, asynchronous="yes") thread_id, i
      end do
    end do
  !$omp end parallel
  ! call inquire with file parameter to test find_file in unix.c
  call flush ()
  close (10, status="delete")
end program
