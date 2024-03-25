! { dg-do run }
! Multiple threads call open/write/read/close in concurrency with different unit number,
! threads can acquire read lock concurrently, to find unit from cache or unit list very frequently,
! if not found, threads will acquire the write lock exclusively to insert unit to cache and unit list.
! This test case is used to stress both the read and write lock when access unit cache and list.
program main
  use omp_lib
  implicit none
  integer:: unit_number, v1, v2, i
  character(11) :: file_name
  character(3) :: async = "no"
  !$omp parallel private (unit_number, v1, v2, file_name, async, i)
    do i = 0, 100
      unit_number = 10 + omp_get_thread_num ()
      write (file_name, "(I3, A)") unit_number, "_tst.dat"
      file_name = adjustl(file_name)
      open (unit_number, file=file_name, asynchronous="yes")
      ! call inquire with file parameter to test find_file in unix.c
      inquire (file=file_name, asynchronous=async)
      if (async /= "YES") stop 1
      write (unit_number, *, asynchronous="yes") unit_number
      write (unit_number, *, asynchronous="yes") unit_number + 1
      close(unit_number)

      open (unit_number, file = file_name, asynchronous="yes")
      read (unit_number, *, asynchronous="yes") v1
      read (unit_number, *, asynchronous="yes") v2
      wait (unit_number)
      if ((v1 /= unit_number) .or. (v2 /= unit_number + 1)) stop 2
      close(unit_number, status="delete")
    end do
  !$omp end parallel
end program
