! { dg-do run }
! PR libfortran/128721 - Asynchronous statements queued after one that fails are
! skipped without leaking memory.
program main
  implicit none
  integer, parameter :: n = 2000000
  real, allocatable, asynchronous :: a(:)
  real, asynchronous :: b(100), c(100)
  integer :: i, ios, nerr
  allocate (a(n))
  a = 1.0
  open (10, file='async_io_12.dat', form='unformatted', &
        asynchronous='yes', status='replace')
  write (10) a
  rewind (10)
  a = 0.0
  nerr = 0
  read (10, asynchronous='yes', iostat=ios) a
  if (ios /= 0) nerr = nerr + 1
  do i = 1, 100
    read (10, asynchronous='yes', iostat=ios) b, c
    if (ios /= 0) nerr = nerr + 1
  end do
  wait (10, iostat=ios)
  if (ios /= 0) nerr = nerr + 1
  if (nerr == 0) stop 1
  if (any (a /= 1.0)) stop 2
  rewind (10)
  b = 0.0
  read (10, asynchronous='yes') b
  wait (10)
  if (any (b /= 1.0)) stop 3
  close (10, status='delete')
  deallocate (a)
end program main
