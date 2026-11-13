! { dg-do run }
! WAIT with ID= must not return before the statement with that ID completes.
program main
  implicit none
  integer, parameter :: n = 500000
  real(8), allocatable, asynchronous :: a(:)
  integer :: id1, id2, i
  integer(8) :: sz
  logical :: pending
  allocate (a(n))
  a = [(1.0d0 + i, i = 1, n)]
  open (10, file='async_io_10.dat', asynchronous='yes', status='replace')
  write (10, '(es24.16)', asynchronous='yes', id=id1) a
  write (10, '(es24.16)', asynchronous='yes', id=id2) a(1:10)
  if (id1 == id2) stop 1
  wait (10, id=id1)
  inquire (file='async_io_10.dat', size=sz)
  if (sz < int (n, 8) * 25 - 8192) stop 2
  inquire (10, id=id1, pending=pending)
  if (pending) stop 3
  wait (10, id=id2)
  inquire (10, id=id2, pending=pending)
  if (pending) stop 4
  close (10, status='delete')
end program main
