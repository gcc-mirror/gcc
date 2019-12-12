! { dg-do run }
!TODO: Move these testcases to gfortran testsuite
! once compilation with pthreads is supported there
! Check basic functionality of async I/O
program main
  implicit none
  integer:: i=1, j=2, k, l
  real :: a, b, c, d
  character(3), parameter:: yes="yes"
  character(4) :: str
  complex :: cc, dd
  integer, dimension(4):: is = [0, 1, 2, 3]
  integer, dimension(4):: res
  character(10) :: inq

  open (10, file='a.dat', asynchronous=yes)
  cc = (1.5, 0.5)
  inquire (10,asynchronous=inq)
  if (inq /= "YES") stop 1
  write (10,*,asynchronous=yes) 4, 3
  write (10,*,asynchronous=yes) 2, 1
  write (10,*,asynchronous=yes) 1.0, 3.0
  write (10,'(A)', asynchronous=yes) 'asdf'
  write (10,*, asynchronous=yes) cc
  close (10)
  open (20, file='a.dat', asynchronous=yes)
  read (20, *, asynchronous=yes) i, j
  read (20, *, asynchronous=yes) k, l
  read (20, *, asynchronous=yes) a, b
  read (20,'(A4)',asynchronous=yes) str
  read (20,*, asynchronous=yes) dd
  wait (20)
  if (i /= 4 .or. j /= 3) stop 2
  if (k /= 2 .or. l /= 1) stop 3
  if (a /= 1.0 .or. b /= 3.0) stop 4
  if (str /= 'asdf') stop 5
  if (cc /= dd) stop 6
  close (20,status="delete")

  open(10, file='c.dat', asynchronous=yes) 
  write(10, *, asynchronous=yes) is
  close(10)
  open(20, file='c.dat', asynchronous=yes) 
  read(20, *, asynchronous=yes) res
  wait (20)
  if (any(res /= is)) stop 7
  close (20,status="delete")
end program
