
!TODO: Move these testcases to gfortran testsuite
! once compilation with pthreads is supported there
! { dg-do run }
program main
  integer :: i
  open (10,file="tst.dat")
  write (10,'(A4)') 'asdf'
  close(10)
  i = 234
  open(10,file="tst.dat", asynchronous="yes")
  read (10,'(I4)',asynchronous="yes") i
  wait(10)
end program main
! { dg-output "Fortran runtime error: Bad value during integer read" }
! { dg-final { remote_file build delete "tst.dat" } }
