! { dg-do  run }
!TODO: Move these testcases to gfortran testsuite
! once compilation with pthreads is supported there
program main
  implicit none
  integer :: i, ios
  character(len=100) :: iom
  open (10,file="tst.dat")
  write (10,'(A4)') 'asdf'
  close(10)
  i = 234
  open(10,file="tst.dat", asynchronous="yes")
  read (10,'(I4)',asynchronous="yes") i
  iom = ' '
  wait (10,iostat=ios,iomsg=iom)
  if (iom == ' ') stop 1
  close(10,status="delete")
end program main
