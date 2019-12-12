! { dg-do run }
!TODO: Move these testcases to gfortran testsuite
! once compilation with pthreads is supported there
! PR40008 F2008: Add NEWUNIT= for OPEN statement 
! Contributed by Jerry DeLisle <jvdelisle@gcc.gnu.org>
program newunit_1
  character(len=25) :: str
  integer(1) :: myunit, myunit2
  myunit = 25
  str = "bad"
  open(newunit=myunit, status="scratch")
  open(newunit = myunit2, file="newunit_1file")
  write(myunit,'(e24.15e2)') 1.0d0
  write(myunit2,*) "abcdefghijklmnop"
  flush(myunit)
  rewind(myunit)
  rewind(myunit2)
  read(myunit2,'(a)') str
  if (str.ne." abcdefghijklmnop") stop 1
  close(myunit)
  close(myunit2, status="delete")
end program newunit_1
