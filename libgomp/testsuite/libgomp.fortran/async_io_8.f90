! { dg-do run }
! PR libfortran/88411
! This used to generate errors due to a mixup of
! synchronous and asynchronous execution.
! Test case by Harald Anlauf.
program gfcbug153
  implicit none
  integer :: iu, irecl
  real    :: a(100,20), b(1,3000)
  iu = 10
  a  = 0.
  b  = 0.
  inquire (iolength = irecl) a
  open (iu, file="file1.dat", access='direct', &
       asynchronous='yes', &
       recl=irecl)
  write(iu, rec=1) a(:,:)
  write(iu, rec=2) a(:,:)
  write(iu, rec=3) a(:,:)
  close (iu,status="delete")

  inquire (iolength = irecl) b
  open (iu, file="file2.dat", access='direct', &
       asynchronous='yes', &
       recl=irecl)
  write(iu, rec=1) b(:,:)
  write(iu, rec=2) b(:,:)
  write(iu, rec=3) b(:,:)
  close (iu,status="delete")
end program
