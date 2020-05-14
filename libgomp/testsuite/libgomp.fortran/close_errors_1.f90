! { dg-do run }
! PR 95115 - this used to hang with -pthread.  Original test case by
! Bill Long.

program test
  character(len=16) my_status
  character(len=1000) :: iomsg
  open (unit=10, file='test.dat')
  print *,42
  write (10, *) 'weird'
  rewind (10)
  read (10, *) my_status
  close (10)
  open (unit=10, file='test.dat')
  close (unit=10, status=my_status, iostat=ios, iomsg=iomsg)
  if (ios == 0) stop 1
  if (iomsg /= "Bad STATUS parameter in CLOSE statement") stop 2
  close (10, status='delete')
end program test
