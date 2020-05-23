! { dg-do run }
! PR 95191 - this used to hang.
! Original test case by Bill Long.
program test
  real a(10000)
  integer my_id
  integer bad_id
  integer :: iostat
  character (len=100) :: iomsg
  data my_id /1/
  data bad_id /2/
  a = 1.
  open (unit=10, file='test.dat', form='unformatted', &
       &                asynchronous='yes')
  write (unit=10, asynchronous='yes', id=my_id) a
  iomsg = ""
  wait (unit=10, id=bad_id, iostat=iostat, iomsg=iomsg)
  if (iostat == 0 .or. iomsg /= "Bad ID in WAIT statement") stop 1
  close (unit=10, status='delete')
end program test
