! { dg-do run }
! PR libfortran/128720 - Derived types with defined I/O in asynchronous data
! transfer statements crashed.
module async_io_11_m
  implicit none
  type :: pt
     real :: x, y
   contains
     procedure :: w => wr
     procedure :: r => rd
     generic :: write(unformatted) => w
     generic :: read(unformatted) => r
  end type pt
contains
  subroutine wr (d, u, ios, msg)
    class(pt), intent(in) :: d
    integer, intent(in) :: u
    integer, intent(out) :: ios
    character(*), intent(inout) :: msg
    write (u, iostat=ios, iomsg=msg) d%y, d%x
  end subroutine wr
  subroutine rd (d, u, ios, msg)
    class(pt), intent(inout) :: d
    integer, intent(in) :: u
    integer, intent(out) :: ios
    character(*), intent(inout) :: msg
    read (u, iostat=ios, iomsg=msg) d%y, d%x
  end subroutine rd
end module async_io_11_m

program main
  use async_io_11_m
  implicit none
  type(pt), asynchronous :: a, b
  real, allocatable, asynchronous :: big(:)
  integer :: id0, id1, id2
  logical :: pend
  allocate (big(2000000))
  a = pt(3.0, 4.0)
  big = 7.0
  open (10, file='async_io_11.bin', form='unformatted', &
        asynchronous='yes', status='replace')
  write (10, asynchronous='yes', id=id0) big
  write (10, asynchronous='yes', id=id1) a
  write (10, asynchronous='yes', id=id2) big
  if (id1 <= id0 .or. id2 <= id1) stop 1
  inquire (10, id=id1, pending=pend)
  if (pend) stop 2
  wait (10, id=id2)
  rewind (10)
  big = 0.0
  read (10) big
  if (any (big /= 7.0)) stop 3
  read (10, asynchronous='yes') b
  big = 0.0
  read (10, asynchronous='yes') big
  wait (10)
  close (10, status='delete')
  if (b%x /= 3.0 .or. b%y /= 4.0) stop 4
  if (any (big /= 7.0)) stop 5
end program main
