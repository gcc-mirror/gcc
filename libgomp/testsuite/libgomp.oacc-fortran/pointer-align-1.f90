! { dg-do run }
!
! PR middle-end/63247

program test
  implicit none

  integer(kind=2) a(4)

  a = 10;

  !$acc parallel copy(a(2:4))
  a(2) = 52
  a(3) = 53
  a(4) = 54
  !$acc end parallel

  if (a(1) .ne. 10) STOP 1
  if (a(2) .ne. 52) STOP 2
  if (a(3) .ne. 53) STOP 3
  if (a(4) .ne. 54) STOP 4

end program test
