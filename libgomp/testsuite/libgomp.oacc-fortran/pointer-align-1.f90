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

  if (a(1) .ne. 10) call abort
  if (a(2) .ne. 52) call abort
  if (a(3) .ne. 53) call abort
  if (a(4) .ne. 54) call abort

end program test
