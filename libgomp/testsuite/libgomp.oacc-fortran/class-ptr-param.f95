! { dg-do run }

module typemod

type mytype
  integer :: a
end type mytype

contains

subroutine mysub(c)
  implicit none

  class(mytype), allocatable :: c

!$acc parallel copy(c)
  c%a = 5
!$acc end parallel
end subroutine mysub

end module typemod

program main
  use typemod
  implicit none

  class(mytype), allocatable :: myvar
  allocate(mytype :: myvar)

  myvar%a = 0
  call mysub(myvar)

  if (myvar%a .ne. 5) stop 1
end program main
