! { dg-do run }

program main
  implicit none

  type mytype
    integer :: a, b, c
  end type mytype

  type(mytype) :: myvar
  integer :: i

  myvar%a = 0
  myvar%b = 0
  myvar%c = 0

!$acc enter data copyin(myvar)

!$acc parallel present(myvar)
  myvar%a = 1
  myvar%b = 2
  myvar%c = 3
!$acc end parallel

!$acc exit data copyout(myvar)

  if (myvar%a .ne. 1) stop 1
  if (myvar%b .ne. 2) stop 2
  if (myvar%c .ne. 3) stop 3
end program main
