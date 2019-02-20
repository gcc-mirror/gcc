! { dg-do run }

program main
  implicit none

  type tnest
    integer :: ia, ib, ic
  end type tnest

  type mytype
    type(tnest) :: nest
    integer :: a, b, c
  end type mytype

  type(mytype) :: myvar
  integer :: i

  myvar%a = 0
  myvar%b = 0
  myvar%c = 0
  myvar%nest%ia = 0
  myvar%nest%ib = 0
  myvar%nest%ic = 0

!$acc enter data copyin(myvar%nest)

!$acc parallel present(myvar%nest)
  myvar%nest%ia = 4
  myvar%nest%ib = 5
  myvar%nest%ic = 6
!$acc end parallel

!$acc exit data copyout(myvar%nest)

  if (myvar%a .ne. 0) stop 1
  if (myvar%b .ne. 0) stop 2
  if (myvar%c .ne. 0) stop 3
  if (myvar%nest%ia .ne. 4) stop 4
  if (myvar%nest%ib .ne. 5) stop 5
  if (myvar%nest%ic .ne. 6) stop 6
end program main
