! { dg-do run }

module typemod

type :: typeimpl
  real, pointer :: p(:) => null()
end type typeimpl

type :: basictype
  class(typeimpl), pointer :: p => null()
end type basictype

type, extends(basictype) :: regulartype
  character :: void
end type regulartype

end module typemod

program main
  use typemod
  implicit none
  type(regulartype), pointer :: myvar
  integer :: i
  real :: j, k

  allocate(myvar)
  allocate(myvar%p)
  allocate(myvar%p%p(1:100))

  do i=1,100
    myvar%p%p(i) = -1.0
  end do

!$acc enter data copyin(myvar)
!$acc enter data copyin(myvar%p) create(myvar%p%p)

!$acc parallel loop present(myvar%p%p)
  do i=1,100
    myvar%p%p(i) = i * 2
  end do
!$acc end parallel loop

!$acc exit data copyout(myvar%p%p) delete(myvar%p)
!$acc exit data delete(myvar)

  do i=1,100
    if (myvar%p%p(i) .ne. i * 2) stop 1
  end do

end program main
