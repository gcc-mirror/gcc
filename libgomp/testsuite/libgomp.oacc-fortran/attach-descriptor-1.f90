! { dg-do run }

program att
  use openacc
  implicit none
  type t
    integer :: arr1(10)
    integer, allocatable :: arr2(:)
  end type t
  integer :: i
  type(t) :: myvar
  integer, target :: tarr(10)
  integer, pointer :: myptr(:)

  allocate(myvar%arr2(10))

  do i=1,10
    myvar%arr1(i) = 0
    myvar%arr2(i) = 0
    tarr(i) = 0
  end do

  call acc_copyin(myvar)
  call acc_copyin(myvar%arr2)
  call acc_copyin(tarr)

  myptr => tarr

  !$acc enter data attach(myvar%arr2, myptr)

  ! FIXME: This warning is emitted on the wrong line number.
  ! { dg-warning "using vector_length \\(32\\), ignoring 1" "" { target openacc_nvidia_accel_selected } 38 }
  !$acc serial present(myvar%arr2)
  do i=1,10
    myvar%arr1(i) = i
    myvar%arr2(i) = i
  end do
  myptr(3) = 99
  !$acc end serial

  !$acc exit data detach(myvar%arr2, myptr)

  call acc_copyout(myvar%arr2)
  call acc_copyout(myvar)
  call acc_copyout(tarr)

  do i=1,10
    if (myvar%arr1(i) .ne. i) stop 1
    if (myvar%arr2(i) .ne. i) stop 2
  end do
  if (tarr(3) .ne. 99) stop 3

end program att
