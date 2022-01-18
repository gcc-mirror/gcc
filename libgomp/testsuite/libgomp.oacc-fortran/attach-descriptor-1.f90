! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

subroutine test(variant)
  use openacc
  implicit none
  integer :: variant
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

  if (variant == 0 &
       .or. variant == 3 &
       .or. variant == 5) then
     !$acc enter data attach(myvar%arr2, myptr)
  else if (variant == 1 &
       .or. variant == 2 &
       .or. variant == 4) then
     !$acc enter data attach(myvar%arr2, myptr)
     !$acc enter data attach(myvar%arr2, myptr)
  else
     ! Internal error.
     stop 1
  end if

  !$acc serial present(myvar%arr2)
  ! { dg-warning "using .vector_length \\(32\\)., ignoring 1" "" { target openacc_nvidia_accel_selected } .-1 }
  do i=1,10
    myvar%arr1(i) = i + variant
    myvar%arr2(i) = i - variant
  end do
  myptr(3) = 99 - variant
  !$acc end serial

  if (variant == 0) then
     !$acc exit data detach(myvar%arr2, myptr)
  else if (variant == 1) then
     !$acc exit data detach(myvar%arr2, myptr)
     !$acc exit data detach(myvar%arr2, myptr)
  else if (variant == 2) then
     !$acc exit data detach(myvar%arr2, myptr)
     !$acc exit data detach(myvar%arr2, myptr) finalize
  else if (variant == 3 &
       .or. variant == 4) then
     !$acc exit data detach(myvar%arr2, myptr) finalize
  else if (variant == 5) then
     ! Do not detach.
  else
     ! Internal error.
     stop 2
  end if

  if (.not. acc_is_present(myvar%arr2)) stop 10
  if (.not. acc_is_present(myvar)) stop 11
  if (.not. acc_is_present(tarr)) stop 12

  call acc_copyout(myvar%arr2)
  if (acc_is_present(myvar%arr2)) stop 20
  if (.not. acc_is_present(myvar)) stop 21
  if (.not. acc_is_present(tarr)) stop 22
  call acc_copyout(myvar)
  if (acc_is_present(myvar%arr2)) stop 30
  if (acc_is_present(myvar)) stop 31
  if (.not. acc_is_present(tarr)) stop 32
  call acc_copyout(tarr)
  if (acc_is_present(myvar%arr2)) stop 40
  if (acc_is_present(myvar)) stop 41
  if (acc_is_present(tarr)) stop 42

  do i=1,10
     if (myvar%arr1(i) .ne. i + variant) stop 50
     if (variant == 5) then
        ! We have not detached, so have copyied out a device pointer, so cannot
        ! access 'myvar%arr2' on the host.
     else
        if (myvar%arr2(i) .ne. i - variant) stop 51
     end if
  end do
  if (tarr(3) .ne. 99 - variant) stop 52

  if (variant == 5) then
     ! If not explicitly stopping here, we'd in the following try to deallocate
     ! the device pointer on the host, SIGSEGV.
     stop
  end if
end subroutine test

program att
  implicit none

  call test(0)

  call test(1)

  call test(2)

  call test(3)

  call test(4)

  call test(5)
  ! Make sure that 'test(5)' has stopped the program.
  stop 60
end program att
