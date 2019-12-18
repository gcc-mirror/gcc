! { dg-do run }

! Test no_create clause with data/parallel constructs.

program no_create
  use openacc
  implicit none
  logical :: shared_memory
  integer, parameter :: n = 512
  integer :: myvar, myarr(n)
  integer i

  shared_memory = .false.
  !$acc kernels copyin (shared_memory)
  shared_memory = .true.
  !$acc end kernels

  myvar = 55
  do i = 1, n
    myarr(i) = 0
  end do

  call do_on_target(myvar, n, myarr)

  if (shared_memory) then
     if (myvar .ne. 44) stop 10
  else
     if (myvar .ne. 33) stop 11
  end if
  do i = 1, n
    if (shared_memory) then
      if (myarr(i) .ne. i * 2) stop 20
    else
      if (myarr(i) .ne. i) stop 21
    end if
  end do

  myvar = 55
  do i = 1, n
    myarr(i) = 0
  end do

  !$acc enter data copyin(myvar, myarr)
  call do_on_target(myvar, n, myarr)
  !$acc exit data copyout(myvar, myarr)

  if (myvar .ne. 44) stop 30
  do i = 1, n
    if (myarr(i) .ne. i * 2) stop 31
  end do
end program no_create

subroutine do_on_target (var, n, arr)
  use openacc
  implicit none
  integer :: var, n, arr(n)
  integer :: i

!$acc data no_create (var, arr)

if (acc_is_present(var)) then
  ! The no_create clause is meant for partially shared-memory machines.  This
  ! test is written to work on non-shared-memory machines, though this is not
  ! necessarily a useful way to use the no_create clause in practice.

  !$acc parallel !no_create (var)
   var = 44
  !$acc end parallel
else
   var = 33
end if
if (acc_is_present(arr)) then
  ! The no_create clause is meant for partially shared-memory machines.  This
  ! test is written to work on non-shared-memory machines, though this is not
  ! necessarily a useful way to use the no_create clause in practice.

  !$acc parallel loop !no_create (arr)
  do i = 1, n
    arr(i) = i * 2
  end do
  !$acc end parallel loop
else
  do i = 1, n
    arr(i) = i
  end do
end if

!$acc end data

end subroutine do_on_target
