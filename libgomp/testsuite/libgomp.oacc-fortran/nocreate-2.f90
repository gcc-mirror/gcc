! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

! Test no_create clause with data/parallel constructs.

program nocreate
  use openacc
  implicit none
  integer, parameter :: n = 512
  integer :: myarr(n)
  integer i

  do i = 1, n
    myarr(i) = 0
  end do

  call do_on_target(myarr, n)

  do i = 1, n
    if (myarr(i) .ne. i) stop 1
  end do

  do i = 1, n
    myarr(i) = 0
  end do

  !$acc enter data copyin(myarr)
  call do_on_target(myarr, n)
  !$acc exit data copyout(myarr)

  do i = 1, n
    if (myarr(i) .ne. i * 2) stop 2
  end do
end program nocreate

subroutine do_on_target (arr, n)
  use openacc
  implicit none
  integer :: n, arr(n)
  integer :: i

!$acc data no_create (arr)

if (acc_is_present(arr)) then
  ! The no_create clause is meant for partially shared-memory machines.  This
  ! test is written to work on non-shared-memory machines, though this is not
  ! necessarily a useful way to use the no_create clause in practice.

  !$acc parallel loop no_create (arr)
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
