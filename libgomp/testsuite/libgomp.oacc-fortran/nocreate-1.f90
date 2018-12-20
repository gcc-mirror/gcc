! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

! Test no_create clause with data construct when data is present/not present.

program nocreate
  use openacc
  implicit none
  integer, parameter :: n = 512
  integer :: myarr(n)
  integer i

  do i = 1, n
    myarr(i) = 0
  end do

  !$acc data no_create (myarr)
  if (acc_is_present (myarr)) stop 1
  !$acc end data

  !$acc enter data copyin (myarr)
  !$acc data no_create (myarr)
  if (acc_is_present (myarr) .eqv. .false.) stop 2
  !$acc end data
  !$acc exit data copyout (myarr)

  do i = 1, n
    if (myarr(i) .ne. 0) stop 3
  end do
end program nocreate
