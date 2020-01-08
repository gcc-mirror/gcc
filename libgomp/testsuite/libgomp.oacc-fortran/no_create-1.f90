! { dg-do run }

! Test no_create clause with data construct when data is present/not present.

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

  myvar = 77
  do i = 1, n
    myarr(i) = 0
  end do

  !$acc data no_create (myvar, myarr)
  if (acc_is_present (myvar) .neqv. shared_memory) stop 10
  if (acc_is_present (myarr) .neqv. shared_memory) stop 11
  !$acc end data

  !$acc enter data copyin (myvar, myarr)
  !$acc data no_create (myvar, myarr)
  if (acc_is_present (myvar) .eqv. .false.) stop 20
  if (acc_is_present (myarr) .eqv. .false.) stop 21
  !$acc end data
  !$acc exit data copyout (myvar, myarr)

  if (myvar .ne. 77) stop 30
  do i = 1, n
    if (myarr(i) .ne. 0) stop 31
  end do
end program no_create
