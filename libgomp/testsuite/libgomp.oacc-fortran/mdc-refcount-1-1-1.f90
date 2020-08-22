! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

! Adapted from 'libgomp.oacc-fortran/deep-copy-6.f90'.

program main
  use openacc
  implicit none
  integer, parameter :: n = 512
  type mytype
    integer, allocatable :: a(:)
  end type mytype
  type(mytype) :: var

  allocate(var%a(1:n))

  !$acc data create(var)

  !$acc enter data create(var%a)

  if (.not. acc_is_present(var%a)) stop 1
  if (.not. acc_is_present(var)) stop 2

  !$acc exit data delete(var%a) finalize
  if (acc_is_present(var%a)) stop 3
  if (.not. acc_is_present(var)) stop 4

  !$acc end data
  if (acc_is_present(var%a)) stop 5
  if (acc_is_present(var)) stop 6

  deallocate(var%a)

end program main
