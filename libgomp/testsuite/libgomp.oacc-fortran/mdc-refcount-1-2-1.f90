! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

! Adapted from 'libgomp.oacc-fortran/mdc-refcount-1-1-1.f90'.

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

  call acc_create(var%a)
  ! After mapping via runtime API call, separately trigger attach action; see <https://github.com/OpenACC/openacc-spec/issues/301>.
  !$acc enter data attach(var%a)

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
