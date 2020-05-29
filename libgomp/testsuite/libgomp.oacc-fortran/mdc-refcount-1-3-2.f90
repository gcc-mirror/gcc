! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

! Copy of 'libgomp.oacc-fortran/mdc-refcount-1-3-1.f90', without 'finalize' clause.

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

  !$acc exit data detach(var%a)
  print *, "CheCKpOInT1"
  ! { dg-output ".*CheCKpOInT1(\n|\r\n|\r)" }
  !$acc exit data delete(var%a)
  !TODO { dg-output "(\n|\r\n|\r)libgomp: attach count underflow(\n|\r\n|\r)$" { target { ! openacc_host_selected } } } ! Scan for what we expect in the "XFAILed" case (without actually XFAILing).
  !TODO { dg-shouldfail "XFAILed" { ! openacc_host_selected } } ! ... instead of 'dg-xfail-run-if' so that 'dg-output' is evaluated at all.
  !TODO { dg-final { if { [dg-process-target { xfail { ! openacc_host_selected } }] == "F" } { xfail "[testname-for-summary] really is XFAILed" } } } ! ... so that we still get an XFAIL visible in the log.
  print *, "CheCKpOInT2"
  ! { dg-output ".CheCKpOInT2(\n|\r\n|\r)" { target { openacc_host_selected } } }
  if (acc_is_present(var%a)) stop 3
  if (.not. acc_is_present(var)) stop 4

  !$acc end data
  if (acc_is_present(var%a)) stop 5
  if (acc_is_present(var)) stop 6

  deallocate(var%a)

end program main
