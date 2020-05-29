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
  print *, "CheCKpOInT1"
  ! { dg-output ".*CheCKpOInT1(\n|\r\n|\r)" }
  if (.not. acc_is_present(var)) stop 4
  !TODO { dg-output "STOP 4(\n|\r\n|\r)$" { target { ! openacc_host_selected } } } ! Scan for what we expect in the "XFAILed" case (without actually XFAILing).
  !TODO { dg-shouldfail "XFAILed" { ! openacc_host_selected } } ! ... instead of 'dg-xfail-run-if' so that 'dg-output' is evaluated at all.
  !TODO { dg-final { if { [dg-process-target { xfail { ! openacc_host_selected } }] == "F" } { xfail "[testname-for-summary] really is XFAILed" } } } ! ... so that we still get an XFAIL visible in the log.
  print *, "CheCKpOInT2"
  ! { dg-output ".CheCKpOInT2(\n|\r\n|\r)" { target { openacc_host_selected } } }

  !$acc end data
  if (acc_is_present(var%a)) stop 5
  if (acc_is_present(var)) stop 6

  deallocate(var%a)

end program main
