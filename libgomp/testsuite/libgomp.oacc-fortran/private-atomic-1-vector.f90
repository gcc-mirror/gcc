! 'atomic' access of vector-private variable

! { dg-do run }

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=-fopt-info-note-omp" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! for testing/documenting aspects of that functionality.


program main
  integer :: w, arr(0:31)

  !$acc parallel num_gangs(32) num_workers(32) copyout(arr)
    !$acc loop gang worker vector private(w)
    ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
    ! { dg-note {variable 'w' in 'private' clause is candidate for adjusting OpenACC privatization level} "" { target *-*-* } .-2 }
    ! { dg-note {variable 'w' ought to be adjusted for OpenACC privatization level: 'vector'} "" { target *-*-* } .-3 }
    ! { dg-note {variable 'w' adjusted for OpenACC privatization level: 'vector'} "" { target { ! openacc_host_selected } } .-4 }
    do j = 0, 31
      w = 0
      !$acc loop seq
      ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
      do i = 0, 31
        !$acc atomic update
        w = w + 1
        ! nvptx offloading: PR83812 "operation not supported on global/shared address space".
        ! { dg-output "(\n|\r\n|\r)libgomp: cuStreamSynchronize error: operation not supported on global/shared address space(\n|\r\n|\r)$" { target openacc_nvidia_accel_selected } }
        !   Scan for what we expect in the "XFAILed" case (without actually XFAILing).
        ! { dg-shouldfail "XFAILed" { openacc_nvidia_accel_selected } }
        !   ... instead of 'dg-xfail-run-if' so that 'dg-output' is evaluated at all.
        ! { dg-final { if { [dg-process-target { xfail openacc_nvidia_accel_selected }] == "F" } { xfail "[testname-for-summary] really is XFAILed" } } }
        !   ... so that we still get an XFAIL visible in the log.
        !$acc end atomic
      end do
      arr(j) = w
    end do
  !$acc end parallel

  if (any (arr .ne. 32)) stop 1
end program main
