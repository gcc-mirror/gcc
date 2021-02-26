! Test for worker-private variables

! { dg-do run }
! { dg-additional-options "-fdump-tree-oaccdevlow-details" }

program main
  integer :: w, arr(0:31)

  !$acc parallel num_gangs(32) num_workers(32) copyout(arr)
    !$acc loop gang worker private(w)
! { dg-final { scan-tree-dump-times "Decl UID \[0-9\]+ has worker partitioning:  integer\\(kind=4\\) w;" 1 "oaccdevlow" } } */
    do j = 0, 31
      w = 0
      !$acc loop seq
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
