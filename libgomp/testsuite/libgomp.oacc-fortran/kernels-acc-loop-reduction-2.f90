! { dg-do run }
! { dg-xfail-run-if "PR102424" { ! openacc_host_selected } }
! TODO PR102424 OpenACC 'reduction' with outer 'loop seq', inner 'loop gang'

! { dg-additional-options "-O2" }
! { dg-additional-options "-fopt-info-omp" }

program foo

  IMPLICIT NONE
  INTEGER :: vol = 0

  call bar (vol)

  if (vol .ne. 4) stop 1
end program foo

subroutine bar(vol)
  IMPLICIT NONE

  INTEGER :: vol
  INTEGER :: j,k

  !$ACC KERNELS
  ! TODO The "reduction" dependence handling in Graphite should be adjusted to take the outer "reduction" into account correctly
  !$ACC LOOP REDUCTION(+:vol) ! { dg-bogus "optimized: assigned OpenACC seq loop parallelism" "TODO Suboptimal parallelism assigned" { xfail *-*-* } }
  DO k=1,2
     !$ACC LOOP REDUCTION(+:vol) ! { dg-bogus "optimized: assigned OpenACC gang vector loop parallelism" "TODO Suboptimal parallelism assigned" { xfail *-*-* } }
     DO j=1,2
	vol = vol + 1
     ENDDO
  ENDDO
  !$ACC END KERNELS
end subroutine bar
