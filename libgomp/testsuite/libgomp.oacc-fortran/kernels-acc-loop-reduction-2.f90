program foo

  IMPLICIT NONE
  INTEGER :: vol = 0

  call bar (vol)

  if (vol .ne. 4) call abort
end program foo

subroutine bar(vol)
  IMPLICIT NONE

  INTEGER :: vol
  INTEGER :: j,k

  !$ACC KERNELS
  !$ACC LOOP REDUCTION(+:vol)
  DO k=1,2
     !$ACC LOOP REDUCTION(+:vol)
     DO j=1,2
	vol = vol + 1
     ENDDO
  ENDDO
  !$ACC END KERNELS
end subroutine bar
