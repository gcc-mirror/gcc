! This test checks if the runtime can properly handle implicit
! firstprivate varaibles inside subroutines in modules.

! { dg-do run }

module test_mod
  contains
    subroutine test(x)

      IMPLICIT NONE

      INTEGER      :: x, y, j

      x = 5

      !$ACC PARALLEL LOOP copyout (y)
      DO j=1,10
         y=x
      ENDDO
      !$ACC END PARALLEL LOOP

      y = -1;

      !$ACC PARALLEL LOOP firstprivate (y) copyout (x)
      DO j=1,10
         x=y
      ENDDO
      !$ACC END PARALLEL LOOP
    end subroutine test
end module test_mod

program t
  use test_mod

  INTEGER      :: x_min

  x_min = 8

  CALL test(x_min)

  if (x_min .ne. -1) STOP 1
end program t
