! { dg-do run }
! { dg-skip-if "" { *-*-* } { "-DACC_MEM_SHARED=1" } }

module globalvars
  implicit none
  real b
  !$acc declare link (b)
end module globalvars

program test
  use openacc
  use globalvars
  implicit none

  real a
  real c
  !$acc declare link (c)

  if (acc_is_present (b) .neqv. .false.) STOP 1
  if (acc_is_present (c) .neqv. .false.) STOP 2

  a = 0.0
  b = 1.0

  !$acc parallel copy (a) copyin (b)
    b = b + 4.0
    a = b
  !$acc end parallel

  if (a .ne. 5.0) STOP 3

  if (acc_is_present (b) .neqv. .false.) STOP 4

  a = 0.0

  !$acc parallel copy (a) create (b)
    b = 4.0
    a = b
  !$acc end parallel

  if (a .ne. 4.0) STOP 5

  if (acc_is_present (b) .neqv. .false.) STOP 6

  a = 0.0

  !$acc parallel copy (a) copy (b)
    b = 4.0
    a = b
  !$acc end parallel

  if (a .ne. 4.0) STOP 7
  if (b .ne. 4.0) STOP 8

  if (acc_is_present (b) .neqv. .false.) STOP 9

  a = 0.0

  !$acc parallel copy (a) copy (b) copy (c)
    b = 4.0
    c = b
    a = c
  !$acc end parallel

  if (a .ne. 4.0) STOP 10

  if (acc_is_present (b) .neqv. .false.) STOP 11

end program test
