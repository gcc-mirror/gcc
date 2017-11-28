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

  if (acc_is_present (b) .neqv. .false.) call abort
  if (acc_is_present (c) .neqv. .false.) call abort

  a = 0.0
  b = 1.0

  !$acc parallel copy (a) copyin (b)
    b = b + 4.0
    a = b
  !$acc end parallel

  if (a .ne. 5.0) call abort

  if (acc_is_present (b) .neqv. .false.) call abort

  a = 0.0

  !$acc parallel copy (a) create (b)
    b = 4.0
    a = b
  !$acc end parallel

  if (a .ne. 4.0) call abort

  if (acc_is_present (b) .neqv. .false.) call abort

  a = 0.0

  !$acc parallel copy (a) copy (b)
    b = 4.0
    a = b
  !$acc end parallel

  if (a .ne. 4.0) call abort
  if (b .ne. 4.0) call abort

  if (acc_is_present (b) .neqv. .false.) call abort

  a = 0.0

  !$acc parallel copy (a) copy (b) copy (c)
    b = 4.0
    c = b
    a = c
  !$acc end parallel

  if (a .ne. 4.0) call abort

  if (acc_is_present (b) .neqv. .false.) call abort

end program test
