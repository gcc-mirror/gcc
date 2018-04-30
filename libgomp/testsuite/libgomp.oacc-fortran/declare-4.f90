! { dg-do run }

module vars
  implicit none
  real b
 !$acc declare create (b)
end module vars

program test
  use vars
  use openacc
  implicit none
  real a

  if (acc_is_present (b) .neqv. .true.) STOP 1

  a = 2.0

  !$acc parallel copy (a)
    b = a
    a = 1.0
    a = a + b
   !$acc end parallel

  if (acc_is_present (b) .neqv. .true.) STOP 2

  if (a .ne. 3.0) STOP 3

end program test
