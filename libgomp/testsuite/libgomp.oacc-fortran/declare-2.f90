! { dg-do run }

module globalvars
  implicit none
  integer a
  !$acc declare create (a)
end module globalvars

program test
  use globalvars
  use openacc
  implicit none

  if (acc_is_present (a) .neqv. .true.) call abort

end program test
