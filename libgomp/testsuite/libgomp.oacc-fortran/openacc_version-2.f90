! { dg-do run }

program main
  use openacc
  implicit none

  if (openacc_version .ne. 201306) call abort;

end program main
