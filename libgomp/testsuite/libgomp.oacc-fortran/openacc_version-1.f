! { dg-do run }

      program main
      implicit none
      include "openacc_lib.h"

      if (openacc_version .ne. 201306) call abort;

      end program main
