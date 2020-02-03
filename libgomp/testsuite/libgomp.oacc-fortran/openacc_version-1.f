! { dg-do run }

      program main
      implicit none
      include "openacc_lib.h"

      if (openacc_version .ne. 201711) STOP 1

      end program main
