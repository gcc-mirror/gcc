! { dg-do run }

      program main
      implicit none
      include "omp_lib.h"

      if (openmp_version .ne. 202111) stop 1;

      end program main
