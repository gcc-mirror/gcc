! { dg-do run }

      program main
      implicit none
      include "omp_lib.h"

      if (openmp_version .ne. 201511) stop 1;

      end program main
