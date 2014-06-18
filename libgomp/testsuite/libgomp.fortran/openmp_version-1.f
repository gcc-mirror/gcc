! { dg-do run }

      program main
      implicit none
      include "omp_lib.h"

      if (openmp_version .ne. 201307) call abort;

      end program main
