! { dg-do run }

      program main
      implicit none
      include "omp_lib.h"

      if (openmp_version .ne. 201107) call abort;

      end program main
