! { dg-do run }

program main
  use omp_lib
  implicit none

  if (openmp_version .ne. 201511) STOP 1;

end program main
