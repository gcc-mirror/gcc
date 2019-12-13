! { dg-do run }

program main
  use omp_lib
  implicit none

  if (openmp_version .ne. 201511) stop 1;

end program main
