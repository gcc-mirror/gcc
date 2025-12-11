! { dg-do run }

program main
  use omp_lib
  implicit none

  if (openmp_version .ne. 202111) stop 1;

end program main
