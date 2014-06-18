! { dg-do run }

program main
  use omp_lib
  implicit none

  if (openmp_version .ne. 201307) call abort;

end program main
