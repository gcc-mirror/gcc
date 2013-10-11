! { dg-do run }

program main
  use omp_lib
  implicit none

  if (openmp_version .ne. 201107) call abort;

end program main
