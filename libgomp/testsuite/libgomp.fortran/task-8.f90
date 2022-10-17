! { dg-do run }

program main
  implicit none
  integer :: i
  i = 0
  !$omp task
    !$omp target nowait private (i)
      i = 1
    !$omp end target
    !$omp taskwait
  !$omp end task
end
