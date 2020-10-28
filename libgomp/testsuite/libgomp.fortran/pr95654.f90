! { dg-do run }
program main
  implicit none
  integer :: d1
  !$omp target map(from: d1)
  !$omp teams distribute parallel do simd default(none) lastprivate(d1) num_teams (2) num_threads (1)
  do d1 = 0, 31
  end do
  !$omp end target
  if (d1 /= 32) stop 3
end program main
