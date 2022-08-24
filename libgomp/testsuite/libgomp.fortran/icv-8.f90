! This tests 'set_num_teams_8' function.

program set_num_teams_8
  use omp_lib
  use, intrinsic :: iso_fortran_env
  integer(int64) :: x
  x = 42
  call omp_set_num_teams (x)
  if (omp_get_max_teams () .ne. 42) stop 1
end program
