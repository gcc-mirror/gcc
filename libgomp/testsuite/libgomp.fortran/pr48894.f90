! PR fortran/48894
! { dg-do run }
! { dg-options "-fdefault-integer-8" }

  use omp_lib
  integer, parameter :: zero = 0
  integer :: err
  logical :: l
  err = 0
  !$omp parallel
    !$omp parallel private (l)
      l = omp_get_ancestor_thread_num (-HUGE (zero)) .ne. -1
      l = l .or. (omp_get_ancestor_thread_num (HUGE (zero)) .ne. -1)
      l = l .or. (omp_get_team_size (-HUGE (zero)) .ne. -1)
      l = l .or. (omp_get_team_size (HUGE (zero)) .ne. -1)
      if (l) then
        !$omp atomic
          err = err + 1
      endif
    !$omp end parallel
  !$omp end parallel
  if (err .ne. 0) stop 1
end
