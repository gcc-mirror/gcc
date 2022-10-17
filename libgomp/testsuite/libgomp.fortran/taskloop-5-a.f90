! { dg-do compile  { target skip-all-targets } }
! Only used by taskloop-5-a.f90
! To avoid inlining

module m2
  use m_taskloop5
  implicit none (external, type)
contains

subroutine grainsize (a, b, c, d)
  integer, value :: a, b, c, d
  integer :: i, j, k
  j = 0
  k = 0
  !$omp taskloop firstprivate (j, k) grainsize(strict:d)
    do i = a, b - 1, c
      if (j == 0) then
        !$omp atomic capture
          k = v
          v = v + 1
        !$omp end atomic
        if (k >= 64) &
          stop 3
        w(k) = i
      end if
      j = j + 1
      u(k) = j
    end do
end

subroutine num_tasks (a, b, c, d)
  integer, value :: a, b, c, d
  integer :: i, j, k
  j = 0
  k = 0
  !$omp taskloop firstprivate (j, k) num_tasks(strict:d)
    do i = a, b - 1, c
      if (j == 0) then
        !$omp atomic capture
          k = v
          v = v + 1
        !$omp end atomic
        if (k >= 64) &
          stop 4
        w(k) = i
      end if
      j = j + 1
      u(k) = j
    end do
end
end module

program main
  use m2
  implicit none (external, type)
  !$omp parallel
    !$omp single
      block
        integer :: min_iters, max_iters, ntasks, sep

        ! If grainsize is present and has strict modifier, # of task loop iters is == grainsize,
        ! except that it can be smaller on the last task.
        if (test (0, 79, 1, 17, grainsize, ntasks, min_iters, max_iters, sep) /= 79) &
          stop 5
        if (ntasks /= 5 .or. min_iters /= 11 .or. max_iters /= 17 .or. sep /= 4) &
          stop
        if (test (-49, 2541, 7, 28, grainsize, ntasks, min_iters, max_iters, sep) /= 370) &
          stop 6
        if (ntasks /= 14 .or. min_iters /= 6 .or. max_iters /= 28 .or. sep /= 13) &
          stop
        if (test (7, 21, 2, 15, grainsize, ntasks, min_iters, max_iters, sep) /= 7) &
          stop 7
        if (ntasks /= 1 .or. min_iters /= 7 .or. max_iters /= 7 .or. sep /= 1) &
          stop 8
        !  If num_tasks is present, # of tasks is min (# of loop iters, num_tasks)
        !  and each task has at least one iteration.  If strict modifier is present,
        !  first set of tasks has ceil (# of loop iters / num_tasks) iterations,
        !  followed by possibly empty set of tasks with floor (# of loop iters / num_tasks)
        !  iterations.
        if (test (-51, 2500, 48, 9, num_tasks, ntasks, min_iters, max_iters, sep) /= 54) &
          stop 9
        if (ntasks /= 9 .or. min_iters /= 6 .or. max_iters /= 6 .or. sep /= 9) &
          stop 10
        if (test (0, 57, 1, 9, num_tasks, ntasks, min_iters, max_iters, sep) /= 57) &
          stop 11
        if (ntasks /= 9 .or. min_iters /= 6 .or. max_iters /= 7 .or. sep /= 3) &
          stop 12
        if (test (0, 25, 2, 17, num_tasks, ntasks, min_iters, max_iters, sep) /= 13) &
          stop 13
        if (ntasks /= 13 .or. min_iters /= 1 .or. max_iters /= 1 .or. sep /= 13) &
          stop 14
      end block
    !$omp end single
  !$omp end parallel
end program
