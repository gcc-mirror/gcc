! { dg-do compile  { target skip-all-targets } }
! Only used by taskloop-4.f90
! To avoid inlining

module m2
  use m_taskloop4
  implicit none (external, type)
contains

subroutine grainsize (a, b, c, d)
  integer, value :: a, b, c, d
  integer :: i, j, k
  j = 0
  k = 0
  !$omp taskloop firstprivate (j, k) grainsize(d)
    do i = a, b - 1, c
      if (j == 0) then
        !$omp atomic capture
          k = v
          v = v + 1
        !$omp end atomic
        if (k >= 64) &
          stop 1
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
  !$omp taskloop firstprivate (j, k) num_tasks(d)
    do i = a, b - 1, c
      if (j == 0) then
	!$omp atomic capture
          k = v
          v = v + 1
        !$omp end atomic
        if (k >= 64) &
          stop 2
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
        integer :: min_iters, max_iters, ntasks

        ! If grainsize is present, # of task loop iters is >= grainsize && < 2 * grainsize,
        ! unless # of loop iterations is smaller than grainsize.
        if (test (0, 79, 1, 17, grainsize, ntasks, min_iters, max_iters) /= 79) &
          stop 3
        if (min_iters < 17 .or. max_iters >= 17 * 2) &
          stop 4
        if (test (-49, 2541, 7, 28, grainsize, ntasks, min_iters, max_iters) /= 370) &
          stop 5
        if (min_iters < 28 .or. max_iters >= 28 * 2) &
          stop 6
        if (test (7, 21, 2, 15, grainsize, ntasks, min_iters, max_iters) /= 7) &
          stop 7
        if (ntasks /= 1 .or. min_iters /= 7 .or. max_iters /= 7) &
          stop 8
        ! If num_tasks is present, # of tasks is min (# of loop iters, num_tasks)
        ! and each task has at least one iteration.
        if (test (-51, 2500, 48, 9, num_tasks, ntasks, min_iters, max_iters) /= 54) &
          stop 9
        if (ntasks /= 9) &
          stop 10
        if (test (0, 25, 2, 17, num_tasks, ntasks, min_iters, max_iters) /= 13) &
          stop 11
        if (ntasks /= 13) &
          stop 12
      end block
    !$omp end single
  !$omp end parallel
end program
