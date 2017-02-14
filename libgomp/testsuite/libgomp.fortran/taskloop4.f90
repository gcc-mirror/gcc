! { dg-do run }
! { dg-options "-O2" }

  integer, save :: u(64), v
  integer :: min_iters, max_iters, ntasks, cnt
  procedure(grainsize), pointer :: fn
  !$omp parallel
  !$omp single
    fn => grainsize
    ! If grainsize is present, # of task loop iters is
    ! >= grainsize && < 2 * grainsize,
    ! unless # of loop iterations is smaller than grainsize.
    call test (0, 79, 1, 17, fn, ntasks, min_iters, max_iters, cnt)
    if (cnt .ne. 79) call abort
    if (min_iters .lt. 17 .or. max_iters .ge. 17 * 2) call abort
    call test (-49, 2541, 7, 28, fn, ntasks, min_iters, max_iters, cnt)
    if (cnt .ne. 370) call abort
    if (min_iters .lt. 28 .or. max_iters .ge. 28 * 2) call abort
    call test (7, 21, 2, 15, fn, ntasks, min_iters, max_iters, cnt)
    if (cnt .ne. 7) call abort
    if (min_iters .ne. 7 .or. max_iters .ne. 7) call abort
    if (ntasks .ne. 1) call abort
    fn => num_tasks
    ! If num_tasks is present, # of task loop iters is
    ! min (# of loop iters, num_tasks).
    call test (-51, 2500, 48, 9, fn, ntasks, min_iters, max_iters, cnt)
    if (cnt .ne. 54 .or. ntasks .ne. 9) call abort
    call test (0, 25, 2, 17, fn, ntasks, min_iters, max_iters, cnt)
    if (cnt .ne. 13 .or. ntasks .ne. 13) call abort
  !$omp end single
  !$omp end parallel
contains
  subroutine grainsize (a, b, c, d)
    integer, intent (in) :: a, b, c, d
    integer :: i, j, k
    j = 0
    k = 0
    !$omp taskloop firstprivate (j, k) grainsize (d)
    do i = a, b - 1, c
      if (j .eq. 0) then
        !$omp atomic capture
          k = v
          v = v + 1
        !$omp end atomic
        if (k .ge. 64) call abort
      end if
      j = j + 1
      u(k + 1) = j
    end do
  end subroutine grainsize
  subroutine num_tasks (a, b, c, d)
    integer, intent (in) :: a, b, c, d
    integer :: i, j, k
    j = 0
    k = 0
    !$omp taskloop firstprivate (j, k) num_tasks (d)
    do i = a, b - 1, c
      if (j .eq. 0) then
        !$omp atomic capture
          k = v
          v = v + 1
        !$omp end atomic
        if (k .ge. 64) call abort
      end if
      j = j + 1
      u(k + 1) = j
    end do
  end subroutine num_tasks
  subroutine test (a, b, c, d, fn, num_tasks, min_iters, max_iters, cnt)
    integer, intent (in) :: a, b, c, d
    procedure(grainsize), pointer :: fn
    integer, intent (out) :: num_tasks, min_iters, max_iters, cnt
    integer :: i
    u(:) = 0
    v = 0
    cnt = 0
    call fn (a, b, c, d)
    min_iters = 0
    max_iters = 0
    num_tasks = v
    if (v .ne. 0) then
      min_iters = minval (u(1:v))
      max_iters = maxval (u(1:v))
      cnt = sum (u(1:v))
    end if
  end subroutine test
end
