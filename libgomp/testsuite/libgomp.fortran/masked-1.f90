module m
  use omp_lib
  implicit none (type, external)
contains
  subroutine foo (x, a)
    integer, value :: x
    integer, contiguous :: a(0:)
    integer :: i

    !$omp masked
      if (omp_get_thread_num () /= 0) &
        stop 1
      a(128) = a(128) + 1
    !$omp end masked

    !$omp masked filter (0)
      if (omp_get_thread_num () /= 0) &
        stop 2
      a(129) = a(129) + 1
    !$omp end masked

    !$omp masked filter (7)
      if (omp_get_thread_num () /= 7) &
        stop 3
      a(130) = a(130) + 1
    !$omp end masked

    !$omp masked filter (x)
      if (omp_get_thread_num () /= x) &
        stop 4
      a(131) = a(131) + 1
    !$omp end masked

    !$omp masked taskloop simd filter (x) shared(a) grainsize (12) simdlen (4)
      do i = 0, 127
        a(i) = a(i) + i
      end do
    !$omp end masked taskloop simd
  end
end

program main
  use m
  implicit none (type, external)
  integer :: i
  integer :: a(0:135)

  a = 0

  !$omp parallel num_threads (4)
    call foo (4, a)
  !$omp end parallel
  do i = 0, 127
    if (a(i) /= 0) &
      stop 5
  end do
  if (a(128) /= 1 .or. a(129) /= 1 .or. a(130) /= 0 .or. a(131) /= 0) &
    stop 6

  !$omp parallel num_threads (4)
    call foo (3, a)
  !$omp end parallel
  do i = 0, 127
    if (a(i) /= i) &
      stop 7
  end do
  if (a(128) /= 2 .or. a(129) /= 2 .or. a(130) /= 0 .or. a(131) /= 1) &
    stop 8

  !$omp parallel num_threads (8)
    call foo (8, a)
  !$omp end parallel
  do i = 0, 127
    if (a(i) /= i) &
      stop 9
  end do
  if (a(128) /= 3 .or. a(129) /= 3 .or. a(130) /= 1 .or. a(131) /= 1) &
    stop 10

  !$omp parallel num_threads (8)
    call foo (6, a)
  !$omp end parallel
  do i = 0, 127
    if (a(i) /= 2 * i) &
      stop 11
  end do
  if (a(128) /= 4 .or. a(129) /= 4 .or. a(130) /= 2 .or. a(131) /= 2) &
    stop 12

  do i = 0, 7
    a(i) = 0
  end do
  ! The filter expression can evaluate to different values in different threads.
  !$omp parallel masked num_threads (8) filter (omp_get_thread_num () + 1)
    a(omp_get_thread_num ()) = a(omp_get_thread_num ()) + 1
  !$omp end parallel masked
  do i = 0, 7
    if (a(i) /= 0) &
      stop 13
  end do

  ! And multiple threads can be filtered.
  !$omp parallel masked num_threads (8) filter (iand (omp_get_thread_num (), not(1)))
    a(omp_get_thread_num ()) = a(omp_get_thread_num ()) + 1
  !$omp end parallel masked
  do i = 0, 7
    block
      integer :: j
      j = iand (i, 1)
      if (j /= 0) then
        j = 0
      else
        j = 1
      end if
      if (a(i) /= j) &
        stop 14
    end block
  end do
end program main
