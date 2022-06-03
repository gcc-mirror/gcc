program main
  implicit none
  integer a(0:63)
  integer r, r2, i, n
  a = 0
  r = 0
  r2 = 0
  n = 64
  !$omp parallel
    !$omp scope
     !$omp scope firstprivate (n)
      !$omp do
      do i = 0, 63
       a(i) = a(i) + 1
      end do
     !$omp end scope nowait
    !$omp end scope nowait

    !$omp scope reduction(+: r) firstprivate (n)
      !$omp do
      do i = 0, 63
        r = r + i
        if (a(i) /= 1) &
          error stop
      end do
      !$omp end do nowait
      !$omp barrier
      if (n /= 64) then
        error stop
      else
        n = 128
      end if
    !$omp end scope nowait

    !$omp barrier
    if (r /= 64 * 63 / 2) &
      error stop
    !$omp scope private (i)
     !$omp scope reduction(+: r2)
      !$omp do
      do i = 0, 63
          r2 = r2 + 2 * i
          a(i) = a(i) + i
      end do
      !$omp end do nowait
     !$omp end scope
    !$omp end scope nowait
    if (r2 /= 64 * 63) &
      error stop
    !$omp do
    do i = 0, 63
      if (a(i) /= i + 1) &
        error stop
    end do
    !$omp end do nowait
  !$omp end parallel
end program
