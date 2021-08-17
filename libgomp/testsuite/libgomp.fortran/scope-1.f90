program main
  implicit none (type, external)
  integer :: r, r2, i
  integer a(0:63)
  a = 0
  r = 0; r2 = 0
  !$omp parallel
    !$omp scope
      !$omp scope
        !$omp do
          do i = 0, 63
            a(i) = a(i) + 1
          end do
        !$omp end do
      !$omp end scope nowait
    !$omp end scope nowait

    !$omp scope reduction(+: r)
      !$omp do
        do i = 0, 63
          r = r + i
          if (a(i) /= 1) &
            stop 1
        end do
      !$omp end do nowait
      !$omp barrier
    !$omp end scope nowait

    !$omp barrier

    if (r /= 64 * 63 / 2) &
      stop 2

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
      stop 3

    !$omp do
      do i = 0, 63
        if (a(i) /= i + 1) &
          stop 4
      end do
    !$omp end do nowait
  !$omp end parallel
end
