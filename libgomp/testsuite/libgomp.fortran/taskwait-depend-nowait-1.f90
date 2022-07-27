program main
  implicit none
  integer :: a(0:63), b = 1
  !$omp parallel num_threads (4)
  block
    !$omp single
    block
      integer :: i
      !$omp taskwait depend(in: a) nowait
      !$omp taskwait depend(in: a) nowait
      !$omp taskwait
      !$omp taskgroup
      block
        !$omp taskwait depend(in: a) nowait
        !$omp taskwait depend(in: a) nowait
      end block
      do i = 0, 63
        !$omp task depend(in: a) shared(a)
        block
          a(i) = i
        end block
      end do
      !$omp taskwait depend(inout: a) nowait
      do i = 0, 63
        !$omp task depend(inoutset: a) shared(a)
        block
          if (a(i) /= i) then
            error stop
          else
            a(i) = 2 * i + 1
          end if
        end block
      end do
      !$omp taskwait nowait depend(out: a) depend(in: b)
      !$omp taskwait depend(inout: b)
      do i = 0, 63
        if (a(i) /= 2 * i + 1) &
          error stop
      end do
    end block
  end block
end program
