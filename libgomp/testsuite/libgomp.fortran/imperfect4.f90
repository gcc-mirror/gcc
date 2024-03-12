! { dg-do run }

! Like imperfect2.f90, but includes blocks that are themselves wholly
! intervening code and not containers for nested loops.

program foo
  integer, save :: f1count(3), f2count(3), g1count(3), g2count(3)

  f1count(1) = 0
  f1count(2) = 0
  f1count(3) = 0
  f2count(1) = 0
  f2count(2) = 0
  f2count(3) = 0

  g1count(1) = 0
  g1count(2) = 0
  g1count(3) = 0
  g2count(1) = 0
  g2count(2) = 0
  g2count(3) = 0

  call s1 (3, 4, 5)

  ! All intervening code at the same depth must be executed the same
  ! number of times.
  if (f1count(1) /= f2count(1)) error stop 101
  if (f1count(2) /= f2count(2)) error stop 102
  if (f1count(3) /= f2count(3)) error stop 103
  if (g1count(1) /= f1count(1)) error stop 104
  if (g2count(1) /= f1count(1)) error stop 105
  if (g1count(2) /= f1count(2)) error stop 106
  if (g2count(2) /= f1count(2)) error stop 107
  if (g1count(3) /= f1count(3)) error stop 108
  if (g2count(3) /= f1count(3)) error stop 109

  ! Intervening code must be executed at least as many times as the loop
  ! that encloses it.
  if (f1count(1) < 3) error stop 111
  if (f1count(2) < 3 * 4) error stop 112

  ! Intervening code must not be executed more times than the number
  ! of logical iterations.
  if (f1count(1) > 3 * 4 * 5) error stop 121
  if (f1count(2) > 3 * 4 * 5) error stop 122

  ! Check that the innermost loop body is executed exactly the number
  ! of logical iterations expected.
  if (f1count(3) /= 3 * 4 * 5) error stop 131

contains

subroutine f1 (depth, iter)
  integer :: depth, iter
  f1count(depth) = f1count(depth) + 1
end subroutine

subroutine f2 (depth, iter)
  integer :: depth, iter
  f2count(depth) = f2count(depth) + 1
end subroutine

subroutine g1 (depth, iter)
  integer :: depth, iter
  g1count(depth) = g1count(depth) + 1
end subroutine

subroutine g2 (depth, iter)
  integer :: depth, iter
  g2count(depth) = g2count(depth) + 1
end subroutine

subroutine s1 (a1, a2, a3)
  integer :: a1, a2, a3
  integer :: i, j, k

  !$omp do collapse(3)
  do i = 1, a1
    block
      call f1 (1, i)
    end block
    block
      block
        call g1 (1, i)
      end block
      do j = 1, a2
        block
          call f1 (2, j)
        end block
        block
          block
            call g1 (2, j)
          end block
          do k = 1, a3
            call f1 (3, k)
            block
              call g1 (3, k)
              call g2 (3, k)
            end block
            call f2 (3, k)
          end do
          block
            call g2 (2, j)
          end block
        end block
        block
          call f2 (2, j)
        end block
      end do
      block
        call g2 (1, i)
      end block
    end block
    block
      call f2 (1, i)
    end block
  end do

end subroutine

end program
