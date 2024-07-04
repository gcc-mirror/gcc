! { dg-do run }

! Like imperfect-transform.f90, but enables offloading.

program foo
  integer, save :: f1count(3), f2count(3)
  !$omp declare target enter (f1count, f2count)

  f1count(1) = 0
  f1count(2) = 0
  f1count(3) = 0
  f2count(1) = 0
  f2count(2) = 0
  f2count(3) = 0

  call s1 (3, 4, 5)

  ! All intervening code at the same depth must be executed the same
  ! number of times.
  if (f1count(1) /= f2count(1)) error stop 101
  if (f1count(2) /= f2count(2)) error stop 102
  if (f1count(3) /= f2count(3)) error stop 103

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
  !$omp atomic
  f1count(depth) = f1count(depth) + 1
end subroutine

subroutine f2 (depth, iter)
  integer :: depth, iter
  !$omp atomic
  f2count(depth) = f2count(depth) + 1
end subroutine

subroutine s1 (a1, a2, a3)
  integer :: a1, a2, a3
  integer :: i, j, k

  !$omp target parallel do collapse(2) map(always, tofrom:f1count, f2count) &
  !$omp & private(j, k)
  do i = 1, a1
    call f1 (1, i)
    do j = 1, a2
      call f1 (2, j)
      !$omp tile sizes(5)
      do k = 1, a3
        call f1 (3, k)
        call f2 (3, k)
      end do
      call f2 (2, j)
    end do
    call f2 (1, i)
  end do

end subroutine

end program
