! { dg-do run }

! Like imperfect2.f90, but adds bindings to the blocks.

module m
  implicit none
  type t
      integer :: i
    contains
      final :: fini
  end type t

  integer :: ccount(3), dcount(3)

  contains

    subroutine init(x, n)
      type(t) :: x
      integer :: n
      x%i = n
      ccount(x%i) = ccount(x%i) + 1
    end subroutine init

    subroutine fini(x)
      type(t) :: x
      dcount(x%i) = dcount(x%i) + 1
    end subroutine fini
end module m

program foo
  use m

  integer :: f1count(3), f2count(3), g1count(3), g2count(3)

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

  ! Check that constructors and destructors are called equal number of times.
  if (ccount(1) /= dcount(1)) error stop 141
  if (ccount(2) /= dcount(2)) error stop 142
  if (ccount(3) /= dcount(3)) error stop 143

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
    call f1 (1, i)
    block
      type (t) :: local1
      call init (local1, 1)
      call g1 (local1%i, i)
      do j = 1, a2
        call f1 (2, j)
        block
	  type (t) :: local2
	  call init (local2, 2)
          call g1 (local2%i, j)
          do k = 1, a3
            call f1 (3, k)
            block
	      type (t) :: local3
	      call init (local3, 3)
              call g1 (local3%i, k)
              call g2 (local3%i, k)
            end block
            call f2 (3, k)
          end do
          call g2 (local2%i, j)
        end block
        call f2 (2, j)
      end do
      call g2 (local1%i, i)
    end block
    call f2 (1, i)
  end do

end subroutine

end program
