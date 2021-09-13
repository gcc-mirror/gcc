module m
  implicit none (external, type)
  integer :: a, b(0:2) = [1, 1, 1]
  integer(8) :: c(0:1) = [not(0_8), not(0_8)]
contains
  subroutine bar (i)
    integer :: i
    !$omp task in_reduction (*: b) in_reduction (iand: c) &
    !$omp&     in_reduction (+: a)
      a = a + 4
      b(1) = b(1) * 4
      c(1) = iand (c(1), not(ishft(1_8, i + 16)))
    !$omp end task
  end subroutine bar

  subroutine foo (x)
    integer :: x
    !$omp scope reduction (task, +: a)
      !$omp scope reduction (task, *: b)
        !$omp scope reduction (task, iand: c)
          !$omp barrier
          !$omp sections
            block
              a = a + 1; b(0) = b(0) * 2; call bar (2); b(2) = b(2) * 3
              c(1) = iand(c(1), not(ishft(1_8, 2)))
            end block
            !$omp section
            block
              b(0) = b(0) * 2; call bar (4); b(2) = b(2) * 3
              c(1) = iand(c(1), not(ishft(1_8, 4))); a = a + 1
            end block
            !$omp section
            block
              call bar (6); b(2) = b(2) * 3; c(1) = iand(c(1), not(ishft(1_8, 6)))
              a = a + 1; b(0) = b(0) * 2
            end block
            !$omp section
            block
              b(2) = b(2) * 3; c(1) = iand(c(1), not(ishft(1_8, 8)))
              a = a + 1; b(0) = b(0) * 2; call bar (8)
            end block
            !$omp section
            block
              c(1) = iand(c(1), not(ishft(1_8, 10))); a = a + 1
              b(0) = b(0) * 2; call bar (10); b(2) = b(2) * 3
            end block
            !$omp section
            block
              a = a + 1; b(0) = b(0) * 2; b(2) = b(2) * 3
              c(1) = iand(c(1), not(ishft(1_8, 12))); call bar (12)
            end block
            !$omp section
              if (x /= 0) then
                a = a + 1; b(0) = b(0) * 2; b(2) = b(2) * 3
                call bar (14); c(1) = iand (c(1), not(ishft(1_8, 14)))
              end if
          !$omp end sections
        !$omp end scope 
      !$omp end scope 
    !$omp end scope 
  end subroutine foo
end module m

program main
  use m
  implicit none (type, external)
  integer, volatile :: one
  one = 1
  call foo (0)
  if (a /= 30 .or. b(0) /= 64 .or. b(1) /= ishft (1, 12) .or. b(2) /= 3 * 3 * 3 * 3 * 3 * 3 &
      .or. c(0) /= not(0_8) .or. c(1) /= not(int(z'15541554', kind=8))) &
    stop 1
  a = 0
  b(:) = [1, 1, 1]
  c(1) = not(0_8)
  !$omp parallel
    call foo (one)
  !$omp end parallel
  if (a /= 35 .or. b(0) /= 128 .or. b(1) /= ishft(1, 14) .or. b(2) /= 3 * 3 * 3 * 3 * 3 * 3 * 3 &
      .or. c(0) /= not(0_8) .or. c(1) /= not(int(z'55545554', kind=8))) &
    stop 2
end program main
