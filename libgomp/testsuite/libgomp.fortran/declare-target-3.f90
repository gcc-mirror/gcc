! { dg-additional-options "-fdump-tree-omplower" }

module m
  implicit none (type, external)
contains
  subroutine mod_proc(x)
    integer :: x(2)
      x = x + 5
    end subroutine
end module m

program main
  use m
  implicit none (type, external)
  if (any (foo() /= [48, 49])) stop 1
contains
  integer function fourty_two(y)
    integer :: y
    fourty_two = y + 42
  end function

  integer function wrapper (x, y)
    integer :: x, y(2)
    call mod_proc(y)
    wrapper = fourty_two(x) + 1
  end function

  function foo()
    integer :: foo(2)
    integer :: a(2)
    integer :: b, summed(2)
    a = [1, 2]
    b = -1
    !$omp target map (tofrom: a, b, summed)
      summed = wrapper (b, a)
    !$omp end target
    if (b /= -1) stop 2            ! unchanged
    if (any (summed /= 42)) stop 3 ! b + 42 + 1 = 42
    if (any (a /= [6, 7])) stop 4  ! [1, 2] + 5
    foo = summed + a               ! [48, 49]
  end function
end

! 3 times: mod_proc, fourty_two and wrapper:
! { dg-final { scan-tree-dump-times "__attribute__..omp declare target" 3 "omplower" } }
