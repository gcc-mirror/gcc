! { dg-do run }

module m
contains
  integer function foo ()
    !$omp declare target to (foo) indirect
    foo = 5
  end function

  integer function bar ()
    !$omp declare target to (bar) indirect
    bar = 8
  end function

  integer function baz ()
    !$omp declare target to (baz) indirect
    baz = 11
  end function
end module

program main
  use m
  implicit none

  type fp
    procedure (foo), pointer, nopass :: f => null ()
  end type

  integer, parameter :: N = 256
  integer :: i, x = 0, expected = 0;
  type (fp) :: fn_ptr (N)

  do i = 1, N
    select case (mod (i, 3))
      case (0)
        fn_ptr (i)%f => foo
      case (1)
        fn_ptr (i)%f => bar
      case (2)
        fn_ptr (i)%f => baz
    end select
    expected = expected + fn_ptr (i)%f ()
  end do

  !$omp target teams distribute parallel do &
  !$omp & reduction(+: x) map (to: fn_ptr) map (tofrom: x)
    do i = 1, N
      x = x + fn_ptr (i)%f ()
    end do
  !$omp end target teams distribute parallel do

  stop x - expected
end program
