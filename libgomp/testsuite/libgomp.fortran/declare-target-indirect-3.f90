! { dg-do run }

! Check that indirect calls work on procedures passed in via a dummy argument

module m
  integer, parameter :: offset = 123
contains
  function bar(x)
    !$omp declare target enter (bar) indirect
    integer :: bar
    integer, intent(in) :: x
    bar = x + offset
  end function

  function foo(f, x)
    integer :: foo
    procedure(bar) :: f
    integer, intent(in) :: x

    !$omp target map (to: x) map (from: foo)
      foo = f(x)
    !$omp end target
  end function
end module

program main
  use m
  implicit none

  integer :: a = 321
  integer :: b

  b = foo(bar, a)
  stop b - (a + offset)
end program
