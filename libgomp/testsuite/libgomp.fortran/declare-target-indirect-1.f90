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

  integer :: x, expected
  procedure (foo), pointer :: foo_ptr, bar_ptr, baz_ptr

  foo_ptr => foo
  bar_ptr => bar
  baz_ptr => baz

  expected = foo () + bar () + baz ()

  !$omp target map (to: foo_ptr, bar_ptr, baz_ptr) map (from: x)
    x = foo_ptr () + bar_ptr () + baz_ptr ()
  !$omp end target

  stop x - expected
end program
