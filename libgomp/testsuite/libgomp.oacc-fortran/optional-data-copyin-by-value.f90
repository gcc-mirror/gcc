! Test OpenACC data regions with optional arguments passed by value.

! { dg-do run }

program test
  implicit none

  integer :: res

  if (foo(27) .ne. 27) stop 1
  if (foo(16, 18) .ne. 288) stop 2
contains
  function foo(x, y)
    integer, value :: x
    integer, value, optional :: y
    integer :: res, foo

    !$acc data copyin(x, y) copyout(res)
    !$acc parallel
    res = x
    if (present(y)) then
      res = res * y
    end if
    !$acc end parallel
    !$acc end data

    foo = res
  end function foo
end program test
