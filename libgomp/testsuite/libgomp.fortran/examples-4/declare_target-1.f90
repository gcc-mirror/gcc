! { dg-do run }

module e_53_1_mod
  integer :: THRESHOLD = 20
contains
  integer recursive function fib (n) result (f)
    !$omp declare target
    integer :: n
    if (n <= 0) then
      f = 0
    else if (n == 1) then
      f = 1
    else
      f = fib (n - 1) + fib (n - 2)
    end if
  end function

  integer function fib_wrapper (n)
    integer :: x
    !$omp target map(to: n) map(from: x) if(n > THRESHOLD)
      x = fib (n)
    !$omp end target
    fib_wrapper = x
  end function
end module

program e_53_1
  use e_53_1_mod, only : fib, fib_wrapper
  if (fib (15) /= fib_wrapper (15)) stop 1
  ! Reduced from 25 to 23, otherwise execution runs out of thread stack on
  ! Nvidia Titan V.
  if (fib (23) /= fib_wrapper (23)) stop 2
end program
