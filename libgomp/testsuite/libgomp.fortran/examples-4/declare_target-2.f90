! { dg-do run }

program e_53_2
  !$omp declare target (fib)
  integer :: x, fib
  !$omp target map(from: x)
    x = fib (25)
  !$omp end target
  if (x /= fib (25)) call abort
end program

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
