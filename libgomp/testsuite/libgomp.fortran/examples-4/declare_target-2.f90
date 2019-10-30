! { dg-do run }

program e_53_2
  !$omp declare target (fib)
  integer :: x, fib
  !$omp target map(from: x)
    ! Reduced from 25 to 23, otherwise execution runs out of thread stack on
    ! Nvidia Titan V.
    x = fib (23)
  !$omp end target
  if (x /= fib (23)) stop 1
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
