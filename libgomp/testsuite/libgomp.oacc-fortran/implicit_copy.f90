! { dg-do run }

integer function test()
  implicit none
  integer, parameter :: n = 10
  real(8), dimension(n) :: a, b, c
  integer i

  do i = 1, n
     a(i) = i
     b(i) = 1
  end do

  !$acc data copyin(a(1:n), b(1:n))
  !$acc parallel loop
  do i = 1, n
     c(i) = a(i) * b(i)
  end do
  !$acc end data

  do i = 1, n
     if (c(i) /= a(i) * b(i)) call abort
  end do
end function test

program main
  implicit none
  integer i, test
  i = test()
end program main
