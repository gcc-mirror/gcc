! { dg-do run }
! { dg-additional-options "-cpp" }

#define N (1024 * 512)

subroutine foo (a,  b,  c)
  integer, parameter :: n = N
  integer, dimension (n) :: a
  integer, dimension (n) :: b
  integer, dimension (n) :: c
  integer i, ii

  do i = 1, n
    a(i) = i * 2;
  end do

  do i = 1, n
    b(i) = i * 4;
  end do

  !$acc kernels copyin (a(1:n), b(1:n)) copyout (c(1:n))
    !$acc loop independent
    do ii = 1, n
      c(ii) = a(ii) + b(ii)
    end do
  !$acc end kernels

  do i = 1, n
    if (c(i) .ne. a(i) + b(i)) call abort
  end do

end subroutine

program main
  integer, parameter :: n = N
  integer :: a(n)
  integer :: b(n)
  integer :: c(n)

  call foo (a, b, c)

end program main
