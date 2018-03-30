! { dg-do run }
! { dg-additional-options "-O2" }
! { dg-additional-options "-ftree-parallelize-loops=2" }

! Constant bound, vector addition.

subroutine foo ()
  integer, parameter :: n = 1000
  integer, dimension (0:n-1) :: a, b, c
  common a, b, c
  integer :: ii

  do ii = 0, n - 1
     c(ii) = a(ii) + b(ii)
  end do
end subroutine foo

program main
  integer, parameter :: n = 1000
  integer, parameter :: distrib = 10
  integer, dimension (0:n-1) :: a, b, c
  common a, b, c
  integer :: i, j, k

  do j = 0, ((n / distrib) -1)
     do i = 0, distrib - 1
	k = i + (distrib * j)
	a(k) = k
	b(k) = MODULO ((k * 3), 7)
	c(k) = k * 2;
     end do
  end do

  call foo ()

  do i = 0, n - 1
     if (c(i) .ne. (i + MODULO ((i * 3), 7))) STOP 1
  end do

end program
