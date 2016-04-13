! { dg-do run }

program main
  implicit none
  integer, parameter         :: n = 1024
  integer, dimension (0:n-1) :: a, b, c
  integer                    :: i, ii

  do i = 0, n - 1
     a(i) = i * 2
  end do

  do i = 0, n -1
     b(i) = i * 4
  end do

  !$acc kernels copyin (a(0:n-1), b(0:n-1)) copyout (c(0:n-1))
  do ii = 0, n - 1
     c(ii) = a(ii) + b(ii)
  end do
  !$acc end kernels

  do i = 0, n - 1
     if (c(i) .ne. a(i) + b(i)) call abort
  end do

end program main
