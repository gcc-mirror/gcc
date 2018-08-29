! This test exercises combined directives.

! { dg-do run }

program main
  integer, parameter :: n = 32
  real :: a(n), b(n);
  integer :: i

  do i = 1, n
    a(i) = 1.0
    b(i) = 0.0
  end do

  !$acc parallel loop copy (a(1:n)) copy (b(1:n))
  do i = 1, n
    b(i) = 2.0
    a(i) = a(i) + b(i)
  end do

  do i = 1, n
    if (a(i) .ne. 3.0) STOP 1

    if (b(i) .ne. 2.0) STOP 2
  end do

  !$acc kernels loop copy (a(1:n)) copy (b(1:n))
  do i = 1, n
    b(i) = 3.0;
    a(i) = a(i) + b(i)
  end do

  do i = 1, n
    if (a(i) .ne. 6.0) STOP 3

    if (b(i) .ne. 3.0) STOP 4
  end do

end program main
