! { dg-do run }

program asyncwait
  integer, parameter :: N = 64
  real, allocatable :: a(:), b(:), c(:)
  integer i

  allocate (a(N))
  allocate (b(N))
  allocate (c(N))

  !$acc parallel async (0)
  !$acc loop
  do i = 1, N
    a(i) = 1
  end do
  !$acc end parallel

  !$acc parallel async (1)
  !$acc loop
  do i = 1, N
    b(i) = 1
  end do
  !$acc end parallel

  !$acc parallel wait (0, 1)
  !$acc loop
  do i = 1, N
    c(i) = a(i) + b(i)
  end do
  !$acc end parallel

  do i = 1, N
    if (c(i) .ne. 2.0) STOP 1
  end do

  !$acc kernels async (0)
  !$acc loop
  do i = 1, N
    a(i) = 1
  end do
  !$acc end kernels

  !$acc kernels async (1)
  !$acc loop
  do i = 1, N
    b(i) = 1
  end do
  !$acc end kernels

  !$acc kernels wait (0, 1)
  !$acc loop
  do i = 1, N
    c(i) = a(i) + b(i)
  end do
  !$acc end kernels

  do i = 1, N
    if (c(i) .ne. 2.0) STOP 2
  end do
  
  deallocate (a)
  deallocate (b)
  deallocate (c)
end program asyncwait
