! { dg-do run }

program asyncwait
  integer, parameter :: N = 64
  real, allocatable :: a(:), b(:), c(:), d(:), e(:)
  integer i

  allocate (a(N))
  allocate (b(N))
  allocate (c(N))
  allocate (d(N))
  allocate (e(N))

  a(:) = 3.0
  b(:) = 0.0

  !$acc data copy (a(1:N)) copy (b(1:N))

  !$acc parallel async
  !$acc loop
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel

  !$acc wait
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 3.0) call abort
     if (b(i) .ne. 3.0) call abort
  end do

  a(:) = 2.0
  b(:) = 0.0

  !$acc data copy (a(1:N)) copy (b(1:N))

  !$acc parallel async (1)
  !$acc loop
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel

  !$acc wait (1)
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 2.0) call abort
     if (b(i) .ne. 2.0) call abort
  end do

  a(:) = 3.0
  b(:) = 0.0
  c(:) = 0.0
  d(:) = 0.0

  !$acc data copy (a(1:N)) copy (b(1:N)) copy (c(1:N)) copy (d(1:N))

  !$acc parallel async (1)
  do i = 1, N
     b(i) = (a(i) * a(i) * a(i)) / a(i)
  end do
  !$acc end parallel

  !$acc parallel async (1)
  do i = 1, N
     c(i) = (a(i) * 4) / a(i)
  end do
  !$acc end parallel

  !$acc parallel async (1)
  !$acc loop
  do i = 1, N
     d(i) = ((a(i) * a(i) + a(i)) / a(i)) - a(i)
  end do
  !$acc end parallel

  !$acc wait (1)
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 3.0) call abort
     if (b(i) .ne. 9.0) call abort
     if (c(i) .ne. 4.0) call abort
     if (d(i) .ne. 1.0) call abort
  end do

  a(:) = 2.0
  b(:) = 0.0
  c(:) = 0.0
  d(:) = 0.0
  e(:) = 0.0

  !$acc data copy (a(1:N), b(1:N), c(1:N), d(1:N), e(1:N))

  !$acc parallel async (1)
  do i = 1, N
     b(i) = (a(i) * a(i) * a(i)) / a(i)
  end do
  !$acc end parallel

  !$acc parallel async (1)
  !$acc loop
  do i = 1, N
     c(i) = (a(i) * 4) / a(i)
  end do
  !$acc end parallel

  !$acc parallel async (1)
  !$acc loop
  do i = 1, N
     d(i) = ((a(i) * a(i) + a(i)) / a(i)) - a(i)
  end do
  !$acc end parallel

  !$acc parallel wait (1) async (1)
  !$acc loop
  do i = 1, N
     e(i) = a(i) + b(i) + c(i) + d(i)
  end do
  !$acc end parallel

  !$acc wait (1)
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 2.0) call abort
     if (b(i) .ne. 4.0) call abort
     if (c(i) .ne. 4.0) call abort
     if (d(i) .ne. 1.0) call abort
     if (e(i) .ne. 11.0) call abort
  end do

  a(:) = 3.0
  b(:) = 0.0

  !$acc data copy (a(1:N)) copy (b(1:N))

  !$acc kernels async
  !$acc loop
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end kernels

  !$acc wait
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 3.0) call abort
     if (b(i) .ne. 3.0) call abort
  end do

  a(:) = 2.0
  b(:) = 0.0

  !$acc data copy (a(1:N)) copy (b(1:N))

  !$acc kernels async (1)
  !$acc loop
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end kernels

  !$acc wait (1)
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 2.0) call abort
     if (b(i) .ne. 2.0) call abort
  end do

  a(:) = 3.0
  b(:) = 0.0
  c(:) = 0.0
  d(:) = 0.0

  !$acc data copy (a(1:N)) copy (b(1:N)) copy (c(1:N)) copy (d(1:N))

  !$acc kernels async (1)
  do i = 1, N
     b(i) = (a(i) * a(i) * a(i)) / a(i)
  end do
  !$acc end kernels

  !$acc kernels async (1)
  do i = 1, N
     c(i) = (a(i) * 4) / a(i)
  end do
  !$acc end kernels

  !$acc kernels async (1)
  !$acc loop
  do i = 1, N
     d(i) = ((a(i) * a(i) + a(i)) / a(i)) - a(i)
  end do
  !$acc end kernels

  !$acc wait (1)
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 3.0) call abort
     if (b(i) .ne. 9.0) call abort
     if (c(i) .ne. 4.0) call abort
     if (d(i) .ne. 1.0) call abort
  end do

  a(:) = 2.0
  b(:) = 0.0
  c(:) = 0.0
  d(:) = 0.0
  e(:) = 0.0

  !$acc data copy (a(1:N), b(1:N), c(1:N), d(1:N), e(1:N))

  !$acc kernels async (1)
  do i = 1, N
     b(i) = (a(i) * a(i) * a(i)) / a(i)
  end do
  !$acc end kernels

  !$acc kernels async (1)
  !$acc loop
  do i = 1, N
     c(i) = (a(i) * 4) / a(i)
  end do
  !$acc end kernels

  !$acc kernels async (1)
  !$acc loop
  do i = 1, N
     d(i) = ((a(i) * a(i) + a(i)) / a(i)) - a(i)
  end do
  !$acc end kernels

  !$acc kernels wait (1) async (1)
  !$acc loop
  do i = 1, N
     e(i) = a(i) + b(i) + c(i) + d(i)
  end do
  !$acc end kernels

  !$acc wait (1)
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 2.0) call abort
     if (b(i) .ne. 4.0) call abort
     if (c(i) .ne. 4.0) call abort
     if (d(i) .ne. 1.0) call abort
     if (e(i) .ne. 11.0) call abort
  end do
end program asyncwait
