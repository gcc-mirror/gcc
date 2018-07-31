! { dg-do run }

program test
  use openacc
  integer, parameter :: N = 8
  real, allocatable :: a(:,:), b(:,:)
  real, allocatable :: c(:), d(:)
  integer i, j

  i = 0
  j = 0

  allocate (a(N,N))
  allocate (b(N,N))

  a(:,:) = 3.0
  b(:,:) = 0.0

  !$acc enter data copyin (a(1:N,1:N), b(1:N,1:N))

  !$acc parallel
  do i = 1, n
    do j = 1, n
      b(j,i) = a (j,i)
    end do
  end do
  !$acc end parallel

  !$acc exit data copyout (a(1:N,1:N), b(1:N,1:N))

  do i = 1, n
    do j = 1, n
      if (a(j,i) .ne. 3.0) STOP 1
      if (b(j,i) .ne. 3.0) STOP 2
    end do
  end do

  allocate (c(N))
  allocate (d(N))

  c(:) = 3.0
  d(:) = 0.0

  !$acc enter data copyin (c(1:N)) create (d(1:N)) async
  !$acc wait
  
  !$acc parallel 
    do i = 1, N
      d(i) = c(i) + 1
    end do
  !$acc end parallel

  !$acc exit data copyout (c(1:N), d(1:N)) async
  !$acc wait

  do i = 1, N
    if (d(i) .ne. 4.0) call abort
  end do

  c(:) = 3.0
  d(:) = 0.0

  !$acc enter data copyin (c(1:N)) async
  !$acc enter data create (d(1:N)) wait
  !$acc wait

  !$acc parallel 
    do i = 1, N
      d(i) = c(i) + 1
    end do
  !$acc end parallel
  
  !$acc exit data copyout (d(1:N)) async
  !$acc exit data async
  !$acc wait

  do i = 1, N
    if (d(i) .ne. 4.0) call abort
  end do

end program test
