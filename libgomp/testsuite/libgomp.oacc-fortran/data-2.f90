! { dg-do run }

program test
  integer, parameter :: N = 8
  real, allocatable :: a(:,:), b(:,:)

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
      if (a(j,i) .ne. 3.0) call abort
      if (b(j,i) .ne. 3.0) call abort
    end do
  end do
end program test
