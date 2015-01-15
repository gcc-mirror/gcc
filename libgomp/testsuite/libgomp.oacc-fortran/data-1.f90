! { dg-do run }

program test
  integer, parameter :: N = 8
  real, allocatable :: a(:), b(:)

  allocate (a(N))
  allocate (b(N))

  a(:) = 3.0
  b(:) = 0.0

  !$acc enter data copyin (a(1:N), b(1:N))

  !$acc parallel
  do i = 1, n
    b(i) = a (i)
  end do
  !$acc end parallel

  !$acc exit data copyout (a(1:N), b(1:N))

  do i = 1, n
    if (a(i) .ne. 3.0) call abort
    if (b(i) .ne. 3.0) call abort
  end do

  a(:) = 5.0
  b(:) = 1.0

  !$acc enter data copyin (a(1:N), b(1:N))

  !$acc parallel
  do i = 1, n
    b(i) = a (i)
  end do
  !$acc end parallel

  !$acc exit data copyout (a(1:N), b(1:N))

  do i = 1, n
    if (a(i) .ne. 5.0) call abort
    if (b(i) .ne. 5.0) call abort
  end do
end program test
