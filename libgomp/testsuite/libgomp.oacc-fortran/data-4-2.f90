! Copy of data-4.f90 with self exchanged with host for !acc update, and with
! default (present) clauses added.

! { dg-do run }

program asyncwait
  real, allocatable :: a(:), b(:), c(:), d(:), e(:)
  integer i, N

  N = 64

  allocate (a(N))
  allocate (b(N))
  allocate (c(N))
  allocate (d(N))
  allocate (e(N))

  a(:) = 3.0
  b(:) = 0.0

  !$acc enter data copyin (a(1:N)) copyin (b(1:N)) copyin (N) async

  !$acc parallel default (present) async wait
  !$acc loop
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel

  !$acc update self (a(1:N), b(1:N)) async wait
  !$acc wait

  do i = 1, N
     if (a(i) .ne. 3.0) STOP 1
     if (b(i) .ne. 3.0) STOP 2
  end do

  a(:) = 2.0
  b(:) = 0.0

  !$acc update device (a(1:N), b(1:N)) async (1)

  !$acc parallel default (present) async (1) wait (1)
  !$acc loop
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel

  !$acc update host (a(1:N), b(1:N)) async (1) wait (1)
  !$acc wait (1)

  do i = 1, N
     if (a(i) .ne. 2.0) STOP 3
     if (b(i) .ne. 2.0) STOP 4
  end do

  a(:) = 3.0
  b(:) = 0.0
  c(:) = 0.0
  d(:) = 0.0

  !$acc enter data copyin (c(1:N), d(1:N)) async (1)
  !$acc update device (a(1:N), b(1:N)) async (1)

  !$acc parallel default (present) async (1)
  do i = 1, N
     b(i) = (a(i) * a(i) * a(i)) / a(i)
  end do
  !$acc end parallel

  !$acc parallel default (present) async (1)
  do i = 1, N
     c(i) = (a(i) * 4) / a(i)
  end do
  !$acc end parallel

  !$acc parallel default (present) async (1)
  do i = 1, N
     d(i) = ((a(i) * a(i)  + a(i)) / a(i)) - a(i)
  end do
  !$acc end parallel

  !$acc update self (a(1:N), b(1:N), c(1:N), d(1:N)) async (1) wait (1)

  !$acc wait (1)

  do i = 1, N
     if (a(i) .ne. 3.0) STOP 5
     if (b(i) .ne. 9.0) STOP 6
     if (c(i) .ne. 4.0) STOP 7
     if (d(i) .ne. 1.0) STOP 8
  end do

  a(:) = 2.0
  b(:) = 0.0
  c(:) = 0.0
  d(:) = 0.0
  e(:) = 0.0

  !$acc enter data copyin (e(1:N)) async (1)
  !$acc update device (a(1:N), b(1:N), c(1:N), d(1:N)) async (1)

  !$acc parallel default (present) async (1)
  do i = 1, N
     b(i) = (a(i) * a(i) * a(i)) / a(i)
  end do
  !$acc end parallel

  !$acc parallel default (present) async (1)
  do i = 1, N
     c(i) = (a(i) * 4) / a(i)
  end do
  !$acc end parallel

  !$acc parallel default (present) async (1)
  do i = 1, N
     d(i) = ((a(i) * a(i) + a(i)) / a(i)) - a(i)
  end do
  !$acc end parallel

  !$acc parallel default (present) wait (1) async (1)
  do i = 1, N
     e(i) = a(i) + b(i) + c(i) + d(i)
  end do
  !$acc end parallel

  !$acc update self (a(1:N), b(1:N), c(1:N), d(1:N), e(1:N)) async (1) wait (1)
  !$acc wait (1)
  !$acc exit data delete (N, a(1:N), b(1:N), c(1:N), d(1:N), e(1:N))

  do i = 1, N
     if (a(i) .ne. 2.0) STOP 9
     if (b(i) .ne. 4.0) STOP 10
     if (c(i) .ne. 4.0) STOP 11
     if (d(i) .ne. 1.0) STOP 12
     if (e(i) .ne. 11.0) STOP 13
  end do
end program asyncwait
