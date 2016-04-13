! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

program main
  use openacc
  implicit none

  integer, parameter :: N = 32
  real, allocatable :: a(:), b(:), c(:)
  integer i

  i = 0

  allocate (a(N))
  allocate (b(N))
  allocate (c(N))

  a(:) = 3.0
  b(:) = 0.0

  !$acc parallel copyin (a(1:N)) copyout (b(1:N))
    do i = 1, N
      b(i) = a(i)
    end do
  !$acc end parallel

  do i = 1, N
    if (b(i) .ne. 3.0) call abort
  end do

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort

  a(:) = 5.0
  b(:) = 1.0

  !$acc parallel copyin (a(1:N)) copyout (b(1:N))
    do i = 1, N
      b(i) = a(i)
    end do
  !$acc end parallel

  do i = 1, N
    if (b(i) .ne. 5.0) call abort
  end do

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort

  a(:) = 6.0
  b(:) = 0.0

  call acc_copyin (a, sizeof (a))

  a(:) = 9.0

  !$acc parallel present_or_copyin (a(1:N)) copyout (b(1:N))
    do i = 1, N
      b(i) = a(i)
    end do
  !$acc end parallel

  do i = 1, N
    if (b(i) .ne. 6.0) call abort
  end do

  call acc_copyout (a, sizeof (a))

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort

  a(:) = 6.0
  b(:) = 0.0

  !$acc parallel copyin (a(1:N)) present_or_copyout (b(1:N))
     do i = 1, N
       b(i) = a(i)
     end do
  !$acc end parallel

  do i = 1, N
     if (b(i) .ne. 6.0) call abort
  end do

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort

  a(:) = 5.0
  b(:) = 2.0

  call acc_copyin (b, sizeof (b))

  !$acc parallel copyin (a(1:N)) present_or_copyout (b(1:N))
     do i = 1, N
       b(i) = a(i)
     end do
  !$acc end parallel

  do i = 1, N
    if (a(i) .ne. 5.0) call abort
    if (b(i) .ne. 2.0) call abort
  end do

  call acc_copyout (b, sizeof (b))

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort

  a(:) = 3.0;
  b(:) = 4.0;

  !$acc parallel copy (a(1:N)) copyout (b(1:N))
    do i = 1, N
      a(i) = a(i) + 1
      b(i) = a(i) + 2
    end do
  !$acc end parallel

  do i = 1, N
    if (a(i) .ne. 4.0) call abort
    if (b(i) .ne. 6.0) call abort
  end do

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort

  a(:) = 4.0
  b(:) = 7.0

  !$acc parallel present_or_copy (a(1:N)) present_or_copy (b(1:N))
    do i = 1, N
      a(i) = a(i) + 1
      b(i) = b(i) + 2
    end do
  !$acc end parallel

  do i = 1, N
    if (a(i) .ne. 5.0) call abort
    if (b(i) .ne. 9.0) call abort
  end do

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort

  a(:) = 3.0
  b(:) = 7.0

  call acc_copyin (a, sizeof (a))
  call acc_copyin (b, sizeof (b))

  !$acc parallel present_or_copy (a(1:N)) present_or_copy (b(1:N))
    do i = 1, N
      a(i) = a(i) + 1
      b(i) = b(i) + 2
    end do
  !$acc end parallel

  do i = 1, N
    if (a(i) .ne. 3.0) call abort
    if (b(i) .ne. 7.0) call abort
  end do

  call acc_copyout (a, sizeof (a))
  call acc_copyout (b, sizeof (b))

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort

  a(:) = 3.0
  b(:) = 7.0

  !$acc parallel copyin (a(1:N)) create (c(1:N)) copyout (b(1:N))
    do i = 1, N
      c(i) = a(i)
      b(i) = c(i)
    end do
  !$acc end parallel

  do i = 1, N
    if (a(i) .ne. 3.0) call abort
    if (b(i) .ne. 3.0) call abort
  end do

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort
  if (acc_is_present (c) .eqv. .TRUE.) call abort

  a(:) = 4.0
  b(:) = 8.0

  !$acc parallel copyin (a(1:N)) present_or_create (c(1:N)) copyout (b(1:N))
    do i = 1, N
      c(i) = a(i)
      b(i) = c(i)
    end do
  !$acc end parallel

  do i = 1, N
    if (a(i) .ne. 4.0) call abort
    if (b(i) .ne. 4.0) call abort
  end do

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort
  if (acc_is_present (c) .eqv. .TRUE.) call abort

  a(:) = 4.0

  call acc_copyin (a, sizeof (a))
  call acc_copyin (b, sizeof (b))
  call acc_copyin (c, sizeof (c))

  !$acc parallel present (a(1:N)) present (c(1:N)) present (b(1:N))
    do i = 1, N
      c(i) = a(i)
      b(i) = c(i)
    end do
  !$acc end parallel

  call acc_copyout (a, sizeof (a))
  call acc_copyout (b, sizeof (b))
  call acc_copyout (c, sizeof (c))
  
  do i = 1, N
    if (a(i) .ne. 4.0) call abort
    if (b(i) .ne. 4.0) call abort
  end do

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort
  if (acc_is_present (c) .eqv. .TRUE.) call abort

  a(:) = 6.0
  b(:) = 0.0

  call acc_copyin (a, sizeof (a))

  a(:) = 9.0

  !$acc parallel pcopyin (a(1:N)) copyout (b(1:N))
    do i = 1, N
      b(i) = a(i)
    end do
  !$acc end parallel

  do i = 1, N
    if (b(i) .ne. 6.0) call abort
  end do
  
  call acc_copyout (a, sizeof (a))

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort

  a(:) = 6.0
  b(:) = 0.0

  !$acc parallel copyin (a(1:N)) pcopyout (b(1:N))
   do i = 1, N
     b(i) = a(i)
   end do
  !$acc end parallel

  do i = 1, N
    if (b(i) .ne. 6.0) call abort
  end do

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort

  a(:) = 5.0
  b(:) = 7.0

  !$acc parallel copyin (a(1:N)) pcreate (c(1:N)) copyout (b(1:N))
    do i = 1, N
      c(i) = a(i)
      b(i) = c(i)
    end do
  !$acc end parallel

  do i = 1, N
    if (a(i) .ne. 5.0) call abort
    if (b(i) .ne. 5.0) call abort
  end do

  if (acc_is_present (a) .eqv. .TRUE.) call abort
  if (acc_is_present (b) .eqv. .TRUE.) call abort
  if (acc_is_present (c) .eqv. .TRUE.) call abort

end program main
