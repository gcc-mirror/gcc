! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

program update
  use openacc
  implicit none 
  integer, parameter :: N = 8
  integer, parameter :: NDIV2 = N / 2
  real :: a(N), b(N)
  integer i

  do i = 1, N
    a(i) = 3.0
    b(i) = 0.0
  end do

  !$acc enter data copyin (a, b)

  !$acc parallel present (a, b)
    do i = 1, N
      b(i) = a(i)
    end do
  !$acc end parallel

  !$acc update host (a, b)

  do i = 1, N
    if (a(i) .ne. 3.0) STOP 1
    if (b(i) .ne. 3.0) STOP 2
  end do

  if (acc_is_present (a) .neqv. .TRUE.) STOP 3
  if (acc_is_present (b) .neqv. .TRUE.) STOP 4

  do i = 1, N
    a(i) = 5.0
    b(i) = 1.0
  end do

  !$acc update device (a, b)

  !$acc parallel present (a, b)
    do i = 1, N
      b(i) = a(i)
    end do 
  !$acc end parallel

  !$acc update host (a, b)

  do i = 1, N
    if (a(i) .ne. 5.0) STOP 5
    if (b(i) .ne. 5.0) STOP 6
 end do

  if (acc_is_present (a) .neqv. .TRUE.) STOP 7
  if (acc_is_present (b) .neqv. .TRUE.) STOP 8

  !$acc parallel present (a, b)
  do i = 1, N
    b(i) = a(i)
  end do
  !$acc end parallel

  !$acc update host (a, b)

  do i = 1, N
    if (a(i) .ne. 5.0) STOP 9
    if (b(i) .ne. 5.0) STOP 10
  end do

  if (acc_is_present (a) .neqv. .TRUE.) STOP 11
  if (acc_is_present (b) .neqv. .TRUE.) STOP 12

  do i = 1, N
    a(i) = 6.0
    b(i) = 0.0
  end do

  !$acc update device (a, b)

  do i = 1, N
    a(i) = 9.0
  end do

  !$acc parallel present (a, b)
    do i = 1, N
      b(i) = a(i)
    end do
  !$acc end parallel

  !$acc update host (a, b)

  do i = 1, N
    if (a(i) .ne. 6.0) STOP 13
    if (b(i) .ne. 6.0) STOP 14
  end do

  if (acc_is_present (a) .neqv. .TRUE.) STOP 15
  if (acc_is_present (b) .neqv. .TRUE.) STOP 16

  do i = 1, N
    a(i) = 7.0
    b(i) = 2.0
  end do

  !$acc update device (a, b)

  do i = 1, N
    a(i) = 9.0
  end do

  !$acc parallel present (a, b)
    do i = 1, N
      b(i) = a(i)
    end do
  !$acc end parallel

  !$acc update host (a, b)

  do i = 1, N
    if (a(i) .ne. 7.0) STOP 17
    if (b(i) .ne. 7.0) STOP 18
  end do

  do i = 1, N
    a(i) = 9.0
  end do

  !$acc update device (a)

  !$acc parallel present (a, b)
    do i = 1, N
      b(i) = a(i)
    end do
  !$acc end parallel

  !$acc update host (a, b)

  do i = 1, N
    if (a(i) .ne. 9.0) STOP 19
    if (b(i) .ne. 9.0) STOP 20
  end do

  if (acc_is_present (a) .neqv. .TRUE.) STOP 21
  if (acc_is_present (b) .neqv. .TRUE.) STOP 22

  do i = 1, N
    a(i) = 5.0
  end do

  !$acc update device (a)

  do i = 1, N
    a(i) = 6.0
  end do

  !$acc update device (a(1:NDIV2))

  !$acc parallel present (a, b)
    do i = 1, N
      b(i) = a(i)
    end do
  !$acc end parallel

  !$acc update host (a, b)

  do i = 1, NDIV2
    if (a(i) .ne. 6.0) STOP 23
    if (b(i) .ne. 6.0) STOP 24
  end do

  do i = NDIV2 + 1, N
    if (a(i) .ne. 5.0) STOP 25
    if (b(i) .ne. 5.0) STOP 26
  end do

  if (acc_is_present (a) .neqv. .TRUE.) STOP 27
  if (acc_is_present (b) .neqv. .TRUE.) STOP 28

  do i = 1, N
    a(i) = 0.0
  end do

  !$acc update device (a(1:4))

  !$acc parallel present (a)
    do i = 1, N
      a(i) = a(i) + 1.0
    end do
  !$acc end parallel

  !$acc update host (a(5:N))

  do i = 1, NDIV2
    if (a(i) .ne. 0.0) STOP 29
  end do

  do i = NDIV2 + 1, N
    if (a(i) .ne. 6.0) STOP 30
  end do

  !$acc update host (a(1:4))

  do i = 1, NDIV2
    if (a(i) .ne. 1.0) STOP 31
  end do

  do i = NDIV2 + 1, N
    if (a(i) .ne. 6.0) STOP 32
  end do

  a(3) = 9
  a(4) = 9
  a(5) = 9
  a(6) = 9

  !$acc update device (a(3:6))

  !$acc parallel present (a(1:N))
    do i = 1, N
      a(i) = a(i) + 1.0
    end do
  !$acc end parallel

  !$acc update host (a(3:6))

  do i = 1, 2
    if (a(i) .ne. 1.0) STOP 33
  end do

  do i = 3, 6
    if (a(i) .ne. 10.0) STOP 34
  end do

  do i = 7, N
    if (a(i) .ne. 6.0) STOP 35
  end do

  !$acc exit data delete (a, b)

end program

