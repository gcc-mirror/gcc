! Exercise the data movement runtime library functions on non-shared memory
! targets.

! { dg-do run { target openacc_nvidia_accel_selected } }

program main
  use openacc
  implicit none

  integer, parameter :: N = 256
  integer, allocatable :: h(:)
  integer :: i

  allocate (h(N))

  do i = 1, N
    h(i) = i
  end do 

  call acc_present_or_copyin (h)

  if (acc_is_present (h) .neqv. .TRUE.) stop 1

  call acc_copyout (h)

  if (acc_is_present (h) .neqv. .FALSE.) stop 1

  do i = 1, N
    if (h(i) /= i) stop 1
  end do

  do i = 1, N
    h(i) = i + i
  end do 

  call acc_pcopyin (h, sizeof (h))

  if (acc_is_present (h) .neqv. .TRUE.) stop 1

  call acc_copyout (h)

  if (acc_is_present (h) .neqv. .FALSE.) stop 1

  do i = 1, N
    if (h(i) /= i + i) stop 1
  end do

  call acc_create (h)

  if (acc_is_present (h) .neqv. .TRUE.) stop 1

  !$acc parallel loop
    do i = 1, N
      h(i) = i
    end do
  !$end acc parallel

  call acc_copyout (h)

  if (acc_is_present (h) .neqv. .FALSE.) stop 1

  do i = 1, N
    if (h(i) /= i) stop 1
  end do

  call acc_present_or_create (h, sizeof (h))

  if (acc_is_present (h) .neqv. .TRUE.) stop 1

  call acc_delete (h)

  if (acc_is_present (h) .neqv. .FALSE.) stop 1

  call acc_pcreate (h)

  if (acc_is_present (h) .neqv. .TRUE.) stop 1

  call acc_delete (h)

  if (acc_is_present (h) .neqv. .FALSE.) stop 1

end program
