! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

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

  call acc_copyin (h)

  do i = 1, N
    h(i) = i + i
  end do 

  call acc_update_device (h, sizeof (h))

  if (acc_is_present (h) .neqv. .TRUE.) stop 1

  h(:) = 0

  call acc_copyout (h, sizeof (h))

  do i = 1, N
    if (h(i) /= i + i) stop 2
  end do 

  call acc_copyin (h, sizeof (h))

  h(:) = 0

  call acc_update_self (h, sizeof (h))
  
  if (acc_is_present (h) .neqv. .TRUE.) stop 3

  do i = 1, N
    if (h(i) /= i + i) stop 4
  end do 

  call acc_delete (h)

  if (acc_is_present (h) .neqv. .FALSE.) stop 5
  
end program
