! Test declare create with allocatable scalars.

! { dg-do run }

program main
  use openacc
  implicit none
  integer, parameter :: n = 100
  integer, allocatable :: a, c
  integer :: i, b(n)
  !$acc declare create (c)

  allocate (a)

  a = 50

  !$acc parallel loop firstprivate(a)
  do i = 1, n;
     b(i) = a
  end do

  do i = 1, n
     if (b(i) /= a) stop 1
  end do

  allocate (c)
  a = 100

  if (.not.acc_is_present(c)) stop 2

  !$acc parallel num_gangs(1) present(c)
  c = a
  !$acc end parallel

  !$acc update host(c)
  if (c /= a) stop 3

  !$acc parallel loop
  do i = 1, n
     b(i) = c
  end do

  do i = 1, n
     if (b(i) /= a) stop 4
  end do

  deallocate (a, c)
end program main
