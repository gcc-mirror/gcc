! Test non-declared allocatable scalars in OpenACC data clauses.

! { dg-do run }

program main
  implicit none
  integer, parameter :: n = 100
  integer, allocatable :: a, c
  integer :: i, b(n)

  allocate (a)

  a = 50

  !$acc parallel loop
  do i = 1, n;
     b(i) = a
  end do

  do i = 1, n
     if (b(i) /= a) stop 1
  end do

  allocate (c)

  !$acc parallel copyout(c) num_gangs(1)
  c = a
  !$acc end parallel

  if (c /= a) stop 2

  deallocate (a, c)
end program main
