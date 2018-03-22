! { dg-do run }

program firstprivate
  integer, parameter :: Nupper=100
  integer :: a, b(Nupper), c, d, n
  include "openacc_lib.h"

  if (acc_get_device_type () .ne. acc_device_host) then
     n = Nupper
  else
     n = 1
  end if

  b(:) = -1
  a = 5

  !$acc parallel firstprivate (a) num_gangs (n)
  !$acc loop gang
  do i = 1, n
     a = a + i
     b(i) = a
  end do
  !$acc end parallel

  do i = 1, n
     if (b(i) .ne. i + a) call abort ()
  end do

  !$acc data copy (a)
  !$acc parallel firstprivate (a) copyout (c)
  a = 10
  c = a
  !$acc end parallel

  !$acc parallel copyout (d) present (a)
  d = a
  !$acc end parallel
  !$acc end data

  if (c .ne. 10) call abort ()
  if (d .ne. 5) call abort ()
end program firstprivate
