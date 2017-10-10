! { dg-do run }
! { dg-additional-options "-w" }

program reduction
  implicit none
  integer, parameter :: n = 10
  integer s1, s2
  include "openacc_lib.h"

  s1 = 0
  s2 = 0

  !$acc parallel reduction(+:s1,s2) num_gangs (n) copy(s1)
  s1 = s1 + 1
  s2 = s2 + 1
  !$acc end parallel

  if (acc_get_device_type () .ne. acc_device_host) then
     if (s1 .ne. n) call abort
     if (s2 .ne. n) call abort
  else
     if (s1 .ne. 1) call abort
     if (s2 .ne. 1) call abort
  end if

  ! Test reductions inside subroutines

  s1 = 0
  s2 = 0
  call redsub (s1, s2, n)

  if (acc_get_device_type () .ne. acc_device_host) then
     if (s1 .ne. n) call abort
  else
     if (s2 .ne. 1) call abort
  end if
end program reduction

subroutine redsub(s1, s2, n)
  implicit none
  integer :: s1, s2, n

  !$acc parallel reduction(+:s1,s2) num_gangs (10)  copy(s1)
  s1 = s1 + 1
  s2 = s2 + 1
  !$acc end parallel
end subroutine redsub
