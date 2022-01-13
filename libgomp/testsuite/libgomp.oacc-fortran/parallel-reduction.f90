! { dg-do run }

! { dg-additional-options -Wuninitialized }

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

program reduction
  implicit none
  integer, parameter :: n = 10
  integer s1, s2
  include "openacc_lib.h"

  s1 = 0
  s2 = 0

  !$acc parallel reduction(+:s1,s2) num_gangs (n) copy(s1)
  ! { dg-bogus "\[Ww\]arning: region is gang partitioned but does not contain gang partitioned code" "TODO 'reduction'" { xfail *-*-* } .-1 }
  s1 = s1 + 1
  s2 = s2 + 1
  !$acc end parallel

  if (acc_get_device_type () .ne. acc_device_host) then
     if (s1 .ne. n) STOP 1
     if (s2 .ne. n) STOP 2
  else
     if (s1 .ne. 1) STOP 3
     if (s2 .ne. 1) STOP 4
  end if

  ! Test reductions inside subroutines

  s1 = 0
  s2 = 0
  call redsub (s1, s2, n)

  if (acc_get_device_type () .ne. acc_device_host) then
     if (s1 .ne. n) STOP 5
  else
     if (s2 .ne. 1) STOP 6
  end if
end program reduction

subroutine redsub(s1, s2, n)
  implicit none
  integer :: s1, s2, n

  !$acc parallel reduction(+:s1,s2) num_gangs (10)  copy(s1)
  ! { dg-bogus {'s1\.[0-9]+' is used uninitialized} TODO { xfail *-*-* } .-1 }
  !   { dg-note {'s1\.[0-9]+' was declared here} {} { target *-*-* } .-2 }
  ! { dg-bogus {'s2\.[0-9]+' is used uninitialized} TODO { xfail *-*-* } .-3 }
  !   { dg-note {'s2\.[0-9]+' was declared here} {} { target *-*-* } .-4 }
  ! { dg-bogus "\[Ww\]arning: region is gang partitioned but does not contain gang partitioned code" "TODO 'reduction'" { xfail *-*-* } .-5 }
  s1 = s1 + 1
  s2 = s2 + 1
  !$acc end parallel
end subroutine redsub
