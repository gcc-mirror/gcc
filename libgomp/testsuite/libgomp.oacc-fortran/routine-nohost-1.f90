! Test 'nohost' clause via 'acc_on_device'.

! { dg-do run }

! With optimizations disabled, we currently don't expect that 'acc_on_device' "evaluates at compile time to a constant".
! { dg-skip-if "TODO PR82391" { *-*-* } { "-O0" } }

! { dg-additional-options "-fdump-tree-oaccloops" }

program main
  use openacc
  implicit none
  integer, parameter :: n = 10
  integer :: a(n), i
  integer, external :: fact_nohost
  !$acc routine (fact_nohost)
  integer, external :: fact

  !$acc parallel loop
  do i = 1, n
     if (acc_on_device(acc_device_not_host)) then
        a(i) = fact_nohost(i)
     else
        a(i) = 0
     end if
  end do
  !$acc end parallel loop

  do i = 1, n
     if (acc_get_device_type() .eq. acc_device_host) then
        if (a(i) .ne. 0) stop 10 + i
     else
        if (a(i) .ne. fact(i)) stop 20 + i
     end if
  end do
end program main

recursive function fact(x) result(res)
  implicit none
  !$acc routine (fact)
  integer, intent(in) :: x
  integer :: res

  if (x < 1) then
     res = 1
  else
     res = x * fact(x - 1)
  end if
end function fact

function fact_nohost(x) result(res)
  use openacc
  implicit none
  !$acc routine (fact_nohost) nohost
  integer, intent(in) :: x
  integer :: res
  integer, external :: fact

  res = fact(x)
end function fact_nohost
! { dg-final { scan-tree-dump-times {(?n)^OpenACC routine 'fact_nohost' has 'nohost' clause\.$} 1 oaccloops { target { ! offloading_enabled } } } }
! { dg-final { scan-tree-dump-times {(?n)^OpenACC routine 'fact_nohost_' has 'nohost' clause\.$} 1 oaccloops { target offloading_enabled } } }
!TODO See PR101551 for 'offloading_enabled' differences.
