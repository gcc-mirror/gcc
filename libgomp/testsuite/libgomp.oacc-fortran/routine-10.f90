! { dg-do run }

! { dg-additional-options -Wuninitialized }

module m
  implicit none
contains
  pure subroutine add_ps_routine(a, b, c)
    implicit none
    !$acc routine seq
    integer, intent(in)  :: a, b
    integer, intent(out) :: c
    integer, parameter :: n = 10
    integer :: i

    do i = 1, n
       if (i .eq. 5) then
          c = a + b
       end if
    end do
  end subroutine add_ps_routine

  elemental impure function add_ef(a, b) result(c)
    implicit none
    !$acc routine
    integer, intent(in)  :: a, b
    integer :: c

    call add_ps_routine(a, b, c)
  end function add_ef
  ! This '-Wmaybe-uninitialized' diagnostic appears for '-O2' only; PR102192.
  ! { dg-xfail-if PR102192 { *-*-* } { -O2 } }
  ! There's another instance (again '-O2' only) further down, but as any number
  ! of 'dg-xfail-if' only apply to the first 'dg-bogus' etc., we have no way to
  ! XFAIL that other one, so we instead match all of them here (via line '0'):
  ! { dg-bogus {'c' may be used uninitialized} {} { target *-*-* } 0 }
  ! { TODO_dg-bogus {'c' may be used uninitialized} {} { target *-*-* } .-7 }
end module m

program main
  use m
  implicit none
  integer, parameter :: n = 10
  integer, dimension(n) :: a_a
  integer, dimension(n) :: b_a
  integer, dimension(n) :: c_a
  integer :: i

  a_a = [(3 * i, i = 1, n)]
  b_a = [(-2 * i, i = 1, n)]
  !$acc parallel copyin(a_a, b_a) copyout(c_a)
  !$acc loop gang
  do i = 1, n
     if (i .eq. 4) then
        c_a = add_ef(a_a, b_a)
        ! See above.
        ! { TODO_dg-xfail-if PR102192 { *-*-* } { -O2 } }
        ! { TODO_dg-bogus {'c' may be used uninitialized} {} { target *-*-* } .-3 }
     end if
  end do
  !$acc end parallel
  if (any (c_a /= [(i, i=1, 10)])) stop 1
  !print *, a
end program main
