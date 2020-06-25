! { dg-do run }
!
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
     end if
  end do
  !$acc end parallel
  if (any (c_a /= [(i, i=1, 10)])) stop 1
  !print *, a
end program main
