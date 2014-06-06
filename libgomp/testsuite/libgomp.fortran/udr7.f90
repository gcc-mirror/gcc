! { dg-do run }

program udr7
  implicit none
  interface
    subroutine omp_priv (x, y, z)
      real, intent (in) :: x
      real, intent (inout) :: y
      real, intent (in) :: z(:)
    end subroutine omp_priv
    real function omp_orig (x)
      real, intent (in) :: x
    end function omp_orig
  end interface
!$omp declare reduction (omp_priv : real : &
!$omp & omp_priv (omp_orig (omp_in), omp_out, (/ 1.0, 2.0, 3.0 /))) &
!$omp & initializer (omp_out (omp_priv, omp_in (omp_orig)))
  real :: x (2:4, 1:1, -2:0)
  integer :: i
  x = 0
!$omp parallel do reduction (omp_priv : x)
  do i = 1, 64
    x = x + i
  end do
  if (any (x /= 2080.0)) call abort
contains
  subroutine omp_out (x, y)
    real, intent (out) :: x
    real, intent (in) :: y
    if (y /= 4.0) call abort
    x = 0.0
  end subroutine omp_out
  real function omp_in (x)
    real, intent (in) :: x
    omp_in = x + 4.0
  end function omp_in
end program udr7
subroutine omp_priv (x, y, z)
  real, intent (in) :: x
  real, intent (inout) :: y
  real, intent (in) :: z(:)
  if (any (z .ne. (/ 1.0, 2.0, 3.0 /))) call abort
  y = y + (x - 4.0)
end subroutine omp_priv
real function omp_orig (x)
  real, intent (in) :: x
  omp_orig = x + 4.0
end function omp_orig
