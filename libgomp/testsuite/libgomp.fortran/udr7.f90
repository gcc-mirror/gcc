! { dg-do run }

program udr7
  implicit none
  interface
    elemental subroutine omp_priv (x, y, z)
      real, intent (in) :: x
      real, intent (inout) :: y
      real, intent (in) :: z
    end subroutine omp_priv
    elemental real function omp_orig (x)
      real, intent (in) :: x
    end function omp_orig
  end interface
!$omp declare reduction (omp_priv : real : &
!$omp & omp_priv (omp_orig (omp_in), omp_out, 1.0)) &
!$omp & initializer (omp_out (omp_priv, omp_in (omp_orig)))
  real :: x (2:4, 1:1, -2:0)
  integer :: i
  x = 0
!$omp parallel do reduction (omp_priv : x)
  do i = 1, 64
    x = x + i
  end do
  if (any (x /= 2080.0)) STOP 1
contains
  elemental subroutine omp_out (x, y)
    real, intent (out) :: x
    real, intent (in) :: y
    x = y - 4.0
  end subroutine omp_out
  elemental real function omp_in (x)
    real, intent (in) :: x
    omp_in = x + 4.0
  end function omp_in
end program udr7
elemental subroutine omp_priv (x, y, z)
  real, intent (in) :: x
  real, intent (inout) :: y
  real, intent (in) :: z
  y = y + (x - 4.0) + (z - 1.0)
end subroutine omp_priv
elemental real function omp_orig (x)
  real, intent (in) :: x
  omp_orig = x + 4.0
end function omp_orig
