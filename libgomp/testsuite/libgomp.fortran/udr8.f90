! { dg-do run }

module udr8m1
  integer, parameter :: a = 6
  integer :: b
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in)
!$omp declare reduction (.add. : integer : &
!$omp & omp_out = omp_out .add. iand (omp_in, -4)) &
!$omp & initializer (omp_priv = 3)
  interface operator (.add.)
    module procedure f1
  end interface
contains
  integer function f1 (x, y)
    integer, intent (in) :: x, y
    f1 = x + y
  end function f1
end module udr8m1
module udr8m2
  use udr8m1
  type dt
    integer :: x
  end type
!$omp declare reduction (+ : dt : omp_out = omp_out + omp_in) &
!$omp & initializer (omp_priv = dt (0))
  interface operator (+)
    module procedure f2
  end interface
contains
  type(dt) function f2 (x, y)
    type(dt), intent (in) :: x, y
    f2%x = x%x + y%x
  end function f2
end module udr8m2
  use udr8m2
  integer :: i, j
  type(dt) :: d
  j = 3
  d%x = 0
!$omp parallel do reduction (.add.: j) reduction (+ : d)
  do i = 1, 100
    j = j.add.iand (i, -4)
    d = d + dt(i)
  end do
  if (d%x /= 5050 .or. j /= 4903) STOP 1
end
