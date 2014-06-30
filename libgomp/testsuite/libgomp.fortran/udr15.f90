! { dg-do run }

module udr15m1
  integer, parameter :: a = 6
  integer :: b
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in)
!$omp declare reduction (.add. : integer : &
!$omp & omp_out = omp_out .add. f3 (omp_in, -4)) &
!$omp & initializer (s1 (omp_priv, omp_orig))
  interface operator (.add.)
    module procedure f1
  end interface
contains
  integer function f1 (x, y)
    integer, intent (in) :: x, y
    f1 = x + y
  end function f1
  integer function f3 (x, y)
    integer, intent (in) :: x, y
    f3 = iand (x, y)
  end function f3
  subroutine s1 (x, y)
    integer, intent (in) :: y
    integer, intent (out) :: x
    x = 3
  end subroutine s1
end module udr15m1
module udr15m2
  use udr15m1, f4 => f1, f5 => f3, s2 => s1, operator (.addtwo.) => operator (.add.)
  type dt
    integer :: x
  end type
!$omp declare reduction (+ : dt : omp_out = f6 (omp_out + omp_in)) &
!$omp & initializer (s3 (omp_priv))
  interface operator (+)
    module procedure f2
  end interface
contains
  type(dt) function f2 (x, y)
    type(dt), intent (in) :: x, y
    f2%x = x%x + y%x
  end function f2
  type(dt) function f6 (x)
    type(dt), intent (in) :: x
    f6%x = x%x
  end function f6
  subroutine s3 (x)
    type(dt), intent (out) :: x
    x = dt(0)
  end subroutine
end module udr15m2
  use udr15m2, operator (.addthree.) => operator (.addtwo.), &
               f7 => f4, f8 => f6, s4 => s3
  integer :: i, j
  type(dt) :: d
  j = 3
  d%x = 0
!$omp parallel do reduction (.addthree.: j) reduction (+ : d)
  do i = 1, 100
    j = j.addthree.iand (i, -4)
    d = d + dt(i)
  end do
  if (d%x /= 5050 .or. j /= 4903) call abort
end
