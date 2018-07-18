! { dg-do run }

module udr9m1
  integer, parameter :: a = 6
  integer :: b
!$omp declare reduction (foo : integer : combiner1 (omp_out, omp_in)) &
!$omp & initializer (initializer1 (omp_priv, omp_orig))
!$omp declare reduction (.add. : integer : &
!$omp & combiner1 (omp_out, omp_in)) &
!$omp & initializer (initializer1 (omp_priv, omp_orig))
  interface operator (.add.)
    module procedure f1
  end interface
contains
  integer function f1 (x, y)
    integer, intent (in) :: x, y
    f1 = x + y
  end function f1
  elemental subroutine combiner1 (x, y)
    integer, intent (inout) :: x
    integer, intent (in) :: y
    x = x + iand (y, -4)
  end subroutine
  subroutine initializer1 (x, y)
    integer :: x, y
    if (y .ne. 3) STOP 1
    x = y
  end subroutine
end module udr9m1
module udr9m2
  use udr9m1
  type dt
    integer :: x
  end type
!$omp declare reduction (+ : dt : combiner2 (omp_in, omp_out)) &
!$omp & initializer (initializer2 (omp_priv))
  interface operator (+)
    module procedure f2
  end interface
contains
  type(dt) function f2 (x, y)
    type(dt), intent (in) :: x, y
    f2%x = x%x + y%x
  end function f2
  subroutine combiner2 (x, y)
    type(dt) :: x, y
    y = y + x
  end subroutine combiner2
  subroutine initializer2 (x)
    type(dt), intent(out) :: x
    x%x = 0
  end subroutine initializer2
end module udr9m2
  use udr9m2
  integer :: i, j
  type(dt) :: d
  j = 3
  d%x = 0
!$omp parallel do reduction (.add.: j) reduction (+ : d)
  do i = 1, 100
    j = j.add.iand (i, -4)
    d = d + dt(i)
  end do
  if (d%x /= 5050 .or. j /= 4903) STOP 2
end
