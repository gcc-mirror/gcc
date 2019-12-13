! { dg-do run }

  type dt
    integer :: g
    integer, allocatable :: h(:)
  end type
!$omp declare reduction (baz : dt : bar (omp_out, omp_in)) &
!$omp & initializer (foo (omp_priv, omp_orig))
  integer :: r
  type (dt), allocatable :: a(:)
  allocate (a(7:8))
  a(:)%g = 0
  a(7)%h = (/ 0, 0, 0 /)
  r = 0
!$omp parallel reduction(+:r) reduction (baz:a)
  if (.not.allocated (a)) stop 1
  if (lbound (a, 1) /= 7 .or. ubound (a, 1) /= 8) stop 2
  if (.not.allocated (a(7)%h)) stop 3
  if (allocated (a(8)%h)) stop 4
  if (lbound (a(7)%h, 1) /= 1 .or. ubound (a(7)%h, 1) /= 3) stop 5
  a(:)%g = a(:)%g + 2
  a(7)%h = a(7)%h + 3
  r = r + 1
!$omp end parallel
  if (.not.allocated (a)) stop 6
  if (lbound (a, 1) /= 7 .or. ubound (a, 1) /= 8) stop 7
  if (.not.allocated (a(7)%h)) stop 8
  if (allocated (a(8)%h)) stop 9
  if (lbound (a(7)%h, 1) /= 1 .or. ubound (a(7)%h, 1) /= 3) stop 10
  if (any (a(:)%g /= 2 * r) .or. any (a(7)%h(:) /= 3 * r)) stop 11
contains
  subroutine foo (x, y)
    type (dt), allocatable :: x(:), y(:)
    if (allocated (x) .neqv. allocated (y)) stop 12
    if (lbound (x, 1) /= lbound (y, 1)) stop 13
    if (ubound (x, 1) /= ubound (y, 1)) stop 14
    if (allocated (x(7)%h) .neqv. allocated (y(7)%h)) stop 15
    if (allocated (x(8)%h) .neqv. allocated (y(8)%h)) stop 16
    if (lbound (x(7)%h, 1) /= lbound (y(7)%h, 1)) stop 17
    if (ubound (x(7)%h, 1) /= ubound (y(7)%h, 1)) stop 18
    x(7)%g = 0
    x(7)%h = 0
    x(8)%g = 0
  end subroutine
  subroutine bar (x, y)
    type (dt), allocatable :: x(:), y(:)
    x(:)%g = x(:)%g + y(:)%g
    x(7)%h(:) = x(7)%h(:) + y(7)%h(:)
  end subroutine
end
