implicit none
integer, parameter :: N = 16
type t
  integer, allocatable :: a, b(:)
end type t
type(t) :: x(N), y(N), z(N)
integer :: i, j
integer :: lo = 3, hi = N

!$omp target map(iterator (it=1:N), to: x(it))
  do i = 1, N
    if (allocated(x(i)%a)) stop 1
    if (allocated(x(i)%b)) stop 2
  end do
!$omp end target

do i = 1, N
  allocate(x(i)%a, x(i)%b(-4:6))
  x(i)%b(:) = [(i, i=-4,6)]
end do

!$omp target map(iterator (it=2:N), to: x(it))
  do i = 2, N
    if (.not. allocated(x(i)%a)) stop 3
    if (.not. allocated(x(i)%b)) stop 4
    if (lbound(x(i)%b,1) /= -4) stop 5
    if (ubound(x(i)%b,1) /= 6) stop 6
    if (any (x(i)%b /= [(i, i=-4,6)])) stop 7
  end do
!$omp end target

!$omp target enter data map(iterator (it=3:N), to: y(it), z(it))

!$omp target map(iterator (it=3:N), to: y(it), z(it))
  do i = 3, N
    if (allocated(y(i)%b)) stop 8
    if (allocated(z(i)%b)) stop 9
  end do
!$omp end target

do i = 1, N
  allocate(y(i)%b(5), z(i)%b(3))
  y(i)%b = 42
  z(i)%b = 99
end do

!$omp target map(iterator (it=3:N), to: y(it))
  do i = 3, N
    if (.not.allocated(y(i)%b)) stop 10
    if (any (y(i)%b /= 42)) stop 11
  end do
!$omp end target

!$omp target map(iterator (it=lo:hi), always, tofrom: z(it))
  do i = 3, N
    if (.not.allocated(z(i)%b)) stop 12
    if (any (z(i)%b /= 99)) stop 13
  end do
!$omp end target

end
