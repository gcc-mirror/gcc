implicit none
type t
  integer, allocatable :: a, b(:)
end type t
type(t) :: x, y, z
integer :: i

!$omp target
  if (allocated(x%a)) stop 1
  if (allocated(x%b)) stop 2
!$omp end target

allocate(x%a, x%b(-4:6))
x%b(:) = [(i, i=-4,6)]

!$omp target
  if (.not. allocated(x%a)) stop 3
  if (.not. allocated(x%b)) stop 4
  if (lbound(x%b,1) /= -4) stop 5
  if (ubound(x%b,1) /= 6) stop 6
  if (any (x%b /= [(i, i=-4,6)])) stop 7
!$omp end target


! The following only works with arrays due to
! PR fortran/96668

!$omp target enter data map(to: y, z)

!$omp target
  if (allocated(y%b)) stop 8
  if (allocated(z%b)) stop 9
!$omp end target

allocate(y%b(5), z%b(3))
y%b = 42
z%b = 99

! (implicitly) 'tofrom' mapped
! Planned for OpenMP 6.0 (but common extension)
! OpenMP <= 5.0 unclear
!$omp target
  if (.not.allocated(y%b)) stop 10
  if (any (y%b /= 42)) stop 11
!$omp end target

! always map: OpenMP 5.1 (clarified)
!$omp target map(always, tofrom: z)
  if (.not.allocated(z%b)) stop 12
  if (any (z%b /= 99)) stop 13
!$omp end target

end
