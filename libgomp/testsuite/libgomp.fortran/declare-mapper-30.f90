! { dg-do run }

type t
integer :: x, y
integer, allocatable :: arr(:)
end type t

!$omp declare mapper (t :: x) map(x%arr)

type(t) :: var

allocate(var%arr(1:20))

var%arr = 0

! The mapper named literally 'default' should be the default mapper, i.e.
! the same as the unnamed mapper defined above.
!$omp target map(mapper(default), tofrom: var)
var%arr(5) = 5
!$omp end target

if (var%arr(5).ne.5) stop 1

end
