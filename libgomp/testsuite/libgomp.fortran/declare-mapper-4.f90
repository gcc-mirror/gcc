! { dg-do run }

program myprog
type s
  integer :: c
  integer, allocatable :: d(:)
end type s

type t
  type(s) :: mys
end type t

type u
  type(t) :: myt
end type u

type(u) :: myu

! Here, the mappers are declared out of order, but earlier ones can still
! trigger mappers defined later.  Implementation-wise, this happens during
! resolution, but from the user perspective it appears to happen at
! instantiation time -- at which point all mappers are visible.  I think
! that makes sense.
!$omp declare mapper (u :: x) map(tofrom: x%myt)
!$omp declare mapper (t :: x) map(tofrom: x%mys)
!$omp declare mapper (s :: x) map(tofrom: x%c, x%d(1:x%c))

allocate(myu%myt%mys%d(1:20))

myu%myt%mys%c = 1
myu%myt%mys%d = 0

!$omp target map(tofrom: myu)
myu%myt%mys%d(1) = myu%myt%mys%d(1) + 1
!$omp end target

! Note: we only mapped the first element of the array 'd'.
if (myu%myt%mys%d(1).ne.1) stop 1

end program myprog
