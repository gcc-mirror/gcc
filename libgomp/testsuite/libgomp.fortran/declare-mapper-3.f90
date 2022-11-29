program myprog
type s
  integer :: c
  integer :: d(99)
end type s

type t
  type(s) :: mys
end type t

type u
  type(t) :: myt
end type u

type(u) :: myu

!$omp declare mapper (s :: x) map(tofrom: x%c, x%d(1:x%c))
!$omp declare mapper (t :: x) map(tofrom: x%mys)
!$omp declare mapper (u :: x) map(tofrom: x%myt)

myu%myt%mys%c = 1
myu%myt%mys%d = 0

! Nested mappers.

!$omp target map(tofrom: myu)
myu%myt%mys%d(1) = myu%myt%mys%d(1) + 1
!$omp end target

if (myu%myt%mys%c.ne.1) stop 1
if (myu%myt%mys%d(1).ne.1) stop 2

end program myprog
