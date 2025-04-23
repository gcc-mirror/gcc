! { dg-do run }

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

!$omp declare mapper (t :: x) map(tofrom: x%mys%c) map(x%mys%d(1:x%mys%c))

myu%myt%mys%c = 1
myu%myt%mys%d = 0

!$omp target map(tofrom: myu%myt)
myu%myt%mys%d(1) = myu%myt%mys%d(1) + 1
myu%myt%mys%c = myu%myt%mys%c + 2
!$omp end target

if (myu%myt%mys%d(1).ne.1) stop 1
if (myu%myt%mys%c.ne.3) stop 2

end program myprog
