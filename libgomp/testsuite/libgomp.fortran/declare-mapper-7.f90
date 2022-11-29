! { dg-do run }

program myprog
type s
  integer :: a
  integer :: b
end type s

type t
  type(s) :: mys
end type t

type(t) :: myt

! Identity mapper

!$omp declare mapper (s :: x) map(tofrom: x)
!$omp declare mapper (t :: x) map(tofrom: x%mys)

myt%mys%a = 0
myt%mys%b = 0

!$omp target map(tofrom: myt)
myt%mys%a = myt%mys%a + 1
!$omp end target

if (myt%mys%a.ne.1) stop 1

end program myprog
