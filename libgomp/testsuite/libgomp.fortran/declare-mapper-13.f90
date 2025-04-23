! { dg-do run }

module mymod
type S
integer :: a
integer :: b
integer :: c
end type S

!$omp declare mapper (S :: x) map(x%c)
end module mymod

program myprog
use mymod
type T
integer :: a
integer :: b
integer :: c
end type T

type(S) :: mys
type(T) :: myt

!$omp declare mapper (T :: x) map(x%b)

myt%a = 0
myt%b = 0
myt%c = 0
mys%a = 0
mys%b = 0
mys%c = 0

!$omp target
myt%b = myt%b + 1
!$omp end target

!$omp target
mys%c = mys%c + 1
!$omp end target

!$omp target
myt%b = myt%b + 2
mys%c = mys%c + 3
!$omp end target

if (myt%b.ne.3) stop 1
if (mys%c.ne.4) stop 2

end program myprog
