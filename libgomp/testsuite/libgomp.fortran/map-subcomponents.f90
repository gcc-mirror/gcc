! { dg-do run }

module mymod
type F
integer :: a, b, c
integer, dimension(10) :: d
end type F

type G
integer :: x, y
type(F), pointer :: myf
integer :: z
end type G
end module mymod

program myprog
use mymod

type(F), target :: ftmp
type(G) :: gvar

gvar%myf => ftmp

gvar%myf%d = 0

!$omp target map(to:gvar%myf) map(tofrom: gvar%myf%b, gvar%myf%d)
gvar%myf%d(1) = gvar%myf%d(1) + 1
!$omp end target

if (gvar%myf%d(1).ne.1) stop 1

end program myprog
