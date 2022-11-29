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

! Check that nested mappers work inside modules.

!$omp declare mapper (F :: f) map(to: f%b) map(f%d)
!$omp declare mapper (G :: g) map(tofrom: g%myf)

end module mymod

program myprog
use mymod

type(F), target :: ftmp
type(G) :: gvar

gvar%myf => ftmp

gvar%myf%d = 0

!$omp target map(gvar%myf)
gvar%myf%d(1) = gvar%myf%d(1) + 1
!$omp end target

!$omp target map(gvar)
gvar%myf%d(1) = gvar%myf%d(1) + 1
!$omp end target

!$omp target
gvar%myf%d(1) = gvar%myf%d(1) + 1
!$omp end target

if (gvar%myf%d(1).ne.3) stop 1

end program myprog
