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

integer, parameter :: N = 8
integer :: i

type(F), target :: ftmp(N)
type(G) :: gvar(N)

do i = 1, N
  gvar(i)%myf => ftmp(i)
  gvar(i)%myf%d = 0
end do

!$omp target map(iterator (n=1:N) tofrom: gvar(n)%myf)
do i = 1, N
  gvar(i)%myf%d(1) = gvar(i)%myf%d(1) + 1
end do
!$omp end target

!$omp target map(iterator (n=1:N) tofrom: gvar(n))
do i = 1, N
  gvar(i)%myf%d(1) = gvar(i)%myf%d(1) + 1
end do
!$omp end target

do i = 1, N
  if (gvar(i)%myf%d(1).ne.2) stop 1
end do

end program myprog
