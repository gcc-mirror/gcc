! { dg-do run }

program myprog

type A
integer :: x
integer :: y(20)
integer, dimension(:), pointer :: z
end type A

integer, target :: arr1(20), arr2(20)
type(A) :: p, q

p%y = 0
q%y = 0

p%z => arr1
q%z => arr2

call mysub (p, q)

if (p%z(1).ne.1) stop 1
if (q%z(1).ne.1) stop 2

p%y = 0
q%y = 0
p%z = 0
q%z = 0

call mysub2 (p, q)

if (p%z(1).ne.1) stop 3
if (q%z(1).ne.1) stop 4

p%y = 0
q%y = 0
p%z = 0
q%z = 0

call mysub3 (p, q)

if (p%z(1).ne.1) stop 5
if (q%z(1).ne.1) stop 6

contains

subroutine mysub(arg1, arg2)
implicit none
type(A), intent(inout) :: arg1
type(A), intent(inout) :: arg2

!$omp declare mapper (A :: x) map(always, to:x) map(tofrom:x%z(:))

!$omp target
arg1%y(1) = arg1%y(1) + 1
arg1%z = arg1%y
arg2%y(1) = arg2%y(1) + 1
arg2%z = arg2%y
!$omp end target
end subroutine mysub

subroutine mysub2(arg1, arg2)
implicit none
type(A), intent(inout) :: arg1
type(A), intent(inout) :: arg2

!$omp declare mapper (A :: x) map(to:x) map(from:x%z(:))

!$omp target
arg1%y(1) = arg1%y(1) + 1
arg1%z = arg1%y
arg2%y(1) = arg2%y(1) + 1
arg2%z = arg2%y
!$omp end target
end subroutine mysub2

subroutine mysub3(arg1, arg2)
implicit none
type(A), intent(inout) :: arg1
type(A), intent(inout) :: arg2

!$omp declare mapper (A :: x) map(to:x) map(from:x%z(:))

!$omp target map(arg1, arg2)
arg1%y(1) = arg1%y(1) + 1
arg1%z = arg1%y
arg2%y(1) = arg2%y(1) + 1
arg2%z = arg2%y
!$omp end target
end subroutine mysub3

end program myprog
