! { dg-do run }

module mymod
type G
integer :: x, y
integer, pointer :: arr(:)
integer :: z
end type G
end module mymod

program myprog
use mymod

integer, target :: arr1(10)
integer, target :: arr2(10)
integer, target :: arr3(10)
type(G), dimension(3) :: gvar

integer :: i, j

gvar(1)%arr => arr1
gvar(2)%arr => arr2
gvar(3)%arr => arr3

gvar(1)%arr = 0
gvar(2)%arr = 0
gvar(3)%arr = 0

i = 1
j = 1

! Here 'gvar(i)' and 'gvar(j)' are the same element, so this should work.
! This generates a whole-array mapping for gvar(i)%arr, but with the
! "runtime implicit" bit set so the smaller subarray gvar(j)%arr(1:5) takes
! precedence.

!$omp target map(gvar(i)%arr, gvar(j)%arr(1:5))
gvar(i)%arr(1) = gvar(i)%arr(1) + 1
gvar(j)%arr(1) = gvar(j)%arr(1) + 2
!$omp end target

!$omp target map(gvar(i)%arr(1:5), gvar(j)%arr)
gvar(i)%arr(1) = gvar(i)%arr(1) + 3
gvar(j)%arr(1) = gvar(j)%arr(1) + 4
!$omp end target

! For these ones, we know the array index is the same, so we can just
! drop the whole-array mapping.

!$omp target map(gvar(i)%arr, gvar(i)%arr(1:5))
gvar(i)%arr(1) = gvar(i)%arr(1) + 1
gvar(i)%arr(1) = gvar(j)%arr(1) + 2
!$omp end target

!$omp target map(gvar(i)%arr(1:5), gvar(i)%arr)
gvar(i)%arr(1) = gvar(i)%arr(1) + 3
gvar(i)%arr(1) = gvar(j)%arr(1) + 4
!$omp end target

if (gvar(1)%arr(1).ne.20) stop 1

end program myprog
