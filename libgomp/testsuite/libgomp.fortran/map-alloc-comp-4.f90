type t2
  integer x, y, z
end type t2
type t
  integer, allocatable :: A
  integer, allocatable :: B(:)
  type(t2), allocatable :: C
  type(t2), allocatable :: D(:,:)
end type t

type t3
  type(t) :: Q
  type(t) :: R(5)
end type

type(t) :: var, var2
type(t3) :: var3, var4

! --------------------------------------
! Assign + allocate
var%A = 45
var%B = [1,2,3]
var%C = t2(6,5,4)
var%D = reshape([t2(1,2,3), t2(4,5,6), t2(11,12,13), t2(14,15,16)], [2,2])

! Assign + allocate
var2%A = 145
var2%B = [991,992,993]
var2%C = t2(996,995,994)
var2%D = reshape([t2(199,299,399), t2(499,599,699), t2(1199,1299,1399), t2(1499,1599,1699)], [2,2])


!$omp target map(to: var%A, var%B, var%C, var%D) &
!$omp&       map(tofrom: var2%A, var2%B, var2%C, var2%D)
  call foo(var, var2)
!$omp end target

if (var2%A /= 45) stop 9
if (any (var2%B /= [1,2,3])) stop 10
if (var2%C%x /= 6) stop 11
if (var2%C%y /= 5) stop 11
if (var2%C%z /= 4) stop 11
if (any (var2%D(:,:)%x /= reshape([1, 4, 11, 14], [2,2]))) stop 12
if (any (var2%D(:,:)%y /= reshape([2, 5, 12, 15], [2,2]))) stop 12
if (any (var2%D(:,:)%z /= reshape([3, 6, 13, 16], [2,2]))) stop 12

! --------------------------------------
! Assign + allocate
var3%Q%A = 45
var3%Q%B = [1,2,3]
var3%Q%C = t2(6,5,4)
var3%Q%D = reshape([t2(1,2,3), t2(4,5,6), t2(11,12,13), t2(14,15,16)], [2,2])

var3%R(2)%A = 45
var3%R(2)%B = [1,2,3]
var3%R(2)%C = t2(6,5,4)
var3%R(2)%D = reshape([t2(1,2,3), t2(4,5,6), t2(11,12,13), t2(14,15,16)], [2,2])

! Assign + allocate
var4%Q%A = 145
var4%Q%B = [991,992,993]
var4%Q%C = t2(996,995,994)
var4%Q%D = reshape([t2(199,299,399), t2(499,599,699), t2(1199,1299,1399), t2(1499,1599,1699)], [2,2])

var4%R(3)%A = 145
var4%R(3)%B = [991,992,993]
var4%R(3)%C = t2(996,995,994)
var4%R(3)%D = reshape([t2(199,299,399), t2(499,599,699), t2(1199,1299,1399), t2(1499,1599,1699)], [2,2])

!$omp target map(to: var3%Q%A, var3%Q%B, var3%Q%C, var3%Q%D) &
!$omp&       map(tofrom: var4%Q%A, var4%Q%B, var4%Q%C, var4%Q%D)
  call foo(var3%Q, var4%Q)
!$omp end target

if (var4%Q%A /= 45) stop 13
if (any (var4%Q%B /= [1,2,3])) stop 14
if (var4%Q%C%x /= 6) stop 15
if (var4%Q%C%y /= 5) stop 15
if (var4%Q%C%z /= 4) stop 15
if (any (var4%Q%D(:,:)%x /= reshape([1, 4, 11, 14], [2,2]))) stop 16
if (any (var4%Q%D(:,:)%y /= reshape([2, 5, 12, 15], [2,2]))) stop 16
if (any (var4%Q%D(:,:)%z /= reshape([3, 6, 13, 16], [2,2]))) stop 16

!$omp target map(to: var3%R(2)%A, var3%R(2)%B, var3%R(2)%C, var3%R(2)%D) &
!$omp&       map(tofrom: var4%R(3)%A, var4%R(3)%B, var4%R(3)%C, var4%R(3)%D)
  call foo(var3%R(2), var4%R(3))
!$omp end target

if (var4%R(3)%A /= 45) stop 17
if (any (var4%R(3)%B /= [1,2,3])) stop 18
if (var4%R(3)%C%x /= 6) stop 19
if (var4%R(3)%C%y /= 5) stop 19
if (var4%R(3)%C%z /= 4) stop 19
if (any (var4%R(3)%D(:,:)%x /= reshape([1, 4, 11, 14], [2,2]))) stop 20
if (any (var4%R(3)%D(:,:)%y /= reshape([2, 5, 12, 15], [2,2]))) stop 20
if (any (var4%R(3)%D(:,:)%z /= reshape([3, 6, 13, 16], [2,2]))) stop 20

contains
  subroutine foo(x, y)
    type(t) :: x, y
    if (x%A /= 45) stop 1
    if (any (x%B /= [1,2,3])) stop 2
    if (x%C%x /= 6) stop 3
    if (x%C%y /= 5) stop 3
    if (x%C%z /= 4) stop 3
    if (any (x%D(:,:)%x /= reshape([1, 4, 11, 14], [2,2]))) stop 4
    if (any (x%D(:,:)%y /= reshape([2, 5, 12, 15], [2,2]))) stop 4
    if (any (x%D(:,:)%z /= reshape([3, 6, 13, 16], [2,2]))) stop 4

    if (y%A /= 145) stop 5
    if (any (y%B /= [991,992,993])) stop 6
    if (y%C%x /= 996) stop 7
    if (y%C%y /= 995) stop 7
    if (y%C%z /= 994) stop 7
    if (any (y%D(:,:)%x /= reshape([199, 499, 1199, 1499], [2,2]))) stop 8
    if (any (y%D(:,:)%y /= reshape([299, 599, 1299, 1599], [2,2]))) stop 8
    if (any (y%D(:,:)%z /= reshape([399, 699, 1399, 1699], [2,2]))) stop 8

    y%A = x%A
    y%B(:) = x%B
    y%C = x%C
    y%D(:,:) = x%D(:,:)
  end
end
