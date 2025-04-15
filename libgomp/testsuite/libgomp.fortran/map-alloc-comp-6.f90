! NOTE: This code uses POINTER.
! While map(p, var%p) etc. maps the ptr/ptr comp p / var%p (incl. allocatable comps),
! map(var) does not map var%p.

use iso_c_binding
implicit none
type t2
  integer, allocatable :: x, y, z
end type t2
type t
  integer, pointer :: A => null()
  integer, pointer :: B(:) => null()
  type(t2), pointer :: C => null()
  type(t2), pointer :: D(:,:) => null()
end type t

type t3
  type(t) :: Q
  type(t) :: R(5)
end type

type(t) :: var, var2
type(t3) :: var3, var4
integer(c_intptr_t) :: iptr

! --------------------------------------
! Assign + allocate
allocate (var%A, source=45)
allocate (var%B(3), source=[1,2,3])
allocate (var%C)
var%C%x = 6; var%C%y = 5; var%C%z = 4
allocate (var%D(2,2))
var%D(1,1)%x = 1
var%D(1,1)%y = 2
var%D(1,1)%z = 3
var%D(2,1)%x = 4
var%D(2,1)%y = 5
var%D(2,1)%z = 6
var%D(1,2)%x = 11
var%D(1,2)%y = 12
var%D(1,2)%z = 13
var%D(2,2)%x = 14
var%D(2,2)%y = 15
var%D(2,2)%z = 16

! Assign + allocate
allocate (var2%A, source=145)
allocate (var2%B, source=[991,992,993])
allocate (var2%C)
var2%C%x = 996; var2%C%y = 995; var2%C%z = 994
allocate (var2%D(2,2))
var2%D(1,1)%x = 199
var2%D(1,1)%y = 299
var2%D(1,1)%z = 399
var2%D(2,1)%x = 499
var2%D(2,1)%y = 599
var2%D(2,1)%z = 699
var2%D(1,2)%x = 1199
var2%D(1,2)%y = 1299
var2%D(1,2)%z = 1399
var2%D(2,2)%x = 1499
var2%D(2,2)%y = 1599
var2%D(2,2)%z = 1699

block
  integer(c_intptr_t) :: loc_a, loc_b, loc_c, loc_d, loc2_a, loc2_b, loc2_c, loc2_d
  loc_a = loc (var%a)
  loc_b = loc (var%b)
  loc_c = loc (var%d)
  loc_d = loc (var%d)
  loc2_a = loc (var2%a)
  loc2_b = loc (var2%b)
  loc2_c = loc (var2%c)
  loc2_d = loc (var2%d)
  ! var/var2 are mapped, but the pointer components aren't
  !$omp target map(to: var) map(tofrom: var2)
    if (loc_a /= loc (var%a)) stop 31
    if (loc_b /= loc (var%b)) stop 32
    if (loc_c /= loc (var%d)) stop 33
    if (loc_d /= loc (var%d)) stop 34
    if (loc2_a /= loc (var2%a)) stop 35
    if (loc2_b /= loc (var2%b)) stop 36
    if (loc2_c /= loc (var2%c)) stop 37
    if (loc2_d /= loc (var2%d)) stop 38
  !$omp end target
  if (loc_a /= loc (var%a)) stop 41
  if (loc_b /= loc (var%b)) stop 42
  if (loc_c /= loc (var%d)) stop 43
  if (loc_d /= loc (var%d)) stop 44
  if (loc2_a /= loc (var2%a)) stop 45
  if (loc2_b /= loc (var2%b)) stop 46
  if (loc2_c /= loc (var2%c)) stop 47
  if (loc2_d /= loc (var2%d)) stop 48
end block

block
  ! Map only (all) components, but this maps also the alloc comps
  !$omp target map(to: var%a, var%b, var%c, var%d) map(tofrom: var2%a, var2%b, var2%c, var2%d)
    call foo (var,var2)
  !$omp end target
end block

if (var2%A /= 45) stop 9
if (any (var2%B /= [1,2,3])) stop 10
if (var2%C%x /= 6) stop 11
if (var2%C%y /= 5) stop 11
if (var2%C%z /= 4) stop 11
block
  integer :: tmp_x(2,2), tmp_y(2,2), tmp_z(2,2), i, j
  tmp_x = reshape([1, 4, 11, 14], [2,2])
  tmp_y = reshape([2, 5, 12, 15], [2,2])
  tmp_z = reshape([3, 6, 13, 16], [2,2])
  do j = 1, 2
    do i = 1, 2
      if (var2%D(i,j)%x /= tmp_x(i,j)) stop 12
      if (var2%D(i,j)%y /= tmp_y(i,j)) stop 12
      if (var2%D(i,j)%z /= tmp_z(i,j)) stop 12
    end do
  end do
end block

! Extra deallocates due to PR fortran/104697
deallocate(var%C%x, var%C%y, var%C%z)
deallocate(var%D(1,1)%x, var%D(1,1)%y, var%D(1,1)%z)
deallocate(var%D(2,1)%x, var%D(2,1)%y, var%D(2,1)%z)
deallocate(var%D(1,2)%x, var%D(1,2)%y, var%D(1,2)%z)
deallocate(var%D(2,2)%x, var%D(2,2)%y, var%D(2,2)%z)
deallocate(var%A, var%B, var%C, var%D)

deallocate(var2%C%x, var2%C%y, var2%C%z)
deallocate(var2%D(1,1)%x, var2%D(1,1)%y, var2%D(1,1)%z)
deallocate(var2%D(2,1)%x, var2%D(2,1)%y, var2%D(2,1)%z)
deallocate(var2%D(1,2)%x, var2%D(1,2)%y, var2%D(1,2)%z)
deallocate(var2%D(2,2)%x, var2%D(2,2)%y, var2%D(2,2)%z)
deallocate(var2%A, var2%B, var2%C, var2%D)

! --------------------------------------
! Assign + allocate
allocate (var3%Q%A, source=45)
allocate (var3%Q%B, source=[1,2,3])
allocate (var3%Q%C, source=t2(6,5,4))
allocate (var3%Q%D(2,2))
var3%Q%D(1,1) = t2(1,2,3)
var3%Q%D(2,1) = t2(4,5,6)
var3%Q%D(1,2) = t2(11,12,13)
var3%Q%D(2,2) = t2(14,15,16)

allocate (var3%R(2)%A, source=45)
allocate (var3%R(2)%B, source=[1,2,3])
allocate (var3%R(2)%C, source=t2(6,5,4))
allocate (var3%R(2)%D(2,2))
var3%R(2)%D(1,1) = t2(1,2,3)
var3%R(2)%D(2,1) = t2(4,5,6)
var3%R(2)%D(1,2) = t2(11,12,13)
var3%R(2)%D(2,2) = t2(14,15,16)

! Assign + allocate
allocate (var4%Q%A, source=145)
allocate (var4%Q%B, source=[991,992,993])
allocate (var4%Q%C, source=t2(996,995,994))
allocate (var4%Q%D(2,2))
var4%Q%D(1,1) = t2(199,299,399)
var4%Q%D(2,1) = t2(499,599,699)
var4%Q%D(1,2) = t2(1199,1299,1399)
var4%Q%D(2,2) = t2(1499,1599,1699)

allocate (var4%R(3)%A, source=145)
allocate (var4%R(3)%B, source=[991,992,993])
allocate (var4%R(3)%C, source=t2(996,995,994))
allocate (var4%R(3)%D(2,2))
var4%R(3)%D(1,1) = t2(199,299,399)
var4%R(3)%D(2,1) = t2(499,599,699)
var4%R(3)%D(1,2) = t2(1199,1299,1399)
var4%R(3)%D(2,2) = t2(1499,1599,1699)

!$omp target map(to: var3%Q%A, var3%Q%B, var3%Q%C, var3%Q%D) &
!$omp&       map(tofrom: var4%Q%A, var4%Q%B, var4%Q%C, var4%Q%D)
  call foo(var3%Q, var4%Q)
!$omp end target

iptr = loc(var3%R(2)%A)

!$omp target map(to: var3%R(2)%A, var3%R(2)%B, var3%R(2)%C, var3%R(2)%D) &
!$omp&       map(tofrom: var4%R(3)%A, var4%R(3)%B, var4%R(3)%C, var4%R(3)%D)
  call foo(var3%R(2), var4%R(3))
!$omp end target

if (var4%Q%A /= 45) stop 13
if (any (var4%Q%B /= [1,2,3])) stop 14
if (var4%Q%C%x /= 6) stop 15
if (var4%Q%C%y /= 5) stop 15
if (var4%Q%C%z /= 4) stop 15
block
  integer :: tmp_x(2,2), tmp_y(2,2), tmp_z(2,2), i, j
  tmp_x = reshape([1, 4, 11, 14], [2,2])
  tmp_y = reshape([2, 5, 12, 15], [2,2])
  tmp_z = reshape([3, 6, 13, 16], [2,2])
  do j = 1, 2
    do i = 1, 2
      if (var4%Q%D(i,j)%x /= tmp_x(i,j)) stop 16
      if (var4%Q%D(i,j)%y /= tmp_y(i,j)) stop 16
      if (var4%Q%D(i,j)%z /= tmp_z(i,j)) stop 16
    end do
  end do
end block

! Cf. PR fortran/104696
! { dg-output "valid mapping, OK" { xfail { offload_device_nonshared_as } } }
if (iptr /= loc(var3%R(2)%A)) then
  print *, "invalid mapping, cf. PR fortran/104696"
else

if (var4%R(3)%A /= 45) stop 17
if (any (var4%R(3)%B /= [1,2,3])) stop 18
if (var4%R(3)%C%x /= 6) stop 19
if (var4%R(3)%C%y /= 5) stop 19
if (var4%R(3)%C%z /= 4) stop 19
block
  integer :: tmp_x(2,2), tmp_y(2,2), tmp_z(2,2), i, j
  tmp_x = reshape([1, 4, 11, 14], [2,2])
  tmp_y = reshape([2, 5, 12, 15], [2,2])
  tmp_z = reshape([3, 6, 13, 16], [2,2])
  do j = 1, 2
    do i = 1, 2
      if (var4%R(3)%D(i,j)%x /= tmp_x(i,j)) stop 20
      if (var4%R(3)%D(i,j)%y /= tmp_y(i,j)) stop 20
      if (var4%R(3)%D(i,j)%z /= tmp_z(i,j)) stop 20
    end do
  end do
end block

! Extra deallocates due to PR fortran/104697
deallocate(var3%Q%C%x, var3%Q%D(1,1)%x, var3%Q%D(2,1)%x, var3%Q%D(1,2)%x, var3%Q%D(2,2)%x)
deallocate(var3%Q%C%y, var3%Q%D(1,1)%y, var3%Q%D(2,1)%y, var3%Q%D(1,2)%y, var3%Q%D(2,2)%y)
deallocate(var3%Q%C%z, var3%Q%D(1,1)%z, var3%Q%D(2,1)%z, var3%Q%D(1,2)%z, var3%Q%D(2,2)%z)
deallocate(var3%Q%A, var3%Q%B, var3%Q%C, var3%Q%D)

deallocate(var4%Q%C%x, var4%Q%D(1,1)%x, var4%Q%D(2,1)%x, var4%Q%D(1,2)%x, var4%Q%D(2,2)%x)
deallocate(var4%Q%C%y, var4%Q%D(1,1)%y, var4%Q%D(2,1)%y, var4%Q%D(1,2)%y, var4%Q%D(2,2)%y)
deallocate(var4%Q%C%z, var4%Q%D(1,1)%z, var4%Q%D(2,1)%z, var4%Q%D(1,2)%z, var4%Q%D(2,2)%z)
deallocate(var4%Q%A, var4%Q%B, var4%Q%C, var4%Q%D)

deallocate(var3%R(2)%C%x, var3%R(2)%D(1,1)%x, var3%R(2)%D(2,1)%x, var3%R(2)%D(1,2)%x, var3%R(2)%D(2,2)%x)
deallocate(var3%R(2)%C%y, var3%R(2)%D(1,1)%y, var3%R(2)%D(2,1)%y, var3%R(2)%D(1,2)%y, var3%R(2)%D(2,2)%y)
deallocate(var3%R(2)%C%z, var3%R(2)%D(1,1)%z, var3%R(2)%D(2,1)%z, var3%R(2)%D(1,2)%z, var3%R(2)%D(2,2)%z)
deallocate(var3%R(2)%A, var3%R(2)%B, var3%R(2)%C, var3%R(2)%D)

deallocate(var4%R(3)%C%x, var4%R(3)%D(1,1)%x, var4%R(3)%D(2,1)%x, var4%R(3)%D(1,2)%x, var4%R(3)%D(2,2)%x)
deallocate(var4%R(3)%C%y, var4%R(3)%D(1,1)%y, var4%R(3)%D(2,1)%y, var4%R(3)%D(1,2)%y, var4%R(3)%D(2,2)%y)
deallocate(var4%R(3)%C%z, var4%R(3)%D(1,1)%z, var4%R(3)%D(2,1)%z, var4%R(3)%D(1,2)%z, var4%R(3)%D(2,2)%z)
deallocate(var4%R(3)%A, var4%R(3)%B, var4%R(3)%C, var4%R(3)%D)

  print *, "valid mapping, OK"
endif

contains
  subroutine foo(x, y)
    type(t) :: x, y
    intent(in) :: x
    intent(inout) :: y
    integer :: tmp_x(2,2), tmp_y(2,2), tmp_z(2,2), i, j
    if (x%A /= 45) stop 1
    if (any (x%B /= [1,2,3])) stop 2
    if (x%C%x /= 6) stop 3
    if (x%C%y /= 5) stop 3
    if (x%C%z /= 4) stop 3

    tmp_x = reshape([1, 4, 11, 14], [2,2])
    tmp_y = reshape([2, 5, 12, 15], [2,2])
    tmp_z = reshape([3, 6, 13, 16], [2,2])
    do j = 1, 2
      do i = 1, 2
        if (x%D(i,j)%x /= tmp_x(i,j)) stop 4
        if (x%D(i,j)%y /= tmp_y(i,j)) stop 4
        if (x%D(i,j)%z /= tmp_z(i,j)) stop 4
      end do
    end do

    if (y%A /= 145) stop 5
    if (any (y%B /= [991,992,993])) stop 6
    if (y%C%x /= 996) stop 7
    if (y%C%y /= 995) stop 7
    if (y%C%z /= 994) stop 7
    tmp_x = reshape([199, 499, 1199, 1499], [2,2])
    tmp_y = reshape([299, 599, 1299, 1599], [2,2])
    tmp_z = reshape([399, 699, 1399, 1699], [2,2])
    do j = 1, 2
      do i = 1, 2
        if (y%D(i,j)%x /= tmp_x(i,j)) stop 8
        if (y%D(i,j)%y /= tmp_y(i,j)) stop 8
        if (y%D(i,j)%z /= tmp_z(i,j)) stop 8
      end do
    end do

    y%A = x%A
    y%B(:) = x%B
    y%C%x = x%C%x
    y%C%y = x%C%y
    y%C%z = x%C%z
    do j = 1, 2
      do i = 1, 2
        y%D(i,j)%x = x%D(i,j)%x
        y%D(i,j)%y = x%D(i,j)%y
        y%D(i,j)%z = x%D(i,j)%z
      end do
    end do
  end
end
