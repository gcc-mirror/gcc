! { dg-skip-if "" { offload_device_shared_as } }
! As runtime test, this only works when mapping actually happens (no self map, no host fallback)

! Add some checks for pointer/allocatable/aggregate and 'defaultmap' (sorry, no scalar)

! Before pointer arrays were misclassified as aggregate
! and aggregates passed as dummy argument as pointer.

module m
  implicit none
  type t
    integer :: x
  end type t
contains

subroutine s(da,dt,pa,p,a,aa)
type(t) :: da(2), dt
integer, pointer :: pa(:), p
integer, allocatable :: aa(:), a

if (da(1)%x /= 333 .or. da(2)%x /= 444 .or. dt%x /= 555   &
    .or. pa(1) /= 11 .or. pa(2) /= 22 .or. pa(3) /= 33  .or. p /= 111 &
    .or. aa(1) /= 121 .or. aa(2) /= 222 .or. aa(3) /= 323 .or. aa(4) /= 424 .or. a /= 222) &
  stop 1

! ---------------

!$omp target defaultmap(to : pointer)
  da(1)%x = 1
  dt%x = 1
  pa(1) = 1
  p = 1
  aa(1) = 1
  a = 1
!$omp end target

! All change but pointer
if (da(1)%x /= 1 .or. da(2)%x /= 444 .or. dt%x /= 1   &
    .or. pa(1) /= 11 .or. pa(2) /= 22 .or. pa(3) /= 33  .or. p /= 111 &
    .or. aa(1) /= 1 .or. aa(2) /= 222 .or. aa(3) /= 323 .or. aa(4) /= 424 .or. a /= 1) &
  stop 2

! ---------------

!$omp target defaultmap(to : allocatable)
  da(1)%x = 2
  dt%x = 2
  pa(1) = 2
  p = 2
  aa(1) = 2
  a = 2
!$omp end target

! All change but allocatable
if (da(1)%x /= 2 .or. da(2)%x /= 444 .or. dt%x /= 2   &
    .or. pa(1) /= 2 .or. pa(2) /= 22 .or. pa(3) /= 33  .or. p /= 2 &
    .or. aa(1) /= 1 .or. aa(2) /= 222 .or. aa(3) /= 323 .or. aa(4) /= 424 .or. a /= 1) &
  stop 3

! ---------------

!$omp target defaultmap(to : aggregate)
  da(1)%x = 3
  dt%x = 3
  pa(1) = 3
  p = 3
  aa(1) = 3
  a = 3
!$omp end target

! All change but aggregate
if (da(1)%x /= 2 .or. da(2)%x /= 444 .or. dt%x /= 2   &
    .or. pa(1) /= 3 .or. pa(2) /= 22 .or. pa(3) /= 33  .or. p /= 3 &
    .or. aa(1) /= 3 .or. aa(2) /= 222 .or. aa(3) /= 323 .or. aa(4) /= 424 .or. a /= 3) &
  stop 4

! ---------------

!$omp target map(to: da, dt, pa, p, aa, a)
  da(1)%x = 4
  dt%x = 4
  pa(1) = 4
  p = 4
  aa(1) = 4
  a = 4
!$omp end target

! None change
if (da(1)%x /= 2 .or. da(2)%x /= 444 .or. dt%x /= 2   &
    .or. pa(1) /= 3 .or. pa(2) /= 22 .or. pa(3) /= 33  .or. p /= 3 &
    .or. aa(1) /= 3 .or. aa(2) /= 222 .or. aa(3) /= 323 .or. aa(4) /= 424 .or. a /= 3) &
  stop 5

! ---------------

!$omp target defaultmap(to : all)
  da(1)%x = 5
  dt%x = 5
  pa(1) = 5
  p = 5
  aa(1) = 5
  a = 5
!$omp end target

! None change
if (da(1)%x /= 2 .or. da(2)%x /= 444 .or. dt%x /= 2   &
    .or. pa(1) /= 3 .or. pa(2) /= 22 .or. pa(3) /= 33  .or. p /= 3 &
    .or. aa(1) /= 3 .or. aa(2) /= 222 .or. aa(3) /= 323 .or. aa(4) /= 424 .or. a /= 3) &
  stop 6

! ---------------

!$omp target defaultmap(tofrom : all)
  da(1)%x = 6
  dt%x = 6
  pa(1) = 6
  p = 6
  aa(1) = 6
  a = 6
!$omp end target


! All change
if (da(1)%x /= 6 .or. da(2)%x /= 444 .or. dt%x /= 6   &
    .or. pa(1) /= 6 .or. pa(2) /= 22 .or. pa(3) /= 33  .or. p /= 6 &
    .or. aa(1) /= 6 .or. aa(2) /= 222 .or. aa(3) /= 323 .or. aa(4) /= 424 .or. a /= 6) &
  stop 7
end
end module m

use m
implicit none

type(t) :: da(2), dt
integer, pointer :: pa(:), p
integer, allocatable :: aa(:), a

allocate(pa(3),p,a)

da = [t(333),t(444)]
dt = t(555)
pa = [11,22,33]
p = 111
aa = [121, 222, 323, 424]
a = 222

call s(da,dt,pa,p,a,aa)
end
