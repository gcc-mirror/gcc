! { dg-do run }

program myprog
type t
  integer, dimension (8) :: arr1
end type t
type u
  integer, dimension (9) :: arr1
end type u
type v
  integer, dimension (10) :: arr1
end type v
type w
  integer, dimension (11) :: arr1
end type w
type y
  integer, dimension(:), pointer :: ptr1
end type y
type z
  integer, dimension(:), pointer :: ptr1
end type z

!$omp declare mapper (t::x) map(tofrom:x%arr1)
!$omp declare mapper (u::x) map(tofrom:x%arr1(:))
!$omp declare mapper (v::x) map(always,tofrom:x%arr1(1:3))
!$omp declare mapper (w::x) map(tofrom:x%arr1(1))
!$omp declare mapper (y::x) map(tofrom:x%ptr1)
!$omp declare mapper (z::x) map(to:x%ptr1) map(tofrom:x%ptr1(1:3))

type(t) :: myt
type(u) :: myu
type(v) :: myv
type(w) :: myw
type(y) :: myy
integer, target, dimension(8) :: arrtgt
type(z) :: myz
integer, target, dimension(8) :: arrtgt2

myy%ptr1 => arrtgt
myz%ptr1 => arrtgt2

myt%arr1 = 0

!$omp target map(myt)
myt%arr1(1) = myt%arr1(1) + 1
!$omp end target

!$omp target
myt%arr1(1) = myt%arr1(1) + 1
!$omp end target

if (myt%arr1(1).ne.2) stop 1

myu%arr1 = 0

!$omp target map(tofrom:myu%arr1(:))
myu%arr1(1) = myu%arr1(1) + 1
!$omp end target

!$omp target
myu%arr1(1) = myu%arr1(1) + 1
!$omp end target

if (myu%arr1(1).ne.2) stop 2

myv%arr1 = 0

!$omp target map(always,tofrom:myv%arr1(1:3))
myv%arr1(1) = myv%arr1(1) + 1
!$omp end target

!$omp target
myv%arr1(1) = myv%arr1(1) + 1
!$omp end target

if (myv%arr1(1).ne.2) stop 3

myw%arr1 = 0

!$omp target map(tofrom:myw%arr1(1))
myw%arr1(1) = myw%arr1(1) + 1
!$omp end target

!$omp target
myw%arr1(1) = myw%arr1(1) + 1
!$omp end target

if (myw%arr1(1).ne.2) stop 4

myy%ptr1 = 0

!$omp target map(tofrom:myy%ptr1)
myy%ptr1(1) = myy%ptr1(1) + 1
!$omp end target

!$omp target map(to:myy%ptr1) map(tofrom:myy%ptr1(1:2))
myy%ptr1(1) = myy%ptr1(1) + 1
!$omp end target

!$omp target
myy%ptr1(1) = myy%ptr1(1) + 1
!$omp end target

if (myy%ptr1(1).ne.3) stop 5

myz%ptr1(1) = 0

!$omp target
myz%ptr1(1) = myz%ptr1(1) + 1
!$omp end target

if (myz%ptr1(1).ne.1) stop 6

end program myprog

