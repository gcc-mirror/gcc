! { dg-do run }

program myprog
type t
  integer, dimension (8) :: arr1
end type t
type u
  type(t), dimension (:), pointer :: tarr
end type u

type(u) :: myu
type(t), dimension (12), target :: myarray

!$omp declare mapper (t :: x) map(x%arr1(1:4))
!$omp declare mapper (u :: x) map(to: x%tarr) map(x%tarr(1))

myu%tarr => myarray

myu%tarr(1)%arr1(1) = 1

! We can't do this: we have a mapper for "t" elements, and this implicitly maps
! the whole array.
!!$omp target map(tofrom:myu%tarr)
!myu%tarr(1)%arr1(1) = myu%tarr(1)%arr1(1) + 1
!!$omp end target

! ...but we can do this, because we're just mapping an element of the "t"
! array.  We still need to map the actual "myu%tarr" descriptor.
!$omp target map(to:myu%tarr) map(myu%tarr(1)%arr1(1:4))
myu%tarr(1)%arr1(1) = myu%tarr(1)%arr1(1) + 1
!$omp end target

!$omp target
myu%tarr(1)%arr1(1) = myu%tarr(1)%arr1(1) + 1
!$omp end target

if (myu%tarr(1)%arr1(1).ne.3) stop 1

end program myprog

