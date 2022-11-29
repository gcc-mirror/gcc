! { dg-do run }

type t
  integer, dimension (8) :: arr1
end type t
type u
  type(t), dimension (:), pointer :: tarr
end type u

type(u) :: myu
type(t), dimension (1), target :: myarray

!$omp declare mapper (named: t :: x) map(x%arr1(1:4))
!$omp declare mapper (u :: x) map(to: x%tarr) map(mapper(named), tofrom: x%tarr(1))

myu%tarr => myarray
myu%tarr(1)%arr1 = 0

! Unnamed mapper invoking named mapper

!$omp target
myu%tarr(1)%arr1(1) = myu%tarr(1)%arr1(1) + 1
!$omp end target

if (myu%tarr(1)%arr1(1).ne.1) stop 1

end
