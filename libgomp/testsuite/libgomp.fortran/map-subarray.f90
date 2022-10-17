! { dg-do run }

program myprog
type u
  integer, dimension (:), pointer :: tarr
end type u

type(u) :: myu
integer, dimension (12), target :: myarray

myu%tarr => myarray

myu%tarr = 0

!$omp target map(to:myu%tarr) map(tofrom:myu%tarr(:))
myu%tarr(1) = myu%tarr(1) + 1
!$omp end target

!$omp target map(to:myu%tarr) map(tofrom:myu%tarr(1:2))
myu%tarr(1) = myu%tarr(1) + 1
!$omp end target

!$omp target map(to:myu%tarr) map(tofrom:myu%tarr(1))
myu%tarr(1) = myu%tarr(1) + 1
!$omp end target

!$omp target map(tofrom:myu%tarr)
myu%tarr(1) = myu%tarr(1) + 1
!$omp end target

if (myu%tarr(1).ne.4) stop 1

end program myprog
