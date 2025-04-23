! { dg-do run }

program myprog
type t
  integer, dimension (8) :: arr1
end type t
type u
  type(t) :: t_elem
end type u

type(u) :: myu

!$omp declare mapper (tmapper: t :: x) map(x%arr1(1:4))
!$omp declare mapper (u :: x) map(mapper(tmapper), tofrom: x%t_elem)

myu%t_elem%arr1(1) = 1

!$omp target map(tofrom:myu%t_elem)
myu%t_elem%arr1(1) = myu%t_elem%arr1(1) + 1
!$omp end target

!$omp target map(tofrom:myu)
myu%t_elem%arr1(1) = myu%t_elem%arr1(1) + 1
!$omp end target

!$omp target
myu%t_elem%arr1(1) = myu%t_elem%arr1(1) + 1
!$omp end target

if (myu%t_elem%arr1(1).ne.4) stop 1

end program myprog

