! { dg-do run }

program myprog
type t
  integer, dimension (8) :: arr1
end type t
type u
  type(t) :: t_elem
end type u

integer :: i
integer, parameter :: N = 10
type(u) :: myu(N)

!$omp declare mapper (t :: x) map(x%arr1(5:8))
!$omp declare mapper (tmapper: t :: x) map(x%arr1(1:4))
!$omp declare mapper (u :: x) map(mapper(tmapper), tofrom: x%t_elem)

do i = 1, N
  myu(i)%t_elem%arr1(1) = 1
  myu(i)%t_elem%arr1(5) = 1
end do

! Different ways of invoking nested mappers, named vs. unnamed

!$omp target map(iterator (n=1:N) tofrom:myu(n)%t_elem)
do i = 1, N
  myu(i)%t_elem%arr1(5) = myu(i)%t_elem%arr1(5) + 1
end do
!$omp end target

!$omp target map(iterator (n=1:N) tofrom:myu(n))
do i = 1, N
  myu(i)%t_elem%arr1(1) = myu(i)%t_elem%arr1(1) + 1
end do
!$omp end target

!$omp target
do i = 1, N
  myu(i)%t_elem%arr1(1) = myu(i)%t_elem%arr1(1) + 1
end do
!$omp end target

do i = 1, N
  if (myu(i)%t_elem%arr1(1).ne.3) stop 1
  if (myu(i)%t_elem%arr1(5).ne.2) stop 2
end do

end program myprog
