! { dg-do run }

program myprog
type bounds
  integer :: lo
  integer :: hi
end type bounds

integer, allocatable :: myarr(:)
type(bounds) :: b

! Use the placeholder variable, but not at the top level.
!$omp declare mapper (bounds :: x) map(tofrom: myarr(x%lo:x%hi))

allocate (myarr(1:100))

b%lo = 4
b%hi = 6

myarr = 0

!$omp target map(tofrom: b)
myarr(5) = myarr(5) + 1
!$omp end target

if (myarr(5).ne.1) stop 1

end program myprog
