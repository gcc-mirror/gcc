! { dg-do run }

program myprog
type F
integer :: a, b, c
integer, dimension(10) :: d
end type F

type(F), pointer :: myf

!$omp declare mapper (F :: f) map(f%d)

allocate(myf)

myf%d = 0

!$omp target map(myf)
myf%d(1) = myf%d(1) + 1
!$omp end target

!$omp target
myf%d(1) = myf%d(1) + 1
!$omp end target

if (myf%d(1).ne.2) stop 1

deallocate(myf)

end program myprog
