! { dg-do run }

program myprog

type A
character(len=20) :: string1
character(len=:), allocatable :: string2
end type A

!$omp declare mapper (A :: x) map(to:x%string1) map(from:x%string2)

type(A) :: var

allocate(character(len=20) :: var%string2)

var%string1 = "hello world"

!$omp target
var%string2 = var%string1
!$omp end target

if (var%string2.ne."hello world") stop 1

end program myprog
