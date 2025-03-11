! { dg-do run }

program myprog

type A
character(len=20) :: string1
character(len=:), pointer :: string2
end type A

integer, parameter :: N = 8
integer :: i

!$omp declare mapper (A :: x) map(to:x%string1) map(from:x%string2)

type(A) :: var(N)

do i = 1, N
  allocate(character(len=20) :: var(i)%string2)

  var(i)%string1 = "hello world"
end do

!$omp target map(iterator (n=1:N) to:var(n)%string1) map(iterator (n=1:N) from:var(n)%string2)
do i = 1, N
  var(i)%string2 = var(i)%string1
end do
!$omp end target

do i = 1, N
  if (var(i)%string2.ne."hello world") stop 1
end do

end program myprog
