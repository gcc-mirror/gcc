program p
! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

integer :: A(200)
A = [(i, i=1,200)]
!$omp target enter data map(to: A(40:200))
call foo(A(101:))

contains

subroutine foo(x)
integer, target :: x(100)
integer, pointer :: p(:,:)
integer :: i, j

p(0:5,-5:-1) => x(::2)

!$omp target
x = x * 2
!$omp end target

!$omp target update from(x(1:20:2))

do i=1,20
if (mod(i,2).eq.1 .and. x(i).ne.(100+i)*2) stop 1
if (mod(i,2).eq.0 .and. x(i).ne.100+i) stop 2
end do

!$omp target
p = 0
!$omp end target

!$omp target update from(p(::3,::2))

do i=0,5
  do j=-5,-1
    if (mod(i,3).eq.0 .and. mod(j+5,2).eq.0) then
      if (p(i,j).ne.0) stop 3
    else
      if (p(i,j).eq.0) stop 4
    end if
  end do
end do

end subroutine foo
end program p
