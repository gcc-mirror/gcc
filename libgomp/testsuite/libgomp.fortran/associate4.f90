! PR fortran/51722

module m
implicit none

contains

subroutine seltype

type :: a
   integer :: p = 2
end type a

type, extends(a) :: b
   integer :: cnt = 0
end type b

integer :: k, s
class(a), pointer :: x

allocate(a :: x)
s = 0
select type (y => x)
class is (a)
!$omp parallel do default(shared) private(k) reduction(+:s)
   do k = 1,10
      s = s + k*y%p
   end do
!$omp end parallel do
end select

if (s /= 110) error stop
deallocate(x)
allocate(b :: x)

s = 0
select type (y => x)
class is (b)
!$omp parallel do default(shared) private(k) reduction(+:s)
   do k = 1,10
      s = s + k*y%p
!$omp atomic update
      y%cnt = y%cnt + 2
   end do
!$omp end parallel do
if (s /= 110) error stop
if (y%p /= 2) error stop
if (y%cnt /= 10*2) error stop
end select

deallocate(x)

end subroutine seltype

subroutine assoc

type :: b
   integer :: r = 3
end type b

type :: a
   integer :: p = 2
   class(b), pointer :: u => null()
end type a

integer :: k, s
class(a), pointer :: x

s = 0
allocate(a :: x)
allocate(b :: x%u)

associate(f => x%u)
!$omp parallel do default(shared) private(k) reduction(+:s)
   do k = 1,10
      s = s + k*f%r
   end do
!$omp end parallel do
end associate

deallocate(x%u)
deallocate(x)

if (s /= 165) error stop
end subroutine assoc
end module m

use m
implicit none (type, external)
call seltype
call assoc
end
