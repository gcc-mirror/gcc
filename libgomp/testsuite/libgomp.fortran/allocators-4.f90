! { dg-additional-options "-fopenmp-allocators" }
module m
implicit none
type t
  integer, allocatable :: Acomp, Bcomp(:)
  class(*), allocatable :: Ccomp, Dcomp(:)
end type t
contains

subroutine intout(c,d,e,f)
implicit none
class(t), intent(out) :: c,d(4)
class(t), allocatable, intent(out) :: e,f(:)
end

subroutine q(c,d,e,f)
implicit none
class(t) :: c,d(4)
class(t), allocatable :: e,f(:)
call intout(c,d,e,f)
end subroutine q

subroutine s
implicit none
type(t) :: xx
class(t), allocatable :: yy
integer :: i, iiiiii
i = 4
!$omp allocate
allocate(xx%Acomp, xx%Bcomp(4))
deallocate(xx%Acomp, xx%Bcomp)

!$omp allocate
allocate(integer :: xx%Ccomp, xx%Dcomp(4))
deallocate(xx%Ccomp, xx%Dcomp)

!$omp allocators allocate(yy)
allocate(t :: yy)

!$omp allocate
allocate(real :: xx%Ccomp, xx%Dcomp(4))
deallocate(xx%Ccomp, xx%Dcomp)

!$omp allocate
allocate(xx%Acomp, xx%Bcomp(4))
!$omp allocate
allocate(logical :: xx%Ccomp, xx%Dcomp(4))

iiiiii = 555
xx = t(1, [1,2])
end

end module

use m
call s
end
