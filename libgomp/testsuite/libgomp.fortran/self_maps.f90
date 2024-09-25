! Basic test whether self_maps work

module m
  !$omp requires self_maps
  implicit none (type, external)
  type t
    integer :: val
    type(t), pointer :: next
  end type t
contains
  subroutine init(p)
    integer :: i
    type(t), pointer :: p, x
    allocate(x)
    p => x
    do i = 1, 5
      x%val = i
      if (i < 5) then
        allocate(x%next)
        x => x%next
      end if
    end do
  end subroutine

  subroutine check(p)
    !$omp declare target enter(check)
    integer :: i
    type(t), pointer :: p, x
    x => p
    do i = 1, 5
      if (x%val /= i) stop 1
       x => x%next
    end do
end subroutine
end module

use omp_lib
use m
implicit none (type, external)
type(t), pointer :: linked
integer :: i

call init(linked)
do i = 0, omp_get_num_devices()
  !$omp target device(i)
    call check(linked)
  !$omp end target
end do
end
