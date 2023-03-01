! PR middle-end/108546
!
module m
   use iso_c_binding
   implicit none
   type(c_ptr) :: p2, p3
contains
   subroutine s(x,y,z)
      type(c_ptr), optional :: x
      integer, pointer, optional :: y
      integer, allocatable, optional, target :: z
      logical is_present, is_null
      is_present = present(x)
      if (is_present) &
        is_null = .not. c_associated(x)

      !$omp target data use_device_ptr(x) use_device_addr(y) use_device_addr(z)
        if (is_present) then
          if (is_null) then
            if (c_associated(x)) stop 1
            if (associated(y)) stop 2
            if (allocated(z)) stop 3
          else
            if (.not. c_associated(x, p2)) stop 4
            if (.not. c_associated(c_loc(y), p2)) stop 5
            if (.not. c_associated(c_loc(z), p3)) stop 6
          end if
        end if
      !$omp end target data
   end
end

use m
implicit none
type(c_ptr) :: cp
integer, pointer :: p
integer, allocatable, target :: a
call s()
p => null()
call s(c_null_ptr, p, a)
allocate(p,a)
p = 7
a = 9
cp = c_loc(p)
!$omp target enter data map(to: cp, p, a)
!$omp target map(from: p2, p3)
  p2 = c_loc(p)
  p3 = c_loc(a)
!$omp end target
call s(cp, p, a)
!$omp target exit data map(delete: cp, p, a)
deallocate(p,a)
end
