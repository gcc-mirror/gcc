module m
   use iso_c_binding
   implicit none
contains
   subroutine s(x,y,z)
      type(c_ptr), optional :: x
      integer, pointer, optional :: y
      integer, allocatable, optional :: z
      logical is_present, is_null
      is_present = present(x)
      if (is_present) &
        is_null = .not. c_associated(x)

      !$omp target is_device_ptr(x) has_device_addr(y) has_device_addr(z)
        if (is_present) then
          if (is_null) then
            if (c_associated(x)) stop 1
            if (associated(y)) stop 2
            if (allocated(z)) stop 3
          else
            if (.not. c_associated(x, c_loc(y))) stop 4
            if (y /= 7) stop 5
            if (z /= 9) stop 6
          end if
        end if
      !$omp end target
   end
end

use m
implicit none
integer, pointer :: p
integer, allocatable :: a
p => null()
call s()
!$omp target data map(p,a) use_device_addr(p,a)
  call s(c_null_ptr, p, a)
!$omp end target data
allocate(p,a)
p = 7
a = 9
!$omp target data map(p,a) use_device_addr(p,a)
  call s(c_loc(p), p, a)
!$omp end target data
deallocate(p,a)
end
