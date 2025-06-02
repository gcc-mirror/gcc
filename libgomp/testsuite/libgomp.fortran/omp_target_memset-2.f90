! PR libgomp/120444
! Async version

use omp_lib
use iso_c_binding
implicit none (type, external)
integer(c_int) :: dev

!$omp parallel do
do dev = omp_initial_device, omp_get_num_devices ()
block
  integer(c_int) :: i, val, start, tail
  type(c_ptr) :: ptr, ptr2, tmpptr
  integer(c_int8_t), pointer, contiguous :: fptr(:)
  integer(c_intptr_t) :: intptr
  integer(c_size_t), parameter :: count = 1024
  integer(omp_depend_kind) :: dep(1)

  ptr = omp_target_alloc (count, dev)

  !$omp depobj(dep(1)) depend(inout: ptr)

  ! Play also around with the alignment - as hsa_amd_memory_fill operates
  ! on multiples of 4 bytes (c_int32_t)

  do start = 0, 31
    do tail = 0, 31
      val = iachar('0') + start + tail

      tmpptr = transfer (transfer (ptr, intptr) + start, tmpptr)
      ptr2 = omp_target_memset_async (tmpptr, val, count - start - tail, dev, 0)

      if (.not. c_associated (tmpptr, ptr2)) stop 1

      !$omp taskwait

      !$omp target device(dev) is_device_ptr(ptr) depend(depobj: dep(1)) nowait
        do i = 1 + start, int(count, c_int) - start - tail
          call c_f_pointer (ptr, fptr, [count])
          if (fptr(i) /= int (val, c_int8_t)) stop 2
          fptr(i) = fptr(i) + 2_c_int8_t
        end do
      !$omp end target

      ptr2 = omp_target_memset_async (tmpptr, val + 3, &
                                      count - start - tail, dev, 1, dep)

      !$omp target device(dev) is_device_ptr(ptr) depend(depobj: dep(1)) nowait
        do i = 1 + start, int(count, c_int) - start - tail
          call c_f_pointer (ptr, fptr, [count])
          if (fptr(i) /= int (val + 3, c_int8_t)) stop 3
          fptr(i) = fptr(i) - 1_c_int8_t
        end do
      !$omp end target

      ptr2 = omp_target_memset_async (tmpptr, val - 3, &
                                      count - start - tail, dev, 1, dep)

      !$omp taskwait depend (depobj: dep(1))
    end do
  end do

  !$omp depobj(dep(1)) destroy
  call omp_target_free (ptr, dev);
end block
end do
end
