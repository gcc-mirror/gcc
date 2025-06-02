! PR libgomp/120444

use omp_lib
use iso_c_binding
implicit none (type, external)

integer(c_int) :: dev, i, val, start, tail
type(c_ptr) :: ptr, ptr2, tmpptr
integer(c_int8_t), pointer, contiguous :: fptr(:)
integer(c_intptr_t) :: intptr
integer(c_size_t), parameter :: count = 1024

do dev = omp_initial_device, omp_get_num_devices ()
  ptr = omp_target_alloc (count, dev)

  ! Play also around with the alignment - as hsa_amd_memory_fill operates
  ! on multiples of 4 bytes (c_int32_t)

  do start = 0, 31
    do tail = 0, 31
      val = iachar('0') + start + tail

      tmpptr = transfer (transfer (ptr, intptr) + start, tmpptr)
      ptr2 = omp_target_memset (tmpptr, val, count - start - tail, dev)

      if (.not. c_associated (tmpptr, ptr2)) stop 1

      !$omp target device(dev) is_device_ptr(ptr)
        do i = 1 + start, int(count, c_int) - start - tail
          call c_f_pointer (ptr, fptr, [count])
          if (fptr(i) /= int (val, c_int8_t)) stop 2
        end do
      !$omp end target
    end do
  end do

  call omp_target_free (ptr, dev);
end do
end
