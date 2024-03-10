! PR libgomp/109837

program main
  use iso_c_binding
  use iso_fortran_env
  use omp_lib
  implicit none (external, type)
  !$omp requires unified_address

  integer(c_intptr_t), parameter :: N = 15
  integer :: i, ntgts

  ntgts = omp_get_num_devices();
  if (ntgts > 0) then
    write (ERROR_UNIT, '(a)') "Offloading devices exist"  ! { dg-output "Offloading devices exist(\n|\r\n|\r)" { target offload_device } }
  else
    write (ERROR_UNIT, '(a)') "Only host fallback"      ! { dg-output "Only host fallback(\n|\r\n|\r)" { target { ! offload_device } } }
  endif

  do i = 0, ntgts
    call test_device (i);
  end do

contains

  subroutine test_device (dev)
    integer, value, intent(in) :: dev

    type t
      integer(c_intptr_t) :: n, m
      integer, pointer :: fptr(:)
      type(c_ptr) :: cptr      
    end type t
    type(t) :: s
    type(c_ptr) :: cptr, qptr, cptr2, cptr2a
    integer, target :: q(4)
    integer, pointer :: fptr(:)
    integer(c_intptr_t) :: i

    s%n = 10;
    s%m = 23;
    s%cptr = omp_target_alloc (s%n * NUMERIC_STORAGE_SIZE/CHARACTER_STORAGE_SIZE, dev);
    cptr = omp_target_alloc (s%m * NUMERIC_STORAGE_SIZE/CHARACTER_STORAGE_SIZE, dev);
    if (.not. c_associated(s%cptr)) stop 1
    if (.not. c_associated(cptr)) stop 2
    call c_f_pointer (cptr, s%fptr, [s%m])

    cptr = omp_target_alloc (N * NUMERIC_STORAGE_SIZE/CHARACTER_STORAGE_SIZE, dev);
    if (.not. c_associated(cptr)) stop 3

    q = [1, 2, 3, 4]
    !$omp target enter data map(q) device(device_num: dev)
    !$omp target data use_device_addr(q) device(device_num: dev)
       qptr = c_loc(q)
    !$omp end target data

    !$omp target map(to:s) device(device_num: dev)
    block
      integer, pointer :: iptr(:)
      call c_f_pointer(s%cptr, iptr, [s%n])
      do i = 1, s%n
        iptr(i) = 23 * int(i)
      end do
      do i = 1, s%m
        s%fptr(i) = 35 * int(i)
      end do
    end block

    cptr2 = c_loc(s%fptr(4))
    cptr2a = s%cptr

    !$omp target firstprivate(qptr) map(tofrom: cptr2) map(to :cptr2a) device(device_num: dev)
    block
      integer, pointer :: iptr(:), iptr2(:), qvar(:)
      call c_f_pointer(cptr2, iptr, [4])
      call c_f_pointer(cptr2a, iptr2, [4])
      call c_f_pointer(qptr, qvar, [4])
      qvar = iptr + iptr2
    end block

    !$omp target exit data map(q) device(device_num: dev)
    do i = 1, 4
      if (q(i) /= 23 * int(i)  +  35 * (int(i) + 4 - 1)) stop 4
    end do

    !$omp target map(to: cptr) device(device_num: dev)
    block
      integer, pointer :: p(:)
      call c_f_pointer(cptr, p, [N])
      do i = 1, N
        p(i) = 11 * int(i)
      end do
    end block

    allocate(fptr(N))
    if (0 /= omp_target_memcpy (c_loc(fptr), cptr,  &
                                N * NUMERIC_STORAGE_SIZE/CHARACTER_STORAGE_SIZE,  &
                                0_c_intptr_t, 0_c_intptr_t, &
                                omp_get_initial_device(), dev))  &
      stop 5

    do i = 1, N
      if (fptr(i) /= 11 * int(i)) stop 6
    end do

    deallocate (fptr);
    call omp_target_free (cptr, dev);
    call omp_target_free (s%cptr, dev);
    call omp_target_free (c_loc(s%fptr), dev);
  end
end
