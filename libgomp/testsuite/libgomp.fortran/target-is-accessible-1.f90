! { dg-do run }

program main
  use omp_lib
  use iso_c_binding
  implicit none (external, type)
  integer :: d, id, n, shared_mem, i, heap_accessible, stack_accessible
  integer, target :: test_accessible
  integer, allocatable, target :: p(:)
  integer, target :: a(1:128)
  integer(c_intptr_t) :: addr
  logical :: condition

  d = omp_get_default_device ()
  id = omp_get_initial_device ()
  n = omp_get_num_devices ()
  allocate (p(1))

  if (d < 0 .or. d >= n) &
    d = id

  if (omp_target_is_accessible (c_loc(p), c_sizeof(p(1)), n) == 0) &
    stop 1

  if (omp_target_is_accessible (c_loc(p), c_sizeof(p(1)), id) == 0) &
    stop 2

  if (omp_target_is_accessible (c_loc(p), c_sizeof(p(1)), omp_initial_device) == 0) &
    stop 3

  if (omp_target_is_accessible (c_loc(p), c_sizeof(p(1)), -6) /= 0) &  ! -6 = omp_default_device - 1
    stop 4

  if (omp_target_is_accessible (c_loc(p), c_sizeof(p(1)), n + 1) /= 0) &
    stop 5

  ! Currently, a host pointer is accessible if the device supports shared
  ! memory or omp_target_is_accessible is executed on the host. This
  ! test case must be adapted when unified shared memory is available.
  do d = 0, omp_get_num_devices ()
    ! Check if libgomp is treating the device as a shared memory device.
    shared_mem = 0
    !$omp target map (alloc: shared_mem) device (d)
      shared_mem = 1
    !$omp end target

    heap_accessible = shared_mem
    condition = omp_target_is_accessible (c_loc(p), c_sizeof(p(1)), d) /= shared_mem
    if (condition) then
      if (shared_mem /= 0) &
        stop 6

      ! shared_mem is false, but the memory is reading as accessible,
      ! so let's check that by reading it. We should not do so
      ! unconditionally because if it's wrong then we'll probably get
      ! a memory fault.
      p(1) = 123
      addr = transfer(c_loc(p), addr)

      !$omp target has_device_addr(p) map(from:heap_accessible) device(d)
        if (transfer(c_loc(p), addr) == addr .and. p(1) == 123) &
          heap_accessible = 1
      !$omp end target

      if (heap_accessible == 0) &
        stop 7
    end if

    stack_accessible = shared_mem
    condition = omp_target_is_accessible (c_loc(a), 128 * c_sizeof(a(1)), d) /= shared_mem
    if (condition) then
      if (shared_mem /= 0) &
        stop 8

      ! shared_mem is false, but the memory is reading as accessible,
      ! so let's check that by reading it. We should not do so
      ! unconditionally because if it's wrong then we'll probably get
      ! a memory fault.
      test_accessible = 123
      addr = transfer(c_loc(test_accessible), addr)

      !$omp target has_device_addr(test_accessible) map(from:stack_accessible) device(d)
        if (transfer(c_loc(test_accessible), addr) == addr &
            .and. test_accessible == 123) &
          stack_accessible = 1
      !$omp end target

      if (stack_accessible == 0) &
        stop 9
    end if

    print '(A,I0,A,I0,A,I0,A,I0)', &
      'device #', d, &
      ': shared_mem=', shared_mem, &
      ' heap_accessible=', heap_accessible, &
      ' stack_accessible=', stack_accessible

    ! omp_target_is_accessible returns false if *any* of the array is
    ! inaccessible, so we only check the aggregate result.
    ! (Varying access observed on amdgcn without xnack.)
    condition = .true.
    do i = 1, 128
      if (omp_target_is_accessible (c_loc(a(i)), c_sizeof(a(i)), d) == 0) &
        condition = .false.
    end do
    if (condition .neqv. stack_accessible /= 0) &
      stop 10
  end do

  deallocate (p)

end program main
