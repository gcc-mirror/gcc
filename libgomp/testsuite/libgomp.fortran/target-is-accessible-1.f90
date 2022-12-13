! { dg-do run }

program main
  use omp_lib
  use iso_c_binding
  implicit none (external, type)
  integer :: d, id, n, shared_mem, i
  integer, target :: a(1:128)
  type(c_ptr) :: p

  d = omp_get_default_device ()
  id = omp_get_initial_device ()
  n = omp_get_num_devices ()

  if (d < 0 .or. d >= n) &
    d = id

  if (omp_target_is_accessible (p, c_sizeof (d), n) /= 1) &
    stop 1

  if (omp_target_is_accessible (p, c_sizeof (d), id) /= 1) &
    stop 2

  if (omp_target_is_accessible (p, c_sizeof (d), omp_initial_device) /= 1) &
    stop 3

  if (omp_target_is_accessible (p, c_sizeof (d), -5) /= 0) &
    stop 4

  if (omp_target_is_accessible (p, c_sizeof (d), n + 1) /= 0) &
    stop 5

  do d = 0, omp_get_num_devices ()
    ! SHARED_MEM is 1 if and only if host and device share the same memory.
    ! OMP_TARGET_IS_ACCESSIBLE should not return 0 for shared memory.
    shared_mem = 0;
    !$omp target map (alloc: shared_mem) device (d)
      shared_mem = 1;
    !$omp end target

    if (shared_mem == 1 .and. omp_target_is_accessible (p, c_sizeof (d), d) == 0) &
      stop 6;

    ! USM is disabled by default.  Hence OMP_TARGET_IS_ACCESSIBLE should
    ! return 0 if shared_mem is false.
    if (shared_mem == 0 .and. omp_target_is_accessible (p, c_sizeof (d), d) /= 0) &
      stop 7;

    if (shared_mem == 1 .and. omp_target_is_accessible (c_loc (a), 128 * sizeof (a(1)), d) == 0) &
      stop 8;

    do i = 1, 128
      if (shared_mem == 1 .and. omp_target_is_accessible (c_loc (a(i)), sizeof (a(i)), d) == 0) &
        stop 9;
    end do

  end do

end program main
