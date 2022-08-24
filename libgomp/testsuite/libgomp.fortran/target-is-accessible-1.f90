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

  ! Currently, a host pointer is accessible if the device supports shared
  ! memory or omp_target_is_accessible is executed on the host. This
  ! test case must be adapted when unified shared memory is avialable.
  do d = 0, omp_get_num_devices ()
    shared_mem = 0;
    !$omp target map (alloc: shared_mem) device (d)
      shared_mem = 1;
    !$omp end target

    if (omp_target_is_accessible (p, c_sizeof (d), d) /= shared_mem) &
      stop 6;

    if (omp_target_is_accessible (c_loc (a), 128 * sizeof (a(1)), d) /= shared_mem) &
      stop 7;

    do i = 1, 128
      if (omp_target_is_accessible (c_loc (a(i)), sizeof (a(i)), d) /= shared_mem) &
        stop 8;
    end do

  end do

end program main
