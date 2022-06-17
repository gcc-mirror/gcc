! Test for omp_target_memcpy_async without considering dependence objects.

program main
  use omp_lib
  use iso_c_binding
  implicit none (external, type)
  integer :: d, id, i, j
  integer, target :: q(0:127), q2(0:127)
  type(c_ptr) :: p
  integer(omp_depend_kind) :: obj(1:0)

  d = omp_get_default_device ()
  id = omp_get_initial_device ()

  if (d < 0 .or. d >= omp_get_num_devices ()) &
    d = id

  p = omp_target_alloc (130 * c_sizeof (q), d)
  if (.not. c_associated (p)) &
    stop 0  ! okay

  q = [(i, i = 0, 127)]
  if (omp_target_memcpy_async (p, c_loc (q), 128 * sizeof (q(0)), 0_c_size_t, &
      0_c_size_t, d, id, 0, obj) /= 0) &
    stop 1

  !$omp taskwait

  q2 = [(0, i = 0, 127)]
  if (omp_target_memcpy_async (c_loc (q2), p, 128 * sizeof (q2(0)), 0_c_size_t,&
      0_c_size_t, id, d, 0, obj) /= 0) &
    stop 2

  !$omp taskwait

  do j = 0, 127
    if (q(j) /= q2(j)) &
      stop 3
  end do

  call omp_target_free (p, d)
end program main
