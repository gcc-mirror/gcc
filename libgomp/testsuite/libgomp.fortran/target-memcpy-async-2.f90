! Test for omp_target_memcpy_async considering dependence objects.

program main
  use omp_lib
  use iso_c_binding
  implicit none (external, type)
  integer :: d, id, i, j
  integer, target :: a(0:127), b(0:63), c(0:31), e(0:15), q(0:127)
  type(c_ptr) :: p
  integer(omp_depend_kind) :: obj(0:1)

  d = omp_get_default_device ()
  id = omp_get_initial_device ()

  if (d < 0 .or. d >= omp_get_num_devices ()) &
    d = id

  p = omp_target_alloc (130 * c_sizeof (q), d)
  if (.not. c_associated (p)) &
    stop 0  ! okay

  a = [(i + 1, i = 0, 127)]
  b = [(i + 2, i = 0, 63)]
  c = [(0, i = 0, 31)]
  e = [(i + 4, i = 0, 15)]

  !$omp parallel num_threads(5)
  !$omp single

    !$omp task depend(out: p)
    if (omp_target_memcpy (p, c_loc (a), 128 * sizeof (a(0)), 0_c_size_t, &
                           0_c_size_t, d, id) /= 0) &
      stop 1
    !$omp end task

    !$omp task depend(inout: p)
    if (omp_target_memcpy (p, c_loc (b), 64 * sizeof (b(0)), 0_c_size_t, &
                           0_c_size_t, d, id) /= 0) &
      stop 2
    !$omp end task

    !$omp task depend(out: c)
    do j = 0, 31
      c(j) = j + 3
    end do
    !$omp end task

    !$omp depobj(obj(0)) depend(inout: p)
    !$omp depobj(obj(1)) depend(in: c)
    if (omp_target_memcpy_async (p, c_loc (c), 32 * sizeof (c(0)), 0_c_size_t, &
                                 0_c_size_t, d, id, 2, obj) /= 0) &
      stop 3

    !$omp task depend(in: p)
    if (omp_target_memcpy (p, c_loc (e), 16 * sizeof (e(0)), 0_c_size_t, &
                           0_c_size_t, d, id) /= 0) &
      stop 4
    !$omp end task

  !$omp end single
  !$omp end parallel

  !$omp taskwait

  q = [(0, i = 0, 127)]
  if (omp_target_memcpy (c_loc (q), p, 128 * sizeof (q(0)), 0_c_size_t, &
                         0_c_size_t, id, d) /= 0) &
    stop 5

  do j = 0, 15
    if (q(j) /= j+4) &
      stop 10
  end do

  do j = 16, 31
    if (q(j) /= j+3) &
      stop 11
  end do

  do j = 32, 63
    if (q(j) /= j+2) &
      stop 12
  end do

  do j = 64, 127
    if (q(j) /= j+1) &
      stop 13
  end do

  call omp_target_free (p, d)
end program main
