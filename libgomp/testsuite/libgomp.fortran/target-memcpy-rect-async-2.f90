! Test for omp_target_memcpy_rect_async considering dependence objects.

program main
  use omp_lib
  use iso_c_binding
  implicit none (external, type)
  integer :: d, id, i, j
  integer, target :: a(0:127), b(0:63), c(0:127), e(0:15), q(0:127)
  type(c_ptr) :: p
  integer(omp_depend_kind) :: obj(0:2)

  integer(kind=c_size_t) :: volume(0:2)
  integer(kind=c_size_t) :: dst_offsets(0:2)
  integer(kind=c_size_t) :: src_offsets(0:2)
  integer(kind=c_size_t) :: dst_dimensions(0:2)
  integer(kind=c_size_t) :: src_dimensions(0:2)

  d = omp_get_default_device ()
  id = omp_get_initial_device ()

  if (d < 0 .or. d >= omp_get_num_devices ()) &
    d = id

  p = omp_target_alloc (130 * c_sizeof (q), d)
  if (.not. c_associated (p)) &
    stop 0  ! okay

  a = [(42, i = 0, 127)]
  b = [(24, i = 0, 63)]
  c = [(0, i = 0, 127)]
  e = [(77, i = 0, 15)]

  volume(2) = 3
  volume(1) = 2
  volume(0) = 2
  dst_offsets(2) = 0
  dst_offsets(1) = 0
  dst_offsets(0) = 0
  src_offsets(2) = 0
  src_offsets(1) = 0
  src_offsets(0) = 0
  dst_dimensions(2) = 5
  dst_dimensions(1) = 4
  dst_dimensions(0) = 3
  src_dimensions(2) = 4
  src_dimensions(1) = 3
  src_dimensions(0) = 2

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
    do j = 0, 127
      c(j) = j + 1
    end do
    !$omp end task

    !$omp depobj(obj(0)) depend(inout: p)
    !$omp depobj(obj(1)) depend(in: c)

    ! This produces: 1 2 3 - - 5 6 7 - - at positions 0..9 and
    !                13 14 15 - - 17 18 19 - - at positions 20..29.
    if (omp_target_memcpy_rect_async (p, c_loc (c), sizeof (c(0)), 3, volume, &
                                      dst_offsets, src_offsets, &
                                      dst_dimensions, src_dimensions, d, id, &
                                      2, obj) /= 0) &
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
    if (q(j) /= 77) &
      stop 6
  end do

  if (q(20) /= 13 .or. q(21) /= 14 .or. q(22) /= 15 .or. q(25) /= 17 &
      .or. q(26) /= 18 .or. q(27) /= 19) &
    stop 7

  do j = 28, 63
    if (q(j) /= 24) &
      stop 8
  end do

  do j = 64, 127
    if (q(j) /= 42) &
      stop 9
  end do

  call omp_target_free (p, d)
end program main
