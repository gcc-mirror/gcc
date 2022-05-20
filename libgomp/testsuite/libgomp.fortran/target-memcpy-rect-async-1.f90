! Test for omp_target_memcpy_rect_async without considering dependence objects.

program main
  use omp_lib
  use iso_c_binding
  implicit none (external, type)
  integer :: d, id, i, j
  integer, target :: q(0:127), q2(0:127)
  type(c_ptr) :: p
  integer(omp_depend_kind) :: obj(1:0)

  integer(kind=c_size_t) :: volume(0:2)
  integer(kind=c_size_t) :: dst_offsets(0:2)
  integer(kind=c_size_t) :: src_offsets(0:2)
  integer(kind=c_size_t) :: dst_dimensions(0:2)
  integer(kind=c_size_t) :: src_dimensions(0:2)
  integer(kind=c_size_t) :: empty(1:0)

  d = omp_get_default_device ()
  id = omp_get_initial_device ()

  if (d < 0 .or. d >= omp_get_num_devices ()) &
    d = id

  p = omp_target_alloc (130 * c_sizeof (q), d)
  if (.not. c_associated (p)) &
    stop 0  ! okay

  if (omp_target_memcpy_rect_async (C_NULL_PTR, C_NULL_PTR, 0_c_size_t, 0, &
                                    empty, empty, empty, empty,  empty, d, id, &
                                    0, obj) < 3 &
     .or. omp_target_memcpy_rect_async (C_NULL_PTR, C_NULL_PTR, 0_c_size_t, 0, &
                                        empty, empty, empty, empty, empty, &
                                        id, d, 0, obj) < 3 &
     .or. omp_target_memcpy_rect_async (C_NULL_PTR, C_NULL_PTR, 0_c_size_t, 0, &
                                        empty, empty, empty, empty, empty, &
                                        id, id, 0, obj) < 3) &
    stop 1

  q = [(0, i = 0, 127)]
  if (omp_target_memcpy (p, c_loc (q), 128 * sizeof (q(0)), 0_c_size_t, &
                         0_c_size_t, d, id) /= 0) &
    stop 2

  q = [(i+1, i = 0, 127)]

  volume(2) = 3
  volume(1) = 2
  volume(0) = 1
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

  if (omp_target_memcpy_rect_async (p, c_loc (q), sizeof (q(0)), 3, volume, &
      dst_offsets, src_offsets, dst_dimensions, src_dimensions, d, id, 0, &
      obj) /= 0) &
    stop 3

  !$omp taskwait

  q2 = [(0, i = 0, 127)]
  if (omp_target_memcpy (c_loc (q2), p, 128 * sizeof (q2(0)), 0_c_size_t, &
                         0_c_size_t, id, d) /= 0) &
    stop 4

  ! q2 is expected to contain: 1 2 3 0 0 5 6 7 0 0 .. 0
  if (q2(0) /= 1 .or. q2(1) /= 2 .or. q2(2) /= 3 .or. q2(3) /= 0 &
      .or. q2(4) /= 0 .or. q2(5) /= 5 .or. q2(6) /= 6 .or. q2(7) /= 7) &
    stop 5

  do j = 8, 127
    if (q2(j) /= 0) &
      stop 6
  end do

  call omp_target_free (p, d)
end program main
