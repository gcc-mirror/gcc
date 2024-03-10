program main
  use omp_lib
  use iso_c_binding
  implicit none (external, type)
  integer :: d, id, i, j, k, l
  logical :: err
  integer, target :: q(0:127)
  type(c_ptr) :: p

  integer(kind=c_size_t) :: volume(0:2)
  integer(kind=c_size_t) :: dst_offsets(0:2)
  integer(kind=c_size_t) :: src_offsets(0:2)
  integer(kind=c_size_t) :: dst_dimensions(0:2)
  integer(kind=c_size_t) :: src_dimensions(0:2)
  integer(kind=c_size_t) :: empty(1:0)

  err = .false.
  d = omp_get_default_device ()
  id = omp_get_initial_device ()

  if (d < 0 .or. d >= omp_get_num_devices ()) &
    d = id

  q = [(i, i = 0, 127)]
  p = omp_target_alloc (130 * c_sizeof (q), d)
  if (.not. c_associated (p)) &
    stop 0  ! okay

  if (omp_target_memcpy_rect (C_NULL_PTR, C_NULL_PTR, 0_c_size_t, 0, &
                              empty, empty, empty, empty,  empty, d, id) < 3 &
      .or. omp_target_memcpy_rect (C_NULL_PTR, C_NULL_PTR, 0_c_size_t, 0, &
                                   empty, empty, empty, empty, empty, &
                                   id, d) < 3 &
      .or. omp_target_memcpy_rect (C_NULL_PTR, C_NULL_PTR, 0_c_size_t, 0, &
                                   empty, empty, empty, empty, empty, &
                                   id, id) < 3) &
    stop 1

  if (omp_target_associate_ptr (c_loc (q), p, 128 * c_sizeof (q(0)), &
                                c_sizeof (q(0)), d) == 0) then
    volume = [ 128, 0, 0 ]
    dst_offsets = [ 0, 0, 0 ]
    src_offsets = [ 1, 0, 0 ]
    dst_dimensions = [ 128, 0, 0 ]
    src_dimensions = [ 128, 0, 0 ]


    if (omp_target_associate_ptr (c_loc (q), p, 128 * sizeof (q(0)), &
                                  sizeof (q(0)), d) /= 0) &
      stop 2

    if (omp_target_is_present (c_loc (q), d) /= 1 &
        .or. omp_target_is_present (c_loc (q(32)), d) /= 1 &
        .or. omp_target_is_present (c_loc (q(127)), d) /= 1) &
      stop 3

    if (omp_target_memcpy (p, c_loc (q), 128 * sizeof (q(0)), sizeof (q(0)), &
                           0_c_size_t, d, id) /= 0) &
      stop 4

    i = 0
    if (d >= 0) i = d
    !$omp target if (d >= 0) device (i) map(alloc:q(0:31)) map(from:err)
      err = .false.
      do j = 0, 127
        if (q(j) /= j) then
          err = .true.
        else
          q(j) = q(j) + 4
        end if
      end do
    !$omp end target

    if (err) &
      stop 5

    if (omp_target_memcpy_rect (c_loc (q), p, sizeof (q(0)), 1, volume, &
                                dst_offsets, src_offsets, dst_dimensions, &
                                src_dimensions, id, d) /= 0) &
      stop 6

    do i = 0, 127
      if (q(i) /= i + 4) &
        stop 7
    end do

    volume(2) = 2
    volume(1) = 3
    volume(0) = 6
    dst_offsets(2) = 1
    dst_offsets(1) = 0
    dst_offsets(0) = 0
    src_offsets(2) = 1
    src_offsets(1) = 0
    src_offsets(0) = 3
    dst_dimensions(2) = 3
    dst_dimensions(1) = 3
    dst_dimensions(0) = 6
    src_dimensions(2) = 3
    src_dimensions(1) = 4
    src_dimensions(0) = 9

    if (omp_target_memcpy_rect (p, c_loc (q), sizeof (q(0)), 3, volume, &
                                dst_offsets, src_offsets, dst_dimensions, &
                                src_dimensions, d, id) /= 0) &
      stop 8

    i = 0
    if (d >= 0) i = d
    !$omp target if (d >= 0) device (i) map(alloc:q(1:32)) map(from:err)
      err = .false.
      do j = 0, 5
        do k = 0, 2
          do l = 0, 1
            if (q(j * 9 + k * 3 + l) /= 3 * 12 + 4 + 1 + l + k * 3 + j * 12) &
              err = .true.
          end do
        end do
      end do
    !$omp end target

    if (err) &
      stop 9
 
    if (omp_target_memcpy (p, p, 10 * sizeof (q(1)), 51 * sizeof (q(1)), &
                           111 * sizeof (q(1)), d, d) /= 0) &
      stop 10

    i = 0
    if (d >= 0) i = d
    !$omp target if (d >= 0) device (i) map(alloc:q(0:31)) map(from:err)
      err = .false.
      do j = 1, 9
        if (q(50+j) /= q(110 + j)) & 
          err = .true.
      end do
    !$omp end target

    if (err) &
      stop 11

    if (omp_target_disassociate_ptr (c_loc (q), d) /= 0) &
      stop 12
  end if

  call omp_target_free (p, d)
end program main
