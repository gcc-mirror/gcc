program main
  use omp_lib
  use iso_c_binding
  implicit none (external, type)
  integer :: d, id
  type(c_ptr) :: p, p1, p2
  integer, target :: a(1:0), b(1:2)

  d = omp_get_default_device ()
  id = omp_get_initial_device ()

  if (d < 0 .or. d >= omp_get_num_devices ()) &
    d = id

  p = omp_target_alloc (c_sizeof (c_int), d)
  if (.not. c_associated (p)) &
    stop 0  ! okay

  if (omp_target_associate_ptr (c_loc (a), p, c_sizeof (c_int), &
                                0_c_size_t, d) == 0) then

  if(.not. c_associated (omp_get_mapped_ptr (c_loc (a), d), p)) &
    stop 1

  if (omp_target_disassociate_ptr (c_loc (a), d) /= 0) &
    stop 2

  if(c_associated (omp_get_mapped_ptr (c_loc (a), d))) &
    stop 3

  !$omp target data map(alloc: a) device(d)
    !$omp target map(from: p1) map(alloc: a) device(d)
    p1 = c_loc (a);
    !$omp end target
    if (c_associated (omp_get_mapped_ptr (c_loc (a), d))) &
      stop 4
  !$omp end target data

  !$omp target data map(alloc: b(1:0)) device(d)
    !$omp target map(from: p2) map(alloc: b(1:0)) device(d)
    p2 = c_loc (b(1));
    !$omp end target
    if (c_associated (omp_get_mapped_ptr (c_loc (b(1)), d))) &
      stop 5
  !$omp end target data
  end if
  call omp_target_free (p, d)
end program main
