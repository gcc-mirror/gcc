program main
  use omp_lib
  use iso_c_binding
  implicit none (external, type)
  integer :: d, id
  type(c_ptr) :: p
  integer, target :: q

  d = omp_get_default_device ()
  id = omp_get_initial_device ()

  if (d < 0 .or. d >= omp_get_num_devices ()) &
    d = id

  p = omp_target_alloc (c_sizeof (q), d)
  if (.not. c_associated (p)) &
    stop 0  ! okay

  if (omp_target_associate_ptr (c_loc (q), p, c_sizeof (q), &
                                0_c_size_t, d) == 0) then

    if(c_associated (omp_get_mapped_ptr (c_loc (q), -5))) &
      stop 1

    if(c_associated (omp_get_mapped_ptr (c_loc (q), &
                     omp_get_num_devices () + 1))) &
      stop 2

    if(.not. c_associated (omp_get_mapped_ptr (c_loc (q), id), c_loc (q))) &
      stop 3

    if(.not. c_associated (omp_get_mapped_ptr (c_loc (q), omp_initial_device), &
                           c_loc (q))) &
      stop 4

    if(.not. c_associated (omp_get_mapped_ptr (c_loc (q), d), p)) &
      stop 5

    if (omp_target_disassociate_ptr (c_loc (q), d) /= 0) &
      stop 6

    if(c_associated (omp_get_mapped_ptr (c_loc (q), d))) &
      stop 7
  end if

  call omp_target_free (p, d)
end program main
