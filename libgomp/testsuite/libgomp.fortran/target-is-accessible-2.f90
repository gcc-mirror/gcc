! { dg-do run }
! { dg-require-effective-target omp_usm }

program main
  use omp_lib
  use iso_c_binding
  implicit none (external, type)
  integer :: d
  type(c_ptr) :: p

  !$omp requires unified_shared_memory

  p = omp_alloc (sizeof (d), ompx_unified_shared_mem_alloc)
  if (.not. c_associated (p)) stop 1

  do d = 0, omp_get_num_devices ()
    if (omp_target_is_accessible (p, c_sizeof (d), d) == 0) &
      stop 2;
  end do

  call omp_free (p, ompx_unified_shared_mem_alloc);
end program main
