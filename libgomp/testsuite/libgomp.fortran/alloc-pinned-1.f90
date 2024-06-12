! Ensure that the ompx_gnu_pinned_mem_alloc predefined allocator is present and
! accepted.  The majority of the functionality testing lives in the C tests.
!
! { dg-xfail-run-if "Pinning not implemented on this host" { ! *-*-linux-gnu* } }

program main
  use omp_lib
  use ISO_C_Binding
  implicit none (external, type)

  type(c_ptr) :: p

  p = omp_alloc (10_c_size_t, ompx_gnu_pinned_mem_alloc);
  if (.not. c_associated (p)) stop 1
  call omp_free (p, ompx_gnu_pinned_mem_alloc);
end program main
