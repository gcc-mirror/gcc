! { dg-do run }
! { dg-require-effective-target omp_managedmem }
! { dg-additional-options -foffload-options=amdgcn-amdhsa=-mxnack=on { target offload_target_amdgcn_with_xnack } }

! Check that omp_alloc can allocate Managed Memory, and that host and target
! can see the data, at the same address, without a mapping.

program main
  use omp_lib
  use iso_c_binding
  implicit none

  type(c_ptr) :: cptr
  integer, pointer :: a
  integer(c_intptr_t) :: a_p, a_p2

  cptr = omp_alloc(c_sizeof(a), ompx_gnu_managed_mem_alloc)
  if (.not. c_associated(cptr)) stop 1

  call c_f_pointer(cptr, a)
  a = 42
  a_p = transfer(c_loc(a), a_p)

  !$omp target is_device_ptr(a)
    a_p2 = transfer(c_loc(a), a_p2)
    if (a /= 42 .or. a_p /= a_p2) stop 2
  !$omp end target

  call omp_free(cptr, ompx_gnu_managed_mem_alloc)
end program main
