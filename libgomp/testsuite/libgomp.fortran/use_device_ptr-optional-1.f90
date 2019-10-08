! Test whether use_device_ptr properly handles OPTIONAL arguments
! (Only case of present arguments is tested)
program test_it
  implicit none
  integer, target :: ixx
  integer, pointer :: ptr_i, ptr_null

  ptr_i => ixx
  call foo(ptr_i)

  ptr_null => null()
  call bar(ptr_null)
contains
  subroutine foo(ii)
    integer, pointer, optional :: ii

    if (.not.present(ii)) call abort()
    if (.not.associated(ii, ixx)) call abort()
    !$omp target data map(to:ixx) use_device_ptr(ii)
    if (.not.present(ii)) call abort()
    if (.not.associated(ii)) call abort()
    !$omp end target data
  end subroutine foo

  ! For bar, it is assumed that a NULL ptr on the host maps to NULL on the device
  subroutine bar(jj)
    integer, pointer, optional :: jj

    if (.not.present(jj)) call abort()
    if (associated(jj)) call abort()
    !$omp target data map(to:ixx) use_device_ptr(jj)
    if (.not.present(jj)) call abort()
   if (associated(jj)) call abort()
    !$omp end target data
  end subroutine bar
end program test_it
