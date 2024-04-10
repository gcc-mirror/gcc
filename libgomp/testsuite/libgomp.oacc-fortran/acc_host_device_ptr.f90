! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

! Fortran version of libgomp.oacc-c-c++-common/lib-59.c

program main
  use iso_c_binding
  use openacc
  implicit none (type, external)

  integer(c_size_t), parameter :: N = 256
  character(c_char), allocatable, target :: h_data(:)
  type(c_ptr) :: dptr, dptr_t
  integer(c_intptr_t) :: iptr, i

  allocate(h_data(0:N))
  dptr = acc_malloc (N+1)

  call acc_map_data (h_data, dptr, N+1)

  ! The following assumes sizeof(void*) being the same on host and device:
  do i = 0, N
    dptr_t = transfer (transfer(dptr, iptr) + i, dptr_t)
    if (.not. c_associated (acc_hostptr (dptr_t), c_loc (h_data(i)))) &
      stop 1
    if (.not. c_associated (dptr_t, acc_deviceptr (h_data(i)))) &
      stop 2
  end do

  call acc_unmap_data (h_data)

  do i = 0, N
    dptr_t = transfer (transfer(dptr, iptr) + i, dptr_t)
    if (c_associated (acc_hostptr (dptr_t))) &
      stop 3
    if (c_associated (acc_deviceptr (h_data(i)))) &
      stop 4
  end do

  call acc_free (dptr)

  deallocate (h_data)
end
