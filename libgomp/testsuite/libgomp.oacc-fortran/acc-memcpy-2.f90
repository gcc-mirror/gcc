! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

! Fortran version of libgomp.oacc-c-c++-common/lib-95.c

program main
  use iso_c_binding
  use openacc
  implicit none (type, external)

  integer(c_size_t), parameter :: N = 127
  integer(c_size_t) :: i
  integer(acc_handle_kind) :: q = 5
  integer(kind=1), allocatable :: h(:), g(:)
  type(c_ptr) :: d

  q = 5

  allocate (h(-N:N), g(-N:N))
  do i = -N, N
    g(i) = i
  end do

  call acc_create_async (h, 2*N + 1, q)

  call acc_memcpy_to_device_async (acc_deviceptr (h), g, 2*N + 1, q)

  call acc_wait (q)

  h = 0

  call acc_update_self_async (h, 2*N + 1, q + 1)
  call acc_delete_async (h, 2*N + 1, q + 1)

  call acc_wait (q + 1)

  do i = -N, N
    if (h(i) /= i) &
      stop 1
  end do
  deallocate (h, g)
end
