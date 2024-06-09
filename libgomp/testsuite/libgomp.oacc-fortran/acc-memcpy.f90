! { dg-do run }
! { dg-skip-if "" { *-*-* } { "*" } { "-DACC_MEM_SHARED=0" } }

! based on libgomp.oacc-c-c++-common/lib-60.c

program main
  use openacc
  use iso_fortran_env
  use iso_c_binding
  implicit none (type, external)
  integer(int8), allocatable :: char(:)
  type(c_ptr) :: dptr
  integer(c_intptr_t) :: i

  allocate(char(-128:127))
  do i = -128, 127
    char(i) = int (i, int8)
  end do

  dptr = acc_malloc (256_c_size_t)
  call acc_memcpy_to_device (dptr, char, 256_c_size_t)

  do i = 0, 255
    if (acc_is_present (transfer (transfer(char, i) + i, dptr), 1)) &
      stop 1
  end do

  char = 0_int8

  call acc_memcpy_from_device (char, dptr, 256_c_size_t)

  do i = -128, 127
    if (char(i) /= i) &
      stop 2
  end do

  do i = 0, 255
    if (acc_is_present (transfer (transfer(char, i) + i, dptr), 1)) &
      stop 3
  end do

  call acc_free (dptr)

  deallocate (char)
end
