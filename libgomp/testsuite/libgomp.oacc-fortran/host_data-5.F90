! { dg-do run }
!
! Test if, if_present clauses on host_data construct.
!
! Fortran variant of 'libgomp.oacc-c-c++-common/host_data-7.c'.
!
program main
  use iso_c_binding
  implicit none
  real, target :: var, arr(100)
  integer(c_intptr_t) :: host_p, host_parr
  host_p = transfer(c_loc(var), host_p)
  host_parr = transfer(c_loc(arr), host_parr)
  call foo (var, arr, host_p, host_parr, .false.)
  call foo (var, arr, host_p, host_parr, .true.)

contains

subroutine foo (p2, parr, host_p, host_parr, cond)
  use openacc
  implicit none
  real, target, intent(in) :: parr(:), p2
  integer(c_intptr_t), value, intent(in) :: host_p, host_parr
  logical, value, intent(in) :: cond
  real, pointer :: p
  p => p2

  if (host_p /= transfer(c_loc(p), host_p)) stop 1
  if (host_parr /= transfer(c_loc(parr), host_parr)) stop 2
#if !ACC_MEM_SHARED
  if (acc_is_present(p, c_sizeof(p))) stop 3
  if (acc_is_present(parr, 1)) stop 4
#endif
  
  !$acc data copyin(host_p, host_parr)
#if !ACC_MEM_SHARED
    if (acc_is_present(p, c_sizeof(p))) stop 5
    if (acc_is_present(parr, 1)) stop 6
#endif
    !$acc host_data use_device(p, parr) if_present
      ! not mapped yet, so it will be equal to the host pointer.
      if (transfer(c_loc(p), host_p) /= host_p) stop 7
      if (transfer(c_loc(parr), host_parr) /= host_parr) stop 8
    !$acc end host_data
#if !ACC_MEM_SHARED
    if (acc_is_present(p, c_sizeof(p))) stop 9
    if (acc_is_present(parr, 1)) stop 10
#endif

    !$acc data copy(p, parr)
      if (.not. acc_is_present(p, c_sizeof(p))) stop 11
      if (.not. acc_is_present(parr, 1)) stop 12
      ! Not inside a host_data construct, so still the host pointer.
      if (transfer(c_loc(p), host_p) /= host_p) stop 13
      if (transfer(c_loc(parr), host_parr) /= host_parr) stop 14
      
      !$acc host_data use_device(p, parr)
#if ACC_MEM_SHARED
        if (transfer(c_loc(p), host_p) /= host_p) stop 15
        if (transfer(c_loc(parr), host_parr) /= host_parr) stop 16
#else
        ! The device address is different from host address.
        if (transfer(c_loc(p), host_p) == host_p) stop 17
        if (transfer(c_loc(parr), host_parr) == host_parr) stop 18
#endif
      !$acc end host_data

      !$acc host_data use_device(p, parr) if_present
#if ACC_MEM_SHARED
        if (transfer(c_loc(p), host_p) /= host_p) stop 19
        if (transfer(c_loc(parr), host_parr) /= host_parr) stop 20
#else
        ! is present now, so this is the same as above.
        if (transfer(c_loc(p), host_p) == host_p) stop 21
        if (transfer(c_loc(parr), host_parr) == host_parr) stop 22
#endif
      !$acc end host_data

      !$acc host_data use_device(p, parr) if(cond)
#if ACC_MEM_SHARED
        if (transfer(c_loc(p), host_p) /= host_p) stop 23
        if (transfer(c_loc(parr), host_parr) /= host_parr) stop 24
#else
        ! is the device pointer iff cond is true.
        if ((transfer(c_loc(p), host_p) /= host_p) .neqv. cond) stop 25
        if ((transfer(c_loc(parr), host_parr) /= host_parr) .neqv. cond) stop 26
#endif
      !$acc end host_data
    !$acc end data
  !$acc end data
end subroutine foo
end
