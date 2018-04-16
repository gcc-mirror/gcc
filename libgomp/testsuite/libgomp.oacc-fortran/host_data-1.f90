! { dg-do run }
! { dg-additional-options "-cpp" }

program test
  implicit none

  integer, target :: i, arr(1000)
  integer, pointer :: ip, iph
  integer, contiguous, pointer :: parr(:), parrh(:)

  ! Assign the same targets
  ip => i
  parr => arr
  iph => i
  parrh => arr

  !$acc data copyin(i, arr)
  !$acc host_data use_device(ip, parr)

  ! Test how the pointers compare inside a host_data construct
#if ACC_MEM_SHARED
  if (.not. associated(ip, iph)) STOP 1
  if (.not. associated(parr, parrh)) STOP 2
#else
  if (associated(ip, iph)) STOP 3
  if (associated(parr, parrh)) STOP 4
#endif

  !$acc end host_data
  !$acc end data

end program test
