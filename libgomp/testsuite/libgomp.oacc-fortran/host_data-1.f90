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
  if (.not. associated(ip, iph)) call abort
  if (.not. associated(parr, parrh)) call abort
#else
  if (associated(ip, iph)) call abort
  if (associated(parr, parrh)) call abort
#endif

  !$acc end host_data
  !$acc end data

end program test
