! Test the host_data construct with optional arguments.
! Based on host_data-1.f90.

! { dg-do run }
! { dg-additional-options "-cpp" }

program test
  implicit none

  integer, target :: i
  integer, pointer :: ip, iph

  ! Assign the same targets
  ip => i
  iph => i

  call foo(iph)
  call foo(iph, ip)
contains
  subroutine foo(iph, ip)
    integer, pointer :: iph
    integer, pointer, optional :: ip

    !$acc data copyin(i)
    !$acc host_data use_device(ip)

    ! Test how the pointers compare inside a host_data construct
    if (present(ip)) then
#if ACC_MEM_SHARED
      if (.not. associated(ip, iph)) STOP 1
#else
      if (associated(ip, iph)) STOP 2
#endif
    end if

    !$acc end host_data
    !$acc end data
  end subroutine foo
end program test
