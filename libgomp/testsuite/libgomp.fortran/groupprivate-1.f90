! { dg-do run { target offload_device_nvptx } }
! { dg-additional-options "-O0 -foffload=nvptx-none" }
!
! FIXME: Enable testing for AMD GCN once implemented
!
! The following code works if
! - either 'target teams' runs in sequence order (this seems to happen on the
!   host for 'target teams' but not for 'teams')
! - or, when teams run concurrently, if 'cnt' and 'local_cnt' are 'groupprivate'.
!
! The latter is check by this testcase.
!
! On nvptx, such memory is called 'shared' and it turns out that the address
! is the same for all teams. However, the value check only succeeds if it is
! indeed shared!  The produced nvptx assembly is:
!    .shared .align 4 .u32 local_cnt$1[1];
!    .visible .shared .align 4 .u32 __m_MOD_cnt[1];
!
module m
  use iso_c_binding
  use omp_lib
  implicit none

  integer :: cnt ! Note that no initializer is permitted!
  !$omp declare target local(cnt) device_type(nohost)
  !$omp groupprivate(cnt) device_type(nohost)

contains

  integer(c_ptrdiff_t) function addr()
    addr = loc(cnt)
  end

  integer function f_global(first)
    !$omp declare target enter(f_global) device_type(nohost)
    logical, value, intent(in) :: first

    if (first) then
      !$omp atomic write
      cnt = 0
    end if
    !$omp atomic update
    cnt = cnt + (omp_get_team_num () + 1);
    !$omp atomic read
    f_global = cnt
  end

  integer function f_local(first)
    !$omp declare target enter(f_local) device_type(nohost)
    logical, value, intent(in) :: first
    integer, save :: local_cnt
    !$omp groupprivate(local_cnt) device_type(nohost)

    if (first) then
      !$omp atomic write
      local_cnt = 5
    end if
    !$omp atomic update
    local_cnt = local_cnt + 2*(omp_get_team_num () + 1);
    !$omp atomic read
    f_local = local_cnt
  end
end module m

program main
  use m
  implicit none
  integer :: team_global(16), team_local(16), j, num_teams
  integer(c_ptrdiff_t) :: addrs(16)

  ! !$omp teams num_teams(16)
  !$omp target teams num_teams(16) map(from: team_global, team_local, num_teams) device_type(nohost)
  block
    !$omp parallel if(.false.)
    block
      integer :: i
      real, volatile :: x
      x = 3.3
      i = f_global(.true.)
      i = f_local(.true.)
      addrs(1+omp_get_team_num()) = addr()
      x = sin(x)
      team_global(1+omp_get_team_num()) = f_global(.false.)
      team_local(1+omp_get_team_num()) = f_local(.false.)
      if (omp_get_team_num() == 0) &
        num_teams = omp_get_num_teams ()
   end block
  end block

  if (num_teams /= 16) error stop "num teams error"

 do j = 1, 16
   if (team_global(j) /= j*2 .or. team_local(j) /= 5+j*4) then
     print '(i2,": ", 4(i6, " "), g0, " ", g0, " - ", z16)', j,&
           team_global(j), team_local(j), j*2,  5+j*4, &
           team_global(j)==j*2, team_local(j)==5+j*4, addrs(j)
     error stop "invalid value"
   end if
 end do
end
