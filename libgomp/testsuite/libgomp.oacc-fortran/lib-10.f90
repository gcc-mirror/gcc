! { dg-do run }

program main
  implicit none
  include "openacc_lib.h"

  integer, target :: a_3d_i(10, 10, 10)
  complex a_3d_c(10, 10, 10)
  real a_3d_r(10, 10, 10)

  integer i, j, k
  complex c
  real r
  integer, parameter :: i_size = sizeof (i)
  integer, parameter :: c_size = sizeof (c)
  integer, parameter :: r_size = sizeof (r)

  if (acc_get_num_devices (acc_device_nvidia) .eq. 0) call exit

  call acc_init (acc_device_nvidia)

  call set3d (.FALSE., a_3d_i, a_3d_c, a_3d_r)

  call acc_copyin (a_3d_i)
  call acc_copyin (a_3d_c)
  call acc_copyin (a_3d_r)

  if (acc_is_present (a_3d_i) .neqv. .TRUE.) STOP 1
  if (acc_is_present (a_3d_c) .neqv. .TRUE.) STOP 2
  if (acc_is_present (a_3d_r) .neqv. .TRUE.) STOP 3

  do i = 1, 10
    do j = 1, 10
      do k = 1, 10
        if (acc_is_present (a_3d_i(i, j, k), i_size) .neqv. .TRUE.) STOP 4
        if (acc_is_present (a_3d_c(i, j, k), i_size) .neqv. .TRUE.) STOP 5
        if (acc_is_present (a_3d_r(i, j, k), i_size) .neqv. .TRUE.) STOP 6
      end do
    end do
  end do

  call acc_shutdown (acc_device_nvidia)

contains

  subroutine set3d (clear, a_i, a_c, a_r)
  logical clear
  integer, dimension (:,:,:), intent (inout) :: a_i
  complex, dimension (:,:,:), intent (inout) :: a_c
  real, dimension (:,:,:), intent (inout) :: a_r

  integer i, j, k
  integer lb1, ub1, lb2, ub2, lb3, ub3

  lb1 = lbound (a_i, 1)
  ub1 = ubound (a_i, 1)

  lb2 = lbound (a_i, 2)
  ub2 = ubound (a_i, 2)

  lb3 = lbound (a_i, 3)
  ub3 = ubound (a_i, 3)

  do i = lb1, ub1
    do j = lb2, ub2
      do k = lb3, ub3
        if (clear) then
          a_i(i, j, k) = 0
          a_c(i, j, k) = cmplx (0.0, 0.0)
          a_r(i, j, k) = 0.0
        else
          a_i(i, j, k) = i
          a_c(i, j, k) = cmplx (i, j)
          a_r(i, j, k) = i
        end if
      end do
    end do
  end do

  end subroutine

end program
