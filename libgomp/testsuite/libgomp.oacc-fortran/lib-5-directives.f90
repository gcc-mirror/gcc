! { dg-do run }
!
! Port of lib-5.f90 replacing applicable runtime calls with directives.

program main
  use openacc
  implicit none

  integer n

  if (acc_get_num_devices (acc_device_nvidia) .ne. 0) then

     !$acc init device_type(nvidia)

     n = 0

     !$acc set device_type(nvidia) device_num(n)

     if (acc_get_device_num (acc_device_nvidia) .ne. 0) stop 11

     if (acc_get_num_devices (acc_device_nvidia) .gt. 1) then

        n = 1

        !$acc set device_type(nvidia) device_num(n)

        if (acc_get_device_num (acc_device_nvidia) .ne. 1) stop 12

     end if

     !$acc shutdown device_type(nvidia)

  end if

  if (acc_get_num_devices (acc_device_radeon) .ne. 0) then

     !$acc init device_type(radeon)

     n = 0

     !$acc set device_type(radeon) device_num(n)

     if (acc_get_device_num (acc_device_radeon) .ne. 0) stop 21

     if (acc_get_num_devices (acc_device_radeon) .gt. 1) then

        n = 1

        !$acc set device_type(radeon) device_num(n)

        if (acc_get_device_num (acc_device_radeon) .ne. 1) stop 22

     end if

     !$acc shutdown device_type(radeon)

  end if

end program
