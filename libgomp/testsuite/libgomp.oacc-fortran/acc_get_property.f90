! Test the `acc_get_property' and '`acc_get_property_string' library
! functions by printing the results of those functions for all devices
! of all device types mentioned in the OpenACC standard.
!
! See also acc_get_property.c

program test
  use openacc
  implicit none

  print *, "acc_device_none:"
  ! For completeness; not expected to print anything
  call print_device_properties (acc_device_none)

  print *, "acc_device_default:"
  call print_device_properties (acc_device_default)

  print *, "acc_device_host:"
  call print_device_properties (acc_device_host)

  print *, "acc_device_not_host:"
  call print_device_properties (acc_device_not_host)
end program test

! Print the values of the properties of all devices of the given type
! and do basic device independent validation.
subroutine print_device_properties (device_type)
  use openacc
  use iso_c_binding, only: c_size_t
  implicit none

  integer, intent(in) :: device_type

  integer :: device_count
  integer :: device
  integer(c_size_t) :: v
  character*256 :: s

  device_count = acc_get_num_devices(device_type)

  do device = 0, device_count - 1
     print "(a, i0)", "  Device ", device

     call acc_get_property_string (device, device_type, acc_property_vendor, s)
     print "(a, a)", "    Vendor: ", trim (s)
     if (s == "") then
        print *, "acc_property_vendor should not be empty."
        stop 1
     end if

     v = acc_get_property (device, device_type, acc_property_memory)
     print "(a, i0)", "    Total memory: ", v
     if (v < 0) then
        print *, "acc_property_memory should not be negative."
        stop 1
     end if

     v = acc_get_property (device, device_type, acc_property_free_memory)
     print "(a, i0)", "    Free memory: ", v
     if (v < 0) then
        print *, "acc_property_free_memory should not to be negative."
        stop 1
     end if

     v = acc_get_property (device, device_type, int(2360, kind = acc_device_property))
     if (v /= 0) then
        print *, "Value of unknown numeric property should be 0."
        stop 1
     end if

     call acc_get_property_string (device, device_type, acc_property_name, s)
     print "(a, a)", "    Name: ", trim (s)
     if (s == "") then
        print *, "acc_property_name should not be empty."
        stop 1
     end if

     call acc_get_property_string (device, device_type, acc_property_driver, s)
     print "(a, a)", "    Driver: ", trim (s)
     if (s == "") then
        print *, "acc_property_driver should not be empty."
        stop 1
     end if

     call acc_get_property_string (device, device_type, int(4060, kind = acc_device_property), s)
     if (s /= "") then
        print *, "Value of unknown string property should be empty string."
        stop 1
     end if

  end do
end subroutine print_device_properties
