! { dg-do run }

program main
  use openacc
  implicit none
  ! TODO Per PR96080, data types chosen so that we can create a
  ! "pointer object 'data_p'" on the device.
  integer, dimension(:), target :: data(1)
  integer, dimension(:), pointer :: data_p

  !TODO Per PR96080, not using OpenACC/Fortran runtime library routines.

  !$acc enter data create(data)
  data_p => data
  !$acc enter data copyin(data_p)

  !$acc enter data attach(data_p)
end program main
