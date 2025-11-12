program main
  use omp_lib
  implicit none (type, external)
  integer :: dev, def_dev

  if (omp_default_device >= -1 .or. omp_default_device == omp_invalid_device) &
    error stop 1

  dev = -99
  def_dev = omp_get_default_device ()
  !$omp target map(from: dev) device(omp_default_device)
    dev = omp_get_device_num ()
  !$omp end target

  if (.not.is_same_dev (def_dev, dev)) &
    error stop 2

  do def_dev = omp_initial_device, omp_get_num_devices ()
  block
    character(:), pointer :: uid

    uid => omp_get_uid_from_device(def_dev)
    call omp_set_default_device (def_dev)
    dev = -99
    !$omp target map(from: dev) device(omp_default_device)
      dev = omp_get_device_num ()
    !$omp end target
    if (.not.is_same_dev (def_dev, dev)) &
      error stop 3

    ! Shall not modify the ICV.  */
    call omp_set_default_device (omp_default_device)
    if (def_dev /= omp_get_default_device ()) &
      error stop 4

    ! Assume the ptr and no only the string is the same.  */
    if (.not.associated(uid, omp_get_uid_from_device (omp_default_device))) &
      error stop 5
  end block
  end do

  call omp_set_default_device (omp_invalid_device)
  ! Shall not modify the ICV.
  call omp_set_default_device (omp_default_device)
  if (omp_invalid_device /= omp_get_default_device ()) &
    error stop 6

contains

  logical function is_same_dev (d1, d2)
    integer, value :: d1, d2
    integer :: num_dev

    num_dev = omp_get_num_devices ()
    if (d1 == omp_initial_device) &
      d1 = num_dev
    if (d2 == omp_initial_device) &
      d2 = num_dev
    is_same_dev = d1 == d2
  end function is_same_dev
end program
