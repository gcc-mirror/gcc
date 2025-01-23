program main
  use omp_lib
  implicit none (type, external)
  integer :: i, j, dev
  character(:), pointer :: str
  type t
    character(:), pointer :: str
  end type t
  type(t), allocatable :: strs(:)

  allocate(strs(0:omp_get_num_devices ()))

  do i = omp_invalid_device - 1, omp_get_num_devices () + 1
    str => omp_get_uid_from_device (i)
    dev = omp_get_device_from_uid (str)
! print *, i, str, dev
    if (i < omp_initial_device .or. i > omp_get_num_devices ()) then
      if (dev /= omp_invalid_device .or. associated(str)) &
        stop 1
      cycle
    end if
    if (.not. associated(str)) &
      stop 2
    if (i == omp_initial_device .or. i == omp_get_num_devices ()) then
      if ((dev /= omp_initial_device .and. dev /= omp_get_num_devices ()) &
          .or. str /= "OMP_INITIAL_DEVICE") & ! /* GCC impl. choice */
       stop 3
      dev = omp_get_num_devices ()
    else if (dev /= i .or. len(str) == 0) then
      stop 4
    end if
    strs(dev)%str => str

    block
      ! Check substring handling
      character(len=100) :: long_str
      integer :: dev2
      long_str = str // "ABCDEF"
      dev2 = omp_get_device_from_uid (long_str(1:len(str)))
      if (i == omp_initial_device .or. i == omp_get_num_devices ()) then
        if (dev2 /= omp_initial_device .and. dev2  /= omp_get_num_devices ()) &
          stop 5
      else if (dev /= dev2) then
        stop 6
      end if
    end block
  end do

  do i = 0, omp_get_num_devices () - 1
    do j = i + 1, omp_get_num_devices ()
      if (strs(i)%str == strs(j)%str) &
        stop 7
    end do
  end do
  deallocate (strs)
end
