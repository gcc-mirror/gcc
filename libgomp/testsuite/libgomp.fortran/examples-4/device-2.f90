! { dg-do run }
! { dg-require-effective-target offload_device }

program e_57_2
  use omp_lib, only: omp_is_initial_device, omp_get_num_devices
  integer, parameter :: N = 10
  integer :: i, num
  logical :: offload(N)
  num = omp_get_num_devices ()
  !$omp parallel do
  do i = 1, N
    !$omp target device(i-1) map(from: offload(i:i))
      offload(i) = omp_is_initial_device ()
    !$omp end target
  end do

  do i = 1, num
    if (offload(i)) stop 1
  end do

  do i = num+1, N
    if (.not. offload(i)) stop 2
  end do
end program
