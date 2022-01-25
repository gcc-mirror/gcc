! { dg-do run }

program main
  use omp_lib

  implicit none

  integer, parameter :: N = 100
  integer :: a(N)
  integer :: on_device_count = 0
  integer :: i

  do i = 1, N
    a(i) = i
  end do

  do i = 0, omp_get_num_devices ()
    on_device_count = on_device_count + f (a, i)
  end do

  if (on_device_count .ne. omp_get_num_devices ()) stop 1

  do i = 1, N
    if (a(i) .ne. 2 * i) stop 2;
  end do
contains
  integer function f (a, num)
    integer, intent(inout) :: a(N)
    integer, intent(in) :: num
    integer :: on_device
    integer :: i

    on_device = 0
    !$omp metadirective &
    !$omp&  when (target_device={device_num(num), kind("gpu")}: &
    !$omp&    target parallel do map(to: a(1:N)), map(from: on_device)) &
    !$omp&  default (parallel do private(on_device))
      do i = 1, N
        a(i) = a(i) + i
        on_device = 1
      end do
    f = on_device;
  end function
end program
