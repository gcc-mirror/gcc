! Check whether absent optional arguments are properly
! handled with use_device_{addr,ptr}.
program main
 implicit none (type, external)
 call foo()
contains
  subroutine foo(v, w, x, y, z)
    integer, target, optional, value :: v
    integer, target, optional :: w
    integer, target, optional :: x(:)
    integer, target, optional, allocatable :: y
    integer, target, optional, allocatable :: z(:)
    integer :: d

    !$omp target data map(d) use_device_addr(v, w, x, y, z)
      if(present(v)) stop 1
      if(present(w)) stop 2
      if(present(x)) stop 3
      if(present(y)) stop 4
      if(present(z)) stop 5
    !$omp end target data

! Using 'v' in use_device_ptr gives an ICE
! TODO: Find out what the OpenMP spec permits for use_device_ptr

    !$omp target data map(d) use_device_ptr(w, x, y, z)
      if(present(w)) stop 6
      if(present(x)) stop 7
      if(present(y)) stop 8
      if(present(z)) stop 9
    !$omp end target data
  end subroutine foo
end program main
