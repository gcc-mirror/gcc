! { dg-do run }
!
! Test user_device_ptr nested within another parallel
! construct
!
program test_nested_use_device_ptr
  use iso_c_binding, only: c_loc, c_ptr
  implicit none
  real, allocatable, target :: arr(:,:)
  integer :: width = 1024, height = 1024, i
  type(c_ptr) :: devptr

  allocate(arr(width,height))

  !$omp target enter data map(alloc: arr)

  !$omp target data use_device_ptr(arr)
  devptr = c_loc(arr(1,1))
  !$omp end target data

  !$omp parallel default(none) shared(arr, devptr)
  !$omp single

  !$omp target data use_device_ptr(arr)
  call thing(c_loc(arr), devptr)
  !$omp end target data

  !$omp end single
  !$omp end parallel
  !$omp target exit data map(delete: arr)

contains

  subroutine thing(myarr, devptr)
    use iso_c_binding, only: c_ptr, c_associated
    implicit none
    type(c_ptr) :: myarr, devptr
    if (.not.c_associated(myarr, devptr)) stop 1
  end subroutine thing

end program
