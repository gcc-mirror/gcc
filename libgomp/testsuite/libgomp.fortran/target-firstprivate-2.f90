! PR fortran/104949

module m
use omp_lib
implicit none (type, external)

contains
subroutine one
  integer, allocatable :: x(:)
  integer :: i

  do i = 1, omp_get_num_devices() + 1
    !$omp target firstprivate(x)
      if (allocated(x)) error stop
    !$omp end target
    if (allocated(x)) error stop
  end do

  do i = 1, omp_get_num_devices() + 1
    !$omp target firstprivate(x, i)
      if (allocated(x)) error stop
      x = [10,20,30,40] + i
      if (any (x /= [10,20,30,40] + i)) error stop
      ! This leaks memory!
      ! deallocate(x)
    !$omp end target
    if (allocated(x)) error stop
  end do

  x = [1,2,3,4]

  do i = 1, omp_get_num_devices() + 1
    !$omp target firstprivate(x, i)
      if (i <= 0) error stop
      if (.not.allocated(x)) error stop
      if (size(x) /= 4) error stop
      if (lbound(x,1) /= 1) error stop
      if (any (x /= [1,2,3,4])) error stop
      ! no reallocation, just malloced + assignment
      x = [10,20,30,40] + i
      if (any (x /= [10,20,30,40] + i)) error stop
      ! This leaks memory!
      ! deallocate(x)
    !$omp end target
    if (.not.allocated(x)) error stop
    if (size(x) /= 4) error stop
    if (lbound(x,1) /= 1) error stop
    if (any (x /= [1,2,3,4])) error stop
  end do
  deallocate(x)
end

subroutine two
  character(len=:), allocatable :: x(:)
  character(len=5)  :: str
  integer :: i

  str = "abcde" ! work around for PR fortran/91544
  do i = 1, omp_get_num_devices() + 1
    !$omp target firstprivate(x)
      if (allocated(x)) error stop
    !$omp end target
    if (allocated(x)) error stop
  end do

  do i = 1, omp_get_num_devices() + 1
    !$omp target firstprivate(x, i)
      if (allocated(x)) error stop
      ! no reallocation, just malloced + assignment
      x = [character(len=2+i) :: str,"fhji","klmno"]
      if (len(x) /= 2+i) error stop
      if (any (x /= [character(len=2+i) :: str,"fhji","klmno"])) error stop
      ! This leaks memory!
      ! deallocate(x)
    !$omp end target
    if (allocated(x)) error stop
  end do

  x = [character(len=4) :: "ABCDE","FHJI","KLMNO"]

  do i = 1, omp_get_num_devices() + 1
    !$omp target firstprivate(x, i)
      if (i <= 0) error stop
      if (.not.allocated(x)) error stop
      if (size(x) /= 3) error stop
      if (lbound(x,1) /= 1) error stop
      if (len(x) /= 4) error stop
      if (any (x /= [character(len=4) :: "ABCDE","FHJI","KLMNO"])) error stop
      !! Reallocation runs into the issue PR fortran/105538
      !!
      !!x = [character(len=2+i) :: str,"fhji","klmno"]
      !!if (len(x) /= 2+i) error stop
      !!if (any (x /= [character(len=2+i) :: str,"fhji","klmno"])) error stop
      !! This leaks memory!
      !! deallocate(x)
      ! Just assign:
      x = [character(len=4) :: "abcde","fhji","klmno"]
      if (any (x /= [character(len=4) :: "abcde","fhji","klmno"])) error stop
    !$omp end target
    if (.not.allocated(x)) error stop
    if (lbound(x,1) /= 1) error stop
    if (size(x) /= 3) error stop
    if (len(x) /= 4) error stop
    if (any (x /= [character(len=4) :: "ABCDE","FHJI","KLMNO"])) error stop
  end do
  deallocate(x)
end
end module m

use m
call one
call two
end
