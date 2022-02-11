! PR90030.
! Test if the array data associated with c is properly aligned
! on the accelerator.  If it is not, this program will crash.

! This is also included from '../libgomp.fortran/pr90030.f90'.

! { dg-do run }

program routine_align_main
  implicit none
  integer :: i, n
  real*8, dimension(:), allocatable :: c

  n = 10

  allocate (c(n))

  !$omp target map(to: n) map(from: c(1:n))
  !$acc parallel copyin(n) copyout(c(1:n))
  do i = 1, n
     c(i) = i
  enddo
  !$acc end parallel
  !$omp end target

  do i = 1, n
     if (c(i) .ne. i) stop i
  enddo
end program routine_align_main
