! { dg-do run }
program stack
  implicit none
  integer id
  integer ilocs(2)
  integer omp_get_thread_num, foo
  call omp_set_num_threads (2)
!$omp parallel private (id)
  id = omp_get_thread_num() + 1
  ilocs(id) = foo()
!$omp end parallel
  ! Check that the two threads are not sharing a location for
  ! the array x in foo()
  if (ilocs(1) .eq. ilocs(2)) STOP 1
end program stack

integer function foo ()
  implicit none
  real x(100,100)
  foo = loc(x)
end function foo
