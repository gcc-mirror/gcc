! Ensure that write on the offload device works.

! { dg-do run }
! { dg-output "The answer is 42(\n|\r\n|\r)+" }

program main
  implicit none
  integer :: var = 42

!$omp target 
  write (0, '("The answer is ", I2)') var
!$omp end target

end program main
