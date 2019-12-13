! { dg-do run }
!
! PR fortran/47886
!
! Test case contributed by Bill Long

!  derived from OpenMP test OMP3f/F03_2_7_1d.F90
program F03_2_7_1d
   use omp_lib
   implicit none
   integer, parameter :: NT = 4
   integer :: sum = 0

   call omp_set_num_threads(NT); 

   !$omp parallel
   !$omp task if(omp_get_num_threads() > 0)
   !$omp atomic
      sum = sum + 1
   !$omp end task
   !$omp end parallel
   if (sum /= NT) then
      print *, "FAIL - sum == ", sum, " (expected ", NT, ")"
      stop 1
   end if
end program F03_2_7_1d
