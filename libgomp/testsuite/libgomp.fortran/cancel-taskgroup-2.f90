! { dg-do run }
! { dg-set-target-env-var OMP_CANCELLATION "true" }

  use omp_lib
  integer :: i

  !$omp parallel
    !$omp taskgroup
      !$omp task
	!$omp cancel taskgroup
	call abort
      !$omp endtask
    !$omp endtaskgroup
  !$omp endparallel
  !$omp parallel private (i)
    !$omp barrier
    !$omp single
      !$omp taskgroup
	do i = 0, 49
	  !$omp task
	    !$omp cancellation point taskgroup
	    !$omp cancel taskgroup if (i.gt.5)
	  !$omp end task
	end do
      !$omp end taskgroup
    !$omp endsingle
  !$omp end parallel
end
