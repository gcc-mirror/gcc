! { dg-do run }
! { dg-set-target-env-var OMP_CANCELLATION "true" }

  use omp_lib

  if (omp_get_cancellation ()) then
    !$omp parallel num_threads(32)
      !$omp sections
	  !$omp cancel sections
	  call abort
	!$omp section
	  !$omp cancel sections
	  call abort
	!$omp section
	  !$omp cancel sections
	  call abort
	!$omp section
	  !$omp cancel sections
	  call abort
      !$omp end sections
    !$omp end parallel
  end if
end
