! { dg-do run }
! { dg-set-target-env-var OMP_CANCELLATION "true" }

  use omp_lib

  if (omp_get_cancellation ()) then
    !$omp parallel num_threads(32)
      !$omp sections
	  !$omp cancel sections
	  stop 1
	!$omp section
	  !$omp cancel sections
	  stop 2
	!$omp section
	  !$omp cancel sections
	  stop 3
	!$omp section
	  !$omp cancel sections
	  stop 4
      !$omp end sections
    !$omp end parallel
  end if
end
