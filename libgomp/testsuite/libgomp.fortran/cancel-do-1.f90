! { dg-do run }
! { dg-set-target-env-var OMP_CANCELLATION "true" }

  use omp_lib
  integer :: i

  !$omp parallel num_threads(32)
    !$omp do
      do i = 0, 999
	!$omp cancel do
	if (omp_get_cancellation ()) call abort
      enddo
  !$omp endparallel
end
