! { dg-do run }
! { dg-set-target-env-var OMP_CANCELLATION "true" }

  use omp_lib

  !$omp parallel num_threads(32)
    !$omp cancel parallel
    if (omp_get_cancellation ()) STOP 1
  !$omp end parallel
end
