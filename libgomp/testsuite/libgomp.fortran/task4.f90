! { dg-do run }

  use omp_lib
  integer :: err, e

!$omp atomic write
  err = 0
!$omp parallel shared(err) private(e)
  if (omp_in_final ()) then
!$omp atomic write
    err = 1
  endif
!$omp task if (.false.) shared(err)
  if (omp_in_final ()) then
!$omp atomic write
    err = 1
  endif
!$omp task if (.false.) shared(err)
  if (omp_in_final ()) then
!$omp atomic write
    err = 1
  endif
!$omp end task
!$omp end task
!$omp atomic read
  e = err
!$omp task final (e .eq. 0) shared(err)
  if (.not.omp_in_final ()) then
!$omp atomic write
    err = 1
  endif
!$omp taskyield
!$omp taskwait
!$omp task shared(err)
  if (.not.omp_in_final ()) then
!$omp atomic write
    err = 1
  endif
!$omp end task
!$omp end task
!$omp end parallel
!$omp atomic read
  e = err
  if (e .ne. 0) call abort
end
