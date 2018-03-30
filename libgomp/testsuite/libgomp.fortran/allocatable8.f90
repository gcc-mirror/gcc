! { dg-do run }
! { dg-require-effective-target tls_runtime }
!$ use omp_lib

  integer, save, allocatable :: a(:, :)
  logical :: l
!$omp threadprivate (a)
  if (allocated (a)) STOP 1
  l = .false.
!$omp parallel copyin (a) num_threads (4) reduction(.or.:l)
  l = l.or.allocated (a)
!$omp end parallel
  if (l.or.allocated (a)) STOP 2
end
