! { dg-do run }
! { dg-require-effective-target tls_runtime }
!$ use omp_lib

  integer, save, allocatable :: a(:, :)
  integer, allocatable :: b(:, :)
  integer :: n
  logical :: l
!$omp threadprivate (a)
  if (allocated (a)) stop 1
  call omp_set_dynamic (.false.)
  l = .false.
!$omp parallel num_threads (4) reduction(.or.:l)
  allocate (a(-1:1, 7:10))
  a(:, :) = omp_get_thread_num () + 6
  l = l.or..not.allocated (a)
  l = l.or.size(a).ne.12.or.size(a,1).ne.3.or.size(a,2).ne.4
!$omp end parallel
  if (l.or.any(a.ne.6)) stop 2
!$omp parallel num_threads (4) copyin (a) reduction(.or.:l) private (b)
  l = l.or.allocated (b)
  l = l.or..not.allocated (a)
  l = l.or.size(a).ne.12.or.size(a,1).ne.3.or.size(a,2).ne.4
  l = l.or.any(a.ne.6)
  allocate (b(1, 3))
  a(:, :) = omp_get_thread_num () + 36
  b(:, :) = omp_get_thread_num () + 66
  !$omp single
    n = omp_get_thread_num ()
  !$omp end single copyprivate (a, b)
  l = l.or..not.allocated (a)
  l = l.or.size(a).ne.12.or.size(a,1).ne.3.or.size(a,2).ne.4
  l = l.or.any(a.ne.(n + 36))
  l = l.or..not.allocated (b)
  l = l.or.size(b).ne.3.or.size(b,1).ne.1.or.size(b,2).ne.3
  l = l.or.any(b.ne.(n + 66))
  deallocate (b)
  l = l.or.allocated (b)
!$omp end parallel
  if (n.lt.0 .or. n.ge.4) stop 3
  if (l.or.any(a.ne.(n + 36))) stop 4
!$omp parallel num_threads (4) reduction(.or.:l)
  deallocate (a)
  l = l.or.allocated (a)
!$omp end parallel
  if (l.or.allocated (a)) stop 5
end
