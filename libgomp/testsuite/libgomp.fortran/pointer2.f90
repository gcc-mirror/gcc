! { dg-do run }
! { dg-require-effective-target tls_runtime }
  integer, pointer, save :: thr(:)
!$omp threadprivate (thr)
  integer, target :: s(3), t(3), u(3)
  integer :: i
  logical :: l
  s = 2
  t = 7
  u = 13
  thr => t
  l = .false.
  i = 0
!$omp parallel copyin (thr) reduction(.or.:l) reduction(+:i)
  if (any (thr.ne.7)) l = .true.
  thr => s
!$omp master
  thr => u
!$omp end master
!$omp atomic
  thr(1) = thr(1) + 1
  i = i + 1
!$omp end parallel
  if (l) stop 1
  if (thr(1).ne.14) stop 2
  if (s(1).ne.1+i) stop 3
  if (u(1).ne.14) stop 4
end
