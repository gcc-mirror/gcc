! { dg-additional-options "-fdump-tree-original" }
program main
  use omp_lib
  implicit none (type, external)
  integer :: p, a(20)
  !$omp parallel master num_threads(4) private (p) shared(a)
    p = omp_get_thread_num ();
    if (p /= 0) stop 1
    a = 0
  !$omp end parallel master
end

! { dg-final { scan-tree-dump "#pragma omp parallel private\\(p\\) shared\\(a\\) num_threads\\(4\\)" "original"} }
! { dg-final { scan-tree-dump "#pragma omp master" "original"} }
