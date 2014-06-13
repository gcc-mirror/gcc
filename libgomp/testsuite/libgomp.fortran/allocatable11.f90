! { dg-do run }
! { dg-require-effective-target tls_runtime }

  use omp_lib
  integer, allocatable, save :: a, b(:), c(:,:)
  integer :: p
!$omp threadprivate (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort

  call omp_set_dynamic (.false.)
  call omp_set_num_threads (4)

!$omp parallel num_threads (4)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort
!$omp end parallel

  allocate (a, b(6:9), c(3, 8:9))
  a = 4
  b = 5
  c = 6
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 4) call abort
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
  if (.not.allocated (c) .or. size (c) /= 6) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort

!$omp parallel num_threads (4) copyin (a, b, c) private (p)
  p = omp_get_thread_num ()
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 4) call abort
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
  if (.not.allocated (c) .or. size (c) /= 6) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
  if (a /= 4 .or. any (b /= 5) .or. any (c /= 6)) call abort
  deallocate (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort
  allocate (a, b(p:9), c(3, p:7))
  a = p
  b = p
  c = p
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= (10 - p)) call abort
  if (lbound (b, 1) /= p .or. ubound (b, 1) /= 9) call abort
  if (.not.allocated (c) .or. size (c) /= (3 * (8 - p))) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= (8 - p)) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= p .or. ubound (c, 2) /= 7) call abort
  if (a /= p .or. any (b /= p) .or. any (c /= p)) call abort
!$omp end parallel

!$omp parallel num_threads (4) copyin (a, b, c)
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 10) call abort
  if (lbound (b, 1) /= 0 .or. ubound (b, 1) /= 9) call abort
  if (.not.allocated (c) .or. size (c) /= 24) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 8) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 0 .or. ubound (c, 2) /= 7) call abort
  if (a /= 0 .or. any (b /= 0) .or. any (c /= 0)) call abort
!$omp end parallel

  deallocate (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort

!$omp parallel num_threads (4) copyin (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort
!$omp end parallel
end
