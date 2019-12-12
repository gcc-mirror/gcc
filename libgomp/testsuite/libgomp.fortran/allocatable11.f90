! { dg-do run }
! { dg-require-effective-target tls_runtime }

  use omp_lib
  integer, allocatable, save :: a, b(:), c(:,:)
  integer :: p
!$omp threadprivate (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) stop 1

  call omp_set_dynamic (.false.)
  call omp_set_num_threads (4)

!$omp parallel num_threads (4)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) stop 2
!$omp end parallel

  allocate (a, b(6:9), c(3, 8:9))
  a = 4
  b = 5
  c = 6
  if (.not.allocated (a)) stop 3
  if (.not.allocated (b) .or. size (b) /= 4) stop 4
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 5
  if (.not.allocated (c) .or. size (c) /= 6) stop 6
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 7
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 8
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 9

!$omp parallel num_threads (4) copyin (a, b, c) private (p)
  p = omp_get_thread_num ()
  if (.not.allocated (a)) stop 10
  if (.not.allocated (b) .or. size (b) /= 4) stop 11
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 12
  if (.not.allocated (c) .or. size (c) /= 6) stop 13
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 14
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 15
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 16
  if (a /= 4 .or. any (b /= 5) .or. any (c /= 6)) stop 17
  deallocate (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) stop 18
  allocate (a, b(p:9), c(3, p:7))
  a = p
  b = p
  c = p
  if (.not.allocated (a)) stop 19
  if (.not.allocated (b) .or. size (b) /= (10 - p)) stop 20
  if (lbound (b, 1) /= p .or. ubound (b, 1) /= 9) stop 21
  if (.not.allocated (c) .or. size (c) /= (3 * (8 - p))) stop 22
  if (size (c, 1) /= 3 .or. size (c, 2) /= (8 - p)) stop 23
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 24
  if (lbound (c, 2) /= p .or. ubound (c, 2) /= 7) stop 25
  if (a /= p .or. any (b /= p) .or. any (c /= p)) stop 26
!$omp end parallel

!$omp parallel num_threads (4) copyin (a, b, c)
  if (.not.allocated (a)) stop 27
  if (.not.allocated (b) .or. size (b) /= 10) stop 28
  if (lbound (b, 1) /= 0 .or. ubound (b, 1) /= 9) stop 29
  if (.not.allocated (c) .or. size (c) /= 24) stop 30
  if (size (c, 1) /= 3 .or. size (c, 2) /= 8) stop 31
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 32
  if (lbound (c, 2) /= 0 .or. ubound (c, 2) /= 7) stop 33
  if (a /= 0 .or. any (b /= 0) .or. any (c /= 0)) stop 34
!$omp end parallel

  deallocate (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) stop 35

!$omp parallel num_threads (4) copyin (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) stop 36
!$omp end parallel
end
