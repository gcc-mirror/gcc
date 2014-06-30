! { dg-do run }

  integer, allocatable :: a, b(:), c(:,:)
  logical :: l
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort

!$omp parallel private (a, b, c, l)
  l = .false.
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort

!$omp single
  allocate (a, b(6:9), c(3, 8:9))
  a = 4
  b = 5
  c = 6
!$omp end single copyprivate (a, b, c)

  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 4) call abort
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
  if (.not.allocated (c) .or. size (c) /= 6) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
  if (a /= 4 .or. any (b /= 5) .or. any (c /= 6)) call abort

!$omp single
  deallocate (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort
  allocate (a, b(0:4), c(3, 2:7))
  a = 1
  b = 2
  c = 3
!$omp end single copyprivate (a, b, c)

  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 5) call abort
  if (lbound (b, 1) /= 0 .or. ubound (b, 1) /= 4) call abort
  if (.not.allocated (c) .or. size (c) /= 18) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 6) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 2 .or. ubound (c, 2) /= 7) call abort
  if (a /= 1 .or. any (b /= 2) .or. any (c /= 3)) call abort

!$omp single
  l = .true.
  deallocate (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort
  allocate (a, b(2:6), c(3:5, 3:8))
  a = 7
  b = 8
  c = 9
!$omp end single copyprivate (a, b, c)

  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 5) call abort
  if (l) then
    if (lbound (b, 1) /= 2 .or. ubound (b, 1) /= 6) call abort
  else
    if (lbound (b, 1) /= 0 .or. ubound (b, 1) /= 4) call abort
  end if
  if (.not.allocated (c) .or. size (c) /= 18) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 6) call abort
  if (l) then
    if (lbound (c, 1) /= 3 .or. ubound (c, 1) /= 5) call abort
    if (lbound (c, 2) /= 3 .or. ubound (c, 2) /= 8) call abort
  else
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
    if (lbound (c, 2) /= 2 .or. ubound (c, 2) /= 7) call abort
  end if
  if (a /= 7 .or. any (b /= 8) .or. any (c /= 9)) call abort

!$omp end parallel
end
