! { dg-do run }

  integer, allocatable :: a, b(:), c(:,:)
  logical :: l
  if (allocated (a) .or. allocated (b) .or. allocated (c)) STOP 1

!$omp parallel private (a, b, c, l)
  l = .false.
  if (allocated (a) .or. allocated (b) .or. allocated (c)) STOP 2

!$omp single
  allocate (a, b(6:9), c(3, 8:9))
  a = 4
  b = 5
  c = 6
!$omp end single copyprivate (a, b, c)

  if (.not.allocated (a)) STOP 3
  if (.not.allocated (b) .or. size (b) /= 4) STOP 4
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 5
  if (.not.allocated (c) .or. size (c) /= 6) STOP 6
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 7
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 8
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 9
  if (a /= 4 .or. any (b /= 5) .or. any (c /= 6)) STOP 10

!$omp single
  deallocate (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) STOP 11
  allocate (a, b(0:4), c(3, 2:7))
  a = 1
  b = 2
  c = 3
!$omp end single copyprivate (a, b, c)

  if (.not.allocated (a)) STOP 12
  if (.not.allocated (b) .or. size (b) /= 5) STOP 13
  if (lbound (b, 1) /= 0 .or. ubound (b, 1) /= 4) STOP 14
  if (.not.allocated (c) .or. size (c) /= 18) STOP 15
  if (size (c, 1) /= 3 .or. size (c, 2) /= 6) STOP 16
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 17
  if (lbound (c, 2) /= 2 .or. ubound (c, 2) /= 7) STOP 18
  if (a /= 1 .or. any (b /= 2) .or. any (c /= 3)) STOP 19

!$omp single
  l = .true.
  deallocate (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) STOP 20
  allocate (a, b(2:6), c(3:5, 3:8))
  a = 7
  b = 8
  c = 9
!$omp end single copyprivate (a, b, c)

  if (.not.allocated (a)) STOP 21
  if (.not.allocated (b) .or. size (b) /= 5) STOP 22
  if (l) then
    if (lbound (b, 1) /= 2 .or. ubound (b, 1) /= 6) STOP 23
  else
    if (lbound (b, 1) /= 0 .or. ubound (b, 1) /= 4) STOP 24
  end if
  if (.not.allocated (c) .or. size (c) /= 18) STOP 25
  if (size (c, 1) /= 3 .or. size (c, 2) /= 6) STOP 26
  if (l) then
    if (lbound (c, 1) /= 3 .or. ubound (c, 1) /= 5) STOP 27
    if (lbound (c, 2) /= 3 .or. ubound (c, 2) /= 8) STOP 28
  else
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 29
    if (lbound (c, 2) /= 2 .or. ubound (c, 2) /= 7) STOP 30
  end if
  if (a /= 7 .or. any (b /= 8) .or. any (c /= 9)) STOP 31

!$omp end parallel
end
