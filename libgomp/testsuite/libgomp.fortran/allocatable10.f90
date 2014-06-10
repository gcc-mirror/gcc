! { dg-do run }

  integer, allocatable :: a, b(:), c(:,:)
  integer :: i
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) &
!$omp & initializer (omp_priv = 0)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) call abort
  allocate (a, b(6:9), c(3, 8:9))
  a = 0
  b = 0
  c = 0
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 4) call abort
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
  if (.not.allocated (c) .or. size (c) /= 6) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
!$omp parallel do reduction (+:a, b, c)
  do i = 1, 10
    if (.not.allocated (a)) call abort
    if (.not.allocated (b) .or. size (b) /= 4) call abort
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
    if (.not.allocated (c) .or. size (c) /= 6) call abort
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
    a = a + i
    b = b + 2 * i
    c = c + 3 * i
  end do
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 4) call abort
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
  if (.not.allocated (c) .or. size (c) /= 6) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
  if (a /= 55 .or. any (b /= 110) .or. any (c /= 165)) call abort
  a = 0
  b = 0
  c = 0
!$omp parallel do reduction (foo : a, b, c)
  do i = 1, 10
    if (.not.allocated (a)) call abort
    if (.not.allocated (b) .or. size (b) /= 4) call abort
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
    if (.not.allocated (c) .or. size (c) /= 6) call abort
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
    a = a + i
    b = b + 2 * i
    c = c + 3 * i
  end do
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 4) call abort
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
  if (.not.allocated (c) .or. size (c) /= 6) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
  if (a /= 55 .or. any (b /= 110) .or. any (c /= 165)) call abort
  a = 0
  b = 0
  c = 0
!$omp simd reduction (+:a, b, c)
  do i = 1, 10
    if (.not.allocated (a)) call abort
    if (.not.allocated (b) .or. size (b) /= 4) call abort
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
    if (.not.allocated (c) .or. size (c) /= 6) call abort
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
    a = a + i
    b = b + 2 * i
    c = c + 3 * i
  end do
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 4) call abort
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
  if (.not.allocated (c) .or. size (c) /= 6) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
  if (a /= 55 .or. any (b /= 110) .or. any (c /= 165)) call abort
  a = 0
  b = 0
  c = 0
!$omp simd reduction (foo : a, b, c)
  do i = 1, 10
    if (.not.allocated (a)) call abort
    if (.not.allocated (b) .or. size (b) /= 4) call abort
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
    if (.not.allocated (c) .or. size (c) /= 6) call abort
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
    a = a + i
    b = b + 2 * i
    c = c + 3 * i
  end do
  if (.not.allocated (a)) call abort
  if (.not.allocated (b) .or. size (b) /= 4) call abort
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) call abort
  if (.not.allocated (c) .or. size (c) /= 6) call abort
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) call abort
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) call abort
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) call abort
  if (a /= 55 .or. any (b /= 110) .or. any (c /= 165)) call abort
end
