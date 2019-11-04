! { dg-do run }

  integer, allocatable :: a, b(:), c(:,:)
  integer :: i
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) &
!$omp & initializer (omp_priv = 0)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) stop 1
  allocate (a, b(6:9), c(3, 8:9))
  a = 0
  b = 0
  c = 0
  if (.not.allocated (a)) stop 2
  if (.not.allocated (b) .or. size (b) /= 4) stop 3
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 4
  if (.not.allocated (c) .or. size (c) /= 6) stop 5
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 6
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 7
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 8
!$omp parallel do reduction (+:a, b, c)
  do i = 1, 10
    if (.not.allocated (a)) stop 9
    if (.not.allocated (b) .or. size (b) /= 4) stop 10
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 11
    if (.not.allocated (c) .or. size (c) /= 6) stop 12
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 13
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 14
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 15
    a = a + i
    b = b + 2 * i
    c = c + 3 * i
  end do
  if (.not.allocated (a)) stop 16
  if (.not.allocated (b) .or. size (b) /= 4) stop 17
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 18
  if (.not.allocated (c) .or. size (c) /= 6) stop 19
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 20
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 21
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 22
  if (a /= 55 .or. any (b /= 110) .or. any (c /= 165)) stop 23
  a = 0
  b = 0
  c = 0
!$omp parallel do reduction (foo : a, b, c)
  do i = 1, 10
    if (.not.allocated (a)) stop 24
    if (.not.allocated (b) .or. size (b) /= 4) stop 25
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 26
    if (.not.allocated (c) .or. size (c) /= 6) stop 27
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 28
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 29
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 30
    a = a + i
    b = b + 2 * i
    c = c + 3 * i
  end do
  if (.not.allocated (a)) stop 31
  if (.not.allocated (b) .or. size (b) /= 4) stop 32
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 33
  if (.not.allocated (c) .or. size (c) /= 6) stop 34
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 35
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 36
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 37
  if (a /= 55 .or. any (b /= 110) .or. any (c /= 165)) stop 38
  a = 0
  b = 0
  c = 0
!$omp simd reduction (+:a, b, c)
  do i = 1, 10
    if (.not.allocated (a)) stop 39
    if (.not.allocated (b) .or. size (b) /= 4) stop 40
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 41
    if (.not.allocated (c) .or. size (c) /= 6) stop 42
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 43
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 44
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 45
    a = a + i
    b = b + 2 * i
    c = c + 3 * i
  end do
  if (.not.allocated (a)) stop 46
  if (.not.allocated (b) .or. size (b) /= 4) stop 47
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 48
  if (.not.allocated (c) .or. size (c) /= 6) stop 49
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 50
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 51
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 52
  if (a /= 55 .or. any (b /= 110) .or. any (c /= 165)) stop 53
  a = 0
  b = 0
  c = 0
!$omp simd reduction (foo : a, b, c)
  do i = 1, 10
    if (.not.allocated (a)) stop 54
    if (.not.allocated (b) .or. size (b) /= 4) stop 55
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 56
    if (.not.allocated (c) .or. size (c) /= 6) stop 57
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 58
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 59
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 60
    a = a + i
    b = b + 2 * i
    c = c + 3 * i
  end do
  if (.not.allocated (a)) stop 61
  if (.not.allocated (b) .or. size (b) /= 4) stop 62
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 63
  if (.not.allocated (c) .or. size (c) /= 6) stop 64
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 65
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 66
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 67
  if (a /= 55 .or. any (b /= 110) .or. any (c /= 165)) stop 68
end
