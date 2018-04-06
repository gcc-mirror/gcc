! { dg-do run }

  integer, allocatable :: a, b(:), c(:,:)
  integer :: i
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) &
!$omp & initializer (omp_priv = 0)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) STOP 1
  allocate (a, b(6:9), c(3, 8:9))
  a = 0
  b = 0
  c = 0
  if (.not.allocated (a)) STOP 2
  if (.not.allocated (b) .or. size (b) /= 4) STOP 3
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 4
  if (.not.allocated (c) .or. size (c) /= 6) STOP 5
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 6
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 7
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 8
!$omp parallel do reduction (+:a, b, c)
  do i = 1, 10
    if (.not.allocated (a)) STOP 9
    if (.not.allocated (b) .or. size (b) /= 4) STOP 10
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 11
    if (.not.allocated (c) .or. size (c) /= 6) STOP 12
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 13
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 14
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 15
    a = a + i
    b = b + 2 * i
    c = c + 3 * i
  end do
  if (.not.allocated (a)) STOP 16
  if (.not.allocated (b) .or. size (b) /= 4) STOP 17
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 18
  if (.not.allocated (c) .or. size (c) /= 6) STOP 19
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 20
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 21
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 22
  if (a /= 55 .or. any (b /= 110) .or. any (c /= 165)) STOP 23
  a = 0
  b = 0
  c = 0
!$omp parallel do reduction (foo : a, b, c)
  do i = 1, 10
    if (.not.allocated (a)) STOP 24
    if (.not.allocated (b) .or. size (b) /= 4) STOP 25
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 26
    if (.not.allocated (c) .or. size (c) /= 6) STOP 27
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 28
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 29
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 30
    a = a + i
    b = b + 2 * i
    c = c + 3 * i
  end do
  if (.not.allocated (a)) STOP 31
  if (.not.allocated (b) .or. size (b) /= 4) STOP 32
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 33
  if (.not.allocated (c) .or. size (c) /= 6) STOP 34
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 35
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 36
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 37
  if (a /= 55 .or. any (b /= 110) .or. any (c /= 165)) STOP 38
  a = 0
  b = 0
  c = 0
!$omp simd reduction (+:a, b, c)
  do i = 1, 10
    if (.not.allocated (a)) STOP 39
    if (.not.allocated (b) .or. size (b) /= 4) STOP 40
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 41
    if (.not.allocated (c) .or. size (c) /= 6) STOP 42
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 43
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 44
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 45
    a = a + i
    b = b + 2 * i
    c = c + 3 * i
  end do
  if (.not.allocated (a)) STOP 46
  if (.not.allocated (b) .or. size (b) /= 4) STOP 47
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 48
  if (.not.allocated (c) .or. size (c) /= 6) STOP 49
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 50
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 51
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 52
  if (a /= 55 .or. any (b /= 110) .or. any (c /= 165)) STOP 53
  a = 0
  b = 0
  c = 0
!$omp simd reduction (foo : a, b, c)
  do i = 1, 10
    if (.not.allocated (a)) STOP 54
    if (.not.allocated (b) .or. size (b) /= 4) STOP 55
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 56
    if (.not.allocated (c) .or. size (c) /= 6) STOP 57
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 58
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 59
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 60
    a = a + i
    b = b + 2 * i
    c = c + 3 * i
  end do
  if (.not.allocated (a)) STOP 61
  if (.not.allocated (b) .or. size (b) /= 4) STOP 62
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 63
  if (.not.allocated (c) .or. size (c) /= 6) STOP 64
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 65
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 66
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 67
  if (a /= 55 .or. any (b /= 110) .or. any (c /= 165)) STOP 68
end
