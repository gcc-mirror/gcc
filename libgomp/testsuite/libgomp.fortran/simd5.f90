! { dg-do run }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

  integer :: i, j, b, c
  c = 0
  i = 4
  j = 4
  b = 7
!$omp simd linear(b:2) reduction(+:c)
  do i = 0, 63
    c = c + b - (7 + 2 * i)
    b = b + 2
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 64 * 2) stop 1
  i = 4
  j = 4
  b = 7
!$omp simd linear(b:3) reduction(+:c)
  do i = 0, 63, 4
    c = c + b - (7 + i / 4 * 3)
    b = b + 3
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 16 * 3) stop 2
  i = 4
  j = 4
  b = 7
!$omp simd linear(i) linear(b:2) reduction(+:c)
  do i = 0, 63
    c = c + b - (7 + 2 * i)
    b = b + 2
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 64 * 2) stop 3
  i = 4
  j = 4
  b = 7
!$omp simd linear(i:4) linear(b:3) reduction(+:c)
  do i = 0, 63, 4
    c = c + b - (7 + i / 4 * 3)
    b = b + 3
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 16 * 3) stop 4
  i = 4
  j = 4
  b = 7
!$omp simd collapse(2) linear(b:2) reduction(+:c)
  do i = 0, 7
    do j = 0, 7
      c = c + b - (7 + 2 * j + 2 * 8 * i)
      b = b + 2
    end do
  end do
  if (c /= 0 .or. i /= 8 .or. j /= 8 .or. b /= 7 + 64 * 2) stop 5
  i = 4
  j = 4
  b = 7
!$omp simd collapse(2) linear(b:2) reduction(+:c) lastprivate (i, j)
  do i = 0, 7
    do j = 0, 7
      c = c + b - (7 + 2 * j + 2 * 8 * i)
      b = b + 2
    end do
  end do
  if (c /= 0 .or. i /= 8 .or. j /= 8 .or. b /= 7 + 64 * 2) stop 6
  i = 4
  j = 4
  b = 7
!$omp parallel do simd schedule (static, 4) linear(b:2) reduction(+:c)
  do i = 0, 63
    c = c + b - (7 + 2 * i)
    b = b + 2
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 64 * 2) stop 7
  i = 4
  j = 4
  b = 7
!$omp parallel do simd schedule (static, 4) linear(b:3) reduction(+:c)
  do i = 0, 63, 4
    c = c + b - (7 + i / 4 * 3)
    b = b + 3
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 16 * 3) stop 8
  i = 4
  j = 4
  b = 7
!$omp parallel do simd schedule (static, 4) linear(i) linear(b:2) reduction(+:c)
  do i = 0, 63
    c = c + b - (7 + 2 * i)
    b = b + 2
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 64 * 2) stop 9
  i = 4
  j = 4
  b = 7
!$omp parallel do simd schedule (static, 4) linear(i:4) linear(b:3) reduction(+:c)
  do i = 0, 63, 4
    c = c + b - (7 + i / 4 * 3)
    b = b + 3
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 16 * 3) stop 10
  i = 4
  j = 4
  b = 7
!$omp parallel do simd schedule (static, 4) collapse(2) linear(b:2) reduction(+:c)
  do i = 0, 7
    do j = 0, 7
      c = c + b - (7 + 2 * j + 2 * 8 * i)
      b = b + 2
    end do
  end do
  if (c /= 0 .or. i /= 8 .or. j /= 8 .or. b /= 7 + 64 * 2) stop 11
  i = 4
  j = 4
  b = 7
!$omp parallel do simd schedule (static, 4) collapse(2) linear(b:2) &
!$omp & reduction(+:c) lastprivate (i, j)
  do i = 0, 7
    do j = 0, 7
      c = c + b - (7 + 2 * j + 2 * 8 * i)
      b = b + 2
    end do
  end do
  if (c /= 0 .or. i /= 8 .or. j /= 8 .or. b /= 7 + 64 * 2) stop 12
end
