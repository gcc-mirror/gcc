! { dg-do run }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

  interface
    subroutine foo (b, i, j, x)
      integer, intent (inout) :: b
      integer, intent (in) :: i, j, x
    end subroutine
  end interface
  integer :: i, j, b, c
  c = 0
  i = 4
  j = 4
  b = 7
!$omp simd linear(b:2) reduction(+:c)
  do i = 0, 63
    c = c + b - (7 + 2 * i)
    call foo (b, i, j, 2)
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 64 * 2) call abort
  i = 4
  j = 4
  b = 7
!$omp simd linear(b:3) reduction(+:c)
  do i = 0, 63, 4
    c = c + b - (7 + i / 4 * 3)
    call foo (b, i, j, 3)
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 16 * 3) call abort
  i = 4
  j = 4
  b = 7
!$omp simd linear(i) linear(b:2) reduction(+:c)
  do i = 0, 63
    c = c + b - (7 + 2 * i)
    call foo (b, i, j, 2)
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 64 * 2) call abort
  i = 4
  j = 4
  b = 7
!$omp simd linear(i:4) linear(b:3) reduction(+:c)
  do i = 0, 63, 4
    c = c + b - (7 + i / 4 * 3)
    call foo (b, i, j, 3)
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 16 * 3) call abort
  i = 4
  j = 4
  b = 7
!$omp simd collapse(2) linear(b:2) reduction(+:c)
  do i = 0, 7
    do j = 0, 7
      c = c + b - (7 + 2 * j + 2 * 8 * i)
      call foo (b, i, j, 2)
    end do
  end do
  if (c /= 0 .or. i /= 8 .or. j /= 8 .or. b /= 7 + 64 * 2) call abort
  i = 4
  j = 4
  b = 7
!$omp simd collapse(2) linear(b:2) reduction(+:c) lastprivate (i, j)
  do i = 0, 7
    do j = 0, 7
      c = c + b - (7 + 2 * j + 2 * 8 * i)
      call foo (b, i, j, 2)
    end do
  end do
  if (c /= 0 .or. i /= 8 .or. j /= 8 .or. b /= 7 + 64 * 2) call abort
  i = 4
  j = 4
  b = 7
!$omp parallel do simd schedule (static, 4) linear(b:2) reduction(+:c)
  do i = 0, 63
    c = c + b - (7 + 2 * i)
    call foo (b, i, j, 2)
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 64 * 2) call abort
  i = 4
  j = 4
  b = 7
!$omp parallel do simd schedule (static, 4) linear(b:3) reduction(+:c)
  do i = 0, 63, 4
    c = c + b - (7 + i / 4 * 3)
    call foo (b, i, j, 3)
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 16 * 3) call abort
  i = 4
  j = 4
  b = 7
!$omp parallel do simd schedule (static, 4) linear(i) linear(b:2) reduction(+:c)
  do i = 0, 63
    c = c + b - (7 + 2 * i)
    call foo (b, i, j, 2)
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 64 * 2) call abort
  i = 4
  j = 4
  b = 7
!$omp parallel do simd schedule (static, 4) linear(i:4) linear(b:3) reduction(+:c)
  do i = 0, 63, 4
    c = c + b - (7 + i / 4 * 3)
    call foo (b, i, j, 3)
  end do
  if (c /= 0 .or. i /= 64 .or. b /= 7 + 16 * 3) call abort
  i = 4
  j = 4
  b = 7
!$omp parallel do simd schedule (static, 4) collapse(2) linear(b:2) reduction(+:c)
  do i = 0, 7
    do j = 0, 7
      c = c + b - (7 + 2 * j + 2 * 8 * i)
      call foo (b, i, j, 2)
    end do
  end do
  if (c /= 0 .or. i /= 8 .or. j /= 8 .or. b /= 7 + 64 * 2) call abort
  i = 4
  j = 4
  b = 7
!$omp parallel do simd schedule (static, 4) collapse(2) linear(b:2) &
!$omp & reduction(+:c) lastprivate (i, j)
  do i = 0, 7
    do j = 0, 7
      c = c + b - (7 + 2 * j + 2 * 8 * i)
      call foo (b, i, j, 2)
    end do
  end do
  if (c /= 0 .or. i /= 8 .or. j /= 8 .or. b /= 7 + 64 * 2) call abort
end
subroutine foo (b, i, j, x)
  integer, intent (inout) :: b
  integer, intent (in) :: i, j, x
  b = b + (i - i) + (j - j) + x
end subroutine
