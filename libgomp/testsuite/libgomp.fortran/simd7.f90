! { dg-do run }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

subroutine foo (d, e, f, g, m, n)
  integer :: i, j, b(2:9), c(3:n), d(:), e(2:n), f(2:,3:), n
  integer, allocatable :: g(:), h(:), k, m
  logical :: l
  l = .false.
  allocate (h(2:7))
  i = 4; j = 4; b = 7; c = 8; d = 9; e = 10; f = 11; g = 12; h = 13; k = 14; m = 15
!$omp simd linear(b)linear(c:2)linear(d:3)linear(e:4)linear(f:5)linear(g:6) &
!$omp & linear(h:7)linear(k:8)linear(m:9) reduction(.or.:l)
  do i = 0, 63
    l = l .or. .not.allocated (g) .or. .not.allocated (h)
    l = l .or. .not.allocated (k) .or. .not.allocated (m)
    l = l .or. any (b /= 7 + i) .or. any (c /= 8 + 2 * i)
    l = l .or. any (d /= 9 + 3 * i) .or. any (e /= 10 + 4 * i)
    l = l .or. any (f /= 11 + 5 * i) .or. any (g /= 12 + 6 * i)
    l = l .or. any (h /= 13 + 7 * i) .or. (k /= 14 + 8 * i)
    l = l .or. (m /= 15 + 9 * i)
    l = l .or. (lbound (b, 1) /= 2) .or. (ubound (b, 1) /= 9)
    l = l .or. (lbound (c, 1) /= 3) .or. (ubound (c, 1) /= n)
    l = l .or. (lbound (d, 1) /= 1) .or. (ubound (d, 1) /= 17)
    l = l .or. (lbound (e, 1) /= 2) .or. (ubound (e, 1) /= n)
    l = l .or. (lbound (f, 1) /= 2) .or. (ubound (f, 1) /= 3)
    l = l .or. (lbound (f, 2) /= 3) .or. (ubound (f, 2) /= 5)
    l = l .or. (lbound (g, 1) /= 7) .or. (ubound (g, 1) /= 10)
    l = l .or. (lbound (h, 1) /= 2) .or. (ubound (h, 1) /= 7)
    b = b + 1; c = c + 2; d = d + 3; e = e + 4; f = f + 5; g = g + 6
    h = h + 7; k = k + 8; m = m + 9
  end do
  if (l .or. i /= 64) STOP 1
  if (any (b /= 7 + 64) .or. any (c /= 8 + 2 * 64)) STOP 2
  if (any (d /= 9 + 3 * 64) .or. any (e /= 10 + 4 * 64)) STOP 3
  if (any (f /= 11 + 5 * 64) .or. any (g /= 12 + 6 * 64)) STOP 4
  if (any (h /= 13 + 7 * 64) .or. (k /= 14 + 8 * 64)) STOP 5
  if (m /= 15 + 9 * 64) STOP 6
  if ((lbound (b, 1) /= 2) .or. (ubound (b, 1) /= 9)) STOP 7
  if ((lbound (c, 1) /= 3) .or. (ubound (c, 1) /= n)) STOP 8
  if ((lbound (d, 1) /= 1) .or. (ubound (d, 1) /= 17)) STOP 9
  if ((lbound (e, 1) /= 2) .or. (ubound (e, 1) /= n)) STOP 10
  if ((lbound (f, 1) /= 2) .or. (ubound (f, 1) /= 3)) STOP 11
  if ((lbound (f, 2) /= 3) .or. (ubound (f, 2) /= 5)) STOP 12
  if ((lbound (g, 1) /= 7) .or. (ubound (g, 1) /= 10)) STOP 13
  if ((lbound (h, 1) /= 2) .or. (ubound (h, 1) /= 7)) STOP 14
  i = 4; j = 4; b = 7; c = 8; d = 9; e = 10; f = 11; g = 12; h = 13; k = 14; m = 15
!$omp simd linear(b)linear(c:2)linear(d:3)linear(e:4)linear(f:5)linear(g:6) &
!$omp & linear(h:7)linear(k:8)linear(m:9) reduction(.or.:l) collapse(2)
  do i = 0, 7
    do j = 0, 7
      l = l .or. .not.allocated (g) .or. .not.allocated (h)
      l = l .or. .not.allocated (k) .or. .not.allocated (m)
      l = l .or. any (b /= 7 + (8 * i + j)) .or. any (c /= 8 + 2 * (8 * i + j))
      l = l .or. any (d /= 9 + 3 * (8 * i + j)) .or. any (e /= 10 + 4 * (8 * i + j))
      l = l .or. any (f /= 11 + 5 * (8 * i + j)) .or. any (g /= 12 + 6 * (8 * i + j))
      l = l .or. any (h /= 13 + 7 * (8 * i + j)) .or. (k /= 14 + 8 * (8 * i + j))
      l = l .or. (m /= 15 + 9 * (8 * i + j))
      l = l .or. (lbound (b, 1) /= 2) .or. (ubound (b, 1) /= 9)
      l = l .or. (lbound (c, 1) /= 3) .or. (ubound (c, 1) /= n)
      l = l .or. (lbound (d, 1) /= 1) .or. (ubound (d, 1) /= 17)
      l = l .or. (lbound (e, 1) /= 2) .or. (ubound (e, 1) /= n)
      l = l .or. (lbound (f, 1) /= 2) .or. (ubound (f, 1) /= 3)
      l = l .or. (lbound (f, 2) /= 3) .or. (ubound (f, 2) /= 5)
      l = l .or. (lbound (g, 1) /= 7) .or. (ubound (g, 1) /= 10)
      l = l .or. (lbound (h, 1) /= 2) .or. (ubound (h, 1) /= 7)
      b = b + 1; c = c + 2; d = d + 3; e = e + 4; f = f + 5; g = g + 6
      h = h + 7; k = k + 8; m = m + 9
    end do
  end do
  if (l .or. i /= 8 .or. j /= 8) STOP 15
  if (any (b /= 7 + 64) .or. any (c /= 8 + 2 * 64)) STOP 16
  if (any (d /= 9 + 3 * 64) .or. any (e /= 10 + 4 * 64)) STOP 17
  if (any (f /= 11 + 5 * 64) .or. any (g /= 12 + 6 * 64)) STOP 18
  if (any (h /= 13 + 7 * 64) .or. (k /= 14 + 8 * 64)) STOP 19
  if (m /= 15 + 9 * 64) STOP 20
  if ((lbound (b, 1) /= 2) .or. (ubound (b, 1) /= 9)) STOP 21
  if ((lbound (c, 1) /= 3) .or. (ubound (c, 1) /= n)) STOP 22
  if ((lbound (d, 1) /= 1) .or. (ubound (d, 1) /= 17)) STOP 23
  if ((lbound (e, 1) /= 2) .or. (ubound (e, 1) /= n)) STOP 24
  if ((lbound (f, 1) /= 2) .or. (ubound (f, 1) /= 3)) STOP 25
  if ((lbound (f, 2) /= 3) .or. (ubound (f, 2) /= 5)) STOP 26
  if ((lbound (g, 1) /= 7) .or. (ubound (g, 1) /= 10)) STOP 27
  if ((lbound (h, 1) /= 2) .or. (ubound (h, 1) /= 7)) STOP 28
  i = 4; j = 4; b = 7; c = 8; d = 9; e = 10; f = 11; g = 12; h = 13; k = 14; m = 15
!$omp parallel do simd linear(b)linear(c:2)linear(d:3)linear(e:4)linear(f:5) &
!$omp & linear(g:6)linear(h:7)linear(k:8)linear(m:9) reduction(.or.:l)
  do i = 0, 63
    l = l .or. .not.allocated (g) .or. .not.allocated (h)
    l = l .or. .not.allocated (k) .or. .not.allocated (m)
    l = l .or. any (b /= 7 + i) .or. any (c /= 8 + 2 * i)
    l = l .or. any (d /= 9 + 3 * i) .or. any (e /= 10 + 4 * i)
    l = l .or. any (f /= 11 + 5 * i) .or. any (g /= 12 + 6 * i)
    l = l .or. any (h /= 13 + 7 * i) .or. (k /= 14 + 8 * i)
    l = l .or. (m /= 15 + 9 * i)
    l = l .or. (lbound (b, 1) /= 2) .or. (ubound (b, 1) /= 9)
    l = l .or. (lbound (c, 1) /= 3) .or. (ubound (c, 1) /= n)
    l = l .or. (lbound (d, 1) /= 1) .or. (ubound (d, 1) /= 17)
    l = l .or. (lbound (e, 1) /= 2) .or. (ubound (e, 1) /= n)
    l = l .or. (lbound (f, 1) /= 2) .or. (ubound (f, 1) /= 3)
    l = l .or. (lbound (f, 2) /= 3) .or. (ubound (f, 2) /= 5)
    l = l .or. (lbound (g, 1) /= 7) .or. (ubound (g, 1) /= 10)
    l = l .or. (lbound (h, 1) /= 2) .or. (ubound (h, 1) /= 7)
    b = b + 1; c = c + 2; d = d + 3; e = e + 4; f = f + 5; g = g + 6
    h = h + 7; k = k + 8; m = m + 9
  end do
  if (l .or. i /= 64) STOP 29
  if (any (b /= 7 + 64) .or. any (c /= 8 + 2 * 64)) STOP 30
  if (any (d /= 9 + 3 * 64) .or. any (e /= 10 + 4 * 64)) STOP 31
  if (any (f /= 11 + 5 * 64) .or. any (g /= 12 + 6 * 64)) STOP 32
  if (any (h /= 13 + 7 * 64) .or. (k /= 14 + 8 * 64)) STOP 33
  if (m /= 15 + 9 * 64) STOP 34
  if ((lbound (b, 1) /= 2) .or. (ubound (b, 1) /= 9)) STOP 35
  if ((lbound (c, 1) /= 3) .or. (ubound (c, 1) /= n)) STOP 36
  if ((lbound (d, 1) /= 1) .or. (ubound (d, 1) /= 17)) STOP 37
  if ((lbound (e, 1) /= 2) .or. (ubound (e, 1) /= n)) STOP 38
  if ((lbound (f, 1) /= 2) .or. (ubound (f, 1) /= 3)) STOP 39
  if ((lbound (f, 2) /= 3) .or. (ubound (f, 2) /= 5)) STOP 40
  if ((lbound (g, 1) /= 7) .or. (ubound (g, 1) /= 10)) STOP 41
  if ((lbound (h, 1) /= 2) .or. (ubound (h, 1) /= 7)) STOP 42
  i = 4; j = 4; b = 7; c = 8; d = 9; e = 10; f = 11; g = 12; h = 13; k = 14; m = 15
!$omp parallel do simd linear(b)linear(c:2)linear(d:3)linear(e:4)linear(f:5) &
!$omp & linear(g:6)linear(h:7)linear(k:8)linear(m:9) reduction(.or.:l) collapse(2)
  do i = 0, 7
    do j = 0, 7
      l = l .or. .not.allocated (g) .or. .not.allocated (h)
      l = l .or. .not.allocated (k) .or. .not.allocated (m)
      l = l .or. any (b /= 7 + (8 * i + j)) .or. any (c /= 8 + 2 * (8 * i + j))
      l = l .or. any (d /= 9 + 3 * (8 * i + j)) .or. any (e /= 10 + 4 * (8 * i + j))
      l = l .or. any (f /= 11 + 5 * (8 * i + j)) .or. any (g /= 12 + 6 * (8 * i + j))
      l = l .or. any (h /= 13 + 7 * (8 * i + j)) .or. (k /= 14 + 8 * (8 * i + j))
      l = l .or. (m /= 15 + 9 * (8 * i + j))
      l = l .or. (lbound (b, 1) /= 2) .or. (ubound (b, 1) /= 9)
      l = l .or. (lbound (c, 1) /= 3) .or. (ubound (c, 1) /= n)
      l = l .or. (lbound (d, 1) /= 1) .or. (ubound (d, 1) /= 17)
      l = l .or. (lbound (e, 1) /= 2) .or. (ubound (e, 1) /= n)
      l = l .or. (lbound (f, 1) /= 2) .or. (ubound (f, 1) /= 3)
      l = l .or. (lbound (f, 2) /= 3) .or. (ubound (f, 2) /= 5)
      l = l .or. (lbound (g, 1) /= 7) .or. (ubound (g, 1) /= 10)
      l = l .or. (lbound (h, 1) /= 2) .or. (ubound (h, 1) /= 7)
      b = b + 1; c = c + 2; d = d + 3; e = e + 4; f = f + 5; g = g + 6
      h = h + 7; k = k + 8; m = m + 9
    end do
  end do
  if (l .or. i /= 8 .or. j /= 8) STOP 43
  if (any (b /= 7 + 64) .or. any (c /= 8 + 2 * 64)) STOP 44
  if (any (d /= 9 + 3 * 64) .or. any (e /= 10 + 4 * 64)) STOP 45
  if (any (f /= 11 + 5 * 64) .or. any (g /= 12 + 6 * 64)) STOP 46
  if (any (h /= 13 + 7 * 64) .or. (k /= 14 + 8 * 64)) STOP 47
  if (m /= 15 + 9 * 64) STOP 48
  if ((lbound (b, 1) /= 2) .or. (ubound (b, 1) /= 9)) STOP 49
  if ((lbound (c, 1) /= 3) .or. (ubound (c, 1) /= n)) STOP 50
  if ((lbound (d, 1) /= 1) .or. (ubound (d, 1) /= 17)) STOP 51
  if ((lbound (e, 1) /= 2) .or. (ubound (e, 1) /= n)) STOP 52
  if ((lbound (f, 1) /= 2) .or. (ubound (f, 1) /= 3)) STOP 53
  if ((lbound (f, 2) /= 3) .or. (ubound (f, 2) /= 5)) STOP 54
  if ((lbound (g, 1) /= 7) .or. (ubound (g, 1) /= 10)) STOP 55
  if ((lbound (h, 1) /= 2) .or. (ubound (h, 1) /= 7)) STOP 56
end subroutine

  interface
    subroutine foo (d, e, f, g, m, n)
      integer :: d(:), e(2:n), f(2:,3:), n
      integer, allocatable :: g(:), m
    end subroutine
  end interface
  integer, parameter :: n = 8
  integer :: d(2:18), e(3:n+1), f(5:6,7:9)
  integer, allocatable :: g(:), m
  allocate (g(7:10))
  call foo (d, e, f, g, m, n)
end
