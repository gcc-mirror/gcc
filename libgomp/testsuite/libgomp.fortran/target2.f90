! { dg-do run }
! { dg-options "-ffree-line-length-160" }

module target2
contains
  subroutine foo (a, b, c, d, e, f, g, n, q)
    integer :: n, q
    integer :: a, b(3:n), c(5:), d(2:*), e(:,:)
    integer, pointer :: f, g(:)
    integer :: h, i(3:n)
    integer, pointer :: j, k(:)
    logical :: r
    allocate (j, k(4:n))
    h = 14
    i = 15
    j = 16
    k = 17
    !$omp target map (to: a, b, c, d(2:n+1), e, f, g, h, i, j, k, n) map (from: r)
      r = a /= 7
      r = r .or. (any (b /= 8)) .or. (lbound (b, 1) /= 3) .or. (ubound (b, 1) /= n)
      r = r .or. (any (c /= 9)) .or. (lbound (c, 1) /= 5) .or. (ubound (c, 1) /= n + 4)
      r = r .or. (any (d(2:n+1) /= 10)) .or. (lbound (d, 1) /= 2)
      r = r .or. (any (e /= 11)) .or. (lbound (e, 1) /= 1) .or. (ubound (e, 1) /= 2)
      r = r .or. (lbound (e, 2) /= 1) .or. (ubound (e, 2) /= 2)
      r = r .or. (f /= 12)
      r = r .or. (any (g /= 13)) .or. (lbound (g, 1) /= 3) .or. (ubound (g, 1) /= n)
      r = r .or. (h /= 14)
      r = r .or. (any (i /= 15)) .or. (lbound (i, 1) /= 3) .or. (ubound (i, 1) /= n)
      r = r .or. (j /= 16)
      r = r .or. (any (k /= 17)) .or. (lbound (k, 1) /= 4) .or. (ubound (k, 1) /= n)
    !$omp end target
    if (r) STOP 1
    !$omp target map (to: b(3:n), c(5:n+4), d(2:n+1), e(1:,:2), g(3:n), i(3:n), k(4:n), n) map (from: r)
      r = (any (b /= 8)) .or. (lbound (b, 1) /= 3) .or. (ubound (b, 1) /= n)
      r = r .or. (any (c /= 9)) .or. (lbound (c, 1) /= 5) .or. (ubound (c, 1) /= n + 4)
      r = r .or. (any (d(2:n+1) /= 10)) .or. (lbound (d, 1) /= 2)
      r = r .or. (any (e /= 11)) .or. (lbound (e, 1) /= 1) .or. (ubound (e, 1) /= 2)
      r = r .or. (lbound (e, 2) /= 1) .or. (ubound (e, 2) /= 2)
      r = r .or. (any (g /= 13)) .or. (lbound (g, 1) /= 3) .or. (ubound (g, 1) /= n)
      r = r .or. (any (i /= 15)) .or. (lbound (i, 1) /= 3) .or. (ubound (i, 1) /= n)
      r = r .or. (any (k /= 17)) .or. (lbound (k, 1) /= 4) .or. (ubound (k, 1) /= n)
    !$omp end target
    if (r) STOP 2
    !$omp target map (to: b(5:n-2), c(7:n), d(4:n-2), e(1:,2:), g(5:n-3), i(6:n-4), k(5:n-5), n) map (from: r)
      r = (any (b(5:n-2) /= 8)) .or. (lbound (b, 1) /= 3) .or. (ubound (b, 1) /= n)
      r = r .or. (any (c(7:n) /= 9)) .or. (lbound (c, 1) /= 5) .or. (ubound (c, 1) /= n + 4)
      r = r .or. (any (d(4:n-2) /= 10)) .or. (lbound (d, 1) /= 2)
      r = r .or. (any (e(1:,2:) /= 11)) .or. (lbound (e, 1) /= 1) .or. (ubound (e, 1) /= 2)
      r = r .or. (lbound (e, 2) /= 1) .or. (ubound (e, 2) /= 2)
      r = r .or. (any (g(5:n-3) /= 13)) .or. (lbound (g, 1) /= 3) .or. (ubound (g, 1) /= n)
      r = r .or. (any (i(6:n-4) /= 15)) .or. (lbound (i, 1) /= 3) .or. (ubound (i, 1) /= n)
      r = r .or. (any (k(5:n-5) /= 17)) .or. (lbound (k, 1) /= 4) .or. (ubound (k, 1) /= n)
    !$omp end target
    !$omp target map (to: b(q+5:n-2+q), c(q+7:q+n), d(q+4:q+n-2), e(1:q+2,2:q+2), g(5+q:n-3+q), &
    !$omp & i(6+q:n-4+q), k(5+q:n-5+q), n) map (from: r)
      r = (any (b(5:n-2) /= 8)) .or. (lbound (b, 1) /= 3) .or. (ubound (b, 1) /= n)
      r = r .or. (any (c(7:n) /= 9)) .or. (lbound (c, 1) /= 5) .or. (ubound (c, 1) /= n + 4)
      r = r .or. (any (d(4:n-2) /= 10)) .or. (lbound (d, 1) /= 2)
      r = r .or. (any (e(1:,2:) /= 11)) .or. (lbound (e, 1) /= 1) .or. (ubound (e, 1) /= 2)
      r = r .or. (lbound (e, 2) /= 1) .or. (ubound (e, 2) /= 2)
      r = r .or. (any (g(5:n-3) /= 13)) .or. (lbound (g, 1) /= 3) .or. (ubound (g, 1) /= n)
      r = r .or. (any (i(6:n-4) /= 15)) .or. (lbound (i, 1) /= 3) .or. (ubound (i, 1) /= n)
      r = r .or. (any (k(5:n-5) /= 17)) .or. (lbound (k, 1) /= 4) .or. (ubound (k, 1) /= n)
    !$omp end target
    if (r) STOP 3
    !$omp target map (to: d(2:n+1), n)
      r = a /= 7
      r = r .or. (any (b /= 8)) .or. (lbound (b, 1) /= 3) .or. (ubound (b, 1) /= n)
      r = r .or. (any (c /= 9)) .or. (lbound (c, 1) /= 5) .or. (ubound (c, 1) /= n + 4)
      r = r .or. (any (d(2:n+1) /= 10)) .or. (lbound (d, 1) /= 2)
      r = r .or. (any (e /= 11)) .or. (lbound (e, 1) /= 1) .or. (ubound (e, 1) /= 2)
      r = r .or. (lbound (e, 2) /= 1) .or. (ubound (e, 2) /= 2)
      r = r .or. (f /= 12)
      r = r .or. (any (g /= 13)) .or. (lbound (g, 1) /= 3) .or. (ubound (g, 1) /= n)
      r = r .or. (h /= 14)
      r = r .or. (any (i /= 15)) .or. (lbound (i, 1) /= 3) .or. (ubound (i, 1) /= n)
      r = r .or. (j /= 16)
      r = r .or. (any (k /= 17)) .or. (lbound (k, 1) /= 4) .or. (ubound (k, 1) /= n)
    !$omp end target
    if (r) STOP 4
  end subroutine foo
end module target2
  use target2, only : foo
  integer, parameter :: n = 15, q = 0
  integer :: a, b(2:n-1), c(n), d(n), e(3:4, 3:4)
  integer, pointer :: f, g(:)
  allocate (f, g(3:n))
  a = 7
  b = 8
  c = 9
  d = 10
  e = 11
  f = 12
  g = 13
  call foo (a, b, c, d, e, f, g, n, q)
end
