! { dg-do run }
! { dg-options "-std=legacy" }

  integer :: err
  err = 0
!$omp parallel num_threads (4) default (none) shared (err)
!$omp single
  call test
!$omp end single
!$omp end parallel
  if (err.ne.0) stop 1
contains
  subroutine check (x, y, l)
    integer :: x, y
    logical :: l
    l = l .or. x .ne. y
  end subroutine check

  subroutine foo (c, d, e, f, g, h, i, j, k, n)
    use omp_lib
    integer :: n
    character (len = *) :: c
    character (len = n) :: d
    integer, dimension (2, 3:5, n) :: e
    integer, dimension (2, 3:n, n) :: f
    character (len = *), dimension (5, 3:n) :: g
    character (len = n), dimension (5, 3:n) :: h
    real, dimension (:, :, :) :: i
    double precision, dimension (3:, 5:, 7:) :: j
    integer, dimension (:, :, :) :: k
    logical :: l
    integer :: p, q, r
    character (len = n) :: s
    integer, dimension (2, 3:5, n) :: t
    integer, dimension (2, 3:n, n) :: u
    character (len = n), dimension (5, 3:n) :: v
    character (len = 2 * n + 24) :: w
    integer :: x, z
    character (len = 1) :: y
    s = 'PQRSTUV'
    forall (p = 1:2, q = 3:5, r = 1:7) t(p, q, r) = -10 + p - q + 2 * r
    forall (p = 1:2, q = 3:7, r = 1:7) u(p, q, r) = 30 - p + q - 2 * r
    forall (p = 1:5, q = 3:7, p + q .le. 8) v(p, q) = '_+|/Oo_'
    forall (p = 1:5, q = 3:7, p + q .gt. 8) v(p, q) = '///|||!'
!$omp task default (none) firstprivate (c, d, e, f, g, h, i, j, k) &
!$omp & firstprivate (s, t, u, v) private (l, p, q, r, w, x, y) shared (err)
    l = .false.
    l = l .or. c .ne. 'abcdefghijkl'
    l = l .or. d .ne. 'ABCDEFG'
    l = l .or. s .ne. 'PQRSTUV'
    do 100, p = 1, 2
      do 100, q = 3, 7
	do 100, r = 1, 7
	  if (q .lt. 6) l = l .or. e(p, q, r) .ne. 5 + p + q + 2 * r
	  l = l .or. f(p, q, r) .ne. 25 + p + q + 2 * r
	  if (r .lt. 6 .and. q + r .le. 8) l = l .or. g(r, q) .ne. '0123456789AB'
	  if (r .lt. 6 .and. q + r .gt. 8) l = l .or. g(r, q) .ne. '9876543210ZY'
	  if (r .lt. 6 .and. q + r .le. 8) l = l .or. h(r, q) .ne. '0123456'
	  if (r .lt. 6 .and. q + r .gt. 8) l = l .or. h(r, q) .ne. '9876543'
	  if (q .lt. 6) l = l .or. t(p, q, r) .ne. -10 + p - q + 2 * r
	  l = l .or. u(p, q, r) .ne. 30 - p + q - 2 * r
	  if (r .lt. 6 .and. q + r .le. 8) l = l .or. v(r, q) .ne. '_+|/Oo_'
	  if (r .lt. 6 .and. q + r .gt. 8) l = l .or. v(r, q) .ne. '///|||!'
100 continue
    do 101, p = 3, 5
      do 101, q = 2, 6
	do 101, r = 1, 7
	  l = l .or. i(p - 2, q - 1, r) .ne. 7.5 * p * q * r
	  l = l .or. j(p, q + 3, r + 6) .ne. 9.5 * p * q * r
101 continue
    do 102, p = 1, 5
      do 102, q = 4, 6
	l = l .or. k(p, 1, q - 3) .ne. 19 + p + 7 + 3 * q
102 continue
    call check (size (e, 1), 2, l)
    call check (size (e, 2), 3, l)
    call check (size (e, 3), 7, l)
    call check (size (e), 42, l)
    call check (size (f, 1), 2, l)
    call check (size (f, 2), 5, l)
    call check (size (f, 3), 7, l)
    call check (size (f), 70, l)
    call check (size (g, 1), 5, l)
    call check (size (g, 2), 5, l)
    call check (size (g), 25, l)
    call check (size (h, 1), 5, l)
    call check (size (h, 2), 5, l)
    call check (size (h), 25, l)
    call check (size (i, 1), 3, l)
    call check (size (i, 2), 5, l)
    call check (size (i, 3), 7, l)
    call check (size (i), 105, l)
    call check (size (j, 1), 4, l)
    call check (size (j, 2), 5, l)
    call check (size (j, 3), 7, l)
    call check (size (j), 140, l)
    call check (size (k, 1), 5, l)
    call check (size (k, 2), 1, l)
    call check (size (k, 3), 3, l)
    call check (size (k), 15, l)
    if (l) then
!$omp atomic
      err = err + 1
    end if
!$omp end task
  c = ''
  d = ''
  e(:, :, :) = 199
  f(:, :, :) = 198
  g(:, :) = ''
  h(:, :) = ''
  i(:, :, :) = 7.0
  j(:, :, :) = 8.0
  k(:, :, :) = 9
  s = ''
  t(:, :, :) = 10
  u(:, :, :) = 11
  v(:, :) = ''
  end subroutine foo

  subroutine test
    character (len = 12) :: c
    character (len = 7) :: d
    integer, dimension (2, 3:5, 7) :: e
    integer, dimension (2, 3:7, 7) :: f
    character (len = 12), dimension (5, 3:7) :: g
    character (len = 7), dimension (5, 3:7) :: h
    real, dimension (3:5, 2:6, 1:7) :: i
    double precision, dimension (3:6, 2:6, 1:7) :: j
    integer, dimension (1:5, 7:7, 4:6) :: k
    integer :: p, q, r
    c = 'abcdefghijkl'
    d = 'ABCDEFG'
    forall (p = 1:2, q = 3:5, r = 1:7) e(p, q, r) = 5 + p + q + 2 * r
    forall (p = 1:2, q = 3:7, r = 1:7) f(p, q, r) = 25 + p + q + 2 * r
    forall (p = 1:5, q = 3:7, p + q .le. 8) g(p, q) = '0123456789AB'
    forall (p = 1:5, q = 3:7, p + q .gt. 8) g(p, q) = '9876543210ZY'
    forall (p = 1:5, q = 3:7, p + q .le. 8) h(p, q) = '0123456'
    forall (p = 1:5, q = 3:7, p + q .gt. 8) h(p, q) = '9876543'
    forall (p = 3:5, q = 2:6, r = 1:7) i(p, q, r) = 7.5 * p * q * r
    forall (p = 3:6, q = 2:6, r = 1:7) j(p, q, r) = 9.5 * p * q * r
    forall (p = 1:5, q = 7:7, r = 4:6) k(p, q, r) = 19 + p + q + 3 * r
    call foo (c, d, e, f, g, h, i, j, k, 7)
  end subroutine test
end
