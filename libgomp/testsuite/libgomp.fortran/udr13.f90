! { dg-do run }

  interface
    subroutine sub1 (x, y)
      integer, intent(in) :: y(:)
      integer, intent(out) :: x(:)
    end subroutine
    function fn2 (x, m1, m2, n1, n2)
      integer, intent(in) :: x(:,:), m1, m2, n1, n2
      integer :: fn2(m1:m2,n1:n2)
    end function
    subroutine sub3 (x, y)
      integer, allocatable, intent(in) :: y(:,:)
      integer, allocatable, intent(inout) :: x(:,:)
    end subroutine
  end interface
!$omp declare reduction (foo : integer : sub3 (omp_out, omp_in)) &
!$omp initializer (omp_priv = fn3 (omp_orig))
!$omp declare reduction (bar : integer : omp_out = fn1 (omp_out, omp_in, &
!$omp & lbound (omp_out, 1), ubound (omp_out, 1))) &
!$omp & initializer (sub1 (omp_priv, omp_orig))
!$omp declare reduction (baz : integer : sub2 (omp_out, omp_in)) &
!$omp initializer (omp_priv = fn2 (omp_orig, lbound (omp_priv, 1), &
!$omp ubound (omp_priv, 1), lbound (omp_priv, 2), ubound (omp_priv, 2)))
  interface
    function fn1 (x, y, m1, m2)
      integer, intent(in) :: x(:), y(:), m1, m2
      integer :: fn1(m1:m2)
    end function
    subroutine sub2 (x, y)
      integer, intent(in) :: y(:,:)
      integer, intent(inout) :: x(:,:)
    end subroutine
    function fn3 (x)
      integer, allocatable, intent(in) :: x(:,:)
      integer, allocatable :: fn3(:,:)
    end function
  end interface
  integer :: a(10), b(3:5,7:9), r
  integer, allocatable :: c(:,:)
  a(:) = 0
  r = 0
!$omp parallel reduction (bar : a) reduction (+: r)
  if (lbound (a, 1) /= 1 .or. ubound (a, 1) /= 10) stop 1
  a = a + 2
  r = r + 1
!$omp end parallel
  if (any (a /= 4 * r) ) stop 2
  b(:,:) = 0
  allocate (c (4:6,8:10))
  c(:,:) = 0
  r = 0
!$omp parallel reduction (baz : b, c) reduction (+: r)
  if (lbound (b, 1) /= 3 .or. ubound (b, 1) /= 5) stop 3
  if (lbound (b, 2) /= 7 .or. ubound (b, 2) /= 9) stop 4
  if (.not. allocated (c)) stop 5
  if (lbound (c, 1) /= 4 .or. ubound (c, 1) /= 6) stop 6
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 10) stop 7
  b = b + 3
  c = c + 4
  r = r + 1
!$omp end parallel
  if (any (b /= 3 * r) .or. any (c /= 4 * r)) stop 8
  deallocate (c)
  allocate (c (0:1,7:11))
  c(:,:) = 0
  r = 0
!$omp parallel reduction (foo : c) reduction (+: r)
  if (.not. allocated (c)) stop 9
  if (lbound (c, 1) /= 0 .or. ubound (c, 1) /= 1) stop 10
  if (lbound (c, 2) /= 7 .or. ubound (c, 2) /= 11) stop 11
  c = c + 5
  r = r + 1
!$omp end parallel
  if (any (c /= 10 * r)) stop 12
end
function fn1 (x, y, m1, m2)
  integer, intent(in) :: x(:), y(:), m1, m2
  integer :: fn1(m1:m2)
  fn1 = x + 2 * y
end function
subroutine sub1 (x, y)
  integer, intent(in) :: y(:)
  integer, intent(out) :: x(:)
  x = 0
end subroutine
function fn2 (x, m1, m2, n1, n2)
  integer, intent(in) :: x(:,:), m1, m2, n1, n2
  integer :: fn2(m1:m2,n1:n2)
  fn2 = x
end function
subroutine sub2 (x, y)
  integer, intent(inout) :: x(:,:)
  integer, intent(in) :: y(:,:)
  x = x + y
end subroutine
function fn3 (x)
  integer, allocatable, intent(in) :: x(:,:)
  integer, allocatable :: fn3(:,:)
  fn3 = x
end function
subroutine sub3 (x, y)
  integer, allocatable, intent(inout) :: x(:,:)
  integer, allocatable, intent(in) :: y(:,:)
  x = x + 2 * y
end subroutine
