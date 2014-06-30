! { dg-do run }

  interface
    elemental subroutine sub1 (x, y)
      integer, intent(in) :: y
      integer, intent(out) :: x
    end subroutine
    elemental function fn2 (x)
      integer, intent(in) :: x
      integer :: fn2
    end function
  end interface
!$omp declare reduction (foo : integer : omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction (bar : integer : omp_out = fn1 (omp_out, omp_in)) &
!$omp & initializer (sub1 (omp_priv, omp_orig))
!$omp declare reduction (baz : integer : sub2 (omp_out, omp_in)) &
!$omp initializer (omp_priv = fn2 (omp_orig))
  interface
    elemental function fn1 (x, y)
      integer, intent(in) :: x, y
      integer :: fn1
    end function
    elemental subroutine sub2 (x, y)
      integer, intent(in) :: y
      integer, intent(inout) :: x
    end subroutine
  end interface
  integer :: a(10), b, r
  a(:) = 0
  b = 0
  r = 0
!$omp parallel reduction (foo : a, b) reduction (+: r)
  a = a + 2
  b = b + 3
  r = r + 1
!$omp end parallel
  if (any (a /= 2 * r) .or. b /= 3 * r) call abort
  a(:) = 0
  b = 0
  r = 0
!$omp parallel reduction (bar : a, b) reduction (+: r)
  a = a + 2
  b = b + 3
  r = r + 1
!$omp end parallel
  if (any (a /= 4 * r) .or. b /= 6 * r) call abort
  a(:) = 0
  b = 0
  r = 0
!$omp parallel reduction (baz : a, b) reduction (+: r)
  a = a + 2
  b = b + 3
  r = r + 1
!$omp end parallel
  if (any (a /= 2 * r) .or. b /= 3 * r) call abort
end
elemental function fn1 (x, y)
  integer, intent(in) :: x, y
  integer :: fn1
  fn1 = x + 2 * y
end function
elemental subroutine sub1 (x, y)
  integer, intent(in) :: y
  integer, intent(out) :: x
  x = 0
end subroutine
elemental function fn2 (x)
  integer, intent(in) :: x
  integer :: fn2
  fn2 = x
end function
elemental subroutine sub2 (x, y)
  integer, intent(inout) :: x
  integer, intent(in) :: y
  x = x + y
end subroutine
