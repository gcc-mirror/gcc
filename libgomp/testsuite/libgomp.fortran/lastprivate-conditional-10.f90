! { dg-do run }
! Fortran version of libgomp.c-c++-common/lastprivate-conditional-10.c

module m
  implicit none
  integer :: v = 0
  integer :: x = 0
contains
  integer function foo (a)
    integer, contiguous :: a(0:)
    integer i

    !$omp parallel do simd lastprivate (conditional: x) schedule(simd : static) if (simd : .false.)
    do i = 0, 127
      if (a(i) /= 0) x = a(i)
    end do
    foo = x
  end

  integer function bar (a, b)
    integer, contiguous :: a(0:), b(0:)
    integer :: i
    !$omp parallel
    !$omp do simd lastprivate (conditional: x, v) schedule(static, 16) simdlen (1)
    do i = 16, 127
      if (a(i) /= 0) x = a(i);
      if (b(i) /= 0) v = b(i) + 10;
    end do
    !$omp end parallel
    bar = x
  end

  integer function baz (a)
    integer, contiguous :: a(0:)
    integer :: i
    !$omp parallel do simd if (simd : .false.) lastprivate (conditional: x) schedule(simd : dynamic, 16)
    do i = 0, 127
      if (a(i) /= 0) x = a(i) + 5
    end do
    baz = x
  end
end module m

program main
  use m
  implicit none
  integer :: a(0:127), b(0:127), i
  do i = 0, 127
      if (mod(i, 11) == 2) then
         a(i) =  i + 10
      else
        a(i) = 0
      endif
      if (mod(i, 13) == 5) then
        b(i) = i * 2
      else
        b(i) = 0
      endif
  end do
  if (foo (a) /= 133) stop 1
  if (bar (b, a) /= 244 .or. v /= 143) stop 2
  if (baz (b) /= 249) stop 3
end
