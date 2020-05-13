! { dg-do run }
!
! PR fortran/94690
! PR middle-end/66199

module m
  implicit none
  integer u(1024), v(1024), w(1024)
  !$omp declare target (v, u, w)

contains

integer function f1 (a, b)
  integer :: a, b, d
  !$omp target map(from: d)
  !$omp teams distribute simd default(none) firstprivate (a, b) shared(u, v, w)
  do d = a, b-1
    u(d) = v(d) + w(d)
  end do
  !$omp end teams distribute simd
  !$omp end target
  f1 = d
end

integer function f2 (a, b, c)
  integer a, b, c, d, e
  !$omp target map(from: d, e)
  !$omp teams distribute simd default(none) firstprivate (a, b, c) shared(u, v, w) linear(d) lastprivate(e)
  do d = a, b-1
    u(d) = v(d) + w(d)
    e = c + d * 5
  end do
  !$omp end teams distribute simd
  !$omp end target
  f2 = d + e
end

integer function f3 (a1, b1, a2, b2)
  integer :: a1, b1, a2, b2, d1, d2
  !$omp target map(from: d1, d2)
  !$omp teams distribute simd default(none) firstprivate (a1, b1, a2, b2) shared(u, v, w) lastprivate(d1, d2) collapse(2)
  do d1 = a1, b1-1
    do d2 = a2, b2-1
      u(d1 * 32 + d2) = v(d1 * 32 + d2) + w(d1 * 32 + d2)
    end do
  end do
  !$omp end teams distribute simd
  !$omp end target
  f3 = d1 + d2
end

integer function f4 (a1, b1, a2, b2)
  integer :: a1, b1, a2, b2, d1, d2
  !$omp target map(from: d1, d2)
  !$omp teams distribute simd default(none) firstprivate (a1, b1, a2, b2) shared(u, v, w) collapse(2)
  do d1 = a1, b1-1
    do d2 = a2, b2-1
      u(d1 * 32 + d2) = v(d1 * 32 + d2) + w(d1 * 32 + d2)
    end do
  end do
  !$omp end teams distribute simd
  !$omp end target
  f4 = d1 + d2
end
end module

use m
  if (f1 (0, 1024) /= 1024) stop 1
  if (f2 (0, 1024, 17) /= 1024 + (17 + 5 * 1023)) stop 2
  if (f3 (0, 32, 0, 32) /= 64) stop 3
  if (f4 (0, 32, 0, 32) /= 64) stop 4
end
