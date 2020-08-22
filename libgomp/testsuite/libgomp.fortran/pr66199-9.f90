! { dg-do run }
!
! PR fortran/94690
! PR middle-end/66199

module m
  implicit none
  integer u(1024), v(1024), w(1024)
  !$omp declare target (u, v, w)

contains

integer function f2 (a, b, c)
  integer :: a, b, c, d, e
  !$omp target map(from: d, e)
  !$omp teams default(none) firstprivate (a, b, c) shared(d, e, u, v, w)
  !$omp distribute lastprivate(d, e)
  do d = a, b-1
    u(d) = v(d) + w(d)
    e = c + d * 5
  end do
  !$omp end teams
  !$omp end target
  f2 = d + e
end

integer function f3 (a1, b1, a2, b2)
  integer :: a1, b1, a2, b2, d1, d2
  !$omp target map(from: d1, d2)
  !$omp teams default(none) shared(a1, b1, a2, b2, d1, d2, u, v, w)
  !$omp distribute firstprivate (a1, b1, a2, b2) lastprivate(d1, d2) collapse(2)
  do d1 = a1, b1-1
    do d2 = a2, b2-1
      u(d1 * 32 + d2) = v(d1 * 32 + d2) + w(d1 * 32 + d2)
    end do
  end do
  !$omp end teams
  !$omp end target
  f3 = d1 + d2
end
end module

use m
  if (f2 (0, 1024, 17) /= 1024 + (17 + 5 * 1023)) stop 1
  if (f3 (0, 32, 0, 32) /= 64) stop 2
end
