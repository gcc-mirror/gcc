! { dg-do run }
!
! PR fortran/94690
! PR middle-end/66199 

module m
  integer u(0:1024-1), v(0:1024-1), w(0:1024-1)
contains

integer(8) function f1 (a, b)
  implicit none
  integer, value :: a, b
  integer(8) :: d
  !$omp parallel do lastprivate (d) default(none) firstprivate (a, b) shared(u, v, w)
  do d = a, b-1
    u(d) = v(d) + w(d)
  end do
  f1 = d
end

integer(8) function f2 (a, b, c)
  implicit none
  integer, value :: a, b, c
  integer(8) :: d, e
  !$omp parallel do lastprivate (d) default(none) firstprivate (a, b) shared(u, v, w) linear(c:5) lastprivate(e)
  do d = a, b-1
      u(d) = v(d) + w(d)
      c = c + 5
      e = c
  end do
  f2 = d + c + e
end

integer(8) function f3 (a1, b1, a2, b2)
  implicit none
  integer, value :: a1, b1, a2, b2
  integer(8) d1, d2
  !$omp parallel do default(none) firstprivate (a1, b1, a2, b2) shared(u, v, w) lastprivate(d1, d2) collapse(2)
  do d1 = a1, b1-1
    do d2 = a2, b2-1
      u(d1 * 32 + d2) = v(d1 * 32 + d2) + w(d1 * 32 + d2)
    end do
  end do
  f3 = d1 + d2
end
end module m

program main
  use m
  if (f1 (0, 1024) /= 1024) stop 1
  if (f2 (0, 1024, 17) /= 1024 + 2 * (17 + 5 * 1024)) stop 2
  if (f3 (0, 32, 0, 32) /= 64) stop 3
end program main
