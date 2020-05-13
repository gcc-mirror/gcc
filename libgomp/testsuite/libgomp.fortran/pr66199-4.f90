! { dg-do run }
!
! PR fortran/94690
! PR middle-end/66199

module m
  implicit none
  integer u(0:1023), v(0:1023), w(0:1023)
  !$omp declare target (u, v, w)

contains

subroutine f1 (a, b)
  integer a, b, d
  !$omp target teams distribute parallel do default(none) firstprivate (a, b) shared(u, v, w)
  do d = a, b-1
    u(d) = v(d) + w(d)
  end do
end

subroutine f2 (a, b, c)
  integer a, b, c, d, e
  !$omp target teams distribute parallel do default(none) firstprivate (a, b, c) shared(u, v, w) lastprivate(d, e)
  do d = a, b-1
    u(d) = v(d) + w(d)
    e = c + d * 5
  end do
end

subroutine f3 (a1, b1, a2, b2)
  integer :: a1, b1, a2, b2, d1, d2
  !$omp target teams distribute parallel do default(none) firstprivate (a1, b1, a2, b2) shared(u, v, w) &
  !$omp&       lastprivate(d1, d2) collapse(2)
  do d1 = a1, b1-1
    do d2 = a2, b2-1
      u(d1 * 32 + d2) = v(d1 * 32 + d2) + w(d1 * 32 + d2)
    end do
  end do
end

subroutine f4 (a1, b1, a2, b2)
  integer :: a1, b1, a2, b2, d1, d2
  !$omp target teams distribute parallel do default(none) firstprivate (a1, b1, a2, b2) shared(u, v, w) &
  !$omp&       collapse(2)
  do d1 = a1, b1-1
    do d2 = a2, b2-1
      u(d1 * 32 + d2) = v(d1 * 32 + d2) + w(d1 * 32 + d2)
    end do
  end do
end
end module m

program main
  use m
  implicit none
  call f1 (0, 1024)
  call f2 (0, 1024, 17)
  call f3 (0, 32, 0, 32)
  call f4 (0, 32, 0, 32)
end
