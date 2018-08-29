! { dg-do run }

  interface
    subroutine bar (q)
      integer :: q(19:)
    end subroutine
  end interface
  integer :: q(7:15)
  q(:) = 5
  call bar (q)
end
subroutine bar (q)
  use iso_c_binding, only: c_ptr, c_loc, c_int
  integer :: a, b, c, d(2:3,4:5), q(19:), h, k, m, n, o, p
  integer(c_int), target :: e(64)
  type (c_ptr) :: f, g(64)
  logical :: l
  a = 1
  b = 2
  c = 3
  d = 4
  l = .false.
  f = c_loc (e)
  call foo
contains
  subroutine foo
    use iso_c_binding, only: c_sizeof
!$omp simd linear(a:2) linear(b:1)
    do a = 1, 20, 2
      b = b + 1
    end do
!$omp end simd
    if (a /= 21 .or. b /= 12) STOP 1
!$omp simd aligned(f : c_sizeof (e(1)))
    do b = 1, 64
      g(b) = f
    end do
!$omp end simd
!$omp parallel
!$omp single
!$omp taskgroup
!$omp task depend(out : a, d(2:2,4:5))
    a = a + 1
    d(2:2,4:5) = d(2:2,4:5) + 1
!$omp end task
!$omp task depend(in : a, d(2:2,4:5))
    if (a /= 22) STOP 2
    if (any (d(2:2,4:5) /= 5)) STOP 3
!$omp end task
!$omp end taskgroup
!$omp end single
!$omp end parallel
    b = 10
!$omp target data map (tofrom: a, d(2:3,4:4), q) map (from: l)
!$omp target map (tofrom: b, d(2:3,4:4)) map (alloc: a, l)
    l = .false.
    if (a /= 22 .or. any (q /= 5)) l = .true.
    if (lbound (q, 1) /= 19 .or. ubound (q, 1) /= 27) l = .true.
    if (d(2,4) /= 5 .or. d(3,4) /= 4) l = .true.
    l = l .or. (b /= 10)
    a = 6
    b = 11
    q = 8
    d(2:3,4:4) = 9
!$omp end target
!$omp target update from (a, q, d(2:3,4:4), l)
    if (a /= 6 .or. l .or. b /= 11 .or. any (q /= 8)) STOP 4
    if (any (d(2:3,4:4) /= 9) .or. d(2,5) /= 5 .or. d(3,5) /= 4) STOP 5
    a = 12
    b = 13
    q = 14
    d = 15
!$omp target update to (a, q, d(2:3,4:4))
!$omp target map (tofrom: b, d(2:3,4:4)) map (alloc: a, l)
    if (a /= 12 .or. b /= 13 .or. any (q /= 14)) l = .true.
    l = l .or. any (d(2:3,4:4) /= 15)
!$omp end target
    a = 0
    b = 1
    c = 100
    h = 8
    m = 0
    n = 64
    o = 16
    if (l) STOP 6
!$omp target teams distribute parallel do simd if (.not.l) device(a) &
!$omp & num_teams(b) dist_schedule(static, c) num_threads (h) &
!$omp & reduction (+: m) safelen (n) schedule(static, o) &
!$omp & defaultmap(tofrom: scalar)
    do p = 1, 64
      m = m + 1
    end do
!$omp end target teams distribute parallel do simd
    if (m /= 64) STOP 7
!$omp end target data
  end subroutine foo
end subroutine bar
