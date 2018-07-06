! { dg-do run }
  integer, pointer :: a, c(:)
  integer, target :: b, d(10)
  b = 0
  a => b
  d = 0
  c => d
  call foo (a, c)
  b = 0
  d = 0
  call bar (a, c)
contains
  subroutine foo (a, c)
    integer, pointer :: a, c(:), b, d(:)
    integer :: r, r2
    r = 0
    !$omp parallel firstprivate (a, c) reduction (+:r)
      !$omp atomic
        a = a + 1
      !$omp atomic
        c(1) = c(1) + 1
      r = r + 1
    !$omp end parallel
    if (a.ne.r.or.c(1).ne.r) STOP 1
    r2 = r
    b => a
    d => c
    r = 0
    !$omp parallel firstprivate (b, d) reduction (+:r)
      !$omp atomic
        b = b + 1
      !$omp atomic
        d(1) = d(1) + 1
      r = r + 1
    !$omp end parallel
    if (b.ne.r+r2.or.d(1).ne.r+r2) STOP 2
  end subroutine foo
  subroutine bar (a, c)
    integer, pointer :: a, c(:), b, d(:)
    integer, target :: q, r(5)
    integer :: i
    q = 17
    r = 21
    b => a
    d => c
    !$omp parallel do firstprivate (a, c) lastprivate (a, c)
      do i = 1, 100
        !$omp atomic
          a = a + 1
        !$omp atomic
          c((i+9)/10) = c((i+9)/10) + 1
        if (i.eq.100) then
          a => q
          c => r
	end if
      end do
    !$omp end parallel do
    if (b.ne.100.or.any(d.ne.10)) STOP 3
    if (a.ne.17.or.any(c.ne.21)) STOP 4
    a => b
    c => d
    !$omp parallel do firstprivate (b, d) lastprivate (b, d)
      do i = 1, 100
        !$omp atomic
          b = b + 1
        !$omp atomic
          d((i+9)/10) = d((i+9)/10) + 1
        if (i.eq.100) then
          b => q
          d => r
	end if
      end do
    !$omp end parallel do
    if (a.ne.200.or.any(c.ne.20)) STOP 5
    if (b.ne.17.or.any(d.ne.21)) STOP 6
  end subroutine bar
end
