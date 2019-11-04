! { dg-options "-O2" }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

  integer, save :: u(1024), v(1024), w(1024), m
  integer :: i
  v = (/ (i, i = 1, 1024) /)
  w = (/ (i + 1, i = 1, 1024) /)
  !$omp parallel
  !$omp single
  call f1 (1, 1024)
  !$omp end single
  !$omp end parallel
  do i = 1, 1024
    if (u(i) .ne. 2 * i + 1) stop 1
    v(i) = 1024 - i
    w(i) = 512 - i
  end do
  !$omp parallel
  !$omp single
    call f2 (2, 1022, 17)
  !$omp end single
  !$omp end parallel
  do i = 1, 1024
    if (i .lt. 2 .or. i .gt. 1022) then
      if (u(i) .ne. 2 * i + 1) stop 2
    else
      if (u(i) .ne. 1536 - 2 * i) stop 3
    end if
    v(i) = i
    w(i) = i + 1
  end do
  if (m .ne. (1023 + 2 * (1021 * 5 + 17) + 9)) stop 4
  !$omp parallel
  !$omp single
    call f3 (1, 1024)
  !$omp end single
  !$omp end parallel 
  do i = 1, 1024
    if (u(i) .ne. 2 * i + 1) stop 5
    v(i) = 1024 - i
    w(i) = 512 - i
  end do
  if (m .ne. 1025) stop 6
  !$omp parallel
  !$omp single
    call f4 (0, 31, 1, 32)
  !$omp end single
  !$omp end parallel 
  do i = 1, 1024
    if (u(i) .ne. 1536 - 2 * i) stop 7
    v(i) = i
    w(i) = i + 1
  end do
  if (m .ne. 32 + 33 + 1024) stop 8
  !$omp parallel
  !$omp single
    call f5 (0, 31, 1, 32)
  !$omp end single
  !$omp end parallel 
  do i = 1, 1024
    if (u(i) .ne. 2 * i + 1) stop 9
  end do
  if (m .ne. 32 + 33) stop 10
contains
  subroutine f1 (a, b)
    integer, intent(in) :: a, b
    integer :: d
    !$omp taskloop simd default(none) shared(u, v, w) nogroup
    do d = a, b
      u(d) = v(d) + w(d)
    end do
    ! d is predetermined linear, so we can't let the tasks continue past
    ! end of this function.
    !$omp taskwait
  end subroutine f1
  subroutine f2 (a, b, cx)
    integer, intent(in) :: a, b, cx
    integer :: c, d, e
    c = cx
    !$omp taskloop simd default(none) shared(u, v, w) linear(d:1) linear(c:5) lastprivate(e)
    do d = a, b
      u(d) = v(d) + w(d)
      c = c + 5
      e = c + 9
    end do
    !$omp end taskloop simd
    m = d + c + e
  end subroutine f2
  subroutine f3 (a, b)
    integer, intent(in) :: a, b
    integer, target :: d
    integer, pointer :: p
    !$omp taskloop simd default(none) shared(u, v, w) private (p)
    do d = a, b
      p => d
      u(d) = v(d) + w(d)
      p => null()
    end do
    m = d
  end subroutine f3
  subroutine f4 (a, b, c, d)
    integer, intent(in) :: a, b, c, d
    integer, target :: e, f
    integer, pointer :: p, q
    integer :: g, r
    !$omp taskloop simd default(none) shared(u, v, w) lastprivate(g) collapse(2) private (r, p, q)
    do e = a, b
      do f = c, d
        p => e
        q => f
        r = 32 * e + f
        u(r) = v(r) + w(r)
        g = r
        p => null()
        q => null()
      end do
    end do
    m = e + f + g
  end subroutine f4
  subroutine f5 (a, b, c, d)
    integer, intent(in) :: a, b, c, d
    integer :: e, f, r
    !$omp taskloop simd default(none) shared(u, v, w) collapse(2) private (r)
    do e = a, b
      do f = c, d
        r = 32 * e + f
        u(r) = v(r) + w(r)
      end do
    end do
    m = e + f
  end subroutine f5
end
