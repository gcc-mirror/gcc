! { dg-do run }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

  integer :: a(1024), b(1024), k, m, i, s, t
  k = 4
  m = 2
  t = 1
  do i = 1, 1024
    a(i) = i - 513
    b(i) = modulo (i - 52, 39)
    if (i.lt.52.and.b(i).ne.0) b(i) = b(i) - 39
  end do
  s = foo (b)
  do i = 1, 1024
    if (a(i).ne.((i - 513) * b(i))) STOP 1
    if (i.lt.52.and.modulo (i - 52, 39).ne.0) then
      if (b(i).ne.(modulo (i - 52, 39) - 39)) STOP 2
    else
      if (b(i).ne.(modulo (i - 52, 39))) STOP 3
    end if
    a(i) = i - 513
  end do
  if (k.ne.(4 + 3 * 1024).or.s.ne.1596127) STOP 4
  k = 4
  m = 2
  t = 1
  s = bar (b)
  do i = 1, 1024
    if (a(i).ne.((i - 513) * b(i))) STOP 5
    if (i.lt.52.and.modulo (i - 52, 39).ne.0) then
      if (b(i).ne.(modulo (i - 52, 39) - 39)) STOP 6
    else
      if (b(i).ne.(modulo (i - 52, 39))) STOP 7
    end if
    a(i) = i - 513
  end do
  if (k.ne.(4 + 3 * 1024).or.s.ne.1596127) STOP 8
  k = 4
  m = 2
  t = 1
  s = baz (b)
  do i = 1, 1024
    if (a(i).ne.((i - 513) * b(i))) STOP 9
    if (i.lt.52.and.modulo (i - 52, 39).ne.0) then
      if (b(i).ne.(modulo (i - 52, 39) - 39)) STOP 10
    else
      if (b(i).ne.(modulo (i - 52, 39))) STOP 11
    end if
  end do
  if (k.ne.(4 + 3 * 1024).or.s.ne.1596127) STOP 12
contains
  function foo (p)
    integer :: p(1024), u, v, i, s, foo
    s = 0
    !$omp simd linear(k : m + 1) reduction(+: s) lastprivate(u, v)
    do i = 1, 1024
      a(i) = a(i) * p(i)
      u = p(i) + k
      k = k + m + 1
      v = p(i) + k
      s = s + p(i) + k
    end do
    !$omp end simd
    if (i.ne.1025) STOP 13
    if (u.ne.(36 + 4 + 3 * 1023).or.v.ne.(36 + 4 + 3 * 1024)) STOP 14
    foo = s
  end function foo
  function bar (p)
    integer :: p(1024), u, v, i, s, bar
    s = 0
    !$omp simd linear(k : m + 1) reduction(+: s) lastprivate(u, v)
    do i = 1, 1024, t
      a(i) = a(i) * p(i)
      u = p(i) + k
      k = k + m + 1
      v = p(i) + k
      s = s + p(i) + k
    end do
    !$omp end simd
    if (i.ne.1025) STOP 15
    if (u.ne.(36 + 4 + 3 * 1023).or.v.ne.(36 + 4 + 3 * 1024)) STOP 16
    bar = s
  end function bar
  function baz (p)
    integer :: p(1024), u, v, i, s, baz
    s = 0
    !$omp simd linear(k : m + 1) reduction(+: s) lastprivate(u, v) &
    !$omp & linear(i : t)
    do i = 1, 1024, t
      a(i) = a(i) * p(i)
      u = p(i) + k
      k = k + m + 1
      v = p(i) + k
      s = s + p(i) + k
    end do
    if (i.ne.1025) STOP 17
    if (u.ne.(36 + 4 + 3 * 1023).or.v.ne.(36 + 4 + 3 * 1024)) STOP 18
    baz = s
  end function baz
end
