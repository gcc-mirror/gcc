! { dg-do run }
! { dg-additional-sources allocate-1.c }
! { dg-additional-options -Wno-complain-wrong-lang }

module m
  use omp_lib
  use iso_c_binding
  implicit none (type, external)

  interface
    integer(c_int) function is_64bit_aligned (a) bind(C)
      import :: c_int
      type(*)  :: a
    end
  end interface

contains

subroutine foo (x, p, q, h, fl)
  use omp_lib
  use iso_c_binding
  integer  :: x
  integer, dimension(4) :: p
  integer, dimension(4) :: q
  integer (kind=omp_allocator_handle_kind) :: h
  integer  :: fl

  integer  :: y
  integer  :: r, i, i1, i2, i3, i4, i5
  integer  :: l, l3, l4, l5, l6
  integer  :: n, n2, n3, n4
  integer  :: j2, j3, j4
  integer, dimension(4) :: l2
  integer, dimension(4) :: r2
  integer, target  :: xo
  integer, target  :: yo
  integer, dimension(x) :: v
  integer, dimension(x) :: w

  type s_type
    integer      :: a
    integer      :: b
  end type

  type (s_type) :: s
  s%a = 27
  s%b = 29
  y = 0
  r = 0
  n = 8
  n2 = 9
  n3 = 10
  n4 = 11
  xo = x
  yo = y

  do i = 1, 4
    r2(i) = 0;
  end do

  do i = 1, 4
    p(i) = 0;
  end do

  do i = 1, 4
    q(i) = 0;
  end do

  do i = 1, x
    w(i) = i
  end do

  !$omp parallel private (y, v) firstprivate (x) allocate (x, y, v)
  if (x /= 42) then
    stop 1
  end if

  !$omp barrier
  y = 1;
  x = x + 1
  v(1) = 7
  v(42) = 8
  !$omp barrier
  if (x /= 43 .or. y /= 1) then
    stop 3
  end if
  if (v(1) /= 7 .or. v(42) /= 8) then
    stop 4
  end if
  if ( (and(fl, 2) /= 0) .and.        &
     ((is_64bit_aligned(x) == 0) .or. &
      (is_64bit_aligned(y) == 0) .or. &
      (is_64bit_aligned(v(1)) == 0))) then
    stop 2
  end if
  !$omp end parallel
  !$omp teams
  !$omp parallel private (y) firstprivate (x, w) allocate (h: x, y, w)

  if (x /= 42 .or. w(17) /= 17 .or. w(42) /= 42) then
    stop 5
  end if
  !$omp barrier
  y = 1;
  x = x + 1
  w(19) = w(19) + 1
  !$omp barrier
  if (x /= 43 .or. y /= 1 .or. w(19) /= 20) then
    stop 6
  end if
  if ( (and(fl, 1) /= 0) .and.          &
       ((is_64bit_aligned(x) == 0) .or. &
        (is_64bit_aligned(y) == 0) .or. &
        (is_64bit_aligned(w(1)) == 0))) then
    stop 7
  end if
  !$omp end parallel
  !$omp end teams

  !$omp parallel do private (y) firstprivate (x)  reduction(+: r) allocate (h: x, y, r, l, n) lastprivate (l)  linear (n: 16)
  do i = 0, 63
    if (x /= 42) then
      stop 8
    end if
    y = 1;
    l = i;
    n = n + y + 15;
    r = r + i;
    if ( (and(fl, 1) /= 0) .and.          &
         ((is_64bit_aligned(x) == 0) .or. &
          (is_64bit_aligned(y) == 0) .or. &
          (is_64bit_aligned(r) == 0) .or. &
          (is_64bit_aligned(l) == 0) .or. &
          (is_64bit_aligned(n) == 0))) then
      stop 9
    end if
  end do
  !$omp end parallel do

  !$omp parallel
    !$omp do lastprivate (l2) private (i1) allocate (h: l2, l3, i1) lastprivate (conditional: l3)
    do i1 = 0, 63
      l2(1) = i1
      l2(2) = i1 + 1
      l2(3) = i1 + 2
      l2(4) = i1 + 3
      if (i1 < 37) then
        l3 = i1
      end if
      if ( (and(fl, 1) /= 0) .and.          &
           ((is_64bit_aligned(l2(1)) == 0) .or. &
            (is_64bit_aligned(l3) == 0) .or. &
            (is_64bit_aligned(i1) == 0))) then
        stop 10
      end if
    end do

    !$omp do collapse(2) lastprivate(l4, i2, j2) linear (n2:17) allocate (h: n2, l4, i2, j2)
    do i2 = 3, 4
      do j2 = 17, 22, 2
        n2 = n2 + 17
        l4 = i2 * 31 + j2
        if ( (and(fl, 1) /= 0) .and.          &
             ((is_64bit_aligned(l4) == 0) .or. &
              (is_64bit_aligned(n2) == 0) .or. &
              (is_64bit_aligned(i2) == 0) .or. &
              (is_64bit_aligned(j2) == 0))) then
          stop 11
        end if
      end do
    end do

    !$omp do collapse(2) lastprivate(l5, i3, j3) linear (n3:17) schedule (static, 3) allocate (n3, l5, i3, j3)
    do i3 = 3, 4
      do j3 = 17, 22, 2
          n3 = n3 + 17
          l5 = i3 * 31 + j3
          if ( (and(fl, 2) /= 0) .and.      &
             ((is_64bit_aligned(l5) == 0) .or. &
              (is_64bit_aligned(n3) == 0) .or. &
              (is_64bit_aligned(i3) == 0) .or. &
              (is_64bit_aligned(j3) == 0))) then
          stop 12
        end if
      end do
    end do

    !$omp do collapse(2) lastprivate(l6, i4, j4) linear (n4:17) schedule (dynamic) allocate (h: n4, l6, i4, j4)
    do i4 = 3, 4
      do j4 = 17, 22,2
          n4 = n4 + 17;
          l6 = i4 * 31 + j4;
        if ( (and(fl, 1) /= 0) .and.          &
            ((is_64bit_aligned(l6) == 0) .or. &
             (is_64bit_aligned(n4) == 0) .or. &
             (is_64bit_aligned(i4) == 0) .or. &
             (is_64bit_aligned(j4) == 0))) then
          stop 13
        end if
      end do
    end do

    !$omp do lastprivate (i5) allocate (i5)
    do i5 = 1, 17, 3
      if ( (and(fl, 2) /= 0) .and.          &
           (is_64bit_aligned(i5) == 0)) then
        stop 14
      end if
    end do

    !$omp do reduction(+:p, q, r2) allocate(h: p, q, r2)
    do i = 0, 31
        p(3) = p(3) +  i;
        p(4) = p(4) + (2 * i)
        q(1) = q(1) + (3 * i)
        q(3) = q(3) + (4 * i)
        r2(1) = r2(1) + (5 * i)
        r2(4) = r2(4) + (6 * i)
        if ( (and(fl, 1) /= 0) .and.             &
             ((is_64bit_aligned(q(1)) == 0) .or. &
              (is_64bit_aligned(p(1)) == 0) .or. &
              (is_64bit_aligned(r2(1)) == 0) )) then
          stop 15
        end if
    end do

    !$omp task private(y) firstprivate(x) allocate(x, y)
    if (x /= 42) then
      stop 16
    end if

    if ( (and(fl, 2) /= 0) .and.          &
      ((is_64bit_aligned(x) == 0) .or. &
      (is_64bit_aligned(y) == 0) )) then
      stop 17
    end if
    !$omp end task

    !$omp task private(y) firstprivate(x) allocate(h: x, y)
    if (x /= 42) then
      stop 16
    end if

    if ( (and(fl, 1) /= 0) .and.          &
      ((is_64bit_aligned(x) == 0) .or. &
      (is_64bit_aligned(y) == 0) )) then
      stop 17
    end if
    !$omp end task

    !$omp task private(y) firstprivate(s) allocate(s, y)
    if (s%a /= 27 .or. s%b /= 29) then
      stop 18
    end if

    if ( (and(fl, 2) /= 0) .and.          &
      ((is_64bit_aligned(s%a) == 0) .or. &
      (is_64bit_aligned(y) == 0) )) then
      stop 19
    end if
    !$omp end task

    !$omp task private(y) firstprivate(s) allocate(h: s, y)
    if (s%a /= 27 .or. s%b /= 29) then
      stop 18
    end if

    if ( (and(fl, 1) /= 0) .and.          &
      ((is_64bit_aligned(s%a) == 0) .or. &
      (is_64bit_aligned(y) == 0) )) then
      stop 19
    end if
    !$omp end task

  !$omp end parallel

  if (r /= ((64 * 63) / 2) .or. l /= 63 .or. n /= (8 + 16 * 64)) then
    stop 20
  end if

  if (l2(1) /= 63 .or. l2(2) /= 64 .or. l2(3) /= 65 .or. l2(4) /= 66 .or. l3 /= 36) then
    stop 21
  end if

  if (i2 /= 5 .or. j2 /= 23 .or. n2 /= (9 + (17 * 6)) .or. l4 /= (4 * 31 + 21)) then
    stop 22
  end if

  if (i3 /= 5 .or. j3 /= 23 .or. n3 /= (10 + (17 * 6))  .or. l5 /= (4 * 31 + 21)) then
    stop 23
  end if

  if (i4 /= 5 .or. j4 /= 23 .or. n4 /= (11 + (17 * 6))  .or. l6 /= (4 * 31 + 21)) then
    stop 24
  end if

  if (i5 /= 19) then
    stop 24
  end if

  if (p(3) /= ((32 * 31) / 2) .or. p(4) /= (2 * p(3))         &
      .or. q(1) /= (3 * p(3)) .or. q(3) /= (4 * p(3))         &
      .or. r2(1) /= (5 * p(3)) .or. r2(4) /= (6 * p(3))) then
    stop 25
  end if
end subroutine
end module m

program main
  use omp_lib
  use m
  implicit none (type, external)
  integer, dimension(4) :: p
  integer, dimension(4) :: q

  type (omp_alloctrait) :: traits(2)
  integer (omp_allocator_handle_kind) :: a

  traits = [omp_alloctrait (omp_atk_alignment, 64), &
            omp_alloctrait (omp_atk_fallback, omp_atv_null_fb)]
  a = omp_init_allocator (omp_default_mem_space, 2, traits)
  if (a == omp_null_allocator) stop 1

  call omp_set_default_allocator (omp_default_mem_alloc);
  call foo (42, p, q, a, 0);
  call foo (42, p, q, omp_default_mem_alloc, 0);
  call foo (42, p, q, a, 1);
  call omp_set_default_allocator (a);
  call foo (42, p, q, omp_null_allocator, 3);
  call foo (42, p, q, omp_default_mem_alloc, 2);
  call omp_destroy_allocator (a);
end
