! { dg-do run }
! { dg-options "-O2" }

  integer, save :: g
  integer :: i
  !$omp parallel
  !$omp single
    if (f1 (74) .ne. 63 + 4) call abort
    g = 77
    call f2
    !$omp taskwait
    if (g .ne. 63 + 9) call abort
    if (f3 (7_8, 11_8, 2_8) .ne. 11 * 7 + 13) call abort
    if (f4 (0_8, 31_8, 16_8, 46_8, 1_8, 2_8, 73) .ne. 32 + 5 * 48 &
&       + 11 * 31 + 17 * 46) call abort
  !$omp end single
  !$omp end parallel
contains
  function f1 (y)
    integer, intent(in) :: y
    integer :: i, f1, x
    x = y
    !$omp taskloop firstprivate(x)lastprivate(x)
    do i = 0, 63
      if (x .ne. 74) call abort
      if (i .eq. 63) then
        x = i + 4
      end if
    end do
    f1 = x
  end function f1
  subroutine f2 ()
    integer :: i
    !$omp taskloop firstprivate(g)lastprivate(g)nogroup
    do i = 0, 63
      if (g .ne. 77) call abort
      if (i .eq. 63) then
        g = i + 9
      end if
    end do
  end subroutine f2
  function f3 (a, b, c)
    integer(kind=8), intent(in) :: a, b, c
    integer(kind=8) :: i, f3
    integer :: l
    !$omp taskloop default(none) lastprivate (i, l)
    do i = a, b, c
      l = i
    end do
    !$omp end taskloop
    f3 = l * 7 + i
  end function f3
  function f4 (a, b, c, d, e, f, m)
    integer(kind=8), intent(in) :: a, b, c, d, e, f
    integer(kind=8) :: i, j, f4
    integer, intent(in) :: m
    integer :: l, k
    k = m
    !$omp taskloop default (none) collapse (2) firstprivate (k) &
    !$omp & lastprivate (i, j, k, l)
    do i = a, b, e
      do j = c, d, f
        if (k .ne. 73) call abort
        if (i .eq. 31 .and. j .eq. 46) then
          k = i
        end if
        l = j
      end do
    end do
    f4 = i + 5 * j + 11 * k + 17 * l
  end function f4
end
