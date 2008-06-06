! { dg-do run }

program collapse3
  call test1
  call test2 (2, 6, -2, 4, 13, 18)
  call test3 (2, 6, -2, 4, 13, 18, 1, 1, 1)
  call test4
  call test5 (2, 6, -2, 4, 13, 18)
  call test6 (2, 6, -2, 4, 13, 18, 1, 1, 1)
contains
  subroutine test1
    integer :: i, j, k, a(1:7, -3:5, 12:19), m
    logical :: l
    l = .false.
    a(:, :, :) = 0
    !$omp parallel do collapse (3) lastprivate (i, j, k, m) reduction (.or.:l)
      do i = 2, 6
        do j = -2, 4
          do k = 13, 18
            l = l.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
            l = l.or.k.lt.13.or.k.gt.18
            if (.not.l) a(i, j, k) = a(i, j, k) + 1
            m = i * 100 + j * 10 + k
          end do
        end do
      end do
    if (i.ne.7.or.j.ne.5.or.k.ne.19) call abort
    if (m.ne.(600+40+18)) call abort
    do i = 1, 7
      do j = -3, 5
        do k = 12, 19
          if (i.eq.1.or.i.eq.7.or.j.eq.-3.or.j.eq.5.or.k.eq.12.or.k.eq.19) then
            if (a(i, j, k).ne.0) print *, i, j, k
          else
            if (a(i, j, k).ne.1) print *, 'kk', i, j, k, a(i, j, k)
          end if
        end do
      end do
    end do
  end subroutine test1

  subroutine test2(v1, v2, v3, v4, v5, v6)
    integer :: i, j, k, a(1:7, -3:5, 12:19), m
    integer :: v1, v2, v3, v4, v5, v6
    logical :: l
    l = .false.
    a(:, :, :) = 0
    !$omp parallel do collapse (3) lastprivate (i, j, k, m) reduction (.or.:l)
      do i = v1, v2
        do j = v3, v4
          do k = v5, v6
            l = l.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
            l = l.or.k.lt.13.or.k.gt.18
            if (.not.l) a(i, j, k) = a(i, j, k) + 1
            m = i * 100 + j * 10 + k
          end do
        end do
      end do
    if (i.ne.7.or.j.ne.5.or.k.ne.19) call abort
    if (m.ne.(600+40+18)) call abort
    do i = 1, 7
      do j = -3, 5
        do k = 12, 19
          if (i.eq.1.or.i.eq.7.or.j.eq.-3.or.j.eq.5.or.k.eq.12.or.k.eq.19) then
            if (a(i, j, k).ne.0) print *, i, j, k
          else
            if (a(i, j, k).ne.1) print *, 'kk', i, j, k, a(i, j, k)
          end if
        end do
      end do
    end do
  end subroutine test2

  subroutine test3(v1, v2, v3, v4, v5, v6, v7, v8, v9)
    integer :: i, j, k, a(1:7, -3:5, 12:19), m
    integer :: v1, v2, v3, v4, v5, v6, v7, v8, v9
    logical :: l
    l = .false.
    a(:, :, :) = 0
    !$omp parallel do collapse (3) lastprivate (i, j, k, m) reduction (.or.:l)
      do i = v1, v2, v7
        do j = v3, v4, v8
          do k = v5, v6, v9
            l = l.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
            l = l.or.k.lt.13.or.k.gt.18
            if (.not.l) a(i, j, k) = a(i, j, k) + 1
            m = i * 100 + j * 10 + k
          end do
        end do
      end do
    if (i.ne.7.or.j.ne.5.or.k.ne.19) call abort
    if (m.ne.(600+40+18)) call abort
    do i = 1, 7
      do j = -3, 5
        do k = 12, 19
          if (i.eq.1.or.i.eq.7.or.j.eq.-3.or.j.eq.5.or.k.eq.12.or.k.eq.19) then
            if (a(i, j, k).ne.0) print *, i, j, k
          else
            if (a(i, j, k).ne.1) print *, 'kk', i, j, k, a(i, j, k)
          end if
        end do
      end do
    end do
  end subroutine test3

  subroutine test4
    integer :: i, j, k, a(1:7, -3:5, 12:19), m
    logical :: l
    l = .false.
    a(:, :, :) = 0
    !$omp parallel do collapse (3) lastprivate (i, j, k, m) reduction (.or.:l) &
    !$omp& schedule (dynamic, 5)
      do i = 2, 6
        do j = -2, 4
          do k = 13, 18
            l = l.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
            l = l.or.k.lt.13.or.k.gt.18
            if (.not.l) a(i, j, k) = a(i, j, k) + 1
            m = i * 100 + j * 10 + k
          end do
        end do
      end do
    if (i.ne.7.or.j.ne.5.or.k.ne.19) call abort
    if (m.ne.(600+40+18)) call abort
    do i = 1, 7
      do j = -3, 5
        do k = 12, 19
          if (i.eq.1.or.i.eq.7.or.j.eq.-3.or.j.eq.5.or.k.eq.12.or.k.eq.19) then
            if (a(i, j, k).ne.0) print *, i, j, k
          else
            if (a(i, j, k).ne.1) print *, 'kk', i, j, k, a(i, j, k)
          end if
        end do
      end do
    end do
  end subroutine test4

  subroutine test5(v1, v2, v3, v4, v5, v6)
    integer :: i, j, k, a(1:7, -3:5, 12:19), m
    integer :: v1, v2, v3, v4, v5, v6
    logical :: l
    l = .false.
    a(:, :, :) = 0
    !$omp parallel do collapse (3) lastprivate (i, j, k, m) reduction (.or.:l) &
    !$omp & schedule (guided)
      do i = v1, v2
        do j = v3, v4
          do k = v5, v6
            l = l.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
            l = l.or.k.lt.13.or.k.gt.18
            if (.not.l) a(i, j, k) = a(i, j, k) + 1
            m = i * 100 + j * 10 + k
          end do
        end do
      end do
    if (i.ne.7.or.j.ne.5.or.k.ne.19) call abort
    if (m.ne.(600+40+18)) call abort
    do i = 1, 7
      do j = -3, 5
        do k = 12, 19
          if (i.eq.1.or.i.eq.7.or.j.eq.-3.or.j.eq.5.or.k.eq.12.or.k.eq.19) then
            if (a(i, j, k).ne.0) print *, i, j, k
          else
            if (a(i, j, k).ne.1) print *, 'kk', i, j, k, a(i, j, k)
          end if
        end do
      end do
    end do
  end subroutine test5

  subroutine test6(v1, v2, v3, v4, v5, v6, v7, v8, v9)
    integer :: i, j, k, a(1:7, -3:5, 12:19), m
    integer :: v1, v2, v3, v4, v5, v6, v7, v8, v9
    logical :: l
    l = .false.
    a(:, :, :) = 0
    !$omp parallel do collapse (3) lastprivate (i, j, k, m) reduction (.or.:l) &
    !$omp & schedule (dynamic)
      do i = v1, v2, v7
        do j = v3, v4, v8
          do k = v5, v6, v9
            l = l.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
            l = l.or.k.lt.13.or.k.gt.18
            if (.not.l) a(i, j, k) = a(i, j, k) + 1
            m = i * 100 + j * 10 + k
          end do
        end do
      end do
    if (i.ne.7.or.j.ne.5.or.k.ne.19) call abort
    if (m.ne.(600+40+18)) call abort
    do i = 1, 7
      do j = -3, 5
        do k = 12, 19
          if (i.eq.1.or.i.eq.7.or.j.eq.-3.or.j.eq.5.or.k.eq.12.or.k.eq.19) then
            if (a(i, j, k).ne.0) print *, i, j, k
          else
            if (a(i, j, k).ne.1) print *, 'kk', i, j, k, a(i, j, k)
          end if
        end do
      end do
    end do
  end subroutine test6

end program collapse3
