! Exercise nested function decomposition, gcc/tree-nested.c.

! { dg-do run }

program sub_collapse_3
  call test1
  call test2 (2, 6, -2, 4, 13, 18)
  call test3 (2, 6, -2, 4, 13, 18, 1, 1, 1)
  call test4
  call test5 (2, 6, -2, 4, 13, 18)
  call test6 (2, 6, -2, 4, 13, 18, 1, 1, 1)
contains
  subroutine test1
    integer :: a(3,3,3), k, kk, kkk, l, ll, lll
    !$acc parallel
    !$acc loop collapse(3)
      do 115 k=1,3
dokk:   do kk=1,3
          do kkk=1,3
            a(k,kk,kkk) = 1
          enddo
        enddo dokk
115   continue
    !$acc end parallel
    if (any(a(1:3,1:3,1:3).ne.1)) STOP 1
    !$acc parallel
    !$acc loop collapse(3)
dol:  do 120 l=1,3
doll:   do ll=1,3
          do lll=1,3
            a(l,ll,lll) = 2
          enddo
        enddo doll
120   end do dol
    !$acc end parallel
    if (any(a(1:3,1:3,1:3).ne.2)) STOP 2
  end subroutine test1

  subroutine test2(v1, v2, v3, v4, v5, v6)
    integer :: i, j, k, a(1:7, -3:5, 12:19), b(1:7, -3:5, 12:19)
    integer :: v1, v2, v3, v4, v5, v6
    logical :: l, r
    l = .false.
    r = .false.
    a(:, :, :) = 0
    b(:, :, :) = 0
    !$acc parallel pcopyin (v1, v2, v3, v4, v5, v6) reduction (.or.:l)
    !$acc loop reduction (.or.:l) collapse (3)
      do i = v1, v2
        do j = v3, v4
          do k = v5, v6
            l = l.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
            l = l.or.k.lt.13.or.k.gt.18
            if (.not.l) a(i, j, k) = a(i, j, k) + 1
          end do
        end do
      end do
    !$acc end parallel
    do i = v1, v2
      do j = v3, v4
        do k = v5, v6
          r = r.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
          r = r.or.k.lt.13.or.k.gt.18
          if (.not.l) b(i, j, k) = b(i, j, k) + 1
        end do
      end do
    end do
    if (l .neqv. r) STOP 3
    do i = v1, v2
      do j = v3, v4
        do k = v5, v6
           if (a(i, j, k) .ne. b(i, j, k)) STOP 4
        end do
      end do
    end do
  end subroutine test2

  subroutine test3(v1, v2, v3, v4, v5, v6, v7, v8, v9)
    integer :: i, j, k, a(1:7, -3:5, 12:19), b(1:7, -3:5, 12:19)
    integer :: v1, v2, v3, v4, v5, v6, v7, v8, v9
    logical :: l, r
    l = .false.
    r = .false.
    a(:, :, :) = 0
    b(:, :, :) = 0
    !$acc parallel pcopyin (v1, v2, v3, v4, v5, v6, v7, v8, v9) reduction (.or.:l)
    !$acc loop reduction (.or.:l) collapse (3)
      do i = v1, v2, v7
        do j = v3, v4, v8
          do k = v5, v6, v9
            l = l.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
            l = l.or.k.lt.13.or.k.gt.18
            if (.not.l) a(i, j, k) = a(i, j, k) + 1
          end do
        end do
      end do
    !$acc end parallel
    do i = v1, v2, v7
      do j = v3, v4, v8
        do k = v5, v6, v9
          r = r.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
          r = r.or.k.lt.13.or.k.gt.18
          if (.not.l) b(i, j, k) = b(i, j, k) + 1
        end do
      end do
    end do
    if (l .neqv. r) STOP 5
    do i = v1, v2, v7
      do j = v3, v4, v8
        do k = v5, v6, v9
           if (a(i, j, k) .ne. b(i, j, k)) STOP 6
        end do
      end do
    end do
  end subroutine test3

  subroutine test4
    integer :: i, j, k, a(1:7, -3:5, 12:19), b(1:7, -3:5, 12:19)
    integer :: v1, v2, v3, v4, v5, v6, v7, v8, v9
    logical :: l, r
    l = .false.
    r = .false.
    a(:, :, :) = 0
    b(:, :, :) = 0
    v1 = 2
    v2 = 6
    v3 = -2
    v4 = 4
    v5 = 13
    v6 = 18
    v7 = 1
    v8 = 1
    v9 = 1
    !$acc parallel pcopyin (v1, v2, v3, v4, v5, v6, v7, v8, v9) reduction (.or.:l)
    !$acc loop reduction (.or.:l) collapse (3)
      do i = v1, v2, v7
        do j = v3, v4, v8
          do k = v5, v6, v9
            l = l.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
            l = l.or.k.lt.13.or.k.gt.18
            if (.not.l) a(i, j, k) = a(i, j, k) + 1
          end do
        end do
      end do
    !$acc end parallel
    do i = v1, v2, v7
      do j = v3, v4, v8
        do k = v5, v6, v9
          r = r.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
          r = r.or.k.lt.13.or.k.gt.18
          if (.not.r) b(i, j, k) = b(i, j, k) + 1
        end do
      end do
    end do
    if (l .neqv. r) STOP 7
    do i = v1, v2, v7
      do j = v3, v4, v8
         do k = v5, v6, v9
           if (a(i, j, k) .ne. b(i, j, k)) STOP 8
         end do
      end do
    end do
  end subroutine test4

  subroutine test5(v1, v2, v3, v4, v5, v6)
    integer :: i, j, k, a(1:7, -3:5, 12:19), b(1:7, -3:5, 12:19)
    integer :: v1, v2, v3, v4, v5, v6
    logical :: l, r
    l = .false.
    r = .false.
    a(:, :, :) = 0
    b(:, :, :) = 0
    !$acc parallel pcopyin (v1, v2, v3, v4, v5, v6) reduction (.or.:l)
    !$acc loop reduction (.or.:l) collapse (3)
      do i = v1, v2
        do j = v3, v4
          do k = v5, v6
            l = l.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
            l = l.or.k.lt.13.or.k.gt.18
            if (.not.l) a(i, j, k) = a(i, j, k) + 1
          end do
        end do
      end do
    !$acc end parallel
    do i = v1, v2
      do j = v3, v4
        do k = v5, v6
          r = r.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
          r = r.or.k.lt.13.or.k.gt.18
          if (.not.r) b(i, j, k) = b(i, j, k) + 1
        end do
      end do
    end do
    if (l .neqv. r) STOP 9
    do i = v1, v2
      do j = v3, v4
        do k = v5, v6
           if (a(i, j, k) .ne. b(i, j, k)) STOP 10
        end do
      end do
    end do
  end subroutine test5

  subroutine test6(v1, v2, v3, v4, v5, v6, v7, v8, v9)
    integer :: i, j, k, a(1:7, -3:5, 12:19), b(1:7, -3:5, 12:19)
    integer :: v1, v2, v3, v4, v5, v6, v7, v8, v9
    logical :: l, r
    l = .false.
    r = .false.
    a(:, :, :) = 0
    b(:, :, :) = 0
    !$acc parallel pcopyin (v1, v2, v3, v4, v5, v6, v7, v8, v9) reduction (.or.:l)
    !$acc loop reduction (.or.:l) collapse (3)
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
    !$acc end parallel
    do i = v1, v2, v7
      do j = v3, v4, v8
        do k = v5, v6, v9
          r = r.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
          r = r.or.k.lt.13.or.k.gt.18
          if (.not.r) b(i, j, k) = b(i, j, k) + 1
        end do
      end do
    end do
    if (l .neqv. r) STOP 11
    do i = v1, v2, v7
      do j = v3, v4, v8
        do k = v5, v6, v9
           if (a(i, j, k) .ne. b(i, j, k)) STOP 12
        end do
      end do
    end do
  end subroutine test6

end program sub_collapse_3
