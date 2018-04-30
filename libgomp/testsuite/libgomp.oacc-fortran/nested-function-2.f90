! Exercise nested function decomposition, gcc/tree-nested.c.

! { dg-do run }

program collapse3
  integer :: p1, p2, p3, p4, p5, p6, p7, p8, p9
  p1 = 2
  p2 = 6
  p3 = -2
  p4 = 4
  p5 = 13
  p6 = 18
  p7 = 1
  p8 = 1
  p9 = 1
  call test1
  call test2 (p1, p2, p3, p4, p5, p6)
  call test3 (p1, p2, p3, p4, p5, p6, p7, p8, p9)
  call test4
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
    !$acc parallel reduction (.or.:l)
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
    !$acc parallel reduction (.or.:l)
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
    v1 = p1
    v2 = p2
    v3 = p3
    v4 = p4
    v5 = p5
    v6 = p6
    v7 = p7
    v8 = p8
    v9 = p9
    !$acc parallel reduction (.or.:l)
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

end program collapse3
