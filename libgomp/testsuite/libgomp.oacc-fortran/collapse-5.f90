! { dg-do run }

! collapse3.f90:test2
program collapse5
  integer :: i, j, k, a(1:7, -3:5, 12:19), b(1:7, -3:5, 12:19)
  integer :: v1, v2, v3, v4, v5, v6
  logical :: l, r
  l = .false.
  r = .false.
  a(:, :, :) = 0
  b(:, :, :) = 0
  v1 = 3
  v2 = 6
  v3 = -2
  v4 = 4
  v5 = 13
  v6 = 18
  !$acc parallel
  !$acc loop collapse (3) reduction (.or.:l)
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
  if (l .neqv. r) STOP 1
  do i = v1, v2
    do j = v3, v4
      do k = v5, v6
         if (a(i, j, k) .ne. b(i, j, k)) STOP 2
      end do
    end do
  end do
end program collapse5
