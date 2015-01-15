! { dg-do run }

! collapse3.f90:test4
program collapse7
  integer :: i, j, k, a(1:7, -3:5, 12:19), b(1:7, -3:5, 12:19)
  logical :: l, r
  l = .false.
  r = .false.
  a(:, :, :) = 0
  b(:, :, :) = 0
  !$acc parallel
  !$acc loop collapse (3) reduction (.or.:l)
    do i = 2, 6
      do j = -2, 4
        do k = 13, 18
          l = l.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
          l = l.or.k.lt.13.or.k.gt.18
          if (.not.l) a(i, j, k) = a(i, j, k) + 1
        end do
      end do
    end do
  !$acc end parallel
  do i = 2, 6
    do j = -2, 4
      do k = 13, 18
        r = r.or.i.lt.2.or.i.gt.6.or.j.lt.-2.or.j.gt.4
        r = r.or.k.lt.13.or.k.gt.18
        if (.not.r) b(i, j, k) = b(i, j, k) + 1
      end do
    end do
  end do
  if (l .neqv. r) call abort
  do i = 1, 7
    do j = -3, 5
      do k = 12, 19
         if (a(i, j, k) .ne. b(i, j, k)) call abort
      end do
    end do
  end do
end program collapse7
