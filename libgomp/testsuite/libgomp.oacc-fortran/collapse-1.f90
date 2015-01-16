! { dg-do run }

program collapse1
  integer :: i, j, k, a(1:3, 4:6, 5:7)
  logical :: l
  l = .false.
  a(:, :, :) = 0
  !$acc parallel
  !$acc loop collapse(4 - 1)
    do i = 1, 3
      do j = 4, 6
        do k = 5, 7
          a(i, j, k) = i + j + k
        end do
      end do
    end do
  !$acc loop collapse(2) reduction(.or.:l)
    do i = 1, 3
      do j = 4, 6
        do k = 5, 7
          if (a(i, j, k) .ne. (i + j + k)) l = .true.
        end do
      end do
    end do
  !$acc end parallel
  if (l) call abort
end program collapse1
