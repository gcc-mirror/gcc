! { dg-do run }
! { dg-options "-std=legacy" }

program collapse2
  integer :: i, j, k, a(1:3, 4:6, 5:7)
  logical :: l
  l = .false.
  a(:, :, :) = 0
  !$acc parallel
  !$acc loop collapse(4 - 1)
    do 164 i = 1, 3
      do 164 j = 4, 6
        do 164 k = 5, 7
          a(i, j, k) = i + j + k
164      end do
  !$acc end parallel

  !$acc parallel
  !$acc loop collapse(2) reduction(.or.:l)
firstdo: do i = 1, 3
      do j = 4, 6
        do k = 5, 7
          if (a(i, j, k) .ne. (i + j + k)) l = .true.
        end do
      end do
    end do firstdo
  !$acc end parallel
  if (l) STOP 1
end program collapse2
