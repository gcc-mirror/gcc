! { dg-do run }

  integer :: i, j, k
  !$omp parallel do lastprivate (i, j, k) collapse (3)
    do i = 0, 17
      do j = 0, 6
        do k = 0, 5
        end do
      end do
    end do
  if (i .ne. 18 .or. j .ne. 7 .or. k .ne. 6) stop 1
end
