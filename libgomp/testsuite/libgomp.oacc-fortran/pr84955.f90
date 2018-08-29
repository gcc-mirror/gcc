! { dg-do compile }

subroutine s
   integer :: i, j
   !$acc parallel loop tile(2,3)
   do i = 1, 10
      do j = 1, 10
         do
         end do
      end do
   end do
  !$acc end parallel loop
end subroutine s
