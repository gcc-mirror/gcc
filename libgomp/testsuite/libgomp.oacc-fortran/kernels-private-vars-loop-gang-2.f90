! Test of gang-private variables declared on loop directive, with broadcasting
! to partitioned workers.

! { dg-do run }

program main
  integer :: x, i, j, arr(0:32*32)

  do i = 0, 32*32 -1
     arr(i) = i
  end do

  !$acc kernels copy(arr)
  !$acc loop gang(num:32) private(x)
  do i = 0, 31
     x = i * 2;

     !$acc loop worker(num:32)
     do j = 0, 31
        arr(i * 32 + j) = arr(i * 32 + j) + x;
     end do
  end do
  !$acc end kernels

  do i = 0, 32 * 32 - 1
     if (arr(i) .ne. i + (i / 32) * 2) stop 1
  end do
end program main
