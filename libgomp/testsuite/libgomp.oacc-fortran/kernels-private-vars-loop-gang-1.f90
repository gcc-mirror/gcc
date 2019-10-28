! Test of gang-private variables declared on loop directive.

! { dg-do run }

program main
  integer :: x, i, arr(32)

  do i = 1, 32
     arr(i) = i
  end do

  !$acc kernels copy(arr)
  !$acc loop gang(num:32) private(x)
  do i = 1, 32
     x = i * 2;
     arr(i) = arr(i) + x;
  end do
  !$acc end kernels

  do i = 1, 32
     if (arr(i) .ne. i * 3) stop 1
  end do
end program main
