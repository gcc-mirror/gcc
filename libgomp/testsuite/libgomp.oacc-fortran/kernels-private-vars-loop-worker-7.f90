! Test of worker-private variables declared on loop directive, broadcasting
! to vector-partitioned mode.  Array worker variable.

! { dg-do run }

program main
  integer :: i, j, k, idx, arr(0:32*32*32), pt(2)

  do i = 0, 32*32*32-1
     arr(i) = i
  end do

  !$acc kernels copy(arr)
  !$acc loop gang(num:32)
  do i = 0, 31
     !$acc loop worker(num:8) private(pt)
     do j = 0, 31
        pt(1) = ieor(i, j * 3)
        pt(2) = ior(i, j * 5)
        
        !$acc loop vector(length:32)
        do k = 0, 31
           arr(i * 1024 + j * 32 + k) = arr(i * 1024 + j * 32 + k) + pt(1) * k
        end do

        !$acc loop vector(length:32)
        do k = 0, 31
           arr(i * 1024 + j * 32 + k) = arr(i * 1024 + j * 32 + k) + pt(2) * k
        end do
     end do
  end do
  !$acc end kernels

  do i = 0, 32 - 1
     do j = 0, 32 -1
        do k = 0, 32 - 1
           idx = i * 1024 + j * 32 + k
           if (arr(idx) .ne. idx + ieor(i, j * 3) * k + ior(i, j * 5) * k) then
              call abort
           end if
        end do
     end do
  end do
end program main
