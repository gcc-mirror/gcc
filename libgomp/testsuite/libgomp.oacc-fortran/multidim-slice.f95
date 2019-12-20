! { dg-do run }

program main
  implicit none
  real, allocatable :: myarr(:,:,:,:,:)
  integer i, j, k, l, m

  allocate(myarr(1:10,1:10,1:10,1:10,1:10))

  do i=1,10
    do j=1,10
      do k=1,10
        do l=1,10
          do m=1,10
            myarr(m,l,k,j,i) = i+j+k+l+m
          end do
        end do
      end do
    end do
  end do

  do i=1,10
    !$acc data copy(myarr(:,:,:,:,i))
    !$acc parallel loop collapse(4) present(myarr(:,:,:,:,i))
    do j=1,10
      do k=1,10
        do l=1,10
          do m=1,10
            myarr(m,l,k,j,i) = myarr(m,l,k,j,i) + 1
          end do
        end do
      end do
    end do
    !$acc end parallel loop
    !$acc end data
  end do

  do i=1,10
    do j=1,10
      do k=1,10
        do l=1,10
          do m=1,10
            if (myarr(m,l,k,j,i) .ne. i+j+k+l+m+1) stop 1
          end do
        end do
      end do
    end do
  end do

end program main
