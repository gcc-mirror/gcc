! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

implicit none
integer, allocatable :: arr(:,:,:,:,:)
integer :: i, j, k, l, m

allocate (arr(18,19,20,21,22))

arr = 0

!$omp target enter data map(to: arr)

arr = 10

!$omp target update to(arr(1:3:2,1:4:3,1:5:4,1:6:5,1:7:6))

!$omp target
do i=1,18
  do j=1,19
    do k=1,20
      do l=1,21
        do m=1,22
          if ((i.eq.1.or.i.eq.3).and.&
              &(j.eq.1.or.j.eq.4).and.&
              &(k.eq.1.or.k.eq.5).and.&
              &(l.eq.1.or.l.eq.6).and.&
              &(m.eq.1.or.m.eq.7)) then
            if (arr(i,j,k,l,m).ne.10) stop 1
          else
            if (arr(i,j,k,l,m).ne.0) stop 2
          end if
        end do
      end do
    end do
  end do
end do
!$omp end target

!$omp target exit data map(delete: arr)

end
