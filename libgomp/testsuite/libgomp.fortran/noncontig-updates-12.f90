! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

! Test plain, fixed-size arrays, and also pointers to same.

implicit none
integer(kind=8) :: arr(10,30)
integer, target :: arr2(9,11,13)
integer, pointer :: parr(:,:,:)
integer :: i, j, k

arr = 0
!$omp target enter data map(to: arr)

!$omp target
arr = 99
!$omp end target

!$omp target update from(arr(1:10:3,5:30:7))

do i=1,10
  do j=1,30
    if (mod(i-1,3).eq.0.and.mod(j-5,7).eq.0) then
      if (arr(i,j).ne.99) stop 1
    else
      if (arr(i,j).ne.0) stop 2
    endif
  end do
end do

!$omp target exit data map(delete: arr)

arr2 = 0
parr => arr2
!$omp target enter data map(to: parr)

!$omp target
parr = 99
!$omp end target

!$omp target update from(parr(7:9:2,5:7:2,3:6:3))

do i=1,9
  do j=1,11
    do k=1,13
      if (i.ge.7.and.j.ge.5.and.k.ge.3.and.&
          &i.le.9.and.j.le.7.and.k.le.6.and.&
          &mod(i-7,2).eq.0.and.mod(j-5,2).eq.0.and.mod(k-3,3).eq.0) then
        if (parr(i,j,k).ne.99) stop 3
      else
        if (parr(i,j,k).ne.0) stop 4
      end if
    end do
  end do
end do

!$omp target exit data map(delete: parr)

end
