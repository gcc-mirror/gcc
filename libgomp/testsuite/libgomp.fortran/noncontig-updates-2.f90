! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

program p
implicit none
integer, allocatable, target :: arr3(:,:,:)
integer, pointer :: ap3(:,:,:)
integer :: i, j, k

allocate(arr3(1:10,1:10,1:10))

! CHECK 1

arr3 = 0
ap3 => arr3(1:10,1:10,1:10:2)

!$omp target enter data map(to: arr3)

!$omp target
ap3 = 5
!$omp end target

!$omp target update from(ap3)

call check(arr3, 0, 1, 1, 2)

!$omp target exit data map(delete: arr3)

! CHECK 2

arr3 = 0
ap3 => arr3(1:10,1:10:2,1:10)

!$omp target enter data map(to: arr3)

!$omp target
ap3 = 5
!$omp end target

!$omp target update from(ap3)

call check(arr3, 2, 1, 2, 1)

!$omp target exit data map(delete: arr3)

! CHECK 3

arr3 = 0
ap3 => arr3(1:10:2,1:10,1:10)

!$omp target enter data map(to: arr3)

!$omp target
ap3 = 5
!$omp end target

!$omp target update from(ap3)

call check(arr3, 4, 2, 1, 1)

!$omp target exit data map(delete: arr3)

! CHECK 4

arr3 = 0
ap3 => arr3(1:10:2,1:10:2,1:10:2)

!$omp target enter data map(to: arr3)

!$omp target
ap3 = 5
!$omp end target

!$omp target update from(ap3)

call check(arr3, 6, 2, 2, 2)

!$omp target exit data map(delete: arr3)

contains

subroutine check(arr,cb,s1,s2,s3)
implicit none
integer :: arr(:,:,:)
integer :: cb, s1, s2, s3

do i=1,10
  do j=1,10
    do k=1,10
      if (mod(k-1,s1).eq.0.and.mod(j-1,s2).eq.0.and.mod(i-1,s3).eq.0) then
        if (arr(k,j,i).ne.5) stop cb+1
      else
        if (arr(k,j,i).ne.0) stop cb+2
      end if
    end do
  end do
end do

end subroutine check

end program p
