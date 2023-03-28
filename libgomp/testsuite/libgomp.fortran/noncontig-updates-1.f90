! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

implicit none
integer, allocatable, target :: arr(:), arr2(:,:)
integer, pointer :: ap(:), ap2(:,:)
integer :: i, j

allocate(arr(1:20))

arr = 0

!$omp target enter data map(to: arr)

ap => arr(1:20:2)
ap = 5

!$omp target update to(ap)

!$omp target exit data map(from: arr)

do i=1,20
  if (mod(i,2).eq.1.and.arr(i).ne.5) stop 1
  if (mod(i,2).eq.0.and.arr(i).ne.0) stop 2
end do

allocate(arr2(1:20,1:20))

ap2 => arr2(2:10:2,3:12:3)

arr2 = 1

!$omp target enter data map(to: arr2)

!$omp target
ap2 = 5
!$omp end target

!$omp target update from(ap2)

do i=1,20
  do j=1,20
    if (i.ge.2.and.i.le.10.and.mod(i-2,2).eq.0.and.&
        &j.ge.3.and.j.le.12.and.mod(j-3,3).eq.0) then
      if (arr2(i,j).ne.5) stop 3
    else
      if (arr2(i,j).ne.1) stop 4
    end if
  end do
end do

!$omp target exit data map(delete: arr2)

end
