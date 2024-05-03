! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

! Only some of an array mapped on the target

integer, target :: arr(100)
integer, pointer :: ptr(:)

arr = [(i * 2, i=1,100)]

!$omp target enter data map(to: arr(51:100))

!$omp target
arr(51:100) = arr(51:100) + 1
!$omp end target

!$omp target update from(arr(51:100:2))

do i=1,100
  if (i.le.50) then
    if (arr(i).ne.i*2) stop 1
  else
    if (mod(i,2).eq.1 .and. arr(i).ne.i*2+1) stop 2
    if (mod(i,2).eq.0 .and. arr(i).ne.i*2) stop 3
  end if
end do

!$omp target exit data map(delete: arr)

arr = [(i * 2, i=1,100)]

! Similar, but update via pointer.

ptr => arr(51:100)

!$omp target enter data map(to: ptr(1:50))

!$omp target
ptr = ptr + 1
!$omp end target

!$omp target update from(ptr(::2))

do i=1,100
  if (i.le.50) then
    if (arr(i).ne.i*2) stop 1
  else
    if (mod(i,2).eq.1 .and. arr(i).ne.i*2+1) stop 2
    if (mod(i,2).eq.0 .and. arr(i).ne.i*2) stop 3
  end if
end do

!$omp target exit data map(delete: ptr)

end
