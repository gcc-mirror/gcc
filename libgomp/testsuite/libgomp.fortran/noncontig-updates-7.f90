! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

! Assumed-shape arrays

program p
implicit none
integer, dimension(100) :: parr
integer :: i

parr = [(i, i=1,100)]

!$omp target enter data map(to: parr)

call s(parr)

do i=1,100
  if (mod(i,3).eq.1 .and. parr(i).ne.999) stop 1
  if (mod(i,3).ne.1 .and. parr(i).ne.i) stop 2
end do

!$omp target exit data map(delete: parr)

contains
subroutine s(arr)
implicit none
integer, intent(inout) :: arr(:)

!$omp target
arr = 999
!$omp end target

!$omp target update from(arr(1:100:3))

end subroutine s
end program p
