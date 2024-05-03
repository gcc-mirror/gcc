! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

! Test biasing for target-region lookup.

implicit none
integer, allocatable, target :: var(:,:,:)
integer, pointer :: p(:,:,:)
integer :: i, j, k

allocate(var(1:20,5:25,10:30))

var = 0

!$omp target enter data map(to: var)

!$omp target
var = 99
!$omp end target

p => var(1:3:2,5:5,10:10)

!$omp target update from(p)

do i=1,20
  do j=5,25
    do k=10,30
      if ((i.eq.1.or.i.eq.3).and.j.eq.5.and.k.eq.10) then
        if (var(i,j,k).ne.99) stop 1
      else
        if (var(i,j,k).ne.0) stop 2
      end if
    end do
  end do
end do

!$omp target exit data map(delete: var)

end
