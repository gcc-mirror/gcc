! { dg-do run }
! { dg-require-effective-target offload_device_nonshared_as }

character(len=8), allocatable, dimension(:) :: lines
integer :: i

allocate(lines(10))

lines = "OMPHELLO"

!$omp target enter data map(to: lines)

!$omp target
lines = "NEWVALUE"
!$omp end target

!$omp target update from(lines(5:7:2))

do i=1,10
  if (i.eq.5.or.i.eq.7) then
    if (lines(i).ne."NEWVALUE") stop 1
  else
    if (lines(i).ne."OMPHELLO") stop 2
  end if
end do

!$omp target exit data map(delete: lines)

end
