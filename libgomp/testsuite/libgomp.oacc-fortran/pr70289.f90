program foo
  implicit none
  integer :: i
  integer :: temp = 0
  integer :: temp2 = 0

  !$acc parallel
  !$acc loop gang private(temp)
  do i=1, 10000
     temp = 0
  enddo
  !$acc end parallel

  !$acc parallel reduction(+:temp2)
  !$acc loop gang reduction(+:temp2)
  do i=1, 10000
     temp2 = 0
  enddo
  !$acc end parallel
end program foo
