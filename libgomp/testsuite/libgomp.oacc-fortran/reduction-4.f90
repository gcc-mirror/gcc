! { dg-do run }

! complex reductions

program reduction_4
  implicit none

  integer, parameter    :: n = 10, vl = 32
  integer               :: i
  complex               :: vresult, result
  complex, dimension (n) :: array

  do i = 1, n
     array(i) = i
  end do

  result = 0
  vresult = 0

  ! '+' reductions

  !$acc parallel vector_length(vl) num_gangs(1)
  !$acc loop reduction(+:result)
  do i = 1, n
     result = result + array(i)
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     vresult = vresult + array(i)
  end do

  if (result .ne. vresult) call abort

  result = 1
  vresult = 1

!  ! '*' reductions
!
!  !$acc parallel vector_length(vl)
!  !$acc loop reduction(*:result)
!  do i = 1, n
!     result = result * array(i)
!  end do
!  !$acc end parallel
!
!  ! Verify the results
!  do i = 1, n
!     vresult = vresult * array(i)
!  end do
!
!  if (result.ne.vresult) call abort
end program reduction_4
