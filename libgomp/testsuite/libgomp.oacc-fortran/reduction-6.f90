! { dg-do run }

program reduction
  implicit none

  integer, parameter    :: n = 100
  integer               :: i, s1, s2, vs1, vs2

  s1 = 0
  s2 = 0
  vs1 = 0
  vs2 = 0

  !$acc parallel vector_length (1000)
  !$acc loop reduction(+:s1, s2)
  do i = 1, n
     s1 = s1 + 1
     s2 = s2 + 2
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     vs1 = vs1 + 1
     vs2 = vs2 + 2
  end do

  if (s1.ne.vs1) call abort ()
  if (s2.ne.vs2) call abort ()
end program reduction
