! { dg-do run }

program reduction
  implicit none
  integer, parameter    :: n = 100
  integer               :: i, h1, h2, s1, s2, a1, a2

  h1 = 0
  h2 = 0
  do i = 1, n
     h1 = h1 + 1
     h2 = h2 + 2
  end do

  s1 = 0
  s2 = 0
  !$acc parallel loop reduction(+:s1, s2)
  do i = 1, n
     s1 = s1 + 1
     s2 = s2 + 2
  end do
  !$acc end parallel loop

  a1 = 0
  a2 = 0
  !$acc parallel loop reduction(+:a1, a2) async(1)
  do i = 1, n
     a1 = a1 + 1
     a2 = a2 + 2
  end do
  !$acc end parallel loop

  if (h1 .ne. s1) call abort ()
  if (h2 .ne. s2) call abort ()

  !$acc wait(1)

  if (h1 .ne. a1) call abort ()
  if (h2 .ne. a2) call abort ()

end program reduction
