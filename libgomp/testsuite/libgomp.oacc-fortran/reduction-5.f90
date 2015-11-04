! { dg-do run }

! subroutine reduction

program reduction
  integer, parameter    :: n = 40, c = 10
  integer               :: i, vsum, sum

  call redsub (sum, n, c)

  vsum = 0

  ! Verify the results
  do i = 1, n
     vsum = vsum + c
  end do

  if (sum.ne.vsum) call abort ()
end program reduction

subroutine redsub(sum, n, c)
  integer :: sum, n, c

  integer :: s
  s = 0

  !$acc parallel vector_length(32) copyin (n, c) copy (s) num_gangs(1)
  !$acc loop reduction(+:s)
  do i = 1, n
     s = s + c
  end do
  !$acc end parallel

  sum = s
end subroutine redsub
