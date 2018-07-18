! { dg-do run }
! { dg-additional-options "-w" }

! subroutine reduction

program reduction
  integer, parameter    :: n = 40, c = 10
  integer               :: i, vsum, gs, ws, vs, cs, ns

  call redsub_gang (gs, n, c)
  call redsub_worker (ws, n, c)
  call redsub_vector (vs, n, c)
  call redsub_combined (cs, n, c)
  call redsub_nested (ns, n, c)

  vsum = 0

  ! Verify the results
  do i = 1, n
     vsum = vsum + c
  end do

  if (gs .ne. vsum) STOP 1
  if (ws .ne. vsum) STOP 2
  if (vs .ne. vsum) STOP 3
  if (cs .ne. vsum) STOP 4
  if (ns .ne. vsum) STOP 5
end program reduction

subroutine redsub_gang(sum, n, c)
  integer :: sum, n, c

  sum = 0

  !$acc parallel copyin (n, c) num_gangs(n) copy(sum)
  !$acc loop reduction(+:sum) gang
  do i = 1, n
     sum = sum + c
  end do
  !$acc end parallel
end subroutine redsub_gang

subroutine redsub_worker(sum, n, c)
  integer :: sum, n, c

  sum = 0

  !$acc parallel copyin (n, c) num_workers(4) vector_length (32) copy(sum)
  !$acc loop reduction(+:sum) worker
  do i = 1, n
     sum = sum + c
  end do
  !$acc end parallel
end subroutine redsub_worker

subroutine redsub_vector(sum, n, c)
  integer :: sum, n, c

  sum = 0

  !$acc parallel copyin (n, c) vector_length(32) copy(sum)
  !$acc loop reduction(+:sum) vector
  do i = 1, n
     sum = sum + c
  end do
  !$acc end parallel
end subroutine redsub_vector

subroutine redsub_combined(sum, n, c)
  integer :: sum, n, c

  sum = 0

  !$acc parallel num_gangs (8) num_workers (4) vector_length(32) copy(sum)
  !$acc loop reduction(+:sum) gang worker vector
  do i = 1, n
     sum = sum + c
  end do
  !$acc end parallel
end subroutine redsub_combined

subroutine redsub_nested(sum, n, c)
  integer :: sum, n, c
  integer :: ii, jj

  ii = n / 10;
  jj = 10;
  sum = 0

  !$acc parallel num_gangs (8) copy(sum)
  !$acc loop reduction(+:sum) gang
  do i = 1, ii
     !$acc loop reduction(+:sum) vector
     do j = 1, jj
        sum = sum + c
     end do
  end do
  !$acc end parallel
end subroutine redsub_nested
