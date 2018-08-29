! { dg-do run }
! { dg-additional-options "-cpp -w" }

program reduction
  implicit none

  integer, parameter    :: n = 100, n2 = 1000, chunksize = 10
  integer               :: i, gs1, gs2, ws1, ws2, vs1, vs2, cs1, cs2, hs1, hs2
  integer               :: j, red, vred

  gs1 = 0
  gs2 = 0
  ws1 = 0
  ws2 = 0
  vs1 = 0
  vs2 = 0
  cs1 = 0
  cs2 = 0
  hs1 = 0
  hs2 = 0

  !$acc parallel num_gangs (1000)
  !$acc loop reduction(+:gs1, gs2) gang
  do i = 1, n
     gs1 = gs1 + 1
     gs2 = gs2 + 2
  end do
  !$acc end parallel

  !$acc parallel num_workers (4) vector_length (32)
  !$acc loop reduction(+:ws1, ws2) worker
  do i = 1, n
     ws1 = ws1 + 1
     ws2 = ws2 + 2
  end do
  !$acc end parallel

  !$acc parallel vector_length (32)
  !$acc loop reduction(+:vs1, vs2) vector
  do i = 1, n
     vs1 = vs1 + 1
     vs2 = vs2 + 2
  end do
  !$acc end parallel

  !$acc parallel num_gangs(8) num_workers(4) vector_length(32)
  !$acc loop reduction(+:cs1, cs2) gang worker vector
  do i = 1, n
     cs1 = cs1 + 1
     cs2 = cs2 + 2
  end do
  !$acc end parallel

  ! Verify the results on the host
  do i = 1, n
     hs1 = hs1 + 1
     hs2 = hs2 + 2
  end do

  if (gs1 .ne. hs1) STOP 1
  if (gs2 .ne. hs2) STOP 2

  if (ws1 .ne. hs1) STOP 3
  if (ws2 .ne. hs2) STOP 4

  if (vs1 .ne. hs1) STOP 5
  if (vs2 .ne. hs2) STOP 6

  if (cs1 .ne. hs1) STOP 7
  if (cs2 .ne. hs2) STOP 8

  ! Nested reductions.

  red = 0
  vred = 0

  !$acc parallel num_gangs(10) vector_length(32)
  !$acc loop reduction(+:red) gang
  do i = 1, n/chunksize
     !$acc loop reduction(+:red) vector
     do j = 1, chunksize
        red = red + chunksize
     end do
  end do
  !$acc end parallel

  do i = 1, n/chunksize
     do j = 1, chunksize
        vred = vred + chunksize
     end do
  end do

  if (red .ne. vred) STOP 9
end program reduction
