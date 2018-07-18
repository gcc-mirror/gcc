! { dg-do run }
! { dg-additional-options "-w" }

! Integer reductions

program reduction_1
  implicit none

  integer, parameter    :: n = 10, ng = 8, nw = 4, vl = 32
  integer               :: i, vresult, rg, rw, rv, rc
  logical               :: lrg, lrw, lrv, lrc, lvresult
  integer, dimension (n) :: array

  do i = 1, n
     array(i) = i
  end do

  !
  ! '+' reductions
  !

  rg = 0
  rw = 0
  rv = 0
  rc = 0
  vresult = 0

  !$acc parallel num_gangs(ng) copy(rg)
  !$acc loop reduction(+:rg) gang
  do i = 1, n
     rg = rg + array(i)
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(+:rw) worker
  do i = 1, n
     rw = rw + array(i)
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(+:rv) vector
  do i = 1, n
     rv = rv + array(i)
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(+:rc) gang worker vector
  do i = 1, n
     rc = rc + array(i)
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     vresult = vresult + array(i)
  end do

  if (rg .ne. vresult) STOP 1
  if (rw .ne. vresult) STOP 2
  if (rv .ne. vresult) STOP 3
  if (rc .ne. vresult) STOP 4

  !
  ! '*' reductions
  !

  rg = 1
  rw = 1
  rv = 1
  rc = 1
  vresult = 1

  !$acc parallel num_gangs(ng) copy(rg)
  !$acc loop reduction(*:rg) gang
  do i = 1, n
     rg = rg * array(i)
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(*:rw) worker
  do i = 1, n
     rw = rw * array(i)
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(*:rv) vector
  do i = 1, n
     rv = rv * array(i)
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(*:rc) gang worker vector
  do i = 1, n
     rc = rc * array(i)
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     vresult = vresult * array(i)
  end do

  if (rg .ne. vresult) STOP 5
  if (rw .ne. vresult) STOP 6
  if (rv .ne. vresult) STOP 7
  if (rc .ne. vresult) STOP 8

  !
  ! 'max' reductions
  !

  rg = 0
  rw = 0
  rv = 0
  rc = 0
  vresult = 0

  !$acc parallel num_gangs(ng) copy(rg)
  !$acc loop reduction(max:rg) gang
  do i = 1, n
     rg = max (rg, array(i))
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(max:rw) worker
  do i = 1, n
     rw = max (rw, array(i))
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(max:rv) vector
  do i = 1, n
     rv = max (rv, array(i))
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) Num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(max:rc) gang worker vector
  do i = 1, n
     rc = max (rc, array(i))
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     vresult = max (vresult, array(i))
  end do

  if (rg .ne. vresult) STOP 9
  if (rw .ne. vresult) STOP 10
  if (rv .ne. vresult) STOP 11
  if (rc .ne. vresult) STOP 12

  !
  ! 'min' reductions
  !

  rg = 0
  rw = 0
  rv = 0
  rc = 0
  vresult = 0

  !$acc parallel num_gangs(ng) copy(rg)
  !$acc loop reduction(min:rg) gang
  do i = 1, n
     rg = min (rg, array(i))
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(min:rw) worker
  do i = 1, n
     rw = min (rw, array(i))
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(min:rv) vector
  do i = 1, n
     rv = min (rv, array(i))
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(min:rc) gang worker vector
  do i = 1, n
     rc = min (rc, array(i))
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     vresult = min (vresult, array(i))
  end do

  if (rg .ne. vresult) STOP 13
  if (rw .ne. vresult) STOP 14
  if (rv .ne. vresult) STOP 15
  if (rc .ne. vresult) STOP 16

  !
  ! 'iand' reductions
  !

  rg = 1
  rw = 1
  rv = 1
  rc = 1
  vresult = 1

  !$acc parallel num_gangs(ng) copy(rg)
  !$acc loop reduction(iand:rg) gang
  do i = 1, n
     rg = iand (rg, array(i))
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(iand:rw) worker
  do i = 1, n
     rw = iand (rw, array(i))
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(iand:rv) vector
  do i = 1, n
     rv = iand (rv, array(i))
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(iand:rc) gang worker vector
  do i = 1, n
     rc = iand (rc, array(i))
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     vresult = iand (vresult, array(i))
  end do

  if (rg .ne. vresult) STOP 17
  if (rw .ne. vresult) STOP 18
  if (rv .ne. vresult) STOP 19
  if (rc .ne. vresult) STOP 20

  !
  ! 'ior' reductions
  !

  rg = 0
  rw = 0
  rv = 0
  rc = 0
  vresult = 0

  !$acc parallel num_gangs(ng) copy(rg)
  !$acc loop reduction(ior:rg) gang
  do i = 1, n
     rg = ior (rg, array(i))
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(ior:rw) worker
  do i = 1, n
     rw = ior (rw, array(i))
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(ior:rv) gang
  do i = 1, n
     rv = ior (rv, array(i))
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(ior:rc) gang worker vector
  do i = 1, n
     rc = ior (rc, array(i))
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     vresult = ior (vresult, array(i))
  end do

  if (rg .ne. vresult) STOP 21
  if (rw .ne. vresult) STOP 22
  if (rv .ne. vresult) STOP 23
  if (rc .ne. vresult) STOP 24

  !
  ! 'ieor' reductions
  !

  rg = 0
  rw = 0
  rv = 0
  rc = 0
  vresult = 0

  !$acc parallel num_gangs(ng) copy(rg)
  !$acc loop reduction(ieor:rg) gang
  do i = 1, n
     rg = ieor (rg, array(i))
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(ieor:rw) worker
  do i = 1, n
     rw = ieor (rw, array(i))
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(ieor:rv) vector
  do i = 1, n
     rv = ieor (rv, array(i))
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(ieor:rc) gang worker vector
  do i = 1, n
     rc = ieor (rc, array(i))
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     vresult = ieor (vresult, array(i))
  end do

  if (rg .ne. vresult) STOP 25
  if (rw .ne. vresult) STOP 26
  if (rv .ne. vresult) STOP 27
  if (rc .ne. vresult) STOP 28

  !
  ! '.and.' reductions
  !

  lrg = .true.
  lrw = .true.
  lrv = .true.
  lrc = .true.
  lvresult = .true.

  !$acc parallel num_gangs(ng) copy(lrg)
  !$acc loop reduction(.and.:lrg) gang
  do i = 1, n
     lrg = lrg .and. (array(i) .ge. 5)
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(lrw)
  !$acc loop reduction(.and.:lrw) worker
  do i = 1, n
     lrw = lrw .and. (array(i) .ge. 5)
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(lrv)
  !$acc loop reduction(.and.:lrv) vector
  do i = 1, n
     lrv = lrv .and. (array(i) .ge. 5)
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(lrc)
  !$acc loop reduction(.and.:lrc) gang worker vector
  do i = 1, n
     lrc = lrc .and. (array(i) .ge. 5)
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     lvresult = lvresult .and. (array(i) .ge. 5)
  end do

  if (lrg .neqv. lvresult) STOP 29
  if (lrw .neqv. lvresult) STOP 30
  if (lrv .neqv. lvresult) STOP 31
  if (lrc .neqv. lvresult) STOP 32

  !
  ! '.or.' reductions
  !

  lrg = .true.
  lrw = .true.
  lrv = .true.
  lrc = .true.
  lvresult = .false.

  !$acc parallel num_gangs(ng) copy(lrg)
  !$acc loop reduction(.or.:lrg) gang
  do i = 1, n
     lrg = lrg .or. (array(i) .ge. 5)
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(lrw)
  !$acc loop reduction(.or.:lrw) worker
  do i = 1, n
     lrw = lrw .or. (array(i) .ge. 5)
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(lrv)
  !$acc loop reduction(.or.:lrv) vector
  do i = 1, n
     lrv = lrv .or. (array(i) .ge. 5)
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(lrc)
  !$acc loop reduction(.or.:lrc) gang worker vector
  do i = 1, n
     lrc = lrc .or. (array(i) .ge. 5)
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     lvresult = lvresult .or. (array(i) .ge. 5)
  end do

  if (lrg .neqv. lvresult) STOP 33
  if (lrw .neqv. lvresult) STOP 34
  if (lrv .neqv. lvresult) STOP 35
  if (lrc .neqv. lvresult) STOP 36

  !
  ! '.eqv.' reductions
  !

  lrg = .true.
  lrw = .true.
  lrv = .true.
  lrc = .true.
  lvresult = .true.

  !$acc parallel num_gangs(ng) copy(lrg)
  !$acc loop reduction(.eqv.:lrg) gang
  do i = 1, n
     lrg = lrg .eqv. (array(i) .ge. 5)
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(lrw)
  !$acc loop reduction(.eqv.:lrw) worker
  do i = 1, n
     lrw = lrw .eqv. (array(i) .ge. 5)
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(lrv)
  !$acc loop reduction(.eqv.:lrv) vector
  do i = 1, n
     lrv = lrv .eqv. (array(i) .ge. 5)
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) vector_length(vl) copy(lrc)
  !$acc loop reduction(.eqv.:lrc) gang worker vector
  do i = 1, n
     lrc = lrc .eqv. (array(i) .ge. 5)
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     lvresult = lvresult .eqv. (array(i) .ge. 5)
  end do

  if (lrg .neqv. lvresult) STOP 37
  if (lrw .neqv. lvresult) STOP 38
  if (lrv .neqv. lvresult) STOP 39
  if (lrc .neqv. lvresult) STOP 40

  !
  ! '.neqv.' reductions
  !

  lrg = .true.
  lrw = .true.
  lrv = .true.
  lrc = .true.
  lvresult = .true.

  !$acc parallel num_gangs(ng) copy(lrg)
  !$acc loop reduction(.neqv.:lrg) gang
  do i = 1, n
     lrg = lrg .neqv. (array(i) .ge. 5)
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(lrw)
  !$acc loop reduction(.neqv.:lrw) worker
  do i = 1, n
     lrw = lrw .neqv. (array(i) .ge. 5)
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(lrv)
  !$acc loop reduction(.neqv.:lrv) vector
  do i = 1, n
     lrv = lrv .neqv. (array(i) .ge. 5)
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(lrc)
  !$acc loop reduction(.neqv.:lrc) gang worker vector
  do i = 1, n
     lrc = lrc .neqv. (array(i) .ge. 5)
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     lvresult = lvresult .neqv. (array(i) .ge. 5)
  end do

  if (lrg .neqv. lvresult) STOP 41
  if (lrw .neqv. lvresult) STOP 42
  if (lrv .neqv. lvresult) STOP 43
  if (lrc .neqv. lvresult) STOP 44
end program reduction_1
