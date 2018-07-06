! { dg-do run }

! double precision reductions

program reduction_3
  implicit none

  integer, parameter    :: n = 10, ng = 8, nw = 4, vl = 32
  integer               :: i
  double precision      :: vresult, rg, rw, rv, rc
  double precision, parameter :: e = 0.001
  logical               :: lrg, lrw, lrv, lrc, lvresult
  double precision, dimension (n) :: array

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

  if (abs (rg - vresult) .ge. e) STOP 1
  if (abs (rw - vresult) .ge. e) STOP 2
  if (abs (rv - vresult) .ge. e) STOP 3
  if (abs (rc - vresult) .ge. e) STOP 4

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

  if (abs (rg - vresult) .ge. e) STOP 5
  if (abs (rw - vresult) .ge. e) STOP 6
  if (abs (rv - vresult) .ge. e) STOP 7
  if (abs (rc - vresult) .ge. e) STOP 8

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

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(max:rc) gang worker vector
  do i = 1, n
     rc = max (rc, array(i))
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     vresult = max (vresult, array(i))
  end do

  if (abs (rg - vresult) .ge. e) STOP 9
  if (abs (rw - vresult) .ge. e) STOP 10
  if (abs (rv - vresult) .ge. e) STOP 11
  if (abs (rc - vresult) .ge. e) STOP 12

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

  if (lrg .neqv. lvresult) STOP 17
  if (lrw .neqv. lvresult) STOP 18
  if (lrv .neqv. lvresult) STOP 19
  if (lrc .neqv. lvresult) STOP 20

  !
  ! '.or.' reductions
  !

  lrg = .false.
  lrw = .false.
  lrv = .false.
  lrc = .false.
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

  if (lrg .neqv. lvresult) STOP 21
  if (lrw .neqv. lvresult) STOP 22
  if (lrv .neqv. lvresult) STOP 23
  if (lrc .neqv. lvresult) STOP 24

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

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(lrc)
  !$acc loop reduction(.eqv.:lrc) gang worker vector
  do i = 1, n
     lrc = lrc .eqv. (array(i) .ge. 5)
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     lvresult = lvresult .eqv. (array(i) .ge. 5)
  end do

  if (lrg .neqv. lvresult) STOP 25
  if (lrw .neqv. lvresult) STOP 26
  if (lrv .neqv. lvresult) STOP 27
  if (lrc .neqv. lvresult) STOP 28

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

  if (lrg .neqv. lvresult) STOP 29
  if (lrw .neqv. lvresult) STOP 30
  if (lrv .neqv. lvresult) STOP 31
  if (lrc .neqv. lvresult) STOP 32
end program reduction_3
