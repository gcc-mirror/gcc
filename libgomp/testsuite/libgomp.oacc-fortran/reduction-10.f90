! { dg-do run }

! integer array reductions

program main
  implicit none

  integer, parameter     :: n = 10, ng = 8, nw = 4, vl = 32
  integer                :: i, j
  integer, dimension (n) :: vresult, rg, rw, rv, rc
  logical, dimension (n) :: lrg, lrw, lrv, lrc, lvresult
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
    do j = 1, n
      rg(j) = rg(j) + array(i)
    end do
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(+:rw) worker
  do i = 1, n
    do j = 1, n
      rw(j) = rw(j) + array(i)
    end do
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(+:rv) vector
  do i = 1, n
    do j = 1, n
      rv(j) = rv(j) + array(i)
    end do
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(+:rc) gang worker vector
  do i = 1, n
    do j = 1, n
      rc(j) = rc(j) + array(i)
    end do
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
    do j = 1, n
      vresult(j) = vresult(j) + array(i)
    end do
  end do

  if (count (rg .ne. vresult) .ne. 0) STOP 1
  if (count (rw .ne. vresult) .ne. 0) STOP 2
  if (count (rv .ne. vresult) .ne. 0) STOP 3
  if (count (rc .ne. vresult) .ne. 0) STOP 4

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
    do j = 1, n
      rg(j) = rg(j) * array(i)
    end do
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(*:rw) worker
  do i = 1, n
    do j = 1, n
      rw(j) = rw(j) * array(i)
    end do
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(*:rv) vector
  do i = 1, n
    do j = 1, n
      rv(j) = rv(j) * array(i)
    end do
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(*:rc) gang worker vector
  do i = 1, n
    do j = 1, n
      rc(j) = rc(j) * array(i)
    end do
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
    do j = 1, n
      vresult(j) = vresult(j) * array(i)
    end do
  end do

  if (count (rg .ne. vresult) .ne. 0) STOP 5
  if (count (rw .ne. vresult) .ne. 0) STOP 6
  if (count (rv .ne. vresult) .ne. 0) STOP 7
  if (count (rc .ne. vresult) .ne. 0) STOP 8

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
    do j = 1, n
      rg(j) = max (rg(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(max:rw) worker
  do i = 1, n
    do j = 1, n
      rw(j) = max (rw(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(max:rv) vector
  do i = 1, n
    do j = 1, n
      rv(j) = max (rv(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(max:rc) gang worker vector
  do i = 1, n
    do j = 1, n
      rc(j) = max (rc(j), array(i))
    end do
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
    do j = 1, n
      vresult(j) = max (vresult(j), array(i))
    end do
  end do

  if (count (rg .ne. vresult) .ne. 0) STOP 9
  if (count (rw .ne. vresult) .ne. 0) STOP 10
  if (count (rv .ne. vresult) .ne. 0) STOP 11
  if (count (rc .ne. vresult) .ne. 0) STOP 12

  !
  ! 'min' reductions
  !

  rg = n + 1
  rw = n + 1
  rv = n + 1
  rc = n + 1
  vresult = n + 1

  !$acc parallel num_gangs(ng) copy(rg)
  !$acc loop reduction(min:rg) gang
  do i = 1, n
    do j = 1, n
      rg(j) = min (rg(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(min:rw) worker
  do i = 1, n
    do j = 1, n
      rw(j) = min (rw(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(min:rv) vector
  do i = 1, n
    do j = 1, n
      rv(j) = min (rv(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(min:rc) gang worker vector
  do i = 1, n
    do j = 1, n
      rc(j) = min (rc(j), array(i))
    end do
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
    do j = 1, n
      vresult(j) = min (vresult(j), array(i))
    end do
  end do

  if (count (rg .ne. vresult) .ne. 0) STOP 13
  if (count (rw .ne. vresult) .ne. 0) STOP 14
  if (count (rv .ne. vresult) .ne. 0) STOP 15
  if (count (rc .ne. vresult) .ne. 0) STOP 16

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
    do j = 1, n
      rg(j) = iand (rg(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(iand:rw) worker
  do i = 1, n
    do j = 1, n
      rw(j) = iand (rw(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(iand:rv) vector
  do i = 1, n
    do j = 1, n
      rv(j) = iand (rv(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(iand:rc) gang worker vector
  do i = 1, n
    do j = 1, n
      rc(j) = iand (rc(j), array(i))
    end do
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
    do j = 1, n
      vresult(j) = iand (vresult(j), array(i))
    end do
  end do

  if (count (rg .ne. vresult) .ne. 0) STOP 17
  if (count (rw .ne. vresult) .ne. 0) STOP 18
  if (count (rv .ne. vresult) .ne. 0) STOP 19
  if (count (rc .ne. vresult) .ne. 0) STOP 20

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
    do j = 1, n
      rg(j) = ior (rg(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(ior:rw) worker
  do i = 1, n
    do j = 1, n
      rw(j) = ior (rw(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(ior:rv) vector
  do i = 1, n
    do j = 1, n
      rv(j) = ior (rv(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(ior:rc) gang worker vector
  do i = 1, n
    do j = 1, n
      rc(j) = ior (rc(j), array(i))
    end do
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
    do j = 1, n
      vresult(j) = ior (vresult(j), array(i))
    end do
  end do

  if (count (rg .ne. vresult) .ne. 0) STOP 21
  if (count (rw .ne. vresult) .ne. 0) STOP 22
  if (count (rv .ne. vresult) .ne. 0) STOP 23
  if (count (rc .ne. vresult) .ne. 0) STOP 24

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
    do j = 1, n
      rg(j) = ieor (rg(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(ieor:rw) worker
  do i = 1, n
    do j = 1, n
      rw(j) = ieor (rw(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(ieor:rv) vector
  do i = 1, n
    do j = 1, n
      rv(j) = ieor (rv(j), array(i))
    end do
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(ieor:rc) gang worker vector
  do i = 1, n
    do j = 1, n
      rc(j) = ieor (rc(j), array(i))
    end do
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
    do j = 1, n
      vresult(j) = ieor (vresult(j), array(i))
    end do
  end do

  if (count (rg .ne. vresult) .ne. 0) STOP 25
  if (count (rw .ne. vresult) .ne. 0) STOP 26
  if (count (rv .ne. vresult) .ne. 0) STOP 27
  if (count (rc .ne. vresult) .ne. 0) STOP 28

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
    do j = 1, n
      lrg(j) = lrg(j) .and. (array(i) .ge. 5)
    end do
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(lrw)
  !$acc loop reduction(.and.:lrw) worker
  do i = 1, n
    do j = 1, n
      lrw(j) = lrw(j) .and. (array(i) .ge. 5)
    end do
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(lrv)
  !$acc loop reduction(.and.:lrv) vector
  do i = 1, n
    do j = 1, n
      lrv(j) = lrv(j) .and. (array(i) .ge. 5)
    end do
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(lrc)
  !$acc loop reduction(.and.:lrc) gang worker vector
  do i = 1, n
    do j = 1, n
      lrc(j) = lrc(j) .and. (array(i) .ge. 5)
    end do
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
    do j = 1, n
      lvresult(j) = lvresult(j) .and. (array(i) .ge. 5)
    end do
  end do

  if (count (lrg .neqv. lvresult) .ne. 0) STOP 29
  if (count (lrw .neqv. lvresult) .ne. 0) STOP 30
  if (count (lrv .neqv. lvresult) .ne. 0) STOP 31
  if (count (lrc .neqv. lvresult) .ne. 0) STOP 32

  !
  ! '.or.' reductions
  !

  lrg = .true.
  lrw = .true.
  lrv = .true.
  lrc = .true.
  lvresult = .true.

  !$acc parallel num_gangs(ng) copy(lrg)
  !$acc loop reduction(.or.:lrg) gang
  do i = 1, n
    do j = 1, n
      lrg(j) = lrg(j) .or. (array(i) .ge. 5)
    end do
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(lrw)
  !$acc loop reduction(.or.:lrw) worker
  do i = 1, n
    do j = 1, n
      lrw(j) = lrw(j) .or. (array(i) .ge. 5)
    end do
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(lrv)
  !$acc loop reduction(.or.:lrv) vector
  do i = 1, n
    do j = 1, n
      lrv(j) = lrv(j) .or. (array(i) .ge. 5)
    end do
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(lrc)
  !$acc loop reduction(.or.:lrc) gang worker vector
  do i = 1, n
    do j = 1, n
      lrc(j) = lrc(j) .or. (array(i) .ge. 5)
    end do
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
    do j = 1, n
      lvresult(j) = lvresult(j) .or. (array(i) .ge. 5)
    end do
  end do

  if (count (lrg .neqv. lvresult) .ne. 0) STOP 33
  if (count (lrw .neqv. lvresult) .ne. 0) STOP 34
  if (count (lrv .neqv. lvresult) .ne. 0) STOP 35
  if (count (lrc .neqv. lvresult) .ne. 0) STOP 36

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
    do j = 1, n
      lrg(j) = lrg(j) .eqv. (array(i) .ge. 5)
    end do
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(lrw)
  !$acc loop reduction(.eqv.:lrw) worker
  do i = 1, n
    do j = 1, n
      lrw(j) = lrw(j) .eqv. (array(i) .ge. 5)
    end do
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(lrv)
  !$acc loop reduction(.eqv.:lrv) vector
  do i = 1, n
    do j = 1, n
      lrv(j) = lrv(j) .eqv. (array(i) .ge. 5)
    end do
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(lrc)
  !$acc loop reduction(.eqv.:lrc) gang worker vector
  do i = 1, n
    do j = 1, n
      lrc(j) = lrc(j) .eqv. (array(i) .ge. 5)
    end do
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
    do j = 1, n
      lvresult(j) = lvresult(j) .eqv. (array(i) .ge. 5)
    end do
  end do

  if (count (lrg .neqv. lvresult) .ne. 0) STOP 37
  if (count (lrw .neqv. lvresult) .ne. 0) STOP 38
  if (count (lrv .neqv. lvresult) .ne. 0) STOP 39
  if (count (lrc .neqv. lvresult) .ne. 0) STOP 40

end program main
