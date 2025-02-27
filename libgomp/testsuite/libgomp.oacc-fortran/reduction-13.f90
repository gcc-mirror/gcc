! { dg-do run }

! complex array reductions

program main
  implicit none

  integer, parameter     :: n = 10, ng = 8, nw = 4, vl = 32
  integer                :: i, j
  complex, dimension (n) :: vresult, rg, rw, rv, rc
  logical, dimension (n) :: lrg, lrw, lrv, lrc, lvresult
  complex, dimension (n) :: array

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

end program main
