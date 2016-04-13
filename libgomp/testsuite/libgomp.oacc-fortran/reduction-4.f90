! { dg-do run }

! complex reductions

program reduction_4
  implicit none

  integer, parameter    :: n = 10, ng = 8, nw = 4, vl = 32
  integer               :: i
  real                  :: vresult, rg, rw, rv, rc
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
     rg = rg + REAL(array(i))
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(+:rw) worker
  do i = 1, n
     rw = rw + REAL(array(i))
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(+:rv) vector
  do i = 1, n
     rv = rv + REAL(array(i))
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(+:rc) gang worker vector
  do i = 1, n
     rc = rc + REAL(array(i))
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     vresult = vresult + REAL(array(i))
  end do

  if (rg .ne. vresult) call abort
  if (rw .ne. vresult) call abort
  if (rv .ne. vresult) call abort
  if (rc .ne. vresult) call abort

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
     rg = rg * REAL(array(i))
  end do
  !$acc end parallel

  !$acc parallel num_workers(nw) copy(rw)
  !$acc loop reduction(*:rw) worker
  do i = 1, n
     rw = rw * REAL(array(i))
  end do
  !$acc end parallel

  !$acc parallel vector_length(vl) copy(rv)
  !$acc loop reduction(*:rv) vector
  do i = 1, n
     rv = rv * REAL(array(i))
  end do
  !$acc end parallel

  !$acc parallel num_gangs(ng) num_workers(nw) vector_length(vl) copy(rc)
  !$acc loop reduction(*:rc) gang worker vector
  do i = 1, n
     rc = rc * REAL(array(i))
  end do
  !$acc end parallel

  ! Verify the results
  do i = 1, n
     vresult = vresult * REAL(array(i))
  end do

  if (rg .ne. vresult) call abort
  if (rw .ne. vresult) call abort
  if (rv .ne. vresult) call abort
  if (rc .ne. vresult) call abort
end program reduction_4
