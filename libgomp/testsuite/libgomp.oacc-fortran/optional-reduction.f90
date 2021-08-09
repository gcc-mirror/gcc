! Test optional arguments in reduction clauses.  The effect of
! non-present arguments in reduction clauses is undefined, and is not tested
! for.  The tests are based on those in reduction-1.f90.

! { dg-do run }

!TODO
! { dg-xfail-run-if TODO { openacc_radeon_accel_selected && { ! __OPTIMIZE__ } } }

program optional_reduction
  implicit none

  integer :: rg, rw, rv, rc

  rg = 0
  rw = 0
  rv = 0
  rc = 0

  call do_test(rg, rw, rv, rc)
contains
  subroutine do_test(rg, rw, rv, rc)
    integer, parameter     :: n = 10, ng = 8, nw = 4, vl = 32
    integer, optional      :: rg, rw, rv, rc
    integer                :: i, vresult
    integer, dimension (n) :: array

    vresult = 0
    do i = 1, n
       array(i) = i
    end do

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
  end subroutine do_test
end program optional_reduction
