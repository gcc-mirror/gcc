! { dg-do run }
!
implicit none (type, external)
integer, allocatable :: a_ii, a_ival, a_iarr(:)
integer, pointer :: p_ii, p_ival, p_iarr(:)

nullify (p_ii, p_ival, p_iarr)

call sub()
call sub2()
call call_present_1()
call call_present_2()

! unallocated/disassociated actual arguments to nonallocatable, nonpointer
! dummy arguments are regarded as absent
! Skipping 'ival' dummy argument due to PR fortran/92887
call sub(ii=a_ii, iarr=a_iarr)
call sub(ii=p_ii, iarr=p_iarr)
call sub2(ii=a_ii, iarr=a_iarr)
call sub2(ii=p_ii, iarr=p_iarr)

contains

subroutine call_present_1()
  integer :: ii, ival, iarr, iptr, iparr
  pointer :: iptr, iparr
  dimension :: iarr(2), iparr(:)
  allocate(iptr,iparr(2))
  ii = 101
  ival = 102
  iptr = 103
  iarr = 104
  iparr = 105
  call sub_present(ii, ival, iarr, iptr, iparr)
  deallocate(iptr,iparr)
end subroutine

subroutine call_present_2()
  integer :: ii, ival, iarr, iptr, iparr
  pointer :: iptr, iparr
  dimension :: iarr(2), iparr(:)
  allocate(iptr,iparr(2))
  ii = 201
  ival = 202
  iptr = 203
  iarr = 204
  iparr = 205
  call sub2_present(ii, ival, iarr, iptr, iparr)
  deallocate(iptr,iparr)
end subroutine

subroutine sub(ii, ival, iarr, iptr, iparr)
  integer, optional :: ii, ival, iarr, iptr, iparr
  pointer :: iptr, iparr
  dimension :: iarr(:), iparr(:)
  value :: ival
  integer :: err
  err = 42
  !$omp target map(ii, ival, iarr, iptr, iparr, err)
  if (present(ii)) then
    ii = iptr + ival
    iarr = iparr
  else
    err = 0
  end if
  if (present(ii)) err = 1
  if (present(ival)) err = 2
  if (present(iarr)) err = 3
  if (present(iptr)) err = 4
  if (present(iparr)) err = 5
  !$omp end target
  if (err /= 0) stop 1
end subroutine sub

subroutine sub2(ii, ival, iarr, iptr, iparr)
  integer, optional :: ii, ival, iarr, iptr, iparr
  pointer :: iptr, iparr
  dimension :: iarr(:), iparr(:)
  value :: ival
  integer :: err(1) ! otherwise, implied defaultmap is firstprivate
  err(1) = 42
  !$omp target  ! automatic mapping with implied defaultmap(tofrom) 
  if (present(ii)) then
    ii = iptr + ival
    iarr = iparr
  else
    err(1) = 0
  end if
  if (present(ii)) err(1) = 1
  if (present(ival)) err(1) = 2
  if (present(iarr)) err(1) = 3
  if (present(iptr)) err(1) = 4
  if (present(iparr)) err(1) = 5
  !$omp end target
  if (err(1) /= 0) stop 2
end subroutine sub2

subroutine sub_present(ii, ival, iarr, iptr, iparr)
  integer, optional :: ii, ival, iarr, iptr, iparr
  pointer :: iptr, iparr
  dimension :: iarr(:), iparr(:)
  value :: ival
  integer :: err
  err = 42
  !$omp target map(ii, ival, iarr, iptr, iparr, err)
  if (.not.present(ii)) err = 1
  if (.not.present(ival)) err = 2
  if (.not.present(iarr)) err = 3
  if (.not.present(iptr)) err = 4
  if (.not.present(iparr)) err = 5
  err = err - 42 - 101-102-103-104-105 + ii+ival+iarr(2)+iptr+iparr(2)
  !$omp end target
  if (err /= 0) stop 3
end subroutine sub_present

subroutine sub2_present(ii, ival, iarr, iptr, iparr)
  integer, optional :: ii, ival, iarr, iptr, iparr
  pointer :: iptr, iparr
  dimension :: iarr(:), iparr(:)
  value :: ival
  integer :: err(1) ! otherwise, implied defaultmap is firstprivate
  err(1) = 53
  !$omp target  ! automatic mapping with implied defaultmap(tofrom) 
  ! Note: OpenMP 4.5's 'defaultmap' is not yet supported, PR 92568
  if (.not.present(ii)) err = 1
  if (.not.present(ival)) err = 2
  if (.not.present(iarr)) err = 3
  if (.not.present(iptr)) err = 4
  if (.not.present(iparr)) err = 5
  err = err - 53 - 201-202-203-204-205 + ii+ival+iarr(2)+iptr+iparr(2)
  !$omp end target
  if (err(1) /= 0) stop 4
end subroutine sub2_present
end
