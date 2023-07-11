! { dg-do run }

program p
implicit none
integer :: myarr(10)

myarr = 0

call subr(myarr)

if (myarr(5).ne.5) stop 1

contains

subroutine subr(arr)
implicit none
integer :: arr(*)

! At first glance, it might not be obvious how this works.  The "enter data"
! and "exit data" operations expand to a pair of mapping nodes for OpenACC,
! GOMP_MAP_{TO/FROM} and GOMP_MAP_POINTER.  The former maps the array data,
! and the latter creates a separate mapping on the target for the pointer
! itself with a bias so it represents the "zeroth" element.

!$acc enter data copyin(arr(2:8))

! ...then this implicit mapping creates a zero-length array section
! (GOMP_MAP_ZERO_LEN_ARRAY_SECTION) followed by another GOMP_MAP_POINTER for
! 'arr'.  But now that pointer is already "present" on the target, so is not
! overwritten.

!$acc serial
! { dg-warning {using .vector_length \(32\)., ignoring 1} "" { target openacc_nvidia_accel_selected } .-1 }
! This access is then done via the on-target pointer.
arr(5) = 5
!$acc end serial

!$acc exit data copyout(arr(2:8))

end subroutine subr
end program p
