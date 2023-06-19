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

!$acc enter data copyin(arr(1:10))

!$acc serial
arr(5) = 5
!$acc end serial

!$acc exit data copyout(arr(1:10))

end subroutine subr
end program p
