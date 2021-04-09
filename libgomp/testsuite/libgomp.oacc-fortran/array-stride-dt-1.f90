! { dg-do run }

type t
   integer, allocatable :: A(:,:)
end type t

type(t), allocatable :: b(:)

integer :: i

allocate(b(1:20))
do i=1,20
  allocate(b(i)%A(1:20,1:20))
end do

do i=1,20
  b(i)%A(:,:) = 0
end do

!$acc enter data copyin(b)
do i=1,20
  !$acc enter data copyin(b(i)%A)
end do

b(1)%A(:,:) = 5

!$acc update device(b(::2))
!$acc update device(b(1)%A(::3,::4))

do i=1,20
  !$acc exit data copyout(b(i)%A)
end do
!$acc exit data copyout(b)

! This is necessarily conservative because the "update" is allowed to copy
! e.g. the whole of the containing block for a discontinuous update.
! Try to ensure that the update covers a sufficient portion of the array.

if (any(b(1)%A(::3,::4) .ne. 5)) stop 1
do i=2,20
  if (any(b(i)%A(:,:) .ne. 0)) stop 2
end do

end
