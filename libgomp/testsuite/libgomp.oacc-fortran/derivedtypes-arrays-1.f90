! { dg-do run }

type type1
  integer, allocatable :: arr1(:,:)
end type type1

type type2
  type(type1) :: t1
end type type2

type type3
  type(type2) :: t2(20)
end type type3

type type4
  type(type3), allocatable :: t3(:)
end type type4

integer :: i, j, k

type(type4), allocatable :: var1(:)
type(type4) :: var2
type(type3) :: var3

allocate(var1(1:20))
do i=1,20
  allocate(var1(i)%t3(1:20))
  do j=1,20
    do k=1,20
      allocate(var1(i)%t3(j)%t2(k)%t1%arr1(1:20,1:20))
    end do
  end do
end do

allocate(var2%t3(1:20))
do i=1,20
  do j=1,20
    allocate(var2%t3(i)%t2(j)%t1%arr1(1:20,1:20))
  end do
end do

do i=1,20
  do j=1,20
    do k=1,20
      var1(i)%t3(j)%t2(k)%t1%arr1(:,:) = 0
    end do
    var2%t3(i)%t2(j)%t1%arr1(:,:) = 0
  end do
end do

!$acc enter data copyin(var2%t3(4)%t2(3)%t1%arr1(:,:))
!$acc enter data copyin(var1(5)%t3(4)%t2(3)%t1%arr1(:,:))

var2%t3(4)%t2(3)%t1%arr1(:,:) = 5
var1(5)%t3(4)%t2(3)%t1%arr1(:,:) = 4

!$acc update device(var2%t3(4)%t2(3)%t1%arr1)
!$acc update device(var1(5)%t3(4)%t2(3)%t1%arr1)

!$acc exit data copyout(var1(5)%t3(4)%t2(3)%t1%arr1(:,:))
!$acc exit data copyout(var2%t3(4)%t2(3)%t1%arr1(:,:))

do i=1,20
  do j=1,20
    do k=1,20
      if (i.eq.5 .and. j.eq.4 .and. k.eq.3) then
        if (any(var1(i)%t3(j)%t2(k)%t1%arr1 .ne. 4)) stop 1
      else
        if (any(var1(i)%t3(j)%t2(k)%t1%arr1 .ne. 0)) stop 2
      end if
    end do
    if (i.eq.4 .and. j.eq.3) then
      if (any(var2%t3(i)%t2(j)%t1%arr1 .ne. 5)) stop 3
    else
      if (any(var2%t3(i)%t2(j)%t1%arr1 .ne. 0)) stop 4
    end if
  end do
end do

do i=1,20
  allocate(var3%t2(i)%t1%arr1(1:20, 1:20))
  var3%t2(i)%t1%arr1(:,:) = 0
end do

!$acc enter data copyin(var3)
!$acc enter data copyin(var3%t2(:))
!$acc enter data copyin(var3%t2(5)%t1)
!$acc data copyin(var3%t2(5)%t1%arr1)

!$acc serial present(var3%t2(5)%t1%arr1)
! { dg-warning "using .vector_length \\(32\\)., ignoring 1" "" { target openacc_nvidia_accel_selected } .-1 }
var3%t2(5)%t1%arr1(:,:) = 6
!$acc end serial

!$acc update host(var3%t2(5)%t1%arr1)

!$acc end data
!$acc exit data delete(var3%t2(5)%t1)
!$acc exit data delete(var3%t2)
!$acc exit data delete(var3)

do i=1,20
  if (i.eq.5) then
    if (any(var3%t2(i)%t1%arr1.ne.6)) stop 5
  else
    if (any(var3%t2(i)%t1%arr1.ne.0)) stop 6
  end if
end do

end
