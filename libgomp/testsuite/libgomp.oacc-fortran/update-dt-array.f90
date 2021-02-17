! { dg-do run }

program myprog

  type mytype
    integer, allocatable :: myarr(:,:)
  end type mytype
  integer :: i

  type(mytype), allocatable :: typearr(:)

  allocate(typearr(1:100))

  do i=1,100
    allocate(typearr(i)%myarr(1:100,1:100))
  end do

  do i=1,100
    typearr(i)%myarr(:,:) = 0
  end do

  !$acc enter data copyin(typearr)

  do i=1,100
    !$acc enter data copyin(typearr(i)%myarr)
  end do

  i=33
  typearr(i)%myarr(:,:) = 50

  !$acc update device(typearr(i)%myarr(:,:))

  do i=1,100
    !$acc exit data copyout(typearr(i)%myarr)
  end do

  !$acc exit data delete(typearr)

  do i=1,100
    if (i.eq.33) then
      if (any(typearr(i)%myarr.ne.50)) stop 1
    else
      if (any(typearr(i)%myarr.ne.0)) stop 2
    end if
  end do

  do i=1,100
    deallocate(typearr(i)%myarr)
  end do

  deallocate(typearr)

end program myprog
