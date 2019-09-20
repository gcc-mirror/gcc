! { dg-do run }

!TODO
! { dg-xfail-run-if TODO { openacc_radeon_accel_selected && { ! __OPTIMIZE__ } } }

program main
  implicit none
  integer :: myint
  integer :: i
  real :: res(65536), tmp

  res(:) = 0.0

  myint = 5
  call workers(myint, res)

  do i=1,65536
    tmp = i * 99
    if (res(i) .ne. tmp) stop 1
  end do

  res(:) = 0.0

  myint = 7
  call vectors(myint, res)

  do i=1,65536
    tmp = i * 101
    if (res(i) .ne. tmp) stop 2
  end do

contains

  subroutine workers(t1, res)
    implicit none
    integer :: t1
    integer :: i, j
    real, intent(out) :: res(:)

    !$acc parallel copyout(res) num_gangs(64) num_workers(64)
    ! { dg-warning "using num_workers \\(32\\), ignoring 64" "" { target openacc_nvidia_accel_selected } .-1 }

    !$acc loop gang
    do i=0,255
      !$acc loop worker private(t1)
      do j=1,256
        t1 = (i * 256 + j) * 99
        res(i * 256 + j) = t1
      end do
    end do

    !$acc end parallel
  end subroutine workers

  subroutine vectors(t1, res)
    implicit none
    integer :: t1
    integer :: i, j
    real, intent(out) :: res(:)

    !$acc parallel copyout(res) num_gangs(64) num_workers(64)
    ! { dg-warning "using num_workers \\(32\\), ignoring 64" "" { target openacc_nvidia_accel_selected } .-1 }

    !$acc loop gang worker
    do i=0,255
      !$acc loop vector private(t1)
      do j=1,256
        t1 = (i * 256 + j) * 101
        res(i * 256 + j) = t1
      end do
    end do

    !$acc end parallel
  end subroutine vectors

end program main
