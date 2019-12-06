! Test that optional arguments work in private clauses.  The effect of
! non-present arguments in private clauses is undefined, and is not tested
! for.  The tests are based on those in private-variables.f90.

! { dg-do run }

program main
  implicit none

  type vec3
     integer x, y, z, attr(13)
  end type vec3
  integer :: x
  type(vec3) :: pt
  integer :: arr(2)

  call t1(x)
  call t2(pt)
  call t3(arr)
contains

  ! Test of gang-private variables declared on loop directive.

  subroutine t1(x)
    integer, optional :: x
    integer :: i, arr(32)

    do i = 1, 32
       arr(i) = i
    end do

    !$acc parallel copy(arr) num_gangs(32) num_workers(8) vector_length(32)
    !$acc loop gang private(x)
    do i = 1, 32
       x = i * 2;
       arr(i) = arr(i) + x
    end do
    !$acc end parallel

    do i = 1, 32
       if (arr(i) .ne. i * 3) STOP 1
    end do
  end subroutine t1


  ! Test of gang-private addressable variable declared on loop directive, with
  ! broadcasting to partitioned workers.

  subroutine t2(pt)
    integer i, j, arr(0:32*32)
    type(vec3), optional :: pt

    do i = 0, 32*32-1
       arr(i) = i
    end do

    !$acc parallel copy(arr) num_gangs(32) num_workers(8) vector_length(32)
    !$acc loop gang private(pt)
    do i = 0, 31
       pt%x = i
       pt%y = i * 2
       pt%z = i * 4
       pt%attr(5) = i * 6

       !$acc loop vector
       do j = 0, 31
          arr(i * 32 + j) = arr(i * 32 + j) + pt%x + pt%y + pt%z + pt%attr(5);
       end do
    end do
    !$acc end parallel

    do i = 0, 32 * 32 - 1
       if (arr(i) .ne. i + (i / 32) * 13) STOP 2
    end do
  end subroutine t2

  ! Test of vector-private variables declared on loop directive. Array type.

  subroutine t3(pt)
    integer, optional :: pt(2)
    integer :: i, j, k, idx, arr(0:32*32*32)

    do i = 0, 32*32*32-1
       arr(i) = i
    end do

    !$acc parallel copy(arr) num_gangs(32) num_workers(8) vector_length(32)
    !$acc loop gang
    do i = 0, 31
       !$acc loop worker
       do j = 0, 31
          !$acc loop vector private(pt)
          do k = 0, 31
             pt(1) = ieor(i, j * 3)
             pt(2) = ior(i, j * 5)
             arr(i * 1024 + j * 32 + k) = arr(i * 1024 + j * 32 + k) + pt(1) * k
             arr(i * 1024 + j * 32 + k) = arr(i * 1024 + j * 32 + k) + pt(2) * k
          end do
       end do
    end do
    !$acc end parallel

    do i = 0, 32 - 1
       do j = 0, 32 -1
          do k = 0, 32 - 1
             idx = i * 1024 + j * 32 + k
             if (arr(idx) .ne. idx + ieor(i, j * 3) * k + ior(i, j * 5) * k) then
                STOP 3
             end if
          end do
       end do
    end do
  end subroutine t3

end program main
