! { dg-do run { target { ! offload_target_nvptx } } }
! { dg-do compile { target offload_target_nvptx } }

program test
  implicit none

  integer, parameter :: N = 100
  integer :: x(N), y(N), z(N)
  integer :: i

  do i = 1, N
    x(i) = i;
    y(i) = -i;
  end do

  call f (x, y, z)

  do i = 1, N
    if (z(i) .ne. x(i) * y(i)) stop 1
  end do

  ! -----
  do i = 1, N
    x(i) = i;
    y(i) = -i;
  end do

  call g (x, y, z)

  do i = 1, N
    if (z(i) .ne. x(i) * y(i)) stop 1
  end do

contains
  subroutine f (x, y, z)
    integer :: x(N), y(N), z(N)
    ! The following fails as on the host the target side cannot be
    ! resolved - and the 'teams' or not status affects how 'target'
    ! is called. -> See PR118694, esp. comment 9.
    ! Note also the dg-do compile above for offload_target_nvptx

    !$omp target map (to: x, y) map(from: z)
      block
      !$omp metadirective &
		!$omp& when(device={arch("nvptx")}: teams loop) &
		!$omp& default(parallel loop)
	do i = 1, N
	  z(i) = x(i) * y(i)
	enddo
      end block
    ! { dg-bogus "'target' construct with nested 'teams' construct contains directives outside of the 'teams' construct" "PR118694" { xfail offload_target_nvptx } .-9 }  */
  end subroutine
  subroutine g (x, y, z)
    integer :: x(N), y(N), z(N)

    !$omp target map (to: x, y) map(from: z)
    block
      !$omp metadirective &
		!$omp& when(device={arch("nvptx")}: teams loop) &
		!$omp& default(parallel loop)
	do i = 1, N
	  z(i) = x(i) * y(i)
	enddo
    end block
    ! { dg-bogus "'target' construct with nested 'teams' construct contains directives outside of the 'teams' construct" "PR118694" { xfail offload_target_nvptx } .-9 }  */
    !$omp end target
  end subroutine
end program
