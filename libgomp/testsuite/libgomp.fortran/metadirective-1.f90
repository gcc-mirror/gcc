! { dg-do run }

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

    !$omp target map (to: x, y) map(from: z)
      block
      !$omp metadirective &
		!$omp& when(device={arch("nvptx")}: teams loop) &
		!$omp& default(parallel loop)
	do i = 1, N
	  z(i) = x(i) * y(i)
	enddo
      end block
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
    !$omp end target
  end subroutine
end program
