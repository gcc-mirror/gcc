! { dg-do compile }

program test
  implicit none

  integer, parameter :: N = 100
  integer :: x(N), y(N), z(N)
  integer :: i

contains
  subroutine f (x, y, z)
    integer :: x(N), y(N), z(N)

    !$omp target map (to: x, y) map(from: z)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
      block
      !$omp metadirective &
		!$omp& when(device={arch("nvptx")}: teams loop) &
		!$omp& default(parallel loop)  ! { dg-error "\\(1\\)" }
 ! FIXME: The line above should be the same error as above but some fails here with -fno-diagnostics-show-caret
 ! Seems as if some gcc/testsuite/ fix is missing for libgomp/testsuite
	do i = 1, N
	  z(i) = x(i) * y(i)
	enddo
       z(N) = z(N) + 1  ! <<< invalid
      end block
  end subroutine

  subroutine f2 (x, y, z)
    integer :: x(N), y(N), z(N)

    !$omp target map (to: x, y) map(from: z)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
      block
      integer :: i ! << invalid
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

    !$omp target map (to: x, y) map(from: z)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    block
      !$omp metadirective &   ! <<<< invalid
		!$omp& when(device={arch("nvptx")}: flush) &
		!$omp& default(nothing)
       !$omp teams loop
	do i = 1, N
	  z(i) = x(i) * y(i)
	enddo
    end block
    !$omp end target
  end subroutine

end program
