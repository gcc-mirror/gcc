! { dg-additional-sources my-usleep.c }
! { dg-prune-output "command-line option '-fintrinsic-modules-path=.*' is valid for Fortran but not for C" }
program main
  implicit none
  interface
    subroutine usleep(t) bind(C, name="my_usleep")
      use iso_c_binding
      integer(c_int), value :: t
    end subroutine
  end interface

  integer :: a(128)
  integer :: i

  !$omp teams num_teams(5)
    !$omp loop bind(teams)
    do i = 1, 128
      a(i) = i
      if (i == 1) then
        call usleep (20)
      else if (i == 17) then
        call usleep (40)
      end if
    end do
    !$omp loop bind(teams)
    do i = 1, 128
      a(i) = a(i) + i
    end do
  !$omp end teams
  do i = 1, 128
    if (a(i) /= 2 * i) &
      stop 1
  end do
  !$omp teams num_teams(5)
    !$omp loop bind(teams) order(concurrent)
    do i = 1, 128
      a(i) = a(i) * 2
      if (i == 1) then
        call usleep (20)
      else if (i == 13) then
        call usleep (40)
      end if
    end do
    !$omp loop bind(teams) order(concurrent)
    do i = 1, 128
      a(i) = a(i) + i
    end do
  !$omp end teams
  do i = 1, 128
    if (a(i) /= 5 * i) &
      stop 2
  end do
  !$omp teams num_teams(5)
    !$omp loop bind(teams) order(reproducible:concurrent)
    do i = 1, 128
      a(i) = a(i) * 2
      if (i == 3) then
        call usleep (20)
      else if (i == 106) then
        call usleep (40)
      end if
    end do
    !$omp loop bind(teams) order(reproducible:concurrent)
    do i = 1, 128
      a(i) = a(i) + i
    end do
  !$omp end teams
  do i = 1, 128
    if (a(i) /= 11 * i) &
      stop 3
  end do
end program main
