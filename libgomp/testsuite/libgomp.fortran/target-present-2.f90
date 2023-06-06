! { dg-do run { target offload_device } }
! { dg-shouldfail "present error triggered" }

program main
  implicit none
  integer, parameter :: N = 100
  integer :: a(N), b(N), c(N), i

  do i = 1, N
    a(i) = i * 2
    b(i) = i * 3 + 1
  end do

  !$omp target enter data map (alloc: a)
    ! a has already been allocated, so this should be okay.
    !$omp target defaultmap (present)
      do i = 1, N
        c(i) = a(i)
      end do
    !$omp end target

    ! b has not been allocated, so this should result in an error.
    ! { dg-output "libgomp: present clause: not present on the device \\\(0x\[0-9a-f\]+, \[0-9\]+\\\)" }
    !$omp target defaultmap (present)
      do i = 1, N
        c(i) = c(i) + b(i)
      end do
    !$omp end target
!$omp target exit data map (from: c)
end program
