program main
  implicit none
  integer, parameter :: N = 100
  integer :: a(N), b(N), c(N), i

  do i = 1, N
    a(i) = i * 2
    b(i) = i * 3 + 1
  end do

  !$omp target enter data map (alloc: a, c)
    ! This should work as a has already been allocated.
    !$omp target update to (present: a)

    print *, "CheCKpOInT"
    ! { dg-output "CheCKpOInT(\n|\r\n|\r).*" }

    ! This should fail as b has not been allocated.
    ! { dg-output "libgomp: present clause: not present on the device \\\(0x\[0-9a-f\]+, \[0-9\]+\\\)" { target offload_device_nonshared_as } }
    ! { dg-shouldfail "present error triggered" { offload_device_nonshared_as } }
    !$omp target update to (present: b)
  !$omp target exit data map (from: c)
end program
