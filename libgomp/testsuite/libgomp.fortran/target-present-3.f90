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

  !$omp target map(present, alloc: a, c)
    do i = 1, N
      if (a(i) /= i * 2) stop 1
      c(i) = 23 * i
    end do
  !$omp end target

  !$omp target update from (present: c)
    do i = 1, N
      if (c(i) /= 23 * i) stop 1
    end do

    print *, "CheCKpOInT"
    ! { dg-output "CheCKpOInT(\n|\r\n|\r).*" }

    ! This should fail as b has not been allocated.
    ! { dg-output "libgomp: present clause: not present on the device \\(addr: 0x\[0-9a-f\]+, size: \[0-9\]+ \\(0x\[0-9a-f\]+\\), dev: \[0-9\]+\\\)" { target offload_device_nonshared_as } }
    ! { dg-shouldfail "present error triggered" { offload_device_nonshared_as } }
    !$omp target update to (present: b)
  !$omp target exit data map (from: c)
end program
