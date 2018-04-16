! { dg-do run }

program main
  implicit none
  integer, parameter         :: n = 1024
  integer, dimension (0:n-1) :: a, b, c
  integer                    :: i, ii

  ! Parallelism dimensions: compiler/runtime decides.
  !$acc kernels copyout (a(0:n-1))
  do i = 0, n - 1
     a(i) = i * 2
  end do
  !$acc end kernels

  ! Parallelism dimensions: variable.
  !$acc kernels copyout (b(0:n-1)) &
  !$acc num_gangs (3 + a(3)) num_workers (5 + a(5)) vector_length (7 + a(7))
  ! { dg-prune-output "using vector_length \\(32\\), ignoring runtime setting" }
  do i = 0, n -1
     b(i) = i * 4
  end do
  !$acc end kernels

  ! Parallelism dimensions: literal.
  !$acc kernels copyin (a(0:n-1), b(0:n-1)) copyout (c(0:n-1)) &
  !$acc num_gangs (3) num_workers (5) vector_length (7)
  ! { dg-prune-output "using vector_length \\(32\\), ignoring 7" }
  do ii = 0, n - 1
     c(ii) = a(ii) + b(ii)
  end do
  !$acc end kernels

  do i = 0, n - 1
     if (a(i) .ne. i * 2) STOP 1
     if (b(i) .ne. i * 4) STOP 2
     if (c(i) .ne. a(i) + b(i)) STOP 3
  end do

end program main
