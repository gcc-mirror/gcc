! { dg-do run }

function dotprod_ref (B, C, N) result (sum)
  implicit none
  real :: B(N), C(N), sum
  integer :: N, i
  sum = 0.0e0
  do i = 1, N
    sum = sum + B(i) * C(i)
  end do
end function

function dotprod (B, C, N, block_size, num_teams, block_threads) result (sum)
  implicit none
  real :: B(N), C(N), sum
  integer :: N, block_size, num_teams, block_threads, i, i0
  sum = 0.0e0
  !$omp target map(to: B, C, block_size, num_teams, block_threads) &
  !$omp& map(tofrom: sum)
    !$omp teams num_teams(num_teams) thread_limit(block_threads) &
    !$omp& reduction(+:sum)
      !$omp distribute
      do i0 = 1, N, block_size
        !$omp parallel do reduction(+:sum)
        do i = i0, min (i0 + block_size - 1, N)
          sum = sum + B(i) * C(i)
        end do
      end do
    !$omp end teams
  !$omp end target
end function

subroutine init (B, C, N)
  real :: B(N), C(N)
  integer :: N, i
  do i = 1, N
    B(i) = 0.0001 * i
    C(i) = 0.000001 * i * i
  end do
end subroutine

subroutine check (a, b)
  real :: a, b, err
  real, parameter :: EPS = 0.0001
  if (b == 0.0) then
    err = a
  else if (a == 0.0) then
    err = b
  else
    err = (a - b) / b
  end if
  if (err > EPS .or. err < -EPS) STOP 1
end subroutine

program e_54_1
  integer :: n
  real :: ref, d
  real, pointer, dimension(:) :: B, C
  n = 1024 * 1024
  allocate (B(n), C(n))
  call init (B, C, n)
  ref = dotprod_ref (B, C, n)
  d = dotprod (B, C, n, n / 8, 2, 8)
  call check (ref, d)
  deallocate (B, C)
end program
