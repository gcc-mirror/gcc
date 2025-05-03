program myprog
  type t
    integer :: size
    integer :: arr(99)
  end type t

  type u
    type(t) :: myt
  end type u

  integer :: i, j
  integer, parameter :: N = 10
  type(u) :: x(N)

  !$omp declare mapper (t :: x) map(tofrom: x%size, x%arr(1:x%size))
  !$omp declare mapper (u :: x) map(tofrom: x%myt)

  do i = 1, N
    x(i)%myt%size = 99
    do j = 1, 99
      x(i)%myt%arr(j) = i*j
    end do
  end do

  !$omp target map(iterator(i=1:N), tofrom: x(i))
    do i = 1, N
      do j = 1, 99
        x(i)%myt%arr(j) = x(i)%myt%arr(j) + 1
      end do
    end do
  !$omp end target

  do i = 1, N
    do j = 1, 99
      if (x(i)%myt%arr(j) /= i*j + 1) stop 1
    end do
  end do
end program myprog
