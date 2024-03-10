implicit none
integer, parameter :: N = 30
integer, parameter :: M = 3

integer :: a(M,N), b(M,N), c(M,N)
integer :: x, y, shift
integer :: i, j

do i = 1, N
  a(1,i) = i*32
  a(2,i) = i*17
  a(3,i) = i*11
  b(1,i) = i*7
  b(2,i) = i*5
  b(3,i) = i*3
end do

x = 0
!$omp parallel do simd collapse(2) reduction(inscan,+: x) private(shift)
do i = 1, N
  do j = 1, M
    x = x + a(j,i)
    x = x + b(j,i)
    !$omp scan inclusive(x)
    shift = i + 29*j
    c(j,i) = x + shift;
  end do
end do

y = 0
do i = 1, N
  do j = 1, M
    y = y +  a(j,i) + b(j,i)
    if (c(j,i) /= y + i + 29*j) error stop 1
  end do
end do
if (x /= y) error stop 2

x = 0
!$omp parallel do simd collapse(2) reduction(inscan,+: x) private(shift)
do i = 1, N
  do j = 1, M
    shift = i + 29*j
    c(j,i) = x + shift;
    !$omp scan exclusive(x)
    x = x + a(j,i)
    x = x + b(j,i)
  end do
end do

y = 0
do i = 1, N
  do j = 1, M
    if (c(j,i) /= y + i + 29*j) error stop 1
    y = y + a(j,i) + b(j,i)
  end do
end do
if (x /= y) error stop 2
end
