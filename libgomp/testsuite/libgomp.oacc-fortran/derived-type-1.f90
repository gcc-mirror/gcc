! Test derived types with subarrays

! { dg-do run }

  implicit none
  type dtype
     integer :: a, b, c
  end type dtype
  integer, parameter :: n = 100
  integer i
  type (dtype), dimension(n) :: d

  !$acc data copy(d(1:n))
  !$acc parallel loop
  do i = 1, n
     d(i)%a = i
     d(i)%b = i-1
     d(i)%c = i+1
  end do
  !$acc end data

  do i = 1, n
     if (d(i)%a /= i) stop 1
     if (d(i)%b /= i-1) stop 2
     if (d(i)%c /= i+1) stop 3
  end do
end program

