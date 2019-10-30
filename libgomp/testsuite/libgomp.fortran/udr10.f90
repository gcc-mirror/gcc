! { dg-do run }

module udr10m
  type dt
    integer :: x = 0
  end type
!$omp declare reduction(.add.:dt:omp_out=omp_out.add.omp_in)
!$omp declare reduction(+:dt:omp_out=omp_out+omp_in)
  interface operator(+)
    module procedure addme
  end interface
  interface operator(.add.)
    module procedure addme
  end interface
contains
  type(dt) function addme (x, y)
    type (dt), intent (in) :: x, y
    addme%x = x%x + y%x
  end function addme
end module udr10m
program udr10
  use udr10m, only : operator(.localadd.) => operator(.add.), &
& operator(+), dl => dt
  type(dl) :: j, k
  integer :: i
!$omp parallel do reduction(+:j) reduction(.localadd.:k)
  do i = 1, 100
    j = j .localadd. dl(i)
    k = k + dl(i * 2)
  end do
  if (j%x /= 5050 .or. k%x /= 10100) stop 1
end
