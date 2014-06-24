! { dg-do run }

module m
  interface operator(.add.)
    module procedure do_add
  end interface
  type dt
    real :: r = 0.0
  end type
contains
  elemental function do_add(x, y)
    type (dt), intent (in) :: x, y
    type (dt) :: do_add
    do_add%r = x%r + y%r
  end function
  elemental subroutine dp_add(x, y)
    double precision, intent (inout) :: x
    double precision, intent (in) :: y
    x = x + y
  end subroutine
  elemental subroutine dp_init(x)
    double precision, intent (out) :: x
    x = 0.0
  end subroutine
end module

program udr6
  use m, only : operator(.add.), dt, dp_add, dp_init
  type(dt), allocatable :: xdt(:)
  type(dt) :: one
  real :: r
  integer (kind = 4), allocatable, dimension(:) :: i4
  integer (kind = 8), allocatable, dimension(:,:) :: i8
  integer :: i
  real (kind = 4), allocatable :: r4(:,:)
  double precision, allocatable :: dp(:)
!$omp declare reduction(.add.:dt:omp_out=omp_out.add.omp_in)
!$omp declare reduction(foo:integer(4),integer(kind=8),real (kind = 4) &
!$omp & :omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction(foo:double precision:dp_add (omp_out, omp_in)) &
!$omp & initializer (dp_init (omp_priv))

  one%r = 1.0
  allocate (xdt(4), i4 (3), i8(-5:-2,2:3), r4(2:5,1:1), dp(7))
  r = 0.0
  i4 = 0
  i8 = 0
  r4 = 0.0
  do i = 1, 7
    call dp_init (dp(i))
  end do
!$omp parallel reduction(.add.: xdt) reduction(+: r) &
!$omp & reduction(foo: i4, i8, r4, dp) private(i)
  do i = 1, 4
    xdt(i) = xdt(i).add.one
  end do
  r = r + 1.0
  i4 = i4 + 1
  i8 = i8 + 1
  r4 = r4 + 1.0
  do i = 1, 7
    call dp_add (dp(i), 1.0d0)
  end do
!$omp end parallel
  if (any (xdt%r .ne. r)) call abort
  if (any (i4.ne.r).or.any(i8.ne.r)) call abort
  if (any(r4.ne.r).or.any(dp.ne.r)) call abort
  deallocate (xdt, i4, i8, r4, dp)
end program udr6
