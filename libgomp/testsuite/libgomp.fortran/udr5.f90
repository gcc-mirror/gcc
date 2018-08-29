! { dg-do run }

module m
  interface operator(.add.)
    module procedure do_add
  end interface
  type dt
    real :: r = 0.0
  end type
contains
  function do_add(x, y)
    type (dt), intent (in) :: x, y
    type (dt) :: do_add
    do_add%r = x%r + y%r
  end function
  subroutine dp_add(x, y)
    double precision :: x, y
    x = x + y
  end subroutine
  subroutine dp_init(x)
    double precision :: x
    x = 0.0
  end subroutine
end module

program udr5
  use m, only : operator(.add.), dt, dp_add, dp_init
  type(dt) :: xdt, one
  real :: r
  integer (kind = 4) :: i4
  integer (kind = 8) :: i8
  real (kind = 4) :: r4
  double precision :: dp
!$omp declare reduction(.add.:dt:omp_out=omp_out.add.omp_in)
!$omp declare reduction(foo:integer(4),integer(kind=8),real (kind = 4) &
!$omp & :omp_out = omp_out + omp_in) initializer (omp_priv = 0)
!$omp declare reduction(foo:double precision:dp_add (omp_out, omp_in)) &
!$omp & initializer (dp_init (omp_priv))

  one%r = 1.0
  r = 0.0
  i4 = 0
  i8 = 0
  r4 = 0.0
  call dp_init (dp)
!$omp parallel reduction(.add.: xdt) reduction(+: r) &
!$omp & reduction(foo: i4, i8, r4, dp)
  xdt = xdt.add.one
  r = r + 1.0
  i4 = i4 + 1
  i8 = i8 + 1
  r4 = r4 + 1.0
  call dp_add (dp, 1.0d0)
!$omp end parallel
  if (xdt%r .ne. r) STOP 1
  if (i4.ne.r.or.i8.ne.r.or.r4.ne.r.or.dp.ne.r) STOP 2
end program udr5
