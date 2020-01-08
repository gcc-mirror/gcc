! { dg-do run }
! Test that the cache directives work with optional arguments.  The effect
! of giving a non-present argument to the cache directive is not tested as
! it is undefined.  The test is based on gfortran.dg/goacc/cache-1.f95.

! { dg-additional-options "-std=f2008" }

program cache_test
  implicit none
  integer :: d(10), e(7,13)

  call do_test(d, e)
contains
  subroutine do_test(d, e)
    integer, optional :: d(10), e(7,13)
    integer :: i
    do concurrent (i=1:5)
      !$acc cache (d(1:3))
      !$acc cache (d(i:i+2))
      !$acc cache (e(1:3,2:4))
      !$acc cache (e(i:i+2,i+1:i+3))
    enddo
  end
end
