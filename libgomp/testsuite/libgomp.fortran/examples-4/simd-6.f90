! { dg-do run { target vect_simd_clones } }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

module SIMD6_mod
contains
  function foo(p) result(r)
  !$omp declare simd(foo) notinbranch
    integer :: p, r
    p = p + 10
    r = p
  end function foo

  function myaddint(a, b, n) result(r)
    implicit none
    integer :: a(*), b(*), n, r
    integer :: i

    !$omp simd
    do i=1, n
        a(i) = foo(b(i))  ! foo is not called under a condition
    end do
    r = a(n)

  end function myaddint

  function myaddint_ref(a, b, n) result(r)
    implicit none
    integer :: a(*), b(*), n, r
    integer :: i

    do i=1, n
        a(i) = foo(b(i))
    end do
    r = a(n)

  end function myaddint_ref

  function goo(p) result(r)
  !$omp declare simd(goo) inbranch
    real :: p, r
    p = p + 18.5
    r = p
  end function goo

  function myaddfloat(x, y, n) result(r)
    implicit none
    real :: x(*), y(*), r
    integer :: n
    integer :: i

    !$omp simd
    do i=1, n
       if (x(i) > y(i)) then
          x(i) = goo(y(i))
          ! goo is called under the condition (or within a branch)
       else
          x(i) = y(i)
       endif
    end do

    r = x(n)
  end function myaddfloat

  function myaddfloat_ref(x, y, n) result(r)
    implicit none
    real :: x(*), y(*), r
    integer :: n
    integer :: i

    do i=1, n
       if (x(i) > y(i)) then
          x(i) = goo(y(i))
       else
          x(i) = y(i)
       endif
    end do

    r = x(n)
  end function myaddfloat_ref

  subroutine init (b, y, n)
    integer :: b(128)
    real :: y(128)

    s = -1
    do i = 1, n
      b(i) = i*i*s
      y(i) = i*i*s
      s = -s
    end do

  end subroutine

  subroutine init2 (b, y, n)
    integer :: b(128)
    real :: y(128)

    do i = 1, n
      b(i) = i
      y(i) = i
    end do

  end subroutine

  subroutine checkfloat (a, b, n)
      integer :: i, n
      real, parameter :: EPS = 0.000001
      real :: diff, a(*), b(*)
      do i = 1, n
        diff = a(i) - b(i)
        if (diff > EPS .or. -diff > EPS) call abort
      end do
  end subroutine

  subroutine checkint (a, b, n)
      integer :: i, n, a(*), b(*)
      do i = 1, n
        if (a(i) .ne. b(i)) call abort
      end do
  end subroutine

  subroutine test ()
    integer :: a(128), a_ref(128), b(128), ri, ri_ref
    real :: x(128), x_ref(128), y(128), rf, rf_ref

    call  init2(a, x, 128)
    call  init2(a_ref, x_ref, 128)

    call  init(b, y, 128)

    ri = myaddint (a, b, 128)
    rf = myaddfloat (x, y, 128)

    call init(b, y, 128)

    ri_ref = myaddint_ref (a_ref, b, 128)
    rf_ref = myaddfloat_ref (x_ref, y, 128)

    call checkint (a, a_ref, 128)
    call checkfloat (x, x_ref, 128)
  end subroutine

end module

program SIMD6
  use SIMD6_mod, only: test

  call test ()

end program
