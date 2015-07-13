! { dg-do run { target vect_simd_clones } }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

module SIMD2_mod
contains
  function add1(a,b,fact) result(c)
  !$omp declare simd(add1) uniform(fact)
     double precision :: a,b,fact, c
     c = a + b + fact
  end function

  function add2(a,b,i, fact) result(c)
  !$omp declare simd(add2) uniform(a,b,fact) linear(i:1)
     integer,          value        :: i
     double precision, dimension(:) :: a, b
     double precision               :: fact, c
     c = a(i) + b(i) + fact
  end function

  subroutine work(a, b, n )
     implicit none
     double precision           :: a(n),b(n), tmp
     integer                    :: n, i

     !$omp simd private(tmp)
     do i = 1,n
        tmp  = add1(a(i), b(i), 1.0d0)
        a(i) = add2(a,    b, i, 1.0d0) + tmp
        a(i) = a(i) + b(i) + 1.0d0
     end do
  end subroutine

  subroutine work_ref(a, b, n )
     implicit none
     double precision           :: a(n),b(n), tmp
     integer                    :: n, i

     do i = 1,n
        tmp  = add1(a(i), b(i), 1.0d0)
        a(i) = add2(a,    b, i, 1.0d0) + tmp
        a(i) = a(i) + b(i) + 1.0d0
     end do
  end subroutine

  subroutine check (a, b, n)
      integer :: i, n
      double precision, parameter :: EPS = 0.0000000000001
      double precision :: diff, a(*), b(*)
      do i = 1, n
        diff = a(i) - b(i)
        if (diff > EPS .or. -diff > EPS) call abort
      end do
  end subroutine
end module

program main
   use SIMD2_mod
   integer, parameter :: N=32
   integer :: i
   double precision   :: a(N), b(N), a_ref(N)
   do i = 1,N
      a(i) = i-1
      a_ref(i) = a(i)
      b(i) = N-(i-1)
   end do

   call work(a, b, N )
   call work_ref(a_ref, b, N )

   call check(a, a_ref, N )
end program
