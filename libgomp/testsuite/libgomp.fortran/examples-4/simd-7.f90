! { dg-do run { target vect_simd_clones } }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

program fibonacci
   implicit none
   integer,parameter :: N=30
   integer           :: a(0:N-1), b(0:N-1)
   integer           :: a_ref(0:N-1)
   integer           :: i
   integer, external :: fib

   !$omp simd
   do i = 0,N-1
      b(i) = i
   end do

   !$omp simd
   do i=0,N-1
      a(i) = fib(b(i))
   end do

   call fib_ref (a_ref, N)

   do i = 0, N-1
     if (a(i) .ne. a_ref(i)) stop 1
   end do

end program

recursive function fib(n) result(r)
!$omp declare simd(fib) inbranch
   integer  :: n, r

   if (n <= 1) then
       r = n
   else
      r = fib(n-1) + fib(n-2)
   endif

end function fib

subroutine fib_ref(a_ref, n)
   integer  :: n, a_ref(0:n-1)

   a_ref(0) = 0
   a_ref(1) = 1

   do i = 2, n-1
     a_ref(i) = a_ref(i-1) + a_ref(i-2)
   end do

end subroutine fib_ref
