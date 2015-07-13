! { dg-do run { target vect_simd_clones } }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

program fibonacci
   implicit none
   integer,parameter :: N=45
   integer           :: a(0:N-1), b(0:N-1)
   integer           :: a_ref(0:N-1), b_ref(0:N-1)
   integer           :: i
   integer, external :: fib

   !$omp simd
   do i = 0,N-1
      b(i) = i
   end do

   do i = 0,N-1
      b_ref(i) = i
   end do

   !$omp simd
   do i=0,N-1
      a(i) = fib(b(i))
   end do

   do i=0,N-1
      a_ref(i) = fib(b_ref(i))
   end do

   do i = 0, N-1
     if (a(i) .ne. a_ref(i)) call abort ()
   end do

   if (a(44) .ne. 1134903170) call abort()

end program

recursive function fib(n) result(r)
!$omp declare simd(fib) inbranch
   integer  :: n, r

   if (n <= 2) then
      r = n
   else
      r = fib(n-1) + fib(n-2)
   endif

end function fib
