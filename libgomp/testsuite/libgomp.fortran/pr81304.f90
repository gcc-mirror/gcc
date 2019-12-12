! PR fortran/81304
! { dg-do run }
! { dg-options "-Wsurprising" }

program pr81304
   integer :: i
   real, dimension(1:3) :: a, b, c
   a = 128
   b = 0
!$omp parallel do reduction(min: a) reduction(max: b) private (c)	! { dg-bogus "Type specified for intrinsic function" }
   do i = 1, 16
     c = (/ i, i - 5, i + 5 /)
     a = min (a, c)
     b = max (b, c)
   end do
   if (any (a /= (/ 1, -4, 6 /)) .or. any (b /= (/ 16, 11, 21 /))) stop 1
end
