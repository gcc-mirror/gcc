program p
   implicit none
   integer :: x(8) = 0
   call sub(x)
end
subroutine sub(x)
   implicit none
   integer i
   integer :: x(8)
   integer :: c(8) = [(11*i, i=1,8)]
   call s
   if (any (x /= c)) stop 1
contains
   subroutine s
      integer :: i
      !$omp parallel do reduction(+:x)
      do i = 1, 8
         x(i) = c(i)
      end do
   end
end
