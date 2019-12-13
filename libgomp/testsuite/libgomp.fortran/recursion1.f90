! { dg-do run }
! { dg-options "-fcheck=recursion" }
!
! PR 42517: Bogus runtime error with -fopenmp -fcheck=recursion
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

implicit none
integer :: i,s

s=0
!$omp parallel do private(i) shared(s)
do i=1,10
  call sub(i)
end do
!$omp end parallel do
if (s/=55) stop 1

contains

  subroutine sub (n)
    integer :: n
!$omp atomic
    s = s + n
    print '(A,i3)',"loop =",n
  end subroutine

end
