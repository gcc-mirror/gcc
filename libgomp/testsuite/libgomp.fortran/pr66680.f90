! { dg-do run }
! PR 66680: ICE with openmp, a loop and a type bound procedure
! Contributed by Miha Polajnar <polajnar.miha@gmail.com>
!
module m1 
  implicit none
  integer :: n = 5
  type :: t1
  contains
    procedure :: s => s1
  end type t1
contains
  pure subroutine s1(self,p,esta)
    class(t1), intent(in) :: self
    integer, optional, intent(in) :: p
    integer, intent(out) :: esta
  end subroutine s1 
end module m1
module m2
  use m1, only: t1, n
  implicit none
  type(t1), allocatable :: test(:)
contains
  pure subroutine s2(test1,esta)
    type(t1), intent(in) :: test1
    integer, intent(out) :: esta
    integer :: p, i
    do p = 1, n
      i = p ! using i instead of p works
      call test1%s(p=p,esta=esta)
      if ( esta /= 0 ) return
    end do
  end subroutine s2
  subroutine s3()
    integer :: i, esta
    !$omp parallel do  &
    !$omp private(i)
    do i = 1, n
        call s2(test(i),esta)
    end do
    !$omp end parallel do
  end subroutine s3
end module m2
program main
  implicit none
end program main
