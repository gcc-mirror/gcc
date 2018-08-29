! { dg-do run }

module param
  integer, parameter :: N = 32
end module param

program main
  use param
  integer :: i
  integer :: a(N)

  do i = 1, N
    a(i) = i
  end do

  !$acc parallel copy (a)
  !$acc loop worker
    do i = 1, N
      call vector (a)
    end do
  !$acc end parallel

  do i = 1, N
    if (a(i) .ne. 0) call abort
  end do

contains

  subroutine vector (a)
  !$acc routine vector
  integer, intent (inout) :: a(N)
  integer :: i

  !$acc loop vector
  do i = 1, N
    a(i) = a(i) - a(i) 
  end do

end subroutine vector

end program main
