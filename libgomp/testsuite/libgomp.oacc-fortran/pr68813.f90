program foo
  implicit none
  integer, parameter :: n = 100
  integer, dimension(n,n) :: a
  integer :: i, j, sum = 0

  a = 1

  !$acc parallel copyin(a(1:n,1:n)) firstprivate (sum)
  !$acc loop gang reduction(+:sum)
  do i=1, n
     !$acc loop vector reduction(+:sum)
     do j=1, n
        sum = sum + a(i, j)
     enddo
  enddo
  !$acc end parallel

end program foo
