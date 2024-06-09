module matrix
  implicit none
  integer :: n = 10
  integer :: m = 10

contains

  function copy (a, b) result (c)
    integer, allocatable, dimension (:,:) :: a,b,c
    integer :: i, j, k, inner

    allocate(c(n, m))
    do i = 1,10
      do j = 1,n
        c(j,i) = 0
      end do
    end do

    !$omp unroll partial(2)
    !$omp tile sizes (1,5)
    do i = 1,10
      do j = 1,n
        c(j,i) = c(j,i) + a(j, i)
      end do
    end do
  end function copy

  subroutine print_matrix (m)
    integer, allocatable :: m(:,:)
    integer :: i, j, n

    n = size (m, 1)
    do i = 1,n
      do j = 1,n
        write (*, fmt="(i4)", advance='no') m(j, i)
      end do
      write (*, *)  ""
    end do
    write (*, *)  ""
  end subroutine
end module matrix

program main
  use matrix
  implicit none

  integer, allocatable :: a(:,:),b(:,:),c(:,:)
  integer :: i,j

  allocate(a(n, m))
  allocate(b(n, m))

  do i = 1,n
    do j = 1,m
      a(j,i) = 1
    end do
  end do

  c = copy (a, b)

  call print_matrix (a)
  call print_matrix (b)
  call print_matrix (c)

  do i = 1,n
    do j = 1,m
      if (c(i,j) .ne. a(i,j)) stop 1
    end do
  end do

end program main
