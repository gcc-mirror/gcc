module matrix
  implicit none
  integer :: n = 10
  integer :: m =  10

contains

  function mult (a, b) result (c)
    integer, allocatable, dimension (:,:) :: a,b,c
    integer :: i, j, k, inner

    allocate(c( n, m ))
    do i = 1,10
      do j = 1,n
        c(j,i) = 0
      end do
    end do

    !$omp unroll partial(10)
    !$omp tile sizes(1, 3)
    do i = 1,10
      do j = 1,n
        do k = 1, n
          c(j,i) = c(j,i) + a(k, i) * b(j, k)
        end do
      end do
    end do
  end function mult

  function mult2 (a, b) result (c)
    integer, allocatable, dimension (:,:) :: a,b,c
    integer :: i, j, k, inner

    allocate(c(n, m))
    do i = 1,10
      do j = 1,n
        c(j,i) = 0
      end do
    end do

    !$omp unroll partial(2)
    !$omp tile sizes(1,2)
    do i = 1,10
      do j = 1,n
        do k = 1, n
          c(j,i) = c(j,i) + a(k, i) * b(j, k)
        end do
      end do
    end do
  end function mult2

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
  integer, allocatable :: a(:,:),b(:,:),c(:,:),d(:,:)
  integer :: i,j

  allocate(a( n, m ))
  allocate(b( n, m ))

  do i = 1,n
    do j = 1,m
      a(j,i) = merge(1,0, i.eq.j)
      b(j,i) = j
    end do
  end do

  d = mult (a, b)

  call print_matrix (a)
  call print_matrix (b)
  call print_matrix (d)

  do i = 1,n
    do j = 1,m
      if (b(i,j) .ne. d(i,j)) stop 1
    end do
  end do

  c = mult2 (a, b)

  call print_matrix (a)
  call print_matrix (b)
  call print_matrix (c)

  do i = 1,n
     do j = 1,m
        if (b(i,j) .ne. c(i,j)) stop 2
     end do
  end do

end program main
