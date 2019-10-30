! { dg-do run }
program foo
  integer :: i, j, k
  integer :: a(10), c(10)
  k = 2
  a(:) = 0
  call test1
  call test2
  do i = 1, 10
    if (a(i) .ne. 10 * i) stop 1
  end do
  !$omp parallel do reduction (+:c)
  do i = 1, 10
    c = c + a
  end do
  do i = 1, 10
    if (c(i) .ne. 10 * a(i)) stop 2
  end do
  !$omp parallel do lastprivate (j)
  do j = 1, 10, k
  end do
  if (j .ne. 11) stop 3
contains
  subroutine test1
    integer :: i
    integer :: b(10)
    do i = 1, 10
      b(i) = i
    end do
    c(:) = 0
    !$omp parallel do reduction (+:a)
    do i = 1, 10
      a = a + b
    end do
  end subroutine test1
  subroutine test2
    !$omp parallel do lastprivate (j)
    do j = 1, 10, k
    end do
    if (j .ne. 11) stop 4
  end subroutine test2
end program foo
