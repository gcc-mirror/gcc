! { dg-do run }

program main
  integer, parameter :: n = 100
  integer i, a(n), b(n)
  integer x

  do i = 1, n
     b(i) = i
  end do

  !$acc parallel loop gang (static:*) num_gangs (10)
  do i = 1, n
     a(i) = b(i) + 0
  end do
  !$acc end parallel loop

  call test (a, b, 0, n)

  !$acc parallel loop gang (static:1) num_gangs (10)
  do i = 1, n
     a(i) = b(i) + 1
  end do
  !$acc end parallel loop

  call test (a, b, 1, n)

  !$acc parallel loop gang (static:2) num_gangs (10)
  do i = 1, n
     a(i) = b(i) + 2
  end do
  !$acc end parallel loop

  call test (a, b, 2, n)

  !$acc parallel loop gang (static:5) num_gangs (10)
  do i = 1, n
     a(i) = b(i) + 5
  end do
  !$acc end parallel loop

  call test (a, b, 5, n)

  !$acc parallel loop gang (static:20) num_gangs (10)
  do i = 1, n
     a(i) = b(i) + 20
  end do
  !$acc end parallel loop

  call test (a, b, 20, n)

  x = 5
  !$acc parallel loop gang (static:0+x) num_gangs (10)
  do i = 1, n
     a(i) = b(i) + 5
  end do
  !$acc end parallel loop

  call test (a, b, 5, n)

  x = 10
  !$acc parallel loop gang (static:x) num_gangs (10)
  do i = 1, n
     a(i) = b(i) + 10
  end do
  !$acc end parallel loop

  call test (a, b, 10, n)
end program main

subroutine test (a, b, sarg, n)
  integer n
  integer a (n), b(n), sarg
  integer i

  do i = 1, n
     if (a(i) .ne. b(i) + sarg) call abort ()
  end do
end subroutine test
