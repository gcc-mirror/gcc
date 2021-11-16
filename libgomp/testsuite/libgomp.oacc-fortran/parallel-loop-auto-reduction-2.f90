! Check that the Graphite-based "auto" loop and "kernels" handling
! is able to assign the parallelism dimensions correctly for a simple
! loop-nest with reductions. All loops should be parallelized.

! { dg-additional-options "-O2 -g" }
! { dg-additional-options "-foffload=-fdump-tree-oaccloops1-details" }
! { dg-additional-options "-foffload=-fopt-info-optimized" }
! { dg-additional-options "-fdump-tree-oaccloops1-details" }
! { dg-additional-options "-fopt-info-optimized" }

module test
  implicit none

  integer, parameter :: n = 10000
  integer :: a(n,n)
  integer :: sums(n,n)

contains
  function sum_loop_auto() result(sum)
    integer :: i, j
    integer :: sum, max_val

    sum = 0
    max_val = 0

    !$acc parallel copyin (a) reduction(+:sum)
    !$acc loop auto reduction(+:sum) reduction(max:max_val) ! { dg-optimized "assigned OpenACC gang worker loop parallelism" }
    ! { dg-optimized ".auto. loop can be parallel" "" { target *-*-* } .-1 }
    do i = 1,size (a, 1)
       !$acc loop auto reduction(max:max_val) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
       ! { dg-optimized ".auto. loop can be parallel" "" { target *-*-* } .-1 }
       do j = 1,size(a, 2)
          max_val = a(i,j)
       end do
       sum = sum + max_val
    end do
    !$acc end parallel
  end function sum_loop_auto

  function sum_kernels() result(sum)
    integer :: i, j
    integer :: sum, max_val

    sum = 0
    max_val = 0

    !$acc kernels
    ! { dg-optimized {'map\(force_tofrom:max_val [^)]+\)' optimized to 'map\(to:max_val [^)]+\)'} "" { target *-*-* } .-1 }
    !$acc loop reduction(+:sum) reduction(max:max_val) ! { dg-optimized "assigned OpenACC gang worker loop parallelism" }
    ! { dg-optimized ".auto. loop can be parallel" "" { target *-*-* } .-1 }
    ! { dg-optimized "forwarded loop nest in OpenACC .kernels. construct to .Graphite." "" { target *-*-* } .-2 }
    do i = 1,size (a, 1)
       !$acc loop reduction(max:max_val) ! { dg-optimized "assigned OpenACC vector loop parallelism" }
       ! { dg-optimized ".auto. loop can be parallel" "" { target *-*-* } .-1 }
       do j = 1,size(a, 2)
          max_val = a(i,j)
       end do
       sum = sum + max_val
    end do
    !$acc end kernels
  end function sum_kernels
end module test

program main
  use test

  implicit none

  integer :: result, i, j

  ! We sum the maxima of n rows, each containing numbers
  ! 1..n
  integer, parameter :: expected_sum = n * n

  do i = 1, size (a, 1) ! { dg-optimized "loop nest optimized" }
     do j = 1, size (a, 2)
        a(i, j) = j
     end do
  end do


  result = sum_loop_auto()
  if (result /= expected_sum) then
     write (*, *) "Wrong result:", result
     call abort()
  endif

  result = sum_kernels()
  if (result /= expected_sum) then
     write (*, *) "Wrong result:", result
     call abort()
  endif
end program main

! This ensures that the dg-optimized assertions above hold for both
! compilers because the output goes to stderr and the dump file.
! { dg-final { scan-offload-tree-dump-times "optimized: assigned OpenACC .*? parallelism" 4 "oaccloops1" } }
! { dg-final { scan-tree-dump-times "optimized: assigned OpenACC .*? parallelism" 4 "oaccloops1" } }
