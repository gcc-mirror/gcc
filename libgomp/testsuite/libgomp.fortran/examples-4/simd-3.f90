! { dg-do run }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

module SIMD3_mod
contains
  subroutine work( a, b, n, sum )
     implicit none
     integer :: i, n
     double precision :: a(n), b(n), sum, tmp

     sum = 0.0d0
     call init(a, b, n)
     !$omp simd private(tmp) reduction(+:sum)
     do i = 1,n
        tmp = a(i) + b(i)
        sum = sum + tmp
     end do

  end subroutine work

  subroutine work_ref( a, b, n, sum )
     implicit none
     integer :: i, n
     double precision :: a(n), b(n), sum, tmp

     sum = 0.0d0
     call init(a, b, n)
     do i = 1,n
        tmp = a(i) + b(i)
        sum = sum + tmp
     end do

  end subroutine work_ref

  subroutine init (a, b, n)
    double precision :: a(*), b(*)
    integer          :: n, i, s

    s = -1
    do i = 1, n
      a(i) = i * i * s
      b(i) = i + i
      s = -s
    end do

  end subroutine
end module

program SIMD3
  use SIMD3_mod
  double precision :: a(128), b(128), sum, sum_ref, diff
  double precision, parameter :: EPS = 0.0000000000000001

  call  work(a, b, 128, sum)
  call  work_ref(a, b, 128, sum_ref)

  diff = sum - sum_ref

  if (diff > EPS .or. -diff > EPS) call abort

end program
