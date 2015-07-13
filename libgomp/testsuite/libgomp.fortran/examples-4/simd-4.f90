! { dg-do run }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

module SIMD4_mod
contains
  subroutine work( b, n, m )
     implicit none
     real       :: b(n)
     integer    :: i,n,m

     call init(b, n)

     !$omp simd safelen(16)
     do i = m+1, n
        b(i) = b(i-m) - 1.0
     end do
  end subroutine work

  subroutine work_ref( b, n, m )
     implicit none
     real       :: b(n)
     integer    :: i,n,m

     call init(b, n)

     do i = m+1, n
        b(i) = b(i-m) - 1.0
     end do
  end subroutine work_ref

  subroutine init (b, n)
    real             :: b(*)
    integer          :: n, i, s

    s = -1
    do i = 1, n
      b(i) = i * i * s
      s = -s
    end do

  end subroutine

  subroutine check (a, b, n)
    integer :: i, n
    real, parameter :: EPS = 0.000001
    real :: diff, a(*), b(*)
    do i = 1, n
      diff = a(i) - b(i)
      if (diff > EPS .or. -diff > EPS) call abort
    end do
  end subroutine

end module

program SIMD4
  use SIMD4_mod
  real :: b(128), b_ref(128)

  call  work(b, 128, 32)
  call  work_ref(b_ref, 128, 32)

  call check(b, b_ref, 128)
end program
