! { dg-do run }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

module SIMD5_mod
contains
  subroutine work( a, b, c,  n )
     implicit none
     integer :: i,j,n
     double precision :: a(n,n), b(n,n), c(n,n), tmp

     !$omp do simd collapse(2) private(tmp)
     do j = 1,n
        do i = 1,n
           tmp = a(i,j) + b(i,j)
           c(i,j) = tmp
        end do
     end do

  end subroutine work

  subroutine work_ref( a, b, c,  n )
     implicit none
     integer :: i,j,n
     double precision :: a(n,n), b(n,n), c(n,n), tmp

     do j = 1,n
        do i = 1,n
           tmp = a(i,j) + b(i,j)
           c(i,j) = tmp
        end do
     end do

  end subroutine work_ref

  subroutine init (a, b, n)
     integer :: i,j,n,s
     double precision :: a(n,n), b(n,n)

     s = -1

     do j = 1,n
        do i = 1,n
           a(i,j) = i*j*s
           b(i,j) = i+j
           s = -s
        end do
     end do

  end subroutine

  subroutine check (a, b, n)
    integer :: i, j, n
    double precision, parameter :: EPS = 0.0000000000000001
    double precision :: diff, a(n,n), b(n,n)
    do j = 1, n
      do i = 1, n
        diff = a(i,j) - b(i,j)
        if (diff > EPS .or. -diff > EPS) STOP 1
      end do
    end do
  end subroutine

end module

program SIMD5
  use SIMD5_mod
  double precision, dimension(32, 32) :: a, b, c, c_ref

  call  init(a, b, 32)

  call  work(a, b, c, 32)
  call  work_ref(a, b, c_ref, 32)

  call check(c, c_ref, 32)
end program
