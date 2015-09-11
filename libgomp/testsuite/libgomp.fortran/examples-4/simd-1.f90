! { dg-do run }
! { dg-additional-options "-msse2" { target sse2_runtime } }
! { dg-additional-options "-mavx" { target avx_runtime } }

module SIMD1_mod
contains
  subroutine init (a, a_ref, b, c, n, ioff_ptr)
    double precision :: a(*), a_ref(*), b(*), c(*)
    integer          :: n, i, s
    integer, pointer :: ioff_ptr

    s = -1
    do i = 1, n
      a(i) = i * i * s
      a_ref(i) = a(i)
      b(i) = i + i
    end do

    do i = 1, n+ioff_ptr
      c(i) = i * 3
    end do

  end subroutine

  subroutine check (a, b, n)
    integer :: i, n
    double precision, parameter :: EPS = 0.0000000000001
    double precision :: diff, a(*), b(*)
    do i = 1, n
      diff = a(i) - b(i)
      if (diff > EPS .or. -diff > EPS) call abort
    end do
  end subroutine

  subroutine star(a, a_ref, b, c, n, ioff_ptr)
     double precision :: a(*), a_ref(*), b(*), c(*)
     integer          :: n, i
     integer, pointer :: ioff_ptr

     call init (a, a_ref, b, c, n, ioff_ptr)

     !$omp simd
     do i = 1,n
        a(i) = a(i) * b(i) * c(i+ioff_ptr)
     end do

     do i = 1,n
        a_ref(i) = a_ref(i) * b(i) * c(i+ioff_ptr)
     end do

     call check (a, a_ref, n)

  end subroutine
end module

program SIMD1
  use SIMD1_mod, only : star
  double precision :: a(128), a_ref(128), b(128), c(144)
  integer, pointer:: ioff_ptr
  integer, target:: offset

  offset = 16
  ioff_ptr => offset

  call star (a, a_ref, b, c, 128, ioff_ptr)

end program
