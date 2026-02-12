! PR middle-end/113436
! { dg-do run }

program main
  use omp_lib
  implicit none

  call test_integer
  call test_derived_type
contains
  subroutine test_integer
    integer :: x

    !$omp target private(x) &
    !$omp & allocate(allocator(omp_high_bw_mem_alloc), align(16): x)
      if (mod (loc (x), 16) /= 0) stop 1
      x = 2
    !$omp end target
  end subroutine

  subroutine test_derived_type
    type :: Ty
      integer :: a(4)
      real*4 :: b(4)
    end type
    type (Ty) :: t
    integer :: i

    !$omp target private(t) &
    !$omp & allocate(allocator(omp_low_lat_mem_alloc), align(32): t)
      if (mod (loc (t), 32) /= 0) stop 2
      do i = 1, 4
	t%a(i) = i
	t%b(i) = i * 2.0
      end do
    !$omp end target
  end subroutine

  subroutine test_vla
    integer :: n = 10
    integer :: i
    block
      integer :: a(n)

      do i = 1, n
	a(i) = i * 3
      end do

      !$omp target firstprivate(a) &
      !$omp & allocate(allocator(omp_low_lat_mem_alloc), align(64): a)
        if (mod (loc (a), 64) /= 0) stop 6
        do i = 1, n
	  a(i) = a(i) * 2
        end do
      !$omp end target
    end block
  end subroutine
end program
