! Test host_data interoperability with CUDA blas.  This test was
! derived from libgomp.oacc-c-c++-common/host_data-1.c.

! { dg-do run { target openacc_nvidia_accel_selected } }
! { dg-additional-options "-lcublas -Wall -Wextra" }

program test
  implicit none

  integer, parameter :: N = 10
  integer :: i
  real*4 :: x_ref(N), y_ref(N), x(N), y(N), a
  
  interface
     subroutine cublassaxpy(N, alpha, x, incx, y, incy) bind(c, name="cublasSaxpy")
       use iso_c_binding
       integer(kind=c_int), value :: N
       real(kind=c_float), value :: alpha
       type(*), dimension(*) :: x
       integer(kind=c_int), value :: incx
       type(*), dimension(*) :: y
       integer(kind=c_int), value :: incy
     end subroutine cublassaxpy
  end interface

  a = 2.0

  do i = 1, N
     x(i) = 4.0 * i
     y(i) = 3.0
     x_ref(i) = x(i)
     y_ref(i) = y(i)
  end do

  call saxpy (N, a, x_ref, y_ref)

  !$acc data copyin (x) copy (y)
  !$acc host_data use_device (x, y)
  call cublassaxpy(N, a, x, 1, y, 1)
  !$acc end host_data
  !$acc end data

  call validate_results (N, y, y_ref)

  !$acc data create (x) copyout (y)
  !$acc parallel loop
  do i = 1, N
     y(i) = 3.0
  end do
  !$acc end parallel loop

  !$acc host_data use_device (x, y)
  call cublassaxpy(N, a, x, 1, y, 1)
  !$acc end host_data
  !$acc end data

  call validate_results (N, y, y_ref)

  y(:) = 3.0

  !$acc data copyin (x) copyin (a) copy (y)
  !$acc parallel present (x) pcopy (y) present (a)
  call saxpy (N, a, x, y)
  !$acc end parallel
  !$acc end data

  call validate_results (N, y, y_ref)

  y(:) = 3.0

  !$acc enter data copyin (x, a, y)
  !$acc parallel present (x) pcopy (y) present (a)
  call saxpy (N, a, x, y)
  !$acc end parallel
  !$acc exit data delete (x, a) copyout (y)

  call validate_results (N, y, y_ref)
end program test

subroutine saxpy (nn, aa, xx, yy)
  integer :: nn
  real*4 :: aa, xx(nn), yy(nn)
  integer i
  !$acc routine

  do i = 1, nn
    yy(i) = yy(i) + aa * xx(i)
  end do
end subroutine saxpy

subroutine validate_results (n, a, b)
  integer :: n
  real*4 :: a(n), b(n)

  do i = 1, N
     if (abs(a(i) - b(i)) > 0.0001) call abort
  end do
end subroutine validate_results
