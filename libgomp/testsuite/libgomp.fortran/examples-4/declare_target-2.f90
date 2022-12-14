! { dg-do run }
! { dg-additional-sources ../on_device_arch.c }
! { dg-additional-options -Wno-complain-wrong-lang }

program e_53_2
  !$omp declare target (fib)
  integer :: x, fib
  integer :: REC_DEPTH = 25

  interface
    integer function on_device_arch_nvptx() bind(C)
    end function on_device_arch_nvptx
  end interface

  if (on_device_arch_nvptx () /= 0) then
     ! Reduced from 25 to 23, otherwise execution runs out of thread stack on
     ! Nvidia Titan V.
     ! Reduced from 23 to 22, otherwise execution runs out of thread stack on
     ! Nvidia T400 (2GB variant), when run with GOMP_NVPTX_JIT=-O0.
     ! Reduced from 22 to 18, otherwise execution runs out of thread stack on
     ! Nvidia RTX A2000 (6GB variant), when run with GOMP_NVPTX_JIT=-O0.
     REC_DEPTH = 18
  end if

  !$omp target map(from: x)
    x = fib (REC_DEPTH)
  !$omp end target
  if (x /= fib (REC_DEPTH)) stop 1
end program

integer recursive function fib (n) result (f)
  !$omp declare target
  integer :: n
  if (n <= 0) then
    f = 0
  else if (n == 1) then
    f = 1
  else
    f = fib (n - 1) + fib (n - 2)
  end if
end function
