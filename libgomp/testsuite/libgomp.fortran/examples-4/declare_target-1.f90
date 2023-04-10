! { dg-do run }
! { dg-additional-sources ../on_device_arch.c }
! { dg-additional-options -Wno-complain-wrong-lang }

module e_53_1_mod
  integer :: THRESHOLD = 20
contains
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

  integer function fib_wrapper (n)
    integer :: x
    !$omp target map(to: n) map(from: x) if(n > THRESHOLD)
      x = fib (n)
    !$omp end target
    fib_wrapper = x
  end function
end module

program e_53_1
  use e_53_1_mod, only : fib, fib_wrapper
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
     ! Reduced from 22 to 20, otherwise execution runs out of thread stack on
     ! Nvidia RTX A2000 (6GB variant), when run with GOMP_NVPTX_JIT=-O0.
     REC_DEPTH = 20
  end if

  if (fib (15) /= fib_wrapper (15)) stop 1
  if (fib (REC_DEPTH) /= fib_wrapper (REC_DEPTH)) stop 2
end program
