! { dg-do run }
! { dg-options "-fno-inline" }

! { dg-additional-options "-fopt-info-all-omp" }
! { dg-additional-options "-foffload=-fopt-info-all-omp" }

! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
! { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} }

  module m1
    contains
    recursive function fact (x) result (res)
      !$acc routine
      integer, intent(in) :: x
      integer :: res
      if (x < 1) then
         res = 1
      else
         res = x * fact (x - 1) ! { dg-optimized {assigned OpenACC seq loop parallelism} }
      end if
    end function fact
  end module m1
  use m1
  integer, parameter :: n = 10
  integer :: a(n), i
  !$acc parallel
  !$acc loop ! { dg-line l_loop1 }
  ! { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l_loop1 }
  !   { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'vector'} {} { target *-*-* } l_loop1 }
  !   { dg-note {variable 'i' adjusted for OpenACC privatization level: 'vector'} {} { target { ! openacc_host_selected } } l_loop1 }
  ! { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop1 }
  ! { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l_loop1 }
  do i = 1, n
     a(i) = fact (i) ! { dg-optimized {assigned OpenACC seq loop parallelism} }
  end do
  !$acc end parallel
  do i = 1, n
     if (a(i) .ne. fact(i)) STOP 1
  end do
end
