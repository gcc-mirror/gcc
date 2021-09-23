! Test of worker-private variables declared on a loop directive.

! { dg-do run }

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=-fopt-info-note-omp" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! for testing/documenting aspects of that functionality.

program main
  integer :: x, i, j, arr(0:32*32)
  common x

  do i = 0, 32*32-1
     arr(i) = i
  end do

  !$acc kernels copy(arr)
  !$acc loop gang(num:32) private(x)
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  do i = 0, 31
     !$acc loop worker(num:8) private(x)
     ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
     ! { dg-note {variable 'x' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }
     do j = 0, 31
        x = ieor(i, j * 3)
        arr(i * 32 + j) = arr(i * 32 + j) + x
     end do
  end do
  !$acc end kernels

  do i = 0, 32 * 32 - 1
     if (arr(i) .ne. i + ieor(i / 32, mod(i, 32) * 3)) stop 1
  end do
end program main
