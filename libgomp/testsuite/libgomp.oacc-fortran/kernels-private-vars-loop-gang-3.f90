! Test of gang-private variables declared on loop directive, with broadcasting
! to partitioned vectors.

! { dg-do run }

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=-fopt-info-note-omp" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! for testing/documenting aspects of that functionality.

program main
  integer :: x, i, j, arr(0:32*32)

  do i = 0, 32*32-1
     arr(i) = i
  end do

  !$acc kernels copy(arr)
  !$acc loop gang(num:32) private(x)
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  ! { dg-note {variable 'x' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }
  do i = 0, 31
     x = i * 2;

     !$acc loop vector(length:32)
     ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
     do j = 0, 31
        arr(i * 32 + j) = arr(i * 32 + j) + x;
     end do
  end do
  !$acc end kernels

  do i = 0, 32 * 32 - 1
     if (arr(i) .ne. i + (i / 32) * 2) stop 1
  end do
end program main
