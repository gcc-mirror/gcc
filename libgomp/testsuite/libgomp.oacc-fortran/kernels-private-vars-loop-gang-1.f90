! Test of gang-private variables declared on loop directive.

! { dg-do run }

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=-fopt-info-note-omp" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! for testing/documenting aspects of that functionality.

program main
  integer :: x, i, arr(32)

  do i = 1, 32
     arr(i) = i
  end do

  !$acc kernels copy(arr)
  !$acc loop gang(num:32) private(x)
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  ! { dg-note {variable 'x' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }
  do i = 1, 32
     x = i * 2;
     arr(i) = arr(i) + x;
  end do
  !$acc end kernels

  do i = 1, 32
     if (arr(i) .ne. i * 3) stop 1
  end do
end program main
