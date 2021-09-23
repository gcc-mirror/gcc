! Test of gang-private addressable variable declared on loop directive, with
! broadcasting to partitioned workers.

! { dg-do run }

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=-fopt-info-note-omp" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! for testing/documenting aspects of that functionality.

program main
  type vec3
     integer x, y, z, attr(13)
  end type vec3

  integer x, i, j, arr(0:32*32)
  type(vec3) pt
  
  do i = 0, 32*32-1
     arr(i) = i
  end do

  !$acc kernels copy(arr)
  !$acc loop gang(num:32) private(pt)
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  ! { dg-note {variable 'pt' in 'private' clause is candidate for adjusting OpenACC privatization level} "" { target *-*-* } .-2 }
  do i = 0, 31
     pt%x = i
     pt%y = i * 2
     pt%z = i * 4
     pt%attr(5) = i * 6

     !$acc loop vector(length:32)
     ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
     do j = 0, 31
        arr(i * 32 + j) = arr(i * 32 + j) + pt%x + pt%y + pt%z + pt%attr(5);
     end do
  end do
  !$acc end kernels

  do i = 0, 32 * 32 - 1
     if (arr(i) .ne. i + (i / 32) * 13) stop 1
  end do
end program main
