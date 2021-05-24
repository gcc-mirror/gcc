! Test of vector-private variables declared on loop directive.

! { dg-do run }

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=-fopt-info-note-omp" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! for testing/documenting aspects of that functionality.

program main
  integer :: x, i, j, k, idx, arr(0:32*32*32)

  do i = 0, 32*32*32-1
     arr(i) = i
  end do

  !$acc kernels copy(arr)
  !$acc loop gang(num:32)
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  do i = 0, 31
     !$acc loop worker(num:8)
     ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
     do j = 0, 31
        !$acc loop vector(length:32) private(x)
        ! { dg-note {variable 'k' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
        ! { dg-note {variable 'x' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }
        do k = 0, 31
           x = ieor(i, j * 3)
           arr(i * 1024 + j * 32 + k) = arr(i * 1024 + j * 32 + k) + x * k
        end do
        !$acc loop vector(length:32) private(x)
        ! { dg-note {variable 'k' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
        ! { dg-note {variable 'x' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }
        do k = 0, 31
           x = ior(i, j * 5)
           arr(i * 1024 + j * 32 + k) = arr(i * 1024 + j * 32 + k) + x * k
        end do
     end do
  end do
  !$acc end kernels

  do i = 0, 32 - 1
     do j = 0, 32 -1
        do k = 0, 32 - 1
           idx = i * 1024 + j * 32 + k
           if (arr(idx) .ne. idx + ieor(i, j * 3) * k + ior(i, j * 5) * k) then
              stop 1
           end if
        end do
     end do
  end do
end program main
