
! { dg-do run }
! { dg-additional-options "-cpp" }

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=-fopt-info-note-omp" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! for testing/documenting aspects of that functionality.

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.
!TODO { dg-additional-options "-fno-inline" } for stable results regarding OpenACC 'routine'.

#define M 8
#define N 32

program main
  integer :: i
  integer :: a(N)
  integer :: b(M * N)

  do i = 1, N
    a(i) = 0
  end do

  !$acc parallel copy (a)
  !$acc loop seq
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
    do i = 1, N
      call seq (a)
    end do
  !$acc end parallel

  do i = 1, N
    if (a(i) .ne.N) STOP 1
  end do

  !$acc parallel copy (a)
  !$acc loop seq
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
    do i = 1, N 
      call gang (a)
    end do
  !$acc end parallel

  do i = 1, N
    if (a(i) .ne. (N + (N * (-1 * i)))) STOP 2
  end do

  do i = 1, N
    b(i) = i
  end do

  !$acc parallel copy (b)
  !$acc loop seq
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
    do i = 1, N
      call worker (b)
    end do
  !$acc end parallel

  do i = 1, N
    if (b(i) .ne. N + i) STOP 3
  end do

  do i = 1, N
    a(i) = i
  end do

  !$acc parallel copy (a)
  !$acc loop seq
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
    do i = 1, N
      call vector (a)
    end do
  !$acc end parallel

  do i = 1, N
    if (a(i) .ne. 0) STOP 4
  end do

contains

subroutine vector (a)
  !$acc routine vector
  integer, intent (inout) :: a(N)
  integer :: i

  !$acc loop vector
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  do i = 1, N
    a(i) = a(i) - a(i) 
  end do

end subroutine vector

subroutine worker (b)
  !$acc routine worker
  integer, intent (inout) :: b(M*N)
  integer :: i, j

  !$acc loop worker
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  do i = 1, N
  !$acc loop vector
     ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
    do j = 1, M
      b(j + ((i - 1) * M)) = b(j + ((i - 1) * M)) + 1
    end do
  end do

end subroutine worker

subroutine gang (a)
  !$acc routine gang
  ! { dg-warning "region is worker partitioned but does not contain worker partitioned code" "" { target *-*-* } .-2 }
  ! { dg-warning "region is vector partitioned but does not contain vector partitioned code" "" { target *-*-* } .-3 }
  integer, intent (inout) :: a(N)
  integer :: i

  !$acc loop gang
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  do i = 1, N
    a(i) = a(i) - i 
  end do

end subroutine gang

subroutine seq (a)
  !$acc routine seq
  integer, intent (inout) :: a(N)
  integer :: i

  do i = 1, N
    a(i) = a(i) + 1
  end do

end subroutine seq

end program main
