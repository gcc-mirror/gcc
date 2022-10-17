! 'atomic' access of vector-private variable

! { dg-do run }

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=-fopt-info-note-omp" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! for testing/documenting aspects of that functionality.


program main
  integer :: w, arr(0:31)

  !$acc parallel num_gangs(32) num_workers(32) copyout(arr)
    !$acc loop gang worker vector private(w)
    ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
    ! { dg-note {variable 'w' in 'private' clause is candidate for adjusting OpenACC privatization level} "" { target *-*-* } .-2 }
    ! { dg-note {variable 'w' ought to be adjusted for OpenACC privatization level: 'vector'} "" { target *-*-* } .-3 }
    ! { dg-note {variable 'w' adjusted for OpenACC privatization level: 'vector'} "" { target { ! openacc_host_selected } } .-4 }
    do j = 0, 31
      w = 0
      !$acc loop seq
      ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
      do i = 0, 31
        !$acc atomic update
        w = w + 1
        !$acc end atomic
      end do
      arr(j) = w
    end do
  !$acc end parallel

  if (any (arr .ne. 32)) stop 1
end program main
