! Test of worker-private variables declared on a loop directive.

! { dg-do run }

! { dg-additional-options "--param=openacc-kernels=decompose" }

! { dg-additional-options "-fopt-info-omp-all" }
! { dg-additional-options "-foffload=-fopt-info-omp-all" }

! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! Prune a few: uninteresting:
! { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} }

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c_compute 0 c_loop_i 0 c_loop_j 0 c_loop_k 0] }
! { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".

program main
  integer :: x, i, j, arr(0:32*32)
  common x

  do i = 0, 32*32-1
     arr(i) = i
  end do

  !$acc kernels copy(arr) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'x\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_compute$c_compute }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } .+1 }
  !$acc loop gang(num:32) private(x) ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i$c_loop_i }
  do i = 0, 31
     !$acc loop worker(num:8) private(x) ! { dg-line l_loop_j[incr c_loop_j] }
     ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j }
     ! { dg-note {variable 'x' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_j$c_loop_j }
     do j = 0, 31
        x = ieor(i, j * 3)
        arr(i * 32 + j) = arr(i * 32 + j) + x
     end do
  end do
  ! { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_i$c_loop_i }
  !$acc end kernels

  do i = 0, 32 * 32 - 1
     if (arr(i) .ne. i + ieor(i / 32, mod(i, 32) * 3)) stop 1
  end do
end program main
