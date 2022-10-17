! { dg-do run }

! See also '../libgomp.oacc-c-c++-common/f-asyncwait-1.c'.

! { dg-additional-options "--param=openacc-kernels=decompose" } */

! { dg-additional-options "-fopt-info-all-omp" }
! { dg-additional-options "-foffload=-fopt-info-all-omp" } */

! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
! { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c_compute 0 c_loop_i 0] }
! { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".  */

program asyncwait
  integer, parameter :: N = 64
  real, allocatable :: a(:), b(:), c(:), d(:), e(:)
  integer i

  allocate (a(N))
  allocate (b(N))
  allocate (c(N))
  allocate (d(N))
  allocate (e(N))

  a(:) = 3.0
  b(:) = 0.0

  !$acc data copy (a(1:N)) copy (b(1:N))

  !$acc parallel async
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel

  !$acc wait
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 3.0) STOP 1
     if (b(i) .ne. 3.0) STOP 2
  end do

  a(:) = 2.0
  b(:) = 0.0

  !$acc data copy (a(1:N)) copy (b(1:N))

  !$acc parallel async (1)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end parallel

  !$acc wait (1)
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 2.0) STOP 3
     if (b(i) .ne. 2.0) STOP 4
  end do

  a(:) = 3.0
  b(:) = 0.0
  c(:) = 0.0
  d(:) = 0.0

  !$acc data copy (a(1:N)) copy (b(1:N)) copy (c(1:N)) copy (d(1:N))

  !$acc parallel async (1)
  do i = 1, N
     b(i) = (a(i) * a(i) * a(i)) / a(i)
  end do
  !$acc end parallel

  !$acc parallel async (1)
  do i = 1, N
     c(i) = (a(i) * 4) / a(i)
  end do
  !$acc end parallel

  !$acc parallel async (1)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     d(i) = ((a(i) * a(i) + a(i)) / a(i)) - a(i)
  end do
  !$acc end parallel

  !$acc wait (1)
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 3.0) STOP 5
     if (b(i) .ne. 9.0) STOP 6
     if (c(i) .ne. 4.0) STOP 7
     if (d(i) .ne. 1.0) STOP 8
  end do

  a(:) = 2.0
  b(:) = 0.0
  c(:) = 0.0
  d(:) = 0.0
  e(:) = 0.0

  !$acc data copy (a(1:N), b(1:N), c(1:N), d(1:N), e(1:N))

  !$acc parallel async (1)
  do i = 1, N
     b(i) = (a(i) * a(i) * a(i)) / a(i)
  end do
  !$acc end parallel

  !$acc parallel async (1)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     c(i) = (a(i) * 4) / a(i)
  end do
  !$acc end parallel

  !$acc parallel async (1)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     d(i) = ((a(i) * a(i) + a(i)) / a(i)) - a(i)
  end do
  !$acc end parallel

  !$acc parallel wait (1) async (1)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC gang vector loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     e(i) = a(i) + b(i) + c(i) + d(i)
  end do
  !$acc end parallel

  !$acc wait (1)
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 2.0) STOP 9
     if (b(i) .ne. 4.0) STOP 10
     if (c(i) .ne. 4.0) STOP 11
     if (d(i) .ne. 1.0) STOP 12
     if (e(i) .ne. 11.0) STOP 13
  end do

  a(:) = 3.0
  b(:) = 0.0

  !$acc data copy (a(1:N)) copy (b(1:N))

  !$acc kernels async
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end kernels

  !$acc wait
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 3.0) STOP 14
     if (b(i) .ne. 3.0) STOP 15
  end do

  a(:) = 2.0
  b(:) = 0.0

  !$acc data copy (a(1:N)) copy (b(1:N))

  !$acc kernels async (1)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     b(i) = a(i)
  end do
  !$acc end kernels

  !$acc wait (1)
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 2.0) STOP 16
     if (b(i) .ne. 2.0) STOP 17
  end do

  a(:) = 3.0
  b(:) = 0.0
  c(:) = 0.0
  d(:) = 0.0

  !$acc data copy (a(1:N)) copy (b(1:N)) copy (c(1:N)) copy (d(1:N))

  !$acc kernels async (1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'i' made addressable} {} { target *-*-* } l_compute$c_compute } */
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_compute$c_compute }
  ! { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 }
  do i = 1, N
     b(i) = (a(i) * a(i) * a(i)) / a(i)
  end do
  !$acc end kernels

  !$acc kernels async (1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'i' already made addressable} {} { target *-*-* } l_compute$c_compute } */
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_compute$c_compute }
  ! { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 }
  do i = 1, N
     c(i) = (a(i) * 4) / a(i)
  end do
  !$acc end kernels

  !$acc kernels async (1)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     d(i) = ((a(i) * a(i) + a(i)) / a(i)) - a(i)
  end do
  !$acc end kernels

  !$acc wait (1)
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 3.0) STOP 18
     if (b(i) .ne. 9.0) STOP 19
     if (c(i) .ne. 4.0) STOP 20
     if (d(i) .ne. 1.0) STOP 21
  end do

  a(:) = 2.0
  b(:) = 0.0
  c(:) = 0.0
  d(:) = 0.0
  e(:) = 0.0

  !$acc data copy (a(1:N), b(1:N), c(1:N), d(1:N), e(1:N))

  !$acc kernels async (1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'i' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute$c_compute }
  !   { dg-note {variable 'i' already made addressable} {} { target *-*-* } l_compute$c_compute } */
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_compute$c_compute }
  ! { dg-note {beginning 'parloops' part in OpenACC 'kernels' region} {} { target *-*-* } .+1 }
  do i = 1, N
     b(i) = (a(i) * a(i) * a(i)) / a(i)
  end do
  !$acc end kernels

  !$acc kernels async (1)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     c(i) = (a(i) * 4) / a(i)
  end do
  !$acc end kernels

  !$acc kernels async (1)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     d(i) = ((a(i) * a(i) + a(i)) / a(i)) - a(i)
  end do
  !$acc end kernels

  !$acc kernels wait (1) async (1)
  !$acc loop ! { dg-line l_loop_i[incr c_loop_i] }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop_i$c_loop_i }
  ! { dg-optimized "assigned OpenACC seq loop parallelism" "" { target *-*-* } l_loop_i$c_loop_i }
  do i = 1, N
     e(i) = a(i) + b(i) + c(i) + d(i)
  end do
  !$acc end kernels

  !$acc wait (1)
  !$acc end data

  do i = 1, N
     if (a(i) .ne. 2.0) STOP 22
     if (b(i) .ne. 4.0) STOP 23
     if (c(i) .ne. 4.0) STOP 24
     if (d(i) .ne. 1.0) STOP 25
     if (e(i) .ne. 11.0) STOP 26
  end do
end program asyncwait
