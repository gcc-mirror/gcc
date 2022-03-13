! Test a simple acc loop reduction inside a kernels region. 

! { dg-do run }

! { dg-additional-options "--param=openacc-kernels=decompose" }

! { dg-additional-options "-fopt-info-all-omp" }
! { dg-additional-options "-foffload=-fopt-info-all-omp" } */

! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
! { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} } */

program reduction
  integer, parameter     :: n = 20
  integer                :: i, red

  red = 0

  !$acc kernels ! { dg-line l_compute1 } */
  ! { dg-note {OpenACC 'kernels' decomposition: variable 'red' in 'copy' clause requested to be made addressable} {} { target *-*-* } l_compute1 }
  !   { dg-note {variable 'red' made addressable} {} { target *-*-* } l_compute1 }
  !$acc loop reduction (+:red) ! { dg-line l_loop_i1 }
  ! { dg-note {forwarded loop nest in OpenACC 'kernels' region to 'parloops' for analysis} {} { target *-*-* } l_loop_i1 }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l_loop_i1 }
  ! { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l_loop_i1 }
  do i = 1, n
     red = red + 1
  end do
  !$acc end kernels

  if (red .ne. n) stop 1
end program reduction
