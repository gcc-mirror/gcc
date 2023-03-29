! OpenACC parallelism dimensions clauses: num_gangs, num_workers,
! vector_length.

! { dg-additional-sources parallel-dims-aux.c }
! { dg-additional-options -Wno-complain-wrong-lang }
! { dg-do run }

! { dg-additional-options "-DEXPENSIVE" { target run_expensive_tests } }
! { dg-additional-options "-cpp" }

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=-fopt-info-note-omp" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! for testing/documenting aspects of that functionality.

! { dg-additional-options "-Wopenacc-parallelism" } for testing/documenting
! aspects of that functionality.

! See also '../libgomp.oacc-c-c++-common/parallel-dims.c'.

module acc_routines
  implicit none (type, external)

  interface
    integer function acc_gang() bind(C)
      !$acc routine seq
    end function acc_gang

    integer function acc_worker() bind(C)
      !$acc routine seq
    end function acc_worker

    integer function acc_vector() bind(C)
      !$acc routine seq
    end function acc_vector
  end interface
end module acc_routines

program main
  use iso_c_binding
  use openacc
  use acc_routines
  implicit none (type, external)

  integer :: gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max
  integer :: vectors_actual
  integer :: i, j, k


#ifdef EXPENSIVE
  integer, parameter :: N = 100
#else
  integer, parameter :: N = 50
#endif

  call acc_init (acc_device_default)

  ! OpenACC parallel construct.

  !TODO


  ! OpenACC kernels construct.

  !TODO


  ! OpenACC serial construct.

  ! GR, WS, VS.

  gangs_min = huge(gangs_min) ! INT_MAX
  workers_min = huge(workers_min) ! INT_MAX
  vectors_min = huge(vectors_min) ! INT_MAX
  gangs_max = -huge(gangs_max) - 1  ! INT_MIN
  workers_max = -huge(gangs_max) - 1 ! INT_MIN
  vectors_max = -huge(gangs_max) - 1 ! INT_MIN
  !$acc serial &
  !$acc   reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max) ! { dg-warning "using .vector_length \\(32\\)., ignoring 1" "" { target openacc_nvidia_accel_selected } }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  do i = N, -(N-1), -1
     gangs_min = acc_gang ();
     gangs_max = acc_gang ();
     workers_min = acc_worker ();
     workers_max = acc_worker ();
     vectors_min = acc_vector ();
     vectors_max = acc_vector ();
  end do
  !$acc end serial
  if (gangs_min /= 0 .or. gangs_max /= 1 - 1 &
      .or. workers_min /= 0 .or. workers_max /= 1 - 1 &
      .or. vectors_min /= 0 .or. vectors_max /= 1 - 1) &
    stop 1

  ! Composition of GP, WP, VP.

  vectors_actual = 1 ! Implicit 'vector_length (1)' clause.
  gangs_min = huge(gangs_min) ! INT_MAX
  workers_min = huge(workers_min) ! INT_MAX
  vectors_min = huge(vectors_min) ! INT_MAX
  gangs_max = -huge(gangs_max) - 1  ! INT_MIN
  workers_max = -huge(gangs_max) - 1 ! INT_MIN
  vectors_max = -huge(gangs_max) - 1 ! INT_MIN
  !$acc serial copy (vectors_actual) &
  !$acc   copy (gangs_min, gangs_max, workers_min, workers_max, vectors_min, vectors_max) ! { dg-warning "using .vector_length \\(32\\)., ignoring 1" "" { target openacc_nvidia_accel_selected } }
  ! { dg-bogus "\[Ww\]arning: region contains gang partitioned code but is not gang partitioned" "TODO 'serial'" { xfail *-*-* } .-1 }
  ! { dg-bogus "\[Ww\]arning: region contains worker partitioned code but is not worker partitioned" "TODO 'serial'" { xfail *-*-* } .-2 }
  ! { dg-bogus "\[Ww\]arning: region contains vector partitioned code but is not vector partitioned" "TODO 'serial'" { xfail *-*-* } .-3 }
  ! { dg-note {variable 'C.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } .-4 }
  !TODO Unhandled 'CONST_DECL' instance for constant argument in 'acc_on_device' call.
  if (acc_on_device (acc_device_nvidia)) then
     ! The GCC nvptx back end enforces vector_length (32).
     ! It's unclear if that's actually permissible here;
     ! <https://github.com/OpenACC/openacc-spec/issues/238> "OpenACC 'serial'
     ! construct might not actually be serial".
   vectors_actual = 32
  end if
  !$acc loop gang reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  do i = N, -(N-1), -1
     !$acc loop worker reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
     ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
     ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }
     do j = N, -(N-1), -1
        !$acc loop vector reduction (min: gangs_min, workers_min, vectors_min) reduction (max: gangs_max, workers_max, vectors_max)
        ! { dg-note {variable 'k' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
        do k = N * vectors_actual, -(N-1) * vectors_actual, -1
           gangs_min = acc_gang ();
           gangs_max = acc_gang ();
           workers_min = acc_worker ();
           workers_max = acc_worker ();
           vectors_min = acc_vector ();
           vectors_max = acc_vector ();
        end do
     end do
  end do
  !$acc end serial
  if (acc_get_device_type () .eq. acc_device_nvidia) then
     if (vectors_actual /= 32) stop 2
  else
     if (vectors_actual /= 1) stop 3
  end if
  if (gangs_min /= 0 .or. gangs_max /= 1 - 1 &
      .or. workers_min /= 0 .or. workers_max /= 1 - 1 &
      .or. vectors_min /= 0 .or. vectors_max /= vectors_actual - 1) &
    stop 4

end program main
