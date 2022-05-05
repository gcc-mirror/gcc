! { dg-do run }

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "-foffload=-fopt-info-note-omp" }

! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }

! { dg-additional-options "-Wuninitialized" }

!TODO
! { dg-xfail-run-if TODO { openacc_radeon_accel_selected && { ! __OPTIMIZE__ } } }

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c_loop 0] }
! { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".  */

program main
  implicit none
  integer :: myint
  integer :: i
  real :: res(65536), tmp

  res(:) = 0.0

  myint = 3
  call gangs(myint, res)

  do i=1,65536
    tmp = i * 97
    if (res(i) .ne. tmp) stop 1
  end do

  res(:) = 0.0

  myint = 5
  call workers(myint, res)

  do i=1,65536
    tmp = i * 99
    if (res(i) .ne. tmp) stop 2
  end do

  res(:) = 0.0

  myint = 7
  call vectors(myint, res)

  do i=1,65536
    tmp = i * 101
    if (res(i) .ne. tmp) stop 3
  end do

  res(:) = 0.0

  myint = 9
  call gangs_workers_vectors(myint, res)

  do i=1,65536
    tmp = i * 103
    if (res(i) .ne. tmp) stop 4
  end do

contains

  subroutine gangs(t1, res)
    implicit none
    integer :: t1
    integer :: i, j
    real, intent(out) :: res(:)

    !$acc parallel copyout(res) num_gangs(64)

    !$acc loop collapse(2) gang private(t1) ! { dg-line l_loop[incr c_loop] }
    ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 't1' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    do i=0,255
      do j=1,256
        t1 = (i * 256 + j) * 97
        res(i * 256 + j) = t1
      end do
    end do

    !$acc end parallel
  end subroutine gangs

  subroutine workers(t1, res)
    implicit none
    integer :: t1
    integer :: i, j
    real, intent(out) :: res(:)

    !$acc parallel copyout(res) num_gangs(64) num_workers(64)
    ! { dg-warning "using .num_workers \\(32\\)., ignoring 64" "" { target openacc_nvidia_accel_selected } .-1 }

    !$acc loop gang ! { dg-line l_loop[incr c_loop] }
    ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    do i=0,255
      !$acc loop worker private(t1) ! { dg-line l_loop[incr c_loop] }
      ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
      ! { dg-note {variable 't1' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
      do j=1,256
        t1 = (i * 256 + j) * 99
        res(i * 256 + j) = t1
      end do
    end do

    !$acc end parallel
  end subroutine workers

  subroutine vectors(t1, res)
    implicit none
    integer :: t1
    integer :: i, j
    real, intent(out) :: res(:)

    !$acc parallel copyout(res) num_gangs(64) num_workers(64)
    ! { dg-warning "using .num_workers \\(32\\)., ignoring 64" "" { target openacc_nvidia_accel_selected } .-1 }

    !$acc loop gang worker ! { dg-line l_loop[incr c_loop] }
    ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    do i=0,255
      !$acc loop vector private(t1) ! { dg-line l_loop[incr c_loop] }
      ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
      ! { dg-note {variable 't1' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
      do j=1,256
        t1 = (i * 256 + j) * 101
        res(i * 256 + j) = t1
      end do
    end do

    !$acc end parallel
  end subroutine vectors

  subroutine gangs_workers_vectors(t1, res)
    implicit none
    integer :: t1
    integer :: i, j
    real, intent(out) :: res(:)

    !$acc parallel copyout(res) num_gangs(64) num_workers(64)
    ! { dg-warning "using .num_workers \\(32\\)., ignoring 64" "" { target openacc_nvidia_accel_selected } .-1 }

    !$acc loop collapse(2) gang worker vector private(t1) ! { dg-line l_loop[incr c_loop] }
    ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'j' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 't1' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    do i=0,255
      do j=1,256
        t1 = (i * 256 + j) * 103
        res(i * 256 + j) = t1
      end do
    end do

    !$acc end parallel
  end subroutine gangs_workers_vectors

end program main
