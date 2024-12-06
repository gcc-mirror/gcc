! { dg-do run }

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=-fopt-info-note-omp" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! for testing/documenting aspects of that functionality.
! Prune a few: uninteresting, and varying depending on GCC configuration (data types):
! { dg-prune-output {note: variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} }

! { dg-additional-options -Wuninitialized }

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c_compute 0 c_loop 0] }
! { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".  */

program main
  implicit none (type, external)
  integer :: j
  integer, allocatable :: A(:)
  character(len=:), allocatable :: my_str
  character(len=15), allocatable :: my_str15

  A = [(3*j, j=1, 10)]
  call foo (A, size(A))
  call bar (A)
  my_str = "1234567890"
  call foo_str(my_str)
  call bar_str(my_str)
  my_str15 = "123456789012345"
  call foobar (my_str15)
  deallocate (A, my_str, my_str15)
contains
  subroutine foo (array, nn)
    integer :: i, nn
    integer :: array(nn)

    !$acc parallel copyout(array) ! { dg-line l_compute[incr c_compute] }
    ! { dg-note {variable 'atmp\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'shadow_loopvar\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'offset\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'S\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    array = [(-i, i = 1, nn)]
    !$acc end parallel
    !$acc parallel copy(array)
    !$acc loop gang private(array) ! { dg-line l_loop[incr c_loop] }
    ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'array' in 'private' clause is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'array' ought to be adjusted for OpenACC privatization level: 'gang'} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'array' adjusted for OpenACC privatization level: 'gang'} "" { target { ! { openacc_host_selected || { openacc_nvidia_accel_selected && __OPTIMIZE__ } } } } l_loop$c_loop }
    do i = 1, 10
      array(i) = i
    end do
    !$acc end parallel
    !$acc parallel copyin(array) ! { dg-line l_compute[incr c_compute] }
    ! { dg-note {variable 'test\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'atmp\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'shadow_loopvar\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'offset\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'S\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    if (any (array /= [(-i, i = 1, nn)])) error stop 1
    !$acc end parallel
  end subroutine foo
  subroutine bar (array)
    integer :: i
    integer :: array(:)

    !$acc parallel copyout(array) ! { dg-line l_compute[incr c_compute] }
    ! { dg-note {variable 'atmp\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'shadow_loopvar\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'offset\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'S\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    array = [(-2*i, i = 1, size(array))]
    !$acc end parallel
    !$acc parallel copy(array)
    !$acc loop gang private(array) ! { dg-line l_loop[incr c_loop] }
    ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'array\.[0-9]+' in 'private' clause is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'array\.[0-9]+' ought to be adjusted for OpenACC privatization level: 'gang'} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'array\.[0-9]+' adjusted for OpenACC privatization level: 'gang'} "" { target { ! { openacc_host_selected || { openacc_nvidia_accel_selected && __OPTIMIZE__ } } } } l_loop$c_loop }
    do i = 1, 10
      array(i) = 9*i
    end do
    !$acc end parallel
    !$acc parallel copyin(array) ! { dg-line l_compute[incr c_compute] }
    ! { dg-note {variable 'test\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'A\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: static} "" { target *-*-* } l_compute$c_compute }
    ! { dg-note {variable 'S\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    if (any (array /= [(-2*i, i = 1, 10)])) error stop 2
    !$acc end parallel
  end subroutine bar
  subroutine foo_str(str)
    integer :: i
    character(len=*) :: str

    !$acc parallel copyout(str)
    str = "abcdefghij"
    !$acc end parallel
    !$acc parallel copy(str)
    !$acc loop gang private(str) ! { dg-line l_loop[incr c_loop] }
    ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'str' in 'private' clause is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'str' ought to be adjusted for OpenACC privatization level: 'gang'} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'str' adjusted for OpenACC privatization level: 'gang'} "" { target { ! { openacc_host_selected || { openacc_nvidia_accel_selected && __OPTIMIZE__ } } } } l_loop$c_loop }
    ! { dg-note {variable 'char\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: artificial} "" { target *-*-* } l_loop$c_loop }
    do i = 1, 10
      str(i:i) = achar(ichar('A') + i)
    end do
    !$acc end parallel
    !$acc parallel copyin(str)
    if (str /= "abcdefghij") error stop 3
    !$acc end parallel
  end
  subroutine bar_str(str)
    integer :: i
    character(len=:), allocatable :: str

! ***************************************
! FIXME: Fails due to PR middle-end/95499
! ***************************************
    !!$acc parallel copyout(str)
    str = "abcdefghij"
    !!$acc end parallel
    !!$acc parallel copy(str)
    !!$acc loop gang private(str)
    !do i = 1, 10
    !  str(i:i) = achar(ichar('A') + i)
    !end do
    !!$acc end parallel
    !!$acc parallel copyin(str)
    if (str /= "abcdefghij") error stop 5
    !!$acc end parallel
  end
  subroutine foobar (scalar)
    integer :: i
    character(len=15), optional :: scalar

    !$acc parallel copyout(scalar)
    scalar = "abcdefghi-12345"
    !$acc end parallel
    !$acc parallel copy(scalar)
    !$acc loop gang private(scalar) ! { dg-line l_loop[incr c_loop] }
    ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'scalar' in 'private' clause is candidate for adjusting OpenACC privatization level} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'scalar' ought to be adjusted for OpenACC privatization level: 'gang'} "" { target *-*-* } l_loop$c_loop }
    ! { dg-note {variable 'scalar' adjusted for OpenACC privatization level: 'gang'} "" { target { ! { openacc_host_selected || { openacc_nvidia_accel_selected && __OPTIMIZE__ } } } } l_loop$c_loop }
    ! { dg-note {variable 'char\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: artificial} "" { target *-*-* } l_loop$c_loop }
    do i = 1, 15
      scalar(i:i) = achar(ichar('A') + i)
    end do
    !$acc end parallel
    !$acc parallel copyin(scalar)
    if (scalar /= "abcdefghi-12345") error stop 6
    !$acc end parallel
  end subroutine foobar
  subroutine foobar15 (scalar)
    integer :: i
    character(len=15), optional, allocatable :: scalar

    !$acc parallel copyout(scalar)
    scalar = "abcdefghi-12345"
    !$acc end parallel
    !$acc parallel copy(scalar)
    !$acc loop gang private(scalar)
    do i = 1, 15
      scalar(i:i) = achar(ichar('A') + i)
    end do
    !$acc end parallel
    !$acc parallel copyin(scalar)
    if (scalar /= "abcdefghi-12345") error stop 1
    !$acc end parallel
  end subroutine foobar15
end
