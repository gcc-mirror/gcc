! Test OpenACC 'declare create' with allocatable arrays.

! { dg-do run }

!TODO-OpenACC-declare-allocate
! Not currently implementing correct '-DACC_MEM_SHARED=0' behavior:
! Missing support for OpenACC "Changes from Version 2.0 to 2.5":
! "The 'declare create' directive with a Fortran 'allocatable' has new behavior".
! { dg-xfail-run-if TODO { *-*-* } { -DACC_MEM_SHARED=0 } }

!TODO { dg-additional-options -fno-inline } for stable results regarding OpenACC 'routine'.

! { dg-additional-options -fopt-info-all-omp }
! { dg-additional-options -foffload=-fopt-info-all-omp }

! { dg-additional-options --param=openacc-privatization=noisy }
! { dg-additional-options -foffload=--param=openacc-privatization=noisy }
! Prune a few: uninteresting, and potentially varying depending on GCC configuration (data types):
! { dg-prune-output {note: variable '[Di]\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} }

! { dg-additional-options -Wopenacc-parallelism }

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c 0] }
! { dg-message dummy {} { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".


module vars
  implicit none
  integer, parameter :: n = 100
  real*8, allocatable :: b(:)
 !$acc declare create (b)
end module vars

program test
  use vars
  use openacc
  implicit none
  real*8 :: a
  integer :: i

  interface
     subroutine sub1
       !$acc routine gang
     end subroutine sub1

     subroutine sub2
     end subroutine sub2

     real*8 function fun1 (ix)
       integer ix
       !$acc routine seq
     end function fun1

     real*8 function fun2 (ix)
       integer ix
       !$acc routine seq
     end function fun2
  end interface

  if (allocated (b)) error stop

  ! Test local usage of an allocated declared array.

  allocate (b(n))

  if (.not.allocated (b)) error stop
  if (.not.acc_is_present (b)) error stop

  a = 2.0

  !$acc parallel loop ! { dg-line l[incr c] }
  ! { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l$c }
  !   { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'vector'} {} { target *-*-* } l$c }
  !   { dg-note {variable 'i' adjusted for OpenACC privatization level: 'vector'} {} { target { ! openacc_host_selected } } l$c }
  ! { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l$c }
  ! { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l$c }
  do i = 1, n
     b(i) = i * a
  end do

  if (.not.acc_is_present (b)) error stop

  !$acc update host(b)

  do i = 1, n
     if (b(i) /= i*a) error stop
  end do

  deallocate (b)

  ! Test the usage of an allocated declared array inside an acc
  ! routine subroutine.

  allocate (b(n))

  if (.not.allocated (b)) error stop
  if (.not.acc_is_present (b)) error stop

  !$acc parallel
  call sub1 ! { dg-line l[incr c] }
  ! { dg-optimized {assigned OpenACC gang worker vector loop parallelism} {} { target *-*-* } l$c }
  !$acc end parallel

  if (.not.acc_is_present (b)) error stop

  !$acc update host(b)

  do i = 1, n
     if (b(i) /= i*2) error stop
  end do

  deallocate (b)

  ! Test the usage of an allocated declared array inside a host
  ! subroutine.

  call sub2

  if (.not.acc_is_present (b)) error stop

  !$acc update host(b)

  do i = 1, n
     if (b(i) /= 1.0) error stop
  end do

  deallocate (b)

  if (allocated (b)) error stop

  ! Test the usage of an allocated declared array inside an acc
  ! routine function.

  allocate (b(n))

  if (.not.allocated (b)) error stop
  if (.not.acc_is_present (b)) error stop

  !$acc parallel loop ! { dg-line l[incr c] }
  ! { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l$c }
  !   { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'vector'} {} { target *-*-* } l$c }
  !   { dg-note {variable 'i' adjusted for OpenACC privatization level: 'vector'} {} { target { ! openacc_host_selected } } l$c }
  ! { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l$c }
  ! { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l$c }
  do i = 1, n
     b(i) = 1.0
  end do

  !$acc parallel loop ! { dg-line l[incr c] }
  ! { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l$c }
  !   { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'vector'} {} { target *-*-* } l$c }
  !   { dg-note {variable 'i' adjusted for OpenACC privatization level: 'vector'} {} { target { ! openacc_host_selected } } l$c }
  ! { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l$c }
  ! { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l$c }
  do i = 1, n
     b(i) = fun1 (i) ! { dg-line l[incr c] }
     ! { dg-optimized {assigned OpenACC seq loop parallelism} {} { target *-*-* } l$c }
  end do

  if (.not.acc_is_present (b)) error stop

  !$acc update host(b)

  do i = 1, n
     if (b(i) /= i) error stop
  end do

  deallocate (b)

  ! Test the usage of an allocated declared array inside a host
  ! function.

  allocate (b(n))

  if (.not.allocated (b)) error stop
  if (.not.acc_is_present (b)) error stop

  !$acc parallel loop ! { dg-line l[incr c] }
  ! { dg-note {variable 'i' in 'private' clause is candidate for adjusting OpenACC privatization level} {} { target *-*-* } l$c }
  !   { dg-note {variable 'i' ought to be adjusted for OpenACC privatization level: 'vector'} {} { target *-*-* } l$c }
  !   { dg-note {variable 'i' adjusted for OpenACC privatization level: 'vector'} {} { target { ! openacc_host_selected } } l$c }
  ! { dg-note {variable 'i\.[0-9]+' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l$c }
  ! { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l$c }
  do i = 1, n
     b(i) = 1.0
  end do

  !$acc update host(b)

  do i = 1, n
     b(i) = fun2 (i)
  end do

  if (.not.acc_is_present (b)) error stop

  do i = 1, n
     if (b(i) /= i*i) error stop
  end do

  deallocate (b)
end program test ! { dg-line l[incr c] }
! { dg-bogus {note: variable 'overflow\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {TODO n/a} { xfail *-*-* } l$c }
! { dg-bogus {note: variable 'not_prev_allocated\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} {TODO n/a} { xfail *-*-* } l$c }
! { dg-bogus {note: variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: artificial} {TODO n/a} { xfail *-*-* } l$c }

! Set each element in array 'b' at index i to i*2.

subroutine sub1 ! { dg-line subroutine_sub1 }
  use vars
  implicit none
  integer i
  !$acc routine gang
  ! { dg-bogus {[Ww]arning: region is worker partitioned but does not contain worker partitioned code} {TODO default 'gang' 'vector'} { xfail *-*-* } subroutine_sub1 }

  !$acc loop ! { dg-line l[incr c] }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l$c }
  ! { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l$c }
  do i = 1, n
     b(i) = i*2
  end do
end subroutine sub1

! Allocate array 'b', and set it to all 1.0.

subroutine sub2
  use vars
  use openacc
  implicit none
  integer i

  allocate (b(n))

  if (.not.allocated (b)) error stop
  if (.not.acc_is_present (b)) error stop

  !$acc parallel loop ! { dg-line l[incr c] }
  ! { dg-note {variable 'i' in 'private' clause isn't candidate for adjusting OpenACC privatization level: not addressable} {} { target *-*-* } l$c }
  ! { dg-optimized {assigned OpenACC gang vector loop parallelism} {} { target *-*-* } l$c }
  do i = 1, n
     b(i) = 1.0
  end do
end subroutine sub2

! Return b(i) * i;

real*8 function fun1 (i)
  use vars
  implicit none
  integer i
  !$acc routine seq

  fun1 = b(i) * i
end function fun1

! Return b(i) * i * i;

real*8 function fun2 (i)
  use vars
  implicit none
  integer i

  fun2 = b(i) * i * i
end function fun2
