! { dg-do run }
! { dg-additional-options "-cpp" }

! { dg-additional-options "-fopt-info-note-omp" }
! { dg-additional-options "--param=openacc-privatization=noisy" }
! { dg-additional-options "-foffload=-fopt-info-note-omp" }
! { dg-additional-options "-foffload=--param=openacc-privatization=noisy" }
! for testing/documenting aspects of that functionality.

! It's only with Tcl 8.5 (released in 2007) that "the variable 'varName'
! passed to 'incr' may be unset, and in that case, it will be set to [...]",
! so to maintain compatibility with earlier Tcl releases, we manually
! initialize counter variables:
! { dg-line l_dummy[variable c_compute 0] }
! { dg-message "dummy" "" { target iN-VAl-Id } l_dummy } to avoid
! "WARNING: dg-line var l_dummy defined, but not used".  */

program main
  use openacc
  implicit none

  integer, parameter :: N = 8
  integer, parameter :: one = 1
  integer, parameter :: zero = 0
  integer i, nn
  real, allocatable :: a(:), b(:)
  real exp, exp2

  i = 0

  allocate (a(N))
  allocate (b(N))

  a(:) = 4.0

  !$acc parallel copyin (a(1:N)) copyout (b(1:N)) if (1 == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
     do i = 1, N
        ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
        !TODO Unhandled 'CONST_DECL' instances for constant argument in 'acc_on_device' call.
        if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
          b(i) = a(i) + 1
        else
          b(i) = a(i)
        end if
     end do
  !$acc end parallel

#if ACC_MEM_SHARED
  exp = 5.0
#else
  exp = 4.0
#endif

  do i = 1, N
    if (b(i) .ne. exp) STOP 1
  end do

  a(:) = 16.0

  !$acc parallel if (0 == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
     do i = 1, N
        ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
       if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
         b(i) = a(i) + 1
       else
         b(i) = a(i)
       end if
     end do
  !$acc end parallel

  do i = 1, N
    if (b(i) .ne. 17.0) STOP 2
  end do

  a(:) = 8.0

  !$acc parallel copyin (a(1:N)) copyout (b(1:N)) if (one == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end parallel

#if ACC_MEM_SHARED
  exp = 9.0
#else
  exp = 8.0
#endif

  do i = 1, N
    if (b(i) .ne. exp) STOP 3
  end do

  a(:) = 22.0

  !$acc parallel if (zero == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end parallel

  do i = 1, N
    if (b(i) .ne. 23.0) STOP 4
  end do

  a(:) = 16.0

  !$acc parallel copyin (a(1:N)) copyout (b(1:N)) if (.TRUE.) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end parallel

#if ACC_MEM_SHARED
  exp = 17.0;
#else
  exp = 16.0;
#endif

  do i = 1, N
    if (b(i) .ne. exp) STOP 5
  end do

  a(:) = 76.0

  !$acc parallel if (.FALSE.) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end parallel

  do i = 1, N
    if (b(i) .ne. 77.0) STOP 6
  end do

  a(:) = 22.0

  nn = 1

  !$acc parallel copyin (a(1:N)) copyout (b(1:N)) if (nn == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end parallel

#if ACC_MEM_SHARED
  exp = 23.0;
#else
  exp = 22.0;
#endif

  do i = 1, N
    if (b(i) .ne. exp) STOP 7
  end do

  a(:) = 18.0

  nn = 0

  !$acc parallel if (nn == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end parallel

  do i = 1, N
    if (b(i) .ne. 19.0) STOP 8
  end do

  a(:) = 49.0

  nn = 1

  !$acc parallel copyin (a(1:N)) copyout (b(1:N)) if ((nn + nn) > 0) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end parallel

#if ACC_MEM_SHARED
  exp = 50.0
#else
  exp = 49.0
#endif

  do i = 1, N
    if (b(i) .ne. exp) STOP 9
  end do

  a(:) = 38.0

  nn = 0;

  !$acc parallel copyin (a(1:N)) copyout (b(1:N)) if ((nn + nn) > 0) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end parallel

  do i = 1, N
    if (b(i) .ne. 39.0) STOP 10
  end do

  a(:) = 91.0

  !$acc parallel copyin (a(1:N)) copyout (b(1:N)) if (-2 > 0) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end parallel

  do i = 1, N
    if (b(i) .ne. 92.0) STOP 11
  end do

  a(:) = 43.0

  !$acc parallel copyin (a(1:N)) copyout (b(1:N)) if (one == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end parallel

#if ACC_MEM_SHARED
  exp = 44.0
#else
  exp = 43.0
#endif

  do i = 1, N
    if (b(i) .ne. exp) STOP 12
  end do

  a(:) = 87.0

  !$acc parallel if (one == 0) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end parallel

  do i = 1, N
    if (b(i) .ne. 88.0) STOP 13
  end do

  a(:) = 3.0
  b(:) = 9.0

#if ACC_MEM_SHARED
  exp = 0.0
  exp2 = 0.0
#else
  call acc_copyin (a, sizeof (a))
  call acc_copyin (b, sizeof (b))
  exp = 3.0;
  exp2 = 9.0;
#endif

  !$acc update device (a(1:N), b(1:N)) if (1 == 1)

  a(:) = 0.0
  b(:) = 0.0

  !$acc update host (a(1:N), b(1:N)) if (1 == 1)

  do i = 1, N
    if (a(i) .ne. exp) STOP 14
    if (b(i) .ne. exp2) STOP 15
  end do

  a(:) = 6.0
  b(:) = 12.0

  !$acc update device (a(1:N), b(1:N)) if (0 == 1)

  a(:) = 0.0
  b(:) = 0.0

  !$acc update host (a(1:N), b(1:N)) if (1 == 1)

  do i = 1, N
    if (a(i) .ne. exp) STOP 16
    if (b(i) .ne. exp2) STOP 17
  end do

  a(:) = 26.0
  b(:) = 21.0

  !$acc update device (a(1:N), b(1:N)) if (1 == 1)

  a(:) = 0.0
  b(:) = 0.0

  !$acc update host (a(1:N), b(1:N)) if (0 == 1)

  do i = 1, N
    if (a(i) .ne. 0.0) STOP 18
    if (b(i) .ne. 0.0) STOP 19
  end do

#if !ACC_MEM_SHARED
  call acc_copyout (a, sizeof (a))
  call acc_copyout (b, sizeof (b))
#endif

  a(:) = 4.0
  b(:) = 0.0

  !$acc data copyin (a(1:N)) copyout (b(1:N)) if (1 == 1)
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  ! { dg-note {variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }

    !$acc parallel present (a(1:N)) ! { dg-line l_compute[incr c_compute] }
    ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
       do i = 1, N
           b(i) = a(i)
       end do
    !$acc end parallel
  !$acc end data

  do i = 1, N
    if (b(i) .ne. 4.0) STOP 20
  end do

  a(:) = 8.0
  b(:) = 1.0

  !$acc data copyin (a(1:N)) copyout (b(1:N)) if (0 == 1)
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target { ! openacc_host_selected } } .-1 }
  ! { dg-note {variable 'parm\.[0-9]+' declared in block is candidate for adjusting OpenACC privatization level} "" { target { ! openacc_host_selected } } .-2 }

#if !ACC_MEM_SHARED
  if (acc_is_present (a) .eqv. .TRUE.) STOP 21
  if (acc_is_present (b) .eqv. .TRUE.) STOP 22
#endif

  !$acc end data

  a(:) = 18.0
  b(:) = 21.0

  !$acc data copyin (a(1:N)) if (1 == 1)
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  ! { dg-note {variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }
  ! { dg-note {variable 'parm\.[0-9]+' declared in block is candidate for adjusting OpenACC privatization level} "" { target { ! openacc_host_selected } } .-3 }

#if !ACC_MEM_SHARED
    if (acc_is_present (a) .eqv. .FALSE.) STOP 23
#endif

    !$acc data copyout (b(1:N)) if (0 == 1)
    ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
    ! { dg-note {variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }
    ! { dg-note {variable 'parm\.[0-9]+' declared in block is candidate for adjusting OpenACC privatization level} "" { target { ! openacc_host_selected } } .-3 }
#if !ACC_MEM_SHARED
      if (acc_is_present (b) .eqv. .TRUE.) STOP 24
#endif
        !$acc data copyout (b(1:N)) if (1 == 1)
        ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
        ! { dg-note {variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }

        !$acc parallel present (a(1:N)) present (b(1:N)) ! { dg-line l_compute[incr c_compute] }
        ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
          do i = 1, N
            b(i) = a(i)
          end do
      !$acc end parallel

    !$acc end data

#if !ACC_MEM_SHARED
    if (acc_is_present (b) .eqv. .TRUE.) STOP 25
#endif
    !$acc end data
  !$acc end data

  do i = 1, N
   if (b(1) .ne. 18.0) STOP 26
  end do

  !$acc enter data copyin (b(1:N)) if (0 == 1)

#if !ACC_MEM_SHARED
  if (acc_is_present (b) .eqv. .TRUE.) STOP 27
#endif

  !$acc exit data delete (b(1:N)) if (0 == 1)

  !$acc enter data copyin (b(1:N)) if (1 == 1)

#if !ACC_MEM_SHARED
    if (acc_is_present (b) .eqv. .FALSE.) STOP 28
#endif

  !$acc exit data delete (b(1:N)) if (1 == 1)

#if !ACC_MEM_SHARED
  if (acc_is_present (b) .eqv. .TRUE.) STOP 29
#endif

  !$acc enter data copyin (b(1:N)) if (zero == 1)

#if !ACC_MEM_SHARED
    if (acc_is_present (b) .eqv. .TRUE.) STOP 30
#endif

  !$acc exit data delete (b(1:N)) if (zero == 1)

  !$acc enter data copyin (b(1:N)) if (one == 1)

#if !ACC_MEM_SHARED
    if (acc_is_present (b) .eqv. .FALSE.) STOP 31
#endif

  !$acc exit data delete (b(1:N)) if (one == 1)

#if !ACC_MEM_SHARED
  if (acc_is_present (b) .eqv. .TRUE.) STOP 32
#endif

  !$acc enter data copyin (b(1:N)) if (one == 0)

#if !ACC_MEM_SHARED
    if (acc_is_present (b) .eqv. .TRUE.) STOP 33
#endif

  !$acc exit data delete (b(1:N)) if (one == 0)

  !$acc enter data copyin (b(1:N)) if (one == 1)

#if !ACC_MEM_SHARED
    if (acc_is_present (b) .eqv. .FALSE.) STOP 34
#endif

  !$acc exit data delete (b(1:N)) if (one == 1)

#if !ACC_MEM_SHARED
  if (acc_is_present (b) .eqv. .TRUE.) STOP 35
#endif

  a(:) = 4.0

  !$acc kernels copyin (a(1:N)) copyout (b(1:N)) if (1 == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
     do i = 1, N
        ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
        if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
          b(i) = a(i) + 1
        else
          b(i) = a(i)
        end if
     end do
  !$acc end kernels

#if ACC_MEM_SHARED
  exp = 5.0
#else
  exp = 4.0
#endif

  do i = 1, N
    if (b(i) .ne. exp) STOP 36
  end do

  a(:) = 16.0

  !$acc kernels if (0 == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
     do i = 1, N
        ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
       if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
         b(i) = a(i) + 1
       else
         b(i) = a(i)
       end if
     end do
  !$acc end kernels

  do i = 1, N
    if (b(i) .ne. 17.0) STOP 37
  end do

  a(:) = 8.0

  !$acc kernels copyin (a(1:N)) copyout (b(1:N)) if (one == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end kernels

#if ACC_MEM_SHARED
  exp = 9.0
#else
  exp = 8.0
#endif

  do i = 1, N
    if (b(i) .ne. exp) STOP 38
  end do

  a(:) = 22.0

  !$acc kernels if (zero == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end kernels

  do i = 1, N
    if (b(i) .ne. 23.0) STOP 39
  end do

  a(:) = 16.0

  !$acc kernels copyin (a(1:N)) copyout (b(1:N)) if (.TRUE.) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end kernels

#if ACC_MEM_SHARED
  exp = 17.0;
#else
  exp = 16.0;
#endif

  do i = 1, N
    if (b(i) .ne. exp) STOP 40
  end do

  a(:) = 76.0

  !$acc kernels if (.FALSE.) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end kernels

  do i = 1, N
    if (b(i) .ne. 77.0) STOP 41
  end do

  a(:) = 22.0

  nn = 1

  !$acc kernels copyin (a(1:N)) copyout (b(1:N)) if (nn == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end kernels

#if ACC_MEM_SHARED
  exp = 23.0;
#else
  exp = 22.0;
#endif

  do i = 1, N
    if (b(i) .ne. exp) STOP 42
  end do

  a(:) = 18.0

  nn = 0

  !$acc kernels if (nn == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end kernels

  do i = 1, N
    if (b(i) .ne. 19.0) STOP 43
  end do

  a(:) = 49.0

  nn = 1

  !$acc kernels copyin (a(1:N)) copyout (b(1:N)) if ((nn + nn) > 0) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end kernels

#if ACC_MEM_SHARED
  exp = 50.0
#else
  exp = 49.0
#endif

  do i = 1, N
    if (b(i) .ne. exp) STOP 44
  end do

  a(:) = 38.0

  nn = 0;

  !$acc kernels copyin (a(1:N)) copyout (b(1:N)) if ((nn + nn) > 0) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end kernels

  do i = 1, N
    if (b(i) .ne. 39.0) STOP 45
  end do

  a(:) = 91.0

  !$acc kernels copyin (a(1:N)) copyout (b(1:N)) if (-2 > 0) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end kernels

  do i = 1, N
    if (b(i) .ne. 92.0) STOP 46
  end do

  a(:) = 43.0

  !$acc kernels copyin (a(1:N)) copyout (b(1:N)) if (one == 1) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
       ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end kernels

#if ACC_MEM_SHARED
  exp = 44.0
#else
  exp = 43.0
#endif

  do i = 1, N
    if (b(i) .ne. exp) STOP 47
  end do

  a(:) = 87.0

  !$acc kernels if (one == 0) ! { dg-line l_compute[incr c_compute] }
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
    do i = 1, N
      ! { dg-note {variable 'C\.[0-9]+' declared in block potentially has improper OpenACC privatization level: 'const_decl'} "TODO" { target *-*-* } l_compute$c_compute }
      if (acc_on_device (acc_device_host) .eqv. .TRUE.) then
        b(i) = a(i) + 1
      else
        b(i) = a(i)
      end if
    end do
  !$acc end kernels

  do i = 1, N
    if (b(i) .ne. 88.0) STOP 48
  end do

  a(:) = 3.0
  b(:) = 9.0

#if ACC_MEM_SHARED
  exp = 0.0
  exp2 = 0.0
#else
  call acc_copyin (a, sizeof (a))
  call acc_copyin (b, sizeof (b))
  exp = 3.0;
  exp2 = 9.0;
#endif

  !$acc update device (a(1:N), b(1:N)) if (1 == 1)

  a(:) = 0.0
  b(:) = 0.0

  !$acc update host (a(1:N), b(1:N)) if (1 == 1)

  do i = 1, N
    if (a(i) .ne. exp) STOP 49
    if (b(i) .ne. exp2) STOP 50
  end do

  a(:) = 6.0
  b(:) = 12.0

  !$acc update device (a(1:N), b(1:N)) if (0 == 1)

  a(:) = 0.0
  b(:) = 0.0

  !$acc update host (a(1:N), b(1:N)) if (1 == 1)

  do i = 1, N
    if (a(i) .ne. exp) STOP 51
    if (b(i) .ne. exp2) STOP 52
  end do

  a(:) = 26.0
  b(:) = 21.0

  !$acc update device (a(1:N), b(1:N)) if (1 == 1)

  a(:) = 0.0
  b(:) = 0.0

  !$acc update host (a(1:N), b(1:N)) if (0 == 1)

  do i = 1, N
    if (a(i) .ne. 0.0) STOP 53
    if (b(i) .ne. 0.0) STOP 54
  end do

#if !ACC_MEM_SHARED
  call acc_copyout (a, sizeof (a))
  call acc_copyout (b, sizeof (b))
#endif

  a(:) = 4.0
  b(:) = 0.0

  !$acc data copyin (a(1:N)) copyout (b(1:N)) if (1 == 1)
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  ! { dg-note {variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }

    !$acc kernels present (a(1:N)) ! { dg-line l_compute[incr c_compute] }
    ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
       do i = 1, N
           b(i) = a(i)
       end do
    !$acc end kernels
  !$acc end data

  do i = 1, N
    if (b(i) .ne. 4.0) STOP 55
  end do

  a(:) = 8.0
  b(:) = 1.0

  !$acc data copyin (a(1:N)) copyout (b(1:N)) if (0 == 1)
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target { ! openacc_host_selected } } .-1 }
  ! { dg-note {variable 'parm\.[0-9]+' declared in block is candidate for adjusting OpenACC privatization level} "" { target { ! openacc_host_selected } } .-2 }

#if !ACC_MEM_SHARED
  if (acc_is_present (a) .eqv. .TRUE.) STOP 56
  if (acc_is_present (b) .eqv. .TRUE.) STOP 57
#endif

  !$acc end data

  a(:) = 18.0
  b(:) = 21.0

  !$acc data copyin (a(1:N)) if (1 == 1)
  ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
  ! { dg-note {variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }
  ! { dg-note {variable 'parm\.[0-9]+' declared in block is candidate for adjusting OpenACC privatization level} "" { target { ! openacc_host_selected } } .-3 }

#if !ACC_MEM_SHARED
    if (acc_is_present (a) .eqv. .FALSE.) STOP 58
#endif

    !$acc data copyout (b(1:N)) if (0 == 1)
    ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
    ! { dg-note {variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }
    ! { dg-note {variable 'parm\.[0-9]+' declared in block is candidate for adjusting OpenACC privatization level} "" { target { ! openacc_host_selected } } .-3 }
#if !ACC_MEM_SHARED
      if (acc_is_present (b) .eqv. .TRUE.) STOP 59
#endif
        !$acc data copyout (b(1:N)) if (1 == 1)
        ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-1 }
        ! { dg-note {variable 'parm\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } .-2 }

        !$acc kernels present (a(1:N)) present (b(1:N)) ! { dg-line l_compute[incr c_compute] }
        ! { dg-note {variable 'D\.[0-9]+' declared in block isn't candidate for adjusting OpenACC privatization level: not addressable} "" { target *-*-* } l_compute$c_compute }
          do i = 1, N
            b(i) = a(i)
          end do
      !$acc end kernels

    !$acc end data

#if !ACC_MEM_SHARED
    if (acc_is_present (b) .eqv. .TRUE.) STOP 60
#endif
    !$acc end data
  !$acc end data

  do i = 1, N
   if (b(1) .ne. 18.0) STOP 61
  end do

  !$acc enter data copyin (b(1:N)) if (0 == 1)

#if !ACC_MEM_SHARED
  if (acc_is_present (b) .eqv. .TRUE.) STOP 62
#endif

  !$acc exit data delete (b(1:N)) if (0 == 1)

  !$acc enter data copyin (b(1:N)) if (1 == 1)

#if !ACC_MEM_SHARED
    if (acc_is_present (b) .eqv. .FALSE.) STOP 63
#endif

  !$acc exit data delete (b(1:N)) if (1 == 1)

#if !ACC_MEM_SHARED
  if (acc_is_present (b) .eqv. .TRUE.) STOP 64
#endif

  !$acc enter data copyin (b(1:N)) if (zero == 1)

#if !ACC_MEM_SHARED
    if (acc_is_present (b) .eqv. .TRUE.) STOP 65
#endif

  !$acc exit data delete (b(1:N)) if (zero == 1)

  !$acc enter data copyin (b(1:N)) if (one == 1)

#if !ACC_MEM_SHARED
    if (acc_is_present (b) .eqv. .FALSE.) STOP 66
#endif

  !$acc exit data delete (b(1:N)) if (one == 1)

#if !ACC_MEM_SHARED
  if (acc_is_present (b) .eqv. .TRUE.) STOP 67
#endif

  !$acc enter data copyin (b(1:N)) if (one == 0)

#if !ACC_MEM_SHARED
    if (acc_is_present (b) .eqv. .TRUE.) STOP 68
#endif

  !$acc exit data delete (b(1:N)) if (one == 0)

  !$acc enter data copyin (b(1:N)) if (one == 1)

#if !ACC_MEM_SHARED
    if (acc_is_present (b) .eqv. .FALSE.) STOP 69
#endif

  !$acc exit data delete (b(1:N)) if (one == 1)

#if !ACC_MEM_SHARED
  if (acc_is_present (b) .eqv. .TRUE.) STOP 70
#endif

end program main
