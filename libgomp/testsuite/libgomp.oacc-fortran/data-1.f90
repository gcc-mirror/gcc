! { dg-do run }
! { dg-additional-options "-cpp" }

function is_mapped (n) result (rc)
  use openacc

  integer, intent (in) :: n
  logical rc

#if ACC_MEM_SHARED
  integer i

  rc = .TRUE.
  i = n
#else
  rc = acc_is_present (n, sizeof (n))
#endif

end function is_mapped

program main
  integer i, j
  logical is_mapped

  i = -1
  j = -2

  !$acc data copyin (i, j)
    if (is_mapped (i) .eqv. .FALSE.) call abort
    if (is_mapped (j) .eqv. .FALSE.) call abort

    if (i .ne. -1 .or. j .ne. -2) call abort

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) call abort
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) call abort

  i = -1
  j = -2

  !$acc data copyout (i, j)
    if (is_mapped (i) .eqv. .FALSE.) call abort
    if (is_mapped (j) .eqv. .FALSE.) call abort

    if (i .ne. -1 .or. j .ne. -2) call abort

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) call abort

    !$acc parallel present (i, j)
      i = 4
      j = 2
    !$acc end parallel
  !$acc end data

  if (i .ne. 4 .or. j .ne. 2) call abort

  i = -1
  j = -2

  !$acc data create (i, j)
    if (is_mapped (i) .eqv. .FALSE.) call abort
    if (is_mapped (j) .eqv. .FALSE.) call abort

    if (i .ne. -1 .or. j .ne. -2) call abort

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) call abort
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) call abort

  i = -1
  j = -2

  !$acc data present_or_copyin (i, j)
    if (is_mapped (i) .eqv. .FALSE.) call abort
    if (is_mapped (j) .eqv. .FALSE.) call abort

    if (i .ne. -1 .or. j .ne. -2) call abort

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) call abort
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) call abort

  i = -1
  j = -2

  !$acc data present_or_copyout (i, j)
    if (is_mapped (i) .eqv. .FALSE.) call abort
    if (is_mapped (j) .eqv. .FALSE.) call abort

    if (i .ne. -1 .or. j .ne. -2) call abort

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) call abort

    !$acc parallel present (i, j)
      i = 4
      j = 2
    !$acc end parallel
  !$acc end data

  if (i .ne. 4 .or. j .ne. 2) call abort

  i = -1
  j = -2

  !$acc data present_or_copy (i, j)
    if (is_mapped (i) .eqv. .FALSE.) call abort
    if (is_mapped (j) .eqv. .FALSE.) call abort

    if (i .ne. -1 .or. j .ne. -2) call abort

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) call abort
  !$acc end data

#if ACC_MEM_SHARED
  if (i .ne. 2 .or. j .ne. 1) call abort
#else
  if (i .ne. -1 .or. j .ne. -2) call abort
#endif

  i = -1
  j = -2

  !$acc data present_or_create (i, j)
    if (is_mapped (i) .eqv. .FALSE.) call abort
    if (is_mapped (j) .eqv. .FALSE.) call abort

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) call abort
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) call abort

  i = -1
  j = -2

  !$acc data copyin (i, j)
    !$acc data present (i, j)
      if (is_mapped (i) .eqv. .FALSE.) call abort
      if (is_mapped (j) .eqv. .FALSE.) call abort

      if (i .ne. -1 .or. j .ne. -2) call abort

      i = 2
      j = 1

      if (i .ne. 2 .or. j .ne. 1) call abort
    !$acc end data
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) call abort

  i = -1
  j = -2

  !$acc data copyin (i, j)
    !$acc data present (i, j)
      if (is_mapped (i) .eqv. .FALSE.) call abort
      if (is_mapped (j) .eqv. .FALSE.) call abort

      if (i .ne. -1 .or. j .ne. -2) call abort

      i = 2
      j = 1

      if (i .ne. 2 .or. j .ne. 1) call abort
    !$acc end data
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) call abort

  i = -1
  j = -2

  !$acc data
#if !ACC_MEM_SHARED
    if (is_mapped (i) .eqv. .TRUE.) call abort
    if (is_mapped (j) .eqv. .TRUE.) call abort
#endif
    if (i .ne. -1 .or. j .ne. -2) call abort

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) call abort
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) call abort

end program main
