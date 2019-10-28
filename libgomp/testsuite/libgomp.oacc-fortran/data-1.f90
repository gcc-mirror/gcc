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
    if (is_mapped (i) .eqv. .FALSE.) stop 1
    if (is_mapped (j) .eqv. .FALSE.) stop 2

    if (i .ne. -1 .or. j .ne. -2) stop 3

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) stop 4
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) stop 5

  i = -1
  j = -2

  !$acc data copyout (i, j)
    if (is_mapped (i) .eqv. .FALSE.) stop 6
    if (is_mapped (j) .eqv. .FALSE.) stop 7

    if (i .ne. -1 .or. j .ne. -2) stop 8

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) stop 9

    !$acc parallel present (i, j)
      i = 4
      j = 2
    !$acc end parallel
  !$acc end data

  if (i .ne. 4 .or. j .ne. 2) stop 10

  i = -1
  j = -2

  !$acc data create (i, j)
    if (is_mapped (i) .eqv. .FALSE.) stop 11
    if (is_mapped (j) .eqv. .FALSE.) stop 12

    if (i .ne. -1 .or. j .ne. -2) stop 13

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) stop 14
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) stop 15

  i = -1
  j = -2

  !$acc data present_or_copyin (i, j)
    if (is_mapped (i) .eqv. .FALSE.) stop 16
    if (is_mapped (j) .eqv. .FALSE.) stop 17

    if (i .ne. -1 .or. j .ne. -2) stop 18

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) stop 19
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) stop 20

  i = -1
  j = -2

  !$acc data present_or_copyout (i, j)
    if (is_mapped (i) .eqv. .FALSE.) stop 21
    if (is_mapped (j) .eqv. .FALSE.) stop 22

    if (i .ne. -1 .or. j .ne. -2) stop 23

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) stop 24

    !$acc parallel present (i, j)
      i = 4
      j = 2
    !$acc end parallel
  !$acc end data

  if (i .ne. 4 .or. j .ne. 2) stop 25

  i = -1
  j = -2

  !$acc data present_or_copy (i, j)
    if (is_mapped (i) .eqv. .FALSE.) stop 26
    if (is_mapped (j) .eqv. .FALSE.) stop 27

    if (i .ne. -1 .or. j .ne. -2) stop 28

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) stop 29
  !$acc end data

#if ACC_MEM_SHARED
  if (i .ne. 2 .or. j .ne. 1) stop 30
#else
  if (i .ne. -1 .or. j .ne. -2) stop 31
#endif

  i = -1
  j = -2

  !$acc data present_or_create (i, j)
    if (is_mapped (i) .eqv. .FALSE.) stop 32
    if (is_mapped (j) .eqv. .FALSE.) stop 33

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) stop 34
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) stop 35

  i = -1
  j = -2

  !$acc data copyin (i, j)
    !$acc data present (i, j)
      if (is_mapped (i) .eqv. .FALSE.) stop 36
      if (is_mapped (j) .eqv. .FALSE.) stop 37

      if (i .ne. -1 .or. j .ne. -2) stop 38

      i = 2
      j = 1

      if (i .ne. 2 .or. j .ne. 1) stop 39
    !$acc end data
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) stop 40

  i = -1
  j = -2

  !$acc data copyin (i, j)
    !$acc data present (i, j)
      if (is_mapped (i) .eqv. .FALSE.) stop 41
      if (is_mapped (j) .eqv. .FALSE.) stop 42

      if (i .ne. -1 .or. j .ne. -2) stop 43

      i = 2
      j = 1

      if (i .ne. 2 .or. j .ne. 1) stop 44
    !$acc end data
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) stop 45

  i = -1
  j = -2

  !$acc data
#if !ACC_MEM_SHARED
    if (is_mapped (i) .eqv. .TRUE.) stop 46
    if (is_mapped (j) .eqv. .TRUE.) stop 47
#endif
    if (i .ne. -1 .or. j .ne. -2) stop 48

    i = 2
    j = 1

    if (i .ne. 2 .or. j .ne. 1) stop 49
  !$acc end data

  if (i .ne. 2 .or. j .ne. 1) stop 50

end program main
