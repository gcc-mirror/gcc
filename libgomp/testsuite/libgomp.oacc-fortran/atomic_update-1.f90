! { dg-do run }

program main
  integer igot, iexp, iexpr
  real fgot, fexp
  integer i
  integer, parameter :: N = 32
  logical lgot, lexp

  fgot = 1234.0
  fexp = 1266.0

  !$acc parallel loop copy (fgot)
    do i = 1, N
  !$acc atomic update
      fgot = fgot + 1.0
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (fgot /= fexp) STOP 1

  fgot = 1.0
  fexp = 2.0**32

  !$acc parallel loop copy (fgot)
    do i = 1, N
  !$acc atomic update
      fgot = fgot * 2.0
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (fgot /= fexp) STOP 2

  fgot = 32.0
  fexp = fgot - N

  !$acc parallel loop copy (fgot)
    do i = 1, N
  !$acc atomic update
      fgot = fgot - 1.0
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (fgot /= fexp) STOP 3

  fgot = 2**32.0
  fexp = 1.0

  !$acc parallel loop copy (fgot)
    do i = 1, N
  !$acc atomic update
      fgot = fgot / 2.0
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (fgot /= fexp) STOP 4

  lgot = .TRUE.
  lexp = .FALSE.

  !$acc parallel copy (lgot)
  !$acc atomic update
    lgot = lgot .and. .FALSE.
  !$acc end atomic
  !$acc end parallel

  if (lgot .neqv. lexp) STOP 5

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot)
  !$acc atomic update
    lgot = lgot .or. .FALSE.
  !$acc end atomic
  !$acc end parallel

  if (lgot .neqv. lexp) STOP 6

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot)
  !$acc atomic update
    lgot = lgot .eqv. .TRUE.
  !$acc end atomic
  !$acc end parallel

  if (lgot .neqv. lexp) STOP 7

  lgot = .FALSE.
  lexp = .TRUE.

  !$acc parallel copy (lgot)
  !$acc atomic update
    lgot = lgot .neqv. .TRUE.
  !$acc end atomic
  !$acc end parallel

  if (lgot .neqv. lexp) STOP 8

  fgot = 1234.0
  fexp = 1266.0

  !$acc parallel loop copy (fgot)
    do i = 1, N
  !$acc atomic update
      fgot = 1.0 + fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (fgot /= fexp) STOP 9

  fgot = 1.0
  fexp = 2.0**32

  !$acc parallel loop copy (fgot)
    do i = 1, N
  !$acc atomic update
      fgot = 2.0 * fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (fgot /= fexp) STOP 10

  fgot = 32.0
  fexp = 32.0

  !$acc parallel loop copy (fgot)
    do i = 1, N
  !$acc atomic update
      fgot = 2.0 - fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (fgot /= fexp) STOP 11

  fgot = 2.0**16
  fexp = 2.0**16

  !$acc parallel loop copy (fgot)
    do i = 1, N
  !$acc atomic update
      fgot = 2.0 / fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (fgot /= fexp) STOP 12

  lgot = .TRUE.
  lexp = .FALSE.

  !$acc parallel copy (lgot)
  !$acc atomic update
    lgot = .FALSE. .and. lgot
  !$acc end atomic
  !$acc end parallel

  if (lgot .neqv. lexp) STOP 13

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot)
  !$acc atomic update
    lgot = .FALSE. .or. lgot
  !$acc end atomic
  !$acc end parallel

  if (lgot .neqv. lexp) STOP 14

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot)
  !$acc atomic update
    lgot = .TRUE. .eqv. lgot
  !$acc end atomic
  !$acc end parallel

  if (lgot .neqv. lexp) STOP 15

  lgot = .FALSE.
  lexp = .TRUE.

  !$acc parallel copy (lgot)
  !$acc atomic update
    lgot = .TRUE. .neqv. lgot
  !$acc end atomic
  !$acc end parallel

  if (lgot .neqv. lexp) STOP 16

  igot = 1
  iexp = N

  !$acc parallel loop copy (igot)
    do i = 1, N
  !$acc atomic update
      igot = max (igot, i)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (igot /= iexp) STOP 17

  igot = N
  iexp = 1

  !$acc parallel loop copy (igot)
    do i = 1, N
  !$acc atomic update
      igot = min (igot, i)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (igot /= iexp) STOP 18

  igot = -1
  iexp = 0

  !$acc parallel loop copy (igot)
    do i = 0, N - 1
      iexpr = ibclr (-2, i)
  !$acc atomic update
      igot = iand (igot, iexpr)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (igot /= iexp) STOP 19

  igot = 0
  iexp = -1 

  !$acc parallel loop copy (igot)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic update
      igot = ior (igot, iexpr)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (igot /= iexp) STOP 20

  igot = -1
  iexp = 0 

  !$acc parallel loop copy (igot)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic update
      igot = ieor (igot, iexpr)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (igot /= iexp) STOP 21

  igot = 1
  iexp = N

  !$acc parallel loop copy (igot)
    do i = 1, N
  !$acc atomic update
      igot = max (i, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (igot /= iexp) STOP 22

  igot = N
  iexp = 1

  !$acc parallel loop copy (igot)
    do i = 1, N
  !$acc atomic update
      igot = min (i, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (igot /= iexp) STOP 23

  igot = -1
  iexp = 0

  !$acc parallel loop copy (igot)
    do i = 0, N - 1
      iexpr = ibclr (-2, i)
  !$acc atomic update
      igot = iand (iexpr, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (igot /= iexp) STOP 24

  igot = 0
  iexp = -1 

  !$acc parallel loop copy (igot)
    do i = 0, N - 1
        iexpr = lshift (1, i)
  !$acc atomic update
      igot = ior (iexpr, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (igot /= iexp) STOP 25

  igot = -1
  iexp = 0 

  !$acc parallel loop copy (igot)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic update
      igot = ieor (iexpr, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (igot /= iexp) STOP 26

end program
