! { dg-do run }

program main
  integer igot, iexp, itmp
  real fgot, fexp, ftmp
  logical lgot, lexp, ltmp
  integer, parameter :: N = 32

  igot = 0
  iexp = N * 2

  !$acc parallel copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      itmp = igot
      igot = i + i
  !$acc end atomic
    end do
  !$acc end parallel

  if (igot /= iexp) call abort
  if (itmp /= iexp - 2) call abort

  fgot = 1234.0
  fexp = 1266.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      ftmp = fgot
      fgot = fgot + 1.0
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp - 1.0) call abort
  if (fgot /= fexp) call abort

  fgot = 1.0
  fexp = 2.0**32

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      ftmp = fgot
      fgot = fgot * 2.0
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp / 2.0) call abort
  if (fgot /= fexp) call abort

  fgot = 32.0
  fexp = fgot - N

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      ftmp = fgot
      fgot = fgot - 1.0
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp + 1.0) call abort
  if (fgot /= fexp) call abort

  fgot = 2**32.0
  fexp = 1.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      ftmp = fgot
      fgot = fgot / 2.0
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fgot * 2.0) call abort
  if (fgot /= fexp) call abort

  lgot = .TRUE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = lgot .and. .FALSE.
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. .not. lexp) call abort
  if (lgot .neqv. lexp) call abort

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = lgot .or. .FALSE.
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) call abort
  if (lgot .neqv. lexp) call abort

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = lgot .eqv. .TRUE.
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) call abort
  if (lgot .neqv. lexp) call abort

  lgot = .FALSE.
  lexp = .TRUE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = lgot .neqv. .TRUE.
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. .not. lexp) call abort
  if (lgot .neqv. lexp) call abort

  fgot = 1234.0
  fexp = 1266.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      ftmp = fgot
      fgot = 1.0 + fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp - 1.0) call abort 
  if (fgot /= fexp) call abort

  fgot = 1.0
  fexp = 2.0**32

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      ftmp = fgot
      fgot = 2.0 * fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp / 2.0) call abort
  if (fgot /= fexp) call abort

  fgot = 32.0
  fexp = 32.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      ftmp = fgot
      fgot = 2.0 - fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= 2.0 - fexp) call abort
  if (fgot /= fexp) call abort

  fgot = 2.0**16
  fexp = 2.0**16

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      ftmp = fgot
      fgot = 2.0 / fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= 2.0 / fexp) call abort
  if (fgot /= fexp) call abort

  lgot = .TRUE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = .FALSE. .and. lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. .not. lexp) call abort
  if (lgot .neqv. lexp) call abort

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = .FALSE. .or. lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) call abort
  if (lgot .neqv. lexp) call abort

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = .TRUE. .eqv. lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) call abort
  if (lgot .neqv. lexp) call abort

  lgot = .FALSE.
  lexp = .TRUE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = .TRUE. .neqv. lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. .not. lexp) call abort
  if (lgot .neqv. lexp) call abort

  igot = 1
  iexp = N

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      itmp = igot
      igot = max (igot, i)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp - 1) call abort
  if (igot /= iexp) call abort

  igot = N
  iexp = 1

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      itmp = igot
      igot = min (igot, i)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp) call abort
  if (igot /= iexp) call abort

  igot = -1
  iexp = 0

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = ibclr (-2, i)
  !$acc atomic capture
      itmp = igot
      igot = iand (igot, iexpr)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= ibset (iexp, N - 1)) call abort
  if (igot /= iexp) call abort

  igot = 0
  iexp = -1 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      itmp = igot
      igot = ior (igot, iexpr)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= ieor (iexp, lshift (1, N - 1))) call abort
  if (igot /= iexp) call abort

  igot = -1
  iexp = 0 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      itmp = igot
      igot = ieor (igot, iexpr)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= ior (iexp, lshift (1, N - 1))) call abort
  if (igot /= iexp) call abort

  igot = 1
  iexp = N

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      itmp = igot
      igot = max (i, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp - 1) call abort
  if (igot /= iexp) call abort

  igot = N
  iexp = 1

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      itmp = igot
      igot = min (i, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp) call abort
  if (igot /= iexp) call abort

  igot = -1
  iexp = 0

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = ibclr (-2, i)
  !$acc atomic capture
      itmp = igot
      igot = iand (iexpr, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= ibset (iexp, N - 1)) call abort
  if (igot /= iexp) call abort

  igot = 0
  iexp = -1 
	!!
  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      itmp = igot
      igot = ior (iexpr, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= ieor (iexp, lshift (1, N - 1))) call abort
  if (igot /= iexp) call abort

  igot = -1
  iexp = 0 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      itmp = igot
      igot = ieor (iexpr, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= ior (iexp, lshift (1, N - 1))) call abort
  if (igot /= iexp) call abort

  fgot = 1234.0
  fexp = 1266.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = fgot + 1.0
      ftmp = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp) call abort
  if (fgot /= fexp) call abort

  fgot = 1.0
  fexp = 2.0**32

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = fgot * 2.0
      ftmp = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp) call abort
  if (fgot /= fexp) call abort

  fgot = 32.0
  fexp = fgot - N

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = fgot - 1.0
      ftmp = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp) call abort
  if (fgot /= fexp) call abort

  fgot = 2**32.0
  fexp = 1.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = fgot / 2.0
      ftmp = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp) call abort
  if (fgot /= fexp) call abort

  lgot = .TRUE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = lgot .and. .FALSE.
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) call abort
  if (lgot .neqv. lexp) call abort

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = lgot .or. .FALSE.
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) call abort
  if (lgot .neqv. lexp) call abort

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = lgot .eqv. .TRUE.
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) call abort
  if (lgot .neqv. lexp) call abort

  lgot = .FALSE.
  lexp = .TRUE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = lgot .neqv. .TRUE.
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) call abort
  if (lgot .neqv. lexp) call abort

  fgot = 1234.0
  fexp = 1266.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = 1.0 + fgot
      ftmp = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp) call abort
  if (fgot /= fexp) call abort

  fgot = 1.0
  fexp = 2.0**32

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = 2.0 * fgot
      ftmp = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp) call abort
  if (fgot /= fexp) call abort

  fgot = 32.0
  fexp = 32.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = 2.0 - fgot
      ftmp = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp) call abort
  if (fgot /= fexp) call abort

  fgot = 2.0**16
  fexp = 2.0**16

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = 2.0 / fgot
      ftmp = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp) call abort
  if (fgot /= fexp) call abort

  lgot = .TRUE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = .FALSE. .and. lgot
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) call abort
  if (lgot .neqv. lexp) call abort

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = .FALSE. .or. lgot
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) call abort
  if (lgot .neqv. lexp) call abort

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = .TRUE. .eqv. lgot
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) call abort
  if (lgot .neqv. lexp) call abort

  lgot = .FALSE.
  lexp = .TRUE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = .TRUE. .neqv. lgot
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) call abort
  if (lgot .neqv. lexp) call abort

  igot = 1
  iexp = N

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      igot = max (igot, i)
      itmp = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp) call abort
  if (igot /= iexp) call abort

  igot = N
  iexp = 1

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      igot = min (igot, i)
      itmp = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp) call abort
  if (igot /= iexp) call abort

  igot = -1
  iexp = 0

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = ibclr (-2, i)
  !$acc atomic capture
      igot = iand (igot, iexpr)
      itmp = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp) call abort
  if (igot /= iexp) call abort

  igot = 0
  iexp = -1 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      igot = ior (igot, iexpr)
      itmp = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp) call abort
  if (igot /= iexp) call abort

  igot = -1
  iexp = 0 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      igot = ieor (igot, iexpr)
      itmp = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp) call abort
  if (igot /= iexp) call abort

  igot = 1
  iexp = N

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      igot = max (i, igot)
      itmp = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp) call abort
  if (igot /= iexp) call abort

  igot = N
  iexp = 1

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      igot = min (i, igot)
      itmp = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp) call abort
  if (igot /= iexp) call abort

  igot = -1
  iexp = 0

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = ibclr (-2, i)
  !$acc atomic capture
      igot = iand (iexpr, igot)
      itmp = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp) call abort
  if (igot /= iexp) call abort

  igot = 0
  iexp = -1 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      igot = ior (iexpr, igot)
      itmp = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp) call abort
  if (igot /= iexp) call abort

  igot = -1
  iexp = 0 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      igot = ieor (iexpr, igot)
      itmp = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp) call abort
  if (igot /= iexp) call abort

end program
