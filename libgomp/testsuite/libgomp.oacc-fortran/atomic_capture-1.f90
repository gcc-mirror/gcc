! { dg-do run }

program main
  integer, parameter :: N = 32
  integer igot, iexp, itmp
  integer, dimension (0:N) :: iarr
  real fgot, fexp, ftmp
  real, dimension (0:N) :: farr
  logical lgot, lexp, ltmp

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

  if (igot /= iexp) STOP 1
  if (itmp /= iexp - 2) STOP 2

  fgot = 1234.0
  fexp = 1266.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      farr(i) = fgot
      fgot = fgot + 1.0
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. &
          (1234.0 <= farr(i) .and. farr(i) < fexp &
          .and. aint (farr(i)) == farr(i))) STOP 3
  end do
  if (fgot /= fexp) STOP 4

  fgot = 1.0
  fexp = 2.0**32

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      farr(i) = fgot
      fgot = fgot * 2.0
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. &
          (1.0 <= farr(i) .and. farr(i) < fexp &
          .and. aint (farr(i)) == farr(i))) STOP 5
  end do
  if (fgot /= fexp) STOP 6

  fgot = 32.0
  fexp = fgot - N

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      farr(i) = fgot
      fgot = fgot - 1.0
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. &
          (fexp < farr(i) .and. farr(i) <= 32.0 &
          .and. aint (farr(i)) == farr(i))) STOP 7
  end do
  if (fgot /= fexp) STOP 8

  fgot = 2**32.0
  fexp = 1.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      farr(i) = fgot
      fgot = fgot / 2.0
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. &
          (fexp < farr(i) .and. farr(i) <= 2**32.0 &
          .and. aint (farr(i)) == farr(i))) STOP 9
  end do
  if (fgot /= fexp) STOP 10

  lgot = .TRUE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = lgot .and. .FALSE.
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. .not. lexp) STOP 11
  if (lgot .neqv. lexp) STOP 12

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = lgot .or. .FALSE.
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) STOP 13
  if (lgot .neqv. lexp) STOP 14

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = lgot .eqv. .TRUE.
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) STOP 15
  if (lgot .neqv. lexp) STOP 16

  lgot = .FALSE.
  lexp = .TRUE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = lgot .neqv. .TRUE.
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. .not. lexp) STOP 17
  if (lgot .neqv. lexp) STOP 18

  fgot = 1234.0
  fexp = 1266.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      farr(i) = fgot
      fgot = 1.0 + fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. &
          (1234.0 <= farr(i) .and. farr(i) < fexp &
          .and. aint (farr(i)) == farr(i))) STOP 19
  end do
  if (fgot /= fexp) STOP 20

  fgot = 1.0
  fexp = 2.0**32

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      farr(i) = fgot
      fgot = 2.0 * fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. &
          (1.0 <= farr(i) .and. farr(i) < fexp &
          .and. aint (farr(i)) == farr(i))) STOP 21
  end do
  if (fgot /= fexp) STOP 22

  fgot = 32.0
  fexp = 32.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      farr(i) = fgot
      fgot = 2.0 - fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (farr(i) == fexp .or. farr(i) == -30.0)) STOP 23
  end do
  if (fgot /= fexp) STOP 24

  fgot = 2.0**16
  fexp = 2.0**16

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      farr(i) = fgot
      fgot = 2.0 / fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (farr(i) == fexp .or. farr(i) == 1.0 / 2.0**15)) STOP 25
  end do
  if (fgot /= fexp) STOP 26

  lgot = .TRUE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = .FALSE. .and. lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. .not. lexp) STOP 27
  if (lgot .neqv. lexp) STOP 28

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = .FALSE. .or. lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) STOP 29
  if (lgot .neqv. lexp) STOP 30

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = .TRUE. .eqv. lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) STOP 31
  if (lgot .neqv. lexp) STOP 32

  lgot = .FALSE.
  lexp = .TRUE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    ltmp = lgot
    lgot = .TRUE. .neqv. lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. .not. lexp) STOP 33
  if (lgot .neqv. lexp) STOP 34

  igot = 1
  iexp = N

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      iarr(i) = igot
      igot = max (igot, i)
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (1 <= iarr(i) .and. iarr(i) < iexp)) STOP 35
  end do
  if (igot /= iexp) STOP 36

  igot = N
  iexp = 1

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      iarr(i) = igot
      igot = min (igot, i)
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i) == 1 .or. iarr(i) == N)) STOP 37
  end do
  if (igot /= iexp) STOP 38

  igot = -1
  iexp = 0

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = ibclr (-2, i)
  !$acc atomic capture
      iarr(i) = igot
      igot = iand (igot, iexpr)
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i - 1) < 0)) STOP 39
  end do
  if (igot /= iexp) STOP 40

  igot = 0
  iexp = -1 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      iarr(i) = igot
      igot = ior (igot, iexpr)
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i - 1) >= 0)) STOP 41
  end do
  if (igot /= iexp) STOP 42

  igot = -1
  iexp = 0 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      iarr(i) = igot
      igot = ieor (igot, iexpr)
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i - 1) < 0)) STOP 43
  end do
  if (igot /= iexp) STOP 44

  igot = 1
  iexp = N

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      iarr(i) = igot
      igot = max (i, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (1 <= iarr(i) .and. iarr(i) < iexp)) STOP 45
  end do
  if (igot /= iexp) STOP 46

  igot = N
  iexp = 1

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      iarr(i) = igot
      igot = min (i, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i) == 1 .or. iarr(i) == N)) STOP 47
  end do
  if (igot /= iexp) STOP 48

  igot = -1
  iexp = 0

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = ibclr (-2, i)
  !$acc atomic capture
      iarr(i) = igot
      igot = iand (iexpr, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i - 1) < 0)) STOP 49
  end do
  if (igot /= iexp) STOP 50

  igot = 0
  iexp = -1 
	!!
  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      iarr(i) = igot
      igot = ior (iexpr, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i - 1) >= 0)) STOP 51
  end do
  if (igot /= iexp) STOP 52

  igot = -1
  iexp = 0 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      iarr(i) = igot
      igot = ieor (iexpr, igot)
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i - 1) < 0)) STOP 53
  end do
  if (igot /= iexp) STOP 54

  fgot = 1234.0
  fexp = 1266.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = fgot + 1.0
      farr(i) = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. &
          (1234.0 < farr(i) .and. farr(i) <= fexp &
          .and. aint (farr(i)) == farr(i))) STOP 55
  end do
  if (fgot /= fexp) STOP 56

  fgot = 1.0
  fexp = 2.0**32

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = fgot * 2.0
      farr(i) = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. &
          (1.0 < farr(i) .and. farr(i) <= fexp &
          .and. aint (farr(i)) == farr(i))) STOP 57
  end do
  if (fgot /= fexp) STOP 58

  fgot = 32.0
  fexp = fgot - N

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = fgot - 1.0
      farr(i) = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. &
          (fexp <= farr(i) .and. farr(i) < 32.0 &
          .and. aint (farr(i)) == farr(i))) STOP 59
  end do
  if (fgot /= fexp) STOP 60

  fgot = 2**32.0
  fexp = 1.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = fgot / 2.0
      farr(i) = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. &
          (fexp <= farr(i) .and. farr(i) < 2**32.0 &
          .and. aint (farr(i)) == farr(i))) STOP 61
  end do
  if (fgot /= fexp) STOP 62

  lgot = .TRUE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = lgot .and. .FALSE.
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) STOP 63
  if (lgot .neqv. lexp) STOP 64

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = lgot .or. .FALSE.
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) STOP 65
  if (lgot .neqv. lexp) STOP 66

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = lgot .eqv. .TRUE.
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) STOP 67
  if (lgot .neqv. lexp) STOP 68

  lgot = .FALSE.
  lexp = .TRUE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = lgot .neqv. .TRUE.
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) STOP 69
  if (lgot .neqv. lexp) STOP 70

  fgot = 1234.0
  fexp = 1266.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = 1.0 + fgot
      farr(i) = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. &
          (1234.0 < farr(i) .and. farr(i) <= fexp &
          .and. aint (farr(i)) == farr(i))) STOP 71
  end do
  if (fgot /= fexp) STOP 72

  fgot = 1.0
  fexp = 2.0**32

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = 2.0 * fgot
      farr(i) = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. &
          (1.0 < farr(i) .and. farr(i) <= 2**32.0 &
          .and. aint (farr(i)) == farr(i))) STOP 73
  end do
  if (fgot /= fexp) STOP 74

  fgot = 32.0
  fexp = 32.0

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = 2.0 - fgot
      farr(i) = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (farr(i) == fexp .or. farr(i) == 2.0 - fexp)) STOP 75
  end do
  if (fgot /= fexp) STOP 76

  fgot = 2.0**16
  fexp = 2.0**16

  !$acc parallel loop copy (fgot, ftmp)
    do i = 1, N
  !$acc atomic capture
      fgot = 2.0 / fgot
      farr(i) = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (farr(i) == fexp .or. farr(i) == 2.0 / fexp)) STOP 77
  end do
  if (fgot /= fexp) STOP 78

  lgot = .TRUE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = .FALSE. .and. lgot
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) STOP 79
  if (lgot .neqv. lexp) STOP 80

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = .FALSE. .or. lgot
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) STOP 81
  if (lgot .neqv. lexp) STOP 82

  lgot = .FALSE.
  lexp = .FALSE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = .TRUE. .eqv. lgot
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) STOP 83
  if (lgot .neqv. lexp) STOP 84

  lgot = .FALSE.
  lexp = .TRUE.

  !$acc parallel copy (lgot, ltmp)
  !$acc atomic capture
    lgot = .TRUE. .neqv. lgot
    ltmp = lgot
  !$acc end atomic
  !$acc end parallel

  if (ltmp .neqv. lexp) STOP 85
  if (lgot .neqv. lexp) STOP 86

  igot = 1
  iexp = N

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      igot = max (igot, i)
      iarr(i) = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (1 <= iarr(i) .and. iarr(i) <= N)) STOP 87
  end do
  if (igot /= iexp) STOP 88

  igot = N
  iexp = 1

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      igot = min (igot, i)
      iarr(i) = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i) == iexp)) STOP 89
  end do
  if (igot /= iexp) STOP 90

  igot = -1
  iexp = 0

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = ibclr (-2, i)
  !$acc atomic capture
      igot = iand (igot, iexpr)
      iarr(i) = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i - 1) <= 0)) STOP 91
  end do
  if (igot /= iexp) STOP 92

  igot = 0
  iexp = -1 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      igot = ior (igot, iexpr)
      iarr(i) = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i - 1) >= -1)) STOP 93
  end do
  if (igot /= iexp) STOP 94

  igot = -1
  iexp = 0 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      igot = ieor (igot, iexpr)
      iarr(i) = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i - 1) <= 0)) STOP 95
  end do
  if (igot /= iexp) STOP 96

  igot = 1
  iexp = N

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      igot = max (i, igot)
      iarr(i) = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (1 <= iarr(i) .and. iarr(i) <= iexp)) STOP 97
  end do
  if (igot /= iexp) STOP 98

  igot = N
  iexp = 1

  !$acc parallel loop copy (igot, itmp)
    do i = 1, N
  !$acc atomic capture
      igot = min (i, igot)
      iarr(i) = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i) == iexp )) STOP 99
  end do
  if (igot /= iexp) STOP 100

  igot = -1
  iexp = 0

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = ibclr (-2, i)
  !$acc atomic capture
      igot = iand (iexpr, igot)
      iarr(i) = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i - 1) <= 0)) STOP 101
  end do
  if (igot /= iexp) STOP 102

  igot = 0
  iexp = -1 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      igot = ior (iexpr, igot)
      iarr(i) = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i - 1) >= iexp)) STOP 103
  end do
  if (igot /= iexp) STOP 104

  igot = -1
  iexp = 0 

  !$acc parallel loop copy (igot, itmp)
    do i = 0, N - 1
      iexpr = lshift (1, i)
  !$acc atomic capture
      igot = ieor (iexpr, igot)
      iarr(i) = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  do i = 1, N
     if (.not. (iarr(i - 1) <= iexp)) STOP 105
  end do
  if (igot /= iexp) STOP 106

end program
