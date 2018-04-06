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

  if (igot /= iexp) STOP 1
  if (itmp /= iexp - 2) STOP 2

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

  if (ftmp /= fexp - 1.0) STOP 3
  if (fgot /= fexp) STOP 4

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

  if (ftmp /= fexp / 2.0) STOP 5
  if (fgot /= fexp) STOP 6

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

  if (ftmp /= fexp + 1.0) STOP 7
  if (fgot /= fexp) STOP 8

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

  if (ftmp /= fgot * 2.0) STOP 9
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
      ftmp = fgot
      fgot = 1.0 + fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp - 1.0) STOP 19 
  if (fgot /= fexp) STOP 20

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

  if (ftmp /= fexp / 2.0) STOP 21
  if (fgot /= fexp) STOP 22

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

  if (ftmp /= 2.0 - fexp) STOP 23
  if (fgot /= fexp) STOP 24

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

  if (ftmp /= 2.0 / fexp) STOP 25
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
      itmp = igot
      igot = max (igot, i)
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp - 1) STOP 35
  if (igot /= iexp) STOP 36

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

  if (itmp /= iexp) STOP 37
  if (igot /= iexp) STOP 38

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

  if (itmp /= ibset (iexp, N - 1)) STOP 39
  if (igot /= iexp) STOP 40

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

  if (itmp /= ieor (iexp, lshift (1, N - 1))) STOP 41
  if (igot /= iexp) STOP 42

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

  if (itmp /= ior (iexp, lshift (1, N - 1))) STOP 43
  if (igot /= iexp) STOP 44

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

  if (itmp /= iexp - 1) STOP 45
  if (igot /= iexp) STOP 46

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

  if (itmp /= iexp) STOP 47
  if (igot /= iexp) STOP 48

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

  if (itmp /= ibset (iexp, N - 1)) STOP 49
  if (igot /= iexp) STOP 50

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

  if (itmp /= ieor (iexp, lshift (1, N - 1))) STOP 51
  if (igot /= iexp) STOP 52

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

  if (itmp /= ior (iexp, lshift (1, N - 1))) STOP 53
  if (igot /= iexp) STOP 54

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

  if (ftmp /= fexp) STOP 55
  if (fgot /= fexp) STOP 56

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

  if (ftmp /= fexp) STOP 57
  if (fgot /= fexp) STOP 58

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

  if (ftmp /= fexp) STOP 59
  if (fgot /= fexp) STOP 60

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

  if (ftmp /= fexp) STOP 61
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
      ftmp = fgot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (ftmp /= fexp) STOP 71
  if (fgot /= fexp) STOP 72

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

  if (ftmp /= fexp) STOP 73
  if (fgot /= fexp) STOP 74

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

  if (ftmp /= fexp) STOP 75
  if (fgot /= fexp) STOP 76

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

  if (ftmp /= fexp) STOP 77
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
      itmp = igot
  !$acc end atomic
    end do
  !$acc end parallel loop

  if (itmp /= iexp) STOP 87
  if (igot /= iexp) STOP 88

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

  if (itmp /= iexp) STOP 89
  if (igot /= iexp) STOP 90

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

  if (itmp /= iexp) STOP 91
  if (igot /= iexp) STOP 92

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

  if (itmp /= iexp) STOP 93
  if (igot /= iexp) STOP 94

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

  if (itmp /= iexp) STOP 95
  if (igot /= iexp) STOP 96

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

  if (itmp /= iexp) STOP 97
  if (igot /= iexp) STOP 98

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

  if (itmp /= iexp) STOP 99
  if (igot /= iexp) STOP 100

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

  if (itmp /= iexp) STOP 101
  if (igot /= iexp) STOP 102

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

  if (itmp /= iexp) STOP 103
  if (igot /= iexp) STOP 104

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

  if (itmp /= iexp) STOP 105
  if (igot /= iexp) STOP 106

end program
