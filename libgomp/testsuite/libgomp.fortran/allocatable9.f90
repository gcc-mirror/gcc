! { dg-do run }

  integer, allocatable :: a, b(:), c(:,:)
  logical :: l
  if (allocated (a) .or. allocated (b) .or. allocated (c)) STOP 1
!$omp parallel private (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) STOP 2
  allocate (a, b(-7:-1), c(2:3, 3:5))
  if (.not.allocated (a)) STOP 3
  if (.not.allocated (b) .or. size (b) /= 7) STOP 4
  if (lbound (b, 1) /= -7 .or. ubound (b, 1) /= -1) STOP 5
  if (.not.allocated (c) .or. size (c) /= 6) STOP 6
  if (size (c, 1) /= 2 .or. size (c, 2) /= 3) STOP 7
  if (lbound (c, 1) /= 2 .or. ubound (c, 1) /= 3) STOP 8
  if (lbound (c, 2) /= 3 .or. ubound (c, 2) /= 5) STOP 9
  a = 4
  b = 3
  c = 2
!$omp end parallel
  if (allocated (a) .or. allocated (b) .or. allocated (c)) STOP 10
!$omp parallel firstprivate (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) STOP 11
  allocate (a, b(-7:-1), c(2:3, 3:5))
  if (.not.allocated (a)) STOP 12
  if (.not.allocated (b) .or. size (b) /= 7) STOP 13
  if (lbound (b, 1) /= -7 .or. ubound (b, 1) /= -1) STOP 14
  if (.not.allocated (c) .or. size (c) /= 6) STOP 15
  if (size (c, 1) /= 2 .or. size (c, 2) /= 3) STOP 16
  if (lbound (c, 1) /= 2 .or. ubound (c, 1) /= 3) STOP 17
  if (lbound (c, 2) /= 3 .or. ubound (c, 2) /= 5) STOP 18
  a = 4
  b = 3
  c = 2
!$omp end parallel
  if (allocated (a) .or. allocated (b) .or. allocated (c)) STOP 19
  allocate (a, b(6:9), c(3, 8:9))
  a = 2
  b = 4
  c = 5
  if (.not.allocated (a)) STOP 20
  if (.not.allocated (b) .or. size (b) /= 4) STOP 21
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 22
  if (.not.allocated (c) .or. size (c) /= 6) STOP 23
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 24
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 25
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 26
!$omp parallel firstprivate (a, b, c)
  if (.not.allocated (a)) STOP 27
  if (.not.allocated (b) .or. size (b) /= 4) STOP 28
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 29
  if (.not.allocated (c) .or. size (c) /= 6) STOP 30
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 31
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 32
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 33
  if (a /= 2 .or. any (b .ne. 4) .or. any (c .ne. 5)) STOP 34
  deallocate (a)
  if (allocated (a)) STOP 35
  allocate (a)
  a = 8
  b = (/ 1, 2, 3 /)
  c = reshape ((/ 1, 2, 3, 4, 5, 6, 7, 8 /), (/ 2, 4 /))
  if (.not.allocated (a)) STOP 36
  if (.not.allocated (b) .or. size (b) /= 3) STOP 37
  if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 3) STOP 38
  if (.not.allocated (c) .or. size (c) /= 8) STOP 39
  if (size (c, 1) /= 2 .or. size (c, 2) /= 4) STOP 40
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 2) STOP 41
  if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 4) STOP 42
  if (a /= 8 .or. b(2) /= 2 .or. c(1, 2) /= 3) STOP 43
!$omp end parallel
  if (.not.allocated (a)) STOP 44
  if (.not.allocated (b) .or. size (b) /= 4) STOP 45
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 46
  if (.not.allocated (c) .or. size (c) /= 6) STOP 47
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 48
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 49
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 50
  if (a /= 2 .or. any (b .ne. 4) .or. any (c .ne. 5)) STOP 51
  l = .false.
!$omp parallel sections lastprivate (a, b, c) firstprivate (l)
!$omp section
  if (.not.allocated (a)) STOP 52
  if (l) then
    if (.not.allocated (b) .or. size (b) /= 6) STOP 53
    if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 6) STOP 54
    if (.not.allocated (c) .or. size (c) /= 8) STOP 55
    if (size (c, 1) /= 4 .or. size (c, 2) /= 2) STOP 56
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 4) STOP 57
    if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 2) STOP 58
    if (a /= 12 .or. b(2) /= 8 .or. c(1, 2) /= 5) STOP 59
  else
    if (.not.allocated (b) .or. size (b) /= 4) STOP 60
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 61
    if (.not.allocated (c) .or. size (c) /= 6) STOP 62
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 63
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 64
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 65
  end if
  l = .true.
  deallocate (a)
  if (allocated (a)) STOP 66
  allocate (a)
  a = 8
  b = (/ 1, 2, 3 /)
  c = reshape ((/ 1, 2, 3, 4, 5, 6, 7, 8 /), (/ 2, 4 /))
  if (.not.allocated (a)) STOP 67
  if (.not.allocated (b) .or. size (b) /= 3) STOP 68
  if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 3) STOP 69
  if (.not.allocated (c) .or. size (c) /= 8) STOP 70
  if (size (c, 1) /= 2 .or. size (c, 2) /= 4) STOP 71
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 2) STOP 72
  if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 4) STOP 73
  if (a /= 8 .or. b(2) /= 2 .or. c(1, 2) /= 3) STOP 74
!$omp section
  if (.not.allocated (a)) STOP 75
  if (l) then
    if (.not.allocated (b) .or. size (b) /= 3) STOP 76
    if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 3) STOP 77
    if (.not.allocated (c) .or. size (c) /= 8) STOP 78
    if (size (c, 1) /= 2 .or. size (c, 2) /= 4) STOP 79
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 2) STOP 80
    if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 4) STOP 81
    if (a /= 8 .or. b(2) /= 2 .or. c(1, 2) /= 3) STOP 82
  else
    if (.not.allocated (b) .or. size (b) /= 4) STOP 83
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) STOP 84
    if (.not.allocated (c) .or. size (c) /= 6) STOP 85
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) STOP 86
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) STOP 87
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) STOP 88
  end if
  l = .true.
  deallocate (a)
  if (allocated (a)) STOP 89
  allocate (a)
  a = 12
  b = (/ 9, 8, 7, 6, 5, 4 /)
  c = reshape ((/ 1, 2, 3, 4, 5, 6, 7, 8 /), (/ 4, 2 /))
  if (.not.allocated (a)) STOP 90
  if (.not.allocated (b) .or. size (b) /= 6) STOP 91
  if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 6) STOP 92
  if (.not.allocated (c) .or. size (c) /= 8) STOP 93
  if (size (c, 1) /= 4 .or. size (c, 2) /= 2) STOP 94
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 4) STOP 95
  if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 2) STOP 96
  if (a /= 12 .or. b(2) /= 8 .or. c(1, 2) /= 5) STOP 97
!$omp end parallel sections
  if (.not.allocated (a)) STOP 98
  if (.not.allocated (b) .or. size (b) /= 6) STOP 99
  if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 6) STOP 100
  if (.not.allocated (c) .or. size (c) /= 8) STOP 101
  if (size (c, 1) /= 4 .or. size (c, 2) /= 2) STOP 102
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 4) STOP 103
  if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 2) STOP 104
  if (a /= 12 .or. b(2) /= 8 .or. c(1, 2) /= 5) STOP 105
end
