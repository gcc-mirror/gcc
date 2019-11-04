! { dg-do run }

  integer, allocatable :: a, b(:), c(:,:)
  logical :: l
  if (allocated (a) .or. allocated (b) .or. allocated (c)) stop 1
!$omp parallel private (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) stop 2
  allocate (a, b(-7:-1), c(2:3, 3:5))
  if (.not.allocated (a)) stop 3
  if (.not.allocated (b) .or. size (b) /= 7) stop 4
  if (lbound (b, 1) /= -7 .or. ubound (b, 1) /= -1) stop 5
  if (.not.allocated (c) .or. size (c) /= 6) stop 6
  if (size (c, 1) /= 2 .or. size (c, 2) /= 3) stop 7
  if (lbound (c, 1) /= 2 .or. ubound (c, 1) /= 3) stop 8
  if (lbound (c, 2) /= 3 .or. ubound (c, 2) /= 5) stop 9
  a = 4
  b = 3
  c = 2
!$omp end parallel
  if (allocated (a) .or. allocated (b) .or. allocated (c)) stop 10
!$omp parallel firstprivate (a, b, c)
  if (allocated (a) .or. allocated (b) .or. allocated (c)) stop 11
  allocate (a, b(-7:-1), c(2:3, 3:5))
  if (.not.allocated (a)) stop 12
  if (.not.allocated (b) .or. size (b) /= 7) stop 13
  if (lbound (b, 1) /= -7 .or. ubound (b, 1) /= -1) stop 14
  if (.not.allocated (c) .or. size (c) /= 6) stop 15
  if (size (c, 1) /= 2 .or. size (c, 2) /= 3) stop 16
  if (lbound (c, 1) /= 2 .or. ubound (c, 1) /= 3) stop 17
  if (lbound (c, 2) /= 3 .or. ubound (c, 2) /= 5) stop 18
  a = 4
  b = 3
  c = 2
!$omp end parallel
  if (allocated (a) .or. allocated (b) .or. allocated (c)) stop 19
  allocate (a, b(6:9), c(3, 8:9))
  a = 2
  b = 4
  c = 5
  if (.not.allocated (a)) stop 20
  if (.not.allocated (b) .or. size (b) /= 4) stop 21
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 22
  if (.not.allocated (c) .or. size (c) /= 6) stop 23
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 24
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 25
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 26
!$omp parallel firstprivate (a, b, c)
  if (.not.allocated (a)) stop 27
  if (.not.allocated (b) .or. size (b) /= 4) stop 28
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 29
  if (.not.allocated (c) .or. size (c) /= 6) stop 30
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 31
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 32
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 33
  if (a /= 2 .or. any (b .ne. 4) .or. any (c .ne. 5)) stop 34
  deallocate (a)
  if (allocated (a)) stop 35
  allocate (a)
  a = 8
  b = (/ 1, 2, 3 /)
  c = reshape ((/ 1, 2, 3, 4, 5, 6, 7, 8 /), (/ 2, 4 /))
  if (.not.allocated (a)) stop 36
  if (.not.allocated (b) .or. size (b) /= 3) stop 37
  if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 3) stop 38
  if (.not.allocated (c) .or. size (c) /= 8) stop 39
  if (size (c, 1) /= 2 .or. size (c, 2) /= 4) stop 40
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 2) stop 41
  if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 4) stop 42
  if (a /= 8 .or. b(2) /= 2 .or. c(1, 2) /= 3) stop 43
!$omp end parallel
  if (.not.allocated (a)) stop 44
  if (.not.allocated (b) .or. size (b) /= 4) stop 45
  if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 46
  if (.not.allocated (c) .or. size (c) /= 6) stop 47
  if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 48
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 49
  if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 50
  if (a /= 2 .or. any (b .ne. 4) .or. any (c .ne. 5)) stop 51
  l = .false.
!$omp parallel sections lastprivate (a, b, c) firstprivate (l)
!$omp section
  if (.not.allocated (a)) stop 52
  if (l) then
    if (.not.allocated (b) .or. size (b) /= 6) stop 53
    if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 6) stop 54
    if (.not.allocated (c) .or. size (c) /= 8) stop 55
    if (size (c, 1) /= 4 .or. size (c, 2) /= 2) stop 56
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 4) stop 57
    if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 2) stop 58
    if (a /= 12 .or. b(2) /= 8 .or. c(1, 2) /= 5) stop 59
  else
    if (.not.allocated (b) .or. size (b) /= 4) stop 60
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 61
    if (.not.allocated (c) .or. size (c) /= 6) stop 62
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 63
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 64
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 65
  end if
  l = .true.
  deallocate (a)
  if (allocated (a)) stop 66
  allocate (a)
  a = 8
  b = (/ 1, 2, 3 /)
  c = reshape ((/ 1, 2, 3, 4, 5, 6, 7, 8 /), (/ 2, 4 /))
  if (.not.allocated (a)) stop 67
  if (.not.allocated (b) .or. size (b) /= 3) stop 68
  if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 3) stop 69
  if (.not.allocated (c) .or. size (c) /= 8) stop 70
  if (size (c, 1) /= 2 .or. size (c, 2) /= 4) stop 71
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 2) stop 72
  if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 4) stop 73
  if (a /= 8 .or. b(2) /= 2 .or. c(1, 2) /= 3) stop 74
!$omp section
  if (.not.allocated (a)) stop 75
  if (l) then
    if (.not.allocated (b) .or. size (b) /= 3) stop 76
    if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 3) stop 77
    if (.not.allocated (c) .or. size (c) /= 8) stop 78
    if (size (c, 1) /= 2 .or. size (c, 2) /= 4) stop 79
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 2) stop 80
    if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 4) stop 81
    if (a /= 8 .or. b(2) /= 2 .or. c(1, 2) /= 3) stop 82
  else
    if (.not.allocated (b) .or. size (b) /= 4) stop 83
    if (lbound (b, 1) /= 6 .or. ubound (b, 1) /= 9) stop 84
    if (.not.allocated (c) .or. size (c) /= 6) stop 85
    if (size (c, 1) /= 3 .or. size (c, 2) /= 2) stop 86
    if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 3) stop 87
    if (lbound (c, 2) /= 8 .or. ubound (c, 2) /= 9) stop 88
  end if
  l = .true.
  deallocate (a)
  if (allocated (a)) stop 89
  allocate (a)
  a = 12
  b = (/ 9, 8, 7, 6, 5, 4 /)
  c = reshape ((/ 1, 2, 3, 4, 5, 6, 7, 8 /), (/ 4, 2 /))
  if (.not.allocated (a)) stop 90
  if (.not.allocated (b) .or. size (b) /= 6) stop 91
  if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 6) stop 92
  if (.not.allocated (c) .or. size (c) /= 8) stop 93
  if (size (c, 1) /= 4 .or. size (c, 2) /= 2) stop 94
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 4) stop 95
  if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 2) stop 96
  if (a /= 12 .or. b(2) /= 8 .or. c(1, 2) /= 5) stop 97
!$omp end parallel sections
  if (.not.allocated (a)) stop 98
  if (.not.allocated (b) .or. size (b) /= 6) stop 99
  if (lbound (b, 1) /= 1 .or. ubound (b, 1) /= 6) stop 100
  if (.not.allocated (c) .or. size (c) /= 8) stop 101
  if (size (c, 1) /= 4 .or. size (c, 2) /= 2) stop 102
  if (lbound (c, 1) /= 1 .or. ubound (c, 1) /= 4) stop 103
  if (lbound (c, 2) /= 1 .or. ubound (c, 2) /= 2) stop 104
  if (a /= 12 .or. b(2) /= 8 .or. c(1, 2) /= 5) stop 105
end
