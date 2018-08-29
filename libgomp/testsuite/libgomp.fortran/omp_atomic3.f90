! { dg-do run }
    integer (kind = 4) :: a, a2
    integer (kind = 2) :: b, b2
    real :: c, f
    double precision :: d, d2, c2
    integer, dimension (10) :: e
!$omp atomic write
    a = 1
!$omp atomic write
    b = 2
!$omp end atomic
!$omp atomic write
    c = 3
!$omp atomic write
    d = 1 + 2 + 3 - 2
    e = 5
!$omp atomic write
    f = 6
!$omp end atomic
!$omp atomic
    a = a + 4
!$omp end atomic
!$omp atomic update
    b = 4 - b
!$omp atomic
    c = c * 2
!$omp atomic update
    d = 2 / d
!$omp end atomic
!$omp atomic read
    a2 = a
!$omp atomic read
    b2 = b
!$omp end atomic
!$omp atomic read
    c2 = c
!$omp atomic read
    d2 = d
    if (a2 .ne. 5 .or. b2 .ne. 2 .or. c2 .ne. 6 .or. d2 .ne. 0.5) STOP 1
!$omp atomic write
    d = 1.2
!$omp atomic
    a = a + c + d
!$omp atomic
    b = b - (a + c + d)
    if (a .ne. 12 .or. b .ne. -17) STOP 2
!$omp atomic
    a = c + d + a
!$omp atomic
    b = a + c + d - b
    if (a .ne. 19 .or. b .ne. 43) STOP 3
!$omp atomic
    b = (a + c + d) - b
    a = 32
!$omp atomic
    a = a / 3.4
    if (a .ne. 9 .or. b .ne. -16) STOP 4
end
