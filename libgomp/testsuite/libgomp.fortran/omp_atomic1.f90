! { dg-do run }
    integer (kind = 4) :: a
    integer (kind = 2) :: b
    real :: c, f
    double precision :: d
    integer, dimension (10) :: e
    a = 1
    b = 2
    c = 3
    d = 4
    e = 5
    f = 6
!$omp atomic
    a = a + 4
!$omp atomic
    b = 4 - b
!$omp atomic
    c = c * 2
!$omp atomic
    d = 2 / d
    if (a .ne. 5 .or. b .ne. 2 .or. c .ne. 6 .or. d .ne. 0.5) STOP 1
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
