! { dg-do run }
    integer (kind = 4) :: a, a2
    integer (kind = 2) :: b, b2
    real :: c
    double precision :: d, d2, c2
    integer, dimension (10) :: e
!$omp atomic write
    a = 1
!$omp atomic write
    b = 2
!$omp atomic write
    c = 3
!$omp atomic write
    d = 4
!$omp atomic capture
    a2 = a
    a = a + 4
!$omp end atomic
!$omp atomic capture
    b = b - 18
    b2 = b
!$omp end atomic
!$omp atomic capture
    c2 = c
    c = 2.0 * c
!$omp end atomic
!$omp atomic capture
    d = d / 2.0
    d2 = d
!$omp end atomic
    if (a2 .ne. 1 .or. b2 .ne. -16 .or. c2 .ne. 3 .or. d2 .ne. 2) stop 1
!$omp atomic read
    a2 = a
!$omp atomic read
    c2 = c
    if (a2 .ne. 5 .or. b2 .ne. -16 .or. c2 .ne. 6 .or. d2 .ne. 2) stop 2
end
