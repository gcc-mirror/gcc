! { dg-do run }
    integer (kind = 4) :: a, a2
    integer (kind = 2) :: b, b2
    real :: c
    double precision :: d, d2, c2
    integer, dimension (10) :: e
    e(:) = 5
    e(7) = 9
!$omp atomic write seq_cst
    a = 1
!$omp atomic seq_cst, write
    b = 2
!$omp atomic write, seq_cst
    c = 3
!$omp atomic seq_cst write
    d = 4
!$omp atomic capture seq_cst
    a2 = a
    a = a + 4
!$omp end atomic
!$omp atomic capture, seq_cst
    b = b - 18
    b2 = b
!$omp end atomic
!$omp atomic seq_cst, capture
    c2 = c
    c = 2.0 * c
!$omp end atomic
!$omp atomic seq_cst capture
    d = d / 2.0
    d2 = d
!$omp end atomic
    if (a2 .ne. 1 .or. b2 .ne. -16 .or. c2 .ne. 3 .or. d2 .ne. 2) call abort
!$omp atomic read seq_cst
    a2 = a
!$omp atomic seq_cst, read
    c2 = c
    if (a2 .ne. 5 .or. b2 .ne. -16 .or. c2 .ne. 6 .or. d2 .ne. 2) call abort
    a2 = 10
    if (a2 .ne. 10) call abort
!$omp atomic capture
    a2 = a
    a = e(1) + e(6) + e(7) * 2
!$omp endatomic
    if (a2 .ne. 5) call abort
!$omp atomic read
    a2 = a
!$omp end atomic
    if (a2 .ne. 28) call abort
!$omp atomic capture seq_cst
    b2 = b
    b = e(1) + e(7) + e(5) * 2
!$omp end atomic
    if (b2 .ne. -16) call abort
!$omp atomic seq_cst, read
    b2 = b
!$omp end atomic
    if (b2 .ne. 24) call abort
end
