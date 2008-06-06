! { dg-options "-O2" }

program strassen_matmul
  use omp_lib
  integer, parameter :: N = 1024
  double precision, save :: A(N,N), B(N,N), C(N,N), D(N,N)
  double precision :: start, end

  call random_seed
  call random_number (A)
  call random_number (B)
  start = omp_get_wtime ()
  C = matmul (A, B)
  end = omp_get_wtime ()
  write(*,'(a, f10.6)') ' Time for matmul      = ', end - start
  D = 0
  start = omp_get_wtime ()
  call strassen (A, B, D, N)
  end = omp_get_wtime ()
  write(*,'(a, f10.6)') ' Time for Strassen    = ', end - start
  if (sqrt (sum ((C - D) ** 2)) / N .gt. 0.1) call abort
  D = 0
  start = omp_get_wtime ()
!$omp parallel
!$omp single
  call strassen (A, B, D, N)
!$omp end single nowait
!$omp end parallel
  end = omp_get_wtime ()
  write(*,'(a, f10.6)') ' Time for Strassen MP = ', end - start
  if (sqrt (sum ((C - D) ** 2)) / N .gt. 0.1) call abort

contains

  recursive subroutine strassen (A, B, C, N)
    integer, intent(in) :: N
    double precision, intent(in) :: A(N,N), B(N,N)
    double precision, intent(out) :: C(N,N)
    double precision :: T(N/2,N/2,7)
    integer :: K, L

    if (iand (N,1) .ne. 0 .or. N < 64) then
      C = matmul (A, B)
      return
    end if
    K = N / 2
    L = N / 2 + 1
!$omp task shared (A, B, T)
    call strassen (A(:K,:K) + A(L:,L:), B(:K,:K) + B(L:,L:), T(:,:,1), K)
!$omp end task
!$omp task shared (A, B, T)
    call strassen (A(L:,:K) + A(L:,L:), B(:K,:K), T(:,:,2), K)
!$omp end task
!$omp task shared (A, B, T)
    call strassen (A(:K,:K), B(:K,L:) - B(L:,L:), T(:,:,3), K)
!$omp end task
!$omp task shared (A, B, T)
    call strassen (A(L:,L:), B(L:,:K) - B(:K,:K), T(:,:,4), K)
!$omp end task
!$omp task shared (A, B, T)
    call strassen (A(:K,:K) + A(:K,L:), B(L:,L:), T(:,:,5), K)
!$omp end task
!$omp task shared (A, B, T)
    call strassen (A(L:,:K) - A(:K,:K), B(:K,:K) + B(:K,L:), T(:,:,6), K)
!$omp end task
!$omp task shared (A, B, T)
    call strassen (A(:K,L:) - A(L:,L:), B(L:,:K) + B(L:,L:), T(:,:,7), K)
!$omp end task
!$omp taskwait
    C(:K,:K) = T(:,:,1) + T(:,:,4) - T(:,:,5) + T(:,:,7)
    C(L:,:K) = T(:,:,2) + T(:,:,4)
    C(:K,L:) = T(:,:,3) + T(:,:,5)
    C(L:,L:) = T(:,:,1) - T(:,:,2) + T(:,:,3) + T(:,:,6)
  end subroutine strassen
end
