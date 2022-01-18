! Exercise three levels of parallelism using SGEMM from BLAS.

! { dg-do run }

! { dg-additional-options -Wuninitialized }

! Explicitly set vector_length to 128 using a vector_length clause.
subroutine openacc_sgemm_128 (m, n, k, alpha, a, b, beta, c)
  integer :: m, n, k
  real :: alpha, beta
  real :: a(k,*), b(k,*), c(m,*)

  integer :: i, j, l
  real :: temp
  ! { dg-note {'temp' was declared here} {} { target *-*-* } .-1 }

  !$acc parallel loop copy(c(1:m,1:n)) copyin(a(1:k,1:m),b(1:k,1:n)) vector_length (128) firstprivate (temp)
  ! { dg-warning {'temp' is used uninitialized} {} { target *-*-* } .-1 }
  do j = 1, n
     !$acc loop
     do i = 1, m
        temp = 0.0
        !$acc loop reduction(+:temp)
        do l = 1, k
           temp = temp + a(l,i)*b(l,j)
        end do
        if(beta == 0.0) then
           c(i,j) = alpha*temp
        else
           c(i,j) = alpha*temp + beta*c(i,j)
        end if
     end do
  end do
end subroutine openacc_sgemm_128

subroutine host_sgemm (m, n, k, alpha, a, b, beta, c)
  integer :: m, n, k
  real :: alpha, beta
  real :: a(k,*), b(k,*), c(m,*)

  integer :: i, j, l
  real :: temp

  do j = 1, n
     do i = 1, m
        temp = 0.0
        do l = 1, k
           temp = temp + a(l,i)*b(l,j)
        end do
        if(beta == 0.0) then
           c(i,j) = alpha*temp
        else
           c(i,j) = alpha*temp + beta*c(i,j)
        end if
     end do
  end do
end subroutine host_sgemm

program main
  integer, parameter :: M = 100, N = 50, K = 2000
  real :: a(K, M), b(K, N), c(M, N), d (M, N), e (M, N)
  real alpha, beta
  integer i, j

  a(:,:) = 1.0
  b(:,:) = 0.25

  c(:,:) = 0.0
  d(:,:) = 0.0
  e(:,:) = 0.0

  alpha = 1.05
  beta = 1.25

  call openacc_sgemm_128 (M, N, K, alpha, a, b, beta, d)
  call host_sgemm (M, N, K, alpha, a, b, beta, e)

  do i = 1, m
     do j = 1, n
        if (d(i,j) /= e(i,j)) stop 1
     end do
  end do
end program main
