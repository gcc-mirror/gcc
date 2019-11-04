! { dg-do run }

module task_dep5_mod
contains
  subroutine matmul_depend (N, BS, A, B, C)
     implicit none
     integer :: N, BS, BM
     real, dimension(N, N) :: A, B, C
     integer :: i, j, k, ii, jj, kk
     BM = BS - 1
     do i = 1, N, BS
        do j = 1, N, BS
           do k = 1, N, BS
  !$omp task shared(A,B,C) private(ii,jj,kk) & ! I,J,K are firstprivate by default
  !$omp depend ( in: A(i:i+BM, k:k+BM), B(k:k+BM, j:j+BM) ) &
  !$omp depend ( inout: C(i:i+BM, j:j+BM) )
              do ii = i, i+BM
                 do jj = j, j+BM
                    do kk = k, k+BM
                       C(jj,ii) = C(jj,ii) + A(kk,ii) * B(jj,kk)
                    end do
                 end do
              end do
  !$omp end task
           end do
        end do
     end do
  end subroutine

  subroutine matmul_ref (N, A, B, C)
     implicit none
     integer :: N
     real, dimension(N, N) :: A, B, C
     integer :: i, j, k
     do i = 1, N
       do j = 1, N
         do k = 1, N
             C(j,i) = C(j,i) + A(k,i) * B(j,k)
         end do
       end do
     end do
  end subroutine

  subroutine check (N, A, B)
    integer :: N
    integer :: i, j
    real, parameter :: EPS = 0.000001
    real, dimension(N,N) :: A, B
    real :: diff
    do i = 1, N
      do j = 1, N
        diff = A(i, j) - B(i, j)
        if (diff > EPS .or. -diff > EPS) then
          stop 1
        end if
      end do
    end do
  end subroutine

  subroutine init (N, A, B)
    integer :: N
    integer :: i, j, s
    real, dimension(N,N) :: A, B
    s = -1
    do i = 1, N
      do j = 1, N
        A(i, j) = i*j*s
        B(i, j) = i+j
        s = -s
      end do
    end do
  end subroutine

  subroutine zero_init (N, A, B)
    integer :: N
    integer :: i, j
    real, dimension(N,N) :: A, B
    do i = 1, N
      do j = 1, N
        A(i, j) = 0
        B(i, j) = 0
      end do
    end do
  end subroutine

end module

program main
  use task_dep5_mod
  real, dimension(32, 32) :: A, B, C, C_ref

  call init (32, A, B)
  call zero_init (32, C, C_ref)

  call matmul_depend(32, 4, A, B, C)
  call matmul_ref(32, A, B, C_ref)

  call check (32, C, C_ref)

end program
