! { dg-do run }

module e_51_3_mod
contains
  subroutine init (Q, rows, cols)
    integer :: i, k, rows, cols
    double precision :: Q(rows,cols)
    do k = 1, cols
      do i = 1, rows
        Q(i,k) = 10 * i + k
      end do
    end do
  end subroutine

  subroutine check (P, Q, rows, cols)
    integer :: i, k, rows, cols
    double precision, parameter :: EPS = 0.00001
    double precision :: P(rows,cols), Q(rows,cols), diff
    do k = 1, cols
      do i = 1, rows
        diff = P(i,k) - Q(i,k)
        if (diff > EPS .or. -diff > EPS) STOP 1
      end do
    end do
  end subroutine

  subroutine gramSchmidt_ref (Q, rows, cols)
    integer :: i, k, rows, cols
    double precision :: Q(rows,cols), tmp
    do k = 1, cols
      tmp = 0.0d0
      do i = 1, rows
        tmp = tmp + (Q(i,k) * Q(i,k))
      end do
      tmp = 1.0d0 / sqrt (tmp)
      do i = 1, rows
        Q(i,k) = Q(i,k) * tmp
      end do
    end do
  end subroutine

  subroutine gramSchmidt (Q, rows, cols)
    integer :: i, k, rows, cols
    double precision :: Q(rows,cols), tmp
    !$omp target data map(Q)
      do k = 1, cols
        tmp = 0.0d0
        !$omp target map(tofrom: tmp)
          !$omp parallel do reduction(+:tmp)
          do i = 1, rows
            tmp = tmp + (Q(i,k) * Q(i,k))
          end do
        !$omp end target
        tmp = 1.0d0 / sqrt (tmp)
        !$omp target
          !$omp parallel do
          do i = 1, rows
            Q(i,k) = Q(i,k) * tmp
          end do
        !$omp end target
      end do
    !$omp end target data
  end subroutine
end module

program e_51_3
  use e_51_3_mod, only : init, check, gramSchmidt, gramSchmidt_ref
  integer :: cols, rows
  double precision, pointer :: P(:,:), Q(:,:)
  cols = 5
  rows = 5
  allocate (P(rows,cols), Q(rows,cols))
  call init (P, rows, cols)
  call init (Q, rows, cols)
  call gramSchmidt_ref (P, rows, cols)
  call gramSchmidt (Q, rows, cols)
  call check (P, Q, rows, cols)
  deallocate (P, Q)
end program
