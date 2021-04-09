! PR fortran/98858
!
! Assumed-size array with use_device_ptr()
!
program test_use_device_ptr
  use iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none
  double precision :: alpha
  integer, parameter :: lda = 10
  integer, allocatable :: mat(:, :)
       integer :: i, j

  allocate(mat(lda, lda))
  do i = 1, lda
    do j = 1, lda
      mat(j,i) = i*100 + j
    end do
  end do

  !$omp target enter data map(to:mat)
  call dgemm(lda, mat)
  !$omp target exit data map(from:mat)

  do i = 1, lda
    do j = 1, lda
      if (mat(j,i) /= -(i*100 + j)) stop 1
    end do
  end do

  !$omp target enter data map(to:mat)
  call dgemm2(lda, mat)
  !$omp target exit data map(from:mat)

  do i = 1, lda
    do j = 1, lda
      if (mat(j,i) /= (i*100 + j)) stop 1
    end do
  end do

  contains

    subroutine dgemm(lda, a)
      implicit none
      integer :: lda
      integer, target:: a(lda,*) ! need target attribute to use c_loc
      !$omp target data use_device_ptr(a)
        call negate_it(c_loc(a), lda)
      !$omp end target data
    end subroutine

    subroutine dgemm2(lda, a)
      implicit none
      integer :: lda
      integer, target:: a(lda,*) ! need target attribute to use c_loc
      !$omp target data use_device_addr(a)
        call negate_it(c_loc(a), lda)
      !$omp end target data
    end subroutine

    subroutine negate_it(a, n)
      type(c_ptr), value :: a
      integer, value :: n
      integer, pointer :: array(:,:)

      ! detour due to OpenMP 5.0 oddness
      call c_f_pointer(a, array, [n,n])
      call do_offload(array, n)
    end

    subroutine do_offload(aptr, n)
      integer, target :: aptr(:,:)
      integer, value :: n
      !$omp target is_device_ptr(aptr)
      call negate_it_tgt(aptr, n)
      !$omp end target
    end subroutine do_offload

    subroutine negate_it_tgt(array, n)
      !$omp declare target
       integer, value :: n
       integer :: array(n,n)
       integer :: i, j
       !$omp parallel do collapse(2)
       do i = 1, n
         do j = 1, n
           array(j,i) = - array(j,i)
         end do
       end do
       !$omp end parallel do
  end subroutine
end program
