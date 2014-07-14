! PR fortran/34020
! { dg-do run }

      subroutine atomic_add(lhs, rhs)
      real lhs, rhs
!$omp atomic
      lhs = rhs + lhs
      end

      external atomic_add
      real lhs, rhs
      integer i
      lhs = 0
      rhs = 1
!$omp parallel do num_threads(8) shared(lhs, rhs)
      do i = 1, 300000
        call atomic_add(lhs, rhs)
      enddo
      if (lhs .ne. 300000) call abort
      end
