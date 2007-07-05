! { dg-do compile }
!
! PR fortran/32359
! Contributed by Bill Long <longb@cray.com>

subroutine test
      use omp_lib
      implicit none
      integer, parameter :: NT = 4
      integer :: a
      save
!$omp threadprivate(a)
      a = 1

!$    call omp_set_num_threads(NT)
!$omp parallel
      print *, omp_get_thread_num(), a
!$omp end parallel

end subroutine test

! Derived from OpenMP test omp1/F2_6_2_8_5i.f90
      use omp_lib
      implicit none
      integer, parameter :: NT = 4
      integer :: a = 1
!$omp threadprivate(a)

!$    call omp_set_num_threads(NT)
!$omp parallel
      print *, omp_get_thread_num(), a
!$omp end parallel

      END
