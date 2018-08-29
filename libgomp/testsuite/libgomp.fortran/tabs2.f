! { dg-options "-ffixed-form" }
      if (b().ne.2) STOP 1
      contains
      subroutine a
!$omp parallel
!$omp	end	parallel
	end subroutine a
      function b()
      integer :: b
	b = 1
!$	b = 2
      end function b
      end
