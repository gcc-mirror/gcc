! { dg-do run }

program collapse5
  implicit none

  integer :: i, j
  integer :: count = 0

  !$omp parallel do collapse (2)
    do i = 1, 80000
      do j = 1, 80000
        if (i .eq. 66666 .and. j .eq. 77777) then
	  ! In the collapsed loop space, this is iteration
	  ! 66666*80000+77777==5,333,357,777.  If the type of the iterator
	  ! for the collapsed loop is only a 32-bit unsigned int, then this
	  ! iteration will exceed its maximum range and be skipped.
	  count = count + 1
	end if
      end do
    end do

  if (count .ne. 1) stop 1
end
