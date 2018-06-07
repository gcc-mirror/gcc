! { dg-do run }
! { dg-options "-fno-inline" }

program main
    integer :: n

    n = 5

    !$acc parallel copy (n)
      n = func (n)
    !$acc end parallel

    if (n .ne. 6) STOP 1

contains

    function func (n) result (rc)
    !$acc routine
    integer, intent (in) :: n
    integer :: rc

    rc = n
    rc = rc + 1

    end function

end program
