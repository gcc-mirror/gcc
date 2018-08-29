! Verify that a conditional "call abort" inside an OpenACC parallel region does
! the right thing.

program main
  implicit none

  integer :: argc
  argc = command_argument_count ()

  !$acc parallel copyin(argc)
  if (argc .ne. 0) then
     call abort
  end if
  !$acc end parallel

end program main
