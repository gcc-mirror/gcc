! { dg-do run }

program foo
  integer :: a(3,3,3), ll, lll

  a = 1

  !$acc parallel num_gangs(1) num_workers(2)

  if (any(a(1:3,1:3,1:3).ne.1)) STOP 1

  do ll=1,3

     !$acc loop vector
     do lll=1,3
        a(1,ll,lll) = 2
     enddo

  enddo

  if (a(1,1,1).ne.2) STOP 2

  !$acc end parallel

end program foo
