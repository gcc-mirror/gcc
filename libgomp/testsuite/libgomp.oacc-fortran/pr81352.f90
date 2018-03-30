! { dg-do run }

program foo
  integer :: a(3,3), l, ll
  a = 0

  !$acc parallel num_gangs (1) num_workers(1)

  do l=1,3
     !$acc loop vector
     do ll=1,3
        a(l,ll) = 2
     enddo
  enddo

  if (any(a(1:3,1:3).ne.2)) STOP 1

  !$acc end parallel

end program foo
