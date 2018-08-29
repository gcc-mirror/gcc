! { dg-do run }

program main
  integer v1, v2
  integer x

  x = 99

  !$acc parallel copy (v1, v2, x)

  !$acc atomic read
    v1 = x;
  !$acc end atomic

  !$acc atomic write
    x = 32;
  !$acc end atomic

  !$acc atomic read
    v2 = x;
  !$acc end atomic

  !$acc end parallel

  if (v1 .ne. 99) STOP 1

  if (v2 .ne. 32) STOP 2

end program main
