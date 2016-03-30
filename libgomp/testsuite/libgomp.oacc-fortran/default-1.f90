! { dg-do run }

program main
  implicit none
  real a, b
  real c
  !$acc declare create (c)

  a = 2.0
  b = 0.0

  !$acc parallel copy (a) create (b) default (none)
    b = a
    a = 1.0
    a = a + b
  !$acc end parallel

  if (a .ne. 3.0) call abort

  !$acc kernels copy (a) create (b) default (none)
    b = a
    a = 1.0
    a = a + b
  !$acc end kernels

  if (a .ne. 4.0) call abort

  !$acc parallel default (none) copy (a) create (b)
    b = a
    a = 1.0
    a = a + b
  !$acc end parallel

  if (a .ne. 5.0) call abort

  !$acc parallel default (none) copy (a)
    c = a
    a = 1.0
    a = a + c
  !$acc end parallel

  if (a .ne. 6.0) call abort

  !$acc data copy (a)
  !$acc parallel default (none)
    c = a
    a = 1.0
    a = a + c
  !$acc end parallel
  !$acc end data

  if (a .ne. 7.0) call abort

end program main
