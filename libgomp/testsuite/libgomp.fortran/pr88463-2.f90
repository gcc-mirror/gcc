! PR fortran/88463
! { dg-do link }
! { dg-options "-fopenmp" }
! { dg-additional-sources pr88463-1.f90 }

module pr88463_2
  integer, parameter :: a = 1
  real, parameter :: b(4) = (/ 2., 3., 4., 5. /)
end module pr88463_2
