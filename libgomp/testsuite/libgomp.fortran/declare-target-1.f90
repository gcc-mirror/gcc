! { dg-do run }
! { dg-additional-sources declare-target-2.f90 }

module declare_target_1_mod
  integer :: var_x, var_y, var_z
  !$omp declare target(var_x)
  !$omp declare target to(var_y)
  !$omp declare target enter(var_z)
end module declare_target_1_mod

  interface
    subroutine foo ()
    end subroutine foo
  end interface

  call foo ()
end
