! { dg-do run }
! { dg-additional-sources declare-target-2.f90 }

module declare_target_1_mod
  integer :: var_x
  !$omp declare target(var_x)
end module declare_target_1_mod

  interface
    subroutine foo ()
    end subroutine foo
  end interface

  call foo ()
end
