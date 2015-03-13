! Don't compile this anywhere, it is just auxiliary
! file compiled together with declare-target-1.f90
! to verify inter-CU module handling of omp declare target.
! { dg-do compile { target { lp64 && { ! lp64 } } } }

subroutine foo
  use declare_target_1_mod

  var_x = 10
  !$omp target update to(var_x)

  !$omp target
    var_x = var_x * 2;
  !$omp end target

  !$omp target update from(var_x)
  if (var_x /= 20) call abort
end subroutine foo
