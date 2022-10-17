! Don't compile this anywhere, it is just auxiliary
! file compiled together with declare-target-1.f90
! to verify inter-CU module handling of omp declare target.
! { dg-do compile { target { lp64 && { ! lp64 } } } }

subroutine foo
  use declare_target_1_mod

  var_x = 10
  var_y = 20
  var_z = 30
  !$omp target update to(var_x, var_y, var_z)

  !$omp target
    var_x = var_x * 2;
    var_y = var_y * 3;
    var_z = var_z * 4;
  !$omp end target

  !$omp target update from(var_x, var_y, var_z)
  if (var_x /= 20) stop 1
  if (var_y /= 20*3) stop 2
  if (var_z /= 30*4) stop 3
end subroutine foo
