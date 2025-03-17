! { dg-do compile  { target skip-all-targets } }
! used by declare-variant-mod-1.f90

! Check that module-file handling works for declare_variant
! and its match/adjust_args/append_args clauses
!
! PR fortran/115271

subroutine test1
  use m1
  use iso_c_binding, only: c_loc, c_ptr
  implicit none (type, external)

  integer :: i, j
  type(c_ptr) :: a1, b1, c1, x1, y1, z1

  !$omp dispatch
    i = m1_g (a1, b1, c1)
   j = m1_g (x1, y1, z1)
end
! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(c1.\[0-9\]+, D\\.\[0-9\]+\\);" 1 "gimplify" } }
! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr \\(a1, D\\.\[0-9\]+\\);" 1 "gimplify" } }
! { dg-final { scan-tree-dump-times "i = m1_f \\(D\\.\[0-9\]+, &b1, &D\\.\[0-9\]+\\);" 1 "gimplify" } }
! { dg-final { scan-tree-dump-times "j = m1_g \\(x1, &y1, &z1\\);" 1 "gimplify" } }

subroutine test2
  use m2, only: m2_g 
  use iso_c_binding, only: c_loc, c_ptr
  implicit none (type, external)

  integer :: i, j
  type(c_ptr) :: a2, b2, c2, x2, y2, z2

  !$omp dispatch
    i = m2_g (a2, b2, c2)
  j = m2_g (x2, y2, z2)
end
! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr (c2.\[0-9\]+, D\\.\[0-9\]+);" 1 "gimplify" } }
! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr (a2, D\\.\[0-9\]+);" 1 "gimplify" } }
! { dg-final { scan-tree-dump-times "i = m2_f (D\\.\[0-9\]+, &b2, &D\\.\[0-9\]+);" 1 "gimplify" } }
! { dg-final { scan-tree-dump-times "j = m2_g \\(x2, &y2, &z2\\);" 1 "gimplify" } }

subroutine test3
  use m2, only: my_func => m2_g 
  use iso_c_binding, only: c_loc, c_ptr
  implicit none (type, external)

  integer :: i, j
  type(c_ptr) :: a3, b3, c3, x3, y3, z3

  !$omp dispatch
    i = my_func (a3, b3, c3)
  j = my_func (x3, y3, z3)
end
! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr (c3.\[0-9\]+, D\\.\[0-9\]+);" 1 "gimplify" } }
! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr (a3, D\\.\[0-9\]+);" 1 "gimplify" } }
! { dg-final { scan-tree-dump-times "i = m2_f (D\\.\[0-9\]+, &b3, &D\\.\[0-9\]+);" 1 "gimplify" } }
! { dg-final { scan-tree-dump-times "j = m2_g \\(x3, &y3, &z3\\);" 1 "gimplify" } }

subroutine test4
  use m3, only: my_m3_g
  use iso_c_binding, only: c_loc, c_ptr
  implicit none (type, external)

  integer :: i, j
  type(c_ptr) :: a4, b4, c4, x4, y4, z4

  !$omp dispatch
    i = my_m3_g (a4, b4, c4)
  j = my_m3_g (x4, y4, z4)
end
! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr (c4.\[0-9\]+, D\\.\[0-9\]+);" 1 "gimplify" } }
! { dg-final { scan-tree-dump-times "D\\.\[0-9\]+ = __builtin_omp_get_mapped_ptr (a4, D\\.\[0-9\]+);" 1 "gimplify" } }
! { dg-final { scan-tree-dump-times "i = m3_f (D\\.\[0-9\]+, &b4, &D\\.\[0-9\]+);" 1 "gimplify" } }
! { dg-final { scan-tree-dump-times "j = m3_g \\(x4, &y4, &z4\\);" 1 "gimplify" } }

program main
  call test1
  call test2
  call test3
end
