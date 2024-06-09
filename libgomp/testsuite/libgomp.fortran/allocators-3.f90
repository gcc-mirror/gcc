! { dg-additional-options "-fdump-tree-original -fopenmp-allocators" }

subroutine s
  character(:), allocatable :: s1,s2

  !$omp allocators allocate(s1)
  allocate(character(len=3) :: s1)

  !$omp allocators allocate(s2)
  allocate(character(len=5) :: s2)

  s2(1:5) = "12"
  s1 = trim(s2)
end
! { dg-final { scan-tree-dump-times "s1 = \\(character\\(kind=1\\)\\\[1:.s1\\\] \\*\\) __builtin_GOMP_alloc \\(1, 3, 0B\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "s2 = \\(character\\(kind=1\\)\\\[1:.s2\\\] \\*\\) __builtin_GOMP_alloc \\(1, 5, 0B\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "s1 = \\(character\\(kind=1\\)\\\[1:.s1\\\] \\*\\) \\(D\\.\[0-9\]+ \\? __builtin_omp_realloc \\(\\(void \\*\\) s1, MAX_EXPR <\\(sizetype\\) len.1, 1>, 0B, 0B\\) : __builtin_realloc \\(\\(void \\*\\) s1, MAX_EXPR <\\(sizetype\\) len.1, 1>\\)\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "GOMP_add_alloc \\(s1\\);" 2 "original" } }
! { dg-final { scan-tree-dump-times "OMP_add_alloc \\(s2\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "if \\(GOMP_is_alloc \\(s2\\)\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_GOMP_free \\(s2, 0B\\);" 1 "original" } }


call s
end
