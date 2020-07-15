! { dg-set-target-env-var OMP_ALLOCATOR "omp_cgroup_mem_alloc" }
! { dg-set-target-env-var OMP_DISPLAY_ENV "true" }

program main
  use omp_lib
  implicit none (external, type)

  character(len=255) :: mem_env
  type (omp_alloctrait) :: traits(3)
  integer (omp_allocator_handle_kind) :: a

  call get_environment_variable ("OMP_ALLOCATOR", mem_env)

  if (mem_env == "omp_cgroup_mem_alloc") then
    if (omp_get_default_allocator () /= omp_cgroup_mem_alloc) stop 1
    !$omp parallel num_threads (2)
      if (omp_get_default_allocator () /= omp_cgroup_mem_alloc) stop 2
      !$omp parallel num_threads (2)
        if (omp_get_default_allocator () /= omp_cgroup_mem_alloc) stop 3
      !$omp end parallel
    !$omp end parallel
  end if
end program
