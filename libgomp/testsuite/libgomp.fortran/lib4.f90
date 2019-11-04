! { dg-do run }

program lib4
  use omp_lib
  integer (omp_sched_kind) :: kind
  integer :: modifier
  call omp_set_schedule (omp_sched_static, 32)
  call omp_get_schedule (kind, modifier)
  if (kind.ne.omp_sched_static.or.modifier.ne.32) stop 1
  call omp_set_schedule (omp_sched_dynamic, 4)
  call omp_get_schedule (kind, modifier)
  if (kind.ne.omp_sched_dynamic.or.modifier.ne.4) stop 2
  if (omp_get_thread_limit ().lt.0) stop 3
  call omp_set_max_active_levels (6)
  if (omp_get_max_active_levels ().ne.6) stop 4
end program lib4
