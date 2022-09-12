/* { dg-do run } */
/* { dg-set-target-env-var OMP_THREAD_LIMIT_DEV_24 "42" } */
/* { dg-set-target-env-var OMP_THREAD_LIMIT_ALL "43" } */
/* { dg-set-target-env-var OMP_THREAD_LIMIT_DEV "44" } */
/* { dg-set-target-env-var OMP_THREAD_LIMIT "45" } */
/* { dg-set-target-env-var OMP_DEFAULT_DEVICE "42" } */
/* { dg-set-target-env-var OMP_SCHEDULE_DEV_24 "guided,4" } */
/* { dg-set-target-env-var OMP_SCHEDULE_ALL "dynamic" } */
/* { dg-set-target-env-var OMP_SCHEDULE_DEV "guided,1" } */
/* { dg-set-target-env-var OMP_SCHEDULE "guided,2" } */
/* { dg-set-target-env-var OMP_DYNAMIC_DEV_24 "true" } */

/* { dg-set-target-env-var OMP_DYNAMIC_ALL "true" } */
/* { dg-set-target-env-var OMP_DYNAMIC_DEV "true" } */
/* { dg-set-target-env-var OMP_DYNAMIC "true" } */
/* { dg-set-target-env-var OMP_NUM_THREADS "4,3,2" } */
/* { dg-set-target-env-var OMP_NUM_THREADS_ALL "45,46,47" } */
/* { dg-set-target-env-var OMP_NUM_THREADS_DEV "42,43,44" } */
/* { dg-set-target-env-var OMP_NUM_THREADS_DEV_24 "14,13,12" } */
/* { dg-set-target-env-var OMP_MAX_ACTIVE_LEVELS "42" } */
/* { dg-set-target-env-var OMP_MAX_ACTIVE_LEVELS_ALL "43" } */
/* { dg-set-target-env-var OMP_MAX_ACTIVE_LEVELS_DEV "44" } */

/* { dg-set-target-env-var OMP_MAX_ACTIVE_LEVELS_DEV_24 "45" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS "42" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_ALL "43" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV "44" } */
/* { dg-set-target-env-var OMP_NUM_TEAMS_DEV_24 "45" } */
/* { dg-set-target-env-var OMP_PROC_BIND "spread" } */
/* { dg-set-target-env-var OMP_PROC_BIND_ALL "close" } */
/* { dg-set-target-env-var OMP_PROC_BIND_DEV "spread,spread" } */
/* { dg-set-target-env-var OMP_PROC_BIND_DEV_24 "spread,close" } */
/* { dg-set-target-env-var OMP_STACKSIZE "42" } */

/* { dg-set-target-env-var OMP_STACKSIZE_ALL "42 M" } */
/* { dg-set-target-env-var OMP_STACKSIZE_DEV "43 k" } */
/* { dg-set-target-env-var OMP_STACKSIZE_DEV_24 "44" } */
/* { dg-set-target-env-var OMP_WAIT_POLICY "active" } */
/* { dg-set-target-env-var OMP_WAIT_POLICY_ALL "ACTIVE" } */
/* { dg-set-target-env-var OMP_WAIT_POLICY_DEV "passive" } */
/* { dg-set-target-env-var OMP_WAIT_POLICY_DEV_24 "PASSIVE" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT "42" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_ALL "43" } */
/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_DEV "44" } */

/* { dg-set-target-env-var OMP_TEAMS_THREAD_LIMIT_DEV_24 "45" } */
/* { dg-set-target-env-var OMP_CANCELLATION "true" } */
/* { dg-set-target-env-var OMP_DISPLAY_AFFINITY "true" } */
/* { dg-set-target-env-var OMP_TARGET_OFFLOAD "mandatory" } */
/* { dg-set-target-env-var OMP_MAX_TASK_PRIORITY "20" } */
/* { dg-set-target-env-var OMP_ALLOCATOR "omp_const_mem_alloc" } */
/* { dg-set-target-env-var OMP_NESTED "false" } */

#include <omp.h>
#include <stdlib.h>

int
main ()
{
  omp_display_env (1);
  return 0;
}

/* { dg-output ".*\\\[host] OMP_DYNAMIC = 'TRUE'.*" { target native } } */
/* { dg-output ".*\\\[all] OMP_DYNAMIC = 'TRUE'.*" { target native } } */
/* { dg-output ".*\\\[device] OMP_DYNAMIC = 'TRUE'.*" { target native } } */
/* { dg-output ".*\\\[24\] OMP_DYNAMIC = 'TRUE'.*" { target native } } */

/* { dg-output ".*\\\[host] OMP_NUM_THREADS = '4,3,2'.*" { target native } } */
/* { dg-output ".*\\\[all\] OMP_NUM_THREADS = '45,46,47'.*" { target native } } */
/* { dg-output ".*\\\[device\] OMP_NUM_THREADS = '42,43,44'.*" { target native } } */
/* { dg-output ".*\\\[24\] OMP_NUM_THREADS = '14,13,12'.*" { target native } } */

/* { dg-output ".*\\\[host] OMP_SCHEDULE = 'GUIDED,2'.*" { target native } } */
/* { dg-output ".*\\\[all\] OMP_SCHEDULE = 'DYNAMIC'.*" { target native } } */
/* { dg-output ".*\\\[device\] OMP_SCHEDULE = 'GUIDED'.*" { target native } } */
/* { dg-output ".*\\\[24\] OMP_SCHEDULE = 'GUIDED,4'.*" { target native } } */

/* { dg-output ".*\\\[host] OMP_PROC_BIND = 'SPREAD'.*" { target native } } */
/* { dg-output ".*\\\[all\] OMP_PROC_BIND = 'CLOSE'.*" { target native } } */
/* { dg-output ".*\\\[device\] OMP_PROC_BIND = 'SPREAD,SPREAD'.*" { target native } } */
/* { dg-output ".*\\\[24\] OMP_PROC_BIND = 'SPREAD,CLOSE'.*" { target native } } */

/* { dg-output ".*\\\[host] OMP_STACKSIZE = '43008'.*" { target native } } */
/* { dg-output ".*\\\[all\] OMP_STACKSIZE = '44040192'.*" { target native } } */
/* { dg-output ".*\\\[device\] OMP_STACKSIZE = '44032'.*" { target native } } */
/* { dg-output ".*\\\[24\] OMP_STACKSIZE = '45056'.*" { target native } } */

/* { dg-output ".*\\\[host] OMP_WAIT_POLICY = 'ACTIVE'.*" { target native } } */
/* { dg-output ".*\\\[all\] OMP_WAIT_POLICY = 'ACTIVE'.*" { target native } } */
/* { dg-output ".*\\\[device\] OMP_WAIT_POLICY = 'PASSIVE'.*" { target native } } */
/* { dg-output ".*\\\[24\] OMP_WAIT_POLICY = 'PASSIVE'.*" { target native } } */

/* { dg-output ".*\\\[host] OMP_THREAD_LIMIT = '45'.*" { target native } } */
/* { dg-output ".*\\\[all\] OMP_THREAD_LIMIT = '43'.*" { target native } } */
/* { dg-output ".*\\\[device\] OMP_THREAD_LIMIT = '44'.*" { target native } } */
/* { dg-output ".*\\\[24\] OMP_THREAD_LIMIT = '42'.*" { target native } } */

/* { dg-output ".*\\\[host] OMP_MAX_ACTIVE_LEVELS = '42'.*" { target native } } */
/* { dg-output ".*\\\[all\] OMP_MAX_ACTIVE_LEVELS = '43'.*" { target native } } */
/* { dg-output ".*\\\[device\] OMP_MAX_ACTIVE_LEVELS = '44'.*" { target native } } */
/* { dg-output ".*\\\[24\] OMP_MAX_ACTIVE_LEVELS = '45'.*" { target native } } */

/* { dg-output ".*\\\[host] OMP_NUM_TEAMS = '42'.*" { target native } } */
/* { dg-output ".*\\\[all\] OMP_NUM_TEAMS = '43'.*" { target native } } */
/* { dg-output ".*\\\[device\] OMP_NUM_TEAMS = '44'.*" { target native } } */
/* { dg-output ".*\\\[24\] OMP_NUM_TEAMS = '45'.*" { target native } } */

/* { dg-output ".*\\\[host] OMP_TEAMS_THREAD_LIMIT = '42'.*" { target native } } */
/* { dg-output ".*\\\[all\] OMP_TEAMS_THREAD_LIMIT = '43'.*" { target native } } */
/* { dg-output ".*\\\[device\] OMP_TEAMS_THREAD_LIMIT = '44'.*" { target native } } */
/* { dg-output ".*\\\[24\] OMP_TEAMS_THREAD_LIMIT = '45'.*" { target native } } */

/* { dg-output ".*\\\[all] OMP_CANCELLATION = 'TRUE'.*" { target native } } */
/* { dg-output ".*\\\[all] OMP_DEFAULT_DEVICE = '42'.*" { target native } } */
/* { dg-output ".*\\\[all] OMP_MAX_TASK_PRIORITY = '20'.*" { target native } } */
/* { dg-output ".*\\\[all] OMP_DISPLAY_AFFINITY = 'TRUE'.*" { target native } } */
/* { dg-output ".*\\\[host] OMP_ALLOCATOR = 'omp_const_mem_alloc'.*" { target native } } */
/* { dg-output ".*\\\[all] OMP_TARGET_OFFLOAD = 'MANDATORY'.*" { target native } } */
