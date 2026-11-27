// clang-format off
// RUN: %libomp-compile-and-run | FileCheck %s
// REQUIRES: ompt
// Trying to work around GCC/'task_late_fulfill.c' issues, but is still unstable (presumed libgomp implementation issue), thus:
// XFAIL: gcc
// clang-format on

#include "callback.h"
#include <omp.h>
#include <stdatomic.h>

int main(int argc, char **argv) {
  atomic_int res = 0;
  int condition = 0;
  #pragma omp parallel num_threads(2)
  {
    #pragma omp masked
    {
      omp_event_handle_t event;
      #pragma omp task shared(event, condition)
      {
        OMPT_WAIT(condition, 1);
        usleep(100000);
        ++res;
        omp_fulfill_event(event);
      }
      #pragma omp task if(0) detach(event) depend(out:res) shared(condition)
      {
        ++res;
        OMPT_SIGNAL(condition);
      }
      #pragma omp task if(0) depend(in:res)
      {
        printf("This task was preceded by %i tasks.\n", res);
      }
    } // single
  } // parallel
}

// clang-format off
// Check if libomp supports the callbacks for this test.
// CHECK-NOT: {{^}}0: Could not register callback 'ompt_callback_task_create'
// CHECK-NOT: {{^}}0: Could not register callback 'ompt_callback_task_schedule'
// CHECK-NOT: {{^}}0: Could not register callback 'ompt_callback_parallel_begin'
// CHECK-NOT: {{^}}0: Could not register callback 'ompt_callback_parallel_end'
// CHECK-NOT: {{^}}0: Could not register callback 'ompt_callback_implicit_task'
// CHECK-NOT: {{^}}0: Could not register callback 'ompt_callback_mutex_acquire'
// CHECK-NOT: {{^}}0: Could not register callback 'ompt_callback_mutex_acquired'
// CHECK-NOT: {{^}}0: Could not register callback 'ompt_callback_mutex_released'

// CHECK: {{^}}0: NULL_POINTER=[[NULL:.*$]]

// CHECK: {{^}}[[MASTER_ID:[0-9]+]]: ompt_event_parallel_begin:
// CHECK-SAME: parent_task_id=[[PARENT_TASK_ID:[0-f]+]],
// CHECK-SAME: parent_task_frame.exit=[[NULL]],
// CHECK-SAME: parent_task_frame.reenter={{(0x)?[0-f]+}},
// CHECK-SAME: parallel_id=[[PARALLEL_ID:[0-f]+]],
// CHECK-SAME: requested_team_size=2,

// CHECK: {{^}}[[THREAD_ID:[0-9]+]]: ompt_event_thread_begin:
// CHECK-SAME: thread_type=ompt_thread_worker=2,

// CHECK: {{^}}[[MASTER_ID]]: ompt_event_implicit_task_begin:
// CHECK-SAME: parallel_id=[[PARALLEL_ID]],
// CHECK-SAME: task_id=[[IMPLICIT_TASK_ID:[0-f]+]]

// Fulfill task
// CHECK: {{^}}[[MASTER_ID]]: ompt_event_task_create:
// CHECK-SAME: parent_task_id=[[IMPLICIT_TASK_ID]],
// CHECK-SAME: parent_task_frame.exit={{(0x)?[0-f]+}},
// CHECK-SAME: parent_task_frame.reenter={{(0x)?[0-f]+}},
// CHECK-SAME: new_task_id=[[FULFILL_TASK_ID:[0-f]+]],

// Detached task
// CHECK: {{^}}[[MASTER_ID]]: ompt_event_task_create:
// CHECK-SAME: parent_task_id=[[IMPLICIT_TASK_ID]],
// CHECK-SAME: parent_task_frame.exit={{(0x)?[0-f]+}},
// CHECK-SAME: parent_task_frame.reenter={{(0x)?[0-f]+}},
// CHECK-SAME: new_task_id=[[DETACHED_TASK_ID:[0-f]+]],

// CHECK-DAG: {{^}}[[MASTER_ID]]: ompt_event_task_schedule: first_task_id=[[IMPLICIT_TASK_ID]], second_task_id=[[DETACHED_TASK_ID]], prior_task_status=ompt_task_switch=7
// CHECK-DAG: {{^}}[[THREAD_ID]]: ompt_event_task_schedule: first_task_id={{[0-f]+}}, second_task_id=[[FULFILL_TASK_ID]], prior_task_status=ompt_task_switch=7

// CHECK: {{^}}[[THREAD_ID]]: ompt_event_task_schedule:
// CHECK-SAME: first_task_id=[[DETACHED_TASK_ID]],
// CHECK-SAME: second_task_id=ffffffffffffffff,
// CHECK-SAME: prior_task_status=ompt_task_late_fulfill=6

// CHECK: {{^}}[[MASTER_ID]]: ompt_event_task_schedule:
// CHECK-SAME: first_task_id=[[DETACHED_TASK_ID]],
// CHECK-SAME: second_task_id=[[IMPLICIT_TASK_ID]],
// CHECK-SAME: prior_task_status=ompt_task_complete=1
// clang-format on
