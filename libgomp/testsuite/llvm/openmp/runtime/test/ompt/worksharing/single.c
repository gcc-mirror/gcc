// clang-format off
// RUN: %libomp-compile-and-run | %sort-threads | FileCheck %s --check-prefixes=CHECK,CHECK-NON-OMPT
// RUN: %libomp-with-ompt-compile-and-run | %sort-threads | FileCheck %s --check-prefixes=CHECK,CHECK-OMPT
// REQUIRES: ompt
// clang-format on

#include "callback.h"
#include <omp.h>

int main() {
  int x = 0;
#pragma omp parallel num_threads(2)
  {
#pragma omp single
    {
      printf("%" PRIu64 ": in single\n", ompt_get_thread_data()->value);
      x++;
    }
  }

  printf("x=%d\n", x);

  // clang-format off
  // Check if libomp supports the callbacks for this test.
  // CHECK-NOT: {{^}}0: Could not register callback 'ompt_callback_work'

  // CHECK: 0: NULL_POINTER=[[NULL:.*$]]

  // CHECK-NON-OMPT-DAG: {{^}}[[THREAD_ID_1:[0-9]+]]: ompt_event_single_in_block_beginend: parallel_id=[[PARALLEL_ID:[0-f]+]], task_id=[[TASK_ID:[0-f]+]], codeptr_ra={{(0x)?[0-f]+}}, count=1
  // CHECK-OMPT-DAG: {{^}}[[THREAD_ID_1:[0-9]+]]: ompt_event_single_in_block_begin: parallel_id=[[PARALLEL_ID:[0-f]+]], task_id=[[TASK_ID:[0-f]+]], codeptr_ra={{(0x)?[0-f]+}}, count=1
  // CHECK-DAG: {{^}}[[THREAD_ID_1]]: in single
  // CHECK-OMPT-DAG: {{^}}[[THREAD_ID_1]]: ompt_event_single_in_block_end: parallel_id=[[PARALLEL_ID]], task_id=[[TASK_ID]], codeptr_ra={{(0x)?[0-f]+}}, count=1

  // CHECK-NON-OMPT-DAG: {{^}}[[THREAD_ID_2:[0-9]+]]: ompt_event_single_others_beginend: parallel_id=[[PARALLEL_ID:[0-f]+]], task_id=[[TASK_ID:[0-f]+]], codeptr_ra={{(0x)?[0-f]+}}, count=1
  // CHECK-OMPT-DAG: {{^}}[[THREAD_ID_2:[0-9]+]]: ompt_event_single_others_begin: parallel_id=[[PARALLEL_ID:[0-f]+]], task_id=[[TASK_ID:[0-f]+]], codeptr_ra={{(0x)?[0-f]+}}, count=1
  // CHECK-OMPT-DAG: {{^}}[[THREAD_ID_2]]: ompt_event_single_others_end: parallel_id=[[PARALLEL_ID]], task_id=[[TASK_ID]], codeptr_ra={{(0x)?[0-f]+}}, count=1
  // clang-format on

  return 0;
}
