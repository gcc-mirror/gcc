// clang-format off
// RUN: %libomp-compile-and-run | %sort-threads | FileCheck %s --check-prefixes=CHECK,CHECK-NON-OMPT
// enable when scope -fopenmp-ompt entry points are implemented
// RUN--: %libomp-with-ompt-compile-and-run | %sort-threads | FileCheck %s --check-prefixes=CHECK,CHECK-OMPT
// REQUIRES: ompt
// clang-format on

#include "callback.h"
#include <omp.h>
#include <stdio.h>

int main() {
  int x = 0;

#pragma omp parallel num_threads(2)
{
#pragma omp scope reduction(task, +:x)
  {
      printf("%" PRIu64 ": in scope\n", ompt_get_thread_data()->value);
#pragma omp task in_reduction(+:x)
    x++;

#pragma omp taskwait
  }
}

printf("%d\n", x);

  // clang-format off
  // Check if libomp supports the callbacks for this test.
  // CHECK-NOT: {{^}}0: Could not register callback 'ompt_callback_work'

  // CHECK: 0: NULL_POINTER=[[NULL:.*$]]

  // CHECK-NON-OMPT-DAG: {{^}}[[THREAD_ID_1:[0-9]+]]: ompt_event_scope_beginend: parallel_id=[[PARALLEL_ID:[0-f]+]], task_id=[[TASK_ID:[0-f]+]], codeptr_ra={{(0x)?[0-f]+}}, count=1
  // CHECK-OMPT-DAG: {{^}}[[THREAD_ID_1:[0-9]+]]: ompt_event_scope_begin: parallel_id=[[PARALLEL_ID:[0-f]+]], task_id=[[TASK_ID:[0-f]+]], codeptr_ra={{(0x)?[0-f]+}}, count=1
  // CHECK-DAG: {{^}}[[THREAD_ID_1]]: in scope
  // CHECK-OMPT-DAG: {{^}}[[THREAD_ID_1]]: ompt_event_scope_end: parallel_id=[[PARALLEL_ID]], task_id=[[TASK_ID]], codeptr_ra={{(0x)?[0-f]+}}, count=1

  // CHECK-NON-OMPT-DAG: {{^}}[[THREAD_ID_2:[0-9]+]]: ompt_event_scope_beginend: parallel_id=[[PARALLEL_ID:[0-f]+]], task_id=[[TASK_ID:[0-f]+]], codeptr_ra={{(0x)?[0-f]+}}, count=1
  // CHECK-OMPT-DAG: {{^}}[[THREAD_ID_2:[0-9]+]]: ompt_event_scopes_begin: parallel_id=[[PARALLEL_ID:[0-f]+]], task_id=[[TASK_ID:[0-f]+]], codeptr_ra={{(0x)?[0-f]+}}, count=1
  // CHECK-OMPT-DAG: {{^}}[[THREAD_ID_2]]: ompt_event_scope_end: parallel_id=[[PARALLEL_ID]], task_id=[[TASK_ID]], codeptr_ra={{(0x)?[0-f]+}}, count=1
  // clang-format on

  return 0;
}
