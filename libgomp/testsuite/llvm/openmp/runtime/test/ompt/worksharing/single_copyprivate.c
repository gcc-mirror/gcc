// clang-format off
// RUN: %libomp-compile-and-run | %sort-threads | FileCheck %s
// REQUIRES: ompt
// clang-format on

#include "callback.h"
#include <omp.h>

omp_lock_t *new_lock() {
    omp_lock_t *lock_ptr;

#pragma omp single copyprivate(lock_ptr)
    {
        lock_ptr = (omp_lock_t *) malloc(sizeof(omp_lock_t));
        omp_init_lock(lock_ptr);
    }

    return lock_ptr;
}

int main(void) {
#pragma omp parallel num_threads(2)
    new_lock();
}

  // clang-format off
  // Check if libomp supports the callbacks for this test.
  // CHECK: 0: NULL_POINTER=[[NULL:.*$]]

  // CHECK-DAG: {{^}}[[THREAD_ID_1:[0-9]+]]: ompt_event_single_in_block_begin: parallel_id=[[PARALLEL_ID:[0-f]+]], task_id=[[TASK_ID:[0-f]+]], codeptr_ra={{(0x)?[0-f]+}}, count=1
  // CHECK-DAG: {{^}}[[THREAD_ID_1]]: ompt_event_init_lock
  // CHECK-DAG: {{^}}[[THREAD_ID_1]]: ompt_event_single_in_block_end: parallel_id=[[PARALLEL_ID]], task_id=[[TASK_ID]], codeptr_ra={{(0x)?[0-f]+}}, count=1

  // CHECK-DAG: {{^}}[[THREAD_ID_2:[0-9]+]]: ompt_event_single_others_begin: parallel_id=[[PARALLEL_ID:[0-f]+]], task_id=[[TASK_ID:[0-f]+]], codeptr_ra={{(0x)?[0-f]+}}, count=1
  // CHECK-DAG: {{^}}[[THREAD_ID_2]]: ompt_event_single_others_end: parallel_id=[[PARALLEL_ID]], task_id=[[TASK_ID]], codeptr_ra={{(0x)?[0-f]+}}, count=1
  // clang-format on

