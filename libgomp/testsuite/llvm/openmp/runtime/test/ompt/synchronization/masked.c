// clang-format off
// RUN: %libomp-compile-and-run | FileCheck %s --check-prefixes=CHECK,CHECK-NON-OMPT
// RUN: %libomp-with-ompt-compile-and-run | FileCheck %s --check-prefixes=CHECK,CHECK-OMPT
// REQUIRES: ompt
// clang-format on

#include "callback.h"
#include <omp.h>

int main() {
  int x = 0;
#pragma omp parallel num_threads(2)
  {
#pragma omp masked
    {
      print_fuzzy_address(1);
      x++;
    }
    print_current_address(2);
  }

  printf("%" PRIu64 ": x=%d\n", ompt_get_thread_data()->value, x);

  return 0;
}

// clang-format off
// Check if libomp supports the callbacks for this test.
// CHECK-NOT: {{^}}0: Could not register callback 'ompt_callback_masked'

// CHECK: 0: NULL_POINTER=[[NULL:.*$]]

// CHECK-NON-OMPT: {{^}}[[MASTER_ID:[0-9]+]]: ompt_event_masked_beginend:
// CHECK-OMPT: {{^}}[[MASTER_ID:[0-9]+]]: ompt_event_masked_begin:
// CHECK-SAME: parallel_id=[[PARALLEL_ID:[0-f]+]], task_id=[[TASK_ID:[0-f]+]],
// CHECK-SAME: codeptr_ra=[[RETURN_ADDRESS:(0x)?[0-f]+]]{{[0-f][0-f]}}
// CHECK: {{^}}[[MASTER_ID]]: fuzzy_address={{.*}}[[RETURN_ADDRESS]]
// CHECK-OMPT: {{^}}[[MASTER_ID]]: ompt_event_masked_end:
// CHECK-OMPT-SAME: parallel_id=[[PARALLEL_ID]], task_id=[[TASK_ID]],
// CHECK-OMPT-SAME: codeptr_ra=[[RETURN_ADDRESS_END:(0x)?[0-f]+]]
// CHECK: {{^}}[[MASTER_ID]]: current_address={{.*}}
// clang-format on
