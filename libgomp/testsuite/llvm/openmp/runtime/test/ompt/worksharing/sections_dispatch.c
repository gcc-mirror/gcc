// clang-format off
// RUN: %libomp-compile-and-run | %sort-threads | FileCheck %s
// REQUIRES: ompt
// UNSUPPORTED: intel-15.0, intel-16.0, intel-17.0, intel-18.0
// clang-format on

#include "callback.h"
#include <omp.h>

int main() {
#pragma omp parallel sections num_threads(2)
  {
#pragma omp section
    {
      printf("%lu: section 1\n", ompt_get_thread_data()->value);
    }
#pragma omp section
    {
      printf("%lu: section 2\n", ompt_get_thread_data()->value);
    }
  }

  // clang-format off
  // Check if libomp supports the callbacks for this test.
  // CHECK-NOT: {{^}}0: Could not register callback 'ompt_callback_work'
  // CHECK-NOT: {{^}}0: Could not register callback 'ompt_callback_dispatch'

  // CHECK: 0: NULL_POINTER=[[NULL:.*$]]

  // CHECK: {{^}}{{[0-9]+}}: ompt_event_section_begin:
  // CHECK-SAME: parallel_id={{[0-f]+}}, task_id={{[0-f]+}}
  // CHECK-SAME: codeptr_ra={{(0x)?[0-f]+}}
 
  // CHECK: {{^}}{{[0-9]+}}: ompt_event_section_begin:
  // CHECK-SAME: parallel_id={{[0-f]+}}, task_id={{[0-f]+}}
  // CHECK-SAME: codeptr_ra={{(0x)?[0-f]+}}
  
  // clang-format on

  return 0;
}
