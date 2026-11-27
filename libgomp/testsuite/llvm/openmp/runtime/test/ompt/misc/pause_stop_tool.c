// clang-format off
// RUN: %libomp-compile-and-run | FileCheck %s
// REQUIRES: ompt

// clang-format on
#include "callback.h"
#include <omp.h>
int main() {
  int x = 0;
  int ret = 0;
#pragma omp parallel num_threads(2)
#pragma omp single
  x++;
  ret = omp_pause_resource(omp_pause_stop_tool, omp_get_initial_device());
  printf("omp_pause_resource %s\n", ret ? "failed" : "succeeded");
#pragma omp parallel num_threads(2)
#pragma omp single
  x++;
  printf("x = %d\n", x);
  return 0;

  // clang-format off
  // Check if
  // -- omp_pause_resource/resource_all returns expected code
  // -- OMPT interface is shut down as expected

  // CHECK-NOT: {{^}}0: Could not register callback
  // CHECK: 0: NULL_POINTER=[[NULL:.*$]]

  // CHECK: ompt_event_parallel_begin
  // CHECK: ompt_event_parallel_end

  // CHECK: omp_pause_resource succeeded

  // CHECK-NOT: ompt_event
  // CHECK: x = 2
  // clang-format on
}
