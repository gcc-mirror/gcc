// clang-format off
// RUN: %libomptarget-compile-run-and-check-generic
// REQUIRES: ompt
// REQUIRES: gpu
// clang-format on

/*
 * Example OpenMP program that registers non-EMI callbacks
 */

#include <omp.h>
#include <stdio.h>

#include "callbacks.h"
#include "register_non_emi.h"

int main() {
  int N = 100000;

  int a[N];
  int b[N];

  int i;

  for (i = 0; i < N; i++)
    a[i] = 0;

  for (i = 0; i < N; i++)
    b[i] = i;

#pragma omp target parallel for
    for (int j = 0; j < N; j++)
      a[j] = b[j];

#pragma omp target teams distribute parallel for
    for (int j = 0; j < N; j++)
      a[j] = b[j];

  int rc = 0;
  for (i = 0; i < N; i++)
    if (a[i] != b[i]) {
      rc++;
      printf("Wrong value: a[%d]=%d\n", i, a[i]);
    }

  if (!rc)
    printf("Success\n");

  return rc;
}

// clang-format off
/// CHECK: Callback Init:
/// CHECK: Callback Load:
/// CHECK-SAME: module_id:[[MODULE_ID:[0-9]+]]
/// CHECK: Callback Target: target_id={{(0x)?[0-f]+}} kind=ompt_target endpoint=ompt_scope_begin device_num=[[DEVICE_NUM:[0-9]+]] code=[[CODE1:(0x)?[0-f]+]]
/// CHECK: Callback DataOp: target_id={{(0x)?[0-f]+}} host_op_id={{(0x)?[0-f]+}} optype=ompt_target_data_alloc
/// CHECK-SAME: dest={{(0x)?[0-f]+}}
/// CHECK-SAME: code=[[CODE1]]
/// CHECK: Callback DataOp: target_id={{(0x)?[0-f]+}} host_op_id={{(0x)?[0-f]+}} optype=ompt_target_data_transfer_to_device
/// CHECK-SAME: code=[[CODE1]]
/// CHECK: Callback Submit: target_id={{(0x)?[0-f]+}} host_op_id={{(0x)?[0-f]+}} req_num_teams=1
/// CHECK: Callback DataOp: target_id={{(0x)?[0-f]+}} host_op_id={{(0x)?[0-f]+}} optype=ompt_target_data_transfer_from_device
/// CHECK-SAME: code=[[CODE1]]
/// CHECK: Callback DataOp: target_id={{(0x)?[0-f]+}} host_op_id={{(0x)?[0-f]+}} optype=ompt_target_data_delete
/// CHECK-SAME: code=[[CODE1]]
/// CHECK: Callback Target: target_id={{(0x)?[0-f]+}} kind=ompt_target endpoint=ompt_scope_end device_num=[[DEVICE_NUM]] code=[[CODE1]]


/// CHECK: Callback Target: target_id={{(0x)?[0-f]+}} kind=ompt_target endpoint=ompt_scope_begin device_num=[[DEVICE_NUM:[0-9]+]] code=[[CODE2:(0x)?[0-f]+]]
/// CHECK: Callback DataOp: target_id={{(0x)?[0-f]+}} host_op_id={{(0x)?[0-f]+}} optype=ompt_target_data_alloc
/// CHECK-SAME: dest={{(0x)?[0-f]+}}
/// CHECK-SAME: code=[[CODE2]]
/// CHECK: Callback DataOp: target_id={{(0x)?[0-f]+}} host_op_id={{(0x)?[0-f]+}} optype=ompt_target_data_transfer_to_device
/// CHECK-SAME: code=[[CODE2]]
/// CHECK: Callback Submit: target_id={{(0x)?[0-f]+}} host_op_id={{(0x)?[0-f]+}} req_num_teams=0
/// CHECK: Callback DataOp: target_id={{(0x)?[0-f]+}} host_op_id={{(0x)?[0-f]+}} optype=ompt_target_data_transfer_from_device
/// CHECK-SAME: code=[[CODE2]]
/// CHECK: Callback DataOp: target_id={{(0x)?[0-f]+}} host_op_id={{(0x)?[0-f]+}} optype=ompt_target_data_delete
/// CHECK-SAME: code=[[CODE2]]
/// CHECK: Callback Target: target_id={{(0x)?[0-f]+}} kind=ompt_target endpoint=ompt_scope_end device_num=[[DEVICE_NUM]] code=[[CODE2]]

/// CHECK: Callback Unload:
/// CHECK-SAME: module_id:[[MODULE_ID]]
/// CHECK: Callback Fini:
