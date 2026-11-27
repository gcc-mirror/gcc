// clang-format off
// RUN: %libomptarget-compile-run-and-check-generic
// REQUIRES: ompt
// REQUIRES: gpu
// clang-format on
// XFAIL: nvptx64-nvidia-cuda

/*
 * Example OpenMP program that registers EMI callbacks
 */

#include <assert.h>
#include <omp.h>
#include <stdio.h>

#include "callbacks.h"
#include "register_emi_tasking.h"

int main() {

  printf("Initial Device Num: %d\n", omp_initial_device);

  int N = 100000;

  int a[N];
  int b[N];

  int i;

  for (i = 0; i < N; i++)
    a[i] = 0;

  for (i = 0; i < N; i++)
    b[i] = i;

#pragma omp target parallel for nowait depend(out:a)
    for (int j = 0; j < N; j++)
      a[j] = b[j];

#pragma omp target teams distribute parallel for nowait depend(out:a)
    for (int j = 0; j < N; j++)
      a[j] = b[j];

#pragma omp taskwait

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
/// CHECK: Initial Device Num: [[INITIAL_DEVICE:(-)?[0-9]+]]
/// CHECK: Callback Init:
/// CHECK: Callback Load:
/// CHECK-SAME: module_id:[[MODULE_ID:[0-9]+]]
/// CHECK: Callback Target EMI: kind=ompt_target_nowait endpoint=ompt_scope_begin device_num=[[DEVICE_NUM:[0-9]+]]
/// CHECK-SAME: code=[[CODE1:(0x)?[0-f]+]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_begin optype=ompt_target_data_alloc
/// CHECK-SAME: code=[[CODE1]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_end optype=ompt_target_data_alloc
/// CHECK-SAME: dest={{(0x)?[0-f]+}}
/// CHECK-SAME: code=[[CODE1]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_begin optype=ompt_target_data_transfer
/// CHECK-SAME: src_device_num=[[INITIAL_DEVICE]]
/// CHECK-SAME: dest_device_num=[[DEVICE_NUM]]
/// CHECK-SAME: code=[[CODE1]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_end optype=ompt_target_data_transfer
/// CHECK-SAME: src_device_num=[[INITIAL_DEVICE]]
/// CHECK-SAME: dest_device_num=[[DEVICE_NUM]]
/// CHECK-SAME: code=[[CODE1]]
/// CHECK: Callback Submit EMI: endpoint=ompt_scope_begin req_num_teams=1
/// CHECK: Callback Submit EMI: endpoint=ompt_scope_end req_num_teams=1
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_begin optype=ompt_target_data_transfer
/// CHECK-SAME: src_device_num=[[DEVICE_NUM]]
/// CHECK-SAME: dest_device_num=[[INITIAL_DEVICE]]
/// CHECK-SAME: code=[[CODE1:(0x)?[0-f]+]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_end optype=ompt_target_data_transfer
/// CHECK-SAME: src_device_num=[[DEVICE_NUM]]
/// CHECK-SAME: dest_device_num=[[INITIAL_DEVICE]]
/// CHECK-SAME: code=[[CODE1]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_begin optype=ompt_target_data_delete
/// CHECK-SAME: code=[[CODE1]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_end optype=ompt_target_data_delete
/// CHECK-SAME: code=[[CODE1]]
/// CHECK: Callback Target EMI: kind=ompt_target_nowait endpoint=ompt_scope_end
/// CHECK-SAME: code=[[CODE1]]

/// CHECK: Callback Target EMI: kind=ompt_target_nowait endpoint=ompt_scope_begin device_num=[[DEVICE_NUM:[0-9]+]]
/// CHECK-SAME: code=[[CODE2:(0x)?[0-f]+]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_begin optype=ompt_target_data_alloc
/// CHECK-SAME: code=[[CODE2]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_end optype=ompt_target_data_alloc
/// CHECK-SAME: dest={{(0x)?[0-f]+}}
/// CHECK-SAME: code=[[CODE2]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_begin optype=ompt_target_data_transfer
/// CHECK-SAME: src_device_num=[[INITIAL_DEVICE]]
/// CHECK-SAME: dest_device_num=[[DEVICE_NUM]]
/// CHECK-SAME: code=[[CODE2]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_end optype=ompt_target_data_transfer
/// CHECK-SAME: src_device_num=[[INITIAL_DEVICE]]
/// CHECK-SAME: dest_device_num=[[DEVICE_NUM]]
/// CHECK-SAME: code=[[CODE2]]
/// CHECK: Callback Submit EMI: endpoint=ompt_scope_begin req_num_teams=0
/// CHECK: Callback Submit EMI: endpoint=ompt_scope_end req_num_teams=0
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_begin optype=ompt_target_data_transfer
/// CHECK-SAME: src_device_num=[[DEVICE_NUM]]
/// CHECK-SAME: dest_device_num=[[INITIAL_DEVICE]]
/// CHECK-SAME: code=[[CODE2:(0x)?[0-f]+]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_end optype=ompt_target_data_transfer
/// CHECK-SAME: src_device_num=[[DEVICE_NUM]]
/// CHECK-SAME: dest_device_num=[[INITIAL_DEVICE]]
/// CHECK-SAME: code=[[CODE2]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_begin optype=ompt_target_data_delete
/// CHECK-SAME: code=[[CODE2]]
/// CHECK: Callback DataOp EMI: endpoint=ompt_scope_end optype=ompt_target_data_delete
/// CHECK-SAME: code=[[CODE2]]
/// CHECK: Callback Target EMI: kind=ompt_target_nowait endpoint=ompt_scope_end
/// CHECK-SAME: code=[[CODE2]]


/// CHECK: Callback Unload:
/// CHECK-SAME: module_id:[[MODULE_ID]]
/// CHECK: Callback Fini:
