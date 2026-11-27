// clang-format off
// RUN: %libomp-compile-and-run | %sort-threads | FileCheck %S/base_serialized.h
// REQUIRES: ompt
// clang-format on

#define SCHEDULE runtime
// Without any schedule specified, the runtime uses dynamic schedule,
// which is a reason choice
#define SCHED_OUTPUT "dynamic"
#include "base_serialized.h"
