// clang-format off
// RUN: %libomp-with-ompt-compile-and-run | %sort-threads | FileCheck %S/base_serialized.h
// REQUIRES: ompt
// clang-format on

#define SCHEDULE auto
// The runtime uses static schedule for serialized loop,
// which is a reason choice
#define SCHED_OUTPUT "static"
#include "base_serialized.h"
