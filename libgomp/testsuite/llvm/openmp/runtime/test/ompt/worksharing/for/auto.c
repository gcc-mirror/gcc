// clang-format off
// RUN: %libomp-with-ompt-compile-and-run | %sort-threads | FileCheck %S/base.h
// REQUIRES: ompt
// clang-format on

#define SCHEDULE auto
// The runtime uses static schedule for auto,
// which is a reason choice
#define SCHED_OUTPUT "static"
#include "base.h"
