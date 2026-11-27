// clang-format off
// RUN: %libomp-with-ompt-compile-and-run | %sort-threads | FileCheck %S/base_split.h
// RUN: %libomp-with-ompt-compile-and-run | %sort-threads | FileCheck --check-prefix=CHECK-LOOP %S/base_split.h
// REQUIRES: ompt
// clang-format on

#define SCHEDULE static
#include "base_split.h"
