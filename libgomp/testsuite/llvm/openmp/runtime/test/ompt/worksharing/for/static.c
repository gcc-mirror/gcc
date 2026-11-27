// clang-format off
// RUN: %libomp-with-ompt-compile-and-run | %sort-threads | FileCheck %S/base.h
// REQUIRES: ompt
// clang-format on

#define SCHEDULE static
#include "base.h"
