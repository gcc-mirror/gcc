/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>
#define MONOTONIC_TYPE unsigned long long
#define MONOTONIC_UNDEF -1ULL
#define MONOTONIC_END(n) n + v

volatile int v;

#include "monotonic-1.c"
