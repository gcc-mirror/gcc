/* { dg-do run } */

#include <omp.h>
#include <stdlib.h>
#define NONMONOTONIC_TYPE unsigned long long
#define NONMONOTONIC_END(n) n + v

volatile int v;

#include "nonmonotonic-1.c"
