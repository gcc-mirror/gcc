/* { dg-do run { target lto } } */
/* { dg-options "-O2 -ftree-vectorize -std=c99 -fipa-pta -flto -flto-partition=max" } */
/* { dg-prune-output "warning: using serial compilation" } */

#include "pr46032.c"
