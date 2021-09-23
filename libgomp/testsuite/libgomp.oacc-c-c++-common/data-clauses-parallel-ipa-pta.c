/* { dg-do run { target lto } } */
/* { dg-additional-options "-fipa-pta -flto -flto-partition=max" } */
/* { dg-prune-output "warning: using serial compilation" } */

#include "data-clauses-parallel.c"
