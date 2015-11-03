/* { dg-do run } */
/* { dg-additional-options "-ftree-parallelize-loops=4 -ffast-math --param parloops-schedule=dynamic --param parloops-chunk-size=100" } */

#include "autopar-1.c"
