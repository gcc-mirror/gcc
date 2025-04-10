/* PR119692 "C++ 'typeinfo', 'vtable' vs. OpenACC, OpenMP 'target' offloading" */

/* { dg-additional-options -DDEFAULT=defaultmap(firstprivate) }
   Wrong code for offloading execution.
   { dg-xfail-run-if PR119692 { offload_device } } */
/* { dg-additional-options -fdump-tree-gimple } */

#include "pr119692-1-1.C"

/* { dg-final { scan-tree-dump-not {(?n)#pragma omp target .* defaultmap\(firstprivate\) firstprivate\(_ZTI2C2\) firstprivate\(_ZTI2C1\) firstprivate\(_ZTV2C1\)$} gimple { xfail *-*-* } } } */
