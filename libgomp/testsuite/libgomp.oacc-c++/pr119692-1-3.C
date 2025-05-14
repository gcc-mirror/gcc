/* PR119692 "C++ 'typeinfo', 'vtable' vs. OpenACC, OpenMP 'target' offloading" */

/* { dg-additional-options -DDEFAULT=default(present) }
   Wrong code for offloading execution.
   { dg-xfail-run-if PR119692 { ! openacc_host_selected } } */
/* { dg-additional-options -fdump-tree-gimple } */

#include "pr119692-1-1.C"

/* { dg-bogus {using 'vector_length \(32\)', ignoring 1} {} { target openacc_nvidia_accel_selected xfail *-*-* } 0 } */

/* { dg-final { scan-tree-dump-not {(?n)#pragma omp target oacc_serial default\(present\) map\(force_present:_ZTI2C2 \[len: [0-9]+\]\) map\(force_present:_ZTI2C1 \[len: [0-9]+\]\) map\(force_present:_ZTV2C1 \[len: [0-9]+\]\)$} gimple { xfail *-*-* } } } */
