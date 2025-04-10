/* PR119692 "C++ 'typeinfo', 'vtable' vs. OpenACC, OpenMP 'target' offloading" */

/* { dg-additional-options -UDEFAULT }
   Wrong code for offloading execution.
   { dg-additional-options -foffload=disable } */
/* { dg-additional-options -fdump-tree-gimple } */

#include "../libgomp.oacc-c++/pr119692-1-1.C"

/* { dg-final { scan-tree-dump-not {(?n)#pragma omp target .* map\(tofrom:_ZTI2C2 \[len: [0-9]+\] \[runtime_implicit\]\) map\(tofrom:_ZTI2C1 \[len: [0-9]+\] \[runtime_implicit\]\) map\(tofrom:_ZTV2C1 \[len: [0-9]+\] \[runtime_implicit\]\)$} gimple { xfail *-*-* } } } */
