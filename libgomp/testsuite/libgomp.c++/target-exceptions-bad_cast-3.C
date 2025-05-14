/* 'std::bad_cast' exception in OpenMP 'target' region, dead code.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -DDEFAULT=defaultmap(to) }
   ... to avoid wrong code for offloading execution; PR119692.
   With this, the device code still isn't correct, but the defects are in dead code.
   { dg-additional-options -fdump-tree-gimple } */
/* { dg-additional-options -fdump-tree-optimized-raw }
   { dg-additional-options -foffload-options=-fdump-tree-optimized-raw } */

#include "../libgomp.oacc-c++/exceptions-bad_cast-3.C"

/* { dg-final { scan-tree-dump-not {(?n)#pragma omp target .* defaultmap\(to\) map\(to:_ZTI2C2 \[len: [0-9]+\] \[runtime_implicit\]\) map\(to:_ZTI2C1 \[len: [0-9]+\] \[runtime_implicit\]\) map\(to:_ZTV2C1 \[len: [0-9]+\] \[runtime_implicit\]\)$} gimple { xfail *-*-* } } } */

/* { dg-final { scan-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } } */
