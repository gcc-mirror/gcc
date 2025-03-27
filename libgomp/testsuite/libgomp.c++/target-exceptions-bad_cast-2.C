/* 'std::bad_cast' exception in OpenMP 'target' region, caught.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -foffload=disable }
   Offloading compilation not yet supported; see
   'target-exceptions-bad_cast-2-offload-sorry-GCN.C',
   'target-exceptions-bad_cast-2-offload-sorry-nvptx.C'.  */
/* { dg-additional-options -fdump-tree-optimized-raw } */

#include "../libgomp.oacc-c++/exceptions-bad_cast-2.C"

/* { dg-final { scan-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } }
   { dg-output {caught 'std::bad_cast'[\r\n]+} } */
