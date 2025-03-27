/* 'throw' in OpenMP 'target' region, caught.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -foffload=disable }
   Offloading compilation not yet supported; see
   'target-exceptions-throw-2-offload-sorry-GCN.C',
   'target-exceptions-throw-2-offload-sorry-nvptx.C'.  */
/* { dg-additional-options -fdump-tree-optimized-raw } */

#include "../libgomp.oacc-c++/exceptions-throw-2.C"

/* { dg-final { scan-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   { dg-output {caught 'MyException'[\r\n]+} } */
