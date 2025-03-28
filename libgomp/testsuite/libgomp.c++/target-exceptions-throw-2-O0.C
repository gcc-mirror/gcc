/* 'throw' in OpenMP 'target' region, caught.  */

/* { dg-additional-options -O0 } */
/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -fdump-tree-optimized-raw }
   { dg-additional-options -foffload-options=-fdump-tree-optimized-raw } */
/* { dg-bogus {undefined symbol: typeinfo name for MyException} PR119806 { target offload_target_amdgcn xfail *-*-* } 0 }
   { dg-excess-errors {'mkoffload' failure etc.} { xfail offload_target_amdgcn } } */
/* { dg-bogus {Initial value type mismatch} PR119806 { target offload_target_nvptx xfail *-*-* } 0 }
   { dg-excess-errors {'mkoffload' failure etc.} { xfail offload_target_nvptx } } */

#include "target-exceptions-throw-2.C"

/* { dg-output {CheCKpOInT[\r\n]+} }

   { dg-final { scan-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   { dg-output {.*caught 'MyException'[\r\n]+} { target { ! offload_device } } }
   For GCN, nvptx offload execution, we don't print anything, but just 'abort'.

   For GCN, nvptx offload execution, there is no 'catch'ing; any exception is fatal.
   { dg-shouldfail {'MyException' exception} { offload_device } } */
