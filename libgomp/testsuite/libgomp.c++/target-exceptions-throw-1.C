/* 'throw' in OpenMP 'target' region.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -fdump-tree-optimized-raw }
   { dg-additional-options -foffload-options=-fdump-tree-optimized-raw } */
/* { dg-bogus {Size expression must be absolute\.} PR119737 { target offload_target_amdgcn xfail *-*-* } 0 }
   { dg-ice PR119737 { offload_target_amdgcn } }
   { dg-excess-errors {'mkoffload' failures etc.} { xfail offload_target_amdgcn } } */

#include "../libgomp.oacc-c++/exceptions-throw-1.C"

/* { dg-output {CheCKpOInT[\r\n]+} }

   { dg-final { scan-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   For host execution, we print something like:
       terminate called after throwing an instance of 'MyException'
       Aborted (core dumped)
   { dg-output {.*MyException} { target { ! offload_device } } }
   For GCN, nvptx offload execution, we don't print anything, but just 'abort'.

   { dg-shouldfail {'MyException' exception} } */
