/* 'std::bad_cast' exception in OpenMP 'target' region, caught.  */

/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -fdump-tree-optimized-raw }
   { dg-additional-options -foffload-options=-fdump-tree-optimized-raw } */
/* { dg-bogus {_ZTISt8bad_cast} PR119734 { target offload_target_nvptx xfail *-*-* } 0 }
   { dg-excess-errors {'mkoffload' failure etc.} { xfail offload_target_nvptx } } */

#include "../libgomp.oacc-c++/exceptions-bad_cast-2.C"

/* { dg-output {CheCKpOInT[\r\n]+} }

   { dg-final { scan-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } }
   { dg-final { scan-offload-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } }
   { dg-output {.*caught 'std::bad_cast'[\r\n]+} { target { ! offload_device } } }
   For GCN, nvptx offload execution, we don't print anything, but just 'abort'.

   TODO For GCN, nvptx offload execution, this currently doesn't 'abort' due to
   the 'std::bad_cast' exception, but rather due to SIGSEGV in 'dynamic_cast';
   PR119692.

   For GCN, nvptx offload execution, there is no 'catch'ing; any exception is fatal.
   { dg-shouldfail {'MyException' exception} { offload_device } } */
