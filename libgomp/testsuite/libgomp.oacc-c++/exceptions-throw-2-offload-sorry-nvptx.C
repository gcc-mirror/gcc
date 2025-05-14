/* 'throw' in OpenACC compute region, caught, '-foffload-options=-mno-fake-exceptions'.  */

/* As this test case involves an expected offload compilation failure, we have to handle each offload target individually.
   { dg-do link { target openacc_nvidia_accel_selected } } */
/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -foffload-options=-mno-fake-exceptions } */
/* { dg-additional-options -fdump-tree-optimized-raw }
   { dg-additional-options -foffload-options=-fdump-tree-optimized-raw } */

#include "exceptions-throw-2.C"

/* { dg-bogus {using 'vector_length \(32\)', ignoring 1} {} { target openacc_nvidia_accel_selected xfail *-*-* } 0 } */

/* { dg-final { scan-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { scan-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   { dg-final { only_for_offload_target nvptx-none scan-offload-tree-dump-times {gimple_call <__cxa_allocate_exception, } 1 optimized } }
   { dg-final { only_for_offload_target nvptx-none scan-offload-tree-dump-times {gimple_call <__cxa_throw, } 1 optimized } }
   Given '-foffload-options=-mno-fake-exceptions', offload compilation fails:
   { dg-regexp {[^\r\n]+: In function 'main[^']+':[\r\n]+(?:[^\r\n]+: sorry, unimplemented: exception handling not supported[\r\n]+)+} }
   (Note, using 'dg-regexp' instead of 'dg-message', as the former runs before the auto-mark-UNSUPPORTED.)
   { dg-excess-errors {'mkoffload' failure etc.} } */
