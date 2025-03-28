/* 'std::bad_cast' exception in OpenACC compute region, caught, '-foffload-options=-mno-fake-exceptions'.  */

/* As this test case involves an expected offload compilation failure, we have to handle each offload target individually.
   { dg-do link { target openacc_radeon_accel_selected } } */
/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -foffload-options=-mno-fake-exceptions } */
/* { dg-additional-options -fdump-tree-optimized-raw }
   { dg-additional-options -foffload-options=-fdump-tree-optimized-raw } */

#include "exceptions-bad_cast-2.C"

/* { dg-final { scan-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } }
   { dg-final { only_for_offload_target amdgcn-amdhsa scan-offload-tree-dump-times {gimple_call <__cxa_bad_cast, } 1 optimized } }
   Given '-foffload-options=-mno-fake-exceptions', offload compilation fails:
   { dg-regexp {[^\r\n]+: In function 'main[^']+':[\r\n]+(?:[^\r\n]+: sorry, unimplemented: exception handling not supported[\r\n]+)+} }
   (Note, using 'dg-regexp' instead of 'dg-message', as the former runs before the auto-mark-UNSUPPORTED.)
   { dg-excess-errors {'mkoffload' failure etc.} } */
