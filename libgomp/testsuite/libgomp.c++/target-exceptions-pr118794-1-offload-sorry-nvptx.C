/* Exception handling constructs in dead code, '-foffload-options=-mno-fake-exceptions'.  */

/* As this test case involves an expected offload compilation failure, we have to handle each offload target individually.
   { dg-do link { target offload_target_nvptx } }
   { dg-additional-options -foffload=nvptx-none } */
/* { dg-require-effective-target exceptions }
   { dg-additional-options -fexceptions } */
/* { dg-additional-options -foffload-options=-mno-fake-exceptions } */
/* { dg-additional-options -O0 } */
/* { dg-additional-options -fdump-tree-optimized-raw }
   { dg-additional-options -foffload-options=-fdump-tree-optimized-raw } */

#include "target-exceptions-pr118794-1.C"

/* In this specific C++ arrangement, distilled from PR118794, GCC synthesizes
   '__builtin_eh_pointer', '__builtin_unwind_resume' calls as dead code in 'f':
   { dg-final { scan-tree-dump-times {gimple_call <__builtin_eh_pointer, } 1 optimized { target { ! { arm_eabi || tic6x-*-* } } } } }
   { dg-final { scan-tree-dump-times {gimple_call <__builtin_unwind_resume, } 1 optimized { target { ! { arm_eabi || tic6x-*-* } } } } }
   ..., just 'targetm.arm_eabi_unwinder' is different:
   { dg-final { scan-tree-dump-times {gimple_call <__builtin_cxa_end_cleanup, } 1 optimized { target { arm_eabi || tic6x-*-* } } } }
   { dg-final { only_for_offload_target nvptx-none scan-offload-tree-dump-times {gimple_call <__builtin_eh_pointer, } 1 optimized } }
   { dg-final { only_for_offload_target nvptx-none scan-offload-tree-dump-times {gimple_call <__builtin_unwind_resume, } 1 optimized } }
   Given '-O0' and '-foffload-options=-mno-fake-exceptions', offload compilation fails:
   { dg-regexp {[^\r\n]+: In function 'f':[\r\n]+(?:[^\r\n]+: sorry, unimplemented: exception handling not supported[\r\n]+)+} }
   (Note, using 'dg-regexp' instead of 'dg-message', as the former runs before the auto-mark-UNSUPPORTED.)
   { dg-excess-errors {'mkoffload' failure etc.} } */
