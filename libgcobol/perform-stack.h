/* Copyright (C) 2026 Free Software Foundation, Inc.
   This file is part of GCC, distributed under the GNU General Public License,
   version 3 or (at your option) any later version.  See COPYING3.  */

#ifndef LIBGCOBOL_PERFORM_STACK_H
#define LIBGCOBOL_PERFORM_STACK_H

#include <stddef.h>
#include <stdint.h>

/* Debugger ABI v1.  The 16-byte header is followed by six native unsigned
   address/count words.  Addresses are integer representations of existing
   buffers.  WORD_SIZE is sizeof (uintptr_t), equal to sizeof (size_t).
   BYTE_SIZE is 16 + 6 * WORD_SIZE, giving 64 bytes on a 64-bit target.

   The process-global stacks retain their existing serialized-writer model.
   An odd generation marks an update in progress.  A reader accepts data
   only when generation is even and unchanged across its complete read.
   Buffers can move or shrink during any update.  A stopped odd generation
   requires a diagnostic or native-frame fallback, without retrying in a
   loop.  The descriptor and all entries use the target's native byte order.
   The exported constant pointer addresses this descriptor for its lifetime.
   Reading needs neither runtime DWARF nor an inferior function call.  */
struct gg_debug_perform_stack_v1
  {
  uint32_t version;
  uint32_t byte_size;
  uint32_t generation;
  uint32_t word_size;
  uintptr_t signatures;
  size_t signature_count;
  uintptr_t indexes;
  size_t index_count;
  uintptr_t bookmarks;
  size_t bookmark_count;
  };

#ifdef __cplusplus
extern "C" {
#endif
extern const struct gg_debug_perform_stack_v1 *const __gg__debug_perform_stack;
/* Forced-return ABI.  INVOCATION is one-based.  For NATIVE != 0 discard
   that native invocation and every deeper one, with DEPTH == 0.  Otherwise
   retain that invocation and its outermost DEPTH PERFORMs.  Restore local
   descriptors, module names, program/currency state and main-entry counters.
   Success is 0.  Invalid/transient requests return 1, older instrumentation
   returns 2.  Rejected requests leave all runtime state unchanged.  Unlike
   stack inspection, this operation requires an inferior function call.  */
/* Supply one int* main-entry counter for each discarded native invocation,
   in innermost-to-outermost order, as variadic arguments.  */
extern int __gg__debug_perform_unwind(size_t invocation, size_t depth, int native, ...);

#ifdef __cplusplus
}
#endif

#endif
