/* Control flow redundancy hardening
   Copyright (C) 2022-2025 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <oliva@adacore.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* Avoid infinite recursion.  */
#pragma GCC optimize ("-fno-harden-control-flow-redundancy")

#include <stddef.h>
#include <stdbool.h>

/* This should be kept in sync with gcc/gimple-harden-control-flow.cc.  */
#if __CHAR_BIT__ >= 28
# define VWORDmode __QI__
#elif __CHAR_BIT__ >= 14
# define VWORDmode __HI__
#else
# define VWORDmode __SI__
#endif

typedef unsigned int __attribute__ ((__mode__ (VWORDmode))) vword;

/* This function is optionally called at the end of a function to verify that
   the VISITED array represents a sensible execution path in the CFG.  It is
   always expected to pass; the purpose is to detect attempts to subvert
   execution by taking unexpected paths, or other execution errors.  The
   function, instrumented by pass_harden_control_flow_redundancy at a time in
   which it had BLOCKS basic blocks (not counting ENTER and EXIT, so block 2
   maps to index 0, the first bit of the first VWORD), sets a bit in the bit
   array VISITED as it enters the corresponding basic block.  CFG holds a
   representation of the control flow graph at the time of the instrumentation:
   an array of VWORDs holding, for each block, a sequence of predecessors, and
   a sequence of successors.  Each pred and succ sequence is represented as a
   sequence of pairs (mask, index), terminated by an index-less all-zero mask.
   If the bit corresponding to the block is set, then at least one of the pred
   masks, and at least one of the succ masks, must have a bit set in
   VISITED[index].  An ENTRY block predecessor and an EXIT block successor are
   represented in a (mask, index) pair that tests the block's own bit.  */
extern void __hardcfr_check (size_t blocks,
			     vword const *visited,
			     vword const *cfg);

/* Compute the MASK for the bit representing BLOCK in WORDIDX's vword in a
   visited blocks bit array.  */
static inline void
block2mask (size_t const block, vword *const mask, size_t *const wordidx)
{
  size_t wbits = __CHAR_BIT__ * sizeof (vword);
  *wordidx = block / wbits;
  *mask = (vword)1 << (block % wbits);
}

/* Check whether the bit corresponding to BLOCK is set in VISITED.  */
static inline bool
visited_p (size_t const block, vword const *const visited)
{
  vword mask;
  size_t wordidx;
  block2mask (block, &mask, &wordidx);
  vword w = visited[wordidx];
  return (w & mask) != 0;
}

/* Check whether any VISITED bits that would correspond to blocks after BLOCKS
   are set.  */
static inline bool
excess_bits_set_p (size_t const blocks, vword const *const visited)
{
  vword mask;
  size_t wordidx;
  block2mask (blocks - 1, &mask, &wordidx);
  mask = -mask - mask;
  vword w = visited[wordidx];
  return (w & mask) != 0;
}

/* Read and consume a mask from **CFG_IT.  (Consume meaning advancing the
   iterator to the next word).  If the mask is zero, return FALSE.  Otherwise,
   also read and consume an index, and set *MASK and/or *WORDIDX, whichever are
   nonNULL, to the corresponding read values, and finally return TRUE.  */
static inline bool
next_pair (vword const **const cfg_it,
	   vword *const mask,
	   size_t *const wordidx)
{
  vword m = **cfg_it;
  ++*cfg_it;
  if (!m)
    return false;

  if (mask)
    *mask = m;

  size_t word = **cfg_it;
  ++*cfg_it;

  if (wordidx)
    *wordidx = word;

  return true;
}

/* Return TRUE iff any of the bits in MASK is set in VISITED[WORDIDX].  */
static inline bool
test_mask (vword const *const visited,
	   vword const mask, size_t const wordidx)
{
  return (visited[wordidx] & mask) != 0;
}

/* Scan a sequence of pairs (mask, index) at **CFG_IT until its terminator is
   reached and consumed.  */
static inline void
consume_seq (vword const **const cfg_it)
{
  while (next_pair (cfg_it, NULL, NULL))
    /* Do nothing.  */;
}

/* Check that at least one of the MASK bits in a sequence of pairs (mask,
   index) at **CFG_IT is set in the corresponding VISITED[INDEX] word.  Trap if
   we reach the terminator without finding any.  Consume the entire sequence
   otherwise, so that *CFG_IT points just past the terminator, which may be the
   beginning of the next sequence.  */
static inline bool
check_seq (vword const *const visited, vword const **const cfg_it)
{
  vword mask;
  size_t wordidx;

  /* If the block was visited, check that at least one of the
     preds/succs was also visited.  */
  do
    /* If we get to the end of the sequence without finding any
       match, something is amiss.  */
    if (!next_pair (cfg_it, &mask, &wordidx))
      return false;
  /* Keep searching until we find a match, at which point the
     condition is satisfied.  */
  while (!test_mask (visited, mask, wordidx));

  /* Consume the remaining entries in the sequence, whether we found a match or
     skipped the block, so as to position the iterator at the beginning of the
     next .  */
  consume_seq (cfg_it);

  return true;
}

/* Print out the CFG with BLOCKS blocks, presumed to be associated with CALLER.
   This is expected to be optimized out entirely, unless the verbose part of
   __hardcfr_check_fail is enabled.  */
static inline void
__hardcfr_debug_cfg (size_t const blocks,
		     void const *const caller,
		     vword const *const cfg)
{
  __builtin_printf ("CFG at %p, for %p", cfg, caller);
  vword const *cfg_it = cfg;
  for (size_t i = 0; i < blocks; i++)
    {
      vword mask; size_t wordidx;
      block2mask (i, &mask, &wordidx);
      __builtin_printf ("\nblock %lu (%lu/0x%lx)\npreds: ",
			(unsigned long)i,
			(unsigned long)wordidx, (unsigned long)mask);
      while (next_pair (&cfg_it, &mask, &wordidx))
	__builtin_printf (" (%lu/0x%lx)",
			  (unsigned long)wordidx, (unsigned long)mask);
      __builtin_printf ("\nsuccs: ");
      while (next_pair (&cfg_it, &mask, &wordidx))
	__builtin_printf (" (%lu/0x%lx)",
			  (unsigned long)wordidx, (unsigned long)mask);
    }
  __builtin_printf ("\n");
}

#ifndef ATTRIBUTE_UNUSED
# define ATTRIBUTE_UNUSED __attribute__ ((__unused__))
#endif

/* This is called when an out-of-line hardcfr check fails.  All the arguments
   are ignored, and it just traps, unless HARDCFR_VERBOSE_FAIL is enabled.  IF
   it is, it prints the PART of the CFG, expected to have BLOCKS blocks, that
   failed at CALLER's BLOCK, and the VISITED bitmap.  When the verbose mode is
   enabled, it also forces __hardcfr_debug_cfg (above) to be compiled into an
   out-of-line function, that could be called from a debugger.
   */

#ifdef __BPF__
__attribute__((__always_inline__))
#endif
static inline void
__hardcfr_check_fail (size_t const blocks ATTRIBUTE_UNUSED,
		      vword const *const visited ATTRIBUTE_UNUSED,
		      vword const *const cfg ATTRIBUTE_UNUSED,
		      size_t const block ATTRIBUTE_UNUSED,
		      int const part ATTRIBUTE_UNUSED,
		      void const *const caller ATTRIBUTE_UNUSED)
{
#if HARDCFR_VERBOSE_FAIL
  static const char *parts[] = { "preds", "succs", "no excess" };

  vword mask; size_t wordidx;
  block2mask (block, &mask, &wordidx);
  if (part == 2)
    mask = -mask - mask;
  __builtin_printf ("hardcfr fail at %p block %lu (%lu/0x%lx), expected %s:",
		    caller, (unsigned long)block,
		    (unsigned long)wordidx, (unsigned long)mask,
		    parts[part]);

  if (part != 2)
    {
      /* Skip data for previous blocks.  */
      vword const *cfg_it = cfg;
      for (size_t i = block; i--; )
	{
	  consume_seq (&cfg_it);
	  consume_seq (&cfg_it);
	}
      for (size_t i = part; i--; )
	consume_seq (&cfg_it);

      while (next_pair (&cfg_it, &mask, &wordidx))
	__builtin_printf (" (%lu/0x%lx)",
			  (unsigned long)wordidx, (unsigned long)mask);
    }

  __builtin_printf ("\nvisited:");
  block2mask (blocks - 1, &mask, &wordidx);
  for (size_t i = 0; i <= wordidx; i++)
    __builtin_printf (" (%lu/0x%lx)",
		      (unsigned long)i, (unsigned long)visited[i]);
  __builtin_printf ("\n");

  /* Reference __hardcfr_debug_cfg so that it's output out-of-line, so that it
     can be called from a debugger.  */
  if (!caller || caller == __hardcfr_debug_cfg)
    return;
#endif
  __builtin_trap ();
}

/* Check that, for each of the BLOCKS basic blocks, if its bit is set in
   VISITED, at least one of its predecessors in CFG is also set, and at also
   that at least one of its successors in CFG is also set.  */
void
__hardcfr_check (size_t const blocks,
		 vword const *const visited,
		 vword const *const cfg)
{
  vword const *cfg_it = cfg;
  for (size_t i = 0; i < blocks; i++)
    {
      bool v = visited_p (i, visited);

      /* For each block, there are two sequences of pairs (mask, index), each
	 sequence terminated by a single all-zero mask (no index).  The first
	 sequence is for predecessor blocks, the second is for successors.  At
	 least one of each must be set.  */
      if (!v)
	{
	  /* Consume predecessors.  */
	  consume_seq (&cfg_it);
	  /* Consume successors.  */
	  consume_seq (&cfg_it);
	}
      else
	{
	  /* Check predecessors.  */
	  if (!check_seq (visited, &cfg_it))
	    __hardcfr_check_fail (blocks, visited, cfg, i, 0,
				  __builtin_return_address (0));
	  /* Check successors.  */
	  if (!check_seq (visited, &cfg_it))
	    __hardcfr_check_fail (blocks, visited, cfg, i, 1,
				  __builtin_return_address (0));
	}
    }
  if (excess_bits_set_p (blocks, visited))
    __hardcfr_check_fail (blocks, visited, cfg, blocks - 1, 2,
			  __builtin_return_address (0));
}
