/* Infrastructure for tracking user variable locations and values
   throughout compilation.
   Copyright (C) 2010, 2011, 2012  Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <aoliva@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "valtrack.h"
#include "function.h"
#include "regs.h"
#include "emit-rtl.h"

/* Replace auto-increment addressing modes with explicit operations to access
   the same addresses without modifying the corresponding registers.  */

static rtx
cleanup_auto_inc_dec (rtx src, enum machine_mode mem_mode ATTRIBUTE_UNUSED)
{
  rtx x = src;
#ifdef AUTO_INC_DEC
  const RTX_CODE code = GET_CODE (x);
  int i;
  const char *fmt;

  switch (code)
    {
    case REG:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_FIXED:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case SCRATCH:
      /* SCRATCH must be shared because they represent distinct values.  */
      return x;
    case CLOBBER:
      if (REG_P (XEXP (x, 0)) && REGNO (XEXP (x, 0)) < FIRST_PSEUDO_REGISTER)
	return x;
      break;

    case CONST:
      if (shared_const_p (x))
	return x;
      break;

    case MEM:
      mem_mode = GET_MODE (x);
      break;

    case PRE_INC:
    case PRE_DEC:
      gcc_assert (mem_mode != VOIDmode && mem_mode != BLKmode);
      return gen_rtx_PLUS (GET_MODE (x),
			   cleanup_auto_inc_dec (XEXP (x, 0), mem_mode),
			   GEN_INT (code == PRE_INC
				    ? GET_MODE_SIZE (mem_mode)
				    : -GET_MODE_SIZE (mem_mode)));

    case POST_INC:
    case POST_DEC:
    case PRE_MODIFY:
    case POST_MODIFY:
      return cleanup_auto_inc_dec (code == PRE_MODIFY
				   ? XEXP (x, 1) : XEXP (x, 0),
				   mem_mode);

    default:
      break;
    }

  /* Copy the various flags, fields, and other information.  We assume
     that all fields need copying, and then clear the fields that should
     not be copied.  That is the sensible default behavior, and forces
     us to explicitly document why we are *not* copying a flag.  */
  x = shallow_copy_rtx (x);

  /* We do not copy the USED flag, which is used as a mark bit during
     walks over the RTL.  */
  RTX_FLAG (x, used) = 0;

  /* We do not copy FRAME_RELATED for INSNs.  */
  if (INSN_P (x))
    RTX_FLAG (x, frame_related) = 0;

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      XEXP (x, i) = cleanup_auto_inc_dec (XEXP (x, i), mem_mode);
    else if (fmt[i] == 'E' || fmt[i] == 'V')
      {
	int j;
	XVEC (x, i) = rtvec_alloc (XVECLEN (x, i));
	for (j = 0; j < XVECLEN (x, i); j++)
	  XVECEXP (x, i, j)
	    = cleanup_auto_inc_dec (XVECEXP (src, i, j), mem_mode);
      }

#else /* !AUTO_INC_DEC */
  x = copy_rtx (x);
#endif /* !AUTO_INC_DEC */

  return x;
}

/* Auxiliary data structure for propagate_for_debug_stmt.  */

struct rtx_subst_pair
{
  rtx to;
  bool adjusted;
};

/* DATA points to an rtx_subst_pair.  Return the value that should be
   substituted.  */

static rtx
propagate_for_debug_subst (rtx from, const_rtx old_rtx, void *data)
{
  struct rtx_subst_pair *pair = (struct rtx_subst_pair *)data;

  if (!rtx_equal_p (from, old_rtx))
    return NULL_RTX;
  if (!pair->adjusted)
    {
      pair->adjusted = true;
      pair->to = cleanup_auto_inc_dec (pair->to, VOIDmode);
      pair->to = make_compound_operation (pair->to, SET);
      return pair->to;
    }
  return copy_rtx (pair->to);
}

/* Replace all the occurrences of DEST with SRC in DEBUG_INSNs between INSN
   and LAST, not including INSN, but including LAST.  Also stop at the end
   of THIS_BASIC_BLOCK.  */

void
propagate_for_debug (rtx insn, rtx last, rtx dest, rtx src,
		     basic_block this_basic_block)
{
  rtx next, loc, end = NEXT_INSN (BB_END (this_basic_block));

  struct rtx_subst_pair p;
  p.to = src;
  p.adjusted = false;

  next = NEXT_INSN (insn);
  last = NEXT_INSN (last);
  while (next != last && next != end)
    {
      insn = next;
      next = NEXT_INSN (insn);
      if (DEBUG_INSN_P (insn))
	{
	  loc = simplify_replace_fn_rtx (INSN_VAR_LOCATION_LOC (insn),
					 dest, propagate_for_debug_subst, &p);
	  if (loc == INSN_VAR_LOCATION_LOC (insn))
	    continue;
	  INSN_VAR_LOCATION_LOC (insn) = loc;
	  df_insn_rescan (insn);
	}
    }
}

/* Initialize DEBUG to an empty list, and clear USED, if given.  */
void
dead_debug_init (struct dead_debug *debug, bitmap used)
{
  debug->head = NULL;
  debug->used = used;
  debug->to_rescan = NULL;
  if (used)
    bitmap_clear (used);
}

/* Reset all debug uses in HEAD, and clear DEBUG->to_rescan bits of
   each reset insn.  DEBUG is not otherwise modified.  If HEAD is
   DEBUG->head, DEBUG->head will be set to NULL at the end.
   Otherwise, entries from DEBUG->head that pertain to reset insns
   will be removed, and only then rescanned.  */

static void
dead_debug_reset_uses (struct dead_debug *debug, struct dead_debug_use *head)
{
  bool got_head = (debug->head == head);
  bitmap rescan;
  struct dead_debug_use **tailp = &debug->head;
  struct dead_debug_use *cur;
  bitmap_iterator bi;
  unsigned int uid;

  if (got_head)
    rescan = NULL;
  else
    rescan = BITMAP_ALLOC (NULL);

  while (head)
    {
      struct dead_debug_use *next = head->next;
      rtx insn;

      insn = DF_REF_INSN (head->use);
      if (!next || DF_REF_INSN (next->use) != insn)
	{
	  INSN_VAR_LOCATION_LOC (insn) = gen_rtx_UNKNOWN_VAR_LOC ();
	  if (got_head)
	    df_insn_rescan_debug_internal (insn);
	  else
	    bitmap_set_bit (rescan, INSN_UID (insn));
	  if (debug->to_rescan)
	    bitmap_clear_bit (debug->to_rescan, INSN_UID (insn));
	}
      XDELETE (head);
      head = next;
    }

  if (got_head)
    {
      debug->head = NULL;
      return;
    }

  while ((cur = *tailp))
    if (bitmap_bit_p (rescan, INSN_UID (DF_REF_INSN (cur->use))))
      {
	*tailp = cur->next;
	XDELETE (cur);
      }
    else
      tailp = &cur->next;

  EXECUTE_IF_SET_IN_BITMAP (rescan, 0, uid, bi)
    {
      struct df_insn_info *insn_info = DF_INSN_UID_SAFE_GET (uid);
      if (insn_info)
	df_insn_rescan_debug_internal (insn_info->insn);
    }

  BITMAP_FREE (rescan);
}

/* Reset all debug insns with pending uses.  Release the bitmap in it,
   unless it is USED.  USED must be the same bitmap passed to
   dead_debug_init.  */
void
dead_debug_finish (struct dead_debug *debug, bitmap used)
{
  if (debug->used != used)
    BITMAP_FREE (debug->used);

  dead_debug_reset_uses (debug, debug->head);

  if (debug->to_rescan)
    {
      bitmap_iterator bi;
      unsigned int uid;

      EXECUTE_IF_SET_IN_BITMAP (debug->to_rescan, 0, uid, bi)
	{
	  struct df_insn_info *insn_info = DF_INSN_UID_SAFE_GET (uid);
	  if (insn_info)
	    df_insn_rescan (insn_info->insn);
	}
      BITMAP_FREE (debug->to_rescan);
    }
}

/* Add USE to DEBUG.  It must be a dead reference to UREGNO in a debug
   insn.  Create a bitmap for DEBUG as needed.  */
void
dead_debug_add (struct dead_debug *debug, df_ref use, unsigned int uregno)
{
  struct dead_debug_use *newddu = XNEW (struct dead_debug_use);

  newddu->use = use;
  newddu->next = debug->head;
  debug->head = newddu;

  if (!debug->used)
    debug->used = BITMAP_ALLOC (NULL);

  /* ??? If we dealt with split multi-registers below, we should set
     all registers for the used mode in case of hardware
     registers.  */
  bitmap_set_bit (debug->used, uregno);
}

/* If UREGNO is referenced by any entry in DEBUG, emit a debug insn
   before or after INSN (depending on WHERE), that binds a debug temp
   to the widest-mode use of UREGNO, if WHERE is *_WITH_REG, or the
   value stored in UREGNO by INSN otherwise, and replace all uses of
   UREGNO in DEBUG with uses of the debug temp.  INSN must be where
   UREGNO dies, if WHERE is *_BEFORE_*, or where it is set otherwise.
   Return the number of debug insns emitted.  */
int
dead_debug_insert_temp (struct dead_debug *debug, unsigned int uregno,
			rtx insn, enum debug_temp_where where)
{
  struct dead_debug_use **tailp = &debug->head;
  struct dead_debug_use *cur;
  struct dead_debug_use *uses = NULL;
  struct dead_debug_use **usesp = &uses;
  rtx reg = NULL;
  rtx breg;
  rtx dval;
  rtx bind;

  if (!debug->used || !bitmap_clear_bit (debug->used, uregno))
    return 0;

  /* Move all uses of uregno from debug->head to uses, setting mode to
     the widest referenced mode.  */
  while ((cur = *tailp))
    {
      if (DF_REF_REGNO (cur->use) == uregno)
	{
	  *usesp = cur;
	  usesp = &cur->next;
	  *tailp = cur->next;
	  cur->next = NULL;
	  if (!reg
	      || (GET_MODE_BITSIZE (GET_MODE (reg))
		  < GET_MODE_BITSIZE (GET_MODE (*DF_REF_REAL_LOC (cur->use)))))
	    reg = *DF_REF_REAL_LOC (cur->use);
	}
      else
	tailp = &(*tailp)->next;
    }

  /* We may have dangling bits in debug->used for registers that were part
     of a multi-register use, one component of which has been reset.  */
  if (reg == NULL)
    {
      gcc_checking_assert (!uses);
      return 0;
    }

  gcc_checking_assert (uses);

  breg = reg;
  /* Recover the expression INSN stores in REG.  */
  if (where == DEBUG_TEMP_BEFORE_WITH_VALUE)
    {
      rtx set = single_set (insn);
      rtx dest, src;

      if (set)
	{
	  dest = SET_DEST (set);
	  src = SET_SRC (set);
	  /* Lose if the REG-setting insn is a CALL.  */
	  if (GET_CODE (src) == CALL)
	    {
	      while (uses)
		{
		  cur = uses->next;
		  XDELETE (uses);
		  uses = cur;
		}
	      return 0;
	    }
	}

      /* ??? Should we try to extract it from a PARALLEL?  */
      if (!set)
	breg = NULL;
      /* Cool, it's the same REG, we can use SRC.  */
      else if (dest == reg)
	breg = cleanup_auto_inc_dec (src, VOIDmode);
      else if (REG_P (dest))
	{
	  /* Hmm...  Something's fishy, we should be setting REG here.  */
	  if (REGNO (dest) != REGNO (reg))
	    breg = NULL;
	  /* If we're not overwriting all the hardware registers that
	     setting REG in its mode would, we won't know what to bind
	     the debug temp to.  ??? We could bind the debug_expr to a
	     CONCAT or PARALLEL with the split multi-registers, and
	     replace them as we found the corresponding sets.  */
	  else if (REGNO (reg) < FIRST_PSEUDO_REGISTER
		   && (hard_regno_nregs[REGNO (reg)][GET_MODE (reg)]
		       != hard_regno_nregs[REGNO (reg)][GET_MODE (dest)]))
	    breg = NULL;
	  /* Ok, it's the same (hardware) REG, but with a different
	     mode, so SUBREG it.  */
	  else
	    breg = lowpart_subreg (GET_MODE (reg),
				   cleanup_auto_inc_dec (src, VOIDmode),
				   GET_MODE (dest));
	}
      else if (GET_CODE (dest) == SUBREG)
	{
	  /* We should be setting REG here.  Lose.  */
	  if (REGNO (SUBREG_REG (dest)) != REGNO (reg))
	    breg = NULL;
	  /* Lose if we're setting something other than the lowpart of
	     REG.  */
	  else if (!subreg_lowpart_p (dest))
	    breg = NULL;
	  /* If we're not overwriting all the hardware registers that
	     setting REG in its mode would, we won't know what to bind
	     the debug temp to.  */
	  else if (REGNO (reg) < FIRST_PSEUDO_REGISTER
		   && (hard_regno_nregs[REGNO (reg)][GET_MODE (reg)]
		       != hard_regno_nregs[REGNO (reg)][GET_MODE (dest)]))
	    breg = NULL;
	  /* Yay, we can use SRC, just adjust its mode.  */
	  else
	    breg = lowpart_subreg (GET_MODE (reg),
				   cleanup_auto_inc_dec (src, VOIDmode),
				   GET_MODE (dest));
	}
      /* Oh well, we're out of luck.  */
      else
	breg = NULL;

      /* We couldn't figure out the value stored in REG, so reset all
	 of its pending debug uses.  */
      if (!breg)
	{
	  dead_debug_reset_uses (debug, uses);
	  return 0;
	}
    }

  /* If there's a single (debug) use of an otherwise unused REG, and
     the debug use is not part of a larger expression, then it
     probably doesn't make sense to introduce a new debug temp.  */
  if (where == DEBUG_TEMP_AFTER_WITH_REG && !uses->next)
    {
      rtx next = DF_REF_INSN (uses->use);

      if (DEBUG_INSN_P (next) && reg == INSN_VAR_LOCATION_LOC (next))
	{
	  XDELETE (uses);
	  return 0;
	}
    }

  /* Create DEBUG_EXPR (and DEBUG_EXPR_DECL).  */
  dval = make_debug_expr_from_rtl (reg);

  /* Emit a debug bind insn before the insn in which reg dies.  */
  bind = gen_rtx_VAR_LOCATION (GET_MODE (reg),
			       DEBUG_EXPR_TREE_DECL (dval), breg,
			       VAR_INIT_STATUS_INITIALIZED);

  if (where == DEBUG_TEMP_AFTER_WITH_REG)
    bind = emit_debug_insn_after (bind, insn);
  else
    bind = emit_debug_insn_before (bind, insn);
  df_insn_rescan (bind);

  /* Adjust all uses.  */
  while ((cur = uses))
    {
      if (GET_MODE (*DF_REF_REAL_LOC (cur->use)) == GET_MODE (reg))
	*DF_REF_REAL_LOC (cur->use) = dval;
      else
	*DF_REF_REAL_LOC (cur->use)
	  = gen_lowpart_SUBREG (GET_MODE (*DF_REF_REAL_LOC (cur->use)), dval);
      /* ??? Should we simplify subreg of subreg?  */
      if (debug->to_rescan == NULL)
	debug->to_rescan = BITMAP_ALLOC (NULL);
      bitmap_set_bit (debug->to_rescan, INSN_UID (DF_REF_INSN (cur->use)));
      uses = cur->next;
      XDELETE (cur);
    }

  return 1;
}
