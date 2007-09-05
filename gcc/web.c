/* Web construction code for GNU compiler.
   Contributed by Jan Hubicka.
   Copyright (C) 2001, 2002, 2004, 2006, 2007
   Free Software Foundation, Inc.

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

/* Simple optimization pass that splits independent uses of each pseudo,
   increasing effectiveness of other optimizations.  The optimization can
   serve as an example of use for the dataflow module.

   We don't split registers with REG_USERVAR set unless -fmessy-debugging
   is specified, because debugging information about such split variables
   is almost unusable.

   TODO
    - Add code to keep debugging up-to-date after splitting user variable
      pseudos.  This can be done by keeping track of all the pseudos used
      for the variable and using life analysis information before reload
      to determine which one is live and, in case more than one are live,
      choose the one with the latest definition.

      Other optimization passes can benefit from the infrastructure too.

    - We may use profile information and ignore infrequent use for the
      purpose of web unifying, inserting the compensation code later to
      implement full induction variable expansion for loops (currently
      we expand only if the induction variable is dead afterward, which
      is often the case).  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "toplev.h"

#include "rtl.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "obstack.h"
#include "basic-block.h"
#include "output.h"
#include "df.h"
#include "function.h"
#include "timevar.h"
#include "tree-pass.h"


static rtx entry_register (struct web_entry *, struct df_ref *, char *);
static void replace_ref (struct df_ref *, rtx);

/* Find the root of unionfind tree (the representative of set).  */

struct web_entry *
unionfind_root (struct web_entry *element)
{
  struct web_entry *element1 = element, *element2;

  while (element->pred)
    element = element->pred;
  while (element1->pred)
    {
      element2 = element1->pred;
      element1->pred = element;
      element1 = element2;
    }
  return element;
}

/* Union sets.  
   Return true if FIRST and SECOND points to the same web entry structure and
   nothing is done.  Otherwise, return false.  */

bool
unionfind_union (struct web_entry *first, struct web_entry *second)
{
  first = unionfind_root (first);
  second = unionfind_root (second);
  if (first == second)
    return true;
  second->pred = first;
  return false;
}

/* For each use, all possible defs reaching it must come in the same
   register, union them.
   FUN is the function that does the union.  */

void
union_defs (struct df_ref *use, struct web_entry *def_entry,
 	    struct web_entry *use_entry,
 	    bool (*fun) (struct web_entry *, struct web_entry *))
{
  rtx insn = DF_REF_INSN (use);
  struct df_link *link = DF_REF_CHAIN (use);
  struct df_ref **use_link;
  struct df_ref **eq_use_link;
  struct df_ref **def_link;
  rtx set;

  if (insn)
    {
      use_link = DF_INSN_USES (insn);
      eq_use_link = DF_INSN_EQ_USES (insn);
      def_link = DF_INSN_DEFS (insn);
      set = single_set (insn);
    }
  else
    {
      use_link = NULL;
      eq_use_link = NULL;
      def_link = NULL;
      set = NULL;
    }

  /* Some instructions may use match_dup for their operands.  In case the
     operands are dead, we will assign them different pseudos, creating
     invalid instructions, so union all uses of the same operand for each
     insn.  */

  if (use_link)
    while (*use_link)
      {
	if (use != *use_link
	    && DF_REF_REAL_REG (use) == DF_REF_REAL_REG (*use_link))
	  (*fun) (use_entry + DF_REF_ID (use),
		  use_entry + DF_REF_ID (*use_link));
	use_link++;
      }

  if (eq_use_link)
    while (*eq_use_link)
      {
	if (use != *eq_use_link
	    && DF_REF_REAL_REG (use) == DF_REF_REAL_REG (*eq_use_link))
	  (*fun) (use_entry + DF_REF_ID (use),
		  use_entry + DF_REF_ID (*eq_use_link));
	eq_use_link++;
    }

  /* Recognize trivial noop moves and attempt to keep them as noop.
     While most of noop moves should be removed, we still keep some
     of them at libcall boundaries and such.  */

  if (set
      && SET_SRC (set) == DF_REF_REG (use)
      && SET_SRC (set) == SET_DEST (set))
    {
      if (def_link)
	while (*def_link)
	  {
	    if (DF_REF_REAL_REG (use) == DF_REF_REAL_REG (*def_link))
	      (*fun) (use_entry + DF_REF_ID (use),
		      def_entry + DF_REF_ID (*def_link));
	    def_link++;
	  }
    }
  while (link)
    {
      (*fun) (use_entry + DF_REF_ID (use),
	      def_entry + DF_REF_ID (link->ref));
      link = link->next;
    }

  /* A READ_WRITE use requires the corresponding def to be in the same
     register.  Find it and union.  */
  if (use->flags & DF_REF_READ_WRITE)
    {
      struct df_ref **link;

      if (DF_REF_INSN (use))
	link = DF_INSN_DEFS (DF_REF_INSN (use));
      else
	link = NULL;

      if (link)
	while (*link)
	  {
	    if (DF_REF_REAL_REG (*link) == DF_REF_REAL_REG (use))
	      (*fun) (use_entry + DF_REF_ID (use),
		      def_entry + DF_REF_ID (*link));
	    link++;
	  }
    }
}

/* Find the corresponding register for the given entry.  */

static rtx
entry_register (struct web_entry *entry, struct df_ref *ref, char *used)
{
  struct web_entry *root;
  rtx reg, newreg;

  /* Find the corresponding web and see if it has been visited.  */
  root = unionfind_root (entry);
  if (root->reg)
    return root->reg;

  /* We are seeing this web for the first time, do the assignment.  */
  reg = DF_REF_REAL_REG (ref);

  /* In case the original register is already assigned, generate new one.  */
  if (!used[REGNO (reg)])
    newreg = reg, used[REGNO (reg)] = 1;
  else if (REG_USERVAR_P (reg) && 0/*&& !flag_messy_debugging*/)
    {
      newreg = reg;
      if (dump_file)
	fprintf (dump_file,
		 "New web forced to keep reg=%i (user variable)\n",
		 REGNO (reg));
    }
  else
    {
      newreg = gen_reg_rtx (GET_MODE (reg));
      REG_USERVAR_P (newreg) = REG_USERVAR_P (reg);
      REG_POINTER (newreg) = REG_POINTER (reg);
      REG_ATTRS (newreg) = REG_ATTRS (reg);
      if (dump_file)
	fprintf (dump_file, "Web oldreg=%i newreg=%i\n", REGNO (reg),
		 REGNO (newreg));
    }

  root->reg = newreg;
  return newreg;
}

/* Replace the reference by REG.  */

static void
replace_ref (struct df_ref *ref, rtx reg)
{
  rtx oldreg = DF_REF_REAL_REG (ref);
  rtx *loc = DF_REF_REAL_LOC (ref);
  unsigned int uid = INSN_UID (DF_REF_INSN (ref));

  if (oldreg == reg)
    return;
  if (dump_file)
    fprintf (dump_file, "Updating insn %i (%i->%i)\n",
	     uid, REGNO (oldreg), REGNO (reg)); 
  *loc = reg;
  df_insn_rescan (DF_REF_INSN (ref));
}


static bool
gate_handle_web (void)
{
  return (optimize > 0 && flag_web);
}

/* Main entry point.  */

static unsigned int
web_main (void)
{
  struct web_entry *def_entry;
  struct web_entry *use_entry;
  unsigned int max = max_reg_num ();
  char *used;
  basic_block bb;
  unsigned int uses_num = 0;
  rtx insn;

  df_set_flags (DF_NO_HARD_REGS + DF_EQ_NOTES);
  df_chain_add_problem (DF_UD_CHAIN);
  df_analyze ();
  df_set_flags (DF_DEFER_INSN_RESCAN);

  /* Assign ids to the uses.  */
  FOR_ALL_BB (bb)
    FOR_BB_INSNS (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);
      if (INSN_P (insn))
	{
	  struct df_ref **use_rec;
	  for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
	    {
	      struct df_ref *use = *use_rec;
	      if (DF_REF_REGNO (use) >= FIRST_PSEUDO_REGISTER)
		DF_REF_ID (use) = uses_num++;
	    }
	  for (use_rec = DF_INSN_UID_EQ_USES (uid); *use_rec; use_rec++)
	    {
	      struct df_ref *use = *use_rec;
	      if (DF_REF_REGNO (use) >= FIRST_PSEUDO_REGISTER)
		DF_REF_ID (use) = uses_num++;
	    }
	}
    }

  /* Record the number of uses and defs at the beginning of the optimization.  */
  def_entry = XCNEWVEC (struct web_entry, DF_DEFS_TABLE_SIZE());
  used = XCNEWVEC (char, max);
  use_entry = XCNEWVEC (struct web_entry, uses_num);

  /* Produce the web.  */
  FOR_ALL_BB (bb)
    FOR_BB_INSNS (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);
      if (INSN_P (insn))
	{
	  struct df_ref **use_rec;
	  for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
	    {
	      struct df_ref *use = *use_rec;
	      if (DF_REF_REGNO (use) >= FIRST_PSEUDO_REGISTER)
		union_defs (use, def_entry, use_entry, unionfind_union);
	    }
	  for (use_rec = DF_INSN_UID_EQ_USES (uid); *use_rec; use_rec++)
	    {
	      struct df_ref *use = *use_rec;
	      if (DF_REF_REGNO (use) >= FIRST_PSEUDO_REGISTER)
		union_defs (use, def_entry, use_entry, unionfind_union);
	    }
	}
    }

  /* Update the instruction stream, allocating new registers for split pseudos
     in progress.  */
  FOR_ALL_BB (bb)
    FOR_BB_INSNS (bb, insn)
    {
      unsigned int uid = INSN_UID (insn);
      if (INSN_P (insn))
	{
	  struct df_ref **use_rec;
	  struct df_ref **def_rec;
	  for (use_rec = DF_INSN_UID_USES (uid); *use_rec; use_rec++)
	    {
	      struct df_ref *use = *use_rec;
	      if (DF_REF_REGNO (use) >= FIRST_PSEUDO_REGISTER)
		replace_ref (use, entry_register (use_entry + DF_REF_ID (use), use, used));
	    }
	  for (use_rec = DF_INSN_UID_EQ_USES (uid); *use_rec; use_rec++)
	    {
	      struct df_ref *use = *use_rec;
	      if (DF_REF_REGNO (use) >= FIRST_PSEUDO_REGISTER)
		replace_ref (use, entry_register (use_entry + DF_REF_ID (use), use, used));
	    }
	  for (def_rec = DF_INSN_UID_DEFS (uid); *def_rec; def_rec++)
	    {
	      struct df_ref *def = *def_rec;
	      if (DF_REF_REGNO (def) >= FIRST_PSEUDO_REGISTER)
		replace_ref (def, entry_register (def_entry + DF_REF_ID (def), def, used));
	    }
	}
    }

  free (def_entry);
  free (use_entry);
  free (used);
  return 0;
}

struct tree_opt_pass pass_web =
{
  "web",                                /* name */
  gate_handle_web,                      /* gate */
  web_main,		                /* execute */
  NULL,                                 /* sub */
  NULL,                                 /* next */
  0,                                    /* static_pass_number */
  TV_WEB,                               /* tv_id */
  0,                                    /* properties_required */
  0,                                    /* properties_provided */
  0,                                    /* properties_destroyed */
  0,                                    /* todo_flags_start */
  TODO_df_finish | TODO_verify_rtl_sharing | 
  TODO_dump_func,                       /* todo_flags_finish */
  'Z'                                   /* letter */
};

