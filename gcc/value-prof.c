/* Transformations based on profile information for values.
   Copyright (C) 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "expr.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "value-prof.h"
#include "output.h"
#include "flags.h"
#include "insn-config.h"
#include "recog.h"
#include "optabs.h"

/* In this file value profile based optimizations will be placed (none are
   here just now, but they are hopefully coming soon).

   Every such optimization should add its requirements for profiled values to
   insn_values_to_profile function.  This function is called from branch_prob
   in profile.c and the requested values are instrumented by it in the first
   compilation with -fprofile-arcs.  The optimization may then read the
   gathered data in the second compilation with -fbranch-probabilities.
   The measured data is appended as REG_VALUE_PROFILE note to the instrumented
   insn.  The argument to the note consists of an EXPR_LIST where its
   members have the following meaning (from the first to the last):
   
   -- type of information gathered (HIST_TYPE*)
   -- the expression that is profiled
   -- list of counters starting from the first one.  */

static void insn_values_to_profile (rtx, unsigned *, struct histogram_value **);

/* Release the list of VALUES of length N_VALUES for that we want to measure
   histograms.  */
void
free_profiled_values (unsigned n_values ATTRIBUTE_UNUSED,
		      struct histogram_value *values)
{
  free (values);
}

/* Find values inside INSN for that we want to measure histograms and adds
   them to list VALUES (increasing the record of its length in N_VALUES).  */
static void
insn_values_to_profile (rtx insn ATTRIBUTE_UNUSED,
			unsigned *n_values ATTRIBUTE_UNUSED,
			struct histogram_value **values ATTRIBUTE_UNUSED)
{
}

/* Find list of values for that we want to measure histograms.  */
void
find_values_to_profile (unsigned *n_values, struct histogram_value **values)
{
  rtx insn;
  unsigned i;

  *n_values = 0;
  *values = NULL;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    insn_values_to_profile (insn, n_values, values);

  for (i = 0; i < *n_values; i++)
    {
      switch ((*values)[i].type)
	{
	case HIST_TYPE_INTERVAL:
	  (*values)[i].n_counters = (*values)[i].hdata.intvl.steps +
		  ((*values)[i].hdata.intvl.may_be_less ? 1 : 0) +
		  ((*values)[i].hdata.intvl.may_be_more ? 1 : 0);
	  break;

	case HIST_TYPE_POW2:
	  (*values)[i].n_counters = GET_MODE_BITSIZE ((*values)[i].mode) +
		  ((*values)[i].hdata.pow2.may_be_other ? 1 : 0);
	  break;

	case HIST_TYPE_SINGLE_VALUE:
	  (*values)[i].n_counters = 3;
	  break;

	case HIST_TYPE_CONST_DELTA:
	  (*values)[i].n_counters = 4;
	  break;

	default:
	  abort ();
	}
    }
}
