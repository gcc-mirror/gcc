/* Routines for performing Temporary Expression Replacement (TER) in SSA trees.
   Copyright (C) 2003-2014 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod  <amacleod@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "gimple-pretty-print.h"
#include "bitmap.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "dumpfile.h"
#include "tree-ssa-live.h"
#include "tree-ssa-ter.h"
#include "tree-outof-ssa.h"
#include "flags.h"
#include "gimple-walk.h"


/* Temporary Expression Replacement (TER)

   Replace SSA version variables during out-of-ssa with their defining
   expression if there is only one use of the variable.

   This pass is required in order for the RTL expansion pass to see larger
   chunks of code.  This allows it to make better choices on RTL pattern
   selection.  When expand is rewritten and merged with out-of-ssa, and
   understands SSA, this should be eliminated.

   A pass is made through the function, one block at a time.  No cross block
   information is tracked.

   Variables which only have one use, and whose defining stmt is considered
   a replaceable expression (see ssa_is_replaceable_p) are tracked to see whether
   they can be replaced at their use location.

   n_12 = C * 10
   a_2 = b_5 + 6
   v_9 = a_2 * n_12

   if there are the only use of n_12 and a_2, TER will substitute in their
   expressions in v_9, and end up with:

   v_9 = (b_5 + 6) * (C * 10)

   which will then have the ssa_name assigned to regular variables, and the
   resulting code which will be passed to the expander looks something like:

   v = (b + 6) * (C * 10)


   This requires ensuring that none of the variables used by the expression
   change between the def point and where it is used.  Furthermore, if any
   of the ssa_names used in this expression are themselves replaceable, we
   have to ensure none of that expressions' arguments change either.
   Although SSA_NAMES themselves don't change, this pass is performed after
   coalescing has coalesced different SSA_NAMES together, so there could be a
   definition of an SSA_NAME which is coalesced with a use that causes a
   problem, i.e.,

   PHI b_5 = <b_8(2), b_14(1)>
   <...>
   a_2 = b_5 + 6
   b_8 = c_4 + 4
   v_9 = a_2 * n_12
   <...>

   If b_5, b_8 and b_14 are all coalesced together...
   The expression b_5 + 6 CANNOT replace the use in the statement defining v_9
   because b_8 is in fact killing the value of b_5 since they share a partition
   and will be assigned the same memory or register location.

   TER implements this but stepping through the instructions in a block and
   tracking potential expressions for replacement, and the partitions they are
   dependent on.  Expressions are represented by the SSA_NAME_VERSION of the
   DEF on the LHS of a GIMPLE_ASSIGN and the expression is the RHS.

   When a stmt is determined to be a possible replacement expression, the
   following steps are taken:

   EXPR_DECL_UID bitmap is allocated and set to the base variable UID of the
   def and any uses in the expression.  non-NULL means the expression is being
   tracked.  The UID's themselves are used to prevent TER substitution into
   accumulating sequences, i.e.,

   x = x + y
   x = x + z
   x = x + w
   etc.
   this can result in very large expressions which don't accomplish anything
   see PR tree-optimization/17549.

   PARTITION_DEPENDENCIES is another bitmap array, and it has a bit set for any
   partition which is used in the expression.  This is primarily used to remove
   an expression from the partition kill lists when a decision is made whether
   to replace it or not.  This is indexed by ssa version number as well, and
   indicates a partition number.  virtual operands are not tracked individually,
   but they are summarized by an artificial partition called VIRTUAL_PARTITION.
   This means a MAY or MUST def will kill *ALL* expressions that are dependent
   on a virtual operand.
   Note that the EXPR_DECL_UID and this bitmap represent very similar
   information, but the info in one is not easy to obtain from the other.

   KILL_LIST is yet another bitmap array, this time it is indexed by partition
   number, and represents a list of active expressions which will will no
   longer be valid if a definition into this partition takes place.

   PARTITION_IN_USE is simply a bitmap which is used to track which partitions
   currently have something in their kill list.  This is used at the end of
   a block to clear out the KILL_LIST bitmaps at the end of each block.

   NEW_REPLACEABLE_DEPENDENCIES is used as a temporary place to store
   dependencies which will be reused by the current definition. All the uses
   on an expression are processed before anything else is done. If a use is
   determined to be a replaceable expression AND the current stmt is also going
   to be replaceable, all the dependencies of this replaceable use will be
   picked up by the current stmt's expression. Rather than recreate them, they
   are simply copied here and then copied into the new expression when it is
   processed.

   a_2 = b_5 + 6
   v_8 = a_2 + c_4

   a_2's expression 'b_5 + 6' is determined to be replaceable at the use
   location. It is dependent on the partition 'b_5' is in. This is cached into
   the NEW_REPLACEABLE_DEPENDENCIES bitmap, and when v_8 is examined for
   replaceability, it is a candidate, and it is dependent on the partition
   b_5 is in *NOT* a_2, as well as c_4's partition.

   if v_8 is also replaceable:

   x_9 = v_8 * 5

   x_9 is dependent on partitions b_5, and c_4

   if a statement is found which has either of those partitions written to
   before x_9 is used, then x_9 itself is NOT replaceable.  */


/* Temporary Expression Replacement (TER) table information.  */

typedef struct temp_expr_table_d
{
  var_map map;
  bitmap *partition_dependencies;	/* Partitions expr is dependent on.  */
  bitmap replaceable_expressions;	/* Replacement expression table.  */
  bitmap *expr_decl_uids;		/* Base uids of exprs.  */
  bitmap *kill_list;			/* Expr's killed by a partition.  */
  int virtual_partition;		/* Pseudo partition for virtual ops.  */
  bitmap partition_in_use;		/* Partitions with kill entries.  */
  bitmap new_replaceable_dependencies;	/* Holding place for pending dep's.  */
  int *num_in_part;			/* # of ssa_names in a partition.  */
  int *call_cnt;			/* Call count at definition.  */
} *temp_expr_table_p;

/* Used to indicate a dependency on VDEFs.  */
#define VIRTUAL_PARTITION(table)	(table->virtual_partition)

/* A place for the many, many bitmaps we create.  */
static bitmap_obstack ter_bitmap_obstack;

#ifdef ENABLE_CHECKING
extern void debug_ter (FILE *, temp_expr_table_p);
#endif


/* Create a new TER table for MAP.  */

static temp_expr_table_p
new_temp_expr_table (var_map map)
{
  temp_expr_table_p t;
  unsigned x;

  t = XNEW (struct temp_expr_table_d);
  t->map = map;

  t->partition_dependencies = XCNEWVEC (bitmap, num_ssa_names + 1);
  t->expr_decl_uids = XCNEWVEC (bitmap, num_ssa_names + 1);
  t->kill_list = XCNEWVEC (bitmap, num_var_partitions (map) + 1);

  t->partition_in_use = BITMAP_ALLOC (&ter_bitmap_obstack);

  t->virtual_partition = num_var_partitions (map);
  t->new_replaceable_dependencies = BITMAP_ALLOC (&ter_bitmap_obstack);

  t->replaceable_expressions = NULL;
  t->num_in_part = XCNEWVEC (int, num_var_partitions (map));
  for (x = 1; x < num_ssa_names; x++)
    {
      int p;
      tree name = ssa_name (x);
      if (!name)
        continue;
      p = var_to_partition (map, name);
      if (p != NO_PARTITION)
        t->num_in_part[p]++;
    }
  t->call_cnt = XCNEWVEC (int, num_ssa_names + 1);

  return t;
}


/* Free TER table T.  If there are valid replacements, return the expression
   vector.  */

static bitmap
free_temp_expr_table (temp_expr_table_p t)
{
  bitmap ret = NULL;

#ifdef ENABLE_CHECKING
  unsigned x;
  for (x = 0; x <= num_var_partitions (t->map); x++)
    gcc_assert (!t->kill_list[x]);
  for (x = 0; x < num_ssa_names; x++)
    {
      gcc_assert (t->expr_decl_uids[x] == NULL);
      gcc_assert (t->partition_dependencies[x] == NULL);
    }
#endif

  BITMAP_FREE (t->partition_in_use);
  BITMAP_FREE (t->new_replaceable_dependencies);

  free (t->expr_decl_uids);
  free (t->kill_list);
  free (t->partition_dependencies);
  free (t->num_in_part);
  free (t->call_cnt);

  if (t->replaceable_expressions)
    ret = t->replaceable_expressions;

  free (t);
  return ret;
}


/* Return TRUE if VERSION is to be replaced by an expression in TAB.  */

static inline bool
version_to_be_replaced_p (temp_expr_table_p tab, int version)
{
  if (!tab->replaceable_expressions)
    return false;
  return bitmap_bit_p (tab->replaceable_expressions, version);
}


/* Add partition P to the list if partitions VERSION is dependent on.  TAB is
   the expression table */

static inline void
make_dependent_on_partition (temp_expr_table_p tab, int version, int p)
{
  if (!tab->partition_dependencies[version])
    tab->partition_dependencies[version] = BITMAP_ALLOC (&ter_bitmap_obstack);

  bitmap_set_bit (tab->partition_dependencies[version], p);
}


/* Add VER to the kill list for P.  TAB is the expression table */

static inline void
add_to_partition_kill_list (temp_expr_table_p tab, int p, int ver)
{
  if (!tab->kill_list[p])
    {
      tab->kill_list[p] = BITMAP_ALLOC (&ter_bitmap_obstack);
      bitmap_set_bit (tab->partition_in_use, p);
    }
  bitmap_set_bit (tab->kill_list[p], ver);
}


/* Remove VER from the partition kill list for P.  TAB is the expression
   table.  */

static inline void
remove_from_partition_kill_list (temp_expr_table_p tab, int p, int version)
{
  gcc_checking_assert (tab->kill_list[p]);
  bitmap_clear_bit (tab->kill_list[p], version);
  if (bitmap_empty_p (tab->kill_list[p]))
    {
      bitmap_clear_bit (tab->partition_in_use, p);
      BITMAP_FREE (tab->kill_list[p]);
    }
}


/* Add a dependency between the def of ssa VERSION and VAR.  If VAR is
   replaceable by an expression, add a dependence each of the elements of the
   expression.  These are contained in the new_replaceable list.  TAB is the
   expression table.  */

static void
add_dependence (temp_expr_table_p tab, int version, tree var)
{
  int i;
  bitmap_iterator bi;
  unsigned x;

  i = SSA_NAME_VERSION (var);
  if (version_to_be_replaced_p (tab, i))
    {
      if (!bitmap_empty_p (tab->new_replaceable_dependencies))
        {
	  /* Version will now be killed by a write to any partition the
	     substituted expression would have been killed by.  */
	  EXECUTE_IF_SET_IN_BITMAP (tab->new_replaceable_dependencies, 0, x, bi)
	    add_to_partition_kill_list (tab, x, version);

	  /* Rather than set partition_dependencies and in_use lists bit by
	     bit, simply OR in the new_replaceable_dependencies bits.  */
	  if (!tab->partition_dependencies[version])
	    tab->partition_dependencies[version] =
	      BITMAP_ALLOC (&ter_bitmap_obstack);
	  bitmap_ior_into (tab->partition_dependencies[version],
			   tab->new_replaceable_dependencies);
	  bitmap_ior_into (tab->partition_in_use,
			   tab->new_replaceable_dependencies);
	  /* It is only necessary to add these once.  */
	  bitmap_clear (tab->new_replaceable_dependencies);
	}
    }
  else
    {
      i = var_to_partition (tab->map, var);
      gcc_checking_assert (i != NO_PARTITION);
      gcc_checking_assert (tab->num_in_part[i] != 0);
      /* Only dependencies on ssa_names which are coalesced with something need
         to be tracked.  Partitions with containing only a single SSA_NAME
	 *cannot* have their value changed.  */
      if (tab->num_in_part[i] > 1)
        {
	  add_to_partition_kill_list (tab, i, version);
	  make_dependent_on_partition (tab, version, i);
	}
    }
}


/* This function will remove the expression for VERSION from replacement
   consideration in table TAB.  If FREE_EXPR is true, then remove the
   expression from consideration as well by freeing the decl uid bitmap.  */

static void
finished_with_expr (temp_expr_table_p tab, int version, bool free_expr)
{
  unsigned i;
  bitmap_iterator bi;

  /* Remove this expression from its dependent lists.  The partition dependence
     list is retained and transferred later to whomever uses this version.  */
  if (tab->partition_dependencies[version])
    {
      EXECUTE_IF_SET_IN_BITMAP (tab->partition_dependencies[version], 0, i, bi)
	remove_from_partition_kill_list (tab, i, version);
      BITMAP_FREE (tab->partition_dependencies[version]);
    }
  if (free_expr)
    BITMAP_FREE (tab->expr_decl_uids[version]);
}


/* Return TRUE if expression STMT is suitable for replacement.
   In addition to ssa_is_replaceable_p, require the same bb, and for -O0
   same locus and same BLOCK), Considers memory loads as replaceable if aliasing
   is available.  */

static inline bool
ter_is_replaceable_p (gimple stmt)
{

  if (ssa_is_replaceable_p (stmt))
    {
      use_operand_p use_p;
      tree def;
      gimple use_stmt;
      location_t locus1, locus2;
      tree block1, block2;

      /* Only consider definitions which have a single use.  ssa_is_replaceable_p
	 already performed this check, but the use stmt pointer is required for
	 further checks.  */
      def = SINGLE_SSA_TREE_OPERAND (stmt, SSA_OP_DEF);
      if (!single_imm_use (def, &use_p, &use_stmt))
	  return false;

      /* If the use isn't in this block, it wont be replaced either.  */
      if (gimple_bb (use_stmt) != gimple_bb (stmt))
        return false;

      locus1 = gimple_location (stmt);
      block1 = LOCATION_BLOCK (locus1);
      locus1 = LOCATION_LOCUS (locus1);

      if (gimple_code (use_stmt) == GIMPLE_PHI)
	locus2 = gimple_phi_arg_location (use_stmt, 
					  PHI_ARG_INDEX_FROM_USE (use_p));
      else
	locus2 = gimple_location (use_stmt);
      block2 = LOCATION_BLOCK (locus2);
      locus2 = LOCATION_LOCUS (locus2);

      if ((!optimize || optimize_debug)
	  && ((locus1 != UNKNOWN_LOCATION && locus1 != locus2)
	      || (block1 != NULL_TREE && block1 != block2)))
	return false;

      return true;
    }
  return false;
}


/* Create an expression entry for a replaceable expression.  */

static void
process_replaceable (temp_expr_table_p tab, gimple stmt, int call_cnt)
{
  tree var, def, basevar;
  int version;
  ssa_op_iter iter;
  bitmap def_vars, use_vars;

  gcc_checking_assert (ter_is_replaceable_p (stmt));

  def = SINGLE_SSA_TREE_OPERAND (stmt, SSA_OP_DEF);
  version = SSA_NAME_VERSION (def);
  def_vars = BITMAP_ALLOC (&ter_bitmap_obstack);

  basevar = SSA_NAME_VAR (def);
  if (basevar)
    bitmap_set_bit (def_vars, DECL_UID (basevar));

  /* Add this expression to the dependency list for each use partition.  */
  FOR_EACH_SSA_TREE_OPERAND (var, stmt, iter, SSA_OP_USE)
    {
      int var_version = SSA_NAME_VERSION (var);

      use_vars = tab->expr_decl_uids[var_version];
      add_dependence (tab, version, var);
      if (use_vars)
        {
	  bitmap_ior_into (def_vars, use_vars);
	  BITMAP_FREE (tab->expr_decl_uids[var_version]);
	}
      else if (SSA_NAME_VAR (var))
	bitmap_set_bit (def_vars, DECL_UID (SSA_NAME_VAR (var)));
    }
  tab->expr_decl_uids[version] = def_vars;

  /* If there are VUSES, add a dependence on virtual defs.  */
  if (gimple_vuse (stmt))
    {
      make_dependent_on_partition (tab, version, VIRTUAL_PARTITION (tab));
      add_to_partition_kill_list (tab, VIRTUAL_PARTITION (tab), version);
    }

  tab->call_cnt[version] = call_cnt;
}


/* This function removes any expression in TAB which is dependent on PARTITION
   from consideration, making it not replaceable.  */

static inline void
kill_expr (temp_expr_table_p tab, int partition)
{
  unsigned version;

  /* Mark every active expr dependent on this var as not replaceable.
     finished_with_expr can modify the bitmap, so we can't execute over it.  */
  while (tab->kill_list[partition])
    {
      version = bitmap_first_set_bit (tab->kill_list[partition]);
      finished_with_expr (tab, version, true);
    }

  gcc_checking_assert (!tab->kill_list[partition]);
}


/* This function kills all expressions in TAB which are dependent on virtual
   partitions.  */

static inline void
kill_virtual_exprs (temp_expr_table_p tab)
{
  kill_expr (tab, VIRTUAL_PARTITION (tab));
}


/* Mark the expression associated with VAR as replaceable, and enter
   the defining stmt into the partition_dependencies table TAB.  If
   MORE_REPLACING is true, accumulate the pending partition dependencies.  */

static void
mark_replaceable (temp_expr_table_p tab, tree var, bool more_replacing)
{
  int version = SSA_NAME_VERSION (var);

  /* Move the dependence list to the pending listpending.  */
  if (more_replacing && tab->partition_dependencies[version])
    bitmap_ior_into (tab->new_replaceable_dependencies,
		     tab->partition_dependencies[version]);

  finished_with_expr (tab, version, !more_replacing);

  /* Set the replaceable expression.
     The bitmap for this "escapes" from this file so it's allocated
     on the default obstack.  */
  if (!tab->replaceable_expressions)
    tab->replaceable_expressions = BITMAP_ALLOC (NULL);
  bitmap_set_bit (tab->replaceable_expressions, version);
}


/* Helper function for find_ssaname_in_stores.  Called via walk_tree to
   find a SSA_NAME DATA somewhere in *TP.  */

static tree
find_ssaname (tree *tp, int *walk_subtrees, void *data)
{
  tree var = (tree) data;
  if (*tp == var)
    return var;
  else if (IS_TYPE_OR_DECL_P (*tp))
    *walk_subtrees = 0;
  return NULL_TREE;
}

/* Helper function for find_replaceable_in_bb.  Return true if SSA_NAME DATA
   is used somewhere in T, which is a store in the statement.  Called via
   walk_stmt_load_store_addr_ops.  */

static bool
find_ssaname_in_store (gimple, tree, tree t, void *data)
{
  return walk_tree (&t, find_ssaname, data, NULL) != NULL_TREE;
}

/* This function processes basic block BB, and looks for variables which can
   be replaced by their expressions.  Results are stored in the table TAB.  */

static void
find_replaceable_in_bb (temp_expr_table_p tab, basic_block bb)
{
  gimple_stmt_iterator bsi;
  gimple stmt;
  tree def, use, fndecl;
  int partition;
  var_map map = tab->map;
  ssa_op_iter iter;
  bool stmt_replaceable;
  int cur_call_cnt = 0;

  for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
    {
      stmt = gsi_stmt (bsi);

      if (is_gimple_debug (stmt))
	continue;

      stmt_replaceable = ter_is_replaceable_p (stmt);

      /* Determine if this stmt finishes an existing expression.  */
      FOR_EACH_SSA_TREE_OPERAND (use, stmt, iter, SSA_OP_USE)
	{
	  unsigned ver = SSA_NAME_VERSION (use);

	  /* If this use is a potential replacement variable, process it.  */
	  if (tab->expr_decl_uids[ver])
	    {
	      bool same_root_var = false;
	      ssa_op_iter iter2;
	      bitmap vars = tab->expr_decl_uids[ver];

	      /* See if the root variables are the same.  If they are, we
		 do not want to do the replacement to avoid problems with
		 code size, see PR tree-optimization/17549.  */
	      if (!bitmap_empty_p (vars))
		FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter2, SSA_OP_DEF)
		  {
		    if (SSA_NAME_VAR (def)
			&& bitmap_bit_p (vars, DECL_UID (SSA_NAME_VAR (def))))
		      {
			same_root_var = true;
			break;
		      }
		  }

	      /* If the stmt does a memory store and the replacement
	         is a load aliasing it avoid creating overlapping
		 assignments which we cannot expand correctly.  */
	      if (gimple_vdef (stmt))
		{
		  gimple def_stmt = SSA_NAME_DEF_STMT (use);
		  while (is_gimple_assign (def_stmt)
			 && gimple_assign_rhs_code (def_stmt) == SSA_NAME)
		    def_stmt
		      = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (def_stmt));
		  if (gimple_vuse (def_stmt)
		      && gimple_assign_single_p (def_stmt)
		      && stmt_may_clobber_ref_p (stmt,
						 gimple_assign_rhs1 (def_stmt)))
		    {
		      /* For calls, it is not a problem if USE is among
			 call's arguments or say OBJ_TYPE_REF argument,
			 all those necessarily need to be evaluated before
			 the call that may clobber the memory.  But if
			 LHS of the call refers to USE, expansion might
			 evaluate it after the call, prevent TER in that
			 case.
			 For inline asm, allow TER of loads into input
			 arguments, but disallow TER for USEs that occur
			 somewhere in outputs.  */
		      if (is_gimple_call (stmt)
			  || gimple_code (stmt) == GIMPLE_ASM)
			{
			  if (walk_stmt_load_store_ops (stmt, use, NULL,
							find_ssaname_in_store))
			    same_root_var = true;
			}
		      else
			same_root_var = true;
		    }
		}

	      /* Mark expression as replaceable unless stmt is volatile, or the
		 def variable has the same root variable as something in the
		 substitution list, or the def and use span a call such that
		 we'll expand lifetimes across a call.  */
	      if (gimple_has_volatile_ops (stmt) || same_root_var
		  || (tab->call_cnt[ver] != cur_call_cnt
		      && SINGLE_SSA_USE_OPERAND (SSA_NAME_DEF_STMT (use), SSA_OP_USE)
			 == NULL_USE_OPERAND_P))
		finished_with_expr (tab, ver, true);
	      else
		mark_replaceable (tab, use, stmt_replaceable);
	    }
	}

      /* Next, see if this stmt kills off an active expression.  */
      FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_DEF)
	{
	  partition = var_to_partition (map, def);
	  if (partition != NO_PARTITION && tab->kill_list[partition])
	    kill_expr (tab, partition);
	}

      /* Increment counter if this is a non BUILT_IN call. We allow
	 replacement over BUILT_IN calls since many will expand to inline
	 insns instead of a true call.  */
      if (is_gimple_call (stmt)
	  && !((fndecl = gimple_call_fndecl (stmt))
	       && DECL_BUILT_IN (fndecl)))
	cur_call_cnt++;

      /* Now see if we are creating a new expression or not.  */
      if (stmt_replaceable)
	process_replaceable (tab, stmt, cur_call_cnt);

      /* Free any unused dependency lists.  */
      bitmap_clear (tab->new_replaceable_dependencies);

      /* A V_{MAY,MUST}_DEF kills any expression using a virtual operand,
	 including the current stmt.  */
      if (gimple_vdef (stmt))
        kill_virtual_exprs (tab);
    }
}


/* This function is the driver routine for replacement of temporary expressions
   in the SSA->normal phase, operating on MAP.  If there are replaceable
   expressions, a table is returned which maps SSA versions to the
   expressions they should be replaced with.  A NULL_TREE indicates no
   replacement should take place.  If there are no replacements at all,
   NULL is returned by the function, otherwise an expression vector indexed
   by SSA_NAME version numbers.  */

bitmap
find_replaceable_exprs (var_map map)
{
  basic_block bb;
  temp_expr_table_p table;
  bitmap ret;

  bitmap_obstack_initialize (&ter_bitmap_obstack);
  table = new_temp_expr_table (map);
  FOR_EACH_BB_FN (bb, cfun)
    {
      find_replaceable_in_bb (table, bb);
      gcc_checking_assert (bitmap_empty_p (table->partition_in_use));
    }
  ret = free_temp_expr_table (table);
  bitmap_obstack_release (&ter_bitmap_obstack);
  return ret;
}

/* Dump TER expression table EXPR to file F.  */

void
dump_replaceable_exprs (FILE *f, bitmap expr)
{
  tree var;
  unsigned x;

  fprintf (f, "\nReplacing Expressions\n");
  for (x = 0; x < num_ssa_names; x++)
    if (bitmap_bit_p (expr, x))
      {
	var = ssa_name (x);
	print_generic_expr (f, var, TDF_SLIM);
	fprintf (f, " replace with --> ");
	print_gimple_stmt (f, SSA_NAME_DEF_STMT (var), 0, TDF_SLIM);
	fprintf (f, "\n");
      }
  fprintf (f, "\n");
}


#ifdef ENABLE_CHECKING
/* Dump the status of the various tables in the expression table.  This is used
   exclusively to debug TER.  F is the place to send debug info and T is the
   table being debugged.  */

DEBUG_FUNCTION void
debug_ter (FILE *f, temp_expr_table_p t)
{
  unsigned x, y;
  bitmap_iterator bi;

  fprintf (f, "\nDumping current state of TER\n virtual partition = %d\n",
	   VIRTUAL_PARTITION (t));
  if (t->replaceable_expressions)
    dump_replaceable_exprs (f, t->replaceable_expressions);
  fprintf (f, "Currently tracking the following expressions:\n");

  for (x = 1; x < num_ssa_names; x++)
    if (t->expr_decl_uids[x])
      {
        print_generic_expr (f, ssa_name (x), TDF_SLIM);
        fprintf (f, " dep-parts : ");
	if (t->partition_dependencies[x]
	    && !bitmap_empty_p (t->partition_dependencies[x]))
	  {
	    EXECUTE_IF_SET_IN_BITMAP (t->partition_dependencies[x], 0, y, bi)
	      fprintf (f, "P%d ",y);
	  }
	fprintf (f, "   basedecls: ");
	EXECUTE_IF_SET_IN_BITMAP (t->expr_decl_uids[x], 0, y, bi)
	  fprintf (f, "%d ",y);
	fprintf (f, "   call_cnt : %d",t->call_cnt[x]);
	fprintf (f, "\n");
      }

  bitmap_print (f, t->partition_in_use, "Partitions in use ",
		"\npartition KILL lists:\n");

  for (x = 0; x <= num_var_partitions (t->map); x++)
    if (t->kill_list[x])
      {
        fprintf (f, "Partition %d : ", x);
	EXECUTE_IF_SET_IN_BITMAP (t->kill_list[x], 0, y, bi)
	  fprintf (f, "_%d ",y);
      }

  fprintf (f, "\n----------\n");
}
#endif
