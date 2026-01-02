/* Tree based points-to analysis
   Copyright (C) 2005-2026 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

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
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "tree-pretty-print.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "stmt.h"
#include "gimple-iterator.h"
#include "tree-into-ssa.h"
#include "tree-dfa.h"
#include "gimple-walk.h"
#include "varasm.h"
#include "stringpool.h"
#include "attribs.h"
#include "tree-ssa.h"
#include "tree-cfg.h"
#include "gimple-range.h"
#include "ipa-modref-tree.h"
#include "ipa-modref.h"
#include "attr-fnspec.h"

#include "tree-ssa-structalias.h"
#include "pta-andersen.h"
#include "gimple-ssa-pta-constraints.h"

/* The idea behind this analyzer is to generate set constraints from the
   program, then solve the resulting constraints in order to generate the
   points-to sets.

   Set constraints are a way of modeling program analysis problems that
   involve sets.  They consist of an inclusion constraint language,
   describing the variables (each variable is a set) and operations that
   are involved on the variables, and a set of rules that derive facts
   from these operations.  To solve a system of set constraints, you derive
   all possible facts under the rules, which gives you the correct sets
   as a consequence.

   See  "Efficient Field-sensitive pointer analysis for C" by "David
   J. Pearce and Paul H. J. Kelly and Chris Hankin", at
   http://citeseer.ist.psu.edu/pearce04efficient.html

   Also see "Ultra-fast Aliasing Analysis using CLA: A Million Lines
   of C Code in a Second" by "Nevin Heintze and Olivier Tardieu" at
   http://citeseer.ist.psu.edu/heintze01ultrafast.html

   There are three types of real constraint expressions, DEREF,
   ADDRESSOF, and SCALAR.  Each constraint expression consists
   of a constraint type, a variable, and an offset.

   SCALAR is a constraint expression type used to represent x, whether
   it appears on the LHS or the RHS of a statement.
   DEREF is a constraint expression type used to represent *x, whether
   it appears on the LHS or the RHS of a statement.
   ADDRESSOF is a constraint expression used to represent &x, whether
   it appears on the LHS or the RHS of a statement.

   Each pointer variable in the program is assigned an integer id, and
   each field of a structure variable is assigned an integer id as well.

   Structure variables are linked to their list of fields through a "next
   field" in each variable that points to the next field in offset
   order.
   Each variable for a structure field has

   1. "size", that tells the size in bits of that field.
   2. "fullsize", that tells the size in bits of the entire structure.
   3. "offset", that tells the offset in bits from the beginning of the
   structure to this field.

   Thus,
   struct f
   {
     int a;
     int b;
   } foo;
   int *bar;

   looks like

   foo.a -> id 1, size 32, offset 0, fullsize 64, next foo.b
   foo.b -> id 2, size 32, offset 32, fullsize 64, next NULL
   bar -> id 3, size 32, offset 0, fullsize 32, next NULL


  In order to solve the system of set constraints, the following is
  done:

  1. Each constraint variable x has a solution set associated with it,
  Sol(x).

  2. Constraints are separated into direct, copy, and complex.
  Direct constraints are ADDRESSOF constraints that require no extra
  processing, such as P = &Q
  Copy constraints are those of the form P = Q.
  Complex constraints are all the constraints involving dereferences
  and offsets (including offsetted copies).

  3. All direct constraints of the form P = &Q are processed, such
  that Q is added to Sol(P)

  4. All complex constraints for a given constraint variable are stored in a
  linked list attached to that variable's node.

  5. A directed graph is built out of the copy constraints. Each
  constraint variable is a node in the graph, and an edge from
  Q to P is added for each copy constraint of the form P = Q

  6. The graph is then walked, and solution sets are
  propagated along the copy edges, such that an edge from Q to P
  causes Sol(P) <- Sol(P) union Sol(Q).

  7.  As we visit each node, all complex constraints associated with
  that node are processed by adding appropriate copy edges to the graph, or the
  appropriate variables to the solution set.

  8. The process of walking the graph is iterated until no solution
  sets change.

  Prior to walking the graph in steps 6 and 7, We perform static
  cycle elimination on the constraint graph, as well
  as off-line variable substitution.

  TODO: Adding offsets to pointer-to-structures can be handled (IE not punted
  on and turned into anything), but isn't.  You can just see what offset
  inside the pointed-to struct it's going to access.

  TODO: Constant bounded arrays can be handled as if they were structs of the
  same number of elements.

  TODO: Modeling heap and incoming pointers becomes much better if we
  add fields to them as we discover them, which we could do.

  TODO: We could handle unions, but to be honest, it's probably not
  worth the pain or slowdown.  */

/* IPA-PTA optimizations possible.

   When the indirect function called is ANYTHING we can add disambiguation
   based on the function signatures (or simply the parameter count which
   is the varinfo size).  We also do not need to consider functions that
   do not have their address taken.

   The is_global_var bit which marks escape points is overly conservative
   in IPA mode.  Split it to is_escape_point and is_global_var - only
   externally visible globals are escape points in IPA mode.
   There is now is_ipa_escape_point but this is only used in a few
   selected places.

   The way we introduce DECL_PT_UID to avoid fixing up all points-to
   sets in the translation unit when we copy a DECL during inlining
   pessimizes precision.  The advantage is that the DECL_PT_UID keeps
   compile-time and memory usage overhead low - the points-to sets
   do not grow or get unshared as they would during a fixup phase.
   An alternative solution is to delay IPA PTA until after all
   inlining transformations have been applied.

   The way we propagate clobber/use information isn't optimized.
   It should use a new complex constraint that properly filters
   out local variables of the callee (though that would make
   the sets invalid after inlining).  OTOH we might as well
   admit defeat to WHOPR and simply do all the clobber/use analysis
   and propagation after PTA finished but before we threw away
   points-to information for memory variables.  WHOPR and PTA
   do not play along well anyway - the whole constraint solving
   would need to be done in WPA phase and it will be very interesting
   to apply the results to local SSA names during LTRANS phase.

   We probably should compute a per-function unit-ESCAPE solution
   propagating it simply like the clobber / uses solutions.  The
   solution can go alongside the non-IPA escaped solution and be
   used to query which vars escape the unit through a function.
   This is also required to make the escaped-HEAP trick work in IPA mode.

   We never put function decls in points-to sets so we do not
   keep the set of called functions for indirect calls.

   And probably more.  */

using namespace pointer_analysis;

/* Pool of variable info structures.  */
static object_allocator<variable_info> variable_info_pool
  ("Variable info pool");

/* Map varinfo to final pt_solution.  */
static hash_map<varinfo_t, pt_solution *> *final_solutions;
static struct obstack final_solutions_obstack;


namespace pointer_analysis {

bool use_field_sensitive = true;
int in_ipa_mode = 0;

/* Used for points-to sets.  */
bitmap_obstack pta_obstack;

/* Used for oldsolution members of variables.  */
bitmap_obstack oldpta_obstack;

/* Table of variable info structures for constraint variables.
   Indexed directly by variable info id.  */
vec<varinfo_t> varmap;

/* List of constraints that we use to build the constraint graph from.  */
vec<constraint_t> constraints;

/* The representative variable for a variable.  The points-to solution for a
   var can be found in its rep.  Trivially, a var can be its own rep.

   The solver provides this array once it is done solving.  */
unsigned int *var_rep;

struct constraint_stats stats;

/* Find the first varinfo in the same variable as START that overlaps with
   OFFSET.  Return NULL if we can't find one.  */

varinfo_t
first_vi_for_offset (varinfo_t start, unsigned HOST_WIDE_INT offset)
{
  /* If the offset is outside of the variable, bail out.  */
  if (offset >= start->fullsize)
    return NULL;

  /* If we cannot reach offset from start, lookup the first field
     and start from there.  */
  if (start->offset > offset)
    start = get_varinfo (start->head);

  while (start)
    {
      /* We may not find a variable in the field list with the actual
	 offset when we have glommed a structure to a variable.
	 In that case, however, offset should still be within the size
	 of the variable.  */
      if (offset >= start->offset
	  && (offset - start->offset) < start->size)
	return start;

      start = vi_next (start);
    }

  return NULL;
}

/* Find the first varinfo in the same variable as START that overlaps with
   OFFSET.  If there is no such varinfo the varinfo directly preceding
   OFFSET is returned.  */

varinfo_t
first_or_preceding_vi_for_offset (varinfo_t start,
				  unsigned HOST_WIDE_INT offset)
{
  /* If we cannot reach offset from start, lookup the first field
     and start from there.  */
  if (start->offset > offset)
    start = get_varinfo (start->head);

  /* We may not find a variable in the field list with the actual
     offset when we have glommed a structure to a variable.
     In that case, however, offset should still be within the size
     of the variable.
     If we got beyond the offset we look for return the field
     directly preceding offset which may be the last field.  */
  while (start->next
	 && offset >= start->offset
	 && !((offset - start->offset) < start->size))
    start = vi_next (start);

  return start;
}

/* Determine global memory access of call STMT and update
   WRITES_GLOBAL_MEMORY, READS_GLOBAL_MEMORY and USES_GLOBAL_MEMORY.  */

void
determine_global_memory_access (gcall *stmt,
				bool *writes_global_memory,
				bool *reads_global_memory,
				bool *uses_global_memory)
{
  tree callee;
  cgraph_node *node;
  modref_summary *summary;

  /* We need to detrmine reads to set uses.  */
  gcc_assert (!uses_global_memory || reads_global_memory);

  if ((callee = gimple_call_fndecl (stmt)) != NULL_TREE
      && (node = cgraph_node::get (callee)) != NULL
      && (summary = get_modref_function_summary (node)))
    {
      if (writes_global_memory && *writes_global_memory)
	*writes_global_memory = summary->global_memory_written;
      if (reads_global_memory && *reads_global_memory)
	*reads_global_memory = summary->global_memory_read;
      if (reads_global_memory && uses_global_memory
	  && !summary->calls_interposable
	  && !*reads_global_memory && node->binds_to_current_def_p ())
	*uses_global_memory = false;
    }
  if ((writes_global_memory && *writes_global_memory)
      || (uses_global_memory && *uses_global_memory)
      || (reads_global_memory && *reads_global_memory))
    {
      attr_fnspec fnspec = gimple_call_fnspec (stmt);
      if (fnspec.known_p ())
	{
	  if (writes_global_memory
	      && !fnspec.global_memory_written_p ())
	    *writes_global_memory = false;
	  if (reads_global_memory && !fnspec.global_memory_read_p ())
	    {
	      *reads_global_memory = false;
	      if (uses_global_memory)
		*uses_global_memory = false;
	    }
	}
    }
}

/* Return true if FNDECL may be part of another lto partition.  */

bool
fndecl_maybe_in_other_partition (tree fndecl)
{
  cgraph_node *fn_node = cgraph_node::get (fndecl);
  if (fn_node == NULL)
    return true;

  return fn_node->in_other_partition;
}

/* Return a new variable info structure consisting for a variable
   named NAME, and using constraint graph node NODE.  Append it
   to the vector of variable info structures.  */

varinfo_t
new_var_info (tree t, const char *name, bool add_id)
{
  unsigned index = varmap.length ();
  varinfo_t ret = variable_info_pool.allocate ();

  if (dump_file && add_id)
    {
      char *tempname = xasprintf ("%s(%d)", name, index);
      name = ggc_strdup (tempname);
      free (tempname);
    }

  ret->id = index;
  ret->name = name;
  ret->decl = t;
  /* Vars without decl are artificial and do not have sub-variables.  */
  ret->is_artificial_var = (t == NULL_TREE);
  ret->is_special_var = false;
  ret->is_unknown_size_var = false;
  ret->is_full_var = (t == NULL_TREE);
  ret->is_heap_var = false;
  ret->may_have_pointers = true;
  ret->only_restrict_pointers = false;
  ret->is_restrict_var = false;
  ret->ruid = 0;
  ret->is_global_var = (t == NULL_TREE);
  ret->is_ipa_escape_point = false;
  ret->is_fn_info = false;
  ret->address_taken = false;
  if (t && DECL_P (t))
    ret->is_global_var = (is_global_var (t)
			  /* We have to treat even local register variables
			     as escape points.  */
			  || (VAR_P (t) && DECL_HARD_REGISTER (t)));
  ret->is_reg_var = (t && TREE_CODE (t) == SSA_NAME);
  ret->solution = BITMAP_ALLOC (&pta_obstack);
  ret->oldsolution = NULL;
  ret->next = 0;
  ret->shadow_var_uid = 0;
  ret->head = ret->id;

  stats.total_vars++;

  varmap.safe_push (ret);

  return ret;
}

/* Print out constraint C to FILE.  */

void
dump_constraint (FILE *file, constraint_t c)
{
  if (c->lhs.type == ADDRESSOF)
    fprintf (file, "&");
  else if (c->lhs.type == DEREF)
    fprintf (file, "*");
  if (dump_file)
    fprintf (file, "%s", get_varinfo (c->lhs.var)->name);
  else
    fprintf (file, "V%d", c->lhs.var);
  if (c->lhs.offset == UNKNOWN_OFFSET)
    fprintf (file, " + UNKNOWN");
  else if (c->lhs.offset != 0)
    fprintf (file, " + " HOST_WIDE_INT_PRINT_DEC, c->lhs.offset);
  fprintf (file, " = ");
  if (c->rhs.type == ADDRESSOF)
    fprintf (file, "&");
  else if (c->rhs.type == DEREF)
    fprintf (file, "*");
  if (dump_file)
    fprintf (file, "%s", get_varinfo (c->rhs.var)->name);
  else
    fprintf (file, "V%d", c->rhs.var);
  if (c->rhs.offset == UNKNOWN_OFFSET)
    fprintf (file, " + UNKNOWN");
  else if (c->rhs.offset != 0)
    fprintf (file, " + " HOST_WIDE_INT_PRINT_DEC, c->rhs.offset);
}

/* Print out constraint C to stderr.  */

DEBUG_FUNCTION void
debug_constraint (constraint_t c)
{
  dump_constraint (stderr, c);
  fprintf (stderr, "\n");
}

/* Print out all constraints to FILE.  */

void
dump_constraints (FILE *file, int from)
{
  int i;
  constraint_t c;
  for (i = from; constraints.iterate (i, &c); i++)
    if (c)
      {
	dump_constraint (file, c);
	fprintf (file, "\n");
      }
}

/* Print out all constraints to stderr.  */

DEBUG_FUNCTION void
debug_constraints (void)
{
  dump_constraints (stderr, 0);
}

/* Print out the points-to solution for VAR to FILE.  */

void
dump_solution_for_var (FILE *file, unsigned int var)
{
  varinfo_t vi = get_varinfo (var);
  unsigned int i;
  bitmap_iterator bi;

  /* Dump the solution for unified vars anyway, this avoids difficulties
     in scanning dumps in the testsuite.  */
  fprintf (file, "%s = { ", vi->name);
  vi = get_varinfo (var_rep[var]);
  EXECUTE_IF_SET_IN_BITMAP (vi->solution, 0, i, bi)
    fprintf (file, "%s ", get_varinfo (i)->name);
  fprintf (file, "}");

  /* But note when the variable was unified.  */
  if (vi->id != var)
    fprintf (file, " same as %s", vi->name);

  fprintf (file, "\n");
}

/* Print the points-to solution for VAR to stderr.  */

DEBUG_FUNCTION void
debug_solution_for_var (unsigned int var)
{
  dump_solution_for_var (stderr, var);
}

/* Dump stats information to OUTFILE.  */

void
dump_sa_stats (FILE *outfile)
{
  fprintf (outfile, "Points-to Stats:\n");
  fprintf (outfile, "Total vars:               %d\n", stats.total_vars);
  fprintf (outfile, "Non-pointer vars:          %d\n",
	   stats.nonpointer_vars);
  fprintf (outfile, "Statically unified vars:  %d\n",
	   stats.unified_vars_static);
  fprintf (outfile, "Dynamically unified vars: %d\n",
	   stats.unified_vars_dynamic);
  fprintf (outfile, "Iterations:               %d\n", stats.iterations);
  fprintf (outfile, "Number of edges:          %d\n", stats.num_edges);
  fprintf (outfile, "Number of implicit edges: %d\n",
	   stats.num_implicit_edges);
  fprintf (outfile, "Number of avoided edges: %d\n",
	   stats.num_avoided_edges);
}

/* Dump points-to information to OUTFILE.  */

void
dump_sa_points_to_info (FILE *outfile)
{
  fprintf (outfile, "\nPoints-to sets\n\n");

  for (unsigned i = 1; i < varmap.length (); i++)
    {
      varinfo_t vi = get_varinfo (i);
      if (!vi->may_have_pointers)
	continue;
      dump_solution_for_var (outfile, i);
    }
}


/* Debug points-to information to stderr.  */

DEBUG_FUNCTION void
debug_sa_points_to_info (void)
{
  dump_sa_points_to_info (stderr);
}

/* Dump varinfo VI to FILE.  */

void
dump_varinfo (FILE *file, varinfo_t vi)
{
  if (vi == NULL)
    return;

  fprintf (file, "%u: %s\n", vi->id, vi->name);

  const char *sep = " ";
  if (vi->is_artificial_var)
    fprintf (file, "%sartificial", sep);
  if (vi->is_special_var)
    fprintf (file, "%sspecial", sep);
  if (vi->is_unknown_size_var)
    fprintf (file, "%sunknown-size", sep);
  if (vi->is_full_var)
    fprintf (file, "%sfull", sep);
  if (vi->is_heap_var)
    fprintf (file, "%sheap", sep);
  if (vi->may_have_pointers)
    fprintf (file, "%smay-have-pointers", sep);
  if (vi->only_restrict_pointers)
    fprintf (file, "%sonly-restrict-pointers", sep);
  if (vi->is_restrict_var)
    fprintf (file, "%sis-restrict-var", sep);
  if (vi->is_global_var)
    fprintf (file, "%sglobal", sep);
  if (vi->is_ipa_escape_point)
    fprintf (file, "%sipa-escape-point", sep);
  if (vi->is_fn_info)
    fprintf (file, "%sfn-info", sep);
  if (vi->ruid)
    fprintf (file, "%srestrict-uid:%u", sep, vi->ruid);
  if (vi->next)
    fprintf (file, "%snext:%u", sep, vi->next);
  if (vi->head != vi->id)
    fprintf (file, "%shead:%u", sep, vi->head);
  if (vi->offset)
    fprintf (file, "%soffset:" HOST_WIDE_INT_PRINT_DEC, sep, vi->offset);
  if (vi->size != ~HOST_WIDE_INT_0U)
    fprintf (file, "%ssize:" HOST_WIDE_INT_PRINT_DEC, sep, vi->size);
  if (vi->fullsize != ~HOST_WIDE_INT_0U && vi->fullsize != vi->size)
    fprintf (file, "%sfullsize:" HOST_WIDE_INT_PRINT_DEC, sep,
	     vi->fullsize);
  fprintf (file, "\n");

  if (vi->solution && !bitmap_empty_p (vi->solution))
    {
      bitmap_iterator bi;
      unsigned i;
      fprintf (file, " solution: {");
      EXECUTE_IF_SET_IN_BITMAP (vi->solution, 0, i, bi)
	fprintf (file, " %u", i);
      fprintf (file, " }\n");
    }

  if (vi->oldsolution && !bitmap_empty_p (vi->oldsolution)
      && !bitmap_equal_p (vi->solution, vi->oldsolution))
    {
      bitmap_iterator bi;
      unsigned i;
      fprintf (file, " oldsolution: {");
      EXECUTE_IF_SET_IN_BITMAP (vi->oldsolution, 0, i, bi)
	fprintf (file, " %u", i);
      fprintf (file, " }\n");
    }
}

/* Dump varinfo VI to stderr.  */

DEBUG_FUNCTION void
debug_varinfo (varinfo_t vi)
{
  dump_varinfo (stderr, vi);
}

/* Dump varmap to FILE.  */

void
dump_varmap (FILE *file)
{
  if (varmap.length () == 0)
    return;

  fprintf (file, "variables:\n");

  for (unsigned int i = 0; i < varmap.length (); ++i)
    {
      varinfo_t vi = get_varinfo (i);
      dump_varinfo (file, vi);
    }

  fprintf (file, "\n");
}

/* Dump varmap to stderr.  */

DEBUG_FUNCTION void
debug_varmap (void)
{
  dump_varmap (stderr);
}

} // namespace pointer_analysis


/* Structure used to put solution bitmaps in a hashtable so they can
   be shared among variables with the same points-to set.  */

typedef struct shared_bitmap_info
{
  bitmap pt_vars;
  hashval_t hashcode;
} *shared_bitmap_info_t;
typedef const struct shared_bitmap_info *const_shared_bitmap_info_t;

/* Shared_bitmap hashtable helpers.  */

struct shared_bitmap_hasher : free_ptr_hash <shared_bitmap_info>
{
  static inline hashval_t hash (const shared_bitmap_info *);
  static inline bool equal (const shared_bitmap_info *,
			    const shared_bitmap_info *);
};

/* Hash function for a shared_bitmap_info_t.  */

inline hashval_t
shared_bitmap_hasher::hash (const shared_bitmap_info *bi)
{
  return bi->hashcode;
}

/* Equality function for two shared_bitmap_info_t's.  */

inline bool
shared_bitmap_hasher::equal (const shared_bitmap_info *sbi1,
			     const shared_bitmap_info *sbi2)
{
  return bitmap_equal_p (sbi1->pt_vars, sbi2->pt_vars);
}

/* Shared_bitmap hashtable.  */

static hash_table<shared_bitmap_hasher> *shared_bitmap_table;

/* Lookup a bitmap in the shared bitmap hashtable, and return an already
   existing instance if there is one, NULL otherwise.  */

static bitmap
shared_bitmap_lookup (bitmap pt_vars)
{
  shared_bitmap_info **slot;
  struct shared_bitmap_info sbi;

  sbi.pt_vars = pt_vars;
  sbi.hashcode = bitmap_hash (pt_vars);

  slot = shared_bitmap_table->find_slot (&sbi, NO_INSERT);
  if (!slot)
    return NULL;
  else
    return (*slot)->pt_vars;
}

/* Add a bitmap to the shared bitmap hashtable.  */

static void
shared_bitmap_add (bitmap pt_vars)
{
  shared_bitmap_info **slot;
  shared_bitmap_info_t sbi = XNEW (struct shared_bitmap_info);

  sbi->pt_vars = pt_vars;
  sbi->hashcode = bitmap_hash (pt_vars);

  slot = shared_bitmap_table->find_slot (sbi, INSERT);
  gcc_assert (!*slot);
  *slot = sbi;
}

/* Set bits in INTO corresponding to the variable uids in solution set FROM.  */

static void
set_uids_in_ptset (bitmap into, bitmap from, struct pt_solution *pt,
		   tree fndecl)
{
  unsigned int i;
  bitmap_iterator bi;
  varinfo_t escaped_vi = get_varinfo (var_rep[escaped_id]);
  varinfo_t escaped_return_vi = get_varinfo (var_rep[escaped_return_id]);
  bool everything_escaped
    = escaped_vi->solution && bitmap_bit_p (escaped_vi->solution, anything_id);

  EXECUTE_IF_SET_IN_BITMAP (from, 0, i, bi)
    {
      varinfo_t vi = get_varinfo (i);

      if (vi->is_artificial_var)
	continue;

      if (everything_escaped
	  || (escaped_vi->solution
	      && bitmap_bit_p (escaped_vi->solution, i)))
	{
	  pt->vars_contains_escaped = true;
	  pt->vars_contains_escaped_heap |= vi->is_heap_var;
	}
      if (escaped_return_vi->solution
	  && bitmap_bit_p (escaped_return_vi->solution, i))
	pt->vars_contains_escaped_heap |= vi->is_heap_var;

      if (vi->is_restrict_var)
	pt->vars_contains_restrict = true;

      if (VAR_P (vi->decl)
	  || TREE_CODE (vi->decl) == PARM_DECL
	  || TREE_CODE (vi->decl) == RESULT_DECL)
	{
	  /* If we are in IPA mode we will not recompute points-to
	     sets after inlining so make sure they stay valid.  */
	  if (in_ipa_mode
	      && !DECL_PT_UID_SET_P (vi->decl))
	    SET_DECL_PT_UID (vi->decl, DECL_UID (vi->decl));

	  /* Add the decl to the points-to set.  Note that the points-to
	     set contains global variables.  */
	  bitmap_set_bit (into, DECL_PT_UID (vi->decl));
	  if (vi->is_global_var
	      /* In IPA mode the escaped_heap trick doesn't work as
		 ESCAPED is escaped from the unit but
		 pt_solution_includes_global needs to answer true for
		 all variables not automatic within a function.
		 For the same reason is_global_var is not the
		 correct flag to track - local variables from other
		 functions also need to be considered global.
		 Conveniently all HEAP vars are not put in function
		 scope.  */
	      || (in_ipa_mode
		  && fndecl
		  && ! auto_var_in_fn_p (vi->decl, fndecl)))
	    pt->vars_contains_nonlocal = true;

	  /* If we have a variable that is interposable record that fact
	     for pointer comparison simplification.  */
	  if (VAR_P (vi->decl)
	      && (TREE_STATIC (vi->decl) || DECL_EXTERNAL (vi->decl))
	      && ! decl_binds_to_current_def_p (vi->decl))
	    pt->vars_contains_interposable = true;

	  /* If this is a local variable we can have overlapping lifetime
	     of different function invocations through recursion duplicate
	     it with its shadow variable.  */
	  if (in_ipa_mode
	      && vi->shadow_var_uid != 0)
	    {
	      bitmap_set_bit (into, vi->shadow_var_uid);
	      pt->vars_contains_nonlocal = true;
	    }
	}

      else if (TREE_CODE (vi->decl) == FUNCTION_DECL
	       || TREE_CODE (vi->decl) == LABEL_DECL)
	{
	  /* Nothing should read/write from/to code so we can
	     save bits by not including them in the points-to bitmaps.
	     Still mark the points-to set as containing global memory
	     to make code-patching possible - see PR70128.  */
	  pt->vars_contains_nonlocal = true;
	}
    }
}


/* Compute the points-to solution *PT for the variable VI.  */

static struct pt_solution
find_what_var_points_to (tree fndecl, varinfo_t orig_vi)
{
  unsigned int i;
  bitmap_iterator bi;
  bitmap finished_solution;
  bitmap result;
  varinfo_t vi;
  struct pt_solution *pt;

  /* This variable may have been collapsed, let's get the real
     variable.  */
  vi = get_varinfo (var_rep[orig_vi->id]);

  /* See if we have already computed the solution and return it.  */
  pt_solution **slot = &final_solutions->get_or_insert (vi);
  if (*slot != NULL)
    return **slot;

  *slot = pt = XOBNEW (&final_solutions_obstack, struct pt_solution);
  memset (pt, 0, sizeof (struct pt_solution));

  /* Translate artificial variables into SSA_NAME_PTR_INFO
     attributes.  */
  EXECUTE_IF_SET_IN_BITMAP (vi->solution, 0, i, bi)
    {
      varinfo_t vi = get_varinfo (i);

      if (vi->is_artificial_var)
	{
	  if (vi->id == nothing_id)
	    pt->null = 1;
	  else if (vi->id == escaped_id)
	    {
	      if (in_ipa_mode)
		pt->ipa_escaped = 1;
	      else
		pt->escaped = 1;
	      /* Expand some special vars of ESCAPED in-place here.  */
	      varinfo_t evi = get_varinfo (var_rep[escaped_id]);
	      if (bitmap_bit_p (evi->solution, nonlocal_id))
		pt->nonlocal = 1;
	    }
	  else if (vi->id == nonlocal_id)
	    pt->nonlocal = 1;
	  else if (vi->id == string_id)
	    pt->const_pool = 1;
	  else if (vi->id == anything_id
		   || vi->id == integer_id)
	    pt->anything = 1;
	}
    }

  /* Instead of doing extra work, simply do not create
     elaborate points-to information for pt_anything pointers.  */
  if (pt->anything)
    return *pt;

  /* Share the final set of variables when possible.  */
  finished_solution = BITMAP_GGC_ALLOC ();
  stats.points_to_sets_created++;

  set_uids_in_ptset (finished_solution, vi->solution, pt, fndecl);
  result = shared_bitmap_lookup (finished_solution);
  if (!result)
    {
      shared_bitmap_add (finished_solution);
      pt->vars = finished_solution;
    }
  else
    {
      pt->vars = result;
      bitmap_clear (finished_solution);
    }

  return *pt;
}

/* Given a pointer variable P, fill in its points-to set.  */

static void
find_what_p_points_to (tree fndecl, tree p)
{
  struct ptr_info_def *pi;
  tree lookup_p = p;
  varinfo_t vi;
  prange vr;
  get_range_query (DECL_STRUCT_FUNCTION (fndecl))->range_of_expr (vr, p);
  bool nonnull = vr.nonzero_p ();

  /* For parameters, get at the points-to set for the actual parm
     decl.  */
  if (TREE_CODE (p) == SSA_NAME
      && SSA_NAME_IS_DEFAULT_DEF (p)
      && (TREE_CODE (SSA_NAME_VAR (p)) == PARM_DECL
	  || TREE_CODE (SSA_NAME_VAR (p)) == RESULT_DECL))
    lookup_p = SSA_NAME_VAR (p);

  vi = lookup_vi_for_tree (lookup_p);
  if (!vi)
    return;

  pi = get_ptr_info (p);
  pi->pt = find_what_var_points_to (fndecl, vi);
  /* Conservatively set to NULL from PTA (to true). */
  pi->pt.null = 1;
  /* Preserve pointer nonnull globally computed.  */
  if (nonnull)
    set_ptr_nonnull (p);
}


/* Query statistics for points-to solutions.  */

static struct {
  unsigned HOST_WIDE_INT pt_solution_includes_may_alias;
  unsigned HOST_WIDE_INT pt_solution_includes_no_alias;
  unsigned HOST_WIDE_INT pt_solutions_intersect_may_alias;
  unsigned HOST_WIDE_INT pt_solutions_intersect_no_alias;
} pta_stats;

void
dump_pta_stats (FILE *s)
{
  fprintf (s, "\nPTA query stats:\n");
  fprintf (s, "  pt_solution_includes: "
	   HOST_WIDE_INT_PRINT_DEC" disambiguations, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n",
	   pta_stats.pt_solution_includes_no_alias,
	   pta_stats.pt_solution_includes_no_alias
	   + pta_stats.pt_solution_includes_may_alias);
  fprintf (s, "  pt_solutions_intersect: "
	   HOST_WIDE_INT_PRINT_DEC" disambiguations, "
	   HOST_WIDE_INT_PRINT_DEC" queries\n",
	   pta_stats.pt_solutions_intersect_no_alias,
	   pta_stats.pt_solutions_intersect_no_alias
	   + pta_stats.pt_solutions_intersect_may_alias);
}


/* Reset the points-to solution *PT to a conservative default
   (point to anything).  */

void
pt_solution_reset (struct pt_solution *pt)
{
  memset (pt, 0, sizeof (struct pt_solution));
  pt->anything = true;
  pt->null = true;
}

/* Set the points-to solution *PT to point only to the variables
   in VARS.  VARS_CONTAINS_GLOBAL specifies whether that contains
   global variables and VARS_CONTAINS_RESTRICT specifies whether
   it contains restrict tag variables.  */

void
pt_solution_set (struct pt_solution *pt, bitmap vars,
		 bool vars_contains_nonlocal)
{
  memset (pt, 0, sizeof (struct pt_solution));
  pt->vars = vars;
  pt->vars_contains_nonlocal = vars_contains_nonlocal;
  pt->vars_contains_escaped
    = (cfun->gimple_df->escaped.anything
       || bitmap_intersect_p (cfun->gimple_df->escaped.vars, vars));
}

/* Set the points-to solution *PT to point only to the variable VAR.  */

void
pt_solution_set_var (struct pt_solution *pt, tree var)
{
  memset (pt, 0, sizeof (struct pt_solution));
  pt->vars = BITMAP_GGC_ALLOC ();
  bitmap_set_bit (pt->vars, DECL_PT_UID (var));
  pt->vars_contains_nonlocal = is_global_var (var);
  pt->vars_contains_escaped
    = (cfun->gimple_df->escaped.anything
       || bitmap_bit_p (cfun->gimple_df->escaped.vars, DECL_PT_UID (var)));
}

/* Computes the union of the points-to solutions *DEST and *SRC and
   stores the result in *DEST.  This changes the points-to bitmap
   of *DEST and thus may not be used if that might be shared.
   The points-to bitmap of *SRC and *DEST will not be shared after
   this function if they were not before.  */

static void
pt_solution_ior_into (struct pt_solution *dest, struct pt_solution *src)
{
  dest->anything |= src->anything;
  if (dest->anything)
    {
      pt_solution_reset (dest);
      return;
    }

  dest->nonlocal |= src->nonlocal;
  dest->escaped |= src->escaped;
  dest->ipa_escaped |= src->ipa_escaped;
  dest->null |= src->null;
  dest->const_pool |= src->const_pool ;
  dest->vars_contains_nonlocal |= src->vars_contains_nonlocal;
  dest->vars_contains_escaped |= src->vars_contains_escaped;
  dest->vars_contains_escaped_heap |= src->vars_contains_escaped_heap;
  if (!src->vars)
    return;

  if (!dest->vars)
    dest->vars = BITMAP_GGC_ALLOC ();
  bitmap_ior_into (dest->vars, src->vars);
}

/* Return true if the points-to solution *PT is empty.  */

bool
pt_solution_empty_p (const pt_solution *pt)
{
  if (pt->anything
      || pt->nonlocal)
    return false;

  if (pt->vars
      && !bitmap_empty_p (pt->vars))
    return false;

  /* If the solution includes ESCAPED, check if that is empty.  */
  if (pt->escaped
      && !pt_solution_empty_p (&cfun->gimple_df->escaped))
    return false;

  /* If the solution includes ESCAPED, check if that is empty.  */
  if (pt->ipa_escaped
      && !pt_solution_empty_p (&ipa_escaped_pt))
    return false;

  return true;
}

/* Return true if the points-to solution *PT only point to a single var, and
   return the var uid in *UID.  */

bool
pt_solution_singleton_or_null_p (struct pt_solution *pt, unsigned *uid)
{
  if (pt->anything || pt->nonlocal || pt->escaped || pt->ipa_escaped
      || pt->vars == NULL
      || !bitmap_single_bit_set_p (pt->vars))
    return false;

  *uid = bitmap_first_set_bit (pt->vars);
  return true;
}

/* Return true if the points-to solution *PT includes global memory.
   If ESCAPED_LOCAL_P is true then escaped local variables are also
   considered global.  */

bool
pt_solution_includes_global (struct pt_solution *pt, bool escaped_local_p)
{
  if (pt->anything
      || pt->nonlocal
      || pt->vars_contains_nonlocal
      /* The following is a hack to make the malloc escape hack work.
	 In reality we'd need different sets for escaped-through-return
	 and escaped-to-callees and passes would need to be updated.  */
      || pt->vars_contains_escaped_heap)
    return true;

  if (escaped_local_p && pt->vars_contains_escaped)
    return true;

  /* 'escaped' is also a placeholder so we have to look into it.  */
  if (pt->escaped)
    return pt_solution_includes_global (&cfun->gimple_df->escaped,
					escaped_local_p);

  if (pt->ipa_escaped)
    return pt_solution_includes_global (&ipa_escaped_pt,
					escaped_local_p);

  return false;
}

/* Return true if the points-to solution *PT includes the variable
   declaration DECL.  */

static bool
pt_solution_includes_1 (struct pt_solution *pt, const_tree decl)
{
  if (pt->anything)
    return true;

  if (pt->nonlocal
      && is_global_var (decl))
    return true;

  if (pt->vars
      && bitmap_bit_p (pt->vars, DECL_PT_UID (decl)))
    return true;

  /* If the solution includes ESCAPED, check it.  */
  if (pt->escaped
      && pt_solution_includes_1 (&cfun->gimple_df->escaped, decl))
    return true;

  /* If the solution includes ESCAPED, check it.  */
  if (pt->ipa_escaped
      && pt_solution_includes_1 (&ipa_escaped_pt, decl))
    return true;

  return false;
}

bool
pt_solution_includes (struct pt_solution *pt, const_tree decl)
{
  bool res = pt_solution_includes_1 (pt, decl);
  if (res)
    ++pta_stats.pt_solution_includes_may_alias;
  else
    ++pta_stats.pt_solution_includes_no_alias;
  return res;
}

/* Return true if the points-to solution *PT contains a reference to a
   constant pool entry.  */

bool
pt_solution_includes_const_pool (struct pt_solution *pt)
{
  return (pt->const_pool
	  || pt->nonlocal
	  || (pt->escaped && (!cfun || cfun->gimple_df->escaped.const_pool))
	  || (pt->ipa_escaped && ipa_escaped_pt.const_pool));
}

/* Return true if both points-to solutions PT1 and PT2 have a non-empty
   intersection.  */

static bool
pt_solutions_intersect_1 (struct pt_solution *pt1, struct pt_solution *pt2)
{
  if (pt1->anything || pt2->anything)
    return true;

  /* If either points to unknown global memory and the other points to
     any global memory they alias.  */
  if ((pt1->nonlocal
       && (pt2->nonlocal
	   || pt2->vars_contains_nonlocal))
      || (pt2->nonlocal
	  && pt1->vars_contains_nonlocal))
    return true;

  /* If either points to all escaped memory and the other points to
     any escaped memory they alias.  */
  if ((pt1->escaped
       && (pt2->escaped
	   || pt2->vars_contains_escaped))
      || (pt2->escaped
	  && pt1->vars_contains_escaped))
    return true;

  /* Check the escaped solution if required.
     ???  Do we need to check the local against the IPA escaped sets?  */
  if ((pt1->ipa_escaped || pt2->ipa_escaped)
      && !pt_solution_empty_p (&ipa_escaped_pt))
    {
      /* If both point to escaped memory and that solution
	 is not empty they alias.  */
      if (pt1->ipa_escaped && pt2->ipa_escaped)
	return true;

      /* If either points to escaped memory see if the escaped solution
	 intersects with the other.  */
      if ((pt1->ipa_escaped
	   && pt_solutions_intersect_1 (&ipa_escaped_pt, pt2))
	  || (pt2->ipa_escaped
	      && pt_solutions_intersect_1 (&ipa_escaped_pt, pt1)))
	return true;
    }

  /* Now both pointers alias if their points-to solution intersects.  */
  return (pt1->vars
	  && pt2->vars
	  && bitmap_intersect_p (pt1->vars, pt2->vars));
}

bool
pt_solutions_intersect (struct pt_solution *pt1, struct pt_solution *pt2)
{
  bool res = pt_solutions_intersect_1 (pt1, pt2);
  if (res)
    ++pta_stats.pt_solutions_intersect_may_alias;
  else
    ++pta_stats.pt_solutions_intersect_no_alias;
  return res;
}


/* Initialize things necessary to perform PTA.  */

static void
init_alias_vars (void)
{
  use_field_sensitive = (param_max_fields_for_field_sensitive > 1);

  bitmap_obstack_initialize (&pta_obstack);
  bitmap_obstack_initialize (&oldpta_obstack);

  constraints.create (8);
  varmap.create (8);

  memset (&stats, 0, sizeof (stats));
  shared_bitmap_table = new hash_table<shared_bitmap_hasher> (511);

  final_solutions = new hash_map<varinfo_t, pt_solution *>;
  gcc_obstack_init (&final_solutions_obstack);

  init_constraint_builder ();
}

/* Create points-to sets for the current function.  See the comments
   at the start of the file for an algorithmic overview.  */

static void
compute_points_to_sets (void)
{
  basic_block bb;
  varinfo_t vi;

  timevar_push (TV_TREE_PTA);

  init_alias_vars ();

  intra_build_constraints ();

  /* From the constraints compute the points-to sets.  */
  solve_constraints ();

  if (dump_file && (dump_flags & TDF_STATS))
    dump_sa_stats (dump_file);

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_sa_points_to_info (dump_file);

  /* Compute the points-to set for ESCAPED used for call-clobber analysis.  */
  cfun->gimple_df->escaped = find_what_var_points_to (cfun->decl,
						      get_varinfo (escaped_id));

  /* Make sure the ESCAPED solution (which is used as placeholder in
     other solutions) does not reference itself.  This simplifies
     points-to solution queries.  */
  cfun->gimple_df->escaped.escaped = 0;

  /* The ESCAPED_RETURN solution is what contains all memory that needs
     to be considered global.  */
  cfun->gimple_df->escaped_return
    = find_what_var_points_to (cfun->decl, get_varinfo (escaped_return_id));
  cfun->gimple_df->escaped_return.escaped = 1;

  /* Compute the points-to sets for pointer SSA_NAMEs.  */
  unsigned i;
  tree ptr;

  FOR_EACH_SSA_NAME (i, ptr, cfun)
    {
      if (POINTER_TYPE_P (TREE_TYPE (ptr)))
	find_what_p_points_to (cfun->decl, ptr);
    }

  /* Compute the call-used/clobbered sets.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gcall *stmt;
	  struct pt_solution *pt;

	  stmt = dyn_cast <gcall *> (gsi_stmt (gsi));
	  if (!stmt)
	    continue;

	  pt = gimple_call_use_set (stmt);
	  if (gimple_call_flags (stmt) & ECF_CONST)
	    memset (pt, 0, sizeof (struct pt_solution));
	  else
	    {
	      bool uses_global_memory = true;
	      bool reads_global_memory = true;

	      determine_global_memory_access (stmt, NULL,
					      &reads_global_memory,
					      &uses_global_memory);
	      if ((vi = lookup_call_use_vi (stmt)) != NULL)
		{
		  *pt = find_what_var_points_to (cfun->decl, vi);
		  /* Escaped (and thus nonlocal) variables are always
		     implicitly used by calls.  */
		  /* ???  ESCAPED can be empty even though NONLOCAL
		     always escaped.  */
		  if (uses_global_memory)
		    {
		      pt->nonlocal = 1;
		      pt->escaped = 1;
		    }
		}
	      else if (uses_global_memory)
		{
		  /* If there is nothing special about this call then
		     we have made everything that is used also escape.  */
		  *pt = cfun->gimple_df->escaped;
		  pt->nonlocal = 1;
		}
	      else
		memset (pt, 0, sizeof (struct pt_solution));
	    }

	  pt = gimple_call_clobber_set (stmt);
	  if (gimple_call_flags (stmt) & (ECF_CONST|ECF_PURE|ECF_NOVOPS))
	    memset (pt, 0, sizeof (struct pt_solution));
	  else
	    {
	      bool writes_global_memory = true;

	      determine_global_memory_access (stmt, &writes_global_memory,
					      NULL, NULL);

	      if ((vi = lookup_call_clobber_vi (stmt)) != NULL)
		{
		  *pt = find_what_var_points_to (cfun->decl, vi);
		  /* Escaped (and thus nonlocal) variables are always
		     implicitly clobbered by calls.  */
		  /* ???  ESCAPED can be empty even though NONLOCAL
		     always escaped.  */
		  if (writes_global_memory)
		    {
		      pt->nonlocal = 1;
		      pt->escaped = 1;
		    }
		}
	      else if (writes_global_memory)
		{
		  /* If there is nothing special about this call then
		     we have made everything that is used also escape.  */
		  *pt = cfun->gimple_df->escaped;
		  pt->nonlocal = 1;
		}
	      else
		memset (pt, 0, sizeof (struct pt_solution));
	    }
	}
    }

  timevar_pop (TV_TREE_PTA);
}

/* Delete created points-to sets.  */

static void
delete_points_to_sets (void)
{
  delete shared_bitmap_table;
  shared_bitmap_table = NULL;
  if (dump_file && (dump_flags & TDF_STATS))
    fprintf (dump_file, "Points to sets created:%d\n",
	     stats.points_to_sets_created);

  bitmap_obstack_release (&pta_obstack);
  constraints.release ();

  free (var_rep);

  varmap.release ();
  variable_info_pool.release ();

  delete final_solutions;
  obstack_free (&final_solutions_obstack, NULL);

  delete_constraint_builder ();
}


struct vls_data
{
  unsigned short clique;
  bool escaped_p;
  bitmap rvars;
};

/* Mark "other" loads and stores as belonging to CLIQUE and with
   base zero.  */

static bool
visit_loadstore (gimple *, tree base, tree ref, void *data)
{
  unsigned short clique = ((vls_data *) data)->clique;
  bitmap rvars = ((vls_data *) data)->rvars;
  bool escaped_p = ((vls_data *) data)->escaped_p;
  if (TREE_CODE (base) == MEM_REF
      || TREE_CODE (base) == TARGET_MEM_REF)
    {
      tree ptr = TREE_OPERAND (base, 0);
      if (TREE_CODE (ptr) == SSA_NAME)
	{
	  /* For parameters, get at the points-to set for the actual parm
	     decl.  */
	  if (SSA_NAME_IS_DEFAULT_DEF (ptr)
	      && (TREE_CODE (SSA_NAME_VAR (ptr)) == PARM_DECL
		  || TREE_CODE (SSA_NAME_VAR (ptr)) == RESULT_DECL))
	    ptr = SSA_NAME_VAR (ptr);

	  /* We need to make sure 'ptr' doesn't include any of
	     the restrict tags we added bases for in its points-to set.  */
	  varinfo_t vi = lookup_vi_for_tree (ptr);
	  if (! vi)
	    return false;

	  vi = get_varinfo (var_rep[vi->id]);
	  if (bitmap_intersect_p (rvars, vi->solution)
	      || (escaped_p && bitmap_bit_p (vi->solution, escaped_id)))
	    return false;
	}

      /* Do not overwrite existing cliques (that includes clique, base
	 pairs we just set).  */
      if (MR_DEPENDENCE_CLIQUE (base) == 0)
	{
	  MR_DEPENDENCE_CLIQUE (base) = clique;
	  MR_DEPENDENCE_BASE (base) = 0;
	}
    }

  /* For plain decl accesses see whether they are accesses to globals
     and rewrite them to MEM_REFs with { clique, 0 }.  */
  if (VAR_P (base)
      && is_global_var (base)
      /* ???  We can't rewrite a plain decl with the walk_stmt_load_store
	 ops callback.  */
      && base != ref)
    {
      tree *basep = &ref;
      while (handled_component_p (*basep))
	basep = &TREE_OPERAND (*basep, 0);
      gcc_assert (VAR_P (*basep));
      tree ptr = build_fold_addr_expr (*basep);
      tree zero = build_int_cst (TREE_TYPE (ptr), 0);
      *basep = build2 (MEM_REF, TREE_TYPE (*basep), ptr, zero);
      MR_DEPENDENCE_CLIQUE (*basep) = clique;
      MR_DEPENDENCE_BASE (*basep) = 0;
    }

  return false;
}

struct msdi_data {
  tree ptr;
  unsigned short *clique;
  unsigned short *last_ruid;
  varinfo_t restrict_var;
};

/* If BASE is a MEM_REF then assign a clique, base pair to it, updating
   CLIQUE, *RESTRICT_VAR and LAST_RUID as passed via DATA.
   Return whether dependence info was assigned to BASE.  */

static bool
maybe_set_dependence_info (gimple *, tree base, tree, void *data)
{
  tree ptr = ((msdi_data *)data)->ptr;
  unsigned short &clique = *((msdi_data *)data)->clique;
  unsigned short &last_ruid = *((msdi_data *)data)->last_ruid;
  varinfo_t restrict_var = ((msdi_data *)data)->restrict_var;
  if ((TREE_CODE (base) == MEM_REF
       || TREE_CODE (base) == TARGET_MEM_REF)
      && TREE_OPERAND (base, 0) == ptr)
    {
      /* Do not overwrite existing cliques.  This avoids overwriting dependence
	 info inlined from a function with restrict parameters inlined
	 into a function with restrict parameters.  This usually means we
	 prefer to be precise in innermost loops.  */
      if (MR_DEPENDENCE_CLIQUE (base) == 0)
	{
	  if (clique == 0)
	    {
	      if (cfun->last_clique == 0)
		cfun->last_clique = 1;
	      clique = 1;
	    }
	  if (restrict_var->ruid == 0)
	    restrict_var->ruid = ++last_ruid;
	  MR_DEPENDENCE_CLIQUE (base) = clique;
	  MR_DEPENDENCE_BASE (base) = restrict_var->ruid;
	  return true;
	}
    }
  return false;
}

/* Clear dependence info for the clique DATA.  */

static bool
clear_dependence_clique (gimple *, tree base, tree, void *data)
{
  unsigned short clique = (uintptr_t)data;
  if ((TREE_CODE (base) == MEM_REF
       || TREE_CODE (base) == TARGET_MEM_REF)
      && MR_DEPENDENCE_CLIQUE (base) == clique)
    {
      MR_DEPENDENCE_CLIQUE (base) = 0;
      MR_DEPENDENCE_BASE (base) = 0;
    }

  return false;
}

/* Compute the set of independend memory references based on restrict
   tags and their conservative propagation to the points-to sets.  */

static void
compute_dependence_clique (void)
{
  /* First clear the special "local" clique.  */
  basic_block bb;
  if (cfun->last_clique != 0)
    FOR_EACH_BB_FN (bb, cfun)
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  walk_stmt_load_store_ops (stmt, (void *)(uintptr_t) 1,
				    clear_dependence_clique,
				    clear_dependence_clique);
	}

  unsigned short clique = 0;
  unsigned short last_ruid = 0;
  bitmap rvars = BITMAP_ALLOC (NULL);
  bool escaped_p = false;
  for (unsigned i = 0; i < num_ssa_names; ++i)
    {
      tree ptr = ssa_name (i);
      if (!ptr || !POINTER_TYPE_P (TREE_TYPE (ptr)))
	continue;

      /* Avoid all this when ptr is not dereferenced?  */
      tree p = ptr;
      if (SSA_NAME_IS_DEFAULT_DEF (ptr)
	  && (TREE_CODE (SSA_NAME_VAR (ptr)) == PARM_DECL
	      || TREE_CODE (SSA_NAME_VAR (ptr)) == RESULT_DECL))
	p = SSA_NAME_VAR (ptr);
      varinfo_t vi = lookup_vi_for_tree (p);
      if (!vi)
	continue;
      vi = get_varinfo (var_rep[vi->id]);
      bitmap_iterator bi;
      unsigned j;
      varinfo_t restrict_var = NULL;
      EXECUTE_IF_SET_IN_BITMAP (vi->solution, 0, j, bi)
	{
	  varinfo_t oi = get_varinfo (j);
	  if (oi->head != j)
	    oi = get_varinfo (oi->head);
	  if (oi->is_restrict_var)
	    {
	      if (restrict_var
		  && restrict_var != oi)
		{
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file, "found restrict pointed-to "
			       "for ");
		      print_generic_expr (dump_file, ptr);
		      fprintf (dump_file, " but not exclusively\n");
		    }
		  restrict_var = NULL;
		  break;
		}
	      restrict_var = oi;
	    }
	  /* NULL is the only other valid points-to entry.  */
	  else if (oi->id != nothing_id)
	    {
	      restrict_var = NULL;
	      break;
	    }
	}
      /* Ok, found that ptr must(!) point to a single(!) restrict
	 variable.  */
      /* ???  PTA isn't really a proper propagation engine to compute
	 this property.
	 ???  We could handle merging of two restricts by unifying them.  */
      if (restrict_var)
	{
	  /* Now look at possible dereferences of ptr.  */
	  imm_use_iterator ui;
	  gimple *use_stmt;
	  bool used = false;
	  msdi_data data = { ptr, &clique, &last_ruid, restrict_var };
	  FOR_EACH_IMM_USE_STMT (use_stmt, ui, ptr)
	    used |= walk_stmt_load_store_ops (use_stmt, &data,
					      maybe_set_dependence_info,
					      maybe_set_dependence_info);
	  if (used)
	    {
	      /* Add all subvars to the set of restrict pointed-to set.  */
	      for (unsigned sv = restrict_var->head; sv != 0;
		   sv = get_varinfo (sv)->next)
		bitmap_set_bit (rvars, sv);
	      varinfo_t escaped = get_varinfo (var_rep[escaped_id]);
	      if (bitmap_bit_p (escaped->solution, restrict_var->id))
		escaped_p = true;
	    }
	}
    }

  if (clique != 0)
    {
      /* Assign the BASE id zero to all accesses not based on a restrict
	 pointer.  That way they get disambiguated against restrict
	 accesses but not against each other.  */
      /* ???  For restricts derived from globals (thus not incoming
	 parameters) we can't restrict scoping properly thus the following
	 is too aggressive there.  For now we have excluded those globals from
	 getting into the MR_DEPENDENCE machinery.  */
      vls_data data = { clique, escaped_p, rvars };
      basic_block bb;
      FOR_EACH_BB_FN (bb, cfun)
	for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	     !gsi_end_p (gsi); gsi_next (&gsi))
	  {
	    gimple *stmt = gsi_stmt (gsi);
	    walk_stmt_load_store_ops (stmt, &data,
				      visit_loadstore, visit_loadstore);
	  }
    }

  BITMAP_FREE (rvars);
}


/* Compute points-to information for every SSA_NAME pointer in the
   current function and compute the transitive closure of escaped
   variables to re-initialize the call-clobber states of local variables.  */

unsigned int
compute_may_aliases (void)
{
  if (cfun->gimple_df->ipa_pta)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "\nNot re-computing points-to information "
		   "because IPA points-to information is available.\n\n");

	  /* But still dump what we have remaining it.  */
	  if (dump_flags & (TDF_DETAILS|TDF_ALIAS))
	    dump_alias_info (dump_file);
	}

      return 0;
    }

  /* For each pointer P_i, determine the sets of variables that P_i may
     point-to.  Compute the reachability set of escaped and call-used
     variables.  */
  compute_points_to_sets ();

  /* Debugging dumps.  */
  if (dump_file && (dump_flags & (TDF_DETAILS|TDF_ALIAS)))
    dump_alias_info (dump_file);

  /* Compute restrict-based memory disambiguations.  */
  compute_dependence_clique ();

  /* Deallocate memory used by aliasing data structures and the internal
     points-to solution.  */
  delete_points_to_sets ();

  gcc_assert (!need_ssa_update_p (cfun));

  return 0;
}

/* A dummy pass to cause points-to information to be computed via
   TODO_rebuild_alias.  */

namespace {

const pass_data pass_data_build_alias =
{
  GIMPLE_PASS, /* type */
  "alias", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_rebuild_alias, /* todo_flags_finish */
};

class pass_build_alias : public gimple_opt_pass
{
public:
  pass_build_alias (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_build_alias, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return flag_tree_pta; }

}; // class pass_build_alias

} // anon namespace

gimple_opt_pass *
make_pass_build_alias (gcc::context *ctxt)
{
  return new pass_build_alias (ctxt);
}

/* A dummy pass to cause points-to information to be computed via
   TODO_rebuild_alias.  */

namespace {

const pass_data pass_data_build_ealias =
{
  GIMPLE_PASS, /* type */
  "ealias", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_rebuild_alias, /* todo_flags_finish */
};

class pass_build_ealias : public gimple_opt_pass
{
public:
  pass_build_ealias (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_build_ealias, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override { return flag_tree_pta; }

}; // class pass_build_ealias

} // anon namespace

gimple_opt_pass *
make_pass_build_ealias (gcc::context *ctxt)
{
  return new pass_build_ealias (ctxt);
}


/* IPA PTA solutions for ESCAPED.  */
struct pt_solution ipa_escaped_pt
  = { true, false, false, false, false, false,
      false, false, false, false, false, NULL };


/* Execute the driver for IPA PTA.  */
static unsigned int
ipa_pta_execute (void)
{
  struct cgraph_node *node;

  in_ipa_mode = 1;

  init_alias_vars ();

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      symtab->dump (dump_file);
      fprintf (dump_file, "\n");
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Generating generic constraints\n\n");
      dump_constraints (dump_file, 0);
      fprintf (dump_file, "\n");
    }

  ipa_build_constraints ();

  /* From the constraints compute the points-to sets.  */
  solve_constraints ();

  if (dump_file && (dump_flags & TDF_STATS))
    dump_sa_stats (dump_file);

  if (dump_file && (dump_flags & TDF_DETAILS))
    dump_sa_points_to_info (dump_file);

  /* Now post-process solutions to handle locals from different
     runtime instantiations coming in through recursive invocations.  */
  unsigned shadow_var_cnt = 0;
  for (unsigned i = 1; i < varmap.length (); ++i)
    {
      varinfo_t fi = get_varinfo (i);
      if (fi->is_fn_info
	  && fi->decl)
	/* Automatic variables pointed to by their containing functions
	   parameters need this treatment.  */
	for (varinfo_t ai = first_vi_for_offset (fi, fi_parm_base);
	     ai; ai = vi_next (ai))
	  {
	    varinfo_t vi = get_varinfo (var_rep[ai->id]);
	    bitmap_iterator bi;
	    unsigned j;
	    EXECUTE_IF_SET_IN_BITMAP (vi->solution, 0, j, bi)
	      {
		varinfo_t pt = get_varinfo (j);
		if (pt->shadow_var_uid == 0
		    && pt->decl
		    && auto_var_in_fn_p (pt->decl, fi->decl))
		  {
		    pt->shadow_var_uid = allocate_decl_uid ();
		    shadow_var_cnt++;
		  }
	      }
	  }
      /* As well as global variables which are another way of passing
	 arguments to recursive invocations.  */
      else if (fi->is_global_var)
	{
	  for (varinfo_t ai = fi; ai; ai = vi_next (ai))
	    {
	      varinfo_t vi = get_varinfo (var_rep[ai->id]);
	      bitmap_iterator bi;
	      unsigned j;
	      EXECUTE_IF_SET_IN_BITMAP (vi->solution, 0, j, bi)
		{
		  varinfo_t pt = get_varinfo (j);
		  if (pt->shadow_var_uid == 0
		      && pt->decl
		      && auto_var_p (pt->decl))
		    {
		      pt->shadow_var_uid = allocate_decl_uid ();
		      shadow_var_cnt++;
		    }
		}
	    }
	}
    }
  if (shadow_var_cnt && dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Allocated %u shadow variables for locals "
	     "maybe leaking into recursive invocations of their containing "
	     "functions\n", shadow_var_cnt);

  /* Compute the global points-to sets for ESCAPED.
     ???  Note that the computed escape set is not correct
     for the whole unit as we fail to consider graph edges to
     externally visible functions.  */
  ipa_escaped_pt = find_what_var_points_to (NULL, get_varinfo (escaped_id));

  /* Make sure the ESCAPED solution (which is used as placeholder in
     other solutions) does not reference itself.  This simplifies
     points-to solution queries.  */
  ipa_escaped_pt.ipa_escaped = 0;

  /* Assign the points-to sets to the SSA names in the unit.  */
  FOR_EACH_DEFINED_FUNCTION (node)
    {
      tree ptr;
      struct function *fn;
      unsigned i;
      basic_block bb;

      /* Nodes without a body in this partition are not interesting.  */
      if (!node->has_gimple_body_p ()
	  || node->in_other_partition
	  || node->clone_of)
	continue;

      fn = DECL_STRUCT_FUNCTION (node->decl);

      /* Compute the points-to sets for pointer SSA_NAMEs.  */
      FOR_EACH_VEC_ELT (*fn->gimple_df->ssa_names, i, ptr)
	{
	  if (ptr
	      && POINTER_TYPE_P (TREE_TYPE (ptr)))
	    find_what_p_points_to (node->decl, ptr);
	}

      /* Compute the call-use and call-clobber sets for indirect calls
	 and calls to external functions.  */
      FOR_EACH_BB_FN (bb, fn)
	{
	  gimple_stmt_iterator gsi;

	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gcall *stmt;
	      struct pt_solution *pt;
	      varinfo_t vi, fi;
	      tree decl;

	      stmt = dyn_cast <gcall *> (gsi_stmt (gsi));
	      if (!stmt)
		continue;

	      /* Handle direct calls to functions with body.  */
	      decl = gimple_call_fndecl (stmt);

	      {
		tree called_decl = NULL_TREE;
		if (gimple_call_builtin_p (stmt, BUILT_IN_GOMP_PARALLEL))
		  called_decl = TREE_OPERAND (gimple_call_arg (stmt, 0), 0);
		else if (gimple_call_builtin_p (stmt, BUILT_IN_GOACC_PARALLEL))
		  called_decl = TREE_OPERAND (gimple_call_arg (stmt, 1), 0);

		if (called_decl != NULL_TREE
		    && !fndecl_maybe_in_other_partition (called_decl))
		  decl = called_decl;
	      }

	      if (decl
		  && (fi = lookup_vi_for_tree (decl))
		  && fi->is_fn_info)
		{
		  *gimple_call_clobber_set (stmt)
		     = find_what_var_points_to
			 (node->decl, first_vi_for_offset (fi, fi_clobbers));
		  *gimple_call_use_set (stmt)
		     = find_what_var_points_to
			 (node->decl, first_vi_for_offset (fi, fi_uses));
		}
	      /* Handle direct calls to external functions.  */
	      else if (decl && (!fi || fi->decl))
		{
		  pt = gimple_call_use_set (stmt);
		  if (gimple_call_flags (stmt) & ECF_CONST)
		    memset (pt, 0, sizeof (struct pt_solution));
		  else if ((vi = lookup_call_use_vi (stmt)) != NULL)
		    {
		      *pt = find_what_var_points_to (node->decl, vi);
		      /* Escaped (and thus nonlocal) variables are always
			 implicitly used by calls.  */
		      /* ???  ESCAPED can be empty even though NONLOCAL
			 always escaped.  */
		      pt->nonlocal = 1;
		      pt->ipa_escaped = 1;
		    }
		  else
		    {
		      /* If there is nothing special about this call then
			 we have made everything that is used also escape.  */
		      *pt = ipa_escaped_pt;
		      pt->nonlocal = 1;
		    }

		  pt = gimple_call_clobber_set (stmt);
		  if (gimple_call_flags (stmt) &
		      (ECF_CONST|ECF_PURE|ECF_NOVOPS))
		    memset (pt, 0, sizeof (struct pt_solution));
		  else if ((vi = lookup_call_clobber_vi (stmt)) != NULL)
		    {
		      *pt = find_what_var_points_to (node->decl, vi);
		      /* Escaped (and thus nonlocal) variables are always
			 implicitly clobbered by calls.  */
		      /* ???  ESCAPED can be empty even though NONLOCAL
			 always escaped.  */
		      pt->nonlocal = 1;
		      pt->ipa_escaped = 1;
		    }
		  else
		    {
		      /* If there is nothing special about this call then
			 we have made everything that is used also escape.  */
		      *pt = ipa_escaped_pt;
		      pt->nonlocal = 1;
		    }
		}
	      /* Handle indirect calls.  */
	      else if ((fi = get_fi_for_callee (stmt)))
		{
		  /* We need to accumulate all clobbers/uses of all possible
		     callees.  */
		  fi = get_varinfo (var_rep[fi->id]);
		  /* If we cannot constrain the set of functions we'll end up
		     calling we end up using/clobbering everything.  */
		  if (bitmap_bit_p (fi->solution, anything_id)
		      || bitmap_bit_p (fi->solution, nonlocal_id)
		      || bitmap_bit_p (fi->solution, escaped_id))
		    {
		      pt_solution_reset (gimple_call_clobber_set (stmt));
		      pt_solution_reset (gimple_call_use_set (stmt));
		    }
		  else
		    {
		      bitmap_iterator bi;
		      unsigned i;
		      struct pt_solution *uses, *clobbers;

		      uses = gimple_call_use_set (stmt);
		      clobbers = gimple_call_clobber_set (stmt);
		      memset (uses, 0, sizeof (struct pt_solution));
		      memset (clobbers, 0, sizeof (struct pt_solution));
		      EXECUTE_IF_SET_IN_BITMAP (fi->solution, 0, i, bi)
			{
			  struct pt_solution sol;

			  vi = get_varinfo (i);
			  if (!vi->is_fn_info)
			    {
			      /* ???  We could be more precise here?  */
			      uses->nonlocal = 1;
			      uses->ipa_escaped = 1;
			      clobbers->nonlocal = 1;
			      clobbers->ipa_escaped = 1;
			      continue;
			    }

			  if (!uses->anything)
			    {
			      sol = find_what_var_points_to
				      (node->decl,
				       first_vi_for_offset (vi, fi_uses));
			      pt_solution_ior_into (uses, &sol);
			    }
			  if (!clobbers->anything)
			    {
			      sol = find_what_var_points_to
				      (node->decl,
				       first_vi_for_offset (vi, fi_clobbers));
			      pt_solution_ior_into (clobbers, &sol);
			    }
			}
		    }
		}
	      else
		gcc_unreachable ();
	    }
	}

      fn->gimple_df->ipa_pta = true;

      /* We have to re-set the final-solution cache after each function
	 because what is a "global" is dependent on function context.  */
      final_solutions->empty ();
      obstack_free (&final_solutions_obstack, NULL);
      gcc_obstack_init (&final_solutions_obstack);
    }

  delete_points_to_sets ();

  in_ipa_mode = 0;

  return 0;
}

namespace {

const pass_data pass_data_ipa_pta =
{
  SIMPLE_IPA_PASS, /* type */
  "pta", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_PTA, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_pta : public simple_ipa_opt_pass
{
public:
  pass_ipa_pta (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_pta, ctxt)
  {}

  /* opt_pass methods: */
  bool gate (function *) final override
    {
      return (optimize
	      && flag_ipa_pta
	      /* Don't bother doing anything if the program has errors.  */
	      && !seen_error ());
    }

  opt_pass * clone () final override { return new pass_ipa_pta (m_ctxt); }

  unsigned int execute (function *) final override
  {
    return ipa_pta_execute ();
  }

}; // class pass_ipa_pta

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_pta (gcc::context *ctxt)
{
  return new pass_ipa_pta (ctxt);
}
