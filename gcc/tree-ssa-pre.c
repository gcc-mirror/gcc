/* SSA-PRE for trees.
   Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dan@dberlin.org>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "errors.h"
#include "ggc.h"
#include "tree.h"

/* These RTL headers are needed for basic-block.h.  */
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-inline.h"
#include "tree-flow.h"
#include "tree-gimple.h"
#include "tree-dump.h"
#include "timevar.h"
#include "fibheap.h"
#include "hashtab.h"
#include "tree-iterator.h"
#include "real.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "flags.h"


/*

   Some of the algorithms are also based on Open64's SSAPRE implementation.

   Since the papers are a bit dense to read, take a while to grasp,
   and have a few bugs, i'll give a quick rundown:

   Normally, in non-SSA form, one performs PRE on expressions using
   bit vectors, determining properties for all expressions at once
   through bitmap operations and iterative dataflow.

   SSAPRE, like most non-SSA->SSA algorithm conversions, operates one
   expression at a time, and doesn't use bitvectors or iterative
   dataflow.
   
   It answers the question "Given a single hypothetical temporary
   variable, what expressions could we eliminate.  

   To be able to do this, we need an SSA form for expressions.
   If you are already confused, you likely think an expression, as
   used here, is something like "b_3 = a_2 + 5".  It's not. It's "a +
   5". "a_2 + 5" is an *occurrence* of the expression "a + 5".  Just
   like PRE, it's lexical equivalence that matters.
   Compilers generally give you an SSA form for variables, and maybe
   arrays (and/or conditionals).  But not for expressions.

   GCC doesn't give you one either, so we have to build it.
   Thus, the first steps of SSAPRE are to do just these things.

   First we collect lists of occurrences of expressions we are going
   to operate on.
   Note that:
   Unlike the paper, we don't have to ever add newly formed
   expressions to the list (for normal SSAPRE, anyway), because we
   don't have expressions with more than two operators, and each
   operator is either a constant or a variable.  Thus, no second
   order effects.
   
   Once we have the lists of occurrences, we process one expression
   at a time, doing the following:
   1. Using a slightly modified SSA phi placement algorithm, place
   expression PHI's for expressions.
   2. Using a two step optimistic SSA renaming algorithm, version the
   nodes and link phi operands to their real occurrences, if they
   exist.  This creates a factored graph of our expression SSA occurrences.
   3. Using the factored graph, compute downsafe, avail, and later for
   EPHIs (which are SSA versions of the same named bitvector PRE
   problems)
   4. Using EPHI availability information and versions, compute what
   occurrences need to have saves, and what occurrences can be
   reloaded from an already saved value.
   5. Insert the saves and reloads, and transform EPHIs into regular
   phis of the temporary we use for insertion/saving.  
   
   See http://citeseer.nj.nec.com/chow97new.html, and
   http://citeseer.nj.nec.com/kennedy99partial.html for details of the
   algorithm.
   
   kennedy99partial is newer, and is what this implementation is based
   on.
   
   For strength reduction addition, see
   http://citeseer.nj.nec.com/kennedy98strength.html.

   There is also a paper on sparse register promotion using PRE that
   contains the basic algorithm for load PRE.  The exact title/url of
   the paper escapes me.  

   Lastly, there is a code hoisting extension that open64 performs
   (see opt_ehoist.cxx), but we don't. It's not documented in any
   papers, but not that difficult to understand of implement.  */


/* TODOS:
   Do strength reduction on a +-b and -a, not just a * <constant>.
   */

struct expr_info;
static void clear_all_eref_arrays (void);
static inline bool expr_lexically_eq (const tree, const tree);
static void free_expr_info (struct expr_info *);
static bitmap compute_idfs (bitmap *, tree);
static void set_var_phis (struct expr_info *, tree);
static inline bool names_match_p (const tree, const tree);
static bool is_strred_cand (const tree);
static int pre_expression (struct expr_info *, void *, bitmap);
static bool is_injuring_def (struct expr_info *, tree);
static inline bool okay_injuring_def (tree, tree);
static bool expr_phi_insertion (bitmap *, struct expr_info *);
static tree factor_through_injuries (struct expr_info *, tree, tree, bool *);
static inline tree maybe_find_rhs_use_for_var (tree, tree, unsigned int);
static inline tree find_rhs_use_for_var (tree, tree);
static tree create_ephi_node (basic_block, unsigned int);
static inline int opnum_of_phi (tree, int);
static inline int opnum_of_ephi (const tree, const edge);
static tree subst_phis (struct expr_info *, tree, basic_block, basic_block);
static void generate_expr_as_of_bb (tree, basic_block, basic_block);
static void generate_vops_as_of_bb (tree, basic_block, basic_block);
static void rename_1 (struct expr_info *);
static void process_delayed_rename (struct expr_info *, tree, tree);
static void assign_new_class (tree, varray_type *, varray_type *);
static void create_and_insert_occ_in_preorder_dt_order (struct expr_info *);
static void insert_euse_in_preorder_dt_order (struct expr_info *);
static bool ephi_has_unsafe_arg (tree);
static void reset_down_safe (tree, int);
static void compute_down_safety (struct expr_info *);
static void compute_will_be_avail (struct expr_info *);
static void compute_stops (struct expr_info *);
static bool finalize_1 (struct expr_info *);
static void finalize_2 (struct expr_info *);
static tree occ_identical_to (tree);
static void require_phi (struct expr_info *, basic_block);
static bool really_available_def (tree node);

/* Functions used for an EPHI based depth first search.  */
struct ephi_df_search 
{
  /* Return true if the ephi has been seen.  */
  bool (*seen) (tree);
  /* Mark the ephi as seen.  */
  void (*set_seen) (tree);
  /* Note that the search reaches from one ephi to it's use.  */
  void (*reach_from_to) (tree, int, tree);
  /* Return true if we should start a search from this PHI.  */
  bool (*start_from) (tree);
  /* Return true if we should continue the search to this use.  */
  bool (*continue_from_to) (tree, int, tree);
};
static bool repl_search_seen (tree);
static void repl_search_set_seen (tree);
static void repl_search_reach_from_to (tree, int, tree);
static bool repl_search_start_from (tree);
static bool repl_search_continue_from_to (tree, int, tree);
static bool stops_search_seen (tree);
static void stops_search_set_seen (tree);
static void stops_search_reach_from_to (tree, int, tree);
static bool stops_search_start_from (tree);
static bool stops_search_continue_from_to (tree, int, tree);
static bool cba_search_seen (tree);
static void cba_search_set_seen (tree);
static bool cba_search_start_from (tree);
static bool cba_search_continue_from_to (tree, int, tree);
struct ephi_df_search cant_be_avail_search = {
  cba_search_seen,
  cba_search_set_seen,
  NULL,
  cba_search_start_from,
  cba_search_continue_from_to
};

struct ephi_df_search stops_search = {
  stops_search_seen,
  stops_search_set_seen,
  stops_search_reach_from_to, 
  stops_search_start_from,
  stops_search_continue_from_to
};

  
/* depth-first replacement search used during temp ESSA minimization.  */
struct ephi_df_search replacing_search = {
  repl_search_seen,
  repl_search_set_seen,
  repl_search_reach_from_to,
  repl_search_start_from,
  repl_search_continue_from_to
};

static void do_ephi_df_search_1 (struct ephi_df_search, tree);
static void do_ephi_df_search (struct expr_info *, struct ephi_df_search);

static inline bool any_operand_injured (tree);
static void code_motion (struct expr_info *);
static tree pick_ssa_name (tree stmt);
#if 0
static tree calculate_increment (struct expr_info *, tree);
#endif
static bool can_insert (tree, int);
static void set_save (struct expr_info *, tree);
static tree reaching_def (tree, tree, basic_block, tree);
static tree do_proper_save (tree , tree, int);
static void process_left_occs_and_kills (varray_type, tree);
static tree create_expr_ref (struct expr_info *, tree, enum tree_code,
                             basic_block, tree);
static inline bool ephi_will_be_avail (tree);
static inline tree ephi_at_block (basic_block);
static tree get_default_def (tree, htab_t);
static inline bool same_e_version_real_occ_real_occ (struct expr_info *,
						     const tree, 
						     const tree);
static inline bool load_modified_phi_result (basic_block, tree);
static inline bool same_e_version_phi_result (struct expr_info *,
					      tree, tree, tree);
static inline bool load_modified_real_occ_real_occ (tree, tree);
static inline bool same_e_version_real_occ_phi_opnd (struct expr_info *, 
						     tree, basic_block, 
						     int, tree, bool *);
static inline bool injured_real_occ_real_occ (struct expr_info *,
					      tree, tree);
static inline bool injured_phi_result_real_occ (struct expr_info *,
						tree, tree, basic_block);
static inline bool injured_real_occ_phi_opnd (struct expr_info *,
					      tree, basic_block, int);
static void compute_du_info (struct expr_info *);
static void add_ephi_use (tree, tree, int);
static void insert_one_operand (struct expr_info *, tree, int, tree, edge, 
				tree **);
static void collect_expressions (basic_block, varray_type *);
static int build_dfn_array (basic_block, int);
static int eref_compare (const void *, const void *);


/* Bitmap of E-PHI predecessor operands have already been created. 
   We only create one phi-pred per block.  */
static bitmap created_phi_preds;

/* PRE dominance frontiers.  */
static bitmap *pre_dfs;

/* Number of redundancy classes.  */
static int class_count = 0;


/* Iterated dominance frontiers cache.  */
static bitmap *idfs_cache;

/* Partial redundancies statistics. */
static struct pre_stats_d
{
  int reloads;
  int saves;
  int repairs;
  int newphis;
  int ephi_allocated;
  int ephis_current;
  int eref_allocated;
  int exprs_generated;  
} pre_stats = {0, 0, 0, 0, 0, 0, 0, 0};


/* USE entry in list of uses of ephi's.  */
struct ephi_use_entry
{
  tree phi;
  int opnd_indx;
};

/* PRE Expression specific info.  */  
struct expr_info
{
  /* The actual expression.  */
  tree expr;
  /* The occurrences. */
  varray_type occurs;
  /* The kills. */
  varray_type kills;
  /* The left occurrences. */
  varray_type lefts;
  /* An array of real occurrences. */
  varray_type reals;
  /* True if it's a strength reduction candidate. */
  bool strred_cand;
  /* True if it's a load PRE candidate. */
  bool loadpre_cand;
  /* The euses/ephis in preorder dt order. */
  varray_type euses_dt_order;
  /* The name of the temporary for this expression. */
  tree temp;
};


/* Cache of expressions generated for given phi operand, to avoid
   recomputation and wasting memory.  */
static tree *phi_pred_cache;
static int n_phi_preds;

/* Trying to lookup ephi pred operand indexes takes forever on graphs
   that have high connectivity because it's an O(n) linked list
   traversal.  Thus, we set up a hashtable that tells us the operand
   index for a given edge.  */

typedef struct ephi_pred_index_elt
{
  tree ephi;
  edge edge;
  int opnd;
} ephi_pindex_t;

/* Hash an (ephi, edge, opnd) tuple.  */

static hashval_t
ephi_pindex_hash (const void *p)
{
  const ephi_pindex_t *ep = (const ephi_pindex_t *)p;
  return htab_hash_pointer (ep->ephi) + htab_hash_pointer (ep->edge);
}

/* Determine equality of an (ephi, edge, opnd) tuple.  */

static int
ephi_pindex_eq (const void *p1, const void *p2)
{
  const ephi_pindex_t *ep1 = (const ephi_pindex_t *)p1;
  const ephi_pindex_t *ep2 = (const ephi_pindex_t *)p2;
  
  return ep1->ephi == ep2->ephi && ep1->edge == ep2->edge;
}

/* The (ephi, edge) => opnd mapping hashtable.  */
static htab_t ephi_pindex_htab;

/* Add an ephi predecessor to a PHI.  */

static int
add_ephi_pred (tree phi, tree def, edge e)
{
  int i = EPHI_NUM_ARGS (phi);
  void **slot;
  ephi_pindex_t ep, *epp;
  
  EPHI_ARG_PRED (phi, i) = def;
  EPHI_ARG_EDGE (phi, i) = e;

  ep.ephi = phi;
  ep.edge = e;
  slot = htab_find_slot (ephi_pindex_htab, (void *)&ep, INSERT);
  if (*slot == NULL)
    {
      epp = xmalloc (sizeof (*epp));
      epp->ephi = phi;
      epp->edge = e;
      epp->opnd = i;
      *slot = (void *)epp;
    }
  else 
    abort ();
  
  EPHI_NUM_ARGS (phi)++;
  return i;
}

/* Create a new EPHI node at basic block BB.  */

static tree
create_ephi_node (basic_block bb, unsigned int add)
{
  tree phi;
  int len;
  edge e;
  size_t size;
  bb_ann_t ann;

  for (len = 0, e = bb->pred; e; e = e->pred_next)
    len++;
  size = (sizeof (struct tree_ephi_node)
	  + ((len - 1) * sizeof (struct ephi_arg_d)));

  phi = xmalloc (size);
  memset (phi, 0, size);
  if (add)
    {
      ann = bb_ann (bb);
      if (ann->ephi_nodes == NULL)
	ann->ephi_nodes = phi;
      else
	chainon (ann->ephi_nodes, phi);
    }
  pre_stats.ephi_allocated += size;
  pre_stats.ephis_current += 1;
  TREE_SET_CODE (phi, EPHI_NODE);
  EPHI_NUM_ARGS (phi) = 0;
  EPHI_ARG_CAPACITY (phi) = len;

  /* Associate BB to the PHI node.  */
  set_bb_for_stmt (phi, bb);

  return phi;
}

/* Given DEF (which can be an SSA_NAME or entire statement), and VAR,
   find a use of VAR on the RHS of DEF, if one exists. Abort if we
   can't find one.  */

static inline tree
find_rhs_use_for_var (tree def, tree var)
{
  tree ret = maybe_find_rhs_use_for_var (def, var, 0);
  if (!ret)
    abort ();
  return ret;
}

/* Determine if two trees are referring to the same variable. 
   Handles SSA_NAME vs non SSA_NAME, etc.  Uses operand_equal_p for
   non-trivial cases (INDIRECT_REF and friends).  */

static inline bool
names_match_p (const tree t1, const tree t2)
{
  tree name1, name2;

  if (t1 == t2)
    return true;
  
  if (TREE_CODE (t1) == INDIRECT_REF)
    return names_match_p (TREE_OPERAND (t1, 0), t2);
  
  if (TREE_CODE (t2) == INDIRECT_REF)
    return names_match_p (t1, TREE_OPERAND (t2, 0));
  
  if (TREE_CODE (t1) == SSA_NAME)
    name1 = SSA_NAME_VAR (t1);
  else if (DECL_P (t1))
    name1 = t1;
  else
    name1 = NULL_TREE;

  if (TREE_CODE (t2) == SSA_NAME)
    name2 = SSA_NAME_VAR (t2);
  else if (DECL_P (t2))
    name2 = t2;
  else
    name2 = NULL_TREE;

  if (name1 == NULL_TREE && name2 != NULL_TREE)
    return false;
  if (name2 == NULL_TREE && name1 != NULL_TREE)
    return false;
  if (name1 == NULL_TREE && name2 == NULL_TREE)
    return operand_equal_p (t1, t2, 0);

  return name1 == name2;
}

/* Given DEF (which can be an SSA_NAME or entire statement), and VAR,
   find a use of VAR on the RHS of DEF, if one exists.  Return NULL if
   we cannot find one.  */

static inline tree
maybe_find_rhs_use_for_var (tree def, tree var, unsigned int startpos)
{
  use_optype uses;
  size_t i;

  if (SSA_VAR_P (def))
    {
      if (names_match_p (var, def))
	return def;
      return NULL_TREE;
    }
  get_stmt_operands (def);
  uses = STMT_USE_OPS (def);

  for (i = startpos; i < NUM_USES (uses); i++)
    {
      tree use = USE_OP (uses, i);
      if (names_match_p (use, var))
	return use;
    }
  return NULL_TREE;
}

/* Determine if an injuring def is one which we can repair, and thus,
   ignore for purposes of determining the version of a variable.  */

static inline bool
okay_injuring_def (tree inj, tree var)
{
  /* Acceptable injuries are those which
     1. aren't empty statements.
     2. aren't phi nodes.
     3. contain a use of VAR on the RHS.  */
  if (!inj || IS_EMPTY_STMT (inj)
      || TREE_CODE (inj) == PHI_NODE
      || !maybe_find_rhs_use_for_var (inj, var, 0))
    return false;
  return true;
}

/* Return true if INJ is an injuring definition */

static bool
is_injuring_def (struct expr_info *ei, tree inj)
{
  /* Things that are never injuring definitions. */
  if (!inj || IS_EMPTY_STMT (inj) || TREE_CODE (inj) == PHI_NODE)
    return false;

  /* Things we can't handle. */
  if (TREE_CODE (TREE_OPERAND (inj, 1)) != PLUS_EXPR
      && TREE_CODE (TREE_OPERAND (inj, 1)) != MINUS_EXPR)
    return false;

  /* given inj: a1 = a2 + 5
     expr: a3 * c
     we are testing:
     if (a1 != a3
     || ! (a2 exists)
     || a2 != a3)
     return false

     Or, in English,  if either the assigned-to variable in
     the injury is different from the first variable in the
     expression, or the incremented variable is different from the
     first variable in the expression, punt.

     This makes sure we have something of the form

     a = a {+,-} {expr}
     for an expression like "a * 5".

     This limitation only exists because we don't know how to repair
     other forms of increments/decrements. */
  if (!names_match_p (TREE_OPERAND (inj, 0), TREE_OPERAND (ei->expr, 0))
      || !TREE_OPERAND (TREE_OPERAND (inj, 1), 0)
      || !names_match_p (TREE_OPERAND (TREE_OPERAND (inj, 1), 0),
			 TREE_OPERAND (ei->expr, 0)))
    return false;

  /* If we are strength reducing a multiply, we have the additional
     constraints that
     1. {expr} is 1
     2. {expr} and the RHS of the expression are constants. */
  if (TREE_CODE (ei->expr) == MULT_EXPR)
    {
      tree irhs1;
      tree irhs2;
      tree irhs;
      irhs = TREE_OPERAND (inj, 1);
      irhs1 = TREE_OPERAND (irhs, 0);
      irhs2 = TREE_OPERAND (irhs, 1);

      if (TREE_CODE (irhs2) != INTEGER_CST)
	return false;
      if (tree_low_cst (irhs2, 0) == 1)
	return true;
      if (really_constant_p (irhs2)
	  && really_constant_p (TREE_OPERAND (ei->expr, 1)))
	return true;
      /* We don't currently support "the injury is inside a loop,expr is
	 loop-invariant, and b is either loop-invariant or is
	 another induction variable with respect to the loop." */
      return false;
    }
  return true;
}

/* Find the statement defining VAR, ignoring injuries we can repair.
   START is the first potential injuring def. */

static tree
factor_through_injuries (struct expr_info *ei, tree start, tree var,
			 bool *injured)
{
  tree end = start;

  while (is_injuring_def (ei, SSA_NAME_DEF_STMT (end)))
    {
      if (injured)
	*injured = true;
      end = find_rhs_use_for_var (SSA_NAME_DEF_STMT (end), var);
      if (!okay_injuring_def (SSA_NAME_DEF_STMT (end), var))
	break;
      if (dump_file)
	{
	  fprintf (dump_file, "Found a real injury:");
	  print_generic_stmt (dump_file, SSA_NAME_DEF_STMT (end), dump_flags);
	  fprintf (dump_file, "\n");
	}
      if (injured)
	*injured = true;
      end = find_rhs_use_for_var (SSA_NAME_DEF_STMT (end), var);
    }
  return end;
}

/* Return true if the result of the EPHI, when transformed into a phi,
   will be available.  */

static inline bool
ephi_will_be_avail (tree ephi)
{
  if (!EPHI_CANT_BE_AVAIL (ephi))
    if (EPHI_STOPS (ephi))
      return true;

  return false;
}

/* EUSE node pool.  We allocate EUSE nodes out of this*/
static alloc_pool euse_node_pool;

/* EREF node pool.  We allocate regular EREF nodes (like EEXIT_NODE)
   out of this.  */
static alloc_pool eref_node_pool;


/* To order EREF's in a given block, we assign them each an ID based
   on when we see them.  */
static int eref_id_counter = 0;

/* Creation an expression reference of TYPE.  */

static tree
create_expr_ref (struct expr_info *ei, tree expr, enum tree_code type,
		 basic_block bb, tree parent)
{
  tree ret;
  if (type == EPHI_NODE)
    {
      int len;
      edge e;

      ret = create_ephi_node (bb, 1);
      for (len = 0, e = bb->pred; e; e = e->pred_next)
	len++;
      
      EREF_TEMP (ret) = make_phi_node (ei->temp, len);
    }
  else
    {
      if (type == EUSE_NODE)
	ret = (tree) pool_alloc (euse_node_pool);
      else
	ret = (tree) pool_alloc (eref_node_pool);      
      TREE_SET_CODE (ret, type);
      memset (ret, 0, tree_size (ret));      
      TREE_SET_CODE (ret, type);
      pre_stats.eref_allocated += tree_size (ret);
    }

  EREF_NAME (ret) = expr;
  set_bb_for_stmt (ret, bb);
  EREF_STMT (ret) = parent;
  EREF_SAVE (ret) = false;
  EREF_ID (ret) = eref_id_counter++;
  
  return ret;
}


/* dfphis is a bitmap of where we need to insert ephis due to the
   iterated dominance frontier of an expression.  */

static bitmap dfphis;

/* varphis is a bitmap of where we need to insert ephis due to the
   presence of phis for a variable.  */

static bitmap varphis;


/* Function to recursively figure out where EPHI's need to be placed
   because of PHI's.
   We always place EPHI's where we place PHI's because they are also
   partially anticipated expression points (because some expression
   alteration reaches that merge point).

   We do this recursively, because we have to figure out
   EPHI's for the variables in the PHI as well. */

static void
set_var_phis (struct expr_info *ei, tree phi)
{
  basic_block bb = bb_for_stmt (phi);
  /* If we've already got an EPHI set to be placed in PHI's BB, we
     don't need to do this again. */
  if (!bitmap_bit_p (varphis, bb->index)
	&& !bitmap_bit_p (dfphis, bb->index))
    {
      tree phi_operand;
      int curr_phi_operand;
      bitmap_set_bit (varphis, bb->index);
      for (curr_phi_operand = 0;
           curr_phi_operand < PHI_NUM_ARGS (phi);
           curr_phi_operand++)
        {
          phi_operand = PHI_ARG_DEF (phi, curr_phi_operand);
	  /* For strength reduction, factor through injuries we can
	     repair. */
	  if (ei->strred_cand && TREE_CODE (phi_operand) != PHI_NODE)
	    {
	      phi_operand = factor_through_injuries (ei, phi_operand,
						     SSA_NAME_VAR (phi_operand),
						     NULL);
	      phi_operand = SSA_NAME_DEF_STMT (phi_operand);
	      if (dump_file)
		{
		  fprintf (dump_file, "After factoring through injuries:");
		  print_generic_stmt (dump_file, phi_operand, dump_flags);
		  fprintf (dump_file, "\n");
		}
	    }

	  /* If our phi operand is defined by a phi, we need to
	     record where the phi operands alter the expression as
	     well, and place EPHI's at each point. */
          if (TREE_CODE (phi_operand) == PHI_NODE)
            set_var_phis (ei, phi_operand);
        }
    }
}


/* Clear all the expression reference arrays.  */

static void
clear_all_eref_arrays (void)
{
  basic_block bb;
  bb_ann_t ann;
  
  FOR_ALL_BB (bb)
    {
      ann = bb_ann (bb);
      if (ann->ephi_nodes)
	{
	  free (ann->ephi_nodes);
	  pre_stats.ephis_current -= 1;
	}
      ann->ephi_nodes = NULL;
    }
}

/* EPHI insertion algorithm.  */

static bool
expr_phi_insertion (bitmap *dfs, struct expr_info *ei)
{
  size_t i, j;
  vuse_optype vuses;
  use_optype uses;
  bool retval = true;

  dfphis = BITMAP_XMALLOC ();
  bitmap_zero (dfphis);
  varphis = BITMAP_XMALLOC ();
  bitmap_zero (varphis);
  
  /*  Compute where we need to place EPHIS. There are two types of
      places we need EPHI's: Those places we would normally place a
      PHI for the occurrence (calculated by determining the IDF+ of
      the statement), and those places we need an EPHI due to partial
      anticipation.  */
  for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->occurs); i++)
    {
      tree occurp = VARRAY_TREE (ei->occurs, i);
      tree occur = occurp ? occurp : NULL;
      tree killp = VARRAY_TREE (ei->kills, i);
      tree kill = killp ? killp : NULL;
      tree leftp = VARRAY_TREE (ei->lefts, i);
      tree left = leftp ? leftp : NULL;
      bitmap temp;
      stmt_ann_t ann;

#ifdef ENABLE_CHECKING
      if ((kill && occur) || (left && occur) || (kill && left))
	abort();
#endif
      occurp = occur ? occurp : kill ? killp : leftp;
      occur = occur ? occur : kill ? kill : left;
      temp = compute_idfs (dfs, occur);
      bitmap_a_or_b (dfphis, dfphis, temp);      
      if (kill != NULL)
	continue;
      get_stmt_operands (occurp);     
      ann = stmt_ann (occurp);
      uses = USE_OPS (ann);
      for (j = 0; j < NUM_USES (uses); j ++)
	{
	  tree use = USE_OP (uses, j);
	  if (ei->strred_cand)
	    use = factor_through_injuries (ei, use, SSA_NAME_VAR (use),
					   NULL);
	  if (TREE_CODE (SSA_NAME_DEF_STMT (use)) != PHI_NODE)
	    continue;
	  set_var_phis (ei, SSA_NAME_DEF_STMT (use));
	}
      if (ei->loadpre_cand && TREE_CODE (ei->expr) == INDIRECT_REF)
	{  
	  vuses = VUSE_OPS (ann);
	  for (j = 0; j < NUM_VUSES (vuses); j ++)
	    {
	      tree use = VUSE_OP (vuses, j);
	      if (ei->strred_cand)
		use = factor_through_injuries (ei, use, SSA_NAME_VAR (use),
					       NULL);
	      if (TREE_CODE (SSA_NAME_DEF_STMT (use)) != PHI_NODE)
		continue;
	      set_var_phis (ei, SSA_NAME_DEF_STMT (use));
	    }
	}
    }
  /* Union the results of the dfphis and the varphis to get the
     answer to everywhere we need EPHIS. */
  bitmap_a_or_b (dfphis, dfphis, varphis);

  /* Now create the EPHI's in each of these blocks. */
  EXECUTE_IF_SET_IN_BITMAP(dfphis, 0, i,
  {
    tree ref = create_expr_ref (ei, ei->expr, EPHI_NODE, BASIC_BLOCK (i),
				NULL);
    EREF_PROCESSED (ref) = false;
    EPHI_DOWNSAFE (ref) = true;
    EPHI_DEAD (ref) = true;
  });
#if 0
  /* If there are no phis, we don't have anything to optimize,
     assuming the dominator optimizer took care of it all.  */
  if (bitmap_first_set_bit (dfphis) == -1)
    retval = false;
#endif
  BITMAP_XFREE (dfphis);
  BITMAP_XFREE (varphis);
  return retval;

}

/* Return the EPHI at block BB, if one exists.  */

static inline tree
ephi_at_block (basic_block bb)
{
  bb_ann_t ann = bb_ann (bb);
  if (ann->ephi_nodes)
    return ann->ephi_nodes;
  else
    return NULL_TREE;
}

/* Depth first numbering array.  */
static int *dfn;

/* Build a depth first numbering array to be used in sorting in
   dominator order.  */

static int
build_dfn_array (basic_block bb, int num)
{
  basic_block son;

  if (bb->index >= 0)
    dfn[bb->index] = num;

  for (son = first_dom_son (CDI_DOMINATORS, bb);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    num = build_dfn_array (son, ++num);
  return num;
}


/* Compare two EREF's in terms of dominator preorder.  Return -1 if
   ELEM1 goes before ELEM2, 1 if ELEM1 goes after ELEM2, and 0 if they
   are equal.  */

static int
eref_compare (const void *elem1, const void *elem2)
{
  tree t1 = *(tree *)elem1;
  tree t2 = *(tree *)elem2;
  basic_block bb1, bb2; 
  if (t1 == t2)
    return 0;
  bb1 = bb_for_stmt (t1);
  bb2 = bb_for_stmt (t2);
  if (bb1 == bb2)
    {
      if (TREE_CODE (t1) == EEXIT_NODE)
	return 1;
      if (TREE_CODE (t2) == EEXIT_NODE)
	return -1;
      if (TREE_CODE (t1) == EPHI_NODE)
	return -1;
      if (TREE_CODE (t2) == EPHI_NODE)
	return 1;
      if ((TREE_CODE (t1) == EUSE_NODE && EUSE_PHIOP (t1)) 
	  && (TREE_CODE (t2) == EUSE_NODE && !EUSE_PHIOP (t2)))
	return 1;
      if ((TREE_CODE (t1) == EUSE_NODE && !EUSE_PHIOP (t1))
	  && (TREE_CODE (t2) == EUSE_NODE && EUSE_PHIOP (t2)))
	return -1;
      if (TREE_CODE (t1) == EUSE_NODE && TREE_CODE (t2) == EUSE_NODE)
	return EREF_ID (t1) - EREF_ID (t2);
      if (TREE_CODE (t1) == EPHI_NODE && TREE_CODE (t2) == EPHI_NODE)
	abort ();
      
    }
  else
    {
      if (dfn[bb1->index] == dfn[bb2->index])
	{
	  if (dominated_by_p (CDI_DOMINATORS, bb1, bb2))
	    return 1;
	  else
	    return -1;
	}
      else
	return (dfn[bb1->index] < dfn[bb2->index]) ? -1 : 1;
    }

  abort ();
}

/* Create expression references for occurrences, kills, phi operands,
   and the like.  At the same time,  insert the occurrences into the
   ei->euses_dt_order array in the proper order.  If this function had
   any use outside of rename_1, you could split it into two
   functions, one creating, one inserting.  */

static void
create_and_insert_occ_in_preorder_dt_order (struct expr_info *ei)
{
  size_t i;
  edge succ;
  tree curr_phi_pred = NULL_TREE;
  basic_block block;
  
  /* The ephis references were already created, so just push them into
     the euses_dt_order list.  */
  FOR_EACH_BB (block)
  {
    tree ephi = ephi_at_block (block);
    /* The ordering for a given BB is EPHI's, real/left/kill
       occurrences, phi preds, exit occurrences.   */
    if (ephi != NULL_TREE)
      VARRAY_PUSH_TREE (ei->euses_dt_order, ephi);
  }

  /* The non-ephis have to actually be created, so do that, then push
     them into the list.  */

  for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->occurs); i++)
      {
	tree newref;
	tree current;
	current = VARRAY_TREE (ei->occurs, i);
	current = current ? current : VARRAY_TREE (ei->kills, i);
	current = current ? current : VARRAY_TREE (ei->lefts, i);
	block = bb_for_stmt (current);
	if (VARRAY_TREE (ei->kills, i) != NULL)
	  {
	    tree killexpr  = VARRAY_TREE (ei->kills, i);
	    tree killname = ei->expr;
	    newref = create_expr_ref (ei, killname, EKILL_NODE, block, killexpr);
	    VARRAY_PUSH_TREE (ei->euses_dt_order, newref);
	  }
	else if (VARRAY_TREE (ei->lefts, i) != NULL)
	  {
	    tree occurexpr = VARRAY_TREE (ei->lefts, i);
	    tree occurname;
	    occurname = ei->expr;
	    newref = create_expr_ref (ei, occurname, EUSE_NODE, block,
				      occurexpr);
	    EUSE_DEF (newref) = NULL_TREE;
	    EUSE_LVAL (newref) = true;
	    EREF_CLASS (newref) = -1;
	    EUSE_PHIOP (newref) = false;
	    EREF_PROCESSED (newref) = false;
	    VARRAY_PUSH_TREE (ei->euses_dt_order, newref);
	  }
	else
	  {
	    tree occurexpr = VARRAY_TREE (ei->occurs, i);
	    tree occurname;
	    occurname = ei->expr;
	    newref = create_expr_ref (ei, occurname, EUSE_NODE, block,
				      occurexpr);
	    EUSE_DEF (newref) = NULL_TREE;
	    EREF_CLASS (newref) = -1;
	    EUSE_PHIOP (newref) = false;
	    EREF_PROCESSED (newref) = false;
	    VARRAY_PUSH_TREE (ei->euses_dt_order, newref);
	  }
      }

  /* Lastly, we need to create and insert the ephi operand occurrences
     into the list.  */
  FOR_ALL_BB (block)
  {
    /* Insert the phi operand occurrences into the list at the
       successors.*/
    for (succ = block->succ; succ; succ = succ->succ_next)
      {
	if (succ->dest != EXIT_BLOCK_PTR)
	  {
	    tree ephi = ephi_at_block (succ->dest);
	    if (ephi != NULL 
		&& !bitmap_bit_p (created_phi_preds, block->index))
	      {
		tree newref = create_expr_ref (ei, 0, EUSE_NODE, block, NULL);
		curr_phi_pred = newref;
		VARRAY_PUSH_TREE (ei->euses_dt_order, newref);
		EUSE_DEF (newref) = NULL_TREE;
		EREF_CLASS (newref) = -1;
		EUSE_PHIOP (newref) = true;
		EREF_SAVE (newref) = false;
		EREF_RELOAD (newref) = false;
		EUSE_INSERTED (newref) = false;
		EREF_PROCESSED (newref) = false;
		bitmap_set_bit (created_phi_preds, block->index);
		add_ephi_pred (ephi, newref, succ); 
	      }
	    else if (ephi != NULL)
	      {
#ifdef ENABLE_CHECKING
		if (curr_phi_pred == NULL_TREE)
		  abort();
#endif
		add_ephi_pred (ephi, curr_phi_pred, succ);
	      }
	  }	
	else if (succ->dest == EXIT_BLOCK_PTR && !(succ->flags & EDGE_FAKE))
	  {
	    /* No point in inserting exit blocks into heap first, since
	       they'll never be anything on the stack. */
	    tree newref;
	    newref = create_expr_ref (ei, ei->expr, EEXIT_NODE, 
				      block,
				      NULL);
	    VARRAY_PUSH_TREE (ei->euses_dt_order, newref); 
	  }
      }
  }
  qsort (ei->euses_dt_order->data.tree, 
	 VARRAY_ACTIVE_SIZE (ei->euses_dt_order), 
	 sizeof (tree),
	 eref_compare);
}

  
/* Assign a new redundancy class to the occurrence, and push it on the
   renaming stack.  */

static void
assign_new_class (tree occ, varray_type * stack, varray_type * stack2)
{
  /* class(occ) <- count
     Push(occ, stack)
     count <- count + 1
  */
  EREF_CLASS (occ) = class_count;
  VARRAY_PUSH_TREE (*stack, occ);
  if (stack2)
    VARRAY_PUSH_TREE (*stack2, occ);
  class_count++;
}

/* Determine if two real occurrences have the same ESSA version.
   We do this by hashing the expressions and comparing the hash
   values.  Even if they don't match, we then see if this is a
   strength reduction candidate, and if so, if the use is simply
   injured.  */

static inline bool
same_e_version_real_occ_real_occ (struct expr_info *ei,
				  const tree def, const tree use)
{
  hashval_t expr1val;
  hashval_t expr2val;
  vuse_optype vuses;
  size_t i;
  const tree t1 = EREF_STMT (def);
  const tree t2 = EREF_STMT (use);

  expr1val = iterative_hash_expr (TREE_OPERAND (t1, 1), 0);
  expr2val = iterative_hash_expr (TREE_OPERAND (t2, 1), 0);
  
  if (expr1val == expr2val)
    {
      vuses = STMT_VUSE_OPS (t1);
      for (i = 0; i < NUM_VUSES (vuses); i++)
        expr1val = iterative_hash_expr (VUSE_OP (vuses, i), expr1val);
      vuses = STMT_VUSE_OPS (t2);
      for (i = 0; i < NUM_VUSES (vuses); i++)
        expr2val = iterative_hash_expr (VUSE_OP (vuses, i), expr2val);
      if (expr1val != expr2val)
        return false;
    }

  /* If the def is injured, and the expressions have the same value,
     then the use is injured.  */
  if (expr1val == expr2val)
    {
      if (EREF_INJURED (def))
	EREF_INJURED (use) = true;
      return true;
    }

  /* Even if the expressions don't have the same value, it might be
     the case that the use is simply injured, in which case, it's
     still okay.  */
  if (expr1val != expr2val && ei->strred_cand)
    {
      if (injured_real_occ_real_occ (ei, def, use))
	{	
	  EREF_INJURED (use) = true;
	  return true;
	}
    }
  return false;
}  

/* Determine if the use occurrence is injured.
   TODO: Finish actually implementing this.  */

static inline bool
injured_real_occ_real_occ (struct expr_info *ei ATTRIBUTE_UNUSED, 
			   tree def ATTRIBUTE_UNUSED, 
			   tree use ATTRIBUTE_UNUSED)
{
  tree defstmt;
  tree defvar;
  
  defstmt = EREF_STMT (def);
  if (TREE_CODE (TREE_OPERAND (defstmt, 0)) != SSA_NAME)
    return false;
  
  defvar = TREE_OPERAND (defstmt, 0);
  /* XXX: Implement.  */
  return false;
  
}

/* Determine the operand number of edge E in EPHI.  */

static inline int
opnum_of_ephi (const tree ephi, const edge e)
{
  ephi_pindex_t ep, *epp;
  
  ep.ephi = ephi;
  ep.edge = e;
  epp = htab_find (ephi_pindex_htab, &ep);
  if (epp == NULL)
    abort ();
  return epp->opnd;
}

/* Determine the phi operand index for J in PHI.  */

static inline int
opnum_of_phi (tree phi, int j)
{
  int i;
  /* We can't just count predecessors, since tree-ssa.c generates them
     when it sees a phi in the successor during it's traversal.  So the
     order is dependent on the traversal order.  */
  for (i = 0 ; i < PHI_NUM_ARGS (phi); i++)
    if (PHI_ARG_EDGE (phi, i)->src->index == j)
      return i;

  abort();
}

/* Generate EXPR as it would look in basic block PRED (using the phi in
   block BB).  We do this by replacing the variables with the phi
   argument definitions for block J if they are defined by a phi in
   block BB.  */

static void
generate_expr_as_of_bb (tree expr, basic_block pred, basic_block bb)
{
  use_optype uses = STMT_USE_OPS (expr);
  bool replaced_constants = false;
  size_t k;

  for (k = 0; k < NUM_USES (uses); k++)
    {
      tree *vp = USE_OP_PTR (uses, k);
      tree v = *vp;
      tree phi;

      for (phi = phi_nodes (bb); phi; phi = TREE_CHAIN (phi))
	{
	  if (PHI_RESULT (phi) ==  v)
	    {
	      int opnum = opnum_of_phi (phi, pred->index);
	      tree p = PHI_ARG_DEF (phi, opnum);
	      replace_exp (vp, p);
	      if (!phi_ssa_name_p (p))
		replaced_constants = true;
	      break;
	    }
	}
    }

  /* If we've substituted in new constants, we must be sure to
     simplify the result lest we crash in get_expr_operands.  */
  if (replaced_constants)
    fold_stmt (&expr);
}

/* Generate VUSE ops as they would look in basic block PRED (using the
   phi in block BB).  Done the same way as we do generation of regular
   ops for the bb.  */

static void
generate_vops_as_of_bb (tree expr, basic_block pred, basic_block bb)
{
  vuse_optype vuses = STMT_VUSE_OPS (expr);
  size_t i;

  for (i = 0; i < NUM_VUSES (vuses); i++)
    {
      tree v = VUSE_OP (vuses, i);
      tree phi;

      for (phi = phi_nodes (bb); phi; phi = TREE_CHAIN (phi))
	{
	  if (PHI_RESULT (phi) == v)
	    {
	      int opnum = opnum_of_phi (phi, pred->index);
	      tree p = PHI_ARG_DEF (phi, opnum);
	      replace_exp (VUSE_OP_PTR (vuses, i), p);
	      break;
	    }
	}
    }
}

/* Make a copy of Z as it would look in basic block PRED, using the PHIs
   in BB. */

static tree
subst_phis (struct expr_info *ei, tree Z, basic_block pred, basic_block bb)
{
  tree stmt_copy;
  size_t i;

  /* Return the cached version, if we have one. */
  if (pred->index < n_phi_preds 
      && phi_pred_cache[pred->index] != NULL_TREE)
    return phi_pred_cache[pred->index];

  /* Otherwise, generate a new expression.  */
  pre_stats.exprs_generated++;
  stmt_copy = unshare_expr (Z);
  create_stmt_ann (stmt_copy);
  modify_stmt (stmt_copy);
  get_stmt_operands (stmt_copy);
  generate_expr_as_of_bb (stmt_copy, pred, bb);
  set_bb_for_stmt (stmt_copy, bb);
  modify_stmt (stmt_copy);
  get_stmt_operands (stmt_copy);

  /* If we have vuses on the original statement, and we still have
     use_ops on the generated expr, we need to copy the vuses.  */ 
  
  if (ei->loadpre_cand
      && NUM_VUSES (STMT_VUSE_OPS (Z)) != 0
      && NUM_USES (STMT_USE_OPS (stmt_copy)) != 0)
    {
      vuse_optype vuses = STMT_VUSE_OPS (Z);
      remove_vuses (stmt_copy);

      start_ssa_stmt_operands (stmt_copy);
      for (i = 0; i < NUM_VUSES (vuses); i++)
        add_vuse (VUSE_OP (vuses, i), stmt_copy);
      finalize_ssa_stmt_operands (stmt_copy);

      generate_vops_as_of_bb (stmt_copy, pred, bb);
    }
  else
    {
      remove_vuses (stmt_copy);
      remove_vdefs (stmt_copy);
    }

  if (pred->index < n_phi_preds)
    phi_pred_cache[pred->index] = stmt_copy;
  return stmt_copy;
}

/* Determine if def and use_tree should have the same e-version.  We do
   this by simply determining if something modifies the expression
   between DEF and USE_TREE.  USE_TREE was generated from the OPND_NUM'th
   operand of the EPHI in USE_BB.  If it is modified, we determine if
   it is simply injured, and thus, okay.  */

static inline bool 
same_e_version_real_occ_phi_opnd (struct expr_info *ei, tree def,
				       basic_block use_bb, int opnd_num,
				       tree use_tree, bool *injured)
{
  bool not_mod = true;
  *injured = false;
  
  if (load_modified_real_occ_real_occ (EREF_STMT (def), 
				       use_tree))
    not_mod = false;
  
  if (not_mod)
    return true;
  else if (ei->strred_cand)
    {
      if (injured_real_occ_phi_opnd (ei, def, use_bb, opnd_num))
	return true;
    }
  return false;
}

/* Determine whether the OPND_NUM'th operand of USE_BB's EPHI is an
   injured version of DEF.  */
static inline bool 
injured_real_occ_phi_opnd (struct expr_info *ei ATTRIBUTE_UNUSED,
				tree def ATTRIBUTE_UNUSED,
				basic_block use_bb ATTRIBUTE_UNUSED, 
				int opnd_num ATTRIBUTE_UNUSED)
{
  /* XXX: Implement. */
  return false;
}

/* Determine whether the expression is modified between DEF and USE,
   by comparing the hash values of the two expressions.  */
static inline bool 
load_modified_real_occ_real_occ (tree def, tree use)
{
  hashval_t expr1val;
  hashval_t expr2val;
  vuse_optype vuses;
  size_t i;
  
  if (TREE_CODE (def) == VA_ARG_EXPR)
    expr1val = iterative_hash_expr (def, 0);
  else
    expr1val = iterative_hash_expr (TREE_OPERAND (def, 1), 0);

  if (TREE_CODE (use) == VA_ARG_EXPR)
    expr2val = iterative_hash_expr (use, 0);
  else
    expr2val = iterative_hash_expr (TREE_OPERAND (use, 1), 0);
  
  if (expr1val == expr2val)
    {
      vuses = STMT_VUSE_OPS (def);
      for (i = 0; i < NUM_VUSES (vuses); i++)
        expr1val = iterative_hash_expr (VUSE_OP (vuses, i), expr1val);
      vuses = STMT_VUSE_OPS (use);
      for (i = 0; i < NUM_VUSES (vuses); i++)
        expr2val = iterative_hash_expr (VUSE_OP (vuses, i), expr2val);
      if (expr1val != expr2val)
	return false;
    }
  return expr1val != expr2val;
}

/* Determine if the expression is modified between the start of BB,
   and the use at USE, ignoring phis.  We do this by simple
   domination, because of the properties of SSA.  */
static bool 
load_modified_phi_result (basic_block bb, tree use)
{
  basic_block defbb = bb_for_stmt (SSA_NAME_DEF_STMT (use));
  if (defbb != bb)
    {
      /* This guards against moving around undefined variables.
	 However, PARM_DECL is special because it *IS* live on entry,
	 so it's not really undefined.  */
      if (!defbb && TREE_CODE (SSA_NAME_VAR (use)) != PARM_DECL)
	return true;
      else if (!defbb && TREE_CODE (SSA_NAME_VAR (use)) == PARM_DECL)
        return false;
      if (dominated_by_p (CDI_DOMINATORS, bb, defbb))
	return false;
    }
  else
    {
      if (TREE_CODE (SSA_NAME_DEF_STMT (use)) == PHI_NODE)
        return false;
    }
  return true;
}

/* Determine if the variables in EXP are modified between DEF and
   USE.  If they are, we have to give a new e-version to the result. 
   For load PRE, we have to check the vuses too.  For strength
   reduction, we need to check whether the modification is a simple
   injury.  */

static bool 
same_e_version_phi_result (struct expr_info *ei, tree def, tree exp,
				tree use)
{
  stmt_ann_t ann = stmt_ann (exp);
  bool not_mod = true;
  size_t i;
  use_optype real_expuses = USE_OPS (ann);
  vuse_optype expuses;
  

  if (NUM_USES (real_expuses) == 0)
    return false;
  
  for (i = 0; i < NUM_USES (real_expuses) && not_mod; i++)
    {
      tree *use1p = USE_OP_PTR (real_expuses, i);
      tree use1;  
      if (!use1p)
	continue;
      use1 = *use1p;
      if (load_modified_phi_result (bb_for_stmt (def), use1))
	not_mod = false;
    }
  
  if (not_mod && ei->loadpre_cand)
    {
      expuses = VUSE_OPS (ann);
      
      for (i = 0; i < NUM_VUSES (expuses) && not_mod; i++)
	{
	  tree use1 = VUSE_OP (expuses, i);
	  if (load_modified_phi_result (bb_for_stmt (def), use1))
	    not_mod = false;
	}
    }
    
  if (not_mod)
    return true;  
  else if (ei->strred_cand)
    {
      if (injured_phi_result_real_occ (ei, def, exp, bb_for_stmt (use)))
	{
	  EREF_INJURED (use) = true;
	  return true;
	}
    }
  
  return false;
}

/* Determine whether USE_TREE is an injured version of DEF.  */

static inline bool
injured_phi_result_real_occ (struct expr_info *ei ATTRIBUTE_UNUSED, 
			     tree def ATTRIBUTE_UNUSED, 
			     tree use_tree ATTRIBUTE_UNUSED,
			     basic_block use_bb ATTRIBUTE_UNUSED)
{
  /* XXX: Implement.  */
  return false;
}

/* Delayed renaming checks to make sure the optimistic assumptions
   about ephi operand versions in rename_1 actually turned out to be
   true.  This requires generating the expressions for each ephi
   operand, and comparing them to the versions of the occurrence it is
   defined by.  
   Delayed rename handling is done like open64 does it.  Basically,
   like the delayed renaming is described in the paper, with
   extensions for strength reduction.  */

static void
process_delayed_rename (struct expr_info *ei, tree use, tree real_occ)
{
  tree exp_phi = use;
  int opnd_num = 0;

  /* We only care about operands we actually have DELAYED_RENAME set
     on.  */
  for (opnd_num = 0; opnd_num < EPHI_NUM_ARGS (exp_phi); opnd_num++)
    {
      tree opnd = EPHI_ARG_DEF (exp_phi, opnd_num);
      if (EPHI_ARG_DELAYED_RENAME (exp_phi, opnd_num))
	{
	  tree def;
	  tree newexp;

	  /* Mark it as being processed, then generate the ephi
	     operand expression.  */
	  EPHI_ARG_DELAYED_RENAME (exp_phi, opnd_num) = false;
	  def = opnd;
	  newexp = subst_phis (ei, real_occ,
			      EPHI_ARG_EDGE (exp_phi, opnd_num)->src,
			      bb_for_stmt (exp_phi));

	  /* For operands defined by EPHIs, we need to compare the
	     generated expression and the phi result.
	     For operands defined by real occurrences, we simply compare
	     the phi operand and the real occurrence.  */
	  if (TREE_CODE (def) == EPHI_NODE)
	    {
	      tree tmp_use = EPHI_ARG_PRED (exp_phi, opnd_num);	     
	      EREF_STMT (tmp_use) = newexp;
	      if (same_e_version_phi_result (ei, def, newexp,
					     tmp_use))
		{
		  
		  if (EREF_INJURED (tmp_use))
		    {
		      EREF_INJURED (tmp_use) = false;
		      EPHI_ARG_INJURED (exp_phi, opnd_num) = true;
		    }
		  if (EREF_STMT (def) == NULL) 
		    {
		      /* If it was injured, we need to make up a new
			 phi result with the actually injured
			 version.  */
		      if (EPHI_ARG_INJURED (exp_phi, opnd_num))
			{
			  /* XXX: Allocate phi result with correct version.  */
			  
			}	
		      EREF_STMT (def) = newexp;
		      process_delayed_rename (ei, def, newexp);
		    }
		}
	      /* If it's not the same version, the defining ephi can't
		 be downsafe, and the operand is not really defined by
		 anything.  */
	      else
		{
		  EPHI_DOWNSAFE (def) = false;
		  EPHI_ARG_DEF (exp_phi, opnd_num) = NULL;		  
		}
	    }
	  else if (TREE_CODE (def) == EUSE_NODE && !EUSE_PHIOP (def))
	    {
	      bool injured = false;
	      if (same_e_version_real_occ_phi_opnd (ei, def, 
						    bb_for_stmt (use),
						    opnd_num, newexp, &injured))
		{
		  tree tmp_use = EPHI_ARG_PRED (exp_phi, opnd_num);
		  EPHI_ARG_HAS_REAL_USE (exp_phi, opnd_num) = true;
		  /*		  EREF_STMT (opnd) = EREF_STMT (def); */
		  if (injured || EREF_INJURED (def))
		    EREF_INJURED (def) = true;
		  if (injured || EREF_INJURED (def))
		    EREF_INJURED (opnd) = true;
		  else
		    EREF_STMT (tmp_use) = EREF_STMT (def);
		  if (EUSE_DEF (def) != NULL)
		    EPHI_ARG_DEF (exp_phi, opnd_num) = EUSE_DEF (def);
		  else
		    EPHI_ARG_DEF (exp_phi, opnd_num) = def;
		}
	      else
		{
		  EPHI_ARG_DEF (exp_phi, opnd_num) = NULL;
		}
	    }
	}
    }
}

/* For the uninitiated, the algorithm is a modified SSA renaming
   algorithm (working on expressions rather than variables) .  We
   attempt to determine which expression occurrences have the same
   ESSA version (we call it class, for equivalence/redundancy class,
   which is what the papers call it.  Open64 calls it e-version), and
   which occurrences are actually operands for an EPHI (since this has
   to be discovered from the program). 

   Renaming is done like Open64 does it.  Basically as the paper says, 
   except that we try to use earlier defined occurrences if they are
   available in order to keep the number of saves down. */

static void
rename_1 (struct expr_info *ei)
{
  tree occur;
  basic_block phi_bb;
  size_t i;
  varray_type stack;

  VARRAY_GENERIC_PTR_NOGC_INIT (stack, 1, "Stack for renaming");

  /* Start by creating and inserting the occurrences in preorder,
     dominator tree into a list.  */
  create_and_insert_occ_in_preorder_dt_order (ei);
  
  /* Walk the occurrences.  */
  for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->euses_dt_order); i++)
    {
      occur = VARRAY_TREE (ei->euses_dt_order, i);
      
      /* The current occurrence can't have the same version as
	 something on the top of the stack unless it is in a basic
	 block dominated by the basic block of the occurrence on the
	 top of the stack.  */
      while (VARRAY_ACTIVE_SIZE (stack) > 0	     
	     && !dominated_by_p (CDI_DOMINATORS,
			     	 bb_for_stmt (occur),
				 bb_for_stmt (VARRAY_TOP_TREE (stack))))
	
	VARRAY_POP (stack);

      /* If the stack is empty, we assign a new version since it isn't
	 dominated by any other version.  */
      if (VARRAY_ACTIVE_SIZE (stack) == 0 || VARRAY_TOP_TREE (stack) == NULL)
	{
	  if (TREE_CODE (occur) == EPHI_NODE)
	    assign_new_class (occur, &stack, NULL);
	  else if (TREE_CODE (occur) == EUSE_NODE && !EUSE_PHIOP (occur))
	    assign_new_class (occur, &stack, NULL);
	}
      else
	{
	  if (TREE_CODE (occur) == EUSE_NODE && !EUSE_PHIOP (occur))
	    {
	      tree tos = VARRAY_TOP_TREE (stack);

	      if (TREE_CODE (tos) == EUSE_NODE && !EUSE_PHIOP (tos))
		{
		  /* If two real occurrences have the same
		     e-version/class, then this occurrence can be
		     defined by the prior occurrence (or whatever
		     the prior occurrence is defined by, if
		     anything).  
		     Otherwise, we have to assign a new version.
		     lvalue occurrences always need a new version,
		     since they are definitions. */
		  if (!EUSE_LVAL (occur) 
		      && same_e_version_real_occ_real_occ (ei, tos, occur))
		    {
		      
		     
		      tree newdef;
		      EREF_CLASS (occur) = EREF_CLASS (tos);
		      newdef = EUSE_DEF (tos) != NULL ? EUSE_DEF (tos) : tos;
		      EUSE_DEF (occur) = newdef;
		    }		 
		  else
		    assign_new_class (occur, &stack, NULL);
		}
	      else if (TREE_CODE (tos) == EPHI_NODE)
		{
		  /* Here we have an ephi occurrence at the top of the
		     stack, and the current occurrence is a real
		     occurrence.  So determine if the real occurrence
		     has the same version as the result of the phi.  
		     If so, then this real occurrence is defined by the
		     EPHI at the top of the stack.
		     If not, the EPHI isn't downsafe (because something
		     must change in between the ephi result and the next
		     occurrence), and we need a new version for the real
		     occurrence.
		     lvalues always need a new version. */
		  if (!EUSE_LVAL (occur)
		      && same_e_version_phi_result (ei, tos, EREF_STMT (occur),
						    occur))
		    {
		      EREF_CLASS (occur) = EREF_CLASS (tos);
		      EUSE_DEF (occur) = tos;
		      EREF_STMT (tos) = EREF_STMT (occur);

		      VARRAY_PUSH_TREE (stack, occur);
		    }
		  else
		    {
		      EPHI_DOWNSAFE (tos) = false;
		      assign_new_class (occur, &stack, NULL);
		    }
		}
	    }
	  /* EPHI occurrences always get new versions. */
	  else if (TREE_CODE (occur) == EPHI_NODE)
	    {	      
	      assign_new_class (occur, &stack, NULL);
	    }
	  /* EPHI operands are optimistcally assumed to be whatever is
	     at the top of the stack at the time we hit the ephi
	     operand occurrence.  The delayed renaming checks this
	     optimistic assumption for validity.  */
	  else if (TREE_CODE (occur) == EUSE_NODE && EUSE_PHIOP (occur))
	    {
	      basic_block pred_bb = bb_for_stmt (occur);
	      edge e;
	      tree tos = VARRAY_TOP_TREE (stack);
	      for (e = pred_bb->succ; e; e = e->succ_next)
		{
		  tree ephi = ephi_at_block (e->dest);
		  if (ephi != NULL_TREE)
		    {
		      int opnum = opnum_of_ephi (ephi, e);

		      EPHI_ARG_DELAYED_RENAME (ephi, opnum) = true;
		      EPHI_ARG_DEF (ephi, opnum) = tos;
		    }
		}	      
	    }
	  /* No EPHI can be downsafe past an exit node.  */
	  else if (TREE_CODE (occur) == EEXIT_NODE)
	    {
	      if (VARRAY_ACTIVE_SIZE (stack) > 0
		  && TREE_CODE (VARRAY_TOP_TREE (stack)) == EPHI_NODE)
		EPHI_DOWNSAFE (VARRAY_TOP_TREE (stack)) = false;
	    }
	}
    }
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      size_t i;
      fprintf (dump_file, "Occurrences for expression ");
      print_generic_expr (dump_file, ei->expr, dump_flags);
      fprintf (dump_file, " after Rename 1\n");
      for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->euses_dt_order); i++)
	{
	  print_generic_expr (dump_file, 
			      VARRAY_TREE (ei->euses_dt_order, i), 1);
	  fprintf (dump_file, "\n");
	}
    }

  /* Now process the renames for EPHI's that actually have the result
     valid and used.  These will be the EPHI's that have the statement
     set above.  */
  FOR_EACH_BB (phi_bb)
  {
    tree ephi = ephi_at_block (phi_bb);
    if (ephi != NULL && EREF_STMT (ephi) != NULL)
      process_delayed_rename (ei, ephi, EREF_STMT (ephi));
  }

  /* If we didn't process the delayed rename for an ephi argument, 
     but thought we needed to, mark the operand as NULL.  Also, if the
     operand was defined by an  EPHI, we can mark it not downsafe
     because there can't have been a real occurrence (or else we would
     have processed a rename for it).  */
  FOR_EACH_BB (phi_bb)
  {
    tree ephi = ephi_at_block (phi_bb);
    if (ephi != NULL)
      {
	int j;
	for (j = 0; j < EPHI_NUM_ARGS (ephi); j++)
	  {
	    if (EPHI_ARG_DELAYED_RENAME (ephi, j))
	      {
		tree def = EPHI_ARG_DEF (ephi, j);
		if (def && TREE_CODE (def) == EPHI_NODE)
		  EPHI_DOWNSAFE (def) = false;
		EPHI_ARG_DEF (ephi, j) = NULL;
	      }
	  }
      }
  }
  VARRAY_FREE (stack);
}

/* Determine if the EPHI has an argument we could never insert
   or extend the lifetime of, such as an argument occurring on 
   an abnormal edge. */

static bool
ephi_has_unsafe_arg (tree ephi)
{
  int i;
  for (i = 0; i < EPHI_NUM_ARGS (ephi); i++)
    if (EPHI_ARG_EDGE (ephi, i)->flags & EDGE_ABNORMAL)
      return true;
  return false;
}

/* Reset down safety flags for non-downsafe ephis. Uses depth first
   search.  */

static void
reset_down_safe (tree currphi, int opnum)
{
  tree ephi;
  int i;

  if (EPHI_ARG_HAS_REAL_USE (currphi, opnum)) 
    return;
  ephi = EPHI_ARG_DEF (currphi, opnum);
  if (!ephi || TREE_CODE (ephi) != EPHI_NODE)
    return;
  if (!EPHI_DOWNSAFE (ephi))
    return;
  EPHI_DOWNSAFE (ephi) = false;
  for (i = 0; i < EPHI_NUM_ARGS (ephi); i++)
    reset_down_safe (ephi, i);
}

/* Compute down_safety using a depth first search.
   This propagates not fully anticipated phi assignments upwards.  */

static void
compute_down_safety (struct expr_info *ei)
{
  size_t i;
  basic_block bb;
  FOR_EACH_BB (bb)
  {
    tree ephi = ephi_at_block (bb);
    if (ephi == NULL_TREE)
      continue;
    if (ephi_has_unsafe_arg (ephi))
      EPHI_DOWNSAFE (ephi) = false;
  }
  for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->euses_dt_order); i++)
    {
      int j;
      tree ephi = VARRAY_TREE (ei->euses_dt_order, i);
      if (TREE_CODE (ephi) != EPHI_NODE)
	continue;

      if (!EPHI_DOWNSAFE (ephi))
	for (j = 0; j < EPHI_NUM_ARGS (ephi); j++)
	  reset_down_safe (ephi, j);
      
    }
}

/* EPHI use node pool. We allocate ephi_use_node's out of this.  */
static alloc_pool ephi_use_pool;

/* Add a use of DEF to it's use list. The use is at operand OPND_INDX
   of USE.  */

static void 
add_ephi_use (tree def, tree use, int opnd_indx)
{
  struct ephi_use_entry *entry;
  if (EPHI_USES (def) == NULL)
    VARRAY_GENERIC_PTR_INIT (EPHI_USES (def), 1, "EPHI uses");
  entry = (struct ephi_use_entry *) pool_alloc (ephi_use_pool);
  entry->phi = use;
  entry->opnd_indx = opnd_indx;
  VARRAY_PUSH_GENERIC_PTR (EPHI_USES (def), entry);  
}
  
/* Compute def-uses of ephis.  */

static void
compute_du_info (struct expr_info *ei)
{
  size_t i;
  for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->euses_dt_order); i++)
    {
      int j;
      tree ephi = VARRAY_TREE (ei->euses_dt_order, i);
      if (TREE_CODE (ephi) != EPHI_NODE)
	continue;
      for (j = 0; j < EPHI_NUM_ARGS (ephi); j++)
	{
	  tree def = EPHI_ARG_DEF (ephi, j);
	  if (def != NULL_TREE)
	    {
	      if (TREE_CODE (def) == EPHI_NODE)
		add_ephi_use (def, ephi, j);
#ifdef ENABLE_CHECKING
	      else
		if (! (TREE_CODE (def) == EUSE_NODE && !EUSE_PHIOP (def)))
		  abort();
#endif
	    }
	}
    }
}

/* STOPS marks what EPHI's/operands stop forward movement. (IE where
   we can't insert past).  */

static void
compute_stops (struct expr_info *ei)
{
  size_t i;
  
  for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->euses_dt_order); i++)
    {
      tree ephi = VARRAY_TREE (ei->euses_dt_order, i);
      int j;
      
      if (TREE_CODE (ephi) != EPHI_NODE)
	continue;
      if (EPHI_CANT_BE_AVAIL (ephi))
	EPHI_STOPS (ephi) = true;
      for (j = 0; j < EPHI_NUM_ARGS (ephi); j++)
	if (EPHI_ARG_HAS_REAL_USE (ephi, j))
	  EPHI_ARG_STOPS (ephi, j) = true;
    }
  do_ephi_df_search (ei, stops_search);
}

/* Compute will_be_avail property, which consists of cant_be_avail and
   stops properties.  */

static void
compute_will_be_avail (struct expr_info *ei)
{
  do_ephi_df_search (ei, cant_be_avail_search);
  compute_stops (ei);  
}

/* Insert the expressions into ei->euses_dt_order in preorder dt order.  */

static void
insert_euse_in_preorder_dt_order (struct expr_info *ei)
{
  varray_type new_euses_dt_order;
  size_t i;
  VARRAY_GENERIC_PTR_NOGC_INIT (new_euses_dt_order, 1, "EUSEs");

  for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->euses_dt_order); i++)
    {
      tree ref = VARRAY_TREE (ei->euses_dt_order, i);
      if (TREE_CODE (ref) == EUSE_NODE || TREE_CODE (ref) == EPHI_NODE)
	VARRAY_PUSH_TREE (new_euses_dt_order, ref);
    }
  VARRAY_FREE (ei->euses_dt_order);
  ei->euses_dt_order = new_euses_dt_order;
  qsort (ei->euses_dt_order->data.tree, 
	 VARRAY_ACTIVE_SIZE (ei->euses_dt_order), 
	 sizeof (tree),
	 eref_compare);

}

/* Determine if we can insert operand OPND_INDX of EPHI.  */

static bool
can_insert (tree ephi, int opnd_indx)
{
  tree def;

  if (EPHI_ARG_DEF (ephi, opnd_indx) == NULL_TREE)
    return true;
  def = EPHI_ARG_DEF (ephi, opnd_indx);
  if (!EPHI_ARG_HAS_REAL_USE (ephi, opnd_indx))
    if (TREE_CODE (def) == EPHI_NODE && !(ephi_will_be_avail (def)))
      return true;
  return false;
}

/* Find the default definition of VAR.
   This is incredibly ugly, since we have to walk back through all
   the definitions to find the one defined by the empty statement.  */

static tree
get_default_def (tree var, htab_t seen)
{
  def_optype defs;
  size_t i;
  tree defstmt = SSA_NAME_DEF_STMT (var);

  if (IS_EMPTY_STMT (defstmt))
    return var;
  *(htab_find_slot (seen, var, INSERT)) = var;
  if (TREE_CODE (defstmt) == PHI_NODE)
    {
      int j;
      for (j = 0; j < PHI_NUM_ARGS (defstmt); j++)
	if (htab_find (seen, PHI_ARG_DEF (defstmt, j)) == NULL)
	  {
	    if (TREE_CODE (PHI_ARG_DEF (defstmt, j)) == SSA_NAME)
	      {
		tree temp = get_default_def (PHI_ARG_DEF (defstmt, j), seen);
		if (temp != NULL_TREE)
		  return temp;
	      }
	  }
    }


  defs = STMT_DEF_OPS (defstmt);
  for (i = 0; i < NUM_DEFS (defs); i++)
    {
      tree def = DEF_OP (defs, i);
      if (SSA_NAME_VAR (def) == SSA_NAME_VAR (var))
	{
	  if (htab_find (seen, def) != NULL)
	    return NULL;
	  return get_default_def (def, seen);
	}
    }

  /* We should never get here.  */
  abort ();
}

/* Hunt down the right reaching def for VAR, starting with BB.  Ignore
   defs in statement IGNORE, and stop if we hit CURRSTMT.  */

static tree
reaching_def (tree var, tree currstmt, basic_block bb, tree ignore)
{
  tree curruse = NULL_TREE;
  block_stmt_iterator bsi;
  basic_block dom;
  tree phi;

  /* Check phis first. */
  for (phi = phi_nodes (bb); phi; phi = TREE_CHAIN (phi))
    {
      if (phi == currstmt)
	break;
      if (phi == ignore)
	continue;
      if (names_match_p (var, PHI_RESULT (phi)))
	curruse = PHI_RESULT (phi);
    }

  /* We can't walk BB's backwards right now, so we have to walk *all*
     the statements, and choose the last name we find. */
  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      tree stmt = bsi_stmt (bsi);
      tree *def;
      def_optype defs;
      size_t i;

      if (stmt == currstmt)
	break;

      get_stmt_operands (stmt);
      defs = STMT_DEF_OPS (stmt);
      for (i = 0; i < NUM_DEFS (defs); i++)
	{
	  def = DEF_OP_PTR (defs, i);
	  if (def && *def != ignore && names_match_p (var, *def))
	    {
	      curruse = *def;
	      break;
	    }
	}
    }
  if (curruse != NULL_TREE)
    return curruse;
  dom = get_immediate_dominator (CDI_DOMINATORS, bb);
  if (bb == ENTRY_BLOCK_PTR)
    {
      htab_t temp;
      temp = htab_create (7, htab_hash_pointer, htab_eq_pointer, NULL);
      curruse = get_default_def (var, temp);
      htab_delete (temp);
    }
  if (!dom)
    return curruse;
  return reaching_def (var, currstmt, dom, ignore);
}

/* Insert one ephi operand that doesn't currently exist as a use.  */

static void
insert_one_operand (struct expr_info *ei, tree ephi, int opnd_indx, 
		    tree x, edge succ, tree **avdefsp)
{
  /* FIXME.  pre_insert_on_edge should probably disappear.  */
  extern void pre_insert_on_edge (edge, tree);
  tree expr;
  tree temp = ei->temp;
  tree copy;
  tree newtemp;
  basic_block bb = bb_for_stmt (x);

  /* Insert definition of expr at end of BB containing x. */
  copy = TREE_OPERAND (EREF_STMT (ephi), 1);
  copy = unshare_expr (copy);
  expr = build (MODIFY_EXPR, TREE_TYPE (ei->expr),
		temp, copy);
  expr = subst_phis (ei, expr, bb, bb_for_stmt (ephi));
  newtemp = make_ssa_name (temp, expr);  
  TREE_OPERAND (expr, 0) = newtemp;
  copy = TREE_OPERAND (expr, 1);
  if (dump_file)
    {
      fprintf (dump_file, "In BB %d, insert save of ", bb->index);
      print_generic_expr (dump_file, expr, dump_flags);
      fprintf (dump_file, " to ");
      print_generic_expr (dump_file, newtemp, dump_flags);
      fprintf (dump_file, " after ");
      print_generic_stmt (dump_file, last_stmt (bb), dump_flags);
      fprintf (dump_file, " (on edge), because of EPHI");
      fprintf (dump_file, " in BB %d\n", bb_for_stmt (ephi)->index);
    }
		      
  /* Do the insertion.  */
  /* ??? Previously we did bizarre searching, presumably to get
     around bugs elsewhere in the infrastructure.  I'm not sure
     if we really should be using pre_insert_on_edge
     or just bsi_insert_after at the end of BB.  */
  pre_insert_on_edge (succ, expr);

  EPHI_ARG_DEF (ephi, opnd_indx)
    = create_expr_ref (ei, ei->expr, EUSE_NODE, bb, 0);
  EUSE_DEF (x) = EPHI_ARG_DEF (ephi, opnd_indx);
  VARRAY_PUSH_TREE (ei->euses_dt_order, EPHI_ARG_DEF (ephi, opnd_indx));
  qsort (ei->euses_dt_order->data.tree, 
	 VARRAY_ACTIVE_SIZE (ei->euses_dt_order), 
	 sizeof (tree),
	 eref_compare);
  EREF_TEMP (EUSE_DEF (x)) = newtemp;
  EREF_RELOAD (EUSE_DEF (x)) = false;
  EREF_SAVE (EUSE_DEF (x)) = false;
  EUSE_INSERTED (EUSE_DEF (x)) = true;
  EUSE_PHIOP (EUSE_DEF (x)) = false;
  EREF_SAVE (x) = false;
  EREF_RELOAD (x) = false;
  EUSE_INSERTED (x) = true;
  EREF_CLASS (x) = class_count++;
  EREF_CLASS (EUSE_DEF (x)) = class_count++;
  *avdefsp = xrealloc (*avdefsp, sizeof (tree) * (class_count + 1));
  (*avdefsp)[class_count - 2] = x;
  (*avdefsp)[class_count - 1] = EUSE_DEF (x);
  pre_stats.saves++;
}

/* First step of finalization.  Determine which expressions are being
   saved and which are being deleted.
   This is done as a simple dominator based availability calculation,
   using the e-versions/redundancy classes.  */

static bool
finalize_1 (struct expr_info *ei)
{
  tree x;
  int nx;
  bool made_a_reload = false;
  size_t i;
  tree *avdefs;
  
  avdefs = xcalloc (class_count + 1, sizeof (tree));

  for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->euses_dt_order); i++)
    {
      x = VARRAY_TREE (ei->euses_dt_order, i);
      nx = EREF_CLASS (x);

      if (TREE_CODE (x) == EPHI_NODE)
	{
	  if (ephi_will_be_avail (x))
	    avdefs[nx] = x;
	}
      else if (TREE_CODE (x) == EUSE_NODE && EUSE_LVAL (x))
	{
	  avdefs[nx] = x;
	}
      else if (TREE_CODE (x) == EUSE_NODE && !EUSE_PHIOP (x))
	{
	  if (avdefs[nx] == NULL
	      || !dominated_by_p (CDI_DOMINATORS, bb_for_stmt (x), 
				  bb_for_stmt (avdefs[nx])))
	    {
	      EREF_RELOAD (x) = false;
	      avdefs[nx] = x;
	      EUSE_DEF (x) = NULL;
	    }
	  else
	    {
	      EREF_RELOAD (x) = true;
	      made_a_reload = true;
	      EUSE_DEF (x) = avdefs[nx];
#ifdef ENABLE_CHECKING
	      if (EREF_CLASS (x) != EREF_CLASS (avdefs[nx]))
		abort ();
#endif
	    }
	}
      else
	{
	  edge succ;
	  basic_block bb = bb_for_stmt (x);
	  /* For each ephi in the successor blocks.  */
	  for (succ = bb->succ; succ; succ = succ->succ_next)
	    {
	      tree ephi = ephi_at_block (succ->dest);
	      if (ephi == NULL_TREE)
		continue;
	      if (ephi_will_be_avail (ephi))
		{
		  int opnd_indx = opnum_of_ephi (ephi, succ);
#ifdef ENABLE_CHECKING
		  if (EPHI_ARG_PRED (ephi, opnd_indx) != x)
		    abort ();
#endif
		  if (can_insert (ephi, opnd_indx))
		    {
		      insert_one_operand (ei, ephi, opnd_indx, x, succ, 
					  &avdefs);
		    }
		  else
		    {
		      nx = EREF_CLASS (EPHI_ARG_DEF (ephi,opnd_indx));
		      EPHI_ARG_DEF (ephi, opnd_indx) = avdefs[nx];
		    }
		}
	    }
	}
    }
  free (avdefs);
  return made_a_reload;
}

/* Mark the necessary SAVE bits on X.  */

static void
set_save (struct expr_info *ei, tree X)
{
  if (TREE_CODE (X) == EUSE_NODE && !EUSE_PHIOP (X))
    EREF_SAVE (X) = true;
  else if (TREE_CODE (X) == EPHI_NODE)
    {
      int curr_phiop;
      for (curr_phiop = 0; curr_phiop < EPHI_NUM_ARGS (X); curr_phiop++)
	{
	  tree w = EPHI_ARG_DEF (X, curr_phiop);
	  if (!EPHI_ARG_PROCESSED2 (X, curr_phiop))
	    {
	      EPHI_ARG_PROCESSED2 (X, curr_phiop) = true;
	      if (w)
		set_save (ei, w);
	    }  
	}
    }
}

/* DFS Search function: Return true if PHI is can't be available.  */

static bool
cba_search_seen (tree phi)
{
  return EPHI_CANT_BE_AVAIL (phi);
}

/* DFS Search function: Mark PHI as can't be available when seen.  */

static void
cba_search_set_seen (tree phi)
{
  EPHI_CANT_BE_AVAIL (phi) = true;
}

/* DFS Search function: Return true if PHI should be marked can't be
   available due to a NULL operand.  */

static bool 
cba_search_start_from (tree phi)
{
  if (!EPHI_DOWNSAFE (phi))
    {
      int i;
      for (i = 0; i < EPHI_NUM_ARGS (phi); i++)
	if (EPHI_ARG_DEF (phi, i) == NULL_TREE 
	    || EPHI_ARG_EDGE (phi, i)->flags & EDGE_ABNORMAL)
	  return true;
    }
  return false;
}

/* DFS Search function: Return true if the used PHI is not downsafe,
   unless we have a real use for the operand.  */

static bool
cba_search_continue_from_to (tree def_phi ATTRIBUTE_UNUSED,
			     int opnd_indx, 
			     tree use_phi)
{
  if (EPHI_ARG_HAS_REAL_USE (use_phi, opnd_indx) && 
      !(EPHI_ARG_EDGE (use_phi, opnd_indx)->flags & EDGE_ABNORMAL))
    return false;
  if (!EPHI_DOWNSAFE (use_phi))
    return true;
  return false;
}
      
/* DFS Search function: Return true if this PHI stops forward
   movement.  */

static bool
stops_search_seen (tree phi)
{
  return EPHI_STOPS (phi);
}

/* DFS Search function:  Mark the PHI as stopping forward movement.  */

static void
stops_search_set_seen (tree phi)
{
  EPHI_STOPS (phi) = true;
}

/* DFS Search function:  Note that the used phi argument stops forward
   movement.  */

static void
stops_search_reach_from_to (tree def_phi ATTRIBUTE_UNUSED, 
			    int opnd_indx,
			    tree use_phi)
{
  EPHI_ARG_STOPS (use_phi, opnd_indx) = true;
}

/* DFS Search function: Return true if the PHI has any arguments
   stopping forward movement.  */

static bool
stops_search_start_from (tree phi)
{
  int i;
  for (i = 0; i < EPHI_NUM_ARGS (phi); i++)
    if (EPHI_ARG_STOPS (phi, i))
      return true;
  return false;
}

/* DFS Search function:  Return true if the PHI has any arguments
   stopping forward movement.  */

static bool
stops_search_continue_from_to (tree def_phi ATTRIBUTE_UNUSED, 
			       int opnd_indx ATTRIBUTE_UNUSED,
			       tree use_phi)
{
  return stops_search_start_from (use_phi);
}

/* DFS Search function:  Return true if the replacing occurrence is
   known.  */

static bool 
repl_search_seen (tree phi)
{
  return EPHI_REP_OCCUR_KNOWN (phi);
}

/* DFS Search function:  Set the identical_to field and note the
   replacing occurrence is now known.  */

static void 
repl_search_set_seen (tree phi)
{
  int i;
  
#ifdef ENABLE_CHECKING
  if (!ephi_will_be_avail (phi))
    abort ();
#endif
  
  if (EPHI_IDENTICAL_TO (phi) == NULL_TREE)
    {
      for (i = 0; i < EPHI_NUM_ARGS (phi); i++)
	{
	  tree identical_to = occ_identical_to (EPHI_ARG_DEF (phi, i));
	  if (identical_to != NULL_TREE)
	    {
	      if (EPHI_IDENTICAL_TO (phi) == NULL)
		EPHI_IDENTICAL_TO (phi) = identical_to;	      
	      if (EPHI_ARG_INJURED (phi, i))
		EPHI_IDENT_INJURED (phi) = true;
	    }
	}
    }
  EPHI_REP_OCCUR_KNOWN (phi) = true;
}

/* Helper function.  Return true if any argument in the PHI is
   injured.  */

static inline bool
any_operand_injured (tree ephi)
{
  int i;
  for (i = 0; i < EPHI_NUM_ARGS (ephi); i++)
    if (EPHI_ARG_INJURED (ephi, i))
      return true;
  return false;
  
}

/* DFS Search function:  Note the identity of the used phi operand is
   the same as it's defining phi operand, if that phi will be
   available, and it's known.  */

static void
repl_search_reach_from_to (tree def_phi, int opnd_indx ATTRIBUTE_UNUSED,
			   tree use_phi)
{
  if (ephi_will_be_avail (use_phi)
      && EPHI_IDENTITY (use_phi) 
      && EPHI_IDENTICAL_TO (use_phi) == NULL_TREE)
    {
      EPHI_IDENTICAL_TO (use_phi) = EPHI_IDENTICAL_TO (def_phi);
      
      if (EPHI_IDENT_INJURED (def_phi)
	  || any_operand_injured (use_phi))
	EPHI_IDENT_INJURED (use_phi) = true;
    }
}

/* DFS Search function:  Return true if the PHI will be available,
   it's an identity PHI, and it's arguments are identical to
   something.  */

static bool 
repl_search_start_from (tree phi)
{
  if (ephi_will_be_avail (phi) && EPHI_IDENTITY (phi))
    {
      int i;
      for (i = 0; i < EPHI_NUM_ARGS (phi); i++)
	if (occ_identical_to (EPHI_ARG_DEF (phi, i)) != NULL_TREE)
	  return true;    
    }
  return false;
}

/* DFS Search function:  Return true if the using PHI is will be available,
   and identity.  */

static bool
repl_search_continue_from_to (tree def_phi ATTRIBUTE_UNUSED,
			      int opnd_indx ATTRIBUTE_UNUSED,
			      tree use_phi)
{
  return ephi_will_be_avail (use_phi) && EPHI_IDENTITY (use_phi);
}

/* Mark all will-be-avail ephi's in the dominance frontier of BB as
   required.  */

static void
require_phi (struct expr_info *ei, basic_block bb)
{
  size_t i;
  EXECUTE_IF_SET_IN_BITMAP (pre_dfs[bb->index], 0, i,
  {
    tree ephi;
    ephi = ephi_at_block (BASIC_BLOCK (i));
    if (ephi != NULL_TREE 
	&& ephi_will_be_avail (ephi) 
	&& EPHI_IDENTITY (ephi))
      {
	EPHI_IDENTITY (ephi) = false;
	require_phi (ei, BASIC_BLOCK (i));
      }
  });
}

/* Return the occurrence this occurrence is identical to, if one exists.  */

static tree
occ_identical_to (tree t)
{
  if (TREE_CODE (t) == EUSE_NODE && !EUSE_PHIOP (t))
    return t;
  else if (TREE_CODE (t) == EUSE_NODE && EUSE_PHIOP (t))
    return t;
  else if (TREE_CODE (t) == EPHI_NODE)
    { 
      if (EPHI_IDENTITY (t) && EPHI_REP_OCCUR_KNOWN (t))
	return EPHI_IDENTICAL_TO (t);
      else if (!EPHI_IDENTITY (t))
	return t;
    }
  return NULL_TREE;
}

/* Return true if NODE was or is going to be saved.  */
static bool
really_available_def (tree node)
{
  if (TREE_CODE (node) == EUSE_NODE 
      && EUSE_PHIOP (node) 
      && EUSE_INSERTED (node))
    return true;
  if (TREE_CODE (node) == EUSE_NODE
      && EUSE_DEF (node) == NULL_TREE)
    return true;
  return false;
}


/* Second part of the finalize step.  Performs save bit setting, and
   ESSA minimization.  */

static void
finalize_2 (struct expr_info *ei)
{
  size_t i;

  insert_euse_in_preorder_dt_order (ei);
  /* Note which uses need to be saved to a temporary.  */
  for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->euses_dt_order); i++)
    {
      tree ref = VARRAY_TREE (ei->euses_dt_order, i);
      if (TREE_CODE (ref) == EUSE_NODE
	  && !EUSE_PHIOP (ref)
	  && EREF_RELOAD (ref))
	{
	  set_save (ei, EUSE_DEF (ref));
	}
    }

  /* ESSA Minimization.  Determines which phis are identical to other
     phis, and not strictly necessary.  */

  for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->euses_dt_order); i++)
    {
      tree ephi = VARRAY_TREE (ei->euses_dt_order, i);
      if (TREE_CODE (ephi) != EPHI_NODE)
	continue;
      EPHI_IDENTITY (ephi) = true;
      EPHI_IDENTICAL_TO (ephi) = NULL;
    }
  
  for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->euses_dt_order); i++)
    {
      tree ephi = VARRAY_TREE (ei->euses_dt_order, i);
      if (!ephi || TREE_CODE (ephi) != EPHI_NODE)
	continue;      
      if (ephi_will_be_avail (ephi))
	{
	  int k;
	  for (k = 0; k < EPHI_NUM_ARGS (ephi); k++)
	    {
	      if (EPHI_ARG_INJURED (ephi, k))
		require_phi (ei, EPHI_ARG_EDGE (ephi, k)->src);
	      else if (EPHI_ARG_DEF (ephi, k) 
		       && TREE_CODE (EPHI_ARG_DEF (ephi, k)) == EUSE_NODE
		       && really_available_def (EPHI_ARG_DEF (ephi, k)))
		require_phi (ei, bb_for_stmt (EPHI_ARG_DEF (ephi, k)));
	    }
	}
    }
  do_ephi_df_search (ei, replacing_search);
}

/* Perform a DFS on EPHI using the functions in SEARCH. */

static void
do_ephi_df_search_1 (struct ephi_df_search search, tree ephi)
{
  varray_type uses;
  size_t i;
  search.set_seen (ephi);
  
  uses = EPHI_USES (ephi);
  if (!uses)
    return;
  for (i = 0; i < VARRAY_ACTIVE_SIZE (uses); i++)
    {
      struct ephi_use_entry *use = VARRAY_GENERIC_PTR (uses, i);
      if (search.reach_from_to)
	search.reach_from_to (ephi, use->opnd_indx, use->phi);
      if (!search.seen (use->phi) &&
	  search.continue_from_to (ephi, use->opnd_indx, use->phi))
	{
	  do_ephi_df_search_1 (search, use->phi);
	}
    }
}

/* Perform a DFS on the EPHI's, using the functions in SEARCH.  */

static void
do_ephi_df_search (struct expr_info *ei, struct ephi_df_search search) 
{
  size_t i;
  for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->euses_dt_order); i++)
    {
      tree ephi = VARRAY_TREE (ei->euses_dt_order, i);
      if (!ephi || TREE_CODE (ephi) != EPHI_NODE)
	continue;
      if (!search.seen (ephi) 
	  && search.start_from (ephi))
	do_ephi_df_search_1 (search, ephi);
    }
}

#if 0
/* Calculate the increment necessary due to EXPR for the temporary. */
static tree
calculate_increment (struct expr_info *ei, tree expr)
{
  tree incr;

  /*XXX: Currently assume it's like a = a + 5, thus, this will give us the 5.
   */
  incr = TREE_OPERAND (TREE_OPERAND (expr, 1), 1);
  if (TREE_CODE (incr) != INTEGER_CST)
    abort();
  if (TREE_CODE (ei->expr) == MULT_EXPR)
    incr = fold (build (MULT_EXPR, TREE_TYPE (ei->expr),
			incr, TREE_OPERAND (ei->expr, 1)));
#if DEBUGGING_STRRED
  if (dump_file)
    {
      fprintf (dump_file, "Increment calculated to be: ");
      print_generic_expr (dump_file, incr, 0);
      fprintf (dump_file, "\n");
    }
#endif
  return incr;
}
#endif


/* Perform an insertion of EXPR before/after USE, depending on the
   value of BEFORE.  */

static tree
do_proper_save (tree use, tree expr, int before)
{
  basic_block bb = bb_for_stmt (use);
  block_stmt_iterator bsi;

  for (bsi = bsi_start (bb); !bsi_end_p (bsi); bsi_next (&bsi))
    {
      if (bsi_stmt (bsi) == use)
	{
	  if (before)
	    bsi_insert_before (&bsi, expr, BSI_SAME_STMT);
	  else
	    bsi_insert_after (&bsi, expr, BSI_SAME_STMT);
	  return use;
	}
    }
  abort ();
}

/* Get the temporary for ESSA node USE.  
   Takes into account minimized ESSA.  */
static tree 
get_temp (tree use)
{
  tree newtemp;
  if (TREE_CODE (use) == EPHI_NODE && EPHI_IDENTITY (use))
    {
      tree newuse = use;
      while  (TREE_CODE (newuse) == EPHI_NODE 
	      && EPHI_IDENTITY (newuse))	    
	{
#ifdef ENABLE_CHECKING
	  if (!EPHI_IDENTICAL_TO (newuse))
	    abort ();
#endif
	  newuse = EPHI_IDENTICAL_TO (newuse);
	  if (TREE_CODE (newuse) != EPHI_NODE)
	    break;
	}
      if (TREE_CODE (EREF_TEMP (newuse)) == PHI_NODE)
	newtemp = PHI_RESULT (EREF_TEMP (newuse));
      else
	newtemp = EREF_TEMP (newuse);    
    }
  else
    {
      if (TREE_CODE (EREF_TEMP (use)) == PHI_NODE)
	newtemp = PHI_RESULT (EREF_TEMP (use));
      else
	newtemp = EREF_TEMP (use);    
    }
  return newtemp;
}

/* Return the side of the statement that contains an SSA name.  */

static tree
pick_ssa_name (tree stmt)
{
  if (TREE_CODE (TREE_OPERAND (stmt, 0)) == SSA_NAME)
    return TREE_OPERAND (stmt, 0);
  else if (TREE_CODE (TREE_OPERAND (stmt, 1)) == SSA_NAME)
    return TREE_OPERAND (stmt, 1);
  else
    abort ();
}

/* Code motion step of SSAPRE.  Take the save bits, and reload bits,
   and perform the saves and reloads.  Also insert new phis where
   necessary.  */

static void
code_motion (struct expr_info *ei)
{
  tree use;
  tree newtemp;
  size_t euse_iter;
  tree temp = ei->temp;
  basic_block bb;

  /* First, add the phi node temporaries so the reaching defs are
     always right. */
  for (euse_iter = 0;
       euse_iter < VARRAY_ACTIVE_SIZE (ei->euses_dt_order);
       euse_iter++)
    {

      use = VARRAY_TREE (ei->euses_dt_order, euse_iter);
      if (TREE_CODE (use) != EPHI_NODE)
	continue;
      if (ephi_will_be_avail (use) && !EPHI_IDENTITY (use))
	{
	  bb = bb_for_stmt (use);
	  /* Add the new PHI node to the list of PHI nodes for block BB.  */
	  bb_ann (bb)->phi_nodes = chainon (phi_nodes (bb), EREF_TEMP (use));
	}
      else if (EPHI_IDENTITY (use))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "Pointless EPHI in block %d\n",
		       bb_for_stmt (use)->index);
	    }
	}
    }
  /* Now do the actual saves and reloads, plus repairs. */
  for (euse_iter = 0;
       euse_iter < VARRAY_ACTIVE_SIZE (ei->euses_dt_order);
       euse_iter++)
    {
      use = VARRAY_TREE (ei->euses_dt_order, euse_iter);
#ifdef ENABLE_CHECKING
      if (TREE_CODE (use) == EUSE_NODE && EUSE_PHIOP (use)
	  && (EREF_RELOAD (use) || EREF_SAVE (use)))
	abort ();
#endif
      if (EREF_SAVE (use) && !EUSE_INSERTED (use))
	{
	  tree newexpr;
	  tree use_stmt;
	  tree copy;
	  basic_block usebb = bb_for_stmt (use);
	  use_stmt = EREF_STMT (use);

	  copy = TREE_OPERAND (use_stmt, 1);
	  copy = unshare_expr (copy);
	  newexpr = build (MODIFY_EXPR, TREE_TYPE (temp), temp, copy);
	  newtemp = make_ssa_name (temp, newexpr);
	  EREF_TEMP (use) = newtemp;	  
	  TREE_OPERAND (newexpr, 0) = newtemp;
	  TREE_OPERAND (use_stmt, 1) = newtemp;

	  if (dump_file)
	    {
	      fprintf (dump_file, "In BB %d, insert save of ",
		       usebb->index);
	      print_generic_expr (dump_file, copy, dump_flags);
	      fprintf (dump_file, " to ");
	      print_generic_expr (dump_file, newtemp, dump_flags);
	      fprintf (dump_file, " before statement ");
	      print_generic_expr (dump_file, use_stmt, dump_flags);
	      fprintf (dump_file, "\n");
	      if (EXPR_HAS_LOCATION (use_stmt))
		fprintf (dump_file, " on line %d\n",
			 EXPR_LINENO (use_stmt));
	    }
	  modify_stmt (newexpr);
	  modify_stmt (use_stmt);
	  set_bb_for_stmt (newexpr, usebb);
	  EREF_STMT (use) = do_proper_save (use_stmt, newexpr, true);
	  pre_stats.saves++;
	}
      else if (EREF_RELOAD (use))
	{
	  tree use_stmt;
	  tree newtemp;

	  use_stmt = EREF_STMT (use);
	  bb = bb_for_stmt (use_stmt);
	  
	  newtemp = get_temp (EUSE_DEF (use));
	  if (!newtemp)
	    abort ();
	  if (dump_file)
	    {
	      fprintf (dump_file, "In BB %d, insert reload of ",
		       bb->index);
	      print_generic_expr (dump_file,
				  TREE_OPERAND (use_stmt, 1), 0);
	      fprintf (dump_file, " from ");
	      print_generic_expr (dump_file, newtemp, dump_flags);
	      fprintf (dump_file, " in statement ");
	      print_generic_stmt (dump_file, use_stmt, dump_flags);
	      fprintf (dump_file, "\n");
	      if (EXPR_HAS_LOCATION (use_stmt))
		fprintf (dump_file, " on line %d\n",
			 EXPR_LINENO (use_stmt));
	    }
	  TREE_OPERAND (use_stmt, 1) = newtemp;
	  EREF_TEMP (use) = newtemp;
	  modify_stmt (use_stmt);
	  pre_stats.reloads++;
	}
    }
  
  /* Now do the phi nodes. */
  for (euse_iter = 0;
       euse_iter < VARRAY_ACTIVE_SIZE (ei->euses_dt_order);
       euse_iter++)
    {
      use = VARRAY_TREE (ei->euses_dt_order, euse_iter);  
      if (TREE_CODE (use) == EPHI_NODE 
	  && ephi_will_be_avail (use) 
	  && !EPHI_IDENTITY (use))
	{
	  int i;
	  tree argdef;
	  bb = bb_for_stmt (use);
	  if (dump_file)
	    {
	      fprintf (dump_file,
		       "In BB %d, insert PHI to replace EPHI\n", bb->index);
	    }
	  newtemp = EREF_TEMP (use);
	  for (i = 0; i < EPHI_NUM_ARGS (use); i++)
	    {
	      tree rdef;
	      argdef = EPHI_ARG_DEF (use, i);
	      if (argdef == use)
		rdef = get_temp (use);
	      else if (EREF_RELOAD (argdef) || EREF_SAVE (argdef))
		rdef = get_temp (argdef);
	      else if (TREE_CODE (argdef) == EPHI_NODE)
		rdef = get_temp (argdef);
	      else if (argdef 
		  && EPHI_ARG_HAS_REAL_USE (use, i) 
		  && EREF_STMT (argdef)
		  && !EPHI_ARG_INJURED (use, i))
		rdef = pick_ssa_name (EREF_STMT (argdef));
	      else
		abort ();
	      	      
	      if (!rdef)
	        abort();
	      add_phi_arg (&newtemp, rdef, EPHI_ARG_EDGE (use, i));
	    }

	  /* Associate BB to the PHI node.  */
	  set_bb_for_stmt (EREF_TEMP (use), bb);
	  pre_stats.newphis++;

	}
    }
}

/* Compute the iterated dominance frontier of a statement.  */

static bitmap
compute_idfs (bitmap * dfs, tree stmt)
{
  fibheap_t worklist;
  sbitmap inworklist, done;
  bitmap idf;
  size_t i;
  basic_block block;
  
  block = bb_for_stmt (stmt);
  if (idfs_cache[block->index] != NULL)
    return idfs_cache[block->index];

  inworklist = sbitmap_alloc (last_basic_block);
  done = sbitmap_alloc (last_basic_block);
  worklist = fibheap_new ();
  sbitmap_zero (inworklist);
  sbitmap_zero (done);

  idf = BITMAP_XMALLOC ();
  bitmap_zero (idf);

  block = bb_for_stmt (stmt);
  fibheap_insert (worklist, block->index, (void *)(size_t)block->index);
  SET_BIT (inworklist, block->index);

  while (!fibheap_empty (worklist))
    {
      int a = (size_t) fibheap_extract_min (worklist);
      if (TEST_BIT (done, a))
	continue;
      SET_BIT (done, a);
      if (idfs_cache[a])
	{
	  bitmap_a_or_b (idf, idf, idfs_cache[a]);
	  EXECUTE_IF_SET_IN_BITMAP (idfs_cache[a], 0, i,
          {
	    SET_BIT (inworklist, i);
	    SET_BIT (done, i);
	  });
	}
      else
	{
	  bitmap_a_or_b (idf, idf, dfs[a]);
	  EXECUTE_IF_SET_IN_BITMAP (dfs[a], 0, i,
          {
	    if (!TEST_BIT (inworklist, i))
	      {
		SET_BIT (inworklist, i);
		fibheap_insert (worklist, i, (void *)i);
	      }
	  });
	}
      
    }
  fibheap_delete (worklist);
  sbitmap_free (inworklist);
  sbitmap_free (done);
  idfs_cache[block->index] = idf;
  return idf;

}

/* Return true if EXPR is a strength reduction candidate. */
static bool
is_strred_cand (const tree expr ATTRIBUTE_UNUSED)
{
#if 0
	if (TREE_CODE (TREE_OPERAND (expr, 1)) != MULT_EXPR
      && TREE_CODE (TREE_OPERAND (expr, 1)) != MINUS_EXPR
      && TREE_CODE (TREE_OPERAND (expr, 1)) != NEGATE_EXPR
      && TREE_CODE (TREE_OPERAND (expr, 1)) != PLUS_EXPR)
    return false;
  return true;
#endif
  return false;
}



/* Determine if two expressions are lexically equivalent. */

static inline bool
expr_lexically_eq (const tree v1, const tree v2)
{
  if (TREE_CODE_CLASS (TREE_CODE (v1)) != TREE_CODE_CLASS (TREE_CODE (v2)))
    return false;
  if (TREE_CODE (v1) != TREE_CODE (v2))
    return false;
  switch (TREE_CODE_CLASS (TREE_CODE (v1)))
    {
    case 'r':
    case '1':
      return names_match_p (TREE_OPERAND (v1, 0), TREE_OPERAND (v2, 0));
    case 'x':
    case 'd':
      return names_match_p (v1, v2);
    case '2':
      {
	bool match;
	match = names_match_p (TREE_OPERAND (v1, 0), TREE_OPERAND (v2, 0));
	if (!match)
	  return false;
	match = names_match_p (TREE_OPERAND (v1, 1), TREE_OPERAND (v2, 1));
	if (!match)
	  return false;
	return true;
      }
    default:
      return false;
    }

}

/* Free an expression info structure.  */

static void
free_expr_info (struct expr_info *v1)
{
  struct expr_info *e1 = (struct expr_info *)v1;
  VARRAY_FREE (e1->occurs);
  VARRAY_FREE (e1->kills);
  VARRAY_FREE (e1->lefts);
  VARRAY_FREE (e1->reals);
  VARRAY_FREE (e1->euses_dt_order);
}

/* Process left occurrences and kills due to EXPR.
   A left occurrence occurs wherever a variable in an expression we
   are PRE'ing is stored to directly in a def, or indirectly because
   of a VDEF of an expression that we VUSE.  */

static void
process_left_occs_and_kills (varray_type bexprs, tree expr)
{
  size_t i, j, k;
  
  stmt_ann_t ann = stmt_ann (expr);
  vdef_optype vdefs;
  vuse_optype vuses;
  def_optype defs;
  defs = DEF_OPS (ann);
  vdefs = VDEF_OPS (ann);
  if (NUM_DEFS (defs) == 0 && NUM_VDEFS (vdefs) == 0)
    return;

  for (j = 0; j < VARRAY_ACTIVE_SIZE (bexprs); j++)
    {
      struct expr_info *ei = VARRAY_GENERIC_PTR (bexprs, j);
      tree vuse_name;
      tree random_occur;
      stmt_ann_t ann;
      
      if (!ei->loadpre_cand)
	continue;
      
      /* If we define the variable itself (IE a in *a, or a in a),
	 it's a left occurrence.  */
      for (i = 0; i < NUM_DEFS (defs); i++)
	{
	  if (names_match_p (DEF_OP (defs, i), ei->expr))    
	    {
	      if (TREE_CODE (expr) == ASM_EXPR)
		{
		  ei->loadpre_cand = false;
		  continue;
		}
	      VARRAY_PUSH_TREE (ei->lefts, expr);
	      VARRAY_PUSH_TREE (ei->occurs, NULL);
	      VARRAY_PUSH_TREE (ei->kills, NULL);
	    }
	}
      
      /* If we VDEF the VUSE of the expression, it's also a left
	 occurrence.  */
      random_occur = VARRAY_TREE (ei->occurs, 0);
      ann = stmt_ann (random_occur);
      vuses = VUSE_OPS (ann);
      if (NUM_VDEFS (vdefs) != 0)
	{
	  for (k = 0; k < NUM_VUSES (vuses); k++)
	    {
	      vuse_name = VUSE_OP (vuses, k);
	      for (i = 0; i < NUM_VDEFS (vdefs); i++)
		{
		  if (names_match_p (VDEF_OP (vdefs, i), vuse_name))
		    {
		      VARRAY_PUSH_TREE (ei->lefts, expr);
		      VARRAY_PUSH_TREE (ei->occurs, NULL);
		      VARRAY_PUSH_TREE (ei->kills, NULL);
		    }
		}
	    }
	}
    }
}

/* Perform SSAPRE on an expression.  */

static int
pre_expression (struct expr_info *slot, void *data, bitmap vars_to_rename)
{
  struct expr_info *ei = (struct expr_info *) slot;
  basic_block bb;

  class_count = 0;
  eref_id_counter = 0;
  
  /* If we don't have two occurrences along any dominated path, and
     it's not load PRE, this is a waste of time.  */

  if (VARRAY_ACTIVE_SIZE (ei->reals) < 2)
    return 1;
  
  memset (&pre_stats, 0, sizeof (struct pre_stats_d));
  
  ei->temp = create_tmp_var (TREE_TYPE (ei->expr), "pretmp");
  add_referenced_tmp_var (ei->temp);

  bitmap_clear (created_phi_preds);
  ephi_pindex_htab = htab_create (500, ephi_pindex_hash, ephi_pindex_eq, free);
  phi_pred_cache = xcalloc (last_basic_block, sizeof (tree));
  n_phi_preds = last_basic_block;

  if (!expr_phi_insertion ((bitmap *)data, ei))
    goto cleanup;  
  rename_1 (ei);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      size_t i;
      fprintf (dump_file, "Occurrences for expression ");
      print_generic_expr (dump_file, ei->expr, dump_flags);
      fprintf (dump_file, " after Rename 2\n");
      for (i = 0; i < VARRAY_ACTIVE_SIZE (ei->euses_dt_order); i++)
	{
	  print_generic_expr (dump_file, 
			      VARRAY_TREE (ei->euses_dt_order, i), 1);
	  fprintf (dump_file, "\n");
	}
    }
  compute_down_safety (ei);
  compute_du_info (ei);
  compute_will_be_avail (ei);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "EPHI's for expression ");
      print_generic_expr (dump_file, ei->expr, dump_flags);
      fprintf (dump_file,
	       " after down safety and will_be_avail computation\n");
      FOR_EACH_BB (bb)
      {
	tree ephi = ephi_at_block (bb);
	if (ephi != NULL)
	  {
	    print_generic_expr (dump_file, ephi, 1);
	    fprintf (dump_file, "\n");
	  }
      }
    }

  if (finalize_1 (ei))
    {
      finalize_2 (ei);
      code_motion (ei);
      if (ei->loadpre_cand)
	bitmap_set_bit (vars_to_rename, var_ann (ei->temp)->uid);
    }

  clear_all_eref_arrays ();
  if (dump_file)
    if (dump_flags & TDF_STATS)
      {
	fprintf (dump_file, "PRE stats:\n");
	fprintf (dump_file, "Reloads:%d\n", pre_stats.reloads);
	fprintf (dump_file, "Saves:%d\n", pre_stats.saves);
	fprintf (dump_file, "Repairs:%d\n", pre_stats.repairs);
	fprintf (dump_file, "New phis:%d\n", pre_stats.newphis);
	fprintf (dump_file, "EPHI memory allocated:%d\n", 
		 pre_stats.ephi_allocated);
	fprintf (dump_file, "EREF memory allocated:%d\n",
		 pre_stats.eref_allocated);
	fprintf (dump_file, "Expressions generated for rename2:%d\n",
		 pre_stats.exprs_generated);
      }
 cleanup:
  free (phi_pred_cache);
  if (ephi_pindex_htab)
    {
      htab_delete (ephi_pindex_htab);
      ephi_pindex_htab = NULL;
    }


  return 0;
}


/* Step 1 - Collect the expressions to perform PRE on.  */

static void 
collect_expressions (basic_block block, varray_type *bexprsp)
{
  size_t k;
  block_stmt_iterator j;
  basic_block son;

  varray_type bexprs = *bexprsp;
  
  for (j = bsi_start (block); !bsi_end_p (j); bsi_next (&j))
    {
      tree stmt = bsi_stmt (j);
      tree expr = stmt;
      tree orig_expr = expr;
      stmt_ann_t ann;
      struct expr_info *slot = NULL;
      
      get_stmt_operands (expr);
      ann = stmt_ann (expr);
      
      if (NUM_USES (USE_OPS (ann)) == 0)
	{
	  process_left_occs_and_kills (bexprs, expr);
	  continue;
	}
      
      if (TREE_CODE (expr) == MODIFY_EXPR)
	expr = TREE_OPERAND (expr, 1);
      if ((TREE_CODE_CLASS (TREE_CODE (expr)) == '2'
	   || TREE_CODE_CLASS (TREE_CODE (expr)) == '<'
	   /*|| TREE_CODE_CLASS (TREE_CODE (expr)) == '1'*/
	   || TREE_CODE (expr) == SSA_NAME
	   || TREE_CODE (expr) == INDIRECT_REF)
	  && !ann->makes_aliased_stores
	  && !ann->has_volatile_ops)
	{
	  bool is_scalar = true;
	  tree origop0 = TREE_OPERAND (orig_expr, 0);
	  
	  if (AGGREGATE_TYPE_P (TREE_TYPE (origop0))
	      || TREE_CODE (TREE_TYPE (origop0)) == COMPLEX_TYPE)
	    is_scalar = false;
	  
	  if (is_scalar 
	      && (TREE_CODE (expr) == SSA_NAME 
		  || (TREE_CODE (expr) == INDIRECT_REF
		      && !DECL_P (TREE_OPERAND (expr, 0)))
		  ||(!DECL_P (TREE_OPERAND (expr, 0))
		     && (!TREE_OPERAND (expr, 1)
			 || !DECL_P (TREE_OPERAND (expr, 1))))))
	    {
	      for (k = 0; k < VARRAY_ACTIVE_SIZE (bexprs); k++)
		{
		  slot = VARRAY_GENERIC_PTR (bexprs, k);
		  if (expr_lexically_eq (slot->expr, expr))
		    break;
		}
	      if (k >= VARRAY_ACTIVE_SIZE (bexprs))
		slot = NULL;
	      if (slot)
		{
		  VARRAY_PUSH_TREE (slot->occurs, orig_expr);
		  VARRAY_PUSH_TREE (slot->kills, NULL);
		  VARRAY_PUSH_TREE (slot->lefts, NULL);
		  VARRAY_PUSH_TREE (slot->reals, stmt);
		  slot->strred_cand &= is_strred_cand (orig_expr);
		}
	      else
		{
		  slot = ggc_alloc (sizeof (struct expr_info));
		  slot->expr = expr;
		  VARRAY_GENERIC_PTR_NOGC_INIT (slot->occurs, 1, "Occurrence");
		  VARRAY_GENERIC_PTR_NOGC_INIT (slot->kills, 1, "Kills");
		  VARRAY_GENERIC_PTR_NOGC_INIT (slot->lefts, 1, "Left occurrences");
		  VARRAY_GENERIC_PTR_NOGC_INIT (slot->reals, 1, "Real occurrences");
		  VARRAY_GENERIC_PTR_NOGC_INIT (slot->euses_dt_order, 1, "EUSEs");
		  
		  VARRAY_PUSH_TREE (slot->occurs, orig_expr);
		  VARRAY_PUSH_TREE (slot->kills, NULL);
		  VARRAY_PUSH_TREE (slot->lefts, NULL);
		  VARRAY_PUSH_TREE (slot->reals, stmt);
		  VARRAY_PUSH_GENERIC_PTR (bexprs, slot);
		  slot->strred_cand = is_strred_cand (orig_expr);
		  slot->loadpre_cand = false;
		  if (TREE_CODE (expr) == SSA_NAME
		      || TREE_CODE (expr) == INDIRECT_REF)
		    slot->loadpre_cand = true;
		}
	    }
	}
      process_left_occs_and_kills (bexprs, orig_expr);
    }
  *bexprsp = bexprs;

  for (son = first_dom_son (CDI_DOMINATORS, block);
       son;
       son = next_dom_son (CDI_DOMINATORS, son))
    collect_expressions (son, bexprsp);
}

/* Main entry point to the SSA-PRE pass.

   PHASE indicates which dump file from the DUMP_FILES array to use when
   dumping debugging information.  */

static void
execute_pre (void)
{
  int currbbs;
  varray_type bexprs;
  size_t k;
  int i;
 
  if (ENTRY_BLOCK_PTR->succ->dest->pred->pred_next)
    if (!(ENTRY_BLOCK_PTR->succ->flags & EDGE_ABNORMAL))
      split_edge (ENTRY_BLOCK_PTR->succ);
 
  euse_node_pool = create_alloc_pool ("EUSE node pool", 
				      sizeof (struct tree_euse_node), 30);
  eref_node_pool = create_alloc_pool ("EREF node pool",
				      sizeof (struct tree_eref_common), 30);
  ephi_use_pool = create_alloc_pool ("EPHI use node pool",
              sizeof (struct ephi_use_entry), 30);
  VARRAY_GENERIC_PTR_INIT (bexprs, 1, "bexprs");
  /* Compute immediate dominators.  */
  calculate_dominance_info (CDI_DOMINATORS);

  /* DCE screws the dom_children up, without bothering to fix it. So fix it. */
  currbbs = n_basic_blocks;
  dfn = xcalloc (last_basic_block + 1, sizeof (int));
  build_dfn_array (ENTRY_BLOCK_PTR, 0);

  /* Initialize IDFS cache.  */
  idfs_cache = xcalloc (currbbs, sizeof (bitmap));

  /* Compute dominance frontiers.  */
  pre_dfs = (bitmap *) xmalloc (sizeof (bitmap) * currbbs);
  for (i = 0; i < currbbs; i++)
     pre_dfs[i] = BITMAP_XMALLOC ();
  compute_dominance_frontiers (pre_dfs);

  created_phi_preds = BITMAP_XMALLOC ();
  
  collect_expressions (ENTRY_BLOCK_PTR, &bexprs);

  ggc_push_context ();
 
  for (k = 0; k < VARRAY_ACTIVE_SIZE (bexprs); k++)
    {
      pre_expression (VARRAY_GENERIC_PTR (bexprs, k), pre_dfs, vars_to_rename);
      free_alloc_pool (euse_node_pool);
      free_alloc_pool (eref_node_pool);
      free_alloc_pool (ephi_use_pool);
      euse_node_pool = create_alloc_pool ("EUSE node pool", 
					  sizeof (struct tree_euse_node), 30);
      eref_node_pool = create_alloc_pool ("EREF node pool",
					  sizeof (struct tree_eref_common), 30);
      ephi_use_pool = create_alloc_pool ("EPHI use node pool",
            sizeof (struct ephi_use_entry), 30);
      free_expr_info (VARRAY_GENERIC_PTR (bexprs, k));
#ifdef ENABLE_CHECKING
      if (pre_stats.ephis_current != 0)
	abort ();
#endif
      ggc_collect (); 
    }

  ggc_pop_context ();

  /* Clean up after PRE.  */
  memset (&pre_stats, 0, sizeof (struct pre_stats_d));
  free_alloc_pool (euse_node_pool);
  free_alloc_pool (eref_node_pool);
  free_alloc_pool (ephi_use_pool);
  VARRAY_CLEAR (bexprs);
  for (i = 0; i < currbbs; i++)
    BITMAP_XFREE (pre_dfs[i]);
  free (pre_dfs);
  BITMAP_XFREE (created_phi_preds);
  for (i = 0; i < currbbs; i++)
    if (idfs_cache[i] != NULL)
      BITMAP_XFREE (idfs_cache[i]);
  
  free (dfn);
  free (idfs_cache);
}

static bool
gate_pre (void)
{
  return flag_tree_pre != 0;
}

struct tree_opt_pass pass_pre = 
{
  "pre",				/* name */
  gate_pre,				/* gate */
  execute_pre,				/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_PRE,				/* tv_id */
  PROP_no_crit_edges | PROP_cfg | PROP_ssa,/* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_rename_vars
    | TODO_ggc_collect | TODO_verify_ssa /* todo_flags_finish */
};
