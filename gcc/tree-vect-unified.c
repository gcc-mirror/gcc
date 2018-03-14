/* lOOP Vectorization using unified representation for permute instructions.
   Copyright (C) 2003-2015 Free Software Foundation, Inc.
   Contributed by Sameera Deshpande <sameera.deshpande@imgtec.com>

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

/* Loop autovectorization using unified representation for permute
   instructions.  */
#if 1
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-ssa-loop-manip.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "tree-vectorizer.h"
#include "tree-ssa-propagate.h"
#include "dbgcnt.h"
#include "tree-scalar-evolution.h"
#include "tree-vect-unified.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "target.h"
#include "rtl.h"
#include "tm_p.h"
#include "optabs-tree.h"
#include "dumpfile.h"
#include "alias.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimplify-me.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop.h"
#include "expr.h"
#include "builtins.h"
#include "params.h"
#include "pretty-print.h"

/* Entry point to the autovectorizer using tree tiling algorithm for permute
   instruction selection using unified representation.  */

namespace {

const pass_data pass_data_unified_vectorize =
{
  GIMPLE_PASS, /* type */
  "unified-vect", /* name */
  OPTGROUP_LOOP | OPTGROUP_VEC, /* optinfo_flags */
  TV_TREE_VECTORIZATION, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_unified_vectorize : public gimple_opt_pass
{
public:
  pass_unified_vectorize (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_unified_vectorize, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *fun)
    {
      return (flag_tree_loop_vectorize && flag_tree_loop_vectorize_unified)
	      || fun->has_force_vectorize_loops;
    }

  virtual unsigned int execute (function *);

}; // class pass_unified_vectorize

unsigned int
pass_unified_vectorize::execute (function *fun)
{
  if (number_of_loops (fun) <= 1)
    return 0;

  return vectorize_loops_using_uniop ();
}

} // anon namespace

gimple_opt_pass *
make_pass_unified_vectorize (gcc::context *ctxt)
{
  return new pass_unified_vectorize (ctxt);
}

/* Function new_iter_node.

   Create new ITER_node for the loop LOOP, and return the pointer.  */

static struct ITER_node *
new_iter_node (struct loop *loop)
{
  struct ITER_node *res;
  basic_block *bbs;
  gimple_stmt_iterator si;
  int i;

  res = (struct ITER_node *) xcalloc (1, sizeof (struct ITER_node));
  ITER_NODE_LOOP (res) = loop;
  ITER_NODE_NITERS (res) = NULL;
  ITER_NODE_PROLOGUE (res) = vNULL;
  ITER_NODE_LOOP_PEEL_NEEDED (res) = 0;
  ITER_NODE_LOOP_BODY (res) = vNULL;
  ITER_NODE_PEELING_FOR_GAPS (res) = false;
  ITER_NODE_PEELING_FOR_NITER (res) = false;
  ITER_NODE_EPILOGUE (res) = vNULL;
  ITER_NODE_PROBABLE_ROOT_NODES (res) = vNULL;

  bbs = get_loop_body (loop);

  for (i = 0; i < loop->num_nodes; i++)
  {
    basic_block bb = bbs[i];

    for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
      {
	gimple *phi = gsi_stmt (si);
	gimple_set_uid (phi, 0);
      }

    for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
      {
	gimple *stmt = gsi_stmt (si);
	gimple_set_uid (stmt, 0);
      }
  }

  return res;
}

/* Function destroy_iter_node_info.

   Clear the statements within the loop corresponding the ITER_node INODE, and
   destroy ITER_node.  */

static void
destroy_iter_node_info (struct ITER_node *inode)
{
  int i;
  basic_block *bbs;
  gimple_stmt_iterator si;

  if (ITER_NODE_PROLOGUE (inode) != vNULL)
    ITER_NODE_PROLOGUE (inode).release ();
  if (ITER_NODE_LOOP_BODY (inode) != vNULL)
    ITER_NODE_LOOP_BODY (inode).release ();
  if (ITER_NODE_EPILOGUE (inode) != vNULL)
    ITER_NODE_EPILOGUE (inode).release ();
  if (ITER_NODE_PROBABLE_ROOT_NODES (inode) != vNULL)
    ITER_NODE_PROBABLE_ROOT_NODES (inode).release ();

  bbs = get_loop_body (ITER_NODE_LOOP (inode));
  for (i = 0; i < ITER_NODE_LOOP (inode)->num_nodes; i++)
  {
    basic_block bb = bbs[i];

    for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
      {
	gimple *phi = gsi_stmt (si);
	gimple_set_uid (phi, 0);
      }

    for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
      {
	gimple *stmt = gsi_stmt (si);
	gimple_set_uid (stmt, 0);
      }
  }

  free (inode);
}

vec<struct stmt_attr *> stmt_attr_vec;

void
init_stmt_attr_vec (void)
{
  gcc_assert (!stmt_attr_vec.exists ());
  stmt_attr_vec.create (50);
}

void
free_stmt_attr_vec (void)
{
  gcc_assert (stmt_attr_vec.exists ());
  stmt_attr_vec.release ();
}

inline void
set_stmt_attr (gimple *stmt, struct stmt_attr *info)
{
  unsigned int uid = gimple_uid (stmt);
  if (uid == 0)
    {
      gcc_checking_assert (info);
      uid = stmt_attr_vec.length () + 1;
      gimple_set_uid (stmt, uid);
      stmt_attr_vec.safe_push (info);
    }
  else
    {
      gcc_checking_assert (info == NULL);
      stmt_attr_vec[uid - 1] = info;
    }
}

inline struct stmt_attr *
get_stmt_attr (gimple *stmt)
{
  unsigned int uid = gimple_uid (stmt);
  if (uid == 0)
    return NULL;

  return stmt_attr_vec[uid - 1];
}



/* Function new_stmt_attr.

   Create statement attribute information, and return the pointer for the
   same.  */

static struct stmt_attr *
new_stmt_attr ()
{
  struct stmt_attr *info;
  info = (struct stmt_attr *) xcalloc (1, sizeof (struct stmt_attr));
  info->use_type = stmt_use_type_undef;
  info->access_fn = NULL;
  info->ptree = NULL;
  info->dr = NULL;
  info->probable_root = false;
  info->vectype = NULL;
  return info;
}


/* Function vect_populate_iter_node_from_loop.

   Create new ITER_node corresponding to loop LOOP, and fill all fields in
   ITER_node necessary for auto-vectorization.  */

static struct ITER_node *
vect_populate_iter_node_from_loop (struct loop *loop)
{
  tree number_of_iterations, number_of_iterationsm1, assumptions;
  basic_block *bbs;
  gcond *loop_cond, *inner_loop_cond = NULL;
  int i;
  gimple_stmt_iterator si;

  if (! vect_analyze_loop_form_1 (loop, &loop_cond, &assumptions,
	      &number_of_iterationsm1, &number_of_iterations, &inner_loop_cond))
    return NULL;

  struct ITER_node * t_iter_node = new_iter_node (loop);
  ITER_NODE_NITERS (t_iter_node) = number_of_iterations;

  bbs = get_loop_body (loop);
  for (i = 0; i < loop->num_nodes; i++)
  {
    basic_block bb = bbs[i];

    for (si = gsi_start_phis (bb); !gsi_end_p (si); gsi_next (&si))
      {
	gimple *phi = gsi_stmt (si);
	set_stmt_attr (phi, new_stmt_attr ());
      }

    for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
      {
	gimple *stmt = gsi_stmt (si);
	set_stmt_attr (stmt, new_stmt_attr ());
      }
  }

  if (!ITER_NODE_NITERS_KNOWN_P (t_iter_node))
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Symbolic number of iterations is ");
	  dump_generic_expr (MSG_NOTE, TDF_DETAILS, number_of_iterations);
	  dump_printf (MSG_NOTE, "\n");
	}
    }

  STMT_ATTR_USE_TYPE (loop_cond) = stmt_use_type_loop_exit_ctrl;
  if (inner_loop_cond)
     STMT_ATTR_USE_TYPE (inner_loop_cond) = stmt_use_type_loop_exit_ctrl;

  gcc_assert (!loop->aux);
  loop->aux = t_iter_node;
  return t_iter_node;
}

/* Function vect_analyze_dataref_access.

   Analyze access pattern of data reference DR within the LOOP.  Except variable
   stride, any access pattern is supported.  */

static bool
vect_analyze_dataref_access (struct data_reference *dr, struct loop * loop)
{
  tree step = DR_STEP (dr);
  gimple *stmt = DR_STMT (dr);

  if (!step)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "bad data-ref access in loop\n");
      return false;
    }

  if (integer_zerop (step))
    {
      /* Stride is 0, i.e.  the data_ref is loop invariant.  So, writes cannot be
	 vectorized.  */
      if (nested_in_vect_loop_p (loop, stmt))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "zero step in inner loop of nest\n");

	  if (!loop->force_vectorize)
	    return false;
	}

	return DR_IS_READ (dr);
    }

  if (loop && nested_in_vect_loop_p (loop, stmt))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "grouped access in outer loop.\n");
      return false;
    }

  /* For now, do not vectorize for variable step size.  */
  if (TREE_CODE (step) != INTEGER_CST)
    return false;

  return true;
}

/* Function vect_analyze_dataref_accesses.

   Analyze all the data refs within the LOOP for the access pattern.  */

static bool
vect_analyze_dataref_accesses (struct ITER_node *inode)
{
  unsigned int i;
  vec<data_reference_p> datarefs = ITER_NODE_DATA_REFS (inode);
  struct data_reference *dr;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "=== vect_analyze_dataref_accesses ===\n");

  if (datarefs.is_empty ())
    return true;

  FOR_EACH_VEC_ELT (datarefs, i, dr)
    if (!vect_analyze_dataref_access (dr, ITER_NODE_LOOP (inode)))
      {
	if (dump_enabled_p ())
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "not vectorized: complicated access pattern.\n");

	return false;
      }

  return true;
}

/* Function vect_analyze_datarefs.

   Analyze all data references DR within the LOOP to check if vectorization is
   possible.  */

static bool
vect_analyze_datarefs (struct ITER_node *inode)
{
  struct loop *loop = ITER_NODE_LOOP (inode);
  vec<data_reference_p> datarefs = ITER_NODE_DATA_REFS (inode);
  tree scalar_type;
  tree vec_type;
  struct data_reference *dr;
  unsigned int i;

  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      gimple *stmt;
      //tree base, offset, init;
      bool simd_lane_access = false;
      int vf;


again:
      if (!dr || !DR_REF (dr))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: unhandled data-ref\n");
	  return false;
	}

      stmt = DR_STMT (dr);

      /* Discard clobbers from the dataref vector.  We will remove
	 clobber stmts during vectorization.  */
      if (gimple_clobber_p (stmt))
	{
	  free_data_ref (dr);
	  if (i == datarefs.length () - 1)
	    {
	      datarefs.pop ();
	      break;
	    }
	  datarefs.ordered_remove (i);
	  dr = datarefs[i];
	  goto again;
	}

      /* Check that analysis of the data-ref succeeded.  */
      if (!DR_BASE_ADDRESS (dr) || !DR_OFFSET (dr) || !DR_INIT (dr)
	  || !DR_STEP (dr))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "not vectorized: data ref analysis "
			       "failed ");
	      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	    }

	  return false;
	}

      if (TREE_CODE (DR_BASE_ADDRESS (dr)) == INTEGER_CST)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: base addr of dr is a "
			     "constant\n");

	  return false;
	}

      if (TREE_THIS_VOLATILE (DR_REF (dr)))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "not vectorized: volatile type ");
	      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	    }

	  return false;
	}

      if (stmt_can_throw_internal (stmt))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "not vectorized: statement can throw an "
			       "exception ");
	      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	    }

	  return false;
	}

      if (TREE_CODE (DR_REF (dr)) == COMPONENT_REF
	  && DECL_BIT_FIELD (TREE_OPERAND (DR_REF (dr), 1)))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "not vectorized: statement is bitfield "
			       "access ");
	      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	    }

	  return false;
	}

      if (is_gimple_call (stmt)
	  && (!gimple_call_internal_p (stmt)
	      || (gimple_call_internal_fn (stmt) != IFN_MASK_LOAD
		  && gimple_call_internal_fn (stmt) != IFN_MASK_STORE)))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION,  vect_location,
			       "not vectorized: dr in a call ");
	      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	    }

	  return false;
	}

      /* If the dataref is in an inner-loop of the loop that is considered for
	 for vectorization, we also want to analyze the access relative to
	 the outer-loop (DR contains information only relative to the
	 inner-most enclosing loop).  We do that by building a reference to the
	 first location accessed by the inner-loop, and analyze it relative to
	 the outer-loop.  */
      if (loop && nested_in_vect_loop_p (loop, stmt))
	{
 	  /* Do nothing for now, as the purpose is unclear.  */
#if 0
	  /* Build a reference to the first location accessed by the
	     inner-loop: *(BASE+INIT).  (The first location is actually
	     BASE+INIT+OFFSET, but we add OFFSET separately later).  */
	  tree inner_base = build_fold_indirect_ref
				(fold_build_pointer_plus (base, init));

	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "analyze in outer-loop: ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, inner_base);
	      dump_printf (MSG_NOTE, "\n");
	    }

	  outer_base = get_inner_reference (inner_base, &pbitsize, &pbitpos,
					    &poffset, &pmode, &punsignedp,
					    &preversep, &pvolatilep, false);
	  gcc_assert (outer_base != NULL_TREE);

	  if (pbitpos % BITS_PER_UNIT != 0)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "failed: bit offset alignment.\n");
	      return false;
	    }

	  if (preversep)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "failed: reverse storage order.\n");
	      return false;
	    }

	  outer_base = build_fold_addr_expr (outer_base);
	  if (!simple_iv (loop, loop_containing_stmt (stmt), outer_base,
			  &base_iv, false))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "failed: evolution of base is not affine.\n");
	      return false;
	    }

	  if (offset)
	    {
	      if (poffset)
		poffset = fold_build2 (PLUS_EXPR, TREE_TYPE (offset), offset,
				       poffset);
	      else
		poffset = offset;
	    }

	  if (!poffset)
	    {
	      offset_iv.base = ssize_int (0);
	      offset_iv.step = ssize_int (0);
	    }
	  else if (!simple_iv (loop, loop_containing_stmt (stmt), poffset,
			       &offset_iv, false))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "evolution of offset is not affine.\n");
	      return false;
	    }

	  outer_init = ssize_int (pbitpos / BITS_PER_UNIT);
	  split_constant_offset (base_iv.base, &base_iv.base, &dinit);
	  outer_init =  size_binop (PLUS_EXPR, outer_init, dinit);
	  split_constant_offset (offset_iv.base, &offset_iv.base, &dinit);
	  outer_init =  size_binop (PLUS_EXPR, outer_init, dinit);

	  outer_step = size_binop (PLUS_EXPR,
				fold_convert (ssizetype, base_iv.step),
				fold_convert (ssizetype, offset_iv.step));
#endif
	}

      STMT_ATTR_DR (stmt) = dr;

      if (simd_lane_access)
	{
	  free_data_ref (datarefs[i]);
	  datarefs[i] = dr;
	}

      /* Set vectype for STMT.  */
      scalar_type = TREE_TYPE (DR_REF (dr));
      vec_type = get_vectype_for_scalar_type (scalar_type);
      if (!vec_type)
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "not vectorized: no vectype for stmt: ");
	      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	      dump_printf (MSG_MISSED_OPTIMIZATION, " scalar_type: ");
	      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_DETAILS,
				 scalar_type);
	      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	    }

	  return false;
	}
      else
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "got vectype for stmt: ");
	      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
	      dump_generic_expr (MSG_NOTE, TDF_SLIM,
				 vec_type);
	      dump_printf (MSG_NOTE, "\n");
	    }
	}

      /* Adjust the minimal vectorization factor according to the
	 vector type.  */
      vf = TYPE_VECTOR_SUBPARTS (vec_type);

      if (TREE_CODE (DR_STEP (dr)) != INTEGER_CST)
	{
	  if (nested_in_vect_loop_p (loop, stmt))
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "not vectorized: not suitable for strided "
				   "load ");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
		  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
		}
	      return false;
	    }
	}
    }

  return true;
}


/* Function is_raw_dependence.

   Helper function to check if there exists read-after-write dependence between
   data reference DRA and DRB.  */

static bool
is_raw_dependence (struct data_reference *dra, struct data_reference *drb)
{
  gimple *earlier;
  struct data_reference *earlier_ref;

  /* Identify first reference, and check if read-after-write condition.  */
  earlier = get_earlier_stmt (DR_STMT (dra), DR_STMT (drb));
  if (earlier)
    {
      if (earlier == DR_STMT (dra))
	earlier_ref = dra;
      else
	earlier_ref = drb;

      if (DR_IS_WRITE (earlier_ref))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "READ_WRITE dependence in interleaving."
			     "\n");
	  return true;
	}
    }
  return false;
}

/* Function vect_analyze_dataref_dependence.

   Return TRUE if there exists dependence between any two data references DRA
   and DRB in DDR, otherwise the loop is vectorizable.  */

static bool
vect_analyze_dataref_dependence (struct data_dependence_relation *ddr,
				 struct ITER_node *inode)
{
  unsigned int i;
  struct loop *loop = ITER_NODE_LOOP (inode);
  struct data_reference *dra = DDR_A (ddr);
  struct data_reference *drb = DDR_B (ddr);
  lambda_vector dist_v;
  unsigned int loop_depth;

  /* Independent data accesses.  */
  if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    return false;

  if (dra == drb
      || (DR_IS_READ (dra) && DR_IS_READ (drb)))
    return false;

  /* Even if we have an anti-dependence then, as the vectorized loop covers at
     least two scalar iterations, there is always also a true dependence.
     As the vectorizer does not re-order loads and stores we can ignore
     the anti-dependence if TBAA can disambiguate both DRs similar to the
     case with known negative distance anti-dependences (positive
     distance anti-dependences would violate TBAA constraints).  */
  if (((DR_IS_READ (dra) && DR_IS_WRITE (drb))
       || (DR_IS_WRITE (dra) && DR_IS_READ (drb)))
      && !alias_sets_conflict_p (get_alias_set (DR_REF (dra)),
				 get_alias_set (DR_REF (drb))))
    return false;

  /* Unknown data dependence.  */
  if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
    {
      /* If user asserted safelen consecutive iterations can be
	 executed concurrently, assume independence.  */
      if (loop->safelen >= 2)
	{
	  return false;
	}

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "versioning for alias required: "
			   "can't determine dependence between ");
	  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
			     DR_REF (dra));
	  dump_printf (MSG_MISSED_OPTIMIZATION, " and ");
	  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
			     DR_REF (drb));
	  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	}

      /* Add to list of ddrs that need to be tested at run-time.  */
      return !vect_mark_for_runtime_alias_test_1 (ddr, loop);
    }

  /* Known data dependence.  */
  if (DDR_NUM_DIST_VECTS (ddr) == 0)
    {
      /* If user asserted safelen consecutive iterations can be
	 executed concurrently, assume independence.  */
      if (loop->safelen >= 2)
	{
	  return false;
	}

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "versioning for alias required: "
			   "bad dist vector for ");
	  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, DR_REF (dra));
	  dump_printf (MSG_MISSED_OPTIMIZATION,  " and ");
	  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, DR_REF (drb));
	  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	}
      /* Add to list of ddrs that need to be tested at run-time.  */
      return !vect_mark_for_runtime_alias_test_1 (ddr, loop);
    }

  loop_depth = index_in_loop_nest (loop->num, DDR_LOOP_NEST (ddr));
  FOR_EACH_VEC_ELT (DDR_DIST_VECTS (ddr), i, dist_v)
    {
      int dist = dist_v[loop_depth];

      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "dependence distance  = %d.\n", dist);

      if (dist == 0)
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "dependence distance == 0 between ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dra));
	      dump_printf (MSG_NOTE, " and ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (drb));
	      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	    }

	  if (is_raw_dependence (dra, drb))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "READ_WRITE dependence in interleaving."
				 "\n");
	      return true;
	    }

	  continue;
	}

      if (dist > 0 && DDR_REVERSED_P (ddr))
	{
	  /* If DDR_REVERSED_P the order of the data-refs in DDR was
	     reversed (to make distance vector positive), and the actual
	     distance is negative.  */
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "dependence distance negative.\n");
	  continue;
	}

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
		       "not vectorized, possible dependence "
		       "between data-refs ");
	  dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dra));
	  dump_printf (MSG_NOTE,  " and ");
	  dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (drb));
	  dump_printf (MSG_NOTE,  "\n");
	}

      return true;
    }

  return false;
}

/* Function vect_analyze_dataref_dependences.

   Examine all the data references within the LOOP, and make sure there does not
   exist any data dependence between them.  */

static bool
vect_analyze_dataref_dependences (struct ITER_node *inode,
				  vec<loop_p> loop_nest)
{
  unsigned int i;
  struct data_dependence_relation *ddr;

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "=== vect_analyze_data_ref_dependences ===\n");

  ITER_NODE_DATA_DEPS (inode).create (ITER_NODE_DATA_REFS (inode).length ()
	     * ITER_NODE_DATA_REFS (inode).length ());
  if (!compute_all_dependences (ITER_NODE_DATA_REFS (inode),
				&ITER_NODE_DATA_DEPS (inode),
				loop_nest, true))
    return false;

  FOR_EACH_VEC_ELT (ITER_NODE_DATA_DEPS (inode), i, ddr)
    if (vect_analyze_dataref_dependence (ddr, inode))
      return false;

  return true;
}

/* Function mark_addr_index_stmt.

   This function marks the statements involved in address computation within the
   loop, which need not be vectorized, as scalars.  Currently, need of this
   function is unknown.  */

static void
mark_addr_index_stmt (tree use, struct loop *loop)
{
  return;
}

/* Function classify_loop_stmt.

   The computations within the loop can be classified as :
   - Induction : Following form where v1 is loop invarient.

 		 var_phi = PHI (var_loop, var_inv)
		 :
		 var_loop = op (var_phi, v1)
		
   - Reduction : Following form where reduction variable var_loop or reduction
		 component var_phi has single use.  v1 can be any vectorizable
		 statement.

		 var_phi = PHI (var_loop, var_inv)
		 :
		 var_loop = op (var_phi, v1)
		 :
		 var_out_phi = PHI (var_loop)

   - Intermediate : Intermediate computations defining ssa_vars which are not
		    live beyond the loop.  The operands can be loop invarient/
		    results of computations within the loop/PHI nodes/constant/
		    memory.

   - Loop invarient : Constant/memory references/ssa_vars with computations
		      outside the loop.  There is no redefinition of components
		      of computations within the loop.

   - Scalar : The statements which contribute in address computations or other
	      accesses which need not be vectorized.

   - Complex : Not vectorizable.

   classify_loop_stmt ():
   - The loop statement with all operands on RHS as loop invariant (constants or
     ssa_vars defined outside loop) are marked as loop invariant.  RO memory can
     be marked as loop invariant.  Currently, RW memory is not marked as loop
     invariant.

   - The loop statement which is not vectorizable is marked as complex, and
     vectorization is terminated.

   - All other loop statements which have non-PHI nodes as output are marked as
     intermediate.

   - If any PHI node, which is neither reduction nor induction variable,
     vectorization is terminated.
*/

static bool
classify_loop_stmt (gimple *stmt, struct loop * loop)
{
  char *attr_name[] = {"Undefined", "Scalar", "loop_invariant", "induction",
		       "reduction", "intermediate", "complex",
		       "loop_exit_control"};
  enum stmt_use_type stmt_type, def_type;
  use_operand_p use_p;
  ssa_op_iter iter;

  if (gimple_has_volatile_ops (stmt))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: stmt has volatile operand\n");
      return false;
    }

  if (STMT_ATTR_USE_TYPE (stmt) != stmt_use_type_undef
      && STMT_ATTR_USE_TYPE (stmt) != stmt_use_type_scalar)
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location, " classify_loop_stmt: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
 	  dump_printf_loc (MSG_NOTE, vect_location,
			   "  Preprocessed : %s\n",
			   attr_name[STMT_ATTR_USE_TYPE (stmt)]);
	}
      return (STMT_ATTR_USE_TYPE (stmt) != stmt_use_type_complex);
    }

  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location, " classify_loop_stmt: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "  PHI : %s\n",
			   attr_name[STMT_ATTR_USE_TYPE (stmt)]);
 	}
      return true;
    }
	
  /* The statement lies outside the loop, so no need to analyze the statement
     further.  */
  if (!flow_bb_inside_loop_p (loop, gimple_bb (stmt)))
    {
      STMT_ATTR_USE_TYPE (stmt) = stmt_use_type_loop_invariant;
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location, " classify_loop_stmt: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
	  dump_printf_loc (MSG_NOTE, vect_location,
		   "  Statement outside the loop under consideration.\n");
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "  : %s\n",
			   attr_name[STMT_ATTR_USE_TYPE (stmt)]);
	}
      return true;
    }

  /* If DR in STMT, there is LOAD/STORE operation.  Mark the instructions
     computing address for indexing as non-vectorizable/scalar.  */
  if (STMT_ATTR_DR (stmt) && gimple_assign_single_p (stmt))
    {
      tree scalar_type;
      tree op0 = gimple_assign_lhs (stmt);
      tree op1 = gimple_assign_rhs1 (stmt);
      enum tree_code code;

      if (TREE_CODE (op0) == SSA_NAME)
	{
	  code = TREE_CODE (op1);
	  if (code == ARRAY_REF
      	      || code == BIT_FIELD_REF
 	      || code == INDIRECT_REF
      	      || code == COMPONENT_REF
      	      || code == IMAGPART_EXPR
      	      || code == REALPART_EXPR
      	      || code == MEM_REF
	      || TREE_CODE_CLASS (code) == tcc_declaration)
	    {
	      /* LOAD - trace SSA_NAME for address if any.  Mark it as scalar if
		 not marked already.  For loads, no need to analyze uses.  */
	      mark_addr_index_stmt (TREE_OPERAND (op1, 0), loop);
	      STMT_ATTR_USE_TYPE (stmt) = stmt_use_type_intermediate;
	      scalar_type = TREE_TYPE (DR_REF (STMT_ATTR_DR (stmt)));
	      STMT_ATTR_VECTYPE (stmt) =
			 get_vectype_for_scalar_type (scalar_type);

	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_NOTE, vect_location,
				   " classify_loop_stmt: ");
		  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
		  dump_printf_loc (MSG_NOTE, vect_location,
				   " : %s\n",
				   attr_name[STMT_ATTR_USE_TYPE (stmt)]);
		}
	      return true;
	    }
	  /* TODO: Conversion needs to be handled here.  */
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Expected load.\n");
	  return false;
	}
      else
	{
	  code = TREE_CODE (op0);
	  if (code == ARRAY_REF
	      || code == BIT_FIELD_REF
	      || code == INDIRECT_REF
	      || code == COMPONENT_REF
	      || code == IMAGPART_EXPR
	      || code == REALPART_EXPR
	      || code == MEM_REF
	      || TREE_CODE_CLASS (code) == tcc_declaration)
	    {
	      /* STORE - Trace SSA_NAME for address if any.  Mark it as scalar
		 if not marked already.  Do not return, as we need to analyze
		 uses.  However, the statement itself is marked of type
		 intermediate, so that it is not marked as loop invariant.  */
	      mark_addr_index_stmt (TREE_OPERAND (op0, 0), loop);
	      STMT_ATTR_USE_TYPE (stmt) = stmt_use_type_intermediate;
	    }
	  else
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Expected store.\n");

	      return false;
	    }
	}
    }

  /* TODO: PAREN_EXPR and CONVERT_EXPR_CODE_P need to be handled here?? - may
     not.  But still, marking the location.  */
  bool retval = true;
  tree vec_type = NULL;
  stmt_type = STMT_ATTR_USE_TYPE (stmt);
  FOR_EACH_PHI_OR_STMT_USE (use_p, stmt, iter, SSA_OP_USE)
    {
      tree op = USE_FROM_PTR (use_p);
      enum tree_code code = TREE_CODE (op);

      if (code == SSA_NAME)
	{
	  if (!SSA_NAME_IS_DEFAULT_DEF (op))
	    {
	      gimple *def_stmt = SSA_NAME_DEF_STMT (op);
	      retval &= classify_loop_stmt (def_stmt, loop);

	      def_type = STMT_ATTR_USE_TYPE (def_stmt);

	      if (def_type == stmt_use_type_induction &&
		  stmt_type == stmt_use_type_undef)
		stmt_type = stmt_use_type_scalar;
	      else if (stmt_type > stmt_use_type_scalar &&
		       stmt_type != def_type)
		stmt_type = stmt_use_type_intermediate;
	      else
		stmt_type = def_type;

	      vec_type = STMT_ATTR_VECTYPE (def_stmt);
 	      if (STMT_ATTR_VECTYPE (stmt) && vec_type
		  && !useless_type_conversion_p (vec_type,
						 STMT_ATTR_VECTYPE (stmt)))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			"Not vectorized: Types of operands do not match.\n");
		  return false;
		}
	      STMT_ATTR_VECTYPE (stmt) = vec_type;
	    }
	}
      else if (CONSTANT_CLASS_P (op) || is_gimple_min_invariant (op))
	{
	  if (stmt_type <= stmt_use_type_loop_invariant)
	    stmt_type = stmt_use_type_loop_invariant;
	  else
	    stmt_type = stmt_use_type_intermediate;
	}
    }

  /* Once all the USEs of stmt are marked, mark the type of this statement.  */
  STMT_ATTR_USE_TYPE (stmt) = stmt_type;

  if (stmt_type == stmt_use_type_undef)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
		 "Not vectorized: stmt use type UNDEF at end of processing.\n");
      return false;
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, " classify_loop_stmt: ");
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
      dump_printf_loc (MSG_NOTE, vect_location,
		       "  : %s\n",
		       attr_name[STMT_ATTR_USE_TYPE (stmt)]);
    }

  return retval;
}

/*
  FUNCTION classify_loop_stmts.

  - Analyze PHI nodes in loop header to identify induction variables.  As
    induction variables have initial definition and a definition within loop,
    each induction variable aught to be in the PHI nodes of loop header.  Hence,
    we need not identify them from loop body.

  - If the induction variable has variable step, ILV node creation is not
    possible (for now), as the arity depends upon the stepsize.  It can still be
    reduction variable, hence decision of vectorization is not taken yet.

  - The variables that are not induction variables, are checked if they are
    reduction variables - the reduction operation is vectorizable only if the
    reduction variable does not have any use apart from that in outgoing PHI
    node.

  - Invoke function classify_loop_stmt () to classify each statement in
    PROBABLE_ROOTS list recursively so that all the USEs in the statement are
    processed.
*/

static bool
classify_loop_stmts (struct ITER_node *inode)
{
  basic_block *bbs;
  int nbbs;
  struct loop *loop = ITER_NODE_LOOP (inode);
  vec<gimple *> worklist = vNULL;

  bbs = get_loop_body (loop);
  nbbs = loop->num_nodes;
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "==== classify_loop_stmts ====\n");
    }

  /* Mark phi node.  */
  for (gphi_iterator si = gsi_start_phis (loop->header);
       !gsi_end_p (si); gsi_next (&si))
    {
      gphi *phi = si.phi ();
      tree access_fn = NULL;
      tree def = PHI_RESULT (phi);
      tree init, step;

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location, "Analyze phi: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
	}

      if (virtual_operand_p (def))
	continue;

      access_fn = analyze_scalar_evolution (loop, def);
      if (access_fn)
	{
	  STMT_ATTR_ACCESS_FN (phi) = access_fn;
	}
      else
	{
	  worklist.safe_push (phi);
	  continue;
	}

      if (vect_is_simple_iv_evolution (loop->num, access_fn, &init, &step))
	{
	  STMT_ATTR_USE_TYPE (phi) = stmt_use_type_induction;
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "Detected induction.\n");
	      dump_printf (MSG_NOTE, "\n");
	    }
	}
      else
	worklist.safe_push (phi);
    }
#define REDUCTION_COMPLETE 0
#if REDUCTION_COMPLETE
  while (worklist.length () > 0)
    {
      gimple *phi = worklist.pop ();
      tree def = PHI_RESULT (phi);

      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Analyze phi for reduction: ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, phi, 0);
	}

      gcc_assert (!virtual_operand_p (def)
		  && STMT_ATTR_USE_TYPE (phi) == stmt_use_type_undef);

      reduc_stmt = vect_simple_reduction ();

      if (reduc_stmt)
	{
	}
      else
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "Unknown def-use cycle pattern.\n");
	      dump_printf (MSG_NOTE, "\n");
	    }
	}
    }
#else
  if (worklist.length () > 0)
    {
      if (dump_enabled_p ())
	{
  	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
	    "Reduction not supported yet.  Unknown def-use cycle pattern.\n");
	  dump_printf (MSG_NOTE, "\n");
	}
      return false;
    }
#endif

  worklist = ITER_NODE_PROBABLE_ROOT_NODES (inode).copy ();

  while (worklist.length () > 0)
    {
      gimple *stmt = worklist.pop ();

      if (!classify_loop_stmt (stmt, loop))
	return false;
    }

  return true;
}

/* FUNCTION mark_probable_root_nodes.

   The loop has side effects only if
   - Loop writes to memory location.
   - The computations within loop are live after the loop.

   This function identifies such nodes.

   mark_probable_root_nodes () -
     FORALL statements within loop DO
     - If the result of statement is in outgoing PHI node (live across loop
       body) mark the statement for probable root p-node.

     - If the result of statement is memory, mark the statement as probable root
       p-node.

     - If the loop is inner loop, and any statement uses induction variable of
       outer loop - because of which stride > vector_size, do not vectorize.
*/

static void
mark_probable_root_nodes (struct ITER_node *inode)
{
  int i;
  gimple *stmt;
  ssa_op_iter op_iter;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  def_operand_p def_p;
  struct loop *loop = ITER_NODE_LOOP (inode);
  int nbbs = loop->num_nodes;
  basic_block *bbs = get_loop_body (ITER_NODE_LOOP (inode));

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];
      gimple_stmt_iterator si;

      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  stmt = gsi_stmt (si);
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
		 " mark_probable_root_nodes: ");
	      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
	    }

	  if (gimple_has_volatile_ops (stmt))
	    continue;

	  if (gimple_vdef (stmt) && !gimple_clobber_p (stmt))
	    {
       	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
      				 "  : Memory def.\n");
	      ITER_NODE_PROBABLE_ROOT_NODES (inode).safe_push (stmt);
	      STMT_ATTR_PROOT (stmt) = true;
	    }


	  FOR_EACH_PHI_OR_STMT_DEF (def_p, stmt, op_iter, SSA_OP_DEF)
	    {
	      FOR_EACH_IMM_USE_FAST (use_p, imm_iter, DEF_FROM_PTR (def_p))
		{
		  basic_block bb = gimple_bb (USE_STMT (use_p));
		  if (!flow_bb_inside_loop_p (loop, bb))
		    {
		      if (dump_enabled_p ())
			dump_printf_loc (MSG_NOTE, vect_location,
	  				 "  : Live beyond loop.\n");

		      if (is_gimple_debug (USE_STMT (use_p)))
	    		continue;

			/* We expect all such uses to be in the loop exit phis
	  		   (because of loop closed form)   */
		      gcc_assert (gimple_code (USE_STMT (use_p)) == GIMPLE_PHI);
	  	      gcc_assert (bb == single_exit (loop)->dest);

		      ITER_NODE_PROBABLE_ROOT_NODES (inode).safe_push (stmt);
		      STMT_ATTR_PROOT (stmt) = true;
	    	    }
		}
    	    }
	}
    }
}

/* Function vect_is_simple_use.

   Return TRUE if OPERAND is either constant, invariant, or internal SSA_NAME
   with simple DEF_STMT.  If it is SSA_NAME, return the statement defining the
   name in DEF_STMT.  */

bool
vect_is_simple_use (tree operand, gimple **def_stmt)
{
  *def_stmt = NULL;

  if (CONSTANT_CLASS_P (operand))
    return true;
  if (is_gimple_min_invariant (operand))
    return true;
  if (TREE_CODE (operand) != SSA_NAME)
    return false;
  if (SSA_NAME_IS_DEFAULT_DEF (operand))
    return true;

  *def_stmt = SSA_NAME_DEF_STMT (operand);

  switch (gimple_code (*def_stmt))
    {
    case GIMPLE_PHI:
    case GIMPLE_ASSIGN:
    case GIMPLE_CALL:
      break;
    default:
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "unsupported defining stmt:\n");
      return false;
    }
  return true;
}

/* Function normalize_base_addr.

   This function normalizes the address used by the STMT within LOOP.  The data
   reference DR holds BASE_ADDRESS, INIT component of first element, and OFFSET
   of current element.  This function combines all the components of the address
   and splits it into 2 components - BASE_ADDRESS and OFFSET which is < stride.
*/

static tree
normalize_base_addr (gimple *stmt, struct loop *loop, tree *offset)
{
  tree addr, var, constant, retval;

  addr = size_binop (PLUS_EXPR,
	      fold_convert (ssizetype, DR_BASE_ADDRESS (STMT_ATTR_DR (stmt))),
	      size_binop (PLUS_EXPR,
	  	   fold_convert (ssizetype, DR_INIT (STMT_ATTR_DR (stmt))),
		   fold_convert (ssizetype, DR_OFFSET (STMT_ATTR_DR (stmt)))));

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "normalize_base_addr: ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM,
			 addr);
      dump_printf (MSG_NOTE, "\n");
    }

  split_constant_offset (addr, &var, &constant);

  if (tree_fits_shwi_p (constant)
      && tree_fits_shwi_p (DR_STEP (STMT_ATTR_DR (stmt))))
    {
      int c, step;
      c = tree_to_shwi (constant);
      step = tree_to_shwi (DR_STEP (STMT_ATTR_DR (stmt)));

      if (c >= step)
	{
	  *offset = ssize_int (c % step);
	  retval = size_binop (PLUS_EXPR, var, ssize_int (c / step * step));
	}
      else
	{
	  *offset = constant;
	  retval = var;
	}

    }
  else
    {
      *offset = ssize_int (0);
      retval = size_binop (PLUS_EXPR, var, constant);
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "\tbase: ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM,
			 retval);
      dump_printf (MSG_NOTE, "\toffset: ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM,
			 *offset);
      dump_printf (MSG_NOTE, "\n");
    }
  return retval;
}

/* Function exists_primTree_with_memref.

    This function checks if PRIMOP_TREE node corresponding to MEMREF already
    exists in ITER_node.  If yes, return the pointer of PRIMOP_TREE node, else,
    return NULL.  */

struct primop_tree *
exists_primTree_with_memref (tree base, tree step, bool is_read,
			     struct ITER_node *inode)
{
  vec<struct primop_tree *> worklist;
  worklist = (ITER_NODE_LOOP_BODY (inode)).copy ();

  while (worklist.length () > 0)
    {
      primop_tree *ptree = worklist.pop ();

      if (!operand_equal_p (PT_MEMVAL_BASE (ptree), base, 0))
	continue;

      if (!operand_equal_p (PT_MEMVAL_MULT_IDX (ptree), step, 0))
	continue;

      if (is_read != PT_MEMVAL_IS_READ (ptree))
	continue;

      if (dump_enabled_p ())
	{
      	  dump_printf_loc (MSG_NOTE, vect_location,
	 		   " exists_primTree_with_memref: TRUE\n");
	}

      return ptree;
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
  	 	       " exists_primTree_with_memref: FALSE\n");
    }

  return NULL;
}

/* Create primtree of type mem_ref - it is only for load/store.  */

struct primop_tree *
create_primTree_memref (tree base, tree step, bool is_read, int num,
			tree iter_count, struct primop_tree *parent,
			tree vec_type)
{
  struct primop_tree * ptree;

  ptree = populate_prim_node (POP_MEMREF, iter_count, parent, NULL, vec_type);

  PT_MEMVAL_BASE (ptree) = unshare_expr (base);
  PT_MEMVAL_MULT_IDX (ptree) = unshare_expr (step);
  PT_MEMVAL_IS_READ (ptree) = is_read;
  PT_VEC_TYPE (ptree) = vec_type;
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       " create_primTree_memref %d : stride - %d\n ",
		       PT_PID (ptree), tree_to_uhwi (step) / num);
     }
  return ptree;
}

/* Function vectorizable_store.

   This function checks if STMT is STORE instruction and is vectorizable for
   target architecture.  If TRUE, it computes MEMREF node with respect to the
   loop LOOP for which the vectorization is targetted.

   TODO: Currently, we are not handling variable strides as vectorization is not
   possible in all the cases with variable stride.

   The variable step can be of 3 types:
   1. Monotonically dependent on index
   2. Non-monotonically dependent on index.
   3. Loop invariant step.
      a. Case for classic vectorization
      b. Case for SLP

   1. Monotonically dependent on index:
   The address expression under consideration is base + step * index + offset.
   However, even if the induction variable is linear in nature, if the step is
   monotonically dependent on index - which is multiplied by induction variable,
   the expression no longer remains linear, but becomes quadratic - because of
   which loop unrolling cannot take place.

   2. Non-monotonically dependent on index:
   Again, variable step can cause RAW or WAW conflicts if it is not monotonic
   function of index, because of which optimization will be incorrect.
    eg:
     step = a[i];
     c[step * i + d] = b[i]

   If it is on RHS, though there won't be conflicts, we cannot determine memory
   locations which are accessed at compile-time, because of which optimization
   not possible.

   3. Loop invariant step:
    a) Classic auto-vectorization for stride = j.
     eg:
      j = op (v1, v2)
      for (i = 0; i < 2048; i++)
	a[i] = b[i*j]
    So, we should extract multiples of j from array b : for which instruction
    generation is very difficult as we don't know what permute order to use.

    If we have some instruction like (un)pack (vreg, reg) which (un)packs vector
    elements from vec with index = multiples of reg to form new vec, we can
    think of this optimization.

    b) The only case in which variable step can work unconditionally is if the
    variable is iteration invariant - for which SLP might work if SLP factor is
    same as vector size.
     eg:
      For vector size = 4,
      for loop body as follows:

      a[4 * i] = b[var * i]
      a[4 * i + 1] = b[var * i + 1]
      a[4 * i + 2] = b[var * i  + 2]
      a[4 * i + 3] = b[var * i + 3]

    above can be vectorized with vector load at b[var * i].  Conditions: a and b
    are distinct.

    Looked at paper "Automatic Vectorization of Interleaved Data Revisited" for
    implementation of this part, however, I personally don't find it useful for
    MIPS.  */

static struct primop_tree *
vectorizable_store (gimple *stmt, struct ITER_node *inode,
		    struct primop_tree *parent)
{
  tree src_op, dest_op, base, offset, step;
  enum tree_code code;
  tree vec_type, scalar_type, rhs_vectype;
  gimple *def_stmt;
  machine_mode vec_mode;
  struct loop *loop = ITER_NODE_LOOP (inode);
  struct primop_tree *pnode, *pchild1, *pchild2;
  int num;

  if (STMT_ATTR_USE_TYPE (stmt) != stmt_use_type_intermediate)
    return NULL;

  if (!gimple_assign_single_p (stmt))
    return NULL;

  dest_op = gimple_assign_lhs (stmt);
  code = TREE_CODE (dest_op);

  if (code != ARRAY_REF
      && code != BIT_FIELD_REF
      && code != INDIRECT_REF
      && code != COMPONENT_REF
      && code != IMAGPART_EXPR
      && code != REALPART_EXPR
      && code != MEM_REF)
    return NULL;

  if (!STMT_ATTR_DR (stmt))
    return NULL;

  scalar_type = TREE_TYPE (DR_REF (STMT_ATTR_DR (stmt)));
  vec_type = get_vectype_for_scalar_type (scalar_type);

  src_op = gimple_assign_rhs1 (stmt);

  if (!vect_is_simple_use (src_op, &def_stmt))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "use not simple.\n");
      return NULL;
    }

  rhs_vectype = STMT_ATTR_VECTYPE (def_stmt);
  if (rhs_vectype && !useless_type_conversion_p (vec_type, rhs_vectype))
    return NULL;


  vec_mode = TYPE_MODE (vec_type);
  if (optab_handler (mov_optab, vec_mode) == CODE_FOR_nothing)
    return NULL;

  /* The dataref for store is available.  Analyze it, and create memref for the
     same.  */

  num = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (STMT_ATTR_DR (stmt)))));
  base = normalize_base_addr (stmt, loop, &offset);
  step = DR_STEP (STMT_ATTR_DR (stmt));

  if (!tree_fits_uhwi_p (step) || !tree_fits_uhwi_p (offset))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "Variable stride or offset.\n");
      return NULL;
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
	  	       " vectorize_store: ptree creation\n ");
    }


  pnode = exists_primTree_with_memref (base, step, false, inode);
  if (pnode == NULL)
    {
      pnode = create_primTree_memref (base, step, false, num,
		ITER_NODE_NITERS (inode), NULL, vec_type);
      ITER_NODE_LOOP_BODY (inode).safe_insert (
			ITER_NODE_LOOP_BODY (inode).length (),
			pnode);
      pchild1 =  create_primTree_combine (POP_ILV, stmt,
			tree_to_uhwi (step) / num, ITER_NODE_NITERS (inode),
			pnode, vec_type);
      add_child_at_index (pnode, pchild1, 0);
    }
  else
    {
      pchild1 = get_child_at_index (pnode, 0);
      gcc_assert (PT_VEC_TYPE (pchild1) == vec_type);
    }
    if (def_stmt)
      {
	pchild2 = analyze_and_create_ptree (pchild1, def_stmt, inode);
	if (pchild2 == NULL)
	  return NULL;
      }
    else
      {
	/* RHS is not SSA name - it is either invariant or constant.  Create
	   appropriate primtree node accordingly.  */
	/* TODO - Create POP_CONST or POP_INV node - which will create vector
	   using VEC_INIT at later stages.  */
	return NULL;
      }

    if (tree_to_uhwi (offset) / num >= PT_DIVISION (pchild1))
      {
	if (dump_enabled_p ())
 	  {
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "Not vectorized: stride < offset\n");
	  }
	return NULL;
      }

    add_child_at_index (pchild1, pchild2, tree_to_uhwi (offset) / num);
  gcc_assert (PT_VEC_TYPE (pchild2));
    return pnode;
}

/* Function vectorizable_load.

   This function checks if STMT is LOAD instruction and is vectorizable for
   target architecture.  If TRUE, it computes MEMREF node with respect to the
   loop LOOP for which the vectorization is targetted.  */

static struct primop_tree *
vectorizable_load (gimple *stmt, struct ITER_node *inode,
		   struct primop_tree *parent)
{
  tree src_op, dest_op, step, base, offset;
  enum tree_code code;
  tree vec_type, scalar_type;
  machine_mode vec_mode;
  struct loop * loop = ITER_NODE_LOOP (inode);
  struct primop_tree *pnode, *pchild1;
  int num;


  if (STMT_ATTR_USE_TYPE (stmt) != stmt_use_type_intermediate)
    return NULL;

  if (!gimple_assign_single_p (stmt))
    return NULL;

  dest_op = gimple_assign_lhs (stmt);
  src_op = gimple_assign_rhs1 (stmt);
  code = TREE_CODE (src_op);

  if (code != ARRAY_REF
      && code != BIT_FIELD_REF
      && code != INDIRECT_REF
      && code != COMPONENT_REF
      && code != IMAGPART_EXPR
      && code != REALPART_EXPR
      && code != MEM_REF)
    return NULL;

  if (!STMT_ATTR_DR (stmt))
    return NULL;


  scalar_type = TREE_TYPE (DR_REF (STMT_ATTR_DR (stmt)));
  vec_type = get_vectype_for_scalar_type (scalar_type);

  vec_mode = TYPE_MODE (vec_type);
  if (optab_handler (mov_optab, vec_mode) == CODE_FOR_nothing)
    return NULL;

  /* The dataref for load is available.  Analyze it, and create memref for the
     same.  */

  num = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (STMT_ATTR_DR (stmt)))));
  base = normalize_base_addr (stmt, loop, &offset);
  step = DR_STEP (STMT_ATTR_DR (stmt));

  if (!tree_fits_uhwi_p (step) || !tree_fits_uhwi_p (offset))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "Variable stride or offset.\n");
      return NULL;
    }

  gcc_assert (parent);

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       " vectorize_load: ptree creation\n ");
    }

  pnode = create_primTree_partition (POP_EXTR, stmt,
		tree_to_uhwi (step) / num,
		tree_to_uhwi (offset) / num, ITER_NODE_NITERS (inode), parent, vec_type);
  pchild1 = create_primTree_memref (base, step, true, num,
		 ITER_NODE_NITERS (inode), pnode, vec_type);
  add_child_at_index (pnode, pchild1, 0);
  return pnode;
}

/* FUNCTION analyze_and_create_ptree ().

    - If the result is storing in memory, try accessing the address - extract
      <base, idx, mult_idx, offset> tuple from gimple chains

	*_4 = op (v1, v2, ...)

      - If <offset> >= <mult_idx> : Cannot vectorize currently.  (Optimization
	TODO: create new induction variable ind_var and adjust the offset to
	<offset> % <mult_idx>, and ind_var = idx + <offset>/<mult_idx>).
      - Else
	1. Create root node with mem_ref <base,idx> (if not existing already.
	   If exists, goto step 3.)
	2. Attach child - ILV node with arity = <mult_idx> to root node.
	3. Create a child to ILV at index <offset>:
	   - analyze_and_create_ptree (op (v1, v2, ...)).

    - If LHS is SSA_NAME, look at the definition of the SSA_NAME.
      -  If RHS of DEF is op (v1, v2, ...) and <op> is vectorizable, create
	 c-node with arity = arity (op), and attach the children:
	 - analyze_and_create_ptree (v1)
	 - analyze_and_create_ptree (v2)
	 -  :
	 -  :

      - If RHS is accessing memory - try accessing address - create mem_ref node
	as above.
	1. Create EXTR node with arity = <mult_idx> and extr_idx = <offset>
	2. Create a child node with mem_ref <base, idx>

    - If result is outgoing PHI node, there should be single use node
      - Access definition of the use node.
      - If op on LHS of definition is collapsible, generate a
	COLPS <op, PHI name> in epilogue.
      - Create root node with memref<PHI name, loop ind_var>
      - Create ITER node with single child -  analyze_and_create (<second_opd>)
*/

struct primop_tree *
analyze_and_create_ptree (struct primop_tree *parent, gimple *stmt,
			  struct ITER_node *inode)
{
  struct primop_tree *ptree;
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		 " analyze_and_create_ptree: ");
      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt, 0);
    }

  /* If the statement is not within the loop, create VEC_INIT node and
     return.  */

  if (!flow_bb_inside_loop_p (ITER_NODE_LOOP (inode), gimple_bb (stmt)))
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "Not vectorized: Loop invariant not handled.\n");
	}
      return NULL;
    }

  if ( (ptree = vectorizable_store (stmt, inode, parent)) == NULL
      && (ptree = vectorizable_load (stmt, inode, parent)) == NULL
      /* && (ptree = vectorizable_assign (stmt, inode, parent)) == NULL
	 && (ptree = vectorizable_reduction (stmt, inode, parent)) == NULL
	 && (ptree = vectorizable_arith (stmt, inode, parent)) == NULL */
     )
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
		 "Not vectorized: Target does not support instruction.\n");
    }
  return ptree;
}

/* Function is_ptree_complete.

   The function checks if the generated ptree  */

static bool
is_ptree_complete (struct ITER_node *inode)
{
  vec<struct primop_tree *> worklist;
  vec<struct primop_tree *> chlist;

  worklist = (ITER_NODE_LOOP_BODY (inode)).copy ();

  while (worklist.length () > 0)
    {
      primop_tree *ptree = worklist.pop ();
      if (PT_ARITY (ptree) == 0)
	continue;

      gcc_assert (ptree->children.length () == 1);

      ptree = get_child_at_index (ptree, 0);
      gcc_assert (PT_NODE_OP (ptree) == POP_ILV);
      if (PT_ARITY (ptree) > ptree->children.length ())
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			"Not vectorized: Multiple writes to same index.\n");
	    }
	  return false;
	}

      chlist = ptree->children.copy ();

      while (chlist.length () > 0)
	{
	  if (chlist.pop () == NULL)
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			"Not vectorized: Void in memory writes.\n");
		}
	      return false;
	    }
	}
    }
  return true;
}

/* Function create_ptree.

   This function is invoked if all the data reference related checks are
   successful and autovectorization is possible.  This function identifies
   statements within the LOOP which have side-effects, as PROBABLE_ROOT_NODES.
   It then classifies all the STMTs within the LOOP.  Once successful, it
   invokes analyze_and_create_ptree which actually create permute order tree.
*/

static bool
create_ptree (struct ITER_node *inode)
{
  vec<gimple *> worklist;
  bool is_ok;

  mark_probable_root_nodes (inode);

  if (ITER_NODE_PROBABLE_ROOT_NODES (inode).length () == 0)
    {
      dump_printf (MSG_MISSED_OPTIMIZATION,
	"Not vectorizable: no probable root nodes.\n");
      return false;
    }

  is_ok = classify_loop_stmts (inode);
  if (! is_ok)
    {
      dump_printf (MSG_MISSED_OPTIMIZATION,
	"Not vectorizable: Classification failed.\n");
      return false;
    }

  worklist = ITER_NODE_PROBABLE_ROOT_NODES (inode).copy ();

  while (worklist.length () > 0)
    {
      is_ok = (analyze_and_create_ptree (NULL, worklist.pop (), inode) != NULL);
      if (! is_ok)
	{
  	  dump_printf (MSG_MISSED_OPTIMIZATION,
		"Not vectorized: p-tree creation failed.\n");
	  return false;
	}
    }

  ITER_NODE_NITERS (inode) = integer_one_node;

  if (is_ptree_complete (inode))
    {
      if (dump_enabled_p ())
	{
	  dump_printf (MSG_NOTE,
		       "Vectorized: ptree complete.\n");
	}
      return true;
    }
  else
    {
      if (dump_enabled_p ())
	{
	  dump_printf (MSG_MISSED_OPTIMIZATION,
		       "Not vectorized: ptree incomplete.\n");
	}
    return false;
    }
}

/* Function vect_analyze_loop_with_prim_tree_2.

   Perform various analysis on the loop LOOP, and record the information in
   ITER_node structure.  */

static bool
vect_analyze_loop_with_prim_tree_2 (struct ITER_node *inode)
{
  bool ok;
  int max_vf = MAX_VECTORIZATION_FACTOR;
  int min_vf = 2;
  unsigned int n_stmts = 0;
  auto_vec<loop_p, 64> loop_nest;

  /* Find all data references in the loop (which correspond to vdefs/vuses)
     and analyze their evolution in the loop.  */

  struct loop *loop = ITER_NODE_LOOP (inode);
  basic_block *bbs = get_loop_body (ITER_NODE_LOOP (inode));
  if (!find_loop_nest (loop, &loop_nest))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: loop contains function calls"
			 " or data references that cannot be analyzed\n");
      return false;
    }

  for (unsigned i = 0; i < loop->num_nodes; i++)
    for (gimple_stmt_iterator gsi = gsi_start_bb (bbs[i]);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);
	if (is_gimple_debug (stmt))
	  continue;
	++n_stmts;
	if (!find_data_references_in_stmt (loop, stmt,
					   &ITER_NODE_DATA_REFS (inode)))
	  {
	    if (is_gimple_call (stmt) && loop->safelen)
	      {
		tree fndecl = gimple_call_fndecl (stmt), op;
		if (fndecl != NULL_TREE)
		  {
		    cgraph_node *node = cgraph_node::get (fndecl);
		    if (node != NULL && node->simd_clones != NULL)
		      {
			unsigned int j, n = gimple_call_num_args (stmt);
			for (j = 0; j < n; j++)
			  {
			    op = gimple_call_arg (stmt, j);
			    if (DECL_P (op)
				|| (REFERENCE_CLASS_P (op)
				    && get_base_address (op)))
			      break;
			  }
			op = gimple_call_lhs (stmt);
			// Ignore #pragma omp declare simd functions
			//   if they don't have data references in the
			//   call stmt itself.
			if (j == n
			    && !(op
				 && (DECL_P (op)
				     || (REFERENCE_CLASS_P (op)
					 && get_base_address (op)))))
			  continue;
		      }
		  }
	      }
	    if (dump_enabled_p ())
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "not vectorized: loop contains function "
			       "calls or data references that cannot "
			       "be analyzed\n");
	    return false;
	  }
	}

  if (!vect_analyze_datarefs (inode))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "bad data references.\n");
      return false;
    }

  ok = vect_analyze_dataref_accesses (inode);
  if (!ok)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "bad data access.\n");
      return false;
    }

  ok = vect_analyze_dataref_dependences (inode, loop_nest);
  if (!ok
      || max_vf < min_vf)
    {
      if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "bad data dependence.\n");
      return false;
    }

  return create_ptree (inode);
}

void
print_primtree (FILE *fp, struct primop_tree *t)
{
  switch (PT_NODE_OP (t))
    {
      case POP_ILV:
      case POP_CONCAT:
	dump_printf (MSG_NOTE, "%s:%d_%d\n", tree_code_name[PT_NODE_OP (t)],
			PT_PID (t), PT_DIVISION (t));
	break;

      case POP_EXTR:
      case POP_SPLT:
	dump_printf (MSG_NOTE, "%s:%d_%d,%d\n", tree_code_name[PT_NODE_OP (t)],
			PT_PID (t), PT_DIVISION (t), PT_OPERAND_SELECTOR (t));
	break;

      case POP_MEMREF:
	dump_printf (MSG_NOTE, "%s:%d\n", tree_code_name[PT_NODE_OP (t)],
			PT_PID (t));
	print_generic_expr (fp, PT_MEMVAL_BASE (t), TDF_SLIM);
	break;

      case POP_CONST:
      case POP_INV:
      case POP_COLLAPSE:
    	break;

      case POP_ITER:
	break;

      default:
	dump_gimple_stmt (MSG_NOTE, TDF_SLIM, PT_COMPUTE_STMT (t), 0);
	break;

    }
}

void
dump_primtree_node (int dump_kind, struct primop_tree *t)
{
    if (dump_file)
      print_primtree (dump_file, t);

    else if (alt_dump_file)
      print_primtree (alt_dump_file, t);
}

/* Function pretty_print_gimple_vec.

   Function to pretty print all the gimple statements in LIST.  */

void
pretty_print_gimple_vec (pretty_printer *pp, vec<gimple *> list)
{

  vec<gimple *> worklist;

  worklist = list.copy ();

  while (worklist.length () > 0)
    {
      pp_newline_and_indent (pp, 2);
      pp_gimple_stmt_1 (pp, worklist.pop (), 2, TDF_SLIM);
      pp_printf (pp, ";");
    }
}

/* Function pp_primop_tree.

   Function to pretty print primop_tree PTREE.  */

void
pp_primop_tree (pretty_printer *pp, struct primop_tree * ptree)
{
  int i;
  pp_newline_and_indent (pp, 0);
  pp_indent (pp);
  pp_printf (pp,"node [shape=record];");
  pp_newline_and_indent (pp, 0);
  pp_indent (pp);
  pp_printf (pp, "%d [label=\"{", PT_PID (ptree));
  dump_generic_node (pp, PT_ITER_COUNT (ptree), 0, TDF_SLIM, false);
  pp_printf (pp, "}|{%s", tree_code_name[PT_NODE_OP (ptree)]);

switch (PT_NODE_OP (ptree))
    {
      case POP_ILV:
      case POP_CONCAT:
	pp_printf (pp, "|%d", PT_DIVISION (ptree));
	break;
      case POP_EXTR:
      case POP_SPLT:
	pp_printf (pp, "|div:%d", PT_DIVISION (ptree));
	pp_printf (pp, "|sel:%d", PT_OPERAND_SELECTOR (ptree));
	break;
      case POP_COLLAPSE:
	break;
      case POP_MEMREF:
	pp_printf (pp, "|");
	dump_generic_node (pp, PT_MEMVAL_BASE (ptree), 0, TDF_SLIM, false);
	pp_printf (pp, "|stride:");
	dump_generic_node (pp, PT_MEMVAL_MULT_IDX (ptree), 0, TDF_SLIM, false);
	break;
      case POP_CONST:
	break;
      case POP_INV:
	break;
      case POP_ITER:
	break;
      default:
	break;
    }

  pp_printf (pp, "}|{%d}\"];", PT_ARITY (ptree));

  if (PT_NODE_OP (ptree) == POP_ITER)
    {
      pretty_print_iter_node (pp, PT_INODE (ptree), 2);
      return;
    }

  if (PT_ARITY (ptree) != 0)
    {
      pretty_print_ptree_vec (pp, ptree->children);

      vec<struct primop_tree *> worklist;

      worklist = ptree->children.copy ();

      for (i = 0; i < worklist.length (); i++)
	{
	  struct primop_tree *child;
	  worklist.iterate (i, &child);
 	  pp_newline_and_indent (pp, 0);
	  pp_indent (pp);
	  pp_printf (pp, "%d -> %d [label = %d];", PT_PID (ptree),
		 PT_PID (child), i);
	}
    }

}

/* Function pretty_print_ptree_vec.

   Function to pretty print the list LIST of primop nodes.  */

void
pretty_print_ptree_vec (pretty_printer *pp, vec<struct primop_tree *> list)
{
  vec<struct primop_tree *> worklist;
  struct primop_tree *tmp;
  int i;

  worklist = list.copy ();

  for (i = 0; i < worklist.length (); i++)
    {
      pp_newline_and_indent (pp, 0);
      worklist.iterate (i, &tmp);
      pp_primop_tree (pp, tmp);
    }
}

/* Function pretty_print_iter_node.

   INODE dup in .dot format.  */

void
pretty_print_iter_node (pretty_printer *pp, struct ITER_node *inode, int depth)
{
  pp_indentation (pp) += depth;
  pp_indent (pp);
  pp_printf (pp, "subgraph cluster_iter_node_%d {\n",
		 ITER_NODE_LOOP (inode)->num);
  pp_indentation (pp) += 2;
  pp_indent (pp);
  pp_printf (pp, "label=\"LOOP #%d. NUM_ITER:", ITER_NODE_LOOP (inode)->num);
  dump_generic_node (pp, ITER_NODE_NITERS (inode), 0, TDF_SLIM, false);
  pp_printf (pp, "\";\n");

  pp_indent (pp);
  pp_printf (pp, "subgraph cluster_iter_node_%d_pro {\n",
		 ITER_NODE_LOOP (inode)->num);
  pp_indentation (pp) += 2;
  pp_indent (pp);
  pp_printf (pp, "label=\"PROLOGUE\";\n");
  pretty_print_gimple_vec (pp, ITER_NODE_PROLOGUE (inode));
  pp_indentation (pp) -= 2;
  pp_indent (pp);
  pp_printf (pp, "}\n");

  pp_printf (pp, "subgraph cluster_iter_node_%d_epi {\n",
		 ITER_NODE_LOOP (inode)->num);
  pp_indentation (pp) += 2;
  pp_indent (pp);
  pp_printf (pp, "label=\"EPILOGUE\";\n");
  pretty_print_gimple_vec (pp, ITER_NODE_EPILOGUE (inode));
  pp_indentation (pp) -= 2;
  pp_indent (pp);
  pp_printf (pp, "}\n");

  pp_indentation (pp) += 2;
  pp_printf (pp, "subgraph cluster_iter_node_%d_body {\n",
		 ITER_NODE_LOOP (inode)->num);
  pp_indentation (pp) += 2;
  pp_indent (pp);
  pp_printf (pp, "label=\"LOOP\\nBODY\";\n");
  pretty_print_ptree_vec (pp, ITER_NODE_LOOP_BODY (inode));
  pp_indentation (pp) -= 2;
  pp_indent (pp);
  pp_printf (pp, "}\n");
  pp_indentation (pp) -= 2;
  pp_indent (pp);
  pp_printf (pp, "}\n");
}

/* Function dump_iter_node.

   Dump the permute order trees represented by INODE in .dot format.  */

void
dump_iter_node (struct ITER_node *inode, FILE *fp)
{
  pretty_printer pp;

  pp.buffer->stream = fp;
  pp_printf (&pp, "digraph uniopDG {\n");
  pp_printf (&pp, "  " "color=blue;" "\n");
  pp_printf (&pp, "  " "style=bold;" "\n");
  pp_printf (&pp, "  " "compound=true;" "\n");

  pretty_print_iter_node (&pp, inode, 4);

  pp_printf (&pp, "}\n");
  pp_flush (&pp);
}

static void
reset_aux_field (struct primop_tree *ptree)
{
  int i;
  PT_AUX (ptree) = -1;
  if (PT_ARITY (ptree) == 0)
    return;

  for (i = 0; i < ptree->children.length (); i++)
    reset_aux_field (get_child_at_index (ptree, i));
}

static int
get_transition_state (struct primop_tree *ptree)
{
  int i;
  vec<int> idx = vNULL;

  /* If the node is non-permute operation, return the state of terminal 'REG' as
     state of this tree, because non-permute operations are evaluated in
     registers.  */
  if (PT_NODE_OP (ptree) < MAX_TREE_CODES) 
  {
    return get_REG_terminal_state (GET_MODE_INNER (TYPE_MODE (PT_VEC_TYPE (ptree))));
  }

  /* We need not handle POP_PH as it is only for tile construction.  POP_CONCAT
     and POP_SPLT are now represented using POP_ILV and POP_EXTR for now.  Hence
     these operators need not be handled here.  POP_MEMREF and POP_CONST are
     leaf nodes, and won't be passed to this function.  POP_INV for loop
     invariants, POP_COLLAPSE for reduction operation and POP_ITER for loop or
     vec_size_reduction operation need TODO.  */

  switch (PT_NODE_OP (ptree))
    {
      case POP_ILV:
  	for (i = 0; i < ptree->children.length (); i++)
    	  {
      	    idx.safe_insert(idx.length (),
			PT_AUX (get_child_at_index (ptree, i)));
    	  }

        return transition_state_for_ilv (PT_DIVISION (ptree), idx, GET_MODE_INNER (TYPE_MODE (PT_VEC_TYPE (ptree))));

      case POP_EXTR:
	return transition_state_for_extr (PT_DIVISION (ptree),
				 PT_OPERAND_SELECTOR (ptree),
				 PT_AUX (get_child_at_index (ptree, 0)), GET_MODE_INNER (TYPE_MODE (PT_VEC_TYPE (ptree))));

      default:
        gcc_assert (!"Operator not handled.");
    }
  return -1;
}

static bool
label_permute_tree (struct primop_tree *ptree)
{
  bool ret = true;
  int i;

  if (PT_ARITY (ptree) == 0)
    {
      switch (PT_NODE_OP (ptree))
	{
	  case POP_MEMREF:
	    PT_AUX (ptree) = get_REG_terminal_state (GET_MODE_INNER (TYPE_MODE (PT_VEC_TYPE (ptree))));
	    printf ("tree : %d >> state : %d\n", PT_PID (ptree), PT_AUX (ptree));
	    break;
	  case POP_CONST:
	    PT_AUX (ptree) = get_CONST_terminal_state (GET_MODE_INNER (TYPE_MODE (PT_VEC_TYPE (ptree))));
	    printf ("tree : %d >> state : %d\n", PT_PID (ptree), PT_AUX (ptree));
	    break;
	  default:
	    gcc_assert (0);
	}
      return true;
    }

  for (i = 0; i < ptree->children.length (); i++)
    {
      ret |= label_permute_tree (get_child_at_index (ptree, i));
      if (ret == false)
	return false;
    }

  if (PT_NODE_OP (ptree) == POP_MEMREF)
    PT_AUX (ptree) = PT_AUX (get_child_at_index (ptree, 0));
  else
    PT_AUX (ptree) = get_transition_state (ptree);
  printf ("tree : %d >> state : %d\n", PT_PID (ptree), PT_AUX (ptree));

  if (PT_AUX (ptree) == -1)
    {
      printf ("\n labeled to REG\n");
      PT_AUX (ptree) = (get_REG_terminal_state (GET_MODE_INNER (TYPE_MODE (PT_VEC_TYPE (ptree)))));
    }
  else
    {
      printf ("%d\t", PT_AUX (ptree));
    }

  return true;
}

static bool
reduce_permute_tree (struct primop_tree *ptree, int goal_nt)
{
  int rule_no;
  int i;

//  if (PT_AUX (ptree) == get_REG_terminal_state ())
//    reduce_permute_tree(ptree, 0);
  rule_no = get_rule_number (ptree, goal_nt);
  if (rule_no == -1) {
    printf ("\n Matched to default rule : %d\n", PT_PID (ptree));
  } else if (is_NT2T_rule (rule_no)) {
    printf ("Terminal matched.\n");
  } else {
    printf ("\n Rule matched: %d.\t State matched: %d.\n", rule_no, PT_AUX (ptree));
    print_permute_order (rule_no);
    if (PT_ARITY (ptree) != 0 && PT_NODE_OP (ptree) == POP_MEMREF)
	return reduce_permute_tree (PT_CHILD (ptree, 0), goal_nt);
    for (i = 0; i < PT_ARITY (ptree); i++)
      {
	reduce_permute_tree (PT_CHILD (ptree, i), get_child_nt (PT_AUX (ptree), rule_no, i));
      }
  }

  return true;//(rule_no >= 0);

}

static bool
unified_perm_tree_code_generation (struct ITER_node *inode)
{
  int i;
  bool ret = false;
  struct primop_tree *tmp_tree;

  for (i = 0; i < (ITER_NODE_LOOP_BODY (inode)).length (); i++)
    {
      tmp_tree = (ITER_NODE_LOOP_BODY (inode))[i];
      reset_aux_field (tmp_tree);
      ret = label_permute_tree (tmp_tree);
      if (ret == true)
        ret = reduce_permute_tree (tmp_tree, get_REG_terminal_state (GET_MODE_INNER (TYPE_MODE (PT_VEC_TYPE (tmp_tree)))));

      return ret;
    }
}

/* Function vectorize_loops_using_uniop.

   Entry point to autovectorization using unified representation:
   For each loop D:
   - analyze the loop, and create p-tree if loop is vectorizable.
   - generate vectorised code for corresponding p-tree.  */

unsigned
vectorize_loops_using_uniop (void)
{
  unsigned int vect_loops_num;
  struct loop *loop;
  bool any_ifcvt_loops = false;
  unsigned int num_vectorized_loops = 0;
  unsigned int ret = 0;
  unsigned int i;
  unsigned int vector_sizes, max_vec_size;

  vect_loops_num = number_of_loops (cfun);

  /* Bail out if there are no loops.  */
  if (vect_loops_num <= 1)
    return 0;

  if (cfun->has_simduid_loops)
    return 0;

  //iter_node = NULL;

  init_stmt_attr_vec ();
  unif_vect_init_funct ();

  FOR_EACH_LOOP (loop, 0)
    if (loop->dont_vectorize)
      any_ifcvt_loops = true;
    else if (loop->simduid)
      continue;
    else if ((flag_tree_loop_vectorize
	      && optimize_loop_nest_for_speed_p (loop))
	     || loop->force_vectorize)
      {
	/* Vectorization should be possible.  Let us find if all statements are
	   vectorizable, and if yes, create p-tree.  */
	struct ITER_node * tmp_iter_node;
	struct primop_tree *tmp_tree;
	bool failed;
	vec<struct primop_tree *> worklist;

	vect_location = find_loop_location (loop);
	if (LOCATION_LOCUS (vect_location) != UNKNOWN_LOCATION
	    && dump_enabled_p ())
	  dump_printf (MSG_NOTE, "\nAnalyzing loop at %s:%d\n",
		       LOCATION_FILE (vect_location),
		       LOCATION_LINE (vect_location));

	tmp_iter_node = vect_populate_iter_node_from_loop (loop);

	if (!tmp_iter_node)
	  {
	    if (dump_enabled_p ())
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
	  		       "bad loop form.\n");
	    continue;
	  }

	if (!vect_analyze_loop_with_prim_tree_2 (tmp_iter_node))
	  {
	    destroy_iter_node_info (tmp_iter_node);
	    loop->aux = NULL;
	    continue;
	  }

	loop->aux = tmp_iter_node;

	if (!dbg_cnt (vect_loop))
	  {
	    any_ifcvt_loops = true;
	    break;
	  }

	if (dump_enabled_p ())
	  {
	    dump_printf (MSG_NOTE, "\nLoop is vectorizable.\n");
	    if (dump_file)
	      dump_iter_node (tmp_iter_node, dump_file);
	    if (alt_dump_file)
	      dump_iter_node (tmp_iter_node, alt_dump_file);
	  }

	/* To enable best possible instruction selection, the tree should be
	   promoted to MAX_VEC_SIZE first, and then reduced to arity supported
	   by architecture.  The macro TARGET_VECTORIZATION_ARITY provides list
  	   of arities supported by architecture.  */

	vector_sizes = targetm.vectorize.autovectorize_vector_sizes ();

	max_vec_size = 1 << floor_log2 (vector_sizes);

	failed =false;
	i = 0;
	worklist = vNULL;
	worklist = (ITER_NODE_LOOP_BODY (tmp_iter_node)).copy ();
	for (i = 0;  i < worklist.length (); i++)
	  {
	    gcc_assert (worklist.iterate (i, &tmp_tree));
	    tmp_tree = k_arity_promotion_reduction (tmp_tree, max_vec_size);
	    if (tmp_tree == NULL)
	      {
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "%d_promotion failed.\n", max_vec_size);
		failed = true;
		break;
	      }

	    tmp_tree = k_arity_promotion_reduction (tmp_tree, 2);
	    if (tmp_tree == NULL)
	      {
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "arity_promotion_reduction failed.\n");
		failed = true;
		break;
	      }

	    ITER_NODE_LOOP_BODY (tmp_iter_node)[i] = tmp_tree;
	  }

	if (failed)
	  continue;

	if (dump_enabled_p ())
	  {
	    dump_printf (MSG_NOTE, "\nk-arity promotion/reduction applied.\n");
	    if (dump_file)
	      dump_iter_node (tmp_iter_node, dump_file);
	    if (alt_dump_file)
	      dump_iter_node (tmp_iter_node, alt_dump_file);
	  }

	worklist = vNULL;
	worklist = (ITER_NODE_LOOP_BODY (tmp_iter_node)).copy ();
	for (i = 0;  i < worklist.length (); i++)
	  {
	    gcc_assert (worklist.iterate (i, &tmp_tree));
	    tmp_tree = unity_redundancy_elimination (tmp_tree);

	    ITER_NODE_LOOP_BODY (tmp_iter_node)[i] = tmp_tree;
	  }

	if (dump_enabled_p ())
	  {
	    dump_printf (MSG_NOTE, "\nUnity redundancy elimination applied.\n");
	    if (dump_file)
	      dump_iter_node (tmp_iter_node, dump_file);
	    if (alt_dump_file)
	      dump_iter_node (tmp_iter_node, alt_dump_file);
	  }

	//unified_vecsize_reduction (tmp_iter_node);

	if (dump_enabled_p ())
	  {
	    dump_printf (MSG_NOTE, "\nVector size reduction applied.\n");
	    if (dump_file)
	      dump_iter_node (tmp_iter_node, dump_file);
	    if (alt_dump_file)
	      dump_iter_node (tmp_iter_node, alt_dump_file);
	  }
	gimple *loop_vectorized_call = vect_loop_vectorized_call (loop);
	/* If the loop is vectorized, set uid of stmts within scalar loop to
	   0.  This change is needed if transform phase uses this loop info.  */
	/*if (loop_vectorized_call)
	  set_uid_loop_bbs (loop_vinfo, loop_vectorized_call);*/

	/* TODO: Insert call to transformation entry point.  */

	unified_perm_tree_code_generation (tmp_iter_node);

	num_vectorized_loops++;
	/* Now that the loop has been vectorized, allow it to be unrolled
	   etc.  */
	loop->force_vectorize = false;
	if (loop_vectorized_call)
	  {
	    fold_loop_vectorized_call (loop_vectorized_call, boolean_true_node);
	    ret |= TODO_cleanup_cfg;
	  }
      }


  vect_location = UNKNOWN_LOCATION;

  statistics_counter_event (cfun, "Vectorized loops", num_vectorized_loops);
  if (dump_enabled_p ()
      || (num_vectorized_loops > 0 && dump_enabled_p ()))
    dump_printf_loc (MSG_NOTE, vect_location,
		     "vectorized %u loops in function.\n",
		     num_vectorized_loops);


  if (any_ifcvt_loops)
    for (i = 1; i < vect_loops_num; i++)
      {
	loop = get_loop (cfun, i);
	if (loop && loop->dont_vectorize)
	  {
	    gimple *g = vect_loop_vectorized_call (loop);
	    if (g)
	      {
		fold_loop_vectorized_call (g, boolean_false_node);
		ret |= TODO_cleanup_cfg;
	      }
	  }
      }

  for (i = 1; i < vect_loops_num; i++)
    {
      struct ITER_node *inode;

      loop = get_loop (cfun, i);
      if (!loop)
	continue;
      inode = (struct ITER_node *) loop->aux;
      if (inode)
	destroy_iter_node_info (inode);
      loop->aux = NULL;
    }

  free_stmt_attr_vec ();

  if (num_vectorized_loops > 0)
    {
      /* If we vectorized any loop only virtual SSA form needs to be updated.
	 ???  Also while we try hard to update loop-closed SSA form we fail
	 to properly do this in some corner-cases (see PR56286).  */
      rewrite_into_loop_closed_ssa (NULL, TODO_update_ssa_only_virtuals);
      return TODO_cleanup_cfg;
    }

  return ret;
}
#endif
