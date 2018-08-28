/* Data References Analysis and Manipulation Utilities for Vectorization.
   Copyright (C) 2003-2018 Free Software Foundation, Inc.
   Contributed by Dorit Naishlos <dorit@il.ibm.com>
   and Ira Rosen <irar@il.ibm.com>

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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "memmodel.h"
#include "tm_p.h"
#include "ssa.h"
#include "optabs-tree.h"
#include "cgraph.h"
#include "dumpfile.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-ssa-loop-manip.h"
#include "tree-ssa-loop.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"
#include "expr.h"
#include "builtins.h"
#include "params.h"
#include "tree-cfg.h"
#include "tree-hash-traits.h"
#include "vec-perm-indices.h"
#include "internal-fn.h"

/* Return true if load- or store-lanes optab OPTAB is implemented for
   COUNT vectors of type VECTYPE.  NAME is the name of OPTAB.  */

static bool
vect_lanes_optab_supported_p (const char *name, convert_optab optab,
			      tree vectype, unsigned HOST_WIDE_INT count)
{
  machine_mode mode, array_mode;
  bool limit_p;

  mode = TYPE_MODE (vectype);
  if (!targetm.array_mode (mode, count).exists (&array_mode))
    {
      poly_uint64 bits = count * GET_MODE_BITSIZE (mode);
      limit_p = !targetm.array_mode_supported_p (mode, count);
      if (!int_mode_for_size (bits, limit_p).exists (&array_mode))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "no array mode for %s[%wu]\n",
			     GET_MODE_NAME (mode), count);
	  return false;
	}
    }

  if (convert_optab_handler (optab, array_mode, mode) == CODE_FOR_nothing)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                         "cannot use %s<%s><%s>\n", name,
                         GET_MODE_NAME (array_mode), GET_MODE_NAME (mode));
      return false;
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "can use %s<%s><%s>\n", name, GET_MODE_NAME (array_mode),
                     GET_MODE_NAME (mode));

  return true;
}


/* Return the smallest scalar part of STMT_INFO.
   This is used to determine the vectype of the stmt.  We generally set the
   vectype according to the type of the result (lhs).  For stmts whose
   result-type is different than the type of the arguments (e.g., demotion,
   promotion), vectype will be reset appropriately (later).  Note that we have
   to visit the smallest datatype in this function, because that determines the
   VF.  If the smallest datatype in the loop is present only as the rhs of a
   promotion operation - we'd miss it.
   Such a case, where a variable of this datatype does not appear in the lhs
   anywhere in the loop, can only occur if it's an invariant: e.g.:
   'int_x = (int) short_inv', which we'd expect to have been optimized away by
   invariant motion.  However, we cannot rely on invariant motion to always
   take invariants out of the loop, and so in the case of promotion we also
   have to check the rhs.
   LHS_SIZE_UNIT and RHS_SIZE_UNIT contain the sizes of the corresponding
   types.  */

tree
vect_get_smallest_scalar_type (stmt_vec_info stmt_info,
			       HOST_WIDE_INT *lhs_size_unit,
			       HOST_WIDE_INT *rhs_size_unit)
{
  tree scalar_type = gimple_expr_type (stmt_info->stmt);
  HOST_WIDE_INT lhs, rhs;

  /* During the analysis phase, this function is called on arbitrary
     statements that might not have scalar results.  */
  if (!tree_fits_uhwi_p (TYPE_SIZE_UNIT (scalar_type)))
    return scalar_type;

  lhs = rhs = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (scalar_type));

  gassign *assign = dyn_cast <gassign *> (stmt_info->stmt);
  if (assign
      && (gimple_assign_cast_p (assign)
	  || gimple_assign_rhs_code (assign) == DOT_PROD_EXPR
	  || gimple_assign_rhs_code (assign) == WIDEN_SUM_EXPR
	  || gimple_assign_rhs_code (assign) == WIDEN_MULT_EXPR
	  || gimple_assign_rhs_code (assign) == WIDEN_LSHIFT_EXPR
	  || gimple_assign_rhs_code (assign) == FLOAT_EXPR))
    {
      tree rhs_type = TREE_TYPE (gimple_assign_rhs1 (assign));

      rhs = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (rhs_type));
      if (rhs < lhs)
        scalar_type = rhs_type;
    }

  *lhs_size_unit = lhs;
  *rhs_size_unit = rhs;
  return scalar_type;
}


/* Insert DDR into LOOP_VINFO list of ddrs that may alias and need to be
   tested at run-time.  Return TRUE if DDR was successfully inserted.
   Return false if versioning is not supported.  */

static bool
vect_mark_for_runtime_alias_test (ddr_p ddr, loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);

  if ((unsigned) PARAM_VALUE (PARAM_VECT_MAX_VERSION_FOR_ALIAS_CHECKS) == 0)
    return false;

  if (!runtime_alias_check_p (ddr, loop,
			      optimize_loop_nest_for_speed_p (loop)))
    return false;

  LOOP_VINFO_MAY_ALIAS_DDRS (loop_vinfo).safe_push (ddr);
  return true;
}

/* Record that loop LOOP_VINFO needs to check that VALUE is nonzero.  */

static void
vect_check_nonzero_value (loop_vec_info loop_vinfo, tree value)
{
  vec<tree> checks = LOOP_VINFO_CHECK_NONZERO (loop_vinfo);
  for (unsigned int i = 0; i < checks.length(); ++i)
    if (checks[i] == value)
      return;

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "need run-time check that ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, value);
      dump_printf (MSG_NOTE, " is nonzero\n");
    }
  LOOP_VINFO_CHECK_NONZERO (loop_vinfo).safe_push (value);
}

/* Return true if we know that the order of vectorized DR_INFO_A and
   vectorized DR_INFO_B will be the same as the order of DR_INFO_A and
   DR_INFO_B.  At least one of the accesses is a write.  */

static bool
vect_preserves_scalar_order_p (dr_vec_info *dr_info_a, dr_vec_info *dr_info_b)
{
  stmt_vec_info stmtinfo_a = dr_info_a->stmt;
  stmt_vec_info stmtinfo_b = dr_info_b->stmt;

  /* Single statements are always kept in their original order.  */
  if (!STMT_VINFO_GROUPED_ACCESS (stmtinfo_a)
      && !STMT_VINFO_GROUPED_ACCESS (stmtinfo_b))
    return true;

  /* STMT_A and STMT_B belong to overlapping groups.  All loads in a
     group are emitted at the position of the first scalar load and all
     stores in a group are emitted at the position of the last scalar store.
     Thus writes will happen no earlier than their current position
     (but could happen later) while reads will happen no later than their
     current position (but could happen earlier).  Reordering is therefore
     only possible if the first access is a write.  */
  stmtinfo_a = vect_orig_stmt (stmtinfo_a);
  stmtinfo_b = vect_orig_stmt (stmtinfo_b);
  stmt_vec_info earlier_stmt_info = get_earlier_stmt (stmtinfo_a, stmtinfo_b);
  return !DR_IS_WRITE (STMT_VINFO_DATA_REF (earlier_stmt_info));
}

/* A subroutine of vect_analyze_data_ref_dependence.  Handle
   DDR_COULD_BE_INDEPENDENT_P ddr DDR that has a known set of dependence
   distances.  These distances are conservatively correct but they don't
   reflect a guaranteed dependence.

   Return true if this function does all the work necessary to avoid
   an alias or false if the caller should use the dependence distances
   to limit the vectorization factor in the usual way.  LOOP_DEPTH is
   the depth of the loop described by LOOP_VINFO and the other arguments
   are as for vect_analyze_data_ref_dependence.  */

static bool
vect_analyze_possibly_independent_ddr (data_dependence_relation *ddr,
				       loop_vec_info loop_vinfo,
				       int loop_depth, unsigned int *max_vf)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  lambda_vector dist_v;
  unsigned int i;
  FOR_EACH_VEC_ELT (DDR_DIST_VECTS (ddr), i, dist_v)
    {
      int dist = dist_v[loop_depth];
      if (dist != 0 && !(dist > 0 && DDR_REVERSED_P (ddr)))
	{
	  /* If the user asserted safelen >= DIST consecutive iterations
	     can be executed concurrently, assume independence.

	     ??? An alternative would be to add the alias check even
	     in this case, and vectorize the fallback loop with the
	     maximum VF set to safelen.  However, if the user has
	     explicitly given a length, it's less likely that that
	     would be a win.  */
	  if (loop->safelen >= 2 && abs_hwi (dist) <= loop->safelen)
	    {
	      if ((unsigned int) loop->safelen < *max_vf)
		*max_vf = loop->safelen;
	      LOOP_VINFO_NO_DATA_DEPENDENCIES (loop_vinfo) = false;
	      continue;
	    }

	  /* For dependence distances of 2 or more, we have the option
	     of limiting VF or checking for an alias at runtime.
	     Prefer to check at runtime if we can, to avoid limiting
	     the VF unnecessarily when the bases are in fact independent.

	     Note that the alias checks will be removed if the VF ends up
	     being small enough.  */
	  dr_vec_info *dr_info_a = loop_vinfo->lookup_dr (DDR_A (ddr));
	  dr_vec_info *dr_info_b = loop_vinfo->lookup_dr (DDR_B (ddr));
	  return (!STMT_VINFO_GATHER_SCATTER_P (dr_info_a->stmt)
		  && !STMT_VINFO_GATHER_SCATTER_P (dr_info_b->stmt)
		  && vect_mark_for_runtime_alias_test (ddr, loop_vinfo));
	}
    }
  return true;
}


/* Function vect_analyze_data_ref_dependence.

   Return TRUE if there (might) exist a dependence between a memory-reference
   DRA and a memory-reference DRB.  When versioning for alias may check a
   dependence at run-time, return FALSE.  Adjust *MAX_VF according to
   the data dependence.  */

static bool
vect_analyze_data_ref_dependence (struct data_dependence_relation *ddr,
				  loop_vec_info loop_vinfo,
				  unsigned int *max_vf)
{
  unsigned int i;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  struct data_reference *dra = DDR_A (ddr);
  struct data_reference *drb = DDR_B (ddr);
  dr_vec_info *dr_info_a = loop_vinfo->lookup_dr (dra);
  dr_vec_info *dr_info_b = loop_vinfo->lookup_dr (drb);
  stmt_vec_info stmtinfo_a = dr_info_a->stmt;
  stmt_vec_info stmtinfo_b = dr_info_b->stmt;
  lambda_vector dist_v;
  unsigned int loop_depth;

  /* In loop analysis all data references should be vectorizable.  */
  if (!STMT_VINFO_VECTORIZABLE (stmtinfo_a)
      || !STMT_VINFO_VECTORIZABLE (stmtinfo_b))
    gcc_unreachable ();

  /* Independent data accesses.  */
  if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    return false;

  if (dra == drb
      || (DR_IS_READ (dra) && DR_IS_READ (drb)))
    return false;

  /* We do not have to consider dependences between accesses that belong
     to the same group, unless the stride could be smaller than the
     group size.  */
  if (DR_GROUP_FIRST_ELEMENT (stmtinfo_a)
      && (DR_GROUP_FIRST_ELEMENT (stmtinfo_a)
	  == DR_GROUP_FIRST_ELEMENT (stmtinfo_b))
      && !STMT_VINFO_STRIDED_P (stmtinfo_a))
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
	  if ((unsigned int) loop->safelen < *max_vf)
	    *max_vf = loop->safelen;
	  LOOP_VINFO_NO_DATA_DEPENDENCIES (loop_vinfo) = false;
	  return false;
	}

      if (STMT_VINFO_GATHER_SCATTER_P (stmtinfo_a)
	  || STMT_VINFO_GATHER_SCATTER_P (stmtinfo_b))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "versioning for alias not supported for: "
			       "can't determine dependence between ");
	      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				 DR_REF (dra));
	      dump_printf (MSG_MISSED_OPTIMIZATION, " and ");
	      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				 DR_REF (drb));
	      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	    }
	  return true;
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
      return !vect_mark_for_runtime_alias_test (ddr, loop_vinfo);
    }

  /* Known data dependence.  */
  if (DDR_NUM_DIST_VECTS (ddr) == 0)
    {
      /* If user asserted safelen consecutive iterations can be
	 executed concurrently, assume independence.  */
      if (loop->safelen >= 2)
	{
	  if ((unsigned int) loop->safelen < *max_vf)
	    *max_vf = loop->safelen;
	  LOOP_VINFO_NO_DATA_DEPENDENCIES (loop_vinfo) = false;
	  return false;
	}

      if (STMT_VINFO_GATHER_SCATTER_P (stmtinfo_a)
	  || STMT_VINFO_GATHER_SCATTER_P (stmtinfo_b))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			       "versioning for alias not supported for: "
			       "bad dist vector for ");
	      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				 DR_REF (dra));
	      dump_printf (MSG_MISSED_OPTIMIZATION, " and ");
	      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				 DR_REF (drb));
	      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	    }
	  return true;
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
      return !vect_mark_for_runtime_alias_test (ddr, loop_vinfo);
    }

  loop_depth = index_in_loop_nest (loop->num, DDR_LOOP_NEST (ddr));

  if (DDR_COULD_BE_INDEPENDENT_P (ddr)
      && vect_analyze_possibly_independent_ddr (ddr, loop_vinfo,
						loop_depth, max_vf))
    return false;

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

	  /* When we perform grouped accesses and perform implicit CSE
	     by detecting equal accesses and doing disambiguation with
	     runtime alias tests like for
	        .. = a[i];
		.. = a[i+1];
		a[i] = ..;
		a[i+1] = ..;
		*p = ..;
		.. = a[i];
		.. = a[i+1];
	     where we will end up loading { a[i], a[i+1] } once, make
	     sure that inserting group loads before the first load and
	     stores after the last store will do the right thing.
	     Similar for groups like
	        a[i] = ...;
		... = a[i];
		a[i+1] = ...;
	     where loads from the group interleave with the store.  */
	  if (!vect_preserves_scalar_order_p (dr_info_a, dr_info_b))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "READ_WRITE dependence in interleaving.\n");
	      return true;
	    }

	  if (loop->safelen < 2)
	    {
	      tree indicator = dr_zero_step_indicator (dra);
	      if (!indicator || integer_zerop (indicator))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "access also has a zero step\n");
		  return true;
		}
	      else if (TREE_CODE (indicator) != INTEGER_CST)
		vect_check_nonzero_value (loop_vinfo, indicator);
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
	  /* Record a negative dependence distance to later limit the
	     amount of stmt copying / unrolling we can perform.
	     Only need to handle read-after-write dependence.  */
	  if (DR_IS_READ (drb)
	      && (STMT_VINFO_MIN_NEG_DIST (stmtinfo_b) == 0
		  || STMT_VINFO_MIN_NEG_DIST (stmtinfo_b) > (unsigned)dist))
	    STMT_VINFO_MIN_NEG_DIST (stmtinfo_b) = dist;
	  continue;
	}

      unsigned int abs_dist = abs (dist);
      if (abs_dist >= 2 && abs_dist < *max_vf)
	{
	  /* The dependence distance requires reduction of the maximal
	     vectorization factor.  */
	  *max_vf = abs (dist);
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
	                     "adjusting maximal vectorization factor to %i\n",
	                     *max_vf);
	}

      if (abs_dist >= *max_vf)
	{
	  /* Dependence distance does not create dependence, as far as
	     vectorization is concerned, in this case.  */
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
	                     "dependence distance >= VF.\n");
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

/* Function vect_analyze_data_ref_dependences.

   Examine all the data references in the loop, and make sure there do not
   exist any data dependences between them.  Set *MAX_VF according to
   the maximum vectorization factor the data dependences allow.  */

bool
vect_analyze_data_ref_dependences (loop_vec_info loop_vinfo,
				   unsigned int *max_vf)
{
  unsigned int i;
  struct data_dependence_relation *ddr;

  DUMP_VECT_SCOPE ("vect_analyze_data_ref_dependences");

  if (!LOOP_VINFO_DDRS (loop_vinfo).exists ())
    {
      LOOP_VINFO_DDRS (loop_vinfo)
	.create (LOOP_VINFO_DATAREFS (loop_vinfo).length ()
		 * LOOP_VINFO_DATAREFS (loop_vinfo).length ());
      /* We need read-read dependences to compute
	 STMT_VINFO_SAME_ALIGN_REFS.  */
      bool res = compute_all_dependences (LOOP_VINFO_DATAREFS (loop_vinfo),
					  &LOOP_VINFO_DDRS (loop_vinfo),
					  LOOP_VINFO_LOOP_NEST (loop_vinfo),
					  true);
      gcc_assert (res);
    }

  LOOP_VINFO_NO_DATA_DEPENDENCIES (loop_vinfo) = true;

  /* For epilogues we either have no aliases or alias versioning
     was applied to original loop.  Therefore we may just get max_vf
     using VF of original loop.  */
  if (LOOP_VINFO_EPILOGUE_P (loop_vinfo))
    *max_vf = LOOP_VINFO_ORIG_MAX_VECT_FACTOR (loop_vinfo);
  else
    FOR_EACH_VEC_ELT (LOOP_VINFO_DDRS (loop_vinfo), i, ddr)
      if (vect_analyze_data_ref_dependence (ddr, loop_vinfo, max_vf))
	return false;

  return true;
}


/* Function vect_slp_analyze_data_ref_dependence.

   Return TRUE if there (might) exist a dependence between a memory-reference
   DRA and a memory-reference DRB for VINFO.  When versioning for alias
   may check a dependence at run-time, return FALSE.  Adjust *MAX_VF
   according to the data dependence.  */

static bool
vect_slp_analyze_data_ref_dependence (vec_info *vinfo,
				      struct data_dependence_relation *ddr)
{
  struct data_reference *dra = DDR_A (ddr);
  struct data_reference *drb = DDR_B (ddr);
  dr_vec_info *dr_info_a = vinfo->lookup_dr (dra);
  dr_vec_info *dr_info_b = vinfo->lookup_dr (drb);

  /* We need to check dependences of statements marked as unvectorizable
     as well, they still can prohibit vectorization.  */

  /* Independent data accesses.  */
  if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    return false;

  if (dra == drb)
    return false;

  /* Read-read is OK.  */
  if (DR_IS_READ (dra) && DR_IS_READ (drb))
    return false;

  /* If dra and drb are part of the same interleaving chain consider
     them independent.  */
  if (STMT_VINFO_GROUPED_ACCESS (dr_info_a->stmt)
      && (DR_GROUP_FIRST_ELEMENT (dr_info_a->stmt)
	  == DR_GROUP_FIRST_ELEMENT (dr_info_b->stmt)))
    return false;

  /* Unknown data dependence.  */
  if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
    {
      if  (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "can't determine dependence between ");
	  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, DR_REF (dra));
	  dump_printf (MSG_MISSED_OPTIMIZATION,  " and ");
	  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, DR_REF (drb));
	  dump_printf (MSG_MISSED_OPTIMIZATION,  "\n");
	}
    }
  else if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "determined dependence between ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dra));
      dump_printf (MSG_NOTE, " and ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (drb));
      dump_printf (MSG_NOTE,  "\n");
    }

  return true;
}


/* Analyze dependences involved in the transform of SLP NODE.  STORES
   contain the vector of scalar stores of this instance if we are
   disambiguating the loads.  */

static bool
vect_slp_analyze_node_dependences (slp_instance instance, slp_tree node,
				   vec<stmt_vec_info> stores,
				   stmt_vec_info last_store_info)
{
  /* This walks over all stmts involved in the SLP load/store done
     in NODE verifying we can sink them up to the last stmt in the
     group.  */
  stmt_vec_info last_access_info = vect_find_last_scalar_stmt_in_slp (node);
  vec_info *vinfo = last_access_info->vinfo;
  for (unsigned k = 0; k < SLP_INSTANCE_GROUP_SIZE (instance); ++k)
    {
      stmt_vec_info access_info = SLP_TREE_SCALAR_STMTS (node)[k];
      if (access_info == last_access_info)
	continue;
      data_reference *dr_a = STMT_VINFO_DATA_REF (access_info);
      ao_ref ref;
      bool ref_initialized_p = false;
      for (gimple_stmt_iterator gsi = gsi_for_stmt (access_info->stmt);
	   gsi_stmt (gsi) != last_access_info->stmt; gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (! gimple_vuse (stmt)
	      || (DR_IS_READ (dr_a) && ! gimple_vdef (stmt)))
	    continue;

	  /* If we couldn't record a (single) data reference for this
	     stmt we have to resort to the alias oracle.  */
	  stmt_vec_info stmt_info = vinfo->lookup_stmt (stmt);
	  data_reference *dr_b = STMT_VINFO_DATA_REF (stmt_info);
	  if (!dr_b)
	    {
	      /* We are moving a store or sinking a load - this means
	         we cannot use TBAA for disambiguation.  */
	      if (!ref_initialized_p)
		ao_ref_init (&ref, DR_REF (dr_a));
	      if (stmt_may_clobber_ref_p_1 (stmt, &ref, false)
		  || ref_maybe_used_by_stmt_p (stmt, &ref, false))
		return false;
	      continue;
	    }

	  bool dependent = false;
	  /* If we run into a store of this same instance (we've just
	     marked those) then delay dependence checking until we run
	     into the last store because this is where it will have
	     been sunk to (and we verify if we can do that as well).  */
	  if (gimple_visited_p (stmt))
	    {
	      if (stmt_info != last_store_info)
		continue;
	      unsigned i;
	      stmt_vec_info store_info;
	      FOR_EACH_VEC_ELT (stores, i, store_info)
		{
		  data_reference *store_dr = STMT_VINFO_DATA_REF (store_info);
		  ddr_p ddr = initialize_data_dependence_relation
				(dr_a, store_dr, vNULL);
		  dependent
		    = vect_slp_analyze_data_ref_dependence (vinfo, ddr);
		  free_dependence_relation (ddr);
		  if (dependent)
		    break;
		}
	    }
	  else
	    {
	      ddr_p ddr = initialize_data_dependence_relation (dr_a,
							       dr_b, vNULL);
	      dependent = vect_slp_analyze_data_ref_dependence (vinfo, ddr);
	      free_dependence_relation (ddr);
	    }
	  if (dependent)
	    return false;
	}
    }
  return true;
}


/* Function vect_analyze_data_ref_dependences.

   Examine all the data references in the basic-block, and make sure there
   do not exist any data dependences between them.  Set *MAX_VF according to
   the maximum vectorization factor the data dependences allow.  */

bool
vect_slp_analyze_instance_dependence (slp_instance instance)
{
  DUMP_VECT_SCOPE ("vect_slp_analyze_instance_dependence");

  /* The stores of this instance are at the root of the SLP tree.  */
  slp_tree store = SLP_INSTANCE_TREE (instance);
  if (! STMT_VINFO_DATA_REF (SLP_TREE_SCALAR_STMTS (store)[0]))
    store = NULL;

  /* Verify we can sink stores to the vectorized stmt insert location.  */
  stmt_vec_info last_store_info = NULL;
  if (store)
    {
      if (! vect_slp_analyze_node_dependences (instance, store, vNULL, NULL))
	return false;

      /* Mark stores in this instance and remember the last one.  */
      last_store_info = vect_find_last_scalar_stmt_in_slp (store);
      for (unsigned k = 0; k < SLP_INSTANCE_GROUP_SIZE (instance); ++k)
	gimple_set_visited (SLP_TREE_SCALAR_STMTS (store)[k]->stmt, true);
    }

  bool res = true;

  /* Verify we can sink loads to the vectorized stmt insert location,
     special-casing stores of this instance.  */
  slp_tree load;
  unsigned int i;
  FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (instance), i, load)
    if (! vect_slp_analyze_node_dependences (instance, load,
					     store
					     ? SLP_TREE_SCALAR_STMTS (store)
					     : vNULL, last_store_info))
      {
	res = false;
	break;
      }

  /* Unset the visited flag.  */
  if (store)
    for (unsigned k = 0; k < SLP_INSTANCE_GROUP_SIZE (instance); ++k)
      gimple_set_visited (SLP_TREE_SCALAR_STMTS (store)[k]->stmt, false);

  return res;
}

/* Record the base alignment guarantee given by DRB, which occurs
   in STMT_INFO.  */

static void
vect_record_base_alignment (stmt_vec_info stmt_info,
			    innermost_loop_behavior *drb)
{
  vec_info *vinfo = stmt_info->vinfo;
  bool existed;
  innermost_loop_behavior *&entry
    = vinfo->base_alignments.get_or_insert (drb->base_address, &existed);
  if (!existed || entry->base_alignment < drb->base_alignment)
    {
      entry = drb;
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "recording new base alignment for ");
	  dump_generic_expr (MSG_NOTE, TDF_SLIM, drb->base_address);
	  dump_printf (MSG_NOTE, "\n");
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "  alignment:    %d\n", drb->base_alignment);
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "  misalignment: %d\n", drb->base_misalignment);
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "  based on:     ");
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt_info->stmt, 0);
	}
    }
}

/* If the region we're going to vectorize is reached, all unconditional
   data references occur at least once.  We can therefore pool the base
   alignment guarantees from each unconditional reference.  Do this by
   going through all the data references in VINFO and checking whether
   the containing statement makes the reference unconditionally.  If so,
   record the alignment of the base address in VINFO so that it can be
   used for all other references with the same base.  */

void
vect_record_base_alignments (vec_info *vinfo)
{
  loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo);
  struct loop *loop = loop_vinfo ? LOOP_VINFO_LOOP (loop_vinfo) : NULL;
  data_reference *dr;
  unsigned int i;
  FOR_EACH_VEC_ELT (vinfo->shared->datarefs, i, dr)
    {
      dr_vec_info *dr_info = vinfo->lookup_dr (dr);
      stmt_vec_info stmt_info = dr_info->stmt;
      if (!DR_IS_CONDITIONAL_IN_STMT (dr)
	  && STMT_VINFO_VECTORIZABLE (stmt_info)
	  && !STMT_VINFO_GATHER_SCATTER_P (stmt_info))
	{
	  vect_record_base_alignment (stmt_info, &DR_INNERMOST (dr));

	  /* If DR is nested in the loop that is being vectorized, we can also
	     record the alignment of the base wrt the outer loop.  */
	  if (loop && nested_in_vect_loop_p (loop, stmt_info))
	    vect_record_base_alignment
	      (stmt_info, &STMT_VINFO_DR_WRT_VEC_LOOP (stmt_info));
	}
    }
}

/* Return the target alignment for the vectorized form of DR_INFO.  */

static unsigned int
vect_calculate_target_alignment (dr_vec_info *dr_info)
{
  tree vectype = STMT_VINFO_VECTYPE (dr_info->stmt);
  return targetm.vectorize.preferred_vector_alignment (vectype);
}

/* Function vect_compute_data_ref_alignment

   Compute the misalignment of the data reference DR_INFO.

   Output:
   1. DR_MISALIGNMENT (DR_INFO) is defined.

   FOR NOW: No analysis is actually performed. Misalignment is calculated
   only for trivial cases. TODO.  */

static void
vect_compute_data_ref_alignment (dr_vec_info *dr_info)
{
  stmt_vec_info stmt_info = dr_info->stmt;
  vec_base_alignments *base_alignments = &stmt_info->vinfo->base_alignments;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = NULL;
  tree ref = DR_REF (dr_info->dr);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vect_compute_data_ref_alignment:\n");

  if (loop_vinfo)
    loop = LOOP_VINFO_LOOP (loop_vinfo);

  /* Initialize misalignment to unknown.  */
  SET_DR_MISALIGNMENT (dr_info, DR_MISALIGNMENT_UNKNOWN);

  if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
    return;

  innermost_loop_behavior *drb = vect_dr_behavior (dr_info);
  bool step_preserves_misalignment_p;

  unsigned HOST_WIDE_INT vector_alignment
    = vect_calculate_target_alignment (dr_info) / BITS_PER_UNIT;
  DR_TARGET_ALIGNMENT (dr_info) = vector_alignment;

  /* No step for BB vectorization.  */
  if (!loop)
    {
      gcc_assert (integer_zerop (drb->step));
      step_preserves_misalignment_p = true;
    }

  /* In case the dataref is in an inner-loop of the loop that is being
     vectorized (LOOP), we use the base and misalignment information
     relative to the outer-loop (LOOP).  This is ok only if the misalignment
     stays the same throughout the execution of the inner-loop, which is why
     we have to check that the stride of the dataref in the inner-loop evenly
     divides by the vector alignment.  */
  else if (nested_in_vect_loop_p (loop, stmt_info))
    {
      step_preserves_misalignment_p
	= (DR_STEP_ALIGNMENT (dr_info->dr) % vector_alignment) == 0;

      if (dump_enabled_p ())
	{
	  if (step_preserves_misalignment_p)
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "inner step divides the vector alignment.\n");
	  else
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "inner step doesn't divide the vector"
			     " alignment.\n");
	}
    }

  /* Similarly we can only use base and misalignment information relative to
     an innermost loop if the misalignment stays the same throughout the
     execution of the loop.  As above, this is the case if the stride of
     the dataref evenly divides by the alignment.  */
  else
    {
      poly_uint64 vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
      step_preserves_misalignment_p
	= multiple_p (DR_STEP_ALIGNMENT (dr_info->dr) * vf, vector_alignment);

      if (!step_preserves_misalignment_p && dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "step doesn't divide the vector alignment.\n");
    }

  unsigned int base_alignment = drb->base_alignment;
  unsigned int base_misalignment = drb->base_misalignment;

  /* Calculate the maximum of the pooled base address alignment and the
     alignment that we can compute for DR itself.  */
  innermost_loop_behavior **entry = base_alignments->get (drb->base_address);
  if (entry && base_alignment < (*entry)->base_alignment)
    {
      base_alignment = (*entry)->base_alignment;
      base_misalignment = (*entry)->base_misalignment;
    }

  if (drb->offset_alignment < vector_alignment
      || !step_preserves_misalignment_p
      /* We need to know whether the step wrt the vectorized loop is
	 negative when computing the starting misalignment below.  */
      || TREE_CODE (drb->step) != INTEGER_CST)
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
	                   "Unknown alignment for access: ");
	  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, ref);
	  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	}
      return;
    }

  if (base_alignment < vector_alignment)
    {
      unsigned int max_alignment;
      tree base = get_base_for_alignment (drb->base_address, &max_alignment);
      if (max_alignment < vector_alignment
	  || !vect_can_force_dr_alignment_p (base,
					     vector_alignment * BITS_PER_UNIT))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
	                       "can't force alignment of ref: ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, ref);
	      dump_printf (MSG_NOTE, "\n");
	    }
	  return;
	}

      /* Force the alignment of the decl.
	 NOTE: This is the only change to the code we make during
	 the analysis phase, before deciding to vectorize the loop.  */
      if (dump_enabled_p ())
        {
          dump_printf_loc (MSG_NOTE, vect_location, "force alignment of ");
          dump_generic_expr (MSG_NOTE, TDF_SLIM, ref);
          dump_printf (MSG_NOTE, "\n");
        }

      dr_info->base_decl = base;
      dr_info->base_misaligned = true;
      base_misalignment = 0;
    }
  poly_int64 misalignment
    = base_misalignment + wi::to_poly_offset (drb->init).force_shwi ();

  /* If this is a backward running DR then first access in the larger
     vectype actually is N-1 elements before the address in the DR.
     Adjust misalign accordingly.  */
  if (tree_int_cst_sgn (drb->step) < 0)
    /* PLUS because STEP is negative.  */
    misalignment += ((TYPE_VECTOR_SUBPARTS (vectype) - 1)
		     * TREE_INT_CST_LOW (drb->step));

  unsigned int const_misalignment;
  if (!known_misalignment (misalignment, vector_alignment,
			   &const_misalignment))
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "Non-constant misalignment for access: ");
	  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, ref);
	  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	}
      return;
    }

  SET_DR_MISALIGNMENT (dr_info, const_misalignment);

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
		       "misalign = %d bytes of ref ",
		       DR_MISALIGNMENT (dr_info));
      dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM, ref);
      dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
    }

  return;
}

/* Function vect_update_misalignment_for_peel.
   Sets DR_INFO's misalignment
   - to 0 if it has the same alignment as DR_PEEL_INFO,
   - to the misalignment computed using NPEEL if DR_INFO's salignment is known,
   - to -1 (unknown) otherwise.

   DR_INFO - the data reference whose misalignment is to be adjusted.
   DR_PEEL_INFO - the data reference whose misalignment is being made
		  zero in the vector loop by the peel.
   NPEEL - the number of iterations in the peel loop if the misalignment
           of DR_PEEL_INFO is known at compile time.  */

static void
vect_update_misalignment_for_peel (dr_vec_info *dr_info,
				   dr_vec_info *dr_peel_info, int npeel)
{
  unsigned int i;
  vec<dr_p> same_aligned_drs;
  struct data_reference *current_dr;
  int dr_size = vect_get_scalar_dr_size (dr_info);
  int dr_peel_size = vect_get_scalar_dr_size (dr_peel_info);
  stmt_vec_info stmt_info = dr_info->stmt;
  stmt_vec_info peel_stmt_info = dr_peel_info->stmt;

 /* For interleaved data accesses the step in the loop must be multiplied by
     the size of the interleaving group.  */
  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    dr_size *= DR_GROUP_SIZE (DR_GROUP_FIRST_ELEMENT (stmt_info));
  if (STMT_VINFO_GROUPED_ACCESS (peel_stmt_info))
    dr_peel_size *= DR_GROUP_SIZE (peel_stmt_info);

  /* It can be assumed that the data refs with the same alignment as dr_peel
     are aligned in the vector loop.  */
  same_aligned_drs = STMT_VINFO_SAME_ALIGN_REFS (peel_stmt_info);
  FOR_EACH_VEC_ELT (same_aligned_drs, i, current_dr)
    {
      if (current_dr != dr_info->dr)
        continue;
      gcc_assert (!known_alignment_for_access_p (dr_info)
		  || !known_alignment_for_access_p (dr_peel_info)
		  || (DR_MISALIGNMENT (dr_info) / dr_size
		      == DR_MISALIGNMENT (dr_peel_info) / dr_peel_size));
      SET_DR_MISALIGNMENT (dr_info, 0);
      return;
    }

  if (known_alignment_for_access_p (dr_info)
      && known_alignment_for_access_p (dr_peel_info))
    {
      bool negative = tree_int_cst_compare (DR_STEP (dr_info->dr),
					    size_zero_node) < 0;
      int misal = DR_MISALIGNMENT (dr_info);
      misal += negative ? -npeel * dr_size : npeel * dr_size;
      misal &= DR_TARGET_ALIGNMENT (dr_info) - 1;
      SET_DR_MISALIGNMENT (dr_info, misal);
      return;
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location, "Setting misalignment " \
		     "to unknown (-1).\n");
  SET_DR_MISALIGNMENT (dr_info, DR_MISALIGNMENT_UNKNOWN);
}


/* Function verify_data_ref_alignment

   Return TRUE if DR_INFO can be handled with respect to alignment.  */

static bool
verify_data_ref_alignment (dr_vec_info *dr_info)
{
  enum dr_alignment_support supportable_dr_alignment
    = vect_supportable_dr_alignment (dr_info, false);
  if (!supportable_dr_alignment)
    {
      if (dump_enabled_p ())
	{
	  if (DR_IS_READ (dr_info->dr))
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: unsupported unaligned load.");
	  else
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: unsupported unaligned "
			     "store.");

	  dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
			     DR_REF (dr_info->dr));
	  dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
	}
      return false;
    }

  if (supportable_dr_alignment != dr_aligned && dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
		     "Vectorizing an unaligned access.\n");

  return true;
}

/* Function vect_verify_datarefs_alignment

   Return TRUE if all data references in the loop can be
   handled with respect to alignment.  */

bool
vect_verify_datarefs_alignment (loop_vec_info vinfo)
{
  vec<data_reference_p> datarefs = vinfo->shared->datarefs;
  struct data_reference *dr;
  unsigned int i;

  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      dr_vec_info *dr_info = vinfo->lookup_dr (dr);
      stmt_vec_info stmt_info = dr_info->stmt;

      if (!STMT_VINFO_RELEVANT_P (stmt_info))
	continue;

      /* For interleaving, only the alignment of the first access matters.   */
      if (STMT_VINFO_GROUPED_ACCESS (stmt_info)
	  && DR_GROUP_FIRST_ELEMENT (stmt_info) != stmt_info)
	continue;

      /* Strided accesses perform only component accesses, alignment is
	 irrelevant for them.  */
      if (STMT_VINFO_STRIDED_P (stmt_info)
	  && !STMT_VINFO_GROUPED_ACCESS (stmt_info))
	continue;

      if (! verify_data_ref_alignment (dr_info))
	return false;
    }

  return true;
}

/* Given an memory reference EXP return whether its alignment is less
   than its size.  */

static bool
not_size_aligned (tree exp)
{
  if (!tree_fits_uhwi_p (TYPE_SIZE (TREE_TYPE (exp))))
    return true;

  return (tree_to_uhwi (TYPE_SIZE (TREE_TYPE (exp)))
	  > get_object_alignment (exp));
}

/* Function vector_alignment_reachable_p

   Return true if vector alignment for DR_INFO is reachable by peeling
   a few loop iterations.  Return false otherwise.  */

static bool
vector_alignment_reachable_p (dr_vec_info *dr_info)
{
  stmt_vec_info stmt_info = dr_info->stmt;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);

  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
    {
      /* For interleaved access we peel only if number of iterations in
	 the prolog loop ({VF - misalignment}), is a multiple of the
	 number of the interleaved accesses.  */
      int elem_size, mis_in_elements;

      /* FORNOW: handle only known alignment.  */
      if (!known_alignment_for_access_p (dr_info))
	return false;

      poly_uint64 nelements = TYPE_VECTOR_SUBPARTS (vectype);
      poly_uint64 vector_size = GET_MODE_SIZE (TYPE_MODE (vectype));
      elem_size = vector_element_size (vector_size, nelements);
      mis_in_elements = DR_MISALIGNMENT (dr_info) / elem_size;

      if (!multiple_p (nelements - mis_in_elements, DR_GROUP_SIZE (stmt_info)))
	return false;
    }

  /* If misalignment is known at the compile time then allow peeling
     only if natural alignment is reachable through peeling.  */
  if (known_alignment_for_access_p (dr_info) && !aligned_access_p (dr_info))
    {
      HOST_WIDE_INT elmsize =
		int_cst_value (TYPE_SIZE_UNIT (TREE_TYPE (vectype)));
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
	                   "data size = %wd. misalignment = %d.\n", elmsize,
			   DR_MISALIGNMENT (dr_info));
	}
      if (DR_MISALIGNMENT (dr_info) % elmsize)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
	                     "data size does not divide the misalignment.\n");
	  return false;
	}
    }

  if (!known_alignment_for_access_p (dr_info))
    {
      tree type = TREE_TYPE (DR_REF (dr_info->dr));
      bool is_packed = not_size_aligned (DR_REF (dr_info->dr));
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
	                 "Unknown misalignment, %snaturally aligned\n",
			 is_packed ? "not " : "");
      return targetm.vectorize.vector_alignment_reachable (type, is_packed);
    }

  return true;
}


/* Calculate the cost of the memory access represented by DR_INFO.  */

static void
vect_get_data_access_cost (dr_vec_info *dr_info,
                           unsigned int *inside_cost,
                           unsigned int *outside_cost,
			   stmt_vector_for_cost *body_cost_vec,
			   stmt_vector_for_cost *prologue_cost_vec)
{
  stmt_vec_info stmt_info = dr_info->stmt;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  int ncopies;

  if (PURE_SLP_STMT (stmt_info))
    ncopies = 1;
  else
    ncopies = vect_get_num_copies (loop_vinfo, STMT_VINFO_VECTYPE (stmt_info));

  if (DR_IS_READ (dr_info->dr))
    vect_get_load_cost (stmt_info, ncopies, true, inside_cost, outside_cost,
			prologue_cost_vec, body_cost_vec, false);
  else
    vect_get_store_cost (stmt_info, ncopies, inside_cost, body_cost_vec);

  if (dump_enabled_p ())
    dump_printf_loc (MSG_NOTE, vect_location,
                     "vect_get_data_access_cost: inside_cost = %d, "
                     "outside_cost = %d.\n", *inside_cost, *outside_cost);
}


typedef struct _vect_peel_info
{
  dr_vec_info *dr_info;
  int npeel;
  unsigned int count;
} *vect_peel_info;

typedef struct _vect_peel_extended_info
{
  struct _vect_peel_info peel_info;
  unsigned int inside_cost;
  unsigned int outside_cost;
} *vect_peel_extended_info;


/* Peeling hashtable helpers.  */

struct peel_info_hasher : free_ptr_hash <_vect_peel_info>
{
  static inline hashval_t hash (const _vect_peel_info *);
  static inline bool equal (const _vect_peel_info *, const _vect_peel_info *);
};

inline hashval_t
peel_info_hasher::hash (const _vect_peel_info *peel_info)
{
  return (hashval_t) peel_info->npeel;
}

inline bool
peel_info_hasher::equal (const _vect_peel_info *a, const _vect_peel_info *b)
{
  return (a->npeel == b->npeel);
}


/* Insert DR_INFO into peeling hash table with NPEEL as key.  */

static void
vect_peeling_hash_insert (hash_table<peel_info_hasher> *peeling_htab,
			  loop_vec_info loop_vinfo, dr_vec_info *dr_info,
                          int npeel)
{
  struct _vect_peel_info elem, *slot;
  _vect_peel_info **new_slot;
  bool supportable_dr_alignment
    = vect_supportable_dr_alignment (dr_info, true);

  elem.npeel = npeel;
  slot = peeling_htab->find (&elem);
  if (slot)
    slot->count++;
  else
    {
      slot = XNEW (struct _vect_peel_info);
      slot->npeel = npeel;
      slot->dr_info = dr_info;
      slot->count = 1;
      new_slot = peeling_htab->find_slot (slot, INSERT);
      *new_slot = slot;
    }

  if (!supportable_dr_alignment
      && unlimited_cost_model (LOOP_VINFO_LOOP (loop_vinfo)))
    slot->count += VECT_MAX_COST;
}


/* Traverse peeling hash table to find peeling option that aligns maximum
   number of data accesses.  */

int
vect_peeling_hash_get_most_frequent (_vect_peel_info **slot,
				     _vect_peel_extended_info *max)
{
  vect_peel_info elem = *slot;

  if (elem->count > max->peel_info.count
      || (elem->count == max->peel_info.count
          && max->peel_info.npeel > elem->npeel))
    {
      max->peel_info.npeel = elem->npeel;
      max->peel_info.count = elem->count;
      max->peel_info.dr_info = elem->dr_info;
    }

  return 1;
}

/* Get the costs of peeling NPEEL iterations for LOOP_VINFO, checking
   data access costs for all data refs.  If UNKNOWN_MISALIGNMENT is true,
   we assume DR0_INFO's misalignment will be zero after peeling.  */

static void
vect_get_peeling_costs_all_drs (loop_vec_info loop_vinfo,
				dr_vec_info *dr0_info,
				unsigned int *inside_cost,
				unsigned int *outside_cost,
				stmt_vector_for_cost *body_cost_vec,
				stmt_vector_for_cost *prologue_cost_vec,
				unsigned int npeel,
				bool unknown_misalignment)
{
  vec<data_reference_p> datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  unsigned i;
  data_reference *dr;

  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      dr_vec_info *dr_info = loop_vinfo->lookup_dr (dr);
      stmt_vec_info stmt_info = dr_info->stmt;
      if (!STMT_VINFO_RELEVANT_P (stmt_info))
	continue;

      /* For interleaving, only the alignment of the first access
         matters.  */
      if (STMT_VINFO_GROUPED_ACCESS (stmt_info)
	  && DR_GROUP_FIRST_ELEMENT (stmt_info) != stmt_info)
	continue;

      /* Strided accesses perform only component accesses, alignment is
         irrelevant for them.  */
      if (STMT_VINFO_STRIDED_P (stmt_info)
	  && !STMT_VINFO_GROUPED_ACCESS (stmt_info))
	continue;

      int save_misalignment;
      save_misalignment = DR_MISALIGNMENT (dr_info);
      if (npeel == 0)
	;
      else if (unknown_misalignment && dr_info == dr0_info)
	SET_DR_MISALIGNMENT (dr_info, 0);
      else
	vect_update_misalignment_for_peel (dr_info, dr0_info, npeel);
      vect_get_data_access_cost (dr_info, inside_cost, outside_cost,
				 body_cost_vec, prologue_cost_vec);
      SET_DR_MISALIGNMENT (dr_info, save_misalignment);
    }
}

/* Traverse peeling hash table and calculate cost for each peeling option.
   Find the one with the lowest cost.  */

int
vect_peeling_hash_get_lowest_cost (_vect_peel_info **slot,
				   _vect_peel_extended_info *min)
{
  vect_peel_info elem = *slot;
  int dummy;
  unsigned int inside_cost = 0, outside_cost = 0;
  stmt_vec_info stmt_info = elem->dr_info->stmt;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  stmt_vector_for_cost prologue_cost_vec, body_cost_vec,
		       epilogue_cost_vec;

  prologue_cost_vec.create (2);
  body_cost_vec.create (2);
  epilogue_cost_vec.create (2);

  vect_get_peeling_costs_all_drs (loop_vinfo, elem->dr_info, &inside_cost,
				  &outside_cost, &body_cost_vec,
				  &prologue_cost_vec, elem->npeel, false);

  body_cost_vec.release ();

  outside_cost += vect_get_known_peeling_cost
    (loop_vinfo, elem->npeel, &dummy,
     &LOOP_VINFO_SCALAR_ITERATION_COST (loop_vinfo),
     &prologue_cost_vec, &epilogue_cost_vec);

  /* Prologue and epilogue costs are added to the target model later.
     These costs depend only on the scalar iteration cost, the
     number of peeling iterations finally chosen, and the number of
     misaligned statements.  So discard the information found here.  */
  prologue_cost_vec.release ();
  epilogue_cost_vec.release ();

  if (inside_cost < min->inside_cost
      || (inside_cost == min->inside_cost
	  && outside_cost < min->outside_cost))
    {
      min->inside_cost = inside_cost;
      min->outside_cost = outside_cost;
      min->peel_info.dr_info = elem->dr_info;
      min->peel_info.npeel = elem->npeel;
      min->peel_info.count = elem->count;
    }

  return 1;
}


/* Choose best peeling option by traversing peeling hash table and either
   choosing an option with the lowest cost (if cost model is enabled) or the
   option that aligns as many accesses as possible.  */

static struct _vect_peel_extended_info
vect_peeling_hash_choose_best_peeling (hash_table<peel_info_hasher> *peeling_htab,
				       loop_vec_info loop_vinfo)
{
   struct _vect_peel_extended_info res;

   res.peel_info.dr_info = NULL;

   if (!unlimited_cost_model (LOOP_VINFO_LOOP (loop_vinfo)))
     {
       res.inside_cost = INT_MAX;
       res.outside_cost = INT_MAX;
       peeling_htab->traverse <_vect_peel_extended_info *,
	   		       vect_peeling_hash_get_lowest_cost> (&res);
     }
   else
     {
       res.peel_info.count = 0;
       peeling_htab->traverse <_vect_peel_extended_info *,
	   		       vect_peeling_hash_get_most_frequent> (&res);
       res.inside_cost = 0;
       res.outside_cost = 0;
     }

   return res;
}

/* Return true if the new peeling NPEEL is supported.  */

static bool
vect_peeling_supportable (loop_vec_info loop_vinfo, dr_vec_info *dr0_info,
			  unsigned npeel)
{
  unsigned i;
  struct data_reference *dr = NULL;
  vec<data_reference_p> datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  enum dr_alignment_support supportable_dr_alignment;

  /* Ensure that all data refs can be vectorized after the peel.  */
  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      int save_misalignment;

      if (dr == dr0_info->dr)
	continue;

      dr_vec_info *dr_info = loop_vinfo->lookup_dr (dr);
      stmt_vec_info stmt_info = dr_info->stmt;
      /* For interleaving, only the alignment of the first access
	 matters.  */
      if (STMT_VINFO_GROUPED_ACCESS (stmt_info)
	  && DR_GROUP_FIRST_ELEMENT (stmt_info) != stmt_info)
	continue;

      /* Strided accesses perform only component accesses, alignment is
	 irrelevant for them.  */
      if (STMT_VINFO_STRIDED_P (stmt_info)
	  && !STMT_VINFO_GROUPED_ACCESS (stmt_info))
	continue;

      save_misalignment = DR_MISALIGNMENT (dr_info);
      vect_update_misalignment_for_peel (dr_info, dr0_info, npeel);
      supportable_dr_alignment
	= vect_supportable_dr_alignment (dr_info, false);
      SET_DR_MISALIGNMENT (dr_info, save_misalignment);

      if (!supportable_dr_alignment)
	return false;
    }

  return true;
}

/* Function vect_enhance_data_refs_alignment

   This pass will use loop versioning and loop peeling in order to enhance
   the alignment of data references in the loop.

   FOR NOW: we assume that whatever versioning/peeling takes place, only the
   original loop is to be vectorized.  Any other loops that are created by
   the transformations performed in this pass - are not supposed to be
   vectorized.  This restriction will be relaxed.

   This pass will require a cost model to guide it whether to apply peeling
   or versioning or a combination of the two.  For example, the scheme that
   intel uses when given a loop with several memory accesses, is as follows:
   choose one memory access ('p') which alignment you want to force by doing
   peeling.  Then, either (1) generate a loop in which 'p' is aligned and all
   other accesses are not necessarily aligned, or (2) use loop versioning to
   generate one loop in which all accesses are aligned, and another loop in
   which only 'p' is necessarily aligned.

   ("Automatic Intra-Register Vectorization for the Intel Architecture",
   Aart J.C. Bik, Milind Girkar, Paul M. Grey and Ximmin Tian, International
   Journal of Parallel Programming, Vol. 30, No. 2, April 2002.)

   Devising a cost model is the most critical aspect of this work.  It will
   guide us on which access to peel for, whether to use loop versioning, how
   many versions to create, etc.  The cost model will probably consist of
   generic considerations as well as target specific considerations (on
   powerpc for example, misaligned stores are more painful than misaligned
   loads).

   Here are the general steps involved in alignment enhancements:

     -- original loop, before alignment analysis:
	for (i=0; i<N; i++){
	  x = q[i];			# DR_MISALIGNMENT(q) = unknown
	  p[i] = y;			# DR_MISALIGNMENT(p) = unknown
	}

     -- After vect_compute_data_refs_alignment:
	for (i=0; i<N; i++){
	  x = q[i];			# DR_MISALIGNMENT(q) = 3
	  p[i] = y;			# DR_MISALIGNMENT(p) = unknown
	}

     -- Possibility 1: we do loop versioning:
     if (p is aligned) {
	for (i=0; i<N; i++){	# loop 1A
	  x = q[i];			# DR_MISALIGNMENT(q) = 3
	  p[i] = y;			# DR_MISALIGNMENT(p) = 0
	}
     }
     else {
	for (i=0; i<N; i++){	# loop 1B
	  x = q[i];			# DR_MISALIGNMENT(q) = 3
	  p[i] = y;			# DR_MISALIGNMENT(p) = unaligned
	}
     }

     -- Possibility 2: we do loop peeling:
     for (i = 0; i < 3; i++){	# (scalar loop, not to be vectorized).
	x = q[i];
	p[i] = y;
     }
     for (i = 3; i < N; i++){	# loop 2A
	x = q[i];			# DR_MISALIGNMENT(q) = 0
	p[i] = y;			# DR_MISALIGNMENT(p) = unknown
     }

     -- Possibility 3: combination of loop peeling and versioning:
     for (i = 0; i < 3; i++){	# (scalar loop, not to be vectorized).
	x = q[i];
	p[i] = y;
     }
     if (p is aligned) {
	for (i = 3; i<N; i++){	# loop 3A
	  x = q[i];			# DR_MISALIGNMENT(q) = 0
	  p[i] = y;			# DR_MISALIGNMENT(p) = 0
	}
     }
     else {
	for (i = 3; i<N; i++){	# loop 3B
	  x = q[i];			# DR_MISALIGNMENT(q) = 0
	  p[i] = y;			# DR_MISALIGNMENT(p) = unaligned
	}
     }

     These loops are later passed to loop_transform to be vectorized.  The
     vectorizer will use the alignment information to guide the transformation
     (whether to generate regular loads/stores, or with special handling for
     misalignment).  */

bool
vect_enhance_data_refs_alignment (loop_vec_info loop_vinfo)
{
  vec<data_reference_p> datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  enum dr_alignment_support supportable_dr_alignment;
  dr_vec_info *first_store = NULL;
  dr_vec_info *dr0_info = NULL;
  struct data_reference *dr;
  unsigned int i, j;
  bool do_peeling = false;
  bool do_versioning = false;
  bool stat;
  unsigned int npeel = 0;
  bool one_misalignment_known = false;
  bool one_misalignment_unknown = false;
  bool one_dr_unsupportable = false;
  dr_vec_info *unsupportable_dr_info = NULL;
  poly_uint64 vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  unsigned possible_npeel_number = 1;
  tree vectype;
  unsigned int mis, same_align_drs_max = 0;
  hash_table<peel_info_hasher> peeling_htab (1);

  DUMP_VECT_SCOPE ("vect_enhance_data_refs_alignment");

  /* Reset data so we can safely be called multiple times.  */
  LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo).truncate (0);
  LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo) = 0;

  /* While cost model enhancements are expected in the future, the high level
     view of the code at this time is as follows:

     A) If there is a misaligned access then see if peeling to align
        this access can make all data references satisfy
        vect_supportable_dr_alignment.  If so, update data structures
        as needed and return true.

     B) If peeling wasn't possible and there is a data reference with an
        unknown misalignment that does not satisfy vect_supportable_dr_alignment
        then see if loop versioning checks can be used to make all data
        references satisfy vect_supportable_dr_alignment.  If so, update
        data structures as needed and return true.

     C) If neither peeling nor versioning were successful then return false if
        any data reference does not satisfy vect_supportable_dr_alignment.

     D) Return true (all data references satisfy vect_supportable_dr_alignment).

     Note, Possibility 3 above (which is peeling and versioning together) is not
     being done at this time.  */

  /* (1) Peeling to force alignment.  */

  /* (1.1) Decide whether to perform peeling, and how many iterations to peel:
     Considerations:
     + How many accesses will become aligned due to the peeling
     - How many accesses will become unaligned due to the peeling,
       and the cost of misaligned accesses.
     - The cost of peeling (the extra runtime checks, the increase
       in code size).  */

  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      dr_vec_info *dr_info = loop_vinfo->lookup_dr (dr);
      stmt_vec_info stmt_info = dr_info->stmt;

      if (!STMT_VINFO_RELEVANT_P (stmt_info))
	continue;

      /* For interleaving, only the alignment of the first access
         matters.  */
      if (STMT_VINFO_GROUPED_ACCESS (stmt_info)
	  && DR_GROUP_FIRST_ELEMENT (stmt_info) != stmt_info)
	continue;

      /* For scatter-gather or invariant accesses there is nothing
	 to enhance.  */
      if (STMT_VINFO_GATHER_SCATTER_P (stmt_info)
	  || integer_zerop (DR_STEP (dr)))
	continue;

      /* Strided accesses perform only component accesses, alignment is
	 irrelevant for them.  */
      if (STMT_VINFO_STRIDED_P (stmt_info)
	  && !STMT_VINFO_GROUPED_ACCESS (stmt_info))
	continue;

      supportable_dr_alignment = vect_supportable_dr_alignment (dr_info, true);
      do_peeling = vector_alignment_reachable_p (dr_info);
      if (do_peeling)
        {
          if (known_alignment_for_access_p (dr_info))
            {
	      unsigned int npeel_tmp = 0;
	      bool negative = tree_int_cst_compare (DR_STEP (dr),
						    size_zero_node) < 0;

	      vectype = STMT_VINFO_VECTYPE (stmt_info);
	      unsigned int target_align = DR_TARGET_ALIGNMENT (dr_info);
	      unsigned int dr_size = vect_get_scalar_dr_size (dr_info);
	      mis = (negative
		     ? DR_MISALIGNMENT (dr_info)
		     : -DR_MISALIGNMENT (dr_info));
	      if (DR_MISALIGNMENT (dr_info) != 0)
		npeel_tmp = (mis & (target_align - 1)) / dr_size;

              /* For multiple types, it is possible that the bigger type access
                 will have more than one peeling option.  E.g., a loop with two
                 types: one of size (vector size / 4), and the other one of
                 size (vector size / 8).  Vectorization factor will 8.  If both
                 accesses are misaligned by 3, the first one needs one scalar
                 iteration to be aligned, and the second one needs 5.  But the
		 first one will be aligned also by peeling 5 scalar
                 iterations, and in that case both accesses will be aligned.
                 Hence, except for the immediate peeling amount, we also want
                 to try to add full vector size, while we don't exceed
                 vectorization factor.
                 We do this automatically for cost model, since we calculate
		 cost for every peeling option.  */
              if (unlimited_cost_model (LOOP_VINFO_LOOP (loop_vinfo)))
		{
		  poly_uint64 nscalars = (STMT_SLP_TYPE (stmt_info)
					  ? vf * DR_GROUP_SIZE (stmt_info) : vf);
		  possible_npeel_number
		    = vect_get_num_vectors (nscalars, vectype);

		  /* NPEEL_TMP is 0 when there is no misalignment, but also
		     allow peeling NELEMENTS.  */
		  if (DR_MISALIGNMENT (dr_info) == 0)
		    possible_npeel_number++;
		}

	      /* Save info about DR in the hash table.  Also include peeling
	         amounts according to the explanation above.  */
              for (j = 0; j < possible_npeel_number; j++)
                {
                  vect_peeling_hash_insert (&peeling_htab, loop_vinfo,
					    dr_info, npeel_tmp);
		  npeel_tmp += target_align / dr_size;
                }

	      one_misalignment_known = true;
            }
          else
            {
              /* If we don't know any misalignment values, we prefer
                 peeling for data-ref that has the maximum number of data-refs
                 with the same alignment, unless the target prefers to align
                 stores over load.  */
	      unsigned same_align_drs
		= STMT_VINFO_SAME_ALIGN_REFS (stmt_info).length ();
	      if (!dr0_info
		  || same_align_drs_max < same_align_drs)
		{
		  same_align_drs_max = same_align_drs;
		  dr0_info = dr_info;
		}
	      /* For data-refs with the same number of related
		 accesses prefer the one where the misalign
		 computation will be invariant in the outermost loop.  */
	      else if (same_align_drs_max == same_align_drs)
		{
		  struct loop *ivloop0, *ivloop;
		  ivloop0 = outermost_invariant_loop_for_expr
		    (loop, DR_BASE_ADDRESS (dr0_info->dr));
		  ivloop = outermost_invariant_loop_for_expr
		    (loop, DR_BASE_ADDRESS (dr));
		  if ((ivloop && !ivloop0)
		      || (ivloop && ivloop0
			  && flow_loop_nested_p (ivloop, ivloop0)))
		    dr0_info = dr_info;
		}

	      one_misalignment_unknown = true;

	      /* Check for data refs with unsupportable alignment that
	         can be peeled.  */
	      if (!supportable_dr_alignment)
	      {
		one_dr_unsupportable = true;
		unsupportable_dr_info = dr_info;
	      }

	      if (!first_store && DR_IS_WRITE (dr))
		first_store = dr_info;
            }
        }
      else
        {
          if (!aligned_access_p (dr_info))
            {
              if (dump_enabled_p ())
                dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                 "vector alignment may not be reachable\n");
              break;
            }
        }
    }

  /* Check if we can possibly peel the loop.  */
  if (!vect_can_advance_ivs_p (loop_vinfo)
      || !slpeel_can_duplicate_loop_p (loop, single_exit (loop))
      || loop->inner)
    do_peeling = false;

  struct _vect_peel_extended_info peel_for_known_alignment;
  struct _vect_peel_extended_info peel_for_unknown_alignment;
  struct _vect_peel_extended_info best_peel;

  peel_for_unknown_alignment.inside_cost = INT_MAX;
  peel_for_unknown_alignment.outside_cost = INT_MAX;
  peel_for_unknown_alignment.peel_info.count = 0;

  if (do_peeling
      && one_misalignment_unknown)
    {
      /* Check if the target requires to prefer stores over loads, i.e., if
         misaligned stores are more expensive than misaligned loads (taking
         drs with same alignment into account).  */
      unsigned int load_inside_cost = 0;
      unsigned int load_outside_cost = 0;
      unsigned int store_inside_cost = 0;
      unsigned int store_outside_cost = 0;
      unsigned int estimated_npeels = vect_vf_for_cost (loop_vinfo) / 2;

      stmt_vector_for_cost dummy;
      dummy.create (2);
      vect_get_peeling_costs_all_drs (loop_vinfo, dr0_info,
				      &load_inside_cost,
				      &load_outside_cost,
				      &dummy, &dummy, estimated_npeels, true);
      dummy.release ();

      if (first_store)
	{
	  dummy.create (2);
	  vect_get_peeling_costs_all_drs (loop_vinfo, first_store,
					  &store_inside_cost,
					  &store_outside_cost,
					  &dummy, &dummy,
					  estimated_npeels, true);
	  dummy.release ();
	}
      else
	{
	  store_inside_cost = INT_MAX;
	  store_outside_cost = INT_MAX;
	}

      if (load_inside_cost > store_inside_cost
	  || (load_inside_cost == store_inside_cost
	      && load_outside_cost > store_outside_cost))
	{
	  dr0_info = first_store;
	  peel_for_unknown_alignment.inside_cost = store_inside_cost;
	  peel_for_unknown_alignment.outside_cost = store_outside_cost;
	}
      else
	{
	  peel_for_unknown_alignment.inside_cost = load_inside_cost;
	  peel_for_unknown_alignment.outside_cost = load_outside_cost;
	}

      stmt_vector_for_cost prologue_cost_vec, epilogue_cost_vec;
      prologue_cost_vec.create (2);
      epilogue_cost_vec.create (2);

      int dummy2;
      peel_for_unknown_alignment.outside_cost += vect_get_known_peeling_cost
	(loop_vinfo, estimated_npeels, &dummy2,
	 &LOOP_VINFO_SCALAR_ITERATION_COST (loop_vinfo),
	 &prologue_cost_vec, &epilogue_cost_vec);

      prologue_cost_vec.release ();
      epilogue_cost_vec.release ();

      peel_for_unknown_alignment.peel_info.count = 1
	+ STMT_VINFO_SAME_ALIGN_REFS (dr0_info->stmt).length ();
    }

  peel_for_unknown_alignment.peel_info.npeel = 0;
  peel_for_unknown_alignment.peel_info.dr_info = dr0_info;

  best_peel = peel_for_unknown_alignment;

  peel_for_known_alignment.inside_cost = INT_MAX;
  peel_for_known_alignment.outside_cost = INT_MAX;
  peel_for_known_alignment.peel_info.count = 0;
  peel_for_known_alignment.peel_info.dr_info = NULL;

  if (do_peeling && one_misalignment_known)
    {
      /* Peeling is possible, but there is no data access that is not supported
         unless aligned.  So we try to choose the best possible peeling from
	 the hash table.  */
      peel_for_known_alignment = vect_peeling_hash_choose_best_peeling
	(&peeling_htab, loop_vinfo);
    }

  /* Compare costs of peeling for known and unknown alignment. */
  if (peel_for_known_alignment.peel_info.dr_info != NULL
      && peel_for_unknown_alignment.inside_cost
      >= peel_for_known_alignment.inside_cost)
    {
      best_peel = peel_for_known_alignment;

      /* If the best peeling for known alignment has NPEEL == 0, perform no
         peeling at all except if there is an unsupportable dr that we can
         align.  */
      if (best_peel.peel_info.npeel == 0 && !one_dr_unsupportable)
	do_peeling = false;
    }

  /* If there is an unsupportable data ref, prefer this over all choices so far
     since we'd have to discard a chosen peeling except when it accidentally
     aligned the unsupportable data ref.  */
  if (one_dr_unsupportable)
    dr0_info = unsupportable_dr_info;
  else if (do_peeling)
    {
      /* Calculate the penalty for no peeling, i.e. leaving everything as-is.
	 TODO: Use nopeel_outside_cost or get rid of it?  */
      unsigned nopeel_inside_cost = 0;
      unsigned nopeel_outside_cost = 0;

      stmt_vector_for_cost dummy;
      dummy.create (2);
      vect_get_peeling_costs_all_drs (loop_vinfo, NULL, &nopeel_inside_cost,
				      &nopeel_outside_cost, &dummy, &dummy,
				      0, false);
      dummy.release ();

      /* Add epilogue costs.  As we do not peel for alignment here, no prologue
	 costs will be recorded.  */
      stmt_vector_for_cost prologue_cost_vec, epilogue_cost_vec;
      prologue_cost_vec.create (2);
      epilogue_cost_vec.create (2);

      int dummy2;
      nopeel_outside_cost += vect_get_known_peeling_cost
	(loop_vinfo, 0, &dummy2,
	 &LOOP_VINFO_SCALAR_ITERATION_COST (loop_vinfo),
	 &prologue_cost_vec, &epilogue_cost_vec);

      prologue_cost_vec.release ();
      epilogue_cost_vec.release ();

      npeel = best_peel.peel_info.npeel;
      dr0_info = best_peel.peel_info.dr_info;

      /* If no peeling is not more expensive than the best peeling we
	 have so far, don't perform any peeling.  */
      if (nopeel_inside_cost <= best_peel.inside_cost)
	do_peeling = false;
    }

  if (do_peeling)
    {
      stmt_vec_info stmt_info = dr0_info->stmt;
      vectype = STMT_VINFO_VECTYPE (stmt_info);

      if (known_alignment_for_access_p (dr0_info))
        {
	  bool negative = tree_int_cst_compare (DR_STEP (dr0_info->dr),
						size_zero_node) < 0;
          if (!npeel)
            {
              /* Since it's known at compile time, compute the number of
                 iterations in the peeled loop (the peeling factor) for use in
                 updating DR_MISALIGNMENT values.  The peeling factor is the
                 vectorization factor minus the misalignment as an element
                 count.  */
	      mis = (negative
		     ? DR_MISALIGNMENT (dr0_info)
		     : -DR_MISALIGNMENT (dr0_info));
	      unsigned int target_align = DR_TARGET_ALIGNMENT (dr0_info);
	      npeel = ((mis & (target_align - 1))
		       / vect_get_scalar_dr_size (dr0_info));
            }

	  /* For interleaved data access every iteration accesses all the
	     members of the group, therefore we divide the number of iterations
	     by the group size.  */
	  if (STMT_VINFO_GROUPED_ACCESS (stmt_info))
	    npeel /= DR_GROUP_SIZE (stmt_info);

          if (dump_enabled_p ())
            dump_printf_loc (MSG_NOTE, vect_location,
                             "Try peeling by %d\n", npeel);
        }

      /* Ensure that all datarefs can be vectorized after the peel.  */
      if (!vect_peeling_supportable (loop_vinfo, dr0_info, npeel))
	do_peeling = false;

      /* Check if all datarefs are supportable and log.  */
      if (do_peeling && known_alignment_for_access_p (dr0_info) && npeel == 0)
        {
          stat = vect_verify_datarefs_alignment (loop_vinfo);
          if (!stat)
            do_peeling = false;
          else
	    return stat;
        }

      /* Cost model #1 - honor --param vect-max-peeling-for-alignment.  */
      if (do_peeling)
        {
          unsigned max_allowed_peel
            = PARAM_VALUE (PARAM_VECT_MAX_PEELING_FOR_ALIGNMENT);
          if (max_allowed_peel != (unsigned)-1)
            {
              unsigned max_peel = npeel;
              if (max_peel == 0)
                {
		  unsigned int target_align = DR_TARGET_ALIGNMENT (dr0_info);
		  max_peel = (target_align
			      / vect_get_scalar_dr_size (dr0_info) - 1);
                }
              if (max_peel > max_allowed_peel)
                {
                  do_peeling = false;
                  if (dump_enabled_p ())
                    dump_printf_loc (MSG_NOTE, vect_location,
                        "Disable peeling, max peels reached: %d\n", max_peel);
                }
            }
        }

      /* Cost model #2 - if peeling may result in a remaining loop not
	 iterating enough to be vectorized then do not peel.  Since this
	 is a cost heuristic rather than a correctness decision, use the
	 most likely runtime value for variable vectorization factors.  */
      if (do_peeling
	  && LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo))
	{
	  unsigned int assumed_vf = vect_vf_for_cost (loop_vinfo);
	  unsigned int max_peel = npeel == 0 ? assumed_vf - 1 : npeel;
	  if ((unsigned HOST_WIDE_INT) LOOP_VINFO_INT_NITERS (loop_vinfo)
	      < assumed_vf + max_peel)
	    do_peeling = false;
	}

      if (do_peeling)
        {
          /* (1.2) Update the DR_MISALIGNMENT of each data reference DR_i.
             If the misalignment of DR_i is identical to that of dr0 then set
             DR_MISALIGNMENT (DR_i) to zero.  If the misalignment of DR_i and
             dr0 are known at compile time then increment DR_MISALIGNMENT (DR_i)
             by the peeling factor times the element size of DR_i (MOD the
             vectorization factor times the size).  Otherwise, the
             misalignment of DR_i must be set to unknown.  */
	  FOR_EACH_VEC_ELT (datarefs, i, dr)
	    if (dr != dr0_info->dr)
	      {
		/* Strided accesses perform only component accesses, alignment
		   is irrelevant for them.  */
		dr_vec_info *dr_info = loop_vinfo->lookup_dr (dr);
		stmt_info = dr_info->stmt;
		if (STMT_VINFO_STRIDED_P (stmt_info)
		    && !STMT_VINFO_GROUPED_ACCESS (stmt_info))
		  continue;

		vect_update_misalignment_for_peel (dr_info, dr0_info, npeel);
	      }

          LOOP_VINFO_UNALIGNED_DR (loop_vinfo) = dr0_info;
          if (npeel)
            LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo) = npeel;
          else
            LOOP_VINFO_PEELING_FOR_ALIGNMENT (loop_vinfo)
	      = DR_MISALIGNMENT (dr0_info);
	  SET_DR_MISALIGNMENT (dr0_info, 0);
	  if (dump_enabled_p ())
            {
              dump_printf_loc (MSG_NOTE, vect_location,
                               "Alignment of access forced using peeling.\n");
              dump_printf_loc (MSG_NOTE, vect_location,
                               "Peeling for alignment will be applied.\n");
            }

	  /* The inside-loop cost will be accounted for in vectorizable_load
	     and vectorizable_store correctly with adjusted alignments.
	     Drop the body_cst_vec on the floor here.  */
	  stat = vect_verify_datarefs_alignment (loop_vinfo);
	  gcc_assert (stat);
          return stat;
        }
    }

  /* (2) Versioning to force alignment.  */

  /* Try versioning if:
     1) optimize loop for speed
     2) there is at least one unsupported misaligned data ref with an unknown
        misalignment, and
     3) all misaligned data refs with a known misalignment are supported, and
     4) the number of runtime alignment checks is within reason.  */

  do_versioning =
	optimize_loop_nest_for_speed_p (loop)
	&& (!loop->inner); /* FORNOW */

  if (do_versioning)
    {
      FOR_EACH_VEC_ELT (datarefs, i, dr)
        {
	  dr_vec_info *dr_info = loop_vinfo->lookup_dr (dr);
	  stmt_vec_info stmt_info = dr_info->stmt;

	  /* For interleaving, only the alignment of the first access
	     matters.  */
	  if (aligned_access_p (dr_info)
	      || (STMT_VINFO_GROUPED_ACCESS (stmt_info)
		  && DR_GROUP_FIRST_ELEMENT (stmt_info) != stmt_info))
	    continue;

	  if (STMT_VINFO_STRIDED_P (stmt_info))
	    {
	      /* Strided loads perform only component accesses, alignment is
		 irrelevant for them.  */
	      if (!STMT_VINFO_GROUPED_ACCESS (stmt_info))
		continue;
	      do_versioning = false;
	      break;
	    }

	  supportable_dr_alignment
	    = vect_supportable_dr_alignment (dr_info, false);

          if (!supportable_dr_alignment)
            {
              int mask;
              tree vectype;

              if (known_alignment_for_access_p (dr_info)
                  || LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo).length ()
                     >= (unsigned) PARAM_VALUE (PARAM_VECT_MAX_VERSION_FOR_ALIGNMENT_CHECKS))
                {
                  do_versioning = false;
                  break;
                }

	      vectype = STMT_VINFO_VECTYPE (stmt_info);
	      gcc_assert (vectype);

	      /* At present we don't support versioning for alignment
		 with variable VF, since there's no guarantee that the
		 VF is a power of two.  We could relax this if we added
		 a way of enforcing a power-of-two size.  */
	      unsigned HOST_WIDE_INT size;
	      if (!GET_MODE_SIZE (TYPE_MODE (vectype)).is_constant (&size))
		{
		  do_versioning = false;
		  break;
		}

              /* The rightmost bits of an aligned address must be zeros.
                 Construct the mask needed for this test.  For example,
                 GET_MODE_SIZE for the vector mode V4SI is 16 bytes so the
                 mask must be 15 = 0xf. */
	      mask = size - 1;

              /* FORNOW: use the same mask to test all potentially unaligned
                 references in the loop.  The vectorizer currently supports
                 a single vector size, see the reference to
                 GET_MODE_NUNITS (TYPE_MODE (vectype)) where the
                 vectorization factor is computed.  */
              gcc_assert (!LOOP_VINFO_PTR_MASK (loop_vinfo)
                          || LOOP_VINFO_PTR_MASK (loop_vinfo) == mask);
              LOOP_VINFO_PTR_MASK (loop_vinfo) = mask;
	      LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo).safe_push (stmt_info);
            }
        }

      /* Versioning requires at least one misaligned data reference.  */
      if (!LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo))
        do_versioning = false;
      else if (!do_versioning)
        LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo).truncate (0);
    }

  if (do_versioning)
    {
      vec<stmt_vec_info> may_misalign_stmts
        = LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo);
      stmt_vec_info stmt_info;

      /* It can now be assumed that the data references in the statements
         in LOOP_VINFO_MAY_MISALIGN_STMTS will be aligned in the version
         of the loop being vectorized.  */
      FOR_EACH_VEC_ELT (may_misalign_stmts, i, stmt_info)
        {
	  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info);
	  SET_DR_MISALIGNMENT (dr_info, 0);
	  if (dump_enabled_p ())
            dump_printf_loc (MSG_NOTE, vect_location,
                             "Alignment of access forced using versioning.\n");
        }

      if (dump_enabled_p ())
        dump_printf_loc (MSG_NOTE, vect_location,
                         "Versioning for alignment will be applied.\n");

      /* Peeling and versioning can't be done together at this time.  */
      gcc_assert (! (do_peeling && do_versioning));

      stat = vect_verify_datarefs_alignment (loop_vinfo);
      gcc_assert (stat);
      return stat;
    }

  /* This point is reached if neither peeling nor versioning is being done.  */
  gcc_assert (! (do_peeling || do_versioning));

  stat = vect_verify_datarefs_alignment (loop_vinfo);
  return stat;
}


/* Function vect_find_same_alignment_drs.

   Update group and alignment relations in VINFO according to the chosen
   vectorization factor.  */

static void
vect_find_same_alignment_drs (vec_info *vinfo, data_dependence_relation *ddr)
{
  struct data_reference *dra = DDR_A (ddr);
  struct data_reference *drb = DDR_B (ddr);
  dr_vec_info *dr_info_a = vinfo->lookup_dr (dra);
  dr_vec_info *dr_info_b = vinfo->lookup_dr (drb);
  stmt_vec_info stmtinfo_a = dr_info_a->stmt;
  stmt_vec_info stmtinfo_b = dr_info_b->stmt;

  if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    return;

  if (dra == drb)
    return;

  if (STMT_VINFO_GATHER_SCATTER_P (stmtinfo_a)
      || STMT_VINFO_GATHER_SCATTER_P (stmtinfo_b))
    return;

  if (!operand_equal_p (DR_BASE_ADDRESS (dra), DR_BASE_ADDRESS (drb), 0)
      || !operand_equal_p (DR_OFFSET (dra), DR_OFFSET (drb), 0)
      || !operand_equal_p (DR_STEP (dra), DR_STEP (drb), 0))
    return;

  /* Two references with distance zero have the same alignment.  */
  poly_offset_int diff = (wi::to_poly_offset (DR_INIT (dra))
			  - wi::to_poly_offset (DR_INIT (drb)));
  if (maybe_ne (diff, 0))
    {
      /* Get the wider of the two alignments.  */
      unsigned int align_a = (vect_calculate_target_alignment (dr_info_a)
			      / BITS_PER_UNIT);
      unsigned int align_b = (vect_calculate_target_alignment (dr_info_b)
			      / BITS_PER_UNIT);
      unsigned int max_align = MAX (align_a, align_b);

      /* Require the gap to be a multiple of the larger vector alignment.  */
      if (!multiple_p (diff, max_align))
	return;
    }

  STMT_VINFO_SAME_ALIGN_REFS (stmtinfo_a).safe_push (drb);
  STMT_VINFO_SAME_ALIGN_REFS (stmtinfo_b).safe_push (dra);
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "accesses have the same alignment: ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dra));
      dump_printf (MSG_NOTE,  " and ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (drb));
      dump_printf (MSG_NOTE, "\n");
    }
}


/* Function vect_analyze_data_refs_alignment

   Analyze the alignment of the data-references in the loop.
   Return FALSE if a data reference is found that cannot be vectorized.  */

bool
vect_analyze_data_refs_alignment (loop_vec_info vinfo)
{
  DUMP_VECT_SCOPE ("vect_analyze_data_refs_alignment");

  /* Mark groups of data references with same alignment using
     data dependence information.  */
  vec<ddr_p> ddrs = vinfo->shared->ddrs;
  struct data_dependence_relation *ddr;
  unsigned int i;

  FOR_EACH_VEC_ELT (ddrs, i, ddr)
    vect_find_same_alignment_drs (vinfo, ddr);

  vec<data_reference_p> datarefs = vinfo->shared->datarefs;
  struct data_reference *dr;

  vect_record_base_alignments (vinfo);
  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      dr_vec_info *dr_info = vinfo->lookup_dr (dr);
      if (STMT_VINFO_VECTORIZABLE (dr_info->stmt))
	vect_compute_data_ref_alignment (dr_info);
    }

  return true;
}


/* Analyze alignment of DRs of stmts in NODE.  */

static bool
vect_slp_analyze_and_verify_node_alignment (slp_tree node)
{
  /* We vectorize from the first scalar stmt in the node unless
     the node is permuted in which case we start from the first
     element in the group.  */
  stmt_vec_info first_stmt_info = SLP_TREE_SCALAR_STMTS (node)[0];
  dr_vec_info *first_dr_info = STMT_VINFO_DR_INFO (first_stmt_info);
  if (SLP_TREE_LOAD_PERMUTATION (node).exists ())
    first_stmt_info = DR_GROUP_FIRST_ELEMENT (first_stmt_info);

  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (first_stmt_info);
  vect_compute_data_ref_alignment (dr_info);
  /* For creating the data-ref pointer we need alignment of the
     first element anyway.  */
  if (dr_info != first_dr_info)
    vect_compute_data_ref_alignment (first_dr_info);
  if (! verify_data_ref_alignment (dr_info))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: bad data alignment in basic "
			 "block.\n");
      return false;
    }

  return true;
}

/* Function vect_slp_analyze_instance_alignment

   Analyze the alignment of the data-references in the SLP instance.
   Return FALSE if a data reference is found that cannot be vectorized.  */

bool
vect_slp_analyze_and_verify_instance_alignment (slp_instance instance)
{
  DUMP_VECT_SCOPE ("vect_slp_analyze_and_verify_instance_alignment");

  slp_tree node;
  unsigned i;
  FOR_EACH_VEC_ELT (SLP_INSTANCE_LOADS (instance), i, node)
    if (! vect_slp_analyze_and_verify_node_alignment (node))
      return false;

  node = SLP_INSTANCE_TREE (instance);
  if (STMT_VINFO_DATA_REF (SLP_TREE_SCALAR_STMTS (node)[0])
      && ! vect_slp_analyze_and_verify_node_alignment
	     (SLP_INSTANCE_TREE (instance)))
    return false;

  return true;
}


/* Analyze groups of accesses: check that DR_INFO belongs to a group of
   accesses of legal size, step, etc.  Detect gaps, single element
   interleaving, and other special cases. Set grouped access info.
   Collect groups of strided stores for further use in SLP analysis.
   Worker for vect_analyze_group_access.  */

static bool
vect_analyze_group_access_1 (dr_vec_info *dr_info)
{
  data_reference *dr = dr_info->dr;
  tree step = DR_STEP (dr);
  tree scalar_type = TREE_TYPE (DR_REF (dr));
  HOST_WIDE_INT type_size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (scalar_type));
  stmt_vec_info stmt_info = dr_info->stmt;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  HOST_WIDE_INT dr_step = -1;
  HOST_WIDE_INT groupsize, last_accessed_element = 1;
  bool slp_impossible = false;

  /* For interleaving, GROUPSIZE is STEP counted in elements, i.e., the
     size of the interleaving group (including gaps).  */
  if (tree_fits_shwi_p (step))
    {
      dr_step = tree_to_shwi (step);
      /* Check that STEP is a multiple of type size.  Otherwise there is
         a non-element-sized gap at the end of the group which we
	 cannot represent in DR_GROUP_GAP or DR_GROUP_SIZE.
	 ???  As we can handle non-constant step fine here we should
	 simply remove uses of DR_GROUP_GAP between the last and first
	 element and instead rely on DR_STEP.  DR_GROUP_SIZE then would
	 simply not include that gap.  */
      if ((dr_step % type_size) != 0)
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
	                       "Step ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, step);
	      dump_printf (MSG_NOTE,
			   " is not a multiple of the element size for ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dr));
	      dump_printf (MSG_NOTE, "\n");
	    }
	  return false;
	}
      groupsize = absu_hwi (dr_step) / type_size;
    }
  else
    groupsize = 0;

  /* Not consecutive access is possible only if it is a part of interleaving.  */
  if (!DR_GROUP_FIRST_ELEMENT (stmt_info))
    {
      /* Check if it this DR is a part of interleaving, and is a single
	 element of the group that is accessed in the loop.  */

      /* Gaps are supported only for loads. STEP must be a multiple of the type
	 size.  */
      if (DR_IS_READ (dr)
	  && (dr_step % type_size) == 0
	  && groupsize > 0)
	{
	  DR_GROUP_FIRST_ELEMENT (stmt_info) = stmt_info;
	  DR_GROUP_SIZE (stmt_info) = groupsize;
	  DR_GROUP_GAP (stmt_info) = groupsize - 1;
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
	                       "Detected single element interleaving ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dr));
	      dump_printf (MSG_NOTE, " step ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, step);
	      dump_printf (MSG_NOTE, "\n");
	    }

	  return true;
	}

      if (dump_enabled_p ())
        {
 	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
	                   "not consecutive access ");
	  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
			    stmt_info->stmt, 0);
        }

      if (bb_vinfo)
	{
	  /* Mark the statement as unvectorizable.  */
	  STMT_VINFO_VECTORIZABLE (stmt_info) = false;
	  return true;
	}

      dump_printf_loc (MSG_NOTE, vect_location, "using strided accesses\n");
      STMT_VINFO_STRIDED_P (stmt_info) = true;
      return true;
    }

  if (DR_GROUP_FIRST_ELEMENT (stmt_info) == stmt_info)
    {
      /* First stmt in the interleaving chain. Check the chain.  */
      stmt_vec_info next = DR_GROUP_NEXT_ELEMENT (stmt_info);
      struct data_reference *data_ref = dr;
      unsigned int count = 1;
      tree prev_init = DR_INIT (data_ref);
      stmt_vec_info prev = stmt_info;
      HOST_WIDE_INT diff, gaps = 0;

      /* By construction, all group members have INTEGER_CST DR_INITs.  */
      while (next)
        {
          /* Skip same data-refs.  In case that two or more stmts share
             data-ref (supported only for loads), we vectorize only the first
             stmt, and the rest get their vectorized loads from the first
             one.  */
          if (!tree_int_cst_compare (DR_INIT (data_ref),
				     DR_INIT (STMT_VINFO_DATA_REF (next))))
            {
              if (DR_IS_WRITE (data_ref))
                {
                  if (dump_enabled_p ())
                    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                     "Two store stmts share the same dr.\n");
                  return false;
                }

	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "Two or more load stmts share the same dr.\n");

	      /* For load use the same data-ref load.  */
	      DR_GROUP_SAME_DR_STMT (next) = prev;

	      prev = next;
	      next = DR_GROUP_NEXT_ELEMENT (next);
	      continue;
            }

	  prev = next;
	  data_ref = STMT_VINFO_DATA_REF (next);

	  /* All group members have the same STEP by construction.  */
	  gcc_checking_assert (operand_equal_p (DR_STEP (data_ref), step, 0));

          /* Check that the distance between two accesses is equal to the type
             size. Otherwise, we have gaps.  */
          diff = (TREE_INT_CST_LOW (DR_INIT (data_ref))
                  - TREE_INT_CST_LOW (prev_init)) / type_size;
	  if (diff != 1)
	    {
	      /* FORNOW: SLP of accesses with gaps is not supported.  */
	      slp_impossible = true;
	      if (DR_IS_WRITE (data_ref))
		{
                  if (dump_enabled_p ())
                    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                     "interleaved store with gaps\n");
		  return false;
		}

              gaps += diff - 1;
	    }

	  last_accessed_element += diff;

          /* Store the gap from the previous member of the group. If there is no
             gap in the access, DR_GROUP_GAP is always 1.  */
	  DR_GROUP_GAP (next) = diff;

	  prev_init = DR_INIT (data_ref);
	  next = DR_GROUP_NEXT_ELEMENT (next);
	  /* Count the number of data-refs in the chain.  */
	  count++;
        }

      if (groupsize == 0)
        groupsize = count + gaps;

      /* This could be UINT_MAX but as we are generating code in a very
         inefficient way we have to cap earlier.  See PR78699 for example.  */
      if (groupsize > 4096)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "group is too large\n");
	  return false;
	}

      /* Check that the size of the interleaving is equal to count for stores,
         i.e., that there are no gaps.  */
      if (groupsize != count
	  && !DR_IS_READ (dr))
        {
	  groupsize = count;
	  STMT_VINFO_STRIDED_P (stmt_info) = true;
	}

      /* If there is a gap after the last load in the group it is the
	 difference between the groupsize and the last accessed
	 element.
	 When there is no gap, this difference should be 0.  */
      DR_GROUP_GAP (stmt_info) = groupsize - last_accessed_element;

      DR_GROUP_SIZE (stmt_info) = groupsize;
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_NOTE, vect_location,
			   "Detected interleaving ");
	  if (DR_IS_READ (dr))
	    dump_printf (MSG_NOTE, "load ");
	  else if (STMT_VINFO_STRIDED_P (stmt_info))
	    dump_printf (MSG_NOTE, "strided store ");
	  else
	    dump_printf (MSG_NOTE, "store ");
	  dump_printf (MSG_NOTE, "of size %u starting with ",
		       (unsigned)groupsize);
	  dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt_info->stmt, 0);
	  if (DR_GROUP_GAP (stmt_info) != 0)
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "There is a gap of %u elements after the group\n",
			     DR_GROUP_GAP (stmt_info));
	}

      /* SLP: create an SLP data structure for every interleaving group of
	 stores for further analysis in vect_analyse_slp.  */
      if (DR_IS_WRITE (dr) && !slp_impossible)
	{
	  if (loop_vinfo)
	    LOOP_VINFO_GROUPED_STORES (loop_vinfo).safe_push (stmt_info);
	  if (bb_vinfo)
	    BB_VINFO_GROUPED_STORES (bb_vinfo).safe_push (stmt_info);
	}
    }

  return true;
}

/* Analyze groups of accesses: check that DR_INFO belongs to a group of
   accesses of legal size, step, etc.  Detect gaps, single element
   interleaving, and other special cases. Set grouped access info.
   Collect groups of strided stores for further use in SLP analysis.  */

static bool
vect_analyze_group_access (dr_vec_info *dr_info)
{
  if (!vect_analyze_group_access_1 (dr_info))
    {
      /* Dissolve the group if present.  */
      stmt_vec_info stmt_info = DR_GROUP_FIRST_ELEMENT (dr_info->stmt);
      while (stmt_info)
	{
	  stmt_vec_info next = DR_GROUP_NEXT_ELEMENT (stmt_info);
	  DR_GROUP_FIRST_ELEMENT (stmt_info) = NULL;
	  DR_GROUP_NEXT_ELEMENT (stmt_info) = NULL;
	  stmt_info = next;
	}
      return false;
    }
  return true;
}

/* Analyze the access pattern of the data-reference DR_INFO.
   In case of non-consecutive accesses call vect_analyze_group_access() to
   analyze groups of accesses.  */

static bool
vect_analyze_data_ref_access (dr_vec_info *dr_info)
{
  data_reference *dr = dr_info->dr;
  tree step = DR_STEP (dr);
  tree scalar_type = TREE_TYPE (DR_REF (dr));
  stmt_vec_info stmt_info = dr_info->stmt;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = NULL;

  if (STMT_VINFO_GATHER_SCATTER_P (stmt_info))
    return true;

  if (loop_vinfo)
    loop = LOOP_VINFO_LOOP (loop_vinfo);

  if (loop_vinfo && !step)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
	                 "bad data-ref access in loop\n");
      return false;
    }

  /* Allow loads with zero step in inner-loop vectorization.  */
  if (loop_vinfo && integer_zerop (step))
    {
      DR_GROUP_FIRST_ELEMENT (stmt_info) = NULL;
      if (!nested_in_vect_loop_p (loop, stmt_info))
	return DR_IS_READ (dr);
      /* Allow references with zero step for outer loops marked
	 with pragma omp simd only - it guarantees absence of
	 loop-carried dependencies between inner loop iterations.  */
      if (loop->safelen < 2)
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
			     "zero step in inner loop of nest\n");
	  return false;
	}
    }

  if (loop && nested_in_vect_loop_p (loop, stmt_info))
    {
      /* Interleaved accesses are not yet supported within outer-loop
        vectorization for references in the inner-loop.  */
      DR_GROUP_FIRST_ELEMENT (stmt_info) = NULL;

      /* For the rest of the analysis we use the outer-loop step.  */
      step = STMT_VINFO_DR_STEP (stmt_info);
      if (integer_zerop (step))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_NOTE, vect_location,
	                     "zero step in outer loop.\n");
	  return DR_IS_READ (dr);
	}
    }

  /* Consecutive?  */
  if (TREE_CODE (step) == INTEGER_CST)
    {
      HOST_WIDE_INT dr_step = TREE_INT_CST_LOW (step);
      if (!tree_int_cst_compare (step, TYPE_SIZE_UNIT (scalar_type))
	  || (dr_step < 0
	      && !compare_tree_int (TYPE_SIZE_UNIT (scalar_type), -dr_step)))
	{
	  /* Mark that it is not interleaving.  */
	  DR_GROUP_FIRST_ELEMENT (stmt_info) = NULL;
	  return true;
	}
    }

  if (loop && nested_in_vect_loop_p (loop, stmt_info))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
	                 "grouped access in outer loop.\n");
      return false;
    }


  /* Assume this is a DR handled by non-constant strided load case.  */
  if (TREE_CODE (step) != INTEGER_CST)
    return (STMT_VINFO_STRIDED_P (stmt_info)
	    && (!STMT_VINFO_GROUPED_ACCESS (stmt_info)
		|| vect_analyze_group_access (dr_info)));

  /* Not consecutive access - check if it's a part of interleaving group.  */
  return vect_analyze_group_access (dr_info);
}

/* Compare two data-references DRA and DRB to group them into chunks
   suitable for grouping.  */

static int
dr_group_sort_cmp (const void *dra_, const void *drb_)
{
  data_reference_p dra = *(data_reference_p *)const_cast<void *>(dra_);
  data_reference_p drb = *(data_reference_p *)const_cast<void *>(drb_);
  int cmp;

  /* Stabilize sort.  */
  if (dra == drb)
    return 0;

  /* DRs in different loops never belong to the same group.  */
  loop_p loopa = gimple_bb (DR_STMT (dra))->loop_father;
  loop_p loopb = gimple_bb (DR_STMT (drb))->loop_father;
  if (loopa != loopb)
    return loopa->num < loopb->num ? -1 : 1;

  /* Ordering of DRs according to base.  */
  cmp = data_ref_compare_tree (DR_BASE_ADDRESS (dra),
			       DR_BASE_ADDRESS (drb));
  if (cmp != 0)
    return cmp;

  /* And according to DR_OFFSET.  */
  cmp = data_ref_compare_tree (DR_OFFSET (dra), DR_OFFSET (drb));
  if (cmp != 0)
    return cmp;

  /* Put reads before writes.  */
  if (DR_IS_READ (dra) != DR_IS_READ (drb))
    return DR_IS_READ (dra) ? -1 : 1;

  /* Then sort after access size.  */
  cmp = data_ref_compare_tree (TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dra))),
			       TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (drb))));
  if (cmp != 0)
    return cmp;

  /* And after step.  */
  cmp = data_ref_compare_tree (DR_STEP (dra), DR_STEP (drb));
  if (cmp != 0)
    return cmp;

  /* Then sort after DR_INIT.  In case of identical DRs sort after stmt UID.  */
  cmp = data_ref_compare_tree (DR_INIT (dra), DR_INIT (drb));
  if (cmp == 0)
    return gimple_uid (DR_STMT (dra)) < gimple_uid (DR_STMT (drb)) ? -1 : 1;
  return cmp;
}

/* If OP is the result of a conversion, return the unconverted value,
   otherwise return null.  */

static tree
strip_conversion (tree op)
{
  if (TREE_CODE (op) != SSA_NAME)
    return NULL_TREE;
  gimple *stmt = SSA_NAME_DEF_STMT (op);
  if (!is_gimple_assign (stmt)
      || !CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (stmt)))
    return NULL_TREE;
  return gimple_assign_rhs1 (stmt);
}

/* Return true if vectorizable_* routines can handle statements STMT1_INFO
   and STMT2_INFO being in a single group.  */

static bool
can_group_stmts_p (stmt_vec_info stmt1_info, stmt_vec_info stmt2_info)
{
  if (gimple_assign_single_p (stmt1_info->stmt))
    return gimple_assign_single_p (stmt2_info->stmt);

  gcall *call1 = dyn_cast <gcall *> (stmt1_info->stmt);
  if (call1 && gimple_call_internal_p (call1))
    {
      /* Check for two masked loads or two masked stores.  */
      gcall *call2 = dyn_cast <gcall *> (stmt2_info->stmt);
      if (!call2 || !gimple_call_internal_p (call2))
	return false;
      internal_fn ifn = gimple_call_internal_fn (call1);
      if (ifn != IFN_MASK_LOAD && ifn != IFN_MASK_STORE)
	return false;
      if (ifn != gimple_call_internal_fn (call2))
	return false;

      /* Check that the masks are the same.  Cope with casts of masks,
	 like those created by build_mask_conversion.  */
      tree mask1 = gimple_call_arg (call1, 2);
      tree mask2 = gimple_call_arg (call2, 2);
      if (!operand_equal_p (mask1, mask2, 0))
	{
	  mask1 = strip_conversion (mask1);
	  if (!mask1)
	    return false;
	  mask2 = strip_conversion (mask2);
	  if (!mask2)
	    return false;
	  if (!operand_equal_p (mask1, mask2, 0))
	    return false;
	}
      return true;
    }

  return false;
}

/* Function vect_analyze_data_ref_accesses.

   Analyze the access pattern of all the data references in the loop.

   FORNOW: the only access pattern that is considered vectorizable is a
	   simple step 1 (consecutive) access.

   FORNOW: handle only arrays and pointer accesses.  */

bool
vect_analyze_data_ref_accesses (vec_info *vinfo)
{
  unsigned int i;
  vec<data_reference_p> datarefs = vinfo->shared->datarefs;
  struct data_reference *dr;

  DUMP_VECT_SCOPE ("vect_analyze_data_ref_accesses");

  if (datarefs.is_empty ())
    return true;

  /* Sort the array of datarefs to make building the interleaving chains
     linear.  Don't modify the original vector's order, it is needed for
     determining what dependencies are reversed.  */
  vec<data_reference_p> datarefs_copy = datarefs.copy ();
  datarefs_copy.qsort (dr_group_sort_cmp);

  /* Build the interleaving chains.  */
  for (i = 0; i < datarefs_copy.length () - 1;)
    {
      data_reference_p dra = datarefs_copy[i];
      dr_vec_info *dr_info_a = vinfo->lookup_dr (dra);
      stmt_vec_info stmtinfo_a = dr_info_a->stmt;
      stmt_vec_info lastinfo = NULL;
      if (!STMT_VINFO_VECTORIZABLE (stmtinfo_a)
	  || STMT_VINFO_GATHER_SCATTER_P (stmtinfo_a))
	{
	  ++i;
	  continue;
	}
      for (i = i + 1; i < datarefs_copy.length (); ++i)
	{
	  data_reference_p drb = datarefs_copy[i];
	  dr_vec_info *dr_info_b = vinfo->lookup_dr (drb);
	  stmt_vec_info stmtinfo_b = dr_info_b->stmt;
	  if (!STMT_VINFO_VECTORIZABLE (stmtinfo_b)
	      || STMT_VINFO_GATHER_SCATTER_P (stmtinfo_b))
	    break;

	  /* ???  Imperfect sorting (non-compatible types, non-modulo
	     accesses, same accesses) can lead to a group to be artificially
	     split here as we don't just skip over those.  If it really
	     matters we can push those to a worklist and re-iterate
	     over them.  The we can just skip ahead to the next DR here.  */

	  /* DRs in a different loop should not be put into the same
	     interleaving group.  */
	  if (gimple_bb (DR_STMT (dra))->loop_father
	      != gimple_bb (DR_STMT (drb))->loop_father)
	    break;

	  /* Check that the data-refs have same first location (except init)
	     and they are both either store or load (not load and store,
	     not masked loads or stores).  */
	  if (DR_IS_READ (dra) != DR_IS_READ (drb)
	      || data_ref_compare_tree (DR_BASE_ADDRESS (dra),
					DR_BASE_ADDRESS (drb)) != 0
	      || data_ref_compare_tree (DR_OFFSET (dra), DR_OFFSET (drb)) != 0
	      || !can_group_stmts_p (stmtinfo_a, stmtinfo_b))
	    break;

	  /* Check that the data-refs have the same constant size.  */
	  tree sza = TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dra)));
	  tree szb = TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (drb)));
	  if (!tree_fits_uhwi_p (sza)
	      || !tree_fits_uhwi_p (szb)
	      || !tree_int_cst_equal (sza, szb))
	    break;

	  /* Check that the data-refs have the same step.  */
	  if (data_ref_compare_tree (DR_STEP (dra), DR_STEP (drb)) != 0)
	    break;

	  /* Check the types are compatible.
	     ???  We don't distinguish this during sorting.  */
	  if (!types_compatible_p (TREE_TYPE (DR_REF (dra)),
				   TREE_TYPE (DR_REF (drb))))
	    break;

	  /* Check that the DR_INITs are compile-time constants.  */
	  if (TREE_CODE (DR_INIT (dra)) != INTEGER_CST
	      || TREE_CODE (DR_INIT (drb)) != INTEGER_CST)
	    break;

	  /* Sorting has ensured that DR_INIT (dra) <= DR_INIT (drb).  */
	  HOST_WIDE_INT init_a = TREE_INT_CST_LOW (DR_INIT (dra));
	  HOST_WIDE_INT init_b = TREE_INT_CST_LOW (DR_INIT (drb));
	  HOST_WIDE_INT init_prev
	    = TREE_INT_CST_LOW (DR_INIT (datarefs_copy[i-1]));
	  gcc_assert (init_a <= init_b
		      && init_a <= init_prev
		      && init_prev <= init_b);

	  /* Do not place the same access in the interleaving chain twice.  */
	  if (init_b == init_prev)
	    {
	      gcc_assert (gimple_uid (DR_STMT (datarefs_copy[i-1]))
			  < gimple_uid (DR_STMT (drb)));
	      /* ???  For now we simply "drop" the later reference which is
	         otherwise the same rather than finishing off this group.
		 In the end we'd want to re-process duplicates forming
		 multiple groups from the refs, likely by just collecting
		 all candidates (including duplicates and split points
		 below) in a vector and then process them together.  */
	      continue;
	    }

	  /* If init_b == init_a + the size of the type * k, we have an
	     interleaving, and DRA is accessed before DRB.  */
	  HOST_WIDE_INT type_size_a = tree_to_uhwi (sza);
	  if (type_size_a == 0
	      || (init_b - init_a) % type_size_a != 0)
	    break;

	  /* If we have a store, the accesses are adjacent.  This splits
	     groups into chunks we support (we don't support vectorization
	     of stores with gaps).  */
	  if (!DR_IS_READ (dra) && init_b - init_prev != type_size_a)
	    break;

	  /* If the step (if not zero or non-constant) is greater than the
	     difference between data-refs' inits this splits groups into
	     suitable sizes.  */
	  if (tree_fits_shwi_p (DR_STEP (dra)))
	    {
	      HOST_WIDE_INT step = tree_to_shwi (DR_STEP (dra));
	      if (step != 0 && step <= (init_b - init_a))
		break;
	    }

	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "Detected interleaving ");
	      if (DR_IS_READ (dra))
		dump_printf (MSG_NOTE, "load ");
	      else
		dump_printf (MSG_NOTE, "store ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dra));
	      dump_printf (MSG_NOTE,  " and ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (drb));
	      dump_printf (MSG_NOTE, "\n");
	    }

	  /* Link the found element into the group list.  */
	  if (!DR_GROUP_FIRST_ELEMENT (stmtinfo_a))
	    {
	      DR_GROUP_FIRST_ELEMENT (stmtinfo_a) = stmtinfo_a;
	      lastinfo = stmtinfo_a;
	    }
	  DR_GROUP_FIRST_ELEMENT (stmtinfo_b) = stmtinfo_a;
	  DR_GROUP_NEXT_ELEMENT (lastinfo) = stmtinfo_b;
	  lastinfo = stmtinfo_b;
	}
    }

  FOR_EACH_VEC_ELT (datarefs_copy, i, dr)
    {
      dr_vec_info *dr_info = vinfo->lookup_dr (dr);
      if (STMT_VINFO_VECTORIZABLE (dr_info->stmt)
	  && !vect_analyze_data_ref_access (dr_info))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "not vectorized: complicated access pattern.\n");

	  if (is_a <bb_vec_info> (vinfo))
	    {
	      /* Mark the statement as not vectorizable.  */
	      STMT_VINFO_VECTORIZABLE (dr_info->stmt) = false;
	      continue;
	    }
	  else
	    {
	      datarefs_copy.release ();
	      return false;
	    }
	}
    }

  datarefs_copy.release ();
  return true;
}

/* Function vect_vfa_segment_size.

   Input:
     DR_INFO: The data reference.
     LENGTH_FACTOR: segment length to consider.

   Return a value suitable for the dr_with_seg_len::seg_len field.
   This is the "distance travelled" by the pointer from the first
   iteration in the segment to the last.  Note that it does not include
   the size of the access; in effect it only describes the first byte.  */

static tree
vect_vfa_segment_size (dr_vec_info *dr_info, tree length_factor)
{
  length_factor = size_binop (MINUS_EXPR,
			      fold_convert (sizetype, length_factor),
			      size_one_node);
  return size_binop (MULT_EXPR, fold_convert (sizetype, DR_STEP (dr_info->dr)),
		     length_factor);
}

/* Return a value that, when added to abs (vect_vfa_segment_size (DR_INFO)),
   gives the worst-case number of bytes covered by the segment.  */

static unsigned HOST_WIDE_INT
vect_vfa_access_size (dr_vec_info *dr_info)
{
  stmt_vec_info stmt_vinfo = dr_info->stmt;
  tree ref_type = TREE_TYPE (DR_REF (dr_info->dr));
  unsigned HOST_WIDE_INT ref_size = tree_to_uhwi (TYPE_SIZE_UNIT (ref_type));
  unsigned HOST_WIDE_INT access_size = ref_size;
  if (DR_GROUP_FIRST_ELEMENT (stmt_vinfo))
    {
      gcc_assert (DR_GROUP_FIRST_ELEMENT (stmt_vinfo) == stmt_vinfo);
      access_size *= DR_GROUP_SIZE (stmt_vinfo) - DR_GROUP_GAP (stmt_vinfo);
    }
  if (STMT_VINFO_VEC_STMT (stmt_vinfo)
      && (vect_supportable_dr_alignment (dr_info, false)
	  == dr_explicit_realign_optimized))
    {
      /* We might access a full vector's worth.  */
      tree vectype = STMT_VINFO_VECTYPE (stmt_vinfo);
      access_size += tree_to_uhwi (TYPE_SIZE_UNIT (vectype)) - ref_size;
    }
  return access_size;
}

/* Get the minimum alignment for all the scalar accesses that DR_INFO
   describes.  */

static unsigned int
vect_vfa_align (dr_vec_info *dr_info)
{
  return TYPE_ALIGN_UNIT (TREE_TYPE (DR_REF (dr_info->dr)));
}

/* Function vect_no_alias_p.

   Given data references A and B with equal base and offset, see whether
   the alias relation can be decided at compilation time.  Return 1 if
   it can and the references alias, 0 if it can and the references do
   not alias, and -1 if we cannot decide at compile time.  SEGMENT_LENGTH_A,
   SEGMENT_LENGTH_B, ACCESS_SIZE_A and ACCESS_SIZE_B are the equivalent
   of dr_with_seg_len::{seg_len,access_size} for A and B.  */

static int
vect_compile_time_alias (dr_vec_info *a, dr_vec_info *b,
			 tree segment_length_a, tree segment_length_b,
			 unsigned HOST_WIDE_INT access_size_a,
			 unsigned HOST_WIDE_INT access_size_b)
{
  poly_offset_int offset_a = wi::to_poly_offset (DR_INIT (a->dr));
  poly_offset_int offset_b = wi::to_poly_offset (DR_INIT (b->dr));
  poly_uint64 const_length_a;
  poly_uint64 const_length_b;

  /* For negative step, we need to adjust address range by TYPE_SIZE_UNIT
     bytes, e.g., int a[3] -> a[1] range is [a+4, a+16) instead of
     [a, a+12) */
  if (tree_int_cst_compare (DR_STEP (a->dr), size_zero_node) < 0)
    {
      const_length_a = (-wi::to_poly_wide (segment_length_a)).force_uhwi ();
      offset_a = (offset_a + access_size_a) - const_length_a;
    }
  else
    const_length_a = tree_to_poly_uint64 (segment_length_a);
  if (tree_int_cst_compare (DR_STEP (b->dr), size_zero_node) < 0)
    {
      const_length_b = (-wi::to_poly_wide (segment_length_b)).force_uhwi ();
      offset_b = (offset_b + access_size_b) - const_length_b;
    }
  else
    const_length_b = tree_to_poly_uint64 (segment_length_b);

  const_length_a += access_size_a;
  const_length_b += access_size_b;

  if (ranges_known_overlap_p (offset_a, const_length_a,
			      offset_b, const_length_b))
    return 1;

  if (!ranges_maybe_overlap_p (offset_a, const_length_a,
			       offset_b, const_length_b))
    return 0;

  return -1;
}

/* Return true if the minimum nonzero dependence distance for loop LOOP_DEPTH
   in DDR is >= VF.  */

static bool
dependence_distance_ge_vf (data_dependence_relation *ddr,
			   unsigned int loop_depth, poly_uint64 vf)
{
  if (DDR_ARE_DEPENDENT (ddr) != NULL_TREE
      || DDR_NUM_DIST_VECTS (ddr) == 0)
    return false;

  /* If the dependence is exact, we should have limited the VF instead.  */
  gcc_checking_assert (DDR_COULD_BE_INDEPENDENT_P (ddr));

  unsigned int i;
  lambda_vector dist_v;
  FOR_EACH_VEC_ELT (DDR_DIST_VECTS (ddr), i, dist_v)
    {
      HOST_WIDE_INT dist = dist_v[loop_depth];
      if (dist != 0
	  && !(dist > 0 && DDR_REVERSED_P (ddr))
	  && maybe_lt ((unsigned HOST_WIDE_INT) abs_hwi (dist), vf))
	return false;
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location,
		       "dependence distance between ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (DDR_A (ddr)));
      dump_printf (MSG_NOTE,  " and ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (DDR_B (ddr)));
      dump_printf (MSG_NOTE,  " is >= VF\n");
    }

  return true;
}

/* Dump LOWER_BOUND using flags DUMP_KIND.  Dumps are known to be enabled.  */

static void
dump_lower_bound (dump_flags_t dump_kind, const vec_lower_bound &lower_bound)
{
  dump_printf (dump_kind, "%s (", lower_bound.unsigned_p ? "unsigned" : "abs");
  dump_generic_expr (dump_kind, TDF_SLIM, lower_bound.expr);
  dump_printf (dump_kind, ") >= ");
  dump_dec (dump_kind, lower_bound.min_value);
}

/* Record that the vectorized loop requires the vec_lower_bound described
   by EXPR, UNSIGNED_P and MIN_VALUE.  */

static void
vect_check_lower_bound (loop_vec_info loop_vinfo, tree expr, bool unsigned_p,
			poly_uint64 min_value)
{
  vec<vec_lower_bound> lower_bounds = LOOP_VINFO_LOWER_BOUNDS (loop_vinfo);
  for (unsigned int i = 0; i < lower_bounds.length (); ++i)
    if (operand_equal_p (lower_bounds[i].expr, expr, 0))
      {
	unsigned_p &= lower_bounds[i].unsigned_p;
	min_value = upper_bound (lower_bounds[i].min_value, min_value);
	if (lower_bounds[i].unsigned_p != unsigned_p
	    || maybe_lt (lower_bounds[i].min_value, min_value))
	  {
	    lower_bounds[i].unsigned_p = unsigned_p;
	    lower_bounds[i].min_value = min_value;
	    if (dump_enabled_p ())
	      {
		dump_printf_loc (MSG_NOTE, vect_location,
				 "updating run-time check to ");
		dump_lower_bound (MSG_NOTE, lower_bounds[i]);
		dump_printf (MSG_NOTE, "\n");
	      }
	  }
	return;
      }

  vec_lower_bound lower_bound (expr, unsigned_p, min_value);
  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "need a run-time check that ");
      dump_lower_bound (MSG_NOTE, lower_bound);
      dump_printf (MSG_NOTE, "\n");
    }
  LOOP_VINFO_LOWER_BOUNDS (loop_vinfo).safe_push (lower_bound);
}

/* Return true if it's unlikely that the step of the vectorized form of DR_INFO
   will span fewer than GAP bytes.  */

static bool
vect_small_gap_p (loop_vec_info loop_vinfo, dr_vec_info *dr_info,
		  poly_int64 gap)
{
  stmt_vec_info stmt_info = dr_info->stmt;
  HOST_WIDE_INT count
    = estimated_poly_value (LOOP_VINFO_VECT_FACTOR (loop_vinfo));
  if (DR_GROUP_FIRST_ELEMENT (stmt_info))
    count *= DR_GROUP_SIZE (DR_GROUP_FIRST_ELEMENT (stmt_info));
  return (estimated_poly_value (gap)
	  <= count * vect_get_scalar_dr_size (dr_info));
}

/* Return true if we know that there is no alias between DR_INFO_A and
   DR_INFO_B when abs (DR_STEP (DR_INFO_A->dr)) >= N for some N.
   When returning true, set *LOWER_BOUND_OUT to this N.  */

static bool
vectorizable_with_step_bound_p (dr_vec_info *dr_info_a, dr_vec_info *dr_info_b,
				poly_uint64 *lower_bound_out)
{
  /* Check that there is a constant gap of known sign between DR_A
     and DR_B.  */
  data_reference *dr_a = dr_info_a->dr;
  data_reference *dr_b = dr_info_b->dr;
  poly_int64 init_a, init_b;
  if (!operand_equal_p (DR_BASE_ADDRESS (dr_a), DR_BASE_ADDRESS (dr_b), 0)
      || !operand_equal_p (DR_OFFSET (dr_a), DR_OFFSET (dr_b), 0)
      || !operand_equal_p (DR_STEP (dr_a), DR_STEP (dr_b), 0)
      || !poly_int_tree_p (DR_INIT (dr_a), &init_a)
      || !poly_int_tree_p (DR_INIT (dr_b), &init_b)
      || !ordered_p (init_a, init_b))
    return false;

  /* Sort DR_A and DR_B by the address they access.  */
  if (maybe_lt (init_b, init_a))
    {
      std::swap (init_a, init_b);
      std::swap (dr_info_a, dr_info_b);
      std::swap (dr_a, dr_b);
    }

  /* If the two accesses could be dependent within a scalar iteration,
     make sure that we'd retain their order.  */
  if (maybe_gt (init_a + vect_get_scalar_dr_size (dr_info_a), init_b)
      && !vect_preserves_scalar_order_p (dr_info_a, dr_info_b))
    return false;

  /* There is no alias if abs (DR_STEP) is greater than or equal to
     the bytes spanned by the combination of the two accesses.  */
  *lower_bound_out = init_b + vect_get_scalar_dr_size (dr_info_b) - init_a;
  return true;
}

/* Function vect_prune_runtime_alias_test_list.

   Prune a list of ddrs to be tested at run-time by versioning for alias.
   Merge several alias checks into one if possible.
   Return FALSE if resulting list of ddrs is longer then allowed by
   PARAM_VECT_MAX_VERSION_FOR_ALIAS_CHECKS, otherwise return TRUE.  */

bool
vect_prune_runtime_alias_test_list (loop_vec_info loop_vinfo)
{
  typedef pair_hash <tree_operand_hash, tree_operand_hash> tree_pair_hash;
  hash_set <tree_pair_hash> compared_objects;

  vec<ddr_p> may_alias_ddrs = LOOP_VINFO_MAY_ALIAS_DDRS (loop_vinfo);
  vec<dr_with_seg_len_pair_t> &comp_alias_ddrs
    = LOOP_VINFO_COMP_ALIAS_DDRS (loop_vinfo);
  vec<vec_object_pair> &check_unequal_addrs
    = LOOP_VINFO_CHECK_UNEQUAL_ADDRS (loop_vinfo);
  poly_uint64 vect_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  tree scalar_loop_iters = LOOP_VINFO_NITERS (loop_vinfo);

  ddr_p ddr;
  unsigned int i;
  tree length_factor;

  DUMP_VECT_SCOPE ("vect_prune_runtime_alias_test_list");

  /* Step values are irrelevant for aliasing if the number of vector
     iterations is equal to the number of scalar iterations (which can
     happen for fully-SLP loops).  */
  bool ignore_step_p = known_eq (LOOP_VINFO_VECT_FACTOR (loop_vinfo), 1U);

  if (!ignore_step_p)
    {
      /* Convert the checks for nonzero steps into bound tests.  */
      tree value;
      FOR_EACH_VEC_ELT (LOOP_VINFO_CHECK_NONZERO (loop_vinfo), i, value)
	vect_check_lower_bound (loop_vinfo, value, true, 1);
    }

  if (may_alias_ddrs.is_empty ())
    return true;

  comp_alias_ddrs.create (may_alias_ddrs.length ());

  unsigned int loop_depth
    = index_in_loop_nest (LOOP_VINFO_LOOP (loop_vinfo)->num,
			  LOOP_VINFO_LOOP_NEST (loop_vinfo));

  /* First, we collect all data ref pairs for aliasing checks.  */
  FOR_EACH_VEC_ELT (may_alias_ddrs, i, ddr)
    {
      int comp_res;
      poly_uint64 lower_bound;
      tree segment_length_a, segment_length_b;
      unsigned HOST_WIDE_INT access_size_a, access_size_b;
      unsigned int align_a, align_b;

      /* Ignore the alias if the VF we chose ended up being no greater
	 than the dependence distance.  */
      if (dependence_distance_ge_vf (ddr, loop_depth, vect_factor))
	continue;

      if (DDR_OBJECT_A (ddr))
	{
	  vec_object_pair new_pair (DDR_OBJECT_A (ddr), DDR_OBJECT_B (ddr));
	  if (!compared_objects.add (new_pair))
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_NOTE, vect_location, "checking that ");
		  dump_generic_expr (MSG_NOTE, TDF_SLIM, new_pair.first);
		  dump_printf (MSG_NOTE, " and ");
		  dump_generic_expr (MSG_NOTE, TDF_SLIM, new_pair.second);
		  dump_printf (MSG_NOTE, " have different addresses\n");
		}
	      LOOP_VINFO_CHECK_UNEQUAL_ADDRS (loop_vinfo).safe_push (new_pair);
	    }
	  continue;
	}

      dr_vec_info *dr_info_a = loop_vinfo->lookup_dr (DDR_A (ddr));
      stmt_vec_info stmt_info_a = dr_info_a->stmt;

      dr_vec_info *dr_info_b = loop_vinfo->lookup_dr (DDR_B (ddr));
      stmt_vec_info stmt_info_b = dr_info_b->stmt;

      /* Skip the pair if inter-iteration dependencies are irrelevant
	 and intra-iteration dependencies are guaranteed to be honored.  */
      if (ignore_step_p
	  && (vect_preserves_scalar_order_p (dr_info_a, dr_info_b)
	      || vectorizable_with_step_bound_p (dr_info_a, dr_info_b,
						 &lower_bound)))
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "no need for alias check between ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dr_info_a->dr));
	      dump_printf (MSG_NOTE, " and ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dr_info_b->dr));
	      dump_printf (MSG_NOTE, " when VF is 1\n");
	    }
	  continue;
	}

      /* See whether we can handle the alias using a bounds check on
	 the step, and whether that's likely to be the best approach.
	 (It might not be, for example, if the minimum step is much larger
	 than the number of bytes handled by one vector iteration.)  */
      if (!ignore_step_p
	  && TREE_CODE (DR_STEP (dr_info_a->dr)) != INTEGER_CST
	  && vectorizable_with_step_bound_p (dr_info_a, dr_info_b,
					     &lower_bound)
	  && (vect_small_gap_p (loop_vinfo, dr_info_a, lower_bound)
	      || vect_small_gap_p (loop_vinfo, dr_info_b, lower_bound)))
	{
	  bool unsigned_p = dr_known_forward_stride_p (dr_info_a->dr);
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location, "no alias between ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dr_info_a->dr));
	      dump_printf (MSG_NOTE, " and ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dr_info_b->dr));
	      dump_printf (MSG_NOTE, " when the step ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_STEP (dr_info_a->dr));
	      dump_printf (MSG_NOTE, " is outside ");
	      if (unsigned_p)
		dump_printf (MSG_NOTE, "[0");
	      else
		{
		  dump_printf (MSG_NOTE, "(");
		  dump_dec (MSG_NOTE, poly_int64 (-lower_bound));
		}
	      dump_printf (MSG_NOTE, ", ");
	      dump_dec (MSG_NOTE, lower_bound);
	      dump_printf (MSG_NOTE, ")\n");
	    }
	  vect_check_lower_bound (loop_vinfo, DR_STEP (dr_info_a->dr),
				  unsigned_p, lower_bound);
	  continue;
	}

      stmt_vec_info dr_group_first_a = DR_GROUP_FIRST_ELEMENT (stmt_info_a);
      if (dr_group_first_a)
	{
	  stmt_info_a = dr_group_first_a;
	  dr_info_a = STMT_VINFO_DR_INFO (stmt_info_a);
	}

      stmt_vec_info dr_group_first_b = DR_GROUP_FIRST_ELEMENT (stmt_info_b);
      if (dr_group_first_b)
	{
	  stmt_info_b = dr_group_first_b;
	  dr_info_b = STMT_VINFO_DR_INFO (stmt_info_b);
	}

      if (ignore_step_p)
	{
	  segment_length_a = size_zero_node;
	  segment_length_b = size_zero_node;
	}
      else
	{
	  if (!operand_equal_p (DR_STEP (dr_info_a->dr),
				DR_STEP (dr_info_b->dr), 0))
	    length_factor = scalar_loop_iters;
	  else
	    length_factor = size_int (vect_factor);
	  segment_length_a = vect_vfa_segment_size (dr_info_a, length_factor);
	  segment_length_b = vect_vfa_segment_size (dr_info_b, length_factor);
	}
      access_size_a = vect_vfa_access_size (dr_info_a);
      access_size_b = vect_vfa_access_size (dr_info_b);
      align_a = vect_vfa_align (dr_info_a);
      align_b = vect_vfa_align (dr_info_b);

      comp_res = data_ref_compare_tree (DR_BASE_ADDRESS (dr_info_a->dr),
					DR_BASE_ADDRESS (dr_info_b->dr));
      if (comp_res == 0)
	comp_res = data_ref_compare_tree (DR_OFFSET (dr_info_a->dr),
					  DR_OFFSET (dr_info_b->dr));

      /* See whether the alias is known at compilation time.  */
      if (comp_res == 0
	  && TREE_CODE (DR_STEP (dr_info_a->dr)) == INTEGER_CST
	  && TREE_CODE (DR_STEP (dr_info_b->dr)) == INTEGER_CST
	  && poly_int_tree_p (segment_length_a)
	  && poly_int_tree_p (segment_length_b))
	{
	  int res = vect_compile_time_alias (dr_info_a, dr_info_b,
					     segment_length_a,
					     segment_length_b,
					     access_size_a,
					     access_size_b);
	  if (res >= 0 && dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "can tell at compile time that ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dr_info_a->dr));
	      dump_printf (MSG_NOTE, " and ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_REF (dr_info_b->dr));
	      if (res == 0)
		dump_printf (MSG_NOTE, " do not alias\n");
	      else
		dump_printf (MSG_NOTE, " alias\n");
	    }

	  if (res == 0)
	    continue;

	  if (res == 1)
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_NOTE, vect_location,
				 "not vectorized: compilation time alias.\n");
	      return false;
	    }
	}

      dr_with_seg_len_pair_t dr_with_seg_len_pair
	(dr_with_seg_len (dr_info_a->dr, segment_length_a,
			  access_size_a, align_a),
	 dr_with_seg_len (dr_info_b->dr, segment_length_b,
			  access_size_b, align_b));

      /* Canonicalize pairs by sorting the two DR members.  */
      if (comp_res > 0)
	std::swap (dr_with_seg_len_pair.first, dr_with_seg_len_pair.second);

      comp_alias_ddrs.safe_push (dr_with_seg_len_pair);
    }

  prune_runtime_alias_test_list (&comp_alias_ddrs, vect_factor);

  unsigned int count = (comp_alias_ddrs.length ()
			+ check_unequal_addrs.length ());

  dump_printf_loc (MSG_NOTE, vect_location,
		   "improved number of alias checks from %d to %d\n",
		   may_alias_ddrs.length (), count);
  if ((int) count > PARAM_VALUE (PARAM_VECT_MAX_VERSION_FOR_ALIAS_CHECKS))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "number of versioning for alias "
			 "run-time tests exceeds %d "
			 "(--param vect-max-version-for-alias-checks)\n",
			 PARAM_VALUE (PARAM_VECT_MAX_VERSION_FOR_ALIAS_CHECKS));
      return false;
    }

  return true;
}

/* Check whether we can use an internal function for a gather load
   or scatter store.  READ_P is true for loads and false for stores.
   MASKED_P is true if the load or store is conditional.  MEMORY_TYPE is
   the type of the memory elements being loaded or stored.  OFFSET_BITS
   is the number of bits in each scalar offset and OFFSET_SIGN is the
   sign of the offset.  SCALE is the amount by which the offset should
   be multiplied *after* it has been converted to address width.

   Return true if the function is supported, storing the function
   id in *IFN_OUT and the type of a vector element in *ELEMENT_TYPE_OUT.  */

bool
vect_gather_scatter_fn_p (bool read_p, bool masked_p, tree vectype,
			  tree memory_type, unsigned int offset_bits,
			  signop offset_sign, int scale,
			  internal_fn *ifn_out, tree *element_type_out)
{
  unsigned int memory_bits = tree_to_uhwi (TYPE_SIZE (memory_type));
  unsigned int element_bits = tree_to_uhwi (TYPE_SIZE (TREE_TYPE (vectype)));
  if (offset_bits > element_bits)
    /* Internal functions require the offset to be the same width as
       the vector elements.  We can extend narrower offsets, but it isn't
       safe to truncate wider offsets.  */
    return false;

  if (element_bits != memory_bits)
    /* For now the vector elements must be the same width as the
       memory elements.  */
    return false;

  /* Work out which function we need.  */
  internal_fn ifn;
  if (read_p)
    ifn = masked_p ? IFN_MASK_GATHER_LOAD : IFN_GATHER_LOAD;
  else
    ifn = masked_p ? IFN_MASK_SCATTER_STORE : IFN_SCATTER_STORE;

  /* Test whether the target supports this combination.  */
  if (!internal_gather_scatter_fn_supported_p (ifn, vectype, memory_type,
					       offset_sign, scale))
    return false;

  *ifn_out = ifn;
  *element_type_out = TREE_TYPE (vectype);
  return true;
}

/* STMT_INFO is a call to an internal gather load or scatter store function.
   Describe the operation in INFO.  */

static void
vect_describe_gather_scatter_call (stmt_vec_info stmt_info,
				   gather_scatter_info *info)
{
  gcall *call = as_a <gcall *> (stmt_info->stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);

  info->ifn = gimple_call_internal_fn (call);
  info->decl = NULL_TREE;
  info->base = gimple_call_arg (call, 0);
  info->offset = gimple_call_arg (call, 1);
  info->offset_dt = vect_unknown_def_type;
  info->offset_vectype = NULL_TREE;
  info->scale = TREE_INT_CST_LOW (gimple_call_arg (call, 2));
  info->element_type = TREE_TYPE (vectype);
  info->memory_type = TREE_TYPE (DR_REF (dr));
}

/* Return true if a non-affine read or write in STMT_INFO is suitable for a
   gather load or scatter store.  Describe the operation in *INFO if so.  */

bool
vect_check_gather_scatter (stmt_vec_info stmt_info, loop_vec_info loop_vinfo,
			   gather_scatter_info *info)
{
  HOST_WIDE_INT scale = 1;
  poly_int64 pbitpos, pbitsize;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  tree offtype = NULL_TREE;
  tree decl = NULL_TREE, base, off;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree memory_type = TREE_TYPE (DR_REF (dr));
  machine_mode pmode;
  int punsignedp, reversep, pvolatilep = 0;
  internal_fn ifn;
  tree element_type;
  bool masked_p = false;

  /* See whether this is already a call to a gather/scatter internal function.
     If not, see whether it's a masked load or store.  */
  gcall *call = dyn_cast <gcall *> (stmt_info->stmt);
  if (call && gimple_call_internal_p (call))
    {
      ifn = gimple_call_internal_fn (call);
      if (internal_gather_scatter_fn_p (ifn))
	{
	  vect_describe_gather_scatter_call (stmt_info, info);
	  return true;
	}
      masked_p = (ifn == IFN_MASK_LOAD || ifn == IFN_MASK_STORE);
    }

  /* True if we should aim to use internal functions rather than
     built-in functions.  */
  bool use_ifn_p = (DR_IS_READ (dr)
		    ? supports_vec_gather_load_p ()
		    : supports_vec_scatter_store_p ());

  base = DR_REF (dr);
  /* For masked loads/stores, DR_REF (dr) is an artificial MEM_REF,
     see if we can use the def stmt of the address.  */
  if (masked_p
      && TREE_CODE (base) == MEM_REF
      && TREE_CODE (TREE_OPERAND (base, 0)) == SSA_NAME
      && integer_zerop (TREE_OPERAND (base, 1))
      && !expr_invariant_in_loop_p (loop, TREE_OPERAND (base, 0)))
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (TREE_OPERAND (base, 0));
      if (is_gimple_assign (def_stmt)
	  && gimple_assign_rhs_code (def_stmt) == ADDR_EXPR)
	base = TREE_OPERAND (gimple_assign_rhs1 (def_stmt), 0);
    }

  /* The gather and scatter builtins need address of the form
     loop_invariant + vector * {1, 2, 4, 8}
     or
     loop_invariant + sign_extend (vector) * { 1, 2, 4, 8 }.
     Unfortunately DR_BASE_ADDRESS/DR_OFFSET can be a mixture
     of loop invariants/SSA_NAMEs defined in the loop, with casts,
     multiplications and additions in it.  To get a vector, we need
     a single SSA_NAME that will be defined in the loop and will
     contain everything that is not loop invariant and that can be
     vectorized.  The following code attempts to find such a preexistng
     SSA_NAME OFF and put the loop invariants into a tree BASE
     that can be gimplified before the loop.  */
  base = get_inner_reference (base, &pbitsize, &pbitpos, &off, &pmode,
			      &punsignedp, &reversep, &pvolatilep);
  if (reversep)
    return false;

  poly_int64 pbytepos = exact_div (pbitpos, BITS_PER_UNIT);

  if (TREE_CODE (base) == MEM_REF)
    {
      if (!integer_zerop (TREE_OPERAND (base, 1)))
	{
	  if (off == NULL_TREE)
	    off = wide_int_to_tree (sizetype, mem_ref_offset (base));
	  else
	    off = size_binop (PLUS_EXPR, off,
			      fold_convert (sizetype, TREE_OPERAND (base, 1)));
	}
      base = TREE_OPERAND (base, 0);
    }
  else
    base = build_fold_addr_expr (base);

  if (off == NULL_TREE)
    off = size_zero_node;

  /* If base is not loop invariant, either off is 0, then we start with just
     the constant offset in the loop invariant BASE and continue with base
     as OFF, otherwise give up.
     We could handle that case by gimplifying the addition of base + off
     into some SSA_NAME and use that as off, but for now punt.  */
  if (!expr_invariant_in_loop_p (loop, base))
    {
      if (!integer_zerop (off))
	return false;
      off = base;
      base = size_int (pbytepos);
    }
  /* Otherwise put base + constant offset into the loop invariant BASE
     and continue with OFF.  */
  else
    {
      base = fold_convert (sizetype, base);
      base = size_binop (PLUS_EXPR, base, size_int (pbytepos));
    }

  /* OFF at this point may be either a SSA_NAME or some tree expression
     from get_inner_reference.  Try to peel off loop invariants from it
     into BASE as long as possible.  */
  STRIP_NOPS (off);
  while (offtype == NULL_TREE)
    {
      enum tree_code code;
      tree op0, op1, add = NULL_TREE;

      if (TREE_CODE (off) == SSA_NAME)
	{
	  gimple *def_stmt = SSA_NAME_DEF_STMT (off);

	  if (expr_invariant_in_loop_p (loop, off))
	    return false;

	  if (gimple_code (def_stmt) != GIMPLE_ASSIGN)
	    break;

	  op0 = gimple_assign_rhs1 (def_stmt);
	  code = gimple_assign_rhs_code (def_stmt);
	  op1 = gimple_assign_rhs2 (def_stmt);
	}
      else
	{
	  if (get_gimple_rhs_class (TREE_CODE (off)) == GIMPLE_TERNARY_RHS)
	    return false;
	  code = TREE_CODE (off);
	  extract_ops_from_tree (off, &code, &op0, &op1);
	}
      switch (code)
	{
	case POINTER_PLUS_EXPR:
	case PLUS_EXPR:
	  if (expr_invariant_in_loop_p (loop, op0))
	    {
	      add = op0;
	      off = op1;
	    do_add:
	      add = fold_convert (sizetype, add);
	      if (scale != 1)
		add = size_binop (MULT_EXPR, add, size_int (scale));
	      base = size_binop (PLUS_EXPR, base, add);
	      continue;
	    }
	  if (expr_invariant_in_loop_p (loop, op1))
	    {
	      add = op1;
	      off = op0;
	      goto do_add;
	    }
	  break;
	case MINUS_EXPR:
	  if (expr_invariant_in_loop_p (loop, op1))
	    {
	      add = fold_convert (sizetype, op1);
	      add = size_binop (MINUS_EXPR, size_zero_node, add);
	      off = op0;
	      goto do_add;
	    }
	  break;
	case MULT_EXPR:
	  if (scale == 1 && tree_fits_shwi_p (op1))
	    {
	      int new_scale = tree_to_shwi (op1);
	      /* Only treat this as a scaling operation if the target
		 supports it.  */
	      if (use_ifn_p
		  && !vect_gather_scatter_fn_p (DR_IS_READ (dr), masked_p,
						vectype, memory_type, 1,
						TYPE_SIGN (TREE_TYPE (op0)),
						new_scale, &ifn,
						&element_type))
		break;
	      scale = new_scale;
	      off = op0;
	      continue;
	    }
	  break;
	case SSA_NAME:
	  off = op0;
	  continue;
	CASE_CONVERT:
	  if (!POINTER_TYPE_P (TREE_TYPE (op0))
	      && !INTEGRAL_TYPE_P (TREE_TYPE (op0)))
	    break;
	  if (TYPE_PRECISION (TREE_TYPE (op0))
	      == TYPE_PRECISION (TREE_TYPE (off)))
	    {
	      off = op0;
	      continue;
	    }

	  /* The internal functions need the offset to be the same width
	     as the elements of VECTYPE.  Don't include operations that
	     cast the offset from that width to a different width.  */
	  if (use_ifn_p
	      && (int_size_in_bytes (TREE_TYPE (vectype))
		  == int_size_in_bytes (TREE_TYPE (off))))
	    break;

	  if (TYPE_PRECISION (TREE_TYPE (op0))
	      < TYPE_PRECISION (TREE_TYPE (off)))
	    {
	      off = op0;
	      offtype = TREE_TYPE (off);
	      STRIP_NOPS (off);
	      continue;
	    }
	  break;
	default:
	  break;
	}
      break;
    }

  /* If at the end OFF still isn't a SSA_NAME or isn't
     defined in the loop, punt.  */
  if (TREE_CODE (off) != SSA_NAME
      || expr_invariant_in_loop_p (loop, off))
    return false;

  if (offtype == NULL_TREE)
    offtype = TREE_TYPE (off);

  if (use_ifn_p)
    {
      if (!vect_gather_scatter_fn_p (DR_IS_READ (dr), masked_p, vectype,
				     memory_type, TYPE_PRECISION (offtype),
				     TYPE_SIGN (offtype), scale, &ifn,
				     &element_type))
	return false;
    }
  else
    {
      if (DR_IS_READ (dr))
	{
	  if (targetm.vectorize.builtin_gather)
	    decl = targetm.vectorize.builtin_gather (vectype, offtype, scale);
	}
      else
	{
	  if (targetm.vectorize.builtin_scatter)
	    decl = targetm.vectorize.builtin_scatter (vectype, offtype, scale);
	}

      if (!decl)
	return false;

      ifn = IFN_LAST;
      element_type = TREE_TYPE (vectype);
    }

  info->ifn = ifn;
  info->decl = decl;
  info->base = base;
  info->offset = off;
  info->offset_dt = vect_unknown_def_type;
  info->offset_vectype = NULL_TREE;
  info->scale = scale;
  info->element_type = element_type;
  info->memory_type = memory_type;
  return true;
}

/* Find the data references in STMT, analyze them with respect to LOOP and
   append them to DATAREFS.  Return false if datarefs in this stmt cannot
   be handled.  */

bool
vect_find_stmt_data_reference (loop_p loop, gimple *stmt,
			       vec<data_reference_p> *datarefs)
{
  /* We can ignore clobbers for dataref analysis - they are removed during
     loop vectorization and BB vectorization checks dependences with a
     stmt walk.  */
  if (gimple_clobber_p (stmt))
    return true;

  if (gimple_has_volatile_ops (stmt))
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "not vectorized: volatile type ");
	  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
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
	}
      return false;
    }

  auto_vec<data_reference_p, 2> refs;
  if (!find_data_references_in_stmt (loop, stmt, &refs))
    return false;

  if (refs.is_empty ())
    return true;

  if (refs.length () > 1)
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "not vectorized: more than one data ref "
			   "in stmt: ");
	  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	}
      return false;
    }

  if (gcall *call = dyn_cast <gcall *> (stmt))
    if (!gimple_call_internal_p (call)
	|| (gimple_call_internal_fn (call) != IFN_MASK_LOAD
	    && gimple_call_internal_fn (call) != IFN_MASK_STORE))
      {
	if (dump_enabled_p ())
	  {
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION,  vect_location,
			     "not vectorized: dr in a call ");
	    dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	  }
	return false;
      }

  data_reference_p dr = refs.pop ();
  if (TREE_CODE (DR_REF (dr)) == COMPONENT_REF
      && DECL_BIT_FIELD (TREE_OPERAND (DR_REF (dr), 1)))
    {
      if (dump_enabled_p ())
	{
	  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			   "not vectorized: statement is bitfield "
			   "access ");
	  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM, stmt, 0);
	}
      return false;
    }

  if (DR_BASE_ADDRESS (dr)
      && TREE_CODE (DR_BASE_ADDRESS (dr)) == INTEGER_CST)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "not vectorized: base addr of dr is a "
			 "constant\n");
      return false;
    }

  /* Check whether this may be a SIMD lane access and adjust the
     DR to make it easier for us to handle it.  */
  if (loop
      && loop->simduid
      && (!DR_BASE_ADDRESS (dr)
	  || !DR_OFFSET (dr)
	  || !DR_INIT (dr)
	  || !DR_STEP (dr)))
    {
      struct data_reference *newdr
	= create_data_ref (NULL, loop_containing_stmt (stmt), DR_REF (dr), stmt,
			   DR_IS_READ (dr), DR_IS_CONDITIONAL_IN_STMT (dr));
      if (DR_BASE_ADDRESS (newdr)
	  && DR_OFFSET (newdr)
	  && DR_INIT (newdr)
	  && DR_STEP (newdr)
	  && integer_zerop (DR_STEP (newdr)))
	{
	  tree off = DR_OFFSET (newdr);
	  STRIP_NOPS (off);
	  if (TREE_CODE (DR_INIT (newdr)) == INTEGER_CST
	      && TREE_CODE (off) == MULT_EXPR
	      && tree_fits_uhwi_p (TREE_OPERAND (off, 1)))
	    {
	      tree step = TREE_OPERAND (off, 1);
	      off = TREE_OPERAND (off, 0);
	      STRIP_NOPS (off);
	      if (CONVERT_EXPR_P (off)
		  && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (off, 0)))
		      < TYPE_PRECISION (TREE_TYPE (off))))
		off = TREE_OPERAND (off, 0);
	      if (TREE_CODE (off) == SSA_NAME)
		{
		  gimple *def = SSA_NAME_DEF_STMT (off);
		  tree reft = TREE_TYPE (DR_REF (newdr));
		  if (is_gimple_call (def)
		      && gimple_call_internal_p (def)
		      && (gimple_call_internal_fn (def) == IFN_GOMP_SIMD_LANE))
		    {
		      tree arg = gimple_call_arg (def, 0);
		      gcc_assert (TREE_CODE (arg) == SSA_NAME);
		      arg = SSA_NAME_VAR (arg);
		      if (arg == loop->simduid
			  /* For now.  */
			  && tree_int_cst_equal (TYPE_SIZE_UNIT (reft), step))
			{
			  DR_OFFSET (newdr) = ssize_int (0);
			  DR_STEP (newdr) = step;
			  DR_OFFSET_ALIGNMENT (newdr) = BIGGEST_ALIGNMENT;
			  DR_STEP_ALIGNMENT (newdr)
			    = highest_pow2_factor (step);
			  /* Mark as simd-lane access.  */
			  newdr->aux = (void *)-1;
			  free_data_ref (dr);
			  datarefs->safe_push (newdr);
			  return true;
			}
		    }
		}
	    }
	}
      free_data_ref (newdr);
    }

  datarefs->safe_push (dr);
  return true;
}

/* Function vect_analyze_data_refs.

  Find all the data references in the loop or basic block.

   The general structure of the analysis of data refs in the vectorizer is as
   follows:
   1- vect_analyze_data_refs(loop/bb): call
      compute_data_dependences_for_loop/bb to find and analyze all data-refs
      in the loop/bb and their dependences.
   2- vect_analyze_dependences(): apply dependence testing using ddrs.
   3- vect_analyze_drs_alignment(): check that ref_stmt.alignment is ok.
   4- vect_analyze_drs_access(): check that ref_stmt.step is ok.

*/

bool
vect_analyze_data_refs (vec_info *vinfo, poly_uint64 *min_vf)
{
  struct loop *loop = NULL;
  unsigned int i;
  struct data_reference *dr;
  tree scalar_type;

  DUMP_VECT_SCOPE ("vect_analyze_data_refs");

  if (loop_vec_info loop_vinfo = dyn_cast <loop_vec_info> (vinfo))
    loop = LOOP_VINFO_LOOP (loop_vinfo);

  /* Go through the data-refs, check that the analysis succeeded.  Update
     pointer from stmt_vec_info struct to DR and vectype.  */

  vec<data_reference_p> datarefs = vinfo->shared->datarefs;
  FOR_EACH_VEC_ELT (datarefs, i, dr)
    {
      enum { SG_NONE, GATHER, SCATTER } gatherscatter = SG_NONE;
      poly_uint64 vf;

      gcc_assert (DR_REF (dr));
      stmt_vec_info stmt_info = vinfo->lookup_stmt (DR_STMT (dr));
      gcc_assert (!stmt_info->dr_aux.dr);
      stmt_info->dr_aux.dr = dr;
      stmt_info->dr_aux.stmt = stmt_info;

      /* Check that analysis of the data-ref succeeded.  */
      if (!DR_BASE_ADDRESS (dr) || !DR_OFFSET (dr) || !DR_INIT (dr)
	  || !DR_STEP (dr))
        {
	  bool maybe_gather
	    = DR_IS_READ (dr)
	      && !TREE_THIS_VOLATILE (DR_REF (dr))
	      && (targetm.vectorize.builtin_gather != NULL
		  || supports_vec_gather_load_p ());
	  bool maybe_scatter
	    = DR_IS_WRITE (dr)
	      && !TREE_THIS_VOLATILE (DR_REF (dr))
	      && (targetm.vectorize.builtin_scatter != NULL
		  || supports_vec_scatter_store_p ());

	  /* If target supports vector gather loads or scatter stores,
	     see if they can't be used.  */
	  if (is_a <loop_vec_info> (vinfo)
	      && !nested_in_vect_loop_p (loop, stmt_info))
	    {
	      if (maybe_gather || maybe_scatter)
		{
		  if (maybe_gather)
		    gatherscatter = GATHER;
		  else
		    gatherscatter = SCATTER;
		}
	    }

	  if (gatherscatter == SG_NONE)
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                                   "not vectorized: data ref analysis "
                                   "failed ");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				    stmt_info->stmt, 0);
		}
	      if (is_a <bb_vec_info> (vinfo))
		{
		  /* In BB vectorization the ref can still participate
		     in dependence analysis, we just can't vectorize it.  */
		  STMT_VINFO_VECTORIZABLE (stmt_info) = false;
		  continue;
		}
	      return false;
	    }
        }

      /* See if this was detected as SIMD lane access.  */
      if (dr->aux == (void *)-1)
	{
	  if (nested_in_vect_loop_p (loop, stmt_info))
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   "not vectorized: data ref analysis "
				   "failed ");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				    stmt_info->stmt, 0);
		}
	      return false;
	    }
	  STMT_VINFO_SIMD_LANE_ACCESS_P (stmt_info) = true;
	}

      tree base = get_base_address (DR_REF (dr));
      if (base && VAR_P (base) && DECL_NONALIASED (base))
	{
          if (dump_enabled_p ())
            {
              dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                               "not vectorized: base object not addressable "
			       "for stmt: ");
	      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				stmt_info->stmt, 0);
            }
          if (is_a <bb_vec_info> (vinfo))
	    {
	      /* In BB vectorization the ref can still participate
	         in dependence analysis, we just can't vectorize it.  */
	      STMT_VINFO_VECTORIZABLE (stmt_info) = false;
	      continue;
	    }
	  return false;
	}

      if (is_a <loop_vec_info> (vinfo)
	  && DR_STEP (dr)
	  && TREE_CODE (DR_STEP (dr)) != INTEGER_CST)
	{
	  if (nested_in_vect_loop_p (loop, stmt_info))
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location, 
                                   "not vectorized: not suitable for strided "
                                   "load ");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				    stmt_info->stmt, 0);
		}
	      return false;
	    }
	  STMT_VINFO_STRIDED_P (stmt_info) = true;
	}

      /* Update DR field in stmt_vec_info struct.  */

      /* If the dataref is in an inner-loop of the loop that is considered for
	 for vectorization, we also want to analyze the access relative to
	 the outer-loop (DR contains information only relative to the
	 inner-most enclosing loop).  We do that by building a reference to the
	 first location accessed by the inner-loop, and analyze it relative to
	 the outer-loop.  */
      if (loop && nested_in_vect_loop_p (loop, stmt_info))
	{
	  /* Build a reference to the first location accessed by the
	     inner loop: *(BASE + INIT + OFFSET).  By construction,
	     this address must be invariant in the inner loop, so we
	     can consider it as being used in the outer loop.  */
	  tree base = unshare_expr (DR_BASE_ADDRESS (dr));
	  tree offset = unshare_expr (DR_OFFSET (dr));
	  tree init = unshare_expr (DR_INIT (dr));
	  tree init_offset = fold_build2 (PLUS_EXPR, TREE_TYPE (offset),
					  init, offset);
	  tree init_addr = fold_build_pointer_plus (base, init_offset);
	  tree init_ref = build_fold_indirect_ref (init_addr);

	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
                               "analyze in outer loop: ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM, init_ref);
	      dump_printf (MSG_NOTE, "\n");
	    }

	  if (!dr_analyze_innermost (&STMT_VINFO_DR_WRT_VEC_LOOP (stmt_info),
				     init_ref, loop))
	    /* dr_analyze_innermost already explained the failure.  */
	    return false;

          if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
                               "\touter base_address: ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM,
                                 STMT_VINFO_DR_BASE_ADDRESS (stmt_info));
	      dump_printf (MSG_NOTE, "\n\touter offset from base address: ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM,
                                 STMT_VINFO_DR_OFFSET (stmt_info));
	      dump_printf (MSG_NOTE,
                           "\n\touter constant offset from base address: ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM,
                                 STMT_VINFO_DR_INIT (stmt_info));
	      dump_printf (MSG_NOTE, "\n\touter step: ");
	      dump_generic_expr (MSG_NOTE, TDF_SLIM,
                                 STMT_VINFO_DR_STEP (stmt_info));
	      dump_printf (MSG_NOTE, "\n\touter base alignment: %d\n",
			   STMT_VINFO_DR_BASE_ALIGNMENT (stmt_info));
	      dump_printf (MSG_NOTE, "\n\touter base misalignment: %d\n",
			   STMT_VINFO_DR_BASE_MISALIGNMENT (stmt_info));
	      dump_printf (MSG_NOTE, "\n\touter offset alignment: %d\n",
			   STMT_VINFO_DR_OFFSET_ALIGNMENT (stmt_info));
	      dump_printf (MSG_NOTE, "\n\touter step alignment: %d\n",
			   STMT_VINFO_DR_STEP_ALIGNMENT (stmt_info));
	    }
	}

      /* Set vectype for STMT.  */
      scalar_type = TREE_TYPE (DR_REF (dr));
      STMT_VINFO_VECTYPE (stmt_info)
	= get_vectype_for_scalar_type (scalar_type);
      if (!STMT_VINFO_VECTYPE (stmt_info))
        {
          if (dump_enabled_p ())
            {
              dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
                               "not vectorized: no vectype for stmt: ");
	      dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				stmt_info->stmt, 0);
              dump_printf (MSG_MISSED_OPTIMIZATION, " scalar_type: ");
              dump_generic_expr (MSG_MISSED_OPTIMIZATION, TDF_DETAILS,
                                 scalar_type);
              dump_printf (MSG_MISSED_OPTIMIZATION, "\n");
            }

          if (is_a <bb_vec_info> (vinfo))
	    {
	      /* No vector type is fine, the ref can still participate
	         in dependence analysis, we just can't vectorize it.  */
	      STMT_VINFO_VECTORIZABLE (stmt_info) = false;
	      continue;
	    }
	  return false;
        }
      else
	{
	  if (dump_enabled_p ())
	    {
	      dump_printf_loc (MSG_NOTE, vect_location,
			       "got vectype for stmt: ");
	      dump_gimple_stmt (MSG_NOTE, TDF_SLIM, stmt_info->stmt, 0);
	      dump_generic_expr (MSG_NOTE, TDF_SLIM,
				 STMT_VINFO_VECTYPE (stmt_info));
	      dump_printf (MSG_NOTE, "\n");
	    }
	}

      /* Adjust the minimal vectorization factor according to the
	 vector type.  */
      vf = TYPE_VECTOR_SUBPARTS (STMT_VINFO_VECTYPE (stmt_info));
      *min_vf = upper_bound (*min_vf, vf);

      if (gatherscatter != SG_NONE)
	{
	  gather_scatter_info gs_info;
	  if (!vect_check_gather_scatter (stmt_info,
					  as_a <loop_vec_info> (vinfo),
					  &gs_info)
	      || !get_vectype_for_scalar_type (TREE_TYPE (gs_info.offset)))
	    {
	      if (dump_enabled_p ())
		{
		  dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				   (gatherscatter == GATHER) ?
				   "not vectorized: not suitable for gather "
				   "load " :
				   "not vectorized: not suitable for scatter "
				   "store ");
		  dump_gimple_stmt (MSG_MISSED_OPTIMIZATION, TDF_SLIM,
				    stmt_info->stmt, 0);
		}
	      return false;
	    }
	  STMT_VINFO_GATHER_SCATTER_P (stmt_info) = gatherscatter;
	}
    }

  /* We used to stop processing and prune the list here.  Verify we no
     longer need to.  */
  gcc_assert (i == datarefs.length ());

  return true;
}


/* Function vect_get_new_vect_var.

   Returns a name for a new variable.  The current naming scheme appends the
   prefix "vect_" or "vect_p" (depending on the value of VAR_KIND) to
   the name of vectorizer generated variables, and appends that to NAME if
   provided.  */

tree
vect_get_new_vect_var (tree type, enum vect_var_kind var_kind, const char *name)
{
  const char *prefix;
  tree new_vect_var;

  switch (var_kind)
  {
  case vect_simple_var:
    prefix = "vect";
    break;
  case vect_scalar_var:
    prefix = "stmp";
    break;
  case vect_mask_var:
    prefix = "mask";
    break;
  case vect_pointer_var:
    prefix = "vectp";
    break;
  default:
    gcc_unreachable ();
  }

  if (name)
    {
      char* tmp = concat (prefix, "_", name, NULL);
      new_vect_var = create_tmp_reg (type, tmp);
      free (tmp);
    }
  else
    new_vect_var = create_tmp_reg (type, prefix);

  return new_vect_var;
}

/* Like vect_get_new_vect_var but return an SSA name.  */

tree
vect_get_new_ssa_name (tree type, enum vect_var_kind var_kind, const char *name)
{
  const char *prefix;
  tree new_vect_var;

  switch (var_kind)
  {
  case vect_simple_var:
    prefix = "vect";
    break;
  case vect_scalar_var:
    prefix = "stmp";
    break;
  case vect_pointer_var:
    prefix = "vectp";
    break;
  default:
    gcc_unreachable ();
  }

  if (name)
    {
      char* tmp = concat (prefix, "_", name, NULL);
      new_vect_var = make_temp_ssa_name (type, NULL, tmp);
      free (tmp);
    }
  else
    new_vect_var = make_temp_ssa_name (type, NULL, prefix);

  return new_vect_var;
}

/* Duplicate ptr info and set alignment/misaligment on NAME from DR_INFO.  */

static void
vect_duplicate_ssa_name_ptr_info (tree name, dr_vec_info *dr_info)
{
  duplicate_ssa_name_ptr_info (name, DR_PTR_INFO (dr_info->dr));
  int misalign = DR_MISALIGNMENT (dr_info);
  if (misalign == DR_MISALIGNMENT_UNKNOWN)
    mark_ptr_info_alignment_unknown (SSA_NAME_PTR_INFO (name));
  else
    set_ptr_info_alignment (SSA_NAME_PTR_INFO (name),
			    DR_TARGET_ALIGNMENT (dr_info), misalign);
}

/* Function vect_create_addr_base_for_vector_ref.

   Create an expression that computes the address of the first memory location
   that will be accessed for a data reference.

   Input:
   STMT_INFO: The statement containing the data reference.
   NEW_STMT_LIST: Must be initialized to NULL_TREE or a statement list.
   OFFSET: Optional. If supplied, it is be added to the initial address.
   LOOP:    Specify relative to which loop-nest should the address be computed.
            For example, when the dataref is in an inner-loop nested in an
	    outer-loop that is now being vectorized, LOOP can be either the
	    outer-loop, or the inner-loop.  The first memory location accessed
	    by the following dataref ('in' points to short):

		for (i=0; i<N; i++)
		   for (j=0; j<M; j++)
		     s += in[i+j]

	    is as follows:
	    if LOOP=i_loop:	&in		(relative to i_loop)
	    if LOOP=j_loop: 	&in+i*2B	(relative to j_loop)
   BYTE_OFFSET: Optional, defaulted to NULL.  If supplied, it is added to the
	    initial address.  Unlike OFFSET, which is number of elements to
	    be added, BYTE_OFFSET is measured in bytes.

   Output:
   1. Return an SSA_NAME whose value is the address of the memory location of
      the first vector of the data reference.
   2. If new_stmt_list is not NULL_TREE after return then the caller must insert
      these statement(s) which define the returned SSA_NAME.

   FORNOW: We are only handling array accesses with step 1.  */

tree
vect_create_addr_base_for_vector_ref (stmt_vec_info stmt_info,
				      gimple_seq *new_stmt_list,
				      tree offset,
				      tree byte_offset)
{
  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info);
  struct data_reference *dr = dr_info->dr;
  const char *base_name;
  tree addr_base;
  tree dest;
  gimple_seq seq = NULL;
  tree vect_ptr_type;
  tree step = TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dr)));
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  innermost_loop_behavior *drb = vect_dr_behavior (dr_info);

  tree data_ref_base = unshare_expr (drb->base_address);
  tree base_offset = unshare_expr (drb->offset);
  tree init = unshare_expr (drb->init);

  if (loop_vinfo)
    base_name = get_name (data_ref_base);
  else
    {
      base_offset = ssize_int (0);
      init = ssize_int (0);
      base_name = get_name (DR_REF (dr));
    }

  /* Create base_offset */
  base_offset = size_binop (PLUS_EXPR,
			    fold_convert (sizetype, base_offset),
			    fold_convert (sizetype, init));

  if (offset)
    {
      offset = fold_build2 (MULT_EXPR, sizetype,
			    fold_convert (sizetype, offset), step);
      base_offset = fold_build2 (PLUS_EXPR, sizetype,
				 base_offset, offset);
    }
  if (byte_offset)
    {
      byte_offset = fold_convert (sizetype, byte_offset);
      base_offset = fold_build2 (PLUS_EXPR, sizetype,
				 base_offset, byte_offset);
    }

  /* base + base_offset */
  if (loop_vinfo)
    addr_base = fold_build_pointer_plus (data_ref_base, base_offset);
  else
    {
      addr_base = build1 (ADDR_EXPR,
			  build_pointer_type (TREE_TYPE (DR_REF (dr))),
			  unshare_expr (DR_REF (dr)));
    }

  vect_ptr_type = build_pointer_type (STMT_VINFO_VECTYPE (stmt_info));
  dest = vect_get_new_vect_var (vect_ptr_type, vect_pointer_var, base_name);
  addr_base = force_gimple_operand (addr_base, &seq, true, dest);
  gimple_seq_add_seq (new_stmt_list, seq);

  if (DR_PTR_INFO (dr)
      && TREE_CODE (addr_base) == SSA_NAME
      && !SSA_NAME_PTR_INFO (addr_base))
    {
      vect_duplicate_ssa_name_ptr_info (addr_base, dr_info);
      if (offset || byte_offset)
	mark_ptr_info_alignment_unknown (SSA_NAME_PTR_INFO (addr_base));
    }

  if (dump_enabled_p ())
    {
      dump_printf_loc (MSG_NOTE, vect_location, "created ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, addr_base);
      dump_printf (MSG_NOTE, "\n");
    }

  return addr_base;
}


/* Function vect_create_data_ref_ptr.

   Create a new pointer-to-AGGR_TYPE variable (ap), that points to the first
   location accessed in the loop by STMT_INFO, along with the def-use update
   chain to appropriately advance the pointer through the loop iterations.
   Also set aliasing information for the pointer.  This pointer is used by
   the callers to this function to create a memory reference expression for
   vector load/store access.

   Input:
   1. STMT_INFO: a stmt that references memory. Expected to be of the form
         GIMPLE_ASSIGN <name, data-ref> or
	 GIMPLE_ASSIGN <data-ref, name>.
   2. AGGR_TYPE: the type of the reference, which should be either a vector
        or an array.
   3. AT_LOOP: the loop where the vector memref is to be created.
   4. OFFSET (optional): an offset to be added to the initial address accessed
	by the data-ref in STMT_INFO.
   5. BSI: location where the new stmts are to be placed if there is no loop
   6. ONLY_INIT: indicate if ap is to be updated in the loop, or remain
        pointing to the initial address.
   7. BYTE_OFFSET (optional, defaults to NULL): a byte offset to be added
	to the initial address accessed by the data-ref in STMT_INFO.  This is
	similar to OFFSET, but OFFSET is counted in elements, while BYTE_OFFSET
	in bytes.
   8. IV_STEP (optional, defaults to NULL): the amount that should be added
	to the IV during each iteration of the loop.  NULL says to move
	by one copy of AGGR_TYPE up or down, depending on the step of the
	data reference.

   Output:
   1. Declare a new ptr to vector_type, and have it point to the base of the
      data reference (initial addressed accessed by the data reference).
      For example, for vector of type V8HI, the following code is generated:

      v8hi *ap;
      ap = (v8hi *)initial_address;

      if OFFSET is not supplied:
         initial_address = &a[init];
      if OFFSET is supplied:
         initial_address = &a[init + OFFSET];
      if BYTE_OFFSET is supplied:
	 initial_address = &a[init] + BYTE_OFFSET;

      Return the initial_address in INITIAL_ADDRESS.

   2. If ONLY_INIT is true, just return the initial pointer.  Otherwise, also
      update the pointer in each iteration of the loop.

      Return the increment stmt that updates the pointer in PTR_INCR.

   3. Return the pointer.  */

tree
vect_create_data_ref_ptr (stmt_vec_info stmt_info, tree aggr_type,
			  struct loop *at_loop, tree offset,
			  tree *initial_address, gimple_stmt_iterator *gsi,
			  gimple **ptr_incr, bool only_init,
			  tree byte_offset, tree iv_step)
{
  const char *base_name;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = NULL;
  bool nested_in_vect_loop = false;
  struct loop *containing_loop = NULL;
  tree aggr_ptr_type;
  tree aggr_ptr;
  tree new_temp;
  gimple_seq new_stmt_list = NULL;
  edge pe = NULL;
  basic_block new_bb;
  tree aggr_ptr_init;
  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info);
  struct data_reference *dr = dr_info->dr;
  tree aptr;
  gimple_stmt_iterator incr_gsi;
  bool insert_after;
  tree indx_before_incr, indx_after_incr;
  gimple *incr;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);

  gcc_assert (iv_step != NULL_TREE
	      || TREE_CODE (aggr_type) == ARRAY_TYPE
	      || TREE_CODE (aggr_type) == VECTOR_TYPE);

  if (loop_vinfo)
    {
      loop = LOOP_VINFO_LOOP (loop_vinfo);
      nested_in_vect_loop = nested_in_vect_loop_p (loop, stmt_info);
      containing_loop = (gimple_bb (stmt_info->stmt))->loop_father;
      pe = loop_preheader_edge (loop);
    }
  else
    {
      gcc_assert (bb_vinfo);
      only_init = true;
      *ptr_incr = NULL;
    }

  /* Create an expression for the first address accessed by this load
     in LOOP.  */
  base_name = get_name (DR_BASE_ADDRESS (dr));

  if (dump_enabled_p ())
    {
      tree dr_base_type = TREE_TYPE (DR_BASE_OBJECT (dr));
      dump_printf_loc (MSG_NOTE, vect_location,
                       "create %s-pointer variable to type: ",
		       get_tree_code_name (TREE_CODE (aggr_type)));
      dump_generic_expr (MSG_NOTE, TDF_SLIM, aggr_type);
      if (TREE_CODE (dr_base_type) == ARRAY_TYPE)
        dump_printf (MSG_NOTE, "  vectorizing an array ref: ");
      else if (TREE_CODE (dr_base_type) == VECTOR_TYPE)
        dump_printf (MSG_NOTE, "  vectorizing a vector ref: ");
      else if (TREE_CODE (dr_base_type) == RECORD_TYPE)
        dump_printf (MSG_NOTE, "  vectorizing a record based array ref: ");
      else
        dump_printf (MSG_NOTE, "  vectorizing a pointer ref: ");
      dump_generic_expr (MSG_NOTE, TDF_SLIM, DR_BASE_OBJECT (dr));
      dump_printf (MSG_NOTE, "\n");
    }

  /* (1) Create the new aggregate-pointer variable.
     Vector and array types inherit the alias set of their component
     type by default so we need to use a ref-all pointer if the data
     reference does not conflict with the created aggregated data
     reference because it is not addressable.  */
  bool need_ref_all = false;
  if (!alias_sets_conflict_p (get_alias_set (aggr_type),
			      get_alias_set (DR_REF (dr))))
    need_ref_all = true;
  /* Likewise for any of the data references in the stmt group.  */
  else if (DR_GROUP_SIZE (stmt_info) > 1)
    {
      stmt_vec_info sinfo = DR_GROUP_FIRST_ELEMENT (stmt_info);
      do
	{
	  struct data_reference *sdr = STMT_VINFO_DATA_REF (sinfo);
	  if (!alias_sets_conflict_p (get_alias_set (aggr_type),
				      get_alias_set (DR_REF (sdr))))
	    {
	      need_ref_all = true;
	      break;
	    }
	  sinfo = DR_GROUP_NEXT_ELEMENT (sinfo);
	}
      while (sinfo);
    }
  aggr_ptr_type = build_pointer_type_for_mode (aggr_type, ptr_mode,
					       need_ref_all);
  aggr_ptr = vect_get_new_vect_var (aggr_ptr_type, vect_pointer_var, base_name);


  /* Note: If the dataref is in an inner-loop nested in LOOP, and we are
     vectorizing LOOP (i.e., outer-loop vectorization), we need to create two
     def-use update cycles for the pointer: one relative to the outer-loop
     (LOOP), which is what steps (3) and (4) below do.  The other is relative
     to the inner-loop (which is the inner-most loop containing the dataref),
     and this is done be step (5) below.

     When vectorizing inner-most loops, the vectorized loop (LOOP) is also the
     inner-most loop, and so steps (3),(4) work the same, and step (5) is
     redundant.  Steps (3),(4) create the following:

	vp0 = &base_addr;
	LOOP:	vp1 = phi(vp0,vp2)
		...
		...
		vp2 = vp1 + step
		goto LOOP

     If there is an inner-loop nested in loop, then step (5) will also be
     applied, and an additional update in the inner-loop will be created:

	vp0 = &base_addr;
	LOOP:   vp1 = phi(vp0,vp2)
		...
        inner:     vp3 = phi(vp1,vp4)
	           vp4 = vp3 + inner_step
	           if () goto inner
		...
		vp2 = vp1 + step
		if () goto LOOP   */

  /* (2) Calculate the initial address of the aggregate-pointer, and set
     the aggregate-pointer to point to it before the loop.  */

  /* Create: (&(base[init_val+offset]+byte_offset) in the loop preheader.  */

  new_temp = vect_create_addr_base_for_vector_ref (stmt_info, &new_stmt_list,
						   offset, byte_offset);
  if (new_stmt_list)
    {
      if (pe)
        {
          new_bb = gsi_insert_seq_on_edge_immediate (pe, new_stmt_list);
          gcc_assert (!new_bb);
        }
      else
        gsi_insert_seq_before (gsi, new_stmt_list, GSI_SAME_STMT);
    }

  *initial_address = new_temp;
  aggr_ptr_init = new_temp;

  /* (3) Handle the updating of the aggregate-pointer inside the loop.
     This is needed when ONLY_INIT is false, and also when AT_LOOP is the
     inner-loop nested in LOOP (during outer-loop vectorization).  */

  /* No update in loop is required.  */
  if (only_init && (!loop_vinfo || at_loop == loop))
    aptr = aggr_ptr_init;
  else
    {
      /* Accesses to invariant addresses should be handled specially
	 by the caller.  */
      tree step = vect_dr_behavior (dr_info)->step;
      gcc_assert (!integer_zerop (step));

      if (iv_step == NULL_TREE)
	{
	  /* The step of the aggregate pointer is the type size,
	     negated for downward accesses.  */
	  iv_step = TYPE_SIZE_UNIT (aggr_type);
	  if (tree_int_cst_sgn (step) == -1)
	    iv_step = fold_build1 (NEGATE_EXPR, TREE_TYPE (iv_step), iv_step);
	}

      standard_iv_increment_position (loop, &incr_gsi, &insert_after);

      create_iv (aggr_ptr_init,
		 fold_convert (aggr_ptr_type, iv_step),
		 aggr_ptr, loop, &incr_gsi, insert_after,
		 &indx_before_incr, &indx_after_incr);
      incr = gsi_stmt (incr_gsi);
      loop_vinfo->add_stmt (incr);

      /* Copy the points-to information if it exists. */
      if (DR_PTR_INFO (dr))
	{
	  vect_duplicate_ssa_name_ptr_info (indx_before_incr, dr_info);
	  vect_duplicate_ssa_name_ptr_info (indx_after_incr, dr_info);
	}
      if (ptr_incr)
	*ptr_incr = incr;

      aptr = indx_before_incr;
    }

  if (!nested_in_vect_loop || only_init)
    return aptr;


  /* (4) Handle the updating of the aggregate-pointer inside the inner-loop
     nested in LOOP, if exists.  */

  gcc_assert (nested_in_vect_loop);
  if (!only_init)
    {
      standard_iv_increment_position (containing_loop, &incr_gsi,
				      &insert_after);
      create_iv (aptr, fold_convert (aggr_ptr_type, DR_STEP (dr)), aggr_ptr,
		 containing_loop, &incr_gsi, insert_after, &indx_before_incr,
		 &indx_after_incr);
      incr = gsi_stmt (incr_gsi);
      loop_vinfo->add_stmt (incr);

      /* Copy the points-to information if it exists. */
      if (DR_PTR_INFO (dr))
	{
	  vect_duplicate_ssa_name_ptr_info (indx_before_incr, dr_info);
	  vect_duplicate_ssa_name_ptr_info (indx_after_incr, dr_info);
	}
      if (ptr_incr)
	*ptr_incr = incr;

      return indx_before_incr;
    }
  else
    gcc_unreachable ();
}


/* Function bump_vector_ptr

   Increment a pointer (to a vector type) by vector-size. If requested,
   i.e. if PTR-INCR is given, then also connect the new increment stmt
   to the existing def-use update-chain of the pointer, by modifying
   the PTR_INCR as illustrated below:

   The pointer def-use update-chain before this function:
                        DATAREF_PTR = phi (p_0, p_2)
                        ....
        PTR_INCR:       p_2 = DATAREF_PTR + step

   The pointer def-use update-chain after this function:
                        DATAREF_PTR = phi (p_0, p_2)
                        ....
                        NEW_DATAREF_PTR = DATAREF_PTR + BUMP
                        ....
        PTR_INCR:       p_2 = NEW_DATAREF_PTR + step

   Input:
   DATAREF_PTR - ssa_name of a pointer (to vector type) that is being updated
                 in the loop.
   PTR_INCR - optional. The stmt that updates the pointer in each iteration of
	      the loop.  The increment amount across iterations is expected
	      to be vector_size.
   BSI - location where the new update stmt is to be placed.
   STMT_INFO - the original scalar memory-access stmt that is being vectorized.
   BUMP - optional. The offset by which to bump the pointer. If not given,
	  the offset is assumed to be vector_size.

   Output: Return NEW_DATAREF_PTR as illustrated above.

*/

tree
bump_vector_ptr (tree dataref_ptr, gimple *ptr_incr, gimple_stmt_iterator *gsi,
		 stmt_vec_info stmt_info, tree bump)
{
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree update = TYPE_SIZE_UNIT (vectype);
  gassign *incr_stmt;
  ssa_op_iter iter;
  use_operand_p use_p;
  tree new_dataref_ptr;

  if (bump)
    update = bump;

  if (TREE_CODE (dataref_ptr) == SSA_NAME)
    new_dataref_ptr = copy_ssa_name (dataref_ptr);
  else
    new_dataref_ptr = make_ssa_name (TREE_TYPE (dataref_ptr));
  incr_stmt = gimple_build_assign (new_dataref_ptr, POINTER_PLUS_EXPR,
				   dataref_ptr, update);
  vect_finish_stmt_generation (stmt_info, incr_stmt, gsi);

  /* Copy the points-to information if it exists. */
  if (DR_PTR_INFO (dr))
    {
      duplicate_ssa_name_ptr_info (new_dataref_ptr, DR_PTR_INFO (dr));
      mark_ptr_info_alignment_unknown (SSA_NAME_PTR_INFO (new_dataref_ptr));
    }

  if (!ptr_incr)
    return new_dataref_ptr;

  /* Update the vector-pointer's cross-iteration increment.  */
  FOR_EACH_SSA_USE_OPERAND (use_p, ptr_incr, iter, SSA_OP_USE)
    {
      tree use = USE_FROM_PTR (use_p);

      if (use == dataref_ptr)
        SET_USE (use_p, new_dataref_ptr);
      else
        gcc_assert (operand_equal_p (use, update, 0));
    }

  return new_dataref_ptr;
}


/* Copy memory reference info such as base/clique from the SRC reference
   to the DEST MEM_REF.  */

void
vect_copy_ref_info (tree dest, tree src)
{
  if (TREE_CODE (dest) != MEM_REF)
    return;

  tree src_base = src;
  while (handled_component_p (src_base))
    src_base = TREE_OPERAND (src_base, 0);
  if (TREE_CODE (src_base) != MEM_REF
      && TREE_CODE (src_base) != TARGET_MEM_REF)
    return;

  MR_DEPENDENCE_CLIQUE (dest) = MR_DEPENDENCE_CLIQUE (src_base);
  MR_DEPENDENCE_BASE (dest) = MR_DEPENDENCE_BASE (src_base);
}


/* Function vect_create_destination_var.

   Create a new temporary of type VECTYPE.  */

tree
vect_create_destination_var (tree scalar_dest, tree vectype)
{
  tree vec_dest;
  const char *name;
  char *new_name;
  tree type;
  enum vect_var_kind kind;

  kind = vectype
    ? VECTOR_BOOLEAN_TYPE_P (vectype)
    ? vect_mask_var
    : vect_simple_var
    : vect_scalar_var;
  type = vectype ? vectype : TREE_TYPE (scalar_dest);

  gcc_assert (TREE_CODE (scalar_dest) == SSA_NAME);

  name = get_name (scalar_dest);
  if (name)
    new_name = xasprintf ("%s_%u", name, SSA_NAME_VERSION (scalar_dest));
  else
    new_name = xasprintf ("_%u", SSA_NAME_VERSION (scalar_dest));
  vec_dest = vect_get_new_vect_var (type, kind, new_name);
  free (new_name);

  return vec_dest;
}

/* Function vect_grouped_store_supported.

   Returns TRUE if interleave high and interleave low permutations
   are supported, and FALSE otherwise.  */

bool
vect_grouped_store_supported (tree vectype, unsigned HOST_WIDE_INT count)
{
  machine_mode mode = TYPE_MODE (vectype);

  /* vect_permute_store_chain requires the group size to be equal to 3 or
     be a power of two.  */
  if (count != 3 && exact_log2 (count) == -1)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "the size of the group of accesses"
			 " is not a power of 2 or not eqaul to 3\n");
      return false;
    }

  /* Check that the permutation is supported.  */
  if (VECTOR_MODE_P (mode))
    {
      unsigned int i;
      if (count == 3)
	{
	  unsigned int j0 = 0, j1 = 0, j2 = 0;
	  unsigned int i, j;

	  unsigned int nelt;
	  if (!GET_MODE_NUNITS (mode).is_constant (&nelt))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "cannot handle groups of 3 stores for"
				 " variable-length vectors\n");
	      return false;
	    }

	  vec_perm_builder sel (nelt, nelt, 1);
	  sel.quick_grow (nelt);
	  vec_perm_indices indices;
	  for (j = 0; j < 3; j++)
	    {
	      int nelt0 = ((3 - j) * nelt) % 3;
	      int nelt1 = ((3 - j) * nelt + 1) % 3;
	      int nelt2 = ((3 - j) * nelt + 2) % 3;
	      for (i = 0; i < nelt; i++)
		{
		  if (3 * i + nelt0 < nelt)
		    sel[3 * i + nelt0] = j0++;
		  if (3 * i + nelt1 < nelt)
		    sel[3 * i + nelt1] = nelt + j1++;
		  if (3 * i + nelt2 < nelt)
		    sel[3 * i + nelt2] = 0;
		}
	      indices.new_vector (sel, 2, nelt);
	      if (!can_vec_perm_const_p (mode, indices))
		{
		  if (dump_enabled_p ())
		    dump_printf (MSG_MISSED_OPTIMIZATION,
				 "permutation op not supported by target.\n");
		  return false;
		}

	      for (i = 0; i < nelt; i++)
		{
		  if (3 * i + nelt0 < nelt)
		    sel[3 * i + nelt0] = 3 * i + nelt0;
		  if (3 * i + nelt1 < nelt)
		    sel[3 * i + nelt1] = 3 * i + nelt1;
		  if (3 * i + nelt2 < nelt)
		    sel[3 * i + nelt2] = nelt + j2++;
		}
	      indices.new_vector (sel, 2, nelt);
	      if (!can_vec_perm_const_p (mode, indices))
		{
		  if (dump_enabled_p ())
		    dump_printf (MSG_MISSED_OPTIMIZATION,
				 "permutation op not supported by target.\n");
		  return false;
		}
	    }
	  return true;
	}
      else
	{
	  /* If length is not equal to 3 then only power of 2 is supported.  */
	  gcc_assert (pow2p_hwi (count));
	  poly_uint64 nelt = GET_MODE_NUNITS (mode);

	  /* The encoding has 2 interleaved stepped patterns.  */
	  vec_perm_builder sel (nelt, 2, 3);
	  sel.quick_grow (6);
	  for (i = 0; i < 3; i++)
	    {
	      sel[i * 2] = i;
	      sel[i * 2 + 1] = i + nelt;
	    }
	  vec_perm_indices indices (sel, 2, nelt);
	  if (can_vec_perm_const_p (mode, indices))
	    {
	      for (i = 0; i < 6; i++)
		sel[i] += exact_div (nelt, 2);
	      indices.new_vector (sel, 2, nelt);
	      if (can_vec_perm_const_p (mode, indices))
		return true;
	    }
	}
    }

  if (dump_enabled_p ())
    dump_printf (MSG_MISSED_OPTIMIZATION,
		 "permutation op not supported by target.\n");
  return false;
}


/* Return TRUE if vec_{mask_}store_lanes is available for COUNT vectors of
   type VECTYPE.  MASKED_P says whether the masked form is needed.  */

bool
vect_store_lanes_supported (tree vectype, unsigned HOST_WIDE_INT count,
			    bool masked_p)
{
  if (masked_p)
    return vect_lanes_optab_supported_p ("vec_mask_store_lanes",
					 vec_mask_store_lanes_optab,
					 vectype, count);
  else
    return vect_lanes_optab_supported_p ("vec_store_lanes",
					 vec_store_lanes_optab,
					 vectype, count);
}


/* Function vect_permute_store_chain.

   Given a chain of interleaved stores in DR_CHAIN of LENGTH that must be
   a power of 2 or equal to 3, generate interleave_high/low stmts to reorder
   the data correctly for the stores.  Return the final references for stores
   in RESULT_CHAIN.

   E.g., LENGTH is 4 and the scalar type is short, i.e., VF is 8.
   The input is 4 vectors each containing 8 elements.  We assign a number to
   each element, the input sequence is:

   1st vec:   0  1  2  3  4  5  6  7
   2nd vec:   8  9 10 11 12 13 14 15
   3rd vec:  16 17 18 19 20 21 22 23
   4th vec:  24 25 26 27 28 29 30 31

   The output sequence should be:

   1st vec:  0  8 16 24  1  9 17 25
   2nd vec:  2 10 18 26  3 11 19 27
   3rd vec:  4 12 20 28  5 13 21 30
   4th vec:  6 14 22 30  7 15 23 31

   i.e., we interleave the contents of the four vectors in their order.

   We use interleave_high/low instructions to create such output.  The input of
   each interleave_high/low operation is two vectors:
   1st vec    2nd vec
   0 1 2 3    4 5 6 7
   the even elements of the result vector are obtained left-to-right from the
   high/low elements of the first vector.  The odd elements of the result are
   obtained left-to-right from the high/low elements of the second vector.
   The output of interleave_high will be:   0 4 1 5
   and of interleave_low:                   2 6 3 7


   The permutation is done in log LENGTH stages.  In each stage interleave_high
   and interleave_low stmts are created for each pair of vectors in DR_CHAIN,
   where the first argument is taken from the first half of DR_CHAIN and the
   second argument from it's second half.
   In our example,

   I1: interleave_high (1st vec, 3rd vec)
   I2: interleave_low (1st vec, 3rd vec)
   I3: interleave_high (2nd vec, 4th vec)
   I4: interleave_low (2nd vec, 4th vec)

   The output for the first stage is:

   I1:  0 16  1 17  2 18  3 19
   I2:  4 20  5 21  6 22  7 23
   I3:  8 24  9 25 10 26 11 27
   I4: 12 28 13 29 14 30 15 31

   The output of the second stage, i.e. the final result is:

   I1:  0  8 16 24  1  9 17 25
   I2:  2 10 18 26  3 11 19 27
   I3:  4 12 20 28  5 13 21 30
   I4:  6 14 22 30  7 15 23 31.  */

void
vect_permute_store_chain (vec<tree> dr_chain,
			  unsigned int length,
			  stmt_vec_info stmt_info,
			  gimple_stmt_iterator *gsi,
			  vec<tree> *result_chain)
{
  tree vect1, vect2, high, low;
  gimple *perm_stmt;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree perm_mask_low, perm_mask_high;
  tree data_ref;
  tree perm3_mask_low, perm3_mask_high;
  unsigned int i, j, n, log_length = exact_log2 (length);

  result_chain->quick_grow (length);
  memcpy (result_chain->address (), dr_chain.address (),
	  length * sizeof (tree));

  if (length == 3)
    {
      /* vect_grouped_store_supported ensures that this is constant.  */
      unsigned int nelt = TYPE_VECTOR_SUBPARTS (vectype).to_constant ();
      unsigned int j0 = 0, j1 = 0, j2 = 0;

      vec_perm_builder sel (nelt, nelt, 1);
      sel.quick_grow (nelt);
      vec_perm_indices indices;
      for (j = 0; j < 3; j++)
        {
	  int nelt0 = ((3 - j) * nelt) % 3;
	  int nelt1 = ((3 - j) * nelt + 1) % 3;
	  int nelt2 = ((3 - j) * nelt + 2) % 3;

	  for (i = 0; i < nelt; i++)
	    {
	      if (3 * i + nelt0 < nelt)
		sel[3 * i + nelt0] = j0++;
	      if (3 * i + nelt1 < nelt)
		sel[3 * i + nelt1] = nelt + j1++;
	      if (3 * i + nelt2 < nelt)
		sel[3 * i + nelt2] = 0;
	    }
	  indices.new_vector (sel, 2, nelt);
	  perm3_mask_low = vect_gen_perm_mask_checked (vectype, indices);

	  for (i = 0; i < nelt; i++)
	    {
	      if (3 * i + nelt0 < nelt)
		sel[3 * i + nelt0] = 3 * i + nelt0;
	      if (3 * i + nelt1 < nelt)
		sel[3 * i + nelt1] = 3 * i + nelt1;
	      if (3 * i + nelt2 < nelt)
		sel[3 * i + nelt2] = nelt + j2++;
	    }
	  indices.new_vector (sel, 2, nelt);
	  perm3_mask_high = vect_gen_perm_mask_checked (vectype, indices);

	  vect1 = dr_chain[0];
	  vect2 = dr_chain[1];

	  /* Create interleaving stmt:
	     low = VEC_PERM_EXPR <vect1, vect2,
				  {j, nelt, *, j + 1, nelt + j + 1, *,
				   j + 2, nelt + j + 2, *, ...}>  */
	  data_ref = make_temp_ssa_name (vectype, NULL, "vect_shuffle3_low");
	  perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR, vect1,
					   vect2, perm3_mask_low);
	  vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);

	  vect1 = data_ref;
	  vect2 = dr_chain[2];
	  /* Create interleaving stmt:
	     low = VEC_PERM_EXPR <vect1, vect2,
				  {0, 1, nelt + j, 3, 4, nelt + j + 1,
				   6, 7, nelt + j + 2, ...}>  */
	  data_ref = make_temp_ssa_name (vectype, NULL, "vect_shuffle3_high");
	  perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR, vect1,
					   vect2, perm3_mask_high);
	  vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
	  (*result_chain)[j] = data_ref;
	}
    }
  else
    {
      /* If length is not equal to 3 then only power of 2 is supported.  */
      gcc_assert (pow2p_hwi (length));

      /* The encoding has 2 interleaved stepped patterns.  */
      poly_uint64 nelt = TYPE_VECTOR_SUBPARTS (vectype);
      vec_perm_builder sel (nelt, 2, 3);
      sel.quick_grow (6);
      for (i = 0; i < 3; i++)
	{
	  sel[i * 2] = i;
	  sel[i * 2 + 1] = i + nelt;
	}
	vec_perm_indices indices (sel, 2, nelt);
	perm_mask_high = vect_gen_perm_mask_checked (vectype, indices);

	for (i = 0; i < 6; i++)
	  sel[i] += exact_div (nelt, 2);
	indices.new_vector (sel, 2, nelt);
	perm_mask_low = vect_gen_perm_mask_checked (vectype, indices);

	for (i = 0, n = log_length; i < n; i++)
	  {
	    for (j = 0; j < length/2; j++)
	      {
		vect1 = dr_chain[j];
		vect2 = dr_chain[j+length/2];

		/* Create interleaving stmt:
		   high = VEC_PERM_EXPR <vect1, vect2, {0, nelt, 1, nelt+1,
							...}>  */
		high = make_temp_ssa_name (vectype, NULL, "vect_inter_high");
		perm_stmt = gimple_build_assign (high, VEC_PERM_EXPR, vect1,
						 vect2, perm_mask_high);
		vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
		(*result_chain)[2*j] = high;

		/* Create interleaving stmt:
		   low = VEC_PERM_EXPR <vect1, vect2,
					{nelt/2, nelt*3/2, nelt/2+1, nelt*3/2+1,
					 ...}>  */
		low = make_temp_ssa_name (vectype, NULL, "vect_inter_low");
		perm_stmt = gimple_build_assign (low, VEC_PERM_EXPR, vect1,
						 vect2, perm_mask_low);
		vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
		(*result_chain)[2*j+1] = low;
	      }
	    memcpy (dr_chain.address (), result_chain->address (),
		    length * sizeof (tree));
	  }
    }
}

/* Function vect_setup_realignment

   This function is called when vectorizing an unaligned load using
   the dr_explicit_realign[_optimized] scheme.
   This function generates the following code at the loop prolog:

      p = initial_addr;
   x  msq_init = *(floor(p));   # prolog load
      realignment_token = call target_builtin;
    loop:
   x  msq = phi (msq_init, ---)

   The stmts marked with x are generated only for the case of
   dr_explicit_realign_optimized.

   The code above sets up a new (vector) pointer, pointing to the first
   location accessed by STMT_INFO, and a "floor-aligned" load using that
   pointer.  It also generates code to compute the "realignment-token"
   (if the relevant target hook was defined), and creates a phi-node at the
   loop-header bb whose arguments are the result of the prolog-load (created
   by this function) and the result of a load that takes place in the loop
   (to be created by the caller to this function).

   For the case of dr_explicit_realign_optimized:
   The caller to this function uses the phi-result (msq) to create the
   realignment code inside the loop, and sets up the missing phi argument,
   as follows:
    loop:
      msq = phi (msq_init, lsq)
      lsq = *(floor(p'));        # load in loop
      result = realign_load (msq, lsq, realignment_token);

   For the case of dr_explicit_realign:
    loop:
      msq = *(floor(p)); 	# load in loop
      p' = p + (VS-1);
      lsq = *(floor(p'));	# load in loop
      result = realign_load (msq, lsq, realignment_token);

   Input:
   STMT_INFO - (scalar) load stmt to be vectorized. This load accesses
	       a memory location that may be unaligned.
   BSI - place where new code is to be inserted.
   ALIGNMENT_SUPPORT_SCHEME - which of the two misalignment handling schemes
			      is used.

   Output:
   REALIGNMENT_TOKEN - the result of a call to the builtin_mask_for_load
                       target hook, if defined.
   Return value - the result of the loop-header phi node.  */

tree
vect_setup_realignment (stmt_vec_info stmt_info, gimple_stmt_iterator *gsi,
                        tree *realignment_token,
			enum dr_alignment_support alignment_support_scheme,
			tree init_addr,
			struct loop **at_loop)
{
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  dr_vec_info *dr_info = STMT_VINFO_DR_INFO (stmt_info);
  struct data_reference *dr = dr_info->dr;
  struct loop *loop = NULL;
  edge pe = NULL;
  tree scalar_dest = gimple_assign_lhs (stmt_info->stmt);
  tree vec_dest;
  gimple *inc;
  tree ptr;
  tree data_ref;
  basic_block new_bb;
  tree msq_init = NULL_TREE;
  tree new_temp;
  gphi *phi_stmt;
  tree msq = NULL_TREE;
  gimple_seq stmts = NULL;
  bool compute_in_loop = false;
  bool nested_in_vect_loop = false;
  struct loop *containing_loop = (gimple_bb (stmt_info->stmt))->loop_father;
  struct loop *loop_for_initial_load = NULL;

  if (loop_vinfo)
    {
      loop = LOOP_VINFO_LOOP (loop_vinfo);
      nested_in_vect_loop = nested_in_vect_loop_p (loop, stmt_info);
    }

  gcc_assert (alignment_support_scheme == dr_explicit_realign
	      || alignment_support_scheme == dr_explicit_realign_optimized);

  /* We need to generate three things:
     1. the misalignment computation
     2. the extra vector load (for the optimized realignment scheme).
     3. the phi node for the two vectors from which the realignment is
      done (for the optimized realignment scheme).  */

  /* 1. Determine where to generate the misalignment computation.

     If INIT_ADDR is NULL_TREE, this indicates that the misalignment
     calculation will be generated by this function, outside the loop (in the
     preheader).  Otherwise, INIT_ADDR had already been computed for us by the
     caller, inside the loop.

     Background: If the misalignment remains fixed throughout the iterations of
     the loop, then both realignment schemes are applicable, and also the
     misalignment computation can be done outside LOOP.  This is because we are
     vectorizing LOOP, and so the memory accesses in LOOP advance in steps that
     are a multiple of VS (the Vector Size), and therefore the misalignment in
     different vectorized LOOP iterations is always the same.
     The problem arises only if the memory access is in an inner-loop nested
     inside LOOP, which is now being vectorized using outer-loop vectorization.
     This is the only case when the misalignment of the memory access may not
     remain fixed throughout the iterations of the inner-loop (as explained in
     detail in vect_supportable_dr_alignment).  In this case, not only is the
     optimized realignment scheme not applicable, but also the misalignment
     computation (and generation of the realignment token that is passed to
     REALIGN_LOAD) have to be done inside the loop.

     In short, INIT_ADDR indicates whether we are in a COMPUTE_IN_LOOP mode
     or not, which in turn determines if the misalignment is computed inside
     the inner-loop, or outside LOOP.  */

  if (init_addr != NULL_TREE || !loop_vinfo)
    {
      compute_in_loop = true;
      gcc_assert (alignment_support_scheme == dr_explicit_realign);
    }


  /* 2. Determine where to generate the extra vector load.

     For the optimized realignment scheme, instead of generating two vector
     loads in each iteration, we generate a single extra vector load in the
     preheader of the loop, and in each iteration reuse the result of the
     vector load from the previous iteration.  In case the memory access is in
     an inner-loop nested inside LOOP, which is now being vectorized using
     outer-loop vectorization, we need to determine whether this initial vector
     load should be generated at the preheader of the inner-loop, or can be
     generated at the preheader of LOOP.  If the memory access has no evolution
     in LOOP, it can be generated in the preheader of LOOP. Otherwise, it has
     to be generated inside LOOP (in the preheader of the inner-loop).  */

  if (nested_in_vect_loop)
    {
      tree outerloop_step = STMT_VINFO_DR_STEP (stmt_info);
      bool invariant_in_outerloop =
            (tree_int_cst_compare (outerloop_step, size_zero_node) == 0);
      loop_for_initial_load = (invariant_in_outerloop ? loop : loop->inner);
    }
  else
    loop_for_initial_load = loop;
  if (at_loop)
    *at_loop = loop_for_initial_load;

  if (loop_for_initial_load)
    pe = loop_preheader_edge (loop_for_initial_load);

  /* 3. For the case of the optimized realignment, create the first vector
      load at the loop preheader.  */

  if (alignment_support_scheme == dr_explicit_realign_optimized)
    {
      /* Create msq_init = *(floor(p1)) in the loop preheader  */
      gassign *new_stmt;

      gcc_assert (!compute_in_loop);
      vec_dest = vect_create_destination_var (scalar_dest, vectype);
      ptr = vect_create_data_ref_ptr (stmt_info, vectype,
				      loop_for_initial_load, NULL_TREE,
				      &init_addr, NULL, &inc, true);
      if (TREE_CODE (ptr) == SSA_NAME)
	new_temp = copy_ssa_name (ptr);
      else
	new_temp = make_ssa_name (TREE_TYPE (ptr));
      unsigned int align = DR_TARGET_ALIGNMENT (dr_info);
      new_stmt = gimple_build_assign
		   (new_temp, BIT_AND_EXPR, ptr,
		    build_int_cst (TREE_TYPE (ptr), -(HOST_WIDE_INT) align));
      new_bb = gsi_insert_on_edge_immediate (pe, new_stmt);
      gcc_assert (!new_bb);
      data_ref
	= build2 (MEM_REF, TREE_TYPE (vec_dest), new_temp,
		  build_int_cst (reference_alias_ptr_type (DR_REF (dr)), 0));
      vect_copy_ref_info (data_ref, DR_REF (dr));
      new_stmt = gimple_build_assign (vec_dest, data_ref);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      gimple_assign_set_lhs (new_stmt, new_temp);
      if (pe)
        {
          new_bb = gsi_insert_on_edge_immediate (pe, new_stmt);
          gcc_assert (!new_bb);
        }
      else
         gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);

      msq_init = gimple_assign_lhs (new_stmt);
    }

  /* 4. Create realignment token using a target builtin, if available.
      It is done either inside the containing loop, or before LOOP (as
      determined above).  */

  if (targetm.vectorize.builtin_mask_for_load)
    {
      gcall *new_stmt;
      tree builtin_decl;

      /* Compute INIT_ADDR - the initial addressed accessed by this memref.  */
      if (!init_addr)
	{
	  /* Generate the INIT_ADDR computation outside LOOP.  */
	  init_addr = vect_create_addr_base_for_vector_ref (stmt_info, &stmts,
							    NULL_TREE);
          if (loop)
            {
   	      pe = loop_preheader_edge (loop);
	      new_bb = gsi_insert_seq_on_edge_immediate (pe, stmts);
	      gcc_assert (!new_bb);
            }
          else
             gsi_insert_seq_before (gsi, stmts, GSI_SAME_STMT);
	}

      builtin_decl = targetm.vectorize.builtin_mask_for_load ();
      new_stmt = gimple_build_call (builtin_decl, 1, init_addr);
      vec_dest =
	vect_create_destination_var (scalar_dest,
				     gimple_call_return_type (new_stmt));
      new_temp = make_ssa_name (vec_dest, new_stmt);
      gimple_call_set_lhs (new_stmt, new_temp);

      if (compute_in_loop)
	gsi_insert_before (gsi, new_stmt, GSI_SAME_STMT);
      else
	{
	  /* Generate the misalignment computation outside LOOP.  */
	  pe = loop_preheader_edge (loop);
	  new_bb = gsi_insert_on_edge_immediate (pe, new_stmt);
	  gcc_assert (!new_bb);
	}

      *realignment_token = gimple_call_lhs (new_stmt);

      /* The result of the CALL_EXPR to this builtin is determined from
         the value of the parameter and no global variables are touched
         which makes the builtin a "const" function.  Requiring the
         builtin to have the "const" attribute makes it unnecessary
         to call mark_call_clobbered.  */
      gcc_assert (TREE_READONLY (builtin_decl));
    }

  if (alignment_support_scheme == dr_explicit_realign)
    return msq;

  gcc_assert (!compute_in_loop);
  gcc_assert (alignment_support_scheme == dr_explicit_realign_optimized);


  /* 5. Create msq = phi <msq_init, lsq> in loop  */

  pe = loop_preheader_edge (containing_loop);
  vec_dest = vect_create_destination_var (scalar_dest, vectype);
  msq = make_ssa_name (vec_dest);
  phi_stmt = create_phi_node (msq, containing_loop->header);
  add_phi_arg (phi_stmt, msq_init, pe, UNKNOWN_LOCATION);

  return msq;
}


/* Function vect_grouped_load_supported.

   COUNT is the size of the load group (the number of statements plus the
   number of gaps).  SINGLE_ELEMENT_P is true if there is actually
   only one statement, with a gap of COUNT - 1.

   Returns true if a suitable permute exists.  */

bool
vect_grouped_load_supported (tree vectype, bool single_element_p,
			     unsigned HOST_WIDE_INT count)
{
  machine_mode mode = TYPE_MODE (vectype);

  /* If this is single-element interleaving with an element distance
     that leaves unused vector loads around punt - we at least create
     very sub-optimal code in that case (and blow up memory,
     see PR65518).  */
  if (single_element_p && maybe_gt (count, TYPE_VECTOR_SUBPARTS (vectype)))
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "single-element interleaving not supported "
			 "for not adjacent vector loads\n");
      return false;
    }

  /* vect_permute_load_chain requires the group size to be equal to 3 or
     be a power of two.  */
  if (count != 3 && exact_log2 (count) == -1)
    {
      if (dump_enabled_p ())
	dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			 "the size of the group of accesses"
			 " is not a power of 2 or not equal to 3\n");
      return false;
    }

  /* Check that the permutation is supported.  */
  if (VECTOR_MODE_P (mode))
    {
      unsigned int i, j;
      if (count == 3)
	{
	  unsigned int nelt;
	  if (!GET_MODE_NUNITS (mode).is_constant (&nelt))
	    {
	      if (dump_enabled_p ())
		dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				 "cannot handle groups of 3 loads for"
				 " variable-length vectors\n");
	      return false;
	    }

	  vec_perm_builder sel (nelt, nelt, 1);
	  sel.quick_grow (nelt);
	  vec_perm_indices indices;
	  unsigned int k;
	  for (k = 0; k < 3; k++)
	    {
	      for (i = 0; i < nelt; i++)
		if (3 * i + k < 2 * nelt)
		  sel[i] = 3 * i + k;
		else
		  sel[i] = 0;
	      indices.new_vector (sel, 2, nelt);
	      if (!can_vec_perm_const_p (mode, indices))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "shuffle of 3 loads is not supported by"
				     " target\n");
		  return false;
		}
	      for (i = 0, j = 0; i < nelt; i++)
		if (3 * i + k < 2 * nelt)
		  sel[i] = i;
		else
		  sel[i] = nelt + ((nelt + k) % 3) + 3 * (j++);
	      indices.new_vector (sel, 2, nelt);
	      if (!can_vec_perm_const_p (mode, indices))
		{
		  if (dump_enabled_p ())
		    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
				     "shuffle of 3 loads is not supported by"
				     " target\n");
		  return false;
		}
	    }
	  return true;
	}
      else
	{
	  /* If length is not equal to 3 then only power of 2 is supported.  */
	  gcc_assert (pow2p_hwi (count));
	  poly_uint64 nelt = GET_MODE_NUNITS (mode);

	  /* The encoding has a single stepped pattern.  */
	  vec_perm_builder sel (nelt, 1, 3);
	  sel.quick_grow (3);
	  for (i = 0; i < 3; i++)
	    sel[i] = i * 2;
	  vec_perm_indices indices (sel, 2, nelt);
	  if (can_vec_perm_const_p (mode, indices))
	    {
	      for (i = 0; i < 3; i++)
		sel[i] = i * 2 + 1;
	      indices.new_vector (sel, 2, nelt);
	      if (can_vec_perm_const_p (mode, indices))
		return true;
	    }
        }
    }

  if (dump_enabled_p ())
    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
		     "extract even/odd not supported by target\n");
  return false;
}

/* Return TRUE if vec_{masked_}load_lanes is available for COUNT vectors of
   type VECTYPE.  MASKED_P says whether the masked form is needed.  */

bool
vect_load_lanes_supported (tree vectype, unsigned HOST_WIDE_INT count,
			   bool masked_p)
{
  if (masked_p)
    return vect_lanes_optab_supported_p ("vec_mask_load_lanes",
					 vec_mask_load_lanes_optab,
					 vectype, count);
  else
    return vect_lanes_optab_supported_p ("vec_load_lanes",
					 vec_load_lanes_optab,
					 vectype, count);
}

/* Function vect_permute_load_chain.

   Given a chain of interleaved loads in DR_CHAIN of LENGTH that must be
   a power of 2 or equal to 3, generate extract_even/odd stmts to reorder
   the input data correctly.  Return the final references for loads in
   RESULT_CHAIN.

   E.g., LENGTH is 4 and the scalar type is short, i.e., VF is 8.
   The input is 4 vectors each containing 8 elements. We assign a number to each
   element, the input sequence is:

   1st vec:   0  1  2  3  4  5  6  7
   2nd vec:   8  9 10 11 12 13 14 15
   3rd vec:  16 17 18 19 20 21 22 23
   4th vec:  24 25 26 27 28 29 30 31

   The output sequence should be:

   1st vec:  0 4  8 12 16 20 24 28
   2nd vec:  1 5  9 13 17 21 25 29
   3rd vec:  2 6 10 14 18 22 26 30
   4th vec:  3 7 11 15 19 23 27 31

   i.e., the first output vector should contain the first elements of each
   interleaving group, etc.

   We use extract_even/odd instructions to create such output.  The input of
   each extract_even/odd operation is two vectors
   1st vec    2nd vec
   0 1 2 3    4 5 6 7

   and the output is the vector of extracted even/odd elements.  The output of
   extract_even will be:   0 2 4 6
   and of extract_odd:     1 3 5 7


   The permutation is done in log LENGTH stages.  In each stage extract_even
   and extract_odd stmts are created for each pair of vectors in DR_CHAIN in
   their order.  In our example,

   E1: extract_even (1st vec, 2nd vec)
   E2: extract_odd (1st vec, 2nd vec)
   E3: extract_even (3rd vec, 4th vec)
   E4: extract_odd (3rd vec, 4th vec)

   The output for the first stage will be:

   E1:  0  2  4  6  8 10 12 14
   E2:  1  3  5  7  9 11 13 15
   E3: 16 18 20 22 24 26 28 30
   E4: 17 19 21 23 25 27 29 31

   In order to proceed and create the correct sequence for the next stage (or
   for the correct output, if the second stage is the last one, as in our
   example), we first put the output of extract_even operation and then the
   output of extract_odd in RESULT_CHAIN (which is then copied to DR_CHAIN).
   The input for the second stage is:

   1st vec (E1):  0  2  4  6  8 10 12 14
   2nd vec (E3): 16 18 20 22 24 26 28 30
   3rd vec (E2):  1  3  5  7  9 11 13 15
   4th vec (E4): 17 19 21 23 25 27 29 31

   The output of the second stage:

   E1: 0 4  8 12 16 20 24 28
   E2: 2 6 10 14 18 22 26 30
   E3: 1 5  9 13 17 21 25 29
   E4: 3 7 11 15 19 23 27 31

   And RESULT_CHAIN after reordering:

   1st vec (E1):  0 4  8 12 16 20 24 28
   2nd vec (E3):  1 5  9 13 17 21 25 29
   3rd vec (E2):  2 6 10 14 18 22 26 30
   4th vec (E4):  3 7 11 15 19 23 27 31.  */

static void
vect_permute_load_chain (vec<tree> dr_chain,
			 unsigned int length,
			 stmt_vec_info stmt_info,
			 gimple_stmt_iterator *gsi,
			 vec<tree> *result_chain)
{
  tree data_ref, first_vect, second_vect;
  tree perm_mask_even, perm_mask_odd;
  tree perm3_mask_low, perm3_mask_high;
  gimple *perm_stmt;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  unsigned int i, j, log_length = exact_log2 (length);

  result_chain->quick_grow (length);
  memcpy (result_chain->address (), dr_chain.address (),
	  length * sizeof (tree));

  if (length == 3)
    {
      /* vect_grouped_load_supported ensures that this is constant.  */
      unsigned nelt = TYPE_VECTOR_SUBPARTS (vectype).to_constant ();
      unsigned int k;

      vec_perm_builder sel (nelt, nelt, 1);
      sel.quick_grow (nelt);
      vec_perm_indices indices;
      for (k = 0; k < 3; k++)
	{
	  for (i = 0; i < nelt; i++)
	    if (3 * i + k < 2 * nelt)
	      sel[i] = 3 * i + k;
	    else
	      sel[i] = 0;
	  indices.new_vector (sel, 2, nelt);
	  perm3_mask_low = vect_gen_perm_mask_checked (vectype, indices);

	  for (i = 0, j = 0; i < nelt; i++)
	    if (3 * i + k < 2 * nelt)
	      sel[i] = i;
	    else
	      sel[i] = nelt + ((nelt + k) % 3) + 3 * (j++);
	  indices.new_vector (sel, 2, nelt);
	  perm3_mask_high = vect_gen_perm_mask_checked (vectype, indices);

	  first_vect = dr_chain[0];
	  second_vect = dr_chain[1];

	  /* Create interleaving stmt (low part of):
	     low = VEC_PERM_EXPR <first_vect, second_vect2, {k, 3 + k, 6 + k,
							     ...}>  */
	  data_ref = make_temp_ssa_name (vectype, NULL, "vect_shuffle3_low");
	  perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR, first_vect,
					   second_vect, perm3_mask_low);
	  vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);

	  /* Create interleaving stmt (high part of):
	     high = VEC_PERM_EXPR <first_vect, second_vect2, {k, 3 + k, 6 + k,
							      ...}>  */
	  first_vect = data_ref;
	  second_vect = dr_chain[2];
	  data_ref = make_temp_ssa_name (vectype, NULL, "vect_shuffle3_high");
	  perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR, first_vect,
					   second_vect, perm3_mask_high);
	  vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
	  (*result_chain)[k] = data_ref;
	}
    }
  else
    {
      /* If length is not equal to 3 then only power of 2 is supported.  */
      gcc_assert (pow2p_hwi (length));

      /* The encoding has a single stepped pattern.  */
      poly_uint64 nelt = TYPE_VECTOR_SUBPARTS (vectype);
      vec_perm_builder sel (nelt, 1, 3);
      sel.quick_grow (3);
      for (i = 0; i < 3; ++i)
	sel[i] = i * 2;
      vec_perm_indices indices (sel, 2, nelt);
      perm_mask_even = vect_gen_perm_mask_checked (vectype, indices);

      for (i = 0; i < 3; ++i)
	sel[i] = i * 2 + 1;
      indices.new_vector (sel, 2, nelt);
      perm_mask_odd = vect_gen_perm_mask_checked (vectype, indices);

      for (i = 0; i < log_length; i++)
	{
	  for (j = 0; j < length; j += 2)
	    {
	      first_vect = dr_chain[j];
	      second_vect = dr_chain[j+1];

	      /* data_ref = permute_even (first_data_ref, second_data_ref);  */
	      data_ref = make_temp_ssa_name (vectype, NULL, "vect_perm_even");
	      perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR,
					       first_vect, second_vect,
					       perm_mask_even);
	      vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
	      (*result_chain)[j/2] = data_ref;

	      /* data_ref = permute_odd (first_data_ref, second_data_ref);  */
	      data_ref = make_temp_ssa_name (vectype, NULL, "vect_perm_odd");
	      perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR,
					       first_vect, second_vect,
					       perm_mask_odd);
	      vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
	      (*result_chain)[j/2+length/2] = data_ref;
	    }
	  memcpy (dr_chain.address (), result_chain->address (),
		  length * sizeof (tree));
	}
    }
}

/* Function vect_shift_permute_load_chain.

   Given a chain of loads in DR_CHAIN of LENGTH 2 or 3, generate
   sequence of stmts to reorder the input data accordingly.
   Return the final references for loads in RESULT_CHAIN.
   Return true if successed, false otherwise.

   E.g., LENGTH is 3 and the scalar type is short, i.e., VF is 8.
   The input is 3 vectors each containing 8 elements.  We assign a
   number to each element, the input sequence is:

   1st vec:   0  1  2  3  4  5  6  7
   2nd vec:   8  9 10 11 12 13 14 15
   3rd vec:  16 17 18 19 20 21 22 23

   The output sequence should be:

   1st vec:  0 3 6  9 12 15 18 21
   2nd vec:  1 4 7 10 13 16 19 22
   3rd vec:  2 5 8 11 14 17 20 23

   We use 3 shuffle instructions and 3 * 3 - 1 shifts to create such output.

   First we shuffle all 3 vectors to get correct elements order:

   1st vec:  ( 0  3  6) ( 1  4  7) ( 2  5)
   2nd vec:  ( 8 11 14) ( 9 12 15) (10 13)
   3rd vec:  (16 19 22) (17 20 23) (18 21)

   Next we unite and shift vector 3 times:

   1st step:
     shift right by 6 the concatenation of:
     "1st vec" and  "2nd vec"
       ( 0  3  6) ( 1  4  7) |( 2  5) _ ( 8 11 14) ( 9 12 15)| (10 13)
     "2nd vec" and  "3rd vec"
       ( 8 11 14) ( 9 12 15) |(10 13) _ (16 19 22) (17 20 23)| (18 21)
     "3rd vec" and  "1st vec"
       (16 19 22) (17 20 23) |(18 21) _ ( 0  3  6) ( 1  4  7)| ( 2  5)
			     | New vectors                   |

     So that now new vectors are:

     1st vec:  ( 2  5) ( 8 11 14) ( 9 12 15)
     2nd vec:  (10 13) (16 19 22) (17 20 23)
     3rd vec:  (18 21) ( 0  3  6) ( 1  4  7)

   2nd step:
     shift right by 5 the concatenation of:
     "1st vec" and  "3rd vec"
       ( 2  5) ( 8 11 14) |( 9 12 15) _ (18 21) ( 0  3  6)| ( 1  4  7)
     "2nd vec" and  "1st vec"
       (10 13) (16 19 22) |(17 20 23) _ ( 2  5) ( 8 11 14)| ( 9 12 15)
     "3rd vec" and  "2nd vec"
       (18 21) ( 0  3  6) |( 1  4  7) _ (10 13) (16 19 22)| (17 20 23)
			  | New vectors                   |

     So that now new vectors are:

     1st vec:  ( 9 12 15) (18 21) ( 0  3  6)
     2nd vec:  (17 20 23) ( 2  5) ( 8 11 14)
     3rd vec:  ( 1  4  7) (10 13) (16 19 22) READY

   3rd step:
     shift right by 5 the concatenation of:
     "1st vec" and  "1st vec"
       ( 9 12 15) (18 21) |( 0  3  6) _ ( 9 12 15) (18 21)| ( 0  3  6)
     shift right by 3 the concatenation of:
     "2nd vec" and  "2nd vec"
               (17 20 23) |( 2  5) ( 8 11 14) _ (17 20 23)| ( 2  5) ( 8 11 14)
			  | New vectors                   |

     So that now all vectors are READY:
     1st vec:  ( 0  3  6) ( 9 12 15) (18 21)
     2nd vec:  ( 2  5) ( 8 11 14) (17 20 23)
     3rd vec:  ( 1  4  7) (10 13) (16 19 22)

   This algorithm is faster than one in vect_permute_load_chain if:
     1.  "shift of a concatination" is faster than general permutation.
	 This is usually so.
     2.  The TARGET machine can't execute vector instructions in parallel.
	 This is because each step of the algorithm depends on previous.
	 The algorithm in vect_permute_load_chain is much more parallel.

   The algorithm is applicable only for LOAD CHAIN LENGTH less than VF.
*/

static bool
vect_shift_permute_load_chain (vec<tree> dr_chain,
			       unsigned int length,
			       stmt_vec_info stmt_info,
			       gimple_stmt_iterator *gsi,
			       vec<tree> *result_chain)
{
  tree vect[3], vect_shift[3], data_ref, first_vect, second_vect;
  tree perm2_mask1, perm2_mask2, perm3_mask;
  tree select_mask, shift1_mask, shift2_mask, shift3_mask, shift4_mask;
  gimple *perm_stmt;

  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  unsigned int i;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);

  unsigned HOST_WIDE_INT nelt, vf;
  if (!TYPE_VECTOR_SUBPARTS (vectype).is_constant (&nelt)
      || !LOOP_VINFO_VECT_FACTOR (loop_vinfo).is_constant (&vf))
    /* Not supported for variable-length vectors.  */
    return false;

  vec_perm_builder sel (nelt, nelt, 1);
  sel.quick_grow (nelt);

  result_chain->quick_grow (length);
  memcpy (result_chain->address (), dr_chain.address (),
	  length * sizeof (tree));

  if (pow2p_hwi (length) && vf > 4)
    {
      unsigned int j, log_length = exact_log2 (length);
      for (i = 0; i < nelt / 2; ++i)
	sel[i] = i * 2;
      for (i = 0; i < nelt / 2; ++i)
	sel[nelt / 2 + i] = i * 2 + 1;
      vec_perm_indices indices (sel, 2, nelt);
      if (!can_vec_perm_const_p (TYPE_MODE (vectype), indices))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "shuffle of 2 fields structure is not \
			      supported by target\n");
	  return false;
	}
      perm2_mask1 = vect_gen_perm_mask_checked (vectype, indices);

      for (i = 0; i < nelt / 2; ++i)
	sel[i] = i * 2 + 1;
      for (i = 0; i < nelt / 2; ++i)
	sel[nelt / 2 + i] = i * 2;
      indices.new_vector (sel, 2, nelt);
      if (!can_vec_perm_const_p (TYPE_MODE (vectype), indices))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "shuffle of 2 fields structure is not \
			      supported by target\n");
	  return false;
	}
      perm2_mask2 = vect_gen_perm_mask_checked (vectype, indices);

      /* Generating permutation constant to shift all elements.
	 For vector length 8 it is {4 5 6 7 8 9 10 11}.  */
      for (i = 0; i < nelt; i++)
	sel[i] = nelt / 2 + i;
      indices.new_vector (sel, 2, nelt);
      if (!can_vec_perm_const_p (TYPE_MODE (vectype), indices))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "shift permutation is not supported by target\n");
	  return false;
	}
      shift1_mask = vect_gen_perm_mask_checked (vectype, indices);

      /* Generating permutation constant to select vector from 2.
	 For vector length 8 it is {0 1 2 3 12 13 14 15}.  */
      for (i = 0; i < nelt / 2; i++)
	sel[i] = i;
      for (i = nelt / 2; i < nelt; i++)
	sel[i] = nelt + i;
      indices.new_vector (sel, 2, nelt);
      if (!can_vec_perm_const_p (TYPE_MODE (vectype), indices))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "select is not supported by target\n");
	  return false;
	}
      select_mask = vect_gen_perm_mask_checked (vectype, indices);

      for (i = 0; i < log_length; i++)
	{
	  for (j = 0; j < length; j += 2)
	    {
	      first_vect = dr_chain[j];
	      second_vect = dr_chain[j + 1];

	      data_ref = make_temp_ssa_name (vectype, NULL, "vect_shuffle2");
	      perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR,
					       first_vect, first_vect,
					       perm2_mask1);
	      vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
	      vect[0] = data_ref;

	      data_ref = make_temp_ssa_name (vectype, NULL, "vect_shuffle2");
	      perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR,
					       second_vect, second_vect,
					       perm2_mask2);
	      vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
	      vect[1] = data_ref;

	      data_ref = make_temp_ssa_name (vectype, NULL, "vect_shift");
	      perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR,
					       vect[0], vect[1], shift1_mask);
	      vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
	      (*result_chain)[j/2 + length/2] = data_ref;

	      data_ref = make_temp_ssa_name (vectype, NULL, "vect_select");
	      perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR,
					       vect[0], vect[1], select_mask);
	      vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
	      (*result_chain)[j/2] = data_ref;
	    }
	  memcpy (dr_chain.address (), result_chain->address (),
		  length * sizeof (tree));
	}
      return true;
    }
  if (length == 3 && vf > 2)
    {
      unsigned int k = 0, l = 0;

      /* Generating permutation constant to get all elements in rigth order.
	 For vector length 8 it is {0 3 6 1 4 7 2 5}.  */
      for (i = 0; i < nelt; i++)
	{
	  if (3 * k + (l % 3) >= nelt)
	    {
	      k = 0;
	      l += (3 - (nelt % 3));
	    }
	  sel[i] = 3 * k + (l % 3);
	  k++;
	}
      vec_perm_indices indices (sel, 2, nelt);
      if (!can_vec_perm_const_p (TYPE_MODE (vectype), indices))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "shuffle of 3 fields structure is not \
			      supported by target\n");
	  return false;
	}
      perm3_mask = vect_gen_perm_mask_checked (vectype, indices);

      /* Generating permutation constant to shift all elements.
	 For vector length 8 it is {6 7 8 9 10 11 12 13}.  */
      for (i = 0; i < nelt; i++)
	sel[i] = 2 * (nelt / 3) + (nelt % 3) + i;
      indices.new_vector (sel, 2, nelt);
      if (!can_vec_perm_const_p (TYPE_MODE (vectype), indices))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "shift permutation is not supported by target\n");
	  return false;
	}
      shift1_mask = vect_gen_perm_mask_checked (vectype, indices);

      /* Generating permutation constant to shift all elements.
	 For vector length 8 it is {5 6 7 8 9 10 11 12}.  */
      for (i = 0; i < nelt; i++)
	sel[i] = 2 * (nelt / 3) + 1 + i;
      indices.new_vector (sel, 2, nelt);
      if (!can_vec_perm_const_p (TYPE_MODE (vectype), indices))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "shift permutation is not supported by target\n");
	  return false;
	}
      shift2_mask = vect_gen_perm_mask_checked (vectype, indices);

      /* Generating permutation constant to shift all elements.
	 For vector length 8 it is {3 4 5 6 7 8 9 10}.  */
      for (i = 0; i < nelt; i++)
	sel[i] = (nelt / 3) + (nelt % 3) / 2 + i;
      indices.new_vector (sel, 2, nelt);
      if (!can_vec_perm_const_p (TYPE_MODE (vectype), indices))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "shift permutation is not supported by target\n");
	  return false;
	}
      shift3_mask = vect_gen_perm_mask_checked (vectype, indices);

      /* Generating permutation constant to shift all elements.
	 For vector length 8 it is {5 6 7 8 9 10 11 12}.  */
      for (i = 0; i < nelt; i++)
	sel[i] = 2 * (nelt / 3) + (nelt % 3) / 2 + i;
      indices.new_vector (sel, 2, nelt);
      if (!can_vec_perm_const_p (TYPE_MODE (vectype), indices))
	{
	  if (dump_enabled_p ())
	    dump_printf_loc (MSG_MISSED_OPTIMIZATION, vect_location,
			     "shift permutation is not supported by target\n");
	  return false;
	}
      shift4_mask = vect_gen_perm_mask_checked (vectype, indices);

      for (k = 0; k < 3; k++)
	{
	  data_ref = make_temp_ssa_name (vectype, NULL, "vect_shuffle3");
	  perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR,
					   dr_chain[k], dr_chain[k],
					   perm3_mask);
	  vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
	  vect[k] = data_ref;
	}

      for (k = 0; k < 3; k++)
	{
	  data_ref = make_temp_ssa_name (vectype, NULL, "vect_shift1");
	  perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR,
					   vect[k % 3], vect[(k + 1) % 3],
					   shift1_mask);
	  vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
	  vect_shift[k] = data_ref;
	}

      for (k = 0; k < 3; k++)
	{
	  data_ref = make_temp_ssa_name (vectype, NULL, "vect_shift2");
	  perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR,
					   vect_shift[(4 - k) % 3],
					   vect_shift[(3 - k) % 3],
					   shift2_mask);
	  vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
	  vect[k] = data_ref;
	}

      (*result_chain)[3 - (nelt % 3)] = vect[2];

      data_ref = make_temp_ssa_name (vectype, NULL, "vect_shift3");
      perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR, vect[0],
				       vect[0], shift3_mask);
      vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
      (*result_chain)[nelt % 3] = data_ref;

      data_ref = make_temp_ssa_name (vectype, NULL, "vect_shift4");
      perm_stmt = gimple_build_assign (data_ref, VEC_PERM_EXPR, vect[1],
				       vect[1], shift4_mask);
      vect_finish_stmt_generation (stmt_info, perm_stmt, gsi);
      (*result_chain)[0] = data_ref;
      return true;
    }
  return false;
}

/* Function vect_transform_grouped_load.

   Given a chain of input interleaved data-refs (in DR_CHAIN), build statements
   to perform their permutation and ascribe the result vectorized statements to
   the scalar statements.
*/

void
vect_transform_grouped_load (stmt_vec_info stmt_info, vec<tree> dr_chain,
			     int size, gimple_stmt_iterator *gsi)
{
  machine_mode mode;
  vec<tree> result_chain = vNULL;

  /* DR_CHAIN contains input data-refs that are a part of the interleaving.
     RESULT_CHAIN is the output of vect_permute_load_chain, it contains permuted
     vectors, that are ready for vector computation.  */
  result_chain.create (size);

  /* If reassociation width for vector type is 2 or greater target machine can
     execute 2 or more vector instructions in parallel.  Otherwise try to
     get chain for loads group using vect_shift_permute_load_chain.  */
  mode = TYPE_MODE (STMT_VINFO_VECTYPE (stmt_info));
  if (targetm.sched.reassociation_width (VEC_PERM_EXPR, mode) > 1
      || pow2p_hwi (size)
      || !vect_shift_permute_load_chain (dr_chain, size, stmt_info,
					 gsi, &result_chain))
    vect_permute_load_chain (dr_chain, size, stmt_info, gsi, &result_chain);
  vect_record_grouped_load_vectors (stmt_info, result_chain);
  result_chain.release ();
}

/* RESULT_CHAIN contains the output of a group of grouped loads that were
   generated as part of the vectorization of STMT_INFO.  Assign the statement
   for each vector to the associated scalar statement.  */

void
vect_record_grouped_load_vectors (stmt_vec_info stmt_info,
				  vec<tree> result_chain)
{
  vec_info *vinfo = stmt_info->vinfo;
  stmt_vec_info first_stmt_info = DR_GROUP_FIRST_ELEMENT (stmt_info);
  unsigned int i, gap_count;
  tree tmp_data_ref;

  /* Put a permuted data-ref in the VECTORIZED_STMT field.
     Since we scan the chain starting from it's first node, their order
     corresponds the order of data-refs in RESULT_CHAIN.  */
  stmt_vec_info next_stmt_info = first_stmt_info;
  gap_count = 1;
  FOR_EACH_VEC_ELT (result_chain, i, tmp_data_ref)
    {
      if (!next_stmt_info)
	break;

      /* Skip the gaps.  Loads created for the gaps will be removed by dead
       code elimination pass later.  No need to check for the first stmt in
       the group, since it always exists.
       DR_GROUP_GAP is the number of steps in elements from the previous
       access (if there is no gap DR_GROUP_GAP is 1).  We skip loads that
       correspond to the gaps.  */
      if (next_stmt_info != first_stmt_info
	  && gap_count < DR_GROUP_GAP (next_stmt_info))
      {
        gap_count++;
        continue;
      }

      while (next_stmt_info)
        {
	  stmt_vec_info new_stmt_info = vinfo->lookup_def (tmp_data_ref);
	  /* We assume that if VEC_STMT is not NULL, this is a case of multiple
	     copies, and we put the new vector statement in the first available
	     RELATED_STMT.  */
	  if (!STMT_VINFO_VEC_STMT (next_stmt_info))
	    STMT_VINFO_VEC_STMT (next_stmt_info) = new_stmt_info;
	  else
            {
	      if (!DR_GROUP_SAME_DR_STMT (next_stmt_info))
                {
		  stmt_vec_info prev_stmt_info
		    = STMT_VINFO_VEC_STMT (next_stmt_info);
		  stmt_vec_info rel_stmt_info
		    = STMT_VINFO_RELATED_STMT (prev_stmt_info);
		  while (rel_stmt_info)
		    {
		      prev_stmt_info = rel_stmt_info;
		      rel_stmt_info = STMT_VINFO_RELATED_STMT (rel_stmt_info);
		    }

		  STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt_info;
                }
            }

	  next_stmt_info = DR_GROUP_NEXT_ELEMENT (next_stmt_info);
	  gap_count = 1;
	  /* If NEXT_STMT_INFO accesses the same DR as the previous statement,
	     put the same TMP_DATA_REF as its vectorized statement; otherwise
	     get the next data-ref from RESULT_CHAIN.  */
	  if (!next_stmt_info || !DR_GROUP_SAME_DR_STMT (next_stmt_info))
	    break;
        }
    }
}

/* Function vect_force_dr_alignment_p.

   Returns whether the alignment of a DECL can be forced to be aligned
   on ALIGNMENT bit boundary.  */

bool
vect_can_force_dr_alignment_p (const_tree decl, unsigned int alignment)
{
  if (!VAR_P (decl))
    return false;

  if (decl_in_symtab_p (decl)
      && !symtab_node::get (decl)->can_increase_alignment_p ())
    return false;

  if (TREE_STATIC (decl))
    return (alignment <= MAX_OFILE_ALIGNMENT);
  else
    return (alignment <= MAX_STACK_ALIGNMENT);
}


/* Return whether the data reference DR_INFO is supported with respect to its
   alignment.
   If CHECK_ALIGNED_ACCESSES is TRUE, check if the access is supported even
   it is aligned, i.e., check if it is possible to vectorize it with different
   alignment.  */

enum dr_alignment_support
vect_supportable_dr_alignment (dr_vec_info *dr_info,
                               bool check_aligned_accesses)
{
  data_reference *dr = dr_info->dr;
  stmt_vec_info stmt_info = dr_info->stmt;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  machine_mode mode = TYPE_MODE (vectype);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *vect_loop = NULL;
  bool nested_in_vect_loop = false;

  if (aligned_access_p (dr_info) && !check_aligned_accesses)
    return dr_aligned;

  /* For now assume all conditional loads/stores support unaligned
     access without any special code.  */
  if (gcall *stmt = dyn_cast <gcall *> (stmt_info->stmt))
    if (gimple_call_internal_p (stmt)
	&& (gimple_call_internal_fn (stmt) == IFN_MASK_LOAD
	    || gimple_call_internal_fn (stmt) == IFN_MASK_STORE))
      return dr_unaligned_supported;

  if (loop_vinfo)
    {
      vect_loop = LOOP_VINFO_LOOP (loop_vinfo);
      nested_in_vect_loop = nested_in_vect_loop_p (vect_loop, stmt_info);
    }

  /* Possibly unaligned access.  */

  /* We can choose between using the implicit realignment scheme (generating
     a misaligned_move stmt) and the explicit realignment scheme (generating
     aligned loads with a REALIGN_LOAD).  There are two variants to the
     explicit realignment scheme: optimized, and unoptimized.
     We can optimize the realignment only if the step between consecutive
     vector loads is equal to the vector size.  Since the vector memory
     accesses advance in steps of VS (Vector Size) in the vectorized loop, it
     is guaranteed that the misalignment amount remains the same throughout the
     execution of the vectorized loop.  Therefore, we can create the
     "realignment token" (the permutation mask that is passed to REALIGN_LOAD)
     at the loop preheader.

     However, in the case of outer-loop vectorization, when vectorizing a
     memory access in the inner-loop nested within the LOOP that is now being
     vectorized, while it is guaranteed that the misalignment of the
     vectorized memory access will remain the same in different outer-loop
     iterations, it is *not* guaranteed that is will remain the same throughout
     the execution of the inner-loop.  This is because the inner-loop advances
     with the original scalar step (and not in steps of VS).  If the inner-loop
     step happens to be a multiple of VS, then the misalignment remains fixed
     and we can use the optimized realignment scheme.  For example:

      for (i=0; i<N; i++)
        for (j=0; j<M; j++)
          s += a[i+j];

     When vectorizing the i-loop in the above example, the step between
     consecutive vector loads is 1, and so the misalignment does not remain
     fixed across the execution of the inner-loop, and the realignment cannot
     be optimized (as illustrated in the following pseudo vectorized loop):

      for (i=0; i<N; i+=4)
        for (j=0; j<M; j++){
          vs += vp[i+j]; // misalignment of &vp[i+j] is {0,1,2,3,0,1,2,3,...}
                         // when j is {0,1,2,3,4,5,6,7,...} respectively.
                         // (assuming that we start from an aligned address).
          }

     We therefore have to use the unoptimized realignment scheme:

      for (i=0; i<N; i+=4)
          for (j=k; j<M; j+=4)
          vs += vp[i+j]; // misalignment of &vp[i+j] is always k (assuming
                           // that the misalignment of the initial address is
                           // 0).

     The loop can then be vectorized as follows:

      for (k=0; k<4; k++){
        rt = get_realignment_token (&vp[k]);
        for (i=0; i<N; i+=4){
          v1 = vp[i+k];
          for (j=k; j<M; j+=4){
            v2 = vp[i+j+VS-1];
            va = REALIGN_LOAD <v1,v2,rt>;
            vs += va;
            v1 = v2;
          }
        }
    } */

  if (DR_IS_READ (dr))
    {
      bool is_packed = false;
      tree type = (TREE_TYPE (DR_REF (dr)));

      if (optab_handler (vec_realign_load_optab, mode) != CODE_FOR_nothing
	  && (!targetm.vectorize.builtin_mask_for_load
	      || targetm.vectorize.builtin_mask_for_load ()))
	{
	  tree vectype = STMT_VINFO_VECTYPE (stmt_info);

	  /* If we are doing SLP then the accesses need not have the
	     same alignment, instead it depends on the SLP group size.  */
	  if (loop_vinfo
	      && STMT_SLP_TYPE (stmt_info)
	      && !multiple_p (LOOP_VINFO_VECT_FACTOR (loop_vinfo)
			      * (DR_GROUP_SIZE
				 (DR_GROUP_FIRST_ELEMENT (stmt_info))),
			      TYPE_VECTOR_SUBPARTS (vectype)))
	    ;
	  else if (!loop_vinfo
		   || (nested_in_vect_loop
		       && maybe_ne (TREE_INT_CST_LOW (DR_STEP (dr)),
				    GET_MODE_SIZE (TYPE_MODE (vectype)))))
	    return dr_explicit_realign;
	  else
	    return dr_explicit_realign_optimized;
	}
      if (!known_alignment_for_access_p (dr_info))
	is_packed = not_size_aligned (DR_REF (dr));

      if (targetm.vectorize.support_vector_misalignment
	    (mode, type, DR_MISALIGNMENT (dr_info), is_packed))
	/* Can't software pipeline the loads, but can at least do them.  */
	return dr_unaligned_supported;
    }
  else
    {
      bool is_packed = false;
      tree type = (TREE_TYPE (DR_REF (dr)));

      if (!known_alignment_for_access_p (dr_info))
	is_packed = not_size_aligned (DR_REF (dr));

     if (targetm.vectorize.support_vector_misalignment
	   (mode, type, DR_MISALIGNMENT (dr_info), is_packed))
       return dr_unaligned_supported;
    }

  /* Unsupported.  */
  return dr_unaligned_unsupported;
}
