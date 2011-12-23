/* Data References Analysis and Manipulation Utilities for Vectorization.
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.
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
#include "tm.h"
#include "ggc.h"
#include "tree.h"
#include "tm_p.h"
#include "target.h"
#include "basic-block.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "cfgloop.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"
#include "diagnostic-core.h"

/* Need to include rtl.h, expr.h, etc. for optabs.  */
#include "expr.h"
#include "optabs.h"

/* Return true if load- or store-lanes optab OPTAB is implemented for
   COUNT vectors of type VECTYPE.  NAME is the name of OPTAB.  */

static bool
vect_lanes_optab_supported_p (const char *name, convert_optab optab,
			      tree vectype, unsigned HOST_WIDE_INT count)
{
  enum machine_mode mode, array_mode;
  bool limit_p;

  mode = TYPE_MODE (vectype);
  limit_p = !targetm.array_mode_supported_p (mode, count);
  array_mode = mode_for_size (count * GET_MODE_BITSIZE (mode),
			      MODE_INT, limit_p);

  if (array_mode == BLKmode)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "no array mode for %s[" HOST_WIDE_INT_PRINT_DEC "]",
		 GET_MODE_NAME (mode), count);
      return false;
    }

  if (convert_optab_handler (optab, array_mode, mode) == CODE_FOR_nothing)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "cannot use %s<%s><%s>",
		 name, GET_MODE_NAME (array_mode), GET_MODE_NAME (mode));
      return false;
    }

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "can use %s<%s><%s>",
	     name, GET_MODE_NAME (array_mode), GET_MODE_NAME (mode));

  return true;
}


/* Return the smallest scalar part of STMT.
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
vect_get_smallest_scalar_type (gimple stmt, HOST_WIDE_INT *lhs_size_unit,
                               HOST_WIDE_INT *rhs_size_unit)
{
  tree scalar_type = gimple_expr_type (stmt);
  HOST_WIDE_INT lhs, rhs;

  lhs = rhs = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (scalar_type));

  if (is_gimple_assign (stmt)
      && (gimple_assign_cast_p (stmt)
          || gimple_assign_rhs_code (stmt) == WIDEN_MULT_EXPR
          || gimple_assign_rhs_code (stmt) == FLOAT_EXPR))
    {
      tree rhs_type = TREE_TYPE (gimple_assign_rhs1 (stmt));

      rhs = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (rhs_type));
      if (rhs < lhs)
        scalar_type = rhs_type;
    }

  *lhs_size_unit = lhs;
  *rhs_size_unit = rhs;
  return scalar_type;
}


/* Find the place of the data-ref in STMT in the interleaving chain that starts
   from FIRST_STMT.  Return -1 if the data-ref is not a part of the chain.  */

int
vect_get_place_in_interleaving_chain (gimple stmt, gimple first_stmt)
{
  gimple next_stmt = first_stmt;
  int result = 0;

  if (first_stmt != GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    return -1;

  while (next_stmt && next_stmt != stmt)
    {
      result++;
      next_stmt = GROUP_NEXT_ELEMENT (vinfo_for_stmt (next_stmt));
    }

  if (next_stmt)
    return result;
  else
    return -1;
}


/* Function vect_insert_into_interleaving_chain.

   Insert DRA into the interleaving chain of DRB according to DRA's INIT.  */

static void
vect_insert_into_interleaving_chain (struct data_reference *dra,
				     struct data_reference *drb)
{
  gimple prev, next;
  tree next_init;
  stmt_vec_info stmtinfo_a = vinfo_for_stmt (DR_STMT (dra));
  stmt_vec_info stmtinfo_b = vinfo_for_stmt (DR_STMT (drb));

  prev = GROUP_FIRST_ELEMENT (stmtinfo_b);
  next = GROUP_NEXT_ELEMENT (vinfo_for_stmt (prev));
  while (next)
    {
      next_init = DR_INIT (STMT_VINFO_DATA_REF (vinfo_for_stmt (next)));
      if (tree_int_cst_compare (next_init, DR_INIT (dra)) > 0)
	{
	  /* Insert here.  */
	  GROUP_NEXT_ELEMENT (vinfo_for_stmt (prev)) = DR_STMT (dra);
	  GROUP_NEXT_ELEMENT (stmtinfo_a) = next;
	  return;
	}
      prev = next;
      next = GROUP_NEXT_ELEMENT (vinfo_for_stmt (prev));
    }

  /* We got to the end of the list. Insert here.  */
  GROUP_NEXT_ELEMENT (vinfo_for_stmt (prev)) = DR_STMT (dra);
  GROUP_NEXT_ELEMENT (stmtinfo_a) = NULL;
}


/* Function vect_update_interleaving_chain.

   For two data-refs DRA and DRB that are a part of a chain interleaved data
   accesses, update the interleaving chain.  DRB's INIT is smaller than DRA's.

   There are four possible cases:
   1. New stmts - both DRA and DRB are not a part of any chain:
      FIRST_DR = DRB
      NEXT_DR (DRB) = DRA
   2. DRB is a part of a chain and DRA is not:
      no need to update FIRST_DR
      no need to insert DRB
      insert DRA according to init
   3. DRA is a part of a chain and DRB is not:
      if (init of FIRST_DR > init of DRB)
          FIRST_DR = DRB
	  NEXT(FIRST_DR) = previous FIRST_DR
      else
          insert DRB according to its init
   4. both DRA and DRB are in some interleaving chains:
      choose the chain with the smallest init of FIRST_DR
      insert the nodes of the second chain into the first one.  */

static void
vect_update_interleaving_chain (struct data_reference *drb,
				struct data_reference *dra)
{
  stmt_vec_info stmtinfo_a = vinfo_for_stmt (DR_STMT (dra));
  stmt_vec_info stmtinfo_b = vinfo_for_stmt (DR_STMT (drb));
  tree next_init, init_dra_chain, init_drb_chain;
  gimple first_a, first_b;
  tree node_init;
  gimple node, prev, next, first_stmt;

  /* 1. New stmts - both DRA and DRB are not a part of any chain.   */
  if (!GROUP_FIRST_ELEMENT (stmtinfo_a) && !GROUP_FIRST_ELEMENT (stmtinfo_b))
    {
      GROUP_FIRST_ELEMENT (stmtinfo_a) = DR_STMT (drb);
      GROUP_FIRST_ELEMENT (stmtinfo_b) = DR_STMT (drb);
      GROUP_NEXT_ELEMENT (stmtinfo_b) = DR_STMT (dra);
      return;
    }

  /* 2. DRB is a part of a chain and DRA is not.  */
  if (!GROUP_FIRST_ELEMENT (stmtinfo_a) && GROUP_FIRST_ELEMENT (stmtinfo_b))
    {
      GROUP_FIRST_ELEMENT (stmtinfo_a) = GROUP_FIRST_ELEMENT (stmtinfo_b);
      /* Insert DRA into the chain of DRB.  */
      vect_insert_into_interleaving_chain (dra, drb);
      return;
    }

  /* 3. DRA is a part of a chain and DRB is not.  */
  if (GROUP_FIRST_ELEMENT (stmtinfo_a) && !GROUP_FIRST_ELEMENT (stmtinfo_b))
    {
      gimple old_first_stmt = GROUP_FIRST_ELEMENT (stmtinfo_a);
      tree init_old = DR_INIT (STMT_VINFO_DATA_REF (vinfo_for_stmt (
							      old_first_stmt)));
      gimple tmp;

      if (tree_int_cst_compare (init_old, DR_INIT (drb)) > 0)
	{
	  /* DRB's init is smaller than the init of the stmt previously marked
	     as the first stmt of the interleaving chain of DRA.  Therefore, we
	     update FIRST_STMT and put DRB in the head of the list.  */
	  GROUP_FIRST_ELEMENT (stmtinfo_b) = DR_STMT (drb);
	  GROUP_NEXT_ELEMENT (stmtinfo_b) = old_first_stmt;

	  /* Update all the stmts in the list to point to the new FIRST_STMT.  */
	  tmp = old_first_stmt;
	  while (tmp)
	    {
	      GROUP_FIRST_ELEMENT (vinfo_for_stmt (tmp)) = DR_STMT (drb);
	      tmp = GROUP_NEXT_ELEMENT (vinfo_for_stmt (tmp));
	    }
	}
      else
	{
	  /* Insert DRB in the list of DRA.  */
	  vect_insert_into_interleaving_chain (drb, dra);
	  GROUP_FIRST_ELEMENT (stmtinfo_b) = GROUP_FIRST_ELEMENT (stmtinfo_a);
	}
      return;
    }

  /* 4. both DRA and DRB are in some interleaving chains.  */
  first_a = GROUP_FIRST_ELEMENT (stmtinfo_a);
  first_b = GROUP_FIRST_ELEMENT (stmtinfo_b);
  if (first_a == first_b)
    return;
  init_dra_chain = DR_INIT (STMT_VINFO_DATA_REF (vinfo_for_stmt (first_a)));
  init_drb_chain = DR_INIT (STMT_VINFO_DATA_REF (vinfo_for_stmt (first_b)));

  if (tree_int_cst_compare (init_dra_chain, init_drb_chain) > 0)
    {
      /* Insert the nodes of DRA chain into the DRB chain.
	 After inserting a node, continue from this node of the DRB chain (don't
         start from the beginning.  */
      node = GROUP_FIRST_ELEMENT (stmtinfo_a);
      prev = GROUP_FIRST_ELEMENT (stmtinfo_b);
      first_stmt = first_b;
    }
  else
    {
      /* Insert the nodes of DRB chain into the DRA chain.
	 After inserting a node, continue from this node of the DRA chain (don't
         start from the beginning.  */
      node = GROUP_FIRST_ELEMENT (stmtinfo_b);
      prev = GROUP_FIRST_ELEMENT (stmtinfo_a);
      first_stmt = first_a;
    }

  while (node)
    {
      node_init = DR_INIT (STMT_VINFO_DATA_REF (vinfo_for_stmt (node)));
      next = GROUP_NEXT_ELEMENT (vinfo_for_stmt (prev));
      while (next)
	{
	  next_init = DR_INIT (STMT_VINFO_DATA_REF (vinfo_for_stmt (next)));
	  if (tree_int_cst_compare (next_init, node_init) > 0)
	    {
	      /* Insert here.  */
	      GROUP_NEXT_ELEMENT (vinfo_for_stmt (prev)) = node;
	      GROUP_NEXT_ELEMENT (vinfo_for_stmt (node)) = next;
	      prev = node;
	      break;
	    }
	  prev = next;
	  next = GROUP_NEXT_ELEMENT (vinfo_for_stmt (prev));
	}
      if (!next)
	{
	  /* We got to the end of the list. Insert here.  */
	  GROUP_NEXT_ELEMENT (vinfo_for_stmt (prev)) = node;
	  GROUP_NEXT_ELEMENT (vinfo_for_stmt (node)) = NULL;
	  prev = node;
	}
      GROUP_FIRST_ELEMENT (vinfo_for_stmt (node)) = first_stmt;
      node = GROUP_NEXT_ELEMENT (vinfo_for_stmt (node));
    }
}

/* Check dependence between DRA and DRB for basic block vectorization.
   If the accesses share same bases and offsets, we can compare their initial
   constant offsets to decide whether they differ or not.  In case of a read-
   write dependence we check that the load is before the store to ensure that
   vectorization will not change the order of the accesses.  */

static bool
vect_drs_dependent_in_basic_block (struct data_reference *dra,
                                   struct data_reference *drb)
{
  HOST_WIDE_INT type_size_a, type_size_b, init_a, init_b;
  gimple earlier_stmt;

  /* We only call this function for pairs of loads and stores, but we verify
     it here.  */
  if (DR_IS_READ (dra) == DR_IS_READ (drb))
    {
      if (DR_IS_READ (dra))
        return false;
      else
        return true;
    }

  /* Check that the data-refs have same bases and offsets.  If not, we can't
     determine if they are dependent.  */
  if (!operand_equal_p (DR_BASE_ADDRESS (dra), DR_BASE_ADDRESS (drb), 0)
      || !dr_equal_offsets_p (dra, drb))
    return true;

  /* Check the types.  */
  type_size_a = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dra))));
  type_size_b = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (drb))));

  if (type_size_a != type_size_b
      || !types_compatible_p (TREE_TYPE (DR_REF (dra)),
                              TREE_TYPE (DR_REF (drb))))
    return true;

  init_a = TREE_INT_CST_LOW (DR_INIT (dra));
  init_b = TREE_INT_CST_LOW (DR_INIT (drb));

  /* Two different locations - no dependence.  */
  if (init_a != init_b)
    return false;

  /* We have a read-write dependence.  Check that the load is before the store.
     When we vectorize basic blocks, vector load can be only before 
     corresponding scalar load, and vector store can be only after its
     corresponding scalar store.  So the order of the acceses is preserved in
     case the load is before the store.  */
  earlier_stmt = get_earlier_stmt (DR_STMT (dra), DR_STMT (drb));   
  if (DR_IS_READ (STMT_VINFO_DATA_REF (vinfo_for_stmt (earlier_stmt))))
    return false;

  return true;
}


/* Function vect_check_interleaving.

   Check if DRA and DRB are a part of interleaving.  In case they are, insert
   DRA and DRB in an interleaving chain.  */

static bool
vect_check_interleaving (struct data_reference *dra,
			 struct data_reference *drb)
{
  HOST_WIDE_INT type_size_a, type_size_b, diff_mod_size, step, init_a, init_b;

  /* Check that the data-refs have same first location (except init) and they
     are both either store or load (not load and store).  */
  if (!operand_equal_p (DR_BASE_ADDRESS (dra), DR_BASE_ADDRESS (drb), 0)
      || !dr_equal_offsets_p (dra, drb)
      || !tree_int_cst_compare (DR_INIT (dra), DR_INIT (drb))
      || DR_IS_READ (dra) != DR_IS_READ (drb))
    return false;

  /* Check:
     1. data-refs are of the same type
     2. their steps are equal
     3. the step (if greater than zero) is greater than the difference between
        data-refs' inits.  */
  type_size_a = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dra))));
  type_size_b = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (drb))));

  if (type_size_a != type_size_b
      || tree_int_cst_compare (DR_STEP (dra), DR_STEP (drb))
      || !types_compatible_p (TREE_TYPE (DR_REF (dra)),
                              TREE_TYPE (DR_REF (drb))))
    return false;

  init_a = TREE_INT_CST_LOW (DR_INIT (dra));
  init_b = TREE_INT_CST_LOW (DR_INIT (drb));
  step = TREE_INT_CST_LOW (DR_STEP (dra));

  if (init_a > init_b)
    {
      /* If init_a == init_b + the size of the type * k, we have an interleaving,
	 and DRB is accessed before DRA.  */
      diff_mod_size = (init_a - init_b) % type_size_a;

      if (step && (init_a - init_b) > step)
         return false;

      if (diff_mod_size == 0)
	{
	  vect_update_interleaving_chain (drb, dra);
	  if (vect_print_dump_info (REPORT_DR_DETAILS))
	    {
	      fprintf (vect_dump, "Detected interleaving ");
	      print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
	      fprintf (vect_dump, " and ");
	      print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
	    }
	  return true;
	}
    }
  else
    {
      /* If init_b == init_a + the size of the type * k, we have an
	 interleaving, and DRA is accessed before DRB.  */
      diff_mod_size = (init_b - init_a) % type_size_a;

      if (step && (init_b - init_a) > step)
         return false;

      if (diff_mod_size == 0)
	{
	  vect_update_interleaving_chain (dra, drb);
	  if (vect_print_dump_info (REPORT_DR_DETAILS))
	    {
	      fprintf (vect_dump, "Detected interleaving ");
	      print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
	      fprintf (vect_dump, " and ");
	      print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
	    }
	  return true;
	}
    }

  return false;
}

/* Check if data references pointed by DR_I and DR_J are same or
   belong to same interleaving group.  Return FALSE if drs are
   different, otherwise return TRUE.  */

static bool
vect_same_range_drs (data_reference_p dr_i, data_reference_p dr_j)
{
  gimple stmt_i = DR_STMT (dr_i);
  gimple stmt_j = DR_STMT (dr_j);

  if (operand_equal_p (DR_REF (dr_i), DR_REF (dr_j), 0)
      || (GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt_i))
	    && GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt_j))
	    && (GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt_i))
		== GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt_j)))))
    return true;
  else
    return false;
}

/* If address ranges represented by DDR_I and DDR_J are equal,
   return TRUE, otherwise return FALSE.  */

static bool
vect_vfa_range_equal (ddr_p ddr_i, ddr_p ddr_j)
{
  if ((vect_same_range_drs (DDR_A (ddr_i), DDR_A (ddr_j))
       && vect_same_range_drs (DDR_B (ddr_i), DDR_B (ddr_j)))
      || (vect_same_range_drs (DDR_A (ddr_i), DDR_B (ddr_j))
	  && vect_same_range_drs (DDR_B (ddr_i), DDR_A (ddr_j))))
    return true;
  else
    return false;
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

  if (vect_print_dump_info (REPORT_DR_DETAILS))
    {
      fprintf (vect_dump, "mark for run-time aliasing test between ");
      print_generic_expr (vect_dump, DR_REF (DDR_A (ddr)), TDF_SLIM);
      fprintf (vect_dump, " and ");
      print_generic_expr (vect_dump, DR_REF (DDR_B (ddr)), TDF_SLIM);
    }

  if (optimize_loop_nest_for_size_p (loop))
    {
      if (vect_print_dump_info (REPORT_DR_DETAILS))
	fprintf (vect_dump, "versioning not supported when optimizing for size.");
      return false;
    }

  /* FORNOW: We don't support versioning with outer-loop vectorization.  */
  if (loop->inner)
    {
      if (vect_print_dump_info (REPORT_DR_DETAILS))
	fprintf (vect_dump, "versioning not yet supported for outer-loops.");
      return false;
    }

  VEC_safe_push (ddr_p, heap, LOOP_VINFO_MAY_ALIAS_DDRS (loop_vinfo), ddr);
  return true;
}


/* Function vect_analyze_data_ref_dependence.

   Return TRUE if there (might) exist a dependence between a memory-reference
   DRA and a memory-reference DRB.  When versioning for alias may check a
   dependence at run-time, return FALSE.  Adjust *MAX_VF according to
   the data dependence.  */

static bool
vect_analyze_data_ref_dependence (struct data_dependence_relation *ddr,
                                  loop_vec_info loop_vinfo, int *max_vf)
{
  unsigned int i;
  struct loop *loop = NULL;
  struct data_reference *dra = DDR_A (ddr);
  struct data_reference *drb = DDR_B (ddr);
  stmt_vec_info stmtinfo_a = vinfo_for_stmt (DR_STMT (dra));
  stmt_vec_info stmtinfo_b = vinfo_for_stmt (DR_STMT (drb));
  lambda_vector dist_v;
  unsigned int loop_depth;

  /* Don't bother to analyze statements marked as unvectorizable.  */
  if (!STMT_VINFO_VECTORIZABLE (stmtinfo_a)
      || !STMT_VINFO_VECTORIZABLE (stmtinfo_b))
    return false;

  if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    {
      /* Independent data accesses.  */
      vect_check_interleaving (dra, drb);
      return false;
    }

  if (loop_vinfo)
    loop = LOOP_VINFO_LOOP (loop_vinfo);

  if ((DR_IS_READ (dra) && DR_IS_READ (drb) && loop_vinfo) || dra == drb)
    return false;

  if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
    {
      gimple earlier_stmt;

      if (loop_vinfo)
        {
          if (vect_print_dump_info (REPORT_DR_DETAILS))
            {
              fprintf (vect_dump, "versioning for alias required: "
                                  "can't determine dependence between ");
              print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
              fprintf (vect_dump, " and ");
              print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
            }

          /* Add to list of ddrs that need to be tested at run-time.  */
          return !vect_mark_for_runtime_alias_test (ddr, loop_vinfo);
        }

      /* When vectorizing a basic block unknown depnedence can still mean
	 strided access.  */
      if (vect_check_interleaving (dra, drb))
         return false;

      /* Read-read is OK (we need this check here, after checking for
         interleaving).  */
      if (DR_IS_READ (dra) && DR_IS_READ (drb))
        return false;

      if (vect_print_dump_info (REPORT_DR_DETAILS))
        {
          fprintf (vect_dump, "can't determine dependence between ");
          print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
          fprintf (vect_dump, " and ");
          print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
        }

      /* We do not vectorize basic blocks with write-write dependencies.  */
      if (DR_IS_WRITE (dra) && DR_IS_WRITE (drb))
        return true;

      /* Check that it's not a load-after-store dependence.  */
      earlier_stmt = get_earlier_stmt (DR_STMT (dra), DR_STMT (drb));
      if (DR_IS_WRITE (STMT_VINFO_DATA_REF (vinfo_for_stmt (earlier_stmt))))
        return true;

      return false;
    }

  /* Versioning for alias is not yet supported for basic block SLP, and
     dependence distance is unapplicable, hence, in case of known data
     dependence, basic block vectorization is impossible for now.  */
  if (!loop_vinfo)
    {
      if (dra != drb && vect_check_interleaving (dra, drb))
        return false;

      if (vect_print_dump_info (REPORT_DR_DETAILS))
        {
          fprintf (vect_dump, "determined dependence between ");
          print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
          fprintf (vect_dump, " and ");
          print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
        }

      /* Do not vectorize basic blcoks with write-write dependences.  */
      if (DR_IS_WRITE (dra) && DR_IS_WRITE (drb))
        return true;

      /* Check if this dependence is allowed in basic block vectorization.  */ 
      return vect_drs_dependent_in_basic_block (dra, drb);
    }

  /* Loop-based vectorization and known data dependence.  */
  if (DDR_NUM_DIST_VECTS (ddr) == 0)
    {
      if (vect_print_dump_info (REPORT_DR_DETAILS))
        {
          fprintf (vect_dump, "versioning for alias required: bad dist vector for ");
          print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
          fprintf (vect_dump, " and ");
          print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
        }
      /* Add to list of ddrs that need to be tested at run-time.  */
      return !vect_mark_for_runtime_alias_test (ddr, loop_vinfo);
    }

  loop_depth = index_in_loop_nest (loop->num, DDR_LOOP_NEST (ddr));
  FOR_EACH_VEC_ELT (lambda_vector, DDR_DIST_VECTS (ddr), i, dist_v)
    {
      int dist = dist_v[loop_depth];

      if (vect_print_dump_info (REPORT_DR_DETAILS))
	fprintf (vect_dump, "dependence distance  = %d.", dist);

      if (dist == 0)
	{
	  if (vect_print_dump_info (REPORT_DR_DETAILS))
	    {
	      fprintf (vect_dump, "dependence distance == 0 between ");
	      print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
	      fprintf (vect_dump, " and ");
	      print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
	    }

          /* For interleaving, mark that there is a read-write dependency if
             necessary. We check before that one of the data-refs is store.  */
          if (DR_IS_READ (dra))
            GROUP_READ_WRITE_DEPENDENCE (stmtinfo_a) = true;
	  else
            {
              if (DR_IS_READ (drb))
                GROUP_READ_WRITE_DEPENDENCE (stmtinfo_b) = true;
	    }

	  continue;
	}

      if (dist > 0 && DDR_REVERSED_P (ddr))
	{
	  /* If DDR_REVERSED_P the order of the data-refs in DDR was
	     reversed (to make distance vector positive), and the actual
	     distance is negative.  */
	  if (vect_print_dump_info (REPORT_DR_DETAILS))
	    fprintf (vect_dump, "dependence distance negative.");
	  continue;
	}

      if (abs (dist) >= 2
	  && abs (dist) < *max_vf)
	{
	  /* The dependence distance requires reduction of the maximal
	     vectorization factor.  */
	  *max_vf = abs (dist);
	  if (vect_print_dump_info (REPORT_DR_DETAILS))
	    fprintf (vect_dump, "adjusting maximal vectorization factor to %i",
		     *max_vf);
	}

      if (abs (dist) >= *max_vf)
	{
	  /* Dependence distance does not create dependence, as far as
	     vectorization is concerned, in this case.  */
	  if (vect_print_dump_info (REPORT_DR_DETAILS))
	    fprintf (vect_dump, "dependence distance >= VF.");
	  continue;
	}

      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
	{
	  fprintf (vect_dump, "not vectorized, possible dependence "
    		              "between data-refs ");
	  print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
	  fprintf (vect_dump, " and ");
	  print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
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
                                   bb_vec_info bb_vinfo, int *max_vf)
{
  unsigned int i;
  VEC (ddr_p, heap) *ddrs = NULL;
  struct data_dependence_relation *ddr;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_analyze_dependences ===");

  if (loop_vinfo)
    ddrs = LOOP_VINFO_DDRS (loop_vinfo);
  else
    ddrs = BB_VINFO_DDRS (bb_vinfo);

  FOR_EACH_VEC_ELT (ddr_p, ddrs, i, ddr)
    if (vect_analyze_data_ref_dependence (ddr, loop_vinfo, max_vf))
      return false;

  return true;
}


/* Function vect_compute_data_ref_alignment

   Compute the misalignment of the data reference DR.

   Output:
   1. If during the misalignment computation it is found that the data reference
      cannot be vectorized then false is returned.
   2. DR_MISALIGNMENT (DR) is defined.

   FOR NOW: No analysis is actually performed. Misalignment is calculated
   only for trivial cases. TODO.  */

static bool
vect_compute_data_ref_alignment (struct data_reference *dr)
{
  gimple stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = NULL;
  tree ref = DR_REF (dr);
  tree vectype;
  tree base, base_addr;
  bool base_aligned;
  tree misalign;
  tree aligned_to, alignment;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "vect_compute_data_ref_alignment:");

  if (loop_vinfo)
    loop = LOOP_VINFO_LOOP (loop_vinfo);

  /* Initialize misalignment to unknown.  */
  SET_DR_MISALIGNMENT (dr, -1);

  misalign = DR_INIT (dr);
  aligned_to = DR_ALIGNED_TO (dr);
  base_addr = DR_BASE_ADDRESS (dr);
  vectype = STMT_VINFO_VECTYPE (stmt_info);

  /* In case the dataref is in an inner-loop of the loop that is being
     vectorized (LOOP), we use the base and misalignment information
     relative to the outer-loop (LOOP).  This is ok only if the misalignment
     stays the same throughout the execution of the inner-loop, which is why
     we have to check that the stride of the dataref in the inner-loop evenly
     divides by the vector size.  */
  if (loop && nested_in_vect_loop_p (loop, stmt))
    {
      tree step = DR_STEP (dr);
      HOST_WIDE_INT dr_step = TREE_INT_CST_LOW (step);

      if (dr_step % GET_MODE_SIZE (TYPE_MODE (vectype)) == 0)
        {
          if (vect_print_dump_info (REPORT_ALIGNMENT))
            fprintf (vect_dump, "inner step divides the vector-size.");
	  misalign = STMT_VINFO_DR_INIT (stmt_info);
	  aligned_to = STMT_VINFO_DR_ALIGNED_TO (stmt_info);
	  base_addr = STMT_VINFO_DR_BASE_ADDRESS (stmt_info);
        }
      else
	{
	  if (vect_print_dump_info (REPORT_ALIGNMENT))
	    fprintf (vect_dump, "inner step doesn't divide the vector-size.");
	  misalign = NULL_TREE;
	}
    }

  base = build_fold_indirect_ref (base_addr);
  alignment = ssize_int (TYPE_ALIGN (vectype)/BITS_PER_UNIT);

  if ((aligned_to && tree_int_cst_compare (aligned_to, alignment) < 0)
      || !misalign)
    {
      if (vect_print_dump_info (REPORT_ALIGNMENT))
	{
	  fprintf (vect_dump, "Unknown alignment for access: ");
	  print_generic_expr (vect_dump, base, TDF_SLIM);
	}
      return true;
    }

  if ((DECL_P (base)
       && tree_int_cst_compare (ssize_int (DECL_ALIGN_UNIT (base)),
				alignment) >= 0)
      || (TREE_CODE (base_addr) == SSA_NAME
	  && tree_int_cst_compare (ssize_int (TYPE_ALIGN_UNIT (TREE_TYPE (
						      TREE_TYPE (base_addr)))),
				   alignment) >= 0)
      || (get_pointer_alignment (base_addr) >= TYPE_ALIGN (vectype)))
    base_aligned = true;
  else
    base_aligned = false;

  if (!base_aligned)
    {
      /* Do not change the alignment of global variables if
	 flag_section_anchors is enabled.  */
      if (!vect_can_force_dr_alignment_p (base, TYPE_ALIGN (vectype))
	  || (TREE_STATIC (base) && flag_section_anchors))
	{
	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "can't force alignment of ref: ");
	      print_generic_expr (vect_dump, ref, TDF_SLIM);
	    }
	  return true;
	}

      /* Force the alignment of the decl.
	 NOTE: This is the only change to the code we make during
	 the analysis phase, before deciding to vectorize the loop.  */
      if (vect_print_dump_info (REPORT_DETAILS))
        {
          fprintf (vect_dump, "force alignment of ");
          print_generic_expr (vect_dump, ref, TDF_SLIM);
        }

      DECL_ALIGN (base) = TYPE_ALIGN (vectype);
      DECL_USER_ALIGN (base) = 1;
    }

  /* At this point we assume that the base is aligned.  */
  gcc_assert (base_aligned
	      || (TREE_CODE (base) == VAR_DECL
		  && DECL_ALIGN (base) >= TYPE_ALIGN (vectype)));

  /* If this is a backward running DR then first access in the larger
     vectype actually is N-1 elements before the address in the DR.
     Adjust misalign accordingly.  */
  if (tree_int_cst_compare (DR_STEP (dr), size_zero_node) < 0)
    {
      tree offset = ssize_int (TYPE_VECTOR_SUBPARTS (vectype) - 1);
      /* DR_STEP(dr) is the same as -TYPE_SIZE of the scalar type,
	 otherwise we wouldn't be here.  */
      offset = fold_build2 (MULT_EXPR, ssizetype, offset, DR_STEP (dr));
      /* PLUS because DR_STEP was negative.  */
      misalign = size_binop (PLUS_EXPR, misalign, offset);
    }

  /* Modulo alignment.  */
  misalign = size_binop (FLOOR_MOD_EXPR, misalign, alignment);

  if (!host_integerp (misalign, 1))
    {
      /* Negative or overflowed misalignment value.  */
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "unexpected misalign value");
      return false;
    }

  SET_DR_MISALIGNMENT (dr, TREE_INT_CST_LOW (misalign));

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "misalign = %d bytes of ref ", DR_MISALIGNMENT (dr));
      print_generic_expr (vect_dump, ref, TDF_SLIM);
    }

  return true;
}


/* Function vect_compute_data_refs_alignment

   Compute the misalignment of data references in the loop.
   Return FALSE if a data reference is found that cannot be vectorized.  */

static bool
vect_compute_data_refs_alignment (loop_vec_info loop_vinfo,
                                  bb_vec_info bb_vinfo)
{
  VEC (data_reference_p, heap) *datarefs;
  struct data_reference *dr;
  unsigned int i;

  if (loop_vinfo)
    datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  else
    datarefs = BB_VINFO_DATAREFS (bb_vinfo);

  FOR_EACH_VEC_ELT (data_reference_p, datarefs, i, dr)
    if (STMT_VINFO_VECTORIZABLE (vinfo_for_stmt (DR_STMT (dr)))
        && !vect_compute_data_ref_alignment (dr))
      {
        if (bb_vinfo)
          {
            /* Mark unsupported statement as unvectorizable.  */
            STMT_VINFO_VECTORIZABLE (vinfo_for_stmt (DR_STMT (dr))) = false;
            continue;
          }
        else
          return false;
      }

  return true;
}


/* Function vect_update_misalignment_for_peel

   DR - the data reference whose misalignment is to be adjusted.
   DR_PEEL - the data reference whose misalignment is being made
             zero in the vector loop by the peel.
   NPEEL - the number of iterations in the peel loop if the misalignment
           of DR_PEEL is known at compile time.  */

static void
vect_update_misalignment_for_peel (struct data_reference *dr,
                                   struct data_reference *dr_peel, int npeel)
{
  unsigned int i;
  VEC(dr_p,heap) *same_align_drs;
  struct data_reference *current_dr;
  int dr_size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (dr))));
  int dr_peel_size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (dr_peel))));
  stmt_vec_info stmt_info = vinfo_for_stmt (DR_STMT (dr));
  stmt_vec_info peel_stmt_info = vinfo_for_stmt (DR_STMT (dr_peel));

 /* For interleaved data accesses the step in the loop must be multiplied by
     the size of the interleaving group.  */
  if (STMT_VINFO_STRIDED_ACCESS (stmt_info))
    dr_size *= GROUP_SIZE (vinfo_for_stmt (GROUP_FIRST_ELEMENT (stmt_info)));
  if (STMT_VINFO_STRIDED_ACCESS (peel_stmt_info))
    dr_peel_size *= GROUP_SIZE (peel_stmt_info);

  /* It can be assumed that the data refs with the same alignment as dr_peel
     are aligned in the vector loop.  */
  same_align_drs
    = STMT_VINFO_SAME_ALIGN_REFS (vinfo_for_stmt (DR_STMT (dr_peel)));
  FOR_EACH_VEC_ELT (dr_p, same_align_drs, i, current_dr)
    {
      if (current_dr != dr)
        continue;
      gcc_assert (DR_MISALIGNMENT (dr) / dr_size ==
                  DR_MISALIGNMENT (dr_peel) / dr_peel_size);
      SET_DR_MISALIGNMENT (dr, 0);
      return;
    }

  if (known_alignment_for_access_p (dr)
      && known_alignment_for_access_p (dr_peel))
    {
      bool negative = tree_int_cst_compare (DR_STEP (dr), size_zero_node) < 0;
      int misal = DR_MISALIGNMENT (dr);
      tree vectype = STMT_VINFO_VECTYPE (stmt_info);
      misal += negative ? -npeel * dr_size : npeel * dr_size;
      misal &= GET_MODE_SIZE (TYPE_MODE (vectype)) - 1;
      SET_DR_MISALIGNMENT (dr, misal);
      return;
    }

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "Setting misalignment to -1.");
  SET_DR_MISALIGNMENT (dr, -1);
}


/* Function vect_verify_datarefs_alignment

   Return TRUE if all data references in the loop can be
   handled with respect to alignment.  */

bool
vect_verify_datarefs_alignment (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo)
{
  VEC (data_reference_p, heap) *datarefs;
  struct data_reference *dr;
  enum dr_alignment_support supportable_dr_alignment;
  unsigned int i;

  if (loop_vinfo)
    datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  else
    datarefs = BB_VINFO_DATAREFS (bb_vinfo);

  FOR_EACH_VEC_ELT (data_reference_p, datarefs, i, dr)
    {
      gimple stmt = DR_STMT (dr);
      stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

      /* For interleaving, only the alignment of the first access matters. 
         Skip statements marked as not vectorizable.  */
      if ((STMT_VINFO_STRIDED_ACCESS (stmt_info)
           && GROUP_FIRST_ELEMENT (stmt_info) != stmt)
          || !STMT_VINFO_VECTORIZABLE (stmt_info))
        continue;

      supportable_dr_alignment = vect_supportable_dr_alignment (dr, false);
      if (!supportable_dr_alignment)
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
            {
              if (DR_IS_READ (dr))
                fprintf (vect_dump,
                         "not vectorized: unsupported unaligned load.");
              else
                fprintf (vect_dump,
                         "not vectorized: unsupported unaligned store.");

              print_generic_expr (vect_dump, DR_REF (dr), TDF_SLIM);
            }
          return false;
        }
      if (supportable_dr_alignment != dr_aligned
          && vect_print_dump_info (REPORT_ALIGNMENT))
        fprintf (vect_dump, "Vectorizing an unaligned access.");
    }
  return true;
}


/* Function vector_alignment_reachable_p

   Return true if vector alignment for DR is reachable by peeling
   a few loop iterations.  Return false otherwise.  */

static bool
vector_alignment_reachable_p (struct data_reference *dr)
{
  gimple stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);

  if (STMT_VINFO_STRIDED_ACCESS (stmt_info))
    {
      /* For interleaved access we peel only if number of iterations in
	 the prolog loop ({VF - misalignment}), is a multiple of the
	 number of the interleaved accesses.  */
      int elem_size, mis_in_elements;
      int nelements = TYPE_VECTOR_SUBPARTS (vectype);

      /* FORNOW: handle only known alignment.  */
      if (!known_alignment_for_access_p (dr))
	return false;

      elem_size = GET_MODE_SIZE (TYPE_MODE (vectype)) / nelements;
      mis_in_elements = DR_MISALIGNMENT (dr) / elem_size;

      if ((nelements - mis_in_elements) % GROUP_SIZE (stmt_info))
	return false;
    }

  /* If misalignment is known at the compile time then allow peeling
     only if natural alignment is reachable through peeling.  */
  if (known_alignment_for_access_p (dr) && !aligned_access_p (dr))
    {
      HOST_WIDE_INT elmsize =
		int_cst_value (TYPE_SIZE_UNIT (TREE_TYPE (vectype)));
      if (vect_print_dump_info (REPORT_DETAILS))
	{
	  fprintf (vect_dump, "data size =" HOST_WIDE_INT_PRINT_DEC, elmsize);
	  fprintf (vect_dump, ". misalignment = %d. ", DR_MISALIGNMENT (dr));
	}
      if (DR_MISALIGNMENT (dr) % elmsize)
	{
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "data size does not divide the misalignment.\n");
	  return false;
	}
    }

  if (!known_alignment_for_access_p (dr))
    {
      tree type = (TREE_TYPE (DR_REF (dr)));
      tree ba = DR_BASE_OBJECT (dr);
      bool is_packed = false;

      if (ba)
	is_packed = contains_packed_reference (ba);

      if (compare_tree_int (TYPE_SIZE (type), TYPE_ALIGN (type)) > 0)
	is_packed = true;

      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "Unknown misalignment, is_packed = %d",is_packed);
      if (targetm.vectorize.vector_alignment_reachable (type, is_packed))
	return true;
      else
	return false;
    }

  return true;
}


/* Calculate the cost of the memory access represented by DR.  */

static void
vect_get_data_access_cost (struct data_reference *dr,
                           unsigned int *inside_cost,
                           unsigned int *outside_cost)
{
  gimple stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  int nunits = TYPE_VECTOR_SUBPARTS (STMT_VINFO_VECTYPE (stmt_info));
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  int ncopies = vf / nunits;
  bool supportable_dr_alignment = vect_supportable_dr_alignment (dr, true);

  if (!supportable_dr_alignment)
    *inside_cost = VECT_MAX_COST;
  else
    {
      if (DR_IS_READ (dr))
        vect_get_load_cost (dr, ncopies, true, inside_cost, outside_cost);
      else
        vect_get_store_cost (dr, ncopies, inside_cost);
    }

  if (vect_print_dump_info (REPORT_COST))
    fprintf (vect_dump, "vect_get_data_access_cost: inside_cost = %d, "
             "outside_cost = %d.", *inside_cost, *outside_cost);
}


static hashval_t
vect_peeling_hash (const void *elem)
{
  const struct _vect_peel_info *peel_info;

  peel_info = (const struct _vect_peel_info *) elem;
  return (hashval_t) peel_info->npeel;
}


static int
vect_peeling_hash_eq (const void *elem1, const void *elem2)
{
  const struct _vect_peel_info *a, *b;

  a = (const struct _vect_peel_info *) elem1;
  b = (const struct _vect_peel_info *) elem2;
  return (a->npeel == b->npeel);
}


/* Insert DR into peeling hash table with NPEEL as key.  */

static void
vect_peeling_hash_insert (loop_vec_info loop_vinfo, struct data_reference *dr,
                          int npeel)
{
  struct _vect_peel_info elem, *slot;
  void **new_slot;
  bool supportable_dr_alignment = vect_supportable_dr_alignment (dr, true);

  elem.npeel = npeel;
  slot = (vect_peel_info) htab_find (LOOP_VINFO_PEELING_HTAB (loop_vinfo),
                                     &elem);
  if (slot)
    slot->count++;
  else
    {
      slot = XNEW (struct _vect_peel_info);
      slot->npeel = npeel;
      slot->dr = dr;
      slot->count = 1;
      new_slot = htab_find_slot (LOOP_VINFO_PEELING_HTAB (loop_vinfo), slot,
                                 INSERT);
      *new_slot = slot;
    }

  if (!supportable_dr_alignment && !flag_vect_cost_model)
    slot->count += VECT_MAX_COST;
}


/* Traverse peeling hash table to find peeling option that aligns maximum
   number of data accesses.  */

static int
vect_peeling_hash_get_most_frequent (void **slot, void *data)
{
  vect_peel_info elem = (vect_peel_info) *slot;
  vect_peel_extended_info max = (vect_peel_extended_info) data;

  if (elem->count > max->peel_info.count
      || (elem->count == max->peel_info.count
          && max->peel_info.npeel > elem->npeel))
    {
      max->peel_info.npeel = elem->npeel;
      max->peel_info.count = elem->count;
      max->peel_info.dr = elem->dr;
    }

  return 1;
}


/* Traverse peeling hash table and calculate cost for each peeling option.
   Find the one with the lowest cost.  */

static int
vect_peeling_hash_get_lowest_cost (void **slot, void *data)
{
  vect_peel_info elem = (vect_peel_info) *slot;
  vect_peel_extended_info min = (vect_peel_extended_info) data;
  int save_misalignment, dummy;
  unsigned int inside_cost = 0, outside_cost = 0, i;
  gimple stmt = DR_STMT (elem->dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  VEC (data_reference_p, heap) *datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  struct data_reference *dr;

  FOR_EACH_VEC_ELT (data_reference_p, datarefs, i, dr)
    {
      stmt = DR_STMT (dr);
      stmt_info = vinfo_for_stmt (stmt);
      /* For interleaving, only the alignment of the first access
         matters.  */
      if (STMT_VINFO_STRIDED_ACCESS (stmt_info)
          && GROUP_FIRST_ELEMENT (stmt_info) != stmt)
        continue;

      save_misalignment = DR_MISALIGNMENT (dr);
      vect_update_misalignment_for_peel (dr, elem->dr, elem->npeel);
      vect_get_data_access_cost (dr, &inside_cost, &outside_cost);
      SET_DR_MISALIGNMENT (dr, save_misalignment);
    }

  outside_cost += vect_get_known_peeling_cost (loop_vinfo, elem->npeel, &dummy,
                         vect_get_single_scalar_iteraion_cost (loop_vinfo));

  if (inside_cost < min->inside_cost
      || (inside_cost == min->inside_cost && outside_cost < min->outside_cost))
    {
      min->inside_cost = inside_cost;
      min->outside_cost = outside_cost;
      min->peel_info.dr = elem->dr;
      min->peel_info.npeel = elem->npeel;
    }

  return 1;
}


/* Choose best peeling option by traversing peeling hash table and either
   choosing an option with the lowest cost (if cost model is enabled) or the
   option that aligns as many accesses as possible.  */

static struct data_reference *
vect_peeling_hash_choose_best_peeling (loop_vec_info loop_vinfo,
                                       unsigned int *npeel)
{
   struct _vect_peel_extended_info res;

   res.peel_info.dr = NULL;

   if (flag_vect_cost_model)
     {
       res.inside_cost = INT_MAX;
       res.outside_cost = INT_MAX;
       htab_traverse (LOOP_VINFO_PEELING_HTAB (loop_vinfo),
                      vect_peeling_hash_get_lowest_cost, &res);
     }
   else
     {
       res.peel_info.count = 0;
       htab_traverse (LOOP_VINFO_PEELING_HTAB (loop_vinfo),
                      vect_peeling_hash_get_most_frequent, &res);
     }

   *npeel = res.peel_info.npeel;
   return res.peel_info.dr;
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
  VEC (data_reference_p, heap) *datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  enum dr_alignment_support supportable_dr_alignment;
  struct data_reference *dr0 = NULL, *first_store = NULL;
  struct data_reference *dr;
  unsigned int i, j;
  bool do_peeling = false;
  bool do_versioning = false;
  bool stat;
  gimple stmt;
  stmt_vec_info stmt_info;
  int vect_versioning_for_alias_required;
  unsigned int npeel = 0;
  bool all_misalignments_unknown = true;
  unsigned int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  unsigned possible_npeel_number = 1;
  tree vectype;
  unsigned int nelements, mis, same_align_drs_max = 0;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_enhance_data_refs_alignment ===");

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

  FOR_EACH_VEC_ELT (data_reference_p, datarefs, i, dr)
    {
      stmt = DR_STMT (dr);
      stmt_info = vinfo_for_stmt (stmt);

      if (!STMT_VINFO_RELEVANT (stmt_info))
	continue;

      /* For interleaving, only the alignment of the first access
         matters.  */
      if (STMT_VINFO_STRIDED_ACCESS (stmt_info)
          && GROUP_FIRST_ELEMENT (stmt_info) != stmt)
        continue;

      /* For invariant accesses there is nothing to enhance.  */
      if (integer_zerop (DR_STEP (dr)))
	continue;

      supportable_dr_alignment = vect_supportable_dr_alignment (dr, true);
      do_peeling = vector_alignment_reachable_p (dr);
      if (do_peeling)
        {
          if (known_alignment_for_access_p (dr))
            {
              unsigned int npeel_tmp;
	      bool negative = tree_int_cst_compare (DR_STEP (dr),
						    size_zero_node) < 0;

              /* Save info about DR in the hash table.  */
              if (!LOOP_VINFO_PEELING_HTAB (loop_vinfo))
                LOOP_VINFO_PEELING_HTAB (loop_vinfo) =
                           htab_create (1, vect_peeling_hash,
                                        vect_peeling_hash_eq, free);

              vectype = STMT_VINFO_VECTYPE (stmt_info);
              nelements = TYPE_VECTOR_SUBPARTS (vectype);
              mis = DR_MISALIGNMENT (dr) / GET_MODE_SIZE (TYPE_MODE (
                                                TREE_TYPE (DR_REF (dr))));
              npeel_tmp = (negative
			   ? (mis - nelements) : (nelements - mis))
		  & (nelements - 1);

              /* For multiple types, it is possible that the bigger type access
                 will have more than one peeling option.  E.g., a loop with two
                 types: one of size (vector size / 4), and the other one of
                 size (vector size / 8).  Vectorization factor will 8.  If both
                 access are misaligned by 3, the first one needs one scalar
                 iteration to be aligned, and the second one needs 5.  But the
                 the first one will be aligned also by peeling 5 scalar
                 iterations, and in that case both accesses will be aligned.
                 Hence, except for the immediate peeling amount, we also want
                 to try to add full vector size, while we don't exceed
                 vectorization factor.
                 We do this automtically for cost model, since we calculate cost
                 for every peeling option.  */
              if (!flag_vect_cost_model)
                possible_npeel_number = vf /nelements;

              /* Handle the aligned case. We may decide to align some other
                 access, making DR unaligned.  */
              if (DR_MISALIGNMENT (dr) == 0)
                {
                  npeel_tmp = 0;
                  if (!flag_vect_cost_model)
                    possible_npeel_number++;
                }

              for (j = 0; j < possible_npeel_number; j++)
                {
                  gcc_assert (npeel_tmp <= vf);
                  vect_peeling_hash_insert (loop_vinfo, dr, npeel_tmp);
                  npeel_tmp += nelements;
                }

              all_misalignments_unknown = false;
              /* Data-ref that was chosen for the case that all the
                 misalignments are unknown is not relevant anymore, since we
                 have a data-ref with known alignment.  */
              dr0 = NULL;
            }
          else
            {
              /* If we don't know all the misalignment values, we prefer
                 peeling for data-ref that has maximum number of data-refs
                 with the same alignment, unless the target prefers to align
                 stores over load.  */
              if (all_misalignments_unknown)
                {
                  if (same_align_drs_max  < VEC_length (dr_p,
                                       STMT_VINFO_SAME_ALIGN_REFS (stmt_info))
                      || !dr0)
                    {
                      same_align_drs_max = VEC_length (dr_p,
                                       STMT_VINFO_SAME_ALIGN_REFS (stmt_info));
                      dr0 = dr;
                    }

                  if (!first_store && DR_IS_WRITE (dr))
                    first_store = dr;
                }

              /* If there are both known and unknown misaligned accesses in the
                 loop, we choose peeling amount according to the known
                 accesses.  */


              if (!supportable_dr_alignment)
                {
                  dr0 = dr;
                  if (!first_store && DR_IS_WRITE (dr))
                    first_store = dr;
                }
            }
        }
      else
        {
          if (!aligned_access_p (dr))
            {
              if (vect_print_dump_info (REPORT_DETAILS))
                fprintf (vect_dump, "vector alignment may not be reachable");

              break;
            }
        }
    }

  vect_versioning_for_alias_required
    = LOOP_REQUIRES_VERSIONING_FOR_ALIAS (loop_vinfo);

  /* Temporarily, if versioning for alias is required, we disable peeling
     until we support peeling and versioning.  Often peeling for alignment
     will require peeling for loop-bound, which in turn requires that we
     know how to adjust the loop ivs after the loop.  */
  if (vect_versioning_for_alias_required
      || !vect_can_advance_ivs_p (loop_vinfo)
      || !slpeel_can_duplicate_loop_p (loop, single_exit (loop)))
    do_peeling = false;

  if (do_peeling && all_misalignments_unknown
      && vect_supportable_dr_alignment (dr0, false))
    {

      /* Check if the target requires to prefer stores over loads, i.e., if
         misaligned stores are more expensive than misaligned loads (taking
         drs with same alignment into account).  */
      if (first_store && DR_IS_READ (dr0))
        {
          unsigned int load_inside_cost = 0, load_outside_cost = 0;
          unsigned int store_inside_cost = 0, store_outside_cost = 0;
          unsigned int load_inside_penalty = 0, load_outside_penalty = 0;
          unsigned int store_inside_penalty = 0, store_outside_penalty = 0;

          vect_get_data_access_cost (dr0, &load_inside_cost,
                                     &load_outside_cost);
          vect_get_data_access_cost (first_store, &store_inside_cost,
                                     &store_outside_cost);

          /* Calculate the penalty for leaving FIRST_STORE unaligned (by
             aligning the load DR0).  */
          load_inside_penalty = store_inside_cost;
          load_outside_penalty = store_outside_cost;
          for (i = 0; VEC_iterate (dr_p, STMT_VINFO_SAME_ALIGN_REFS
                                   (vinfo_for_stmt (DR_STMT (first_store))),
                                   i, dr);
               i++)
            if (DR_IS_READ (dr))
              {
                load_inside_penalty += load_inside_cost;
                load_outside_penalty += load_outside_cost;
              }
            else
              {
                load_inside_penalty += store_inside_cost;
                load_outside_penalty += store_outside_cost;
              }

          /* Calculate the penalty for leaving DR0 unaligned (by
             aligning the FIRST_STORE).  */
          store_inside_penalty = load_inside_cost;
          store_outside_penalty = load_outside_cost;
          for (i = 0; VEC_iterate (dr_p, STMT_VINFO_SAME_ALIGN_REFS
                                   (vinfo_for_stmt (DR_STMT (dr0))),
                                   i, dr);
               i++)
            if (DR_IS_READ (dr))
              {
                store_inside_penalty += load_inside_cost;
                store_outside_penalty += load_outside_cost;
              }
            else
              {
                store_inside_penalty += store_inside_cost;
                store_outside_penalty += store_outside_cost;
              }

          if (load_inside_penalty > store_inside_penalty
              || (load_inside_penalty == store_inside_penalty
                  && load_outside_penalty > store_outside_penalty))
            dr0 = first_store;
        }

      /* In case there are only loads with different unknown misalignments, use
         peeling only if it may help to align other accesses in the loop.  */
      if (!first_store && !VEC_length (dr_p, STMT_VINFO_SAME_ALIGN_REFS
                                            (vinfo_for_stmt (DR_STMT (dr0))))
          && vect_supportable_dr_alignment (dr0, false)
              != dr_unaligned_supported)
        do_peeling = false;
    }

  if (do_peeling && !dr0)
    {
      /* Peeling is possible, but there is no data access that is not supported
         unless aligned. So we try to choose the best possible peeling.  */

      /* We should get here only if there are drs with known misalignment.  */
      gcc_assert (!all_misalignments_unknown);

      /* Choose the best peeling from the hash table.  */
      dr0 = vect_peeling_hash_choose_best_peeling (loop_vinfo, &npeel);
      if (!dr0 || !npeel)
        do_peeling = false;
    }

  if (do_peeling)
    {
      stmt = DR_STMT (dr0);
      stmt_info = vinfo_for_stmt (stmt);
      vectype = STMT_VINFO_VECTYPE (stmt_info);
      nelements = TYPE_VECTOR_SUBPARTS (vectype);

      if (known_alignment_for_access_p (dr0))
        {
	  bool negative = tree_int_cst_compare (DR_STEP (dr0),
						size_zero_node) < 0;
          if (!npeel)
            {
              /* Since it's known at compile time, compute the number of
                 iterations in the peeled loop (the peeling factor) for use in
                 updating DR_MISALIGNMENT values.  The peeling factor is the
                 vectorization factor minus the misalignment as an element
                 count.  */
              mis = DR_MISALIGNMENT (dr0);
              mis /= GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (dr0))));
              npeel = ((negative ? mis - nelements : nelements - mis)
		       & (nelements - 1));
            }

	  /* For interleaved data access every iteration accesses all the
	     members of the group, therefore we divide the number of iterations
	     by the group size.  */
	  stmt_info = vinfo_for_stmt (DR_STMT (dr0));
	  if (STMT_VINFO_STRIDED_ACCESS (stmt_info))
	    npeel /= GROUP_SIZE (stmt_info);

          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "Try peeling by %d", npeel);
        }

      /* Ensure that all data refs can be vectorized after the peel.  */
      FOR_EACH_VEC_ELT (data_reference_p, datarefs, i, dr)
        {
          int save_misalignment;

	  if (dr == dr0)
	    continue;

	  stmt = DR_STMT (dr);
	  stmt_info = vinfo_for_stmt (stmt);
	  /* For interleaving, only the alignment of the first access
            matters.  */
	  if (STMT_VINFO_STRIDED_ACCESS (stmt_info)
	      && GROUP_FIRST_ELEMENT (stmt_info) != stmt)
	    continue;

	  save_misalignment = DR_MISALIGNMENT (dr);
	  vect_update_misalignment_for_peel (dr, dr0, npeel);
	  supportable_dr_alignment = vect_supportable_dr_alignment (dr, false);
	  SET_DR_MISALIGNMENT (dr, save_misalignment);

	  if (!supportable_dr_alignment)
	    {
	      do_peeling = false;
	      break;
	    }
	}

      if (do_peeling && known_alignment_for_access_p (dr0) && npeel == 0)
        {
          stat = vect_verify_datarefs_alignment (loop_vinfo, NULL);
          if (!stat)
            do_peeling = false;
          else
            return stat;
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
	  FOR_EACH_VEC_ELT (data_reference_p, datarefs, i, dr)
	    if (dr != dr0)
	      vect_update_misalignment_for_peel (dr, dr0, npeel);

          LOOP_VINFO_UNALIGNED_DR (loop_vinfo) = dr0;
          if (npeel)
            LOOP_PEELING_FOR_ALIGNMENT (loop_vinfo) = npeel;
          else
            LOOP_PEELING_FOR_ALIGNMENT (loop_vinfo) = DR_MISALIGNMENT (dr0);
	  SET_DR_MISALIGNMENT (dr0, 0);
	  if (vect_print_dump_info (REPORT_ALIGNMENT))
            fprintf (vect_dump, "Alignment of access forced using peeling.");

          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "Peeling for alignment will be applied.");

	  stat = vect_verify_datarefs_alignment (loop_vinfo, NULL);
	  gcc_assert (stat);
          return stat;
        }
    }


  /* (2) Versioning to force alignment.  */

  /* Try versioning if:
     1) flag_tree_vect_loop_version is TRUE
     2) optimize loop for speed
     3) there is at least one unsupported misaligned data ref with an unknown
        misalignment, and
     4) all misaligned data refs with a known misalignment are supported, and
     5) the number of runtime alignment checks is within reason.  */

  do_versioning =
	flag_tree_vect_loop_version
	&& optimize_loop_nest_for_speed_p (loop)
	&& (!loop->inner); /* FORNOW */

  if (do_versioning)
    {
      FOR_EACH_VEC_ELT (data_reference_p, datarefs, i, dr)
        {
	  stmt = DR_STMT (dr);
	  stmt_info = vinfo_for_stmt (stmt);

	  /* For interleaving, only the alignment of the first access
	     matters.  */
	  if (aligned_access_p (dr)
	      || (STMT_VINFO_STRIDED_ACCESS (stmt_info)
		  && GROUP_FIRST_ELEMENT (stmt_info) != stmt))
	    continue;

	  supportable_dr_alignment = vect_supportable_dr_alignment (dr, false);

          if (!supportable_dr_alignment)
            {
              gimple stmt;
              int mask;
              tree vectype;

              if (known_alignment_for_access_p (dr)
                  || VEC_length (gimple,
                                 LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo))
                     >= (unsigned) PARAM_VALUE (PARAM_VECT_MAX_VERSION_FOR_ALIGNMENT_CHECKS))
                {
                  do_versioning = false;
                  break;
                }

              stmt = DR_STMT (dr);
              vectype = STMT_VINFO_VECTYPE (vinfo_for_stmt (stmt));
              gcc_assert (vectype);

              /* The rightmost bits of an aligned address must be zeros.
                 Construct the mask needed for this test.  For example,
                 GET_MODE_SIZE for the vector mode V4SI is 16 bytes so the
                 mask must be 15 = 0xf. */
              mask = GET_MODE_SIZE (TYPE_MODE (vectype)) - 1;

              /* FORNOW: use the same mask to test all potentially unaligned
                 references in the loop.  The vectorizer currently supports
                 a single vector size, see the reference to
                 GET_MODE_NUNITS (TYPE_MODE (vectype)) where the
                 vectorization factor is computed.  */
              gcc_assert (!LOOP_VINFO_PTR_MASK (loop_vinfo)
                          || LOOP_VINFO_PTR_MASK (loop_vinfo) == mask);
              LOOP_VINFO_PTR_MASK (loop_vinfo) = mask;
              VEC_safe_push (gimple, heap,
                             LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo),
                             DR_STMT (dr));
            }
        }

      /* Versioning requires at least one misaligned data reference.  */
      if (!LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT (loop_vinfo))
        do_versioning = false;
      else if (!do_versioning)
        VEC_truncate (gimple, LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo), 0);
    }

  if (do_versioning)
    {
      VEC(gimple,heap) *may_misalign_stmts
        = LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo);
      gimple stmt;

      /* It can now be assumed that the data references in the statements
         in LOOP_VINFO_MAY_MISALIGN_STMTS will be aligned in the version
         of the loop being vectorized.  */
      FOR_EACH_VEC_ELT (gimple, may_misalign_stmts, i, stmt)
        {
          stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
          dr = STMT_VINFO_DATA_REF (stmt_info);
	  SET_DR_MISALIGNMENT (dr, 0);
	  if (vect_print_dump_info (REPORT_ALIGNMENT))
            fprintf (vect_dump, "Alignment of access forced using versioning.");
        }

      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "Versioning for alignment will be applied.");

      /* Peeling and versioning can't be done together at this time.  */
      gcc_assert (! (do_peeling && do_versioning));

      stat = vect_verify_datarefs_alignment (loop_vinfo, NULL);
      gcc_assert (stat);
      return stat;
    }

  /* This point is reached if neither peeling nor versioning is being done.  */
  gcc_assert (! (do_peeling || do_versioning));

  stat = vect_verify_datarefs_alignment (loop_vinfo, NULL);
  return stat;
}


/* Function vect_find_same_alignment_drs.

   Update group and alignment relations according to the chosen
   vectorization factor.  */

static void
vect_find_same_alignment_drs (struct data_dependence_relation *ddr,
			      loop_vec_info loop_vinfo)
{
  unsigned int i;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  int vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  struct data_reference *dra = DDR_A (ddr);
  struct data_reference *drb = DDR_B (ddr);
  stmt_vec_info stmtinfo_a = vinfo_for_stmt (DR_STMT (dra));
  stmt_vec_info stmtinfo_b = vinfo_for_stmt (DR_STMT (drb));
  int dra_size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (dra))));
  int drb_size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (drb))));
  lambda_vector dist_v;
  unsigned int loop_depth;

  if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    return;

  if (dra == drb)
    return;

  if (DDR_ARE_DEPENDENT (ddr) == chrec_dont_know)
    return;

  /* Loop-based vectorization and known data dependence.  */
  if (DDR_NUM_DIST_VECTS (ddr) == 0)
    return;

  /* Data-dependence analysis reports a distance vector of zero
     for data-references that overlap only in the first iteration
     but have different sign step (see PR45764).
     So as a sanity check require equal DR_STEP.  */
  if (!operand_equal_p (DR_STEP (dra), DR_STEP (drb), 0))
    return;

  loop_depth = index_in_loop_nest (loop->num, DDR_LOOP_NEST (ddr));
  FOR_EACH_VEC_ELT (lambda_vector, DDR_DIST_VECTS (ddr), i, dist_v)
    {
      int dist = dist_v[loop_depth];

      if (vect_print_dump_info (REPORT_DR_DETAILS))
	fprintf (vect_dump, "dependence distance  = %d.", dist);

      /* Same loop iteration.  */
      if (dist == 0
	  || (dist % vectorization_factor == 0 && dra_size == drb_size))
	{
	  /* Two references with distance zero have the same alignment.  */
	  VEC_safe_push (dr_p, heap, STMT_VINFO_SAME_ALIGN_REFS (stmtinfo_a), drb);
	  VEC_safe_push (dr_p, heap, STMT_VINFO_SAME_ALIGN_REFS (stmtinfo_b), dra);
	  if (vect_print_dump_info (REPORT_ALIGNMENT))
	    fprintf (vect_dump, "accesses have the same alignment.");
	  if (vect_print_dump_info (REPORT_DR_DETAILS))
	    {
	      fprintf (vect_dump, "dependence distance modulo vf == 0 between ");
	      print_generic_expr (vect_dump, DR_REF (dra), TDF_SLIM);
	      fprintf (vect_dump, " and ");
	      print_generic_expr (vect_dump, DR_REF (drb), TDF_SLIM);
	    }
	}
    }
}


/* Function vect_analyze_data_refs_alignment

   Analyze the alignment of the data-references in the loop.
   Return FALSE if a data reference is found that cannot be vectorized.  */

bool
vect_analyze_data_refs_alignment (loop_vec_info loop_vinfo,
                                  bb_vec_info bb_vinfo)
{
  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_analyze_data_refs_alignment ===");

  /* Mark groups of data references with same alignment using
     data dependence information.  */
  if (loop_vinfo)
    {
      VEC (ddr_p, heap) *ddrs = LOOP_VINFO_DDRS (loop_vinfo);
      struct data_dependence_relation *ddr;
      unsigned int i;

      FOR_EACH_VEC_ELT (ddr_p, ddrs, i, ddr)
	vect_find_same_alignment_drs (ddr, loop_vinfo);
    }

  if (!vect_compute_data_refs_alignment (loop_vinfo, bb_vinfo))
    {
      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
	fprintf (vect_dump,
		 "not vectorized: can't calculate alignment for data ref.");
      return false;
    }

  return true;
}


/* Analyze groups of strided accesses: check that DR belongs to a group of
   strided accesses of legal size, step, etc.  Detect gaps, single element
   interleaving, and other special cases. Set strided access info.
   Collect groups of strided stores for further use in SLP analysis.  */

static bool
vect_analyze_group_access (struct data_reference *dr)
{
  tree step = DR_STEP (dr);
  tree scalar_type = TREE_TYPE (DR_REF (dr));
  HOST_WIDE_INT type_size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (scalar_type));
  gimple stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  HOST_WIDE_INT dr_step = TREE_INT_CST_LOW (step);
  HOST_WIDE_INT stride, last_accessed_element = 1;
  bool slp_impossible = false;
  struct loop *loop = NULL;

  if (loop_vinfo)
    loop = LOOP_VINFO_LOOP (loop_vinfo);

  /* For interleaving, STRIDE is STEP counted in elements, i.e., the size of the
     interleaving group (including gaps).  */
  stride = dr_step / type_size;

  /* Not consecutive access is possible only if it is a part of interleaving.  */
  if (!GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)))
    {
      /* Check if it this DR is a part of interleaving, and is a single
	 element of the group that is accessed in the loop.  */

      /* Gaps are supported only for loads. STEP must be a multiple of the type
	 size.  The size of the group must be a power of 2.  */
      if (DR_IS_READ (dr)
	  && (dr_step % type_size) == 0
	  && stride > 0
	  && exact_log2 (stride) != -1)
	{
	  GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)) = stmt;
	  GROUP_SIZE (vinfo_for_stmt (stmt)) = stride;
	  if (vect_print_dump_info (REPORT_DR_DETAILS))
	    {
	      fprintf (vect_dump, "Detected single element interleaving ");
	      print_generic_expr (vect_dump, DR_REF (dr), TDF_SLIM);
	      fprintf (vect_dump, " step ");
	      print_generic_expr (vect_dump, step, TDF_SLIM);
	    }

	  if (loop_vinfo)
	    {
	      if (vect_print_dump_info (REPORT_DETAILS))
		fprintf (vect_dump, "Data access with gaps requires scalar "
				    "epilogue loop");
              if (loop->inner)
                {
                  if (vect_print_dump_info (REPORT_DETAILS))
                    fprintf (vect_dump, "Peeling for outer loop is not"
                                        " supported");
                  return false;
                }

              LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo) = true;
	    }

	  return true;
	}

      if (vect_print_dump_info (REPORT_DETAILS))
        {
 	  fprintf (vect_dump, "not consecutive access ");
          print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
        }

      if (bb_vinfo)
        {
          /* Mark the statement as unvectorizable.  */
          STMT_VINFO_VECTORIZABLE (vinfo_for_stmt (DR_STMT (dr))) = false;
          return true;
        }
    
      return false;
    }

  if (GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)) == stmt)
    {
      /* First stmt in the interleaving chain. Check the chain.  */
      gimple next = GROUP_NEXT_ELEMENT (vinfo_for_stmt (stmt));
      struct data_reference *data_ref = dr;
      unsigned int count = 1;
      tree next_step;
      tree prev_init = DR_INIT (data_ref);
      gimple prev = stmt;
      HOST_WIDE_INT diff, count_in_bytes, gaps = 0;

      while (next)
        {
          /* Skip same data-refs.  In case that two or more stmts share
             data-ref (supported only for loads), we vectorize only the first
             stmt, and the rest get their vectorized loads from the first
             one.  */
          if (!tree_int_cst_compare (DR_INIT (data_ref),
                                     DR_INIT (STMT_VINFO_DATA_REF (
						   vinfo_for_stmt (next)))))
            {
              if (DR_IS_WRITE (data_ref))
                {
                  if (vect_print_dump_info (REPORT_DETAILS))
                    fprintf (vect_dump, "Two store stmts share the same dr.");
                  return false;
                }

              /* Check that there is no load-store dependencies for this loads
                 to prevent a case of load-store-load to the same location.  */
              if (GROUP_READ_WRITE_DEPENDENCE (vinfo_for_stmt (next))
                  || GROUP_READ_WRITE_DEPENDENCE (vinfo_for_stmt (prev)))
                {
                  if (vect_print_dump_info (REPORT_DETAILS))
                    fprintf (vect_dump,
                             "READ_WRITE dependence in interleaving.");
                  return false;
                }

              /* For load use the same data-ref load.  */
              GROUP_SAME_DR_STMT (vinfo_for_stmt (next)) = prev;

              prev = next;
              next = GROUP_NEXT_ELEMENT (vinfo_for_stmt (next));
              continue;
            }

          prev = next;

          /* Check that all the accesses have the same STEP.  */
          next_step = DR_STEP (STMT_VINFO_DATA_REF (vinfo_for_stmt (next)));
          if (tree_int_cst_compare (step, next_step))
            {
              if (vect_print_dump_info (REPORT_DETAILS))
                fprintf (vect_dump, "not consecutive access in interleaving");
              return false;
            }

          data_ref = STMT_VINFO_DATA_REF (vinfo_for_stmt (next));
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
		  if (vect_print_dump_info (REPORT_DETAILS))
		    fprintf (vect_dump, "interleaved store with gaps");
		  return false;
		}

              gaps += diff - 1;
	    }

	  last_accessed_element += diff;

          /* Store the gap from the previous member of the group. If there is no
             gap in the access, GROUP_GAP is always 1.  */
          GROUP_GAP (vinfo_for_stmt (next)) = diff;

          prev_init = DR_INIT (data_ref);
          next = GROUP_NEXT_ELEMENT (vinfo_for_stmt (next));
          /* Count the number of data-refs in the chain.  */
          count++;
        }

      /* COUNT is the number of accesses found, we multiply it by the size of
         the type to get COUNT_IN_BYTES.  */
      count_in_bytes = type_size * count;

      /* Check that the size of the interleaving (including gaps) is not
         greater than STEP.  */
      if (dr_step && dr_step < count_in_bytes + gaps * type_size)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            {
              fprintf (vect_dump, "interleaving size is greater than step for ");
              print_generic_expr (vect_dump, DR_REF (dr), TDF_SLIM);
            }
          return false;
        }

      /* Check that the size of the interleaving is equal to STEP for stores,
         i.e., that there are no gaps.  */
      if (dr_step && dr_step != count_in_bytes)
        {
          if (DR_IS_READ (dr))
            {
              slp_impossible = true;
              /* There is a gap after the last load in the group. This gap is a
                 difference between the stride and the number of elements. When
                 there is no gap, this difference should be 0.  */
              GROUP_GAP (vinfo_for_stmt (stmt)) = stride - count;
            }
          else
            {
              if (vect_print_dump_info (REPORT_DETAILS))
                fprintf (vect_dump, "interleaved store with gaps");
              return false;
            }
        }

      /* Check that STEP is a multiple of type size.  */
      if (dr_step && (dr_step % type_size) != 0)
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            {
              fprintf (vect_dump, "step is not a multiple of type size: step ");
              print_generic_expr (vect_dump, step, TDF_SLIM);
              fprintf (vect_dump, " size ");
              print_generic_expr (vect_dump, TYPE_SIZE_UNIT (scalar_type),
                                  TDF_SLIM);
            }
          return false;
        }

      if (stride == 0)
        stride = count;

      GROUP_SIZE (vinfo_for_stmt (stmt)) = stride;
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "Detected interleaving of size %d", (int)stride);

      /* SLP: create an SLP data structure for every interleaving group of
	 stores for further analysis in vect_analyse_slp.  */
      if (DR_IS_WRITE (dr) && !slp_impossible)
        {
          if (loop_vinfo)
            VEC_safe_push (gimple, heap, LOOP_VINFO_STRIDED_STORES (loop_vinfo),
                           stmt);
          if (bb_vinfo)
            VEC_safe_push (gimple, heap, BB_VINFO_STRIDED_STORES (bb_vinfo),
                           stmt);
        }

      /* There is a gap in the end of the group.  */
      if (stride - last_accessed_element > 0 && loop_vinfo)
	{
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "Data access with gaps requires scalar "
				"epilogue loop");
          if (loop->inner)
            {
              if (vect_print_dump_info (REPORT_DETAILS))
                fprintf (vect_dump, "Peeling for outer loop is not supported");
              return false;
            }

          LOOP_VINFO_PEELING_FOR_GAPS (loop_vinfo) = true;
	}
    }

  return true;
}


/* Analyze the access pattern of the data-reference DR.
   In case of non-consecutive accesses call vect_analyze_group_access() to
   analyze groups of strided accesses.  */

static bool
vect_analyze_data_ref_access (struct data_reference *dr)
{
  tree step = DR_STEP (dr);
  tree scalar_type = TREE_TYPE (DR_REF (dr));
  gimple stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = NULL;
  HOST_WIDE_INT dr_step = TREE_INT_CST_LOW (step);

  if (loop_vinfo)
    loop = LOOP_VINFO_LOOP (loop_vinfo);

  if (loop_vinfo && !step)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "bad data-ref access in loop");
      return false;
    }

  /* Allow invariant loads in loops.  */
  if (loop_vinfo && dr_step == 0)
    {
      GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)) = NULL;
      return DR_IS_READ (dr);
    }

  if (loop && nested_in_vect_loop_p (loop, stmt))
    {
      /* Interleaved accesses are not yet supported within outer-loop
        vectorization for references in the inner-loop.  */
      GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)) = NULL;

      /* For the rest of the analysis we use the outer-loop step.  */
      step = STMT_VINFO_DR_STEP (stmt_info);
      dr_step = TREE_INT_CST_LOW (step);

      if (dr_step == 0)
	{
	  if (vect_print_dump_info (REPORT_ALIGNMENT))
	    fprintf (vect_dump, "zero step in outer loop.");
	  if (DR_IS_READ (dr))
  	    return true;
	  else
	    return false;
	}
    }

  /* Consecutive?  */
  if (!tree_int_cst_compare (step, TYPE_SIZE_UNIT (scalar_type))
      || (dr_step < 0
	  && !compare_tree_int (TYPE_SIZE_UNIT (scalar_type), -dr_step)))
    {
      /* Mark that it is not interleaving.  */
      GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt)) = NULL;
      return true;
    }

  if (loop && nested_in_vect_loop_p (loop, stmt))
    {
      if (vect_print_dump_info (REPORT_ALIGNMENT))
	fprintf (vect_dump, "strided access in outer loop.");
      return false;
    }

  /* Not consecutive access - check if it's a part of interleaving group.  */
  return vect_analyze_group_access (dr);
}


/* Function vect_analyze_data_ref_accesses.

   Analyze the access pattern of all the data references in the loop.

   FORNOW: the only access pattern that is considered vectorizable is a
	   simple step 1 (consecutive) access.

   FORNOW: handle only arrays and pointer accesses.  */

bool
vect_analyze_data_ref_accesses (loop_vec_info loop_vinfo, bb_vec_info bb_vinfo)
{
  unsigned int i;
  VEC (data_reference_p, heap) *datarefs;
  struct data_reference *dr;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_analyze_data_ref_accesses ===");

  if (loop_vinfo)
    datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  else
    datarefs = BB_VINFO_DATAREFS (bb_vinfo);

  FOR_EACH_VEC_ELT (data_reference_p, datarefs, i, dr)
    if (STMT_VINFO_VECTORIZABLE (vinfo_for_stmt (DR_STMT (dr))) 
        && !vect_analyze_data_ref_access (dr))
      {
	if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
	  fprintf (vect_dump, "not vectorized: complicated access pattern.");

        if (bb_vinfo)
          {
            /* Mark the statement as not vectorizable.  */
            STMT_VINFO_VECTORIZABLE (vinfo_for_stmt (DR_STMT (dr))) = false;
            continue;
          }
        else
          return false;
      }

  return true;
}

/* Function vect_prune_runtime_alias_test_list.

   Prune a list of ddrs to be tested at run-time by versioning for alias.
   Return FALSE if resulting list of ddrs is longer then allowed by
   PARAM_VECT_MAX_VERSION_FOR_ALIAS_CHECKS, otherwise return TRUE.  */

bool
vect_prune_runtime_alias_test_list (loop_vec_info loop_vinfo)
{
  VEC (ddr_p, heap) * ddrs =
    LOOP_VINFO_MAY_ALIAS_DDRS (loop_vinfo);
  unsigned i, j;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_prune_runtime_alias_test_list ===");

  for (i = 0; i < VEC_length (ddr_p, ddrs); )
    {
      bool found;
      ddr_p ddr_i;

      ddr_i = VEC_index (ddr_p, ddrs, i);
      found = false;

      for (j = 0; j < i; j++)
        {
	  ddr_p ddr_j = VEC_index (ddr_p, ddrs, j);

	  if (vect_vfa_range_equal (ddr_i, ddr_j))
	    {
	      if (vect_print_dump_info (REPORT_DR_DETAILS))
		{
		  fprintf (vect_dump, "found equal ranges ");
		  print_generic_expr (vect_dump, DR_REF (DDR_A (ddr_i)), TDF_SLIM);
		  fprintf (vect_dump, ", ");
		  print_generic_expr (vect_dump, DR_REF (DDR_B (ddr_i)), TDF_SLIM);
		  fprintf (vect_dump, " and ");
		  print_generic_expr (vect_dump, DR_REF (DDR_A (ddr_j)), TDF_SLIM);
		  fprintf (vect_dump, ", ");
		  print_generic_expr (vect_dump, DR_REF (DDR_B (ddr_j)), TDF_SLIM);
		}
	      found = true;
	      break;
	    }
	}

      if (found)
      {
	VEC_ordered_remove (ddr_p, ddrs, i);
	continue;
      }
      i++;
    }

  if (VEC_length (ddr_p, ddrs) >
       (unsigned) PARAM_VALUE (PARAM_VECT_MAX_VERSION_FOR_ALIAS_CHECKS))
    {
      if (vect_print_dump_info (REPORT_DR_DETAILS))
	{
	  fprintf (vect_dump,
		   "disable versioning for alias - max number of generated "
		   "checks exceeded.");
	}

      VEC_truncate (ddr_p, LOOP_VINFO_MAY_ALIAS_DDRS (loop_vinfo), 0);

      return false;
    }

  return true;
}

/* Check whether a non-affine read in stmt is suitable for gather load
   and if so, return a builtin decl for that operation.  */

tree
vect_check_gather (gimple stmt, loop_vec_info loop_vinfo, tree *basep,
		   tree *offp, int *scalep)
{
  HOST_WIDE_INT scale = 1, pbitpos, pbitsize;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  tree offtype = NULL_TREE;
  tree decl, base, off;
  enum machine_mode pmode;
  int punsignedp, pvolatilep;

  /* The gather builtins need address of the form
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
  base = get_inner_reference (DR_REF (dr), &pbitsize, &pbitpos, &off,
			      &pmode, &punsignedp, &pvolatilep, false);
  gcc_assert (base != NULL_TREE && (pbitpos % BITS_PER_UNIT) == 0);

  if (TREE_CODE (base) == MEM_REF)
    {
      if (!integer_zerop (TREE_OPERAND (base, 1)))
	{
	  if (off == NULL_TREE)
	    {
	      double_int moff = mem_ref_offset (base);
	      off = double_int_to_tree (sizetype, moff);
	    }
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
	return NULL_TREE;
      off = base;
      base = size_int (pbitpos / BITS_PER_UNIT);
    }
  /* Otherwise put base + constant offset into the loop invariant BASE
     and continue with OFF.  */
  else
    {
      base = fold_convert (sizetype, base);
      base = size_binop (PLUS_EXPR, base, size_int (pbitpos / BITS_PER_UNIT));
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
	  gimple def_stmt = SSA_NAME_DEF_STMT (off);

	  if (expr_invariant_in_loop_p (loop, off))
	    return NULL_TREE;

	  if (gimple_code (def_stmt) != GIMPLE_ASSIGN)
	    break;

	  op0 = gimple_assign_rhs1 (def_stmt);
	  code = gimple_assign_rhs_code (def_stmt);
	  op1 = gimple_assign_rhs2 (def_stmt);
	}
      else
	{
	  if (get_gimple_rhs_class (TREE_CODE (off)) == GIMPLE_TERNARY_RHS)
	    return NULL_TREE;
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
	  if (scale == 1 && host_integerp (op1, 0))
	    {
	      scale = tree_low_cst (op1, 0);
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
    return NULL_TREE;

  if (offtype == NULL_TREE)
    offtype = TREE_TYPE (off);

  decl = targetm.vectorize.builtin_gather (STMT_VINFO_VECTYPE (stmt_info),
					   offtype, scale);
  if (decl == NULL_TREE)
    return NULL_TREE;

  if (basep)
    *basep = base;
  if (offp)
    *offp = off;
  if (scalep)
    *scalep = scale;
  return decl;
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
vect_analyze_data_refs (loop_vec_info loop_vinfo,
			bb_vec_info bb_vinfo,
			int *min_vf)
{
  struct loop *loop = NULL;
  basic_block bb = NULL;
  unsigned int i;
  VEC (data_reference_p, heap) *datarefs;
  struct data_reference *dr;
  tree scalar_type;
  bool res, stop_bb_analysis = false;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_analyze_data_refs ===\n");

  if (loop_vinfo)
    {
      loop = LOOP_VINFO_LOOP (loop_vinfo);
      res = compute_data_dependences_for_loop
	(loop, true,
	 &LOOP_VINFO_LOOP_NEST (loop_vinfo),
	 &LOOP_VINFO_DATAREFS (loop_vinfo),
	 &LOOP_VINFO_DDRS (loop_vinfo));

      if (!res)
	{
	  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
	    fprintf (vect_dump, "not vectorized: loop contains function calls"
		     " or data references that cannot be analyzed");
	  return false;
	}

      datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
    }
  else
    {
      bb = BB_VINFO_BB (bb_vinfo);
      res = compute_data_dependences_for_bb (bb, true,
					     &BB_VINFO_DATAREFS (bb_vinfo),
					     &BB_VINFO_DDRS (bb_vinfo));
      if (!res)
	{
	  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
	    fprintf (vect_dump, "not vectorized: basic block contains function"
		     " calls or data references that cannot be analyzed");
	  return false;
	}

      datarefs = BB_VINFO_DATAREFS (bb_vinfo);
    }

  /* Go through the data-refs, check that the analysis succeeded.  Update
     pointer from stmt_vec_info struct to DR and vectype.  */

  FOR_EACH_VEC_ELT (data_reference_p, datarefs, i, dr)
    {
      gimple stmt;
      stmt_vec_info stmt_info;
      tree base, offset, init;
      bool gather = false;
      int vf;

      if (!dr || !DR_REF (dr))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
	    fprintf (vect_dump, "not vectorized: unhandled data-ref ");

          return false;
        }

      stmt = DR_STMT (dr);
      stmt_info = vinfo_for_stmt (stmt);

      if (stop_bb_analysis)
        {
          STMT_VINFO_VECTORIZABLE (stmt_info) = false;
          continue;
        }

      /* Check that analysis of the data-ref succeeded.  */
      if (!DR_BASE_ADDRESS (dr) || !DR_OFFSET (dr) || !DR_INIT (dr)
	  || !DR_STEP (dr))
        {
	  /* If target supports vector gather loads, see if they can't
	     be used.  */
	  if (loop_vinfo
	      && DR_IS_READ (dr)
	      && !TREE_THIS_VOLATILE (DR_REF (dr))
	      && targetm.vectorize.builtin_gather != NULL
	      && !nested_in_vect_loop_p (loop, stmt))
	    {
	      struct data_reference *newdr
		= create_data_ref (NULL, loop_containing_stmt (stmt),
				   DR_REF (dr), stmt, true);
	      gcc_assert (newdr != NULL && DR_REF (newdr));
	      if (DR_BASE_ADDRESS (newdr)
		  && DR_OFFSET (newdr)
		  && DR_INIT (newdr)
		  && DR_STEP (newdr)
		  && integer_zerop (DR_STEP (newdr)))
		{
		  dr = newdr;
		  gather = true;
		}
	      else
		free_data_ref (newdr);
	    }

	  if (!gather)
	    {
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
		{
		  fprintf (vect_dump, "not vectorized: data ref analysis "
				      "failed ");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}

	      if (bb_vinfo)
		{
		  STMT_VINFO_VECTORIZABLE (stmt_info) = false;
		  stop_bb_analysis = true;
		  continue;
		}

	      return false;
	    }
        }

      if (TREE_CODE (DR_BASE_ADDRESS (dr)) == INTEGER_CST)
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
            fprintf (vect_dump, "not vectorized: base addr of dr is a "
                     "constant");

          if (bb_vinfo)
            {
              STMT_VINFO_VECTORIZABLE (stmt_info) = false;
              stop_bb_analysis = true;
              continue;
            }

	  if (gather)
	    free_data_ref (dr);
	  return false;
        }

      if (TREE_THIS_VOLATILE (DR_REF (dr)))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
            {
              fprintf (vect_dump, "not vectorized: volatile type ");
              print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
            }

          if (bb_vinfo)
            {
              STMT_VINFO_VECTORIZABLE (stmt_info) = false;
              stop_bb_analysis = true;
              continue;
            }

          return false;
        }

      base = unshare_expr (DR_BASE_ADDRESS (dr));
      offset = unshare_expr (DR_OFFSET (dr));
      init = unshare_expr (DR_INIT (dr));

      if (stmt_can_throw_internal (stmt))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
            {
              fprintf (vect_dump, "not vectorized: statement can throw an "
                       "exception ");
              print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
            }

          if (bb_vinfo)
            {
              STMT_VINFO_VECTORIZABLE (stmt_info) = false;
              stop_bb_analysis = true;
              continue;
            }

	  if (gather)
	    free_data_ref (dr);
          return false;
        }

      if (is_gimple_call (stmt))
	{
	  if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
	    {
	      fprintf (vect_dump, "not vectorized: dr in a call ");
	      print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
	    }

	  if (bb_vinfo)
	    {
	      STMT_VINFO_VECTORIZABLE (stmt_info) = false;
	      stop_bb_analysis = true;
	      continue;
	    }

	  if (gather)
	    free_data_ref (dr);
	  return false;
	}

      /* Update DR field in stmt_vec_info struct.  */

      /* If the dataref is in an inner-loop of the loop that is considered for
	 for vectorization, we also want to analyze the access relative to
	 the outer-loop (DR contains information only relative to the
	 inner-most enclosing loop).  We do that by building a reference to the
	 first location accessed by the inner-loop, and analyze it relative to
	 the outer-loop.  */
      if (loop && nested_in_vect_loop_p (loop, stmt))
	{
	  tree outer_step, outer_base, outer_init;
	  HOST_WIDE_INT pbitsize, pbitpos;
	  tree poffset;
	  enum machine_mode pmode;
	  int punsignedp, pvolatilep;
	  affine_iv base_iv, offset_iv;
	  tree dinit;

	  /* Build a reference to the first location accessed by the
	     inner-loop: *(BASE+INIT).  (The first location is actually
	     BASE+INIT+OFFSET, but we add OFFSET separately later).  */
          tree inner_base = build_fold_indirect_ref
                                (fold_build_pointer_plus (base, init));

	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "analyze in outer-loop: ");
	      print_generic_expr (vect_dump, inner_base, TDF_SLIM);
	    }

	  outer_base = get_inner_reference (inner_base, &pbitsize, &pbitpos,
		          &poffset, &pmode, &punsignedp, &pvolatilep, false);
	  gcc_assert (outer_base != NULL_TREE);

	  if (pbitpos % BITS_PER_UNIT != 0)
	    {
	      if (vect_print_dump_info (REPORT_DETAILS))
		fprintf (vect_dump, "failed: bit offset alignment.\n");
	      return false;
	    }

	  outer_base = build_fold_addr_expr (outer_base);
	  if (!simple_iv (loop, loop_containing_stmt (stmt), outer_base,
                          &base_iv, false))
	    {
	      if (vect_print_dump_info (REPORT_DETAILS))
		fprintf (vect_dump, "failed: evolution of base is not affine.\n");
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
	      if (vect_print_dump_info (REPORT_DETAILS))
	        fprintf (vect_dump, "evolution of offset is not affine.\n");
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

	  STMT_VINFO_DR_STEP (stmt_info) = outer_step;
	  /* FIXME: Use canonicalize_base_object_address (base_iv.base); */
	  STMT_VINFO_DR_BASE_ADDRESS (stmt_info) = base_iv.base;
	  STMT_VINFO_DR_INIT (stmt_info) = outer_init;
	  STMT_VINFO_DR_OFFSET (stmt_info) =
				fold_convert (ssizetype, offset_iv.base);
	  STMT_VINFO_DR_ALIGNED_TO (stmt_info) =
				size_int (highest_pow2_factor (offset_iv.base));

	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "\touter base_address: ");
	      print_generic_expr (vect_dump, STMT_VINFO_DR_BASE_ADDRESS (stmt_info), TDF_SLIM);
	      fprintf (vect_dump, "\n\touter offset from base address: ");
	      print_generic_expr (vect_dump, STMT_VINFO_DR_OFFSET (stmt_info), TDF_SLIM);
	      fprintf (vect_dump, "\n\touter constant offset from base address: ");
	      print_generic_expr (vect_dump, STMT_VINFO_DR_INIT (stmt_info), TDF_SLIM);
	      fprintf (vect_dump, "\n\touter step: ");
	      print_generic_expr (vect_dump, STMT_VINFO_DR_STEP (stmt_info), TDF_SLIM);
	      fprintf (vect_dump, "\n\touter aligned to: ");
	      print_generic_expr (vect_dump, STMT_VINFO_DR_ALIGNED_TO (stmt_info), TDF_SLIM);
	    }
	}

      if (STMT_VINFO_DATA_REF (stmt_info))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
            {
              fprintf (vect_dump,
                       "not vectorized: more than one data ref in stmt: ");
              print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
            }

          if (bb_vinfo)
            {
              STMT_VINFO_VECTORIZABLE (stmt_info) = false;
              stop_bb_analysis = true;
              continue;
            }

	  if (gather)
	    free_data_ref (dr);
          return false;
        }

      STMT_VINFO_DATA_REF (stmt_info) = dr;

      /* Set vectype for STMT.  */
      scalar_type = TREE_TYPE (DR_REF (dr));
      STMT_VINFO_VECTYPE (stmt_info) =
                get_vectype_for_scalar_type (scalar_type);
      if (!STMT_VINFO_VECTYPE (stmt_info))
        {
          if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
            {
              fprintf (vect_dump,
                       "not vectorized: no vectype for stmt: ");
              print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
              fprintf (vect_dump, " scalar_type: ");
              print_generic_expr (vect_dump, scalar_type, TDF_DETAILS);
            }

          if (bb_vinfo)
            {
              /* Mark the statement as not vectorizable.  */
              STMT_VINFO_VECTORIZABLE (stmt_info) = false;
              stop_bb_analysis = true;
              continue;
            }

	  if (gather)
	    {
	      STMT_VINFO_DATA_REF (stmt_info) = NULL;
	      free_data_ref (dr);
	    }
	  return false;
        }

      /* Adjust the minimal vectorization factor according to the
	 vector type.  */
      vf = TYPE_VECTOR_SUBPARTS (STMT_VINFO_VECTYPE (stmt_info));
      if (vf > *min_vf)
	*min_vf = vf;

      if (gather)
	{
	  unsigned int j, k, n;
	  struct data_reference *olddr
	    = VEC_index (data_reference_p, datarefs, i);
	  VEC (ddr_p, heap) *ddrs = LOOP_VINFO_DDRS (loop_vinfo);
	  struct data_dependence_relation *ddr, *newddr;
	  bool bad = false;
	  tree off;
	  VEC (loop_p, heap) *nest = LOOP_VINFO_LOOP_NEST (loop_vinfo);

	  if (!vect_check_gather (stmt, loop_vinfo, NULL, &off, NULL)
	      || get_vectype_for_scalar_type (TREE_TYPE (off)) == NULL_TREE)
	    {
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
		{
		  fprintf (vect_dump,
			   "not vectorized: not suitable for gather ");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}
	      return false;
	    }

	  n = VEC_length (data_reference_p, datarefs) - 1;
	  for (j = 0, k = i - 1; j < i; j++)
	    {
	      ddr = VEC_index (ddr_p, ddrs, k);
	      gcc_assert (DDR_B (ddr) == olddr);
	      newddr = initialize_data_dependence_relation (DDR_A (ddr), dr,
							    nest);
	      VEC_replace (ddr_p, ddrs, k, newddr);
	      free_dependence_relation (ddr);
	      if (!bad
		  && DR_IS_WRITE (DDR_A (newddr))
		  && DDR_ARE_DEPENDENT (newddr) != chrec_known)
		bad = true;
	      k += --n;
	    }

	  k++;
	  n = k + VEC_length (data_reference_p, datarefs) - i - 1;
	  for (; k < n; k++)
	    {
	      ddr = VEC_index (ddr_p, ddrs, k);
	      gcc_assert (DDR_A (ddr) == olddr);
	      newddr = initialize_data_dependence_relation (dr, DDR_B (ddr),
							    nest);
	      VEC_replace (ddr_p, ddrs, k, newddr);
	      free_dependence_relation (ddr);
	      if (!bad
		  && DR_IS_WRITE (DDR_B (newddr))
		  && DDR_ARE_DEPENDENT (newddr) != chrec_known)
		bad = true;
	    }

	  k = VEC_length (ddr_p, ddrs)
	      - VEC_length (data_reference_p, datarefs) + i;
	  ddr = VEC_index (ddr_p, ddrs, k);
	  gcc_assert (DDR_A (ddr) == olddr && DDR_B (ddr) == olddr);
	  newddr = initialize_data_dependence_relation (dr, dr, nest);
	  VEC_replace (ddr_p, ddrs, k, newddr);
	  free_dependence_relation (ddr);
	  VEC_replace (data_reference_p, datarefs, i, dr);

	  if (bad)
	    {
	      if (vect_print_dump_info (REPORT_UNVECTORIZED_LOCATIONS))
		{
		  fprintf (vect_dump,
			   "not vectorized: data dependence conflict"
			   " prevents gather");
		  print_gimple_stmt (vect_dump, stmt, 0, TDF_SLIM);
		}
	      return false;
	    }

	  STMT_VINFO_GATHER_P (stmt_info) = true;
	}
    }

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
    prefix = "vect_";
    break;
  case vect_scalar_var:
    prefix = "stmp_";
    break;
  case vect_pointer_var:
    prefix = "vect_p";
    break;
  default:
    gcc_unreachable ();
  }

  if (name)
    {
      char* tmp = concat (prefix, name, NULL);
      new_vect_var = create_tmp_var (type, tmp);
      free (tmp);
    }
  else
    new_vect_var = create_tmp_var (type, prefix);

  /* Mark vector typed variable as a gimple register variable.  */
  if (TREE_CODE (type) == VECTOR_TYPE)
    DECL_GIMPLE_REG_P (new_vect_var) = true;

  return new_vect_var;
}


/* Function vect_create_addr_base_for_vector_ref.

   Create an expression that computes the address of the first memory location
   that will be accessed for a data reference.

   Input:
   STMT: The statement containing the data reference.
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

   Output:
   1. Return an SSA_NAME whose value is the address of the memory location of
      the first vector of the data reference.
   2. If new_stmt_list is not NULL_TREE after return then the caller must insert
      these statement(s) which define the returned SSA_NAME.

   FORNOW: We are only handling array accesses with step 1.  */

tree
vect_create_addr_base_for_vector_ref (gimple stmt,
				      gimple_seq *new_stmt_list,
				      tree offset,
				      struct loop *loop)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  tree data_ref_base = unshare_expr (DR_BASE_ADDRESS (dr));
  tree base_name;
  tree data_ref_base_var;
  tree vec_stmt;
  tree addr_base, addr_expr;
  tree dest;
  gimple_seq seq = NULL;
  tree base_offset = unshare_expr (DR_OFFSET (dr));
  tree init = unshare_expr (DR_INIT (dr));
  tree vect_ptr_type;
  tree step = TYPE_SIZE_UNIT (TREE_TYPE (DR_REF (dr)));
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  tree base;

  if (loop_vinfo && loop && loop != (gimple_bb (stmt))->loop_father)
    {
      struct loop *outer_loop = LOOP_VINFO_LOOP (loop_vinfo);

      gcc_assert (nested_in_vect_loop_p (outer_loop, stmt));

      data_ref_base = unshare_expr (STMT_VINFO_DR_BASE_ADDRESS (stmt_info));
      base_offset = unshare_expr (STMT_VINFO_DR_OFFSET (stmt_info));
      init = unshare_expr (STMT_VINFO_DR_INIT (stmt_info));
    }

  if (loop_vinfo)
    base_name = build_fold_indirect_ref (data_ref_base);
  else
    {
      base_offset = ssize_int (0);
      init = ssize_int (0);
      base_name = build_fold_indirect_ref (unshare_expr (DR_REF (dr)));
    }

  data_ref_base_var = create_tmp_var (TREE_TYPE (data_ref_base), "batmp");
  add_referenced_var (data_ref_base_var);
  data_ref_base = force_gimple_operand (data_ref_base, &seq, true,
					data_ref_base_var);
  gimple_seq_add_seq (new_stmt_list, seq);

  /* Create base_offset */
  base_offset = size_binop (PLUS_EXPR,
			    fold_convert (sizetype, base_offset),
			    fold_convert (sizetype, init));
  dest = create_tmp_var (sizetype, "base_off");
  add_referenced_var (dest);
  base_offset = force_gimple_operand (base_offset, &seq, true, dest);
  gimple_seq_add_seq (new_stmt_list, seq);

  if (offset)
    {
      tree tmp = create_tmp_var (sizetype, "offset");

      add_referenced_var (tmp);
      offset = fold_build2 (MULT_EXPR, sizetype,
			    fold_convert (sizetype, offset), step);
      base_offset = fold_build2 (PLUS_EXPR, sizetype,
				 base_offset, offset);
      base_offset = force_gimple_operand (base_offset, &seq, false, tmp);
      gimple_seq_add_seq (new_stmt_list, seq);
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
  base = get_base_address (DR_REF (dr));
  if (base
      && TREE_CODE (base) == MEM_REF)
    vect_ptr_type
      = build_qualified_type (vect_ptr_type,
			      TYPE_QUALS (TREE_TYPE (TREE_OPERAND (base, 0))));

  vec_stmt = fold_convert (vect_ptr_type, addr_base);
  addr_expr = vect_get_new_vect_var (vect_ptr_type, vect_pointer_var,
                                     get_name (base_name));
  add_referenced_var (addr_expr);
  vec_stmt = force_gimple_operand (vec_stmt, &seq, false, addr_expr);
  gimple_seq_add_seq (new_stmt_list, seq);

  if (DR_PTR_INFO (dr)
      && TREE_CODE (vec_stmt) == SSA_NAME)
    {
      duplicate_ssa_name_ptr_info (vec_stmt, DR_PTR_INFO (dr));
      if (offset)
	{
	  SSA_NAME_PTR_INFO (vec_stmt)->align = 1;
	  SSA_NAME_PTR_INFO (vec_stmt)->misalign = 0;
	}
    }

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "created ");
      print_generic_expr (vect_dump, vec_stmt, TDF_SLIM);
    }

  return vec_stmt;
}


/* Function vect_create_data_ref_ptr.

   Create a new pointer-to-AGGR_TYPE variable (ap), that points to the first
   location accessed in the loop by STMT, along with the def-use update
   chain to appropriately advance the pointer through the loop iterations.
   Also set aliasing information for the pointer.  This pointer is used by
   the callers to this function to create a memory reference expression for
   vector load/store access.

   Input:
   1. STMT: a stmt that references memory. Expected to be of the form
         GIMPLE_ASSIGN <name, data-ref> or
	 GIMPLE_ASSIGN <data-ref, name>.
   2. AGGR_TYPE: the type of the reference, which should be either a vector
        or an array.
   3. AT_LOOP: the loop where the vector memref is to be created.
   4. OFFSET (optional): an offset to be added to the initial address accessed
        by the data-ref in STMT.
   5. BSI: location where the new stmts are to be placed if there is no loop
   6. ONLY_INIT: indicate if ap is to be updated in the loop, or remain
        pointing to the initial address.

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

      Return the initial_address in INITIAL_ADDRESS.

   2. If ONLY_INIT is true, just return the initial pointer.  Otherwise, also
      update the pointer in each iteration of the loop.

      Return the increment stmt that updates the pointer in PTR_INCR.

   3. Set INV_P to true if the access pattern of the data reference in the
      vectorized loop is invariant.  Set it to false otherwise.

   4. Return the pointer.  */

tree
vect_create_data_ref_ptr (gimple stmt, tree aggr_type, struct loop *at_loop,
			  tree offset, tree *initial_address,
			  gimple_stmt_iterator *gsi, gimple *ptr_incr,
			  bool only_init, bool *inv_p)
{
  tree base_name;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = NULL;
  bool nested_in_vect_loop = false;
  struct loop *containing_loop = NULL;
  tree aggr_ptr_type;
  tree aggr_ptr;
  tree new_temp;
  gimple vec_stmt;
  gimple_seq new_stmt_list = NULL;
  edge pe = NULL;
  basic_block new_bb;
  tree aggr_ptr_init;
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  tree aptr;
  gimple_stmt_iterator incr_gsi;
  bool insert_after;
  bool negative;
  tree indx_before_incr, indx_after_incr;
  gimple incr;
  tree step;
  bb_vec_info bb_vinfo = STMT_VINFO_BB_VINFO (stmt_info);
  tree base;

  gcc_assert (TREE_CODE (aggr_type) == ARRAY_TYPE
	      || TREE_CODE (aggr_type) == VECTOR_TYPE);

  if (loop_vinfo)
    {
      loop = LOOP_VINFO_LOOP (loop_vinfo);
      nested_in_vect_loop = nested_in_vect_loop_p (loop, stmt);
      containing_loop = (gimple_bb (stmt))->loop_father;
      pe = loop_preheader_edge (loop);
    }
  else
    {
      gcc_assert (bb_vinfo);
      only_init = true;
      *ptr_incr = NULL;
    }

  /* Check the step (evolution) of the load in LOOP, and record
     whether it's invariant.  */
  if (nested_in_vect_loop)
    step = STMT_VINFO_DR_STEP (stmt_info);
  else
    step = DR_STEP (STMT_VINFO_DATA_REF (stmt_info));

  if (tree_int_cst_compare (step, size_zero_node) == 0)
    *inv_p = true;
  else
    *inv_p = false;
  negative = tree_int_cst_compare (step, size_zero_node) < 0;

  /* Create an expression for the first address accessed by this load
     in LOOP.  */
  base_name = build_fold_indirect_ref (unshare_expr (DR_BASE_ADDRESS (dr)));

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      tree data_ref_base = base_name;
      fprintf (vect_dump, "create %s-pointer variable to type: ",
	       tree_code_name[(int) TREE_CODE (aggr_type)]);
      print_generic_expr (vect_dump, aggr_type, TDF_SLIM);
      if (TREE_CODE (data_ref_base) == VAR_DECL
          || TREE_CODE (data_ref_base) == ARRAY_REF)
        fprintf (vect_dump, "  vectorizing an array ref: ");
      else if (TREE_CODE (data_ref_base) == COMPONENT_REF)
        fprintf (vect_dump, "  vectorizing a record based array ref: ");
      else if (TREE_CODE (data_ref_base) == SSA_NAME)
        fprintf (vect_dump, "  vectorizing a pointer ref: ");
      print_generic_expr (vect_dump, base_name, TDF_SLIM);
    }

  /* (1) Create the new aggregate-pointer variable.  */
  aggr_ptr_type = build_pointer_type (aggr_type);
  base = get_base_address (DR_REF (dr));
  if (base
      && TREE_CODE (base) == MEM_REF)
    aggr_ptr_type
      = build_qualified_type (aggr_ptr_type,
			      TYPE_QUALS (TREE_TYPE (TREE_OPERAND (base, 0))));
  aggr_ptr = vect_get_new_vect_var (aggr_ptr_type, vect_pointer_var,
                                    get_name (base_name));

  /* Vector and array types inherit the alias set of their component
     type by default so we need to use a ref-all pointer if the data
     reference does not conflict with the created aggregated data
     reference because it is not addressable.  */
  if (!alias_sets_conflict_p (get_deref_alias_set (aggr_ptr),
			      get_alias_set (DR_REF (dr))))
    {
      aggr_ptr_type
	= build_pointer_type_for_mode (aggr_type,
				       TYPE_MODE (aggr_ptr_type), true);
      aggr_ptr = vect_get_new_vect_var (aggr_ptr_type, vect_pointer_var,
					get_name (base_name));
    }

  /* Likewise for any of the data references in the stmt group.  */
  else if (STMT_VINFO_GROUP_SIZE (stmt_info) > 1)
    {
      gimple orig_stmt = STMT_VINFO_GROUP_FIRST_ELEMENT (stmt_info);
      do
	{
	  tree lhs = gimple_assign_lhs (orig_stmt);
	  if (!alias_sets_conflict_p (get_deref_alias_set (aggr_ptr),
				      get_alias_set (lhs)))
	    {
	      aggr_ptr_type
		= build_pointer_type_for_mode (aggr_type,
					       TYPE_MODE (aggr_ptr_type), true);
	      aggr_ptr
		= vect_get_new_vect_var (aggr_ptr_type, vect_pointer_var,
					 get_name (base_name));
	      break;
	    }

	  orig_stmt = STMT_VINFO_GROUP_NEXT_ELEMENT (vinfo_for_stmt (orig_stmt));
	}
      while (orig_stmt);
    }

  add_referenced_var (aggr_ptr);

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

  /* Create: (&(base[init_val+offset]) in the loop preheader.  */

  new_temp = vect_create_addr_base_for_vector_ref (stmt, &new_stmt_list,
                                                   offset, loop);
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

  /* Create: p = (aggr_type *) initial_base  */
  if (TREE_CODE (new_temp) != SSA_NAME
      || !useless_type_conversion_p (aggr_ptr_type, TREE_TYPE (new_temp)))
    {
      vec_stmt = gimple_build_assign (aggr_ptr,
				      fold_convert (aggr_ptr_type, new_temp));
      aggr_ptr_init = make_ssa_name (aggr_ptr, vec_stmt);
      /* Copy the points-to information if it exists. */
      if (DR_PTR_INFO (dr))
	duplicate_ssa_name_ptr_info (aggr_ptr_init, DR_PTR_INFO (dr));
      gimple_assign_set_lhs (vec_stmt, aggr_ptr_init);
      if (pe)
	{
	  new_bb = gsi_insert_on_edge_immediate (pe, vec_stmt);
	  gcc_assert (!new_bb);
	}
      else
	gsi_insert_before (gsi, vec_stmt, GSI_SAME_STMT);
    }
  else
    aggr_ptr_init = new_temp;

  /* (3) Handle the updating of the aggregate-pointer inside the loop.
     This is needed when ONLY_INIT is false, and also when AT_LOOP is the
     inner-loop nested in LOOP (during outer-loop vectorization).  */

  /* No update in loop is required.  */
  if (only_init && (!loop_vinfo || at_loop == loop))
    aptr = aggr_ptr_init;
  else
    {
      /* The step of the aggregate pointer is the type size.  */
      tree step = TYPE_SIZE_UNIT (aggr_type);
      /* One exception to the above is when the scalar step of the load in
	 LOOP is zero. In this case the step here is also zero.  */
      if (*inv_p)
	step = size_zero_node;
      else if (negative)
	step = fold_build1 (NEGATE_EXPR, TREE_TYPE (step), step);

      standard_iv_increment_position (loop, &incr_gsi, &insert_after);

      create_iv (aggr_ptr_init,
		 fold_convert (aggr_ptr_type, step),
		 aggr_ptr, loop, &incr_gsi, insert_after,
		 &indx_before_incr, &indx_after_incr);
      incr = gsi_stmt (incr_gsi);
      set_vinfo_for_stmt (incr, new_stmt_vec_info (incr, loop_vinfo, NULL));

      /* Copy the points-to information if it exists. */
      if (DR_PTR_INFO (dr))
	{
	  duplicate_ssa_name_ptr_info (indx_before_incr, DR_PTR_INFO (dr));
	  duplicate_ssa_name_ptr_info (indx_after_incr, DR_PTR_INFO (dr));
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
      set_vinfo_for_stmt (incr, new_stmt_vec_info (incr, loop_vinfo, NULL));

      /* Copy the points-to information if it exists. */
      if (DR_PTR_INFO (dr))
	{
	  duplicate_ssa_name_ptr_info (indx_before_incr, DR_PTR_INFO (dr));
	  duplicate_ssa_name_ptr_info (indx_after_incr, DR_PTR_INFO (dr));
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
   STMT - the original scalar memory-access stmt that is being vectorized.
   BUMP - optional. The offset by which to bump the pointer. If not given,
	  the offset is assumed to be vector_size.

   Output: Return NEW_DATAREF_PTR as illustrated above.

*/

tree
bump_vector_ptr (tree dataref_ptr, gimple ptr_incr, gimple_stmt_iterator *gsi,
		 gimple stmt, tree bump)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree ptr_var = SSA_NAME_VAR (dataref_ptr);
  tree update = TYPE_SIZE_UNIT (vectype);
  gimple incr_stmt;
  ssa_op_iter iter;
  use_operand_p use_p;
  tree new_dataref_ptr;

  if (bump)
    update = bump;

  incr_stmt = gimple_build_assign_with_ops (POINTER_PLUS_EXPR, ptr_var,
					    dataref_ptr, update);
  new_dataref_ptr = make_ssa_name (ptr_var, incr_stmt);
  gimple_assign_set_lhs (incr_stmt, new_dataref_ptr);
  vect_finish_stmt_generation (stmt, incr_stmt, gsi);

  /* Copy the points-to information if it exists. */
  if (DR_PTR_INFO (dr))
    {
      duplicate_ssa_name_ptr_info (new_dataref_ptr, DR_PTR_INFO (dr));
      SSA_NAME_PTR_INFO (new_dataref_ptr)->align = 1;
      SSA_NAME_PTR_INFO (new_dataref_ptr)->misalign = 0;
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
        gcc_assert (tree_int_cst_compare (use, update) == 0);
    }

  return new_dataref_ptr;
}


/* Function vect_create_destination_var.

   Create a new temporary of type VECTYPE.  */

tree
vect_create_destination_var (tree scalar_dest, tree vectype)
{
  tree vec_dest;
  const char *new_name;
  tree type;
  enum vect_var_kind kind;

  kind = vectype ? vect_simple_var : vect_scalar_var;
  type = vectype ? vectype : TREE_TYPE (scalar_dest);

  gcc_assert (TREE_CODE (scalar_dest) == SSA_NAME);

  new_name = get_name (scalar_dest);
  if (!new_name)
    new_name = "var_";
  vec_dest = vect_get_new_vect_var (type, kind, new_name);
  add_referenced_var (vec_dest);

  return vec_dest;
}

/* Function vect_strided_store_supported.

   Returns TRUE if interleave high and interleave low permutations
   are supported, and FALSE otherwise.  */

bool
vect_strided_store_supported (tree vectype, unsigned HOST_WIDE_INT count)
{
  enum machine_mode mode = TYPE_MODE (vectype);

  /* vect_permute_store_chain requires the group size to be a power of two.  */
  if (exact_log2 (count) == -1)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "the size of the group of strided accesses"
		 " is not a power of 2");
      return false;
    }

  /* Check that the permutation is supported.  */
  if (VECTOR_MODE_P (mode))
    {
      unsigned int i, nelt = GET_MODE_NUNITS (mode);
      unsigned char *sel = XALLOCAVEC (unsigned char, nelt);
      for (i = 0; i < nelt / 2; i++)
	{
	  sel[i * 2] = i;
	  sel[i * 2 + 1] = i + nelt;
	}
      if (can_vec_perm_p (mode, false, sel))
	{
	  for (i = 0; i < nelt; i++)
	    sel[i] += nelt / 2;
	  if (can_vec_perm_p (mode, false, sel))
	    return true;
	}
    }

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "interleave op not supported by target.");
  return false;
}


/* Return TRUE if vec_store_lanes is available for COUNT vectors of
   type VECTYPE.  */

bool
vect_store_lanes_supported (tree vectype, unsigned HOST_WIDE_INT count)
{
  return vect_lanes_optab_supported_p ("vec_store_lanes",
				       vec_store_lanes_optab,
				       vectype, count);
}


/* Function vect_permute_store_chain.

   Given a chain of interleaved stores in DR_CHAIN of LENGTH that must be
   a power of 2, generate interleave_high/low stmts to reorder the data
   correctly for the stores.  Return the final references for stores in
   RESULT_CHAIN.

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
vect_permute_store_chain (VEC(tree,heap) *dr_chain,
			  unsigned int length,
			  gimple stmt,
			  gimple_stmt_iterator *gsi,
			  VEC(tree,heap) **result_chain)
{
  tree perm_dest, vect1, vect2, high, low;
  gimple perm_stmt;
  tree vectype = STMT_VINFO_VECTYPE (vinfo_for_stmt (stmt));
  tree perm_mask_low, perm_mask_high;
  unsigned int i, n;
  unsigned int j, nelt = TYPE_VECTOR_SUBPARTS (vectype);
  unsigned char *sel = XALLOCAVEC (unsigned char, nelt);

  *result_chain = VEC_copy (tree, heap, dr_chain);

  for (i = 0, n = nelt / 2; i < n; i++)
    {
      sel[i * 2] = i;
      sel[i * 2 + 1] = i + nelt;
    }
  perm_mask_high = vect_gen_perm_mask (vectype, sel);
  gcc_assert (perm_mask_high != NULL);

  for (i = 0; i < nelt; i++)
    sel[i] += nelt / 2;
  perm_mask_low = vect_gen_perm_mask (vectype, sel);
  gcc_assert (perm_mask_low != NULL);

  for (i = 0, n = exact_log2 (length); i < n; i++)
    {
      for (j = 0; j < length/2; j++)
	{
	  vect1 = VEC_index (tree, dr_chain, j);
	  vect2 = VEC_index (tree, dr_chain, j+length/2);

	  /* Create interleaving stmt:
	     high = VEC_PERM_EXPR <vect1, vect2, {0, nelt, 1, nelt+1, ...}>  */
	  perm_dest = create_tmp_var (vectype, "vect_inter_high");
	  DECL_GIMPLE_REG_P (perm_dest) = 1;
	  add_referenced_var (perm_dest);
	  high = make_ssa_name (perm_dest, NULL);
	  perm_stmt
	    = gimple_build_assign_with_ops3 (VEC_PERM_EXPR, high,
					     vect1, vect2, perm_mask_high);
	  vect_finish_stmt_generation (stmt, perm_stmt, gsi);
	  VEC_replace (tree, *result_chain, 2*j, high);

	  /* Create interleaving stmt:
	     low = VEC_PERM_EXPR <vect1, vect2, {nelt/2, nelt*3/2, nelt/2+1,
						 nelt*3/2+1, ...}>  */
	  perm_dest = create_tmp_var (vectype, "vect_inter_low");
	  DECL_GIMPLE_REG_P (perm_dest) = 1;
	  add_referenced_var (perm_dest);
	  low = make_ssa_name (perm_dest, NULL);
	  perm_stmt
	    = gimple_build_assign_with_ops3 (VEC_PERM_EXPR, low,
					     vect1, vect2, perm_mask_low);
	  vect_finish_stmt_generation (stmt, perm_stmt, gsi);
	  VEC_replace (tree, *result_chain, 2*j+1, low);
	}
      dr_chain = VEC_copy (tree, heap, *result_chain);
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
   location accessed by STMT, and a "floor-aligned" load using that pointer.
   It also generates code to compute the "realignment-token" (if the relevant
   target hook was defined), and creates a phi-node at the loop-header bb
   whose arguments are the result of the prolog-load (created by this
   function) and the result of a load that takes place in the loop (to be
   created by the caller to this function).

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
   STMT - (scalar) load stmt to be vectorized. This load accesses
          a memory location that may be unaligned.
   BSI - place where new code is to be inserted.
   ALIGNMENT_SUPPORT_SCHEME - which of the two misalignment handling schemes
			      is used.

   Output:
   REALIGNMENT_TOKEN - the result of a call to the builtin_mask_for_load
                       target hook, if defined.
   Return value - the result of the loop-header phi node.  */

tree
vect_setup_realignment (gimple stmt, gimple_stmt_iterator *gsi,
                        tree *realignment_token,
			enum dr_alignment_support alignment_support_scheme,
			tree init_addr,
			struct loop **at_loop)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  struct loop *loop = NULL;
  edge pe = NULL;
  tree scalar_dest = gimple_assign_lhs (stmt);
  tree vec_dest;
  gimple inc;
  tree ptr;
  tree data_ref;
  gimple new_stmt;
  basic_block new_bb;
  tree msq_init = NULL_TREE;
  tree new_temp;
  gimple phi_stmt;
  tree msq = NULL_TREE;
  gimple_seq stmts = NULL;
  bool inv_p;
  bool compute_in_loop = false;
  bool nested_in_vect_loop = false;
  struct loop *containing_loop = (gimple_bb (stmt))->loop_father;
  struct loop *loop_for_initial_load = NULL;

  if (loop_vinfo)
    {
      loop = LOOP_VINFO_LOOP (loop_vinfo);
      nested_in_vect_loop = nested_in_vect_loop_p (loop, stmt);
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

      gcc_assert (!compute_in_loop);
      vec_dest = vect_create_destination_var (scalar_dest, vectype);
      ptr = vect_create_data_ref_ptr (stmt, vectype, loop_for_initial_load,
				      NULL_TREE, &init_addr, NULL, &inc,
				      true, &inv_p);
      new_stmt = gimple_build_assign_with_ops
		   (BIT_AND_EXPR, NULL_TREE, ptr,
		    build_int_cst (TREE_TYPE (ptr),
				   -(HOST_WIDE_INT)TYPE_ALIGN_UNIT (vectype)));
      new_temp = make_ssa_name (SSA_NAME_VAR (ptr), new_stmt);
      gimple_assign_set_lhs (new_stmt, new_temp);
      new_bb = gsi_insert_on_edge_immediate (pe, new_stmt);
      gcc_assert (!new_bb);
      data_ref
	= build2 (MEM_REF, TREE_TYPE (vec_dest), new_temp,
		  build_int_cst (reference_alias_ptr_type (DR_REF (dr)), 0));
      new_stmt = gimple_build_assign (vec_dest, data_ref);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      gimple_assign_set_lhs (new_stmt, new_temp);
      mark_symbols_for_renaming (new_stmt);
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
      tree builtin_decl;

      /* Compute INIT_ADDR - the initial addressed accessed by this memref.  */
      if (!init_addr)
	{
	  /* Generate the INIT_ADDR computation outside LOOP.  */
	  init_addr = vect_create_addr_base_for_vector_ref (stmt, &stmts,
							NULL_TREE, loop);
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
  msq = make_ssa_name (vec_dest, NULL);
  phi_stmt = create_phi_node (msq, containing_loop->header);
  SSA_NAME_DEF_STMT (msq) = phi_stmt;
  add_phi_arg (phi_stmt, msq_init, pe, UNKNOWN_LOCATION);

  return msq;
}


/* Function vect_strided_load_supported.

   Returns TRUE if even and odd permutations are supported,
   and FALSE otherwise.  */

bool
vect_strided_load_supported (tree vectype, unsigned HOST_WIDE_INT count)
{
  enum machine_mode mode = TYPE_MODE (vectype);

  /* vect_permute_load_chain requires the group size to be a power of two.  */
  if (exact_log2 (count) == -1)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "the size of the group of strided accesses"
		 " is not a power of 2");
      return false;
    }

  /* Check that the permutation is supported.  */
  if (VECTOR_MODE_P (mode))
    {
      unsigned int i, nelt = GET_MODE_NUNITS (mode);
      unsigned char *sel = XALLOCAVEC (unsigned char, nelt);

      for (i = 0; i < nelt; i++)
	sel[i] = i * 2;
      if (can_vec_perm_p (mode, false, sel))
	{
	  for (i = 0; i < nelt; i++)
	    sel[i] = i * 2 + 1;
	  if (can_vec_perm_p (mode, false, sel))
	    return true;
	}
    }

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "extract even/odd not supported by target");
  return false;
}

/* Return TRUE if vec_load_lanes is available for COUNT vectors of
   type VECTYPE.  */

bool
vect_load_lanes_supported (tree vectype, unsigned HOST_WIDE_INT count)
{
  return vect_lanes_optab_supported_p ("vec_load_lanes",
				       vec_load_lanes_optab,
				       vectype, count);
}

/* Function vect_permute_load_chain.

   Given a chain of interleaved loads in DR_CHAIN of LENGTH that must be
   a power of 2, generate extract_even/odd stmts to reorder the input data
   correctly.  Return the final references for loads in RESULT_CHAIN.

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
vect_permute_load_chain (VEC(tree,heap) *dr_chain,
			 unsigned int length,
			 gimple stmt,
			 gimple_stmt_iterator *gsi,
			 VEC(tree,heap) **result_chain)
{
  tree perm_dest, data_ref, first_vect, second_vect;
  tree perm_mask_even, perm_mask_odd;
  gimple perm_stmt;
  tree vectype = STMT_VINFO_VECTYPE (vinfo_for_stmt (stmt));
  unsigned int i, j, log_length = exact_log2 (length);
  unsigned nelt = TYPE_VECTOR_SUBPARTS (vectype);
  unsigned char *sel = XALLOCAVEC (unsigned char, nelt);

  *result_chain = VEC_copy (tree, heap, dr_chain);

  for (i = 0; i < nelt; ++i)
    sel[i] = i * 2;
  perm_mask_even = vect_gen_perm_mask (vectype, sel);
  gcc_assert (perm_mask_even != NULL);

  for (i = 0; i < nelt; ++i)
    sel[i] = i * 2 + 1;
  perm_mask_odd = vect_gen_perm_mask (vectype, sel);
  gcc_assert (perm_mask_odd != NULL);

  for (i = 0; i < log_length; i++)
    {
      for (j = 0; j < length; j += 2)
	{
	  first_vect = VEC_index (tree, dr_chain, j);
	  second_vect = VEC_index (tree, dr_chain, j+1);

	  /* data_ref = permute_even (first_data_ref, second_data_ref);  */
	  perm_dest = create_tmp_var (vectype, "vect_perm_even");
	  DECL_GIMPLE_REG_P (perm_dest) = 1;
	  add_referenced_var (perm_dest);

	  perm_stmt = gimple_build_assign_with_ops3 (VEC_PERM_EXPR, perm_dest,
						     first_vect, second_vect,
						     perm_mask_even);

	  data_ref = make_ssa_name (perm_dest, perm_stmt);
	  gimple_assign_set_lhs (perm_stmt, data_ref);
	  vect_finish_stmt_generation (stmt, perm_stmt, gsi);
	  mark_symbols_for_renaming (perm_stmt);

	  VEC_replace (tree, *result_chain, j/2, data_ref);

	  /* data_ref = permute_odd (first_data_ref, second_data_ref);  */
	  perm_dest = create_tmp_var (vectype, "vect_perm_odd");
	  DECL_GIMPLE_REG_P (perm_dest) = 1;
	  add_referenced_var (perm_dest);

	  perm_stmt = gimple_build_assign_with_ops3 (VEC_PERM_EXPR, perm_dest,
						     first_vect, second_vect,
						     perm_mask_odd);

	  data_ref = make_ssa_name (perm_dest, perm_stmt);
	  gimple_assign_set_lhs (perm_stmt, data_ref);
	  vect_finish_stmt_generation (stmt, perm_stmt, gsi);
	  mark_symbols_for_renaming (perm_stmt);

	  VEC_replace (tree, *result_chain, j/2+length/2, data_ref);
	}
      dr_chain = VEC_copy (tree, heap, *result_chain);
    }
}


/* Function vect_transform_strided_load.

   Given a chain of input interleaved data-refs (in DR_CHAIN), build statements
   to perform their permutation and ascribe the result vectorized statements to
   the scalar statements.
*/

void
vect_transform_strided_load (gimple stmt, VEC(tree,heap) *dr_chain, int size,
			     gimple_stmt_iterator *gsi)
{
  VEC(tree,heap) *result_chain = NULL;

  /* DR_CHAIN contains input data-refs that are a part of the interleaving.
     RESULT_CHAIN is the output of vect_permute_load_chain, it contains permuted
     vectors, that are ready for vector computation.  */
  result_chain = VEC_alloc (tree, heap, size);
  vect_permute_load_chain (dr_chain, size, stmt, gsi, &result_chain);
  vect_record_strided_load_vectors (stmt, result_chain);
  VEC_free (tree, heap, result_chain);
}

/* RESULT_CHAIN contains the output of a group of strided loads that were
   generated as part of the vectorization of STMT.  Assign the statement
   for each vector to the associated scalar statement.  */

void
vect_record_strided_load_vectors (gimple stmt, VEC(tree,heap) *result_chain)
{
  gimple first_stmt = GROUP_FIRST_ELEMENT (vinfo_for_stmt (stmt));
  gimple next_stmt, new_stmt;
  unsigned int i, gap_count;
  tree tmp_data_ref;

  /* Put a permuted data-ref in the VECTORIZED_STMT field.
     Since we scan the chain starting from it's first node, their order
     corresponds the order of data-refs in RESULT_CHAIN.  */
  next_stmt = first_stmt;
  gap_count = 1;
  FOR_EACH_VEC_ELT (tree, result_chain, i, tmp_data_ref)
    {
      if (!next_stmt)
	break;

      /* Skip the gaps.  Loads created for the gaps will be removed by dead
       code elimination pass later.  No need to check for the first stmt in
       the group, since it always exists.
       GROUP_GAP is the number of steps in elements from the previous
       access (if there is no gap GROUP_GAP is 1).  We skip loads that
       correspond to the gaps.  */
      if (next_stmt != first_stmt
          && gap_count < GROUP_GAP (vinfo_for_stmt (next_stmt)))
      {
        gap_count++;
        continue;
      }

      while (next_stmt)
        {
	  new_stmt = SSA_NAME_DEF_STMT (tmp_data_ref);
	  /* We assume that if VEC_STMT is not NULL, this is a case of multiple
	     copies, and we put the new vector statement in the first available
	     RELATED_STMT.  */
	  if (!STMT_VINFO_VEC_STMT (vinfo_for_stmt (next_stmt)))
	    STMT_VINFO_VEC_STMT (vinfo_for_stmt (next_stmt)) = new_stmt;
	  else
            {
              if (!GROUP_SAME_DR_STMT (vinfo_for_stmt (next_stmt)))
                {
 	          gimple prev_stmt =
		    STMT_VINFO_VEC_STMT (vinfo_for_stmt (next_stmt));
	          gimple rel_stmt =
		    STMT_VINFO_RELATED_STMT (vinfo_for_stmt (prev_stmt));
	          while (rel_stmt)
		    {
		      prev_stmt = rel_stmt;
		      rel_stmt =
                        STMT_VINFO_RELATED_STMT (vinfo_for_stmt (rel_stmt));
		    }

  	          STMT_VINFO_RELATED_STMT (vinfo_for_stmt (prev_stmt)) =
                    new_stmt;
                }
            }

	  next_stmt = GROUP_NEXT_ELEMENT (vinfo_for_stmt (next_stmt));
	  gap_count = 1;
	  /* If NEXT_STMT accesses the same DR as the previous statement,
	     put the same TMP_DATA_REF as its vectorized statement; otherwise
	     get the next data-ref from RESULT_CHAIN.  */
	  if (!next_stmt || !GROUP_SAME_DR_STMT (vinfo_for_stmt (next_stmt)))
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
  if (TREE_CODE (decl) != VAR_DECL)
    return false;

  if (DECL_EXTERNAL (decl))
    return false;

  if (TREE_ASM_WRITTEN (decl))
    return false;

  if (TREE_STATIC (decl))
    return (alignment <= MAX_OFILE_ALIGNMENT);
  else
    return (alignment <= MAX_STACK_ALIGNMENT);
}


/* Return whether the data reference DR is supported with respect to its
   alignment.
   If CHECK_ALIGNED_ACCESSES is TRUE, check if the access is supported even
   it is aligned, i.e., check if it is possible to vectorize it with different
   alignment.  */

enum dr_alignment_support
vect_supportable_dr_alignment (struct data_reference *dr,
                               bool check_aligned_accesses)
{
  gimple stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  enum machine_mode mode = TYPE_MODE (vectype);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *vect_loop = NULL;
  bool nested_in_vect_loop = false;

  if (aligned_access_p (dr) && !check_aligned_accesses)
    return dr_aligned;

  if (loop_vinfo)
    {
      vect_loop = LOOP_VINFO_LOOP (loop_vinfo);
      nested_in_vect_loop = nested_in_vect_loop_p (vect_loop, stmt);
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
	  if ((nested_in_vect_loop
	       && (TREE_INT_CST_LOW (DR_STEP (dr))
	 	   != GET_MODE_SIZE (TYPE_MODE (vectype))))
              || !loop_vinfo)
	    return dr_explicit_realign;
	  else
	    return dr_explicit_realign_optimized;
	}
      if (!known_alignment_for_access_p (dr))
	{
	  tree ba = DR_BASE_OBJECT (dr);

	  if (ba)
	    is_packed = contains_packed_reference (ba);
	}

      if (targetm.vectorize.
	  support_vector_misalignment (mode, type,
				       DR_MISALIGNMENT (dr), is_packed))
	/* Can't software pipeline the loads, but can at least do them.  */
	return dr_unaligned_supported;
    }
  else
    {
      bool is_packed = false;
      tree type = (TREE_TYPE (DR_REF (dr)));

      if (!known_alignment_for_access_p (dr))
	{
	  tree ba = DR_BASE_OBJECT (dr);

	  if (ba)
	    is_packed = contains_packed_reference (ba);
	}

     if (targetm.vectorize.
         support_vector_misalignment (mode, type,
				      DR_MISALIGNMENT (dr), is_packed))
       return dr_unaligned_supported;
    }

  /* Unsupported.  */
  return dr_unaligned_unsupported;
}
