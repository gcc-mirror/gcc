/* SSA operands management for trees.
   Copyright (C) 2003, 2004, 2005 Free Software Foundation, Inc.

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
#include "tree.h"
#include "flags.h"
#include "function.h"
#include "diagnostic.h"
#include "errors.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "ggc.h"
#include "timevar.h"

#include "langhooks.h"

/* This file contains the code required to manage the operands cache of the 
   SSA optimizer.  For every stmt, we maintain an operand cache in the stmt 
   annotation.  This cache contains operands that will be of interest to 
   optimizers and other passes wishing to manipulate the IL. 

   The operand type are broken up into REAL and VIRTUAL operands.  The real 
   operands are represented as pointers into the stmt's operand tree.  Thus 
   any manipulation of the real operands will be reflected in the actual tree.
   Virtual operands are represented solely in the cache, although the base 
   variable for the SSA_NAME may, or may not occur in the stmt's tree.  
   Manipulation of the virtual operands will not be reflected in the stmt tree.

   The routines in this file are concerned with creating this operand cache 
   from a stmt tree.

   get_stmt_operands() in the primary entry point. 

   The operand tree is the parsed by the various get_* routines which look 
   through the stmt tree for the occurrence of operands which may be of 
   interest, and calls are made to the append_* routines whenever one is 
   found.  There are 5 of these routines, each representing one of the 
   5 types of operands. Defs, Uses, Virtual Uses, Virtual May Defs, and 
   Virtual Must Defs.

   The append_* routines check for duplication, and simply keep a list of 
   unique objects for each operand type in the build_* extendable vectors.

   Once the stmt tree is completely parsed, the finalize_ssa_operands() 
   routine is called, which proceeds to perform the finalization routine 
   on each of the 5 operand vectors which have been built up.

   If the stmt had a previous operand cache, the finalization routines 
   attempt to match up the new operands with the old ones.  If its a perfect 
   match, the old vector is simply reused.  If it isn't a perfect match, then 
   a new vector is created and the new operands are placed there.  For 
   virtual operands, if the previous cache had SSA_NAME version of a 
   variable, and that same variable occurs in the same operands cache, then 
   the new cache vector will also get the same SSA_NAME.

  i.e., if a stmt had a VUSE of 'a_5', and 'a' occurs in the new operand 
  vector for VUSE, then the new vector will also be modified such that 
  it contains 'a_5' rather than 'a'.

*/


/* Flags to describe operand properties in get_stmt_operands and helpers.  */

/* By default, operands are loaded.  */
#define opf_none	0

/* Operand is the target of an assignment expression or a 
   call-clobbered variable  */
#define opf_is_def 	(1 << 0)

/* Operand is the target of an assignment expression.  */
#define opf_kill_def 	(1 << 1)

/* No virtual operands should be created in the expression.  This is used
   when traversing ADDR_EXPR nodes which have different semantics than
   other expressions.  Inside an ADDR_EXPR node, the only operands that we
   need to consider are indices into arrays.  For instance, &a.b[i] should
   generate a USE of 'i' but it should not generate a VUSE for 'a' nor a
   VUSE for 'b'.  */
#define opf_no_vops 	(1 << 2)

/* Array for building all the def operands.  */
static GTY (()) varray_type build_defs;

/* Array for building all the use operands.  */
static GTY (()) varray_type build_uses;

/* Array for building all the v_may_def operands.  */
static GTY (()) varray_type build_v_may_defs;

/* Array for building all the vuse operands.  */
static GTY (()) varray_type build_vuses;

/* Array for building all the v_must_def operands.  */
static GTY (()) varray_type build_v_must_defs;

/* True if the operands for call clobbered vars are cached and valid.  */
bool ssa_call_clobbered_cache_valid;
bool ssa_ro_call_cache_valid;

/* These arrays are the cached operand vectors for call clobbered calls.  */
static GTY (()) varray_type clobbered_v_may_defs;
static GTY (()) varray_type clobbered_vuses;
static GTY (()) varray_type ro_call_vuses;
static bool clobbered_aliased_loads;
static bool clobbered_aliased_stores;
static bool ro_call_aliased_loads;

#ifdef ENABLE_CHECKING
/* Used to make sure operand construction is working on the proper stmt.  */
tree check_build_stmt;
#endif

def_operand_p NULL_DEF_OPERAND_P = { NULL };
use_operand_p NULL_USE_OPERAND_P = { NULL };

static void note_addressable (tree, stmt_ann_t);
static void get_expr_operands (tree, tree *, int);
static void get_asm_expr_operands (tree);
static void get_indirect_ref_operands (tree, tree, int);
static void get_call_expr_operands (tree, tree);
static inline void append_def (tree *);
static inline void append_use (tree *);
static void append_v_may_def (tree);
static void append_v_must_def (tree);
static void add_call_clobber_ops (tree);
static void add_call_read_ops (tree);
static void add_stmt_operand (tree *, stmt_ann_t, int);

/* Return a vector of contiguous memory for NUM def operands.  */

static inline def_optype
allocate_def_optype (unsigned num)
{
  def_optype def_ops;
  unsigned size;
  size = sizeof (struct def_optype_d) + sizeof (tree *) * (num - 1);
  def_ops =  ggc_alloc (size);
  def_ops->num_defs = num;
  return def_ops;
}


/* Return a vector of contiguous memory for NUM use operands.  */

static inline use_optype
allocate_use_optype (unsigned num)
{
  use_optype use_ops;
  unsigned size;
  size = sizeof (struct use_optype_d) + sizeof (tree *) * (num - 1);
  use_ops =  ggc_alloc (size);
  use_ops->num_uses = num;
  return use_ops;
}


/* Return a vector of contiguous memory for NUM v_may_def operands.  */

static inline v_may_def_optype
allocate_v_may_def_optype (unsigned num)
{
  v_may_def_optype v_may_def_ops;
  unsigned size;
  size = sizeof (struct v_may_def_optype_d) 
	   + sizeof (v_def_use_operand_type_t) * (num - 1);
  v_may_def_ops =  ggc_alloc (size);
  v_may_def_ops->num_v_may_defs = num;
  return v_may_def_ops;
}


/* Return a vector of contiguous memory for NUM v_use operands.  */

static inline vuse_optype
allocate_vuse_optype (unsigned num)
{
  vuse_optype vuse_ops;
  unsigned size;
  size = sizeof (struct vuse_optype_d) + sizeof (tree) * (num - 1);
  vuse_ops =  ggc_alloc (size);
  vuse_ops->num_vuses = num;
  return vuse_ops;
}


/* Return a vector of contiguous memory for NUM v_must_def operands.  */

static inline v_must_def_optype
allocate_v_must_def_optype (unsigned num)
{
  v_must_def_optype v_must_def_ops;
  unsigned size;
  size = sizeof (struct v_must_def_optype_d) + sizeof (v_def_use_operand_type_t) * (num - 1);
  v_must_def_ops =  ggc_alloc (size);
  v_must_def_ops->num_v_must_defs = num;
  return v_must_def_ops;
}


/* Free memory for USES.  */

static inline void
free_uses (use_optype *uses)
{
  if (*uses)
    {
      ggc_free (*uses);
      *uses = NULL;
    }
}


/* Free memory for DEFS.  */

static inline void
free_defs (def_optype *defs)
{
  if (*defs)
    {
      ggc_free (*defs);
      *defs = NULL;
    }
}


/* Free memory for VUSES.  */

static inline void
free_vuses (vuse_optype *vuses)
{
  if (*vuses)
    {
      ggc_free (*vuses);
      *vuses = NULL;
    }
}


/* Free memory for V_MAY_DEFS.  */

static inline void
free_v_may_defs (v_may_def_optype *v_may_defs)
{
  if (*v_may_defs)
    {
      ggc_free (*v_may_defs);
      *v_may_defs = NULL;
    }
}


/* Free memory for V_MUST_DEFS.  */

static inline void
free_v_must_defs (v_must_def_optype *v_must_defs)
{
  if (*v_must_defs)
    {
      ggc_free (*v_must_defs);
      *v_must_defs = NULL;
    }
}


/* Initialize the operand cache routines.  */

void
init_ssa_operands (void)
{
  VARRAY_TREE_PTR_INIT (build_defs, 5, "build defs");
  VARRAY_TREE_PTR_INIT (build_uses, 10, "build uses");
  VARRAY_TREE_INIT (build_v_may_defs, 10, "build v_may_defs");
  VARRAY_TREE_INIT (build_vuses, 10, "build vuses");
  VARRAY_TREE_INIT (build_v_must_defs, 10, "build v_must_defs");
}


/* Dispose of anything required by the operand routines.  */

void
fini_ssa_operands (void)
{
  ggc_free (build_defs);
  ggc_free (build_uses);
  ggc_free (build_v_may_defs);
  ggc_free (build_vuses);
  ggc_free (build_v_must_defs);
  build_defs = NULL;
  build_uses = NULL;
  build_v_may_defs = NULL;
  build_vuses = NULL;
  build_v_must_defs = NULL;
  if (clobbered_v_may_defs)
    {
      ggc_free (clobbered_v_may_defs);
      ggc_free (clobbered_vuses);
      clobbered_v_may_defs = NULL;
      clobbered_vuses = NULL;
    }
  if (ro_call_vuses)
    {
      ggc_free (ro_call_vuses);
      ro_call_vuses = NULL;
    }
}


/* All the finalize_ssa_* routines do the work required to turn the build_
   VARRAY into an operand_vector of the appropriate type.  The original vector,
   if any, is passed in for comparison and virtual SSA_NAME reuse.  If the
   old vector is reused, the pointer passed in is set to NULL so that 
   the memory is not freed when the old operands are freed.  */

/* Return a new def operand vector for STMT, comparing to OLD_OPS_P.  */

static def_optype
finalize_ssa_defs (def_optype *old_ops_p, tree stmt ATTRIBUTE_UNUSED)
{
  unsigned num, x;
  def_optype def_ops, old_ops;
  bool build_diff;

  num = VARRAY_ACTIVE_SIZE (build_defs);
  if (num == 0)
    return NULL;

  /* There should only be a single real definition per assignment.  */
  gcc_assert (TREE_CODE (stmt) != MODIFY_EXPR || num <= 1);

  old_ops = *old_ops_p;

  /* Compare old vector and new array.  */
  build_diff = true;
  if (old_ops && old_ops->num_defs == num)
    {
      build_diff = false;
      for (x = 0; x < num; x++)
        if (old_ops->defs[x].def != VARRAY_TREE_PTR (build_defs, x))
	  {
	    build_diff = true;
	    break;
	  }
    }

  if (!build_diff)
    {
      def_ops = old_ops;
      *old_ops_p = NULL;
    }
  else
    {
      def_ops = allocate_def_optype (num);
      for (x = 0; x < num ; x++)
	def_ops->defs[x].def = VARRAY_TREE_PTR (build_defs, x);
    }

  VARRAY_POP_ALL (build_defs);

  return def_ops;
}


/* Return a new use operand vector for STMT, comparing to OLD_OPS_P.  */

static use_optype
finalize_ssa_uses (use_optype *old_ops_p, tree stmt ATTRIBUTE_UNUSED)
{
  unsigned num, x;
  use_optype use_ops, old_ops;
  bool build_diff;

  num = VARRAY_ACTIVE_SIZE (build_uses);
  if (num == 0)
    return NULL;

#ifdef ENABLE_CHECKING
  {
    unsigned x;
    /* If the pointer to the operand is the statement itself, something is
       wrong.  It means that we are pointing to a local variable (the 
       initial call to get_stmt_operands does not pass a pointer to a 
       statement).  */
    for (x = 0; x < num; x++)
      gcc_assert (*(VARRAY_TREE_PTR (build_uses, x)) != stmt);
  }
#endif
  old_ops = *old_ops_p;

  /* Check if the old vector and the new array are the same.  */
  build_diff = true;
  if (old_ops && old_ops->num_uses == num)
    {
      build_diff = false;
      for (x = 0; x < num; x++)
        if (old_ops->uses[x].use != VARRAY_TREE_PTR (build_uses, x))
	  {
	    build_diff = true;
	    break;
	  }
    }

  if (!build_diff)
    {
      use_ops = old_ops;
      *old_ops_p = NULL;
    }
  else
    {
      use_ops = allocate_use_optype (num);
      for (x = 0; x < num ; x++)
	use_ops->uses[x].use = VARRAY_TREE_PTR (build_uses, x);
    }
  VARRAY_POP_ALL (build_uses);

  return use_ops;
}


/* Return a new v_may_def operand vector for STMT, comparing to OLD_OPS_P.  */

static v_may_def_optype
finalize_ssa_v_may_defs (v_may_def_optype *old_ops_p)
{
  unsigned num, x, i, old_num;
  v_may_def_optype v_may_def_ops, old_ops;
  tree result, var;
  bool build_diff;

  num = VARRAY_ACTIVE_SIZE (build_v_may_defs);
  if (num == 0)
    return NULL;

  old_ops = *old_ops_p;

  /* Check if the old vector and the new array are the same.  */
  build_diff = true;
  if (old_ops && old_ops->num_v_may_defs == num)
    {
      old_num = num;
      build_diff = false;
      for (x = 0; x < num; x++)
        {
	  var = old_ops->v_may_defs[x].def;
	  if (TREE_CODE (var) == SSA_NAME)
	    var = SSA_NAME_VAR (var);
	  if (var != VARRAY_TREE (build_v_may_defs, x))
	    {
	      build_diff = true;
	      break;
	    }
	}
    }
  else
    old_num = (old_ops ? old_ops->num_v_may_defs : 0);

  if (!build_diff)
    {
      v_may_def_ops = old_ops;
      *old_ops_p = NULL;
    }
  else
    {
      v_may_def_ops = allocate_v_may_def_optype (num);
      for (x = 0; x < num; x++)
        {
	  var = VARRAY_TREE (build_v_may_defs, x);
	  /* Look for VAR in the old operands vector.  */
	  for (i = 0; i < old_num; i++)
	    {
	      result = old_ops->v_may_defs[i].def;
	      if (TREE_CODE (result) == SSA_NAME)
		result = SSA_NAME_VAR (result);
	      if (result == var)
	        {
		  v_may_def_ops->v_may_defs[x] = old_ops->v_may_defs[i];
		  break;
		}
	    }
	  if (i == old_num)
	    {
	      v_may_def_ops->v_may_defs[x].def = var;
	      v_may_def_ops->v_may_defs[x].use = var;
	    }
	}
    }

  /* Empty the V_MAY_DEF build vector after VUSES have been processed.  */

  return v_may_def_ops;
}


/* Clear the in_list bits and empty the build array for v_may_defs.  */

static inline void
cleanup_v_may_defs (void)
{
  unsigned x, num;
  num = VARRAY_ACTIVE_SIZE (build_v_may_defs);

  for (x = 0; x < num; x++)
    {
      tree t = VARRAY_TREE (build_v_may_defs, x);
      var_ann_t ann = var_ann (t);
      ann->in_v_may_def_list = 0;
    }
  VARRAY_POP_ALL (build_v_may_defs);
}

/* Return a new vuse operand vector, comparing to OLD_OPS_P.  */

static vuse_optype
finalize_ssa_vuses (vuse_optype *old_ops_p)
{
  unsigned num, x, i, num_v_may_defs, old_num;
  vuse_optype vuse_ops, old_ops;
  bool build_diff;

  num = VARRAY_ACTIVE_SIZE (build_vuses);
  if (num == 0)
    {
      cleanup_v_may_defs ();
      return NULL;
    }

  /* Remove superfluous VUSE operands.  If the statement already has a
   V_MAY_DEF operation for a variable 'a', then a VUSE for 'a' is not
   needed because V_MAY_DEFs imply a VUSE of the variable.  For instance,
   suppose that variable 'a' is aliased:

	      # VUSE <a_2>
	      # a_3 = V_MAY_DEF <a_2>
	      a = a + 1;

  The VUSE <a_2> is superfluous because it is implied by the V_MAY_DEF
  operation.  */

  num_v_may_defs = VARRAY_ACTIVE_SIZE (build_v_may_defs);

  if (num_v_may_defs > 0)
    {
      size_t i;
      tree vuse;
      for (i = 0; i < VARRAY_ACTIVE_SIZE (build_vuses); i++)
	{
	  vuse = VARRAY_TREE (build_vuses, i);
	  if (TREE_CODE (vuse) != SSA_NAME)
	    {
	      var_ann_t ann = var_ann (vuse);
	      ann->in_vuse_list = 0;
	      if (ann->in_v_may_def_list)
	        {
		  /* If we found a useless VUSE operand, remove it from the
		     operand array by replacing it with the last active element
		     in the operand array (unless the useless VUSE was the
		     last operand, in which case we simply remove it.  */
		  if (i != VARRAY_ACTIVE_SIZE (build_vuses) - 1)
		    {
		      VARRAY_TREE (build_vuses, i)
			= VARRAY_TREE (build_vuses,
				       VARRAY_ACTIVE_SIZE (build_vuses) - 1);
		    }
		  VARRAY_POP (build_vuses);

		  /* We want to rescan the element at this index, unless
		     this was the last element, in which case the loop
		     terminates.  */
		  i--;
		}
	    }
	}
    }
  else
    /* Clear out the in_list bits.  */
    for (x = 0; x < num; x++)
      {
	tree t = VARRAY_TREE (build_vuses, x);
	if (TREE_CODE (t) != SSA_NAME)
	  {
	    var_ann_t ann = var_ann (t);
	    ann->in_vuse_list = 0;
	  }
      }


  num = VARRAY_ACTIVE_SIZE (build_vuses);
  /* We could have reduced the size to zero now, however.  */
  if (num == 0)
    {
      cleanup_v_may_defs ();
      return NULL;
    }

  old_ops = *old_ops_p;

  /* Determine whether vuses is the same as the old vector.  */
  build_diff = true;
  if (old_ops && old_ops->num_vuses == num)
    {
      old_num = num;
      build_diff = false;
      for (x = 0; x < num ; x++)
        {
	  tree v;
	  v = old_ops->vuses[x];
	  if (TREE_CODE (v) == SSA_NAME)
	    v = SSA_NAME_VAR (v);
	  if (v != VARRAY_TREE (build_vuses, x))
	    {
	      build_diff = true;
	      break;
	    }
	}
    }
  else
    old_num = (old_ops ? old_ops->num_vuses : 0);

  if (!build_diff)
    {
      vuse_ops = old_ops;
      *old_ops_p = NULL;
    }
  else
    {
      vuse_ops = allocate_vuse_optype (num);
      for (x = 0; x < num; x++)
        {
	  tree result, var = VARRAY_TREE (build_vuses, x);
	  /* Look for VAR in the old vector, and use that SSA_NAME.  */
	  for (i = 0; i < old_num; i++)
	    {
	      result = old_ops->vuses[i];
	      if (TREE_CODE (result) == SSA_NAME)
		result = SSA_NAME_VAR (result);
	      if (result == var)
	        {
		  vuse_ops->vuses[x] = old_ops->vuses[i];
		  break;
		}
	    }
	  if (i == old_num)
	    vuse_ops->vuses[x] = var;
	}
    }

  /* The v_may_def build vector wasn't freed because we needed it here.
     Free it now with the vuses build vector.  */
  VARRAY_POP_ALL (build_vuses);
  cleanup_v_may_defs ();

  return vuse_ops;
}

/* Return a new v_must_def operand vector for STMT, comparing to OLD_OPS_P.  */

static v_must_def_optype
finalize_ssa_v_must_defs (v_must_def_optype *old_ops_p, 
			  tree stmt ATTRIBUTE_UNUSED)
{
  unsigned num, x, i, old_num = 0;
  v_must_def_optype v_must_def_ops, old_ops;
  bool build_diff;

  num = VARRAY_ACTIVE_SIZE (build_v_must_defs);
  if (num == 0)
    return NULL;

  /* There should only be a single V_MUST_DEF per assignment.  */
  gcc_assert (TREE_CODE (stmt) != MODIFY_EXPR || num <= 1);

  old_ops = *old_ops_p;

  /* Check if the old vector and the new array are the same.  */
  build_diff = true;
  if (old_ops && old_ops->num_v_must_defs == num)
    {
      old_num = num;
      build_diff = false;
      for (x = 0; x < num; x++)
        {
	  tree var = old_ops->v_must_defs[x].def;
	  if (TREE_CODE (var) == SSA_NAME)
	    var = SSA_NAME_VAR (var);
	  if (var != VARRAY_TREE (build_v_must_defs, x))
	    {
	      build_diff = true;
	      break;
	    }
	}
    }
  else
    old_num = (old_ops ? old_ops->num_v_must_defs : 0);

  if (!build_diff)
    {
      v_must_def_ops = old_ops;
      *old_ops_p = NULL;
    }
  else
    {
      v_must_def_ops = allocate_v_must_def_optype (num);
      for (x = 0; x < num ; x++)
	{
	  tree result, var = VARRAY_TREE (build_v_must_defs, x);
	  /* Look for VAR in the original vector.  */
	  for (i = 0; i < old_num; i++)
	    {
	      result = old_ops->v_must_defs[i].def;
	      if (TREE_CODE (result) == SSA_NAME)
		result = SSA_NAME_VAR (result);
	      if (result == var)
	        {
		  v_must_def_ops->v_must_defs[x].def = old_ops->v_must_defs[i].def;
		  v_must_def_ops->v_must_defs[x].use = old_ops->v_must_defs[i].use;
		  break;
		}
	    }
	  if (i == old_num)
	    {
	      v_must_def_ops->v_must_defs[x].def = var;
	      v_must_def_ops->v_must_defs[x].use = var;
	    }
	}
    }
  VARRAY_POP_ALL (build_v_must_defs);

  return v_must_def_ops;
}


/* Finalize all the build vectors, fill the new ones into INFO.  */

static inline void
finalize_ssa_stmt_operands (tree stmt, stmt_operands_p old_ops, 
			    stmt_operands_p new_ops)
{
  new_ops->def_ops = finalize_ssa_defs (&(old_ops->def_ops), stmt);
  new_ops->use_ops = finalize_ssa_uses (&(old_ops->use_ops), stmt);
  new_ops->v_must_def_ops 
    = finalize_ssa_v_must_defs (&(old_ops->v_must_def_ops), stmt);
  new_ops->v_may_def_ops = finalize_ssa_v_may_defs (&(old_ops->v_may_def_ops));
  new_ops->vuse_ops = finalize_ssa_vuses (&(old_ops->vuse_ops));
}


/* Start the process of building up operands vectors in INFO.  */

static inline void
start_ssa_stmt_operands (void)
{
  gcc_assert (VARRAY_ACTIVE_SIZE (build_defs) == 0);
  gcc_assert (VARRAY_ACTIVE_SIZE (build_uses) == 0);
  gcc_assert (VARRAY_ACTIVE_SIZE (build_vuses) == 0);
  gcc_assert (VARRAY_ACTIVE_SIZE (build_v_may_defs) == 0);
  gcc_assert (VARRAY_ACTIVE_SIZE (build_v_must_defs) == 0);
}


/* Add DEF_P to the list of pointers to operands.  */

static inline void
append_def (tree *def_p)
{
  VARRAY_PUSH_TREE_PTR (build_defs, def_p);
}


/* Add USE_P to the list of pointers to operands.  */

static inline void
append_use (tree *use_p)
{
  VARRAY_PUSH_TREE_PTR (build_uses, use_p);
}


/* Add a new virtual may def for variable VAR to the build array.  */

static inline void
append_v_may_def (tree var)
{
  var_ann_t ann = get_var_ann (var);

  /* Don't allow duplicate entries.  */
  if (ann->in_v_may_def_list)
    return;
  ann->in_v_may_def_list = 1;

  VARRAY_PUSH_TREE (build_v_may_defs, var);
}


/* Add VAR to the list of virtual uses.  */

static inline void
append_vuse (tree var)
{

  /* Don't allow duplicate entries.  */
  if (TREE_CODE (var) != SSA_NAME)
    {
      var_ann_t ann = get_var_ann (var);

      if (ann->in_vuse_list || ann->in_v_may_def_list)
        return;
      ann->in_vuse_list = 1;
    }

  VARRAY_PUSH_TREE (build_vuses, var);
}


/* Add VAR to the list of virtual must definitions for INFO.  */

static inline void
append_v_must_def (tree var)
{
  unsigned i;

  /* Don't allow duplicate entries.  */
  for (i = 0; i < VARRAY_ACTIVE_SIZE (build_v_must_defs); i++)
    if (var == VARRAY_TREE (build_v_must_defs, i))
      return;

  VARRAY_PUSH_TREE (build_v_must_defs, var);
}

/* Create an operands cache for STMT, returning it in NEW_OPS. OLD_OPS are the
   original operands, and if ANN is non-null, appropriate stmt flags are set
   in the stmt's annotation.  Note that some fields in old_ops may 
   change to NULL, although none of the memory they originally pointed to 
   will be destroyed.  It is appropriate to call free_stmt_operands() on 
   the value returned in old_ops.

   The rationale for this: Certain optimizations wish to examine the difference
   between new_ops and old_ops after processing.  If a set of operands don't
   change, new_ops will simply assume the pointer in old_ops, and the old_ops
   pointer will be set to NULL, indicating no memory needs to be cleared.  
   Usage might appear something like:

       old_ops_copy = old_ops = stmt_ann(stmt)->operands;
       build_ssa_operands (stmt, NULL, &old_ops, &new_ops);
          <* compare old_ops_copy and new_ops *>
       free_ssa_operands (old_ops);					*/

static void
build_ssa_operands (tree stmt, stmt_ann_t ann, stmt_operands_p old_ops, 
		    stmt_operands_p new_ops)
{
  enum tree_code code;
  tree_ann_t saved_ann = stmt->common.ann;
  
  /* Replace stmt's annotation with the one passed in for the duration
     of the operand building process.  This allows "fake" stmts to be built
     and not be included in other data structures which can be built here.  */
  stmt->common.ann = (tree_ann_t) ann;
  
  /* Initially assume that the statement has no volatile operands, nor
     makes aliased loads or stores.  */
  if (ann)
    {
      ann->has_volatile_ops = false;
      ann->makes_aliased_stores = false;
      ann->makes_aliased_loads = false;
    }

  start_ssa_stmt_operands ();

  code = TREE_CODE (stmt);
  switch (code)
    {
    case MODIFY_EXPR:
      /* First get operands from the RHS.  For the LHS, we use a V_MAY_DEF if
	 either only part of LHS is modified or if the RHS might throw,
	 otherwise, use V_MUST_DEF.

	 ??? If it might throw, we should represent somehow that it is killed
	 on the fallthrough path.  */
      {
	tree lhs = TREE_OPERAND (stmt, 0);
	int lhs_flags = opf_is_def;

	get_expr_operands (stmt, &TREE_OPERAND (stmt, 1), opf_none);

	/* If the LHS is a VIEW_CONVERT_EXPR, it isn't changing whether
	   or not the entire LHS is modified; that depends on what's
	   inside the VIEW_CONVERT_EXPR.  */
	if (TREE_CODE (lhs) == VIEW_CONVERT_EXPR)
	  lhs = TREE_OPERAND (lhs, 0);

	if (TREE_CODE (lhs) != ARRAY_REF && TREE_CODE (lhs) != ARRAY_RANGE_REF
	    && TREE_CODE (lhs) != COMPONENT_REF
	    && TREE_CODE (lhs) != BIT_FIELD_REF
	    && TREE_CODE (lhs) != REALPART_EXPR
	    && TREE_CODE (lhs) != IMAGPART_EXPR)
	  lhs_flags |= opf_kill_def;

        get_expr_operands (stmt, &TREE_OPERAND (stmt, 0), lhs_flags);
      }
      break;

    case COND_EXPR:
      get_expr_operands (stmt, &COND_EXPR_COND (stmt), opf_none);
      break;

    case SWITCH_EXPR:
      get_expr_operands (stmt, &SWITCH_COND (stmt), opf_none);
      break;

    case ASM_EXPR:
      get_asm_expr_operands (stmt);
      break;

    case RETURN_EXPR:
      get_expr_operands (stmt, &TREE_OPERAND (stmt, 0), opf_none);
      break;

    case GOTO_EXPR:
      get_expr_operands (stmt, &GOTO_DESTINATION (stmt), opf_none);
      break;

    case LABEL_EXPR:
      get_expr_operands (stmt, &LABEL_EXPR_LABEL (stmt), opf_none);
      break;

      /* These nodes contain no variable references.  */
    case BIND_EXPR:
    case CASE_LABEL_EXPR:
    case TRY_CATCH_EXPR:
    case TRY_FINALLY_EXPR:
    case EH_FILTER_EXPR:
    case CATCH_EXPR:
    case RESX_EXPR:
      break;

    default:
      /* Notice that if get_expr_operands tries to use &STMT as the operand
	 pointer (which may only happen for USE operands), we will abort in
	 append_use.  This default will handle statements like empty
	 statements, or CALL_EXPRs that may appear on the RHS of a statement
	 or as statements themselves.  */
      get_expr_operands (stmt, &stmt, opf_none);
      break;
    }

  finalize_ssa_stmt_operands (stmt, old_ops, new_ops);
  stmt->common.ann = saved_ann;
}


/* Free any operands vectors in OPS.  */

static void 
free_ssa_operands (stmt_operands_p ops)
{
  if (ops->def_ops)
    free_defs (&(ops->def_ops));
  if (ops->use_ops)
    free_uses (&(ops->use_ops));
  if (ops->vuse_ops)
    free_vuses (&(ops->vuse_ops));
  if (ops->v_may_def_ops)
    free_v_may_defs (&(ops->v_may_def_ops));
  if (ops->v_must_def_ops)
    free_v_must_defs (&(ops->v_must_def_ops));
}


/* Get the operands of statement STMT.  Note that repeated calls to
   get_stmt_operands for the same statement will do nothing until the
   statement is marked modified by a call to modify_stmt().  */

void
get_stmt_operands (tree stmt)
{
  stmt_ann_t ann;
  stmt_operands_t old_operands;

  /* The optimizers cannot handle statements that are nothing but a
     _DECL.  This indicates a bug in the gimplifier.  */
  gcc_assert (!SSA_VAR_P (stmt));

  /* Ignore error statements.  */
  if (TREE_CODE (stmt) == ERROR_MARK)
    return;

  ann = get_stmt_ann (stmt);

  /* If the statement has not been modified, the operands are still valid.  */
  if (!ann->modified)
    return;

  timevar_push (TV_TREE_OPS);

  old_operands = ann->operands;
  memset (&(ann->operands), 0, sizeof (stmt_operands_t));

  build_ssa_operands (stmt, ann, &old_operands, &(ann->operands));
  free_ssa_operands (&old_operands);

  /* Clear the modified bit for STMT.  Subsequent calls to
     get_stmt_operands for this statement will do nothing until the
     statement is marked modified by a call to modify_stmt().  */
  ann->modified = 0;

  timevar_pop (TV_TREE_OPS);
}


/* Recursively scan the expression pointed by EXPR_P in statement referred to
   by INFO.  FLAGS is one of the OPF_* constants modifying how to interpret the
   operands found.  */

static void
get_expr_operands (tree stmt, tree *expr_p, int flags)
{
  enum tree_code code;
  enum tree_code_class class;
  tree expr = *expr_p;
  stmt_ann_t s_ann = stmt_ann (stmt);

  if (expr == NULL || expr == error_mark_node)
    return;

  code = TREE_CODE (expr);
  class = TREE_CODE_CLASS (code);

  switch (code)
    {
    case ADDR_EXPR:
      /* We could have the address of a component, array member,
	 etc which has interesting variable references.  */
      /* Taking the address of a variable does not represent a
	 reference to it, but the fact that the stmt takes its address will be
	 of interest to some passes (e.g. alias resolution).  */
      add_stmt_operand (expr_p, s_ann, 0);

      /* If the address is invariant, there may be no interesting variable
	 references inside.  */
      if (is_gimple_min_invariant (expr))
	return;

      /* There should be no VUSEs created, since the referenced objects are
	 not really accessed.  The only operands that we should find here
	 are ARRAY_REF indices which will always be real operands (GIMPLE
	 does not allow non-registers as array indices).  */
      flags |= opf_no_vops;

      get_expr_operands (stmt, &TREE_OPERAND (expr, 0), flags);
      return;

    case SSA_NAME:
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
    case CONST_DECL:
      /* If we found a variable, add it to DEFS or USES depending
	 on the operand flags.  */
      add_stmt_operand (expr_p, s_ann, flags);
      return;

    case MISALIGNED_INDIRECT_REF:
      get_expr_operands (stmt, &TREE_OPERAND (expr, 1), flags);
      /* fall through */

    case ALIGN_INDIRECT_REF:
    case INDIRECT_REF:
      get_indirect_ref_operands (stmt, expr, flags);
      return;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      /* Treat array references as references to the virtual variable
	 representing the array.  The virtual variable for an ARRAY_REF
	 is the VAR_DECL for the array.  */

      /* Add the virtual variable for the ARRAY_REF to VDEFS or VUSES
	 according to the value of IS_DEF.  Recurse if the LHS of the
	 ARRAY_REF node is not a regular variable.  */
      if (SSA_VAR_P (TREE_OPERAND (expr, 0)))
	add_stmt_operand (expr_p, s_ann, flags);
      else
	get_expr_operands (stmt, &TREE_OPERAND (expr, 0), flags);

      get_expr_operands (stmt, &TREE_OPERAND (expr, 1), opf_none);
      get_expr_operands (stmt, &TREE_OPERAND (expr, 2), opf_none);
      get_expr_operands (stmt, &TREE_OPERAND (expr, 3), opf_none);
      return;

    case COMPONENT_REF:
    case REALPART_EXPR:
    case IMAGPART_EXPR:
      /* Similarly to arrays, references to compound variables (complex
	 types and structures/unions) are globbed.

	 FIXME: This means that

     			a.x = 6;
			a.y = 7;
			foo (a.x, a.y);

	 will not be constant propagated because the two partial
	 definitions to 'a' will kill each other.  Note that SRA may be
	 able to fix this problem if 'a' can be scalarized.  */

      /* If the LHS of the compound reference is not a regular variable,
	 recurse to keep looking for more operands in the subexpression.  */
      if (SSA_VAR_P (TREE_OPERAND (expr, 0)))
	add_stmt_operand (expr_p, s_ann, flags);
      else
	get_expr_operands (stmt, &TREE_OPERAND (expr, 0), flags);

      if (code == COMPONENT_REF)
	get_expr_operands (stmt, &TREE_OPERAND (expr, 2), opf_none);
      return;

    case WITH_SIZE_EXPR:
      /* WITH_SIZE_EXPR is a pass-through reference to its first argument,
	 and an rvalue reference to its second argument.  */
      get_expr_operands (stmt, &TREE_OPERAND (expr, 1), opf_none);
      get_expr_operands (stmt, &TREE_OPERAND (expr, 0), flags);
      return;

    case CALL_EXPR:
      get_call_expr_operands (stmt, expr);
      return;

    case COND_EXPR:
    case VEC_COND_EXPR:
      get_expr_operands (stmt, &TREE_OPERAND (expr, 0), opf_none);
      get_expr_operands (stmt, &TREE_OPERAND (expr, 1), opf_none);
      get_expr_operands (stmt, &TREE_OPERAND (expr, 2), opf_none);
      return;

    case MODIFY_EXPR:
      {
	int subflags;
	tree op;

	get_expr_operands (stmt, &TREE_OPERAND (expr, 1), opf_none);

	op = TREE_OPERAND (expr, 0);
	if (TREE_CODE (op) == WITH_SIZE_EXPR)
	  op = TREE_OPERAND (expr, 0);
	if (TREE_CODE (op) == ARRAY_REF
	    || TREE_CODE (op) == ARRAY_RANGE_REF
	    || TREE_CODE (op) == COMPONENT_REF
	    || TREE_CODE (op) == REALPART_EXPR
	    || TREE_CODE (op) == IMAGPART_EXPR)
	  subflags = opf_is_def;
	else
	  subflags = opf_is_def | opf_kill_def;

	get_expr_operands (stmt, &TREE_OPERAND (expr, 0), subflags);
	return;
      }

    case CONSTRUCTOR:
      {
	/* General aggregate CONSTRUCTORs have been decomposed, but they
	   are still in use as the COMPLEX_EXPR equivalent for vectors.  */

	tree t;
	for (t = TREE_OPERAND (expr, 0); t ; t = TREE_CHAIN (t))
	  get_expr_operands (stmt, &TREE_VALUE (t), opf_none);

	return;
      }

    case TRUTH_NOT_EXPR:
    case BIT_FIELD_REF:
    case VIEW_CONVERT_EXPR:
    do_unary:
      get_expr_operands (stmt, &TREE_OPERAND (expr, 0), flags);
      return;

    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case COMPOUND_EXPR:
    case OBJ_TYPE_REF:
    do_binary:
      {
	tree op0 = TREE_OPERAND (expr, 0);
	tree op1 = TREE_OPERAND (expr, 1);

	/* If it would be profitable to swap the operands, then do so to
	   canonicalize the statement, enabling better optimization.

	   By placing canonicalization of such expressions here we
	   transparently keep statements in canonical form, even
	   when the statement is modified.  */
	if (tree_swap_operands_p (op0, op1, false))
	  {
	    /* For relationals we need to swap the operands
	       and change the code.  */
	    if (code == LT_EXPR
		|| code == GT_EXPR
		|| code == LE_EXPR
		|| code == GE_EXPR)
	      {
		TREE_SET_CODE (expr, swap_tree_comparison (code));
		TREE_OPERAND (expr, 0) = op1;
		TREE_OPERAND (expr, 1) = op0;
	      }
	  
	    /* For a commutative operator we can just swap the operands.  */
	    else if (commutative_tree_code (code))
	      {
		TREE_OPERAND (expr, 0) = op1;
		TREE_OPERAND (expr, 1) = op0;
	      }
	  }

	get_expr_operands (stmt, &TREE_OPERAND (expr, 0), flags);
	get_expr_operands (stmt, &TREE_OPERAND (expr, 1), flags);
	return;
      }

    case REALIGN_LOAD_EXPR:
      {
	get_expr_operands (stmt, &TREE_OPERAND (expr, 0), flags);
        get_expr_operands (stmt, &TREE_OPERAND (expr, 1), flags);
        get_expr_operands (stmt, &TREE_OPERAND (expr, 2), flags);
        return;
      }

    case BLOCK:
    case FUNCTION_DECL:
    case EXC_PTR_EXPR:
    case FILTER_EXPR:
    case LABEL_DECL:
      /* Expressions that make no memory references.  */
      return;

    default:
      if (class == tcc_unary)
	goto do_unary;
      if (class == tcc_binary || class == tcc_comparison)
	goto do_binary;
      if (class == tcc_constant || class == tcc_type)
	return;
    }

  /* If we get here, something has gone wrong.  */
#ifdef ENABLE_CHECKING
  fprintf (stderr, "unhandled expression in get_expr_operands():\n");
  debug_tree (expr);
  fputs ("\n", stderr);
  internal_error ("internal error");
#endif
  gcc_unreachable ();
}


/* Scan operands in the ASM_EXPR stmt referred to in INFO.  */

static void
get_asm_expr_operands (tree stmt)
{
  stmt_ann_t s_ann = stmt_ann (stmt);
  int noutputs = list_length (ASM_OUTPUTS (stmt));
  const char **oconstraints
    = (const char **) alloca ((noutputs) * sizeof (const char *));
  int i;
  tree link;
  const char *constraint;
  bool allows_mem, allows_reg, is_inout;

  for (i=0, link = ASM_OUTPUTS (stmt); link; ++i, link = TREE_CHAIN (link))
    {
      oconstraints[i] = constraint
	= TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
      parse_output_constraint (&constraint, i, 0, 0,
	  &allows_mem, &allows_reg, &is_inout);

      /* This should have been split in gimplify_asm_expr.  */
      gcc_assert (!allows_reg || !is_inout);

      /* Memory operands are addressable.  Note that STMT needs the
	 address of this operand.  */
      if (!allows_reg && allows_mem)
	{
	  tree t = get_base_address (TREE_VALUE (link));
	  if (t && DECL_P (t))
	    note_addressable (t, s_ann);
	}

      get_expr_operands (stmt, &TREE_VALUE (link), opf_is_def);
    }

  for (link = ASM_INPUTS (stmt); link; link = TREE_CHAIN (link))
    {
      constraint
	= TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
      parse_input_constraint (&constraint, 0, 0, noutputs, 0,
	  oconstraints, &allows_mem, &allows_reg);

      /* Memory operands are addressable.  Note that STMT needs the
	 address of this operand.  */
      if (!allows_reg && allows_mem)
	{
	  tree t = get_base_address (TREE_VALUE (link));
	  if (t && DECL_P (t))
	    note_addressable (t, s_ann);
	}

      get_expr_operands (stmt, &TREE_VALUE (link), 0);
    }


  /* Clobber memory for asm ("" : : : "memory");  */
  for (link = ASM_CLOBBERS (stmt); link; link = TREE_CHAIN (link))
    if (strcmp (TREE_STRING_POINTER (TREE_VALUE (link)), "memory") == 0)
      {
	unsigned i;
	bitmap_iterator bi;

	/* Clobber all call-clobbered variables (or .GLOBAL_VAR if we
	   decided to group them).  */
	if (global_var)
	  add_stmt_operand (&global_var, s_ann, opf_is_def);
	else
	  EXECUTE_IF_SET_IN_BITMAP (call_clobbered_vars, 0, i, bi)
	      {
		tree var = referenced_var (i);
		add_stmt_operand (&var, s_ann, opf_is_def);
	      }

	/* Now clobber all addressables.  */
	EXECUTE_IF_SET_IN_BITMAP (addressable_vars, 0, i, bi)
	    {
	      tree var = referenced_var (i);
	      add_stmt_operand (&var, s_ann, opf_is_def);
	    }

	break;
      }
}

/* A subroutine of get_expr_operands to handle INDIRECT_REF,
   ALIGN_INDIRECT_REF and MISALIGNED_INDIRECT_REF.  */

static void
get_indirect_ref_operands (tree stmt, tree expr, int flags)
{
  tree *pptr = &TREE_OPERAND (expr, 0);
  tree ptr = *pptr;
  stmt_ann_t s_ann = stmt_ann (stmt);

  /* Stores into INDIRECT_REF operands are never killing definitions.  */
  flags &= ~opf_kill_def;

  if (SSA_VAR_P (ptr))
    {
      struct ptr_info_def *pi = NULL;

      /* If PTR has flow-sensitive points-to information, use it.  */
      if (TREE_CODE (ptr) == SSA_NAME
	  && (pi = SSA_NAME_PTR_INFO (ptr)) != NULL
	  && pi->name_mem_tag)
	{
	  /* PTR has its own memory tag.  Use it.  */
	  add_stmt_operand (&pi->name_mem_tag, s_ann, flags);
	}
      else
	{
	  /* If PTR is not an SSA_NAME or it doesn't have a name
	     tag, use its type memory tag.  */
	  var_ann_t v_ann;

	  /* If we are emitting debugging dumps, display a warning if
	     PTR is an SSA_NAME with no flow-sensitive alias
	     information.  That means that we may need to compute
	     aliasing again.  */
	  if (dump_file
	      && TREE_CODE (ptr) == SSA_NAME
	      && pi == NULL)
	    {
	      fprintf (dump_file,
		  "NOTE: no flow-sensitive alias info for ");
	      print_generic_expr (dump_file, ptr, dump_flags);
	      fprintf (dump_file, " in ");
	      print_generic_stmt (dump_file, stmt, dump_flags);
	    }

	  if (TREE_CODE (ptr) == SSA_NAME)
	    ptr = SSA_NAME_VAR (ptr);
	  v_ann = var_ann (ptr);
	  if (v_ann->type_mem_tag)
	    add_stmt_operand (&v_ann->type_mem_tag, s_ann, flags);
	}
    }

  /* If a constant is used as a pointer, we can't generate a real
     operand for it but we mark the statement volatile to prevent
     optimizations from messing things up.  */
  else if (TREE_CODE (ptr) == INTEGER_CST)
    {
      if (s_ann)
	s_ann->has_volatile_ops = true;
      return;
    }

  /* Everything else *should* have been folded elsewhere, but users
     are smarter than we in finding ways to write invalid code.  We
     cannot just abort here.  If we were absolutely certain that we
     do handle all valid cases, then we could just do nothing here.
     That seems optimistic, so attempt to do something logical... */
  else if ((TREE_CODE (ptr) == PLUS_EXPR || TREE_CODE (ptr) == MINUS_EXPR)
	   && TREE_CODE (TREE_OPERAND (ptr, 0)) == ADDR_EXPR
	   && TREE_CODE (TREE_OPERAND (ptr, 1)) == INTEGER_CST)
    {
      /* Make sure we know the object is addressable.  */
      pptr = &TREE_OPERAND (ptr, 0);
      add_stmt_operand (pptr, s_ann, 0);

      /* Mark the object itself with a VUSE.  */
      pptr = &TREE_OPERAND (*pptr, 0);
      get_expr_operands (stmt, pptr, flags);
      return;
    }

  /* Ok, this isn't even is_gimple_min_invariant.  Something's broke.  */
  else
    gcc_unreachable ();

  /* Add a USE operand for the base pointer.  */
  get_expr_operands (stmt, pptr, opf_none);
}

/* A subroutine of get_expr_operands to handle CALL_EXPR.  */

static void
get_call_expr_operands (tree stmt, tree expr)
{
  tree op;
  int call_flags = call_expr_flags (expr);

  /* If aliases have been computed already, add V_MAY_DEF or V_USE
     operands for all the symbols that have been found to be
     call-clobbered.
     
     Note that if aliases have not been computed, the global effects
     of calls will not be included in the SSA web. This is fine
     because no optimizer should run before aliases have been
     computed.  By not bothering with virtual operands for CALL_EXPRs
     we avoid adding superfluous virtual operands, which can be a
     significant compile time sink (See PR 15855).  */
  if (aliases_computed_p && !bitmap_empty_p (call_clobbered_vars))
    {
      /* A 'pure' or a 'const' functions never call clobber anything. 
	 A 'noreturn' function might, but since we don't return anyway 
	 there is no point in recording that.  */ 
      if (TREE_SIDE_EFFECTS (expr)
	  && !(call_flags & (ECF_PURE | ECF_CONST | ECF_NORETURN)))
	add_call_clobber_ops (stmt);
      else if (!(call_flags & ECF_CONST))
	add_call_read_ops (stmt);
    }

  /* Find uses in the called function.  */
  get_expr_operands (stmt, &TREE_OPERAND (expr, 0), opf_none);

  for (op = TREE_OPERAND (expr, 1); op; op = TREE_CHAIN (op))
    get_expr_operands (stmt, &TREE_VALUE (op), opf_none);

  get_expr_operands (stmt, &TREE_OPERAND (expr, 2), opf_none);

}


/* Add *VAR_P to the appropriate operand array for INFO.  FLAGS is as in
   get_expr_operands.  If *VAR_P is a GIMPLE register, it will be added to
   the statement's real operands, otherwise it is added to virtual
   operands.  */

static void
add_stmt_operand (tree *var_p, stmt_ann_t s_ann, int flags)
{
  bool is_real_op;
  tree var, sym;
  var_ann_t v_ann;

  var = *var_p;
  STRIP_NOPS (var);

  /* If the operand is an ADDR_EXPR, add its operand to the list of
     variables that have had their address taken in this statement.  */
  if (TREE_CODE (var) == ADDR_EXPR)
    {
      note_addressable (TREE_OPERAND (var, 0), s_ann);
      return;
    }

  /* If the original variable is not a scalar, it will be added to the list
     of virtual operands.  In that case, use its base symbol as the virtual
     variable representing it.  */
  is_real_op = is_gimple_reg (var);
  if (!is_real_op && !DECL_P (var))
    var = get_virtual_var (var);

  /* If VAR is not a variable that we care to optimize, do nothing.  */
  if (var == NULL_TREE || !SSA_VAR_P (var))
    return;

  sym = (TREE_CODE (var) == SSA_NAME ? SSA_NAME_VAR (var) : var);
  v_ann = var_ann (sym);

  /* Mark statements with volatile operands.  Optimizers should back
     off from statements having volatile operands.  */
  if (TREE_THIS_VOLATILE (sym) && s_ann)
    s_ann->has_volatile_ops = true;

  if (is_real_op)
    {
      /* The variable is a GIMPLE register.  Add it to real operands.  */
      if (flags & opf_is_def)
	append_def (var_p);
      else
	append_use (var_p);
    }
  else
    {
      varray_type aliases;

      /* The variable is not a GIMPLE register.  Add it (or its aliases) to
	 virtual operands, unless the caller has specifically requested
	 not to add virtual operands (used when adding operands inside an
	 ADDR_EXPR expression).  */
      if (flags & opf_no_vops)
	return;

      aliases = v_ann->may_aliases;

      if (aliases == NULL)
	{
	  /* The variable is not aliased or it is an alias tag.  */
	  if (flags & opf_is_def)
	    {
	      if (flags & opf_kill_def)
		{
		  /* Only regular variables may get a V_MUST_DEF
		     operand.  */
		  gcc_assert (v_ann->mem_tag_kind == NOT_A_TAG);
		  /* V_MUST_DEF for non-aliased, non-GIMPLE register 
		    variable definitions.  */
		  append_v_must_def (var);
		}
	      else
		{
		  /* Add a V_MAY_DEF for call-clobbered variables and
		     memory tags.  */
		  append_v_may_def (var);
		}
	    }
	  else
	    {
	      append_vuse (var);
	      if (s_ann && v_ann->is_alias_tag)
		s_ann->makes_aliased_loads = 1;
	    }
	}
      else
	{
	  size_t i;

	  /* The variable is aliased.  Add its aliases to the virtual
	     operands.  */
	  gcc_assert (VARRAY_ACTIVE_SIZE (aliases) != 0);

	  if (flags & opf_is_def)
	    {
	      /* If the variable is also an alias tag, add a virtual
		 operand for it, otherwise we will miss representing
		 references to the members of the variable's alias set.
		 This fixes the bug in gcc.c-torture/execute/20020503-1.c.  */
	      if (v_ann->is_alias_tag)
		append_v_may_def (var);

	      for (i = 0; i < VARRAY_ACTIVE_SIZE (aliases); i++)
		append_v_may_def (VARRAY_TREE (aliases, i));

	      if (s_ann)
		s_ann->makes_aliased_stores = 1;
	    }
	  else
	    {
	      /* Similarly, append a virtual uses for VAR itself, when
		 it is an alias tag.  */
	      if (v_ann->is_alias_tag)
		append_vuse (var);

	      for (i = 0; i < VARRAY_ACTIVE_SIZE (aliases); i++)
		append_vuse (VARRAY_TREE (aliases, i));

	      if (s_ann)
		s_ann->makes_aliased_loads = 1;
	    }
	}
    }
}


/* Record that VAR had its address taken in the statement with annotations
   S_ANN.  */

static void
note_addressable (tree var, stmt_ann_t s_ann)
{
  if (!s_ann)
    return;

  var = get_base_address (var);
  if (var && SSA_VAR_P (var))
    {
      if (s_ann->addresses_taken == NULL)
	s_ann->addresses_taken = BITMAP_GGC_ALLOC ();
      bitmap_set_bit (s_ann->addresses_taken, var_ann (var)->uid);
    }
}


/* Add clobbering definitions for .GLOBAL_VAR or for each of the call
   clobbered variables in the function.  */

static void
add_call_clobber_ops (tree stmt)
{
  unsigned i;
  tree t;
  bitmap_iterator bi;
  stmt_ann_t s_ann = stmt_ann (stmt);
  struct stmt_ann_d empty_ann;

  /* Functions that are not const, pure or never return may clobber
     call-clobbered variables.  */
  if (s_ann)
    s_ann->makes_clobbering_call = true;

  /* If we created .GLOBAL_VAR earlier, just use it.  See compute_may_aliases 
     for the heuristic used to decide whether to create .GLOBAL_VAR or not.  */
  if (global_var)
    {
      add_stmt_operand (&global_var, s_ann, opf_is_def);
      return;
    }

  /* If cache is valid, copy the elements into the build vectors.  */
  if (ssa_call_clobbered_cache_valid)
    {
      for (i = 0; i < VARRAY_ACTIVE_SIZE (clobbered_vuses); i++)
	{
	  t = VARRAY_TREE (clobbered_vuses, i);
	  gcc_assert (TREE_CODE (t) != SSA_NAME);
	  var_ann (t)->in_vuse_list = 1;
	  VARRAY_PUSH_TREE (build_vuses, t);
	}
      for (i = 0; i < VARRAY_ACTIVE_SIZE (clobbered_v_may_defs); i++)
	{
	  t = VARRAY_TREE (clobbered_v_may_defs, i);
	  gcc_assert (TREE_CODE (t) != SSA_NAME);
	  var_ann (t)->in_v_may_def_list = 1;
	  VARRAY_PUSH_TREE (build_v_may_defs, t);
	}
      if (s_ann)
	{
	  s_ann->makes_aliased_loads = clobbered_aliased_loads;
	  s_ann->makes_aliased_stores = clobbered_aliased_stores;
	}
      return;
    }

  memset (&empty_ann, 0, sizeof (struct stmt_ann_d));

  /* Add a V_MAY_DEF operand for every call clobbered variable.  */
  EXECUTE_IF_SET_IN_BITMAP (call_clobbered_vars, 0, i, bi)
    {
      tree var = referenced_var (i);
      if (TREE_READONLY (var)
	  && (TREE_STATIC (var) || DECL_EXTERNAL (var)))
	add_stmt_operand (&var, &empty_ann, opf_none);
      else
	add_stmt_operand (&var, &empty_ann, opf_is_def);
    }

  clobbered_aliased_loads = empty_ann.makes_aliased_loads;
  clobbered_aliased_stores = empty_ann.makes_aliased_stores;

  /* Set the flags for a stmt's annotation.  */
  if (s_ann)
    {
      s_ann->makes_aliased_loads = empty_ann.makes_aliased_loads;
      s_ann->makes_aliased_stores = empty_ann.makes_aliased_stores;
    }

  /* Prepare empty cache vectors.  */
  if (clobbered_v_may_defs)
    {
      VARRAY_POP_ALL (clobbered_vuses);
      VARRAY_POP_ALL (clobbered_v_may_defs);
    }
  else
    {
      VARRAY_TREE_INIT (clobbered_v_may_defs, 10, "clobbered_v_may_defs");
      VARRAY_TREE_INIT (clobbered_vuses, 10, "clobbered_vuses");
    }

  /* Now fill the clobbered cache with the values that have been found.  */
  for (i = 0; i < VARRAY_ACTIVE_SIZE (build_vuses); i++)
    VARRAY_PUSH_TREE (clobbered_vuses, VARRAY_TREE (build_vuses, i));
  for (i = 0; i < VARRAY_ACTIVE_SIZE (build_v_may_defs); i++)
    VARRAY_PUSH_TREE (clobbered_v_may_defs, VARRAY_TREE (build_v_may_defs, i));

  ssa_call_clobbered_cache_valid = true;
}


/* Add VUSE operands for .GLOBAL_VAR or all call clobbered variables in the
   function.  */

static void
add_call_read_ops (tree stmt)
{
  unsigned i;
  tree t;
  bitmap_iterator bi;
  stmt_ann_t s_ann = stmt_ann (stmt);
  struct stmt_ann_d empty_ann;

  /* if the function is not pure, it may reference memory.  Add
     a VUSE for .GLOBAL_VAR if it has been created.  See add_referenced_var
     for the heuristic used to decide whether to create .GLOBAL_VAR.  */
  if (global_var)
    {
      add_stmt_operand (&global_var, s_ann, opf_none);
      return;
    }
  
  /* If cache is valid, copy the elements into the build vector.  */
  if (ssa_ro_call_cache_valid)
    {
      for (i = 0; i < VARRAY_ACTIVE_SIZE (ro_call_vuses); i++)
	{
	  t = VARRAY_TREE (ro_call_vuses, i);
	  gcc_assert (TREE_CODE (t) != SSA_NAME);
	  var_ann (t)->in_vuse_list = 1;
	  VARRAY_PUSH_TREE (build_vuses, t);
	}
      if (s_ann)
	s_ann->makes_aliased_loads = ro_call_aliased_loads;
      return;
    }

  memset (&empty_ann, 0, sizeof (struct stmt_ann_d));

  /* Add a VUSE for each call-clobbered variable.  */
  EXECUTE_IF_SET_IN_BITMAP (call_clobbered_vars, 0, i, bi)
    {
      tree var = referenced_var (i);
      add_stmt_operand (&var, &empty_ann, opf_none);
    }

  ro_call_aliased_loads = empty_ann.makes_aliased_loads;
  if (s_ann)
    s_ann->makes_aliased_loads = empty_ann.makes_aliased_loads;

  /* Prepare empty cache vectors.  */
  if (ro_call_vuses)
    VARRAY_POP_ALL (ro_call_vuses);
  else
    VARRAY_TREE_INIT (ro_call_vuses, 10, "ro_call_vuses");

  /* Now fill the clobbered cache with the values that have been found.  */
  for (i = 0; i < VARRAY_ACTIVE_SIZE (build_vuses); i++)
    VARRAY_PUSH_TREE (ro_call_vuses, VARRAY_TREE (build_vuses, i));

  ssa_ro_call_cache_valid = true;
}

/* Copies virtual operands from SRC to DST.  */

void
copy_virtual_operands (tree dst, tree src)
{
  unsigned i;
  vuse_optype vuses = STMT_VUSE_OPS (src);
  v_may_def_optype v_may_defs = STMT_V_MAY_DEF_OPS (src);
  v_must_def_optype v_must_defs = STMT_V_MUST_DEF_OPS (src);
  vuse_optype *vuses_new = &stmt_ann (dst)->operands.vuse_ops;
  v_may_def_optype *v_may_defs_new = &stmt_ann (dst)->operands.v_may_def_ops;
  v_must_def_optype *v_must_defs_new = &stmt_ann (dst)->operands.v_must_def_ops;

  if (vuses)
    {
      *vuses_new = allocate_vuse_optype (NUM_VUSES (vuses));
      for (i = 0; i < NUM_VUSES (vuses); i++)
	SET_VUSE_OP (*vuses_new, i, VUSE_OP (vuses, i));
    }

  if (v_may_defs)
    {
      *v_may_defs_new = allocate_v_may_def_optype (NUM_V_MAY_DEFS (v_may_defs));
      for (i = 0; i < NUM_V_MAY_DEFS (v_may_defs); i++)
	{
	  SET_V_MAY_DEF_OP (*v_may_defs_new, i, V_MAY_DEF_OP (v_may_defs, i));
	  SET_V_MAY_DEF_RESULT (*v_may_defs_new, i, 
				V_MAY_DEF_RESULT (v_may_defs, i));
	}
    }

  if (v_must_defs)
    {
      *v_must_defs_new = allocate_v_must_def_optype (NUM_V_MUST_DEFS (v_must_defs));
      for (i = 0; i < NUM_V_MUST_DEFS (v_must_defs); i++)
	{
	  SET_V_MUST_DEF_RESULT (*v_must_defs_new, i, V_MUST_DEF_RESULT (v_must_defs, i));
	  SET_V_MUST_DEF_KILL (*v_must_defs_new, i, V_MUST_DEF_KILL (v_must_defs, i));
	}
    }
}


/* Specifically for use in DOM's expression analysis.  Given a store, we
   create an artificial stmt which looks like a load from the store, this can
   be used to eliminate redundant loads.  OLD_OPS are the operands from the 
   store stmt, and NEW_STMT is the new load which represents a load of the
   values stored.  */

void
create_ssa_artficial_load_stmt (stmt_operands_p old_ops, tree new_stmt)
{
  stmt_ann_t ann;
  tree op;
  stmt_operands_t tmp;
  unsigned j;

  memset (&tmp, 0, sizeof (stmt_operands_t));
  ann = get_stmt_ann (new_stmt);

  /* Free operands just in case is was an existing stmt.  */
  free_ssa_operands (&(ann->operands));

  build_ssa_operands (new_stmt, NULL, &tmp, &(ann->operands));
  free_vuses (&(ann->operands.vuse_ops));
  free_v_may_defs (&(ann->operands.v_may_def_ops));
  free_v_must_defs (&(ann->operands.v_must_def_ops));
  
  /* For each VDEF on the original statement, we want to create a
     VUSE of the V_MAY_DEF result or V_MUST_DEF op on the new 
     statement.  */
  for (j = 0; j < NUM_V_MAY_DEFS (old_ops->v_may_def_ops); j++)
    {
      op = V_MAY_DEF_RESULT (old_ops->v_may_def_ops, j);
      append_vuse (op);
    }
    
  for (j = 0; j < NUM_V_MUST_DEFS (old_ops->v_must_def_ops); j++)
    {
      op = V_MUST_DEF_RESULT (old_ops->v_must_def_ops, j);
      append_vuse (op);
    }

  /* Now set the vuses for this new stmt.  */
  ann->operands.vuse_ops = finalize_ssa_vuses (&(tmp.vuse_ops));
}

#include "gt-tree-ssa-operands.h"
