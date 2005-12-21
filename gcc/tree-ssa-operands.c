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
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "flags.h"
#include "function.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "ggc.h"
#include "timevar.h"
#include "toplev.h"
#include "langhooks.h"
#include "ipa-reference.h"

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
   attempt to match up the new operands with the old ones.  If it's a perfect 
   match, the old vector is simply reused.  If it isn't a perfect match, then 
   a new vector is created and the new operands are placed there.  For 
   virtual operands, if the previous cache had SSA_NAME version of a 
   variable, and that same variable occurs in the same operands cache, then 
   the new cache vector will also get the same SSA_NAME.

  i.e., if a stmt had a VUSE of 'a_5', and 'a' occurs in the new operand 
  vector for VUSE, then the new vector will also be modified such that 
  it contains 'a_5' rather than 'a'.

*/


/* Flags to describe operand properties in helpers.  */

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

/* Operand is a "non-specific" kill for call-clobbers and such.  This is used
   to distinguish "reset the world" events from explicit MODIFY_EXPRs.  */
#define opf_non_specific  (1 << 3)


/* Array for building all the def operands.  */
static VEC(tree,heap) *build_defs;

/* Array for building all the use operands.  */
static VEC(tree,heap) *build_uses;

/* Array for building all the v_may_def operands.  */
static VEC(tree,heap) *build_v_may_defs;

/* Array for building all the vuse operands.  */
static VEC(tree,heap) *build_vuses;

/* Array for building all the v_must_def operands.  */
static VEC(tree,heap) *build_v_must_defs;

/* True if the operands for call clobbered vars are cached and valid.  */
bool ssa_call_clobbered_cache_valid;
bool ssa_ro_call_cache_valid;

/* These arrays are the cached operand vectors for call clobbered calls.  */
static VEC(tree,heap) *clobbered_v_may_defs;
static VEC(tree,heap) *clobbered_vuses;
static VEC(tree,heap) *ro_call_vuses;
static bool clobbered_aliased_loads;
static bool clobbered_aliased_stores;
static bool ro_call_aliased_loads;
static bool ops_active = false;

static GTY (()) struct ssa_operand_memory_d *operand_memory = NULL;
static unsigned operand_memory_index;

static void get_expr_operands (tree, tree *, int);
static void get_asm_expr_operands (tree);
static void get_indirect_ref_operands (tree, tree, int);
static void get_tmr_operands (tree, tree, int);
static void get_call_expr_operands (tree, tree);
static inline void append_def (tree *);
static inline void append_use (tree *);
static void append_v_may_def (tree);
static void append_v_must_def (tree);
static void add_call_clobber_ops (tree, tree);
static void add_call_read_ops (tree);
static void add_stmt_operand (tree *, stmt_ann_t, int);
static void build_ssa_operands (tree stmt);
                                                                                
static def_optype_p free_defs = NULL;
static use_optype_p free_uses = NULL;
static vuse_optype_p free_vuses = NULL;
static maydef_optype_p free_maydefs = NULL;
static mustdef_optype_p free_mustdefs = NULL;


/* Return the DECL_UID of the base variable of T.  */

static inline unsigned
get_name_decl (tree t)
{
  if (TREE_CODE (t) != SSA_NAME)
    return DECL_UID (t);
  else
    return DECL_UID (SSA_NAME_VAR (t));
}

/* Comparison function for qsort used in operand_build_sort_virtual.  */

static int
operand_build_cmp (const void *p, const void *q)
{
  tree e1 = *((const tree *)p);
  tree e2 = *((const tree *)q);
  unsigned int u1,u2;

  u1 = get_name_decl (e1);
  u2 = get_name_decl (e2);

  /* We want to sort in ascending order.  They can never be equal.  */
#ifdef ENABLE_CHECKING
  gcc_assert (u1 != u2);
#endif
  return (u1 > u2 ? 1 : -1);
}

/* Sort the virtual operands in LIST from lowest DECL_UID to highest.  */

static inline void
operand_build_sort_virtual (VEC(tree,heap) *list)
{
  int num = VEC_length (tree, list);
  if (num < 2)
    return;
  if (num == 2)
    {
      if (get_name_decl (VEC_index (tree, list, 0)) 
	  > get_name_decl (VEC_index (tree, list, 1)))
	{  
	  /* Swap elements if in the wrong order.  */
	  tree tmp = VEC_index (tree, list, 0);
	  VEC_replace (tree, list, 0, VEC_index (tree, list, 1));
	  VEC_replace (tree, list, 1, tmp);
	}
      return;
    }
  /* There are 3 or more elements, call qsort.  */
  qsort (VEC_address (tree, list), 
	 VEC_length (tree, list), 
	 sizeof (tree),
	 operand_build_cmp);
}



/*  Return true if the ssa operands cache is active.  */

bool
ssa_operands_active (void)
{
  return ops_active;
}


/* Initialize the operand cache routines.  */

void
init_ssa_operands (void)
{
  build_defs = VEC_alloc (tree, heap, 5);
  build_uses = VEC_alloc (tree, heap, 10);
  build_vuses = VEC_alloc (tree, heap, 25);
  build_v_may_defs = VEC_alloc (tree, heap, 25);
  build_v_must_defs = VEC_alloc (tree, heap, 25);

  gcc_assert (operand_memory == NULL);
  operand_memory_index = SSA_OPERAND_MEMORY_SIZE;
  ops_active = true;
}


/* Dispose of anything required by the operand routines.  */

void
fini_ssa_operands (void)
{
  struct ssa_operand_memory_d *ptr;
  VEC_free (tree, heap, build_defs);
  VEC_free (tree, heap, build_uses);
  VEC_free (tree, heap, build_v_must_defs);
  VEC_free (tree, heap, build_v_may_defs);
  VEC_free (tree, heap, build_vuses);
  free_defs = NULL;
  free_uses = NULL;
  free_vuses = NULL;
  free_maydefs = NULL;
  free_mustdefs = NULL;
  while ((ptr = operand_memory) != NULL)
    {
      operand_memory = operand_memory->next;
      ggc_free (ptr);
    }

  VEC_free (tree, heap, clobbered_v_may_defs);
  VEC_free (tree, heap, clobbered_vuses);
  VEC_free (tree, heap, ro_call_vuses);
  ops_active = false;
}


/* Return memory for operands of SIZE chunks.  */
                                                                              
static inline void *
ssa_operand_alloc (unsigned size)
{
  char *ptr;
  if (operand_memory_index + size >= SSA_OPERAND_MEMORY_SIZE)
    {
      struct ssa_operand_memory_d *ptr;
      ptr = ggc_alloc (sizeof (struct ssa_operand_memory_d));
      ptr->next = operand_memory;
      operand_memory = ptr;
      operand_memory_index = 0;
    }
  ptr = &(operand_memory->mem[operand_memory_index]);
  operand_memory_index += size;
  return ptr;
}


/* Make sure PTR is in the correct immediate use list.  Since uses are simply
   pointers into the stmt TREE, there is no way of telling if anyone has
   changed what this pointer points to via TREE_OPERANDS (exp, 0) = <...>.
   The contents are different, but the pointer is still the same.  This
   routine will check to make sure PTR is in the correct list, and if it isn't
   put it in the correct list.  We cannot simply check the previous node 
   because all nodes in the same stmt might have be changed.  */

static inline void
correct_use_link (use_operand_p ptr, tree stmt)
{
  use_operand_p prev;
  tree root;

  /*  Fold_stmt () may have changed the stmt pointers.  */
  if (ptr->stmt != stmt)
    ptr->stmt = stmt;

  prev = ptr->prev;
  if (prev)
    {
      /* Find the root element, making sure we skip any safe iterators.  */
      while (prev->use != NULL || prev->stmt == NULL)
	prev = prev->prev;

      /* Get the ssa_name of the list the node is in.  */
      root = prev->stmt;
      /* If it's the right list, simply return.  */
      if (root == *(ptr->use))
	return;
    }
  /* Its in the wrong list if we reach here.  */
  delink_imm_use (ptr);
  link_imm_use (ptr, *(ptr->use));
}


/* This routine makes sure that PTR is in an immediate use list, and makes
   sure the stmt pointer is set to the current stmt.  Virtual uses do not need
   the overhead of correct_use_link since they cannot be directly manipulated
   like a real use can be.  (They don't exist in the TREE_OPERAND nodes.)  */
static inline void
set_virtual_use_link (use_operand_p ptr, tree stmt)
{
  /*  Fold_stmt () may have changed the stmt pointers.  */
  if (ptr->stmt != stmt)
    ptr->stmt = stmt;

  /* If this use isn't in a list, add it to the correct list.  */
  if (!ptr->prev)
    link_imm_use (ptr, *(ptr->use));
}



#define FINALIZE_OPBUILD		build_defs
#define FINALIZE_OPBUILD_BASE(I)	(tree *)VEC_index (tree,	\
							   build_defs, (I))
#define FINALIZE_OPBUILD_ELEM(I)	(tree *)VEC_index (tree,	\
							   build_defs, (I))
#define FINALIZE_FUNC			finalize_ssa_def_ops
#define FINALIZE_ALLOC			alloc_def
#define FINALIZE_FREE			free_defs
#define FINALIZE_TYPE			struct def_optype_d
#define FINALIZE_ELEM(PTR)		((PTR)->def_ptr)
#define FINALIZE_OPS			DEF_OPS
#define FINALIZE_BASE(VAR)		VAR
#define FINALIZE_BASE_TYPE		tree *
#define FINALIZE_BASE_ZERO		NULL
#define FINALIZE_INITIALIZE(PTR, VAL, STMT)	FINALIZE_ELEM (PTR) = (VAL)
#include "tree-ssa-opfinalize.h"


/* This routine will create stmt operands for STMT from the def build list.  */

static void
finalize_ssa_defs (tree stmt)
{
  unsigned int num = VEC_length (tree, build_defs);
  /* There should only be a single real definition per assignment.  */
  gcc_assert ((stmt && TREE_CODE (stmt) != MODIFY_EXPR) || num <= 1);

  /* If there is an old list, often the new list is identical, or close, so
     find the elements at the beginning that are the same as the vector.  */

  finalize_ssa_def_ops (stmt);
  VEC_truncate (tree, build_defs, 0);
}

#define FINALIZE_OPBUILD	build_uses
#define FINALIZE_OPBUILD_BASE(I)	(tree *)VEC_index (tree,	\
							   build_uses, (I))
#define FINALIZE_OPBUILD_ELEM(I)	(tree *)VEC_index (tree,	\
							   build_uses, (I))
#define FINALIZE_FUNC		finalize_ssa_use_ops
#define FINALIZE_ALLOC		alloc_use
#define FINALIZE_FREE		free_uses
#define FINALIZE_TYPE		struct use_optype_d
#define FINALIZE_ELEM(PTR)	((PTR)->use_ptr.use)
#define FINALIZE_OPS		USE_OPS
#define FINALIZE_USE_PTR(PTR)	USE_OP_PTR (PTR)
#define FINALIZE_CORRECT_USE	correct_use_link
#define FINALIZE_BASE(VAR)	VAR
#define FINALIZE_BASE_TYPE	tree *
#define FINALIZE_BASE_ZERO	NULL
#define FINALIZE_INITIALIZE(PTR, VAL, STMT)				\
				(PTR)->use_ptr.use = (VAL);             \
				link_imm_use_stmt (&((PTR)->use_ptr),   \
						   *(VAL), (STMT))
#include "tree-ssa-opfinalize.h"

/* Return a new use operand vector for STMT, comparing to OLD_OPS_P.  */
                                                                              
static void
finalize_ssa_uses (tree stmt)
{
#ifdef ENABLE_CHECKING
  {
    unsigned x;
    unsigned num = VEC_length (tree, build_uses);

    /* If the pointer to the operand is the statement itself, something is
       wrong.  It means that we are pointing to a local variable (the 
       initial call to get_stmt_operands does not pass a pointer to a 
       statement).  */
    for (x = 0; x < num; x++)
      gcc_assert (*((tree *)VEC_index (tree, build_uses, x)) != stmt);
  }
#endif
  finalize_ssa_use_ops (stmt);
  VEC_truncate (tree, build_uses, 0);
}
                                                                              
                                                                              
/* Return a new v_may_def operand vector for STMT, comparing to OLD_OPS_P.  */                                                                                
#define FINALIZE_OPBUILD	build_v_may_defs
#define FINALIZE_OPBUILD_ELEM(I)	VEC_index (tree, build_v_may_defs, (I))
#define FINALIZE_OPBUILD_BASE(I)	get_name_decl (VEC_index (tree,	\
							build_v_may_defs, (I)))
#define FINALIZE_FUNC		finalize_ssa_v_may_def_ops
#define FINALIZE_ALLOC		alloc_maydef
#define FINALIZE_FREE		free_maydefs
#define FINALIZE_TYPE		struct maydef_optype_d
#define FINALIZE_ELEM(PTR)	MAYDEF_RESULT (PTR)
#define FINALIZE_OPS		MAYDEF_OPS
#define FINALIZE_USE_PTR(PTR)	MAYDEF_OP_PTR (PTR)
#define FINALIZE_CORRECT_USE	set_virtual_use_link
#define FINALIZE_BASE_ZERO	0
#define FINALIZE_BASE(VAR)	get_name_decl (VAR)
#define FINALIZE_BASE_TYPE	unsigned
#define FINALIZE_INITIALIZE(PTR, VAL, STMT)				\
				(PTR)->def_var = (VAL);			\
				(PTR)->use_var = (VAL);			\
				(PTR)->use_ptr.use = &((PTR)->use_var);	\
				link_imm_use_stmt (&((PTR)->use_ptr),	\
						   (VAL), (STMT))
#include "tree-ssa-opfinalize.h"
                                                                              
                                                                              
static void
finalize_ssa_v_may_defs (tree stmt)
{
  finalize_ssa_v_may_def_ops (stmt);
}
                                                                               

/* Clear the in_list bits and empty the build array for v_may_defs.  */

static inline void
cleanup_v_may_defs (void)
{
  unsigned x, num;
  num = VEC_length (tree, build_v_may_defs);

  for (x = 0; x < num; x++)
    {
      tree t = VEC_index (tree, build_v_may_defs, x);
      if (TREE_CODE (t) != SSA_NAME)
	{
	  var_ann_t ann = var_ann (t);
	  ann->in_v_may_def_list = 0;
	}
    }
  VEC_truncate (tree, build_v_may_defs, 0);
}                                                                             

                                                                              
#define FINALIZE_OPBUILD	build_vuses
#define FINALIZE_OPBUILD_ELEM(I)	VEC_index (tree, build_vuses, (I))
#define FINALIZE_OPBUILD_BASE(I)	get_name_decl (VEC_index (tree,	\
							build_vuses, (I)))
#define FINALIZE_FUNC		finalize_ssa_vuse_ops
#define FINALIZE_ALLOC		alloc_vuse
#define FINALIZE_FREE		free_vuses
#define FINALIZE_TYPE		struct vuse_optype_d
#define FINALIZE_ELEM(PTR)	VUSE_OP (PTR)
#define FINALIZE_OPS		VUSE_OPS
#define FINALIZE_USE_PTR(PTR)	VUSE_OP_PTR (PTR)
#define FINALIZE_CORRECT_USE	set_virtual_use_link
#define FINALIZE_BASE_ZERO	0
#define FINALIZE_BASE(VAR)	get_name_decl (VAR)
#define FINALIZE_BASE_TYPE	unsigned
#define FINALIZE_INITIALIZE(PTR, VAL, STMT)				\
				(PTR)->use_var = (VAL);			\
				(PTR)->use_ptr.use = &((PTR)->use_var);	\
				link_imm_use_stmt (&((PTR)->use_ptr),	\
						   (VAL), (STMT))
#include "tree-ssa-opfinalize.h"


/* Return a new vuse operand vector, comparing to OLD_OPS_P.  */
                                                                              
static void
finalize_ssa_vuses (tree stmt)
{
  unsigned num, num_v_may_defs;
  unsigned vuse_index;

  /* Remove superfluous VUSE operands.  If the statement already has a
   V_MAY_DEF operation for a variable 'a', then a VUSE for 'a' is not
   needed because V_MAY_DEFs imply a VUSE of the variable.  For instance,
   suppose that variable 'a' is aliased:

	      # VUSE <a_2>
	      # a_3 = V_MAY_DEF <a_2>
	      a = a + 1;

  The VUSE <a_2> is superfluous because it is implied by the V_MAY_DEF
  operation.  */

  num = VEC_length (tree, build_vuses);
  num_v_may_defs = VEC_length (tree, build_v_may_defs);

  if (num > 0 && num_v_may_defs > 0)
    {
      for (vuse_index = 0; vuse_index < VEC_length (tree, build_vuses); )
        {
	  tree vuse;
	  vuse = VEC_index (tree, build_vuses, vuse_index);
	  if (TREE_CODE (vuse) != SSA_NAME)
	    {
	      var_ann_t ann = var_ann (vuse);
	      ann->in_vuse_list = 0;
	      if (ann->in_v_may_def_list)
	        {
		  VEC_ordered_remove (tree, build_vuses, vuse_index);
		  continue;
		}
	    }
	  vuse_index++;
	}
    }
  else
    /* Clear out the in_list bits.  */
    for (vuse_index = 0;
	 vuse_index < VEC_length (tree, build_vuses);
	 vuse_index++)
      {
	tree t = VEC_index (tree, build_vuses, vuse_index);
	if (TREE_CODE (t) != SSA_NAME)
	  {
	    var_ann_t ann = var_ann (t);
	    ann->in_vuse_list = 0;
	  }
      }

  finalize_ssa_vuse_ops (stmt);
  /* The v_may_def build vector wasn't cleaned up because we needed it.  */
  cleanup_v_may_defs ();
                                                                              
  /* Free the vuses build vector.  */
  VEC_truncate (tree, build_vuses, 0);

}
                                                                              
/* Return a new v_must_def operand vector for STMT, comparing to OLD_OPS_P.  */
                                                                              
#define FINALIZE_OPBUILD	build_v_must_defs
#define FINALIZE_OPBUILD_ELEM(I)	VEC_index (tree, build_v_must_defs, (I))
#define FINALIZE_OPBUILD_BASE(I)	get_name_decl (VEC_index (tree,	\
							build_v_must_defs, (I)))
#define FINALIZE_FUNC		finalize_ssa_v_must_def_ops
#define FINALIZE_ALLOC		alloc_mustdef
#define FINALIZE_FREE		free_mustdefs
#define FINALIZE_TYPE		struct mustdef_optype_d
#define FINALIZE_ELEM(PTR)	MUSTDEF_RESULT (PTR)
#define FINALIZE_OPS		MUSTDEF_OPS
#define FINALIZE_USE_PTR(PTR)	MUSTDEF_KILL_PTR (PTR)
#define FINALIZE_CORRECT_USE	set_virtual_use_link
#define FINALIZE_BASE_ZERO	0
#define FINALIZE_BASE(VAR)	get_name_decl (VAR)
#define FINALIZE_BASE_TYPE	unsigned
#define FINALIZE_INITIALIZE(PTR, VAL, STMT)				\
				(PTR)->def_var = (VAL);			\
				(PTR)->kill_var = (VAL);		\
				(PTR)->use_ptr.use = &((PTR)->kill_var);\
				link_imm_use_stmt (&((PTR)->use_ptr),	\
						   (VAL), (STMT))
#include "tree-ssa-opfinalize.h"


static void
finalize_ssa_v_must_defs (tree stmt)
{
  /* In the presence of subvars, there may be more than one V_MUST_DEF per
     statement (one for each subvar).  It is a bit expensive to verify that
     all must-defs in a statement belong to subvars if there is more than one
     MUST-def, so we don't do it.  Suffice to say, if you reach here without
     having subvars, and have num >1, you have hit a bug. */

  finalize_ssa_v_must_def_ops (stmt);
  VEC_truncate (tree, build_v_must_defs, 0);
}


/* Finalize all the build vectors, fill the new ones into INFO.  */
                                                                              
static inline void
finalize_ssa_stmt_operands (tree stmt)
{
  finalize_ssa_defs (stmt);
  finalize_ssa_uses (stmt);
  finalize_ssa_v_must_defs (stmt);
  finalize_ssa_v_may_defs (stmt);
  finalize_ssa_vuses (stmt);
}


/* Start the process of building up operands vectors in INFO.  */

static inline void
start_ssa_stmt_operands (void)
{
  gcc_assert (VEC_length (tree, build_defs) == 0);
  gcc_assert (VEC_length (tree, build_uses) == 0);
  gcc_assert (VEC_length (tree, build_vuses) == 0);
  gcc_assert (VEC_length (tree, build_v_may_defs) == 0);
  gcc_assert (VEC_length (tree, build_v_must_defs) == 0);
}


/* Add DEF_P to the list of pointers to operands.  */

static inline void
append_def (tree *def_p)
{
  VEC_safe_push (tree, heap, build_defs, (tree)def_p);
}


/* Add USE_P to the list of pointers to operands.  */

static inline void
append_use (tree *use_p)
{
  VEC_safe_push (tree, heap, build_uses, (tree)use_p);
}


/* Add a new virtual may def for variable VAR to the build array.  */

static inline void
append_v_may_def (tree var)
{
  if (TREE_CODE (var) != SSA_NAME)
    {
      var_ann_t ann = get_var_ann (var);

      /* Don't allow duplicate entries.  */
      if (ann->in_v_may_def_list)
	return;
      ann->in_v_may_def_list = 1;
    }

  VEC_safe_push (tree, heap, build_v_may_defs, (tree)var);
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

  VEC_safe_push (tree, heap, build_vuses, (tree)var);
}


/* Add VAR to the list of virtual must definitions for INFO.  */

static inline void
append_v_must_def (tree var)
{
  unsigned i;

  /* Don't allow duplicate entries.  */
  for (i = 0; i < VEC_length (tree, build_v_must_defs); i++)
    if (var == VEC_index (tree, build_v_must_defs, i))
      return;

  VEC_safe_push (tree, heap, build_v_must_defs, (tree)var);
}


/* Parse STMT looking for operands.  OLD_OPS is the original stmt operand
   cache for STMT, if it existed before.  When finished, the various build_*
   operand vectors will have potential operands. in them.  */
                                                                                
static void
parse_ssa_operands (tree stmt)
{
  enum tree_code code;

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

	if (TREE_CODE (lhs) != ARRAY_REF
	    && TREE_CODE (lhs) != ARRAY_RANGE_REF
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
	 pointer (which may only happen for USE operands), we will fail in
	 append_use.  This default will handle statements like empty
	 statements, or CALL_EXPRs that may appear on the RHS of a statement
	 or as statements themselves.  */
      get_expr_operands (stmt, &stmt, opf_none);
      break;
    }
}

/* Create an operands cache for STMT, returning it in NEW_OPS. OLD_OPS are the
   original operands, and if ANN is non-null, appropriate stmt flags are set
   in the stmt's annotation.  If ANN is NULL, this is not considered a "real"
   stmt, and none of the operands will be entered into their respective
   immediate uses tables.  This is to allow stmts to be processed when they
   are not actually in the CFG.

   Note that some fields in old_ops may change to NULL, although none of the
   memory they originally pointed to will be destroyed.  It is appropriate
   to call free_stmt_operands() on the value returned in old_ops.

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
build_ssa_operands (tree stmt)
{
  stmt_ann_t ann = get_stmt_ann (stmt);
  
  /* Initially assume that the statement has no volatile operands, nor
     makes aliased loads or stores.  */
  if (ann)
    {
      ann->has_volatile_ops = false;
      ann->makes_aliased_stores = false;
      ann->makes_aliased_loads = false;
    }

  start_ssa_stmt_operands ();

  parse_ssa_operands (stmt);
  operand_build_sort_virtual (build_vuses);
  operand_build_sort_virtual (build_v_may_defs);
  operand_build_sort_virtual (build_v_must_defs);

  finalize_ssa_stmt_operands (stmt);
}


/* Free any operands vectors in OPS.  */
void 
free_ssa_operands (stmt_operands_p ops)
{
  ops->def_ops = NULL;
  ops->use_ops = NULL;
  ops->maydef_ops = NULL;
  ops->mustdef_ops = NULL;
  ops->vuse_ops = NULL;
}


/* Get the operands of statement STMT.  Note that repeated calls to
   get_stmt_operands for the same statement will do nothing until the
   statement is marked modified by a call to mark_stmt_modified().  */

void
update_stmt_operands (tree stmt)
{
  stmt_ann_t ann = get_stmt_ann (stmt);
  /* If get_stmt_operands is called before SSA is initialized, dont
  do anything.  */
  if (!ssa_operands_active ())
    return;
  /* The optimizers cannot handle statements that are nothing but a
     _DECL.  This indicates a bug in the gimplifier.  */
  gcc_assert (!SSA_VAR_P (stmt));

  gcc_assert (ann->modified);

  timevar_push (TV_TREE_OPS);

  build_ssa_operands (stmt);

  /* Clear the modified bit for STMT.  Subsequent calls to
     get_stmt_operands for this statement will do nothing until the
     statement is marked modified by a call to mark_stmt_modified().  */
  ann->modified = 0;

  timevar_pop (TV_TREE_OPS);
}

  
/* Copies virtual operands from SRC to DST.  */

void
copy_virtual_operands (tree dest, tree src)
{
  tree t;
  ssa_op_iter iter, old_iter;
  use_operand_p use_p, u2;
  def_operand_p def_p, d2;

  build_ssa_operands (dest);

  /* Copy all the virtual fields.  */
  FOR_EACH_SSA_TREE_OPERAND (t, src, iter, SSA_OP_VUSE)
    append_vuse (t);
  FOR_EACH_SSA_TREE_OPERAND (t, src, iter, SSA_OP_VMAYDEF)
    append_v_may_def (t);
  FOR_EACH_SSA_TREE_OPERAND (t, src, iter, SSA_OP_VMUSTDEF)
    append_v_must_def (t);

  if (VEC_length (tree, build_vuses) == 0
      && VEC_length (tree, build_v_may_defs) == 0
      && VEC_length (tree, build_v_must_defs) == 0)
    return;

  /* Now commit the virtual operands to this stmt.  */
  finalize_ssa_v_must_defs (dest);
  finalize_ssa_v_may_defs (dest);
  finalize_ssa_vuses (dest);

  /* Finally, set the field to the same values as then originals.  */

  
  t = op_iter_init_tree (&old_iter, src, SSA_OP_VUSE);
  FOR_EACH_SSA_USE_OPERAND (use_p, dest, iter, SSA_OP_VUSE)
    {
      gcc_assert (!op_iter_done (&old_iter));
      SET_USE (use_p, t);
      t = op_iter_next_tree (&old_iter);
    }
  gcc_assert (op_iter_done (&old_iter));

  op_iter_init_maydef (&old_iter, src, &u2, &d2);
  FOR_EACH_SSA_MAYDEF_OPERAND (def_p, use_p, dest, iter)
    {
      gcc_assert (!op_iter_done (&old_iter));
      SET_USE (use_p, USE_FROM_PTR (u2));
      SET_DEF (def_p, DEF_FROM_PTR (d2));
      op_iter_next_maymustdef (&u2, &d2, &old_iter);
    }
  gcc_assert (op_iter_done (&old_iter));

  op_iter_init_mustdef (&old_iter, src, &u2, &d2);
  FOR_EACH_SSA_MUSTDEF_OPERAND (def_p, use_p, dest, iter)
    {
      gcc_assert (!op_iter_done (&old_iter));
      SET_USE (use_p, USE_FROM_PTR (u2));
      SET_DEF (def_p, DEF_FROM_PTR (d2));
      op_iter_next_maymustdef (&u2, &d2, &old_iter);
    }
  gcc_assert (op_iter_done (&old_iter));

}


/* Specifically for use in DOM's expression analysis.  Given a store, we
   create an artificial stmt which looks like a load from the store, this can
   be used to eliminate redundant loads.  OLD_OPS are the operands from the 
   store stmt, and NEW_STMT is the new load which represents a load of the
   values stored.  */

void
create_ssa_artficial_load_stmt (tree new_stmt, tree old_stmt)
{
  stmt_ann_t ann;
  tree op;
  ssa_op_iter iter;
  use_operand_p use_p;
  unsigned x;

  ann = get_stmt_ann (new_stmt);

  /* process the stmt looking for operands.  */
  start_ssa_stmt_operands ();
  parse_ssa_operands (new_stmt);

  for (x = 0; x < VEC_length (tree, build_vuses); x++)
    {
      tree t = VEC_index (tree, build_vuses, x);
      if (TREE_CODE (t) != SSA_NAME)
	{
	  var_ann_t ann = var_ann (t);
	  ann->in_vuse_list = 0;
	}
    }
   
  for (x = 0; x < VEC_length (tree, build_v_may_defs); x++)
    {
      tree t = VEC_index (tree, build_v_may_defs, x);
      if (TREE_CODE (t) != SSA_NAME)
	{
	  var_ann_t ann = var_ann (t);
	  ann->in_v_may_def_list = 0;
	}
    }
  /* Remove any virtual operands that were found.  */
  VEC_truncate (tree, build_v_may_defs, 0);
  VEC_truncate (tree, build_v_must_defs, 0);
  VEC_truncate (tree, build_vuses, 0);

  /* For each VDEF on the original statement, we want to create a
     VUSE of the V_MAY_DEF result or V_MUST_DEF op on the new 
     statement.  */
  FOR_EACH_SSA_TREE_OPERAND (op, old_stmt, iter, 
			     (SSA_OP_VMAYDEF | SSA_OP_VMUSTDEF))
    append_vuse (op);
    
  /* Now build the operands for this new stmt.  */
  finalize_ssa_stmt_operands (new_stmt);

  /* All uses in this fake stmt must not be in the immediate use lists.  */
  FOR_EACH_SSA_USE_OPERAND (use_p, new_stmt, iter, SSA_OP_ALL_USES)
    delink_imm_use (use_p);
}

void
swap_tree_operands (tree stmt, tree *exp0, tree *exp1)
{
  tree op0, op1;
  op0 = *exp0;
  op1 = *exp1;

  /* If the operand cache is active, attempt to preserve the relative positions
     of these two operands in their respective immediate use lists.  */
  if (ssa_operands_active () && op0 != op1)
    {
      use_optype_p use0, use1, ptr;
      use0 = use1 = NULL;
      /* Find the 2 operands in the cache, if they are there.  */
      for (ptr = USE_OPS (stmt); ptr; ptr = ptr->next)
	if (USE_OP_PTR (ptr)->use == exp0)
	  {
	    use0 = ptr;
	    break;
	  }
      for (ptr = USE_OPS (stmt); ptr; ptr = ptr->next)
	if (USE_OP_PTR (ptr)->use == exp1)
	  {
	    use1 = ptr;
	    break;
	  }
      /* If both uses don't have operand entries, there isn't much we can do
         at this point.  Presumably we dont need to worry about it.  */
      if (use0 && use1)
        {
	  tree *tmp = USE_OP_PTR (use1)->use;
	  USE_OP_PTR (use1)->use = USE_OP_PTR (use0)->use;
	  USE_OP_PTR (use0)->use = tmp;
	}
    }

  /* Now swap the data.  */
  *exp0 = op1;
  *exp1 = op0;
}


/* Recursively scan the expression pointed to by EXPR_P in statement referred
   to by INFO.  FLAGS is one of the OPF_* constants modifying how to interpret
   the operands found.  */

static void
get_expr_operands (tree stmt, tree *expr_p, int flags)
{
  enum tree_code code;
  enum tree_code_class class;
  tree expr = *expr_p;
  stmt_ann_t s_ann = stmt_ann (stmt);

  if (expr == NULL)
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
      {
	subvar_t svars;
	
	/* Add the subvars for a variable if it has subvars, to DEFS or USES.
	   Otherwise, add the variable itself.  
	   Whether it goes to USES or DEFS depends on the operand flags.  */
	if (var_can_have_subvars (expr)
	    && (svars = get_subvars_for_var (expr)))
	  {
	    subvar_t sv;
	    for (sv = svars; sv; sv = sv->next)
	      add_stmt_operand (&sv->var, s_ann, flags);
	  }
	else
	  {
	    add_stmt_operand (expr_p, s_ann, flags);
	  }
	return;
      }
    case MISALIGNED_INDIRECT_REF:
      get_expr_operands (stmt, &TREE_OPERAND (expr, 1), flags);
      /* fall through */

    case ALIGN_INDIRECT_REF:
    case INDIRECT_REF:
      get_indirect_ref_operands (stmt, expr, flags);
      return;

    case TARGET_MEM_REF:
      get_tmr_operands (stmt, expr, flags);
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
      {
	tree ref;
	unsigned HOST_WIDE_INT offset, size;
 	/* This component ref becomes an access to all of the subvariables
	   it can touch,  if we can determine that, but *NOT* the real one.
	   If we can't determine which fields we could touch, the recursion
	   will eventually get to a variable and add *all* of its subvars, or
	   whatever is the minimum correct subset.  */

	ref = okay_component_ref_for_subvars (expr, &offset, &size);
	if (ref)
	  {	  
	    subvar_t svars = get_subvars_for_var (ref);
	    subvar_t sv;
	    for (sv = svars; sv; sv = sv->next)
	      {
		bool exact;		
		if (overlap_subvar (offset, size, sv, &exact))
		  {
	            int subvar_flags = flags;
		    if (!exact)
		      subvar_flags &= ~opf_kill_def;
		    add_stmt_operand (&sv->var, s_ann, subvar_flags);
		  }
	      }
	  }
	else
	  get_expr_operands (stmt, &TREE_OPERAND (expr, 0), 
			     flags & ~opf_kill_def);
	
	if (code == COMPONENT_REF)
	  {
	    if (s_ann && TREE_THIS_VOLATILE (TREE_OPERAND (expr, 1)))
	      s_ann->has_volatile_ops = true; 
	    get_expr_operands (stmt, &TREE_OPERAND (expr, 2), opf_none);
	  }
	return;
      }
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
	constructor_elt *ce;
	unsigned HOST_WIDE_INT idx;

	for (idx = 0;
	     VEC_iterate (constructor_elt, CONSTRUCTOR_ELTS (expr), idx, ce);
	     idx++)
	  get_expr_operands (stmt, &ce->value, opf_none);

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
    case ASSERT_EXPR:
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
		swap_tree_operands (stmt,
				    &TREE_OPERAND (expr, 0),			
				    &TREE_OPERAND (expr, 1));
	      }
	  
	    /* For a commutative operator we can just swap the operands.  */
	    else if (commutative_tree_code (code))
	      {
		swap_tree_operands (stmt,
				    &TREE_OPERAND (expr, 0),			
				    &TREE_OPERAND (expr, 1));
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
	  if (t && DECL_P (t) && s_ann)
	    add_to_addressable_set (t, &s_ann->addresses_taken);
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
	  if (t && DECL_P (t) && s_ann)
	    add_to_addressable_set (t, &s_ann->addresses_taken);
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
	      add_stmt_operand (&var, s_ann, opf_is_def | opf_non_specific);
	    }

	/* Now clobber all addressables.  */
	EXECUTE_IF_SET_IN_BITMAP (addressable_vars, 0, i, bi)
	    {
	      tree var = referenced_var (i);

	      /* Subvars are explicitly represented in this list, so
		 we don't need the original to be added to the clobber
		 ops, but the original *will* be in this list because 
		 we keep the addressability of the original
		 variable up-to-date so we don't screw up the rest of
		 the backend.  */
	      if (var_can_have_subvars (var)
		  && get_subvars_for_var (var) != NULL)
		continue;		

	      add_stmt_operand (&var, s_ann, opf_is_def | opf_non_specific);
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
     cannot just assert here.  If we were absolutely certain that we
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

/* A subroutine of get_expr_operands to handle TARGET_MEM_REF.  */

static void
get_tmr_operands (tree stmt, tree expr, int flags)
{
  tree tag = TMR_TAG (expr), ref;
  unsigned HOST_WIDE_INT offset, size;
  subvar_t svars, sv;
  stmt_ann_t s_ann = stmt_ann (stmt);

  /* First record the real operands.  */
  get_expr_operands (stmt, &TMR_BASE (expr), opf_none);
  get_expr_operands (stmt, &TMR_INDEX (expr), opf_none);

  /* MEM_REFs should never be killing.  */
  flags &= ~opf_kill_def;

  if (TMR_SYMBOL (expr))
    {
      stmt_ann_t ann = stmt_ann (stmt);
      add_to_addressable_set (TMR_SYMBOL (expr), &ann->addresses_taken);
    }

  if (!tag)
    {
      /* Something weird, so ensure that we will be careful.  */
      stmt_ann (stmt)->has_volatile_ops = true;
      return;
    }

  if (DECL_P (tag))
    {
      get_expr_operands (stmt, &tag, flags);
      return;
    }

  ref = okay_component_ref_for_subvars (tag, &offset, &size);
  gcc_assert (ref != NULL_TREE);
  svars = get_subvars_for_var (ref);
  for (sv = svars; sv; sv = sv->next)
    {
      bool exact;		
      if (overlap_subvar (offset, size, sv, &exact))
	{
	  int subvar_flags = flags;
	  if (!exact)
	    subvar_flags &= ~opf_kill_def;
	  add_stmt_operand (&sv->var, s_ann, subvar_flags);
	}
    }
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
  if (aliases_computed_p
      && !bitmap_empty_p (call_clobbered_vars)
      && !(call_flags & ECF_NOVOPS))
    {
      /* A 'pure' or a 'const' function never call-clobbers anything. 
	 A 'noreturn' function might, but since we don't return anyway 
	 there is no point in recording that.  */ 
      if (TREE_SIDE_EFFECTS (expr)
	  && !(call_flags & (ECF_PURE | ECF_CONST | ECF_NORETURN)))
	add_call_clobber_ops (stmt, get_callee_fndecl (expr));
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
  if (TREE_CODE (var) == ADDR_EXPR && s_ann)
    {
      add_to_addressable_set (TREE_OPERAND (var, 0), &s_ann->addresses_taken);
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

  /* If the variable cannot be modified and this is a V_MAY_DEF change
     it into a VUSE.  This happens when read-only variables are marked
     call-clobbered and/or aliased to writable variables.  So we only
     check that this only happens on non-specific stores.

     Note that if this is a specific store, i.e. associated with a
     modify_expr, then we can't suppress the V_DEF, lest we run into
     validation problems.

     This can happen when programs cast away const, leaving us with a
     store to read-only memory.  If the statement is actually executed
     at runtime, then the program is ill formed.  If the statement is
     not executed then all is well.  At the very least, we cannot ICE.  */
  if ((flags & opf_non_specific) && unmodifiable_var_p (var))
    {
      gcc_assert (!is_real_op);
      flags &= ~(opf_is_def | opf_kill_def);
    }

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
		  /* Only regular variables or struct fields may get a
		     V_MUST_DEF operand.  */
		  gcc_assert (v_ann->mem_tag_kind == NOT_A_TAG 
			      || v_ann->mem_tag_kind == STRUCT_FIELD);
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

  
/* Add the base address of REF to the set *ADDRESSES_TAKEN.  If
   *ADDRESSES_TAKEN is NULL, a new set is created.  REF may be
   a single variable whose address has been taken or any other valid
   GIMPLE memory reference (structure reference, array, etc).  If the
   base address of REF is a decl that has sub-variables, also add all
   of its sub-variables.  */

void
add_to_addressable_set (tree ref, bitmap *addresses_taken)
{
  tree var;
  subvar_t svars;

  gcc_assert (addresses_taken);

  /* Note that it is *NOT OKAY* to use the target of a COMPONENT_REF
     as the only thing we take the address of.  If VAR is a structure,
     taking the address of a field means that the whole structure may
     be referenced using pointer arithmetic.  See PR 21407 and the
     ensuing mailing list discussion.  */
  var = get_base_address (ref);
  if (var && SSA_VAR_P (var))
    {
      if (*addresses_taken == NULL)
	*addresses_taken = BITMAP_GGC_ALLOC ();      
      
      if (var_can_have_subvars (var)
	  && (svars = get_subvars_for_var (var)))
	{
	  subvar_t sv;
	  for (sv = svars; sv; sv = sv->next)
	    {
	      bitmap_set_bit (*addresses_taken, DECL_UID (sv->var));
	      TREE_ADDRESSABLE (sv->var) = 1;
	    }
	}
      else
	{
	  bitmap_set_bit (*addresses_taken, DECL_UID (var));
	  TREE_ADDRESSABLE (var) = 1;
	}
    }
}


/* Add clobbering definitions for .GLOBAL_VAR or for each of the call
   clobbered variables in the function.  */

static void
add_call_clobber_ops (tree stmt, tree callee)
{
  unsigned u;
  tree t;
  bitmap_iterator bi;
  stmt_ann_t s_ann = stmt_ann (stmt);
  struct stmt_ann_d empty_ann;
  bitmap not_read_b, not_written_b;

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

  /* FIXME - if we have better information from the static vars
     analysis, we need to make the cache call site specific.  This way
     we can have the performance benefits even if we are doing good
     optimization.  */

  /* Get info for local and module level statics.  There is a bit
     set for each static if the call being processed does not read
     or write that variable.  */

  not_read_b = callee ? ipa_reference_get_not_read_global (callee) : NULL; 
  not_written_b = callee ? ipa_reference_get_not_written_global (callee) : NULL; 

  /* If cache is valid, copy the elements into the build vectors.  */
  if (ssa_call_clobbered_cache_valid
      && (!not_read_b || bitmap_empty_p (not_read_b))
      && (!not_written_b || bitmap_empty_p (not_written_b)))
    {
      for (u = 0 ; u < VEC_length (tree, clobbered_vuses); u++)
	{
	  t = VEC_index (tree, clobbered_vuses, u);
	  gcc_assert (TREE_CODE (t) != SSA_NAME);
	  var_ann (t)->in_vuse_list = 1;
	  VEC_safe_push (tree, heap, build_vuses, (tree)t);
	}
      for (u = 0; u < VEC_length (tree, clobbered_v_may_defs); u++)
	{
	  t = VEC_index (tree, clobbered_v_may_defs, u);
	  gcc_assert (TREE_CODE (t) != SSA_NAME);
	  var_ann (t)->in_v_may_def_list = 1;
	  VEC_safe_push (tree, heap, build_v_may_defs, (tree)t);
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
  EXECUTE_IF_SET_IN_BITMAP (call_clobbered_vars, 0, u, bi)
    {
      tree var = referenced_var (u);
      if (unmodifiable_var_p (var))
	add_stmt_operand (&var, &empty_ann, opf_none);
      else
	{
	  bool not_read
	    = not_read_b ? bitmap_bit_p (not_read_b, u) : false;
	  bool not_written
	    = not_written_b ? bitmap_bit_p (not_written_b, u) : false;

	  if ((TREE_READONLY (var)
	       && (TREE_STATIC (var) || DECL_EXTERNAL (var)))
	      || not_written)
	    {
	      if (!not_read)
		add_stmt_operand (&var, &empty_ann, opf_none);
	    }
	  else
	    add_stmt_operand (&var, &empty_ann, opf_is_def);
	}
    }

  if ((!not_read_b || bitmap_empty_p (not_read_b))
      && (!not_written_b || bitmap_empty_p (not_written_b)))
    {
      clobbered_aliased_loads = empty_ann.makes_aliased_loads;
      clobbered_aliased_stores = empty_ann.makes_aliased_stores;

      /* Set the flags for a stmt's annotation.  */
      if (s_ann)
	{
	  s_ann->makes_aliased_loads = empty_ann.makes_aliased_loads;
	  s_ann->makes_aliased_stores = empty_ann.makes_aliased_stores;
	}

      /* Prepare empty cache vectors.  */
      VEC_truncate (tree, clobbered_vuses, 0);
      VEC_truncate (tree, clobbered_v_may_defs, 0);

      /* Now fill the clobbered cache with the values that have been found.  */
      for (u = 0; u < VEC_length (tree, build_vuses); u++)
	VEC_safe_push (tree, heap, clobbered_vuses,
		       VEC_index (tree, build_vuses, u));

      gcc_assert (VEC_length (tree, build_vuses) 
		  == VEC_length (tree, clobbered_vuses));

      for (u = 0; u < VEC_length (tree, build_v_may_defs); u++)
	VEC_safe_push (tree, heap, clobbered_v_may_defs, 
		       VEC_index (tree, build_v_may_defs, u));

      gcc_assert (VEC_length (tree, build_v_may_defs) 
		  == VEC_length (tree, clobbered_v_may_defs));

      ssa_call_clobbered_cache_valid = true;
    }
}


/* Add VUSE operands for .GLOBAL_VAR or all call clobbered variables in the
   function.  */

static void
add_call_read_ops (tree stmt)
{
  unsigned u;
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
      for (u = 0; u < VEC_length (tree, ro_call_vuses); u++)
	{
	  t = VEC_index (tree, ro_call_vuses, u);
	  gcc_assert (TREE_CODE (t) != SSA_NAME);
	  var_ann (t)->in_vuse_list = 1;
	  VEC_safe_push (tree, heap, build_vuses, (tree)t);
	}
      if (s_ann)
	s_ann->makes_aliased_loads = ro_call_aliased_loads;
      return;
    }

  memset (&empty_ann, 0, sizeof (struct stmt_ann_d));

  /* Add a VUSE for each call-clobbered variable.  */
  EXECUTE_IF_SET_IN_BITMAP (call_clobbered_vars, 0, u, bi)
    {
      tree var = referenced_var (u);
      add_stmt_operand (&var, &empty_ann, opf_none | opf_non_specific);
    }

  ro_call_aliased_loads = empty_ann.makes_aliased_loads;
  if (s_ann)
    s_ann->makes_aliased_loads = empty_ann.makes_aliased_loads;

  /* Prepare empty cache vectors.  */
  VEC_truncate (tree, ro_call_vuses, 0);

  /* Now fill the clobbered cache with the values that have been found.  */
  for (u = 0; u <  VEC_length (tree, build_vuses); u++)
    VEC_safe_push (tree, heap, ro_call_vuses,
		   VEC_index (tree, build_vuses, u));

  gcc_assert (VEC_length (tree, build_vuses) 
	      == VEC_length (tree, ro_call_vuses));

  ssa_ro_call_cache_valid = true;
}


/* Scan the immediate_use list for VAR making sure its linked properly.
   return RTUE iof there is a problem.  */

bool
verify_imm_links (FILE *f, tree var)
{
  use_operand_p ptr, prev, list;
  int count;

  gcc_assert (TREE_CODE (var) == SSA_NAME);

  list = &(SSA_NAME_IMM_USE_NODE (var));
  gcc_assert (list->use == NULL);

  if (list->prev == NULL)
    {
      gcc_assert (list->next == NULL);
      return false;
    }

  prev = list;
  count = 0;
  for (ptr = list->next; ptr != list; )
    {
      if (prev != ptr->prev)
	goto error;
      
      if (ptr->use == NULL)
	goto error; /* 2 roots, or SAFE guard node.  */
      else if (*(ptr->use) != var)
	goto error;

      prev = ptr;
      ptr = ptr->next;
      /* Avoid infinite loops.  50,000,000 uses probably indicates a problem.  */
      if (count++ > 50000000)
	goto error;
    }

  /* Verify list in the other direction.  */
  prev = list;
  for (ptr = list->prev; ptr != list; )
    {
      if (prev != ptr->next)
	goto error;
      prev = ptr;
      ptr = ptr->prev;
      if (count-- < 0)
	goto error;
    }

  if (count != 0)
    goto error;

  return false;

 error:
  if (ptr->stmt && stmt_modified_p (ptr->stmt))
    {
      fprintf (f, " STMT MODIFIED. - <%p> ", (void *)ptr->stmt);
      print_generic_stmt (f, ptr->stmt, TDF_SLIM);
    }
  fprintf (f, " IMM ERROR : (use_p : tree - %p:%p)", (void *)ptr, 
	   (void *)ptr->use);
  print_generic_expr (f, USE_FROM_PTR (ptr), TDF_SLIM);
  fprintf(f, "\n");
  return true;
}


/* Dump all the immediate uses to FILE.  */

void
dump_immediate_uses_for (FILE *file, tree var)
{
  imm_use_iterator iter;
  use_operand_p use_p;

  gcc_assert (var && TREE_CODE (var) == SSA_NAME);

  print_generic_expr (file, var, TDF_SLIM);
  fprintf (file, " : -->");
  if (has_zero_uses (var))
    fprintf (file, " no uses.\n");
  else
    if (has_single_use (var))
      fprintf (file, " single use.\n");
    else
      fprintf (file, "%d uses.\n", num_imm_uses (var));

  FOR_EACH_IMM_USE_FAST (use_p, iter, var)
    {
      if (!is_gimple_reg (USE_FROM_PTR (use_p)))
	print_generic_stmt (file, USE_STMT (use_p), TDF_VOPS);
      else
	print_generic_stmt (file, USE_STMT (use_p), TDF_SLIM);
    }
  fprintf(file, "\n");
}

/* Dump all the immediate uses to FILE.  */

void
dump_immediate_uses (FILE *file)
{
  tree var;
  unsigned int x;

  fprintf (file, "Immediate_uses: \n\n");
  for (x = 1; x < num_ssa_names; x++)
    {
      var = ssa_name(x);
      if (!var)
        continue;
      dump_immediate_uses_for (file, var);
    }
}


/* Dump def-use edges on stderr.  */

void
debug_immediate_uses (void)
{
  dump_immediate_uses (stderr);
}

/* Dump def-use edges on stderr.  */

void
debug_immediate_uses_for (tree var)
{
  dump_immediate_uses_for (stderr, var);
}
#include "gt-tree-ssa-operands.h"
