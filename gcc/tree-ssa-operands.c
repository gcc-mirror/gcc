/* SSA operands management for trees.
   Copyright (C) 2003 Free Software Foundation, Inc.

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
#include "tree-flow.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "ggc.h"
#include "timevar.h"

/* Flags to describe operand properties in get_stmt_operands and helpers.  */

/* By default, operands are loaded.  */
#define opf_none	0

/* Operand is the target of an assignment expression or a 
   call-clobbered variable  */
#define opf_is_def 	(1 << 0)

/* Operand is the target of an assignment expression.  */
#define opf_kill_def 	(1 << 2)

/* No virtual operands should be created in the expression.  This is used
   when traversing ADDR_EXPR nodes which have different semantics than
   other expressions.  Inside an ADDR_EXPR node, the only operands that we
   need to consider are indices into arrays.  For instance, &a.b[i] should
   generate a USE of 'i' but it should not generate a VUSE for 'a' nor a
   VUSE for 'b'.  */
#define opf_no_vops 	(1 << 1)

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

#ifdef ENABLE_CHECKING
tree check_build_stmt;
#endif

typedef struct voperands_d 
{
  v_may_def_optype v_may_def_ops;
  vuse_optype vuse_ops;
  v_must_def_optype v_must_def_ops;
} *voperands_t;

static void note_addressable (tree, stmt_ann_t);
static void get_expr_operands (tree, tree *, int, voperands_t);
static inline void append_def (tree *, tree);
static inline void append_use (tree *, tree);
static void append_v_may_def (tree, tree, voperands_t);
static void append_v_must_def (tree, tree, voperands_t);
static void add_call_clobber_ops (tree, voperands_t);
static void add_call_read_ops (tree, voperands_t);
static void add_stmt_operand (tree *, tree, int, voperands_t);


struct freelist_d GTY((chain_next ("%h.next")))
{
   struct freelist_d *next;
};

#define NUM_FREE	5
static GTY ((length ("NUM_FREE"))) struct freelist_d optype_freelist[NUM_FREE] = { {0}, {0}, {0}, {0}, {0} };


static inline void *
check_optype_freelist (size_t num ATTRIBUTE_UNUSED)
{
  return NULL;
#if 0
  void *vec = NULL;

  if (num <= NUM_FREE && optype_freelist[num - 1].next)
    {
      vec = (void *)optype_freelist[num - 1].next;
      optype_freelist[num - 1].next = optype_freelist[num - 1].next->next;
    }
  return vec;
#endif
}
/* Return a vector of contiguous memory of a specified size.  */


static inline void
add_optype_freelist (void *vec ATTRIBUTE_UNUSED, size_t size ATTRIBUTE_UNUSED)
{
#if 0
  struct freelist_d *ptr;
#ifdef ENABLE_CHECKING
  if (size == 0)
    abort ();
#endif

  /* if its bigger than one of our lists, simply let it go and let GC 
     collect it.  */
  if (size > NUM_FREE)
    return;

  ptr = vec;
  ptr->next = optype_freelist[size - 1].next;;
  optype_freelist[size - 1].next = ptr;
#endif
}


static inline def_optype
allocate_def_optype (unsigned num)
{
  def_optype def_ops;
  unsigned size;
  size = sizeof (struct def_optype_d) + sizeof (tree *) * (num - 1);
  def_ops = check_optype_freelist (num);
  if (!def_ops)
    def_ops =  ggc_alloc (size);
  def_ops->num_defs = num;
  return def_ops;
}

static inline use_optype
allocate_use_optype (unsigned num)
{
  use_optype use_ops;
  unsigned size;
  size = sizeof (struct use_optype_d) + sizeof (tree *) * (num - 1);
  use_ops = check_optype_freelist (num);
  if (!use_ops)
    use_ops =  ggc_alloc (size);
  use_ops->num_uses = num;
  return use_ops;
}

static inline v_may_def_optype
allocate_v_may_def_optype (unsigned num)
{
  v_may_def_optype v_may_def_ops;
  unsigned size;
  size = sizeof (struct v_may_def_optype_d) + sizeof (tree) * ((num * 2) - 1);
  v_may_def_ops = check_optype_freelist (num * 2);
  if (!v_may_def_ops)
    v_may_def_ops =  ggc_alloc (size);
  v_may_def_ops->num_v_may_defs = num;
  return v_may_def_ops;
}

static inline vuse_optype
allocate_vuse_optype (unsigned num)
{
  vuse_optype vuse_ops;
  unsigned size;
  size = sizeof (struct vuse_optype_d) + sizeof (tree) * (num - 1);
  vuse_ops = check_optype_freelist (num);
  if (!vuse_ops)
    vuse_ops =  ggc_alloc (size);
  vuse_ops->num_vuses = num;
  return vuse_ops;
}

static inline v_must_def_optype
allocate_v_must_def_optype (unsigned num)
{
  v_must_def_optype v_must_def_ops;
  unsigned size;
  size = sizeof (struct v_must_def_optype_d) + sizeof (tree *) * (num - 1);
  v_must_def_ops = check_optype_freelist (num);
  if (!v_must_def_ops)
    v_must_def_ops =  ggc_alloc (size);
  v_must_def_ops->num_v_must_defs = num;
  return v_must_def_ops;
}

static inline void
free_uses (use_optype *uses, bool dealloc)
{
  if (*uses)
    {
      if (dealloc)
	add_optype_freelist (*uses, (*uses)->num_uses);
      *uses = NULL;
    }
}

static inline void
free_defs (def_optype *defs, bool dealloc)
{
  if (*defs)
    {
      if (dealloc)
	add_optype_freelist (*defs, (*defs)->num_defs);
      *defs = NULL;
    }
}

static inline void
free_vuses (vuse_optype *vuses, bool dealloc)
{
  if (*vuses)
    {
      if (dealloc)
	add_optype_freelist (*vuses, (*vuses)->num_vuses);
      *vuses = NULL;
    }
}

static inline void
free_v_may_defs (v_may_def_optype *v_may_defs, bool dealloc)
{
  if (*v_may_defs)
    {
      if (dealloc)
	add_optype_freelist (*v_may_defs, (*v_may_defs)->num_v_may_defs);
      *v_may_defs = NULL;
    }
}

static inline void
free_v_must_defs (v_must_def_optype *v_must_defs, bool dealloc)
{
  if (*v_must_defs)
    {
      if (dealloc)
	add_optype_freelist (*v_must_defs, (*v_must_defs)->num_v_must_defs);
      *v_must_defs = NULL;
    }
}

void
remove_vuses (tree stmt)
{
  stmt_ann_t ann;

  ann = stmt_ann (stmt);
  if (ann)
    free_vuses (&(ann->vuse_ops), true);
}

void
remove_v_may_defs (tree stmt)
{
  stmt_ann_t ann;

  ann = stmt_ann (stmt);
  if (ann)
    free_v_may_defs (&(ann->v_may_def_ops), true);
}

void
remove_v_must_defs (tree stmt)
{
  stmt_ann_t ann;

  ann = stmt_ann (stmt);
  if (ann)
    free_v_must_defs (&(ann->v_must_def_ops), true);
}

void
init_ssa_operands (void)
{
  int x;

  VARRAY_TREE_PTR_INIT (build_defs, 5, "build defs");
  VARRAY_TREE_PTR_INIT (build_uses, 10, "build uses");
  VARRAY_TREE_INIT (build_v_may_defs, 10, "build v_may_defs");
  VARRAY_TREE_INIT (build_vuses, 10, "build vuses");
  VARRAY_TREE_INIT (build_v_must_defs, 10, "build v_must_defs");

  for (x = 0; x < NUM_FREE; x++)
    optype_freelist[x].next = NULL;
}

void
fini_ssa_operands (void)
{
  int x;
  for (x = 0; x < NUM_FREE; x++)
    optype_freelist[x].next = NULL;
}

static void
finalize_ssa_defs (tree stmt)
{
  unsigned num, x;
  stmt_ann_t ann;
  def_optype def_ops;

  num = VARRAY_ACTIVE_SIZE (build_defs);
  if (num == 0)
    return;

#ifdef ENABLE_CHECKING
  /* There should only be a single real definition per assignment.  */
  if (TREE_CODE (stmt) == MODIFY_EXPR && num > 1)
    abort ();
#endif

  def_ops = allocate_def_optype (num);
  for (x = 0; x < num ; x++)
    def_ops->defs[x] = VARRAY_TREE_PTR (build_defs, x);
  VARRAY_POP_ALL (build_defs);

  ann = stmt_ann (stmt);
  ann->def_ops = def_ops;
}

static void
finalize_ssa_uses (tree stmt)
{
  unsigned num, x;
  use_optype use_ops;
  stmt_ann_t ann;

  num = VARRAY_ACTIVE_SIZE (build_uses);
  if (num == 0)
    return;

#ifdef ENABLE_CHECKING
  {
    unsigned x;
    /* If the pointer to the operand is the statement itself, something is
       wrong.  It means that we are pointing to a local variable (the 
       initial call to get_stmt_operands does not pass a pointer to a 
       statement).  */
    for (x = 0; x < num; x++)
      if (*(VARRAY_TREE_PTR (build_uses, x)) == stmt)
	abort ();
  }
#endif

  use_ops = allocate_use_optype (num);
  for (x = 0; x < num ; x++)
    use_ops->uses[x] = VARRAY_TREE_PTR (build_uses, x);
  VARRAY_POP_ALL (build_uses);

  ann = stmt_ann (stmt);
  ann->use_ops = use_ops;
}

static void
finalize_ssa_v_may_defs (tree stmt)
{
  unsigned num, x;
  v_may_def_optype v_may_def_ops;
  stmt_ann_t ann;

  num = VARRAY_ACTIVE_SIZE (build_v_may_defs);
  if (num == 0)
    return;

#ifdef ENABLE_CHECKING
  /* V_MAY_DEFs must be entered in pairs of result/uses.  */
  if (num % 2 != 0)
    abort();
#endif

  v_may_def_ops = allocate_v_may_def_optype (num / 2);
  for (x = 0; x < num; x++)
    v_may_def_ops->v_may_defs[x] = VARRAY_TREE (build_v_may_defs, x);
  VARRAY_CLEAR (build_v_may_defs);

  ann = stmt_ann (stmt);
  ann->v_may_def_ops = v_may_def_ops;
}

static inline void
finalize_ssa_vuses (tree stmt)
{
  unsigned num, x;
  stmt_ann_t ann;
  vuse_optype vuse_ops;
  v_may_def_optype v_may_defs;

#ifdef ENABLE_CHECKING
  if (VARRAY_ACTIVE_SIZE (build_v_may_defs) > 0)
    {
      fprintf (stderr, "Please finalize V_MAY_DEFs before finalize VUSES.\n");
      abort ();
    }
#endif

  num = VARRAY_ACTIVE_SIZE (build_vuses);
  if (num == 0)
    return;

  /* Remove superfluous VUSE operands.  If the statement already has a
   V_MAY_DEF operation for a variable 'a', then a VUSE for 'a' is not
   needed because V_MAY_DEFs imply a VUSE of the variable.  For instance,
   suppose that variable 'a' is aliased:

	      # VUSE <a_2>
	      # a_3 = V_MAY_DEF <a_2>
	      a = a + 1;

  The VUSE <a_2> is superfluous because it is implied by the V_MAY_DEF
  operation.  */

  ann = stmt_ann (stmt);
  v_may_defs = V_MAY_DEF_OPS (ann);
  if (NUM_V_MAY_DEFS (v_may_defs) > 0)
    {
      size_t i, j;
      for (i = 0; i < VARRAY_ACTIVE_SIZE (build_vuses); i++)
	{
	  bool found = false;
	  for (j = 0; j < NUM_V_MAY_DEFS (v_may_defs); j++)
	    {
	      tree vuse_var, v_may_def_var;
	      tree vuse = VARRAY_TREE (build_vuses, i);
	      tree v_may_def = V_MAY_DEF_OP (v_may_defs, j);

	      if (TREE_CODE (vuse) == SSA_NAME)
		vuse_var = SSA_NAME_VAR (vuse);
	      else
		vuse_var = vuse;

	      if (TREE_CODE (v_may_def) == SSA_NAME)
		v_may_def_var = SSA_NAME_VAR (v_may_def);
	      else
		v_may_def_var = v_may_def;

	    if (vuse_var == v_may_def_var)
	      {
		found = true;
		break;
	      }
	    }

	  /* If we found a useless VUSE operand, remove it from the
	     operand array by replacing it with the last active element
	     in the operand array (unless the useless VUSE was the
	     last operand, in which case we simply remove it.  */
	  if (found)
	    {
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

  num = VARRAY_ACTIVE_SIZE (build_vuses);
  /* We could have reduced the size to zero now, however.  */
  if (num == 0)
    return;

  vuse_ops = allocate_vuse_optype (num);
  for (x = 0; x < num; x++)
    vuse_ops->vuses[x] = VARRAY_TREE (build_vuses, x);
  VARRAY_CLEAR (build_vuses);
  ann->vuse_ops = vuse_ops;
}

static void
finalize_ssa_v_must_defs (tree stmt)
{
  unsigned num, x;
  stmt_ann_t ann;
  v_must_def_optype v_must_def_ops;

  num = VARRAY_ACTIVE_SIZE (build_v_must_defs);
  if (num == 0)
    return;

#ifdef ENABLE_CHECKING
  /* There should only be a single V_MUST_DEF per assignment.  */
  if (TREE_CODE (stmt) == MODIFY_EXPR && num > 1)
    abort ();
#endif

  v_must_def_ops = allocate_v_must_def_optype (num);
  for (x = 0; x < num ; x++)
    v_must_def_ops->v_must_defs[x] = VARRAY_TREE (build_v_must_defs, x);
  VARRAY_POP_ALL (build_v_must_defs);

  ann = stmt_ann (stmt);
  ann->v_must_def_ops = v_must_def_ops;
}

extern void
finalize_ssa_stmt_operands (tree stmt)
{
#ifdef ENABLE_CHECKING
  if (check_build_stmt == NULL)
    abort();
#endif

  finalize_ssa_defs (stmt);
  finalize_ssa_uses (stmt);
  finalize_ssa_v_must_defs (stmt);
  finalize_ssa_v_may_defs (stmt);
  finalize_ssa_vuses (stmt);

#ifdef ENABLE_CHECKING
  check_build_stmt = NULL;
#endif
}


extern void
verify_start_operands (tree stmt ATTRIBUTE_UNUSED)
{
#ifdef ENABLE_CHECKING
  if (VARRAY_ACTIVE_SIZE (build_defs) > 0 
      || VARRAY_ACTIVE_SIZE (build_uses) > 0
      || VARRAY_ACTIVE_SIZE (build_vuses) > 0
      || VARRAY_ACTIVE_SIZE (build_v_may_defs) > 0
      || VARRAY_ACTIVE_SIZE (build_v_must_defs) > 0)
    abort ();
  if (check_build_stmt != NULL)
    abort();
  check_build_stmt = stmt;
#endif
}


/* Add DEF_P to the list of pointers to operands defined by STMT.  */

static inline void
append_def (tree *def_p, tree stmt ATTRIBUTE_UNUSED)
{
#ifdef ENABLE_CHECKING
  if (check_build_stmt != stmt)
    abort();
#endif
  VARRAY_PUSH_TREE_PTR (build_defs, def_p);
}


/* Add USE_P to the list of pointers to operands used by STMT.  */

static inline void
append_use (tree *use_p, tree stmt ATTRIBUTE_UNUSED)
{
#ifdef ENABLE_CHECKING
  if (check_build_stmt != stmt)
    abort();
#endif
  VARRAY_PUSH_TREE_PTR (build_uses, use_p);
}


/* Add a new virtual def for variable VAR to statement STMT.  If PREV_VOPS
   is not NULL, the existing entries are preserved and no new entries are
   added here.  This is done to preserve the SSA numbering of virtual
   operands.  */

static void
append_v_may_def (tree var, tree stmt, voperands_t prev_vops)
{
  stmt_ann_t ann;
  size_t i;
  tree result, source;

#ifdef ENABLE_CHECKING
  if (check_build_stmt != stmt)
    abort();
#endif

  ann = stmt_ann (stmt);

  /* Don't allow duplicate entries.  */

  for (i = 0; i < VARRAY_ACTIVE_SIZE (build_v_may_defs); i += 2)
    {
      tree result = VARRAY_TREE (build_v_may_defs, i);
      if (var == result
	  || (TREE_CODE (result) == SSA_NAME
	      && var == SSA_NAME_VAR (result)))
	return;
    }

  /* If the statement already had virtual definitions, see if any of the
     existing V_MAY_DEFs matches VAR.  If so, re-use it, otherwise add a new
     V_MAY_DEF for VAR.  */
  result = NULL_TREE;
  source = NULL_TREE;
  if (prev_vops)
    for (i = 0; i < NUM_V_MAY_DEFS (prev_vops->v_may_def_ops); i++)
      {
	result = V_MAY_DEF_RESULT (prev_vops->v_may_def_ops, i);
	if (result == var
	    || (TREE_CODE (result) == SSA_NAME
		&& SSA_NAME_VAR (result) == var))
	  {
	    source = V_MAY_DEF_OP (prev_vops->v_may_def_ops, i);
	    break;
	  }
      }

  /* If no previous V_MAY_DEF operand was found for VAR, create one now.  */
  if (source == NULL_TREE)
    {
      result = var;
      source = var;
    }

  VARRAY_PUSH_TREE (build_v_may_defs, result);
  VARRAY_PUSH_TREE (build_v_may_defs, source);
}


/* Add VAR to the list of virtual uses for STMT.  If PREV_VOPS
   is not NULL, the existing entries are preserved and no new entries are
   added here.  This is done to preserve the SSA numbering of virtual
   operands.  */

static void
append_vuse (tree var, tree stmt, voperands_t prev_vops)
{
  stmt_ann_t ann;
  size_t i;
  bool found;
  tree vuse;

#ifdef ENABLE_CHECKING
  if (check_build_stmt != stmt)
    abort();
#endif

  ann = stmt_ann (stmt);

  /* Don't allow duplicate entries.  */
  for (i = 0; i < VARRAY_ACTIVE_SIZE (build_vuses); i++)
    {
      tree vuse_var = VARRAY_TREE (build_vuses, i);
      if (var == vuse_var
	  || (TREE_CODE (vuse_var) == SSA_NAME
	      && var == SSA_NAME_VAR (vuse_var)))
	return;
    }

  /* If the statement already had virtual uses, see if any of the
     existing VUSEs matches VAR.  If so, re-use it, otherwise add a new
     VUSE for VAR.  */
  found = false;
  vuse = NULL_TREE;
  if (prev_vops)
    for (i = 0; i < NUM_VUSES (prev_vops->vuse_ops); i++)
      {
	vuse = VUSE_OP (prev_vops->vuse_ops, i);
	if (vuse == var
	    || (TREE_CODE (vuse) == SSA_NAME
		&& SSA_NAME_VAR (vuse) == var))
	  {
	    found = true;
	    break;
	  }
      }

  /* If VAR existed already in PREV_VOPS, re-use it.  */
  if (found)
    var = vuse;

  VARRAY_PUSH_TREE (build_vuses, var);
}

/* Add VAR to the list of virtual must definitions for STMT.  If PREV_VOPS
   is not NULL, the existing entries are preserved and no new entries are
   added here.  This is done to preserve the SSA numbering of virtual
   operands.  */

static void
append_v_must_def (tree var, tree stmt, voperands_t prev_vops)
{
  stmt_ann_t ann;
  size_t i;
  bool found;
  tree v_must_def;

#ifdef ENABLE_CHECKING
  if (check_build_stmt != stmt)
    abort();
#endif

  ann = stmt_ann (stmt);

  /* Don't allow duplicate entries.  */
  for (i = 0; i < VARRAY_ACTIVE_SIZE (build_v_must_defs); i++)
    {
      tree v_must_def_var = VARRAY_TREE (build_v_must_defs, i);
      if (var == v_must_def_var
	  || (TREE_CODE (v_must_def_var) == SSA_NAME
	      && var == SSA_NAME_VAR (v_must_def_var)))
	return;
    }

  /* If the statement already had virtual must defs, see if any of the
     existing V_MUST_DEFs matches VAR.  If so, re-use it, otherwise add a new
     V_MUST_DEF for VAR.  */
  found = false;
  v_must_def = NULL_TREE;
  if (prev_vops)
    for (i = 0; i < NUM_V_MUST_DEFS (prev_vops->v_must_def_ops); i++)
      {
	v_must_def = V_MUST_DEF_OP (prev_vops->v_must_def_ops, i);
	if (v_must_def == var
	    || (TREE_CODE (v_must_def) == SSA_NAME
		&& SSA_NAME_VAR (v_must_def) == var))
	  {
	    found = true;
	    break;
	  }
      }

  /* If VAR existed already in PREV_VOPS, re-use it.  */
  if (found)
    var = v_must_def;

  VARRAY_PUSH_TREE (build_v_must_defs, var);
}


/* External entry point which by-passes the previous vops mechanism.  */
void
add_vuse (tree var, tree stmt)
{
  append_vuse (var, stmt, NULL);
}


/* Get the operands of statement STMT.  Note that repeated calls to
   get_stmt_operands for the same statement will do nothing until the
   statement is marked modified by a call to modify_stmt().  */

void
get_stmt_operands (tree stmt)
{
  enum tree_code code;
  stmt_ann_t ann;
  struct voperands_d prev_vops;

#if defined ENABLE_CHECKING
  /* The optimizers cannot handle statements that are nothing but a
     _DECL.  This indicates a bug in the gimplifier.  */
  if (SSA_VAR_P (stmt))
    abort ();
#endif

  /* Ignore error statements.  */
  if (TREE_CODE (stmt) == ERROR_MARK)
    return;

  ann = get_stmt_ann (stmt);

  /* If the statement has not been modified, the operands are still valid.  */
  if (!ann->modified)
    return;

  timevar_push (TV_TREE_OPS);

  /* Initially assume that the statement has no volatile operands, nor
     makes aliased loads or stores.  */
  ann->has_volatile_ops = false;
  ann->makes_aliased_stores = false;
  ann->makes_aliased_loads = false;

  /* Remove any existing operands as they will be scanned again.  */
  free_defs (&(ann->def_ops), true);
  free_uses (&(ann->use_ops), true);

  /* Before removing existing virtual operands, save them in PREV_VOPS so 
     that we can re-use their SSA versions.  */
  prev_vops.v_may_def_ops = V_MAY_DEF_OPS (ann);
  prev_vops.vuse_ops = VUSE_OPS (ann);
  prev_vops.v_must_def_ops = V_MUST_DEF_OPS (ann);

  /* Dont free the previous values to memory since we're still using them.  */
  free_v_may_defs (&(ann->v_may_def_ops), false);
  free_vuses (&(ann->vuse_ops), false);
  free_v_must_defs (&(ann->v_must_def_ops), false);

  start_ssa_stmt_operands (stmt);

  code = TREE_CODE (stmt);
  switch (code)
    {
    case MODIFY_EXPR:
      get_expr_operands (stmt, &TREE_OPERAND (stmt, 1), opf_none, &prev_vops);
      if (TREE_CODE (TREE_OPERAND (stmt, 0)) == ARRAY_REF 
          || TREE_CODE (TREE_OPERAND (stmt, 0)) == COMPONENT_REF
	  || TREE_CODE (TREE_OPERAND (stmt, 0)) == REALPART_EXPR
	  || TREE_CODE (TREE_OPERAND (stmt, 0)) == IMAGPART_EXPR
	  /* Use a V_MAY_DEF if the RHS might throw, as the LHS won't be
	     modified in that case.  FIXME we should represent somehow
	     that it is killed on the fallthrough path.  */
	  || tree_could_throw_p (TREE_OPERAND (stmt, 1)))
        get_expr_operands (stmt, &TREE_OPERAND (stmt, 0), opf_is_def, 
	                   &prev_vops);
      else
        get_expr_operands (stmt, &TREE_OPERAND (stmt, 0), 
	                   opf_is_def | opf_kill_def, &prev_vops);
      break;

    case COND_EXPR:
      get_expr_operands (stmt, &COND_EXPR_COND (stmt), opf_none, &prev_vops);
      break;

    case SWITCH_EXPR:
      get_expr_operands (stmt, &SWITCH_COND (stmt), opf_none, &prev_vops);
      break;

    case ASM_EXPR:
      {
	int noutputs = list_length (ASM_OUTPUTS (stmt));
	const char **oconstraints
	  = (const char **) alloca ((noutputs) * sizeof (const char *));
	int i;
	tree link;
	const char *constraint;
	bool allows_mem, allows_reg, is_inout;

	for (i=0, link = ASM_OUTPUTS (stmt); link;
	     ++i, link = TREE_CHAIN (link))
	  {
	    oconstraints[i] = constraint
	      = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
	    parse_output_constraint (&constraint, i, 0, 0,
				     &allows_mem, &allows_reg, &is_inout);
	    if (allows_reg && is_inout)
	      /* This should have been split in gimplify_asm_expr.  */
	      abort ();

	    if (!allows_reg && allows_mem)
	      {
		tree t = get_base_address (TREE_VALUE (link));
		if (t && DECL_P (t))
		  mark_call_clobbered (t);
	      }

	    get_expr_operands (stmt, &TREE_VALUE (link), opf_is_def,
			       &prev_vops);
	  }

	for (link = ASM_INPUTS (stmt); link; link = TREE_CHAIN (link))
	  {
	    constraint
	      = TREE_STRING_POINTER (TREE_VALUE (TREE_PURPOSE (link)));
	    parse_input_constraint (&constraint, 0, 0, noutputs, 0,
				    oconstraints, &allows_mem, &allows_reg);

	    if (!allows_reg && allows_mem)
	      {
		tree t = get_base_address (TREE_VALUE (link));
		if (t && DECL_P (t))
		  mark_call_clobbered (t);
	      }

	    get_expr_operands (stmt, &TREE_VALUE (link), 0, &prev_vops);
	  }

	/* Clobber memory for asm ("" : : : "memory");  */
	for (link = ASM_CLOBBERS (stmt); link; link = TREE_CHAIN (link))
	  if (!strcmp (TREE_STRING_POINTER (TREE_VALUE (link)), "memory"))
	    add_call_clobber_ops (stmt, &prev_vops);
      }
      break;

    case RETURN_EXPR:
      get_expr_operands (stmt, &TREE_OPERAND (stmt, 0), opf_none, &prev_vops);
      break;

    case GOTO_EXPR:
      get_expr_operands (stmt, &GOTO_DESTINATION (stmt), opf_none, &prev_vops);
      break;

    case LABEL_EXPR:
      get_expr_operands (stmt, &LABEL_EXPR_LABEL (stmt), opf_none, &prev_vops);
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
	 append_use.  This default will handle statements like empty statements,
	 CALL_EXPRs or VA_ARG_EXPRs that may appear on the RHS of a statement
	 or as statements themselves.  */
      get_expr_operands (stmt, &stmt, opf_none, &prev_vops);
      break;
    }

  finalize_ssa_stmt_operands (stmt);

  /* Now free the previous virtual ops to memory.  */
  free_v_may_defs (&(prev_vops.v_may_def_ops), true);
  free_vuses (&(prev_vops.vuse_ops), true);
  free_v_must_defs (&(prev_vops.v_must_def_ops), true);

  /* Clear the modified bit for STMT.  Subsequent calls to
     get_stmt_operands for this statement will do nothing until the
     statement is marked modified by a call to modify_stmt().  */
  ann->modified = 0;

  timevar_pop (TV_TREE_OPS);
}


/* Recursively scan the expression pointed by EXPR_P in statement STMT.
   FLAGS is one of the OPF_* constants modifying how to interpret the
   operands found.  PREV_VOPS is as in append_v_may_def and append_vuse.  */

static void
get_expr_operands (tree stmt, tree *expr_p, int flags, voperands_t prev_vops)
{
  enum tree_code code;
  char class;
  tree expr = *expr_p;

  if (expr == NULL || expr == error_mark_node)
    return;

  code = TREE_CODE (expr);
  class = TREE_CODE_CLASS (code);

  /* Expressions that make no memory references.  */
  if (class == 'c'
      || class == 't'
      || code == BLOCK
      || code == FUNCTION_DECL
      || code == EXC_PTR_EXPR
      || code == FILTER_EXPR
      || code == LABEL_DECL)
    return;

  /* We could have the address of a component, array member, etc which
     has interesting variable references.  */
  if (code == ADDR_EXPR)
    {
      enum tree_code subcode = TREE_CODE (TREE_OPERAND (expr, 0));

      /* Taking the address of a variable does not represent a
	 reference to it, but the fact that STMT takes its address will be
	 of interest to some passes (e.g. alias resolution).  */
      add_stmt_operand (expr_p, stmt, 0, NULL);

      /* If the address is invariant, there may be no interesting variable
	 references inside.  */
      if (is_gimple_min_invariant (expr))
	return;

      /* There should be no VUSEs created, since the referenced objects are
	 not really accessed.  The only operands that we should find here
	 are ARRAY_REF indices which will always be real operands (GIMPLE
	 does not allow non-registers as array indices).  */
      flags |= opf_no_vops;

      /* Avoid recursion.  */
      code = subcode;
      class = TREE_CODE_CLASS (code);
      expr_p = &TREE_OPERAND (expr, 0);
      expr = *expr_p;
    }

  /* If we found a variable, add it to DEFS or USES depending on the
     operand flags.  */
  if (SSA_VAR_P (expr))
    {
      add_stmt_operand (expr_p, stmt, flags, prev_vops);
      return;
    }

  /* Pointer dereferences always represent a use of the base pointer.  */
  if (code == INDIRECT_REF)
    {
      tree *pptr = &TREE_OPERAND (expr, 0);
      tree ptr = *pptr;

      if (SSA_VAR_P (ptr))
	{
	  if (!aliases_computed_p)
	    {
	      /* If the pointer does not have a memory tag and aliases have not
		 been computed yet, mark the statement as having volatile
		 operands to prevent DOM from entering it in equivalence tables
		 and DCE from killing it.  */
	      stmt_ann (stmt)->has_volatile_ops = true;
	    }
	  else
	    {
	      struct ptr_info_def *pi = NULL;

	      /* If we have computed aliasing already, check if PTR has
		 flow-sensitive points-to information.  */
	      if (TREE_CODE (ptr) == SSA_NAME
		  && (pi = SSA_NAME_PTR_INFO (ptr)) != NULL
		  && pi->name_mem_tag)
		{
		  /* PTR has its own memory tag.  Use it.  */
		  add_stmt_operand (&pi->name_mem_tag, stmt, flags,
		                    prev_vops);
		}
	      else
		{
		  /* If PTR is not an SSA_NAME or it doesn't have a name
		     tag, use its type memory tag.  */
		  var_ann_t ann;

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
		  ann = var_ann (ptr);
		  add_stmt_operand (&ann->type_mem_tag, stmt, flags, prev_vops);
		}
	    }
	}

      /* If a constant is used as a pointer, we can't generate a real
	 operand for it but we mark the statement volatile to prevent
	 optimizations from messing things up.  */
      else if (TREE_CODE (ptr) == INTEGER_CST)
	{
	  stmt_ann (stmt)->has_volatile_ops = true;
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
          add_stmt_operand (pptr, stmt, 0, NULL);

	  /* Mark the object itself with a VUSE.  */
	  pptr = &TREE_OPERAND (*pptr, 0);
	  get_expr_operands (stmt, pptr, flags, prev_vops);
	  return;
	}

      /* Ok, this isn't even is_gimple_min_invariant.  Something's broke.  */
      else
	abort ();

      /* Add a USE operand for the base pointer.  */
      get_expr_operands (stmt, pptr, opf_none, prev_vops);
      return;
    }

  /* Treat array references as references to the virtual variable
     representing the array.  The virtual variable for an ARRAY_REF
     is the VAR_DECL for the array.  */
  if (code == ARRAY_REF)
    {
      /* Add the virtual variable for the ARRAY_REF to VDEFS or VUSES
	 according to the value of IS_DEF.  Recurse if the LHS of the
	 ARRAY_REF node is not a regular variable.  */
      if (SSA_VAR_P (TREE_OPERAND (expr, 0)))
	add_stmt_operand (expr_p, stmt, flags, prev_vops);
      else
	get_expr_operands (stmt, &TREE_OPERAND (expr, 0), flags, prev_vops);

      get_expr_operands (stmt, &TREE_OPERAND (expr, 1), opf_none, prev_vops);
      return;
    }

  /* Similarly to arrays, references to compound variables (complex types
     and structures/unions) are globbed.

     FIXME: This means that

     			a.x = 6;
			a.y = 7;
			foo (a.x, a.y);

	   will not be constant propagated because the two partial
	   definitions to 'a' will kill each other.  Note that SRA may be
	   able to fix this problem if 'a' can be scalarized.  */
  if (code == IMAGPART_EXPR || code == REALPART_EXPR || code == COMPONENT_REF)
    {
      /* If the LHS of the compound reference is not a regular variable,
	 recurse to keep looking for more operands in the subexpression.  */
      if (SSA_VAR_P (TREE_OPERAND (expr, 0)))
	add_stmt_operand (expr_p, stmt, flags, prev_vops);
      else
	get_expr_operands (stmt, &TREE_OPERAND (expr, 0), flags, prev_vops);

      return;
    }

  /* Function calls.  Add every argument to USES.  If the callee is
     neither pure nor const, create a VDEF reference for GLOBAL_VAR
     (See find_vars_r).  */
  if (code == CALL_EXPR)
    {
      tree op;
      int call_flags = call_expr_flags (expr);

      /* Find uses in the called function.  */
      get_expr_operands (stmt, &TREE_OPERAND (expr, 0), opf_none, prev_vops);

      for (op = TREE_OPERAND (expr, 1); op; op = TREE_CHAIN (op))
        get_expr_operands (stmt, &TREE_VALUE (op), opf_none, prev_vops);

      get_expr_operands (stmt, &TREE_OPERAND (expr, 2), opf_none, prev_vops);

      if (bitmap_first_set_bit (call_clobbered_vars) >= 0)
	{
	  /* A 'pure' or a 'const' functions never call clobber anything. 
	     A 'noreturn' function might, but since we don't return anyway 
	     there is no point in recording that.  */ 
	  if (!(call_flags
		& (ECF_PURE | ECF_CONST | ECF_NORETURN)))
	    add_call_clobber_ops (stmt, prev_vops);
	  else if (!(call_flags & (ECF_CONST | ECF_NORETURN)))
	    add_call_read_ops (stmt, prev_vops);
	}
      else if (!aliases_computed_p)
	stmt_ann (stmt)->has_volatile_ops = true;

      return;
    }

  /* Lists.  */
  if (code == TREE_LIST)
    {
      tree op;

      for (op = expr; op; op = TREE_CHAIN (op))
        get_expr_operands (stmt, &TREE_VALUE (op), flags, prev_vops);

      return;
    }

  /* Assignments.  */
  if (code == MODIFY_EXPR)
    {
      get_expr_operands (stmt, &TREE_OPERAND (expr, 1), opf_none, prev_vops);
      if (TREE_CODE (TREE_OPERAND (expr, 0)) == ARRAY_REF 
          || TREE_CODE (TREE_OPERAND (expr, 0)) == COMPONENT_REF
	  || TREE_CODE (TREE_OPERAND (expr, 0)) == REALPART_EXPR
	  || TREE_CODE (TREE_OPERAND (expr, 0)) == IMAGPART_EXPR)
        get_expr_operands (stmt, &TREE_OPERAND (expr, 0), opf_is_def, 
	                   prev_vops);
      else
        get_expr_operands (stmt, &TREE_OPERAND (expr, 0), 
	                   opf_is_def | opf_kill_def, prev_vops);
      return;
    }


  /* Mark VA_ARG_EXPR nodes as making volatile references.  FIXME,
     this is needed because we currently do not gimplify VA_ARG_EXPR
     properly.  */
  if (code == VA_ARG_EXPR)
    {
      stmt_ann (stmt)->has_volatile_ops = true;
      return;
    }

  /* Unary expressions.  */
  if (class == '1'
      || code == TRUTH_NOT_EXPR
      || code == BIT_FIELD_REF
      || code == CONSTRUCTOR)
    {
      get_expr_operands (stmt, &TREE_OPERAND (expr, 0), flags, prev_vops);
      return;
    }

  /* Binary expressions.  */
  if (class == '2'
      || class == '<'
      || code == TRUTH_AND_EXPR
      || code == TRUTH_OR_EXPR
      || code == TRUTH_XOR_EXPR
      || code == COMPOUND_EXPR)
    {
      get_expr_operands (stmt, &TREE_OPERAND (expr, 0), flags, prev_vops);
      get_expr_operands (stmt, &TREE_OPERAND (expr, 1), flags, prev_vops);
      return;
    }

  /* If we get here, something has gone wrong.  */
  fprintf (stderr, "unhandled expression in get_expr_operands():\n");
  debug_tree (expr);
  fputs ("\n", stderr);
  abort ();
}


/* Add *VAR_P to the appropriate operand array of STMT.  FLAGS is as in
   get_expr_operands.  If *VAR_P is a GIMPLE register, it will be added to
   the statement's real operands, otherwise it is added to virtual
   operands.

   PREV_VOPS is used when adding virtual operands to statements that
      already had them (See append_v_may_def and append_vuse).  */

static void
add_stmt_operand (tree *var_p, tree stmt, int flags, voperands_t prev_vops)
{
  bool is_real_op;
  tree var, sym;
  stmt_ann_t s_ann;
  var_ann_t v_ann;

  var = *var_p;
  STRIP_NOPS (var);

  s_ann = stmt_ann (stmt);

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

  /* FIXME: We currently refuse to optimize variables that have hidden uses
     (variables used in VLA declarations, MD builtin calls and variables
     from the parent function in nested functions).  This is because not
     all uses of these variables are exposed in the IL or the statements
     that reference them are not in GIMPLE form.  If that's the case, mark
     the statement as having volatile operands and return.  */
  if (v_ann->has_hidden_use)
    {
      s_ann->has_volatile_ops = true;
      return;
    }

  /* Don't expose volatile variables to the optimizers.  */
  if (TREE_THIS_VOLATILE (sym))
    {
      s_ann->has_volatile_ops = true;
      return;
    }

  if (is_real_op)
    {
      /* The variable is a GIMPLE register.  Add it to real operands.  */
      if (flags & opf_is_def)
	append_def (var_p, stmt);
      else
	append_use (var_p, stmt);
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

      /* If alias information hasn't been computed yet, then
	 addressable variables will not be an alias tag nor will they
	 have aliases.  In this case, mark the statement as having
	 volatile operands.  */
      if (!aliases_computed_p && may_be_aliased (var))
	s_ann->has_volatile_ops = true;

      if (aliases == NULL)
	{
	  /* The variable is not aliased or it is an alias tag.  */
	  if (flags & opf_is_def)
	    {
	      if (v_ann->is_alias_tag)
	        {
		  /* Alias tagged vars get regular V_MAY_DEF  */
		  s_ann->makes_aliased_stores = 1;
		  append_v_may_def (var, stmt, prev_vops);
		}
	      else if ((flags & opf_kill_def) 
	                && v_ann->mem_tag_kind == NOT_A_TAG)
	        /* V_MUST_DEF for non-aliased non-GIMPLE register 
		   variable definitions. Avoid memory tags.  */
	        append_v_must_def (var, stmt, prev_vops);
	      else
	        /* Call-clobbered variables & memory tags get 
		   V_MAY_DEF  */
		append_v_may_def (var, stmt, prev_vops);
	    }
	  else
	    {
	      append_vuse (var, stmt, prev_vops);
	      if (v_ann->is_alias_tag)
		s_ann->makes_aliased_loads = 1;
	    }
	}
      else
	{
	  size_t i;

	  /* The variable is aliased.  Add its aliases to the virtual
	     operands.  */
	  if (VARRAY_ACTIVE_SIZE (aliases) == 0)
	    abort ();

	  if (flags & opf_is_def)
	    {
	      /* If the variable is also an alias tag, add a virtual
		 operand for it, otherwise we will miss representing
		 references to the members of the variable's alias set.
		 This fixes the bug in gcc.c-torture/execute/20020503-1.c.  */
	      if (v_ann->is_alias_tag)
		append_v_may_def (var, stmt, prev_vops);

	      for (i = 0; i < VARRAY_ACTIVE_SIZE (aliases); i++)
		append_v_may_def (VARRAY_TREE (aliases, i), stmt, prev_vops);

	      s_ann->makes_aliased_stores = 1;
	    }
	  else
	    {
	      if (v_ann->is_alias_tag)
		append_vuse (var, stmt, prev_vops);

	      for (i = 0; i < VARRAY_ACTIVE_SIZE (aliases); i++)
		append_vuse (VARRAY_TREE (aliases, i), stmt, prev_vops);

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
add_call_clobber_ops (tree stmt, voperands_t prev_vops)
{
  /* Functions that are not const, pure or never return may clobber
     call-clobbered variables.  */
  stmt_ann (stmt)->makes_clobbering_call = true;

  /* If we had created .GLOBAL_VAR earlier, use it.  Otherwise, add 
     a V_MAY_DEF operand for every call clobbered variable.  See 
     compute_may_aliases for the heuristic used to decide whether 
     to create .GLOBAL_VAR or not.  */
  if (global_var)
    add_stmt_operand (&global_var, stmt, opf_is_def, prev_vops);
  else
    {
      size_t i;

      EXECUTE_IF_SET_IN_BITMAP (call_clobbered_vars, 0, i,
	{
	  tree var = referenced_var (i);

	  /* If VAR is read-only, don't add a V_MAY_DEF, just a 
	     VUSE operand.  */
	  if (!TREE_READONLY (var))
	    add_stmt_operand (&var, stmt, opf_is_def, prev_vops);
	  else
	    add_stmt_operand (&var, stmt, opf_none, prev_vops);
	});
    }
}


/* Add VUSE operands for .GLOBAL_VAR or all call clobbered variables in the
   function.  */

static void
add_call_read_ops (tree stmt, voperands_t prev_vops)
{
  /* Otherwise, if the function is not pure, it may reference memory.  Add
     a VUSE for .GLOBAL_VAR if it has been created.  Otherwise, add a VUSE
     for each call-clobbered variable.  See add_referenced_var for the
     heuristic used to decide whether to create .GLOBAL_VAR.  */
  if (global_var)
    add_stmt_operand (&global_var, stmt, opf_none, prev_vops);
  else
    {
      size_t i;

      EXECUTE_IF_SET_IN_BITMAP (call_clobbered_vars, 0, i,
	{
	  tree var = referenced_var (i);
	  add_stmt_operand (&var, stmt, opf_none, prev_vops);
	});
    }
}

#include "gt-tree-ssa-operands.h"
