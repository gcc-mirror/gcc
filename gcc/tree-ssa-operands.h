/* SSA operand management for trees.
   Copyright (C) 2003, 2005, 2006, 2007 Free Software Foundation, Inc.

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

#ifndef GCC_TREE_SSA_OPERANDS_H
#define GCC_TREE_SSA_OPERANDS_H

/* Interface to SSA operands.  */


/* This represents a pointer to a DEF operand.  */
typedef tree *def_operand_p;

/* This represents a pointer to a USE operand.  */
typedef ssa_use_operand_t *use_operand_p;

/* NULL operand types.  */
#define NULL_USE_OPERAND_P 		NULL
#define NULL_DEF_OPERAND_P 		NULL

/* This represents the DEF operands of a stmt.  */
struct def_optype_d
{
  struct def_optype_d *next;
  tree *def_ptr;
};
typedef struct def_optype_d *def_optype_p;

/* This represents the USE operands of a stmt.  */
struct use_optype_d 
{
  struct use_optype_d *next;
  struct ssa_use_operand_d use_ptr;
};
typedef struct use_optype_d *use_optype_p;

typedef struct vuse_element_d
{
  tree use_var;
  struct ssa_use_operand_d use_ptr;
} vuse_element_t;

typedef struct vuse_vec_d
{
  unsigned int num_vuse;
  vuse_element_t uses[1];
} vuse_vec_t;
typedef struct vuse_vec_d *vuse_vec_p;

#define VUSE_VECT_NUM_ELEM(V)		(V).num_vuse
#define VUSE_VECT_ELEMENT_NC(V,X)	(V).uses[(X)]
#define VUSE_ELEMENT_PTR_NC(V,X)	(&(VUSE_VECT_ELEMENT_NC ((V),(X)).use_ptr))
#define VUSE_ELEMENT_VAR_NC(V,X)	(VUSE_VECT_ELEMENT_NC ((V),(X)).use_var)

#ifdef ENABLE_CHECKING
#define VUSE_VECT_ELEMENT(V,X)						\
    (gcc_assert (((unsigned int) (X)) < VUSE_VECT_NUM_ELEM (V)),	\
     VUSE_VECT_ELEMENT_NC (V,X))

#define VUSE_ELEMENT_PTR(V,X)						\
    (gcc_assert (((unsigned int) (X)) < VUSE_VECT_NUM_ELEM (V)),	\
     VUSE_ELEMENT_PTR_NC (V, X))

#define SET_VUSE_VECT_ELEMENT(V,X,N)					\
    (gcc_assert (((unsigned int) (X)) < VUSE_VECT_NUM_ELEM (V)),	\
     VUSE_VECT_ELEMENT_NC (V,X) = (N))

#define SET_VUSE_ELEMENT_VAR(V,X,N)					\
    (gcc_assert (((unsigned int) (X)) < VUSE_VECT_NUM_ELEM (V)),	\
     VUSE_VECT_ELEMENT_NC ((V),(X)).use_var = (N))

#define SET_VUSE_ELEMENT_PTR(V,X,N)					\
    (gcc_assert (((unsigned int) (X)) < VUSE_VECT_NUM_ELEM (V)),	\
     VUSE_ELEMENT_PTR_NC (V, X) = (N))
#else
#define VUSE_VECT_ELEMENT(V,X) VUSE_VECT_ELEMENT_NC(V,X)
#define VUSE_ELEMENT_PTR(V,X) VUSE_ELEMENT_PTR_NC(V,X)
#define SET_VUSE_VECT_ELEMENT(V,X,N) VUSE_VECT_ELEMENT_NC(V,X) = (N)
#define SET_VUSE_ELEMENT_PTR(V,X,N) VUSE_ELEMENT_PTR_NC(V,X) = (N)
#define SET_VUSE_ELEMENT_VAR(V,X,N) VUSE_VECT_ELEMENT_NC ((V),(X)).use_var = (N)
#endif

#define VUSE_ELEMENT_VAR(V,X)	(VUSE_VECT_ELEMENT ((V),(X)).use_var)

/* This represents the virtual ops of a stmt.  */
struct voptype_d
{
  struct voptype_d *next;
  tree def_var;
  vuse_vec_t usev;
};
typedef struct voptype_d *voptype_p;

/* This structure represents a variable sized buffer which is allocated by the
   operand memory manager.  Operands are suballocated out of this block.  The
   MEM array varies in size.  */
   
struct ssa_operand_memory_d GTY((chain_next("%h.next")))
{
  struct ssa_operand_memory_d *next;
  char mem[1];
};

/* Number of different size free buckets for virtual operands.  */
#define NUM_VOP_FREE_BUCKETS		29

/* Per-function operand caches.  */
struct ssa_operands GTY(()) {
   struct ssa_operand_memory_d *operand_memory;
   unsigned operand_memory_index;
   /* Current size of the operand memory buffer.  */
   unsigned int ssa_operand_mem_size;

   bool ops_active;

   struct def_optype_d * GTY ((skip (""))) free_defs;
   struct use_optype_d * GTY ((skip (""))) free_uses;
   struct voptype_d * GTY ((skip (""))) vop_free_buckets[NUM_VOP_FREE_BUCKETS];
   VEC(tree,heap) * GTY ((skip (""))) mpt_table;
};

/* This represents the operand cache for a stmt.  */
struct stmt_operands_d
{
  /* Statement operands.  */
  struct def_optype_d * def_ops;
  struct use_optype_d * use_ops;
                                                                              
  /* Virtual operands (VDEF, VUSE).  */
  struct voptype_d * vdef_ops;
  struct voptype_d * vuse_ops;

  /* Sets of memory symbols loaded and stored.  */
  bitmap stores;
  bitmap loads;
};
                                                                              
typedef struct stmt_operands_d *stmt_operands_p;
                                                                              
#define USE_FROM_PTR(PTR)	get_use_from_ptr (PTR)
#define DEF_FROM_PTR(PTR)	get_def_from_ptr (PTR)
#define SET_USE(USE, V)		set_ssa_use_from_ptr (USE, V)
#define SET_DEF(DEF, V)		((*(DEF)) = (V))

#define USE_STMT(USE)		(USE)->stmt

#define DEF_OPS(STMT)		(stmt_ann (STMT)->operands.def_ops)
#define USE_OPS(STMT)		(stmt_ann (STMT)->operands.use_ops)
#define VUSE_OPS(STMT)		(stmt_ann (STMT)->operands.vuse_ops)
#define VDEF_OPS(STMT)		(stmt_ann (STMT)->operands.vdef_ops)

#define LOADED_SYMS(STMT)	(stmt_ann (STMT)->operands.loads)
#define STORED_SYMS(STMT)	(stmt_ann (STMT)->operands.stores)

#define USE_OP_PTR(OP)		(&((OP)->use_ptr))
#define USE_OP(OP)		(USE_FROM_PTR (USE_OP_PTR (OP)))

#define DEF_OP_PTR(OP)		((OP)->def_ptr)
#define DEF_OP(OP)		(DEF_FROM_PTR (DEF_OP_PTR (OP)))

#define VUSE_OP_PTR(OP,X)	VUSE_ELEMENT_PTR ((OP)->usev, (X)) 
#define VUSE_OP(OP,X)		VUSE_ELEMENT_VAR ((OP)->usev, (X))
#define SET_VUSE_OP(OP,X,N)	SET_VUSE_ELEMENT_VAR ((OP)->usev, (X), (N))
#define VUSE_NUM(OP)		VUSE_VECT_NUM_ELEM ((OP)->usev)
#define VUSE_VECT(OP)		&((OP)->usev)

#define VDEF_RESULT_PTR(OP)	(&((OP)->def_var))
#define VDEF_RESULT(OP)		((OP)->def_var)
#define VDEF_OP_PTR(OP,X)	VUSE_OP_PTR (OP, X)
#define VDEF_OP(OP,X)		VUSE_OP (OP, X)
#define SET_VDEF_OP(OP,X,N)	SET_VUSE_OP (OP, X, N)
#define VDEF_NUM(OP)		VUSE_VECT_NUM_ELEM ((OP)->usev)
#define VDEF_VECT(OP)		&((OP)->usev)

#define PHI_RESULT_PTR(PHI)	get_phi_result_ptr (PHI)
#define PHI_RESULT(PHI)		DEF_FROM_PTR (PHI_RESULT_PTR (PHI))
#define SET_PHI_RESULT(PHI, V)	SET_DEF (PHI_RESULT_PTR (PHI), (V))

#define PHI_ARG_DEF_PTR(PHI, I)	get_phi_arg_def_ptr ((PHI), (I))
#define PHI_ARG_DEF(PHI, I)	USE_FROM_PTR (PHI_ARG_DEF_PTR ((PHI), (I)))
#define SET_PHI_ARG_DEF(PHI, I, V)					\
				SET_USE (PHI_ARG_DEF_PTR ((PHI), (I)), (V))
#define PHI_ARG_DEF_FROM_EDGE(PHI, E)					\
				PHI_ARG_DEF ((PHI), (E)->dest_idx)
#define PHI_ARG_DEF_PTR_FROM_EDGE(PHI, E)				\
				PHI_ARG_DEF_PTR ((PHI), (E)->dest_idx)
#define PHI_ARG_INDEX_FROM_USE(USE)   phi_arg_index_from_use (USE)


extern void init_ssa_operands (void);
extern void fini_ssa_operands (void);
extern void free_ssa_operands (stmt_operands_p);
extern void update_stmt_operands (tree);
extern void free_stmt_operands (tree);
extern bool verify_imm_links (FILE *f, tree var);

extern void copy_virtual_operands (tree, tree);
extern int operand_build_cmp (const void *, const void *);
extern void create_ssa_artificial_load_stmt (tree, tree, bool);

extern void dump_immediate_uses (FILE *file);
extern void dump_immediate_uses_for (FILE *file, tree var);
extern void debug_immediate_uses (void);
extern void debug_immediate_uses_for (tree var);
extern void dump_decl_set (FILE *, bitmap);
extern void debug_decl_set (bitmap);

extern bool ssa_operands_active (void);

extern void add_to_addressable_set (tree, bitmap *);
extern void push_stmt_changes (tree *);
extern void pop_stmt_changes (tree *);
extern void discard_stmt_changes (tree *);

enum ssa_op_iter_type {
  ssa_op_iter_none = 0,
  ssa_op_iter_tree,
  ssa_op_iter_use,
  ssa_op_iter_def,
  ssa_op_iter_vdef
};

/* This structure is used in the operand iterator loops.  It contains the 
   items required to determine which operand is retrieved next.  During
   optimization, this structure is scalarized, and any unused fields are 
   optimized away, resulting in little overhead.  */

typedef struct ssa_operand_iterator_d
{
  def_optype_p defs;
  use_optype_p uses;
  voptype_p vuses;
  voptype_p vdefs;
  voptype_p mayuses;
  enum ssa_op_iter_type iter_type;
  int phi_i;
  int num_phi;
  tree phi_stmt;
  bool done;
  unsigned int vuse_index;
  unsigned int mayuse_index;
} ssa_op_iter;

/* These flags are used to determine which operands are returned during 
   execution of the loop.  */
#define SSA_OP_USE		0x01	/* Real USE operands.  */
#define SSA_OP_DEF		0x02	/* Real DEF operands.  */
#define SSA_OP_VUSE		0x04	/* VUSE operands.  */
#define SSA_OP_VMAYUSE		0x08	/* USE portion of VDEFS.  */
#define SSA_OP_VDEF		0x10	/* DEF portion of VDEFS.  */

/* These are commonly grouped operand flags.  */
#define SSA_OP_VIRTUAL_USES	(SSA_OP_VUSE | SSA_OP_VMAYUSE)
#define SSA_OP_VIRTUAL_DEFS	(SSA_OP_VDEF)
#define SSA_OP_ALL_VIRTUALS     (SSA_OP_VIRTUAL_USES | SSA_OP_VIRTUAL_DEFS)
#define SSA_OP_ALL_USES		(SSA_OP_VIRTUAL_USES | SSA_OP_USE)
#define SSA_OP_ALL_DEFS		(SSA_OP_VIRTUAL_DEFS | SSA_OP_DEF)
#define SSA_OP_ALL_OPERANDS	(SSA_OP_ALL_USES | SSA_OP_ALL_DEFS)

/* This macro executes a loop over the operands of STMT specified in FLAG, 
   returning each operand as a 'tree' in the variable TREEVAR.  ITER is an
   ssa_op_iter structure used to control the loop.  */
#define FOR_EACH_SSA_TREE_OPERAND(TREEVAR, STMT, ITER, FLAGS)	\
  for (TREEVAR = op_iter_init_tree (&(ITER), STMT, FLAGS);	\
       !op_iter_done (&(ITER));					\
       TREEVAR = op_iter_next_tree (&(ITER)))

/* This macro executes a loop over the operands of STMT specified in FLAG, 
   returning each operand as a 'use_operand_p' in the variable USEVAR.  
   ITER is an ssa_op_iter structure used to control the loop.  */
#define FOR_EACH_SSA_USE_OPERAND(USEVAR, STMT, ITER, FLAGS)	\
  for (USEVAR = op_iter_init_use (&(ITER), STMT, FLAGS);	\
       !op_iter_done (&(ITER));					\
       USEVAR = op_iter_next_use (&(ITER)))

/* This macro executes a loop over the operands of STMT specified in FLAG, 
   returning each operand as a 'def_operand_p' in the variable DEFVAR.  
   ITER is an ssa_op_iter structure used to control the loop.  */
#define FOR_EACH_SSA_DEF_OPERAND(DEFVAR, STMT, ITER, FLAGS)	\
  for (DEFVAR = op_iter_init_def (&(ITER), STMT, FLAGS);	\
       !op_iter_done (&(ITER));					\
       DEFVAR = op_iter_next_def (&(ITER)))

/* This macro executes a loop over the VDEF operands of STMT.  The def
   and use vector for each VDEF is returned in DEFVAR and USEVECT. 
   ITER is an ssa_op_iter structure used to control the loop.  */
#define FOR_EACH_SSA_VDEF_OPERAND(DEFVAR, USEVECT, STMT, ITER)	\
  for (op_iter_init_vdef (&(ITER), STMT, &(USEVECT), &(DEFVAR));	\
       !op_iter_done (&(ITER));					\
       op_iter_next_vdef (&(USEVECT), &(DEFVAR), &(ITER)))

/* This macro will execute a loop over all the arguments of a PHI which
   match FLAGS.   A use_operand_p is always returned via USEVAR.  FLAGS
   can be either SSA_OP_USE or SSA_OP_VIRTUAL_USES or SSA_OP_ALL_USES.  */
#define FOR_EACH_PHI_ARG(USEVAR, STMT, ITER, FLAGS)		\
  for ((USEVAR) = op_iter_init_phiuse (&(ITER), STMT, FLAGS);	\
       !op_iter_done (&(ITER));					\
       (USEVAR) = op_iter_next_use (&(ITER)))


/* This macro will execute a loop over a stmt, regardless of whether it is
   a real stmt or a PHI node, looking at the USE nodes matching FLAGS.  */
#define FOR_EACH_PHI_OR_STMT_USE(USEVAR, STMT, ITER, FLAGS)	\
  for ((USEVAR) = (TREE_CODE (STMT) == PHI_NODE 		\
		   ? op_iter_init_phiuse (&(ITER), STMT, FLAGS)	\
		   : op_iter_init_use (&(ITER), STMT, FLAGS));	\
       !op_iter_done (&(ITER));					\
       (USEVAR) = op_iter_next_use (&(ITER)))

/* This macro will execute a loop over a stmt, regardless of whether it is
   a real stmt or a PHI node, looking at the DEF nodes matching FLAGS.  */
#define FOR_EACH_PHI_OR_STMT_DEF(DEFVAR, STMT, ITER, FLAGS)	\
  for ((DEFVAR) = (TREE_CODE (STMT) == PHI_NODE 		\
		   ? op_iter_init_phidef (&(ITER), STMT, FLAGS)	\
		   : op_iter_init_def (&(ITER), STMT, FLAGS));	\
       !op_iter_done (&(ITER));					\
       (DEFVAR) = op_iter_next_def (&(ITER)))
  
/* This macro returns an operand in STMT as a tree if it is the ONLY
   operand matching FLAGS.  If there are 0 or more than 1 operand matching
   FLAGS, then NULL_TREE is returned.  */
#define SINGLE_SSA_TREE_OPERAND(STMT, FLAGS)			\
  single_ssa_tree_operand (STMT, FLAGS)
                                                                                
/* This macro returns an operand in STMT as a use_operand_p if it is the ONLY
   operand matching FLAGS.  If there are 0 or more than 1 operand matching
   FLAGS, then NULL_USE_OPERAND_P is returned.  */
#define SINGLE_SSA_USE_OPERAND(STMT, FLAGS)			\
  single_ssa_use_operand (STMT, FLAGS)
                                                                                
/* This macro returns an operand in STMT as a def_operand_p if it is the ONLY
   operand matching FLAGS.  If there are 0 or more than 1 operand matching
   FLAGS, then NULL_DEF_OPERAND_P is returned.  */
#define SINGLE_SSA_DEF_OPERAND(STMT, FLAGS)			\
  single_ssa_def_operand (STMT, FLAGS)

/* This macro returns TRUE if there are no operands matching FLAGS in STMT.  */
#define ZERO_SSA_OPERANDS(STMT, FLAGS) 	zero_ssa_operands (STMT, FLAGS)

/* This macro counts the number of operands in STMT matching FLAGS.  */
#define NUM_SSA_OPERANDS(STMT, FLAGS)	num_ssa_operands (STMT, FLAGS)

#endif  /* GCC_TREE_SSA_OPERANDS_H  */
