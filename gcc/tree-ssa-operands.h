/* SSA operand management for trees.
   Copyright (C) 2003, 2005 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_TREE_SSA_OPERANDS_H
#define GCC_TREE_SSA_OPERANDS_H

/* Interface to SSA operands.  */


/* This represents a pointer to a DEF operand.  */
typedef struct def_operand_ptr GTY(())
{
  tree * GTY((skip(""))) def;
} def_operand_p;

/* This represents a pointer to a USE operand.  */
typedef struct use_operand_ptr GTY(())
{
  tree * GTY((skip(""))) use;
} use_operand_p;

extern def_operand_p NULL_DEF_OPERAND_P;
extern use_operand_p NULL_USE_OPERAND_P;

/* This represents the DEF operands of a stmt.  */
typedef struct def_optype_d GTY(())
{
  unsigned num_defs; 
  struct def_operand_ptr GTY((length("%h.num_defs"))) defs[1];
} def_optype_t;

typedef def_optype_t *def_optype;

/* This represents the USE operands of a stmt.  */
typedef struct use_optype_d GTY(())
{
  unsigned num_uses; 
  struct use_operand_ptr GTY((length("%h.num_uses"))) uses[1];
} use_optype_t;

typedef use_optype_t *use_optype;

/* Operand type which stores a def and a use tree.  */
typedef struct v_def_use_operand_type GTY(())
{
  tree def;
  tree use;
} v_def_use_operand_type_t;

/* This represents the MAY_DEFS for a stmt.  */
typedef struct v_may_def_optype_d GTY(())
{
  unsigned num_v_may_defs; 
  struct v_def_use_operand_type GTY((length ("%h.num_v_may_defs")))
							      v_may_defs[1];
} v_may_def_optype_t;

typedef v_may_def_optype_t *v_may_def_optype;

/* This represents the VUSEs for a stmt.  */
typedef struct vuse_optype_d GTY(()) 
{
  unsigned num_vuses; 
  tree GTY((length ("%h.num_vuses"))) vuses[1];
} vuse_optype_t;

typedef vuse_optype_t *vuse_optype;

/* This represents the V_MUST_DEFS for a stmt.  */
typedef struct v_must_def_optype_d GTY(())
{
  unsigned num_v_must_defs; 
  v_def_use_operand_type_t GTY((length("%h.num_v_must_defs"))) v_must_defs[1];
} v_must_def_optype_t;

typedef v_must_def_optype_t *v_must_def_optype;

/* This represents the operand cache fora stmt.  */
typedef struct stmt_operands_d GTY(())
{
  /* Statement operands.  */
  struct def_optype_d * GTY (()) def_ops;
  struct use_optype_d * GTY (()) use_ops;

  /* Virtual operands (V_MAY_DEF, VUSE, and V_MUST_DEF).  */
  struct v_may_def_optype_d * GTY (()) v_may_def_ops;
  struct vuse_optype_d * GTY (()) vuse_ops;
  struct v_must_def_optype_d * GTY (()) v_must_def_ops;
} stmt_operands_t;

typedef stmt_operands_t *stmt_operands_p;

#define USE_FROM_PTR(OP)	get_use_from_ptr (OP)
#define DEF_FROM_PTR(OP)	get_def_from_ptr (OP)
#define SET_USE(OP, V)		((*((OP).use)) = (V))
#define SET_DEF(OP, V)		((*((OP).def)) = (V))


#define USE_OPS(ANN)		get_use_ops (ANN)
#define STMT_USE_OPS(STMT)	get_use_ops (stmt_ann (STMT))
#define NUM_USES(OPS)		((OPS) ? (OPS)->num_uses : 0)
#define USE_OP_PTR(OPS, I)	get_use_op_ptr ((OPS), (I))
#define USE_OP(OPS, I)		(USE_FROM_PTR (USE_OP_PTR ((OPS), (I))))
#define SET_USE_OP(OPS, I, V)	(SET_USE (USE_OP_PTR ((OPS), (I)), (V)))



#define DEF_OPS(ANN)		get_def_ops (ANN)
#define STMT_DEF_OPS(STMT)	get_def_ops (stmt_ann (STMT))
#define NUM_DEFS(OPS)		((OPS) ? (OPS)->num_defs : 0)
#define DEF_OP_PTR(OPS, I)	get_def_op_ptr ((OPS), (I))
#define DEF_OP(OPS, I)		(DEF_FROM_PTR (DEF_OP_PTR ((OPS), (I))))
#define SET_DEF_OP(OPS, I, V)	(SET_DEF (DEF_OP_PTR ((OPS), (I)), (V)))



#define V_MAY_DEF_OPS(ANN)		get_v_may_def_ops (ANN)
#define STMT_V_MAY_DEF_OPS(STMT)	get_v_may_def_ops (stmt_ann(STMT))
#define NUM_V_MAY_DEFS(OPS)		((OPS) ? (OPS)->num_v_may_defs : 0)
#define V_MAY_DEF_RESULT_PTR(OPS, I)	get_v_may_def_result_ptr ((OPS), (I))
#define V_MAY_DEF_RESULT(OPS, I)					\
			    (DEF_FROM_PTR (V_MAY_DEF_RESULT_PTR ((OPS), (I))))
#define SET_V_MAY_DEF_RESULT(OPS, I, V)					\
			    (SET_DEF (V_MAY_DEF_RESULT_PTR ((OPS), (I)), (V)))
#define V_MAY_DEF_OP_PTR(OPS, I)	get_v_may_def_op_ptr ((OPS), (I))
#define V_MAY_DEF_OP(OPS, I)						\
			    (USE_FROM_PTR (V_MAY_DEF_OP_PTR ((OPS), (I))))
#define SET_V_MAY_DEF_OP(OPS, I, V)					\
			    (SET_USE (V_MAY_DEF_OP_PTR ((OPS), (I)), (V)))


#define VUSE_OPS(ANN)		get_vuse_ops (ANN)
#define STMT_VUSE_OPS(STMT)	get_vuse_ops (stmt_ann(STMT))
#define NUM_VUSES(OPS)		((OPS) ? (OPS)->num_vuses : 0)
#define VUSE_OP_PTR(OPS, I)  	get_vuse_op_ptr ((OPS), (I))
#define VUSE_OP(OPS, I)  	(USE_FROM_PTR (VUSE_OP_PTR ((OPS), (I))))
#define SET_VUSE_OP(OPS, I, V)	(SET_USE (VUSE_OP_PTR ((OPS), (I)), (V)))


#define V_MUST_DEF_OPS(ANN)		get_v_must_def_ops (ANN)
#define STMT_V_MUST_DEF_OPS(STMT)	get_v_must_def_ops (stmt_ann (STMT))
#define NUM_V_MUST_DEFS(OPS)		((OPS) ? (OPS)->num_v_must_defs : 0)
#define V_MUST_DEF_RESULT_PTR(OPS, I)	get_v_must_def_result_ptr ((OPS), (I))
#define V_MUST_DEF_RESULT(OPS, I) \
				(DEF_FROM_PTR (V_MUST_DEF_RESULT_PTR ((OPS), (I))))
#define SET_V_MUST_DEF_RESULT(OPS, I, V) \
				(SET_DEF (V_MUST_DEF_RESULT_PTR ((OPS), (I)), (V)))
#define V_MUST_DEF_KILL_PTR(OPS, I)  get_v_must_def_kill_ptr ((OPS), (I))
#define V_MUST_DEF_KILL(OPS, I) (USE_FROM_PTR (V_MUST_DEF_KILL_PTR ((OPS), (I))))
#define SET_V_MUST_DEF_KILL(OPS, I, V) (SET_USE (V_MUST_DEF_KILL_PTR ((OPS), (I)), (V)))

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


extern void init_ssa_operands (void);
extern void fini_ssa_operands (void);
extern void get_stmt_operands (tree);
extern void copy_virtual_operands (tree, tree);
extern void create_ssa_artficial_load_stmt (stmt_operands_p, tree);

extern bool ssa_call_clobbered_cache_valid;
extern bool ssa_ro_call_cache_valid;

/* This structure is used in the operand iterator loops.  It contains the 
   items required to determine which operand is retrieved next.  During
   optimization, this structure is scalarized, and any unused fields are 
   optimized away, resulting in little overhead.  */

typedef struct ssa_operand_iterator_d
{
  int num_use;
  int num_def;
  int num_vuse;
  int num_v_mayu;
  int num_v_mayd;
  int num_v_mustu;
  int num_v_mustd;
  int use_i;
  int def_i;
  int vuse_i;
  int v_mayu_i;
  int v_mayd_i;
  int v_mustu_i;
  int v_mustd_i;
  stmt_operands_p ops;
  bool done;
} ssa_op_iter;

/* These flags are used to determine which operands are returned during 
   execution of the loop.  */
#define SSA_OP_USE		0x01	/* Real USE operands.  */
#define SSA_OP_DEF		0x02	/* Real DEF operands.  */
#define SSA_OP_VUSE		0x04	/* VUSE operands.  */
#define SSA_OP_VMAYUSE		0x08	/* USE portion of V_MAY_DEFS.  */
#define SSA_OP_VMAYDEF		0x10	/* DEF portion of V_MAY_DEFS.  */
#define SSA_OP_VMUSTDEF		0x20	/* V_MUST_DEF definitions.  */
#define SSA_OP_VMUSTDEFKILL     0x40    /* V_MUST_DEF kills.  */

/* These are commonly grouped operand flags.  */
#define SSA_OP_VIRTUAL_USES	(SSA_OP_VUSE | SSA_OP_VMAYUSE)
#define SSA_OP_VIRTUAL_DEFS	(SSA_OP_VMAYDEF | SSA_OP_VMUSTDEF)
#define SSA_OP_VIRTUAL_KILLS    (SSA_OP_VMUSTDEFKILL)
#define SSA_OP_ALL_VIRTUALS     (SSA_OP_VIRTUAL_USES | SSA_OP_VIRTUAL_KILLS | SSA_OP_VIRTUAL_DEFS)
#define SSA_OP_ALL_USES		(SSA_OP_VIRTUAL_USES | SSA_OP_USE)
#define SSA_OP_ALL_DEFS		(SSA_OP_VIRTUAL_DEFS | SSA_OP_DEF)
#define SSA_OP_ALL_KILLS        (SSA_OP_VIRTUAL_KILLS)
#define SSA_OP_ALL_OPERANDS	(SSA_OP_ALL_USES | SSA_OP_ALL_DEFS | SSA_OP_ALL_KILLS)

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

/* This macro executes a loop over the V_MAY_DEF operands of STMT.  The def
   and use for each V_MAY_DEF is returned in DEFVAR and USEVAR. 
   ITER is an ssa_op_iter structure used to control the loop.  */
#define FOR_EACH_SSA_MAYDEF_OPERAND(DEFVAR, USEVAR, STMT, ITER)	\
  for (op_iter_init_maydef (&(ITER), STMT, &(USEVAR), &(DEFVAR));	\
       !op_iter_done (&(ITER));					\
       op_iter_next_maydef (&(USEVAR), &(DEFVAR), &(ITER)))

/* This macro executes a loop over the V_MUST_DEF operands of STMT.  The def
   and kill for each V_MUST_DEF is returned in DEFVAR and KILLVAR. 
   ITER is an ssa_op_iter structure used to control the loop.  */
#define FOR_EACH_SSA_MUSTDEF_OPERAND(DEFVAR, KILLVAR, STMT, ITER)	\
  for (op_iter_init_mustdef (&(ITER), STMT, &(KILLVAR), &(DEFVAR));	\
       !op_iter_done (&(ITER));					\
       op_iter_next_mustdef (&(KILLVAR), &(DEFVAR), &(ITER)))

#endif  /* GCC_TREE_SSA_OPERANDS_H  */
