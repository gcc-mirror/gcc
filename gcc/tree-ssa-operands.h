/* SSA operand management for trees.
   Copyright (C) 2003 Free Software Foundation, Inc.

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

/* This represents the MAY_DEFS for a stmt.  */
typedef struct v_may_def_optype_d GTY(())
{
  unsigned num_v_may_defs; 
  tree GTY((length ("%h.num_v_may_defs * 2"))) v_may_defs[1];
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
  tree GTY((length("%h.num_v_must_defs"))) v_must_defs[1];
} v_must_def_optype_t;

typedef v_must_def_optype_t *v_must_def_optype;

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
#define V_MUST_DEF_OP_PTR(OPS, I)	get_v_must_def_op_ptr ((OPS), (I))
#define V_MUST_DEF_OP(OPS, I)						\
				(DEF_FROM_PTR (V_MUST_DEF_OP_PTR ((OPS), (I))))
#define SET_V_MUST_DEF_OP(OPS, I, V)					\
				(SET_DEF (V_MUST_DEF_OP_PTR ((OPS), (I)), (V)))


#define PHI_RESULT_PTR(PHI)	get_phi_result_ptr (PHI)
#define PHI_RESULT(PHI)		DEF_FROM_PTR (PHI_RESULT_PTR (PHI))
#define SET_PHI_RESULT(PHI, V)	SET_DEF (PHI_RESULT_PTR (PHI), (V))

#define PHI_ARG_DEF_PTR(PHI, I)	get_phi_arg_def_ptr ((PHI), (I))
#define PHI_ARG_DEF(PHI, I)	USE_FROM_PTR (PHI_ARG_DEF_PTR ((PHI), (I)))
#define SET_PHI_ARG_DEF(PHI, I, V)					\
				SET_USE (PHI_ARG_DEF_PTR ((PHI), (I)), (V))
#define PHI_ARG_DEF_FROM_EDGE(PHI, E)					\
				PHI_ARG_DEF ((PHI),			\
					     phi_arg_from_edge ((PHI),(E)))
#define PHI_ARG_DEF_PTR_FROM_EDGE(PHI, E)				\
				PHI_ARG_DEF_PTR ((PHI), 		\
					     phi_arg_from_edge ((PHI),(E)))


extern void init_ssa_operands (void);
extern void fini_ssa_operands (void);
extern void verify_start_operands (tree);
extern void finalize_ssa_stmt_operands (tree);
void add_vuse (tree, tree);
extern void get_stmt_operands (tree);
extern void remove_vuses (tree);
extern void remove_v_may_defs (tree);
extern void remove_v_must_defs (tree);
extern void copy_virtual_operands (tree, tree);

#endif  /* GCC_TREE_SSA_OPERANDS_H  */
