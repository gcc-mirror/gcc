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

typedef struct def_optype_d GTY(())
{
  unsigned num_defs; 
  tree * GTY((length("%h.num_defs"), skip(""))) defs[1];
} def_optype_t;

typedef def_optype_t *def_optype;

typedef struct use_optype_d GTY(())
{
  unsigned num_uses; 
  tree * GTY((length("%h.num_uses"), skip(""))) uses[1];
} use_optype_t;

typedef use_optype_t *use_optype;

typedef struct vdef_optype_d GTY(())
{
  unsigned num_vdefs; 
  tree GTY((length ("%h.num_vdefs * 2"))) vdefs[1];
} vdef_optype_t;

typedef vdef_optype_t *vdef_optype;

typedef struct vuse_optype_d GTY(()) 
{
  unsigned num_vuses; 
  tree GTY((length ("%h.num_vuses"))) vuses[1];
} vuse_optype_t;

typedef vuse_optype_t *vuse_optype;

#define USE_OPS(ANN)		get_use_ops (ANN)
#define STMT_USE_OPS(STMT)	get_use_ops (stmt_ann (STMT))
#define NUM_USES(OPS)		((OPS) ? (OPS)->num_uses : 0)
#define USE_OP_PTR(OPS, I)	get_use_op_ptr ((OPS), (I))
#define USE_OP(OPS, I)		(*(USE_OP_PTR ((OPS), (I))))


#define DEF_OPS(ANN)		get_def_ops (ANN)
#define STMT_DEF_OPS(STMT)	get_def_ops (stmt_ann (STMT))
#define NUM_DEFS(OPS)		((OPS) ? (OPS)->num_defs : 0)
#define DEF_OP_PTR(OPS, I)	get_def_op_ptr ((OPS), (I))
#define DEF_OP(OPS, I)		(*(DEF_OP_PTR ((OPS), (I))))


#define VDEF_OPS(ANN)		get_vdef_ops (ANN)
#define STMT_VDEF_OPS(STMT)	get_vdef_ops (stmt_ann(STMT))
#define NUM_VDEFS(OPS)		((OPS) ? (OPS)->num_vdefs : 0)
#define VDEF_RESULT_PTR(OPS, I)	get_vdef_result_ptr ((OPS), (I))
#define VDEF_RESULT(OPS, I)	(*(VDEF_RESULT_PTR ((OPS), (I))))
#define VDEF_OP_PTR(OPS, I)	get_vdef_op_ptr ((OPS), (I))
#define VDEF_OP(OPS, I)		(*(VDEF_OP_PTR ((OPS), (I))))


#define VUSE_OPS(ANN)		get_vuse_ops (ANN)
#define STMT_VUSE_OPS(STMT)	get_vuse_ops (stmt_ann(STMT))
#define NUM_VUSES(OPS)		((OPS) ? (OPS)->num_vuses : 0)
#define VUSE_OP_PTR(OPS, I)  	get_vuse_op_ptr ((OPS), (I))
#define VUSE_OP(OPS, I)  	(*(VUSE_OP_PTR ((OPS), (I))))


extern void init_ssa_operands (void);
extern void fini_ssa_operands (void);
extern void verify_start_operands (tree);
extern void finalize_ssa_stmt_operands (tree);
void add_vuse (tree, tree);
extern void get_stmt_operands (tree);
extern void remove_vuses (tree);
extern void remove_vdefs (tree);

#endif  /* GCC_TREE_SSA_OPERANDS_H  */
