/* Tree SCC value numbering
   Copyright (C) 2007-2018 Free Software Foundation, Inc.
   Contributed by Daniel Berlin <dberlin@dberlin.org>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef TREE_SSA_SCCVN_H
#define TREE_SSA_SCCVN_H

/* In tree-ssa-sccvn.c  */
bool expressions_equal_p (tree, tree);


/* TOP of the VN lattice.  */
extern tree VN_TOP;

/* A predicated value.  */
struct vn_pval
{
  vn_pval *next;
  /* The value of the expression this is attached to is RESULT in
     case the expression is computed dominated by one of the blocks
     in valid_dominated_by_p.  */
  tree result;
  unsigned n;
  int valid_dominated_by_p[1];
};

/* N-ary operations in the hashtable consist of length operands, an
   opcode, and a type.  Result is the value number of the operation,
   and hashcode is stored to avoid having to calculate it
   repeatedly.  */

typedef struct vn_nary_op_s
{
  vn_nary_op_s *next;
  vn_nary_op_s *unwind_to;
  /* Unique identify that all expressions with the same value have. */
  unsigned int value_id;
  ENUM_BITFIELD(tree_code) opcode : 16;
  unsigned length : 16;
  hashval_t hashcode;
  unsigned predicated_values : 1;
  union {
      /* If ! predicated_values this is the value of the expression.  */
      tree result;
      /* If predicated_values this is a list of values of the expression.  */
      vn_pval *values;
  } u;
  tree type;
  tree op[1];
} *vn_nary_op_t;
typedef const struct vn_nary_op_s *const_vn_nary_op_t;

/* Return the size of a vn_nary_op_t with LENGTH operands.  */

static inline size_t
sizeof_vn_nary_op (unsigned int length)
{
  return sizeof (struct vn_nary_op_s) + sizeof (tree) * length - sizeof (tree);
}

/* Phi nodes in the hashtable consist of their non-VN_TOP phi
   arguments, and the basic block the phi is in. Result is the value
   number of the operation, and hashcode is stored to avoid having to
   calculate it repeatedly.  Phi nodes not in the same block are never
   considered equivalent.  */

typedef struct vn_phi_s
{
  vn_phi_s *next;
  /* Unique identifier that all expressions with the same value have. */
  unsigned int value_id;
  hashval_t hashcode;
  basic_block block;
  /* Controlling condition lhs/rhs.  */
  tree cclhs;
  tree ccrhs;
  tree type;
  tree result;
  /* The number of args is determined by EDGE_COUT (block->preds).  */
  tree phiargs[1];
} *vn_phi_t;
typedef const struct vn_phi_s *const_vn_phi_t;

/* Reference operands only exist in reference operations structures.
   They consist of an opcode, type, and some number of operands.  For
   a given opcode, some, all, or none of the operands may be used.
   The operands are there to store the information that makes up the
   portion of the addressing calculation that opcode performs.  */

typedef struct vn_reference_op_struct
{
  ENUM_BITFIELD(tree_code) opcode : 16;
  /* Dependence info, used for [TARGET_]MEM_REF only.  */
  unsigned short clique;
  unsigned short base;
  unsigned reverse : 1;
  /* For storing TYPE_ALIGN for array ref element size computation.  */
  unsigned align : 6;
  /* Constant offset this op adds or -1 if it is variable.  */
  poly_int64_pod off;
  tree type;
  tree op0;
  tree op1;
  tree op2;
} vn_reference_op_s;
typedef vn_reference_op_s *vn_reference_op_t;
typedef const vn_reference_op_s *const_vn_reference_op_t;

inline unsigned
vn_ref_op_align_unit (vn_reference_op_t op)
{
  return op->align ? ((unsigned)1 << (op->align - 1)) / BITS_PER_UNIT : 0;
}

/* A reference operation in the hashtable is representation as
   the vuse, representing the memory state at the time of
   the operation, and a collection of operands that make up the
   addressing calculation.  If two vn_reference_t's have the same set
   of operands, they access the same memory location. We also store
   the resulting value number, and the hashcode.  */

typedef struct vn_reference_s
{
  vn_reference_s *next;
  /* Unique identifier that all expressions with the same value have. */
  unsigned int value_id;
  hashval_t hashcode;
  tree vuse;
  alias_set_type set;
  tree type;
  vec<vn_reference_op_s> operands;
  tree result;
  tree result_vdef;
} *vn_reference_t;
typedef const struct vn_reference_s *const_vn_reference_t;

typedef struct vn_constant_s
{
  unsigned int value_id;
  hashval_t hashcode;
  tree constant;
} *vn_constant_t;

enum vn_kind { VN_NONE, VN_CONSTANT, VN_NARY, VN_REFERENCE, VN_PHI };
enum vn_kind vn_get_stmt_kind (gimple *);

/* Hash the type TYPE using bits that distinguishes it in the
   types_compatible_p sense.  */

static inline hashval_t
vn_hash_type (tree type)
{
  return (INTEGRAL_TYPE_P (type)
	  + (INTEGRAL_TYPE_P (type)
	     ? TYPE_PRECISION (type) + TYPE_UNSIGNED (type) : 0));
}

/* Hash the constant CONSTANT with distinguishing type incompatible
   constants in the types_compatible_p sense.  */

static inline hashval_t
vn_hash_constant_with_type (tree constant)
{
  inchash::hash hstate;
  inchash::add_expr (constant, hstate);
  hstate.merge_hash (vn_hash_type (TREE_TYPE (constant)));
  return hstate.end ();
}

/* Compare the constants C1 and C2 with distinguishing type incompatible
   constants in the types_compatible_p sense.  */

static inline bool
vn_constant_eq_with_type (tree c1, tree c2)
{
  return (expressions_equal_p (c1, c2)
	  && types_compatible_p (TREE_TYPE (c1), TREE_TYPE (c2)));
}

typedef struct vn_ssa_aux
{
  /* SSA name this vn_ssa_aux is associated with in the lattice.  */
  tree name;
  /* Value number. This may be an SSA name or a constant.  */
  tree valnum;
  /* Statements to insert if needs_insertion is true.  */
  gimple_seq expr;

  /* Unique identifier that all expressions with the same value have. */
  unsigned int value_id;

  /* Whether the SSA_NAME has been processed at least once.  */
  unsigned visited : 1;

  /* Whether the SSA_NAME has no defining statement and thus an
     insertion of such with EXPR as definition is required before
     a use can be created of it.  */
  unsigned needs_insertion : 1;
} *vn_ssa_aux_t;

enum vn_lookup_kind { VN_NOWALK, VN_WALK, VN_WALKREWRITE };

/* Return the value numbering info for an SSA_NAME.  */
bool has_VN_INFO (tree);
extern vn_ssa_aux_t VN_INFO (tree);
tree vn_get_expr_for (tree);
void scc_vn_restore_ssa_info (void);
tree vn_nary_op_lookup (tree, vn_nary_op_t *);
tree vn_nary_op_lookup_stmt (gimple *, vn_nary_op_t *);
tree vn_nary_op_lookup_pieces (unsigned int, enum tree_code,
			       tree, tree *, vn_nary_op_t *);
vn_nary_op_t vn_nary_op_insert (tree, tree);
vn_nary_op_t vn_nary_op_insert_pieces (unsigned int, enum tree_code,
				       tree, tree *, tree, unsigned int);
bool ao_ref_init_from_vn_reference (ao_ref *, alias_set_type, tree,
				    vec<vn_reference_op_s> );
vec<vn_reference_op_s> vn_reference_operands_for_lookup (tree);
tree vn_reference_lookup_pieces (tree, alias_set_type, tree,
				 vec<vn_reference_op_s> ,
				 vn_reference_t *, vn_lookup_kind);
tree vn_reference_lookup (tree, tree, vn_lookup_kind, vn_reference_t *, bool);
void vn_reference_lookup_call (gcall *, vn_reference_t *, vn_reference_t);
vn_reference_t vn_reference_insert_pieces (tree, alias_set_type, tree,
					   vec<vn_reference_op_s> ,
					   tree, unsigned int);

bool vn_nary_op_eq (const_vn_nary_op_t const vno1,
		    const_vn_nary_op_t const vno2);
bool vn_nary_may_trap (vn_nary_op_t);
bool vn_reference_eq (const_vn_reference_t const, const_vn_reference_t const);
unsigned int get_max_value_id (void);
unsigned int get_next_value_id (void);
unsigned int get_constant_value_id (tree);
unsigned int get_or_alloc_constant_value_id (tree);
bool value_id_constant_p (unsigned int);
tree fully_constant_vn_reference_p (vn_reference_t);
tree vn_nary_simplify (vn_nary_op_t);

unsigned do_rpo_vn (function *, edge, bitmap);
void run_rpo_vn (vn_lookup_kind);
unsigned eliminate_with_rpo_vn (bitmap);
void free_rpo_vn (void);

/* Valueize NAME if it is an SSA name, otherwise just return it.  This hook
   is initialized by run_scc_vn.  */
extern tree (*vn_valueize) (tree);

/* Context that valueization should operate on.  */
extern basic_block vn_context_bb;


#endif /* TREE_SSA_SCCVN_H  */
