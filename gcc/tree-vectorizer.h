/* Vectorizer
   Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Dorit Naishlos <dorit@il.ibm.com>

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

#ifndef GCC_TREE_VECTORIZER_H
#define GCC_TREE_VECTORIZER_H

#include "tree-data-ref.h"

typedef source_location LOC;
#define UNKNOWN_LOC UNKNOWN_LOCATION
#define EXPR_LOC(e) EXPR_LOCATION(e)
#define LOC_FILE(l) LOCATION_FILE (l)
#define LOC_LINE(l) LOCATION_LINE (l)

/* Used for naming of new temporaries.  */
enum vect_var_kind {
  vect_simple_var,
  vect_pointer_var,
  vect_scalar_var
};

/* Defines type of operation.  */
enum operation_type {
  unary_op = 1,
  binary_op,
  ternary_op
};

/* Define type of available alignment support.  */
enum dr_alignment_support {
  dr_unaligned_unsupported,
  dr_unaligned_supported,
  dr_explicit_realign,
  dr_explicit_realign_optimized,
  dr_aligned
};

/* Define type of def-use cross-iteration cycle.  */
enum vect_def_type {
  vect_uninitialized_def = 0,
  vect_constant_def = 1,
  vect_external_def,
  vect_internal_def,
  vect_induction_def,
  vect_reduction_def,
  vect_double_reduction_def,
  vect_nested_cycle,
  vect_unknown_def_type
};

#define VECTORIZABLE_CYCLE_DEF(D) (((D) == vect_reduction_def)           \
                                   || ((D) == vect_double_reduction_def) \
                                   || ((D) == vect_nested_cycle))

/************************************************************************
  SLP
 ************************************************************************/

/* A computation tree of an SLP instance. Each node corresponds to a group of
   stmts to be packed in a SIMD stmt.  */
typedef struct _slp_tree {
  /* Only binary and unary operations are supported. LEFT child corresponds to
     the first operand and RIGHT child to the second if the operation is
     binary.  */
  struct _slp_tree *left;
  struct _slp_tree *right;
  /* A group of scalar stmts to be vectorized together.  */
  VEC (gimple, heap) *stmts;
  /* Vectorized stmt/s.  */
  VEC (gimple, heap) *vec_stmts;
  /* Number of vector stmts that are created to replace the group of scalar
     stmts. It is calculated during the transformation phase as the number of
     scalar elements in one scalar iteration (GROUP_SIZE) multiplied by VF
     divided by vector size.  */
  unsigned int vec_stmts_size;
  /* Vectorization costs associated with SLP node.  */
  struct
  {
    int outside_of_loop;     /* Statements generated outside loop.  */
    int inside_of_loop;      /* Statements generated inside loop.  */
  } cost;
} *slp_tree;

DEF_VEC_P(slp_tree);
DEF_VEC_ALLOC_P(slp_tree, heap);

/* SLP instance is a sequence of stmts in a loop that can be packed into
   SIMD stmts.  */
typedef struct _slp_instance {
  /* The root of SLP tree.  */
  slp_tree root;

  /* Size of groups of scalar stmts that will be replaced by SIMD stmt/s.  */
  unsigned int group_size;

  /* The unrolling factor required to vectorized this SLP instance.  */
  unsigned int unrolling_factor;

  /* Vectorization costs associated with SLP instance.  */
  struct
  {
    int outside_of_loop;     /* Statements generated outside loop.  */
    int inside_of_loop;      /* Statements generated inside loop.  */
  } cost;

  /* Loads permutation relatively to the stores, NULL if there is no
     permutation.  */
  VEC (int, heap) *load_permutation;

  /* The group of nodes that contain loads of this SLP instance.  */
  VEC (slp_tree, heap) *loads;

  /* The first scalar load of the instance. The created vector loads will be
     inserted before this statement.  */
  gimple first_load;
} *slp_instance;

DEF_VEC_P(slp_instance);
DEF_VEC_ALLOC_P(slp_instance, heap);

/* Access Functions.  */
#define SLP_INSTANCE_TREE(S)                     (S)->root
#define SLP_INSTANCE_GROUP_SIZE(S)               (S)->group_size
#define SLP_INSTANCE_UNROLLING_FACTOR(S)         (S)->unrolling_factor
#define SLP_INSTANCE_OUTSIDE_OF_LOOP_COST(S)     (S)->cost.outside_of_loop
#define SLP_INSTANCE_INSIDE_OF_LOOP_COST(S)      (S)->cost.inside_of_loop
#define SLP_INSTANCE_LOAD_PERMUTATION(S)         (S)->load_permutation
#define SLP_INSTANCE_LOADS(S)                    (S)->loads
#define SLP_INSTANCE_FIRST_LOAD_STMT(S)          (S)->first_load

#define SLP_TREE_LEFT(S)                         (S)->left
#define SLP_TREE_RIGHT(S)                        (S)->right
#define SLP_TREE_SCALAR_STMTS(S)                 (S)->stmts
#define SLP_TREE_VEC_STMTS(S)                    (S)->vec_stmts
#define SLP_TREE_NUMBER_OF_VEC_STMTS(S)          (S)->vec_stmts_size
#define SLP_TREE_OUTSIDE_OF_LOOP_COST(S)         (S)->cost.outside_of_loop
#define SLP_TREE_INSIDE_OF_LOOP_COST(S)          (S)->cost.inside_of_loop


typedef struct _vect_peel_info
{
  int npeel;
  struct data_reference *dr;
  unsigned int count;
} *vect_peel_info;

typedef struct _vect_peel_extended_info
{
  struct _vect_peel_info peel_info;
  unsigned int inside_cost;
  unsigned int outside_cost;
} *vect_peel_extended_info;

/*-----------------------------------------------------------------*/
/* Info on vectorized loops.                                       */
/*-----------------------------------------------------------------*/
typedef struct _loop_vec_info {

  /* The loop to which this info struct refers to.  */
  struct loop *loop;

  /* The loop basic blocks.  */
  basic_block *bbs;

  /* Number of iterations.  */
  tree num_iters;
  tree num_iters_unchanged;

  /* Minimum number of iterations below which vectorization is expected to
     not be profitable (as estimated by the cost model).
     -1 indicates that vectorization will not be profitable.
     FORNOW: This field is an int. Will be a tree in the future, to represent
	     values unknown at compile time.  */
  int min_profitable_iters;

  /* Is the loop vectorizable? */
  bool vectorizable;

  /* Unrolling factor  */
  int vectorization_factor;

  /* The loop location in the source.  */
  LOC loop_line_number;

  /* Unknown DRs according to which loop was peeled.  */
  struct data_reference *unaligned_dr;

  /* peeling_for_alignment indicates whether peeling for alignment will take
     place, and what the peeling factor should be:
     peeling_for_alignment = X means:
        If X=0: Peeling for alignment will not be applied.
        If X>0: Peel first X iterations.
        If X=-1: Generate a runtime test to calculate the number of iterations
                 to be peeled, using the dataref recorded in the field
                 unaligned_dr.  */
  int peeling_for_alignment;

  /* The mask used to check the alignment of pointers or arrays.  */
  int ptr_mask;

  /* The loop nest in which the data dependences are computed.  */
  VEC (loop_p, heap) *loop_nest;

  /* All data references in the loop.  */
  VEC (data_reference_p, heap) *datarefs;

  /* All data dependences in the loop.  */
  VEC (ddr_p, heap) *ddrs;

  /* Data Dependence Relations defining address ranges that are candidates
     for a run-time aliasing check.  */
  VEC (ddr_p, heap) *may_alias_ddrs;

  /* Statements in the loop that have data references that are candidates for a
     runtime (loop versioning) misalignment check.  */
  VEC(gimple,heap) *may_misalign_stmts;

  /* All interleaving chains of stores in the loop, represented by the first
     stmt in the chain.  */
  VEC(gimple, heap) *strided_stores;

  /* All SLP instances in the loop. This is a subset of the set of STRIDED_STORES
     of the loop.  */
  VEC(slp_instance, heap) *slp_instances;

  /* The unrolling factor needed to SLP the loop. In case of that pure SLP is
     applied to the loop, i.e., no unrolling is needed, this is 1.  */
  unsigned slp_unrolling_factor;

  /* Reduction cycles detected in the loop. Used in loop-aware SLP.  */
  VEC (gimple, heap) *reductions;

  /* Hash table used to choose the best peeling option.  */
  htab_t peeling_htab;

  /* When we have strided data accesses with gaps, we may introduce invalid
     memory accesses.  We peel the last iteration of the loop to prevent
     this.  */
  bool peeling_for_gaps;

} *loop_vec_info;

/* Access Functions.  */
#define LOOP_VINFO_LOOP(L)                 (L)->loop
#define LOOP_VINFO_BBS(L)                  (L)->bbs
#define LOOP_VINFO_NITERS(L)               (L)->num_iters
/* Since LOOP_VINFO_NITERS can change after prologue peeling
   retain total unchanged scalar loop iterations for cost model.  */
#define LOOP_VINFO_NITERS_UNCHANGED(L)     (L)->num_iters_unchanged
#define LOOP_VINFO_COST_MODEL_MIN_ITERS(L) (L)->min_profitable_iters
#define LOOP_VINFO_VECTORIZABLE_P(L)       (L)->vectorizable
#define LOOP_VINFO_VECT_FACTOR(L)          (L)->vectorization_factor
#define LOOP_VINFO_PTR_MASK(L)             (L)->ptr_mask
#define LOOP_VINFO_LOOP_NEST(L)            (L)->loop_nest
#define LOOP_VINFO_DATAREFS(L)             (L)->datarefs
#define LOOP_VINFO_DDRS(L)                 (L)->ddrs
#define LOOP_VINFO_INT_NITERS(L)           (TREE_INT_CST_LOW ((L)->num_iters))
#define LOOP_PEELING_FOR_ALIGNMENT(L)      (L)->peeling_for_alignment
#define LOOP_VINFO_UNALIGNED_DR(L)         (L)->unaligned_dr
#define LOOP_VINFO_MAY_MISALIGN_STMTS(L)   (L)->may_misalign_stmts
#define LOOP_VINFO_LOC(L)                  (L)->loop_line_number
#define LOOP_VINFO_MAY_ALIAS_DDRS(L)       (L)->may_alias_ddrs
#define LOOP_VINFO_STRIDED_STORES(L)       (L)->strided_stores
#define LOOP_VINFO_SLP_INSTANCES(L)        (L)->slp_instances
#define LOOP_VINFO_SLP_UNROLLING_FACTOR(L) (L)->slp_unrolling_factor
#define LOOP_VINFO_REDUCTIONS(L)           (L)->reductions
#define LOOP_VINFO_PEELING_HTAB(L)         (L)->peeling_htab
#define LOOP_VINFO_PEELING_FOR_GAPS(L)     (L)->peeling_for_gaps

#define LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT(L) \
VEC_length (gimple, (L)->may_misalign_stmts) > 0
#define LOOP_REQUIRES_VERSIONING_FOR_ALIAS(L)     \
VEC_length (ddr_p, (L)->may_alias_ddrs) > 0

#define NITERS_KNOWN_P(n)                     \
(host_integerp ((n),0)                        \
&& TREE_INT_CST_LOW ((n)) > 0)

#define LOOP_VINFO_NITERS_KNOWN_P(L)          \
NITERS_KNOWN_P((L)->num_iters)

static inline loop_vec_info
loop_vec_info_for_loop (struct loop *loop)
{
  return (loop_vec_info) loop->aux;
}

static inline bool
nested_in_vect_loop_p (struct loop *loop, gimple stmt)
{
  return (loop->inner
          && (loop->inner == (gimple_bb (stmt))->loop_father));
}

typedef struct _bb_vec_info {

  basic_block bb;
  /* All interleaving chains of stores in the basic block, represented by the
     first stmt in the chain.  */
  VEC(gimple, heap) *strided_stores;

  /* All SLP instances in the basic block. This is a subset of the set of
     STRIDED_STORES of the basic block.  */
  VEC(slp_instance, heap) *slp_instances;

  /* All data references in the basic block.  */
  VEC (data_reference_p, heap) *datarefs;

  /* All data dependences in the basic block.  */
  VEC (ddr_p, heap) *ddrs;
} *bb_vec_info;

#define BB_VINFO_BB(B)              (B)->bb
#define BB_VINFO_STRIDED_STORES(B)  (B)->strided_stores
#define BB_VINFO_SLP_INSTANCES(B)   (B)->slp_instances
#define BB_VINFO_DATAREFS(B)        (B)->datarefs
#define BB_VINFO_DDRS(B)            (B)->ddrs

static inline bb_vec_info
vec_info_for_bb (basic_block bb)
{
  return (bb_vec_info) bb->aux;
}

/*-----------------------------------------------------------------*/
/* Info on vectorized defs.                                        */
/*-----------------------------------------------------------------*/
enum stmt_vec_info_type {
  undef_vec_info_type = 0,
  load_vec_info_type,
  store_vec_info_type,
  shift_vec_info_type,
  op_vec_info_type,
  call_vec_info_type,
  assignment_vec_info_type,
  condition_vec_info_type,
  reduc_vec_info_type,
  induc_vec_info_type,
  type_promotion_vec_info_type,
  type_demotion_vec_info_type,
  type_conversion_vec_info_type,
  loop_exit_ctrl_vec_info_type
};

/* Indicates whether/how a variable is used in the scope of loop/basic
   block.  */
enum vect_relevant {
  vect_unused_in_scope = 0,
  /* The def is in the inner loop, and the use is in the outer loop, and the
     use is a reduction stmt.  */
  vect_used_in_outer_by_reduction,
  /* The def is in the inner loop, and the use is in the outer loop (and is
     not part of reduction).  */
  vect_used_in_outer,

  /* defs that feed computations that end up (only) in a reduction. These
     defs may be used by non-reduction stmts, but eventually, any
     computations/values that are affected by these defs are used to compute
     a reduction (i.e. don't get stored to memory, for example). We use this
     to identify computations that we can change the order in which they are
     computed.  */
  vect_used_by_reduction,

  vect_used_in_scope
};

/* The type of vectorization that can be applied to the stmt: regular loop-based
   vectorization; pure SLP - the stmt is a part of SLP instances and does not
   have uses outside SLP instances; or hybrid SLP and loop-based - the stmt is
   a part of SLP instance and also must be loop-based vectorized, since it has
   uses outside SLP sequences.

   In the loop context the meanings of pure and hybrid SLP are slightly
   different. By saying that pure SLP is applied to the loop, we mean that we
   exploit only intra-iteration parallelism in the loop; i.e., the loop can be
   vectorized without doing any conceptual unrolling, cause we don't pack
   together stmts from different iterations, only within a single iteration.
   Loop hybrid SLP means that we exploit both intra-iteration and
   inter-iteration parallelism (e.g., number of elements in the vector is 4
   and the slp-group-size is 2, in which case we don't have enough parallelism
   within an iteration, so we obtain the rest of the parallelism from subsequent
   iterations by unrolling the loop by 2).  */
enum slp_vect_type {
  loop_vect = 0,
  pure_slp,
  hybrid
};


typedef struct data_reference *dr_p;
DEF_VEC_P(dr_p);
DEF_VEC_ALLOC_P(dr_p,heap);

typedef struct _stmt_vec_info {

  enum stmt_vec_info_type type;

  /* Indicates whether this stmts is part of a computation whose result is
     used outside the loop.  */
  bool live;

  /* Stmt is part of some pattern (computation idiom)  */
  bool in_pattern_p;

  /* For loads only, if there is a store with the same location, this field is
     TRUE.  */
  bool read_write_dep;

  /* The stmt to which this info struct refers to.  */
  gimple stmt;

  /* The loop_vec_info with respect to which STMT is vectorized.  */
  loop_vec_info loop_vinfo;

  /* The vector type to be used for the LHS of this statement.  */
  tree vectype;

  /* The vectorized version of the stmt.  */
  gimple vectorized_stmt;


  /** The following is relevant only for stmts that contain a non-scalar
     data-ref (array/pointer/struct access). A GIMPLE stmt is expected to have
     at most one such data-ref.  **/

  /* Information about the data-ref (access function, etc),
     relative to the inner-most containing loop.  */
  struct data_reference *data_ref_info;

  /* Information about the data-ref relative to this loop
     nest (the loop that is being considered for vectorization).  */
  tree dr_base_address;
  tree dr_init;
  tree dr_offset;
  tree dr_step;
  tree dr_aligned_to;

  /* Used for various bookkeeping purposes, generally holding a pointer to
     some other stmt S that is in some way "related" to this stmt.
     Current use of this field is:
        If this stmt is part of a pattern (i.e. the field 'in_pattern_p' is
        true): S is the "pattern stmt" that represents (and replaces) the
        sequence of stmts that constitutes the pattern.  Similarly, the
        related_stmt of the "pattern stmt" points back to this stmt (which is
        the last stmt in the original sequence of stmts that constitutes the
        pattern).  */
  gimple related_stmt;

  /* List of datarefs that are known to have the same alignment as the dataref
     of this stmt.  */
  VEC(dr_p,heap) *same_align_refs;

  /* Classify the def of this stmt.  */
  enum vect_def_type def_type;

  /*  Whether the stmt is SLPed, loop-based vectorized, or both.  */
  enum slp_vect_type slp_type;

  /* Interleaving info.  */
  /* First data-ref in the interleaving group.  */
  gimple first_dr;
  /* Pointer to the next data-ref in the group.  */
  gimple next_dr;
  /* In case that two or more stmts share data-ref, this is the pointer to the
     previously detected stmt with the same dr.  */
  gimple same_dr_stmt;
  /* The size of the interleaving group.  */
  unsigned int size;
  /* For stores, number of stores from this group seen. We vectorize the last
     one.  */
  unsigned int store_count;
  /* For loads only, the gap from the previous load. For consecutive loads, GAP
     is 1.  */
  unsigned int gap;

  /* Not all stmts in the loop need to be vectorized. e.g, the increment
     of the loop induction variable and computation of array indexes. relevant
     indicates whether the stmt needs to be vectorized.  */
  enum vect_relevant relevant;

  /* Vectorization costs associated with statement.  */
  struct
  {
    int outside_of_loop;     /* Statements generated outside loop.  */
    int inside_of_loop;      /* Statements generated inside loop.  */
  } cost;

  /* The bb_vec_info with respect to which STMT is vectorized.  */
  bb_vec_info bb_vinfo;

  /* Is this statement vectorizable or should it be skipped in (partial)
     vectorization.  */
  bool vectorizable;
} *stmt_vec_info;

/* Access Functions.  */
#define STMT_VINFO_TYPE(S)                 (S)->type
#define STMT_VINFO_STMT(S)                 (S)->stmt
#define STMT_VINFO_LOOP_VINFO(S)           (S)->loop_vinfo
#define STMT_VINFO_BB_VINFO(S)             (S)->bb_vinfo
#define STMT_VINFO_RELEVANT(S)             (S)->relevant
#define STMT_VINFO_LIVE_P(S)               (S)->live
#define STMT_VINFO_VECTYPE(S)              (S)->vectype
#define STMT_VINFO_VEC_STMT(S)             (S)->vectorized_stmt
#define STMT_VINFO_VECTORIZABLE(S)         (S)->vectorizable
#define STMT_VINFO_DATA_REF(S)             (S)->data_ref_info

#define STMT_VINFO_DR_BASE_ADDRESS(S)      (S)->dr_base_address
#define STMT_VINFO_DR_INIT(S)              (S)->dr_init
#define STMT_VINFO_DR_OFFSET(S)            (S)->dr_offset
#define STMT_VINFO_DR_STEP(S)              (S)->dr_step
#define STMT_VINFO_DR_ALIGNED_TO(S)        (S)->dr_aligned_to

#define STMT_VINFO_IN_PATTERN_P(S)         (S)->in_pattern_p
#define STMT_VINFO_RELATED_STMT(S)         (S)->related_stmt
#define STMT_VINFO_SAME_ALIGN_REFS(S)      (S)->same_align_refs
#define STMT_VINFO_DEF_TYPE(S)             (S)->def_type
#define STMT_VINFO_DR_GROUP_FIRST_DR(S)    (S)->first_dr
#define STMT_VINFO_DR_GROUP_NEXT_DR(S)     (S)->next_dr
#define STMT_VINFO_DR_GROUP_SIZE(S)        (S)->size
#define STMT_VINFO_DR_GROUP_STORE_COUNT(S) (S)->store_count
#define STMT_VINFO_DR_GROUP_GAP(S)         (S)->gap
#define STMT_VINFO_DR_GROUP_SAME_DR_STMT(S)(S)->same_dr_stmt
#define STMT_VINFO_DR_GROUP_READ_WRITE_DEPENDENCE(S)  (S)->read_write_dep
#define STMT_VINFO_STRIDED_ACCESS(S)      ((S)->first_dr != NULL)

#define DR_GROUP_FIRST_DR(S)               (S)->first_dr
#define DR_GROUP_NEXT_DR(S)                (S)->next_dr
#define DR_GROUP_SIZE(S)                   (S)->size
#define DR_GROUP_STORE_COUNT(S)            (S)->store_count
#define DR_GROUP_GAP(S)                    (S)->gap
#define DR_GROUP_SAME_DR_STMT(S)           (S)->same_dr_stmt
#define DR_GROUP_READ_WRITE_DEPENDENCE(S)  (S)->read_write_dep

#define STMT_VINFO_RELEVANT_P(S)          ((S)->relevant != vect_unused_in_scope)
#define STMT_VINFO_OUTSIDE_OF_LOOP_COST(S) (S)->cost.outside_of_loop
#define STMT_VINFO_INSIDE_OF_LOOP_COST(S)  (S)->cost.inside_of_loop

#define HYBRID_SLP_STMT(S)                ((S)->slp_type == hybrid)
#define PURE_SLP_STMT(S)                  ((S)->slp_type == pure_slp)
#define STMT_SLP_TYPE(S)                   (S)->slp_type

#define VECT_MAX_COST 1000

/* The maximum number of intermediate steps required in multi-step type
   conversion.  */
#define MAX_INTERM_CVT_STEPS         3

/* The maximum vectorization factor supported by any target (V32QI).  */
#define MAX_VECTORIZATION_FACTOR 32

/* Avoid GTY(()) on stmt_vec_info.  */
typedef void *vec_void_p;
DEF_VEC_P (vec_void_p);
DEF_VEC_ALLOC_P (vec_void_p, heap);

extern VEC(vec_void_p,heap) *stmt_vec_info_vec;

void init_stmt_vec_info_vec (void);
void free_stmt_vec_info_vec (void);

/* Return a stmt_vec_info corresponding to STMT.  */

static inline stmt_vec_info
vinfo_for_stmt (gimple stmt)
{
  unsigned int uid = gimple_uid (stmt);
  if (uid == 0)
    return NULL;

  return (stmt_vec_info) VEC_index (vec_void_p, stmt_vec_info_vec, uid - 1);
}

/* Set vectorizer information INFO for STMT.  */

static inline void
set_vinfo_for_stmt (gimple stmt, stmt_vec_info info)
{
  unsigned int uid = gimple_uid (stmt);
  if (uid == 0)
    {
      gcc_checking_assert (info);
      uid = VEC_length (vec_void_p, stmt_vec_info_vec) + 1;
      gimple_set_uid (stmt, uid);
      VEC_safe_push (vec_void_p, heap, stmt_vec_info_vec, (vec_void_p) info);
    }
  else
    VEC_replace (vec_void_p, stmt_vec_info_vec, uid - 1, (vec_void_p) info);
}

/* Return the earlier statement between STMT1 and STMT2.  */

static inline gimple
get_earlier_stmt (gimple stmt1, gimple stmt2)
{
  unsigned int uid1, uid2;

  if (stmt1 == NULL)
    return stmt2;

  if (stmt2 == NULL)
    return stmt1;

  uid1 = gimple_uid (stmt1);
  uid2 = gimple_uid (stmt2);

  if (uid1 == 0 || uid2 == 0)
    return NULL;

  gcc_checking_assert (uid1 <= VEC_length (vec_void_p, stmt_vec_info_vec)
		       && uid2 <= VEC_length (vec_void_p, stmt_vec_info_vec));

  if (uid1 < uid2)
    return stmt1;
  else
    return stmt2;
}

/* Return the later statement between STMT1 and STMT2.  */

static inline gimple
get_later_stmt (gimple stmt1, gimple stmt2)
{
  unsigned int uid1, uid2;

  if (stmt1 == NULL)
    return stmt2;

  if (stmt2 == NULL)
    return stmt1;

  uid1 = gimple_uid (stmt1);
  uid2 = gimple_uid (stmt2);

  if (uid1 == 0 || uid2 == 0)
    return NULL;

  gcc_assert (uid1 <= VEC_length (vec_void_p, stmt_vec_info_vec));
  gcc_assert (uid2 <= VEC_length (vec_void_p, stmt_vec_info_vec));

  if (uid1 > uid2)
    return stmt1;
  else
    return stmt2;
}

/* Return TRUE if a statement represented by STMT_INFO is a part of a
   pattern.  */

static inline bool
is_pattern_stmt_p (stmt_vec_info stmt_info)
{
  gimple related_stmt;
  stmt_vec_info related_stmt_info;

  related_stmt = STMT_VINFO_RELATED_STMT (stmt_info);
  if (related_stmt
      && (related_stmt_info = vinfo_for_stmt (related_stmt))
      && STMT_VINFO_IN_PATTERN_P (related_stmt_info))
    return true;

  return false;
}

/* Return true if BB is a loop header.  */

static inline bool
is_loop_header_bb_p (basic_block bb)
{
  if (bb == (bb->loop_father)->header)
    return true;
  gcc_checking_assert (EDGE_COUNT (bb->preds) == 1);
  return false;
}

/* Set inside loop vectorization cost.  */

static inline void
stmt_vinfo_set_inside_of_loop_cost (stmt_vec_info stmt_info, slp_tree slp_node,
				    int cost)
{
  if (slp_node)
    SLP_TREE_INSIDE_OF_LOOP_COST (slp_node) = cost;
  else
    STMT_VINFO_INSIDE_OF_LOOP_COST (stmt_info) = cost;
}

/* Set inside loop vectorization cost.  */

static inline void
stmt_vinfo_set_outside_of_loop_cost (stmt_vec_info stmt_info, slp_tree slp_node,
				     int cost)
{
  if (slp_node)
    SLP_TREE_OUTSIDE_OF_LOOP_COST (slp_node) = cost;
  else
    STMT_VINFO_OUTSIDE_OF_LOOP_COST (stmt_info) = cost;
}

/* Return pow2 (X).  */

static inline int
vect_pow2 (int x)
{
  int i, res = 1;

  for (i = 0; i < x; i++)
    res *= 2;

  return res;
}

/*-----------------------------------------------------------------*/
/* Info on data references alignment.                              */
/*-----------------------------------------------------------------*/

/* Reflects actual alignment of first access in the vectorized loop,
   taking into account peeling/versioning if applied.  */
#define DR_MISALIGNMENT(DR)   ((int) (size_t) (DR)->aux)
#define SET_DR_MISALIGNMENT(DR, VAL)   ((DR)->aux = (void *) (size_t) (VAL))

/* Return TRUE if the data access is aligned, and FALSE otherwise.  */

static inline bool
aligned_access_p (struct data_reference *data_ref_info)
{
  return (DR_MISALIGNMENT (data_ref_info) == 0);
}

/* Return TRUE if the alignment of the data access is known, and FALSE
   otherwise.  */

static inline bool
known_alignment_for_access_p (struct data_reference *data_ref_info)
{
  return (DR_MISALIGNMENT (data_ref_info) != -1);
}

/* vect_dump will be set to stderr or dump_file if exist.  */
extern FILE *vect_dump;
extern LOC vect_loop_location;

/*-----------------------------------------------------------------*/
/* Function prototypes.                                            */
/*-----------------------------------------------------------------*/

/* Simple loop peeling and versioning utilities for vectorizer's purposes -
   in tree-vect-loop-manip.c.  */
extern void slpeel_make_loop_iterate_ntimes (struct loop *, tree);
extern bool slpeel_can_duplicate_loop_p (const struct loop *, const_edge);
extern void vect_loop_versioning (loop_vec_info, bool, tree *, gimple_seq *);
extern void vect_do_peeling_for_loop_bound (loop_vec_info, tree *,
                                            tree, gimple_seq);
extern void vect_do_peeling_for_alignment (loop_vec_info);
extern LOC find_loop_location (struct loop *);
extern bool vect_can_advance_ivs_p (loop_vec_info);

/* In tree-vect-stmts.c.  */
extern unsigned int current_vector_size;
extern tree get_vectype_for_scalar_type (tree);
extern tree get_same_sized_vectype (tree, tree);
extern bool vect_is_simple_use (tree, loop_vec_info, bb_vec_info, gimple *,
                                tree *,  enum vect_def_type *);
extern bool vect_is_simple_use_1 (tree, loop_vec_info, bb_vec_info, gimple *,
				  tree *,  enum vect_def_type *, tree *);
extern bool supportable_widening_operation (enum tree_code, gimple, tree, tree,
                                            tree *, tree *, enum tree_code *,
                                            enum tree_code *, int *,
                                            VEC (tree, heap) **);
extern bool supportable_narrowing_operation (enum tree_code, tree, tree,
					     enum tree_code *,
					     int *, VEC (tree, heap) **);
extern stmt_vec_info new_stmt_vec_info (gimple stmt, loop_vec_info,
                                        bb_vec_info);
extern void free_stmt_vec_info (gimple stmt);
extern tree vectorizable_function (gimple, tree, tree);
extern void vect_model_simple_cost (stmt_vec_info, int, enum vect_def_type *,
                                    slp_tree);
extern void vect_model_store_cost (stmt_vec_info, int, enum vect_def_type,
                                   slp_tree);
extern void vect_model_load_cost (stmt_vec_info, int, slp_tree);
extern void vect_finish_stmt_generation (gimple, gimple,
                                         gimple_stmt_iterator *);
extern bool vect_mark_stmts_to_be_vectorized (loop_vec_info);
extern int cost_for_stmt (gimple);
extern tree vect_get_vec_def_for_operand (tree, gimple, tree *);
extern tree vect_init_vector (gimple, tree, tree,
                              gimple_stmt_iterator *);
extern tree vect_get_vec_def_for_stmt_copy (enum vect_def_type, tree);
extern bool vect_transform_stmt (gimple, gimple_stmt_iterator *,
                                 bool *, slp_tree, slp_instance);
extern void vect_remove_stores (gimple);
extern bool vect_analyze_stmt (gimple, bool *, slp_tree);
extern bool vectorizable_condition (gimple, gimple_stmt_iterator *, gimple *,
                                    tree, int);
extern void vect_get_load_cost (struct data_reference *, int, bool,
                                unsigned int *, unsigned int *);
extern void vect_get_store_cost (struct data_reference *, int, unsigned int *);

/* In tree-vect-data-refs.c.  */
extern bool vect_can_force_dr_alignment_p (const_tree, unsigned int);
extern enum dr_alignment_support vect_supportable_dr_alignment
                                           (struct data_reference *, bool);
extern tree vect_get_smallest_scalar_type (gimple, HOST_WIDE_INT *,
                                           HOST_WIDE_INT *);
extern bool vect_analyze_data_ref_dependences (loop_vec_info, bb_vec_info,
					       int *, bool *);
extern bool vect_enhance_data_refs_alignment (loop_vec_info);
extern bool vect_analyze_data_refs_alignment (loop_vec_info, bb_vec_info);
extern bool vect_verify_datarefs_alignment (loop_vec_info, bb_vec_info);
extern bool vect_analyze_data_ref_accesses (loop_vec_info, bb_vec_info);
extern bool vect_prune_runtime_alias_test_list (loop_vec_info);
extern bool vect_analyze_data_refs (loop_vec_info, bb_vec_info, int *);
extern tree vect_create_data_ref_ptr (gimple, struct loop *, tree, tree *,
                                      gimple *, bool, bool *);
extern tree bump_vector_ptr (tree, gimple, gimple_stmt_iterator *, gimple, tree);
extern tree vect_create_destination_var (tree, tree);
extern bool vect_strided_store_supported (tree);
extern bool vect_strided_load_supported (tree);
extern bool vect_permute_store_chain (VEC(tree,heap) *,unsigned int, gimple,
                                    gimple_stmt_iterator *, VEC(tree,heap) **);
extern tree vect_setup_realignment (gimple, gimple_stmt_iterator *, tree *,
                                    enum dr_alignment_support, tree,
                                    struct loop **);
extern bool vect_permute_load_chain (VEC(tree,heap) *,unsigned int, gimple,
                                    gimple_stmt_iterator *, VEC(tree,heap) **);
extern bool vect_transform_strided_load (gimple, VEC(tree,heap) *, int,
                                         gimple_stmt_iterator *);
extern int vect_get_place_in_interleaving_chain (gimple, gimple);
extern tree vect_get_new_vect_var (tree, enum vect_var_kind, const char *);
extern tree vect_create_addr_base_for_vector_ref (gimple, gimple_seq *,
                                                  tree, struct loop *);

/* In tree-vect-loop.c.  */
/* FORNOW: Used in tree-parloops.c.  */
extern void destroy_loop_vec_info (loop_vec_info, bool);
extern gimple vect_force_simple_reduction (loop_vec_info, gimple, bool, bool *);
/* Drive for loop analysis stage.  */
extern loop_vec_info vect_analyze_loop (struct loop *);
/* Drive for loop transformation stage.  */
extern void vect_transform_loop (loop_vec_info);
extern loop_vec_info vect_analyze_loop_form (struct loop *);
extern bool vectorizable_live_operation (gimple, gimple_stmt_iterator *,
                                         gimple *);
extern bool vectorizable_reduction (gimple, gimple_stmt_iterator *, gimple *,
                                    slp_tree);
extern bool vectorizable_induction (gimple, gimple_stmt_iterator *, gimple *);
extern int vect_estimate_min_profitable_iters (loop_vec_info);
extern tree get_initial_def_for_reduction (gimple, tree, tree *);
extern int vect_min_worthwhile_factor (enum tree_code);
extern int vect_get_known_peeling_cost (loop_vec_info, int, int *, int);
extern int vect_get_single_scalar_iteraion_cost (loop_vec_info);

/* In tree-vect-slp.c.  */
extern void vect_free_slp_instance (slp_instance);
extern bool vect_transform_slp_perm_load (gimple, VEC (tree, heap) *,
                                          gimple_stmt_iterator *, int,
                                          slp_instance, bool);
extern bool vect_schedule_slp (loop_vec_info, bb_vec_info);
extern void vect_update_slp_costs_according_to_vf (loop_vec_info);
extern bool vect_analyze_slp (loop_vec_info, bb_vec_info);
extern void vect_make_slp_decision (loop_vec_info);
extern void vect_detect_hybrid_slp (loop_vec_info);
extern void vect_get_slp_defs (tree, tree, slp_tree, VEC (tree,heap) **,
                               VEC (tree,heap) **, int);
extern LOC find_bb_location (basic_block);
extern bb_vec_info vect_slp_analyze_bb (basic_block);
extern void vect_slp_transform_bb (basic_block);

/* In tree-vect-patterns.c.  */
/* Pattern recognition functions.
   Additional pattern recognition functions can (and will) be added
   in the future.  */
typedef gimple (* vect_recog_func_ptr) (gimple, tree *, tree *);
#define NUM_PATTERNS 4
void vect_pattern_recog (loop_vec_info);

/* In tree-vectorizer.c.  */
unsigned vectorize_loops (void);
/* Vectorization debug information */
extern bool vect_print_dump_info (enum vect_verbosity_levels);

#endif  /* GCC_TREE_VECTORIZER_H  */
