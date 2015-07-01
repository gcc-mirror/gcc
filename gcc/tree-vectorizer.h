/* Vectorizer
   Copyright (C) 2003-2015 Free Software Foundation, Inc.
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
#include "target.h"

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

/* Structure to encapsulate information about a group of like
   instructions to be presented to the target cost model.  */
typedef struct _stmt_info_for_cost {
  int count;
  enum vect_cost_for_stmt kind;
  gimple stmt;
  int misalign;
} stmt_info_for_cost;


typedef vec<stmt_info_for_cost> stmt_vector_for_cost;

static inline void
add_stmt_info_to_vec (stmt_vector_for_cost *stmt_cost_vec, int count,
		      enum vect_cost_for_stmt kind, gimple stmt, int misalign)
{
  stmt_info_for_cost si;
  si.count = count;
  si.kind = kind;
  si.stmt = stmt;
  si.misalign = misalign;
  stmt_cost_vec->safe_push (si);
}

/************************************************************************
  SLP
 ************************************************************************/
typedef struct _slp_tree *slp_tree;

/* A computation tree of an SLP instance.  Each node corresponds to a group of
   stmts to be packed in a SIMD stmt.  */
struct _slp_tree {
  /* Nodes that contain def-stmts of this node statements operands.  */
  vec<slp_tree> children;
  /* A group of scalar stmts to be vectorized together.  */
  vec<gimple> stmts;
  /* Load permutation relative to the stores, NULL if there is no
     permutation.  */
  vec<unsigned> load_permutation;
  /* Vectorized stmt/s.  */
  vec<gimple> vec_stmts;
  /* Number of vector stmts that are created to replace the group of scalar
     stmts. It is calculated during the transformation phase as the number of
     scalar elements in one scalar iteration (GROUP_SIZE) multiplied by VF
     divided by vector size.  */
  unsigned int vec_stmts_size;
  /* Whether the scalar computations use two different operators.  */
  bool two_operators;
};


/* SLP instance is a sequence of stmts in a loop that can be packed into
   SIMD stmts.  */
typedef struct _slp_instance {
  /* The root of SLP tree.  */
  slp_tree root;

  /* Size of groups of scalar stmts that will be replaced by SIMD stmt/s.  */
  unsigned int group_size;

  /* The unrolling factor required to vectorized this SLP instance.  */
  unsigned int unrolling_factor;

  /* The group of nodes that contain loads of this SLP instance.  */
  vec<slp_tree> loads;
} *slp_instance;


/* Access Functions.  */
#define SLP_INSTANCE_TREE(S)                     (S)->root
#define SLP_INSTANCE_GROUP_SIZE(S)               (S)->group_size
#define SLP_INSTANCE_UNROLLING_FACTOR(S)         (S)->unrolling_factor
#define SLP_INSTANCE_LOADS(S)                    (S)->loads

#define SLP_TREE_CHILDREN(S)                     (S)->children
#define SLP_TREE_SCALAR_STMTS(S)                 (S)->stmts
#define SLP_TREE_VEC_STMTS(S)                    (S)->vec_stmts
#define SLP_TREE_NUMBER_OF_VEC_STMTS(S)          (S)->vec_stmts_size
#define SLP_TREE_LOAD_PERMUTATION(S)             (S)->load_permutation
#define SLP_TREE_TWO_OPERATORS(S)		 (S)->two_operators

/* This structure is used in creation of an SLP tree.  Each instance
   corresponds to the same operand in a group of scalar stmts in an SLP
   node.  */
typedef struct _slp_oprnd_info
{
  /* Def-stmts for the operands.  */
  vec<gimple> def_stmts;
  /* Information about the first statement, its vector def-type, type, the
     operand itself in case it's constant, and an indication if it's a pattern
     stmt.  */
  enum vect_def_type first_dt;
  tree first_op_type;
  bool first_pattern;
  bool second_pattern;
} *slp_oprnd_info;



/* This struct is used to store the information of a data reference,
   including the data ref itself, the access offset (calculated by summing its
   offset and init) and the segment length for aliasing checks.
   This is used to merge alias checks.  */

struct dr_with_seg_len
{
  dr_with_seg_len (data_reference_p d, tree len)
    : dr (d),
      offset (size_binop (PLUS_EXPR, DR_OFFSET (d), DR_INIT (d))),
      seg_len (len) {}

  data_reference_p dr;
  tree offset;
  tree seg_len;
};

/* This struct contains two dr_with_seg_len objects with aliasing data
   refs.  Two comparisons are generated from them.  */

struct dr_with_seg_len_pair_t
{
  dr_with_seg_len_pair_t (const dr_with_seg_len& d1,
			       const dr_with_seg_len& d2)
    : first (d1), second (d2) {}

  dr_with_seg_len first;
  dr_with_seg_len second;
};


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
  stmt_vector_for_cost body_cost_vec;
} *vect_peel_extended_info;


/* Peeling hashtable helpers.  */

struct peel_info_hasher : free_ptr_hash <_vect_peel_info>
{
  static inline hashval_t hash (const _vect_peel_info *);
  static inline bool equal (const _vect_peel_info *, const _vect_peel_info *);
};

inline hashval_t
peel_info_hasher::hash (const _vect_peel_info *peel_info)
{
  return (hashval_t) peel_info->npeel;
}

inline bool
peel_info_hasher::equal (const _vect_peel_info *a, const _vect_peel_info *b)
{
  return (a->npeel == b->npeel);
}


/*-----------------------------------------------------------------*/
/* Info on vectorized loops.                                       */
/*-----------------------------------------------------------------*/
typedef struct _loop_vec_info {

  /* The loop to which this info struct refers to.  */
  struct loop *loop;

  /* The loop basic blocks.  */
  basic_block *bbs;

  /* Number of latch executions.  */
  tree num_itersm1;
  /* Number of iterations.  */
  tree num_iters;
  /* Number of iterations of the original loop.  */
  tree num_iters_unchanged;

  /* Minimum number of iterations below which vectorization is expected to
     not be profitable (as estimated by the cost model).
     -1 indicates that vectorization will not be profitable.
     FORNOW: This field is an int. Will be a tree in the future, to represent
	     values unknown at compile time.  */
  int min_profitable_iters;

  /* Threshold of number of iterations below which vectorzation will not be
     performed. It is calculated from MIN_PROFITABLE_ITERS and
     PARAM_MIN_VECT_LOOP_BOUND.  */
  unsigned int th;

  /* Is the loop vectorizable? */
  bool vectorizable;

  /* Unrolling factor  */
  int vectorization_factor;

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
  vec<loop_p> loop_nest;

  /* All data references in the loop.  */
  vec<data_reference_p> datarefs;

  /* All data dependences in the loop.  */
  vec<ddr_p> ddrs;

  /* Data Dependence Relations defining address ranges that are candidates
     for a run-time aliasing check.  */
  vec<ddr_p> may_alias_ddrs;

  /* Data Dependence Relations defining address ranges together with segment
     lengths from which the run-time aliasing check is built.  */
  vec<dr_with_seg_len_pair_t> comp_alias_ddrs;

  /* Statements in the loop that have data references that are candidates for a
     runtime (loop versioning) misalignment check.  */
  vec<gimple> may_misalign_stmts;

  /* All interleaving chains of stores in the loop, represented by the first
     stmt in the chain.  */
  vec<gimple> grouped_stores;

  /* All SLP instances in the loop. This is a subset of the set of GROUP_STORES
     of the loop.  */
  vec<slp_instance> slp_instances;

  /* The unrolling factor needed to SLP the loop. In case of that pure SLP is
     applied to the loop, i.e., no unrolling is needed, this is 1.  */
  unsigned slp_unrolling_factor;

  /* Reduction cycles detected in the loop. Used in loop-aware SLP.  */
  vec<gimple> reductions;

  /* All reduction chains in the loop, represented by the first
     stmt in the chain.  */
  vec<gimple> reduction_chains;

  /* Hash table used to choose the best peeling option.  */
  hash_table<peel_info_hasher> *peeling_htab;

  /* Cost vector for a single scalar iteration.  */
  vec<stmt_info_for_cost> scalar_cost_vec;

  /* Cost of a single scalar iteration.  */
  int single_scalar_iteration_cost;

  /* Cost data used by the target cost model.  */
  void *target_cost_data;

  /* When we have grouped data accesses with gaps, we may introduce invalid
     memory accesses.  We peel the last iteration of the loop to prevent
     this.  */
  bool peeling_for_gaps;

  /* When the number of iterations is not a multiple of the vector size
     we need to peel off iterations at the end to form an epilogue loop.  */
  bool peeling_for_niter;

  /* Reductions are canonicalized so that the last operand is the reduction
     operand.  If this places a constant into RHS1, this decanonicalizes
     GIMPLE for other phases, so we must track when this has occurred and
     fix it up.  */
  bool operands_swapped;

  /* True if there are no loop carried data dependencies in the loop.
     If loop->safelen <= 1, then this is always true, either the loop
     didn't have any loop carried data dependencies, or the loop is being
     vectorized guarded with some runtime alias checks, or couldn't
     be vectorized at all, but then this field shouldn't be used.
     For loop->safelen >= 2, the user has asserted that there are no
     backward dependencies, but there still could be loop carried forward
     dependencies in such loops.  This flag will be false if normal
     vectorizer data dependency analysis would fail or require versioning
     for alias, but because of loop->safelen >= 2 it has been vectorized
     even without versioning for alias.  E.g. in:
     #pragma omp simd
     for (int i = 0; i < m; i++)
       a[i] = a[i + k] * c;
     (or #pragma simd or #pragma ivdep) we can vectorize this and it will
     DTRT even for k > 0 && k < m, but without safelen we would not
     vectorize this, so this field would be false.  */
  bool no_data_dependencies;

  /* If if-conversion versioned this loop before conversion, this is the
     loop version without if-conversion.  */
  struct loop *scalar_loop;

} *loop_vec_info;

/* Access Functions.  */
#define LOOP_VINFO_LOOP(L)                 (L)->loop
#define LOOP_VINFO_BBS(L)                  (L)->bbs
#define LOOP_VINFO_NITERSM1(L)             (L)->num_itersm1
#define LOOP_VINFO_NITERS(L)               (L)->num_iters
/* Since LOOP_VINFO_NITERS and LOOP_VINFO_NITERSM1 can change after
   prologue peeling retain total unchanged scalar loop iterations for
   cost model.  */
#define LOOP_VINFO_NITERS_UNCHANGED(L)     (L)->num_iters_unchanged
#define LOOP_VINFO_COST_MODEL_MIN_ITERS(L) (L)->min_profitable_iters
#define LOOP_VINFO_COST_MODEL_THRESHOLD(L) (L)->th
#define LOOP_VINFO_VECTORIZABLE_P(L)       (L)->vectorizable
#define LOOP_VINFO_VECT_FACTOR(L)          (L)->vectorization_factor
#define LOOP_VINFO_PTR_MASK(L)             (L)->ptr_mask
#define LOOP_VINFO_LOOP_NEST(L)            (L)->loop_nest
#define LOOP_VINFO_DATAREFS(L)             (L)->datarefs
#define LOOP_VINFO_DDRS(L)                 (L)->ddrs
#define LOOP_VINFO_INT_NITERS(L)           (TREE_INT_CST_LOW ((L)->num_iters))
#define LOOP_VINFO_PEELING_FOR_ALIGNMENT(L) (L)->peeling_for_alignment
#define LOOP_VINFO_UNALIGNED_DR(L)         (L)->unaligned_dr
#define LOOP_VINFO_MAY_MISALIGN_STMTS(L)   (L)->may_misalign_stmts
#define LOOP_VINFO_MAY_ALIAS_DDRS(L)       (L)->may_alias_ddrs
#define LOOP_VINFO_COMP_ALIAS_DDRS(L)      (L)->comp_alias_ddrs
#define LOOP_VINFO_GROUPED_STORES(L)       (L)->grouped_stores
#define LOOP_VINFO_SLP_INSTANCES(L)        (L)->slp_instances
#define LOOP_VINFO_SLP_UNROLLING_FACTOR(L) (L)->slp_unrolling_factor
#define LOOP_VINFO_REDUCTIONS(L)           (L)->reductions
#define LOOP_VINFO_REDUCTION_CHAINS(L)     (L)->reduction_chains
#define LOOP_VINFO_PEELING_HTAB(L)         (L)->peeling_htab
#define LOOP_VINFO_TARGET_COST_DATA(L)     (L)->target_cost_data
#define LOOP_VINFO_PEELING_FOR_GAPS(L)     (L)->peeling_for_gaps
#define LOOP_VINFO_OPERANDS_SWAPPED(L)     (L)->operands_swapped
#define LOOP_VINFO_PEELING_FOR_NITER(L)    (L)->peeling_for_niter
#define LOOP_VINFO_NO_DATA_DEPENDENCIES(L) (L)->no_data_dependencies
#define LOOP_VINFO_SCALAR_LOOP(L)	   (L)->scalar_loop
#define LOOP_VINFO_SCALAR_ITERATION_COST(L) (L)->scalar_cost_vec
#define LOOP_VINFO_SINGLE_SCALAR_ITERATION_COST(L) (L)->single_scalar_iteration_cost

#define LOOP_REQUIRES_VERSIONING_FOR_ALIGNMENT(L) \
  ((L)->may_misalign_stmts.length () > 0)
#define LOOP_REQUIRES_VERSIONING_FOR_ALIAS(L)     \
  ((L)->may_alias_ddrs.length () > 0)

#define LOOP_VINFO_NITERS_KNOWN_P(L)          \
  (tree_fits_shwi_p ((L)->num_iters) && tree_to_shwi ((L)->num_iters) > 0)

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
  vec<gimple> grouped_stores;

  /* All SLP instances in the basic block. This is a subset of the set of
     GROUP_STORES of the basic block.  */
  vec<slp_instance> slp_instances;

  /* All data references in the basic block.  */
  vec<data_reference_p> datarefs;

  /* All data dependences in the basic block.  */
  vec<ddr_p> ddrs;

  /* Cost data used by the target cost model.  */
  void *target_cost_data;

} *bb_vec_info;

#define BB_VINFO_BB(B)               (B)->bb
#define BB_VINFO_GROUPED_STORES(B)   (B)->grouped_stores
#define BB_VINFO_SLP_INSTANCES(B)    (B)->slp_instances
#define BB_VINFO_DATAREFS(B)         (B)->datarefs
#define BB_VINFO_DDRS(B)             (B)->ddrs
#define BB_VINFO_TARGET_COST_DATA(B) (B)->target_cost_data

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
  call_simd_clone_vec_info_type,
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

typedef struct _stmt_vec_info {

  enum stmt_vec_info_type type;

  /* Indicates whether this stmts is part of a computation whose result is
     used outside the loop.  */
  bool live;

  /* Stmt is part of some pattern (computation idiom)  */
  bool in_pattern_p;

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

  /* For loop PHI nodes, the evolution part of it.  This makes sure
     this information is still available in vect_update_ivs_after_vectorizer
     where we may not be able to re-analyze the PHI nodes evolution as
     peeling for the prologue loop can make it unanalyzable.  The evolution
     part is still correct though.  */
  tree loop_phi_evolution_part;

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

  /* Used to keep a sequence of def stmts of a pattern stmt if such exists.  */
  gimple_seq pattern_def_seq;

  /* List of datarefs that are known to have the same alignment as the dataref
     of this stmt.  */
  vec<dr_p> same_align_refs;

  /* Selected SIMD clone's function info.  First vector element
     is SIMD clone's function decl, followed by a pair of trees (base + step)
     for linear arguments (pair of NULLs for other arguments).  */
  vec<tree> simd_clone_info;

  /* Classify the def of this stmt.  */
  enum vect_def_type def_type;

  /*  Whether the stmt is SLPed, loop-based vectorized, or both.  */
  enum slp_vect_type slp_type;

  /* Interleaving and reduction chains info.  */
  /* First element in the group.  */
  gimple first_element;
  /* Pointer to the next element in the group.  */
  gimple next_element;
  /* For data-refs, in case that two or more stmts share data-ref, this is the
     pointer to the previously detected stmt with the same dr.  */
  gimple same_dr_stmt;
  /* The size of the group.  */
  unsigned int size;
  /* For stores, number of stores from this group seen. We vectorize the last
     one.  */
  unsigned int store_count;
  /* For loads only, the gap from the previous load. For consecutive loads, GAP
     is 1.  */
  unsigned int gap;

  /* The minimum negative dependence distance this stmt participates in
     or zero if none.  */
  unsigned int min_neg_dist;

  /* Not all stmts in the loop need to be vectorized. e.g, the increment
     of the loop induction variable and computation of array indexes. relevant
     indicates whether the stmt needs to be vectorized.  */
  enum vect_relevant relevant;

  /* The bb_vec_info with respect to which STMT is vectorized.  */
  bb_vec_info bb_vinfo;

  /* Is this statement vectorizable or should it be skipped in (partial)
     vectorization.  */
  bool vectorizable;

  /* For loads only, true if this is a gather load.  */
  bool gather_p;

  /* True if this is an access with loop-invariant stride.  */
  bool strided_p;

  /* For both loads and stores.  */
  bool simd_lane_access_p;
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
#define STMT_VINFO_GATHER_P(S)		   (S)->gather_p
#define STMT_VINFO_STRIDED_P(S)	   	   (S)->strided_p
#define STMT_VINFO_SIMD_LANE_ACCESS_P(S)   (S)->simd_lane_access_p

#define STMT_VINFO_DR_BASE_ADDRESS(S)      (S)->dr_base_address
#define STMT_VINFO_DR_INIT(S)              (S)->dr_init
#define STMT_VINFO_DR_OFFSET(S)            (S)->dr_offset
#define STMT_VINFO_DR_STEP(S)              (S)->dr_step
#define STMT_VINFO_DR_ALIGNED_TO(S)        (S)->dr_aligned_to

#define STMT_VINFO_IN_PATTERN_P(S)         (S)->in_pattern_p
#define STMT_VINFO_RELATED_STMT(S)         (S)->related_stmt
#define STMT_VINFO_PATTERN_DEF_SEQ(S)      (S)->pattern_def_seq
#define STMT_VINFO_SAME_ALIGN_REFS(S)      (S)->same_align_refs
#define STMT_VINFO_SIMD_CLONE_INFO(S)	   (S)->simd_clone_info
#define STMT_VINFO_DEF_TYPE(S)             (S)->def_type
#define STMT_VINFO_GROUP_FIRST_ELEMENT(S)  (S)->first_element
#define STMT_VINFO_GROUP_NEXT_ELEMENT(S)   (S)->next_element
#define STMT_VINFO_GROUP_SIZE(S)           (S)->size
#define STMT_VINFO_GROUP_STORE_COUNT(S)    (S)->store_count
#define STMT_VINFO_GROUP_GAP(S)            (S)->gap
#define STMT_VINFO_GROUP_SAME_DR_STMT(S)   (S)->same_dr_stmt
#define STMT_VINFO_GROUPED_ACCESS(S)      ((S)->first_element != NULL && (S)->data_ref_info)
#define STMT_VINFO_LOOP_PHI_EVOLUTION_PART(S) (S)->loop_phi_evolution_part
#define STMT_VINFO_MIN_NEG_DIST(S)	(S)->min_neg_dist

#define GROUP_FIRST_ELEMENT(S)          (S)->first_element
#define GROUP_NEXT_ELEMENT(S)           (S)->next_element
#define GROUP_SIZE(S)                   (S)->size
#define GROUP_STORE_COUNT(S)            (S)->store_count
#define GROUP_GAP(S)                    (S)->gap
#define GROUP_SAME_DR_STMT(S)           (S)->same_dr_stmt

#define STMT_VINFO_RELEVANT_P(S)          ((S)->relevant != vect_unused_in_scope)

#define HYBRID_SLP_STMT(S)                ((S)->slp_type == hybrid)
#define PURE_SLP_STMT(S)                  ((S)->slp_type == pure_slp)
#define STMT_SLP_TYPE(S)                   (S)->slp_type

struct dataref_aux {
  tree base_decl;
  bool base_misaligned;
  int misalignment;
};

#define VECT_MAX_COST 1000

/* The maximum number of intermediate steps required in multi-step type
   conversion.  */
#define MAX_INTERM_CVT_STEPS         3

/* The maximum vectorization factor supported by any target (V64QI).  */
#define MAX_VECTORIZATION_FACTOR 64

/* Avoid GTY(()) on stmt_vec_info.  */
typedef void *vec_void_p;

extern vec<vec_void_p> stmt_vec_info_vec;

void init_stmt_vec_info_vec (void);
void free_stmt_vec_info_vec (void);

/* Return a stmt_vec_info corresponding to STMT.  */

static inline stmt_vec_info
vinfo_for_stmt (gimple stmt)
{
  unsigned int uid = gimple_uid (stmt);
  if (uid == 0)
    return NULL;

  return (stmt_vec_info) stmt_vec_info_vec[uid - 1];
}

/* Set vectorizer information INFO for STMT.  */

static inline void
set_vinfo_for_stmt (gimple stmt, stmt_vec_info info)
{
  unsigned int uid = gimple_uid (stmt);
  if (uid == 0)
    {
      gcc_checking_assert (info);
      uid = stmt_vec_info_vec.length () + 1;
      gimple_set_uid (stmt, uid);
      stmt_vec_info_vec.safe_push ((vec_void_p) info);
    }
  else
    stmt_vec_info_vec[uid - 1] = (vec_void_p) info;
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

  gcc_checking_assert (uid1 <= stmt_vec_info_vec.length ()
		       && uid2 <= stmt_vec_info_vec.length ());

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

  gcc_assert (uid1 <= stmt_vec_info_vec.length ());
  gcc_assert (uid2 <= stmt_vec_info_vec.length ());

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

/* Return pow2 (X).  */

static inline int
vect_pow2 (int x)
{
  int i, res = 1;

  for (i = 0; i < x; i++)
    res *= 2;

  return res;
}

/* Alias targetm.vectorize.builtin_vectorization_cost.  */

static inline int
builtin_vectorization_cost (enum vect_cost_for_stmt type_of_cost,
			    tree vectype, int misalign)
{
  return targetm.vectorize.builtin_vectorization_cost (type_of_cost,
						       vectype, misalign);
}

/* Get cost by calling cost target builtin.  */

static inline
int vect_get_stmt_cost (enum vect_cost_for_stmt type_of_cost)
{
  return builtin_vectorization_cost (type_of_cost, NULL, 0);
}

/* Alias targetm.vectorize.init_cost.  */

static inline void *
init_cost (struct loop *loop_info)
{
  return targetm.vectorize.init_cost (loop_info);
}

/* Alias targetm.vectorize.add_stmt_cost.  */

static inline unsigned
add_stmt_cost (void *data, int count, enum vect_cost_for_stmt kind,
	       stmt_vec_info stmt_info, int misalign,
	       enum vect_cost_model_location where)
{
  return targetm.vectorize.add_stmt_cost (data, count, kind,
					  stmt_info, misalign, where);
}

/* Alias targetm.vectorize.finish_cost.  */

static inline void
finish_cost (void *data, unsigned *prologue_cost,
	     unsigned *body_cost, unsigned *epilogue_cost)
{
  targetm.vectorize.finish_cost (data, prologue_cost, body_cost, epilogue_cost);
}

/* Alias targetm.vectorize.destroy_cost_data.  */

static inline void
destroy_cost_data (void *data)
{
  targetm.vectorize.destroy_cost_data (data);
}


/*-----------------------------------------------------------------*/
/* Info on data references alignment.                              */
/*-----------------------------------------------------------------*/
inline void
set_dr_misalignment (struct data_reference *dr, int val)
{
  dataref_aux *data_aux = (dataref_aux *) dr->aux;

  if (!data_aux)
    {
      data_aux = XCNEW (dataref_aux);
      dr->aux = data_aux;
    }

  data_aux->misalignment = val;
}

inline int
dr_misalignment (struct data_reference *dr)
{
  gcc_assert (dr->aux);
  return ((dataref_aux *) dr->aux)->misalignment;
}

/* Reflects actual alignment of first access in the vectorized loop,
   taking into account peeling/versioning if applied.  */
#define DR_MISALIGNMENT(DR) dr_misalignment (DR)
#define SET_DR_MISALIGNMENT(DR, VAL) set_dr_misalignment (DR, VAL)

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


/* Return true if the vect cost model is unlimited.  */
static inline bool
unlimited_cost_model (loop_p loop)
{
  if (loop != NULL && loop->force_vectorize
      && flag_simd_cost_model != VECT_COST_MODEL_DEFAULT)
    return flag_simd_cost_model == VECT_COST_MODEL_UNLIMITED;
  return (flag_vect_cost_model == VECT_COST_MODEL_UNLIMITED);
}

/* Source location */
extern source_location vect_location;

/*-----------------------------------------------------------------*/
/* Function prototypes.                                            */
/*-----------------------------------------------------------------*/

/* Simple loop peeling and versioning utilities for vectorizer's purposes -
   in tree-vect-loop-manip.c.  */
extern void slpeel_make_loop_iterate_ntimes (struct loop *, tree);
extern bool slpeel_can_duplicate_loop_p (const struct loop *, const_edge);
struct loop *slpeel_tree_duplicate_loop_to_edge_cfg (struct loop *,
						     struct loop *, edge);
extern void vect_loop_versioning (loop_vec_info, unsigned int, bool);
extern void vect_do_peeling_for_loop_bound (loop_vec_info, tree, tree,
					    unsigned int, bool);
extern void vect_do_peeling_for_alignment (loop_vec_info, tree,
					   unsigned int, bool);
extern source_location find_loop_location (struct loop *);
extern bool vect_can_advance_ivs_p (loop_vec_info);

/* In tree-vect-stmts.c.  */
extern unsigned int current_vector_size;
extern tree get_vectype_for_scalar_type (tree);
extern tree get_same_sized_vectype (tree, tree);
extern bool vect_is_simple_use (tree, gimple, loop_vec_info,
			        bb_vec_info, gimple *,
                                tree *,  enum vect_def_type *);
extern bool vect_is_simple_use_1 (tree, gimple, loop_vec_info,
				  bb_vec_info, gimple *,
				  tree *,  enum vect_def_type *, tree *);
extern bool supportable_widening_operation (enum tree_code, gimple, tree, tree,
                                            enum tree_code *, enum tree_code *,
					    int *, vec<tree> *);
extern bool supportable_narrowing_operation (enum tree_code, tree, tree,
					     enum tree_code *,
					     int *, vec<tree> *);
extern stmt_vec_info new_stmt_vec_info (gimple stmt, loop_vec_info,
                                        bb_vec_info);
extern void free_stmt_vec_info (gimple stmt);
extern tree vectorizable_function (gcall *, tree, tree);
extern void vect_model_simple_cost (stmt_vec_info, int, enum vect_def_type *,
                                    stmt_vector_for_cost *,
				    stmt_vector_for_cost *);
extern void vect_model_store_cost (stmt_vec_info, int, bool,
				   enum vect_def_type, slp_tree,
				   stmt_vector_for_cost *,
				   stmt_vector_for_cost *);
extern void vect_model_load_cost (stmt_vec_info, int, bool, slp_tree,
				  stmt_vector_for_cost *,
				  stmt_vector_for_cost *);
extern unsigned record_stmt_cost (stmt_vector_for_cost *, int,
				  enum vect_cost_for_stmt, stmt_vec_info,
				  int, enum vect_cost_model_location);
extern void vect_finish_stmt_generation (gimple, gimple,
                                         gimple_stmt_iterator *);
extern bool vect_mark_stmts_to_be_vectorized (loop_vec_info);
extern tree vect_get_vec_def_for_operand (tree, gimple, tree *);
extern tree vect_init_vector (gimple, tree, tree,
                              gimple_stmt_iterator *);
extern tree vect_get_vec_def_for_stmt_copy (enum vect_def_type, tree);
extern bool vect_transform_stmt (gimple, gimple_stmt_iterator *,
                                 bool *, slp_tree, slp_instance);
extern void vect_remove_stores (gimple);
extern bool vect_analyze_stmt (gimple, bool *, slp_tree);
extern bool vectorizable_condition (gimple, gimple_stmt_iterator *, gimple *,
                                    tree, int, slp_tree);
extern void vect_get_load_cost (struct data_reference *, int, bool,
				unsigned int *, unsigned int *,
				stmt_vector_for_cost *,
				stmt_vector_for_cost *, bool);
extern void vect_get_store_cost (struct data_reference *, int,
				 unsigned int *, stmt_vector_for_cost *);
extern bool vect_supportable_shift (enum tree_code, tree);
extern void vect_get_vec_defs (tree, tree, gimple, vec<tree> *,
			       vec<tree> *, slp_tree, int);
extern tree vect_gen_perm_mask_any (tree, const unsigned char *);
extern tree vect_gen_perm_mask_checked (tree, const unsigned char *);

/* In tree-vect-data-refs.c.  */
extern bool vect_can_force_dr_alignment_p (const_tree, unsigned int);
extern enum dr_alignment_support vect_supportable_dr_alignment
                                           (struct data_reference *, bool);
extern tree vect_get_smallest_scalar_type (gimple, HOST_WIDE_INT *,
                                           HOST_WIDE_INT *);
extern bool vect_analyze_data_ref_dependences (loop_vec_info, int *);
extern bool vect_slp_analyze_data_ref_dependences (bb_vec_info);
extern bool vect_enhance_data_refs_alignment (loop_vec_info);
extern bool vect_analyze_data_refs_alignment (loop_vec_info, bb_vec_info);
extern bool vect_verify_datarefs_alignment (loop_vec_info, bb_vec_info);
extern bool vect_analyze_data_ref_accesses (loop_vec_info, bb_vec_info);
extern bool vect_prune_runtime_alias_test_list (loop_vec_info);
extern tree vect_check_gather (gimple, loop_vec_info, tree *, tree *,
			       int *);
extern bool vect_analyze_data_refs (loop_vec_info, bb_vec_info, int *,
				    unsigned *);
extern tree vect_create_data_ref_ptr (gimple, tree, struct loop *, tree,
				      tree *, gimple_stmt_iterator *,
				      gimple *, bool, bool *,
				      tree = NULL_TREE);
extern tree bump_vector_ptr (tree, gimple, gimple_stmt_iterator *, gimple, tree);
extern tree vect_create_destination_var (tree, tree);
extern bool vect_grouped_store_supported (tree, unsigned HOST_WIDE_INT);
extern bool vect_store_lanes_supported (tree, unsigned HOST_WIDE_INT);
extern bool vect_grouped_load_supported (tree, unsigned HOST_WIDE_INT);
extern bool vect_load_lanes_supported (tree, unsigned HOST_WIDE_INT);
extern void vect_permute_store_chain (vec<tree> ,unsigned int, gimple,
                                    gimple_stmt_iterator *, vec<tree> *);
extern tree vect_setup_realignment (gimple, gimple_stmt_iterator *, tree *,
                                    enum dr_alignment_support, tree,
                                    struct loop **);
extern void vect_transform_grouped_load (gimple, vec<tree> , int,
                                         gimple_stmt_iterator *);
extern void vect_record_grouped_load_vectors (gimple, vec<tree> );
extern tree vect_get_new_vect_var (tree, enum vect_var_kind, const char *);
extern tree vect_create_addr_base_for_vector_ref (gimple, gimple_seq *,
						  tree, struct loop *,
						  tree = NULL_TREE);

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
extern tree get_initial_def_for_reduction (gimple, tree, tree *);
extern int vect_min_worthwhile_factor (enum tree_code);
extern int vect_get_known_peeling_cost (loop_vec_info, int, int *,
					stmt_vector_for_cost *,
					stmt_vector_for_cost *,
					stmt_vector_for_cost *);

/* In tree-vect-slp.c.  */
extern void vect_free_slp_instance (slp_instance);
extern bool vect_transform_slp_perm_load (slp_tree, vec<tree> ,
                                          gimple_stmt_iterator *, int,
                                          slp_instance, bool);
extern bool vect_slp_analyze_operations (vec<slp_instance> slp_instances,
					 void *);
extern bool vect_schedule_slp (loop_vec_info, bb_vec_info);
extern bool vect_analyze_slp (loop_vec_info, bb_vec_info, unsigned);
extern bool vect_make_slp_decision (loop_vec_info);
extern void vect_detect_hybrid_slp (loop_vec_info);
extern void vect_get_slp_defs (vec<tree> , slp_tree,
			       vec<vec<tree> > *, int);

extern source_location find_bb_location (basic_block);
extern bb_vec_info vect_slp_analyze_bb (basic_block);
extern void vect_slp_transform_bb (basic_block);

/* In tree-vect-patterns.c.  */
/* Pattern recognition functions.
   Additional pattern recognition functions can (and will) be added
   in the future.  */
typedef gimple (* vect_recog_func_ptr) (vec<gimple> *, tree *, tree *);
#define NUM_PATTERNS 12
void vect_pattern_recog (loop_vec_info, bb_vec_info);

/* In tree-vectorizer.c.  */
unsigned vectorize_loops (void);
void vect_destroy_datarefs (loop_vec_info, bb_vec_info);

#endif  /* GCC_TREE_VECTORIZER_H  */
