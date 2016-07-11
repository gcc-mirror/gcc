/* Loop Vectorization using unified representation for permute instructions.
   Copyright (C) 2003-2015 Free Software Foundation, Inc.
   Contributed by Sameera Deshpande <sameera.deshpande@imgtec.com>

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


/* Prim-op ITER : ITER node has 3 main objectives in loop representation using
   primitive-ops for reordering -
   1) Enlist all p-trees representing live destination vectors written within
      the loop.
   2) Record preparatory operations, loop transformations and cleanup operations
      needed to enable vectorization.
   3) Record number of iterations once vector-size reduction is applied on
      primitive trees.  */

struct ITER_node {
  /* Preamble */

  /* Loop for which the ITER node is created.  */
  struct loop *loop;

  /* Number of iterations - At the beginning, it is 1.  After vector-size
     reduction operation, it has appropriate value.  */
  tree num_iter;

  /* Preparatory statements to be emitted before vectorized loop body.  */
  vec<gimple *> prep_stmts;

  /* If loop peeling is needed - before/after depending upon this variable.  */
  int loop_peel_needed;

  /* Actual loop body */

  /* This vector enlists the statements which are live beyond the loop.  Each
     iteration performs all the operations in this vector in same order.  For
     most of the cases, this vector holds perm-trees responsible for vector
     updation.  */
  vec<struct primop_tree *> stmts;

  /* Epilogue */

  /* When we have grouped data accesses with gaps, we may introduce invalid
     memory accesses.  We peel the last iteration of the loop to prevent
     this.  */
  bool peeling_for_gaps;

  /* When the number of iterations is not a multiple of the vector size
     we need to peel off iterations at the end to form an epilogue loop.  */
  bool peeling_for_niter;

  /* Concluding operations to be performed after loop body - e.g: collapse op
     on temporary vectors.  */
  vec<gimple *> finish_stmts;

  /* All data references within the loop.  */
  vec<data_reference_p> datarefs;

  /* All data dependences within the loop.  */
  vec<ddr_p> ddrs;
};

#define ITER_NODE_NITERS(x) (x)->num_iter
#define ITER_NODE_NITERS_KNOWN_P(x) \
  (tree_fits_shwi_p ((x)->num_iter) && tree_to_shwi ((x)->num_iter) > 0)
#define ITER_NODE_LOOP(x) (x)->loop
#define ITER_NODE_PROLOGUE(x) (x)->prep_stmts
#define ITER_NODE_LOOP_BODY(x) (x)->stmts
#define ITER_NODE_EPILOGUE(x) (x)->finish_stmts
#define ITER_NODE_LOOP_PEEL_NEEDED(x) (x)->loop_peel_needed
#define ITER_NODE_PEELING_FOR_GAPS(x) (x)->peeling_for_gaps
#define ITER_NODE_PEELING_FOR_NITER(x) (x)->peeling_for_niter
#define ITER_NODE_DATA_REFS(x) (x)->datarefs
#define ITER_NODE_DATA_DEPS(x) (x)->ddrs

enum stmt_use_type {
  stmt_use_type_undef,
  stmt_use_type_scalar,
  stmt_use_type_loop_invariant,
  stmt_use_type_induction,
  stmt_use_type_reduction,
  stmt_use_type_intermediate,
  stmt_use_type_complex,
  stmt_use_type_loop_exit_ctrl
};

struct stmt_attr {
  enum stmt_vec_info_type type;
  enum stmt_use_type use_type;
  tree access_fn;
  struct primop_tree *ptree;
  bool probable_root;
  struct data_reference *dr;
  tree vectype;
};

#define STMT_ATTR_USE_TYPE(s) (get_stmt_attr (s))->use_type
#define STMT_ATTR_ACCESS_FN(s) (get_stmt_attr (s))->access_fn
#define STMT_ATTR_TREE(s) (get_stmt_attr (s))->ptree
#define STMT_ATTR_PROOT(s) (get_stmt_attr (s))->probable_root
#define STMT_ATTR_DR(s) (get_stmt_attr (s))->dr
#define STMT_ATTR_VECTYPE(s) (get_stmt_attr (s))->vectype

vec<struct stmt_attr *> stmt_attr_vec;

void
init_stmt_attr_vec (void)
{ 
  gcc_assert (!stmt_attr_vec.exists ());
  stmt_attr_vec.create (50);
}

void
free_stmt_attr_vec (void)
{ 
  gcc_assert (stmt_attr_vec.exists ());
  stmt_attr_vec.release ();
}

inline void
set_stmt_attr (gimple *stmt, struct stmt_attr *info)
{ 
  unsigned int uid = gimple_uid (stmt);
  if (uid == 0)
    { 
      gcc_checking_assert (info);
      uid = stmt_attr_vec.length () + 1;
      gimple_set_uid (stmt, uid);
      stmt_attr_vec.safe_push (info);
    }
  else
    {
      gcc_checking_assert (info == NULL);
      stmt_attr_vec[uid - 1] = info;
    }
}

inline struct stmt_attr *
get_stmt_attr (gimple *stmt)
{ 
  unsigned int uid = gimple_uid (stmt);
  if (uid == 0)
    return NULL;

  return stmt_attr_vec[uid - 1];
}

/* PRIMOP_TREE : Memory accesses within a loop have definite repetative pattern
   which can be captured using primitive permute operators which can be used to
   determine desired permute order for the vector computations.  The PRIMOP_TREE
   is AST which records all computations and permutations required to store
   destination vector into continuous memory at the end of all iterations of the
   loop.  */
struct primop_tree {
  /* Unique ptree ID for dump.  */
  int pid;

  /* stmt_attr number.  */
  int attr_no;

  /* Operation.  */
  int node_op;

  /* Arity of the operation.  */
  int arity;

  /* List of children to this node.  */
  vec<struct primop_tree *> children;

  /* Parent node.  */
  struct primop_tree *parent;

  /* Number of iterations - At the beginning, it is loop_count.  After vec-size
     reduction operation, it is changed to vectorization factor for the
     operation.  */
  tree iter_count;

  /* Actual vector size supported by target.  */
  int vec_size;

  /* The vector type which should be used to vectorize this node.  */
  tree vec_type;

  /* Set of vector instructions to represent this node.  */
  vec<gimple *> vec_inst;

  /* Instruction cost for vec_size.  */
  int target_cost;

  /* Number of instances of vec_inst needed for iter_count.  */
  int instances;

  /* If the tree is loop-varient, the loops on which this tree depends.  */
  /* TODO: I am not very sure of we need all the ITERs or just innermost
     affecting loop.  However, for now, having list of all loops from inner-
     most to outer-most.  */
  vec<struct ITER_node *> loop_dependences;

  /* Dependence links if any to other statements.  */
  vec<ddr_p> dependences;

  /* Depth within sub-tree of same type.  */
  int depth;

  union {
    /* In case of c-node, gimple statement cooresponding to c-op.  */
    gimple * gimple_for_comp;

    /* In case of permute-node, some permute-specific attributes.  */
    struct perm_node {
      /* The component to be selected for EXTRACT or SPLIT op.  */
      int opd_selector;

      /* Number of partitions for permute op.  In case of variable mult-idx,
	 this gives ARITY for ILV and CONCAT as well.  For constant mult-idx,
	 ARITY = DIVISION.  */
      int division;
      tree *var_stride;
    } val;

    /* ITER-node representing inner loop, if any.  */
    struct ITER_node * inode;

    /* mem_ref without offset information.  */
    struct mem_ref {
      tree base;
      tree mult_idx;
      bool is_read;
    } memval;
  } u;
  void *aux;
};

#define PT_PID(x) (x)->pid
#define PT_NODE_OP(x) (x)->node_op
#define PT_ATTR_NO(x) (x)->attr_no
#define PT_ARITY(x) (x)->arity
#define PT_CHILD(x,i) (x)->children[i]
#define PT_PARENT(x) (x)->parent
#define PT_ITER_COUNT(x) (x)->iter_count
#define PT_VEC_SIZE(x) (x)->vec_size
#define PT_VEC_TYPE(x) (x)->vec_type
#define PT_VEC_INST(x) (x)->vec_inst
#define PT_TARGET_COST(x) (x)->target_cost
#define PT_NUM_INSTANCES(x) (x)->instances
#define PT_LOOP_DEPENDENCES(x) (x)->loop_dependences
#define PT_DEP(x) (x)->dependences
#define PT_DEPTH(x) (x)->depth
#define PT_COMPUTE_STMT(x) (x)->u.gimple_for_comp
#define PT_OPERAND_SELECTOR(x) (x)->u.val.opd_selector
#define PT_DIVISION(x) (x)->u.val.division
#define PT_VAR_STRIDE(x) (x)->u.val.var_stride
#define PT_INODE(x) (x)->u.inode
#define PT_MEMVAL_BASE(x) (x)->u.memval.base
#define PT_MEMVAL_MULT_IDX(x) (x)->u.memval.mult_idx
#define PT_MEMVAL_IS_READ(x) (x)->u.memval.is_read
#define PT_AUX(x) (x)->aux

//struct ITER_node *iter_node;

extern unsigned int vectorize_loops_using_uniop (void);
extern struct primop_tree * analyze_and_create_ptree (struct primop_tree *,
		 gimple *, struct ITER_node *);
extern void pretty_print_ptree_vec (pretty_printer *,
				    vec<struct primop_tree*>);
extern void pretty_print_iter_node (pretty_printer *, struct ITER_node *, int);

enum primop_code {
  POP_ILV=MAX_TREE_CODES,
  POP_CONCAT,
  POP_EXTR,
  POP_SPLT,
  POP_COLLAPSE,
  POP_MEMREF,
  POP_CONST,
  POP_INV,
  POP_ITER};
