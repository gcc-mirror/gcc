/* Loop Vectorization
   Copyright (C) 2003, 2004 Free Software Foundation, Inc.
   Contributed by Dorit Naishlos <dorit@il.ibm.com>

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

/* Loop Vectorization Pass.

   This pass tries to vectorize loops. This first implementation focuses on
   simple inner-most loops, with no conditional control flow, and a set of
   simple operations which vector form can be expressed using existing
   tree codes (PLUS, MULT etc).

   For example, the vectorizer transforms the following simple loop:

	short a[N]; short b[N]; short c[N]; int i;

	for (i=0; i<N; i++){
	  a[i] = b[i] + c[i];
	}

   as if it was manually vectorized by rewriting the source code into:

	typedef int __attribute__((mode(V8HI))) v8hi;
	short a[N];  short b[N]; short c[N];   int i;
	v8hi *pa = (v8hi*)a, *pb = (v8hi*)b, *pc = (v8hi*)c;
	v8hi va, vb, vc;

	for (i=0; i<N/8; i++){
	  vb = pb[i];
	  vc = pc[i];
	  va = vb + vc;
	  pa[i] = va;
	}

	The main entry to this pass is vectorize_loops(), in which
   the vectorizer applies a set of analyses on a given set of loops,
   followed by the actual vectorization transformation for the loops that
   had successfully passed the analysis phase.

	Throughout this pass we make a distinction between two types of
   data: scalars (which are represented by SSA_NAMES), and memory references
   ("data-refs"). These two types of data require different handling both 
   during analysis and transformation. The types of data-refs that the 
   vectorizer currently supports are ARRAY_REFS which base is an array DECL 
   (not a pointer), and INDIRECT_REFS through pointers; both array and pointer
   accesses are required to have a  simple (consecutive) access pattern.

   Analysis phase:
   ===============
	The driver for the analysis phase is vect_analyze_loop_nest().
   It applies a set of analyses, some of which rely on the scalar evolution 
   analyzer (scev) developed by Sebastian Pop.

	During the analysis phase the vectorizer records some information
   per stmt in a "stmt_vec_info" struct which is attached to each stmt in the 
   loop, as well as general information about the loop as a whole, which is
   recorded in a "loop_vec_info" struct attached to each loop.

   Transformation phase:
   =====================
	The loop transformation phase scans all the stmts in the loop, and
   creates a vector stmt (or a sequence of stmts) for each scalar stmt S in
   the loop that needs to be vectorized. It insert the vector code sequence
   just before the scalar stmt S, and records a pointer to the vector code
   in STMT_VINFO_VEC_STMT (stmt_info) (stmt_info is the stmt_vec_info struct 
   attached to S). This pointer will be used for the vectorization of following
   stmts which use the def of stmt S. Stmt S is removed if it writes to memory;
   otherwise, we rely on dead code elimination for removing it.

	For example, say stmt S1 was vectorized into stmt VS1:

   VS1: vb = px[i];
   S1:	b = x[i];    STMT_VINFO_VEC_STMT (stmt_info (S1)) = VS1
   S2:  a = b;

   To vectorize stmt S2, the vectorizer first finds the stmt that defines
   the operand 'b' (S1), and gets the relevant vector def 'vb' from the
   vector stmt VS1 pointed by STMT_VINFO_VEC_STMT (stmt_info (S1)). The
   resulting sequence would be:

   VS1: vb = px[i];
   S1:	b = x[i];	STMT_VINFO_VEC_STMT (stmt_info (S1)) = VS1
   VS2: va = vb;
   S2:  a = b;          STMT_VINFO_VEC_STMT (stmt_info (S2)) = VS2

	Operands that are not SSA_NAMEs, are data-refs that appear in 
   load/store operations (like 'x[i]' in S1), and are handled differently.

   Target modeling:
   =================
	Currently the only target specific information that is used is the
   size of the vector (in bytes) - "UNITS_PER_SIMD_WORD". Targets that can 
   support different sizes of vectors, for now will need to specify one value 
   for "UNITS_PER_SIMD_WORD". More flexibility will be added in the future.

	Since we only vectorize operations which vector form can be
   expressed using existing tree codes, to verify that an operation is
   supported, the vectorizer checks the relevant optab at the relevant
   machine_mode (e.g, add_optab->handlers[(int) V8HImode].insn_code). If
   the value found is CODE_FOR_nothing, then there's no target support, and
   we can't vectorize the stmt.

   For additional information on this project see:
   http://gcc.gnu.org/projects/tree-ssa/vectorization.html
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "errors.h"
#include "ggc.h"
#include "tree.h"
#include "target.h"

#include "rtl.h"
#include "basic-block.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "tree-dump.h"
#include "timevar.h"
#include "cfgloop.h"
#include "cfglayout.h"
#include "expr.h"
#include "optabs.h"
#include "tree-chrec.h"
#include "tree-data-ref.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"
#include "tree-pass.h"

/* Main analysis functions.  */
static loop_vec_info vect_analyze_loop (struct loop *);
static loop_vec_info vect_analyze_loop_form (struct loop *);
static bool vect_analyze_data_refs (loop_vec_info);
static bool vect_mark_stmts_to_be_vectorized (loop_vec_info);
static bool vect_analyze_scalar_cycles (loop_vec_info);
static bool vect_analyze_data_ref_accesses (loop_vec_info);
static bool vect_analyze_data_refs_alignment (loop_vec_info);
static void vect_compute_data_refs_alignment (loop_vec_info);
static bool vect_analyze_operations (loop_vec_info);

/* Main code transformation functions.  */
static void vect_transform_loop (loop_vec_info, struct loops *);
static void vect_transform_loop_bound (loop_vec_info);
static bool vect_transform_stmt (tree, block_stmt_iterator *);
static bool vectorizable_load (tree, block_stmt_iterator *, tree *);
static bool vectorizable_store (tree, block_stmt_iterator *, tree *);
static bool vectorizable_operation (tree, block_stmt_iterator *, tree *);
static bool vectorizable_assignment (tree, block_stmt_iterator *, tree *);
static void vect_align_data_ref (tree);
static void vect_enhance_data_refs_alignment (loop_vec_info);

/* Utility functions for the analyses.  */
static bool vect_is_simple_use (tree , struct loop *, tree *);
static bool exist_non_indexing_operands_for_use_p (tree, tree);
static bool vect_is_simple_iv_evolution (unsigned, tree, tree *, tree *, bool);
static void vect_mark_relevant (varray_type, tree);
static bool vect_stmt_relevant_p (tree, loop_vec_info);
static tree vect_get_loop_niters (struct loop *, HOST_WIDE_INT *);
static bool vect_compute_data_ref_alignment 
  (struct data_reference *, loop_vec_info);
static bool vect_analyze_data_ref_access (struct data_reference *);
static bool vect_get_first_index (tree, tree *);
static bool vect_can_force_dr_alignment_p (tree, unsigned int);
static struct data_reference * vect_analyze_pointer_ref_access 
  (tree, tree, bool);
static tree vect_get_base_and_bit_offset
  (struct data_reference *, tree, tree, loop_vec_info, tree *, bool*);
static struct data_reference * vect_analyze_pointer_ref_access
  (tree, tree, bool);
static tree vect_compute_array_base_alignment (tree, tree, tree *, tree *);
static tree vect_compute_array_ref_alignment
  (struct data_reference *, loop_vec_info, tree, tree *);
static tree vect_get_ptr_offset (tree, tree, tree *);
static tree vect_get_symbl_and_dr
  (tree, tree, bool, loop_vec_info, struct data_reference **);

/* Utility functions for the code transformation.  */
static tree vect_create_destination_var (tree, tree);
static tree vect_create_data_ref_ptr 
  (tree, block_stmt_iterator *, tree, tree *, bool); 
static tree vect_create_index_for_vector_ref 
  (struct loop *, block_stmt_iterator *);
static tree vect_create_addr_base_for_vector_ref (tree, tree *, tree);
static tree get_vectype_for_scalar_type (tree);
static tree vect_get_new_vect_var (tree, enum vect_var_kind, const char *);
static tree vect_get_vec_def_for_operand (tree, tree);
static tree vect_init_vector (tree, tree);
static void vect_finish_stmt_generation 
  (tree stmt, tree vec_stmt, block_stmt_iterator *bsi);

/* Utilities for creation and deletion of vec_info structs.  */
loop_vec_info new_loop_vec_info (struct loop *loop);
void destroy_loop_vec_info (loop_vec_info);
stmt_vec_info new_stmt_vec_info (tree stmt, struct loop *loop);

static bool vect_debug_stats (struct loop *loop);
static bool vect_debug_details (struct loop *loop);


/* Function new_stmt_vec_info.

   Create and initialize a new stmt_vec_info struct for STMT.  */

stmt_vec_info
new_stmt_vec_info (tree stmt, struct loop *loop)
{
  stmt_vec_info res;
  res = (stmt_vec_info) xcalloc (1, sizeof (struct _stmt_vec_info));

  STMT_VINFO_TYPE (res) = undef_vec_info_type;
  STMT_VINFO_STMT (res) = stmt;
  STMT_VINFO_LOOP (res) = loop;
  STMT_VINFO_RELEVANT_P (res) = 0;
  STMT_VINFO_VECTYPE (res) = NULL;
  STMT_VINFO_VEC_STMT (res) = NULL;
  STMT_VINFO_DATA_REF (res) = NULL;
  STMT_VINFO_MEMTAG (res) = NULL;
  STMT_VINFO_VECT_DR_BASE (res) = NULL;

  return res;
}


/* Function new_loop_vec_info.

   Create and initialize a new loop_vec_info struct for LOOP, as well as
   stmt_vec_info structs for all the stmts in LOOP.  */

loop_vec_info
new_loop_vec_info (struct loop *loop)
{
  loop_vec_info res;
  basic_block *bbs;
  block_stmt_iterator si;
  unsigned int i;

  res = (loop_vec_info) xcalloc (1, sizeof (struct _loop_vec_info));

  bbs = get_loop_body (loop);

  /* Create stmt_info for all stmts in the loop.  */
  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = bbs[i];
      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	{
	  tree stmt = bsi_stmt (si);
	  stmt_ann_t ann;

	  get_stmt_operands (stmt);
	  ann = stmt_ann (stmt);
	  set_stmt_info (ann, new_stmt_vec_info (stmt, loop));
	}
    }

  LOOP_VINFO_LOOP (res) = loop;
  LOOP_VINFO_BBS (res) = bbs;
  LOOP_VINFO_EXIT_COND (res) = NULL;
  LOOP_VINFO_NITERS (res) = -1;
  LOOP_VINFO_VECTORIZABLE_P (res) = 0;
  LOOP_VINFO_VECT_FACTOR (res) = 0;
  VARRAY_GENERIC_PTR_INIT (LOOP_VINFO_DATAREF_WRITES (res), 20,
			   "loop_write_datarefs");
  VARRAY_GENERIC_PTR_INIT (LOOP_VINFO_DATAREF_READS (res), 20,
			   "loop_read_datarefs");
  return res;
}


/* Function destroy_loop_vec_info.
 
   Free LOOP_VINFO struct, as well as all the stmt_vec_info structs of all the 
   stmts in the loop.  */

void
destroy_loop_vec_info (loop_vec_info loop_vinfo)
{
  struct loop *loop;
  basic_block *bbs;
  int nbbs;
  block_stmt_iterator si;
  int j;

  if (!loop_vinfo)
    return;

  loop = LOOP_VINFO_LOOP (loop_vinfo);

  bbs = LOOP_VINFO_BBS (loop_vinfo);
  nbbs = loop->num_nodes;

  for (j = 0; j < nbbs; j++)
    {
      basic_block bb = bbs[j];
      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	{
	  tree stmt = bsi_stmt (si);
	  stmt_ann_t ann = stmt_ann (stmt);
	  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
	  free (stmt_info);
	  set_stmt_info (ann, NULL);
	}
    }

  free (LOOP_VINFO_BBS (loop_vinfo));
  varray_clear (LOOP_VINFO_DATAREF_WRITES (loop_vinfo));
  varray_clear (LOOP_VINFO_DATAREF_READS (loop_vinfo));

  free (loop_vinfo);
}


/* Function debug_loop_stats.

   For vectorization statistics dumps.  */

static bool
vect_debug_stats (struct loop *loop)
{
  basic_block bb;
  block_stmt_iterator si;
  tree node = NULL_TREE;

  if (!dump_file || !(dump_flags & TDF_STATS))
    return false;

  if (!loop)
    {
      fprintf (dump_file, "\n");
      return true;
    }

  if (!loop->header)
    return false;

  bb = loop->header;

  for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
    {
      node = bsi_stmt (si);
      if (node && EXPR_P (node) && EXPR_LOCUS (node))
        break;
    }

  if (node && EXPR_P (node) && EXPR_LOCUS (node) 
      && EXPR_FILENAME (node) && EXPR_LINENO (node))
    {
      fprintf (dump_file, "\nloop at %s:%d: ", 
	EXPR_FILENAME (node), EXPR_LINENO (node));
      return true;
    }

  return false;
}


/* Function debug_loop_details.

   For vectorization debug dumps.  */

static bool
vect_debug_details (struct loop *loop)
{
   basic_block bb;
   block_stmt_iterator si;
   tree node = NULL_TREE;

  if (!dump_file || !(dump_flags & TDF_DETAILS))
    return false;

  if (!loop)
    {
      fprintf (dump_file, "\n");
      return true;
    }

  if (!loop->header)
    return false;

  bb = loop->header;

  for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
    {
      node = bsi_stmt (si);
      if (node && EXPR_P (node) && EXPR_LOCUS (node))
	break;
    }

  if (node && EXPR_P (node) && EXPR_LOCUS (node)
      && EXPR_FILENAME (node) && EXPR_LINENO (node))
    {
      fprintf (dump_file, "\nloop at %s:%d: ", 
               EXPR_FILENAME (node), EXPR_LINENO (node));
      return true;
    }

  return false;
}


/* Function vect_get_ptr_offset

   Compute the OFFSET modulo vector-type alignment of pointer REF in bits.  */

static tree 
vect_get_ptr_offset (tree ref ATTRIBUTE_UNUSED, 
		     tree vectype ATTRIBUTE_UNUSED, 
		     tree *offset ATTRIBUTE_UNUSED)
{
  /* TODO: Use alignment information.  */
  return NULL_TREE; 
}


/* Function vect_get_base_and_bit_offset

   Return the BASE of the data reference EXPR.
   If VECTYPE is given, also compute the OFFSET from BASE in bits.
   E.g., for EXPR a.b[i] + 4B, BASE is a, and OFFSET is the overall offset in 
   bits of 'a.b[i] + 4B' from a.

   Input:
   EXPR - the memory reference that is being analyzed
   DR - the data_reference struct of the _original_ memory reference
        (Note: DR_REF (DR) is not necessarily EXPR)
   VECTYPE - the type that defines the alignment (i.e, we compute
             alignment relative to TYPE_ALIGN(VECTYPE))
   
   Output:
   BASE (returned value) - the base of the data reference EXPR.
                           E.g, if EXPR is a.b[k].c[i][j] the returned
			   base is a.
   OFFSET - offset of EXPR from BASE in bits
   BASE_ALIGNED_P - indicates if BASE is aligned
 
   If something unexpected is encountered (an unsupported form of data-ref),
   or if VECTYPE is given but OFFSET cannot be determined:
   then NULL_TREE is returned.  */

static tree 
vect_get_base_and_bit_offset (struct data_reference *dr, 
			      tree expr, 
			      tree vectype, 
			      loop_vec_info loop_vinfo,
			      tree *offset,
			      bool *base_aligned_p)
{
  tree this_offset = size_zero_node;
  tree base = NULL_TREE;
  tree next_ref;
  tree oprnd0, oprnd1;
  struct data_reference *array_dr;
  enum tree_code code = TREE_CODE (expr);

  *base_aligned_p = false;

  switch (code)
    {
    /* These cases end the recursion:  */
    case VAR_DECL:
      *offset = size_zero_node;
      if (vectype && DECL_ALIGN (expr) >= TYPE_ALIGN (vectype))
	*base_aligned_p = true;
      return expr;

    case SSA_NAME:
      if (!vectype)
	return expr;

      if (TREE_CODE (TREE_TYPE (expr)) != POINTER_TYPE)
	return NULL_TREE;
      
      if (TYPE_ALIGN (TREE_TYPE (TREE_TYPE (expr))) < TYPE_ALIGN (vectype)) 
	{
	  base = vect_get_ptr_offset (expr, vectype, offset);
	  if (base)
	    *base_aligned_p = true;
	}
      else
	{	  
	  *base_aligned_p = true;
	  *offset = size_zero_node;
	  base = expr;
	}
      return base;
      
    case INTEGER_CST:      
      *offset = int_const_binop (MULT_EXPR, expr,     
				 build_int_cst (NULL_TREE, BITS_PER_UNIT), 1);
      return expr;

    /* These cases continue the recursion:  */
    case COMPONENT_REF:
      oprnd0 = TREE_OPERAND (expr, 0);
      oprnd1 = TREE_OPERAND (expr, 1);

      this_offset = bit_position (oprnd1);
      if (vectype && !host_integerp (this_offset, 1))
        return NULL_TREE;
      next_ref = oprnd0;
      break;

    case ADDR_EXPR:
      oprnd0 = TREE_OPERAND (expr, 0);
      next_ref = oprnd0;
      break;

    case INDIRECT_REF:
      oprnd0 = TREE_OPERAND (expr, 0);
      next_ref = oprnd0;
      break;
    
    case ARRAY_REF:
      if (DR_REF (dr) != expr)
	/* Build array data_reference struct if the existing DR_REF 
	   doesn't match EXPR. This happens, for example, when the 
	   EXPR is *T and T is initialized to &arr[indx]. The DR struct
	   contains information on the access of T, not of arr. In order
	   to continue  the analysis, we create a new DR struct that
	   describes the access of arr.  
	*/
	array_dr = analyze_array (DR_STMT (dr), expr, DR_IS_READ (dr));
      else
	array_dr = dr;
	  
      next_ref = vect_compute_array_ref_alignment (array_dr, loop_vinfo,  
						   vectype, &this_offset);
      if (!next_ref)
	return NULL_TREE;

      if (vectype &&
	  TYPE_ALIGN (TREE_TYPE (TREE_TYPE (next_ref))) >= TYPE_ALIGN (vectype))
	{
	  *offset = this_offset;
	  *base_aligned_p = true;
	  return next_ref;
	}
      break;

    case PLUS_EXPR:
    case MINUS_EXPR:
      /* In case we have a PLUS_EXPR of the form
	 (oprnd0 + oprnd1), we assume that only oprnd0 determines the base. 
	 This is verified in  vect_get_symbl_and_dr.  */ 
      oprnd0 = TREE_OPERAND (expr, 0);
      oprnd1 = TREE_OPERAND (expr, 1);

      base = vect_get_base_and_bit_offset 
	(dr, oprnd1, vectype, loop_vinfo, &this_offset, base_aligned_p);  
      if (vectype && !base) 
	return NULL_TREE;

      next_ref = oprnd0;
      break;

    default:
      return NULL_TREE;
    }

  base = vect_get_base_and_bit_offset (dr, next_ref, vectype, 
				       loop_vinfo, offset, base_aligned_p);  

  if (vectype && base)
    {
      *offset = int_const_binop (PLUS_EXPR, *offset, this_offset, 1);
      if (!host_integerp (*offset, 1) || TREE_OVERFLOW (*offset))
        return NULL_TREE;

      if (vect_debug_details (NULL))
        {
          print_generic_expr (dump_file, expr, TDF_SLIM);
          fprintf (dump_file, " --> total offset for ref: ");
          print_generic_expr (dump_file, *offset, TDF_SLIM);
        }
    }    
  return base;
}



/* Function vect_force_dr_alignment_p.

   Returns whether the alignment of a DECL can be forced to be aligned
   on ALIGNMENT bit boundary.  */

static bool 
vect_can_force_dr_alignment_p (tree decl, unsigned int alignment)
{
  if (TREE_CODE (decl) != VAR_DECL)
    return false;

  if (DECL_EXTERNAL (decl))
    return false;

  if (TREE_STATIC (decl))
    return (alignment <= MAX_OFILE_ALIGNMENT);
  else
    /* This is not 100% correct.  The absolute correct stack alignment
       is STACK_BOUNDARY.  We're supposed to hope, but not assume, that
       PREFERRED_STACK_BOUNDARY is honored by all translation units.
       However, until someone implements forced stack alignment, SSE
       isn't really usable without this.  */  
    return (alignment <= PREFERRED_STACK_BOUNDARY); 
}


/* Function vect_get_new_vect_var.

   Returns a name for a new variable. The current naming scheme appends the 
   prefix "vect_" or "vect_p" (depending on the value of VAR_KIND) to 
   the name of vectorizer generated variables, and appends that to NAME if 
   provided.  */

static tree
vect_get_new_vect_var (tree type, enum vect_var_kind var_kind, const char *name)
{
  const char *prefix;
  int prefix_len;
  tree new_vect_var;

  if (var_kind == vect_simple_var)
    prefix = "vect_"; 
  else
    prefix = "vect_p";

  prefix_len = strlen (prefix);

  if (name)
    new_vect_var = create_tmp_var (type, concat (prefix, name, NULL));
  else
    new_vect_var = create_tmp_var (type, prefix);

  return new_vect_var;
}


/* Function vect_create_index_for_vector_ref.

   Create (and return) an index variable, along with it's update chain in the
   loop. This variable will be used to access a memory location in a vector
   operation.

   Input:
   LOOP: The loop being vectorized.
   BSI: The block_stmt_iterator where STMT is. Any new stmts created by this
        function can be added here, or in the loop pre-header.

   Output:
   Return an index that will be used to index a vector array.  It is expected
   that a pointer to the first vector will be used as the base address for the
   indexed reference.

   FORNOW: we are not trying to be efficient, just creating a new index each
   time from scratch.  At this time all vector references could use the same
   index.

   TODO: create only one index to be used by all vector references.  Record
   the index in the LOOP_VINFO the first time this procedure is called and
   return it on subsequent calls.  The increment of this index must be placed
   just before the conditional expression that ends the single block loop.  */

static tree
vect_create_index_for_vector_ref (struct loop *loop, block_stmt_iterator *bsi)
{
  tree init, step;
  tree indx_before_incr, indx_after_incr;

  /* It is assumed that the base pointer used for vectorized access contains
     the address of the first vector.  Therefore the index used for vectorized
     access must be initialized to zero and incremented by 1.  */

  init = integer_zero_node;
  step = integer_one_node;

  /* Assuming that bsi_insert is used with BSI_NEW_STMT  */
  create_iv (init, step, NULL_TREE, loop, bsi, false,
	&indx_before_incr, &indx_after_incr);

  return indx_before_incr;
}


/* Function vect_create_addr_base_for_vector_ref.

   Create an expression that computes the address of the first memory location
   that will be accessed for a data reference.

   Input:
   STMT: The statement containing the data reference.
   NEW_STMT_LIST: Must be initialized to NULL_TREE or a statement list.
   OFFSET: Optional. If supplied, it is be added to the initial address.

   Output:
   1. Return an SSA_NAME whose value is the address of the memory location of the
      first vector of the data reference.
   2. If new_stmt_list is not NULL_TREE after return then the caller must insert
      these statement(s) which define the returned SSA_NAME.

   FORNOW: We are only handling array accesses with step 1.  */

static tree
vect_create_addr_base_for_vector_ref (tree stmt,
                                      tree *new_stmt_list,
				      tree offset)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  struct loop *loop = STMT_VINFO_LOOP (stmt_info);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  tree data_ref_base = unshare_expr (STMT_VINFO_VECT_DR_BASE (stmt_info));
  tree base_name = unshare_expr (DR_BASE_NAME (dr));
  tree ref = DR_REF (dr);
  tree data_ref_base_type = TREE_TYPE (data_ref_base);
  tree scalar_type = TREE_TYPE (ref);
  tree scalar_ptr_type = build_pointer_type (scalar_type);
  tree access_fn;
  tree init_val, step, init_oval;
  bool ok;
  bool is_ptr_ref, is_array_ref, is_addr_expr;
  tree array_base;
  tree vec_stmt;
  tree new_temp;
  tree array_ref;
  tree addr_base, addr_expr;
  tree dest, new_stmt;

  /* Only the access function of the last index is relevant (i_n in
     a[i_1][i_2]...[i_n]), the others correspond to loop invariants. */
  access_fn = DR_ACCESS_FN (dr, 0);
  ok = vect_is_simple_iv_evolution (loop->num, access_fn, &init_oval, &step, true);
  if (!ok)
    init_oval = integer_zero_node;

  is_ptr_ref = TREE_CODE (data_ref_base_type) == POINTER_TYPE
	       && TREE_CODE (data_ref_base) == SSA_NAME;
  is_array_ref = TREE_CODE (data_ref_base_type) == ARRAY_TYPE
		 && (TREE_CODE (data_ref_base) == VAR_DECL
		     || TREE_CODE (data_ref_base) == COMPONENT_REF
		     || TREE_CODE (data_ref_base) == ARRAY_REF);
  is_addr_expr = TREE_CODE (data_ref_base) == ADDR_EXPR
                 || TREE_CODE (data_ref_base) == PLUS_EXPR
                 || TREE_CODE (data_ref_base) == MINUS_EXPR;
  gcc_assert (is_ptr_ref || is_array_ref || is_addr_expr);

  /** Create: &(base[init_val])

      if data_ref_base is an ARRAY_TYPE:
	 base = data_ref_base

      if data_ref_base is the SSA_NAME of a POINTER_TYPE:
	 base = *((scalar_array *) data_ref_base)
   **/

  if (is_array_ref)
    array_base = data_ref_base;
  else /* is_ptr_ref  or is_addr_expr */
    {
      /* array_ptr = (scalar_array_ptr_type *) data_ref_base;  */
      tree scalar_array_type = build_array_type (scalar_type, 0);
      tree scalar_array_ptr_type = build_pointer_type (scalar_array_type);
      tree array_ptr = create_tmp_var (scalar_array_ptr_type, "array_ptr");
      add_referenced_tmp_var (array_ptr);

      dest = create_tmp_var (TREE_TYPE (data_ref_base), "dataref");
      add_referenced_tmp_var (dest);
      data_ref_base = 
	force_gimple_operand (data_ref_base, &new_stmt, false, dest);  
      append_to_statement_list_force (new_stmt, new_stmt_list);

      vec_stmt = fold_convert (scalar_array_ptr_type, data_ref_base);
      vec_stmt = build2 (MODIFY_EXPR, void_type_node, array_ptr, vec_stmt);
      new_temp = make_ssa_name (array_ptr, vec_stmt);
      TREE_OPERAND (vec_stmt, 0) = new_temp;
      append_to_statement_list_force (vec_stmt, new_stmt_list);

      /* (*array_ptr)  */
      array_base = build_fold_indirect_ref (new_temp);
    }

  dest = create_tmp_var (TREE_TYPE (init_oval), "newinit");
  add_referenced_tmp_var (dest);
  init_val = force_gimple_operand (init_oval, &new_stmt, false, dest);  
  append_to_statement_list_force (new_stmt, new_stmt_list);

  if (offset)
    {
      tree tmp = create_tmp_var (TREE_TYPE (init_val), "offset");
      add_referenced_tmp_var (tmp);
      vec_stmt = build2 (PLUS_EXPR, TREE_TYPE (init_val), init_val, offset);
      vec_stmt = build2 (MODIFY_EXPR, TREE_TYPE (init_val), tmp, vec_stmt);
      init_val = make_ssa_name (tmp, vec_stmt);
      TREE_OPERAND (vec_stmt, 0) = init_val;
      append_to_statement_list_force (vec_stmt, new_stmt_list);
    }

  array_ref = build4 (ARRAY_REF, scalar_type, array_base, init_val, 
		      NULL_TREE, NULL_TREE);
  addr_base = build_fold_addr_expr (array_ref);

  /* addr_expr = addr_base */
  addr_expr = vect_get_new_vect_var (scalar_ptr_type, vect_pointer_var,
                                     get_name (base_name));
  add_referenced_tmp_var (addr_expr);
  vec_stmt = build2 (MODIFY_EXPR, void_type_node, addr_expr, addr_base);
  new_temp = make_ssa_name (addr_expr, vec_stmt);
  TREE_OPERAND (vec_stmt, 0) = new_temp;
  append_to_statement_list_force (vec_stmt, new_stmt_list);

  return new_temp;
}


/* Function get_vectype_for_scalar_type.

   Returns the vector type corresponding to SCALAR_TYPE as supported
   by the target.  */

static tree
get_vectype_for_scalar_type (tree scalar_type)
{
  enum machine_mode inner_mode = TYPE_MODE (scalar_type);
  int nbytes = GET_MODE_SIZE (inner_mode);
  int nunits;
  tree vectype;

  if (nbytes == 0)
    return NULL_TREE;

  /* FORNOW: Only a single vector size per target (UNITS_PER_SIMD_WORD)
     is expected.  */
  nunits = UNITS_PER_SIMD_WORD / nbytes;

  vectype = build_vector_type (scalar_type, nunits);
  if (TYPE_MODE (vectype) == BLKmode)
    return NULL_TREE;
  return vectype;
}


/* Function vect_align_data_ref.

   Handle mislignment of a memory accesses.

   FORNOW: Can't handle misaligned accesses. 
   Make sure that the dataref is aligned.  */

static void
vect_align_data_ref (tree stmt)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);

  /* FORNOW: can't handle misaligned accesses; 
             all accesses expected to be aligned.  */
  gcc_assert (aligned_access_p (dr));
}


/* Function vect_create_data_ref_ptr.

   Create a memory reference expression for vector access, to be used in a
   vector load/store stmt. The reference is based on a new pointer to vector
   type (vp).

   Input:
   1. STMT: a stmt that references memory. Expected to be of the form
         MODIFY_EXPR <name, data-ref> or MODIFY_EXPR <data-ref, name>.
   2. BSI: block_stmt_iterator where new stmts can be added.
   3. OFFSET (optional): an offset to be added to the initial address accessed
        by the data-ref in STMT.
   4. ONLY_INIT: indicate if vp is to be updated in the loop, or remain
        pointing to the initial address.

   Output:
   1. Declare a new ptr to vector_type, and have it point to the base of the
      data reference (initial addressed accessed by the data reference).
      For example, for vector of type V8HI, the following code is generated:

      v8hi *vp;
      vp = (v8hi *)initial_address;

      if OFFSET is not supplied:
         initial_address = &a[init];
      if OFFSET is supplied:
         initial_address = &a[init + OFFSET];

      Return the initial_address in INITIAL_ADDRESS.

   2. Create a data-reference in the loop based on the new vector pointer vp,
      and using a new index variable 'idx' as follows:

      vp' = vp + update

      where if ONLY_INIT is true:
         update = zero
      and otherwise
         update = idx + vector_type_size

      Return the pointer vp'.


   FORNOW: handle only aligned and consecutive accesses.  */

static tree
vect_create_data_ref_ptr (tree stmt, block_stmt_iterator *bsi, tree offset,
                          tree *initial_address, bool only_init)
{
  tree base_name;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  struct loop *loop = STMT_VINFO_LOOP (stmt_info);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree vect_ptr_type;
  tree vect_ptr;
  tree tag;
  v_may_def_optype v_may_defs = STMT_V_MAY_DEF_OPS (stmt);
  v_must_def_optype v_must_defs = STMT_V_MUST_DEF_OPS (stmt);
  vuse_optype vuses = STMT_VUSE_OPS (stmt);
  int nvuses, nv_may_defs, nv_must_defs;
  int i;
  tree new_temp;
  tree vec_stmt;
  tree new_stmt_list = NULL_TREE;
  tree idx;
  edge pe = loop_preheader_edge (loop);
  basic_block new_bb;
  tree vect_ptr_init;
  tree vectype_size;
  tree ptr_update;
  tree data_ref_ptr;

  base_name = unshare_expr (DR_BASE_NAME (dr));
  if (vect_debug_details (NULL))
    {
      tree data_ref_base = base_name;
      fprintf (dump_file, "create array_ref of type: ");
      print_generic_expr (dump_file, vectype, TDF_SLIM);
      if (TREE_CODE (data_ref_base) == VAR_DECL)
        fprintf (dump_file, "vectorizing a one dimensional array ref: ");
      else if (TREE_CODE (data_ref_base) == ARRAY_REF)
        fprintf (dump_file, "vectorizing a multidimensional array ref: ");
      else if (TREE_CODE (data_ref_base) == COMPONENT_REF)
        fprintf (dump_file, "vectorizing a record based array ref: ");
      else if (TREE_CODE (data_ref_base) == SSA_NAME)
        fprintf (dump_file, "vectorizing a pointer ref: ");
      print_generic_expr (dump_file, base_name, TDF_SLIM);
    }

  /** (1) Create the new vector-pointer variable:  **/

  vect_ptr_type = build_pointer_type (vectype);
  vect_ptr = vect_get_new_vect_var (vect_ptr_type, vect_pointer_var,
                                    get_name (base_name));
  add_referenced_tmp_var (vect_ptr);
  
  
  /** (2) Handle aliasing information of the new vector-pointer:  **/
  
  tag = STMT_VINFO_MEMTAG (stmt_info);
  gcc_assert (tag);
  get_var_ann (vect_ptr)->type_mem_tag = tag;
  
  /* Mark for renaming all aliased variables
     (i.e, the may-aliases of the type-mem-tag).  */
  nvuses = NUM_VUSES (vuses);
  nv_may_defs = NUM_V_MAY_DEFS (v_may_defs);
  nv_must_defs = NUM_V_MUST_DEFS (v_must_defs);
  for (i = 0; i < nvuses; i++)
    {
      tree use = VUSE_OP (vuses, i);
      if (TREE_CODE (use) == SSA_NAME)
        bitmap_set_bit (vars_to_rename, var_ann (SSA_NAME_VAR (use))->uid);
    }
  for (i = 0; i < nv_may_defs; i++)
    {
      tree def = V_MAY_DEF_RESULT (v_may_defs, i);
      if (TREE_CODE (def) == SSA_NAME)
        bitmap_set_bit (vars_to_rename, var_ann (SSA_NAME_VAR (def))->uid);
    }
  for (i = 0; i < nv_must_defs; i++)
    {
      tree def = V_MUST_DEF_OP (v_must_defs, i);
      if (TREE_CODE (def) == SSA_NAME)
        bitmap_set_bit (vars_to_rename, var_ann (SSA_NAME_VAR (def))->uid);
    }


  /** (3) Calculate the initial address the vector-pointer, and set
          the vector-pointer to point to it before the loop:  **/

  /* Create: (&(base[init_val+offset]) in the loop preheader.  */
  new_temp = vect_create_addr_base_for_vector_ref (stmt, &new_stmt_list,
                                                   offset);
  pe = loop_preheader_edge (loop);
  new_bb = bsi_insert_on_edge_immediate (pe, new_stmt_list);
  gcc_assert (!new_bb);
  *initial_address = new_temp;

  /* Create: p = (vectype *) initial_base  */
  vec_stmt = fold_convert (vect_ptr_type, new_temp);
  vec_stmt = build2 (MODIFY_EXPR, void_type_node, vect_ptr, vec_stmt);
  new_temp = make_ssa_name (vect_ptr, vec_stmt);
  TREE_OPERAND (vec_stmt, 0) = new_temp;
  new_bb = bsi_insert_on_edge_immediate (pe, vec_stmt);
  gcc_assert (!new_bb);
  vect_ptr_init = TREE_OPERAND (vec_stmt, 0);


  /** (4) Handle the updating of the vector-pointer inside the loop: **/

  if (only_init) /* No update in loop is required.  */
    return vect_ptr_init;

  idx = vect_create_index_for_vector_ref (loop, bsi);

  /* Create: update = idx * vectype_size  */
  ptr_update = create_tmp_var (integer_type_node, "update");
  add_referenced_tmp_var (ptr_update);
  vectype_size = build_int_cst (integer_type_node,
                                GET_MODE_SIZE (TYPE_MODE (vectype)));
  vec_stmt = build2 (MULT_EXPR, integer_type_node, idx, vectype_size);
  vec_stmt = build2 (MODIFY_EXPR, void_type_node, ptr_update, vec_stmt);
  new_temp = make_ssa_name (ptr_update, vec_stmt);
  TREE_OPERAND (vec_stmt, 0) = new_temp;
  bsi_insert_before (bsi, vec_stmt, BSI_SAME_STMT);

  /* Create: data_ref_ptr = vect_ptr_init + update  */
  vec_stmt = build2 (PLUS_EXPR, vect_ptr_type, vect_ptr_init, new_temp);
  vec_stmt = build2 (MODIFY_EXPR, void_type_node, vect_ptr, vec_stmt);
  new_temp = make_ssa_name (vect_ptr, vec_stmt);
  TREE_OPERAND (vec_stmt, 0) = new_temp;
  bsi_insert_before (bsi, vec_stmt, BSI_SAME_STMT);
  data_ref_ptr = TREE_OPERAND (vec_stmt, 0);

  return data_ref_ptr;
}


/* Function vect_create_destination_var.

   Create a new temporary of type VECTYPE.  */

static tree
vect_create_destination_var (tree scalar_dest, tree vectype)
{
  tree vec_dest;
  const char *new_name;

  gcc_assert (TREE_CODE (scalar_dest) == SSA_NAME);

  new_name = get_name (scalar_dest);
  if (!new_name)
    new_name = "var_";
  vec_dest = vect_get_new_vect_var (vectype, vect_simple_var, new_name);
  add_referenced_tmp_var (vec_dest);

  return vec_dest;
}


/* Function vect_init_vector.

   Insert a new stmt (INIT_STMT) that initializes a new vector variable with
   the vector elements of VECTOR_VAR. Return the DEF of INIT_STMT. It will be
   used in the vectorization of STMT.  */

static tree
vect_init_vector (tree stmt, tree vector_var)
{
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  struct loop *loop = STMT_VINFO_LOOP (stmt_vinfo);
  tree new_var;
  tree init_stmt;
  tree vectype = STMT_VINFO_VECTYPE (stmt_vinfo); 
  tree vec_oprnd;
  edge pe;
  tree new_temp;
  basic_block new_bb;
 
  new_var = vect_get_new_vect_var (vectype, vect_simple_var, "cst_");
  add_referenced_tmp_var (new_var); 
 
  init_stmt = build2 (MODIFY_EXPR, vectype, new_var, vector_var);
  new_temp = make_ssa_name (new_var, init_stmt);
  TREE_OPERAND (init_stmt, 0) = new_temp;

  pe = loop_preheader_edge (loop);
  new_bb = bsi_insert_on_edge_immediate (pe, init_stmt);
  gcc_assert (!new_bb);

  if (vect_debug_details (NULL))
    {
      fprintf (dump_file, "created new init_stmt: ");
      print_generic_expr (dump_file, init_stmt, TDF_SLIM);
    }

  vec_oprnd = TREE_OPERAND (init_stmt, 0);
  return vec_oprnd;
}


/* Function vect_get_vec_def_for_operand.

   OP is an operand in STMT. This function returns a (vector) def that will be
   used in the vectorized stmt for STMT.

   In the case that OP is an SSA_NAME which is defined in the loop, then
   STMT_VINFO_VEC_STMT of the defining stmt holds the relevant def.

   In case OP is an invariant or constant, a new stmt that creates a vector def
   needs to be introduced.  */

static tree
vect_get_vec_def_for_operand (tree op, tree stmt)
{
  tree vec_oprnd;
  tree vec_stmt;
  tree def_stmt;
  stmt_vec_info def_stmt_info = NULL;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_vinfo);
  int nunits = GET_MODE_NUNITS (TYPE_MODE (vectype));
  struct loop *loop = STMT_VINFO_LOOP (stmt_vinfo);
  basic_block bb;
  tree vec_inv;
  tree t = NULL_TREE;
  tree def;
  int i;

  if (vect_debug_details (NULL))
    {
      fprintf (dump_file, "vect_get_vec_def_for_operand: ");
      print_generic_expr (dump_file, op, TDF_SLIM);
    }

  /** ===> Case 1: operand is a constant.  **/

  if (TREE_CODE (op) == INTEGER_CST || TREE_CODE (op) == REAL_CST)
    {
      /* Create 'vect_cst_ = {cst,cst,...,cst}'  */

      tree vec_cst;
      stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
      tree vectype = STMT_VINFO_VECTYPE (stmt_vinfo);
      int nunits = GET_MODE_NUNITS (TYPE_MODE (vectype));
      tree t = NULL_TREE;
      int i;

      /* Build a tree with vector elements.  */
      if (vect_debug_details (NULL))
        fprintf (dump_file, "Create vector_cst. nunits = %d", nunits);

      for (i = nunits - 1; i >= 0; --i)
        {
          t = tree_cons (NULL_TREE, op, t);
        }
      vec_cst = build_vector (vectype, t);
      return vect_init_vector (stmt, vec_cst);
    }

  gcc_assert (TREE_CODE (op) == SSA_NAME);
 
  /** ===> Case 2: operand is an SSA_NAME - find the stmt that defines it.  **/

  def_stmt = SSA_NAME_DEF_STMT (op);
  def_stmt_info = vinfo_for_stmt (def_stmt);

  if (vect_debug_details (NULL))
    {
      fprintf (dump_file, "vect_get_vec_def_for_operand: def_stmt: ");
      print_generic_expr (dump_file, def_stmt, TDF_SLIM);
    }


  /** ==> Case 2.1: operand is defined inside the loop.  **/

  if (def_stmt_info)
    {
      /* Get the def from the vectorized stmt.  */

      vec_stmt = STMT_VINFO_VEC_STMT (def_stmt_info);
      gcc_assert (vec_stmt);
      vec_oprnd = TREE_OPERAND (vec_stmt, 0);
      return vec_oprnd;
    }


  /** ==> Case 2.2: operand is defined by the loop-header phi-node - 
                    it is a reduction/induction.  **/

  bb = bb_for_stmt (def_stmt);
  if (TREE_CODE (def_stmt) == PHI_NODE && flow_bb_inside_loop_p (loop, bb))
    {
      if (vect_debug_details (NULL))
	fprintf (dump_file, "reduction/induction - unsupported.");
      internal_error ("no support for reduction/induction"); /* FORNOW */
    }


  /** ==> Case 2.3: operand is defined outside the loop - 
                    it is a loop invariant.  */

  switch (TREE_CODE (def_stmt))
    {
    case PHI_NODE:
      def = PHI_RESULT (def_stmt);
      break;
    case MODIFY_EXPR:
      def = TREE_OPERAND (def_stmt, 0);
      break;
    case NOP_EXPR:
      def = TREE_OPERAND (def_stmt, 0);
      gcc_assert (IS_EMPTY_STMT (def_stmt));
      def = op;
      break;
    default:
      if (vect_debug_details (NULL))
	{
          fprintf (dump_file, "unsupported defining stmt: ");
	  print_generic_expr (dump_file, def_stmt, TDF_SLIM);
	}
      internal_error ("unsupported defining stmt");
    }

  /* Build a tree with vector elements. Create 'vec_inv = {inv,inv,..,inv}'  */

  if (vect_debug_details (NULL))
    fprintf (dump_file, "Create vector_inv.");

  for (i = nunits - 1; i >= 0; --i)
    {
      t = tree_cons (NULL_TREE, def, t);
    }

  vec_inv = build_constructor (vectype, t);
  return vect_init_vector (stmt, vec_inv);
}


/* Function vect_finish_stmt_generation.

   Insert a new stmt.  */

static void
vect_finish_stmt_generation (tree stmt, tree vec_stmt, block_stmt_iterator *bsi)
{
  bsi_insert_before (bsi, vec_stmt, BSI_SAME_STMT);

  if (vect_debug_details (NULL))
    {
      fprintf (dump_file, "add new stmt: ");
      print_generic_expr (dump_file, vec_stmt, TDF_SLIM);
    }

  /* Make sure bsi points to the stmt that is being vectorized.  */

  /* Assumption: any stmts created for the vectorization of stmt S were
     inserted before S. BSI is expected to point to S or some new stmt before S.  */

  while (stmt != bsi_stmt (*bsi) && !bsi_end_p (*bsi))
    bsi_next (bsi);
  gcc_assert (stmt == bsi_stmt (*bsi));
}


/* Function vectorizable_assignment.

   Check if STMT performs an assignment (copy) that can be vectorized. 
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized 
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

static bool
vectorizable_assignment (tree stmt, block_stmt_iterator *bsi, tree *vec_stmt)
{
  tree vec_dest;
  tree scalar_dest;
  tree op;
  tree vec_oprnd;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  struct loop *loop = STMT_VINFO_LOOP (stmt_info);
  tree new_temp;

  /* Is vectorizable assignment?  */

  if (TREE_CODE (stmt) != MODIFY_EXPR)
    return false;

  scalar_dest = TREE_OPERAND (stmt, 0);
  if (TREE_CODE (scalar_dest) != SSA_NAME)
    return false;

  op = TREE_OPERAND (stmt, 1);
  if (!vect_is_simple_use (op, loop, NULL))
    {
      if (vect_debug_details (NULL))
        fprintf (dump_file, "use not simple.");
      return false;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = assignment_vec_info_type;
      return true;
    }

  /** Trasform.  **/
  if (vect_debug_details (NULL))
    fprintf (dump_file, "transform assignment.");

  /* Handle def.  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

  /* Handle use.  */
  op = TREE_OPERAND (stmt, 1);
  vec_oprnd = vect_get_vec_def_for_operand (op, stmt);

  /* Arguments are ready. create the new vector stmt.  */
  *vec_stmt = build2 (MODIFY_EXPR, vectype, vec_dest, vec_oprnd);
  new_temp = make_ssa_name (vec_dest, *vec_stmt);
  TREE_OPERAND (*vec_stmt, 0) = new_temp;
  vect_finish_stmt_generation (stmt, *vec_stmt, bsi);
  
  return true;
}


/* Function vectorizable_operation.

   Check if STMT performs a binary or unary operation that can be vectorized. 
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized 
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

static bool
vectorizable_operation (tree stmt, block_stmt_iterator *bsi, tree *vec_stmt)
{
  tree vec_dest;
  tree scalar_dest;
  tree operation;
  tree op0, op1 = NULL;
  tree vec_oprnd0, vec_oprnd1=NULL;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  struct loop *loop = STMT_VINFO_LOOP (stmt_info);
  int i;
  enum tree_code code;
  enum machine_mode vec_mode;
  tree new_temp;
  int op_type;
  tree op;
  optab optab;

  /* Is STMT a vectorizable binary/unary operation?   */
  if (TREE_CODE (stmt) != MODIFY_EXPR)
    return false;

  if (TREE_CODE (TREE_OPERAND (stmt, 0)) != SSA_NAME)
    return false;

  operation = TREE_OPERAND (stmt, 1);
  code = TREE_CODE (operation);
  optab = optab_for_tree_code (code, vectype);

  /* Support only unary or binary operations.  */
  op_type = TREE_CODE_LENGTH (code);
  if (op_type != unary_op && op_type != binary_op)
    {
      if (vect_debug_details (NULL))
	fprintf (dump_file, "num. args = %d (not unary/binary op).", op_type);
      return false;
    }

  for (i = 0; i < op_type; i++)
    {
      op = TREE_OPERAND (operation, i);
      if (!vect_is_simple_use (op, loop, NULL))
	{
	  if (vect_debug_details (NULL))
	    fprintf (dump_file, "use not simple.");
	  return false;
	}	
    } 

  /* Supportable by target?  */
  if (!optab)
    {
      if (vect_debug_details (NULL))
	fprintf (dump_file, "no optab.");
      return false;
    }
  vec_mode = TYPE_MODE (vectype);
  if (optab->handlers[(int) vec_mode].insn_code == CODE_FOR_nothing)
    {
      if (vect_debug_details (NULL))
	fprintf (dump_file, "op not supported by target.");
      return false;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = op_vec_info_type;
      return true;
    }

  /** Trasform.  **/

  if (vect_debug_details (NULL))
    fprintf (dump_file, "transform binary/unary operation.");

  /* Handle def.  */
  scalar_dest = TREE_OPERAND (stmt, 0);
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

  /* Handle uses.  */
  op0 = TREE_OPERAND (operation, 0);
  vec_oprnd0 = vect_get_vec_def_for_operand (op0, stmt);

  if (op_type == binary_op)
    {
      op1 = TREE_OPERAND (operation, 1);
      vec_oprnd1 = vect_get_vec_def_for_operand (op1, stmt); 
    }

  /* Arguments are ready. create the new vector stmt.  */

  if (op_type == binary_op)
    *vec_stmt = build2 (MODIFY_EXPR, vectype, vec_dest,
		build2 (code, vectype, vec_oprnd0, vec_oprnd1));
  else
    *vec_stmt = build2 (MODIFY_EXPR, vectype, vec_dest,
		build1 (code, vectype, vec_oprnd0));
  new_temp = make_ssa_name (vec_dest, *vec_stmt);
  TREE_OPERAND (*vec_stmt, 0) = new_temp;
  vect_finish_stmt_generation (stmt, *vec_stmt, bsi);

  return true;
}


/* Function vectorizable_store.

   Check if STMT defines a non scalar data-ref (array/pointer/structure) that 
   can be vectorized. 
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized 
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

static bool
vectorizable_store (tree stmt, block_stmt_iterator *bsi, tree *vec_stmt)
{
  tree scalar_dest;
  tree data_ref;
  tree op;
  tree vec_oprnd1;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  struct loop *loop = STMT_VINFO_LOOP (stmt_info);
  enum machine_mode vec_mode;
  tree dummy;

  /* Is vectorizable store? */

  if (TREE_CODE (stmt) != MODIFY_EXPR)
    return false;

  scalar_dest = TREE_OPERAND (stmt, 0);
  if (TREE_CODE (scalar_dest) != ARRAY_REF
      && TREE_CODE (scalar_dest) != INDIRECT_REF)
    return false;

  op = TREE_OPERAND (stmt, 1);
  if (!vect_is_simple_use (op, loop, NULL))
    {
      if (vect_debug_details (NULL))
        fprintf (dump_file, "use not simple.");
      return false;
    }

  vec_mode = TYPE_MODE (vectype);
  /* FORNOW. In some cases can vectorize even if data-type not supported
     (e.g. - array initialization with 0).  */
  if (mov_optab->handlers[(int)vec_mode].insn_code == CODE_FOR_nothing)
    return false;

  if (!STMT_VINFO_DATA_REF (stmt_info))
    return false;

  if (!aligned_access_p (STMT_VINFO_DATA_REF (stmt_info)))
    return false;

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = store_vec_info_type;
      return true;
    }

  /** Trasform.  **/

  if (vect_debug_details (NULL))
    fprintf (dump_file, "transform store");

  /* Handle use - get the vectorized def from the defining stmt.  */
  vec_oprnd1 = vect_get_vec_def_for_operand (op, stmt);

  /* Handle def.  */
  /* FORNOW: make sure the data reference is aligned.  */
  vect_align_data_ref (stmt);
  data_ref = vect_create_data_ref_ptr (stmt, bsi, NULL_TREE, &dummy, false);
  data_ref = build_fold_indirect_ref (data_ref);

  /* Arguments are ready. create the new vector stmt.  */
  *vec_stmt = build2 (MODIFY_EXPR, vectype, data_ref, vec_oprnd1);
  vect_finish_stmt_generation (stmt, *vec_stmt, bsi);

  return true;
}


/* vectorizable_load.

   Check if STMT reads a non scalar data-ref (array/pointer/structure) that 
   can be vectorized. 
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized 
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

static bool
vectorizable_load (tree stmt, block_stmt_iterator *bsi, tree *vec_stmt)
{
  tree scalar_dest;
  tree vec_dest = NULL;
  tree data_ref = NULL;
  tree op;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree new_temp;
  int mode;
  tree init_addr;
  tree new_stmt;
  tree dummy;
  basic_block new_bb;
  struct loop *loop = STMT_VINFO_LOOP (stmt_info);
  edge pe = loop_preheader_edge (loop);
  bool software_pipeline_loads_p = false;

  /* Is vectorizable load? */

  if (TREE_CODE (stmt) != MODIFY_EXPR)
    return false;

  scalar_dest = TREE_OPERAND (stmt, 0);
  if (TREE_CODE (scalar_dest) != SSA_NAME)
    return false;

  op = TREE_OPERAND (stmt, 1);
  if (TREE_CODE (op) != ARRAY_REF && TREE_CODE (op) != INDIRECT_REF)
    return false;

  if (!STMT_VINFO_DATA_REF (stmt_info))
    return false;

  mode = (int) TYPE_MODE (vectype);

  /* FORNOW. In some cases can vectorize even if data-type not supported
    (e.g. - data copies).  */
  if (mov_optab->handlers[mode].insn_code == CODE_FOR_nothing)
    {
      if (vect_debug_details (loop))
	fprintf (dump_file, "Aligned load, but unsupported type.");
      return false;
    }

  if (!aligned_access_p (dr))
    {
      if (vec_realign_load_optab->handlers[mode].insn_code != CODE_FOR_nothing
	  && (!targetm.vectorize.builtin_mask_for_load
	      || targetm.vectorize.builtin_mask_for_load ()))
	software_pipeline_loads_p = true;
      else if (!targetm.vectorize.misaligned_mem_ok (mode))
	{
	  /* Possibly unaligned access, and can't software pipeline the loads  */
	  if (vect_debug_details (loop))
	    fprintf (dump_file, "Arbitrary load not supported.");
	  return false;
	}
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = load_vec_info_type;
      return true;
    }

  /** Trasform.  **/

  if (vect_debug_details (NULL))
    fprintf (dump_file, "transform load.");

  if (!software_pipeline_loads_p)
    {
      /* Create:
         p = initial_addr;
         indx = 0;
         loop {
           vec_dest = *(p);
           indx = indx + 1;
         }
      */

      vec_dest = vect_create_destination_var (scalar_dest, vectype);
      data_ref = vect_create_data_ref_ptr (stmt, bsi, NULL_TREE, &dummy, false);
      if (aligned_access_p (dr))
        data_ref = build_fold_indirect_ref (data_ref);
      else
	{
	  int mis = DR_MISALIGNMENT (dr);
	  tree tmis = (mis == -1 ?
		       integer_zero_node : 
		       build_int_cst (integer_type_node, mis));
	  tmis = int_const_binop (MULT_EXPR, tmis, 
			build_int_cst (integer_type_node, BITS_PER_UNIT), 1);
	  data_ref = build2 (MISALIGNED_INDIRECT_REF, vectype, data_ref, tmis);
	}
      new_stmt = build2 (MODIFY_EXPR, vectype, vec_dest, data_ref);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      TREE_OPERAND (new_stmt, 0) = new_temp;
      vect_finish_stmt_generation (stmt, new_stmt, bsi);
    }
  else /* software-pipeline the loads  */
    {
      /* Create:
	 p1 = initial_addr;
	 msq_init = *(floor(p1))
	 p2 = initial_addr + VS - 1;
	 magic = have_builtin ? builtin_result : initial_address;
	 indx = 0;
	 loop {
	   p2' = p2 + indx * vectype_size
	   lsq = *(floor(p2'))
	   vec_dest = realign_load (msq, lsq, magic)
	   indx = indx + 1;
	   msq = lsq;
	 }
      */

      tree offset;
      tree magic;
      tree phi_stmt;
      tree msq_init;
      tree msq, lsq;
      tree dataref_ptr;
      tree params;

      /* <1> Create msq_init = *(floor(p1)) in the loop preheader  */
      vec_dest = vect_create_destination_var (scalar_dest, vectype);
      data_ref = vect_create_data_ref_ptr (stmt, bsi, NULL_TREE, 
					   &init_addr, true);
      data_ref = build1 (ALIGN_INDIRECT_REF, vectype, data_ref);
      new_stmt = build2 (MODIFY_EXPR, vectype, vec_dest, data_ref);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      TREE_OPERAND (new_stmt, 0) = new_temp;
      new_bb = bsi_insert_on_edge_immediate (pe, new_stmt);
      gcc_assert (!new_bb);
      msq_init = TREE_OPERAND (new_stmt, 0);


      /* <2> Create lsq = *(floor(p2')) in the loop  */ 
      offset = build_int_cst (integer_type_node, 
			      GET_MODE_NUNITS (TYPE_MODE (vectype)));
      offset = int_const_binop (MINUS_EXPR, offset, integer_one_node, 1);
      vec_dest = vect_create_destination_var (scalar_dest, vectype);
      dataref_ptr = vect_create_data_ref_ptr (stmt, bsi, offset, &dummy, false);
      data_ref = build1 (ALIGN_INDIRECT_REF, vectype, dataref_ptr);
      new_stmt = build2 (MODIFY_EXPR, vectype, vec_dest, data_ref);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      TREE_OPERAND (new_stmt, 0) = new_temp;
      vect_finish_stmt_generation (stmt, new_stmt, bsi);
      lsq = TREE_OPERAND (new_stmt, 0);


      /* <3> */
      if (targetm.vectorize.builtin_mask_for_load)
	{
	  /* Create permutation mask, if required, in loop preheader.  */
	  tree builtin_decl;
	  params = build_tree_list (NULL_TREE, init_addr);
	  vec_dest = vect_create_destination_var (scalar_dest, vectype);
	  builtin_decl = targetm.vectorize.builtin_mask_for_load ();
	  new_stmt = build_function_call_expr (builtin_decl, params);
	  new_stmt = build2 (MODIFY_EXPR, vectype, vec_dest, new_stmt);
	  new_temp = make_ssa_name (vec_dest, new_stmt);
	  TREE_OPERAND (new_stmt, 0) = new_temp;
	  new_bb = bsi_insert_on_edge_immediate (pe, new_stmt);
	  gcc_assert (!new_bb);
	  magic = TREE_OPERAND (new_stmt, 0);
	}
      else
	{
	  /* Use current address instead of init_addr for reduced reg pressure.  */
	  magic = dataref_ptr;
	}


      /* <4> Create msq = phi <msq_init, lsq> in loop  */ 
      vec_dest = vect_create_destination_var (scalar_dest, vectype);
      msq = make_ssa_name (vec_dest, NULL_TREE);
      phi_stmt = create_phi_node (msq, loop->header); /* CHECKME */
      SSA_NAME_DEF_STMT (msq) = phi_stmt;
      add_phi_arg (&phi_stmt, msq_init, loop_preheader_edge (loop));
      add_phi_arg (&phi_stmt, lsq, loop_latch_edge (loop));


      /* <5> Create <vec_dest = realign_load (msq, lsq, magic)> in loop  */
      vec_dest = vect_create_destination_var (scalar_dest, vectype);
      new_stmt = build3 (REALIGN_LOAD_EXPR, vectype, msq, lsq, magic);
      new_stmt = build2 (MODIFY_EXPR, vectype, vec_dest, new_stmt);
      new_temp = make_ssa_name (vec_dest, new_stmt); 
      TREE_OPERAND (new_stmt, 0) = new_temp;
      vect_finish_stmt_generation (stmt, new_stmt, bsi);
    }

  *vec_stmt = new_stmt;
  return true;
}


/* Function vect_transform_stmt.

   Create a vectorized stmt to replace STMT, and insert it at BSI.  */

static bool
vect_transform_stmt (tree stmt, block_stmt_iterator *bsi)
{
  bool is_store = false;
  tree vec_stmt = NULL_TREE;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  bool done;

  switch (STMT_VINFO_TYPE (stmt_info))
    {
    case op_vec_info_type:
      done = vectorizable_operation (stmt, bsi, &vec_stmt);
      gcc_assert (done);
      break;

    case assignment_vec_info_type:
      done = vectorizable_assignment (stmt, bsi, &vec_stmt);
      gcc_assert (done);
      break;

    case load_vec_info_type:
      done = vectorizable_load (stmt, bsi, &vec_stmt);
      gcc_assert (done);
      break;

    case store_vec_info_type:
      done = vectorizable_store (stmt, bsi, &vec_stmt);
      gcc_assert (done);
      is_store = true;
      break;
    default:
      if (vect_debug_details (NULL))
        fprintf (dump_file, "stmt not supported.");
      gcc_unreachable ();
    }

  STMT_VINFO_VEC_STMT (stmt_info) = vec_stmt;

  return is_store;
}


/* Function vect_transform_loop_bound.

   Create a new exit condition for the loop.  */

static void
vect_transform_loop_bound (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  edge exit_edge = loop->single_exit;
  block_stmt_iterator loop_exit_bsi = bsi_last (exit_edge->src);
  tree indx_before_incr, indx_after_incr;
  tree orig_cond_expr;
  HOST_WIDE_INT old_N = 0;
  int vf;
  tree cond_stmt;
  tree new_loop_bound;
  tree cond;
  tree lb_type;

  gcc_assert (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo));
  old_N = LOOP_VINFO_NITERS (loop_vinfo);
  vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);

  /* FORNOW: 
     assuming number-of-iterations divides by the vectorization factor.  */
  gcc_assert (!(old_N % vf));

  orig_cond_expr = LOOP_VINFO_EXIT_COND (loop_vinfo);
  gcc_assert (orig_cond_expr);
  gcc_assert (orig_cond_expr == bsi_stmt (loop_exit_bsi));

  create_iv (integer_zero_node, integer_one_node, NULL_TREE, loop, 
	     &loop_exit_bsi, false, &indx_before_incr, &indx_after_incr);

  /* bsi_insert is using BSI_NEW_STMT. We need to bump it back 
     to point to the exit condition.  */
  bsi_next (&loop_exit_bsi);
  gcc_assert (bsi_stmt (loop_exit_bsi) == orig_cond_expr);

  /* new loop exit test:  */
  lb_type = TREE_TYPE (TREE_OPERAND (TREE_OPERAND (orig_cond_expr, 0), 1));
  new_loop_bound = build_int_cst (lb_type, old_N/vf);

  if (exit_edge->flags & EDGE_TRUE_VALUE) /* 'then' edge exits the loop.  */
    cond = build2 (GE_EXPR, boolean_type_node, indx_after_incr, new_loop_bound);
  else /* 'then' edge loops back.   */
    cond = build2 (LT_EXPR, boolean_type_node, indx_after_incr, new_loop_bound);

  cond_stmt = build3 (COND_EXPR, TREE_TYPE (orig_cond_expr), cond,
	TREE_OPERAND (orig_cond_expr, 1), TREE_OPERAND (orig_cond_expr, 2));

  bsi_insert_before (&loop_exit_bsi, cond_stmt, BSI_SAME_STMT);   

  /* remove old loop exit test:  */
  bsi_remove (&loop_exit_bsi);

  if (vect_debug_details (NULL))
    print_generic_expr (dump_file, cond_stmt, TDF_SLIM);
}


/* Function vect_transform_loop.

   The analysis phase has determined that the loop is vectorizable.
   Vectorize the loop - created vectorized stmts to replace the scalar
   stmts in the loop, and update the loop exit condition.  */

static void
vect_transform_loop (loop_vec_info loop_vinfo, 
		     struct loops *loops ATTRIBUTE_UNUSED)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  block_stmt_iterator si;
  int i;
#ifdef ENABLE_CHECKING
  int vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
#endif

  if (vect_debug_details (NULL))
    fprintf (dump_file, "\n<<vec_transform_loop>>\n");

  /* 1) Make sure the loop header has exactly two entries
     2) Make sure we have a preheader basic block.  */

  gcc_assert (EDGE_COUNT (loop->header->preds) == 2);

  loop_split_edge_with (loop_preheader_edge (loop), NULL);


  /* FORNOW: the vectorizer supports only loops which body consist
     of one basic block (header + empty latch). When the vectorizer will 
     support more involved loop forms, the order by which the BBs are 
     traversed need to be reconsidered.  */

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];

      for (si = bsi_start (bb); !bsi_end_p (si);)
	{
	  tree stmt = bsi_stmt (si);
	  stmt_vec_info stmt_info;
	  bool is_store;
#ifdef ENABLE_CHECKING
	  tree vectype;
#endif

	  if (vect_debug_details (NULL))
	    {
	      fprintf (dump_file, "------>vectorizing statement: ");
	      print_generic_expr (dump_file, stmt, TDF_SLIM);
	    }	
	  stmt_info = vinfo_for_stmt (stmt);
	  gcc_assert (stmt_info);
	  if (!STMT_VINFO_RELEVANT_P (stmt_info))
	    {
	      bsi_next (&si);
	      continue;
	    }
#ifdef ENABLE_CHECKING
	  /* FORNOW: Verify that all stmts operate on the same number of
	             units and no inner unrolling is necessary.  */
	  vectype = STMT_VINFO_VECTYPE (stmt_info);
	  gcc_assert (GET_MODE_NUNITS (TYPE_MODE (vectype))
		      == vectorization_factor);
#endif
	  /* -------- vectorize statement ------------ */
	  if (vect_debug_details (NULL))
	    fprintf (dump_file, "transform statement.");

	  is_store = vect_transform_stmt (stmt, &si);
	  if (is_store)
	    {
	      /* free the attached stmt_vec_info and remove the stmt.  */
	      stmt_ann_t ann = stmt_ann (stmt);
	      free (stmt_info);
	      set_stmt_info (ann, NULL);
	      bsi_remove (&si);
	      continue;
	    }

	  bsi_next (&si);
	}		        /* stmts in BB */
    }				/* BBs in loop */

  vect_transform_loop_bound (loop_vinfo);

  if (vect_debug_details (loop))
    fprintf (dump_file,"Success! loop vectorized.");
  if (vect_debug_stats (loop))
    fprintf (dump_file, "LOOP VECTORIZED.");
}


/* Function vect_is_simple_use.

   Input:
   LOOP - the loop that is being vectorized.
   OPERAND - operand of a stmt in LOOP.
   DEF - the defining stmt in case OPERAND is an SSA_NAME.

   Returns whether a stmt with OPERAND can be vectorized.
   Supportable operands are constants, loop invariants, and operands that are
   defined by the current iteration of the loop. Unsupportable operands are 
   those that are defined by a previous iteration of the loop (as is the case
   in reduction/induction computations).  */

static bool
vect_is_simple_use (tree operand, struct loop *loop, tree *def)
{ 
  tree def_stmt;
  basic_block bb;

  if (def)
    *def = NULL_TREE;

  if (TREE_CODE (operand) == INTEGER_CST || TREE_CODE (operand) == REAL_CST)
    return true;

  if (TREE_CODE (operand) != SSA_NAME)
    return false;

  def_stmt = SSA_NAME_DEF_STMT (operand);
  if (def_stmt == NULL_TREE )
    {
      if (vect_debug_details (NULL))
        fprintf (dump_file, "no def_stmt.");
      return false;
    }

  /* empty stmt is expected only in case of a function argument.
     (Otherwise - we expect a phi_node or a modify_expr).  */
  if (IS_EMPTY_STMT (def_stmt))
    {
      tree arg = TREE_OPERAND (def_stmt, 0);
      if (TREE_CODE (arg) == INTEGER_CST || TREE_CODE (arg) == REAL_CST)
	return true;
      if (vect_debug_details (NULL))
	{
	  fprintf (dump_file, "Unexpected empty stmt: ");
	  print_generic_expr (dump_file, def_stmt, TDF_SLIM);
	}
      return false;  
    }

  /* phi_node inside the loop indicates an induction/reduction pattern.
     This is not supported yet.  */
  bb = bb_for_stmt (def_stmt);
  if (TREE_CODE (def_stmt) == PHI_NODE && flow_bb_inside_loop_p (loop, bb))
    {
      if (vect_debug_details (NULL))
	fprintf (dump_file, "reduction/induction - unsupported.");
      return false; /* FORNOW: not supported yet.  */
    }

  /* Expecting a modify_expr or a phi_node.  */
  if (TREE_CODE (def_stmt) == MODIFY_EXPR
      || TREE_CODE (def_stmt) == PHI_NODE)
    {
      if (def)
        *def = def_stmt; 	
      return true;
    }

  return false;
}


/* Function vect_analyze_operations.

   Scan the loop stmts and make sure they are all vectorizable.  */

static bool
vect_analyze_operations (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  block_stmt_iterator si;
  int vectorization_factor = 0;
  int i;
  bool ok;
  tree scalar_type;

  if (vect_debug_details (NULL))
    fprintf (dump_file, "\n<<vect_analyze_operations>>\n");

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];

      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	{
	  tree stmt = bsi_stmt (si);
	  int nunits;
	  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
	  tree vectype;

	  if (vect_debug_details (NULL))
	    {
	      fprintf (dump_file, "==> examining statement: ");
	      print_generic_expr (dump_file, stmt, TDF_SLIM);
	    }

	  gcc_assert (stmt_info);

	  /* skip stmts which do not need to be vectorized.
	     this is expected to include:
	     - the COND_EXPR which is the loop exit condition
	     - any LABEL_EXPRs in the loop
	     - computations that are used only for array indexing or loop
	     control  */

	  if (!STMT_VINFO_RELEVANT_P (stmt_info))
	    {
	      if (vect_debug_details (NULL))
	        fprintf (dump_file, "irrelevant.");
	      continue;
	    }

	  if (VECTOR_MODE_P (TYPE_MODE (TREE_TYPE (stmt))))
	    {
	      if (vect_debug_stats (loop) || vect_debug_details (loop))
		{
                  fprintf (dump_file, "not vectorized: vector stmt in loop:");
		  print_generic_expr (dump_file, stmt, TDF_SLIM);
		}
	      return false;
	    }

          if (STMT_VINFO_DATA_REF (stmt_info))
            scalar_type = TREE_TYPE (DR_REF (STMT_VINFO_DATA_REF (stmt_info)));    
          else if (TREE_CODE (stmt) == MODIFY_EXPR)
	    scalar_type = TREE_TYPE (TREE_OPERAND (stmt, 0));
	  else
	    scalar_type = TREE_TYPE (stmt);

	  if (vect_debug_details (NULL))
	    {
	      fprintf (dump_file, "get vectype for scalar type:  ");
	      print_generic_expr (dump_file, scalar_type, TDF_SLIM);
	    }

	  vectype = get_vectype_for_scalar_type (scalar_type);
	  if (!vectype)
	    {
	      if (vect_debug_stats (loop) || vect_debug_details (loop))
		{
                  fprintf (dump_file, "not vectorized: unsupported data-type ");
		  print_generic_expr (dump_file, scalar_type, TDF_SLIM);
		}
	      return false;
	    }

	  if (vect_debug_details (NULL))
	    {
	      fprintf (dump_file, "vectype: ");
	      print_generic_expr (dump_file, vectype, TDF_SLIM);
	    }
	  STMT_VINFO_VECTYPE (stmt_info) = vectype;

	  ok = (vectorizable_operation (stmt, NULL, NULL)
		|| vectorizable_assignment (stmt, NULL, NULL)
		|| vectorizable_load (stmt, NULL, NULL)
		|| vectorizable_store (stmt, NULL, NULL));

	  if (!ok)
	    {
	      if (vect_debug_stats (loop) || vect_debug_details (loop))
		{
                  fprintf (dump_file, "not vectorized: stmt not supported: ");
		  print_generic_expr (dump_file, stmt, TDF_SLIM);
		}
	      return false;
	    }

	  nunits = GET_MODE_NUNITS (TYPE_MODE (vectype));
	  if (vect_debug_details (NULL))
	    fprintf (dump_file, "nunits = %d", nunits);

	  if (vectorization_factor)
	    {
	      /* FORNOW: don't allow mixed units.
	         This restriction will be relaxed in the future.  */
	      if (nunits != vectorization_factor)
		{
	          if (vect_debug_stats (loop) || vect_debug_details (loop))
		    fprintf (dump_file, "not vectorized: mixed data-types");
		  return false;
		}
	    }
	  else
	    vectorization_factor = nunits;
	}
    }

  /* TODO: Analyze cost. Decide if worth while to vectorize.  */
  if (!vectorization_factor)
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop))
        fprintf (dump_file, "not vectorized: unsupported data-type");
      return false;
    }
  LOOP_VINFO_VECT_FACTOR (loop_vinfo) = vectorization_factor;

  /* FORNOW: handle only cases where the loop bound divides by the
     vectorization factor.  */

  if (vect_debug_details (NULL))
    fprintf (dump_file, 
	"vectorization_factor = %d, niters = " HOST_WIDE_INT_PRINT_DEC,
	vectorization_factor, LOOP_VINFO_NITERS (loop_vinfo));

  if (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)) 
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop))
	fprintf (dump_file, "not vectorized: Unknown loop bound.");
      return false;
    }

  if (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo) 
      && LOOP_VINFO_NITERS (loop_vinfo) % vectorization_factor != 0)
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop))
        fprintf (dump_file, "not vectorized: loop bound doesn't divided by %d.",
		 vectorization_factor);
      return false;
    }

  return true;
}


/* Function exist_non_indexing_operands_for_use_p 

   USE is one of the uses attached to STMT. Check if USE is 
   used in STMT for anything other than indexing an array.  */

static bool
exist_non_indexing_operands_for_use_p (tree use, tree stmt)
{
  tree operand;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
 
  /* USE corresponds to some operand in STMT. If there is no data
     reference in STMT, then any operand that corresponds to USE
     is not indexing an array.  */
  if (!STMT_VINFO_DATA_REF (stmt_info))
    return true;
 
  /* STMT has a data_ref. FORNOW this means that its of one of
     the following forms:
     -1- ARRAY_REF = var
     -2- var = ARRAY_REF
     (This should have been verified in analyze_data_refs).

     'var' in the second case corresponds to a def, not a use,
     so USE cannot correspond to any operands that are not used 
     for array indexing.

     Therefore, all we need to check is if STMT falls into the
     first case, and whether var corresponds to USE.  */
 
  if (TREE_CODE (TREE_OPERAND (stmt, 0)) == SSA_NAME)
    return false;

  operand = TREE_OPERAND (stmt, 1);

  if (TREE_CODE (operand) != SSA_NAME)
    return false;

  if (operand == use)
    return true;

  return false;
}


/* Function vect_is_simple_iv_evolution.

   FORNOW: A simple evolution of an induction variables in the loop is
   considered a polynomial evolution with constant step.  */

static bool
vect_is_simple_iv_evolution (unsigned loop_nb, tree access_fn, tree * init, 
				tree * step, bool strict)
{
  tree init_expr;
  tree step_expr;
  
  tree evolution_part = evolution_part_in_loop_num (access_fn, loop_nb);

  /* When there is no evolution in this loop, the evolution function
     is not "simple".  */  
  if (evolution_part == NULL_TREE)
    return false;
  
  /* When the evolution is a polynomial of degree >= 2
     the evolution function is not "simple".  */
  if (tree_is_chrec (evolution_part))
    return false;
  
  step_expr = evolution_part;
  init_expr = initial_condition (access_fn);

  if (vect_debug_details (NULL))
    {
      fprintf (dump_file, "step: ");
      print_generic_expr (dump_file, step_expr, TDF_SLIM);
      fprintf (dump_file, ",  init: ");
      print_generic_expr (dump_file, init_expr, TDF_SLIM);
    }

  *init = init_expr;
  *step = step_expr;

  if (TREE_CODE (step_expr) != INTEGER_CST)
    {
      if (vect_debug_details (NULL))
        fprintf (dump_file, "step unknown.");
      return false;
    }

  if (strict)
    if (!integer_onep (step_expr))
      {
        if (vect_debug_details (NULL))
	  print_generic_expr (dump_file, step_expr, TDF_SLIM);
        return false;
      }

  return true;
}


/* Function vect_analyze_scalar_cycles.

   Examine the cross iteration def-use cycles of scalar variables, by
   analyzing the loop (scalar) PHIs; verify that the cross iteration def-use
   cycles that they represent do not impede vectorization.

   FORNOW: Reduction as in the following loop, is not supported yet:
              loop1:
              for (i=0; i<N; i++)
                 sum += a[i];
	   The cross-iteration cycle corresponding to variable 'sum' will be
	   considered too complicated and will impede vectorization.

   FORNOW: Induction as in the following loop, is not supported yet:
              loop2:
              for (i=0; i<N; i++)
                 a[i] = i;

           However, the following loop *is* vectorizable:
              loop3:
              for (i=0; i<N; i++)
                 a[i] = b[i];

           In both loops there exists a def-use cycle for the variable i:
              loop: i_2 = PHI (i_0, i_1)
                    a[i_2] = ...;
                    i_1 = i_2 + 1;
                    GOTO loop;

           The evolution of the above cycle is considered simple enough,
	   however, we also check that the cycle does not need to be
	   vectorized, i.e - we check that the variable that this cycle
	   defines is only used for array indexing or in stmts that do not
	   need to be vectorized. This is not the case in loop2, but it
	   *is* the case in loop3.  */

static bool
vect_analyze_scalar_cycles (loop_vec_info loop_vinfo)
{
  tree phi;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block bb = loop->header;
  tree dummy;

  if (vect_debug_details (NULL))
    fprintf (dump_file, "\n<<vect_analyze_scalar_cycles>>\n");

  for (phi = phi_nodes (bb); phi; phi = TREE_CHAIN (phi))
    {
      tree access_fn = NULL;

      if (vect_debug_details (NULL))
	{
          fprintf (dump_file, "Analyze phi: ");
          print_generic_expr (dump_file, phi, TDF_SLIM);
	}

      /* Skip virtual phi's. The data dependences that are associated with
         virtual defs/uses (i.e., memory accesses) are analyzed elsewhere.  */

      if (!is_gimple_reg (SSA_NAME_VAR (PHI_RESULT (phi))))
	{
	  if (vect_debug_details (NULL))
	    fprintf (dump_file, "virtual phi. skip.");
	  continue;
	}

      /* Analyze the evolution function.  */

      /* FORNOW: The only scalar cross-iteration cycles that we allow are
         those of loop induction variables; This property is verified here.

         Furthermore, if that induction variable is used in an operation
         that needs to be vectorized (i.e, is not solely used to index
         arrays and check the exit condition) - we do not support its
         vectorization yet. This property is verified in vect_is_simple_use,
         during vect_analyze_operations.  */

      access_fn = /* instantiate_parameters
		     (loop,*/
	 analyze_scalar_evolution (loop, PHI_RESULT (phi));

      if (!access_fn)
	{
	  if (vect_debug_stats (loop) || vect_debug_details (loop))
	    fprintf (dump_file, "not vectorized: unsupported scalar cycle.");
	  return false;
	}

      if (vect_debug_details (NULL))
        {
           fprintf (dump_file, "Access function of PHI: ");
           print_generic_expr (dump_file, access_fn, TDF_SLIM);
        }

      if (!vect_is_simple_iv_evolution (loop->num, access_fn, &dummy, 
					&dummy, false))
	{
	  if (vect_debug_stats (loop) || vect_debug_details (loop))
	    fprintf (dump_file, "not vectorized: unsupported scalar cycle.");
	  return false;
	}
    }

  return true;
}


/* Function vect_analyze_data_ref_dependence.

   Return TRUE if there (might) exist a dependence between a memory-reference
   DRA and a memory-reference DRB.  */

static bool
vect_analyze_data_ref_dependence (struct data_reference *dra,
				  struct data_reference *drb, 
				  struct loop *loop)
{
  bool differ_p; 
  struct data_dependence_relation *ddr;
  
  if (!array_base_name_differ_p (dra, drb, &differ_p))
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop))   
        {
          fprintf (dump_file,
                "not vectorized: can't determine dependence between: ");
          print_generic_expr (dump_file, DR_REF (dra), TDF_SLIM);
          fprintf (dump_file, " and ");
          print_generic_expr (dump_file, DR_REF (drb), TDF_SLIM);
        }
      return true;
    }

  if (differ_p)
    return false;

  ddr = initialize_data_dependence_relation (dra, drb);
  compute_affine_dependence (ddr);

  if (DDR_ARE_DEPENDENT (ddr) == chrec_known)
    return false;
  
  if (vect_debug_stats (loop) || vect_debug_details (loop))
    {
      fprintf (dump_file,
	"not vectorized: possible dependence between data-refs ");
      print_generic_expr (dump_file, DR_REF (dra), TDF_SLIM);
      fprintf (dump_file, " and ");
      print_generic_expr (dump_file, DR_REF (drb), TDF_SLIM);
    }

  return true;
}


/* Function vect_analyze_data_ref_dependences.

   Examine all the data references in the loop, and make sure there do not
   exist any data dependences between them.

   TODO: dependences which distance is greater than the vectorization factor
         can be ignored.   */

static bool
vect_analyze_data_ref_dependences (loop_vec_info loop_vinfo)
{
  unsigned int i, j;
  varray_type loop_write_refs = LOOP_VINFO_DATAREF_WRITES (loop_vinfo);
  varray_type loop_read_refs = LOOP_VINFO_DATAREF_READS (loop_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);

  /* Examine store-store (output) dependences.  */

  if (vect_debug_details (NULL))
    fprintf (dump_file, "\n<<vect_analyze_dependences>>\n");

  if (vect_debug_details (NULL))
    fprintf (dump_file, "compare all store-store pairs.");

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_write_refs); i++)
    {
      for (j = i + 1; j < VARRAY_ACTIVE_SIZE (loop_write_refs); j++)
	{
	  struct data_reference *dra =
	    VARRAY_GENERIC_PTR (loop_write_refs, i);
	  struct data_reference *drb =
	    VARRAY_GENERIC_PTR (loop_write_refs, j);
	  if (vect_analyze_data_ref_dependence (dra, drb, loop))
	    return false;
	}
    }

  /* Examine load-store (true/anti) dependences.  */

  if (vect_debug_details (NULL))
    fprintf (dump_file, "compare all load-store pairs.");

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_read_refs); i++)
    {
      for (j = 0; j < VARRAY_ACTIVE_SIZE (loop_write_refs); j++)
	{
	  struct data_reference *dra = VARRAY_GENERIC_PTR (loop_read_refs, i);
	  struct data_reference *drb =
	    VARRAY_GENERIC_PTR (loop_write_refs, j);
	  if (vect_analyze_data_ref_dependence (dra, drb, loop))
	    return false;
	}
    }

  return true;
}


/* Function vect_get_first_index.

   REF is a data reference.  
   If it is an ARRAY_REF: if its lower bound is simple enough, 
   put it in ARRAY_FIRST_INDEX and return TRUE; otherwise - return FALSE.
   If it is not an ARRAY_REF: REF has no "first index";
   ARRAY_FIRST_INDEX in zero, and the function returns TRUE.  */

static bool
vect_get_first_index (tree ref, tree *array_first_index)
{
  tree array_start;

  if (TREE_CODE (ref) != ARRAY_REF)
    *array_first_index = size_zero_node;
  else
    {
      array_start = array_ref_low_bound (ref);
      if (!host_integerp (array_start,0))
	{
	  if (vect_debug_details (NULL))
	    {
	      fprintf (dump_file, "array min val not simple integer cst.");
	      print_generic_expr (dump_file, array_start, TDF_DETAILS);
	    }
	  return false;
	}
      *array_first_index = array_start;
    }

  return true;
}


/* Function vect_compute_array_base_alignment.
   A utility function of vect_compute_array_ref_alignment.

   Compute the misalignment of ARRAY in bits.

   Input:
   ARRAY - an array_ref (possibly multidimensional) of type ARRAY_TYPE.
   VECTYPE - we are interested in the misalignment modulo the size of vectype.
	     if NULL: don't compute misalignment, just return the base of ARRAY.
   PREV_DIMENSIONS - initialized to one.
   MISALIGNMENT - the computed misalignment in bits.

   Output:
   If VECTYPE is not NULL:
     Return NULL_TREE if the misalignment cannot be computed. Otherwise, return 
     the base of the array, and put the computed misalignment in MISALIGNMENT. 
   If VECTYPE is NULL:
     Return the base of the array.

   For a[idx_N]...[idx_2][idx_1][idx_0], the address of 
   a[idx_N]...[idx_2][idx_1] is 
   {&a + idx_1 * dim_0 + idx_2 * dim_0 * dim_1 + ...  
    ... + idx_N * dim_0 * ... * dim_N-1}. 
   (The misalignment of &a is not checked here).
   Note, that every term contains dim_0, therefore, if dim_0 is a 
   multiple of NUNITS, the whole sum is a multiple of NUNITS.
   Otherwise, if idx_1 is constant, and dim_1 is a multiple of
   NUINTS, we can say that the misalignment of the sum is equal to
   the misalignment of {idx_1 * dim_0}.  If idx_1 is not constant,
   we can't determine this array misalignment, and we return
   false. 
   We proceed recursively in this manner, accumulating total misalignment
   and the multiplication of previous dimensions for correct misalignment
   calculation.  */

static tree
vect_compute_array_base_alignment (tree array,
				   tree vectype,
				   tree *prev_dimensions,
				   tree *misalignment)
{
  tree index;
  tree domain;
  tree dimension_size;
  tree mis;
  tree bits_per_vectype;
  tree bits_per_vectype_unit;

  /* The 'stop condition' of the recursion.  */
  if (TREE_CODE (array) != ARRAY_REF)
    return array;
  
  if (!vectype)
    /* Just get the base decl.  */
    return vect_compute_array_base_alignment 
		(TREE_OPERAND (array, 0), NULL, NULL, NULL);

  if (!host_integerp (*misalignment, 1) || TREE_OVERFLOW (*misalignment) || 
      !host_integerp (*prev_dimensions, 1) || TREE_OVERFLOW (*prev_dimensions))
    return NULL_TREE;

  domain = TYPE_DOMAIN (TREE_TYPE (array));
  dimension_size = 
	int_const_binop (PLUS_EXPR,
		int_const_binop (MINUS_EXPR, TYPE_MAX_VALUE (domain), 
					     TYPE_MIN_VALUE (domain), 1),
		size_one_node, 1);

  /* Check if the dimension size is a multiple of NUNITS, the remaining sum
     is a multiple of NUNITS: 

     dimension_size % GET_MODE_NUNITS (TYPE_MODE (vectype)) == 0 ?
   */
  mis = int_const_binop (TRUNC_MOD_EXPR, dimension_size,
	 build_int_cst (NULL_TREE, GET_MODE_NUNITS (TYPE_MODE (vectype))), 1);
  if (integer_zerop (mis))
    /* This array is aligned. Continue just in order to get the base decl.  */
    return vect_compute_array_base_alignment 
		(TREE_OPERAND (array, 0), NULL, NULL, NULL);

  index = TREE_OPERAND (array, 1);
  if (!host_integerp (index, 1))
    /* The current index is not constant.  */
    return NULL_TREE;
   
  index = int_const_binop (MINUS_EXPR, index, TYPE_MIN_VALUE (domain), 0);

  bits_per_vectype = fold_convert (unsigned_type_node, 
    build_int_cst (NULL_TREE, BITS_PER_UNIT * 
		 GET_MODE_SIZE (TYPE_MODE (vectype))));
  bits_per_vectype_unit =  fold_convert (unsigned_type_node,
    build_int_cst (NULL_TREE, BITS_PER_UNIT * 
		 GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (vectype)))));
  
  /* Add {idx_i * dim_i-1 * ... * dim_0 } to the misalignment computed
     earlier:

     *misalignment = 
       (*misalignment + index_val * dimension_size * *prev_dimensions) 
							% vectype_nunits;
   */

  mis = int_const_binop (MULT_EXPR, index, dimension_size, 1);
  mis = int_const_binop (MULT_EXPR, mis, *prev_dimensions, 1);
  mis = int_const_binop (MULT_EXPR, mis, bits_per_vectype_unit, 1);
  mis = int_const_binop (PLUS_EXPR, *misalignment, mis, 1);
  *misalignment = int_const_binop (TRUNC_MOD_EXPR, mis, bits_per_vectype, 1);


  *prev_dimensions = int_const_binop (MULT_EXPR, 
				*prev_dimensions, dimension_size, 1);

  return vect_compute_array_base_alignment (TREE_OPERAND (array, 0), vectype,
					    prev_dimensions,
					    misalignment);
}

 
/* Function vect_compute_data_ref_alignment

   Compute the misalignment of the data reference DR.

   Output:
   1. If during the misalignment computation it is found that the data reference
      cannot be vectorized then false is returned.
   2. DR_MISALIGNMENT (DR) is defined.

   FOR NOW: No analysis is actually performed. Misalignment is calculated
   only for trivial cases. TODO.  */

static bool
vect_compute_data_ref_alignment (struct data_reference *dr, 
				 loop_vec_info loop_vinfo)
{
  tree stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);  
  tree ref = DR_REF (dr);
  tree vectype;
  tree scalar_type;
  tree offset = size_zero_node;
  tree base, bit_offset, alignment;
  tree unit_bits = fold_convert (unsigned_type_node, 
				 build_int_cst (NULL_TREE, BITS_PER_UNIT));
  tree dr_base;
  bool base_aligned_p;
   
  if (vect_debug_details (NULL))
    fprintf (dump_file, "vect_compute_data_ref_alignment:");

  /* Initialize misalignment to unknown.  */
  DR_MISALIGNMENT (dr) = -1;

  scalar_type = TREE_TYPE (ref);
  vectype = get_vectype_for_scalar_type (scalar_type);
  if (!vectype)
    {
      if (vect_debug_details (NULL))
        {
          fprintf (dump_file, "no vectype for stmt: ");
          print_generic_expr (dump_file, stmt, TDF_SLIM);
          fprintf (dump_file, " scalar_type: ");
          print_generic_expr (dump_file, scalar_type, TDF_DETAILS);
        }
      /* It is not possible to vectorize this data reference. */
      return false;
    }
  gcc_assert (TREE_CODE (ref) == ARRAY_REF || TREE_CODE (ref) == INDIRECT_REF);
  
  if (TREE_CODE (ref) == ARRAY_REF)
    dr_base = ref;
  else
    dr_base = STMT_VINFO_VECT_DR_BASE (stmt_info);

  base = vect_get_base_and_bit_offset (dr, dr_base, vectype, 
			  loop_vinfo, &bit_offset, &base_aligned_p);
  if (!base)
    {
      if (vect_debug_details (NULL)) 
	{
	  fprintf (dump_file, "Unknown alignment for access: ");
	  print_generic_expr (dump_file, 
			      STMT_VINFO_VECT_DR_BASE (stmt_info), TDF_SLIM);
	}
      return true;
    }

  if (!base_aligned_p) 
    {
      if (!vect_can_force_dr_alignment_p (base, TYPE_ALIGN (vectype)))
	{
	  if (vect_debug_details (NULL))
	    {
	      fprintf (dump_file, "can't force alignment of ref: ");
	      print_generic_expr (dump_file, ref, TDF_SLIM);
	    }
	  return true;
	}
      
      /* Force the alignment of the decl.
	 NOTE: This is the only change to the code we make during
	 the analysis phase, before deciding to vectorize the loop.  */
      if (vect_debug_details (NULL))
	fprintf (dump_file, "force alignment");
      DECL_ALIGN (base) = TYPE_ALIGN (vectype);
      DECL_USER_ALIGN (base) = TYPE_ALIGN (vectype);
    }

  /* At this point we assume that the base is aligned, and the offset from it
     (including index, if relevant) has been computed and is in BIT_OFFSET.  */
  gcc_assert (base_aligned_p 
	      || (TREE_CODE (base) == VAR_DECL 
		  && DECL_ALIGN (base) >= TYPE_ALIGN (vectype)));

  /* Convert into bytes.  */
  offset = int_const_binop (TRUNC_DIV_EXPR, bit_offset, unit_bits, 1);
  /* Check that there is no remainder in bits.  */
  bit_offset = int_const_binop (TRUNC_MOD_EXPR, bit_offset, unit_bits, 1);
  if (!integer_zerop (bit_offset))
    {
      if (vect_debug_details (NULL))
	{
	  fprintf (dump_file, "bit offset alignment: ");
	  print_generic_expr (dump_file, bit_offset, TDF_SLIM);
	}
      return false;
    }
  
  /* Alignment required, in bytes:  */
  alignment = fold_convert (unsigned_type_node,
	    build_int_cst (NULL_TREE, TYPE_ALIGN (vectype)/BITS_PER_UNIT));

  /* Modulo alignment.  */
  offset = int_const_binop (TRUNC_MOD_EXPR, offset, alignment, 0);
  if (!host_integerp (offset, 1) || TREE_OVERFLOW (offset))
    {
      if (vect_debug_details (NULL))
	fprintf (dump_file, "unexpected misalign value");
      return false;
    }

  DR_MISALIGNMENT (dr) = tree_low_cst (offset, 1);

  if (vect_debug_details (NULL))
    fprintf (dump_file, "misalign = %d", DR_MISALIGNMENT (dr));

  return true;
}


/* Function vect_compute_array_ref_alignment

   Compute the alignment of an array-ref.
   The alignment we compute here is relative to 
   TYPE_ALIGN(VECTYPE) boundary.  

   Output:
   OFFSET - the alignment in bits
   Return value - the base of the array-ref. E.g, 
                  if the array-ref is a.b[k].c[i][j] the returned
		  base is a.b[k].c
*/

static tree
vect_compute_array_ref_alignment (struct data_reference *dr,
				  loop_vec_info loop_vinfo,
				  tree vectype,
				  tree *offset)
{
  tree array_first_index = size_zero_node;
  tree init;
  tree ref = DR_REF (dr);
  tree scalar_type = TREE_TYPE (ref);
  tree oprnd0 = TREE_OPERAND (ref, 0);
  tree dims = size_one_node;  
  tree misalign = size_zero_node;
  tree next_ref, this_offset = size_zero_node;
  tree nunits;
  tree nbits;

  if (TREE_CODE (TREE_TYPE (ref)) == ARRAY_TYPE)
      /* The reference is an array without its last index. */
      next_ref = vect_compute_array_base_alignment (ref, vectype, &dims, &misalign);
  else
      next_ref = 
	vect_compute_array_base_alignment (oprnd0, vectype, &dims, &misalign);
  if (!vectype)
    /* Alignment is not requested. Just return the base.  */
    return next_ref;

  /* Compute alignment.  */
  if (!host_integerp (misalign, 1) || TREE_OVERFLOW (misalign) || !next_ref)
    return NULL_TREE;
  this_offset = misalign;

  /* Check the first index accessed.  */
  if (!vect_get_first_index (ref, &array_first_index))
    {
      if (vect_debug_details (NULL))
        fprintf (dump_file, "no first_index for array.");
      return NULL_TREE;
    }

  /* Check the index of the array_ref.  */
  init = initial_condition_in_loop_num (DR_ACCESS_FN (dr, 0), 
					LOOP_VINFO_LOOP (loop_vinfo)->num);

  /* FORNOW: In order to simplify the handling of alignment, we make sure
     that the first location at which the array is accessed ('init') is on an
     'NUNITS' boundary, since we are assuming here that 'array base' is aligned. 
     This is too conservative, since we require that
     both {'array_base' is a multiple of NUNITS} && {'init' is a multiple of
     NUNITS}, instead of just {('array_base' + 'init') is a multiple of NUNITS}.
     This should be relaxed in the future.  */

  if (!init || !host_integerp (init, 0))
    {
      if (vect_debug_details (NULL))
	fprintf (dump_file, "non constant init. ");
      return NULL_TREE;
    }

  /* bytes per scalar element: */
  nunits = fold_convert (unsigned_type_node,
	build_int_cst (NULL_TREE, GET_MODE_SIZE (TYPE_MODE (scalar_type))));
  nbits = int_const_binop (MULT_EXPR, nunits,     
			   build_int_cst (NULL_TREE, BITS_PER_UNIT), 1);

  /* misalign = offset + (init-array_first_index)*nunits*bits_in_byte */
  misalign = int_const_binop (MINUS_EXPR, init, array_first_index, 0);
  misalign = int_const_binop (MULT_EXPR, misalign, nbits, 0);
  misalign = int_const_binop (PLUS_EXPR, misalign, this_offset, 0);

  /* TODO: allow negative misalign values.  */
  if (!host_integerp (misalign, 1) || TREE_OVERFLOW (misalign))
    {
      if (vect_debug_details (NULL))
        fprintf (dump_file, "unexpected misalign value");
      return NULL_TREE;
    }
  *offset = misalign;
  return next_ref;
}


/* Function vect_compute_data_refs_alignment

   Compute the misalignment of data references in the loop.
   This pass may take place at function granularity instead of at loop
   granularity.

   FOR NOW: No analysis is actually performed. Misalignment is calculated
   only for trivial cases. TODO.  */

static void
vect_compute_data_refs_alignment (loop_vec_info loop_vinfo)
{
  varray_type loop_write_datarefs = LOOP_VINFO_DATAREF_WRITES (loop_vinfo);
  varray_type loop_read_datarefs = LOOP_VINFO_DATAREF_READS (loop_vinfo);
  unsigned int i;

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_write_datarefs); i++)
    {
      struct data_reference *dr = VARRAY_GENERIC_PTR (loop_write_datarefs, i);
      vect_compute_data_ref_alignment (dr, loop_vinfo);
    }

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_read_datarefs); i++)
    {
      struct data_reference *dr = VARRAY_GENERIC_PTR (loop_read_datarefs, i);
      vect_compute_data_ref_alignment (dr, loop_vinfo);
    }
}


/* Function vect_enhance_data_refs_alignment

   This pass will use loop versioning and loop peeling in order to enhance
   the alignment of data references in the loop.

   FOR NOW: we assume that whatever versioning/peeling takes place, only the
   original loop is to be vectorized; Any other loops that are created by
   the transformations performed in this pass - are not supposed to be
   vectorized. This restriction will be relaxed.

   FOR NOW: No transformation is actually performed. TODO.  */

static void
vect_enhance_data_refs_alignment (loop_vec_info loop_info ATTRIBUTE_UNUSED)
{
  /*
     This pass will require a cost model to guide it whether to apply peeling 
     or versioning or a combination of the two. For example, the scheme that
     intel uses when given a loop with several memory accesses, is as follows:
     choose one memory access ('p') which alignment you want to force by doing 
     peeling. Then, either (1) generate a loop in which 'p' is aligned and all 
     other accesses are not necessarily aligned, or (2) use loop versioning to 
     generate one loop in which all accesses are aligned, and another loop in 
     which only 'p' is necessarily aligned. 

     ("Automatic Intra-Register Vectorization for the Intel Architecture",
      Aart J.C. Bik, Milind Girkar, Paul M. Grey and Ximmin Tian, International
      Journal of Parallel Programming, Vol. 30, No. 2, April 2002.)	

     Devising a cost model is the most critical aspect of this work. It will 
     guide us on which access to peel for, whether to use loop versioning, how 
     many versions to create, etc. The cost model will probably consist of 
     generic considerations as well as target specific considerations (on 
     powerpc for example, misaligned stores are more painful than misaligned 
     loads). 

     Here is the general steps involved in alignment enhancements:
    
     -- original loop, before alignment analysis:
	for (i=0; i<N; i++){
	  x = q[i];			# DR_MISALIGNMENT(q) = unknown
	  p[i] = y;			# DR_MISALIGNMENT(p) = unknown
	}

     -- After vect_compute_data_refs_alignment:
	for (i=0; i<N; i++){
	  x = q[i];			# DR_MISALIGNMENT(q) = 3
	  p[i] = y;			# DR_MISALIGNMENT(p) = unknown
	}

     -- Possibility 1: we do loop versioning:
     if (p is aligned) {
	for (i=0; i<N; i++){	# loop 1A
	  x = q[i];			# DR_MISALIGNMENT(q) = 3
	  p[i] = y;			# DR_MISALIGNMENT(p) = 0
	}
     } 
     else {
	for (i=0; i<N; i++){	# loop 1B
	  x = q[i];			# DR_MISALIGNMENT(q) = 3
	  p[i] = y;			# DR_MISALIGNMENT(p) = unaligned
	}
     }
   
     -- Possibility 2: we do loop peeling:
     for (i = 0; i < 3; i++){	# (scalar loop, not to be vectorized).
	x = q[i];
	p[i] = y;
     }
     for (i = 3; i < N; i++){	# loop 2A
	x = q[i];			# DR_MISALIGNMENT(q) = 0
	p[i] = y;			# DR_MISALIGNMENT(p) = unknown
     }

     -- Possibility 3: combination of loop peeling and versioning:
     for (i = 0; i < 3; i++){	# (scalar loop, not to be vectorized).
	x = q[i];
	p[i] = y;
     }
     if (p is aligned) {
	for (i = 3; i<N; i++){  # loop 3A
	  x = q[i];			# DR_MISALIGNMENT(q) = 0
	  p[i] = y;			# DR_MISALIGNMENT(p) = 0
	}
     } 
     else {
	for (i = 3; i<N; i++){	# loop 3B
	  x = q[i];			# DR_MISALIGNMENT(q) = 0
	  p[i] = y;			# DR_MISALIGNMENT(p) = unaligned
	}
     }

     These loops are later passed to loop_transform to be vectorized. The 
     vectorizer will use the alignment information to guide the transformation 
     (whether to generate regular loads/stores, or with special handling for 
     misalignment). 
   */
}


/* Function vect_analyze_data_refs_alignment

   Analyze the alignment of the data-references in the loop.
   FOR NOW: Until support for misliagned accesses is in place, only if all
   accesses are aligned can the loop be vectorized. This restriction will be 
   relaxed.  */ 

static bool
vect_analyze_data_refs_alignment (loop_vec_info loop_vinfo)
{
  varray_type loop_write_datarefs = LOOP_VINFO_DATAREF_WRITES (loop_vinfo);
  /*varray_type loop_read_datarefs = LOOP_VINFO_DATAREF_READS (loop_vinfo);*/

  unsigned int i;

  if (vect_debug_details (NULL))
    fprintf (dump_file, "\n<<vect_analyze_data_refs_alignment>>\n");


  /* This pass may take place at function granularity instead of at loop
     granularity.  */

  vect_compute_data_refs_alignment (loop_vinfo);


  /* This pass will use loop versioning and loop peeling in order to enhance
     the alignment of data references in the loop.
     FOR NOW: we assume that whatever versioning/peeling took place, the 
     original loop is to be vectorized. Any other loops that were created by
     the transformations performed in this pass - are not supposed to be 
     vectorized. This restriction will be relaxed.  */

  vect_enhance_data_refs_alignment (loop_vinfo);


  /* Finally, check that loop can be vectorized. 
     FOR NOW: Until support for misaligned accesses is in place, only if all
     accesses are aligned can the loop be vectorized. This restriction will be 
     relaxed.  */

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_write_datarefs); i++)
    {
      struct data_reference *dr = VARRAY_GENERIC_PTR (loop_write_datarefs, i);
      if (!aligned_access_p (dr))
	{
	  if (vect_debug_stats (LOOP_VINFO_LOOP (loop_vinfo))
	      || vect_debug_details (LOOP_VINFO_LOOP (loop_vinfo)))
	    fprintf (dump_file, "not vectorized: unaligned store.");
	  return false;
	}
    }

  /* The vectorizer now supports misaligned loads, so we don't fail anymore
     in the presence of a misaligned read dataref.  For some targets however
     it may be preferable not to vectorize in such a case as misaligned
     accesses are very costly.  This should be considered in the future.  */
/*
  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_read_datarefs); i++)
    {
      struct data_reference *dr = VARRAY_GENERIC_PTR (loop_read_datarefs, i);
      if (!aligned_access_p (dr))
	{
	  if (vect_debug_stats (LOOP_VINFO_LOOP (loop_vinfo))
	      || vect_debug_details (LOOP_VINFO_LOOP (loop_vinfo)))
	    fprintf (dump_file, "not vectorized: unaligned load.");
	  return false;
	}
    }
*/

  return true;
}


/* Function vect_analyze_data_ref_access.

   Analyze the access pattern of the data-reference DR. For now, a data access
   has to consecutive and aligned to be considered vectorizable.  */

static bool
vect_analyze_data_ref_access (struct data_reference *dr)
{
  varray_type access_fns = DR_ACCESS_FNS (dr);
  tree access_fn;
  tree init, step;
  unsigned int dimensions, i;

  /* Check that in case of multidimensional array ref A[i1][i2]..[iN],
     i1, i2, ..., iN-1 are loop invariant (to make sure that the memory
     access is contiguous).  */
  dimensions = VARRAY_ACTIVE_SIZE (access_fns);

  for (i = 1; i < dimensions; i++) /* Not including the last dimension.  */
    {
      access_fn = DR_ACCESS_FN (dr, i);

      if (evolution_part_in_loop_num (access_fn, 
				      loop_containing_stmt (DR_STMT (dr))->num))
	{
	  /* Evolution part is not NULL in this loop (it is neither constant nor 
	     invariant). */
	  if (vect_debug_details (NULL))
	    {
	      fprintf (dump_file, 
		       "not vectorized: complicated multidimensional array access.");
	      print_generic_expr (dump_file, access_fn, TDF_SLIM);
	    }
	  return false;
	}
    }
  
  access_fn = DR_ACCESS_FN (dr, 0); /*  The last dimension access function.  */
  if (!evolution_function_is_constant_p (access_fn)
      && !vect_is_simple_iv_evolution (loop_containing_stmt (DR_STMT (dr))->num,
				       access_fn, &init, &step, true))
    {
      if (vect_debug_details (NULL))
	{
	  fprintf (dump_file, "not vectorized: too complicated access function.");
	  print_generic_expr (dump_file, access_fn, TDF_SLIM);
	}
      return false;
    }
  
  return true;
}


/* Function vect_analyze_data_ref_accesses.

   Analyze the access pattern of all the data references in the loop.

   FORNOW: the only access pattern that is considered vectorizable is a
	   simple step 1 (consecutive) access.

   FORNOW: handle only arrays and pointer accesses.  */

static bool
vect_analyze_data_ref_accesses (loop_vec_info loop_vinfo)
{
  unsigned int i;
  varray_type loop_write_datarefs = LOOP_VINFO_DATAREF_WRITES (loop_vinfo);
  varray_type loop_read_datarefs = LOOP_VINFO_DATAREF_READS (loop_vinfo);

  if (vect_debug_details (NULL))
    fprintf (dump_file, "\n<<vect_analyze_data_ref_accesses>>\n");

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_write_datarefs); i++)
    {
      struct data_reference *dr = VARRAY_GENERIC_PTR (loop_write_datarefs, i);
      bool ok = vect_analyze_data_ref_access (dr);
      if (!ok)
	{
	  if (vect_debug_stats (LOOP_VINFO_LOOP (loop_vinfo))
	      || vect_debug_details (LOOP_VINFO_LOOP (loop_vinfo)))
	    fprintf (dump_file, "not vectorized: complicated access pattern.");
	  return false;
	}
    }

  for (i = 0; i < VARRAY_ACTIVE_SIZE (loop_read_datarefs); i++)
    {
      struct data_reference *dr = VARRAY_GENERIC_PTR (loop_read_datarefs, i);
      bool ok = vect_analyze_data_ref_access (dr);
      if (!ok)
	{
	  if (vect_debug_stats (LOOP_VINFO_LOOP (loop_vinfo))
	      || vect_debug_details (LOOP_VINFO_LOOP (loop_vinfo))) 
	    fprintf (dump_file, "not vectorized: complicated access pattern.");
	  return false;
	}
    }

  return true;
}


/* Function vect_analyze_pointer_ref_access.

   Input:
   STMT - a stmt that contains a data-ref
   MEMREF - a data-ref in STMT, which is an INDIRECT_REF.

   If the data-ref access is vectorizable, return a data_reference structure
   that represents it (DR). Otherwise - return NULL.   */

static struct data_reference *
vect_analyze_pointer_ref_access (tree memref, tree stmt, bool is_read)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  struct loop *loop = STMT_VINFO_LOOP (stmt_info);
  tree access_fn = analyze_scalar_evolution (loop, TREE_OPERAND (memref, 0));
  tree init, step;	
  int step_val;
  tree reftype, innertype;
  enum machine_mode innermode;
  tree indx_access_fn; 
  int loopnum = loop->num;
  struct data_reference *dr;

  if (!access_fn)
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop))
        fprintf (dump_file, "not vectorized: complicated pointer access.");	
      return NULL;
    }

  if (vect_debug_details (NULL))
    {
      fprintf (dump_file, "Access function of ptr: ");
      print_generic_expr (dump_file, access_fn, TDF_SLIM);
    }

  if (!vect_is_simple_iv_evolution (loopnum, access_fn, &init, &step, false))
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop)) 
	fprintf (dump_file, "not vectorized: pointer access is not simple.");	
      return NULL;
    }
		
  STRIP_NOPS (init);

  if (!host_integerp (step,0))
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop)) 
	fprintf (dump_file, 
		"not vectorized: non constant step for pointer access.");	
      return NULL;
    }

  step_val = TREE_INT_CST_LOW (step);

  reftype = TREE_TYPE (TREE_OPERAND (memref, 0));
  if (TREE_CODE (reftype) != POINTER_TYPE) 
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop))
	fprintf (dump_file, "not vectorized: unexpected pointer access form.");	
      return NULL;
    }

  reftype = TREE_TYPE (init);
  if (TREE_CODE (reftype) != POINTER_TYPE) 
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop)) 
	fprintf (dump_file, "not vectorized: unexpected pointer access form.");
      return NULL;
    }

  innertype = TREE_TYPE (reftype);
  innermode = TYPE_MODE (innertype);
  if (GET_MODE_SIZE (innermode) != step_val) 
    {
      /* FORNOW: support only consecutive access */
      if (vect_debug_stats (loop) || vect_debug_details (loop)) 
	fprintf (dump_file, "not vectorized: non consecutive access.");	
      return NULL;
    }

  indx_access_fn = 
	build_polynomial_chrec (loopnum, integer_zero_node, integer_one_node);
  if (vect_debug_details (NULL)) 
    {
      fprintf (dump_file, "Access function of ptr indx: ");
      print_generic_expr (dump_file, indx_access_fn, TDF_SLIM);
    }
  dr = init_data_ref (stmt, memref, init, indx_access_fn, is_read);
  return dr;
}


/* Function vect_get_symbl_and_dr.  

   The function returns SYMBL - the relevant variable for
   memory tag (for aliasing purposes). 
   Also data reference structure DR is created.  

   Input:
   MEMREF - data reference in STMT
   IS_READ - TRUE if STMT reads from MEMREF, FALSE if writes to MEMREF
   
   Output:
   DR - data_reference struct for MEMREF
   return value - the relevant variable for memory tag (for aliasing purposes).

*/ 

static tree
vect_get_symbl_and_dr (tree memref, tree stmt, bool is_read, 
		       loop_vec_info loop_vinfo, struct data_reference **dr)
{
  tree symbl, oprnd0, oprnd1;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree offset;
  tree array_base, base;
  struct data_reference *new_dr;
  bool base_aligned_p;

  *dr = NULL;
  switch (TREE_CODE (memref))
    {
    case INDIRECT_REF:
      new_dr = vect_analyze_pointer_ref_access (memref, stmt, is_read);
      if (! new_dr)
	return NULL_TREE; 
      *dr = new_dr;
      symbl = DR_BASE_NAME (new_dr);
      STMT_VINFO_VECT_DR_BASE (stmt_info) = symbl;

      switch (TREE_CODE (symbl))
	{
	case PLUS_EXPR:
	case MINUS_EXPR:
	  oprnd0 = TREE_OPERAND (symbl, 0);
	  oprnd1 = TREE_OPERAND (symbl, 1);

	  STRIP_NOPS(oprnd1);
	  /* Only {address_base + offset} expressions are supported,  
	     where address_base can be POINTER_TYPE or ARRAY_TYPE and 
	     offset can be anything but POINTER_TYPE or ARRAY_TYPE.  
	     TODO: swap operands if {offset + address_base}.  */
	  if ((TREE_CODE (TREE_TYPE (oprnd1)) == POINTER_TYPE 
	       && TREE_CODE (oprnd1) != INTEGER_CST)
	      || TREE_CODE (TREE_TYPE (oprnd1)) == ARRAY_TYPE)
	    return NULL_TREE;

	  if (TREE_CODE (TREE_TYPE (oprnd0)) == POINTER_TYPE)
	    symbl = oprnd0;
	  else
	    symbl = vect_get_symbl_and_dr (oprnd0, stmt, is_read, 
					   loop_vinfo, &new_dr); 

	case SSA_NAME:
	case ADDR_EXPR:
	  /* symbl remains unchanged.  */
	  break;

	default:
	  if (vect_debug_details (NULL))
	    {
	      fprintf (dump_file, "unhandled data ref: ");
	      print_generic_expr (dump_file, memref, TDF_SLIM);
	      fprintf (dump_file, " (symbl ");
	      print_generic_expr (dump_file, symbl, TDF_SLIM);
	      fprintf (dump_file, ") in stmt  ");
	      print_generic_expr (dump_file, stmt, TDF_SLIM);
	    }
	  return NULL_TREE;	
	}
      break;

    case ARRAY_REF:
      offset = size_zero_node;

      /* Store the array base in the stmt info. 
	 For one dimensional array ref a[i], the base is a,
	 for multidimensional a[i1][i2]..[iN], the base is 
	 a[i1][i2]..[iN-1]. */
      array_base = TREE_OPERAND (memref, 0);
      STMT_VINFO_VECT_DR_BASE (stmt_info) = array_base;	     

      new_dr = analyze_array (stmt, memref, is_read);
      *dr = new_dr;

      /* Find the relevant symbol for aliasing purposes.  */	
      base = DR_BASE_NAME (new_dr);
      switch (TREE_CODE (base))	
	{
	case VAR_DECL:
	  symbl = base;
	  break;

	case INDIRECT_REF:
	  symbl = TREE_OPERAND (base, 0); 
	  break;

	case COMPONENT_REF:
	  /* Could have recorded more accurate information - 
	     i.e, the actual FIELD_DECL that is being referenced -
	     but later passes expect VAR_DECL as the nmt.  */	
	  symbl = vect_get_base_and_bit_offset (new_dr, base, NULL_TREE, 
					loop_vinfo, &offset, &base_aligned_p);
	  if (symbl)
	    break;
	  /* fall through */	
	default:
	  if (vect_debug_details (NULL))
	    {
	      fprintf (dump_file, "unhandled struct/class field access ");
	      print_generic_expr (dump_file, stmt, TDF_SLIM);
	    }
	  return NULL_TREE;
	}
      break;

    default:
      if (vect_debug_details (NULL))
	{
	  fprintf (dump_file, "unhandled data ref: ");
	  print_generic_expr (dump_file, memref, TDF_SLIM);
	  fprintf (dump_file, " in stmt  ");
	  print_generic_expr (dump_file, stmt, TDF_SLIM);
	}
      return NULL_TREE;
    }
  return symbl;
}


/* Function vect_analyze_data_refs.

   Find all the data references in the loop.

   FORNOW: Handle aligned INDIRECT_REFs and ARRAY_REFs 
	   which base is really an array (not a pointer) and which alignment 
	   can be forced. This restriction will be relaxed.   */

static bool
vect_analyze_data_refs (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  block_stmt_iterator si;
  int j;
  struct data_reference *dr;
  tree tag;
  tree address_base;

  if (vect_debug_details (NULL))
    fprintf (dump_file, "\n<<vect_analyze_data_refs>>\n");

  for (j = 0; j < nbbs; j++)
    {
      basic_block bb = bbs[j];
      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	{
	  bool is_read = false;
	  tree stmt = bsi_stmt (si);
	  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
	  v_may_def_optype v_may_defs = STMT_V_MAY_DEF_OPS (stmt);
	  v_must_def_optype v_must_defs = STMT_V_MUST_DEF_OPS (stmt);
	  vuse_optype vuses = STMT_VUSE_OPS (stmt);
	  varray_type *datarefs = NULL;
	  int nvuses, nv_may_defs, nv_must_defs;
	  tree memref = NULL;
	  tree symbl;

	  /* Assumption: there exists a data-ref in stmt, if and only if 
             it has vuses/vdefs.  */

	  if (!vuses && !v_may_defs && !v_must_defs)
	    continue;

	  nvuses = NUM_VUSES (vuses);
	  nv_may_defs = NUM_V_MAY_DEFS (v_may_defs);
	  nv_must_defs = NUM_V_MUST_DEFS (v_must_defs);

	  if (nvuses && (nv_may_defs || nv_must_defs))
	    {
	      if (vect_debug_details (NULL))
		{
		  fprintf (dump_file, "unexpected vdefs and vuses in stmt: ");
		  print_generic_expr (dump_file, stmt, TDF_SLIM);
		}
	      return false;
	    }

	  if (TREE_CODE (stmt) != MODIFY_EXPR)
	    {
	      if (vect_debug_details (NULL))
		{
		  fprintf (dump_file, "unexpected vops in stmt: ");
		  print_generic_expr (dump_file, stmt, TDF_SLIM);
		}
	      return false;
	    }

	  if (vuses)
	    {
	      memref = TREE_OPERAND (stmt, 1);
	      datarefs = &(LOOP_VINFO_DATAREF_READS (loop_vinfo));
	      is_read = true;
	    } 
	  else /* vdefs */
	    {
	      memref = TREE_OPERAND (stmt, 0);
	      datarefs = &(LOOP_VINFO_DATAREF_WRITES (loop_vinfo));
	      is_read = false;
	    }

	  /* Analyze MEMREF. If it is of a supported form, build data_reference
	     struct for it (DR) and find the relevant symbol for aliasing 
	     purposes.  */
	  symbl = vect_get_symbl_and_dr (memref, stmt, is_read, loop_vinfo, &dr);
	  if (!symbl)
	    {
	      if (vect_debug_stats (loop) || vect_debug_details (loop))
		{
		  fprintf (dump_file, "not vectorized: unhandled data ref: "); 
		  print_generic_expr (dump_file, stmt, TDF_SLIM);
		}
	      return false;
	    }

	  /* Find and record the memtag assigned to this data-ref.  */
	   switch (TREE_CODE (symbl))
	    {
	    case VAR_DECL:
	      STMT_VINFO_MEMTAG (stmt_info) = symbl;
	      break;
	      
	    case SSA_NAME:
	      symbl = SSA_NAME_VAR (symbl);
	      tag = get_var_ann (symbl)->type_mem_tag;
	      if (!tag)
		{
		  tree ptr = TREE_OPERAND (memref, 0);
		  if (TREE_CODE (ptr) == SSA_NAME)
		    tag = get_var_ann (SSA_NAME_VAR (ptr))->type_mem_tag;
		}
	      if (!tag)
		{
		  if (vect_debug_stats (loop) || vect_debug_details (loop))
		    fprintf (dump_file, "not vectorized: no memtag for ref.");
		  return false;
		}
	      STMT_VINFO_MEMTAG (stmt_info) = tag;
	      break;

	    case ADDR_EXPR:
	      address_base = TREE_OPERAND (symbl, 0);

	      switch (TREE_CODE (address_base))
		{
		case ARRAY_REF:
		  dr = analyze_array (stmt, TREE_OPERAND (symbl, 0), DR_IS_READ(dr));
		  STMT_VINFO_MEMTAG (stmt_info) = DR_BASE_NAME (dr);
		  break;
		  
		case VAR_DECL: 
		  STMT_VINFO_MEMTAG (stmt_info) = address_base;
		  break;

		default:
		  if (vect_debug_stats (loop) || vect_debug_details (loop))
		    {
		      fprintf (dump_file, "not vectorized: unhandled address expression: ");
		      print_generic_expr (dump_file, stmt, TDF_SLIM);
		    }
		  return false;
		}
	      break;
	      
	    default:
	      if (vect_debug_stats (loop) || vect_debug_details (loop))
		{
		  fprintf (dump_file, "not vectorized: unsupported data-ref: ");
		  print_generic_expr (dump_file, memref, TDF_SLIM);
		}
	      return false;
	    }

	  VARRAY_PUSH_GENERIC_PTR (*datarefs, dr);
	  STMT_VINFO_DATA_REF (stmt_info) = dr;
	}
    }

  return true;
}


/* Utility functions used by vect_mark_stmts_to_be_vectorized.  */

/* Function vect_mark_relevant.

   Mark STMT as "relevant for vectorization" and add it to WORKLIST.  */

static void
vect_mark_relevant (varray_type worklist, tree stmt)
{
  stmt_vec_info stmt_info;

  if (vect_debug_details (NULL))
    fprintf (dump_file, "mark relevant.");

  if (TREE_CODE (stmt) == PHI_NODE)
    {
      VARRAY_PUSH_TREE (worklist, stmt);
      return;
    }

  stmt_info = vinfo_for_stmt (stmt);

  if (!stmt_info)
    {
      if (vect_debug_details (NULL))
	{
	  fprintf (dump_file, "mark relevant: no stmt info!!.");
	  print_generic_expr (dump_file, stmt, TDF_SLIM);
	}
      return;
    }

  if (STMT_VINFO_RELEVANT_P (stmt_info))
    {
      if (vect_debug_details (NULL))
        fprintf (dump_file, "already marked relevant.");
      return;
    }

  STMT_VINFO_RELEVANT_P (stmt_info) = 1;
  VARRAY_PUSH_TREE (worklist, stmt);
}


/* Function vect_stmt_relevant_p.

   Return true if STMT in loop that is represented by LOOP_VINFO is
   "relevant for vectorization".

   A stmt is considered "relevant for vectorization" if:
   - it has uses outside the loop.
   - it has vdefs (it alters memory).
   - control stmts in the loop (except for the exit condition).

   CHECKME: what other side effects would the vectorizer allow?  */

static bool
vect_stmt_relevant_p (tree stmt, loop_vec_info loop_vinfo)
{
  v_may_def_optype v_may_defs;
  v_must_def_optype v_must_defs;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  int i;
  dataflow_t df;
  int num_uses;

  /* cond stmt other than loop exit cond.  */
  if (is_ctrl_stmt (stmt) && (stmt != LOOP_VINFO_EXIT_COND (loop_vinfo)))
    return true;

  /* changing memory.  */
  v_may_defs = STMT_V_MAY_DEF_OPS (stmt);
  v_must_defs = STMT_V_MUST_DEF_OPS (stmt);
  if (v_may_defs || v_must_defs)
    {
      if (vect_debug_details (NULL))
        fprintf (dump_file, "vec_stmt_relevant_p: stmt has vdefs.");
      return true;
    }

  /* uses outside the loop.  */
  df = get_immediate_uses (stmt);
  num_uses = num_immediate_uses (df);
  for (i = 0; i < num_uses; i++)
    {
      tree use = immediate_use (df, i);
      basic_block bb = bb_for_stmt (use);
      if (!flow_bb_inside_loop_p (loop, bb))
	{
	  if (vect_debug_details (NULL))
	    fprintf (dump_file, "vec_stmt_relevant_p: used out of loop.");
	  return true;
	}
    }

  return false;
}


/* Function vect_mark_stmts_to_be_vectorized.

   Not all stmts in the loop need to be vectorized. For example:

     for i...
       for j...
   1.    T0 = i + j
   2.	 T1 = a[T0]

   3.    j = j + 1

   Stmt 1 and 3 do not need to be vectorized, because loop control and
   addressing of vectorized data-refs are handled differently.

   This pass detects such stmts.  */

static bool
vect_mark_stmts_to_be_vectorized (loop_vec_info loop_vinfo)
{
  varray_type worklist;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  unsigned int nbbs = loop->num_nodes;
  block_stmt_iterator si;
  tree stmt;
  stmt_ann_t ann;
  unsigned int i;
  int j;
  use_optype use_ops;
  stmt_vec_info stmt_info;

  if (vect_debug_details (NULL))
    fprintf (dump_file, "\n<<vect_mark_stmts_to_be_vectorized>>\n");

  VARRAY_TREE_INIT (worklist, 64, "work list");

  /* 1. Init worklist.  */

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];
      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	{
	  stmt = bsi_stmt (si);

	  if (vect_debug_details (NULL))
	    {
	      fprintf (dump_file, "init: stmt relevant? ");
	      print_generic_expr (dump_file, stmt, TDF_SLIM);
	    } 

	  stmt_info = vinfo_for_stmt (stmt);
	  STMT_VINFO_RELEVANT_P (stmt_info) = 0;

	  if (vect_stmt_relevant_p (stmt, loop_vinfo))
	    vect_mark_relevant (worklist, stmt);
	}
    }


  /* 2. Process_worklist */

  while (VARRAY_ACTIVE_SIZE (worklist) > 0)
    {
      stmt = VARRAY_TOP_TREE (worklist);
      VARRAY_POP (worklist);

      if (vect_debug_details (NULL))
	{
          fprintf (dump_file, "worklist: examine stmt: ");
          print_generic_expr (dump_file, stmt, TDF_SLIM);
	}

      /* Examine the USES in this statement. Mark all the statements which
         feed this statement's uses as "relevant", unless the USE is used as
         an array index.  */

      if (TREE_CODE (stmt) == PHI_NODE)
	{
	  /* follow the def-use chain inside the loop.  */
	  for (j = 0; j < PHI_NUM_ARGS (stmt); j++)
	    {
	      tree arg = PHI_ARG_DEF (stmt, j);
	      tree def_stmt = NULL_TREE;
	      basic_block bb;
	      if (!vect_is_simple_use (arg, loop, &def_stmt))
		{
		  if (vect_debug_details (NULL))	
		    fprintf (dump_file, "worklist: unsupported use.");
		  varray_clear (worklist);
		  return false;
		}
	      if (!def_stmt)
		continue;

	      if (vect_debug_details (NULL))
	        {
	          fprintf (dump_file, "worklist: def_stmt: ");
		  print_generic_expr (dump_file, def_stmt, TDF_SLIM);
		}

	      bb = bb_for_stmt (def_stmt);
	      if (flow_bb_inside_loop_p (loop, bb))
	        vect_mark_relevant (worklist, def_stmt);
	    }
	} 

      ann = stmt_ann (stmt);
      use_ops = USE_OPS (ann);

      for (i = 0; i < NUM_USES (use_ops); i++)
	{
	  tree use = USE_OP (use_ops, i);

	  /* We are only interested in uses that need to be vectorized. Uses 
	     that are used for address computation are not considered relevant.
	   */
	  if (exist_non_indexing_operands_for_use_p (use, stmt))
	    {
              tree def_stmt = NULL_TREE;
              basic_block bb;
              if (!vect_is_simple_use (use, loop, &def_stmt))
                {
                  if (vect_debug_details (NULL))        
                    fprintf (dump_file, "worklist: unsupported use.");
                  varray_clear (worklist);
                  return false;
                }

	      if (!def_stmt)
		continue;

              if (vect_debug_details (NULL))
                {
                  fprintf (dump_file, "worklist: examine use %d: ", i);
                  print_generic_expr (dump_file, use, TDF_SLIM);
                }

	      bb = bb_for_stmt (def_stmt);
	      if (flow_bb_inside_loop_p (loop, bb))
		vect_mark_relevant (worklist, def_stmt);
	    }
	}
    }				/* while worklist */

  varray_clear (worklist);
  return true;
}


/* Function vect_get_loop_niters.

   Determine how many iterations the loop is executed.  */

static tree
vect_get_loop_niters (struct loop *loop, HOST_WIDE_INT *number_of_iterations)
{
  tree niters;

  if (vect_debug_details (NULL))
    fprintf (dump_file, "\n<<get_loop_niters>>\n");

  niters = number_of_iterations_in_loop (loop);

  if (niters != NULL_TREE
      && niters != chrec_dont_know
      && host_integerp (niters,0))
    {
      *number_of_iterations = TREE_INT_CST_LOW (niters);

      if (vect_debug_details (NULL))
        fprintf (dump_file, "==> get_loop_niters:" HOST_WIDE_INT_PRINT_DEC,
				 *number_of_iterations);
    }

  return get_loop_exit_condition (loop);
}


/* Function vect_analyze_loop_form.

   Verify the following restrictions (some may be relaxed in the future):
   - it's an inner-most loop
   - number of BBs = 2 (which are the loop header and the latch)
   - the loop has a pre-header
   - the loop has a single entry and exit
   - the loop exit condition is simple enough, and the number of iterations
     can be analyzed (a countable loop).  */

static loop_vec_info
vect_analyze_loop_form (struct loop *loop)
{
  loop_vec_info loop_vinfo;
  tree loop_cond;
  HOST_WIDE_INT number_of_iterations = -1;

  if (vect_debug_details (loop))
    fprintf (dump_file, "\n<<vect_analyze_loop_form>>\n");

  if (loop->inner
      || !loop->single_exit
      || loop->num_nodes != 2)
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop))	
	{
	  fprintf (dump_file, "not vectorized: bad loop form. ");
	  if (loop->inner)
	    fprintf (dump_file, "nested loop.");
	  else if (!loop->single_exit)
	    fprintf (dump_file, "multiple exits.");
	  else if (loop->num_nodes != 2)
	    fprintf (dump_file, "too many BBs in loop.");
	}

      return NULL;
    }

  /* We assume that the loop exit condition is at the end of the loop. i.e,
     that the loop is represented as a do-while (with a proper if-guard
     before the loop if needed), where the loop header contains all the
     executable statements, and the latch is empty.  */
  if (!empty_block_p (loop->latch))
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop))
        fprintf (dump_file, "not vectorized: unexpectd loop form.");
      return NULL;
    }

  if (empty_block_p (loop->header))
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop))
        fprintf (dump_file, "not vectorized: empty loop.");
      return NULL;
    }

  loop_cond = vect_get_loop_niters (loop, &number_of_iterations);
  if (!loop_cond)
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop))
	fprintf (dump_file, "not vectorized: complicated exit condition.");
      return NULL;
    }

  if (number_of_iterations < 0)
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop))
        fprintf (dump_file, "not vectorized: unknown loop bound.");
      return NULL;
    }

  if (number_of_iterations == 0) /* CHECKME: can this happen? */
    {
      if (vect_debug_stats (loop) || vect_debug_details (loop))
	fprintf (dump_file, "not vectorized: number of iterations = 0.");
      return NULL;
    }

  loop_vinfo = new_loop_vec_info (loop);
  LOOP_VINFO_EXIT_COND (loop_vinfo) = loop_cond;
  LOOP_VINFO_NITERS (loop_vinfo) = number_of_iterations;

  return loop_vinfo;
}


/* Function vect_analyze_loop.

   Apply a set of analyses on LOOP, and create a loop_vec_info struct
   for it. The different analyses will record information in the
   loop_vec_info struct.  */

static loop_vec_info
vect_analyze_loop (struct loop *loop)
{
  bool ok;
  loop_vec_info loop_vinfo;

  if (vect_debug_details (NULL))
    fprintf (dump_file, "\n<<<<<<< analyze_loop_nest >>>>>>>\n");

  /* Check the CFG characteristics of the loop (nesting, entry/exit, etc.  */

  loop_vinfo = vect_analyze_loop_form (loop);
  if (!loop_vinfo)
    {
      if (vect_debug_details (loop))
	fprintf (dump_file, "bad loop form.");
      return NULL;
    }

  /* Find all data references in the loop (which correspond to vdefs/vuses)
     and analyze their evolution in the loop.

     FORNOW: Handle only simple, array references, which
     alignment can be forced, and aligned pointer-references.  */

  ok = vect_analyze_data_refs (loop_vinfo);
  if (!ok)
    {
      if (vect_debug_details (loop))
	fprintf (dump_file, "bad data references.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  /* Data-flow analysis to detect stmts that do not need to be vectorized.  */

  ok = vect_mark_stmts_to_be_vectorized (loop_vinfo);
  if (!ok)
    {
      if (vect_debug_details (loop))
	fprintf (dump_file, "unexpected pattern.");
      if (vect_debug_details (loop))
	fprintf (dump_file, "not vectorized: unexpected pattern.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  /* Check that all cross-iteration scalar data-flow cycles are OK.
     Cross-iteration cycles caused by virtual phis are analyzed separately.  */

  ok = vect_analyze_scalar_cycles (loop_vinfo);
  if (!ok)
    {
      if (vect_debug_details (loop))
	fprintf (dump_file, "bad scalar cycle.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  /* Analyze data dependences between the data-refs in the loop. 
     FORNOW: fail at the first data dependence that we encounter.  */

  ok = vect_analyze_data_ref_dependences (loop_vinfo);
  if (!ok)
    {
      if (vect_debug_details (loop))
	fprintf (dump_file, "bad data dependence.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  /* Analyze the access patterns of the data-refs in the loop (consecutive,
     complex, etc.). FORNOW: Only handle consecutive access pattern.  */

  ok = vect_analyze_data_ref_accesses (loop_vinfo);
  if (!ok)
    {
      if (vect_debug_details (loop))
	fprintf (dump_file, "bad data access.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  /* Analyze the alignment of the data-refs in the loop.
     FORNOW: Only aligned accesses are handled.  */

  ok = vect_analyze_data_refs_alignment (loop_vinfo);
  if (!ok)
    {
      if (vect_debug_details (loop))
	fprintf (dump_file, "bad data alignment.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  /* Scan all the operations in the loop and make sure they are
     vectorizable.  */

  ok = vect_analyze_operations (loop_vinfo);
  if (!ok)
    {
      if (vect_debug_details (loop))
	fprintf (dump_file, "bad operation or unsupported loop bound.");
      destroy_loop_vec_info (loop_vinfo);
      return NULL;
    }

  LOOP_VINFO_VECTORIZABLE_P (loop_vinfo) = 1;

  return loop_vinfo;
}


/* Function need_imm_uses_for.

   Return whether we ought to include information for 'var'
   when calculating immediate uses.  For this pass we only want use
   information for non-virtual variables.  */

static bool
need_imm_uses_for (tree var)
{
  return is_gimple_reg (var);
}


/* Function vectorize_loops.
   
   Entry Point to loop vectorization phase.  */

void
vectorize_loops (struct loops *loops)
{
  unsigned int i, loops_num;
  unsigned int num_vectorized_loops = 0;

  /* Does the target support SIMD?  */
  /* FORNOW: until more sophisticated machine modelling is in place.  */
  if (!UNITS_PER_SIMD_WORD)
    {
      if (vect_debug_details (NULL))
	fprintf (dump_file, "vectorizer: target vector size is not defined.");
      return;
    }

  compute_immediate_uses (TDFA_USE_OPS, need_imm_uses_for);

  /*  ----------- Analyze loops. -----------  */

  /* If some loop was duplicated, it gets bigger number 
     than all previously defined loops. This fact allows us to run 
     only over initial loops skipping newly generated ones.  */
  loops_num = loops->num;
  for (i = 1; i < loops_num; i++)
    {
      loop_vec_info loop_vinfo;
      struct loop *loop = loops->parray[i];

      if (!loop)
        continue;

      loop_vinfo = vect_analyze_loop (loop);
      loop->aux = loop_vinfo;

      if (!loop_vinfo || !LOOP_VINFO_VECTORIZABLE_P (loop_vinfo))
	continue;

      vect_transform_loop (loop_vinfo, loops); 
      num_vectorized_loops++;
    }

  if (vect_debug_stats (NULL) || vect_debug_details (NULL))
    fprintf (dump_file, "\nvectorized %u loops in function.\n",
	     num_vectorized_loops);

  /*  ----------- Finalize. -----------  */

  free_df ();
  for (i = 1; i < loops_num; i++)
    {
      struct loop *loop = loops->parray[i];
      loop_vec_info loop_vinfo;

      if (!loop)
	continue;
      loop_vinfo = loop->aux;
      destroy_loop_vec_info (loop_vinfo);
      loop->aux = NULL;
    }

  rewrite_into_ssa (false);
  if (bitmap_first_set_bit (vars_to_rename) >= 0)
    {
      /* The rewrite of ssa names may cause violation of loop closed ssa
         form invariants.  TODO -- avoid these rewrites completely.
         Information in virtual phi nodes is sufficient for it.  */
      rewrite_into_loop_closed_ssa (); 
    }
  bitmap_clear (vars_to_rename);
}
