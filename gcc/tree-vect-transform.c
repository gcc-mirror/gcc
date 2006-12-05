/* Transformation Utilities for Loop Vectorization.
   Copyright (C) 2003,2004,2005,2006 Free Software Foundation, Inc.
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
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
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
#include "expr.h"
#include "optabs.h"
#include "recog.h"
#include "tree-data-ref.h"
#include "tree-chrec.h"
#include "tree-scalar-evolution.h"
#include "tree-vectorizer.h"
#include "langhooks.h"
#include "tree-pass.h"
#include "toplev.h"
#include "real.h"

/* Utility functions for the code transformation.  */
static bool vect_transform_stmt (tree, block_stmt_iterator *, bool *);
static tree vect_create_destination_var (tree, tree);
static tree vect_create_data_ref_ptr 
  (tree, block_stmt_iterator *, tree, tree *, tree *, bool, tree); 
static tree vect_create_addr_base_for_vector_ref (tree, tree *, tree);
static tree vect_setup_realignment (tree, block_stmt_iterator *, tree *);
static tree vect_get_new_vect_var (tree, enum vect_var_kind, const char *);
static tree vect_get_vec_def_for_operand (tree, tree, tree *);
static tree vect_init_vector (tree, tree, tree);
static void vect_finish_stmt_generation 
  (tree stmt, tree vec_stmt, block_stmt_iterator *bsi);
static bool vect_is_simple_cond (tree, loop_vec_info); 
static void update_vuses_to_preheader (tree, struct loop*);
static void vect_create_epilog_for_reduction (tree, tree, enum tree_code, tree);
static tree get_initial_def_for_reduction (tree, tree, tree *);

/* Utility function dealing with loop peeling (not peeling itself).  */
static void vect_generate_tmps_on_preheader 
  (loop_vec_info, tree *, tree *, tree *);
static tree vect_build_loop_niters (loop_vec_info);
static void vect_update_ivs_after_vectorizer (loop_vec_info, tree, edge); 
static tree vect_gen_niters_for_prolog_loop (loop_vec_info, tree);
static void vect_update_init_of_dr (struct data_reference *, tree niters);
static void vect_update_inits_of_drs (loop_vec_info, tree);
static int vect_min_worthwhile_factor (enum tree_code);


/* Function vect_get_new_vect_var.

   Returns a name for a new variable. The current naming scheme appends the 
   prefix "vect_" or "vect_p" (depending on the value of VAR_KIND) to 
   the name of vectorizer generated variables, and appends that to NAME if 
   provided.  */

static tree
vect_get_new_vect_var (tree type, enum vect_var_kind var_kind, const char *name)
{
  const char *prefix;
  tree new_vect_var;

  switch (var_kind)
  {
  case vect_simple_var:
    prefix = "vect_";
    break;
  case vect_scalar_var:
    prefix = "stmp_";
    break;
  case vect_pointer_var:
    prefix = "vect_p";
    break;
  default:
    gcc_unreachable ();
  }

  if (name)
    new_vect_var = create_tmp_var (type, concat (prefix, name, NULL));
  else
    new_vect_var = create_tmp_var (type, prefix);

  return new_vect_var;
}


/* Function vect_create_addr_base_for_vector_ref.

   Create an expression that computes the address of the first memory location
   that will be accessed for a data reference.

   Input:
   STMT: The statement containing the data reference.
   NEW_STMT_LIST: Must be initialized to NULL_TREE or a statement list.
   OFFSET: Optional. If supplied, it is be added to the initial address.

   Output:
   1. Return an SSA_NAME whose value is the address of the memory location of 
      the first vector of the data reference.
   2. If new_stmt_list is not NULL_TREE after return then the caller must insert
      these statement(s) which define the returned SSA_NAME.

   FORNOW: We are only handling array accesses with step 1.  */

static tree
vect_create_addr_base_for_vector_ref (tree stmt,
                                      tree *new_stmt_list,
				      tree offset)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  tree data_ref_base = unshare_expr (DR_BASE_ADDRESS (dr));
  tree base_name = build_fold_indirect_ref (data_ref_base);
  tree vec_stmt;
  tree addr_base, addr_expr;
  tree dest, new_stmt;
  tree base_offset = unshare_expr (DR_OFFSET (dr));
  tree init = unshare_expr (DR_INIT (dr));
  tree vect_ptr_type, addr_expr2;

  /* Create base_offset */
  base_offset = size_binop (PLUS_EXPR, base_offset, init);
  dest = create_tmp_var (TREE_TYPE (base_offset), "base_off");
  add_referenced_var (dest);
  base_offset = force_gimple_operand (base_offset, &new_stmt, false, dest);  
  append_to_statement_list_force (new_stmt, new_stmt_list);

  if (offset)
    {
      tree tmp = create_tmp_var (TREE_TYPE (base_offset), "offset");
      tree step; 

      /* For interleaved access step we divide STEP by the size of the
        interleaving group.  */
      if (DR_GROUP_SIZE (stmt_info))
	step = fold_build2 (TRUNC_DIV_EXPR, TREE_TYPE (offset), DR_STEP (dr),
			    build_int_cst (TREE_TYPE (offset),
					   DR_GROUP_SIZE (stmt_info)));
      else
	step = DR_STEP (dr);

      add_referenced_var (tmp);
      offset = fold_build2 (MULT_EXPR, TREE_TYPE (offset), offset, step);
      base_offset = fold_build2 (PLUS_EXPR, TREE_TYPE (base_offset),
				 base_offset, offset);
      base_offset = force_gimple_operand (base_offset, &new_stmt, false, tmp);  
      append_to_statement_list_force (new_stmt, new_stmt_list);
    }
  
  /* base + base_offset */
  addr_base = fold_build2 (PLUS_EXPR, TREE_TYPE (data_ref_base), data_ref_base,
			   base_offset);

  vect_ptr_type = build_pointer_type (STMT_VINFO_VECTYPE (stmt_info));

  /* addr_expr = addr_base */
  addr_expr = vect_get_new_vect_var (vect_ptr_type, vect_pointer_var,
                                     get_name (base_name));
  add_referenced_var (addr_expr);
  vec_stmt = fold_convert (vect_ptr_type, addr_base);
  addr_expr2 = vect_get_new_vect_var (vect_ptr_type, vect_pointer_var,
                                     get_name (base_name));
  add_referenced_var (addr_expr2);
  vec_stmt = force_gimple_operand (vec_stmt, &new_stmt, false, addr_expr2);
  append_to_statement_list_force (new_stmt, new_stmt_list);

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "created ");
      print_generic_expr (vect_dump, vec_stmt, TDF_SLIM);
    }
  return vec_stmt;
}


/* Function vect_create_data_ref_ptr.

   Create a new pointer to vector type (vp), that points to the first location
   accessed in the loop by STMT, along with the def-use update chain to 
   appropriately advance the pointer through the loop iterations. Also set
   aliasing information for the pointer.  This vector pointer is used by the
   callers to this function to create a memory reference expression for vector 
   load/store access.

   Input:
   1. STMT: a stmt that references memory. Expected to be of the form
         GIMPLE_MODIFY_STMT <name, data-ref> or
	 GIMPLE_MODIFY_STMT <data-ref, name>.
   2. BSI: block_stmt_iterator where new stmts can be added.
   3. OFFSET (optional): an offset to be added to the initial address accessed
        by the data-ref in STMT.
   4. ONLY_INIT: indicate if vp is to be updated in the loop, or remain
        pointing to the initial address.
   5. TYPE: if not NULL indicates the required type of the data-ref

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

   2. If ONLY_INIT is true, just return the initial pointer.  Otherwise, also
      update the pointer in each iteration of the loop.  

      Return the increment stmt that updates the pointer in PTR_INCR.

   3. Return the pointer.  */

static tree
vect_create_data_ref_ptr (tree stmt,
			  block_stmt_iterator *bsi ATTRIBUTE_UNUSED,
			  tree offset, tree *initial_address, tree *ptr_incr,
			  bool only_init, tree type)
{
  tree base_name;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree vect_ptr_type;
  tree vect_ptr;
  tree tag;
  tree new_temp;
  tree vec_stmt;
  tree new_stmt_list = NULL_TREE;
  edge pe = loop_preheader_edge (loop);
  basic_block new_bb;
  tree vect_ptr_init;
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);

  base_name =  build_fold_indirect_ref (unshare_expr (DR_BASE_ADDRESS (dr)));

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      tree data_ref_base = base_name;
      fprintf (vect_dump, "create vector-pointer variable to type: ");
      print_generic_expr (vect_dump, vectype, TDF_SLIM);
      if (TREE_CODE (data_ref_base) == VAR_DECL)
        fprintf (vect_dump, "  vectorizing a one dimensional array ref: ");
      else if (TREE_CODE (data_ref_base) == ARRAY_REF)
        fprintf (vect_dump, "  vectorizing a multidimensional array ref: ");
      else if (TREE_CODE (data_ref_base) == COMPONENT_REF)
        fprintf (vect_dump, "  vectorizing a record based array ref: ");
      else if (TREE_CODE (data_ref_base) == SSA_NAME)
        fprintf (vect_dump, "  vectorizing a pointer ref: ");
      print_generic_expr (vect_dump, base_name, TDF_SLIM);
    }

  /** (1) Create the new vector-pointer variable:  **/
  if (type)  
    vect_ptr_type = build_pointer_type (type);
  else
    vect_ptr_type = build_pointer_type (vectype);
  vect_ptr = vect_get_new_vect_var (vect_ptr_type, vect_pointer_var,
                                    get_name (base_name));
  add_referenced_var (vect_ptr);

  /** (2) Add aliasing information to the new vector-pointer:
          (The points-to info (DR_PTR_INFO) may be defined later.)  **/
  
  tag = DR_MEMTAG (dr);
  gcc_assert (tag);

  /* If tag is a variable (and NOT_A_TAG) than a new symbol memory
     tag must be created with tag added to its may alias list.  */
  if (!MTAG_P (tag))
    new_type_alias (vect_ptr, tag, DR_REF (dr));
  else
    var_ann (vect_ptr)->symbol_mem_tag = tag;

  var_ann (vect_ptr)->subvars = DR_SUBVARS (dr);

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
  vec_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, vect_ptr, vec_stmt);
  vect_ptr_init = make_ssa_name (vect_ptr, vec_stmt);
  GIMPLE_STMT_OPERAND (vec_stmt, 0) = vect_ptr_init;
  new_bb = bsi_insert_on_edge_immediate (pe, vec_stmt);
  gcc_assert (!new_bb);


  /** (4) Handle the updating of the vector-pointer inside the loop: **/

  if (only_init) /* No update in loop is required.  */
    {
      /* Copy the points-to information if it exists. */
      if (DR_PTR_INFO (dr))
        duplicate_ssa_name_ptr_info (vect_ptr_init, DR_PTR_INFO (dr));
      return vect_ptr_init;
    }
  else
    {
      block_stmt_iterator incr_bsi;
      bool insert_after;
      tree indx_before_incr, indx_after_incr;
      tree incr;

      standard_iv_increment_position (loop, &incr_bsi, &insert_after);
      create_iv (vect_ptr_init,
		 fold_convert (vect_ptr_type, TYPE_SIZE_UNIT (vectype)),
		 NULL_TREE, loop, &incr_bsi, insert_after,
		 &indx_before_incr, &indx_after_incr);
      incr = bsi_stmt (incr_bsi);
      set_stmt_info (stmt_ann (incr),
		     new_stmt_vec_info (incr, loop_vinfo));

      /* Copy the points-to information if it exists. */
      if (DR_PTR_INFO (dr))
	{
	  duplicate_ssa_name_ptr_info (indx_before_incr, DR_PTR_INFO (dr));
	  duplicate_ssa_name_ptr_info (indx_after_incr, DR_PTR_INFO (dr));
	}
      merge_alias_info (vect_ptr_init, indx_before_incr);
      merge_alias_info (vect_ptr_init, indx_after_incr);
      if (ptr_incr)
	*ptr_incr = incr;

      return indx_before_incr;
    }
}


/* Function bump_vector_ptr

   Increment a pointer (to a vector type) by vector-size. Connect the new 
   increment stmt to the existing def-use update-chain of the pointer.

   The pointer def-use update-chain before this function:
                        DATAREF_PTR = phi (p_0, p_2)
                        ....
        PTR_INCR:       p_2 = DATAREF_PTR + step 

   The pointer def-use update-chain after this function:
                        DATAREF_PTR = phi (p_0, p_2)
                        ....
                        NEW_DATAREF_PTR = DATAREF_PTR + vector_size
                        ....
        PTR_INCR:       p_2 = NEW_DATAREF_PTR + step

   Input:
   DATAREF_PTR - ssa_name of a pointer (to vector type) that is being updated 
                 in the loop.
   PTR_INCR - the stmt that updates the pointer in each iteration of the loop.
              The increment amount across iterations is also expected to be
              vector_size.      
   BSI - location where the new update stmt is to be placed.
   STMT - the original scalar memory-access stmt that is being vectorized.

   Output: Return NEW_DATAREF_PTR as illustrated above.
   
*/

static tree
bump_vector_ptr (tree dataref_ptr, tree ptr_incr, block_stmt_iterator *bsi,
                 tree stmt)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree vptr_type = TREE_TYPE (dataref_ptr);
  tree ptr_var = SSA_NAME_VAR (dataref_ptr);
  tree update = fold_convert (vptr_type, TYPE_SIZE_UNIT (vectype));
  tree incr_stmt;
  ssa_op_iter iter;
  use_operand_p use_p;
  tree new_dataref_ptr;

  incr_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, ptr_var,
                build2 (PLUS_EXPR, vptr_type, dataref_ptr, update));
  new_dataref_ptr = make_ssa_name (ptr_var, incr_stmt);
  GIMPLE_STMT_OPERAND (incr_stmt, 0) = new_dataref_ptr;
  vect_finish_stmt_generation (stmt, incr_stmt, bsi);

  /* Update the vector-pointer's cross-iteration increment.  */
  FOR_EACH_SSA_USE_OPERAND (use_p, ptr_incr, iter, SSA_OP_USE)
    {
      tree use = USE_FROM_PTR (use_p);

      if (use == dataref_ptr)
        SET_USE (use_p, new_dataref_ptr);
      else
        gcc_assert (tree_int_cst_compare (use, update) == 0);
    }

  /* Copy the points-to information if it exists. */
  if (DR_PTR_INFO (dr))
    duplicate_ssa_name_ptr_info (new_dataref_ptr, DR_PTR_INFO (dr));
  merge_alias_info (new_dataref_ptr, dataref_ptr);

  return new_dataref_ptr;
}


/* Function vect_create_destination_var.

   Create a new temporary of type VECTYPE.  */

static tree
vect_create_destination_var (tree scalar_dest, tree vectype)
{
  tree vec_dest;
  const char *new_name;
  tree type;
  enum vect_var_kind kind;

  kind = vectype ? vect_simple_var : vect_scalar_var;
  type = vectype ? vectype : TREE_TYPE (scalar_dest);

  gcc_assert (TREE_CODE (scalar_dest) == SSA_NAME);

  new_name = get_name (scalar_dest);
  if (!new_name)
    new_name = "var_";
  vec_dest = vect_get_new_vect_var (type, vect_simple_var, new_name);
  add_referenced_var (vec_dest);

  return vec_dest;
}


/* Function vect_init_vector.

   Insert a new stmt (INIT_STMT) that initializes a new vector variable with
   the vector elements of VECTOR_VAR. Return the DEF of INIT_STMT. It will be
   used in the vectorization of STMT.  */

static tree
vect_init_vector (tree stmt, tree vector_var, tree vector_type)
{
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree new_var;
  tree init_stmt;
  tree vec_oprnd;
  edge pe;
  tree new_temp;
  basic_block new_bb;
 
  new_var = vect_get_new_vect_var (vector_type, vect_simple_var, "cst_");
  add_referenced_var (new_var); 
 
  init_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, new_var, vector_var);
  new_temp = make_ssa_name (new_var, init_stmt);
  GIMPLE_STMT_OPERAND (init_stmt, 0) = new_temp;

  pe = loop_preheader_edge (loop);
  new_bb = bsi_insert_on_edge_immediate (pe, init_stmt);
  gcc_assert (!new_bb);

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "created new init_stmt: ");
      print_generic_expr (vect_dump, init_stmt, TDF_SLIM);
    }

  vec_oprnd = GIMPLE_STMT_OPERAND (init_stmt, 0);
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
vect_get_vec_def_for_operand (tree op, tree stmt, tree *scalar_def)
{
  tree vec_oprnd;
  tree vec_stmt;
  tree def_stmt;
  stmt_vec_info def_stmt_info = NULL;
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_vinfo);
  int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree vec_inv;
  tree vec_cst;
  tree t = NULL_TREE;
  tree def;
  int i;
  enum vect_def_type dt;
  bool is_simple_use;
  tree vector_type;

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "vect_get_vec_def_for_operand: ");
      print_generic_expr (vect_dump, op, TDF_SLIM);
    }

  is_simple_use = vect_is_simple_use (op, loop_vinfo, &def_stmt, &def, &dt);
  gcc_assert (is_simple_use);
  if (vect_print_dump_info (REPORT_DETAILS))
    {
      if (def)
        {
          fprintf (vect_dump, "def =  ");
          print_generic_expr (vect_dump, def, TDF_SLIM);
        }
      if (def_stmt)
        {
          fprintf (vect_dump, "  def_stmt =  ");
          print_generic_expr (vect_dump, def_stmt, TDF_SLIM);
        }
    }

  switch (dt)
    {
    /* Case 1: operand is a constant.  */
    case vect_constant_def:
      {
	if (scalar_def) 
	  *scalar_def = op;

        /* Create 'vect_cst_ = {cst,cst,...,cst}'  */
        if (vect_print_dump_info (REPORT_DETAILS))
          fprintf (vect_dump, "Create vector_cst. nunits = %d", nunits);

        for (i = nunits - 1; i >= 0; --i)
          {
            t = tree_cons (NULL_TREE, op, t);
          }
        vector_type = get_vectype_for_scalar_type (TREE_TYPE (op));
        vec_cst = build_vector (vector_type, t);

        return vect_init_vector (stmt, vec_cst, vector_type);
      }

    /* Case 2: operand is defined outside the loop - loop invariant.  */
    case vect_invariant_def:
      {
	if (scalar_def) 
	  *scalar_def = def;

        /* Create 'vec_inv = {inv,inv,..,inv}'  */
        if (vect_print_dump_info (REPORT_DETAILS))
          fprintf (vect_dump, "Create vector_inv.");

        for (i = nunits - 1; i >= 0; --i)
          {
            t = tree_cons (NULL_TREE, def, t);
          }

	/* FIXME: use build_constructor directly.  */
	vector_type = get_vectype_for_scalar_type (TREE_TYPE (def));
        vec_inv = build_constructor_from_list (vector_type, t);
        return vect_init_vector (stmt, vec_inv, vector_type);
      }

    /* Case 3: operand is defined inside the loop.  */
    case vect_loop_def:
      {
	if (scalar_def) 
	  *scalar_def = def_stmt;

        /* Get the def from the vectorized stmt.  */
        def_stmt_info = vinfo_for_stmt (def_stmt);
        vec_stmt = STMT_VINFO_VEC_STMT (def_stmt_info);
        gcc_assert (vec_stmt);
        vec_oprnd = GIMPLE_STMT_OPERAND (vec_stmt, 0);
        return vec_oprnd;
      }

    /* Case 4: operand is defined by a loop header phi - reduction  */
    case vect_reduction_def:
      {
        gcc_assert (TREE_CODE (def_stmt) == PHI_NODE);

        /* Get the def before the loop  */
        op = PHI_ARG_DEF_FROM_EDGE (def_stmt, loop_preheader_edge (loop));
        return get_initial_def_for_reduction (stmt, op, scalar_def);
     }

    /* Case 5: operand is defined by loop-header phi - induction.  */
    case vect_induction_def:
      {
        if (vect_print_dump_info (REPORT_DETAILS))
          fprintf (vect_dump, "induction - unsupported.");
        internal_error ("no support for induction"); /* FORNOW */
      }

    default:
      gcc_unreachable ();
    }
}


/* Function vect_get_vec_def_for_stmt_copy

   Return a vector-def for an operand. This function is used when the 
   vectorized stmt to be created (by the caller to this function) is a "copy" 
   created in case the vectorized result cannot fit in one vector, and several 
   copies of the vector-stmt are required. In this case the vector-def is 
   retrieved from the vector stmt recorded in the STMT_VINFO_RELATED_STMT field 
   of the stmt that defines VEC_OPRND. 
   DT is the type of the vector def VEC_OPRND.

   Context:
        In case the vectorization factor (VF) is bigger than the number
   of elements that can fit in a vectype (nunits), we have to generate
   more than one vector stmt to vectorize the scalar stmt. This situation
   arises when there are multiple data-types operated upon in the loop; the 
   smallest data-type determines the VF, and as a result, when vectorizing
   stmts operating on wider types we need to create 'VF/nunits' "copies" of the
   vector stmt (each computing a vector of 'nunits' results, and together
   computing 'VF' results in each iteration).  This function is called when 
   vectorizing such a stmt (e.g. vectorizing S2 in the illustration below, in
   which VF=16 and nuniti=4, so the number of copies required is 4):

   scalar stmt:         vectorized into:        STMT_VINFO_RELATED_STMT
 
   S1: x = load         VS1.0:  vx.0 = memref0      VS1.1
                        VS1.1:  vx.1 = memref1      VS1.2
                        VS1.2:  vx.2 = memref2      VS1.3
                        VS1.3:  vx.3 = memref3 

   S2: z = x + ...      VSnew.0:  vz0 = vx.0 + ...  VSnew.1
                        VSnew.1:  vz1 = vx.1 + ...  VSnew.2
                        VSnew.2:  vz2 = vx.2 + ...  VSnew.3
                        VSnew.3:  vz3 = vx.3 + ...

   The vectorization of S1 is explained in vectorizable_load.
   The vectorization of S2:
        To create the first vector-stmt out of the 4 copies - VSnew.0 - 
   the function 'vect_get_vec_def_for_operand' is called to 
   get the relevant vector-def for each operand of S2. For operand x it
   returns  the vector-def 'vx.0'.

        To create the remaining copies of the vector-stmt (VSnew.j), this 
   function is called to get the relevant vector-def for each operand.  It is 
   obtained from the respective VS1.j stmt, which is recorded in the 
   STMT_VINFO_RELATED_STMT field of the stmt that defines VEC_OPRND.

        For example, to obtain the vector-def 'vx.1' in order to create the 
   vector stmt 'VSnew.1', this function is called with VEC_OPRND='vx.0'. 
   Given 'vx0' we obtain the stmt that defines it ('VS1.0'); from the 
   STMT_VINFO_RELATED_STMT field of 'VS1.0' we obtain the next copy - 'VS1.1',
   and return its def ('vx.1').
   Overall, to create the above sequence this function will be called 3 times:
        vx.1 = vect_get_vec_def_for_stmt_copy (dt, vx.0);
        vx.2 = vect_get_vec_def_for_stmt_copy (dt, vx.1);
        vx.3 = vect_get_vec_def_for_stmt_copy (dt, vx.2);  */

static tree
vect_get_vec_def_for_stmt_copy (enum vect_def_type dt, tree vec_oprnd)
{
  tree vec_stmt_for_operand;
  stmt_vec_info def_stmt_info;

  if (dt == vect_invariant_def || dt == vect_constant_def)
    {
      /* Do nothing; can reuse same def.  */ ;
      return vec_oprnd;
    }

  vec_stmt_for_operand = SSA_NAME_DEF_STMT (vec_oprnd);
  def_stmt_info = vinfo_for_stmt (vec_stmt_for_operand);
  gcc_assert (def_stmt_info);
  vec_stmt_for_operand = STMT_VINFO_RELATED_STMT (def_stmt_info);
  gcc_assert (vec_stmt_for_operand);
  vec_oprnd = GIMPLE_STMT_OPERAND (vec_stmt_for_operand, 0);

  return vec_oprnd;
}


/* Function vect_finish_stmt_generation.

   Insert a new stmt.  */

static void
vect_finish_stmt_generation (tree stmt, tree vec_stmt, 
			     block_stmt_iterator *bsi)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);

  bsi_insert_before (bsi, vec_stmt, BSI_SAME_STMT);
  set_stmt_info (get_stmt_ann (vec_stmt), 
		 new_stmt_vec_info (vec_stmt, loop_vinfo)); 

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "add new stmt: ");
      print_generic_expr (vect_dump, vec_stmt, TDF_SLIM);
    }

  /* Make sure bsi points to the stmt that is being vectorized.  */
  gcc_assert (stmt == bsi_stmt (*bsi));

#ifdef USE_MAPPED_LOCATION
  SET_EXPR_LOCATION (vec_stmt, EXPR_LOCATION (stmt));
#else
  SET_EXPR_LOCUS (vec_stmt, EXPR_LOCUS (stmt));
#endif
}


#define ADJUST_IN_EPILOG 1

/* Function get_initial_def_for_reduction

   Input:
   STMT - a stmt that performs a reduction operation in the loop.
   INIT_VAL - the initial value of the reduction variable

   Output:
   SCALAR_DEF - a tree that holds a value to be added to the final result
	of the reduction (used for "ADJUST_IN_EPILOG" - see below).
   Return a vector variable, initialized according to the operation that STMT
	performs. This vector will be used as the initial value of the
	vector of partial results.

   Option1 ("ADJUST_IN_EPILOG"): Initialize the vector as follows:
     add:         [0,0,...,0,0]
     mult:        [1,1,...,1,1]
     min/max:     [init_val,init_val,..,init_val,init_val]
     bit and/or:  [init_val,init_val,..,init_val,init_val]
   and when necessary (e.g. add/mult case) let the caller know 
   that it needs to adjust the result by init_val.

   Option2: Initialize the vector as follows:
     add:         [0,0,...,0,init_val]
     mult:        [1,1,...,1,init_val]
     min/max:     [init_val,init_val,...,init_val]
     bit and/or:  [init_val,init_val,...,init_val]
   and no adjustments are needed.

   For example, for the following code:

   s = init_val;
   for (i=0;i<n;i++)
     s = s + a[i];

   STMT is 's = s + a[i]', and the reduction variable is 's'.
   For a vector of 4 units, we want to return either [0,0,0,init_val],
   or [0,0,0,0] and let the caller know that it needs to adjust
   the result at the end by 'init_val'.

   FORNOW: We use the "ADJUST_IN_EPILOG" scheme.
   TODO: Use some cost-model to estimate which scheme is more profitable.
*/

static tree
get_initial_def_for_reduction (tree stmt, tree init_val, tree *scalar_def)
{
  stmt_vec_info stmt_vinfo = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_vinfo);
  int nunits = GET_MODE_NUNITS (TYPE_MODE (vectype));
  int nelements;
  enum tree_code code = TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 1));
  tree type = TREE_TYPE (init_val);
  tree def;
  tree vec, t = NULL_TREE;
  bool need_epilog_adjust;
  int i;
  tree vector_type;

  gcc_assert (INTEGRAL_TYPE_P (type) || SCALAR_FLOAT_TYPE_P (type));

  switch (code)
  {
  case WIDEN_SUM_EXPR:
  case DOT_PROD_EXPR:
  case PLUS_EXPR:
    if (INTEGRAL_TYPE_P (type))
      def = build_int_cst (type, 0);
    else
      def = build_real (type, dconst0);

#ifdef ADJUST_IN_EPILOG
    /* All the 'nunits' elements are set to 0. The final result will be
       adjusted by 'init_val' at the loop epilog.  */
    nelements = nunits;
    need_epilog_adjust = true;
#else
    /* 'nunits - 1' elements are set to 0; The last element is set to 
        'init_val'.  No further adjustments at the epilog are needed.  */
    nelements = nunits - 1;
    need_epilog_adjust = false;
#endif
    break;

  case MIN_EXPR:
  case MAX_EXPR:
    def = init_val;
    nelements = nunits;
    need_epilog_adjust = false;
    break;

  default:
    gcc_unreachable ();
  }

  for (i = nelements - 1; i >= 0; --i)
    t = tree_cons (NULL_TREE, def, t);

  if (nelements == nunits - 1)
    {
      /* Set the last element of the vector.  */
      t = tree_cons (NULL_TREE, init_val, t);
      nelements += 1;
    }
  gcc_assert (nelements == nunits);

  vector_type = get_vectype_for_scalar_type (TREE_TYPE (def));
  if (TREE_CODE (init_val) == INTEGER_CST || TREE_CODE (init_val) == REAL_CST)
    vec = build_vector (vector_type, t);
  else
    vec = build_constructor_from_list (vector_type, t);
    
  if (!need_epilog_adjust)
    *scalar_def = NULL_TREE;
  else
    *scalar_def = init_val;

  return vect_init_vector (stmt, vec, vector_type);
}


/* Function vect_create_epilog_for_reduction
    
   Create code at the loop-epilog to finalize the result of a reduction
   computation. 
  
   VECT_DEF is a vector of partial results. 
   REDUC_CODE is the tree-code for the epilog reduction.
   STMT is the scalar reduction stmt that is being vectorized.
   REDUCTION_PHI is the phi-node that carries the reduction computation.

   This function:
   1. Creates the reduction def-use cycle: sets the the arguments for 
      REDUCTION_PHI:
      The loop-entry argument is the vectorized initial-value of the reduction.
      The loop-latch argument is VECT_DEF - the vector of partial sums.
   2. "Reduces" the vector of partial results VECT_DEF into a single result,
      by applying the operation specified by REDUC_CODE if available, or by 
      other means (whole-vector shifts or a scalar loop).
      The function also creates a new phi node at the loop exit to preserve 
      loop-closed form, as illustrated below.
  
     The flow at the entry to this function:
    
        loop:
          vec_def = phi <null, null>            # REDUCTION_PHI
          VECT_DEF = vector_stmt                # vectorized form of STMT       
          s_loop = scalar_stmt                  # (scalar) STMT
        loop_exit:
          s_out0 = phi <s_loop>                 # (scalar) EXIT_PHI
          use <s_out0>
          use <s_out0>

     The above is transformed by this function into:

        loop:
          vec_def = phi <vec_init, VECT_DEF>    # REDUCTION_PHI
          VECT_DEF = vector_stmt                # vectorized form of STMT
          s_loop = scalar_stmt                  # (scalar) STMT 
        loop_exit:
          s_out0 = phi <s_loop>                 # (scalar) EXIT_PHI
          v_out1 = phi <VECT_DEF>               # NEW_EXIT_PHI
          v_out2 = reduce <v_out1>
          s_out3 = extract_field <v_out2, 0>
          s_out4 = adjust_result <s_out3>
          use <s_out4>
          use <s_out4>
*/

static void
vect_create_epilog_for_reduction (tree vect_def, tree stmt,
                                  enum tree_code reduc_code, tree reduction_phi)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype;
  enum machine_mode mode;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block exit_bb;
  tree scalar_dest;
  tree scalar_type;
  tree new_phi;
  block_stmt_iterator exit_bsi;
  tree vec_dest;
  tree new_temp;
  tree new_name;
  tree epilog_stmt;
  tree new_scalar_dest, exit_phi;
  tree bitsize, bitpos, bytesize; 
  enum tree_code code = TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 1));
  tree scalar_initial_def;
  tree vec_initial_def;
  tree orig_name;
  imm_use_iterator imm_iter;
  use_operand_p use_p;
  bool extract_scalar_result;
  tree reduction_op;
  tree orig_stmt;
  tree use_stmt;
  tree operation = GIMPLE_STMT_OPERAND (stmt, 1);
  int op_type;
  
  op_type = TREE_CODE_LENGTH (TREE_CODE (operation));
  reduction_op = TREE_OPERAND (operation, op_type-1);
  vectype = get_vectype_for_scalar_type (TREE_TYPE (reduction_op));
  mode = TYPE_MODE (vectype);

  /*** 1. Create the reduction def-use cycle  ***/
  
  /* 1.1 set the loop-entry arg of the reduction-phi:  */
  /* For the case of reduction, vect_get_vec_def_for_operand returns
     the scalar def before the loop, that defines the initial value
     of the reduction variable.  */
  vec_initial_def = vect_get_vec_def_for_operand (reduction_op, stmt,
						  &scalar_initial_def);
  add_phi_arg (reduction_phi, vec_initial_def, loop_preheader_edge (loop));

  /* 1.2 set the loop-latch arg for the reduction-phi:  */
  add_phi_arg (reduction_phi, vect_def, loop_latch_edge (loop));

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "transform reduction: created def-use cycle:");
      print_generic_expr (vect_dump, reduction_phi, TDF_SLIM);
      fprintf (vect_dump, "\n");
      print_generic_expr (vect_dump, SSA_NAME_DEF_STMT (vect_def), TDF_SLIM);
    }


  /*** 2. Create epilog code
	  The reduction epilog code operates across the elements of the vector
          of partial results computed by the vectorized loop.
          The reduction epilog code consists of:
          step 1: compute the scalar result in a vector (v_out2)
          step 2: extract the scalar result (s_out3) from the vector (v_out2)
          step 3: adjust the scalar result (s_out3) if needed.

          Step 1 can be accomplished using one the following three schemes:
          (scheme 1) using reduc_code, if available.
          (scheme 2) using whole-vector shifts, if available.
          (scheme 3) using a scalar loop. In this case steps 1+2 above are 
                     combined.
                
          The overall epilog code looks like this:

          s_out0 = phi <s_loop>         # original EXIT_PHI
          v_out1 = phi <VECT_DEF>       # NEW_EXIT_PHI
          v_out2 = reduce <v_out1>              # step 1
          s_out3 = extract_field <v_out2, 0>    # step 2
          s_out4 = adjust_result <s_out3>       # step 3

          (step 3 is optional, and step2 1 and 2 may be combined).
          Lastly, the uses of s_out0 are replaced by s_out4.

	  ***/

  /* 2.1 Create new loop-exit-phi to preserve loop-closed form:
        v_out1 = phi <v_loop>  */

  exit_bb = single_exit (loop)->dest;
  new_phi = create_phi_node (SSA_NAME_VAR (vect_def), exit_bb);
  SET_PHI_ARG_DEF (new_phi, single_exit (loop)->dest_idx, vect_def);
  exit_bsi = bsi_start (exit_bb);

  /* 2.2 Get the relevant tree-code to use in the epilog for schemes 2,3 
         (i.e. when reduc_code is not available) and in the final adjustment code
         (if needed).  Also get the original scalar reduction variable as
         defined in the loop.  In case STMT is a "pattern-stmt" (i.e. - it 
         represents a reduction pattern), the tree-code and scalar-def are 
         taken from the original stmt that the pattern-stmt (STMT) replaces.  
         Otherwise (it is a regular reduction) - the tree-code and scalar-def
         are taken from STMT.  */ 

  orig_stmt = STMT_VINFO_RELATED_STMT (stmt_info);
  if (!orig_stmt)
    {
      /* Regular reduction  */
      orig_stmt = stmt;
    }
  else
    {
      /* Reduction pattern  */
      stmt_vec_info stmt_vinfo = vinfo_for_stmt (orig_stmt);
      gcc_assert (STMT_VINFO_IN_PATTERN_P (stmt_vinfo));
      gcc_assert (STMT_VINFO_RELATED_STMT (stmt_vinfo) == stmt);
    }
  code = TREE_CODE (GIMPLE_STMT_OPERAND (orig_stmt, 1));
  scalar_dest = GIMPLE_STMT_OPERAND (orig_stmt, 0);
  scalar_type = TREE_TYPE (scalar_dest);
  new_scalar_dest = vect_create_destination_var (scalar_dest, NULL);
  bitsize = TYPE_SIZE (scalar_type);
  bytesize = TYPE_SIZE_UNIT (scalar_type);

  /* 2.3 Create the reduction code, using one of the three schemes described
         above.  */

  if (reduc_code < NUM_TREE_CODES)
    {
      /*** Case 1:  Create:
	   v_out2 = reduc_expr <v_out1>  */

      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "Reduce using direct vector reduction.");

      vec_dest = vect_create_destination_var (scalar_dest, vectype);
      epilog_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, vec_dest,
			build1 (reduc_code, vectype,  PHI_RESULT (new_phi)));
      new_temp = make_ssa_name (vec_dest, epilog_stmt);
      GIMPLE_STMT_OPERAND (epilog_stmt, 0) = new_temp;
      bsi_insert_after (&exit_bsi, epilog_stmt, BSI_NEW_STMT);

      extract_scalar_result = true;
    }
  else
    {
      enum tree_code shift_code = 0;
      bool have_whole_vector_shift = true;
      int bit_offset;
      int element_bitsize = tree_low_cst (bitsize, 1);
      int vec_size_in_bits = tree_low_cst (TYPE_SIZE (vectype), 1);
      tree vec_temp;

      if (vec_shr_optab->handlers[mode].insn_code != CODE_FOR_nothing)
	shift_code = VEC_RSHIFT_EXPR;
      else
	have_whole_vector_shift = false;

      /* Regardless of whether we have a whole vector shift, if we're
	 emulating the operation via tree-vect-generic, we don't want
	 to use it.  Only the first round of the reduction is likely
	 to still be profitable via emulation.  */
      /* ??? It might be better to emit a reduction tree code here, so that
	 tree-vect-generic can expand the first round via bit tricks.  */
      if (!VECTOR_MODE_P (mode))
	have_whole_vector_shift = false;
      else
	{
	  optab optab = optab_for_tree_code (code, vectype);
	  if (optab->handlers[mode].insn_code == CODE_FOR_nothing)
	    have_whole_vector_shift = false;
	}

      if (have_whole_vector_shift)
        {
	  /*** Case 2: Create:
	     for (offset = VS/2; offset >= element_size; offset/=2)
	        {
	          Create:  va' = vec_shift <va, offset>
	          Create:  va = vop <va, va'>
	        }  */

	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "Reduce using vector shifts");

	  vec_dest = vect_create_destination_var (scalar_dest, vectype);
	  new_temp = PHI_RESULT (new_phi);

	  for (bit_offset = vec_size_in_bits/2;
	       bit_offset >= element_bitsize;
	       bit_offset /= 2)
	    {
	      tree bitpos = size_int (bit_offset);

	      epilog_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node,
		  		    vec_dest,
				    build2 (shift_code, vectype,
					    new_temp, bitpos));
	      new_name = make_ssa_name (vec_dest, epilog_stmt);
	      GIMPLE_STMT_OPERAND (epilog_stmt, 0) = new_name;
	      bsi_insert_after (&exit_bsi, epilog_stmt, BSI_NEW_STMT);

	      epilog_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node,
		  		    vec_dest,
				    build2 (code, vectype,
					    new_name, new_temp));
	      new_temp = make_ssa_name (vec_dest, epilog_stmt);
	      GIMPLE_STMT_OPERAND (epilog_stmt, 0) = new_temp;
	      bsi_insert_after (&exit_bsi, epilog_stmt, BSI_NEW_STMT);
	    }

	  extract_scalar_result = true;
	}
      else
        {
	  tree rhs;

	  /*** Case 3: Create:  
	     s = extract_field <v_out2, 0>
	     for (offset = element_size; 
		  offset < vector_size; 
		  offset += element_size;)
	       {
	         Create:  s' = extract_field <v_out2, offset>
	         Create:  s = op <s, s'>
	       }  */

	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "Reduce using scalar code. ");

	  vec_temp = PHI_RESULT (new_phi);
	  vec_size_in_bits = tree_low_cst (TYPE_SIZE (vectype), 1);
	  rhs = build3 (BIT_FIELD_REF, scalar_type, vec_temp, bitsize,
			 bitsize_zero_node);
	  BIT_FIELD_REF_UNSIGNED (rhs) = TYPE_UNSIGNED (scalar_type);
	  epilog_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node,
	      			new_scalar_dest, rhs);
	  new_temp = make_ssa_name (new_scalar_dest, epilog_stmt);
	  GIMPLE_STMT_OPERAND (epilog_stmt, 0) = new_temp;
	  bsi_insert_after (&exit_bsi, epilog_stmt, BSI_NEW_STMT);
	      
	  for (bit_offset = element_bitsize;
	       bit_offset < vec_size_in_bits;
	       bit_offset += element_bitsize)
	    { 
	      tree bitpos = bitsize_int (bit_offset);
	      tree rhs = build3 (BIT_FIELD_REF, scalar_type, vec_temp, bitsize,
				 bitpos);
		
	      BIT_FIELD_REF_UNSIGNED (rhs) = TYPE_UNSIGNED (scalar_type);
	      epilog_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node,
		  		    new_scalar_dest, rhs);	
	      new_name = make_ssa_name (new_scalar_dest, epilog_stmt);
	      GIMPLE_STMT_OPERAND (epilog_stmt, 0) = new_name;
	      bsi_insert_after (&exit_bsi, epilog_stmt, BSI_NEW_STMT);

	      epilog_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node,
		  		new_scalar_dest,
				build2 (code, scalar_type, new_name, new_temp));
	      new_temp = make_ssa_name (new_scalar_dest, epilog_stmt);
	      GIMPLE_STMT_OPERAND (epilog_stmt, 0) = new_temp;
	      bsi_insert_after (&exit_bsi, epilog_stmt, BSI_NEW_STMT);
	    }

	  extract_scalar_result = false;
	}
    }

  /* 2.4  Extract the final scalar result.  Create:
         s_out3 = extract_field <v_out2, bitpos>  */
  
  if (extract_scalar_result)
    {
      tree rhs;

      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "extract scalar result");

      if (BYTES_BIG_ENDIAN)
	bitpos = size_binop (MULT_EXPR,
		       bitsize_int (TYPE_VECTOR_SUBPARTS (vectype) - 1),
		       TYPE_SIZE (scalar_type));
      else
	bitpos = bitsize_zero_node;

      rhs = build3 (BIT_FIELD_REF, scalar_type, new_temp, bitsize, bitpos);
      BIT_FIELD_REF_UNSIGNED (rhs) = TYPE_UNSIGNED (scalar_type);
      epilog_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node,
	  		    new_scalar_dest, rhs);
      new_temp = make_ssa_name (new_scalar_dest, epilog_stmt);
      GIMPLE_STMT_OPERAND (epilog_stmt, 0) = new_temp; 
      bsi_insert_after (&exit_bsi, epilog_stmt, BSI_NEW_STMT);
    }

  /* 2.4 Adjust the final result by the initial value of the reduction
	 variable. (When such adjustment is not needed, then
	 'scalar_initial_def' is zero).

	 Create: 
	 s_out4 = scalar_expr <s_out3, scalar_initial_def>  */
  
  if (scalar_initial_def)
    {
      epilog_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node,
	  	      new_scalar_dest,
                      build2 (code, scalar_type, new_temp, scalar_initial_def));
      new_temp = make_ssa_name (new_scalar_dest, epilog_stmt);
      GIMPLE_STMT_OPERAND (epilog_stmt, 0) = new_temp;
      bsi_insert_after (&exit_bsi, epilog_stmt, BSI_NEW_STMT);
    }

  /* 2.6 Replace uses of s_out0 with uses of s_out3  */

  /* Find the loop-closed-use at the loop exit of the original scalar result.  
     (The reduction result is expected to have two immediate uses - one at the 
     latch block, and one at the loop exit).  */
  exit_phi = NULL;
  FOR_EACH_IMM_USE_FAST (use_p, imm_iter, scalar_dest)
    {
      if (!flow_bb_inside_loop_p (loop, bb_for_stmt (USE_STMT (use_p))))
	{
	  exit_phi = USE_STMT (use_p);
	  break;
	}
    }
  /* We expect to have found an exit_phi because of loop-closed-ssa form.  */
  gcc_assert (exit_phi);
  /* Replace the uses:  */
  orig_name = PHI_RESULT (exit_phi);
  FOR_EACH_IMM_USE_STMT (use_stmt, imm_iter, orig_name)
    FOR_EACH_IMM_USE_ON_STMT (use_p, imm_iter)
      SET_USE (use_p, new_temp);
} 


/* Function vectorizable_reduction.

   Check if STMT performs a reduction operation that can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.

   This function also handles reduction idioms (patterns) that have been 
   recognized in advance during vect_pattern_recog. In this case, STMT may be
   of this form:
     X = pattern_expr (arg0, arg1, ..., X)
   and it's STMT_VINFO_RELATED_STMT points to the last stmt in the original
   sequence that had been detected and replaced by the pattern-stmt (STMT).
  
   In some cases of reduction patterns, the type of the reduction variable X is 
   different than the type of the other arguments of STMT.
   In such cases, the vectype that is used when transforming STMT into a vector
   stmt is different than the vectype that is used to determine the 
   vectorization factor, because it consists of a different number of elements 
   than the actual number of elements that are being operated upon in parallel.

   For example, consider an accumulation of shorts into an int accumulator. 
   On some targets it's possible to vectorize this pattern operating on 8
   shorts at a time (hence, the vectype for purposes of determining the
   vectorization factor should be V8HI); on the other hand, the vectype that
   is used to create the vector form is actually V4SI (the type of the result). 

   Upon entry to this function, STMT_VINFO_VECTYPE records the vectype that 
   indicates what is the actual level of parallelism (V8HI in the example), so 
   that the right vectorization factor would be derived. This vectype 
   corresponds to the type of arguments to the reduction stmt, and should *NOT* 
   be used to create the vectorized stmt. The right vectype for the vectorized
   stmt is obtained from the type of the result X: 
        get_vectype_for_scalar_type (TREE_TYPE (X))

   This means that, contrary to "regular" reductions (or "regular" stmts in 
   general), the following equation:
      STMT_VINFO_VECTYPE == get_vectype_for_scalar_type (TREE_TYPE (X))
   does *NOT* necessarily hold for reduction patterns.  */

bool
vectorizable_reduction (tree stmt, block_stmt_iterator *bsi, tree *vec_stmt)
{
  tree vec_dest;
  tree scalar_dest;
  tree op;
  tree loop_vec_def0 = NULL_TREE, loop_vec_def1 = NULL_TREE;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree operation;
  enum tree_code code, orig_code, epilog_reduc_code = 0;
  enum machine_mode vec_mode;
  int op_type;
  optab optab, reduc_optab;
  tree new_temp = NULL_TREE;
  tree def, def_stmt;
  enum vect_def_type dt;
  tree new_phi;
  tree scalar_type;
  bool is_simple_use;
  tree orig_stmt;
  stmt_vec_info orig_stmt_info;
  tree expr = NULL_TREE;
  int i;
  int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits;
  stmt_vec_info prev_stmt_info;
  tree reduc_def;
  tree new_stmt = NULL_TREE;
  int j;

  gcc_assert (ncopies >= 1);

  /* 1. Is vectorizable reduction?  */

  /* Not supportable if the reduction variable is used in the loop.  */
  if (STMT_VINFO_RELEVANT_P (stmt_info))
    return false;

  if (!STMT_VINFO_LIVE_P (stmt_info))
    return false;

  /* Make sure it was already recognized as a reduction computation.  */
  if (STMT_VINFO_DEF_TYPE (stmt_info) != vect_reduction_def)
    return false;

  /* 2. Has this been recognized as a reduction pattern? 

     Check if STMT represents a pattern that has been recognized
     in earlier analysis stages.  For stmts that represent a pattern,
     the STMT_VINFO_RELATED_STMT field records the last stmt in
     the original sequence that constitutes the pattern.  */

  orig_stmt = STMT_VINFO_RELATED_STMT (stmt_info);
  if (orig_stmt)
    {
      orig_stmt_info = vinfo_for_stmt (orig_stmt);
      gcc_assert (STMT_VINFO_RELATED_STMT (orig_stmt_info) == stmt);
      gcc_assert (STMT_VINFO_IN_PATTERN_P (orig_stmt_info));
      gcc_assert (!STMT_VINFO_IN_PATTERN_P (stmt_info));
    }
 
  /* 3. Check the operands of the operation. The first operands are defined
        inside the loop body. The last operand is the reduction variable,
        which is defined by the loop-header-phi.  */

  gcc_assert (TREE_CODE (stmt) == GIMPLE_MODIFY_STMT);

  operation = GIMPLE_STMT_OPERAND (stmt, 1);
  code = TREE_CODE (operation);
  op_type = TREE_CODE_LENGTH (code);
  if (op_type != binary_op && op_type != ternary_op)
    return false;
  scalar_dest = GIMPLE_STMT_OPERAND (stmt, 0);
  scalar_type = TREE_TYPE (scalar_dest);

  /* All uses but the last are expected to be defined in the loop.
     The last use is the reduction variable.  */
  for (i = 0; i < op_type-1; i++)
    {
      op = TREE_OPERAND (operation, i);
      is_simple_use = vect_is_simple_use (op, loop_vinfo, &def_stmt, &def, &dt);
      gcc_assert (is_simple_use);
      gcc_assert (dt == vect_loop_def || dt == vect_invariant_def ||
                  dt == vect_constant_def);
    }

  op = TREE_OPERAND (operation, i);
  is_simple_use = vect_is_simple_use (op, loop_vinfo, &def_stmt, &def, &dt);
  gcc_assert (is_simple_use);
  gcc_assert (dt == vect_reduction_def);
  gcc_assert (TREE_CODE (def_stmt) == PHI_NODE);
  if (orig_stmt) 
    gcc_assert (orig_stmt == vect_is_simple_reduction (loop, def_stmt));
  else
    gcc_assert (stmt == vect_is_simple_reduction (loop, def_stmt));
  
  if (STMT_VINFO_LIVE_P (vinfo_for_stmt (def_stmt)))
    return false;

  /* 4. Supportable by target?  */

  /* 4.1. check support for the operation in the loop  */
  optab = optab_for_tree_code (code, vectype);
  if (!optab)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "no optab.");
      return false;
    }
  vec_mode = TYPE_MODE (vectype);
  if (optab->handlers[(int) vec_mode].insn_code == CODE_FOR_nothing)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "op not supported by target.");
      if (GET_MODE_SIZE (vec_mode) != UNITS_PER_WORD
          || LOOP_VINFO_VECT_FACTOR (loop_vinfo)
	     < vect_min_worthwhile_factor (code))
        return false;
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "proceeding using word mode.");
    }

  /* Worthwhile without SIMD support?  */
  if (!VECTOR_MODE_P (TYPE_MODE (vectype))
      && LOOP_VINFO_VECT_FACTOR (loop_vinfo)
	 < vect_min_worthwhile_factor (code))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "not worthwhile without SIMD support.");
      return false;
    }

  /* 4.2. Check support for the epilog operation.

          If STMT represents a reduction pattern, then the type of the
          reduction variable may be different than the type of the rest
          of the arguments.  For example, consider the case of accumulation
          of shorts into an int accumulator; The original code:
                        S1: int_a = (int) short_a;
          orig_stmt->   S2: int_acc = plus <int_a ,int_acc>;

          was replaced with:
                        STMT: int_acc = widen_sum <short_a, int_acc>

          This means that:
          1. The tree-code that is used to create the vector operation in the 
             epilog code (that reduces the partial results) is not the 
             tree-code of STMT, but is rather the tree-code of the original 
             stmt from the pattern that STMT is replacing. I.e, in the example 
             above we want to use 'widen_sum' in the loop, but 'plus' in the 
             epilog.
          2. The type (mode) we use to check available target support
             for the vector operation to be created in the *epilog*, is 
             determined by the type of the reduction variable (in the example 
             above we'd check this: plus_optab[vect_int_mode]).
             However the type (mode) we use to check available target support
             for the vector operation to be created *inside the loop*, is
             determined by the type of the other arguments to STMT (in the
             example we'd check this: widen_sum_optab[vect_short_mode]).
  
          This is contrary to "regular" reductions, in which the types of all 
          the arguments are the same as the type of the reduction variable. 
          For "regular" reductions we can therefore use the same vector type 
          (and also the same tree-code) when generating the epilog code and
          when generating the code inside the loop.  */

  if (orig_stmt)
    {
      /* This is a reduction pattern: get the vectype from the type of the
         reduction variable, and get the tree-code from orig_stmt.  */
      orig_code = TREE_CODE (GIMPLE_STMT_OPERAND (orig_stmt, 1));
      vectype = get_vectype_for_scalar_type (TREE_TYPE (def));
      vec_mode = TYPE_MODE (vectype);
    }
  else
    {
      /* Regular reduction: use the same vectype and tree-code as used for
         the vector code inside the loop can be used for the epilog code. */
      orig_code = code;
    }

  if (!reduction_code_for_scalar_code (orig_code, &epilog_reduc_code))
    return false;
  reduc_optab = optab_for_tree_code (epilog_reduc_code, vectype);
  if (!reduc_optab)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "no optab for reduction.");
      epilog_reduc_code = NUM_TREE_CODES;
    }
  if (reduc_optab->handlers[(int) vec_mode].insn_code == CODE_FOR_nothing)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "reduc op not supported by target.");
      epilog_reduc_code = NUM_TREE_CODES;
    }
 
  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = reduc_vec_info_type;
      return true;
    }

  /** Transform.  **/

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "transform reduction.");

  /* Create the destination vector  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

  /* Create the reduction-phi that defines the reduction-operand.  */
  new_phi = create_phi_node (vec_dest, loop->header);

  /* In case the vectorization factor (VF) is bigger than the number
     of elements that we can fit in a vectype (nunits), we have to generate
     more than one vector stmt - i.e - we need to "unroll" the
     vector stmt by a factor VF/nunits.  For more details see documentation
     in vectorizable_operation.  */

  prev_stmt_info = NULL;
  for (j = 0; j < ncopies; j++)
    {
      /* Handle uses.  */
      if (j == 0)
        {
          op = TREE_OPERAND (operation, 0);
          loop_vec_def0 = vect_get_vec_def_for_operand (op, stmt, NULL);
          if (op_type == ternary_op)
            {
              op = TREE_OPERAND (operation, 1);
              loop_vec_def1 = vect_get_vec_def_for_operand (op, stmt, NULL);
            }
                                                                                
          /* Get the vector def for the reduction variable from the phi node */
          reduc_def = PHI_RESULT (new_phi);
        }
      else
        {
          enum vect_def_type dt = vect_unknown_def_type; /* Dummy */
          loop_vec_def0 = vect_get_vec_def_for_stmt_copy (dt, loop_vec_def0);
          if (op_type == ternary_op)
            loop_vec_def1 = vect_get_vec_def_for_stmt_copy (dt, loop_vec_def1);
                                                                                
          /* Get the vector def for the reduction variable from the vectorized
             reduction operation generated in the previous iteration (j-1)  */
          reduc_def = GIMPLE_STMT_OPERAND (new_stmt ,0);
        }
                                                                                
      /* Arguments are ready. create the new vector stmt.  */
                                                                                
      if (op_type == binary_op)
        expr = build2 (code, vectype, loop_vec_def0, reduc_def);
      else
        expr = build3 (code, vectype, loop_vec_def0, loop_vec_def1, 
								reduc_def);
      new_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, vec_dest, expr);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      GIMPLE_STMT_OPERAND (new_stmt, 0) = new_temp;
      vect_finish_stmt_generation (stmt, new_stmt, bsi);
                                                                                
      if (j == 0)
	STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
      prev_stmt_info = vinfo_for_stmt (new_stmt);
    }
                                                                                
  /* Finalize the reduction-phi (set it's arguments) and create the
     epilog reduction code.  */
  vect_create_epilog_for_reduction (new_temp, stmt, epilog_reduc_code, new_phi);                                                                                
  return true;
}

/* Checks if CALL can be vectorized in type VECTYPE.  Returns
   true if the target has a vectorized version of the function,
   or false if the function cannot be vectorized.  */

bool
vectorizable_function (tree call, tree vectype)
{
  tree fndecl = get_callee_fndecl (call);

  /* We only handle functions that do not read or clobber memory -- i.e.
     const or novops ones.  */
  if (!(call_expr_flags (call) & (ECF_CONST | ECF_NOVOPS)))
    return false;

  if (!fndecl
      || TREE_CODE (fndecl) != FUNCTION_DECL
      || !DECL_BUILT_IN (fndecl))
    return false;

  if (targetm.vectorize.builtin_vectorized_function (DECL_FUNCTION_CODE (fndecl), vectype))
    return true;

  return false;
}

/* Returns an expression that performs a call to vectorized version
   of FNDECL in type VECTYPE, with the arguments given by ARGS.
   If extra statements need to be generated, they are inserted
   before BSI.  */

static tree
build_vectorized_function_call (tree fndecl,
				tree vectype, tree args)
{
  tree vfndecl;
  enum built_in_function code = DECL_FUNCTION_CODE (fndecl);

  /* The target specific builtin should be available.  */
  vfndecl = targetm.vectorize.builtin_vectorized_function (code, vectype);
  gcc_assert (vfndecl != NULL_TREE);

  return build_function_call_expr (vfndecl, args);
}

/* Function vectorizable_call.

   Check if STMT performs a function call that can be vectorized. 
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized 
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

bool
vectorizable_call (tree stmt, block_stmt_iterator *bsi, tree *vec_stmt)
{
  tree vec_dest;
  tree scalar_dest;
  tree operation;
  tree op, args, type;
  tree vec_oprnd, vargs, *pvargs_end;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  tree fndecl, rhs, new_temp, def, def_stmt;
  enum vect_def_type dt;

  /* Is STMT a vectorizable call?   */
  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    return false;

  if (TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 0)) != SSA_NAME)
    return false;

  operation = GIMPLE_STMT_OPERAND (stmt, 1);
  if (TREE_CODE (operation) != CALL_EXPR)
    return false;
   
  /* For now, we only vectorize functions if a target specific builtin
     is available.  TODO -- in some cases, it might be profitable to
     insert the calls for pieces of the vector, in order to be able
     to vectorize other operations in the loop.  */
  if (!vectorizable_function (operation, vectype))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "function is not vectorizable.");

      return false;
    }
  gcc_assert (!stmt_references_memory_p (stmt));

  for (args = TREE_OPERAND (operation, 1); args; args = TREE_CHAIN (args))
    {
      op = TREE_VALUE (args);

      if (!vect_is_simple_use (op, loop_vinfo, &def_stmt, &def, &dt))
	{
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "use not simple.");
	  return false;
	}
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = call_vec_info_type;
      return true;
    }

  /** Transform.  **/

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "transform operation.");

  /* Handle def.  */
  scalar_dest = GIMPLE_STMT_OPERAND (stmt, 0);
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

  /* Handle uses.  */
  vargs = NULL_TREE;
  pvargs_end = &vargs;
  for (args = TREE_OPERAND (operation, 1); args; args = TREE_CHAIN (args))
    {
      op = TREE_VALUE (args);
      vec_oprnd = vect_get_vec_def_for_operand (op, stmt, NULL);
	  
      *pvargs_end = tree_cons (NULL_TREE, vec_oprnd, NULL_TREE);
      pvargs_end = &TREE_CHAIN (*pvargs_end);
    }

  fndecl = get_callee_fndecl (operation);
  rhs = build_vectorized_function_call (fndecl, vectype, vargs);
  *vec_stmt = build2 (GIMPLE_MODIFY_STMT, vectype, vec_dest, rhs);
  new_temp = make_ssa_name (vec_dest, *vec_stmt);
  GIMPLE_STMT_OPERAND (*vec_stmt, 0) = new_temp;

  vect_finish_stmt_generation (stmt, *vec_stmt, bsi);

  /* The call in STMT might prevent it from being removed in dce.  We however
     cannot remove it here, due to the way the ssa name it defines is mapped
     to the new definition.  So just replace rhs of the statement with something
     harmless.  */
  type = TREE_TYPE (scalar_dest);
  TREE_OPERAND (stmt, 1) = fold_convert (type, integer_zero_node);

  return true;
}


/* Function vectorizable_assignment.

   Check if STMT performs an assignment (copy) that can be vectorized. 
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized 
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

bool
vectorizable_assignment (tree stmt, block_stmt_iterator *bsi, tree *vec_stmt)
{
  tree vec_dest;
  tree scalar_dest;
  tree op;
  tree vec_oprnd;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  tree new_temp;
  tree def, def_stmt;
  enum vect_def_type dt;
  int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits;

  gcc_assert (ncopies >= 1);
  if (ncopies > 1)
    return false; /* FORNOW */

  /* Is vectorizable assignment?  */
  if (!STMT_VINFO_RELEVANT_P (stmt_info))
    return false;

  gcc_assert (STMT_VINFO_DEF_TYPE (stmt_info) == vect_loop_def);

  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    return false;

  scalar_dest = GIMPLE_STMT_OPERAND (stmt, 0);
  if (TREE_CODE (scalar_dest) != SSA_NAME)
    return false;

  op = GIMPLE_STMT_OPERAND (stmt, 1);
  if (!vect_is_simple_use (op, loop_vinfo, &def_stmt, &def, &dt))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "use not simple.");
      return false;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = assignment_vec_info_type;
      return true;
    }

  /** Transform.  **/
  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "transform assignment.");

  /* Handle def.  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

  /* Handle use.  */
  op = GIMPLE_STMT_OPERAND (stmt, 1);
  vec_oprnd = vect_get_vec_def_for_operand (op, stmt, NULL);

  /* Arguments are ready. create the new vector stmt.  */
  *vec_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, vec_dest, vec_oprnd);
  new_temp = make_ssa_name (vec_dest, *vec_stmt);
  GIMPLE_STMT_OPERAND (*vec_stmt, 0) = new_temp;
  vect_finish_stmt_generation (stmt, *vec_stmt, bsi);
  
  return true;
}


/* Function vect_min_worthwhile_factor.

   For a loop where we could vectorize the operation indicated by CODE,
   return the minimum vectorization factor that makes it worthwhile
   to use generic vectors.  */
static int
vect_min_worthwhile_factor (enum tree_code code)
{
  switch (code)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
    case NEGATE_EXPR:
      return 4;

    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_NOT_EXPR:
      return 2;

    default:
      return INT_MAX;
    }
}


/* Function vectorizable_operation.

   Check if STMT performs a binary or unary operation that can be vectorized. 
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized 
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

bool
vectorizable_operation (tree stmt, block_stmt_iterator *bsi, tree *vec_stmt)
{
  tree vec_dest;
  tree scalar_dest;
  tree operation;
  tree op0, op1 = NULL;
  tree vec_oprnd0 = NULL_TREE, vec_oprnd1 = NULL_TREE;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  enum tree_code code;
  enum machine_mode vec_mode;
  tree new_temp;
  int op_type;
  optab optab;
  int icode;
  enum machine_mode optab_op2_mode;
  tree def, def_stmt;
  enum vect_def_type dt0, dt1;
  tree new_stmt;
  stmt_vec_info prev_stmt_info;
  int nunits_in = TYPE_VECTOR_SUBPARTS (vectype);
  int nunits_out;
  tree vectype_out;
  int ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits_in;
  int j;

  gcc_assert (ncopies >= 1);

  /* Is STMT a vectorizable binary/unary operation?   */
  if (!STMT_VINFO_RELEVANT_P (stmt_info))
    return false;

  gcc_assert (STMT_VINFO_DEF_TYPE (stmt_info) == vect_loop_def);

  if (STMT_VINFO_LIVE_P (stmt_info))
    {
      /* FORNOW: not yet supported.  */
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "value used after loop.");
      return false;
    }

  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    return false;

  if (TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 0)) != SSA_NAME)
    return false;

  scalar_dest = GIMPLE_STMT_OPERAND (stmt, 0);
  vectype_out = get_vectype_for_scalar_type (TREE_TYPE (scalar_dest));
  nunits_out = TYPE_VECTOR_SUBPARTS (vectype_out);
  if (nunits_out != nunits_in)
    return false;

  operation = GIMPLE_STMT_OPERAND (stmt, 1);
  code = TREE_CODE (operation);
  optab = optab_for_tree_code (code, vectype);

  /* Support only unary or binary operations.  */
  op_type = TREE_CODE_LENGTH (code);
  if (op_type != unary_op && op_type != binary_op)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "num. args = %d (not unary/binary op).", op_type);
      return false;
    }

  op0 = TREE_OPERAND (operation, 0);
  if (!vect_is_simple_use (op0, loop_vinfo, &def_stmt, &def, &dt0))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "use not simple.");
      return false;
    }
                                                                                
  if (op_type == binary_op)
    {
      op1 = TREE_OPERAND (operation, 1);
      if (!vect_is_simple_use (op1, loop_vinfo, &def_stmt, &def, &dt1))
	{
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "use not simple.");
	  return false;
	}
    }

  /* Supportable by target?  */
  if (!optab)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "no optab.");
      return false;
    }
  vec_mode = TYPE_MODE (vectype);
  icode = (int) optab->handlers[(int) vec_mode].insn_code;
  if (icode == CODE_FOR_nothing)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "op not supported by target.");
      if (GET_MODE_SIZE (vec_mode) != UNITS_PER_WORD
          || LOOP_VINFO_VECT_FACTOR (loop_vinfo)
	     < vect_min_worthwhile_factor (code))
        return false;
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "proceeding using word mode.");
    }

  /* Worthwhile without SIMD support?  */
  if (!VECTOR_MODE_P (TYPE_MODE (vectype))
      && LOOP_VINFO_VECT_FACTOR (loop_vinfo)
	 < vect_min_worthwhile_factor (code))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "not worthwhile without SIMD support.");
      return false;
    }

  if (code == LSHIFT_EXPR || code == RSHIFT_EXPR)
    {
      /* FORNOW: not yet supported.  */
      if (!VECTOR_MODE_P (vec_mode))
	return false;

      /* Invariant argument is needed for a vector shift
	 by a scalar shift operand.  */
      optab_op2_mode = insn_data[icode].operand[2].mode;
      if (! (VECTOR_MODE_P (optab_op2_mode)
	     || dt1 == vect_constant_def
	     || dt1 == vect_invariant_def))
	{
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "operand mode requires invariant argument.");
	  return false;
	}
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = op_vec_info_type;
      return true;
    }

  /** Transform.  **/

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "transform binary/unary operation.");

  /* Handle def.  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

  /* In case the vectorization factor (VF) is bigger than the number
     of elements that we can fit in a vectype (nunits), we have to generate
     more than one vector stmt - i.e - we need to "unroll" the
     vector stmt by a factor VF/nunits. In doing so, we record a pointer
     from one copy of the vector stmt to the next, in the field
     STMT_VINFO_RELATED_STMT. This is necessary in order to allow following
     stages to find the correct vector defs to be used when vectorizing
     stmts that use the defs of the current stmt. The example below illustrates
     the vectorization process when VF=16 and nunits=4 (i.e - we need to create
     4 vectorized stmts):
                                                                                
     before vectorization:
                                RELATED_STMT    VEC_STMT
        S1:     x = memref      -               -
        S2:     z = x + 1       -               -
                                                                                
     step 1: vectorize stmt S1 (done in vectorizable_load. See more details
             there):
                                RELATED_STMT    VEC_STMT
        VS1_0:  vx0 = memref0   VS1_1           -
        VS1_1:  vx1 = memref1   VS1_2           -
        VS1_2:  vx2 = memref2   VS1_3           -
        VS1_3:  vx3 = memref3   -               -
        S1:     x = load        -               VS1_0
        S2:     z = x + 1       -               -
                                                                                
     step2: vectorize stmt S2 (done here):
        To vectorize stmt S2 we first need to find the relevant vector
        def for the first operand 'x'. This is, as usual, obtained from
        the vector stmt recorded in the STMT_VINFO_VEC_STMT of the stmt
        that defines 'x' (S1). This way we find the stmt VS1_0, and the
        relevant vector def 'vx0'. Having found 'vx0' we can generate
        the vector stmt VS2_0, and as usual, record it in the
        STMT_VINFO_VEC_STMT of stmt S2.
        When creating the second copy (VS2_1), we obtain the relevant vector
        def from the vector stmt recorded in the STMT_VINFO_RELATED_STMT of
        stmt VS1_0. This way we find the stmt VS1_1 and the relevant
        vector def 'vx1'. Using 'vx1' we create stmt VS2_1 and record a
        pointer to it in the STMT_VINFO_RELATED_STMT of the vector stmt VS2_0.
        Similarly when creating stmts VS2_2 and VS2_3. This is the resulting
        chain of stmts and pointers:
                                RELATED_STMT    VEC_STMT
        VS1_0:  vx0 = memref0   VS1_1           -
        VS1_1:  vx1 = memref1   VS1_2           -
        VS1_2:  vx2 = memref2   VS1_3           -
        VS1_3:  vx3 = memref3   -               -
        S1:     x = load        -               VS1_0
        VS2_0:  vz0 = vx0 + v1  VS2_1           -
        VS2_1:  vz1 = vx1 + v1  VS2_2           -
        VS2_2:  vz2 = vx2 + v1  VS2_3           -
        VS2_3:  vz3 = vx3 + v1  -               -
        S2:     z = x + 1       -               VS2_0  */
                                                                                
  prev_stmt_info = NULL;
  for (j = 0; j < ncopies; j++)
    {
      /* Handle uses.  */
      if (j == 0)
	{
	  vec_oprnd0 = vect_get_vec_def_for_operand (op0, stmt, NULL);
	  if (op_type == binary_op)
	    {
	      if (code == LSHIFT_EXPR || code == RSHIFT_EXPR)
	        {
	          /* Vector shl and shr insn patterns can be defined with
	             scalar operand 2 (shift operand).  In this case, use
	             constant or loop invariant op1 directly, without
	             extending it to vector mode first.  */
	          optab_op2_mode = insn_data[icode].operand[2].mode;
	          if (!VECTOR_MODE_P (optab_op2_mode))
	            {
	              if (vect_print_dump_info (REPORT_DETAILS))
	                fprintf (vect_dump, "operand 1 using scalar mode.");
	              vec_oprnd1 = op1;
	            }
	        }
	      if (!vec_oprnd1)
	        vec_oprnd1 = vect_get_vec_def_for_operand (op1, stmt, NULL);
	    }
	}
      else
	{
	  vec_oprnd0 = vect_get_vec_def_for_stmt_copy (dt0, vec_oprnd0);
	  if (op_type == binary_op)
	    vec_oprnd1 = vect_get_vec_def_for_stmt_copy (dt1, vec_oprnd1);
	}

      /* Arguments are ready. create the new vector stmt.  */
                                                                                
      if (op_type == binary_op)
        new_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, vec_dest,
                    build2 (code, vectype, vec_oprnd0, vec_oprnd1));
      else
        new_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, vec_dest,
                    build1 (code, vectype, vec_oprnd0));
      new_temp = make_ssa_name (vec_dest, new_stmt);
      GIMPLE_STMT_OPERAND (new_stmt, 0) = new_temp;
      vect_finish_stmt_generation (stmt, new_stmt, bsi);
                                                                                
      if (j == 0)
	STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
      prev_stmt_info = vinfo_for_stmt (new_stmt);
    }

  return true;
}


/* Function vectorizable_type_demotion
                                                                                
   Check if STMT performs a binary or unary operation that involves
   type demotion, and if it can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */
                                                                                
bool
vectorizable_type_demotion (tree stmt, block_stmt_iterator *bsi,
                             tree *vec_stmt)
{
  tree vec_dest;
  tree scalar_dest;
  tree operation;
  tree op0;
  tree vec_oprnd0=NULL, vec_oprnd1=NULL;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  enum tree_code code;
  tree new_temp;
  tree def, def_stmt;
  enum vect_def_type dt0;
  tree new_stmt;
  stmt_vec_info prev_stmt_info;
  int nunits_in;
  int nunits_out;
  tree vectype_out;
  int ncopies;
  int j;
  tree expr;
  tree vectype_in;
  tree scalar_type;
  optab optab;
  enum machine_mode vec_mode;
                                                                                
  /* Is STMT a vectorizable type-demotion operation?  */
                                                                                
  if (!STMT_VINFO_RELEVANT_P (stmt_info))
    return false;
                                                                                
  gcc_assert (STMT_VINFO_DEF_TYPE (stmt_info) == vect_loop_def);
                                                                                
  if (STMT_VINFO_LIVE_P (stmt_info))
    {
      /* FORNOW: not yet supported.  */
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "value used after loop.");
      return false;
    }
                                                                                
  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    return false;
                                                                                
  if (TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 0)) != SSA_NAME)
    return false;
                                                                                
  operation = GIMPLE_STMT_OPERAND (stmt, 1);
  code = TREE_CODE (operation);
  if (code != NOP_EXPR && code != CONVERT_EXPR)
    return false;
                                                                                
  op0 = TREE_OPERAND (operation, 0);
  vectype_in = get_vectype_for_scalar_type (TREE_TYPE (op0));
  nunits_in = TYPE_VECTOR_SUBPARTS (vectype_in);
                                                                                
  scalar_dest = GIMPLE_STMT_OPERAND (stmt, 0);
  scalar_type = TREE_TYPE (scalar_dest);
  vectype_out = get_vectype_for_scalar_type (scalar_type);
  nunits_out = TYPE_VECTOR_SUBPARTS (vectype_out);
  if (nunits_in != nunits_out / 2) /* FORNOW */
    return false;
                                                                                
  ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits_out;
  gcc_assert (ncopies >= 1);
                                                                                
  /* Check the operands of the operation.  */
  if (!vect_is_simple_use (op0, loop_vinfo, &def_stmt, &def, &dt0))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "use not simple.");
      return false;
    }
                                                                                
  /* Supportable by target?  */
  code = VEC_PACK_MOD_EXPR;
  optab = optab_for_tree_code (VEC_PACK_MOD_EXPR, vectype_in);
  if (!optab)
    return false;
                                                                                
  vec_mode = TYPE_MODE (vectype_in);
  if (optab->handlers[(int) vec_mode].insn_code == CODE_FOR_nothing)
    return false;
                                                                                
  STMT_VINFO_VECTYPE (stmt_info) = vectype_in;
                                                                                
  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = type_demotion_vec_info_type;
      return true;
    }
                                                                                
  /** Transform.  **/
                                                                                
  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "transform type demotion operation. ncopies = %d.",
                        ncopies);
                                                                                
  /* Handle def.  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype_out);
  
  /* In case the vectorization factor (VF) is bigger than the number
     of elements that we can fit in a vectype (nunits), we have to generate
     more than one vector stmt - i.e - we need to "unroll" the
     vector stmt by a factor VF/nunits.   */
  prev_stmt_info = NULL;
  for (j = 0; j < ncopies; j++)
    {
      /* Handle uses.  */
      if (j == 0)
	{
	  enum vect_def_type dt = vect_unknown_def_type; /* Dummy */
	  vec_oprnd0 = vect_get_vec_def_for_operand (op0, stmt, NULL);
	  vec_oprnd1 = vect_get_vec_def_for_stmt_copy (dt, vec_oprnd0);
	}
      else
	{
	  vec_oprnd0 = vect_get_vec_def_for_stmt_copy (dt0, vec_oprnd1);
	  vec_oprnd1 = vect_get_vec_def_for_stmt_copy (dt0, vec_oprnd0);
	}
                                                                                
      /* Arguments are ready. Create the new vector stmt.  */
      expr = build2 (code, vectype_out, vec_oprnd0, vec_oprnd1);
      new_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, vec_dest, expr);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      GIMPLE_STMT_OPERAND (new_stmt, 0) = new_temp;
      vect_finish_stmt_generation (stmt, new_stmt, bsi);
                                                                                
      if (j == 0)
	STMT_VINFO_VEC_STMT (stmt_info) = new_stmt;
      else
	STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
                                                                                
      prev_stmt_info = vinfo_for_stmt (new_stmt);
    }
                                                                                
  *vec_stmt = STMT_VINFO_VEC_STMT (stmt_info);
  return true;
}


/* Function vect_gen_widened_results_half

   Create a vector stmt whose code, type, number of arguments, and result
   variable are CODE, VECTYPE, OP_TYPE, and VEC_DEST, and its arguments are 
   VEC_OPRND0 and VEC_OPRND1. The new vector stmt is to be inserted at BSI.
   In the case that CODE is a CALL_EXPR, this means that a call to DECL
   needs to be created (DECL is a function-decl of a target-builtin).
   STMT is the original scalar stmt that we are vectorizing.  */

static tree
vect_gen_widened_results_half (enum tree_code code, tree vectype, tree decl,
                               tree vec_oprnd0, tree vec_oprnd1, int op_type,
                               tree vec_dest, block_stmt_iterator *bsi,
			       tree stmt)
{ 
  tree vec_params;
  tree expr; 
  tree new_stmt; 
  tree new_temp; 
  tree sym; 
  ssa_op_iter iter;
 
  /* Generate half of the widened result:  */ 
  if (code == CALL_EXPR) 
    {  
      /* Target specific support  */ 
      vec_params = build_tree_list (NULL_TREE, vec_oprnd0); 
      if (op_type == binary_op) 
        vec_params = tree_cons (NULL_TREE, vec_oprnd1, vec_params); 
      expr = build_function_call_expr (decl, vec_params); 
    } 
  else 
    { 
      /* Generic support */ 
      gcc_assert (op_type == TREE_CODE_LENGTH (code)); 
      if (op_type == binary_op) 
        expr = build2 (code, vectype, vec_oprnd0, vec_oprnd1); 
      else  
        expr = build1 (code, vectype, vec_oprnd0); 
    } 
  new_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, vec_dest, expr);
  new_temp = make_ssa_name (vec_dest, new_stmt); 
  GIMPLE_STMT_OPERAND (new_stmt, 0) = new_temp; 
  vect_finish_stmt_generation (stmt, new_stmt, bsi); 

  if (code == CALL_EXPR)
    {
      FOR_EACH_SSA_TREE_OPERAND (sym, new_stmt, iter, SSA_OP_ALL_VIRTUALS)
        {
          if (TREE_CODE (sym) == SSA_NAME)
            sym = SSA_NAME_VAR (sym);
          mark_sym_for_renaming (sym);
        }
    }

  return new_stmt;
}


/* Function vectorizable_type_promotion

   Check if STMT performs a binary or unary operation that involves
   type promotion, and if it can be vectorized.
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

bool
vectorizable_type_promotion (tree stmt, block_stmt_iterator *bsi,
                             tree *vec_stmt)
{
  tree vec_dest;
  tree scalar_dest;
  tree operation;
  tree op0, op1 = NULL;
  tree vec_oprnd0=NULL, vec_oprnd1=NULL;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  enum tree_code code, code1 = CODE_FOR_nothing, code2 = CODE_FOR_nothing;
  tree decl1 = NULL_TREE, decl2 = NULL_TREE;
  int op_type; 
  tree def, def_stmt;
  enum vect_def_type dt0, dt1;
  tree new_stmt;
  stmt_vec_info prev_stmt_info;
  int nunits_in;
  int nunits_out;
  tree vectype_out;
  int ncopies;
  int j;
  tree vectype_in;
  
  /* Is STMT a vectorizable type-promotion operation?  */

  if (!STMT_VINFO_RELEVANT_P (stmt_info))
    return false;

  gcc_assert (STMT_VINFO_DEF_TYPE (stmt_info) == vect_loop_def);

  if (STMT_VINFO_LIVE_P (stmt_info))
    {
      /* FORNOW: not yet supported.  */
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "value used after loop.");
      return false;
    }

  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    return false;

  if (TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 0)) != SSA_NAME)
    return false;

  operation = GIMPLE_STMT_OPERAND (stmt, 1);
  code = TREE_CODE (operation);
  if (code != NOP_EXPR && code != WIDEN_MULT_EXPR)
    return false;

  op0 = TREE_OPERAND (operation, 0);
  vectype_in = get_vectype_for_scalar_type (TREE_TYPE (op0));
  nunits_in = TYPE_VECTOR_SUBPARTS (vectype_in);
  ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits_in;
  gcc_assert (ncopies >= 1);

  scalar_dest = GIMPLE_STMT_OPERAND (stmt, 0);
  vectype_out = get_vectype_for_scalar_type (TREE_TYPE (scalar_dest));
  nunits_out = TYPE_VECTOR_SUBPARTS (vectype_out);
  if (nunits_out != nunits_in / 2) /* FORNOW */
    return false;

  /* Check the operands of the operation.  */
  if (!vect_is_simple_use (op0, loop_vinfo, &def_stmt, &def, &dt0))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "use not simple.");
      return false;
    }

  op_type = TREE_CODE_LENGTH (code);
  if (op_type == binary_op)
    {
      op1 = TREE_OPERAND (operation, 1);
      if (!vect_is_simple_use (op1, loop_vinfo, &def_stmt, &def, &dt1))
        {
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "use not simple.");
          return false;
        }
    }

  /* Supportable by target?  */
  if (!supportable_widening_operation (code, stmt, vectype_in,
				       &decl1, &decl2, &code1, &code2))
    return false;

  STMT_VINFO_VECTYPE (stmt_info) = vectype_in;

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = type_promotion_vec_info_type;
      return true;
    }

  /** Transform.  **/

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "transform type promotion operation. ncopies = %d.",
                        ncopies);

  /* Handle def.  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype_out);

  /* In case the vectorization factor (VF) is bigger than the number
     of elements that we can fit in a vectype (nunits), we have to generate
     more than one vector stmt - i.e - we need to "unroll" the
     vector stmt by a factor VF/nunits.   */

  prev_stmt_info = NULL;
  for (j = 0; j < ncopies; j++)
    {
      /* Handle uses.  */
      if (j == 0)
        {
	  vec_oprnd0 = vect_get_vec_def_for_operand (op0, stmt, NULL);
	  if (op_type == binary_op)
	    vec_oprnd1 = vect_get_vec_def_for_operand (op1, stmt, NULL);
        }
      else
        {
	  vec_oprnd0 = vect_get_vec_def_for_stmt_copy (dt0, vec_oprnd0);
	  if (op_type == binary_op)
	    vec_oprnd1 = vect_get_vec_def_for_stmt_copy (dt1, vec_oprnd1);
        }

      /* Arguments are ready. Create the new vector stmt.  We are creating 
         two vector defs because the widened result does not fit in one vector.
         The vectorized stmt can be expressed as a call to a taregt builtin,
         or a using a tree-code.  */
      /* Generate first half of the widened result:  */
      new_stmt = vect_gen_widened_results_half (code1, vectype_out, decl1, 
                        vec_oprnd0, vec_oprnd1, op_type, vec_dest, bsi, stmt);
      if (j == 0)
        STMT_VINFO_VEC_STMT (stmt_info) = new_stmt;
      else
        STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
      prev_stmt_info = vinfo_for_stmt (new_stmt);

      /* Generate second half of the widened result:  */
      new_stmt = vect_gen_widened_results_half (code2, vectype_out, decl2,
                        vec_oprnd0, vec_oprnd1, op_type, vec_dest, bsi, stmt);
      STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
      prev_stmt_info = vinfo_for_stmt (new_stmt);

    }

  *vec_stmt = STMT_VINFO_VEC_STMT (stmt_info);
  return true;
}


/* Function vect_strided_store_supported.

   Returns TRUE is INTERLEAVE_HIGH and INTERLEAVE_LOW operations are supported,
   and FALSE otherwise.  */

static bool
vect_strided_store_supported (tree vectype)
{
  optab interleave_high_optab, interleave_low_optab;
  int mode;

  mode = (int) TYPE_MODE (vectype);
      
  /* Check that the operation is supported.  */
  interleave_high_optab = optab_for_tree_code (VEC_INTERLEAVE_HIGH_EXPR, 
					       vectype);
  interleave_low_optab = optab_for_tree_code (VEC_INTERLEAVE_LOW_EXPR, 
					      vectype);
  if (!interleave_high_optab || !interleave_low_optab)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "no optab for interleave.");
      return false;
    }

  if (interleave_high_optab->handlers[(int) mode].insn_code 
      == CODE_FOR_nothing
      || interleave_low_optab->handlers[(int) mode].insn_code 
      == CODE_FOR_nothing)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "interleave op not supported by target.");
      return false;
    }
  return true;
}


/* Function vect_permute_store_chain.

   Given a chain of interleaved stores in DR_CHAIN of LENGTH that must be
   a power of 2, generate interleave_high/low stmts to reorder the data 
   correctly for the stores. Return the final references for stores in
   RESULT_CHAIN.

   E.g., LENGTH is 4 and the scalar type is short, i.e., VF is 8.
   The input is 4 vectors each containing 8 elements. We assign a number to each
   element, the input sequence is:

   1st vec:   0  1  2  3  4  5  6  7
   2nd vec:   8  9 10 11 12 13 14 15
   3rd vec:  16 17 18 19 20 21 22 23 
   4th vec:  24 25 26 27 28 29 30 31

   The output sequence should be:

   1st vec:  0  8 16 24  1  9 17 25
   2nd vec:  2 10 18 26  3 11 19 27
   3rd vec:  4 12 20 28  5 13 21 30
   4th vec:  6 14 22 30  7 15 23 31

   i.e., we interleave the contents of the four vectors in their order.

   We use interleave_high/low instructions to create such output. The input of 
   each interleave_high/low operation is two vectors:
   1st vec    2nd vec 
   0 1 2 3    4 5 6 7 
   the even elements of the result vector are obtained left-to-right from the 
   high/low elements of the first vector. The odd elements of the result are 
   obtained left-to-right from the high/low elements of the second vector.
   The output of interleave_high will be:   0 4 1 5
   and of interleave_low:                   2 6 3 7

   
   The permutation is done in log LENGTH stages. In each stage interleave_high
   and interleave_low stmts are created for each pair of vectors in DR_CHAIN, 
   where the first argument is taken from the first half of DR_CHAIN and the 
   second argument from it's second half. 
   In our example, 

   I1: interleave_high (1st vec, 3rd vec)
   I2: interleave_low (1st vec, 3rd vec)
   I3: interleave_high (2nd vec, 4th vec)
   I4: interleave_low (2nd vec, 4th vec)

   The output for the first stage is:

   I1:  0 16  1 17  2 18  3 19
   I2:  4 20  5 21  6 22  7 23
   I3:  8 24  9 25 10 26 11 27
   I4: 12 28 13 29 14 30 15 31

   The output of the second stage, i.e. the final result is:

   I1:  0  8 16 24  1  9 17 25
   I2:  2 10 18 26  3 11 19 27
   I3:  4 12 20 28  5 13 21 30
   I4:  6 14 22 30  7 15 23 31.  */
 
static bool
vect_permute_store_chain (VEC(tree,heap) *dr_chain, 
			  unsigned int length, 
			  tree stmt, 
			  block_stmt_iterator *bsi,
			  VEC(tree,heap) **result_chain)
{
  tree perm_dest, perm_stmt, vect1, vect2, high, low;
  tree vectype = STMT_VINFO_VECTYPE (vinfo_for_stmt (stmt));
  tree scalar_dest;
  int i;
  unsigned int j;
  VEC(tree,heap) *first, *second;
  
  scalar_dest = TREE_OPERAND (stmt, 0);
  first = VEC_alloc (tree, heap, length/2);
  second = VEC_alloc (tree, heap, length/2);

  /* Check that the operation is supported.  */
  if (!vect_strided_store_supported (vectype))
    return false;

  *result_chain = VEC_copy (tree, heap, dr_chain);

  for (i = 0; i < exact_log2 (length); i++)
    {
      for (j = 0; j < length/2; j++)
	{
	  vect1 = VEC_index (tree, dr_chain, j);
	  vect2 = VEC_index (tree, dr_chain, j+length/2);

	  /* high = interleave_high (vect1, vect2);  */
	  perm_dest = create_tmp_var (vectype, "vect_inter_high");
	  add_referenced_var (perm_dest);
	  perm_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, perm_dest,
			      build2 (VEC_INTERLEAVE_HIGH_EXPR, vectype, vect1, 
				      vect2));
	  high = make_ssa_name (perm_dest, perm_stmt);
	  GIMPLE_STMT_OPERAND (perm_stmt, 0) = high;
	  vect_finish_stmt_generation (stmt, perm_stmt, bsi);
	  VEC_replace (tree, *result_chain, 2*j, high);

	  /* low = interleave_low (vect1, vect2);  */
	  perm_dest = create_tmp_var (vectype, "vect_inter_low");
	  add_referenced_var (perm_dest);
	  perm_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, perm_dest,
			      build2 (VEC_INTERLEAVE_LOW_EXPR, vectype, vect1, 
				      vect2));
	  low = make_ssa_name (perm_dest, perm_stmt);
	  GIMPLE_STMT_OPERAND (perm_stmt, 0) = low;
	  vect_finish_stmt_generation (stmt, perm_stmt, bsi);
	  VEC_replace (tree, *result_chain, 2*j+1, low);
	}
      dr_chain = VEC_copy (tree, heap, *result_chain);
    }
  return true;
}


/* Function vectorizable_store.

   Check if STMT defines a non scalar data-ref (array/pointer/structure) that 
   can be vectorized. 
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized 
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

bool
vectorizable_store (tree stmt, block_stmt_iterator *bsi, tree *vec_stmt)
{
  tree scalar_dest;
  tree data_ref;
  tree op;
  tree vec_oprnd = NULL_TREE;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info), *first_dr = NULL;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  enum machine_mode vec_mode;
  tree dummy;
  enum dr_alignment_support alignment_support_cheme;
  ssa_op_iter iter;
  def_operand_p def_p;
  tree def, def_stmt;
  enum vect_def_type dt;
  stmt_vec_info prev_stmt_info = NULL;
  tree dataref_ptr = NULL_TREE;
  int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits;
  int j;
  tree next_stmt, first_stmt;
  bool strided_store = false;
  unsigned int group_size, i;
  VEC(tree,heap) *dr_chain = NULL, *oprnds = NULL, *result_chain = NULL;
  gcc_assert (ncopies >= 1);

  /* Is vectorizable store? */

  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    return false;

  scalar_dest = GIMPLE_STMT_OPERAND (stmt, 0);
  if (TREE_CODE (scalar_dest) != ARRAY_REF
      && TREE_CODE (scalar_dest) != INDIRECT_REF
      && !DR_GROUP_FIRST_DR (stmt_info))
    return false;

  op = GIMPLE_STMT_OPERAND (stmt, 1);
  if (!vect_is_simple_use (op, loop_vinfo, &def_stmt, &def, &dt))
    {
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "use not simple.");
      return false;
    }

  vec_mode = TYPE_MODE (vectype);
  /* FORNOW. In some cases can vectorize even if data-type not supported
     (e.g. - array initialization with 0).  */
  if (mov_optab->handlers[(int)vec_mode].insn_code == CODE_FOR_nothing)
    return false;

  if (!STMT_VINFO_DATA_REF (stmt_info))
    return false;

  if (DR_GROUP_FIRST_DR (stmt_info))
    {
      strided_store = true;
      if (!vect_strided_store_supported (vectype))
	return false;      
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = store_vec_info_type;
      return true;
    }

  /** Transform.  **/

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "transform store. ncopies = %d",ncopies);

  if (strided_store)
    {
      first_stmt = DR_GROUP_FIRST_DR (stmt_info);
      first_dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (first_stmt));
      group_size = DR_GROUP_SIZE (vinfo_for_stmt (first_stmt));

      DR_GROUP_STORE_COUNT (vinfo_for_stmt (first_stmt))++;

      /* We vectorize all the stmts of the interleaving group when we
	 reach the last stmt in the group.  */
      if (DR_GROUP_STORE_COUNT (vinfo_for_stmt (first_stmt)) 
	  < DR_GROUP_SIZE (vinfo_for_stmt (first_stmt)))
	{
	  *vec_stmt = NULL_TREE;
	  return true;
	}
    }
  else 
    {
      first_stmt = stmt;
      first_dr = dr;
      group_size = 1;
    }
  
  dr_chain = VEC_alloc (tree, heap, group_size);
  oprnds = VEC_alloc (tree, heap, group_size);

  alignment_support_cheme = vect_supportable_dr_alignment (first_dr);
  gcc_assert (alignment_support_cheme);
  gcc_assert (alignment_support_cheme == dr_aligned);  /* FORNOW */

  /* In case the vectorization factor (VF) is bigger than the number
     of elements that we can fit in a vectype (nunits), we have to generate
     more than one vector stmt - i.e - we need to "unroll" the
     vector stmt by a factor VF/nunits.  For more details see documentation in 
     vect_get_vec_def_for_copy_stmt.  */

  /* In case of interleaving (non-unit strided access):

        S1:  &base + 2 = x2
        S2:  &base = x0
        S3:  &base + 1 = x1
        S4:  &base + 3 = x3

     We create vectorized storess starting from base address (the access of the
     first stmt in the chain (S2 in the above example), when the last store stmt
     of the chain (S4) is reached:

        VS1: &base = vx2
	VS2: &base + vec_size*1 = vx0
	VS3: &base + vec_size*2 = vx1
	VS4: &base + vec_size*3 = vx3

     Then permutation statements are generated:

        VS5: vx5 = VEC_INTERLEAVE_HIGH_EXPR < vx0, vx3 >
        VS6: vx6 = VEC_INTERLEAVE_LOW_EXPR < vx0, vx3 >
	...
	
     And they are put in STMT_VINFO_VEC_STMT of the corresponding scalar stmts
     (the order of the data-refs in the output of vect_permute_store_chain
     corresponds to the order of scalar stmts in the interleaving chain - see
     the documentation of vect_permute_store_chain()).

     In case of both multiple types and interleaving, above vector stores and
     permutation stmts are created for every copy. The result vector stmts are
     put in STMT_VINFO_VEC_STMT for the first copy and in the corresponding
     STMT_VINFO_RELATED_STMT for the next copies.     
  */

  prev_stmt_info = NULL;
  for (j = 0; j < ncopies; j++)
    {
      tree new_stmt;
      tree ptr_incr;

      if (j == 0)
	{
	  /* For interleaved stores we collect vectorized defs for all the 
	     stores in the group in DR_CHAIN and OPRNDS. DR_CHAIN is then used
	     as an input to vect_permute_store_chain(), and OPRNDS as an input
	     to vect_get_vec_def_for_stmt_copy() for the next copy.
	     If the store is not strided, GROUP_SIZE is 1, and DR_CHAIN and
	     OPRNDS are of size 1.
	  */
	  next_stmt = first_stmt;	  
	  for (i = 0; i < group_size; i++)
	    {
	      /* Since gaps are not supported for interleaved stores, GROUP_SIZE
		 is the exact number of stmts in the chain. Therefore, NEXT_STMT
		 can't be NULL_TREE.  In case that there is no interleaving, 
		 GROUP_SIZE is 1, and only one iteration of the loop will be 
		 executed.
	      */
	      gcc_assert (next_stmt);
	      op = GIMPLE_STMT_OPERAND (next_stmt, 1);
	      vec_oprnd = vect_get_vec_def_for_operand (op, next_stmt, NULL);
	      VEC_quick_push(tree, dr_chain, vec_oprnd); 
	      VEC_quick_push(tree, oprnds, vec_oprnd); 
	      next_stmt = DR_GROUP_NEXT_DR (vinfo_for_stmt (next_stmt));
	    }
	  dataref_ptr = vect_create_data_ref_ptr (first_stmt, bsi, NULL_TREE, 
						  &dummy, &ptr_incr, false,
						  TREE_TYPE (vec_oprnd));
	}
      else 
	{
	  /* For interleaved stores we created vectorized defs for all the 
	     defs stored in OPRNDS in the previous iteration (previous copy). 
	     DR_CHAIN is then used as an input to vect_permute_store_chain(), 
	     and OPRNDS as an input to vect_get_vec_def_for_stmt_copy() for the 
	     next copy.
	     If the store is not strided, GROUP_SIZE is 1, and DR_CHAIN and
	     OPRNDS are of size 1.
	  */
	  for (i = 0; i < group_size; i++)
	    {
	      vec_oprnd = vect_get_vec_def_for_stmt_copy (dt, 
						   VEC_index (tree, oprnds, i));
	      VEC_replace(tree, dr_chain, i, vec_oprnd);
	      VEC_replace(tree, oprnds, i, vec_oprnd);
	    }
	  dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, bsi, stmt);
	}

      if (strided_store)
	{
	  result_chain = VEC_alloc (tree, heap, group_size);     
	  /* Permute.  */
	  if (!vect_permute_store_chain (dr_chain, group_size, stmt, bsi, 
					 &result_chain))
	    return false;
	}

      next_stmt = first_stmt;
      for (i = 0; i < group_size; i++)
	{
	  /* For strided stores vectorized defs are interleaved in 
	     vect_permute_store_chain().  */
	  if (strided_store)
	    vec_oprnd = VEC_index(tree, result_chain, i);

	  data_ref = build_fold_indirect_ref (dataref_ptr);
	  /* Arguments are ready. Create the new vector stmt.  */
	  new_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, data_ref, 
			     vec_oprnd);
	  vect_finish_stmt_generation (stmt, new_stmt, bsi);

	  /* Set the V_MAY_DEFS for the vector pointer. If this virtual def has a 
	     use outside the loop and a loop peel is performed then the def may be 
	     renamed by the peel.  Mark it for renaming so the later use will also 
	     be renamed.  */
	  copy_virtual_operands (new_stmt, next_stmt);
	  if (j == 0)
	    {
	      /* The original store is deleted so the same SSA_NAMEs can be used.  
	       */
	      FOR_EACH_SSA_TREE_OPERAND (def, next_stmt, iter, SSA_OP_VMAYDEF)
		{
		  SSA_NAME_DEF_STMT (def) = new_stmt;
		  mark_sym_for_renaming (SSA_NAME_VAR (def));
		}
	      
	      STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt =  new_stmt;
	    }
	  else
	    {
	      /* Create new names for all the definitions created by COPY and
		 add replacement mappings for each new name.  */
	      FOR_EACH_SSA_DEF_OPERAND (def_p, new_stmt, iter, SSA_OP_VMAYDEF)
		{
		  create_new_def_for (DEF_FROM_PTR (def_p), new_stmt, def_p);
		  mark_sym_for_renaming (SSA_NAME_VAR (DEF_FROM_PTR (def_p)));
		}
	      
	      STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
	    }

	  prev_stmt_info = vinfo_for_stmt (new_stmt);
	  	  next_stmt = DR_GROUP_NEXT_DR (vinfo_for_stmt (next_stmt));
	  if (!next_stmt)
	    break;
	  /* Bump the vector pointer.  */
	  dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, bsi, stmt);
	}
    }

  return true;
}


/* Function vect_setup_realignment
  
   This function is called when vectorizing an unaligned load using
   the dr_unaligned_software_pipeline scheme.
   This function generates the following code at the loop prolog:

      p = initial_addr;
      msq_init = *(floor(p));   # prolog load
      realignment_token = call target_builtin; 
    loop:
      msq = phi (msq_init, ---)

   The code above sets up a new (vector) pointer, pointing to the first 
   location accessed by STMT, and a "floor-aligned" load using that pointer.
   It also generates code to compute the "realignment-token" (if the relevant
   target hook was defined), and creates a phi-node at the loop-header bb
   whose arguments are the result of the prolog-load (created by this
   function) and the result of a load that takes place in the loop (to be
   created by the caller to this function).
   The caller to this function uses the phi-result (msq) to create the 
   realignment code inside the loop, and sets up the missing phi argument,
   as follows:

    loop: 
      msq = phi (msq_init, lsq)
      lsq = *(floor(p'));        # load in loop
      result = realign_load (msq, lsq, realignment_token);

   Input:
   STMT - (scalar) load stmt to be vectorized. This load accesses
          a memory location that may be unaligned.
   BSI - place where new code is to be inserted.
   
   Output:
   REALIGNMENT_TOKEN - the result of a call to the builtin_mask_for_load
                       target hook, if defined.
   Return value - the result of the loop-header phi node.  */

static tree
vect_setup_realignment (tree stmt, block_stmt_iterator *bsi,
                        tree *realignment_token)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  edge pe = loop_preheader_edge (loop);
  tree scalar_dest = TREE_OPERAND (stmt, 0);
  tree vec_dest;
  tree init_addr;
  tree inc;
  tree ptr;
  tree data_ref;
  tree new_stmt;
  basic_block new_bb;
  tree msq_init;
  tree new_temp;
  tree phi_stmt;
  tree msq;

  /* 1. Create msq_init = *(floor(p1)) in the loop preheader  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype);
  ptr = vect_create_data_ref_ptr (stmt, bsi, NULL_TREE, &init_addr, &inc, true,
				  NULL_TREE);
  data_ref = build1 (ALIGN_INDIRECT_REF, vectype, ptr);
  new_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, vec_dest, data_ref);
  new_temp = make_ssa_name (vec_dest, new_stmt);
  GIMPLE_STMT_OPERAND (new_stmt, 0) = new_temp;
  new_bb = bsi_insert_on_edge_immediate (pe, new_stmt);
  gcc_assert (!new_bb);
  msq_init = GIMPLE_STMT_OPERAND (new_stmt, 0);
  copy_virtual_operands (new_stmt, stmt);
  update_vuses_to_preheader (new_stmt, loop);

  /* 2. Create permutation mask, if required, in loop preheader.  */
  if (targetm.vectorize.builtin_mask_for_load)
    {
      tree builtin_decl;
      tree params = build_tree_list (NULL_TREE, init_addr);

      builtin_decl = targetm.vectorize.builtin_mask_for_load ();
      new_stmt = build_function_call_expr (builtin_decl, params);
      vec_dest = vect_create_destination_var (scalar_dest, 
					      TREE_TYPE (new_stmt));
      new_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, vec_dest,
	  		 new_stmt);
      new_temp = make_ssa_name (vec_dest, new_stmt);
      GIMPLE_STMT_OPERAND (new_stmt, 0) = new_temp;
      new_bb = bsi_insert_on_edge_immediate (pe, new_stmt);
      gcc_assert (!new_bb);
      *realignment_token = GIMPLE_STMT_OPERAND (new_stmt, 0);

      /* The result of the CALL_EXPR to this builtin is determined from
         the value of the parameter and no global variables are touched
         which makes the builtin a "const" function.  Requiring the
         builtin to have the "const" attribute makes it unnecessary
         to call mark_call_clobbered.  */
      gcc_assert (TREE_READONLY (builtin_decl));
    }

  /* 3. Create msq = phi <msq_init, lsq> in loop  */
  vec_dest = vect_create_destination_var (scalar_dest, vectype);
  msq = make_ssa_name (vec_dest, NULL_TREE);
  phi_stmt = create_phi_node (msq, loop->header); 
  SSA_NAME_DEF_STMT (msq) = phi_stmt;
  add_phi_arg (phi_stmt, msq_init, loop_preheader_edge (loop));

  return msq;
}


/* Function vect_strided_load_supported.

   Returns TRUE is EXTRACT_EVEN and EXTRACT_ODD operations are supported,
   and FALSE otherwise.  */

static bool
vect_strided_load_supported (tree vectype)
{
  optab perm_even_optab, perm_odd_optab;
  int mode;

  mode = (int) TYPE_MODE (vectype);

  perm_even_optab = optab_for_tree_code (VEC_EXTRACT_EVEN_EXPR, vectype);
  if (!perm_even_optab)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "no optab for perm_even.");
      return false;
    }

  if (perm_even_optab->handlers[mode].insn_code == CODE_FOR_nothing)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "perm_even op not supported by target.");
      return false;
    }

  perm_odd_optab = optab_for_tree_code (VEC_EXTRACT_ODD_EXPR, vectype);
  if (!perm_odd_optab)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "no optab for perm_odd.");
      return false;
    }

  if (perm_odd_optab->handlers[mode].insn_code == CODE_FOR_nothing)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "perm_odd op not supported by target.");
      return false;
    }
  return true;
}


/* Function vect_permute_load_chain.

   Given a chain of interleaved loads in DR_CHAIN of LENGTH that must be
   a power of 2, generate extract_even/odd stmts to reorder the input data 
   correctly. Return the final references for loads in RESULT_CHAIN.

   E.g., LENGTH is 4 and the scalar type is short, i.e., VF is 8.
   The input is 4 vectors each containing 8 elements. We assign a number to each
   element, the input sequence is:

   1st vec:   0  1  2  3  4  5  6  7
   2nd vec:   8  9 10 11 12 13 14 15
   3rd vec:  16 17 18 19 20 21 22 23 
   4th vec:  24 25 26 27 28 29 30 31

   The output sequence should be:

   1st vec:  0 4  8 12 16 20 24 28
   2nd vec:  1 5  9 13 17 21 25 29
   3rd vec:  2 6 10 14 18 22 26 30 
   4th vec:  3 7 11 15 19 23 27 31

   i.e., the first output vector should contain the first elements of each
   interleaving group, etc.

   We use extract_even/odd instructions to create such output. The input of each
   extract_even/odd operation is two vectors
   1st vec    2nd vec 
   0 1 2 3    4 5 6 7 

   and the output is the vector of extracted even/odd elements. The output of 
   extract_even will be:   0 2 4 6
   and of extract_odd:     1 3 5 7

   
   The permutation is done in log LENGTH stages. In each stage extract_even and
   extract_odd stmts are created for each pair of vectors in DR_CHAIN in their 
   order. In our example, 

   E1: extract_even (1st vec, 2nd vec)
   E2: extract_odd (1st vec, 2nd vec)
   E3: extract_even (3rd vec, 4th vec)
   E4: extract_odd (3rd vec, 4th vec)

   The output for the first stage will be:

   E1:  0  2  4  6  8 10 12 14
   E2:  1  3  5  7  9 11 13 15
   E3: 16 18 20 22 24 26 28 30 
   E4: 17 19 21 23 25 27 29 31

   In order to proceed and create the correct sequence for the next stage (or
   for the correct output, if the second stage is the last one, as in our 
   example), we first put the output of extract_even operation and then the 
   output of extract_odd in RESULT_CHAIN (which is then copied to DR_CHAIN).
   The input for the second stage is:

   1st vec (E1):  0  2  4  6  8 10 12 14
   2nd vec (E3): 16 18 20 22 24 26 28 30  
   3rd vec (E2):  1  3  5  7  9 11 13 15    
   4th vec (E4): 17 19 21 23 25 27 29 31

   The output of the second stage:

   E1: 0 4  8 12 16 20 24 28
   E2: 2 6 10 14 18 22 26 30
   E3: 1 5  9 13 17 21 25 29
   E4: 3 7 11 15 19 23 27 31

   And RESULT_CHAIN after reordering:

   1st vec (E1):  0 4  8 12 16 20 24 28
   2nd vec (E3):  1 5  9 13 17 21 25 29
   3rd vec (E2):  2 6 10 14 18 22 26 30 
   4th vec (E4):  3 7 11 15 19 23 27 31.  */

static bool
vect_permute_load_chain (VEC(tree,heap) *dr_chain, 
			 unsigned int length, 
			 tree stmt, 
			 block_stmt_iterator *bsi,
			 VEC(tree,heap) **result_chain)
{
  tree perm_dest, perm_stmt, data_ref, first_vect, second_vect;
  tree vectype = STMT_VINFO_VECTYPE (vinfo_for_stmt (stmt));
  int i;
  unsigned int j;

  /* Check that the operation is supported.  */
  if (!vect_strided_load_supported (vectype))
    return false;

  *result_chain = VEC_copy (tree, heap, dr_chain);
  for (i = 0; i < exact_log2 (length); i++)
    {
      for (j = 0; j < length; j +=2)
	{
	  first_vect = VEC_index (tree, dr_chain, j);
	  second_vect = VEC_index (tree, dr_chain, j+1);

	  /* data_ref = permute_even (first_data_ref, second_data_ref);  */
	  perm_dest = create_tmp_var (vectype, "vect_perm_even");
	  add_referenced_var (perm_dest);
	 
	  perm_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, perm_dest,
			      build2 (VEC_EXTRACT_EVEN_EXPR, vectype, 
				      first_vect, second_vect));

	  data_ref = make_ssa_name (perm_dest, perm_stmt);
	  GIMPLE_STMT_OPERAND (perm_stmt, 0) = data_ref;
	  vect_finish_stmt_generation (stmt, perm_stmt, bsi);
	  mark_new_vars_to_rename (perm_stmt);

	  VEC_replace (tree, *result_chain, j/2, data_ref);	      
	      
	  /* data_ref = permute_odd (first_data_ref, second_data_ref);  */
	  perm_dest = create_tmp_var (vectype, "vect_perm_odd");
	  add_referenced_var (perm_dest);

	  perm_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, perm_dest,
			      build2 (VEC_EXTRACT_ODD_EXPR, vectype, 
				      first_vect, second_vect));
	  data_ref = make_ssa_name (perm_dest, perm_stmt);
	  GIMPLE_STMT_OPERAND (perm_stmt, 0) = data_ref;
	  vect_finish_stmt_generation (stmt, perm_stmt, bsi);
	  mark_new_vars_to_rename (perm_stmt);

	  VEC_replace (tree, *result_chain, j/2+length/2, data_ref);
	}
      dr_chain = VEC_copy (tree, heap, *result_chain);
    }
  return true;
}


/* Function vect_transform_strided_load.

   Given a chain of input interleaved data-refs (in DR_CHAIN), build statements
   to perform their permutation and ascribe the result vectorized statements to
   the scalar statements.
*/

static bool
vect_transform_strided_load (tree stmt, VEC(tree,heap) *dr_chain, int size,
			     block_stmt_iterator *bsi)
{
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree first_stmt = DR_GROUP_FIRST_DR (stmt_info);
  tree next_stmt, new_stmt;
  VEC(tree,heap) *result_chain = NULL;
  unsigned int i, gap_count;
  tree tmp_data_ref;

  /* DR_CHAIN contains input data-refs that are a part of the interleaving. 
     RESULT_CHAIN is the output of vect_permute_load_chain, it contains permuted 
     vectors, that are ready for vector computation.  */
  result_chain = VEC_alloc (tree, heap, size);
  /* Permute.  */
  if (!vect_permute_load_chain (dr_chain, size, stmt, bsi, &result_chain))
    return false;

  /* Put a permuted data-ref in the VECTORIZED_STMT field.  
     Since we scan the chain starting from it's first node, their order 
     corresponds the order of data-refs in RESULT_CHAIN.  */
  next_stmt = first_stmt;
  gap_count = 1;
  for (i = 0; VEC_iterate(tree, result_chain, i, tmp_data_ref); i++)
    {
      if (!next_stmt)
	break;

      /* Skip the gaps. Loads created for the gaps will be removed by dead
       code elimination pass later.
       DR_GROUP_GAP is the number of steps in elements from the previous
       access (if there is no gap DR_GROUP_GAP is 1). We skip loads that
       correspond to the gaps.
      */
      if (gap_count < DR_GROUP_GAP (vinfo_for_stmt (next_stmt)))
      {
        gap_count++;
        continue;
      }

      while (next_stmt)
        {
	  new_stmt = SSA_NAME_DEF_STMT (tmp_data_ref);
	  /* We assume that if VEC_STMT is not NULL, this is a case of multiple
	     copies, and we put the new vector statement in the first available
	     RELATED_STMT.  */
	  if (!STMT_VINFO_VEC_STMT (vinfo_for_stmt (next_stmt)))
	    STMT_VINFO_VEC_STMT (vinfo_for_stmt (next_stmt)) = new_stmt;
	  else
            {
	      tree prev_stmt = STMT_VINFO_VEC_STMT (vinfo_for_stmt (next_stmt));
	      tree rel_stmt = STMT_VINFO_RELATED_STMT (
						       vinfo_for_stmt (prev_stmt));
	      while (rel_stmt)
		{
		  prev_stmt = rel_stmt;
		  rel_stmt = STMT_VINFO_RELATED_STMT (vinfo_for_stmt (rel_stmt));
		}
	      STMT_VINFO_RELATED_STMT (vinfo_for_stmt (prev_stmt)) = new_stmt;
            }
	  next_stmt = DR_GROUP_NEXT_DR (vinfo_for_stmt (next_stmt));
	  gap_count = 1;
	  /* If NEXT_STMT accesses the same DR as the previous statement,
	     put the same TMP_DATA_REF as its vectorized statement; otherwise
	     get the next data-ref from RESULT_CHAIN.  */
	  if (!next_stmt || !DR_GROUP_SAME_DR_STMT (vinfo_for_stmt (next_stmt)))
	    break;
        }
    }
  return true;
}


/* vectorizable_load.

   Check if STMT reads a non scalar data-ref (array/pointer/structure) that 
   can be vectorized. 
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized 
   stmt to replace it, put it in VEC_STMT, and insert it at BSI.
   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

bool
vectorizable_load (tree stmt, block_stmt_iterator *bsi, tree *vec_stmt)
{
  tree scalar_dest;
  tree vec_dest = NULL;
  tree data_ref = NULL;
  tree op;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  stmt_vec_info prev_stmt_info; 
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  struct data_reference *dr = STMT_VINFO_DATA_REF (stmt_info), *first_dr;
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree new_temp;
  int mode;
  tree new_stmt = NULL_TREE;
  tree dummy;
  enum dr_alignment_support alignment_support_cheme;
  tree dataref_ptr = NULL_TREE;
  tree ptr_incr;
  int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits;
  int i, j, group_size;
  tree msq = NULL_TREE, lsq;
  tree offset = NULL_TREE;
  tree realignment_token = NULL_TREE;
  tree phi_stmt = NULL_TREE;
  VEC(tree,heap) *dr_chain = NULL;
  bool strided_load = false;
  tree first_stmt;

  /* Is vectorizable load? */
  if (!STMT_VINFO_RELEVANT_P (stmt_info))
    return false;

  gcc_assert (STMT_VINFO_DEF_TYPE (stmt_info) == vect_loop_def);

  if (STMT_VINFO_LIVE_P (stmt_info))
    {
      /* FORNOW: not yet supported.  */
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "value used after loop.");
      return false;
    }

  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    return false;

  scalar_dest = GIMPLE_STMT_OPERAND (stmt, 0);
  if (TREE_CODE (scalar_dest) != SSA_NAME)
    return false;

  op = GIMPLE_STMT_OPERAND (stmt, 1);
  if (TREE_CODE (op) != ARRAY_REF 
      && TREE_CODE (op) != INDIRECT_REF
      && !DR_GROUP_FIRST_DR (stmt_info))
    return false;

  if (!STMT_VINFO_DATA_REF (stmt_info))
    return false;

  mode = (int) TYPE_MODE (vectype);

  /* FORNOW. In some cases can vectorize even if data-type not supported
    (e.g. - data copies).  */
  if (mov_optab->handlers[mode].insn_code == CODE_FOR_nothing)
    {
      if (vect_print_dump_info (REPORT_DETAILS))
	fprintf (vect_dump, "Aligned load, but unsupported type.");
      return false;
    }

  /* Check if the load is a part of an interleaving chain.  */
  if (DR_GROUP_FIRST_DR (stmt_info))
    {
      strided_load = true;

      /* Check if interleaving is supported.  */
      if (!vect_strided_load_supported (vectype))
	return false;
    }

  if (!vec_stmt) /* transformation not required.  */
    {
      STMT_VINFO_TYPE (stmt_info) = load_vec_info_type;
      return true;
    }

  /** Transform.  **/

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "transform load.");

  if (strided_load)
    {
      first_stmt = DR_GROUP_FIRST_DR (stmt_info);
      /* Check if the chain of loads is already vectorized.  */
      if (STMT_VINFO_VEC_STMT (vinfo_for_stmt (first_stmt)))
	{
	  *vec_stmt = STMT_VINFO_VEC_STMT (stmt_info);
	  return true;
	}
      first_dr = STMT_VINFO_DATA_REF (vinfo_for_stmt (first_stmt));
      group_size = DR_GROUP_SIZE (vinfo_for_stmt (first_stmt));
      dr_chain = VEC_alloc (tree, heap, group_size);
    }
  else
    {
      first_stmt = stmt;
      first_dr = dr;
      group_size = 1;
    }

  alignment_support_cheme = vect_supportable_dr_alignment (first_dr);
  gcc_assert (alignment_support_cheme);


  /* In case the vectorization factor (VF) is bigger than the number
     of elements that we can fit in a vectype (nunits), we have to generate
     more than one vector stmt - i.e - we need to "unroll" the
     vector stmt by a factor VF/nunits. In doing so, we record a pointer
     from one copy of the vector stmt to the next, in the field
     STMT_VINFO_RELATED_STMT. This is necessary in order to allow following
     stages to find the correct vector defs to be used when vectorizing
     stmts that use the defs of the current stmt. The example below illustrates
     the vectorization process when VF=16 and nunits=4 (i.e - we need to create
     4 vectorized stmts):

     before vectorization:
                                RELATED_STMT    VEC_STMT
        S1:     x = memref      -               -
        S2:     z = x + 1       -               -

     step 1: vectorize stmt S1:
        We first create the vector stmt VS1_0, and, as usual, record a
        pointer to it in the STMT_VINFO_VEC_STMT of the scalar stmt S1.
        Next, we create the vector stmt VS1_1, and record a pointer to
        it in the STMT_VINFO_RELATED_STMT of the vector stmt VS1_0.
        Similarly, for VS1_2 and VS1_3. This is the resulting chain of
        stmts and pointers:
                                RELATED_STMT    VEC_STMT
        VS1_0:  vx0 = memref0   VS1_1           -
        VS1_1:  vx1 = memref1   VS1_2           -
        VS1_2:  vx2 = memref2   VS1_3           -
        VS1_3:  vx3 = memref3   -               -
        S1:     x = load        -               VS1_0
        S2:     z = x + 1       -               -

     See in documentation in vect_get_vec_def_for_stmt_copy for how the 
     information we recorded in RELATED_STMT field is used to vectorize 
     stmt S2.  */

  /* In case of interleaving (non-unit strided access):

     S1:  x2 = &base + 2
     S2:  x0 = &base
     S3:  x1 = &base + 1
     S4:  x3 = &base + 3

     Vectorized loads are created in the order of memory accesses 
     starting from the access of the first stmt of the chain:

     VS1: vx0 = &base
     VS2: vx1 = &base + vec_size*1
     VS3: vx3 = &base + vec_size*2
     VS4: vx4 = &base + vec_size*3

     Then permutation statements are generated:

     VS5: vx5 = VEC_EXTRACT_EVEN_EXPR < vx0, vx1 >
     VS6: vx6 = VEC_EXTRACT_ODD_EXPR < vx0, vx1 >
       ...

     And they are put in STMT_VINFO_VEC_STMT of the corresponding scalar stmts
     (the order of the data-refs in the output of vect_permute_load_chain
     corresponds to the order of scalar stmts in the interleaving chain - see
     the documentation of vect_permute_load_chain()).
     The generation of permutation stmts and recording them in
     STMT_VINFO_VEC_STMT is done in vect_transform_strided_load().

     In case of both multiple types and interleaving, the vector loads and 
     permutation stmts above are created for every copy. The result vector stmts
     are put in STMT_VINFO_VEC_STMT for the first copy and in the corresponding
     STMT_VINFO_RELATED_STMT for the next copies.  */

  /* If the data reference is aligned (dr_aligned) or potentially unaligned
     on a target that supports unaligned accesses (dr_unaligned_supported)
     we generate the following code:
         p = initial_addr;
         indx = 0;
         loop {
	   p = p + indx * vectype_size;
           vec_dest = *(p);
           indx = indx + 1;
         }

     Otherwise, the data reference is potentially unaligned on a target that
     does not support unaligned accesses (dr_unaligned_software_pipeline) - 
     then generate the following code, in which the data in each iteration is
     obtained by two vector loads, one from the previous iteration, and one
     from the current iteration:
         p1 = initial_addr;
         msq_init = *(floor(p1))
         p2 = initial_addr + VS - 1;
         realignment_token = call target_builtin;
         indx = 0;
         loop {
           p2 = p2 + indx * vectype_size
           lsq = *(floor(p2))
           vec_dest = realign_load (msq, lsq, realignment_token)
           indx = indx + 1;
           msq = lsq;
         }   */

  if (alignment_support_cheme == dr_unaligned_software_pipeline)
    {
      msq = vect_setup_realignment (first_stmt, bsi, &realignment_token);
      phi_stmt = SSA_NAME_DEF_STMT (msq);
      offset = size_int (TYPE_VECTOR_SUBPARTS (vectype) - 1);
    }

  prev_stmt_info = NULL;
  for (j = 0; j < ncopies; j++)
    { 
      /* 1. Create the vector pointer update chain.  */
      if (j == 0)
        dataref_ptr = vect_create_data_ref_ptr (first_stmt, bsi, offset, &dummy,
                                                &ptr_incr, false, NULL_TREE);
      else
        dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, bsi, stmt);

      for (i = 0; i < group_size; i++)
	{
	  /* 2. Create the vector-load in the loop.  */
	  switch (alignment_support_cheme)
	    {
	    case dr_aligned:
	      gcc_assert (aligned_access_p (first_dr));
	      data_ref = build_fold_indirect_ref (dataref_ptr);
	      break;
	    case dr_unaligned_supported:
	      {
		int mis = DR_MISALIGNMENT (first_dr);
		tree tmis = (mis == -1 ? size_zero_node : size_int (mis));

		gcc_assert (!aligned_access_p (first_dr));
		tmis = size_binop (MULT_EXPR, tmis, size_int(BITS_PER_UNIT));
		data_ref =
		  build2 (MISALIGNED_INDIRECT_REF, vectype, dataref_ptr, tmis);
		break;
	      }
	    case dr_unaligned_software_pipeline:
	      gcc_assert (!aligned_access_p (first_dr));
	      data_ref = build1 (ALIGN_INDIRECT_REF, vectype, dataref_ptr);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  vec_dest = vect_create_destination_var (scalar_dest, vectype);
	  new_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, vec_dest,
	      		     data_ref);
	  new_temp = make_ssa_name (vec_dest, new_stmt);
	  GIMPLE_STMT_OPERAND (new_stmt, 0) = new_temp;
	  vect_finish_stmt_generation (stmt, new_stmt, bsi);
	  copy_virtual_operands (new_stmt, stmt);
	  mark_new_vars_to_rename (new_stmt);

	  /* 3. Handle explicit realignment if necessary/supported.  */
	  if (alignment_support_cheme == dr_unaligned_software_pipeline)
	    {
	      /* Create in loop: 
		 <vec_dest = realign_load (msq, lsq, realignment_token)>  */
	      lsq = GIMPLE_STMT_OPERAND (new_stmt, 0);
	      if (!realignment_token)
		realignment_token = dataref_ptr;
	      vec_dest = vect_create_destination_var (scalar_dest, vectype);
	      new_stmt =
		build3 (REALIGN_LOAD_EXPR, vectype, msq, lsq, realignment_token);
	      new_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, vec_dest,
		  		 new_stmt);
	      new_temp = make_ssa_name (vec_dest, new_stmt);
	      GIMPLE_STMT_OPERAND (new_stmt, 0) = new_temp;
	      vect_finish_stmt_generation (stmt, new_stmt, bsi);
	      if (i == group_size - 1 && j == ncopies - 1)
		add_phi_arg (phi_stmt, lsq, loop_latch_edge (loop));
	      msq = lsq;
	    }
	  if (strided_load)
	    VEC_quick_push (tree, dr_chain, new_temp);
	  if (i < group_size - 1)
	    dataref_ptr = bump_vector_ptr (dataref_ptr, ptr_incr, bsi, stmt);	  
	}

      if (strided_load)
	{
	  if (!vect_transform_strided_load (stmt, dr_chain, group_size, bsi))
	    return false;	  
	  *vec_stmt = STMT_VINFO_VEC_STMT (stmt_info);
	  dr_chain = VEC_alloc (tree, heap, group_size);
	}
      else
	{
	  if (j == 0)
	    STMT_VINFO_VEC_STMT (stmt_info) = *vec_stmt = new_stmt;
	  else
	    STMT_VINFO_RELATED_STMT (prev_stmt_info) = new_stmt;
	  prev_stmt_info = vinfo_for_stmt (new_stmt);
	}
    }

  return true;
}


/* Function vectorizable_live_operation.

   STMT computes a value that is used outside the loop. Check if 
   it can be supported.  */

bool
vectorizable_live_operation (tree stmt,
                             block_stmt_iterator *bsi ATTRIBUTE_UNUSED,
                             tree *vec_stmt ATTRIBUTE_UNUSED)
{
  tree operation;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  int i;
  enum tree_code code;
  int op_type;
  tree op;
  tree def, def_stmt;
  enum vect_def_type dt; 

  if (!STMT_VINFO_LIVE_P (stmt_info))
    return false;

  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    return false;

  if (TREE_CODE (GIMPLE_STMT_OPERAND (stmt, 0)) != SSA_NAME)
    return false;

  operation = GIMPLE_STMT_OPERAND (stmt, 1);
  code = TREE_CODE (operation);

  op_type = TREE_CODE_LENGTH (code);

  /* FORNOW: support only if all uses are invariant. This means
     that the scalar operations can remain in place, unvectorized.
     The original last scalar value that they compute will be used.  */

  for (i = 0; i < op_type; i++)
    {
      op = TREE_OPERAND (operation, i);
      if (!vect_is_simple_use (op, loop_vinfo, &def_stmt, &def, &dt))
        {
          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "use not simple.");
          return false;
        }

      if (dt != vect_invariant_def && dt != vect_constant_def)
        return false;
    }

  /* No transformation is required for the cases we currently support.  */
  return true;
}


/* Function vect_is_simple_cond.
  
   Input:
   LOOP - the loop that is being vectorized.
   COND - Condition that is checked for simple use.

   Returns whether a COND can be vectorized.  Checks whether
   condition operands are supportable using vec_is_simple_use.  */

static bool
vect_is_simple_cond (tree cond, loop_vec_info loop_vinfo)
{
  tree lhs, rhs;
  tree def;
  enum vect_def_type dt;

  if (!COMPARISON_CLASS_P (cond))
    return false;

  lhs = TREE_OPERAND (cond, 0);
  rhs = TREE_OPERAND (cond, 1);

  if (TREE_CODE (lhs) == SSA_NAME)
    {
      tree lhs_def_stmt = SSA_NAME_DEF_STMT (lhs);
      if (!vect_is_simple_use (lhs, loop_vinfo, &lhs_def_stmt, &def, &dt))
	return false;
    }
  else if (TREE_CODE (lhs) != INTEGER_CST && TREE_CODE (lhs) != REAL_CST)
    return false;

  if (TREE_CODE (rhs) == SSA_NAME)
    {
      tree rhs_def_stmt = SSA_NAME_DEF_STMT (rhs);
      if (!vect_is_simple_use (rhs, loop_vinfo, &rhs_def_stmt, &def, &dt))
	return false;
    }
  else if (TREE_CODE (rhs) != INTEGER_CST  && TREE_CODE (rhs) != REAL_CST)
    return false;

  return true;
}

/* vectorizable_condition.

   Check if STMT is conditional modify expression that can be vectorized. 
   If VEC_STMT is also passed, vectorize the STMT: create a vectorized 
   stmt using VEC_COND_EXPR  to replace it, put it in VEC_STMT, and insert it 
   at BSI.

   Return FALSE if not a vectorizable STMT, TRUE otherwise.  */

bool
vectorizable_condition (tree stmt, block_stmt_iterator *bsi, tree *vec_stmt)
{
  tree scalar_dest = NULL_TREE;
  tree vec_dest = NULL_TREE;
  tree op = NULL_TREE;
  tree cond_expr, then_clause, else_clause;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  tree vec_cond_lhs, vec_cond_rhs, vec_then_clause, vec_else_clause;
  tree vec_compare, vec_cond_expr;
  tree new_temp;
  loop_vec_info loop_vinfo = STMT_VINFO_LOOP_VINFO (stmt_info);
  enum machine_mode vec_mode;
  tree def;
  enum vect_def_type dt;
  int nunits = TYPE_VECTOR_SUBPARTS (vectype);
  int ncopies = LOOP_VINFO_VECT_FACTOR (loop_vinfo) / nunits;

  gcc_assert (ncopies >= 1);
  if (ncopies > 1)
    return false; /* FORNOW */

  if (!STMT_VINFO_RELEVANT_P (stmt_info))
    return false;

  gcc_assert (STMT_VINFO_DEF_TYPE (stmt_info) == vect_loop_def);

  if (STMT_VINFO_LIVE_P (stmt_info))
    {
      /* FORNOW: not yet supported.  */
      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "value used after loop.");
      return false;
    }

  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    return false;

  op = GIMPLE_STMT_OPERAND (stmt, 1);

  if (TREE_CODE (op) != COND_EXPR)
    return false;

  cond_expr = TREE_OPERAND (op, 0);
  then_clause = TREE_OPERAND (op, 1);
  else_clause = TREE_OPERAND (op, 2);

  if (!vect_is_simple_cond (cond_expr, loop_vinfo))
    return false;

  /* We do not handle two different vector types for the condition
     and the values.  */
  if (TREE_TYPE (TREE_OPERAND (cond_expr, 0)) != TREE_TYPE (vectype))
    return false;

  if (TREE_CODE (then_clause) == SSA_NAME)
    {
      tree then_def_stmt = SSA_NAME_DEF_STMT (then_clause);
      if (!vect_is_simple_use (then_clause, loop_vinfo, 
			       &then_def_stmt, &def, &dt))
	return false;
    }
  else if (TREE_CODE (then_clause) != INTEGER_CST 
	   && TREE_CODE (then_clause) != REAL_CST)
    return false;

  if (TREE_CODE (else_clause) == SSA_NAME)
    {
      tree else_def_stmt = SSA_NAME_DEF_STMT (else_clause);
      if (!vect_is_simple_use (else_clause, loop_vinfo, 
			       &else_def_stmt, &def, &dt))
	return false;
    }
  else if (TREE_CODE (else_clause) != INTEGER_CST 
	   && TREE_CODE (else_clause) != REAL_CST)
    return false;


  vec_mode = TYPE_MODE (vectype);

  if (!vec_stmt) 
    {
      STMT_VINFO_TYPE (stmt_info) = condition_vec_info_type;
      return expand_vec_cond_expr_p (op, vec_mode);
    }

  /* Transform */

  /* Handle def.  */
  scalar_dest = GIMPLE_STMT_OPERAND (stmt, 0);
  vec_dest = vect_create_destination_var (scalar_dest, vectype);

  /* Handle cond expr.  */
  vec_cond_lhs = 
    vect_get_vec_def_for_operand (TREE_OPERAND (cond_expr, 0), stmt, NULL);
  vec_cond_rhs = 
    vect_get_vec_def_for_operand (TREE_OPERAND (cond_expr, 1), stmt, NULL);
  vec_then_clause = vect_get_vec_def_for_operand (then_clause, stmt, NULL);
  vec_else_clause = vect_get_vec_def_for_operand (else_clause, stmt, NULL);

  /* Arguments are ready. create the new vector stmt.  */
  vec_compare = build2 (TREE_CODE (cond_expr), vectype, 
			vec_cond_lhs, vec_cond_rhs);
  vec_cond_expr = build3 (VEC_COND_EXPR, vectype, 
			  vec_compare, vec_then_clause, vec_else_clause);

  *vec_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node, vec_dest,
      		      vec_cond_expr);
  new_temp = make_ssa_name (vec_dest, *vec_stmt);
  GIMPLE_STMT_OPERAND (*vec_stmt, 0) = new_temp;
  vect_finish_stmt_generation (stmt, *vec_stmt, bsi);
  
  return true;
}

/* Function vect_transform_stmt.

   Create a vectorized stmt to replace STMT, and insert it at BSI.  */

bool
vect_transform_stmt (tree stmt, block_stmt_iterator *bsi, bool *strided_store)
{
  bool is_store = false;
  tree vec_stmt = NULL_TREE;
  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);
  tree orig_stmt_in_pattern;
  bool done;

  if (STMT_VINFO_RELEVANT_P (stmt_info))
    {
      switch (STMT_VINFO_TYPE (stmt_info))
      {
      case type_demotion_vec_info_type:
        done = vectorizable_type_demotion (stmt, bsi, &vec_stmt);
        gcc_assert (done);
        break;
                                                                                
      case type_promotion_vec_info_type:
	done = vectorizable_type_promotion (stmt, bsi, &vec_stmt);
	gcc_assert (done);
	break;

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
	if (DR_GROUP_FIRST_DR (stmt_info))
	  {
	    /* In case of interleaving, the whole chain is vectorized when the
	       last store in the chain is reached. Store stmts before the last
	       one are skipped, and there vec_stmt_info shouldn't be freed
	       meanwhile.  */
	    *strided_store = true;
	    if (STMT_VINFO_VEC_STMT (stmt_info))
	      is_store = true;
	  }
	else
	  is_store = true;
	break;

      case condition_vec_info_type:
	done = vectorizable_condition (stmt, bsi, &vec_stmt);
	gcc_assert (done);
	break;

      case call_vec_info_type:
	done = vectorizable_call (stmt, bsi, &vec_stmt);
	break;

      default:
	if (vect_print_dump_info (REPORT_DETAILS))
	  fprintf (vect_dump, "stmt not supported.");
	gcc_unreachable ();
      }

      gcc_assert (vec_stmt || *strided_store);
      if (vec_stmt)
	{
	  STMT_VINFO_VEC_STMT (stmt_info) = vec_stmt;
	  orig_stmt_in_pattern = STMT_VINFO_RELATED_STMT (stmt_info);
	  if (orig_stmt_in_pattern)
	    {
	      stmt_vec_info stmt_vinfo = vinfo_for_stmt (orig_stmt_in_pattern);
	      if (STMT_VINFO_IN_PATTERN_P (stmt_vinfo))
		{
		  gcc_assert (STMT_VINFO_RELATED_STMT (stmt_vinfo) == stmt);
		  
		  /* STMT was inserted by the vectorizer to replace a 
		     computation idiom.  ORIG_STMT_IN_PATTERN is a stmt in the 
		     original sequence that computed this idiom.  We need to 
		     record a pointer to VEC_STMT in the stmt_info of 
		     ORIG_STMT_IN_PATTERN.  See more details in the 
		     documentation of vect_pattern_recog.  */

		  STMT_VINFO_VEC_STMT (stmt_vinfo) = vec_stmt;
		}
	    }
	}
    }

  if (STMT_VINFO_LIVE_P (stmt_info))
    {
      switch (STMT_VINFO_TYPE (stmt_info))
      {
      case reduc_vec_info_type:
        done = vectorizable_reduction (stmt, bsi, &vec_stmt);
        gcc_assert (done);
        break;

      default:
        done = vectorizable_live_operation (stmt, bsi, &vec_stmt);
        gcc_assert (done);
      }
    }

  return is_store; 
}


/* This function builds ni_name = number of iterations loop executes
   on the loop preheader.  */

static tree
vect_build_loop_niters (loop_vec_info loop_vinfo)
{
  tree ni_name, stmt, var;
  edge pe;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree ni = unshare_expr (LOOP_VINFO_NITERS (loop_vinfo));

  var = create_tmp_var (TREE_TYPE (ni), "niters");
  add_referenced_var (var);
  ni_name = force_gimple_operand (ni, &stmt, false, var);

  pe = loop_preheader_edge (loop);
  if (stmt)
    {
      basic_block new_bb = bsi_insert_on_edge_immediate (pe, stmt);
      gcc_assert (!new_bb);
    }
      
  return ni_name;
}


/* This function generates the following statements:

 ni_name = number of iterations loop executes
 ratio = ni_name / vf
 ratio_mult_vf_name = ratio * vf

 and places them at the loop preheader edge.  */

static void 
vect_generate_tmps_on_preheader (loop_vec_info loop_vinfo, 
				 tree *ni_name_ptr,
				 tree *ratio_mult_vf_name_ptr, 
				 tree *ratio_name_ptr)
{

  edge pe;
  basic_block new_bb;
  tree stmt, ni_name;
  tree var;
  tree ratio_name;
  tree ratio_mult_vf_name;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree ni = LOOP_VINFO_NITERS (loop_vinfo);
  int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  tree log_vf;

  pe = loop_preheader_edge (loop);

  /* Generate temporary variable that contains 
     number of iterations loop executes.  */

  ni_name = vect_build_loop_niters (loop_vinfo);
  log_vf = build_int_cst (TREE_TYPE (ni), exact_log2 (vf));

  /* Create: ratio = ni >> log2(vf) */

  ratio_name = fold_build2 (RSHIFT_EXPR, TREE_TYPE (ni_name), ni_name, log_vf);
  if (!is_gimple_val (ratio_name))
    {
      var = create_tmp_var (TREE_TYPE (ni), "bnd");
      add_referenced_var (var);

      ratio_name = force_gimple_operand (ratio_name, &stmt, true, var);
      pe = loop_preheader_edge (loop);
      new_bb = bsi_insert_on_edge_immediate (pe, stmt);
      gcc_assert (!new_bb);
    }
       
  /* Create: ratio_mult_vf = ratio << log2 (vf).  */

  ratio_mult_vf_name = fold_build2 (LSHIFT_EXPR, TREE_TYPE (ratio_name),
				    ratio_name, log_vf);
  if (!is_gimple_val (ratio_mult_vf_name))
    {
      var = create_tmp_var (TREE_TYPE (ni), "ratio_mult_vf");
      add_referenced_var (var);

      ratio_mult_vf_name = force_gimple_operand (ratio_mult_vf_name, &stmt,
						 true, var);
      pe = loop_preheader_edge (loop);
      new_bb = bsi_insert_on_edge_immediate (pe, stmt);
      gcc_assert (!new_bb);
    }

  *ni_name_ptr = ni_name;
  *ratio_mult_vf_name_ptr = ratio_mult_vf_name;
  *ratio_name_ptr = ratio_name;
    
  return;  
}


/* Function update_vuses_to_preheader.

   Input:
   STMT - a statement with potential VUSEs.
   LOOP - the loop whose preheader will contain STMT.

   It's possible to vectorize a loop even though an SSA_NAME from a VUSE
   appears to be defined in a V_MAY_DEF in another statement in a loop.
   One such case is when the VUSE is at the dereference of a __restricted__
   pointer in a load and the V_MAY_DEF is at the dereference of a different
   __restricted__ pointer in a store.  Vectorization may result in
   copy_virtual_uses being called to copy the problematic VUSE to a new
   statement that is being inserted in the loop preheader.  This procedure
   is called to change the SSA_NAME in the new statement's VUSE from the
   SSA_NAME updated in the loop to the related SSA_NAME available on the
   path entering the loop.

   When this function is called, we have the following situation:

        # vuse <name1>
        S1: vload
    do {
        # name1 = phi < name0 , name2>

        # vuse <name1>
        S2: vload

        # name2 = vdef <name1>
        S3: vstore

    }while...

   Stmt S1 was created in the loop preheader block as part of misaligned-load
   handling. This function fixes the name of the vuse of S1 from 'name1' to
   'name0'.  */

static void
update_vuses_to_preheader (tree stmt, struct loop *loop)
{
  basic_block header_bb = loop->header;
  edge preheader_e = loop_preheader_edge (loop);
  ssa_op_iter iter;
  use_operand_p use_p;

  FOR_EACH_SSA_USE_OPERAND (use_p, stmt, iter, SSA_OP_VUSE)
    {
      tree ssa_name = USE_FROM_PTR (use_p);
      tree def_stmt = SSA_NAME_DEF_STMT (ssa_name);
      tree name_var = SSA_NAME_VAR (ssa_name);
      basic_block bb = bb_for_stmt (def_stmt);

      /* For a use before any definitions, def_stmt is a NOP_EXPR.  */
      if (!IS_EMPTY_STMT (def_stmt)
	  && flow_bb_inside_loop_p (loop, bb))
        {
          /* If the block containing the statement defining the SSA_NAME
             is in the loop then it's necessary to find the definition
             outside the loop using the PHI nodes of the header.  */
	  tree phi;
	  bool updated = false;

	  for (phi = phi_nodes (header_bb); phi; phi = TREE_CHAIN (phi))
	    {
	      if (SSA_NAME_VAR (PHI_RESULT (phi)) == name_var)
		{
		  SET_USE (use_p, PHI_ARG_DEF (phi, preheader_e->dest_idx));
		  updated = true;
		  break;
		}
	    }
	  gcc_assert (updated);
	}
    }
}


/*   Function vect_update_ivs_after_vectorizer.

     "Advance" the induction variables of LOOP to the value they should take
     after the execution of LOOP.  This is currently necessary because the
     vectorizer does not handle induction variables that are used after the
     loop.  Such a situation occurs when the last iterations of LOOP are
     peeled, because:
     1. We introduced new uses after LOOP for IVs that were not originally used
        after LOOP: the IVs of LOOP are now used by an epilog loop.
     2. LOOP is going to be vectorized; this means that it will iterate N/VF
        times, whereas the loop IVs should be bumped N times.

     Input:
     - LOOP - a loop that is going to be vectorized. The last few iterations
              of LOOP were peeled.
     - NITERS - the number of iterations that LOOP executes (before it is
                vectorized). i.e, the number of times the ivs should be bumped.
     - UPDATE_E - a successor edge of LOOP->exit that is on the (only) path
                  coming out from LOOP on which there are uses of the LOOP ivs
		  (this is the path from LOOP->exit to epilog_loop->preheader).

                  The new definitions of the ivs are placed in LOOP->exit.
                  The phi args associated with the edge UPDATE_E in the bb
                  UPDATE_E->dest are updated accordingly.

     Assumption 1: Like the rest of the vectorizer, this function assumes
     a single loop exit that has a single predecessor.

     Assumption 2: The phi nodes in the LOOP header and in update_bb are
     organized in the same order.

     Assumption 3: The access function of the ivs is simple enough (see
     vect_can_advance_ivs_p).  This assumption will be relaxed in the future.

     Assumption 4: Exactly one of the successors of LOOP exit-bb is on a path
     coming out of LOOP on which the ivs of LOOP are used (this is the path 
     that leads to the epilog loop; other paths skip the epilog loop).  This
     path starts with the edge UPDATE_E, and its destination (denoted update_bb)
     needs to have its phis updated.
 */

static void
vect_update_ivs_after_vectorizer (loop_vec_info loop_vinfo, tree niters, 
				  edge update_e)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block exit_bb = single_exit (loop)->dest;
  tree phi, phi1;
  basic_block update_bb = update_e->dest;

  /* gcc_assert (vect_can_advance_ivs_p (loop_vinfo)); */

  /* Make sure there exists a single-predecessor exit bb:  */
  gcc_assert (single_pred_p (exit_bb));

  for (phi = phi_nodes (loop->header), phi1 = phi_nodes (update_bb); 
       phi && phi1; 
       phi = PHI_CHAIN (phi), phi1 = PHI_CHAIN (phi1))
    {
      tree access_fn = NULL;
      tree evolution_part;
      tree init_expr;
      tree step_expr;
      tree var, stmt, ni, ni_name;
      block_stmt_iterator last_bsi;

      if (vect_print_dump_info (REPORT_DETAILS))
        {
          fprintf (vect_dump, "vect_update_ivs_after_vectorizer: phi: ");
          print_generic_expr (vect_dump, phi, TDF_SLIM);
        }

      /* Skip virtual phi's.  */
      if (!is_gimple_reg (SSA_NAME_VAR (PHI_RESULT (phi))))
	{
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "virtual phi. skip.");
	  continue;
	}

      /* Skip reduction phis.  */
      if (STMT_VINFO_DEF_TYPE (vinfo_for_stmt (phi)) == vect_reduction_def)
        { 
          if (vect_print_dump_info (REPORT_DETAILS))
            fprintf (vect_dump, "reduc phi. skip.");
          continue;
        } 

      access_fn = analyze_scalar_evolution (loop, PHI_RESULT (phi)); 
      gcc_assert (access_fn);
      evolution_part =
	 unshare_expr (evolution_part_in_loop_num (access_fn, loop->num));
      gcc_assert (evolution_part != NULL_TREE);
      
      /* FORNOW: We do not support IVs whose evolution function is a polynomial
         of degree >= 2 or exponential.  */
      gcc_assert (!tree_is_chrec (evolution_part));

      step_expr = evolution_part;
      init_expr = unshare_expr (initial_condition_in_loop_num (access_fn, 
							       loop->num));

      ni = fold_build2 (PLUS_EXPR, TREE_TYPE (init_expr),
			fold_build2 (MULT_EXPR, TREE_TYPE (init_expr),
				     fold_convert (TREE_TYPE (init_expr), 
						   niters), 
				     step_expr),
			init_expr);

      var = create_tmp_var (TREE_TYPE (init_expr), "tmp");
      add_referenced_var (var);

      ni_name = force_gimple_operand (ni, &stmt, false, var);
      
      /* Insert stmt into exit_bb.  */
      last_bsi = bsi_last (exit_bb);
      if (stmt)
        bsi_insert_before (&last_bsi, stmt, BSI_SAME_STMT);   

      /* Fix phi expressions in the successor bb.  */
      SET_PHI_ARG_DEF (phi1, update_e->dest_idx, ni_name);
    }
}


/* Function vect_do_peeling_for_loop_bound

   Peel the last iterations of the loop represented by LOOP_VINFO.
   The peeled iterations form a new epilog loop.  Given that the loop now 
   iterates NITERS times, the new epilog loop iterates
   NITERS % VECTORIZATION_FACTOR times.
   
   The original loop will later be made to iterate 
   NITERS / VECTORIZATION_FACTOR times (this value is placed into RATIO).  */

static void 
vect_do_peeling_for_loop_bound (loop_vec_info loop_vinfo, tree *ratio)
{
  tree ni_name, ratio_mult_vf_name;
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  struct loop *new_loop;
  edge update_e;
  basic_block preheader;
  int loop_num;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_do_peeling_for_loop_bound ===");

  initialize_original_copy_tables ();

  /* Generate the following variables on the preheader of original loop:
	 
     ni_name = number of iteration the original loop executes
     ratio = ni_name / vf
     ratio_mult_vf_name = ratio * vf  */
  vect_generate_tmps_on_preheader (loop_vinfo, &ni_name,
				   &ratio_mult_vf_name, ratio);

  loop_num  = loop->num; 
  new_loop = slpeel_tree_peel_loop_to_edge (loop, single_exit (loop),
					    ratio_mult_vf_name, ni_name, false);
  gcc_assert (new_loop);
  gcc_assert (loop_num == loop->num);
#ifdef ENABLE_CHECKING
  slpeel_verify_cfg_after_peeling (loop, new_loop);
#endif

  /* A guard that controls whether the new_loop is to be executed or skipped
     is placed in LOOP->exit.  LOOP->exit therefore has two successors - one
     is the preheader of NEW_LOOP, where the IVs from LOOP are used.  The other
     is a bb after NEW_LOOP, where these IVs are not used.  Find the edge that
     is on the path where the LOOP IVs are used and need to be updated.  */

  preheader = loop_preheader_edge (new_loop)->src;
  if (EDGE_PRED (preheader, 0)->src == single_exit (loop)->dest)
    update_e = EDGE_PRED (preheader, 0);
  else
    update_e = EDGE_PRED (preheader, 1);

  /* Update IVs of original loop as if they were advanced 
     by ratio_mult_vf_name steps.  */
  vect_update_ivs_after_vectorizer (loop_vinfo, ratio_mult_vf_name, update_e); 

  /* After peeling we have to reset scalar evolution analyzer.  */
  scev_reset ();

  free_original_copy_tables ();
}


/* Function vect_gen_niters_for_prolog_loop

   Set the number of iterations for the loop represented by LOOP_VINFO
   to the minimum between LOOP_NITERS (the original iteration count of the loop)
   and the misalignment of DR - the data reference recorded in
   LOOP_VINFO_UNALIGNED_DR (LOOP_VINFO).  As a result, after the execution of 
   this loop, the data reference DR will refer to an aligned location.

   The following computation is generated:

   If the misalignment of DR is known at compile time:
     addr_mis = int mis = DR_MISALIGNMENT (dr);
   Else, compute address misalignment in bytes:
     addr_mis = addr & (vectype_size - 1)

   prolog_niters = min ( LOOP_NITERS , (VF - addr_mis/elem_size)&(VF-1) )
   
   (elem_size = element type size; an element is the scalar element 
	whose type is the inner type of the vectype)  

   For interleaving,

   prolog_niters = min ( LOOP_NITERS , 
                        (VF/group_size - addr_mis/elem_size)&(VF/group_size-1) )
	 where group_size is the size of the interleaved group.
*/

static tree 
vect_gen_niters_for_prolog_loop (loop_vec_info loop_vinfo, tree loop_niters)
{
  struct data_reference *dr = LOOP_VINFO_UNALIGNED_DR (loop_vinfo);
  int vf = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree var, stmt;
  tree iters, iters_name;
  edge pe;
  basic_block new_bb;
  tree dr_stmt = DR_STMT (dr);
  stmt_vec_info stmt_info = vinfo_for_stmt (dr_stmt);
  tree vectype = STMT_VINFO_VECTYPE (stmt_info);
  int vectype_align = TYPE_ALIGN (vectype) / BITS_PER_UNIT;
  tree niters_type = TREE_TYPE (loop_niters);
  int group_size = 1;
  int element_size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (DR_REF (dr))));

  if (DR_GROUP_FIRST_DR (stmt_info))
    {
      /* For interleaved access element size must be multiplied by the size of
	 the interleaved group.  */
      group_size = DR_GROUP_SIZE (vinfo_for_stmt (
					       DR_GROUP_FIRST_DR (stmt_info)));
      element_size *= group_size;
    }

  pe = loop_preheader_edge (loop); 

  if (LOOP_PEELING_FOR_ALIGNMENT (loop_vinfo) > 0)
    {
      int byte_misalign = LOOP_PEELING_FOR_ALIGNMENT (loop_vinfo);
      int elem_misalign = byte_misalign / element_size;

      if (vect_print_dump_info (REPORT_DETAILS))
        fprintf (vect_dump, "known alignment = %d.", byte_misalign);
      iters = build_int_cst (niters_type, 
			     (vf - elem_misalign)&(vf/group_size-1));
    }
  else
    {
      tree new_stmts = NULL_TREE;
      tree start_addr =
        vect_create_addr_base_for_vector_ref (dr_stmt, &new_stmts, NULL_TREE);
      tree ptr_type = TREE_TYPE (start_addr);
      tree size = TYPE_SIZE (ptr_type);
      tree type = lang_hooks.types.type_for_size (tree_low_cst (size, 1), 1);
      tree vectype_size_minus_1 = build_int_cst (type, vectype_align - 1);
      tree elem_size_log =
        build_int_cst (type, exact_log2 (vectype_align/vf));
      tree vf_minus_1 = build_int_cst (type, vf - 1);
      tree vf_tree = build_int_cst (type, vf);
      tree byte_misalign;
      tree elem_misalign;

      new_bb = bsi_insert_on_edge_immediate (pe, new_stmts);
      gcc_assert (!new_bb);
  
      /* Create:  byte_misalign = addr & (vectype_size - 1)  */
      byte_misalign = 
        fold_build2 (BIT_AND_EXPR, type, start_addr, vectype_size_minus_1);
  
      /* Create:  elem_misalign = byte_misalign / element_size  */
      elem_misalign =
        fold_build2 (RSHIFT_EXPR, type, byte_misalign, elem_size_log);

      /* Create:  (niters_type) (VF - elem_misalign)&(VF - 1)  */
      iters = fold_build2 (MINUS_EXPR, type, vf_tree, elem_misalign);
      iters = fold_build2 (BIT_AND_EXPR, type, iters, vf_minus_1);
      iters = fold_convert (niters_type, iters);
    }

  /* Create:  prolog_loop_niters = min (iters, loop_niters) */
  /* If the loop bound is known at compile time we already verified that it is
     greater than vf; since the misalignment ('iters') is at most vf, there's
     no need to generate the MIN_EXPR in this case.  */
  if (TREE_CODE (loop_niters) != INTEGER_CST)
    iters = fold_build2 (MIN_EXPR, niters_type, iters, loop_niters);

  if (vect_print_dump_info (REPORT_DETAILS))
    {
      fprintf (vect_dump, "niters for prolog loop: ");
      print_generic_expr (vect_dump, iters, TDF_SLIM);
    }

  var = create_tmp_var (niters_type, "prolog_loop_niters");
  add_referenced_var (var);
  iters_name = force_gimple_operand (iters, &stmt, false, var);

  /* Insert stmt on loop preheader edge.  */
  if (stmt)
    {
      basic_block new_bb = bsi_insert_on_edge_immediate (pe, stmt);
      gcc_assert (!new_bb);
    }

  return iters_name; 
}


/* Function vect_update_init_of_dr

   NITERS iterations were peeled from LOOP.  DR represents a data reference
   in LOOP.  This function updates the information recorded in DR to
   account for the fact that the first NITERS iterations had already been 
   executed.  Specifically, it updates the OFFSET field of DR.  */

static void
vect_update_init_of_dr (struct data_reference *dr, tree niters)
{
  tree offset = DR_OFFSET (dr);
      
  niters = fold_build2 (MULT_EXPR, TREE_TYPE (niters), niters, DR_STEP (dr));
  offset = fold_build2 (PLUS_EXPR, TREE_TYPE (offset), offset, niters);
  DR_OFFSET (dr) = offset;
}


/* Function vect_update_inits_of_drs

   NITERS iterations were peeled from the loop represented by LOOP_VINFO.  
   This function updates the information recorded for the data references in 
   the loop to account for the fact that the first NITERS iterations had 
   already been executed.  Specifically, it updates the initial_condition of the
   access_function of all the data_references in the loop.  */

static void
vect_update_inits_of_drs (loop_vec_info loop_vinfo, tree niters)
{
  unsigned int i;
  VEC (data_reference_p, heap) *datarefs = LOOP_VINFO_DATAREFS (loop_vinfo);
  struct data_reference *dr;

  if (vect_dump && (dump_flags & TDF_DETAILS))
    fprintf (vect_dump, "=== vect_update_inits_of_dr ===");

  for (i = 0; VEC_iterate (data_reference_p, datarefs, i, dr); i++)
    vect_update_init_of_dr (dr, niters);
}


/* Function vect_do_peeling_for_alignment

   Peel the first 'niters' iterations of the loop represented by LOOP_VINFO.
   'niters' is set to the misalignment of one of the data references in the
   loop, thereby forcing it to refer to an aligned location at the beginning
   of the execution of this loop.  The data reference for which we are
   peeling is recorded in LOOP_VINFO_UNALIGNED_DR.  */

static void
vect_do_peeling_for_alignment (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  tree niters_of_prolog_loop, ni_name;
  tree n_iters;
  struct loop *new_loop;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vect_do_peeling_for_alignment ===");

  initialize_original_copy_tables ();

  ni_name = vect_build_loop_niters (loop_vinfo);
  niters_of_prolog_loop = vect_gen_niters_for_prolog_loop (loop_vinfo, ni_name);
  
  /* Peel the prolog loop and iterate it niters_of_prolog_loop.  */
  new_loop = 
	slpeel_tree_peel_loop_to_edge (loop, loop_preheader_edge (loop), 
				       niters_of_prolog_loop, ni_name, true); 
  gcc_assert (new_loop);
#ifdef ENABLE_CHECKING
  slpeel_verify_cfg_after_peeling (new_loop, loop);
#endif

  /* Update number of times loop executes.  */
  n_iters = LOOP_VINFO_NITERS (loop_vinfo);
  LOOP_VINFO_NITERS (loop_vinfo) = fold_build2 (MINUS_EXPR,
		TREE_TYPE (n_iters), n_iters, niters_of_prolog_loop);

  /* Update the init conditions of the access functions of all data refs.  */
  vect_update_inits_of_drs (loop_vinfo, niters_of_prolog_loop);

  /* After peeling we have to reset scalar evolution analyzer.  */
  scev_reset ();

  free_original_copy_tables ();
}


/* Function vect_create_cond_for_align_checks.

   Create a conditional expression that represents the alignment checks for
   all of data references (array element references) whose alignment must be
   checked at runtime.

   Input:
   LOOP_VINFO - two fields of the loop information are used.
                LOOP_VINFO_PTR_MASK is the mask used to check the alignment.
                LOOP_VINFO_MAY_MISALIGN_STMTS contains the refs to be checked.

   Output:
   COND_EXPR_STMT_LIST - statements needed to construct the conditional
                         expression.
   The returned value is the conditional expression to be used in the if
   statement that controls which version of the loop gets executed at runtime.

   The algorithm makes two assumptions:
     1) The number of bytes "n" in a vector is a power of 2.
     2) An address "a" is aligned if a%n is zero and that this
        test can be done as a&(n-1) == 0.  For example, for 16
        byte vectors the test is a&0xf == 0.  */

static tree
vect_create_cond_for_align_checks (loop_vec_info loop_vinfo,
                                   tree *cond_expr_stmt_list)
{
  VEC(tree,heap) *may_misalign_stmts
    = LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo);
  tree ref_stmt;
  int mask = LOOP_VINFO_PTR_MASK (loop_vinfo);
  tree mask_cst;
  unsigned int i;
  tree psize;
  tree int_ptrsize_type;
  char tmp_name[20];
  tree or_tmp_name = NULL_TREE;
  tree and_tmp, and_tmp_name, and_stmt;
  tree ptrsize_zero;

  /* Check that mask is one less than a power of 2, i.e., mask is
     all zeros followed by all ones.  */
  gcc_assert ((mask != 0) && ((mask & (mask+1)) == 0));

  /* CHECKME: what is the best integer or unsigned type to use to hold a
     cast from a pointer value?  */
  psize = TYPE_SIZE (ptr_type_node);
  int_ptrsize_type
    = lang_hooks.types.type_for_size (tree_low_cst (psize, 1), 0);

  /* Create expression (mask & (dr_1 || ... || dr_n)) where dr_i is the address
     of the first vector of the i'th data reference. */

  for (i = 0; VEC_iterate (tree, may_misalign_stmts, i, ref_stmt); i++)
    {
      tree new_stmt_list = NULL_TREE;   
      tree addr_base;
      tree addr_tmp, addr_tmp_name, addr_stmt;
      tree or_tmp, new_or_tmp_name, or_stmt;

      /* create: addr_tmp = (int)(address_of_first_vector) */
      addr_base = vect_create_addr_base_for_vector_ref (ref_stmt, 
							&new_stmt_list, 
							NULL_TREE);

      if (new_stmt_list != NULL_TREE)
        append_to_statement_list_force (new_stmt_list, cond_expr_stmt_list);

      sprintf (tmp_name, "%s%d", "addr2int", i);
      addr_tmp = create_tmp_var (int_ptrsize_type, tmp_name);
      add_referenced_var (addr_tmp);
      addr_tmp_name = make_ssa_name (addr_tmp, NULL_TREE);
      addr_stmt = fold_convert (int_ptrsize_type, addr_base);
      addr_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node,
                          addr_tmp_name, addr_stmt);
      SSA_NAME_DEF_STMT (addr_tmp_name) = addr_stmt;
      append_to_statement_list_force (addr_stmt, cond_expr_stmt_list);

      /* The addresses are OR together.  */

      if (or_tmp_name != NULL_TREE)
        {
          /* create: or_tmp = or_tmp | addr_tmp */
          sprintf (tmp_name, "%s%d", "orptrs", i);
          or_tmp = create_tmp_var (int_ptrsize_type, tmp_name);
          add_referenced_var (or_tmp);
          new_or_tmp_name = make_ssa_name (or_tmp, NULL_TREE);
          or_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node,
	      		    new_or_tmp_name,
                            build2 (BIT_IOR_EXPR, int_ptrsize_type,
	                            or_tmp_name,
                                    addr_tmp_name));
          SSA_NAME_DEF_STMT (new_or_tmp_name) = or_stmt;
          append_to_statement_list_force (or_stmt, cond_expr_stmt_list);
          or_tmp_name = new_or_tmp_name;
        }
      else
        or_tmp_name = addr_tmp_name;

    } /* end for i */

  mask_cst = build_int_cst (int_ptrsize_type, mask);

  /* create: and_tmp = or_tmp & mask  */
  and_tmp = create_tmp_var (int_ptrsize_type, "andmask" );
  add_referenced_var (and_tmp);
  and_tmp_name = make_ssa_name (and_tmp, NULL_TREE);

  and_stmt = build2 (GIMPLE_MODIFY_STMT, void_type_node,
                     and_tmp_name,
                     build2 (BIT_AND_EXPR, int_ptrsize_type,
                             or_tmp_name, mask_cst));
  SSA_NAME_DEF_STMT (and_tmp_name) = and_stmt;
  append_to_statement_list_force (and_stmt, cond_expr_stmt_list);

  /* Make and_tmp the left operand of the conditional test against zero.
     if and_tmp has a nonzero bit then some address is unaligned.  */
  ptrsize_zero = build_int_cst (int_ptrsize_type, 0);
  return build2 (EQ_EXPR, boolean_type_node,
                 and_tmp_name, ptrsize_zero);
}


/* Function vect_transform_loop.

   The analysis phase has determined that the loop is vectorizable.
   Vectorize the loop - created vectorized stmts to replace the scalar
   stmts in the loop, and update the loop exit condition.  */

void
vect_transform_loop (loop_vec_info loop_vinfo)
{
  struct loop *loop = LOOP_VINFO_LOOP (loop_vinfo);
  basic_block *bbs = LOOP_VINFO_BBS (loop_vinfo);
  int nbbs = loop->num_nodes;
  block_stmt_iterator si;
  int i;
  tree ratio = NULL;
  int vectorization_factor = LOOP_VINFO_VECT_FACTOR (loop_vinfo);
  bitmap_iterator bi;
  unsigned int j;
  bool strided_store;

  if (vect_print_dump_info (REPORT_DETAILS))
    fprintf (vect_dump, "=== vec_transform_loop ===");

  /* If the loop has data references that may or may not be aligned then
     two versions of the loop need to be generated, one which is vectorized
     and one which isn't.  A test is then generated to control which of the
     loops is executed.  The test checks for the alignment of all of the
     data references that may or may not be aligned. */

  if (VEC_length (tree, LOOP_VINFO_MAY_MISALIGN_STMTS (loop_vinfo)))
    {
      struct loop *nloop;
      tree cond_expr;
      tree cond_expr_stmt_list = NULL_TREE;
      basic_block condition_bb;
      block_stmt_iterator cond_exp_bsi;
      basic_block merge_bb;
      basic_block new_exit_bb;
      edge new_exit_e, e;
      tree orig_phi, new_phi, arg;

      cond_expr = vect_create_cond_for_align_checks (loop_vinfo,
                                                     &cond_expr_stmt_list);
      initialize_original_copy_tables ();
      nloop = loop_version (loop, cond_expr, &condition_bb, true);
      free_original_copy_tables();

      /** Loop versioning violates an assumption we try to maintain during 
	 vectorization - that the loop exit block has a single predecessor.
	 After versioning, the exit block of both loop versions is the same
	 basic block (i.e. it has two predecessors). Just in order to simplify
	 following transformations in the vectorizer, we fix this situation
	 here by adding a new (empty) block on the exit-edge of the loop,
	 with the proper loop-exit phis to maintain loop-closed-form.  **/
      
      merge_bb = single_exit (loop)->dest;
      gcc_assert (EDGE_COUNT (merge_bb->preds) == 2);
      new_exit_bb = split_edge (single_exit (loop));
      new_exit_e = single_exit (loop);
      e = EDGE_SUCC (new_exit_bb, 0);

      for (orig_phi = phi_nodes (merge_bb); orig_phi; 
	   orig_phi = PHI_CHAIN (orig_phi))
	{
          new_phi = create_phi_node (SSA_NAME_VAR (PHI_RESULT (orig_phi)),
				     new_exit_bb);
          arg = PHI_ARG_DEF_FROM_EDGE (orig_phi, e);
          add_phi_arg (new_phi, arg, new_exit_e);
	  SET_PHI_ARG_DEF (orig_phi, e->dest_idx, PHI_RESULT (new_phi));
	} 

      /** end loop-exit-fixes after versioning  **/

      update_ssa (TODO_update_ssa);
      cond_exp_bsi = bsi_last (condition_bb);
      bsi_insert_before (&cond_exp_bsi, cond_expr_stmt_list, BSI_SAME_STMT);
    }

  /* CHECKME: we wouldn't need this if we called update_ssa once
     for all loops.  */
  bitmap_zero (vect_vnames_to_rename);

  /* Peel the loop if there are data refs with unknown alignment.
     Only one data ref with unknown store is allowed.  */

  if (LOOP_PEELING_FOR_ALIGNMENT (loop_vinfo))
    vect_do_peeling_for_alignment (loop_vinfo);
  
  /* If the loop has a symbolic number of iterations 'n' (i.e. it's not a
     compile time constant), or it is a constant that doesn't divide by the
     vectorization factor, then an epilog loop needs to be created.
     We therefore duplicate the loop: the original loop will be vectorized,
     and will compute the first (n/VF) iterations. The second copy of the loop
     will remain scalar and will compute the remaining (n%VF) iterations.
     (VF is the vectorization factor).  */

  if (!LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
      || (LOOP_VINFO_NITERS_KNOWN_P (loop_vinfo)
          && LOOP_VINFO_INT_NITERS (loop_vinfo) % vectorization_factor != 0))
    vect_do_peeling_for_loop_bound (loop_vinfo, &ratio);
  else
    ratio = build_int_cst (TREE_TYPE (LOOP_VINFO_NITERS (loop_vinfo)),
		LOOP_VINFO_INT_NITERS (loop_vinfo) / vectorization_factor);

  /* 1) Make sure the loop header has exactly two entries
     2) Make sure we have a preheader basic block.  */

  gcc_assert (EDGE_COUNT (loop->header->preds) == 2);

  split_edge (loop_preheader_edge (loop));

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

	  if (vect_print_dump_info (REPORT_DETAILS))
	    {
	      fprintf (vect_dump, "------>vectorizing statement: ");
	      print_generic_expr (vect_dump, stmt, TDF_SLIM);
	    }	
	  stmt_info = vinfo_for_stmt (stmt);
	  gcc_assert (stmt_info);
	  if (!STMT_VINFO_RELEVANT_P (stmt_info)
	      && !STMT_VINFO_LIVE_P (stmt_info))
	    {
	      bsi_next (&si);
	      continue;
	    }

	  if ((TYPE_VECTOR_SUBPARTS (STMT_VINFO_VECTYPE (stmt_info))
		 != (unsigned HOST_WIDE_INT) vectorization_factor)
	      && vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "multiple-types.");

	  /* -------- vectorize statement ------------ */
	  if (vect_print_dump_info (REPORT_DETAILS))
	    fprintf (vect_dump, "transform statement.");

	  strided_store = false;
	  is_store = vect_transform_stmt (stmt, &si, &strided_store);
          if (is_store)
            {
	      stmt_ann_t ann;
	      if (DR_GROUP_FIRST_DR (stmt_info))
		{
		  /* Interleaving. If IS_STORE is TRUE, the vectorization of the
		     interleaving chain was completed - free all the stores in
		     the chain.  */
		  tree next = DR_GROUP_FIRST_DR (stmt_info);
		  tree tmp;
		  stmt_vec_info next_stmt_info;

		  while (next)
		    {
		      next_stmt_info = vinfo_for_stmt (next);
		      /* Free the attached stmt_vec_info and remove the stmt.  */
		      ann = stmt_ann (next);
		      tmp = DR_GROUP_NEXT_DR (next_stmt_info);
		      free (next_stmt_info);
		      set_stmt_info (ann, NULL);
		      next = tmp;
		    }
		  bsi_remove (&si, true);
		  continue;
		}
	      else
		{
		  /* Free the attached stmt_vec_info and remove the stmt.  */
		  ann = stmt_ann (stmt);
		  free (stmt_info);
		  set_stmt_info (ann, NULL);
		  bsi_remove (&si, true);
		  continue;
		}
	    }
	  else
	    {
	      if (strided_store)
		{
		  /* This is case of skipped interleaved store. We don't free
		     its stmt_vec_info.  */
		  bsi_remove (&si, true);
		  continue;
		}
            }
	  bsi_next (&si);
	}		        /* stmts in BB */
    }				/* BBs in loop */

  slpeel_make_loop_iterate_ntimes (loop, ratio);

  EXECUTE_IF_SET_IN_BITMAP (vect_vnames_to_rename, 0, j, bi)
    mark_sym_for_renaming (SSA_NAME_VAR (ssa_name (j)));

  /* The memory tags and pointers in vectorized statements need to
     have their SSA forms updated.  FIXME, why can't this be delayed
     until all the loops have been transformed?  */
  update_ssa (TODO_update_ssa);

  if (vect_print_dump_info (REPORT_VECTORIZED_LOOPS))
    fprintf (vect_dump, "LOOP VECTORIZED.");
}
