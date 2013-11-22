/* UndefinedBehaviorSanitizer, undefined behavior detector.
   Copyright (C) 2013 Free Software Foundation, Inc.
   Contributed by Marek Polacek <polacek@redhat.com>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "stor-layout.h"
#include "stringpool.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "gimple-walk.h"
#include "hashtab.h"
#include "output.h"
#include "tm_p.h"
#include "toplev.h"
#include "cfgloop.h"
#include "ubsan.h"
#include "c-family/c-common.h"

/* From trans-mem.c.  */
#define PROB_VERY_UNLIKELY      (REG_BR_PROB_BASE / 2000 - 1)

/* Map from a tree to a VAR_DECL tree.  */

struct GTY(()) tree_type_map {
  struct tree_map_base type;
  tree decl;
};

#define tree_type_map_eq tree_map_base_eq
#define tree_type_map_marked_p tree_map_base_marked_p

/* Hash from a tree in a tree_type_map.  */

unsigned int
tree_type_map_hash (const void *item)
{
  return TYPE_UID (((const struct tree_type_map *)item)->type.from);
}

static GTY ((if_marked ("tree_type_map_marked_p"), param_is (struct tree_type_map)))
     htab_t decl_tree_for_type;

/* Lookup a VAR_DECL for TYPE, and return it if we find one.  */

static tree
decl_for_type_lookup (tree type)
{
  /* If the hash table is not initialized yet, create it now.  */
  if (decl_tree_for_type == NULL)
    {
      decl_tree_for_type = htab_create_ggc (10, tree_type_map_hash,
					    tree_type_map_eq, 0);
      /* That also means we don't have to bother with the lookup.  */
      return NULL_TREE;
    }

  struct tree_type_map *h, in;
  in.type.from = type;

  h = (struct tree_type_map *)
      htab_find_with_hash (decl_tree_for_type, &in, TYPE_UID (type));
  return h ? h->decl : NULL_TREE;
}

/* Insert a mapping TYPE->DECL in the VAR_DECL for type hashtable.  */

static void
decl_for_type_insert (tree type, tree decl)
{
  struct tree_type_map *h;
  void **slot;

  h = ggc_alloc_tree_type_map ();
  h->type.from = type;
  h->decl = decl;
  slot = htab_find_slot_with_hash (decl_tree_for_type, h, TYPE_UID (type),
                                  INSERT);
  *(struct tree_type_map **) slot = h;
}

/* Helper routine, which encodes a value in the pointer_sized_int_node.
   Arguments with precision <= POINTER_SIZE are passed directly,
   the rest is passed by reference.  T is a value we are to encode.  */

tree
ubsan_encode_value (tree t)
{
  tree type = TREE_TYPE (t);
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
      if (TYPE_PRECISION (type) <= POINTER_SIZE)
	return fold_build1 (NOP_EXPR, pointer_sized_int_node, t);
      else
	return build_fold_addr_expr (t);
    case REAL_TYPE:
      {
	unsigned int bitsize = GET_MODE_BITSIZE (TYPE_MODE (type));
	if (bitsize <= POINTER_SIZE)
	  {
	    tree itype = build_nonstandard_integer_type (bitsize, true);
	    t = fold_build1 (VIEW_CONVERT_EXPR, itype, t);
	    return fold_convert (pointer_sized_int_node, t);
	  }
	else
	  {
	    if (!TREE_ADDRESSABLE (t))
	      {
		/* The reason for this is that we don't want to pessimize
		   code by making vars unnecessarily addressable.  */
		tree var = create_tmp_var (TREE_TYPE (t), NULL);
		tree tem = build2 (MODIFY_EXPR, void_type_node, var, t);
		t = build_fold_addr_expr (var);
		return build2 (COMPOUND_EXPR, TREE_TYPE (t), tem, t);
	      }
	    else
	      return build_fold_addr_expr (t);
	  }
      }
    default:
      gcc_unreachable ();
    }
}

/* Build
   struct __ubsan_type_descriptor
   {
     unsigned short __typekind;
     unsigned short __typeinfo;
     char __typename[];
   }
   type.  */

static tree
ubsan_type_descriptor_type (void)
{
  static const char *field_names[3]
    = { "__typekind", "__typeinfo", "__typename" };
  tree fields[3], ret;
  tree itype = build_range_type (sizetype, size_zero_node, NULL_TREE);
  tree flex_arr_type = build_array_type (char_type_node, itype);

  ret = make_node (RECORD_TYPE);
  for (int i = 0; i < 3; i++)
    {
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			      get_identifier (field_names[i]),
			      (i == 2) ? flex_arr_type
			      : short_unsigned_type_node);
      DECL_CONTEXT (fields[i]) = ret;
      if (i)
	DECL_CHAIN (fields[i - 1]) = fields[i];
    }
  TYPE_FIELDS (ret) = fields[0];
  TYPE_NAME (ret) = get_identifier ("__ubsan_type_descriptor");
  layout_type (ret);
  return ret;
}

/* Build
   struct __ubsan_source_location
   {
     const char *__filename;
     unsigned int __line;
     unsigned int __column;
   }
   type.  */

static tree
ubsan_source_location_type (void)
{
  static const char *field_names[3]
    = { "__filename", "__line", "__column" };
  tree fields[3], ret;
  tree const_char_type = build_qualified_type (char_type_node,
					       TYPE_QUAL_CONST);

  ret = make_node (RECORD_TYPE);
  for (int i = 0; i < 3; i++)
    {
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL,
			      get_identifier (field_names[i]),
			      (i == 0) ? build_pointer_type (const_char_type)
			      : unsigned_type_node);
      DECL_CONTEXT (fields[i]) = ret;
      if (i)
	DECL_CHAIN (fields[i - 1]) = fields[i];
    }
  TYPE_FIELDS (ret) = fields[0];
  TYPE_NAME (ret) = get_identifier ("__ubsan_source_location");
  layout_type (ret);
  return ret;
}

/* Helper routine that returns a CONSTRUCTOR of __ubsan_source_location
   type with its fields filled from a location_t LOC.  */

static tree
ubsan_source_location (location_t loc)
{
  expanded_location xloc;
  tree type = ubsan_source_location_type ();

  xloc = expand_location (loc);

  /* Fill in the values from LOC.  */
  size_t len = strlen (xloc.file);
  tree str = build_string (len + 1, xloc.file);
  TREE_TYPE (str) = build_array_type (char_type_node,
				      build_index_type (size_int (len)));
  TREE_READONLY (str) = 1;
  TREE_STATIC (str) = 1;
  str = build_fold_addr_expr_loc (loc, str);
  tree ctor = build_constructor_va (type, 3, NULL_TREE, str, NULL_TREE,
				    build_int_cst (unsigned_type_node,
						   xloc.line), NULL_TREE,
				    build_int_cst (unsigned_type_node,
						   xloc.column));
  TREE_CONSTANT (ctor) = 1;
  TREE_STATIC (ctor) = 1;

  return ctor;
}

/* This routine returns a magic number for TYPE.  */

static unsigned short
get_ubsan_type_info_for_type (tree type)
{
  gcc_assert (TYPE_SIZE (type) && tree_fits_uhwi_p (TYPE_SIZE (type)));
  int prec = exact_log2 (tree_to_uhwi (TYPE_SIZE (type)));
  gcc_assert (prec != -1);
  return (prec << 1) | !TYPE_UNSIGNED (type);
}

/* Helper routine that returns ADDR_EXPR of a VAR_DECL of a type
   descriptor.  It first looks into the hash table; if not found,
   create the VAR_DECL, put it into the hash table and return the
   ADDR_EXPR of it.  TYPE describes a particular type.  WANT_POINTER_TYPE_P
   means whether we are interested in the pointer type and not the pointer
   itself.  */

tree
ubsan_type_descriptor (tree type, bool want_pointer_type_p)
{
  /* See through any typedefs.  */
  type = TYPE_MAIN_VARIANT (type);

  tree decl = decl_for_type_lookup (type);
  if (decl != NULL_TREE)
    return decl;

  tree dtype = ubsan_type_descriptor_type ();
  tree type2 = type;
  const char *tname = NULL;
  char *pretty_name;
  unsigned char deref_depth = 0;
  unsigned short tkind, tinfo;

  /* Get the name of the type, or the name of the pointer type.  */
  if (want_pointer_type_p)
    {
      gcc_assert (POINTER_TYPE_P (type));
      type2 = TREE_TYPE (type);

      /* Remove any '*' operators from TYPE.  */
      while (POINTER_TYPE_P (type2))
        deref_depth++, type2 = TREE_TYPE (type2);

      if (TREE_CODE (type2) == METHOD_TYPE)
        type2 = TYPE_METHOD_BASETYPE (type2);
    }

  if (TYPE_NAME (type2) != NULL)
    {
      if (TREE_CODE (TYPE_NAME (type2)) == IDENTIFIER_NODE)
	tname = IDENTIFIER_POINTER (TYPE_NAME (type2));
      else
	tname = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type2)));
    }

  if (tname == NULL)
    /* We weren't able to determine the type name.  */
    tname = "<unknown>";

  /* Decorate the type name with '', '*', "struct", or "union".  */
  pretty_name = (char *) alloca (strlen (tname) + 16 + deref_depth);
  if (want_pointer_type_p)
    {
      int pos = sprintf (pretty_name, "'%s%s%s%s%s%s%s",
			 TYPE_VOLATILE (type2) ? "volatile " : "",
			 TYPE_READONLY (type2) ? "const " : "",
			 TYPE_RESTRICT (type2) ? "restrict " : "",
			 TYPE_ATOMIC (type2) ? "_Atomic " : "",
			 TREE_CODE (type2) == RECORD_TYPE
			 ? "struct "
			 : TREE_CODE (type2) == UNION_TYPE
			   ? "union " : "", tname,
			 deref_depth == 0 ? "" : " ");
      while (deref_depth-- > 0)
        pretty_name[pos++] = '*';
      pretty_name[pos++] = '\'';
      pretty_name[pos] = '\0';
    }
  else
    sprintf (pretty_name, "'%s'", tname);

  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE:
      tkind = 0x0000;
      break;
    case REAL_TYPE:
      tkind = 0x0001;
      break;
    default:
      tkind = 0xffff;
      break;
    }
  tinfo = get_ubsan_type_info_for_type (type);

  /* Create a new VAR_DECL of type descriptor.  */
  char tmp_name[32];
  static unsigned int type_var_id_num;
  ASM_GENERATE_INTERNAL_LABEL (tmp_name, "Lubsan_type", type_var_id_num++);
  decl = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier (tmp_name),
			  dtype);
  TREE_STATIC (decl) = 1;
  TREE_PUBLIC (decl) = 0;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  DECL_EXTERNAL (decl) = 0;

  size_t len = strlen (pretty_name);
  tree str = build_string (len + 1, pretty_name);
  TREE_TYPE (str) = build_array_type (char_type_node,
				      build_index_type (size_int (len)));
  TREE_READONLY (str) = 1;
  TREE_STATIC (str) = 1;
  tree ctor = build_constructor_va (dtype, 3, NULL_TREE,
				    build_int_cst (short_unsigned_type_node,
						   tkind), NULL_TREE,
				    build_int_cst (short_unsigned_type_node,
						   tinfo), NULL_TREE, str);
  TREE_CONSTANT (ctor) = 1;
  TREE_STATIC (ctor) = 1;
  DECL_INITIAL (decl) = ctor;
  rest_of_decl_compilation (decl, 1, 0);

  /* Save the address of the VAR_DECL into the hash table.  */
  decl = build_fold_addr_expr (decl);
  decl_for_type_insert (type, decl);

  return decl;
}

/* Create a structure for the ubsan library.  NAME is a name of the new
   structure.  The arguments in ... are of __ubsan_type_descriptor type
   and there are at most two of them.  MISMATCH are data used by ubsan
   pointer checking.  */

tree
ubsan_create_data (const char *name, location_t loc,
		   const struct ubsan_mismatch_data *mismatch, ...)
{
  va_list args;
  tree ret, t;
  tree fields[3];
  vec<tree, va_gc> *saved_args = NULL;
  size_t i = 0;

  /* Firstly, create a pointer to type descriptor type.  */
  tree td_type = ubsan_type_descriptor_type ();
  TYPE_READONLY (td_type) = 1;
  td_type = build_pointer_type (td_type);

  /* Create the structure type.  */
  ret = make_node (RECORD_TYPE);
  if (loc != UNKNOWN_LOCATION)
    {
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE,
			      ubsan_source_location_type ());
      DECL_CONTEXT (fields[i]) = ret;
      i++;
    }

  va_start (args, mismatch);
  for (t = va_arg (args, tree); t != NULL_TREE;
       i++, t = va_arg (args, tree))
    {
      gcc_checking_assert (i < 3);
      /* Save the tree arguments for later use.  */
      vec_safe_push (saved_args, t);
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE,
			      td_type);
      DECL_CONTEXT (fields[i]) = ret;
      if (i)
	DECL_CHAIN (fields[i - 1]) = fields[i];
    }
  va_end (args);

  if (mismatch != NULL)
    {
      /* We have to add two more decls.  */
      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE,
				pointer_sized_int_node);
      DECL_CONTEXT (fields[i]) = ret;
      DECL_CHAIN (fields[i - 1]) = fields[i];
      i++;

      fields[i] = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL_TREE,
			      unsigned_char_type_node);
      DECL_CONTEXT (fields[i]) = ret;
      DECL_CHAIN (fields[i - 1]) = fields[i];
      i++;
    }

  TYPE_FIELDS (ret) = fields[0];
  TYPE_NAME (ret) = get_identifier (name);
  layout_type (ret);

  /* Now, fill in the type.  */
  char tmp_name[32];
  static unsigned int ubsan_var_id_num;
  ASM_GENERATE_INTERNAL_LABEL (tmp_name, "Lubsan_data", ubsan_var_id_num++);
  tree var = build_decl (UNKNOWN_LOCATION, VAR_DECL, get_identifier (tmp_name),
			 ret);
  TREE_STATIC (var) = 1;
  TREE_PUBLIC (var) = 0;
  DECL_ARTIFICIAL (var) = 1;
  DECL_IGNORED_P (var) = 1;
  DECL_EXTERNAL (var) = 0;

  vec<constructor_elt, va_gc> *v;
  vec_alloc (v, i);
  tree ctor = build_constructor (ret, v);

  /* If desirable, set the __ubsan_source_location element.  */
  if (loc != UNKNOWN_LOCATION)
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, ubsan_source_location (loc));

  size_t nelts = vec_safe_length (saved_args);
  for (i = 0; i < nelts; i++)
    {
      t = (*saved_args)[i];
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, t);
    }

  if (mismatch != NULL)
    {
      /* Append the pointer data.  */
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, mismatch->align);
      CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, mismatch->ckind);
    }

  TREE_CONSTANT (ctor) = 1;
  TREE_STATIC (ctor) = 1;
  DECL_INITIAL (var) = ctor;
  rest_of_decl_compilation (var, 1, 0);

  return var;
}

/* Instrument the __builtin_unreachable call.  We just call the libubsan
   routine instead.  */

tree
ubsan_instrument_unreachable (location_t loc)
{
  tree data = ubsan_create_data ("__ubsan_unreachable_data", loc, NULL,
				 NULL_TREE);
  tree t = builtin_decl_explicit (BUILT_IN_UBSAN_HANDLE_BUILTIN_UNREACHABLE);
  return build_call_expr_loc (loc, t, 1, build_fold_addr_expr_loc (loc, data));
}

/* Return true if T is a call to a libubsan routine.  */

bool
is_ubsan_builtin_p (tree t)
{
  gcc_checking_assert (TREE_CODE (t) == FUNCTION_DECL);
  return strncmp (IDENTIFIER_POINTER (DECL_NAME (t)),
		  "__builtin___ubsan_", 18) == 0;
}

/* Expand UBSAN_NULL internal call.  */

void
ubsan_expand_null_ifn (gimple_stmt_iterator gsi)
{
  gimple stmt = gsi_stmt (gsi);
  location_t loc = gimple_location (stmt);
  gcc_assert (gimple_call_num_args (stmt) == 2);
  tree ptr = gimple_call_arg (stmt, 0);
  tree ckind = gimple_call_arg (stmt, 1);

  basic_block cur_bb = gsi_bb (gsi);

  /* Split the original block holding the pointer dereference.  */
  edge e = split_block (cur_bb, stmt);

  /* Get a hold on the 'condition block', the 'then block' and the
     'else block'.  */
  basic_block cond_bb = e->src;
  basic_block fallthru_bb = e->dest;
  basic_block then_bb = create_empty_bb (cond_bb);
  if (current_loops)
    {
      add_bb_to_loop (then_bb, cond_bb->loop_father);
      loops_state_set (LOOPS_NEED_FIXUP);
    }

  /* Make an edge coming from the 'cond block' into the 'then block';
     this edge is unlikely taken, so set up the probability accordingly.  */
  e = make_edge (cond_bb, then_bb, EDGE_TRUE_VALUE);
  e->probability = PROB_VERY_UNLIKELY;

  /* Connect 'then block' with the 'else block'.  This is needed
     as the ubsan routines we call in the 'then block' are not noreturn.
     The 'then block' only has one outcoming edge.  */
  make_single_succ_edge (then_bb, fallthru_bb, EDGE_FALLTHRU);

  /* Set up the fallthrough basic block.  */
  e = find_edge (cond_bb, fallthru_bb);
  e->flags = EDGE_FALSE_VALUE;
  e->count = cond_bb->count;
  e->probability = REG_BR_PROB_BASE - PROB_VERY_UNLIKELY;

  /* Update dominance info for the newly created then_bb; note that
     fallthru_bb's dominance info has already been updated by
     split_bock.  */
  if (dom_info_available_p (CDI_DOMINATORS))
    set_immediate_dominator (CDI_DOMINATORS, then_bb, cond_bb);

  /* Put the ubsan builtin call into the newly created BB.  */
  tree fn = builtin_decl_implicit (BUILT_IN_UBSAN_HANDLE_TYPE_MISMATCH);
  const struct ubsan_mismatch_data m
    = { build_zero_cst (pointer_sized_int_node), ckind };
  tree data = ubsan_create_data ("__ubsan_null_data",
				 loc, &m,
				 ubsan_type_descriptor (TREE_TYPE (ptr), true),
				 NULL_TREE);
  data = build_fold_addr_expr_loc (loc, data);
  gimple g = gimple_build_call (fn, 2, data,
				build_zero_cst (pointer_sized_int_node));
  gimple_set_location (g, loc);
  gimple_stmt_iterator gsi2 = gsi_start_bb (then_bb);
  gsi_insert_after (&gsi2, g, GSI_NEW_STMT);

  /* Unlink the UBSAN_NULLs vops before replacing it.  */
  unlink_stmt_vdef (stmt);

  g = gimple_build_cond (EQ_EXPR, ptr, build_int_cst (TREE_TYPE (ptr), 0),
			 NULL_TREE, NULL_TREE);
  gimple_set_location (g, loc);

  /* Replace the UBSAN_NULL with a GIMPLE_COND stmt.  */
  gsi_replace (&gsi, g, false);
}

/* Instrument a member call.  We check whether 'this' is NULL.  */

static void
instrument_member_call (gimple_stmt_iterator *iter)
{
  tree this_parm = gimple_call_arg (gsi_stmt (*iter), 0);
  tree kind = build_int_cst (unsigned_char_type_node, UBSAN_MEMBER_CALL);
  gimple g = gimple_build_call_internal (IFN_UBSAN_NULL, 2, this_parm, kind);
  gimple_set_location (g, gimple_location (gsi_stmt (*iter)));
  gsi_insert_before (iter, g, GSI_SAME_STMT);
}

/* Instrument a memory reference.  T is the pointer, IS_LHS says
   whether the pointer is on the left hand side of the assignment.  */

static void
instrument_mem_ref (tree t, gimple_stmt_iterator *iter, bool is_lhs)
{
  enum ubsan_null_ckind ikind = is_lhs ? UBSAN_STORE_OF : UBSAN_LOAD_OF;
  if (RECORD_OR_UNION_TYPE_P (TREE_TYPE (TREE_TYPE (t))))
    ikind = UBSAN_MEMBER_ACCESS;
  tree kind = build_int_cst (unsigned_char_type_node, ikind);
  gimple g = gimple_build_call_internal (IFN_UBSAN_NULL, 2, t, kind);
  gimple_set_location (g, gimple_location (gsi_stmt (*iter)));
  gsi_insert_before (iter, g, GSI_SAME_STMT);
}

/* Callback function for the pointer instrumentation.  */

static tree
instrument_null (tree *tp, int * /*walk_subtree*/, void *data)
{
  tree t = *tp;
  const enum tree_code code = TREE_CODE (t);
  struct walk_stmt_info *wi = (struct walk_stmt_info *) data;

  if (code == MEM_REF
      && TREE_CODE (TREE_OPERAND (t, 0)) == SSA_NAME)
    instrument_mem_ref (TREE_OPERAND (t, 0), &wi->gsi, wi->is_lhs);
  else if (code == ADDR_EXPR
	   && POINTER_TYPE_P (TREE_TYPE (t))
	   && TREE_CODE (TREE_TYPE (TREE_TYPE (t))) == METHOD_TYPE)
    instrument_member_call (&wi->gsi);

  return NULL_TREE;
}

/* Gate and execute functions for ubsan pass.  */

static unsigned int
ubsan_pass (void)
{
  basic_block bb;
  gimple_stmt_iterator gsi;

  FOR_EACH_BB (bb)
    {
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi);)
	{
	  struct walk_stmt_info wi;
	  gimple stmt = gsi_stmt (gsi);
	  if (is_gimple_debug (stmt))
	    {
	      gsi_next (&gsi);
	      continue;
	    }

	  memset (&wi, 0, sizeof (wi));
	  wi.gsi = gsi;
	  walk_gimple_op (stmt, instrument_null, &wi);
	  gsi_next (&gsi);
	}
    }
  return 0;
}

static bool
gate_ubsan (void)
{
  return flag_sanitize & SANITIZE_NULL;
}

namespace {

const pass_data pass_data_ubsan =
{
  GIMPLE_PASS, /* type */
  "ubsan", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_gate */
  true, /* has_execute */
  TV_TREE_UBSAN, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_ubsan : public gimple_opt_pass
{
public:
  pass_ubsan (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_ubsan, ctxt)
  {}

  /* opt_pass methods: */
  bool gate () { return gate_ubsan (); }
  unsigned int execute () { return ubsan_pass (); }

}; // class pass_ubsan

} // anon namespace

gimple_opt_pass *
make_pass_ubsan (gcc::context *ctxt)
{
  return new pass_ubsan (ctxt);
}

#include "gt-ubsan.h"
