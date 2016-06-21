/* Language-independent node constructors for parse phase of GNU compiler.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

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

/* This file contains the low level primitives for operating on tree nodes,
   including allocation, list operations, interning of identifiers,
   construction of data type nodes and statement nodes,
   and construction of type conversion nodes.  It also contains
   tables index by tree code that describe how to take apart
   nodes of that code.

   It is intended to be language-independent but can occasionally
   calls language-dependent routines.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "flags.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "attribs.h"
#include "toplev.h" /* get_random_seed */
#include "output.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "tree-inline.h"
#include "tree-iterator.h"
#include "internal-fn.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "tree-dfa.h"
#include "params.h"
#include "langhooks-def.h"
#include "tree-diagnostic.h"
#include "except.h"
#include "builtins.h"
#include "print-tree.h"
#include "ipa-utils.h"
#include "selftest.h"

/* Tree code classes.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,
#define END_OF_BASE_TREE_CODES tcc_exceptional,

const enum tree_code_class tree_code_type[] = {
#include "all-tree.def"
};

#undef DEFTREECODE
#undef END_OF_BASE_TREE_CODES

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,
#define END_OF_BASE_TREE_CODES 0,

const unsigned char tree_code_length[] = {
#include "all-tree.def"
};

#undef DEFTREECODE
#undef END_OF_BASE_TREE_CODES

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,
#define END_OF_BASE_TREE_CODES "@dummy",

static const char *const tree_code_name[] = {
#include "all-tree.def"
};

#undef DEFTREECODE
#undef END_OF_BASE_TREE_CODES

/* Each tree code class has an associated string representation.
   These must correspond to the tree_code_class entries.  */

const char *const tree_code_class_strings[] =
{
  "exceptional",
  "constant",
  "type",
  "declaration",
  "reference",
  "comparison",
  "unary",
  "binary",
  "statement",
  "vl_exp",
  "expression"
};

/* obstack.[ch] explicitly declined to prototype this.  */
extern int _obstack_allocated_p (struct obstack *h, void *obj);

/* Statistics-gathering stuff.  */

static int tree_code_counts[MAX_TREE_CODES];
int tree_node_counts[(int) all_kinds];
int tree_node_sizes[(int) all_kinds];

/* Keep in sync with tree.h:enum tree_node_kind.  */
static const char * const tree_node_kind_names[] = {
  "decls",
  "types",
  "blocks",
  "stmts",
  "refs",
  "exprs",
  "constants",
  "identifiers",
  "vecs",
  "binfos",
  "ssa names",
  "constructors",
  "random kinds",
  "lang_decl kinds",
  "lang_type kinds",
  "omp clauses",
};

/* Unique id for next decl created.  */
static GTY(()) int next_decl_uid;
/* Unique id for next type created.  */
static GTY(()) int next_type_uid = 1;
/* Unique id for next debug decl created.  Use negative numbers,
   to catch erroneous uses.  */
static GTY(()) int next_debug_decl_uid;

/* Since we cannot rehash a type after it is in the table, we have to
   keep the hash code.  */

struct GTY((for_user)) type_hash {
  unsigned long hash;
  tree type;
};

/* Initial size of the hash table (rounded to next prime).  */
#define TYPE_HASH_INITIAL_SIZE 1000

struct type_cache_hasher : ggc_cache_ptr_hash<type_hash>
{
  static hashval_t hash (type_hash *t) { return t->hash; }
  static bool equal (type_hash *a, type_hash *b);

  static int
  keep_cache_entry (type_hash *&t)
  {
    return ggc_marked_p (t->type);
  }
};

/* Now here is the hash table.  When recording a type, it is added to
   the slot whose index is the hash code.  Note that the hash table is
   used for several kinds of types (function types, array types and
   array index range types, for now).  While all these live in the
   same table, they are completely independent, and the hash code is
   computed differently for each of these.  */

static GTY ((cache)) hash_table<type_cache_hasher> *type_hash_table;

/* Hash table and temporary node for larger integer const values.  */
static GTY (()) tree int_cst_node;

struct int_cst_hasher : ggc_cache_ptr_hash<tree_node>
{
  static hashval_t hash (tree t);
  static bool equal (tree x, tree y);
};

static GTY ((cache)) hash_table<int_cst_hasher> *int_cst_hash_table;

/* Hash table for optimization flags and target option flags.  Use the same
   hash table for both sets of options.  Nodes for building the current
   optimization and target option nodes.  The assumption is most of the time
   the options created will already be in the hash table, so we avoid
   allocating and freeing up a node repeatably.  */
static GTY (()) tree cl_optimization_node;
static GTY (()) tree cl_target_option_node;

struct cl_option_hasher : ggc_cache_ptr_hash<tree_node>
{
  static hashval_t hash (tree t);
  static bool equal (tree x, tree y);
};

static GTY ((cache)) hash_table<cl_option_hasher> *cl_option_hash_table;

/* General tree->tree mapping  structure for use in hash tables.  */


static GTY ((cache))
     hash_table<tree_decl_map_cache_hasher> *debug_expr_for_decl;

static GTY ((cache))
     hash_table<tree_decl_map_cache_hasher> *value_expr_for_decl;

struct tree_vec_map_cache_hasher : ggc_cache_ptr_hash<tree_vec_map>
{
  static hashval_t hash (tree_vec_map *m) { return DECL_UID (m->base.from); }

  static bool
  equal (tree_vec_map *a, tree_vec_map *b)
  {
    return a->base.from == b->base.from;
  }

  static int
  keep_cache_entry (tree_vec_map *&m)
  {
    return ggc_marked_p (m->base.from);
  }
};

static GTY ((cache))
     hash_table<tree_vec_map_cache_hasher> *debug_args_for_decl;

static void set_type_quals (tree, int);
static void print_type_hash_statistics (void);
static void print_debug_expr_statistics (void);
static void print_value_expr_statistics (void);
static void type_hash_list (const_tree, inchash::hash &);
static void attribute_hash_list (const_tree, inchash::hash &);

tree global_trees[TI_MAX];
tree integer_types[itk_none];

bool int_n_enabled_p[NUM_INT_N_ENTS];
struct int_n_trees_t int_n_trees [NUM_INT_N_ENTS];

unsigned char tree_contains_struct[MAX_TREE_CODES][64];

/* Number of operands for each OpenMP clause.  */
unsigned const char omp_clause_num_ops[] =
{
  0, /* OMP_CLAUSE_ERROR  */
  1, /* OMP_CLAUSE_PRIVATE  */
  1, /* OMP_CLAUSE_SHARED  */
  1, /* OMP_CLAUSE_FIRSTPRIVATE  */
  2, /* OMP_CLAUSE_LASTPRIVATE  */
  5, /* OMP_CLAUSE_REDUCTION  */
  1, /* OMP_CLAUSE_COPYIN  */
  1, /* OMP_CLAUSE_COPYPRIVATE  */
  3, /* OMP_CLAUSE_LINEAR  */
  2, /* OMP_CLAUSE_ALIGNED  */
  1, /* OMP_CLAUSE_DEPEND  */
  1, /* OMP_CLAUSE_UNIFORM  */
  1, /* OMP_CLAUSE_TO_DECLARE  */
  1, /* OMP_CLAUSE_LINK  */
  2, /* OMP_CLAUSE_FROM  */
  2, /* OMP_CLAUSE_TO  */
  2, /* OMP_CLAUSE_MAP  */
  1, /* OMP_CLAUSE_USE_DEVICE_PTR  */
  1, /* OMP_CLAUSE_IS_DEVICE_PTR  */
  2, /* OMP_CLAUSE__CACHE_  */
  2, /* OMP_CLAUSE_GANG  */
  1, /* OMP_CLAUSE_ASYNC  */
  1, /* OMP_CLAUSE_WAIT  */
  0, /* OMP_CLAUSE_AUTO  */
  0, /* OMP_CLAUSE_SEQ  */
  1, /* OMP_CLAUSE__LOOPTEMP_  */
  1, /* OMP_CLAUSE_IF  */
  1, /* OMP_CLAUSE_NUM_THREADS  */
  1, /* OMP_CLAUSE_SCHEDULE  */
  0, /* OMP_CLAUSE_NOWAIT  */
  1, /* OMP_CLAUSE_ORDERED  */
  0, /* OMP_CLAUSE_DEFAULT  */
  3, /* OMP_CLAUSE_COLLAPSE  */
  0, /* OMP_CLAUSE_UNTIED   */
  1, /* OMP_CLAUSE_FINAL  */
  0, /* OMP_CLAUSE_MERGEABLE  */
  1, /* OMP_CLAUSE_DEVICE  */
  1, /* OMP_CLAUSE_DIST_SCHEDULE  */
  0, /* OMP_CLAUSE_INBRANCH  */
  0, /* OMP_CLAUSE_NOTINBRANCH  */
  1, /* OMP_CLAUSE_NUM_TEAMS  */
  1, /* OMP_CLAUSE_THREAD_LIMIT  */
  0, /* OMP_CLAUSE_PROC_BIND  */
  1, /* OMP_CLAUSE_SAFELEN  */
  1, /* OMP_CLAUSE_SIMDLEN  */
  0, /* OMP_CLAUSE_FOR  */
  0, /* OMP_CLAUSE_PARALLEL  */
  0, /* OMP_CLAUSE_SECTIONS  */
  0, /* OMP_CLAUSE_TASKGROUP  */
  1, /* OMP_CLAUSE_PRIORITY  */
  1, /* OMP_CLAUSE_GRAINSIZE  */
  1, /* OMP_CLAUSE_NUM_TASKS  */
  0, /* OMP_CLAUSE_NOGROUP  */
  0, /* OMP_CLAUSE_THREADS  */
  0, /* OMP_CLAUSE_SIMD  */
  1, /* OMP_CLAUSE_HINT  */
  0, /* OMP_CLAUSE_DEFALTMAP  */
  1, /* OMP_CLAUSE__SIMDUID_  */
  1, /* OMP_CLAUSE__CILK_FOR_COUNT_  */
  0, /* OMP_CLAUSE_INDEPENDENT  */
  1, /* OMP_CLAUSE_WORKER  */
  1, /* OMP_CLAUSE_VECTOR  */
  1, /* OMP_CLAUSE_NUM_GANGS  */
  1, /* OMP_CLAUSE_NUM_WORKERS  */
  1, /* OMP_CLAUSE_VECTOR_LENGTH  */
  1, /* OMP_CLAUSE_TILE  */
  2, /* OMP_CLAUSE__GRIDDIM_  */
};

const char * const omp_clause_code_name[] =
{
  "error_clause",
  "private",
  "shared",
  "firstprivate",
  "lastprivate",
  "reduction",
  "copyin",
  "copyprivate",
  "linear",
  "aligned",
  "depend",
  "uniform",
  "to",
  "link",
  "from",
  "to",
  "map",
  "use_device_ptr",
  "is_device_ptr",
  "_cache_",
  "gang",
  "async",
  "wait",
  "auto",
  "seq",
  "_looptemp_",
  "if",
  "num_threads",
  "schedule",
  "nowait",
  "ordered",
  "default",
  "collapse",
  "untied",
  "final",
  "mergeable",
  "device",
  "dist_schedule",
  "inbranch",
  "notinbranch",
  "num_teams",
  "thread_limit",
  "proc_bind",
  "safelen",
  "simdlen",
  "for",
  "parallel",
  "sections",
  "taskgroup",
  "priority",
  "grainsize",
  "num_tasks",
  "nogroup",
  "threads",
  "simd",
  "hint",
  "defaultmap",
  "_simduid_",
  "_Cilk_for_count_",
  "independent",
  "worker",
  "vector",
  "num_gangs",
  "num_workers",
  "vector_length",
  "tile",
  "_griddim_"
};


/* Return the tree node structure used by tree code CODE.  */

static inline enum tree_node_structure_enum
tree_node_structure_for_code (enum tree_code code)
{
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_declaration:
      {
	switch (code)
	  {
	  case FIELD_DECL:
	    return TS_FIELD_DECL;
	  case PARM_DECL:
	    return TS_PARM_DECL;
	  case VAR_DECL:
	    return TS_VAR_DECL;
	  case LABEL_DECL:
	    return TS_LABEL_DECL;
	  case RESULT_DECL:
	    return TS_RESULT_DECL;
	  case DEBUG_EXPR_DECL:
	    return TS_DECL_WRTL;
	  case CONST_DECL:
	    return TS_CONST_DECL;
	  case TYPE_DECL:
	    return TS_TYPE_DECL;
	  case FUNCTION_DECL:
	    return TS_FUNCTION_DECL;
	  case TRANSLATION_UNIT_DECL:
	    return TS_TRANSLATION_UNIT_DECL;
	  default:
	    return TS_DECL_NON_COMMON;
	  }
      }
    case tcc_type:
      return TS_TYPE_NON_COMMON;
    case tcc_reference:
    case tcc_comparison:
    case tcc_unary:
    case tcc_binary:
    case tcc_expression:
    case tcc_statement:
    case tcc_vl_exp:
      return TS_EXP;
    default:  /* tcc_constant and tcc_exceptional */
      break;
    }
  switch (code)
    {
      /* tcc_constant cases.  */
    case VOID_CST:		return TS_TYPED;
    case INTEGER_CST:		return TS_INT_CST;
    case REAL_CST:		return TS_REAL_CST;
    case FIXED_CST:		return TS_FIXED_CST;
    case COMPLEX_CST:		return TS_COMPLEX;
    case VECTOR_CST:		return TS_VECTOR;
    case STRING_CST:		return TS_STRING;
      /* tcc_exceptional cases.  */
    case ERROR_MARK:		return TS_COMMON;
    case IDENTIFIER_NODE:	return TS_IDENTIFIER;
    case TREE_LIST:		return TS_LIST;
    case TREE_VEC:		return TS_VEC;
    case SSA_NAME:		return TS_SSA_NAME;
    case PLACEHOLDER_EXPR:	return TS_COMMON;
    case STATEMENT_LIST:	return TS_STATEMENT_LIST;
    case BLOCK:			return TS_BLOCK;
    case CONSTRUCTOR:		return TS_CONSTRUCTOR;
    case TREE_BINFO:		return TS_BINFO;
    case OMP_CLAUSE:		return TS_OMP_CLAUSE;
    case OPTIMIZATION_NODE:	return TS_OPTIMIZATION;
    case TARGET_OPTION_NODE:	return TS_TARGET_OPTION;

    default:
      gcc_unreachable ();
    }
}


/* Initialize tree_contains_struct to describe the hierarchy of tree
   nodes.  */

static void
initialize_tree_contains_struct (void)
{
  unsigned i;

  for (i = ERROR_MARK; i < LAST_AND_UNUSED_TREE_CODE; i++)
    {
      enum tree_code code;
      enum tree_node_structure_enum ts_code;

      code = (enum tree_code) i;
      ts_code = tree_node_structure_for_code (code);

      /* Mark the TS structure itself.  */
      tree_contains_struct[code][ts_code] = 1;

      /* Mark all the structures that TS is derived from.  */
      switch (ts_code)
	{
	case TS_TYPED:
	case TS_BLOCK:
	  MARK_TS_BASE (code);
	  break;

	case TS_COMMON:
	case TS_INT_CST:
	case TS_REAL_CST:
	case TS_FIXED_CST:
	case TS_VECTOR:
	case TS_STRING:
	case TS_COMPLEX:
	case TS_SSA_NAME:
	case TS_CONSTRUCTOR:
	case TS_EXP:
	case TS_STATEMENT_LIST:
	  MARK_TS_TYPED (code);
	  break;

	case TS_IDENTIFIER:
	case TS_DECL_MINIMAL:
	case TS_TYPE_COMMON:
	case TS_LIST:
	case TS_VEC:
	case TS_BINFO:
	case TS_OMP_CLAUSE:
	case TS_OPTIMIZATION:
	case TS_TARGET_OPTION:
	  MARK_TS_COMMON (code);
	  break;

	case TS_TYPE_WITH_LANG_SPECIFIC:
	  MARK_TS_TYPE_COMMON (code);
	  break;

	case TS_TYPE_NON_COMMON:
	  MARK_TS_TYPE_WITH_LANG_SPECIFIC (code);
	  break;

	case TS_DECL_COMMON:
	  MARK_TS_DECL_MINIMAL (code);
	  break;

	case TS_DECL_WRTL:
	case TS_CONST_DECL:
	  MARK_TS_DECL_COMMON (code);
	  break;

	case TS_DECL_NON_COMMON:
	  MARK_TS_DECL_WITH_VIS (code);
	  break;

	case TS_DECL_WITH_VIS:
	case TS_PARM_DECL:
	case TS_LABEL_DECL:
	case TS_RESULT_DECL:
	  MARK_TS_DECL_WRTL (code);
	  break;

	case TS_FIELD_DECL:
	  MARK_TS_DECL_COMMON (code);
	  break;

	case TS_VAR_DECL:
	  MARK_TS_DECL_WITH_VIS (code);
	  break;

	case TS_TYPE_DECL:
	case TS_FUNCTION_DECL:
	  MARK_TS_DECL_NON_COMMON (code);
	  break;

	case TS_TRANSLATION_UNIT_DECL:
	  MARK_TS_DECL_COMMON (code);
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  /* Basic consistency checks for attributes used in fold.  */
  gcc_assert (tree_contains_struct[FUNCTION_DECL][TS_DECL_NON_COMMON]);
  gcc_assert (tree_contains_struct[TYPE_DECL][TS_DECL_NON_COMMON]);
  gcc_assert (tree_contains_struct[CONST_DECL][TS_DECL_COMMON]);
  gcc_assert (tree_contains_struct[VAR_DECL][TS_DECL_COMMON]);
  gcc_assert (tree_contains_struct[PARM_DECL][TS_DECL_COMMON]);
  gcc_assert (tree_contains_struct[RESULT_DECL][TS_DECL_COMMON]);
  gcc_assert (tree_contains_struct[FUNCTION_DECL][TS_DECL_COMMON]);
  gcc_assert (tree_contains_struct[TYPE_DECL][TS_DECL_COMMON]);
  gcc_assert (tree_contains_struct[TRANSLATION_UNIT_DECL][TS_DECL_COMMON]);
  gcc_assert (tree_contains_struct[LABEL_DECL][TS_DECL_COMMON]);
  gcc_assert (tree_contains_struct[FIELD_DECL][TS_DECL_COMMON]);
  gcc_assert (tree_contains_struct[VAR_DECL][TS_DECL_WRTL]);
  gcc_assert (tree_contains_struct[PARM_DECL][TS_DECL_WRTL]);
  gcc_assert (tree_contains_struct[RESULT_DECL][TS_DECL_WRTL]);
  gcc_assert (tree_contains_struct[FUNCTION_DECL][TS_DECL_WRTL]);
  gcc_assert (tree_contains_struct[LABEL_DECL][TS_DECL_WRTL]);
  gcc_assert (tree_contains_struct[CONST_DECL][TS_DECL_MINIMAL]);
  gcc_assert (tree_contains_struct[VAR_DECL][TS_DECL_MINIMAL]);
  gcc_assert (tree_contains_struct[PARM_DECL][TS_DECL_MINIMAL]);
  gcc_assert (tree_contains_struct[RESULT_DECL][TS_DECL_MINIMAL]);
  gcc_assert (tree_contains_struct[FUNCTION_DECL][TS_DECL_MINIMAL]);
  gcc_assert (tree_contains_struct[TYPE_DECL][TS_DECL_MINIMAL]);
  gcc_assert (tree_contains_struct[TRANSLATION_UNIT_DECL][TS_DECL_MINIMAL]);
  gcc_assert (tree_contains_struct[LABEL_DECL][TS_DECL_MINIMAL]);
  gcc_assert (tree_contains_struct[FIELD_DECL][TS_DECL_MINIMAL]);
  gcc_assert (tree_contains_struct[VAR_DECL][TS_DECL_WITH_VIS]);
  gcc_assert (tree_contains_struct[FUNCTION_DECL][TS_DECL_WITH_VIS]);
  gcc_assert (tree_contains_struct[TYPE_DECL][TS_DECL_WITH_VIS]);
  gcc_assert (tree_contains_struct[VAR_DECL][TS_VAR_DECL]);
  gcc_assert (tree_contains_struct[FIELD_DECL][TS_FIELD_DECL]);
  gcc_assert (tree_contains_struct[PARM_DECL][TS_PARM_DECL]);
  gcc_assert (tree_contains_struct[LABEL_DECL][TS_LABEL_DECL]);
  gcc_assert (tree_contains_struct[RESULT_DECL][TS_RESULT_DECL]);
  gcc_assert (tree_contains_struct[CONST_DECL][TS_CONST_DECL]);
  gcc_assert (tree_contains_struct[TYPE_DECL][TS_TYPE_DECL]);
  gcc_assert (tree_contains_struct[FUNCTION_DECL][TS_FUNCTION_DECL]);
  gcc_assert (tree_contains_struct[IMPORTED_DECL][TS_DECL_MINIMAL]);
  gcc_assert (tree_contains_struct[IMPORTED_DECL][TS_DECL_COMMON]);
  gcc_assert (tree_contains_struct[NAMELIST_DECL][TS_DECL_MINIMAL]);
  gcc_assert (tree_contains_struct[NAMELIST_DECL][TS_DECL_COMMON]);
}


/* Init tree.c.  */

void
init_ttree (void)
{
  /* Initialize the hash table of types.  */
  type_hash_table
    = hash_table<type_cache_hasher>::create_ggc (TYPE_HASH_INITIAL_SIZE);

  debug_expr_for_decl
    = hash_table<tree_decl_map_cache_hasher>::create_ggc (512);

  value_expr_for_decl
    = hash_table<tree_decl_map_cache_hasher>::create_ggc (512);

  int_cst_hash_table = hash_table<int_cst_hasher>::create_ggc (1024);

  int_cst_node = make_int_cst (1, 1);

  cl_option_hash_table = hash_table<cl_option_hasher>::create_ggc (64);

  cl_optimization_node = make_node (OPTIMIZATION_NODE);
  cl_target_option_node = make_node (TARGET_OPTION_NODE);

  /* Initialize the tree_contains_struct array.  */
  initialize_tree_contains_struct ();
  lang_hooks.init_ts ();
}


/* The name of the object as the assembler will see it (but before any
   translations made by ASM_OUTPUT_LABELREF).  Often this is the same
   as DECL_NAME.  It is an IDENTIFIER_NODE.  */
tree
decl_assembler_name (tree decl)
{
  if (!DECL_ASSEMBLER_NAME_SET_P (decl))
    lang_hooks.set_decl_assembler_name (decl);
  return DECL_WITH_VIS_CHECK (decl)->decl_with_vis.assembler_name;
}

/* When the target supports COMDAT groups, this indicates which group the
   DECL is associated with.  This can be either an IDENTIFIER_NODE or a
   decl, in which case its DECL_ASSEMBLER_NAME identifies the group.  */
tree
decl_comdat_group (const_tree node)
{
  struct symtab_node *snode = symtab_node::get (node);
  if (!snode)
    return NULL;
  return snode->get_comdat_group ();
}

/* Likewise, but make sure it's been reduced to an IDENTIFIER_NODE.  */
tree
decl_comdat_group_id (const_tree node)
{
  struct symtab_node *snode = symtab_node::get (node);
  if (!snode)
    return NULL;
  return snode->get_comdat_group_id ();
}

/* When the target supports named section, return its name as IDENTIFIER_NODE
   or NULL if it is in no section.  */
const char *
decl_section_name (const_tree node)
{
  struct symtab_node *snode = symtab_node::get (node);
  if (!snode)
    return NULL;
  return snode->get_section ();
}

/* Set section name of NODE to VALUE (that is expected to be
   identifier node) */
void
set_decl_section_name (tree node, const char *value)
{
  struct symtab_node *snode;

  if (value == NULL)
    {
      snode = symtab_node::get (node);
      if (!snode)
	return;
    }
  else if (TREE_CODE (node) == VAR_DECL)
    snode = varpool_node::get_create (node);
  else
    snode = cgraph_node::get_create (node);
  snode->set_section (value);
}

/* Return TLS model of a variable NODE.  */
enum tls_model
decl_tls_model (const_tree node)
{
  struct varpool_node *snode = varpool_node::get (node);
  if (!snode)
    return TLS_MODEL_NONE;
  return snode->tls_model;
}

/* Set TLS model of variable NODE to MODEL.  */
void
set_decl_tls_model (tree node, enum tls_model model)
{
  struct varpool_node *vnode;

  if (model == TLS_MODEL_NONE)
    {
      vnode = varpool_node::get (node);
      if (!vnode)
	return;
    }
  else
    vnode = varpool_node::get_create (node);
  vnode->tls_model = model;
}

/* Compute the number of bytes occupied by a tree with code CODE.
   This function cannot be used for nodes that have variable sizes,
   including TREE_VEC, INTEGER_CST, STRING_CST, and CALL_EXPR.  */
size_t
tree_code_size (enum tree_code code)
{
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_declaration:  /* A decl node */
      {
	switch (code)
	  {
	  case FIELD_DECL:
	    return sizeof (struct tree_field_decl);
	  case PARM_DECL:
	    return sizeof (struct tree_parm_decl);
	  case VAR_DECL:
	    return sizeof (struct tree_var_decl);
	  case LABEL_DECL:
	    return sizeof (struct tree_label_decl);
	  case RESULT_DECL:
	    return sizeof (struct tree_result_decl);
	  case CONST_DECL:
	    return sizeof (struct tree_const_decl);
	  case TYPE_DECL:
	    return sizeof (struct tree_type_decl);
	  case FUNCTION_DECL:
	    return sizeof (struct tree_function_decl);
	  case DEBUG_EXPR_DECL:
	    return sizeof (struct tree_decl_with_rtl);
	  case TRANSLATION_UNIT_DECL:
	    return sizeof (struct tree_translation_unit_decl);
	  case NAMESPACE_DECL:
	  case IMPORTED_DECL:
	  case NAMELIST_DECL:
	    return sizeof (struct tree_decl_non_common);
	  default:
	    return lang_hooks.tree_size (code);
	  }
      }

    case tcc_type:  /* a type node */
      return sizeof (struct tree_type_non_common);

    case tcc_reference:   /* a reference */
    case tcc_expression:  /* an expression */
    case tcc_statement:   /* an expression with side effects */
    case tcc_comparison:  /* a comparison expression */
    case tcc_unary:       /* a unary arithmetic expression */
    case tcc_binary:      /* a binary arithmetic expression */
      return (sizeof (struct tree_exp)
	      + (TREE_CODE_LENGTH (code) - 1) * sizeof (tree));

    case tcc_constant:  /* a constant */
      switch (code)
	{
	case VOID_CST:		return sizeof (struct tree_typed);
	case INTEGER_CST:	gcc_unreachable ();
	case REAL_CST:		return sizeof (struct tree_real_cst);
	case FIXED_CST:		return sizeof (struct tree_fixed_cst);
	case COMPLEX_CST:	return sizeof (struct tree_complex);
	case VECTOR_CST:	return sizeof (struct tree_vector);
	case STRING_CST:	gcc_unreachable ();
	default:
	  return lang_hooks.tree_size (code);
	}

    case tcc_exceptional:  /* something random, like an identifier.  */
      switch (code)
	{
	case IDENTIFIER_NODE:	return lang_hooks.identifier_size;
	case TREE_LIST:		return sizeof (struct tree_list);

	case ERROR_MARK:
	case PLACEHOLDER_EXPR:	return sizeof (struct tree_common);

	case TREE_VEC:
	case OMP_CLAUSE:	gcc_unreachable ();

	case SSA_NAME:		return sizeof (struct tree_ssa_name);

	case STATEMENT_LIST:	return sizeof (struct tree_statement_list);
	case BLOCK:		return sizeof (struct tree_block);
	case CONSTRUCTOR:	return sizeof (struct tree_constructor);
	case OPTIMIZATION_NODE: return sizeof (struct tree_optimization_option);
	case TARGET_OPTION_NODE: return sizeof (struct tree_target_option);

	default:
	  return lang_hooks.tree_size (code);
	}

    default:
      gcc_unreachable ();
    }
}

/* Compute the number of bytes occupied by NODE.  This routine only
   looks at TREE_CODE, except for those nodes that have variable sizes.  */
size_t
tree_size (const_tree node)
{
  const enum tree_code code = TREE_CODE (node);
  switch (code)
    {
    case INTEGER_CST:
      return (sizeof (struct tree_int_cst)
	      + (TREE_INT_CST_EXT_NUNITS (node) - 1) * sizeof (HOST_WIDE_INT));

    case TREE_BINFO:
      return (offsetof (struct tree_binfo, base_binfos)
	      + vec<tree, va_gc>
		  ::embedded_size (BINFO_N_BASE_BINFOS (node)));

    case TREE_VEC:
      return (sizeof (struct tree_vec)
	      + (TREE_VEC_LENGTH (node) - 1) * sizeof (tree));

    case VECTOR_CST:
      return (sizeof (struct tree_vector)
	      + (TYPE_VECTOR_SUBPARTS (TREE_TYPE (node)) - 1) * sizeof (tree));

    case STRING_CST:
      return TREE_STRING_LENGTH (node) + offsetof (struct tree_string, str) + 1;

    case OMP_CLAUSE:
      return (sizeof (struct tree_omp_clause)
	      + (omp_clause_num_ops[OMP_CLAUSE_CODE (node)] - 1)
	        * sizeof (tree));

    default:
      if (TREE_CODE_CLASS (code) == tcc_vl_exp)
	return (sizeof (struct tree_exp)
		+ (VL_EXP_OPERAND_LENGTH (node) - 1) * sizeof (tree));
      else
	return tree_code_size (code);
    }
}

/* Record interesting allocation statistics for a tree node with CODE
   and LENGTH.  */

static void
record_node_allocation_statistics (enum tree_code code ATTRIBUTE_UNUSED,
				   size_t length ATTRIBUTE_UNUSED)
{
  enum tree_code_class type = TREE_CODE_CLASS (code);
  tree_node_kind kind;

  if (!GATHER_STATISTICS)
    return;

  switch (type)
    {
    case tcc_declaration:  /* A decl node */
      kind = d_kind;
      break;

    case tcc_type:  /* a type node */
      kind = t_kind;
      break;

    case tcc_statement:  /* an expression with side effects */
      kind = s_kind;
      break;

    case tcc_reference:  /* a reference */
      kind = r_kind;
      break;

    case tcc_expression:  /* an expression */
    case tcc_comparison:  /* a comparison expression */
    case tcc_unary:  /* a unary arithmetic expression */
    case tcc_binary:  /* a binary arithmetic expression */
      kind = e_kind;
      break;

    case tcc_constant:  /* a constant */
      kind = c_kind;
      break;

    case tcc_exceptional:  /* something random, like an identifier.  */
      switch (code)
	{
	case IDENTIFIER_NODE:
	  kind = id_kind;
	  break;

	case TREE_VEC:
	  kind = vec_kind;
	  break;

	case TREE_BINFO:
	  kind = binfo_kind;
	  break;

	case SSA_NAME:
	  kind = ssa_name_kind;
	  break;

	case BLOCK:
	  kind = b_kind;
	  break;

	case CONSTRUCTOR:
	  kind = constr_kind;
	  break;

	case OMP_CLAUSE:
	  kind = omp_clause_kind;
	  break;

	default:
	  kind = x_kind;
	  break;
	}
      break;

    case tcc_vl_exp:
      kind = e_kind;
      break;

    default:
      gcc_unreachable ();
    }

  tree_code_counts[(int) code]++;
  tree_node_counts[(int) kind]++;
  tree_node_sizes[(int) kind] += length;
}

/* Allocate and return a new UID from the DECL_UID namespace.  */

int
allocate_decl_uid (void)
{
  return next_decl_uid++;
}

/* Return a newly allocated node of code CODE.  For decl and type
   nodes, some other fields are initialized.  The rest of the node is
   initialized to zero.  This function cannot be used for TREE_VEC,
   INTEGER_CST or OMP_CLAUSE nodes, which is enforced by asserts in
   tree_code_size.

   Achoo!  I got a code in the node.  */

tree
make_node_stat (enum tree_code code MEM_STAT_DECL)
{
  tree t;
  enum tree_code_class type = TREE_CODE_CLASS (code);
  size_t length = tree_code_size (code);

  record_node_allocation_statistics (code, length);

  t = ggc_alloc_cleared_tree_node_stat (length PASS_MEM_STAT);
  TREE_SET_CODE (t, code);

  switch (type)
    {
    case tcc_statement:
      TREE_SIDE_EFFECTS (t) = 1;
      break;

    case tcc_declaration:
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
	{
	  if (code == FUNCTION_DECL)
	    {
	      SET_DECL_ALIGN (t, FUNCTION_BOUNDARY);
	      DECL_MODE (t) = FUNCTION_MODE;
	    }
	  else
	    SET_DECL_ALIGN (t, 1);
	}
      DECL_SOURCE_LOCATION (t) = input_location;
      if (TREE_CODE (t) == DEBUG_EXPR_DECL)
	DECL_UID (t) = --next_debug_decl_uid;
      else
	{
	  DECL_UID (t) = allocate_decl_uid ();
	  SET_DECL_PT_UID (t, -1);
	}
      if (TREE_CODE (t) == LABEL_DECL)
	LABEL_DECL_UID (t) = -1;

      break;

    case tcc_type:
      TYPE_UID (t) = next_type_uid++;
      SET_TYPE_ALIGN (t, BITS_PER_UNIT);
      TYPE_USER_ALIGN (t) = 0;
      TYPE_MAIN_VARIANT (t) = t;
      TYPE_CANONICAL (t) = t;

      /* Default to no attributes for type, but let target change that.  */
      TYPE_ATTRIBUTES (t) = NULL_TREE;
      targetm.set_default_type_attributes (t);

      /* We have not yet computed the alias set for this type.  */
      TYPE_ALIAS_SET (t) = -1;
      break;

    case tcc_constant:
      TREE_CONSTANT (t) = 1;
      break;

    case tcc_expression:
      switch (code)
	{
	case INIT_EXPR:
	case MODIFY_EXPR:
	case VA_ARG_EXPR:
	case PREDECREMENT_EXPR:
	case PREINCREMENT_EXPR:
	case POSTDECREMENT_EXPR:
	case POSTINCREMENT_EXPR:
	  /* All of these have side-effects, no matter what their
	     operands are.  */
	  TREE_SIDE_EFFECTS (t) = 1;
	  break;

	default:
	  break;
	}
      break;

    case tcc_exceptional:
      switch (code)
        {
	case TARGET_OPTION_NODE:
	  TREE_TARGET_OPTION(t)
			    = ggc_cleared_alloc<struct cl_target_option> ();
	  break;

	case OPTIMIZATION_NODE:
	  TREE_OPTIMIZATION (t)
			    = ggc_cleared_alloc<struct cl_optimization> ();
	  break;

	default:
	  break;
	}
      break;

    default:
      /* Other classes need no special treatment.  */
      break;
    }

  return t;
}

/* Free tree node.  */

void
free_node (tree node)
{
  enum tree_code code = TREE_CODE (node);
  if (GATHER_STATISTICS)
    {
      tree_code_counts[(int) TREE_CODE (node)]--;
      tree_node_counts[(int) t_kind]--;
      tree_node_sizes[(int) t_kind] -= tree_size (node);
    }
  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    vec_free (CONSTRUCTOR_ELTS (node));
  else if (code == BLOCK)
    vec_free (BLOCK_NONLOCALIZED_VARS (node));
  else if (code == TREE_BINFO)
    vec_free (BINFO_BASE_ACCESSES (node));
  ggc_free (node);
}

/* Return a new node with the same contents as NODE except that its
   TREE_CHAIN, if it has one, is zero and it has a fresh uid.  */

tree
copy_node_stat (tree node MEM_STAT_DECL)
{
  tree t;
  enum tree_code code = TREE_CODE (node);
  size_t length;

  gcc_assert (code != STATEMENT_LIST);

  length = tree_size (node);
  record_node_allocation_statistics (code, length);
  t = ggc_alloc_tree_node_stat (length PASS_MEM_STAT);
  memcpy (t, node, length);

  if (CODE_CONTAINS_STRUCT (code, TS_COMMON))
    TREE_CHAIN (t) = 0;
  TREE_ASM_WRITTEN (t) = 0;
  TREE_VISITED (t) = 0;

  if (TREE_CODE_CLASS (code) == tcc_declaration)
    {
      if (code == DEBUG_EXPR_DECL)
	DECL_UID (t) = --next_debug_decl_uid;
      else
	{
	  DECL_UID (t) = allocate_decl_uid ();
	  if (DECL_PT_UID_SET_P (node))
	    SET_DECL_PT_UID (t, DECL_PT_UID (node));
	}
      if ((TREE_CODE (node) == PARM_DECL || TREE_CODE (node) == VAR_DECL)
	  && DECL_HAS_VALUE_EXPR_P (node))
	{
	  SET_DECL_VALUE_EXPR (t, DECL_VALUE_EXPR (node));
	  DECL_HAS_VALUE_EXPR_P (t) = 1;
	}
      /* DECL_DEBUG_EXPR is copied explicitely by callers.  */
      if (TREE_CODE (node) == VAR_DECL)
	{
	  DECL_HAS_DEBUG_EXPR_P (t) = 0;
	  t->decl_with_vis.symtab_node = NULL;
	}
      if (TREE_CODE (node) == VAR_DECL && DECL_HAS_INIT_PRIORITY_P (node))
	{
	  SET_DECL_INIT_PRIORITY (t, DECL_INIT_PRIORITY (node));
	  DECL_HAS_INIT_PRIORITY_P (t) = 1;
	}
      if (TREE_CODE (node) == FUNCTION_DECL)
	{
	  DECL_STRUCT_FUNCTION (t) = NULL;
	  t->decl_with_vis.symtab_node = NULL;
	}
    }
  else if (TREE_CODE_CLASS (code) == tcc_type)
    {
      TYPE_UID (t) = next_type_uid++;
      /* The following is so that the debug code for
	 the copy is different from the original type.
	 The two statements usually duplicate each other
	 (because they clear fields of the same union),
	 but the optimizer should catch that.  */
      TYPE_SYMTAB_POINTER (t) = 0;
      TYPE_SYMTAB_ADDRESS (t) = 0;

      /* Do not copy the values cache.  */
      if (TYPE_CACHED_VALUES_P (t))
	{
	  TYPE_CACHED_VALUES_P (t) = 0;
	  TYPE_CACHED_VALUES (t) = NULL_TREE;
	}
    }
    else if (code == TARGET_OPTION_NODE)
      {
	TREE_TARGET_OPTION (t) = ggc_alloc<struct cl_target_option>();
	memcpy (TREE_TARGET_OPTION (t), TREE_TARGET_OPTION (node),
		sizeof (struct cl_target_option));
      }
    else if (code == OPTIMIZATION_NODE)
      {
	TREE_OPTIMIZATION (t) = ggc_alloc<struct cl_optimization>();
	memcpy (TREE_OPTIMIZATION (t), TREE_OPTIMIZATION (node),
		sizeof (struct cl_optimization));
      }

  return t;
}

/* Return a copy of a chain of nodes, chained through the TREE_CHAIN field.
   For example, this can copy a list made of TREE_LIST nodes.  */

tree
copy_list (tree list)
{
  tree head;
  tree prev, next;

  if (list == 0)
    return 0;

  head = prev = copy_node (list);
  next = TREE_CHAIN (list);
  while (next)
    {
      TREE_CHAIN (prev) = copy_node (next);
      prev = TREE_CHAIN (prev);
      next = TREE_CHAIN (next);
    }
  return head;
}


/* Return the value that TREE_INT_CST_EXT_NUNITS should have for an
   INTEGER_CST with value CST and type TYPE.   */

static unsigned int
get_int_cst_ext_nunits (tree type, const wide_int &cst)
{
  gcc_checking_assert (cst.get_precision () == TYPE_PRECISION (type));
  /* We need extra HWIs if CST is an unsigned integer with its
     upper bit set.  */
  if (TYPE_UNSIGNED (type) && wi::neg_p (cst))
    return cst.get_precision () / HOST_BITS_PER_WIDE_INT + 1;
  return cst.get_len ();
}

/* Return a new INTEGER_CST with value CST and type TYPE.  */

static tree
build_new_int_cst (tree type, const wide_int &cst)
{
  unsigned int len = cst.get_len ();
  unsigned int ext_len = get_int_cst_ext_nunits (type, cst);
  tree nt = make_int_cst (len, ext_len);

  if (len < ext_len)
    {
      --ext_len;
      TREE_INT_CST_ELT (nt, ext_len)
	= zext_hwi (-1, cst.get_precision () % HOST_BITS_PER_WIDE_INT);
      for (unsigned int i = len; i < ext_len; ++i)
	TREE_INT_CST_ELT (nt, i) = -1;
    }
  else if (TYPE_UNSIGNED (type)
	   && cst.get_precision () < len * HOST_BITS_PER_WIDE_INT)
    {
      len--;
      TREE_INT_CST_ELT (nt, len)
	= zext_hwi (cst.elt (len),
		    cst.get_precision () % HOST_BITS_PER_WIDE_INT);
    }

  for (unsigned int i = 0; i < len; i++)
    TREE_INT_CST_ELT (nt, i) = cst.elt (i);
  TREE_TYPE (nt) = type;
  return nt;
}

/* Create an INT_CST node with a LOW value sign extended to TYPE.  */

tree
build_int_cst (tree type, HOST_WIDE_INT low)
{
  /* Support legacy code.  */
  if (!type)
    type = integer_type_node;

  return wide_int_to_tree (type, wi::shwi (low, TYPE_PRECISION (type)));
}

tree
build_int_cstu (tree type, unsigned HOST_WIDE_INT cst)
{
  return wide_int_to_tree (type, wi::uhwi (cst, TYPE_PRECISION (type)));
}

/* Create an INT_CST node with a LOW value sign extended to TYPE.  */

tree
build_int_cst_type (tree type, HOST_WIDE_INT low)
{
  gcc_assert (type);
  return wide_int_to_tree (type, wi::shwi (low, TYPE_PRECISION (type)));
}

/* Constructs tree in type TYPE from with value given by CST.  Signedness
   of CST is assumed to be the same as the signedness of TYPE.  */

tree
double_int_to_tree (tree type, double_int cst)
{
  return wide_int_to_tree (type, widest_int::from (cst, TYPE_SIGN (type)));
}

/* We force the wide_int CST to the range of the type TYPE by sign or
   zero extending it.  OVERFLOWABLE indicates if we are interested in
   overflow of the value, when >0 we are only interested in signed
   overflow, for <0 we are interested in any overflow.  OVERFLOWED
   indicates whether overflow has already occurred.  CONST_OVERFLOWED
   indicates whether constant overflow has already occurred.  We force
   T's value to be within range of T's type (by setting to 0 or 1 all
   the bits outside the type's range).  We set TREE_OVERFLOWED if,
        OVERFLOWED is nonzero,
        or OVERFLOWABLE is >0 and signed overflow occurs
        or OVERFLOWABLE is <0 and any overflow occurs
   We return a new tree node for the extended wide_int.  The node
   is shared if no overflow flags are set.  */


tree
force_fit_type (tree type, const wide_int_ref &cst,
		int overflowable, bool overflowed)
{
  signop sign = TYPE_SIGN (type);

  /* If we need to set overflow flags, return a new unshared node.  */
  if (overflowed || !wi::fits_to_tree_p (cst, type))
    {
      if (overflowed
	  || overflowable < 0
	  || (overflowable > 0 && sign == SIGNED))
	{
	  wide_int tmp = wide_int::from (cst, TYPE_PRECISION (type), sign);
	  tree t = build_new_int_cst (type, tmp);
	  TREE_OVERFLOW (t) = 1;
	  return t;
	}
    }

  /* Else build a shared node.  */
  return wide_int_to_tree (type, cst);
}

/* These are the hash table functions for the hash table of INTEGER_CST
   nodes of a sizetype.  */

/* Return the hash code X, an INTEGER_CST.  */

hashval_t
int_cst_hasher::hash (tree x)
{
  const_tree const t = x;
  hashval_t code = TYPE_UID (TREE_TYPE (t));
  int i;

  for (i = 0; i < TREE_INT_CST_NUNITS (t); i++)
    code = iterative_hash_host_wide_int (TREE_INT_CST_ELT(t, i), code);

  return code;
}

/* Return nonzero if the value represented by *X (an INTEGER_CST tree node)
   is the same as that given by *Y, which is the same.  */

bool
int_cst_hasher::equal (tree x, tree y)
{
  const_tree const xt = x;
  const_tree const yt = y;

  if (TREE_TYPE (xt) != TREE_TYPE (yt)
      || TREE_INT_CST_NUNITS (xt) != TREE_INT_CST_NUNITS (yt)
      || TREE_INT_CST_EXT_NUNITS (xt) != TREE_INT_CST_EXT_NUNITS (yt))
    return false;

  for (int i = 0; i < TREE_INT_CST_NUNITS (xt); i++)
    if (TREE_INT_CST_ELT (xt, i) != TREE_INT_CST_ELT (yt, i))
      return false;

  return true;
}

/* Create an INT_CST node of TYPE and value CST.
   The returned node is always shared.  For small integers we use a
   per-type vector cache, for larger ones we use a single hash table.
   The value is extended from its precision according to the sign of
   the type to be a multiple of HOST_BITS_PER_WIDE_INT.  This defines
   the upper bits and ensures that hashing and value equality based
   upon the underlying HOST_WIDE_INTs works without masking.  */

tree
wide_int_to_tree (tree type, const wide_int_ref &pcst)
{
  tree t;
  int ix = -1;
  int limit = 0;

  gcc_assert (type);
  unsigned int prec = TYPE_PRECISION (type);
  signop sgn = TYPE_SIGN (type);

  /* Verify that everything is canonical.  */
  int l = pcst.get_len ();
  if (l > 1)
    {
      if (pcst.elt (l - 1) == 0)
	gcc_checking_assert (pcst.elt (l - 2) < 0);
      if (pcst.elt (l - 1) == (HOST_WIDE_INT) -1)
	gcc_checking_assert (pcst.elt (l - 2) >= 0);
    }

  wide_int cst = wide_int::from (pcst, prec, sgn);
  unsigned int ext_len = get_int_cst_ext_nunits (type, cst);

  if (ext_len == 1)
    {
      /* We just need to store a single HOST_WIDE_INT.  */
      HOST_WIDE_INT hwi;
      if (TYPE_UNSIGNED (type))
	hwi = cst.to_uhwi ();
      else
	hwi = cst.to_shwi ();

      switch (TREE_CODE (type))
	{
	case NULLPTR_TYPE:
	  gcc_assert (hwi == 0);
	  /* Fallthru.  */

	case POINTER_TYPE:
	case REFERENCE_TYPE:
	case POINTER_BOUNDS_TYPE:
	  /* Cache NULL pointer and zero bounds.  */
	  if (hwi == 0)
	    {
	      limit = 1;
	      ix = 0;
	    }
	  break;

	case BOOLEAN_TYPE:
	  /* Cache false or true.  */
	  limit = 2;
	  if (IN_RANGE (hwi, 0, 1))
	    ix = hwi;
	  break;

	case INTEGER_TYPE:
	case OFFSET_TYPE:
	  if (TYPE_SIGN (type) == UNSIGNED)
	    {
	      /* Cache [0, N).  */
	      limit = INTEGER_SHARE_LIMIT;
	      if (IN_RANGE (hwi, 0, INTEGER_SHARE_LIMIT - 1))
		ix = hwi;
	    }
	  else
	    {
	      /* Cache [-1, N).  */
	      limit = INTEGER_SHARE_LIMIT + 1;
	      if (IN_RANGE (hwi, -1, INTEGER_SHARE_LIMIT - 1))
		ix = hwi + 1;
	    }
	  break;

	case ENUMERAL_TYPE:
	  break;

	default:
	  gcc_unreachable ();
	}

      if (ix >= 0)
	{
	  /* Look for it in the type's vector of small shared ints.  */
	  if (!TYPE_CACHED_VALUES_P (type))
	    {
	      TYPE_CACHED_VALUES_P (type) = 1;
	      TYPE_CACHED_VALUES (type) = make_tree_vec (limit);
	    }

	  t = TREE_VEC_ELT (TYPE_CACHED_VALUES (type), ix);
	  if (t)
	    /* Make sure no one is clobbering the shared constant.  */
	    gcc_checking_assert (TREE_TYPE (t) == type
				 && TREE_INT_CST_NUNITS (t) == 1
				 && TREE_INT_CST_OFFSET_NUNITS (t) == 1
				 && TREE_INT_CST_EXT_NUNITS (t) == 1
				 && TREE_INT_CST_ELT (t, 0) == hwi);
	  else
	    {
	      /* Create a new shared int.  */
	      t = build_new_int_cst (type, cst);
	      TREE_VEC_ELT (TYPE_CACHED_VALUES (type), ix) = t;
	    }
	}
      else
	{
	  /* Use the cache of larger shared ints, using int_cst_node as
	     a temporary.  */

	  TREE_INT_CST_ELT (int_cst_node, 0) = hwi;
	  TREE_TYPE (int_cst_node) = type;

	  tree *slot = int_cst_hash_table->find_slot (int_cst_node, INSERT);
	  t = *slot;
	  if (!t)
	    {
	      /* Insert this one into the hash table.  */
	      t = int_cst_node;
	      *slot = t;
	      /* Make a new node for next time round.  */
	      int_cst_node = make_int_cst (1, 1);
	    }
	}
    }
  else
    {
      /* The value either hashes properly or we drop it on the floor
	 for the gc to take care of.  There will not be enough of them
	 to worry about.  */

      tree nt = build_new_int_cst (type, cst);
      tree *slot = int_cst_hash_table->find_slot (nt, INSERT);
      t = *slot;
      if (!t)
	{
	  /* Insert this one into the hash table.  */
	  t = nt;
	  *slot = t;
	}
    }

  return t;
}

void
cache_integer_cst (tree t)
{
  tree type = TREE_TYPE (t);
  int ix = -1;
  int limit = 0;
  int prec = TYPE_PRECISION (type);

  gcc_assert (!TREE_OVERFLOW (t));

  switch (TREE_CODE (type))
    {
    case NULLPTR_TYPE:
      gcc_assert (integer_zerop (t));
      /* Fallthru.  */

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* Cache NULL pointer.  */
      if (integer_zerop (t))
	{
	  limit = 1;
	  ix = 0;
	}
      break;

    case BOOLEAN_TYPE:
      /* Cache false or true.  */
      limit = 2;
      if (wi::ltu_p (t, 2))
	ix = TREE_INT_CST_ELT (t, 0);
      break;

    case INTEGER_TYPE:
    case OFFSET_TYPE:
      if (TYPE_UNSIGNED (type))
	{
	  /* Cache 0..N */
	  limit = INTEGER_SHARE_LIMIT;

	  /* This is a little hokie, but if the prec is smaller than
	     what is necessary to hold INTEGER_SHARE_LIMIT, then the
	     obvious test will not get the correct answer.  */
	  if (prec < HOST_BITS_PER_WIDE_INT)
	    {
	      if (tree_to_uhwi (t) < (unsigned HOST_WIDE_INT) INTEGER_SHARE_LIMIT)
		ix = tree_to_uhwi (t);
	    }
	  else if (wi::ltu_p (t, INTEGER_SHARE_LIMIT))
	    ix = tree_to_uhwi (t);
	}
      else
	{
	  /* Cache -1..N */
	  limit = INTEGER_SHARE_LIMIT + 1;

	  if (integer_minus_onep (t))
	    ix = 0;
	  else if (!wi::neg_p (t))
	    {
	      if (prec < HOST_BITS_PER_WIDE_INT)
		{
		  if (tree_to_shwi (t) < INTEGER_SHARE_LIMIT)
		    ix = tree_to_shwi (t) + 1;
		}
	      else if (wi::ltu_p (t, INTEGER_SHARE_LIMIT))
		ix = tree_to_shwi (t) + 1;
	    }
	}
      break;

    case ENUMERAL_TYPE:
      break;

    default:
      gcc_unreachable ();
    }

  if (ix >= 0)
    {
      /* Look for it in the type's vector of small shared ints.  */
      if (!TYPE_CACHED_VALUES_P (type))
	{
	  TYPE_CACHED_VALUES_P (type) = 1;
	  TYPE_CACHED_VALUES (type) = make_tree_vec (limit);
	}

      gcc_assert (TREE_VEC_ELT (TYPE_CACHED_VALUES (type), ix) == NULL_TREE);
      TREE_VEC_ELT (TYPE_CACHED_VALUES (type), ix) = t;
    }
  else
    {
      /* Use the cache of larger shared ints.  */
      tree *slot = int_cst_hash_table->find_slot (t, INSERT);
      /* If there is already an entry for the number verify it's the
         same.  */
      if (*slot)
	gcc_assert (wi::eq_p (tree (*slot), t));
      else
	/* Otherwise insert this one into the hash table.  */
	*slot = t;
    }
}


/* Builds an integer constant in TYPE such that lowest BITS bits are ones
   and the rest are zeros.  */

tree
build_low_bits_mask (tree type, unsigned bits)
{
  gcc_assert (bits <= TYPE_PRECISION (type));

  return wide_int_to_tree (type, wi::mask (bits, false,
					   TYPE_PRECISION (type)));
}

/* Checks that X is integer constant that can be expressed in (unsigned)
   HOST_WIDE_INT without loss of precision.  */

bool
cst_and_fits_in_hwi (const_tree x)
{
  return (TREE_CODE (x) == INTEGER_CST
	  && TYPE_PRECISION (TREE_TYPE (x)) <= HOST_BITS_PER_WIDE_INT);
}

/* Build a newly constructed VECTOR_CST node of length LEN.  */

tree
make_vector_stat (unsigned len MEM_STAT_DECL)
{
  tree t;
  unsigned length = (len - 1) * sizeof (tree) + sizeof (struct tree_vector);

  record_node_allocation_statistics (VECTOR_CST, length);

  t = ggc_alloc_cleared_tree_node_stat (length PASS_MEM_STAT);

  TREE_SET_CODE (t, VECTOR_CST);
  TREE_CONSTANT (t) = 1;

  return t;
}

/* Return a new VECTOR_CST node whose type is TYPE and whose values
   are in a list pointed to by VALS.  */

tree
build_vector_stat (tree type, tree *vals MEM_STAT_DECL)
{
  int over = 0;
  unsigned cnt = 0;
  tree v = make_vector (TYPE_VECTOR_SUBPARTS (type));
  TREE_TYPE (v) = type;

  /* Iterate through elements and check for overflow.  */
  for (cnt = 0; cnt < TYPE_VECTOR_SUBPARTS (type); ++cnt)
    {
      tree value = vals[cnt];

      VECTOR_CST_ELT (v, cnt) = value;

      /* Don't crash if we get an address constant.  */
      if (!CONSTANT_CLASS_P (value))
	continue;

      over |= TREE_OVERFLOW (value);
    }

  TREE_OVERFLOW (v) = over;
  return v;
}

/* Return a new VECTOR_CST node whose type is TYPE and whose values
   are extracted from V, a vector of CONSTRUCTOR_ELT.  */

tree
build_vector_from_ctor (tree type, vec<constructor_elt, va_gc> *v)
{
  tree *vec = XALLOCAVEC (tree, TYPE_VECTOR_SUBPARTS (type));
  unsigned HOST_WIDE_INT idx, pos = 0;
  tree value;

  FOR_EACH_CONSTRUCTOR_VALUE (v, idx, value)
    {
      if (TREE_CODE (value) == VECTOR_CST)
	for (unsigned i = 0; i < VECTOR_CST_NELTS (value); ++i)
	  vec[pos++] = VECTOR_CST_ELT (value, i);
      else
	vec[pos++] = value;
    }
  while (pos < TYPE_VECTOR_SUBPARTS (type))
    vec[pos++] = build_zero_cst (TREE_TYPE (type));

  return build_vector (type, vec);
}

/* Build a vector of type VECTYPE where all the elements are SCs.  */
tree
build_vector_from_val (tree vectype, tree sc) 
{
  int i, nunits = TYPE_VECTOR_SUBPARTS (vectype);

  if (sc == error_mark_node)
    return sc;

  /* Verify that the vector type is suitable for SC.  Note that there
     is some inconsistency in the type-system with respect to restrict
     qualifications of pointers.  Vector types always have a main-variant
     element type and the qualification is applied to the vector-type.
     So TREE_TYPE (vector-type) does not return a properly qualified
     vector element-type.  */
  gcc_checking_assert (types_compatible_p (TYPE_MAIN_VARIANT (TREE_TYPE (sc)),
					   TREE_TYPE (vectype)));

  if (CONSTANT_CLASS_P (sc))
    {
      tree *v = XALLOCAVEC (tree, nunits);
      for (i = 0; i < nunits; ++i)
	v[i] = sc;
      return build_vector (vectype, v);
    }
  else
    {
      vec<constructor_elt, va_gc> *v;
      vec_alloc (v, nunits);
      for (i = 0; i < nunits; ++i)
	CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, sc);
      return build_constructor (vectype, v);
    }
}

/* Something has messed with the elements of CONSTRUCTOR C after it was built;
   calculate TREE_CONSTANT and TREE_SIDE_EFFECTS.  */

void
recompute_constructor_flags (tree c)
{
  unsigned int i;
  tree val;
  bool constant_p = true;
  bool side_effects_p = false;
  vec<constructor_elt, va_gc> *vals = CONSTRUCTOR_ELTS (c);

  FOR_EACH_CONSTRUCTOR_VALUE (vals, i, val)
    {
      /* Mostly ctors will have elts that don't have side-effects, so
	 the usual case is to scan all the elements.  Hence a single
	 loop for both const and side effects, rather than one loop
	 each (with early outs).  */
      if (!TREE_CONSTANT (val))
	constant_p = false;
      if (TREE_SIDE_EFFECTS (val))
	side_effects_p = true;
    }

  TREE_SIDE_EFFECTS (c) = side_effects_p;
  TREE_CONSTANT (c) = constant_p;
}

/* Make sure that TREE_CONSTANT and TREE_SIDE_EFFECTS are correct for
   CONSTRUCTOR C.  */

void
verify_constructor_flags (tree c)
{
  unsigned int i;
  tree val;
  bool constant_p = TREE_CONSTANT (c);
  bool side_effects_p = TREE_SIDE_EFFECTS (c);
  vec<constructor_elt, va_gc> *vals = CONSTRUCTOR_ELTS (c);

  FOR_EACH_CONSTRUCTOR_VALUE (vals, i, val)
    {
      if (constant_p && !TREE_CONSTANT (val))
	internal_error ("non-constant element in constant CONSTRUCTOR");
      if (!side_effects_p && TREE_SIDE_EFFECTS (val))
	internal_error ("side-effects element in no-side-effects CONSTRUCTOR");
    }
}

/* Return a new CONSTRUCTOR node whose type is TYPE and whose values
   are in the vec pointed to by VALS.  */
tree
build_constructor (tree type, vec<constructor_elt, va_gc> *vals)
{
  tree c = make_node (CONSTRUCTOR);

  TREE_TYPE (c) = type;
  CONSTRUCTOR_ELTS (c) = vals;

  recompute_constructor_flags (c);

  return c;
}

/* Build a CONSTRUCTOR node made of a single initializer, with the specified
   INDEX and VALUE.  */
tree
build_constructor_single (tree type, tree index, tree value)
{
  vec<constructor_elt, va_gc> *v;
  constructor_elt elt = {index, value};

  vec_alloc (v, 1);
  v->quick_push (elt);

  return build_constructor (type, v);
}


/* Return a new CONSTRUCTOR node whose type is TYPE and whose values
   are in a list pointed to by VALS.  */
tree
build_constructor_from_list (tree type, tree vals)
{
  tree t;
  vec<constructor_elt, va_gc> *v = NULL;

  if (vals)
    {
      vec_alloc (v, list_length (vals));
      for (t = vals; t; t = TREE_CHAIN (t))
	CONSTRUCTOR_APPEND_ELT (v, TREE_PURPOSE (t), TREE_VALUE (t));
    }

  return build_constructor (type, v);
}

/* Return a new CONSTRUCTOR node whose type is TYPE.  NELTS is the number
   of elements, provided as index/value pairs.  */

tree
build_constructor_va (tree type, int nelts, ...)
{
  vec<constructor_elt, va_gc> *v = NULL;
  va_list p;

  va_start (p, nelts);
  vec_alloc (v, nelts);
  while (nelts--)
    {
      tree index = va_arg (p, tree);
      tree value = va_arg (p, tree);
      CONSTRUCTOR_APPEND_ELT (v, index, value);
    }
  va_end (p);
  return build_constructor (type, v);
}

/* Return a new FIXED_CST node whose type is TYPE and value is F.  */

tree
build_fixed (tree type, FIXED_VALUE_TYPE f)
{
  tree v;
  FIXED_VALUE_TYPE *fp;

  v = make_node (FIXED_CST);
  fp = ggc_alloc<fixed_value> ();
  memcpy (fp, &f, sizeof (FIXED_VALUE_TYPE));

  TREE_TYPE (v) = type;
  TREE_FIXED_CST_PTR (v) = fp;
  return v;
}

/* Return a new REAL_CST node whose type is TYPE and value is D.  */

tree
build_real (tree type, REAL_VALUE_TYPE d)
{
  tree v;
  REAL_VALUE_TYPE *dp;
  int overflow = 0;

  /* ??? Used to check for overflow here via CHECK_FLOAT_TYPE.
     Consider doing it via real_convert now.  */

  v = make_node (REAL_CST);
  dp = ggc_alloc<real_value> ();
  memcpy (dp, &d, sizeof (REAL_VALUE_TYPE));

  TREE_TYPE (v) = type;
  TREE_REAL_CST_PTR (v) = dp;
  TREE_OVERFLOW (v) = overflow;
  return v;
}

/* Like build_real, but first truncate D to the type.  */

tree
build_real_truncate (tree type, REAL_VALUE_TYPE d)
{
  return build_real (type, real_value_truncate (TYPE_MODE (type), d));
}

/* Return a new REAL_CST node whose type is TYPE
   and whose value is the integer value of the INTEGER_CST node I.  */

REAL_VALUE_TYPE
real_value_from_int_cst (const_tree type, const_tree i)
{
  REAL_VALUE_TYPE d;

  /* Clear all bits of the real value type so that we can later do
     bitwise comparisons to see if two values are the same.  */
  memset (&d, 0, sizeof d);

  real_from_integer (&d, type ? TYPE_MODE (type) : VOIDmode, i,
		     TYPE_SIGN (TREE_TYPE (i)));
  return d;
}

/* Given a tree representing an integer constant I, return a tree
   representing the same value as a floating-point constant of type TYPE.  */

tree
build_real_from_int_cst (tree type, const_tree i)
{
  tree v;
  int overflow = TREE_OVERFLOW (i);

  v = build_real (type, real_value_from_int_cst (type, i));

  TREE_OVERFLOW (v) |= overflow;
  return v;
}

/* Return a newly constructed STRING_CST node whose value is
   the LEN characters at STR.
   Note that for a C string literal, LEN should include the trailing NUL.
   The TREE_TYPE is not initialized.  */

tree
build_string (int len, const char *str)
{
  tree s;
  size_t length;

  /* Do not waste bytes provided by padding of struct tree_string.  */
  length = len + offsetof (struct tree_string, str) + 1;

  record_node_allocation_statistics (STRING_CST, length);

  s = (tree) ggc_internal_alloc (length);

  memset (s, 0, sizeof (struct tree_typed));
  TREE_SET_CODE (s, STRING_CST);
  TREE_CONSTANT (s) = 1;
  TREE_STRING_LENGTH (s) = len;
  memcpy (s->string.str, str, len);
  s->string.str[len] = '\0';

  return s;
}

/* Return a newly constructed COMPLEX_CST node whose value is
   specified by the real and imaginary parts REAL and IMAG.
   Both REAL and IMAG should be constant nodes.  TYPE, if specified,
   will be the type of the COMPLEX_CST; otherwise a new type will be made.  */

tree
build_complex (tree type, tree real, tree imag)
{
  tree t = make_node (COMPLEX_CST);

  TREE_REALPART (t) = real;
  TREE_IMAGPART (t) = imag;
  TREE_TYPE (t) = type ? type : build_complex_type (TREE_TYPE (real));
  TREE_OVERFLOW (t) = TREE_OVERFLOW (real) | TREE_OVERFLOW (imag);
  return t;
}

/* Build a complex (inf +- 0i), such as for the result of cproj.
   TYPE is the complex tree type of the result.  If NEG is true, the
   imaginary zero is negative.  */

tree
build_complex_inf (tree type, bool neg)
{
  REAL_VALUE_TYPE rinf, rzero = dconst0;

  real_inf (&rinf);
  rzero.sign = neg;
  return build_complex (type, build_real (TREE_TYPE (type), rinf),
			build_real (TREE_TYPE (type), rzero));
}

/* Return the constant 1 in type TYPE.  If TYPE has several elements, each
   element is set to 1.  In particular, this is 1 + i for complex types.  */

tree
build_each_one_cst (tree type)
{
  if (TREE_CODE (type) == COMPLEX_TYPE)
    {
      tree scalar = build_one_cst (TREE_TYPE (type));
      return build_complex (type, scalar, scalar);
    }
  else
    return build_one_cst (type);
}

/* Return a constant of arithmetic type TYPE which is the
   multiplicative identity of the set TYPE.  */

tree
build_one_cst (tree type)
{
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE: case ENUMERAL_TYPE: case BOOLEAN_TYPE:
    case POINTER_TYPE: case REFERENCE_TYPE:
    case OFFSET_TYPE:
      return build_int_cst (type, 1);

    case REAL_TYPE:
      return build_real (type, dconst1);

    case FIXED_POINT_TYPE:
      /* We can only generate 1 for accum types.  */
      gcc_assert (ALL_SCALAR_ACCUM_MODE_P (TYPE_MODE (type)));
      return build_fixed (type, FCONST1 (TYPE_MODE (type)));

    case VECTOR_TYPE:
      {
	tree scalar = build_one_cst (TREE_TYPE (type));

	return build_vector_from_val (type, scalar);
      }

    case COMPLEX_TYPE:
      return build_complex (type,
			    build_one_cst (TREE_TYPE (type)),
			    build_zero_cst (TREE_TYPE (type)));

    default:
      gcc_unreachable ();
    }
}

/* Return an integer of type TYPE containing all 1's in as much precision as
   it contains, or a complex or vector whose subparts are such integers.  */

tree
build_all_ones_cst (tree type)
{
  if (TREE_CODE (type) == COMPLEX_TYPE)
    {
      tree scalar = build_all_ones_cst (TREE_TYPE (type));
      return build_complex (type, scalar, scalar);
    }
  else
    return build_minus_one_cst (type);
}

/* Return a constant of arithmetic type TYPE which is the
   opposite of the multiplicative identity of the set TYPE.  */

tree
build_minus_one_cst (tree type)
{
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE: case ENUMERAL_TYPE: case BOOLEAN_TYPE:
    case POINTER_TYPE: case REFERENCE_TYPE:
    case OFFSET_TYPE:
      return build_int_cst (type, -1);

    case REAL_TYPE:
      return build_real (type, dconstm1);

    case FIXED_POINT_TYPE:
      /* We can only generate 1 for accum types.  */
      gcc_assert (ALL_SCALAR_ACCUM_MODE_P (TYPE_MODE (type)));
      return build_fixed (type, fixed_from_double_int (double_int_minus_one,
						       TYPE_MODE (type)));

    case VECTOR_TYPE:
      {
	tree scalar = build_minus_one_cst (TREE_TYPE (type));

	return build_vector_from_val (type, scalar);
      }

    case COMPLEX_TYPE:
      return build_complex (type,
			    build_minus_one_cst (TREE_TYPE (type)),
			    build_zero_cst (TREE_TYPE (type)));

    default:
      gcc_unreachable ();
    }
}

/* Build 0 constant of type TYPE.  This is used by constructor folding
   and thus the constant should be represented in memory by
   zero(es).  */

tree
build_zero_cst (tree type)
{
  switch (TREE_CODE (type))
    {
    case INTEGER_TYPE: case ENUMERAL_TYPE: case BOOLEAN_TYPE:
    case POINTER_TYPE: case REFERENCE_TYPE:
    case OFFSET_TYPE: case NULLPTR_TYPE:
      return build_int_cst (type, 0);

    case REAL_TYPE:
      return build_real (type, dconst0);

    case FIXED_POINT_TYPE:
      return build_fixed (type, FCONST0 (TYPE_MODE (type)));

    case VECTOR_TYPE:
      {
	tree scalar = build_zero_cst (TREE_TYPE (type));

	return build_vector_from_val (type, scalar);
      }

    case COMPLEX_TYPE:
      {
	tree zero = build_zero_cst (TREE_TYPE (type));

	return build_complex (type, zero, zero);
      }

    default:
      if (!AGGREGATE_TYPE_P (type))
	return fold_convert (type, integer_zero_node);
      return build_constructor (type, NULL);
    }
}


/* Build a BINFO with LEN language slots.  */

tree
make_tree_binfo_stat (unsigned base_binfos MEM_STAT_DECL)
{
  tree t;
  size_t length = (offsetof (struct tree_binfo, base_binfos)
		   + vec<tree, va_gc>::embedded_size (base_binfos));

  record_node_allocation_statistics (TREE_BINFO, length);

  t = ggc_alloc_tree_node_stat (length PASS_MEM_STAT);

  memset (t, 0, offsetof (struct tree_binfo, base_binfos));

  TREE_SET_CODE (t, TREE_BINFO);

  BINFO_BASE_BINFOS (t)->embedded_init (base_binfos);

  return t;
}

/* Create a CASE_LABEL_EXPR tree node and return it.  */

tree
build_case_label (tree low_value, tree high_value, tree label_decl)
{
  tree t = make_node (CASE_LABEL_EXPR);

  TREE_TYPE (t) = void_type_node;
  SET_EXPR_LOCATION (t, DECL_SOURCE_LOCATION (label_decl));

  CASE_LOW (t) = low_value;
  CASE_HIGH (t) = high_value;
  CASE_LABEL (t) = label_decl;
  CASE_CHAIN (t) = NULL_TREE;

  return t;
}

/* Build a newly constructed INTEGER_CST node.  LEN and EXT_LEN are the
   values of TREE_INT_CST_NUNITS and TREE_INT_CST_EXT_NUNITS respectively.
   The latter determines the length of the HOST_WIDE_INT vector.  */

tree
make_int_cst_stat (int len, int ext_len MEM_STAT_DECL)
{
  tree t;
  int length = ((ext_len - 1) * sizeof (HOST_WIDE_INT)
		+ sizeof (struct tree_int_cst));

  gcc_assert (len);
  record_node_allocation_statistics (INTEGER_CST, length);

  t = ggc_alloc_cleared_tree_node_stat (length PASS_MEM_STAT);

  TREE_SET_CODE (t, INTEGER_CST);
  TREE_INT_CST_NUNITS (t) = len;
  TREE_INT_CST_EXT_NUNITS (t) = ext_len;
  /* to_offset can only be applied to trees that are offset_int-sized
     or smaller.  EXT_LEN is correct if it fits, otherwise the constant
     must be exactly the precision of offset_int and so LEN is correct.  */
  if (ext_len <= OFFSET_INT_ELTS)
    TREE_INT_CST_OFFSET_NUNITS (t) = ext_len;
  else
    TREE_INT_CST_OFFSET_NUNITS (t) = len;

  TREE_CONSTANT (t) = 1;

  return t;
}

/* Build a newly constructed TREE_VEC node of length LEN.  */

tree
make_tree_vec_stat (int len MEM_STAT_DECL)
{
  tree t;
  int length = (len - 1) * sizeof (tree) + sizeof (struct tree_vec);

  record_node_allocation_statistics (TREE_VEC, length);

  t = ggc_alloc_cleared_tree_node_stat (length PASS_MEM_STAT);

  TREE_SET_CODE (t, TREE_VEC);
  TREE_VEC_LENGTH (t) = len;

  return t;
}

/* Grow a TREE_VEC node to new length LEN.  */

tree
grow_tree_vec_stat (tree v, int len MEM_STAT_DECL)
{
  gcc_assert (TREE_CODE (v) == TREE_VEC);

  int oldlen = TREE_VEC_LENGTH (v);
  gcc_assert (len > oldlen);

  int oldlength = (oldlen - 1) * sizeof (tree) + sizeof (struct tree_vec);
  int length = (len - 1) * sizeof (tree) + sizeof (struct tree_vec);

  record_node_allocation_statistics (TREE_VEC, length - oldlength);

  v = (tree) ggc_realloc (v, length PASS_MEM_STAT);

  TREE_VEC_LENGTH (v) = len;

  return v;
}

/* Return 1 if EXPR is the constant zero, whether it is integral, float or
   fixed, and scalar, complex or vector.  */

int
zerop (const_tree expr)
{
  return (integer_zerop (expr)
	  || real_zerop (expr)
	  || fixed_zerop (expr));
}

/* Return 1 if EXPR is the integer constant zero or a complex constant
   of zero.  */

int
integer_zerop (const_tree expr)
{
  switch (TREE_CODE (expr))
    {
    case INTEGER_CST:
      return wi::eq_p (expr, 0);
    case COMPLEX_CST:
      return (integer_zerop (TREE_REALPART (expr))
	      && integer_zerop (TREE_IMAGPART (expr)));
    case VECTOR_CST:
      {
	unsigned i;
	for (i = 0; i < VECTOR_CST_NELTS (expr); ++i)
	  if (!integer_zerop (VECTOR_CST_ELT (expr, i)))
	    return false;
	return true;
      }
    default:
      return false;
    }
}

/* Return 1 if EXPR is the integer constant one or the corresponding
   complex constant.  */

int
integer_onep (const_tree expr)
{
  switch (TREE_CODE (expr))
    {
    case INTEGER_CST:
      return wi::eq_p (wi::to_widest (expr), 1);
    case COMPLEX_CST:
      return (integer_onep (TREE_REALPART (expr))
	      && integer_zerop (TREE_IMAGPART (expr)));
    case VECTOR_CST:
      {
	unsigned i;
	for (i = 0; i < VECTOR_CST_NELTS (expr); ++i)
	  if (!integer_onep (VECTOR_CST_ELT (expr, i)))
	    return false;
	return true;
      }
    default:
      return false;
    }
}

/* Return 1 if EXPR is the integer constant one.  For complex and vector,
   return 1 if every piece is the integer constant one.  */

int
integer_each_onep (const_tree expr)
{
  if (TREE_CODE (expr) == COMPLEX_CST)
    return (integer_onep (TREE_REALPART (expr))
	    && integer_onep (TREE_IMAGPART (expr)));
  else
    return integer_onep (expr);
}

/* Return 1 if EXPR is an integer containing all 1's in as much precision as
   it contains, or a complex or vector whose subparts are such integers.  */

int
integer_all_onesp (const_tree expr)
{
  if (TREE_CODE (expr) == COMPLEX_CST
      && integer_all_onesp (TREE_REALPART (expr))
      && integer_all_onesp (TREE_IMAGPART (expr)))
    return 1;

  else if (TREE_CODE (expr) == VECTOR_CST)
    {
      unsigned i;
      for (i = 0; i < VECTOR_CST_NELTS (expr); ++i)
	if (!integer_all_onesp (VECTOR_CST_ELT (expr, i)))
	  return 0;
      return 1;
    }

  else if (TREE_CODE (expr) != INTEGER_CST)
    return 0;

  return wi::max_value (TYPE_PRECISION (TREE_TYPE (expr)), UNSIGNED) == expr;
}

/* Return 1 if EXPR is the integer constant minus one.  */

int
integer_minus_onep (const_tree expr)
{
  if (TREE_CODE (expr) == COMPLEX_CST)
    return (integer_all_onesp (TREE_REALPART (expr))
	    && integer_zerop (TREE_IMAGPART (expr)));
  else
    return integer_all_onesp (expr);
}

/* Return 1 if EXPR is an integer constant that is a power of 2 (i.e., has only
   one bit on).  */

int
integer_pow2p (const_tree expr)
{
  if (TREE_CODE (expr) == COMPLEX_CST
      && integer_pow2p (TREE_REALPART (expr))
      && integer_zerop (TREE_IMAGPART (expr)))
    return 1;

  if (TREE_CODE (expr) != INTEGER_CST)
    return 0;

  return wi::popcount (expr) == 1;
}

/* Return 1 if EXPR is an integer constant other than zero or a
   complex constant other than zero.  */

int
integer_nonzerop (const_tree expr)
{
  return ((TREE_CODE (expr) == INTEGER_CST
	   && !wi::eq_p (expr, 0))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && (integer_nonzerop (TREE_REALPART (expr))
		  || integer_nonzerop (TREE_IMAGPART (expr)))));
}

/* Return 1 if EXPR is the integer constant one.  For vector,
   return 1 if every piece is the integer constant minus one
   (representing the value TRUE).  */

int
integer_truep (const_tree expr)
{
  if (TREE_CODE (expr) == VECTOR_CST)
    return integer_all_onesp (expr);
  return integer_onep (expr);
}

/* Return 1 if EXPR is the fixed-point constant zero.  */

int
fixed_zerop (const_tree expr)
{
  return (TREE_CODE (expr) == FIXED_CST
	  && TREE_FIXED_CST (expr).data.is_zero ());
}

/* Return the power of two represented by a tree node known to be a
   power of two.  */

int
tree_log2 (const_tree expr)
{
  if (TREE_CODE (expr) == COMPLEX_CST)
    return tree_log2 (TREE_REALPART (expr));

  return wi::exact_log2 (expr);
}

/* Similar, but return the largest integer Y such that 2 ** Y is less
   than or equal to EXPR.  */

int
tree_floor_log2 (const_tree expr)
{
  if (TREE_CODE (expr) == COMPLEX_CST)
    return tree_log2 (TREE_REALPART (expr));

  return wi::floor_log2 (expr);
}

/* Return number of known trailing zero bits in EXPR, or, if the value of
   EXPR is known to be zero, the precision of it's type.  */

unsigned int
tree_ctz (const_tree expr)
{
  if (!INTEGRAL_TYPE_P (TREE_TYPE (expr))
      && !POINTER_TYPE_P (TREE_TYPE (expr)))
    return 0;

  unsigned int ret1, ret2, prec = TYPE_PRECISION (TREE_TYPE (expr));
  switch (TREE_CODE (expr))
    {
    case INTEGER_CST:
      ret1 = wi::ctz (expr);
      return MIN (ret1, prec);
    case SSA_NAME:
      ret1 = wi::ctz (get_nonzero_bits (expr));
      return MIN (ret1, prec);
    case PLUS_EXPR:
    case MINUS_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
      ret1 = tree_ctz (TREE_OPERAND (expr, 0));
      if (ret1 == 0)
	return ret1;
      ret2 = tree_ctz (TREE_OPERAND (expr, 1));
      return MIN (ret1, ret2);
    case POINTER_PLUS_EXPR:
      ret1 = tree_ctz (TREE_OPERAND (expr, 0));
      ret2 = tree_ctz (TREE_OPERAND (expr, 1));
      /* Second operand is sizetype, which could be in theory
	 wider than pointer's precision.  Make sure we never
	 return more than prec.  */
      ret2 = MIN (ret2, prec);
      return MIN (ret1, ret2);
    case BIT_AND_EXPR:
      ret1 = tree_ctz (TREE_OPERAND (expr, 0));
      ret2 = tree_ctz (TREE_OPERAND (expr, 1));
      return MAX (ret1, ret2);
    case MULT_EXPR:
      ret1 = tree_ctz (TREE_OPERAND (expr, 0));
      ret2 = tree_ctz (TREE_OPERAND (expr, 1));
      return MIN (ret1 + ret2, prec);
    case LSHIFT_EXPR:
      ret1 = tree_ctz (TREE_OPERAND (expr, 0));
      if (tree_fits_uhwi_p (TREE_OPERAND (expr, 1))
	  && (tree_to_uhwi (TREE_OPERAND (expr, 1)) < prec))
	{
	  ret2 = tree_to_uhwi (TREE_OPERAND (expr, 1));
	  return MIN (ret1 + ret2, prec);
	}
      return ret1;
    case RSHIFT_EXPR:
      if (tree_fits_uhwi_p (TREE_OPERAND (expr, 1))
	  && (tree_to_uhwi (TREE_OPERAND (expr, 1)) < prec))
	{
	  ret1 = tree_ctz (TREE_OPERAND (expr, 0));
	  ret2 = tree_to_uhwi (TREE_OPERAND (expr, 1));
	  if (ret1 > ret2)
	    return ret1 - ret2;
	}
      return 0;
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if (TREE_CODE (TREE_OPERAND (expr, 1)) == INTEGER_CST
	  && tree_int_cst_sgn (TREE_OPERAND (expr, 1)) == 1)
	{
	  int l = tree_log2 (TREE_OPERAND (expr, 1));
	  if (l >= 0)
	    {
	      ret1 = tree_ctz (TREE_OPERAND (expr, 0));
	      ret2 = l;
	      if (ret1 > ret2)
		return ret1 - ret2;
	    }
	}
      return 0;
    CASE_CONVERT:
      ret1 = tree_ctz (TREE_OPERAND (expr, 0));
      if (ret1 && ret1 == TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (expr, 0))))
	ret1 = prec;
      return MIN (ret1, prec);
    case SAVE_EXPR:
      return tree_ctz (TREE_OPERAND (expr, 0));
    case COND_EXPR:
      ret1 = tree_ctz (TREE_OPERAND (expr, 1));
      if (ret1 == 0)
	return 0;
      ret2 = tree_ctz (TREE_OPERAND (expr, 2));
      return MIN (ret1, ret2);
    case COMPOUND_EXPR:
      return tree_ctz (TREE_OPERAND (expr, 1));
    case ADDR_EXPR:
      ret1 = get_pointer_alignment (CONST_CAST_TREE (expr));
      if (ret1 > BITS_PER_UNIT)
	{
	  ret1 = ctz_hwi (ret1 / BITS_PER_UNIT);
	  return MIN (ret1, prec);
	}
      return 0;
    default:
      return 0;
    }
}

/* Return 1 if EXPR is the real constant zero.  Trailing zeroes matter for
   decimal float constants, so don't return 1 for them.  */

int
real_zerop (const_tree expr)
{
  switch (TREE_CODE (expr))
    {
    case REAL_CST:
      return real_equal (&TREE_REAL_CST (expr), &dconst0)
	     && !(DECIMAL_FLOAT_MODE_P (TYPE_MODE (TREE_TYPE (expr))));
    case COMPLEX_CST:
      return real_zerop (TREE_REALPART (expr))
	     && real_zerop (TREE_IMAGPART (expr));
    case VECTOR_CST:
      {
	unsigned i;
	for (i = 0; i < VECTOR_CST_NELTS (expr); ++i)
	  if (!real_zerop (VECTOR_CST_ELT (expr, i)))
	    return false;
	return true;
      }
    default:
      return false;
    }
}

/* Return 1 if EXPR is the real constant one in real or complex form.
   Trailing zeroes matter for decimal float constants, so don't return
   1 for them.  */

int
real_onep (const_tree expr)
{
  switch (TREE_CODE (expr))
    {
    case REAL_CST:
      return real_equal (&TREE_REAL_CST (expr), &dconst1)
	     && !(DECIMAL_FLOAT_MODE_P (TYPE_MODE (TREE_TYPE (expr))));
    case COMPLEX_CST:
      return real_onep (TREE_REALPART (expr))
	     && real_zerop (TREE_IMAGPART (expr));
    case VECTOR_CST:
      {
	unsigned i;
	for (i = 0; i < VECTOR_CST_NELTS (expr); ++i)
	  if (!real_onep (VECTOR_CST_ELT (expr, i)))
	    return false;
	return true;
      }
    default:
      return false;
    }
}

/* Return 1 if EXPR is the real constant minus one.  Trailing zeroes
   matter for decimal float constants, so don't return 1 for them.  */

int
real_minus_onep (const_tree expr)
{
  switch (TREE_CODE (expr))
    {
    case REAL_CST:
      return real_equal (&TREE_REAL_CST (expr), &dconstm1)
	     && !(DECIMAL_FLOAT_MODE_P (TYPE_MODE (TREE_TYPE (expr))));
    case COMPLEX_CST:
      return real_minus_onep (TREE_REALPART (expr))
	     && real_zerop (TREE_IMAGPART (expr));
    case VECTOR_CST:
      {
	unsigned i;
	for (i = 0; i < VECTOR_CST_NELTS (expr); ++i)
	  if (!real_minus_onep (VECTOR_CST_ELT (expr, i)))
	    return false;
	return true;
      }
    default:
      return false;
    }
}

/* Nonzero if EXP is a constant or a cast of a constant.  */

int
really_constant_p (const_tree exp)
{
  /* This is not quite the same as STRIP_NOPS.  It does more.  */
  while (CONVERT_EXPR_P (exp)
	 || TREE_CODE (exp) == NON_LVALUE_EXPR)
    exp = TREE_OPERAND (exp, 0);
  return TREE_CONSTANT (exp);
}

/* Return first list element whose TREE_VALUE is ELEM.
   Return 0 if ELEM is not in LIST.  */

tree
value_member (tree elem, tree list)
{
  while (list)
    {
      if (elem == TREE_VALUE (list))
	return list;
      list = TREE_CHAIN (list);
    }
  return NULL_TREE;
}

/* Return first list element whose TREE_PURPOSE is ELEM.
   Return 0 if ELEM is not in LIST.  */

tree
purpose_member (const_tree elem, tree list)
{
  while (list)
    {
      if (elem == TREE_PURPOSE (list))
	return list;
      list = TREE_CHAIN (list);
    }
  return NULL_TREE;
}

/* Return true if ELEM is in V.  */

bool
vec_member (const_tree elem, vec<tree, va_gc> *v)
{
  unsigned ix;
  tree t;
  FOR_EACH_VEC_SAFE_ELT (v, ix, t)
    if (elem == t)
      return true;
  return false;
}

/* Returns element number IDX (zero-origin) of chain CHAIN, or
   NULL_TREE.  */

tree
chain_index (int idx, tree chain)
{
  for (; chain && idx > 0; --idx)
    chain = TREE_CHAIN (chain);
  return chain;
}

/* Return nonzero if ELEM is part of the chain CHAIN.  */

int
chain_member (const_tree elem, const_tree chain)
{
  while (chain)
    {
      if (elem == chain)
	return 1;
      chain = DECL_CHAIN (chain);
    }

  return 0;
}

/* Return the length of a chain of nodes chained through TREE_CHAIN.
   We expect a null pointer to mark the end of the chain.
   This is the Lisp primitive `length'.  */

int
list_length (const_tree t)
{
  const_tree p = t;
#ifdef ENABLE_TREE_CHECKING
  const_tree q = t;
#endif
  int len = 0;

  while (p)
    {
      p = TREE_CHAIN (p);
#ifdef ENABLE_TREE_CHECKING
      if (len % 2)
	q = TREE_CHAIN (q);
      gcc_assert (p != q);
#endif
      len++;
    }

  return len;
}

/* Returns the first FIELD_DECL in the TYPE_FIELDS of the RECORD_TYPE or
   UNION_TYPE TYPE, or NULL_TREE if none.  */

tree
first_field (const_tree type)
{
  tree t = TYPE_FIELDS (type);
  while (t && TREE_CODE (t) != FIELD_DECL)
    t = TREE_CHAIN (t);
  return t;
}

/* Concatenate two chains of nodes (chained through TREE_CHAIN)
   by modifying the last node in chain 1 to point to chain 2.
   This is the Lisp primitive `nconc'.  */

tree
chainon (tree op1, tree op2)
{
  tree t1;

  if (!op1)
    return op2;
  if (!op2)
    return op1;

  for (t1 = op1; TREE_CHAIN (t1); t1 = TREE_CHAIN (t1))
    continue;
  TREE_CHAIN (t1) = op2;

#ifdef ENABLE_TREE_CHECKING
  {
    tree t2;
    for (t2 = op2; t2; t2 = TREE_CHAIN (t2))
      gcc_assert (t2 != t1);
  }
#endif

  return op1;
}

/* Return the last node in a chain of nodes (chained through TREE_CHAIN).  */

tree
tree_last (tree chain)
{
  tree next;
  if (chain)
    while ((next = TREE_CHAIN (chain)))
      chain = next;
  return chain;
}

/* Reverse the order of elements in the chain T,
   and return the new head of the chain (old last element).  */

tree
nreverse (tree t)
{
  tree prev = 0, decl, next;
  for (decl = t; decl; decl = next)
    {
      /* We shouldn't be using this function to reverse BLOCK chains; we
	 have blocks_nreverse for that.  */
      gcc_checking_assert (TREE_CODE (decl) != BLOCK);
      next = TREE_CHAIN (decl);
      TREE_CHAIN (decl) = prev;
      prev = decl;
    }
  return prev;
}

/* Return a newly created TREE_LIST node whose
   purpose and value fields are PARM and VALUE.  */

tree
build_tree_list_stat (tree parm, tree value MEM_STAT_DECL)
{
  tree t = make_node_stat (TREE_LIST PASS_MEM_STAT);
  TREE_PURPOSE (t) = parm;
  TREE_VALUE (t) = value;
  return t;
}

/* Build a chain of TREE_LIST nodes from a vector.  */

tree
build_tree_list_vec_stat (const vec<tree, va_gc> *vec MEM_STAT_DECL)
{
  tree ret = NULL_TREE;
  tree *pp = &ret;
  unsigned int i;
  tree t;
  FOR_EACH_VEC_SAFE_ELT (vec, i, t)
    {
      *pp = build_tree_list_stat (NULL, t PASS_MEM_STAT);
      pp = &TREE_CHAIN (*pp);
    }
  return ret;
}

/* Return a newly created TREE_LIST node whose
   purpose and value fields are PURPOSE and VALUE
   and whose TREE_CHAIN is CHAIN.  */

tree 
tree_cons_stat (tree purpose, tree value, tree chain MEM_STAT_DECL)
{
  tree node;

  node = ggc_alloc_tree_node_stat (sizeof (struct tree_list) PASS_MEM_STAT);
  memset (node, 0, sizeof (struct tree_common));

  record_node_allocation_statistics (TREE_LIST, sizeof (struct tree_list));

  TREE_SET_CODE (node, TREE_LIST);
  TREE_CHAIN (node) = chain;
  TREE_PURPOSE (node) = purpose;
  TREE_VALUE (node) = value;
  return node;
}

/* Return the values of the elements of a CONSTRUCTOR as a vector of
   trees.  */

vec<tree, va_gc> *
ctor_to_vec (tree ctor)
{
  vec<tree, va_gc> *vec;
  vec_alloc (vec, CONSTRUCTOR_NELTS (ctor));
  unsigned int ix;
  tree val;

  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (ctor), ix, val)
    vec->quick_push (val);

  return vec;
}

/* Return the size nominally occupied by an object of type TYPE
   when it resides in memory.  The value is measured in units of bytes,
   and its data type is that normally used for type sizes
   (which is the first type created by make_signed_type or
   make_unsigned_type).  */

tree
size_in_bytes_loc (location_t loc, const_tree type)
{
  tree t;

  if (type == error_mark_node)
    return integer_zero_node;

  type = TYPE_MAIN_VARIANT (type);
  t = TYPE_SIZE_UNIT (type);

  if (t == 0)
    {
      lang_hooks.types.incomplete_type_error (loc, NULL_TREE, type);
      return size_zero_node;
    }

  return t;
}

/* Return the size of TYPE (in bytes) as a wide integer
   or return -1 if the size can vary or is larger than an integer.  */

HOST_WIDE_INT
int_size_in_bytes (const_tree type)
{
  tree t;

  if (type == error_mark_node)
    return 0;

  type = TYPE_MAIN_VARIANT (type);
  t = TYPE_SIZE_UNIT (type);

  if (t && tree_fits_uhwi_p (t))
    return TREE_INT_CST_LOW (t);
  else
    return -1;
}

/* Return the maximum size of TYPE (in bytes) as a wide integer
   or return -1 if the size can vary or is larger than an integer.  */

HOST_WIDE_INT
max_int_size_in_bytes (const_tree type)
{
  HOST_WIDE_INT size = -1;
  tree size_tree;

  /* If this is an array type, check for a possible MAX_SIZE attached.  */

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      size_tree = TYPE_ARRAY_MAX_SIZE (type);

      if (size_tree && tree_fits_uhwi_p (size_tree))
	size = tree_to_uhwi (size_tree);
    }

  /* If we still haven't been able to get a size, see if the language
     can compute a maximum size.  */

  if (size == -1)
    {
      size_tree = lang_hooks.types.max_size (type);

      if (size_tree && tree_fits_uhwi_p (size_tree))
	size = tree_to_uhwi (size_tree);
    }

  return size;
}

/* Return the bit position of FIELD, in bits from the start of the record.
   This is a tree of type bitsizetype.  */

tree
bit_position (const_tree field)
{
  return bit_from_pos (DECL_FIELD_OFFSET (field),
		       DECL_FIELD_BIT_OFFSET (field));
}

/* Return the byte position of FIELD, in bytes from the start of the record.
   This is a tree of type sizetype.  */

tree
byte_position (const_tree field)
{
  return byte_from_pos (DECL_FIELD_OFFSET (field),
			DECL_FIELD_BIT_OFFSET (field));
}

/* Likewise, but return as an integer.  It must be representable in
   that way (since it could be a signed value, we don't have the
   option of returning -1 like int_size_in_byte can.  */

HOST_WIDE_INT
int_byte_position (const_tree field)
{
  return tree_to_shwi (byte_position (field));
}

/* Return the strictest alignment, in bits, that T is known to have.  */

unsigned int
expr_align (const_tree t)
{
  unsigned int align0, align1;

  switch (TREE_CODE (t))
    {
    CASE_CONVERT:  case NON_LVALUE_EXPR:
      /* If we have conversions, we know that the alignment of the
	 object must meet each of the alignments of the types.  */
      align0 = expr_align (TREE_OPERAND (t, 0));
      align1 = TYPE_ALIGN (TREE_TYPE (t));
      return MAX (align0, align1);

    case SAVE_EXPR:         case COMPOUND_EXPR:       case MODIFY_EXPR:
    case INIT_EXPR:         case TARGET_EXPR:         case WITH_CLEANUP_EXPR:
    case CLEANUP_POINT_EXPR:
      /* These don't change the alignment of an object.  */
      return expr_align (TREE_OPERAND (t, 0));

    case COND_EXPR:
      /* The best we can do is say that the alignment is the least aligned
	 of the two arms.  */
      align0 = expr_align (TREE_OPERAND (t, 1));
      align1 = expr_align (TREE_OPERAND (t, 2));
      return MIN (align0, align1);

      /* FIXME: LABEL_DECL and CONST_DECL never have DECL_ALIGN set
	 meaningfully, it's always 1.  */
    case LABEL_DECL:     case CONST_DECL:
    case VAR_DECL:       case PARM_DECL:   case RESULT_DECL:
    case FUNCTION_DECL:
      gcc_assert (DECL_ALIGN (t) != 0);
      return DECL_ALIGN (t);

    default:
      break;
    }

  /* Otherwise take the alignment from that of the type.  */
  return TYPE_ALIGN (TREE_TYPE (t));
}

/* Return, as a tree node, the number of elements for TYPE (which is an
   ARRAY_TYPE) minus one. This counts only elements of the top array.  */

tree
array_type_nelts (const_tree type)
{
  tree index_type, min, max;

  /* If they did it with unspecified bounds, then we should have already
     given an error about it before we got here.  */
  if (! TYPE_DOMAIN (type))
    return error_mark_node;

  index_type = TYPE_DOMAIN (type);
  min = TYPE_MIN_VALUE (index_type);
  max = TYPE_MAX_VALUE (index_type);

  /* TYPE_MAX_VALUE may not be set if the array has unknown length.  */
  if (!max)
    return error_mark_node;

  return (integer_zerop (min)
	  ? max
	  : fold_build2 (MINUS_EXPR, TREE_TYPE (max), max, min));
}

/* If arg is static -- a reference to an object in static storage -- then
   return the object.  This is not the same as the C meaning of `static'.
   If arg isn't static, return NULL.  */

tree
staticp (tree arg)
{
  switch (TREE_CODE (arg))
    {
    case FUNCTION_DECL:
      /* Nested functions are static, even though taking their address will
	 involve a trampoline as we unnest the nested function and create
	 the trampoline on the tree level.  */
      return arg;

    case VAR_DECL:
      return ((TREE_STATIC (arg) || DECL_EXTERNAL (arg))
	      && ! DECL_THREAD_LOCAL_P (arg)
	      && ! DECL_DLLIMPORT_P (arg)
	      ? arg : NULL);

    case CONST_DECL:
      return ((TREE_STATIC (arg) || DECL_EXTERNAL (arg))
	      ? arg : NULL);

    case CONSTRUCTOR:
      return TREE_STATIC (arg) ? arg : NULL;

    case LABEL_DECL:
    case STRING_CST:
      return arg;

    case COMPONENT_REF:
      /* If the thing being referenced is not a field, then it is
	 something language specific.  */
      gcc_assert (TREE_CODE (TREE_OPERAND (arg, 1)) == FIELD_DECL);

      /* If we are referencing a bitfield, we can't evaluate an
	 ADDR_EXPR at compile time and so it isn't a constant.  */
      if (DECL_BIT_FIELD (TREE_OPERAND (arg, 1)))
	return NULL;

      return staticp (TREE_OPERAND (arg, 0));

    case BIT_FIELD_REF:
      return NULL;

    case INDIRECT_REF:
      return TREE_CONSTANT (TREE_OPERAND (arg, 0)) ? arg : NULL;

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      if (TREE_CODE (TYPE_SIZE (TREE_TYPE (arg))) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg, 1)) == INTEGER_CST)
	return staticp (TREE_OPERAND (arg, 0));
      else
	return NULL;

    case COMPOUND_LITERAL_EXPR:
      return TREE_STATIC (COMPOUND_LITERAL_EXPR_DECL (arg)) ? arg : NULL;

    default:
      return NULL;
    }
}




/* Return whether OP is a DECL whose address is function-invariant.  */

bool
decl_address_invariant_p (const_tree op)
{
  /* The conditions below are slightly less strict than the one in
     staticp.  */

  switch (TREE_CODE (op))
    {
    case PARM_DECL:
    case RESULT_DECL:
    case LABEL_DECL:
    case FUNCTION_DECL:
      return true;

    case VAR_DECL:
      if ((TREE_STATIC (op) || DECL_EXTERNAL (op))
          || DECL_THREAD_LOCAL_P (op)
          || DECL_CONTEXT (op) == current_function_decl
          || decl_function_context (op) == current_function_decl)
        return true;
      break;

    case CONST_DECL:
      if ((TREE_STATIC (op) || DECL_EXTERNAL (op))
          || decl_function_context (op) == current_function_decl)
        return true;
      break;

    default:
      break;
    }

  return false;
}

/* Return whether OP is a DECL whose address is interprocedural-invariant.  */

bool
decl_address_ip_invariant_p (const_tree op)
{
  /* The conditions below are slightly less strict than the one in
     staticp.  */

  switch (TREE_CODE (op))
    {
    case LABEL_DECL:
    case FUNCTION_DECL:
    case STRING_CST:
      return true;

    case VAR_DECL:
      if (((TREE_STATIC (op) || DECL_EXTERNAL (op))
           && !DECL_DLLIMPORT_P (op))
          || DECL_THREAD_LOCAL_P (op))
        return true;
      break;

    case CONST_DECL:
      if ((TREE_STATIC (op) || DECL_EXTERNAL (op)))
        return true;
      break;

    default:
      break;
    }

  return false;
}


/* Return true if T is function-invariant (internal function, does
   not handle arithmetic; that's handled in skip_simple_arithmetic and
   tree_invariant_p).  */

static bool
tree_invariant_p_1 (tree t)
{
  tree op;

  if (TREE_CONSTANT (t)
      || (TREE_READONLY (t) && !TREE_SIDE_EFFECTS (t)))
    return true;

  switch (TREE_CODE (t))
    {
    case SAVE_EXPR:
      return true;

    case ADDR_EXPR:
      op = TREE_OPERAND (t, 0);
      while (handled_component_p (op))
	{
	  switch (TREE_CODE (op))
	    {
	    case ARRAY_REF:
	    case ARRAY_RANGE_REF:
	      if (!tree_invariant_p (TREE_OPERAND (op, 1))
		  || TREE_OPERAND (op, 2) != NULL_TREE
		  || TREE_OPERAND (op, 3) != NULL_TREE)
		return false;
	      break;

	    case COMPONENT_REF:
	      if (TREE_OPERAND (op, 2) != NULL_TREE)
		return false;
	      break;

	    default:;
	    }
	  op = TREE_OPERAND (op, 0);
	}

      return CONSTANT_CLASS_P (op) || decl_address_invariant_p (op);

    default:
      break;
    }

  return false;
}

/* Return true if T is function-invariant.  */

bool
tree_invariant_p (tree t)
{
  tree inner = skip_simple_arithmetic (t);
  return tree_invariant_p_1 (inner);
}

/* Wrap a SAVE_EXPR around EXPR, if appropriate.
   Do this to any expression which may be used in more than one place,
   but must be evaluated only once.

   Normally, expand_expr would reevaluate the expression each time.
   Calling save_expr produces something that is evaluated and recorded
   the first time expand_expr is called on it.  Subsequent calls to
   expand_expr just reuse the recorded value.

   The call to expand_expr that generates code that actually computes
   the value is the first call *at compile time*.  Subsequent calls
   *at compile time* generate code to use the saved value.
   This produces correct result provided that *at run time* control
   always flows through the insns made by the first expand_expr
   before reaching the other places where the save_expr was evaluated.
   You, the caller of save_expr, must make sure this is so.

   Constants, and certain read-only nodes, are returned with no
   SAVE_EXPR because that is safe.  Expressions containing placeholders
   are not touched; see tree.def for an explanation of what these
   are used for.  */

tree
save_expr (tree expr)
{
  tree t = fold (expr);
  tree inner;

  /* If the tree evaluates to a constant, then we don't want to hide that
     fact (i.e. this allows further folding, and direct checks for constants).
     However, a read-only object that has side effects cannot be bypassed.
     Since it is no problem to reevaluate literals, we just return the
     literal node.  */
  inner = skip_simple_arithmetic (t);
  if (TREE_CODE (inner) == ERROR_MARK)
    return inner;

  if (tree_invariant_p_1 (inner))
    return t;

  /* If INNER contains a PLACEHOLDER_EXPR, we must evaluate it each time, since
     it means that the size or offset of some field of an object depends on
     the value within another field.

     Note that it must not be the case that T contains both a PLACEHOLDER_EXPR
     and some variable since it would then need to be both evaluated once and
     evaluated more than once.  Front-ends must assure this case cannot
     happen by surrounding any such subexpressions in their own SAVE_EXPR
     and forcing evaluation at the proper time.  */
  if (contains_placeholder_p (inner))
    return t;

  t = build1 (SAVE_EXPR, TREE_TYPE (expr), t);
  SET_EXPR_LOCATION (t, EXPR_LOCATION (expr));

  /* This expression might be placed ahead of a jump to ensure that the
     value was computed on both sides of the jump.  So make sure it isn't
     eliminated as dead.  */
  TREE_SIDE_EFFECTS (t) = 1;
  return t;
}

/* Look inside EXPR into any simple arithmetic operations.  Return the
   outermost non-arithmetic or non-invariant node.  */

tree
skip_simple_arithmetic (tree expr)
{
  /* We don't care about whether this can be used as an lvalue in this
     context.  */
  while (TREE_CODE (expr) == NON_LVALUE_EXPR)
    expr = TREE_OPERAND (expr, 0);

  /* If we have simple operations applied to a SAVE_EXPR or to a SAVE_EXPR and
     a constant, it will be more efficient to not make another SAVE_EXPR since
     it will allow better simplification and GCSE will be able to merge the
     computations if they actually occur.  */
  while (true)
    {
      if (UNARY_CLASS_P (expr))
	expr = TREE_OPERAND (expr, 0);
      else if (BINARY_CLASS_P (expr))
	{
	  if (tree_invariant_p (TREE_OPERAND (expr, 1)))
	    expr = TREE_OPERAND (expr, 0);
	  else if (tree_invariant_p (TREE_OPERAND (expr, 0)))
	    expr = TREE_OPERAND (expr, 1);
	  else
	    break;
	}
      else
	break;
    }

  return expr;
}

/* Look inside EXPR into simple arithmetic operations involving constants.
   Return the outermost non-arithmetic or non-constant node.  */

tree
skip_simple_constant_arithmetic (tree expr)
{
  while (TREE_CODE (expr) == NON_LVALUE_EXPR)
    expr = TREE_OPERAND (expr, 0);

  while (true)
    {
      if (UNARY_CLASS_P (expr))
	expr = TREE_OPERAND (expr, 0);
      else if (BINARY_CLASS_P (expr))
	{
	  if (TREE_CONSTANT (TREE_OPERAND (expr, 1)))
	    expr = TREE_OPERAND (expr, 0);
	  else if (TREE_CONSTANT (TREE_OPERAND (expr, 0)))
	    expr = TREE_OPERAND (expr, 1);
	  else
	    break;
	}
      else
	break;
    }

  return expr;
}

/* Return which tree structure is used by T.  */

enum tree_node_structure_enum
tree_node_structure (const_tree t)
{
  const enum tree_code code = TREE_CODE (t);
  return tree_node_structure_for_code (code);
}

/* Set various status flags when building a CALL_EXPR object T.  */

static void
process_call_operands (tree t)
{
  bool side_effects = TREE_SIDE_EFFECTS (t);
  bool read_only = false;
  int i = call_expr_flags (t);

  /* Calls have side-effects, except those to const or pure functions.  */
  if ((i & ECF_LOOPING_CONST_OR_PURE) || !(i & (ECF_CONST | ECF_PURE)))
    side_effects = true;
  /* Propagate TREE_READONLY of arguments for const functions.  */
  if (i & ECF_CONST)
    read_only = true;

  if (!side_effects || read_only)
    for (i = 1; i < TREE_OPERAND_LENGTH (t); i++)
      {
	tree op = TREE_OPERAND (t, i);
	if (op && TREE_SIDE_EFFECTS (op))
	  side_effects = true;
	if (op && !TREE_READONLY (op) && !CONSTANT_CLASS_P (op))
	  read_only = false;
      }

  TREE_SIDE_EFFECTS (t) = side_effects;
  TREE_READONLY (t) = read_only;
}

/* Return true if EXP contains a PLACEHOLDER_EXPR, i.e. if it represents a
   size or offset that depends on a field within a record.  */

bool
contains_placeholder_p (const_tree exp)
{
  enum tree_code code;

  if (!exp)
    return 0;

  code = TREE_CODE (exp);
  if (code == PLACEHOLDER_EXPR)
    return 1;

  switch (TREE_CODE_CLASS (code))
    {
    case tcc_reference:
      /* Don't look at any PLACEHOLDER_EXPRs that might be in index or bit
	 position computations since they will be converted into a
	 WITH_RECORD_EXPR involving the reference, which will assume
	 here will be valid.  */
      return CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 0));

    case tcc_exceptional:
      if (code == TREE_LIST)
	return (CONTAINS_PLACEHOLDER_P (TREE_VALUE (exp))
		|| CONTAINS_PLACEHOLDER_P (TREE_CHAIN (exp)));
      break;

    case tcc_unary:
    case tcc_binary:
    case tcc_comparison:
    case tcc_expression:
      switch (code)
	{
	case COMPOUND_EXPR:
	  /* Ignoring the first operand isn't quite right, but works best.  */
	  return CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 1));

	case COND_EXPR:
	  return (CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 0))
		  || CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 1))
		  || CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 2)));

	case SAVE_EXPR:
	  /* The save_expr function never wraps anything containing
	     a PLACEHOLDER_EXPR. */
	  return 0;

	default:
	  break;
	}

      switch (TREE_CODE_LENGTH (code))
	{
	case 1:
	  return CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 0));
	case 2:
	  return (CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 0))
		  || CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 1)));
	default:
	  return 0;
	}

    case tcc_vl_exp:
      switch (code)
	{
	case CALL_EXPR:
	  {
	    const_tree arg;
	    const_call_expr_arg_iterator iter;
	    FOR_EACH_CONST_CALL_EXPR_ARG (arg, iter, exp)
	      if (CONTAINS_PLACEHOLDER_P (arg))
		return 1;
	    return 0;
	  }
	default:
	  return 0;
	}

    default:
      return 0;
    }
  return 0;
}

/* Return true if any part of the structure of TYPE involves a PLACEHOLDER_EXPR
   directly.  This includes size, bounds, qualifiers (for QUAL_UNION_TYPE) and
   field positions.  */

static bool
type_contains_placeholder_1 (const_tree type)
{
  /* If the size contains a placeholder or the parent type (component type in
     the case of arrays) type involves a placeholder, this type does.  */
  if (CONTAINS_PLACEHOLDER_P (TYPE_SIZE (type))
      || CONTAINS_PLACEHOLDER_P (TYPE_SIZE_UNIT (type))
      || (!POINTER_TYPE_P (type)
	  && TREE_TYPE (type)
	  && type_contains_placeholder_p (TREE_TYPE (type))))
    return true;

  /* Now do type-specific checks.  Note that the last part of the check above
     greatly limits what we have to do below.  */
  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
    case POINTER_BOUNDS_TYPE:
    case COMPLEX_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case POINTER_TYPE:
    case OFFSET_TYPE:
    case REFERENCE_TYPE:
    case METHOD_TYPE:
    case FUNCTION_TYPE:
    case VECTOR_TYPE:
    case NULLPTR_TYPE:
      return false;

    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
      /* Here we just check the bounds.  */
      return (CONTAINS_PLACEHOLDER_P (TYPE_MIN_VALUE (type))
	      || CONTAINS_PLACEHOLDER_P (TYPE_MAX_VALUE (type)));

    case ARRAY_TYPE:
      /* We have already checked the component type above, so just check
	 the domain type.  Flexible array members have a null domain.  */
      return TYPE_DOMAIN (type) ?
	type_contains_placeholder_p (TYPE_DOMAIN (type)) : false;

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree field;

	for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	  if (TREE_CODE (field) == FIELD_DECL
	      && (CONTAINS_PLACEHOLDER_P (DECL_FIELD_OFFSET (field))
		  || (TREE_CODE (type) == QUAL_UNION_TYPE
		      && CONTAINS_PLACEHOLDER_P (DECL_QUALIFIER (field)))
		  || type_contains_placeholder_p (TREE_TYPE (field))))
	    return true;

	return false;
      }

    default:
      gcc_unreachable ();
    }
}

/* Wrapper around above function used to cache its result.  */

bool
type_contains_placeholder_p (tree type)
{
  bool result;

  /* If the contains_placeholder_bits field has been initialized,
     then we know the answer.  */
  if (TYPE_CONTAINS_PLACEHOLDER_INTERNAL (type) > 0)
    return TYPE_CONTAINS_PLACEHOLDER_INTERNAL (type) - 1;

  /* Indicate that we've seen this type node, and the answer is false.
     This is what we want to return if we run into recursion via fields.  */
  TYPE_CONTAINS_PLACEHOLDER_INTERNAL (type) = 1;

  /* Compute the real value.  */
  result = type_contains_placeholder_1 (type);

  /* Store the real value.  */
  TYPE_CONTAINS_PLACEHOLDER_INTERNAL (type) = result + 1;

  return result;
}

/* Push tree EXP onto vector QUEUE if it is not already present.  */

static void
push_without_duplicates (tree exp, vec<tree> *queue)
{
  unsigned int i;
  tree iter;

  FOR_EACH_VEC_ELT (*queue, i, iter)
    if (simple_cst_equal (iter, exp) == 1)
      break;

  if (!iter)
    queue->safe_push (exp);
}

/* Given a tree EXP, find all occurrences of references to fields
   in a PLACEHOLDER_EXPR and place them in vector REFS without
   duplicates.  Also record VAR_DECLs and CONST_DECLs.  Note that
   we assume here that EXP contains only arithmetic expressions
   or CALL_EXPRs with PLACEHOLDER_EXPRs occurring only in their
   argument list.  */

void
find_placeholder_in_expr (tree exp, vec<tree> *refs)
{
  enum tree_code code = TREE_CODE (exp);
  tree inner;
  int i;

  /* We handle TREE_LIST and COMPONENT_REF separately.  */
  if (code == TREE_LIST)
    {
      FIND_PLACEHOLDER_IN_EXPR (TREE_CHAIN (exp), refs);
      FIND_PLACEHOLDER_IN_EXPR (TREE_VALUE (exp), refs);
    }
  else if (code == COMPONENT_REF)
    {
      for (inner = TREE_OPERAND (exp, 0);
	   REFERENCE_CLASS_P (inner);
	   inner = TREE_OPERAND (inner, 0))
	;

      if (TREE_CODE (inner) == PLACEHOLDER_EXPR)
	push_without_duplicates (exp, refs);
      else
	FIND_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 0), refs);
   }
  else
    switch (TREE_CODE_CLASS (code))
      {
      case tcc_constant:
	break;

      case tcc_declaration:
	/* Variables allocated to static storage can stay.  */
        if (!TREE_STATIC (exp))
	  push_without_duplicates (exp, refs);
	break;

      case tcc_expression:
	/* This is the pattern built in ada/make_aligning_type.  */
	if (code == ADDR_EXPR
	    && TREE_CODE (TREE_OPERAND (exp, 0)) == PLACEHOLDER_EXPR)
	  {
	    push_without_duplicates (exp, refs);
	    break;
	  }

        /* Fall through...  */

      case tcc_exceptional:
      case tcc_unary:
      case tcc_binary:
      case tcc_comparison:
      case tcc_reference:
	for (i = 0; i < TREE_CODE_LENGTH (code); i++)
	  FIND_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, i), refs);
	break;

      case tcc_vl_exp:
	for (i = 1; i < TREE_OPERAND_LENGTH (exp); i++)
	  FIND_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, i), refs);
	break;

      default:
	gcc_unreachable ();
      }
}

/* Given a tree EXP, a FIELD_DECL F, and a replacement value R,
   return a tree with all occurrences of references to F in a
   PLACEHOLDER_EXPR replaced by R.  Also handle VAR_DECLs and
   CONST_DECLs.  Note that we assume here that EXP contains only
   arithmetic expressions or CALL_EXPRs with PLACEHOLDER_EXPRs
   occurring only in their argument list.  */

tree
substitute_in_expr (tree exp, tree f, tree r)
{
  enum tree_code code = TREE_CODE (exp);
  tree op0, op1, op2, op3;
  tree new_tree;

  /* We handle TREE_LIST and COMPONENT_REF separately.  */
  if (code == TREE_LIST)
    {
      op0 = SUBSTITUTE_IN_EXPR (TREE_CHAIN (exp), f, r);
      op1 = SUBSTITUTE_IN_EXPR (TREE_VALUE (exp), f, r);
      if (op0 == TREE_CHAIN (exp) && op1 == TREE_VALUE (exp))
	return exp;

      return tree_cons (TREE_PURPOSE (exp), op1, op0);
    }
  else if (code == COMPONENT_REF)
    {
      tree inner;

      /* If this expression is getting a value from a PLACEHOLDER_EXPR
	 and it is the right field, replace it with R.  */
      for (inner = TREE_OPERAND (exp, 0);
	   REFERENCE_CLASS_P (inner);
	   inner = TREE_OPERAND (inner, 0))
	;

      /* The field.  */
      op1 = TREE_OPERAND (exp, 1);

      if (TREE_CODE (inner) == PLACEHOLDER_EXPR && op1 == f)
	return r;

      /* If this expression hasn't been completed let, leave it alone.  */
      if (TREE_CODE (inner) == PLACEHOLDER_EXPR && !TREE_TYPE (inner))
	return exp;

      op0 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 0), f, r);
      if (op0 == TREE_OPERAND (exp, 0))
	return exp;

      new_tree
	= fold_build3 (COMPONENT_REF, TREE_TYPE (exp), op0, op1, NULL_TREE);
   }
  else
    switch (TREE_CODE_CLASS (code))
      {
      case tcc_constant:
	return exp;

      case tcc_declaration:
	if (exp == f)
	  return r;
	else
	  return exp;

      case tcc_expression:
	if (exp == f)
	  return r;

        /* Fall through...  */

      case tcc_exceptional:
      case tcc_unary:
      case tcc_binary:
      case tcc_comparison:
      case tcc_reference:
	switch (TREE_CODE_LENGTH (code))
	  {
	  case 0:
	    return exp;

	  case 1:
	    op0 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 0), f, r);
	    if (op0 == TREE_OPERAND (exp, 0))
	      return exp;

	    new_tree = fold_build1 (code, TREE_TYPE (exp), op0);
	    break;

	  case 2:
	    op0 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 0), f, r);
	    op1 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 1), f, r);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1))
	      return exp;

	    new_tree = fold_build2 (code, TREE_TYPE (exp), op0, op1);
	    break;

	  case 3:
	    op0 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 0), f, r);
	    op1 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 1), f, r);
	    op2 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 2), f, r);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2))
	      return exp;

	    new_tree = fold_build3 (code, TREE_TYPE (exp), op0, op1, op2);
	    break;

	  case 4:
	    op0 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 0), f, r);
	    op1 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 1), f, r);
	    op2 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 2), f, r);
	    op3 = SUBSTITUTE_IN_EXPR (TREE_OPERAND (exp, 3), f, r);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2)
		&& op3 == TREE_OPERAND (exp, 3))
	      return exp;

	    new_tree
	      = fold (build4 (code, TREE_TYPE (exp), op0, op1, op2, op3));
	    break;

	  default:
	    gcc_unreachable ();
	  }
	break;

      case tcc_vl_exp:
	{
	  int i;

	  new_tree = NULL_TREE;

	  /* If we are trying to replace F with a constant, inline back
	     functions which do nothing else than computing a value from
	     the arguments they are passed.  This makes it possible to
	     fold partially or entirely the replacement expression.  */
	  if (CONSTANT_CLASS_P (r) && code == CALL_EXPR)
	    {
	      tree t = maybe_inline_call_in_expr (exp);
	      if (t)
		return SUBSTITUTE_IN_EXPR (t, f, r);
	    }

	  for (i = 1; i < TREE_OPERAND_LENGTH (exp); i++)
	    {
	      tree op = TREE_OPERAND (exp, i);
	      tree new_op = SUBSTITUTE_IN_EXPR (op, f, r);
	      if (new_op != op)
		{
		  if (!new_tree)
		    new_tree = copy_node (exp);
		  TREE_OPERAND (new_tree, i) = new_op;
		}
	    }

	  if (new_tree)
	    {
	      new_tree = fold (new_tree);
	      if (TREE_CODE (new_tree) == CALL_EXPR)
		process_call_operands (new_tree);
	    }
	  else
	    return exp;
	}
	break;

      default:
	gcc_unreachable ();
      }

  TREE_READONLY (new_tree) |= TREE_READONLY (exp);

  if (code == INDIRECT_REF || code == ARRAY_REF || code == ARRAY_RANGE_REF)
    TREE_THIS_NOTRAP (new_tree) |= TREE_THIS_NOTRAP (exp);

  return new_tree;
}

/* Similar, but look for a PLACEHOLDER_EXPR in EXP and find a replacement
   for it within OBJ, a tree that is an object or a chain of references.  */

tree
substitute_placeholder_in_expr (tree exp, tree obj)
{
  enum tree_code code = TREE_CODE (exp);
  tree op0, op1, op2, op3;
  tree new_tree;

  /* If this is a PLACEHOLDER_EXPR, see if we find a corresponding type
     in the chain of OBJ.  */
  if (code == PLACEHOLDER_EXPR)
    {
      tree need_type = TYPE_MAIN_VARIANT (TREE_TYPE (exp));
      tree elt;

      for (elt = obj; elt != 0;
	   elt = ((TREE_CODE (elt) == COMPOUND_EXPR
		   || TREE_CODE (elt) == COND_EXPR)
		  ? TREE_OPERAND (elt, 1)
		  : (REFERENCE_CLASS_P (elt)
		     || UNARY_CLASS_P (elt)
		     || BINARY_CLASS_P (elt)
		     || VL_EXP_CLASS_P (elt)
		     || EXPRESSION_CLASS_P (elt))
		  ? TREE_OPERAND (elt, 0) : 0))
	if (TYPE_MAIN_VARIANT (TREE_TYPE (elt)) == need_type)
	  return elt;

      for (elt = obj; elt != 0;
	   elt = ((TREE_CODE (elt) == COMPOUND_EXPR
		   || TREE_CODE (elt) == COND_EXPR)
		  ? TREE_OPERAND (elt, 1)
		  : (REFERENCE_CLASS_P (elt)
		     || UNARY_CLASS_P (elt)
		     || BINARY_CLASS_P (elt)
		     || VL_EXP_CLASS_P (elt)
		     || EXPRESSION_CLASS_P (elt))
		  ? TREE_OPERAND (elt, 0) : 0))
	if (POINTER_TYPE_P (TREE_TYPE (elt))
	    && (TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (elt)))
		== need_type))
	  return fold_build1 (INDIRECT_REF, need_type, elt);

      /* If we didn't find it, return the original PLACEHOLDER_EXPR.  If it
	 survives until RTL generation, there will be an error.  */
      return exp;
    }

  /* TREE_LIST is special because we need to look at TREE_VALUE
     and TREE_CHAIN, not TREE_OPERANDS.  */
  else if (code == TREE_LIST)
    {
      op0 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_CHAIN (exp), obj);
      op1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_VALUE (exp), obj);
      if (op0 == TREE_CHAIN (exp) && op1 == TREE_VALUE (exp))
	return exp;

      return tree_cons (TREE_PURPOSE (exp), op1, op0);
    }
  else
    switch (TREE_CODE_CLASS (code))
      {
      case tcc_constant:
      case tcc_declaration:
	return exp;

      case tcc_exceptional:
      case tcc_unary:
      case tcc_binary:
      case tcc_comparison:
      case tcc_expression:
      case tcc_reference:
      case tcc_statement:
	switch (TREE_CODE_LENGTH (code))
	  {
	  case 0:
	    return exp;

	  case 1:
	    op0 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 0), obj);
	    if (op0 == TREE_OPERAND (exp, 0))
	      return exp;

	    new_tree = fold_build1 (code, TREE_TYPE (exp), op0);
	    break;

	  case 2:
	    op0 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 0), obj);
	    op1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 1), obj);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1))
	      return exp;

	    new_tree = fold_build2 (code, TREE_TYPE (exp), op0, op1);
	    break;

	  case 3:
	    op0 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 0), obj);
	    op1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 1), obj);
	    op2 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 2), obj);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2))
	      return exp;

	    new_tree = fold_build3 (code, TREE_TYPE (exp), op0, op1, op2);
	    break;

	  case 4:
	    op0 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 0), obj);
	    op1 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 1), obj);
	    op2 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 2), obj);
	    op3 = SUBSTITUTE_PLACEHOLDER_IN_EXPR (TREE_OPERAND (exp, 3), obj);

	    if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
		&& op2 == TREE_OPERAND (exp, 2)
		&& op3 == TREE_OPERAND (exp, 3))
	      return exp;

	    new_tree
	      = fold (build4 (code, TREE_TYPE (exp), op0, op1, op2, op3));
	    break;

	  default:
	    gcc_unreachable ();
	  }
	break;

      case tcc_vl_exp:
	{
	  int i;

	  new_tree = NULL_TREE;

	  for (i = 1; i < TREE_OPERAND_LENGTH (exp); i++)
	    {
	      tree op = TREE_OPERAND (exp, i);
	      tree new_op = SUBSTITUTE_PLACEHOLDER_IN_EXPR (op, obj);
	      if (new_op != op)
		{
		  if (!new_tree)
		    new_tree = copy_node (exp);
		  TREE_OPERAND (new_tree, i) = new_op;
		}
	    }

	  if (new_tree)
	    {
	      new_tree = fold (new_tree);
	      if (TREE_CODE (new_tree) == CALL_EXPR)
		process_call_operands (new_tree);
	    }
	  else
	    return exp;
	}
	break;

      default:
	gcc_unreachable ();
      }

  TREE_READONLY (new_tree) |= TREE_READONLY (exp);

  if (code == INDIRECT_REF || code == ARRAY_REF || code == ARRAY_RANGE_REF)
    TREE_THIS_NOTRAP (new_tree) |= TREE_THIS_NOTRAP (exp);

  return new_tree;
}


/* Subroutine of stabilize_reference; this is called for subtrees of
   references.  Any expression with side-effects must be put in a SAVE_EXPR
   to ensure that it is only evaluated once.

   We don't put SAVE_EXPR nodes around everything, because assigning very
   simple expressions to temporaries causes us to miss good opportunities
   for optimizations.  Among other things, the opportunity to fold in the
   addition of a constant into an addressing mode often gets lost, e.g.
   "y[i+1] += x;".  In general, we take the approach that we should not make
   an assignment unless we are forced into it - i.e., that any non-side effect
   operator should be allowed, and that cse should take care of coalescing
   multiple utterances of the same expression should that prove fruitful.  */

static tree
stabilize_reference_1 (tree e)
{
  tree result;
  enum tree_code code = TREE_CODE (e);

  /* We cannot ignore const expressions because it might be a reference
     to a const array but whose index contains side-effects.  But we can
     ignore things that are actual constant or that already have been
     handled by this function.  */

  if (tree_invariant_p (e))
    return e;

  switch (TREE_CODE_CLASS (code))
    {
    case tcc_exceptional:
    case tcc_type:
    case tcc_declaration:
    case tcc_comparison:
    case tcc_statement:
    case tcc_expression:
    case tcc_reference:
    case tcc_vl_exp:
      /* If the expression has side-effects, then encase it in a SAVE_EXPR
	 so that it will only be evaluated once.  */
      /* The reference (r) and comparison (<) classes could be handled as
	 below, but it is generally faster to only evaluate them once.  */
      if (TREE_SIDE_EFFECTS (e))
	return save_expr (e);
      return e;

    case tcc_constant:
      /* Constants need no processing.  In fact, we should never reach
	 here.  */
      return e;

    case tcc_binary:
      /* Division is slow and tends to be compiled with jumps,
	 especially the division by powers of 2 that is often
	 found inside of an array reference.  So do it just once.  */
      if (code == TRUNC_DIV_EXPR || code == TRUNC_MOD_EXPR
	  || code == FLOOR_DIV_EXPR || code == FLOOR_MOD_EXPR
	  || code == CEIL_DIV_EXPR || code == CEIL_MOD_EXPR
	  || code == ROUND_DIV_EXPR || code == ROUND_MOD_EXPR)
	return save_expr (e);
      /* Recursively stabilize each operand.  */
      result = build_nt (code, stabilize_reference_1 (TREE_OPERAND (e, 0)),
			 stabilize_reference_1 (TREE_OPERAND (e, 1)));
      break;

    case tcc_unary:
      /* Recursively stabilize each operand.  */
      result = build_nt (code, stabilize_reference_1 (TREE_OPERAND (e, 0)));
      break;

    default:
      gcc_unreachable ();
    }

  TREE_TYPE (result) = TREE_TYPE (e);
  TREE_READONLY (result) = TREE_READONLY (e);
  TREE_SIDE_EFFECTS (result) = TREE_SIDE_EFFECTS (e);
  TREE_THIS_VOLATILE (result) = TREE_THIS_VOLATILE (e);

  return result;
}

/* Stabilize a reference so that we can use it any number of times
   without causing its operands to be evaluated more than once.
   Returns the stabilized reference.  This works by means of save_expr,
   so see the caveats in the comments about save_expr.

   Also allows conversion expressions whose operands are references.
   Any other kind of expression is returned unchanged.  */

tree
stabilize_reference (tree ref)
{
  tree result;
  enum tree_code code = TREE_CODE (ref);

  switch (code)
    {
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      /* No action is needed in this case.  */
      return ref;

    CASE_CONVERT:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
      result = build_nt (code, stabilize_reference (TREE_OPERAND (ref, 0)));
      break;

    case INDIRECT_REF:
      result = build_nt (INDIRECT_REF,
			 stabilize_reference_1 (TREE_OPERAND (ref, 0)));
      break;

    case COMPONENT_REF:
      result = build_nt (COMPONENT_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 TREE_OPERAND (ref, 1), NULL_TREE);
      break;

    case BIT_FIELD_REF:
      result = build_nt (BIT_FIELD_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 TREE_OPERAND (ref, 1), TREE_OPERAND (ref, 2));
      REF_REVERSE_STORAGE_ORDER (result) = REF_REVERSE_STORAGE_ORDER (ref);
      break;

    case ARRAY_REF:
      result = build_nt (ARRAY_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 1)),
			 TREE_OPERAND (ref, 2), TREE_OPERAND (ref, 3));
      break;

    case ARRAY_RANGE_REF:
      result = build_nt (ARRAY_RANGE_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 1)),
			 TREE_OPERAND (ref, 2), TREE_OPERAND (ref, 3));
      break;

    case COMPOUND_EXPR:
      /* We cannot wrap the first expression in a SAVE_EXPR, as then
	 it wouldn't be ignored.  This matters when dealing with
	 volatiles.  */
      return stabilize_reference_1 (ref);

      /* If arg isn't a kind of lvalue we recognize, make no change.
	 Caller should recognize the error for an invalid lvalue.  */
    default:
      return ref;

    case ERROR_MARK:
      return error_mark_node;
    }

  TREE_TYPE (result) = TREE_TYPE (ref);
  TREE_READONLY (result) = TREE_READONLY (ref);
  TREE_SIDE_EFFECTS (result) = TREE_SIDE_EFFECTS (ref);
  TREE_THIS_VOLATILE (result) = TREE_THIS_VOLATILE (ref);

  return result;
}

/* Low-level constructors for expressions.  */

/* A helper function for build1 and constant folders.  Set TREE_CONSTANT,
   and TREE_SIDE_EFFECTS for an ADDR_EXPR.  */

void
recompute_tree_invariant_for_addr_expr (tree t)
{
  tree node;
  bool tc = true, se = false;

  gcc_assert (TREE_CODE (t) == ADDR_EXPR);

  /* We started out assuming this address is both invariant and constant, but
     does not have side effects.  Now go down any handled components and see if
     any of them involve offsets that are either non-constant or non-invariant.
     Also check for side-effects.

     ??? Note that this code makes no attempt to deal with the case where
     taking the address of something causes a copy due to misalignment.  */

#define UPDATE_FLAGS(NODE)  \
do { tree _node = (NODE); \
     if (_node && !TREE_CONSTANT (_node)) tc = false; \
     if (_node && TREE_SIDE_EFFECTS (_node)) se = true; } while (0)

  for (node = TREE_OPERAND (t, 0); handled_component_p (node);
       node = TREE_OPERAND (node, 0))
    {
      /* If the first operand doesn't have an ARRAY_TYPE, this is a bogus
	 array reference (probably made temporarily by the G++ front end),
	 so ignore all the operands.  */
      if ((TREE_CODE (node) == ARRAY_REF
	   || TREE_CODE (node) == ARRAY_RANGE_REF)
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (node, 0))) == ARRAY_TYPE)
	{
	  UPDATE_FLAGS (TREE_OPERAND (node, 1));
	  if (TREE_OPERAND (node, 2))
	    UPDATE_FLAGS (TREE_OPERAND (node, 2));
	  if (TREE_OPERAND (node, 3))
	    UPDATE_FLAGS (TREE_OPERAND (node, 3));
	}
      /* Likewise, just because this is a COMPONENT_REF doesn't mean we have a
	 FIELD_DECL, apparently.  The G++ front end can put something else
	 there, at least temporarily.  */
      else if (TREE_CODE (node) == COMPONENT_REF
	       && TREE_CODE (TREE_OPERAND (node, 1)) == FIELD_DECL)
	{
	  if (TREE_OPERAND (node, 2))
	    UPDATE_FLAGS (TREE_OPERAND (node, 2));
	}
    }

  node = lang_hooks.expr_to_decl (node, &tc, &se);

  /* Now see what's inside.  If it's an INDIRECT_REF, copy our properties from
     the address, since &(*a)->b is a form of addition.  If it's a constant, the
     address is constant too.  If it's a decl, its address is constant if the
     decl is static.  Everything else is not constant and, furthermore,
     taking the address of a volatile variable is not volatile.  */
  if (TREE_CODE (node) == INDIRECT_REF
      || TREE_CODE (node) == MEM_REF)
    UPDATE_FLAGS (TREE_OPERAND (node, 0));
  else if (CONSTANT_CLASS_P (node))
    ;
  else if (DECL_P (node))
    tc &= (staticp (node) != NULL_TREE);
  else
    {
      tc = false;
      se |= TREE_SIDE_EFFECTS (node);
    }


  TREE_CONSTANT (t) = tc;
  TREE_SIDE_EFFECTS (t) = se;
#undef UPDATE_FLAGS
}

/* Build an expression of code CODE, data type TYPE, and operands as
   specified.  Expressions and reference nodes can be created this way.
   Constants, decls, types and misc nodes cannot be.

   We define 5 non-variadic functions, from 0 to 4 arguments.  This is
   enough for all extant tree codes.  */

tree
build0_stat (enum tree_code code, tree tt MEM_STAT_DECL)
{
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 0);

  t = make_node_stat (code PASS_MEM_STAT);
  TREE_TYPE (t) = tt;

  return t;
}

tree
build1_stat (enum tree_code code, tree type, tree node MEM_STAT_DECL)
{
  int length = sizeof (struct tree_exp);
  tree t;

  record_node_allocation_statistics (code, length);

  gcc_assert (TREE_CODE_LENGTH (code) == 1);

  t = ggc_alloc_tree_node_stat (length PASS_MEM_STAT);

  memset (t, 0, sizeof (struct tree_common));

  TREE_SET_CODE (t, code);

  TREE_TYPE (t) = type;
  SET_EXPR_LOCATION (t, UNKNOWN_LOCATION);
  TREE_OPERAND (t, 0) = node;
  if (node && !TYPE_P (node))
    {
      TREE_SIDE_EFFECTS (t) = TREE_SIDE_EFFECTS (node);
      TREE_READONLY (t) = TREE_READONLY (node);
    }

  if (TREE_CODE_CLASS (code) == tcc_statement)
    TREE_SIDE_EFFECTS (t) = 1;
  else switch (code)
    {
    case VA_ARG_EXPR:
      /* All of these have side-effects, no matter what their
	 operands are.  */
      TREE_SIDE_EFFECTS (t) = 1;
      TREE_READONLY (t) = 0;
      break;

    case INDIRECT_REF:
      /* Whether a dereference is readonly has nothing to do with whether
	 its operand is readonly.  */
      TREE_READONLY (t) = 0;
      break;

    case ADDR_EXPR:
      if (node)
	recompute_tree_invariant_for_addr_expr (t);
      break;

    default:
      if ((TREE_CODE_CLASS (code) == tcc_unary || code == VIEW_CONVERT_EXPR)
	  && node && !TYPE_P (node)
	  && TREE_CONSTANT (node))
	TREE_CONSTANT (t) = 1;
      if (TREE_CODE_CLASS (code) == tcc_reference
	  && node && TREE_THIS_VOLATILE (node))
	TREE_THIS_VOLATILE (t) = 1;
      break;
    }

  return t;
}

#define PROCESS_ARG(N)				\
  do {						\
    TREE_OPERAND (t, N) = arg##N;		\
    if (arg##N &&!TYPE_P (arg##N))		\
      {						\
        if (TREE_SIDE_EFFECTS (arg##N))		\
	  side_effects = 1;			\
        if (!TREE_READONLY (arg##N)		\
	    && !CONSTANT_CLASS_P (arg##N))	\
	  (void) (read_only = 0);		\
        if (!TREE_CONSTANT (arg##N))		\
	  (void) (constant = 0);		\
      }						\
  } while (0)

tree
build2_stat (enum tree_code code, tree tt, tree arg0, tree arg1 MEM_STAT_DECL)
{
  bool constant, read_only, side_effects;
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 2);

  if ((code == MINUS_EXPR || code == PLUS_EXPR || code == MULT_EXPR)
      && arg0 && arg1 && tt && POINTER_TYPE_P (tt)
      /* When sizetype precision doesn't match that of pointers
         we need to be able to build explicit extensions or truncations
	 of the offset argument.  */
      && TYPE_PRECISION (sizetype) == TYPE_PRECISION (tt))
    gcc_assert (TREE_CODE (arg0) == INTEGER_CST
		&& TREE_CODE (arg1) == INTEGER_CST);

  if (code == POINTER_PLUS_EXPR && arg0 && arg1 && tt)
    gcc_assert (POINTER_TYPE_P (tt) && POINTER_TYPE_P (TREE_TYPE (arg0))
		&& ptrofftype_p (TREE_TYPE (arg1)));

  t = make_node_stat (code PASS_MEM_STAT);
  TREE_TYPE (t) = tt;

  /* Below, we automatically set TREE_SIDE_EFFECTS and TREE_READONLY for the
     result based on those same flags for the arguments.  But if the
     arguments aren't really even `tree' expressions, we shouldn't be trying
     to do this.  */

  /* Expressions without side effects may be constant if their
     arguments are as well.  */
  constant = (TREE_CODE_CLASS (code) == tcc_comparison
	      || TREE_CODE_CLASS (code) == tcc_binary);
  read_only = 1;
  side_effects = TREE_SIDE_EFFECTS (t);

  PROCESS_ARG (0);
  PROCESS_ARG (1);

  TREE_SIDE_EFFECTS (t) = side_effects;
  if (code == MEM_REF)
    {
      if (arg0 && TREE_CODE (arg0) == ADDR_EXPR)
	{
	  tree o = TREE_OPERAND (arg0, 0);
	  TREE_READONLY (t) = TREE_READONLY (o);
	  TREE_THIS_VOLATILE (t) = TREE_THIS_VOLATILE (o);
	}
    }
  else
    {
      TREE_READONLY (t) = read_only;
      TREE_CONSTANT (t) = constant;
      TREE_THIS_VOLATILE (t)
	= (TREE_CODE_CLASS (code) == tcc_reference
	   && arg0 && TREE_THIS_VOLATILE (arg0));
    }

  return t;
}


tree
build3_stat (enum tree_code code, tree tt, tree arg0, tree arg1,
	     tree arg2 MEM_STAT_DECL)
{
  bool constant, read_only, side_effects;
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 3);
  gcc_assert (TREE_CODE_CLASS (code) != tcc_vl_exp);

  t = make_node_stat (code PASS_MEM_STAT);
  TREE_TYPE (t) = tt;

  read_only = 1;

  /* As a special exception, if COND_EXPR has NULL branches, we
     assume that it is a gimple statement and always consider
     it to have side effects.  */
  if (code == COND_EXPR
      && tt == void_type_node
      && arg1 == NULL_TREE
      && arg2 == NULL_TREE)
    side_effects = true;
  else
    side_effects = TREE_SIDE_EFFECTS (t);

  PROCESS_ARG (0);
  PROCESS_ARG (1);
  PROCESS_ARG (2);

  if (code == COND_EXPR)
    TREE_READONLY (t) = read_only;

  TREE_SIDE_EFFECTS (t) = side_effects;
  TREE_THIS_VOLATILE (t)
    = (TREE_CODE_CLASS (code) == tcc_reference
       && arg0 && TREE_THIS_VOLATILE (arg0));

  return t;
}

tree
build4_stat (enum tree_code code, tree tt, tree arg0, tree arg1,
	     tree arg2, tree arg3 MEM_STAT_DECL)
{
  bool constant, read_only, side_effects;
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 4);

  t = make_node_stat (code PASS_MEM_STAT);
  TREE_TYPE (t) = tt;

  side_effects = TREE_SIDE_EFFECTS (t);

  PROCESS_ARG (0);
  PROCESS_ARG (1);
  PROCESS_ARG (2);
  PROCESS_ARG (3);

  TREE_SIDE_EFFECTS (t) = side_effects;
  TREE_THIS_VOLATILE (t)
    = (TREE_CODE_CLASS (code) == tcc_reference
       && arg0 && TREE_THIS_VOLATILE (arg0));

  return t;
}

tree
build5_stat (enum tree_code code, tree tt, tree arg0, tree arg1,
	     tree arg2, tree arg3, tree arg4 MEM_STAT_DECL)
{
  bool constant, read_only, side_effects;
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 5);

  t = make_node_stat (code PASS_MEM_STAT);
  TREE_TYPE (t) = tt;

  side_effects = TREE_SIDE_EFFECTS (t);

  PROCESS_ARG (0);
  PROCESS_ARG (1);
  PROCESS_ARG (2);
  PROCESS_ARG (3);
  PROCESS_ARG (4);

  TREE_SIDE_EFFECTS (t) = side_effects;
  if (code == TARGET_MEM_REF)
    {
      if (arg0 && TREE_CODE (arg0) == ADDR_EXPR)
	{
	  tree o = TREE_OPERAND (arg0, 0);
	  TREE_READONLY (t) = TREE_READONLY (o);
	  TREE_THIS_VOLATILE (t) = TREE_THIS_VOLATILE (o);
	}
    }
  else
    TREE_THIS_VOLATILE (t)
      = (TREE_CODE_CLASS (code) == tcc_reference
	 && arg0 && TREE_THIS_VOLATILE (arg0));

  return t;
}

/* Build a simple MEM_REF tree with the sematics of a plain INDIRECT_REF
   on the pointer PTR.  */

tree
build_simple_mem_ref_loc (location_t loc, tree ptr)
{
  HOST_WIDE_INT offset = 0;
  tree ptype = TREE_TYPE (ptr);
  tree tem;
  /* For convenience allow addresses that collapse to a simple base
     and offset.  */
  if (TREE_CODE (ptr) == ADDR_EXPR
      && (handled_component_p (TREE_OPERAND (ptr, 0))
	  || TREE_CODE (TREE_OPERAND (ptr, 0)) == MEM_REF))
    {
      ptr = get_addr_base_and_unit_offset (TREE_OPERAND (ptr, 0), &offset);
      gcc_assert (ptr);
      ptr = build_fold_addr_expr (ptr);
      gcc_assert (is_gimple_reg (ptr) || is_gimple_min_invariant (ptr));
    }
  tem = build2 (MEM_REF, TREE_TYPE (ptype),
		ptr, build_int_cst (ptype, offset));
  SET_EXPR_LOCATION (tem, loc);
  return tem;
}

/* Return the constant offset of a MEM_REF or TARGET_MEM_REF tree T.  */

offset_int
mem_ref_offset (const_tree t)
{
  return offset_int::from (TREE_OPERAND (t, 1), SIGNED);
}

/* Return an invariant ADDR_EXPR of type TYPE taking the address of BASE
   offsetted by OFFSET units.  */

tree
build_invariant_address (tree type, tree base, HOST_WIDE_INT offset)
{
  tree ref = fold_build2 (MEM_REF, TREE_TYPE (type),
			  build_fold_addr_expr (base),
			  build_int_cst (ptr_type_node, offset));
  tree addr = build1 (ADDR_EXPR, type, ref);
  recompute_tree_invariant_for_addr_expr (addr);
  return addr;
}

/* Similar except don't specify the TREE_TYPE
   and leave the TREE_SIDE_EFFECTS as 0.
   It is permissible for arguments to be null,
   or even garbage if their values do not matter.  */

tree
build_nt (enum tree_code code, ...)
{
  tree t;
  int length;
  int i;
  va_list p;

  gcc_assert (TREE_CODE_CLASS (code) != tcc_vl_exp);

  va_start (p, code);

  t = make_node (code);
  length = TREE_CODE_LENGTH (code);

  for (i = 0; i < length; i++)
    TREE_OPERAND (t, i) = va_arg (p, tree);

  va_end (p);
  return t;
}

/* Similar to build_nt, but for creating a CALL_EXPR object with a
   tree vec.  */

tree
build_nt_call_vec (tree fn, vec<tree, va_gc> *args)
{
  tree ret, t;
  unsigned int ix;

  ret = build_vl_exp (CALL_EXPR, vec_safe_length (args) + 3);
  CALL_EXPR_FN (ret) = fn;
  CALL_EXPR_STATIC_CHAIN (ret) = NULL_TREE;
  FOR_EACH_VEC_SAFE_ELT (args, ix, t)
    CALL_EXPR_ARG (ret, ix) = t;
  return ret;
}

/* Create a DECL_... node of code CODE, name NAME and data type TYPE.
   We do NOT enter this node in any sort of symbol table.

   LOC is the location of the decl.

   layout_decl is used to set up the decl's storage layout.
   Other slots are initialized to 0 or null pointers.  */

tree
build_decl_stat (location_t loc, enum tree_code code, tree name,
    		 tree type MEM_STAT_DECL)
{
  tree t;

  t = make_node_stat (code PASS_MEM_STAT);
  DECL_SOURCE_LOCATION (t) = loc;

/*  if (type == error_mark_node)
    type = integer_type_node; */
/* That is not done, deliberately, so that having error_mark_node
   as the type can suppress useless errors in the use of this variable.  */

  DECL_NAME (t) = name;
  TREE_TYPE (t) = type;

  if (code == VAR_DECL || code == PARM_DECL || code == RESULT_DECL)
    layout_decl (t, 0);

  return t;
}

/* Builds and returns function declaration with NAME and TYPE.  */

tree
build_fn_decl (const char *name, tree type)
{
  tree id = get_identifier (name);
  tree decl = build_decl (input_location, FUNCTION_DECL, id, type);

  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  TREE_NOTHROW (decl) = 1;

  return decl;
}

vec<tree, va_gc> *all_translation_units;

/* Builds a new translation-unit decl with name NAME, queues it in the
   global list of translation-unit decls and returns it.   */

tree
build_translation_unit_decl (tree name)
{
  tree tu = build_decl (UNKNOWN_LOCATION, TRANSLATION_UNIT_DECL,
			name, NULL_TREE);
  TRANSLATION_UNIT_LANGUAGE (tu) = lang_hooks.name;
  vec_safe_push (all_translation_units, tu);
  return tu;
}


/* BLOCK nodes are used to represent the structure of binding contours
   and declarations, once those contours have been exited and their contents
   compiled.  This information is used for outputting debugging info.  */

tree
build_block (tree vars, tree subblocks, tree supercontext, tree chain)
{
  tree block = make_node (BLOCK);

  BLOCK_VARS (block) = vars;
  BLOCK_SUBBLOCKS (block) = subblocks;
  BLOCK_SUPERCONTEXT (block) = supercontext;
  BLOCK_CHAIN (block) = chain;
  return block;
}


/* Like SET_EXPR_LOCATION, but make sure the tree can have a location.

   LOC is the location to use in tree T.  */

void
protected_set_expr_location (tree t, location_t loc)
{
  if (CAN_HAVE_LOCATION_P (t))
    SET_EXPR_LOCATION (t, loc);
}

/* Return a declaration like DDECL except that its DECL_ATTRIBUTES
   is ATTRIBUTE.  */

tree
build_decl_attribute_variant (tree ddecl, tree attribute)
{
  DECL_ATTRIBUTES (ddecl) = attribute;
  return ddecl;
}

/* Return a type like TTYPE except that its TYPE_ATTRIBUTE
   is ATTRIBUTE and its qualifiers are QUALS.

   Record such modified types already made so we don't make duplicates.  */

tree
build_type_attribute_qual_variant (tree ttype, tree attribute, int quals)
{
  if (! attribute_list_equal (TYPE_ATTRIBUTES (ttype), attribute))
    {
      inchash::hash hstate;
      tree ntype;
      int i;
      tree t;
      enum tree_code code = TREE_CODE (ttype);

      /* Building a distinct copy of a tagged type is inappropriate; it
	 causes breakage in code that expects there to be a one-to-one
	 relationship between a struct and its fields.
	 build_duplicate_type is another solution (as used in
	 handle_transparent_union_attribute), but that doesn't play well
	 with the stronger C++ type identity model.  */
      if (TREE_CODE (ttype) == RECORD_TYPE
	  || TREE_CODE (ttype) == UNION_TYPE
	  || TREE_CODE (ttype) == QUAL_UNION_TYPE
	  || TREE_CODE (ttype) == ENUMERAL_TYPE)
	{
	  warning (OPT_Wattributes,
		   "ignoring attributes applied to %qT after definition",
		   TYPE_MAIN_VARIANT (ttype));
	  return build_qualified_type (ttype, quals);
	}

      ttype = build_qualified_type (ttype, TYPE_UNQUALIFIED);
      ntype = build_distinct_type_copy (ttype);

      TYPE_ATTRIBUTES (ntype) = attribute;

      hstate.add_int (code);
      if (TREE_TYPE (ntype))
	hstate.add_object (TYPE_HASH (TREE_TYPE (ntype)));
      attribute_hash_list (attribute, hstate);

      switch (TREE_CODE (ntype))
	{
	case FUNCTION_TYPE:
	  type_hash_list (TYPE_ARG_TYPES (ntype), hstate);
	  break;
	case ARRAY_TYPE:
	  if (TYPE_DOMAIN (ntype))
	    hstate.add_object (TYPE_HASH (TYPE_DOMAIN (ntype)));
	  break;
	case INTEGER_TYPE:
	  t = TYPE_MAX_VALUE (ntype);
	  for (i = 0; i < TREE_INT_CST_NUNITS (t); i++)
	    hstate.add_object (TREE_INT_CST_ELT (t, i));
	  break;
	case REAL_TYPE:
	case FIXED_POINT_TYPE:
	  {
	    unsigned int precision = TYPE_PRECISION (ntype);
	    hstate.add_object (precision);
	  }
	  break;
	default:
	  break;
	}

      ntype = type_hash_canon (hstate.end(), ntype);

      /* If the target-dependent attributes make NTYPE different from
	 its canonical type, we will need to use structural equality
	 checks for this type. */
      if (TYPE_STRUCTURAL_EQUALITY_P (ttype)
          || !comp_type_attributes (ntype, ttype))
	SET_TYPE_STRUCTURAL_EQUALITY (ntype);
      else if (TYPE_CANONICAL (ntype) == ntype)
	TYPE_CANONICAL (ntype) = TYPE_CANONICAL (ttype);

      ttype = build_qualified_type (ntype, quals);
    }
  else if (TYPE_QUALS (ttype) != quals)
    ttype = build_qualified_type (ttype, quals);

  return ttype;
}

/* Check if "omp declare simd" attribute arguments, CLAUSES1 and CLAUSES2, are
   the same.  */

static bool
omp_declare_simd_clauses_equal (tree clauses1, tree clauses2)
{
  tree cl1, cl2;
  for (cl1 = clauses1, cl2 = clauses2;
       cl1 && cl2;
       cl1 = OMP_CLAUSE_CHAIN (cl1), cl2 = OMP_CLAUSE_CHAIN (cl2))
    {
      if (OMP_CLAUSE_CODE (cl1) != OMP_CLAUSE_CODE (cl2))
	return false;
      if (OMP_CLAUSE_CODE (cl1) != OMP_CLAUSE_SIMDLEN)
	{
	  if (simple_cst_equal (OMP_CLAUSE_DECL (cl1),
				OMP_CLAUSE_DECL (cl2)) != 1)
	    return false;
	}
      switch (OMP_CLAUSE_CODE (cl1))
	{
	case OMP_CLAUSE_ALIGNED:
	  if (simple_cst_equal (OMP_CLAUSE_ALIGNED_ALIGNMENT (cl1),
				OMP_CLAUSE_ALIGNED_ALIGNMENT (cl2)) != 1)
	    return false;
	  break;
	case OMP_CLAUSE_LINEAR:
	  if (simple_cst_equal (OMP_CLAUSE_LINEAR_STEP (cl1),
				OMP_CLAUSE_LINEAR_STEP (cl2)) != 1)
	    return false;
	  break;
	case OMP_CLAUSE_SIMDLEN:
	  if (simple_cst_equal (OMP_CLAUSE_SIMDLEN_EXPR (cl1),
				OMP_CLAUSE_SIMDLEN_EXPR (cl2)) != 1)
	    return false;
	default:
	  break;
	}
    }
  return true;
}

/* Compare two constructor-element-type constants.  Return 1 if the lists
   are known to be equal; otherwise return 0.  */

static bool
simple_cst_list_equal (const_tree l1, const_tree l2)
{
  while (l1 != NULL_TREE && l2 != NULL_TREE)
    {
      if (simple_cst_equal (TREE_VALUE (l1), TREE_VALUE (l2)) != 1)
	return false;

      l1 = TREE_CHAIN (l1);
      l2 = TREE_CHAIN (l2);
    }

  return l1 == l2;
}

/* Compare two identifier nodes representing attributes.  Either one may
   be in wrapped __ATTR__ form.  Return true if they are the same, false
   otherwise.  */

static bool
cmp_attrib_identifiers (const_tree attr1, const_tree attr2)
{
  /* Make sure we're dealing with IDENTIFIER_NODEs.  */
  gcc_checking_assert (TREE_CODE (attr1) == IDENTIFIER_NODE
		       && TREE_CODE (attr2) == IDENTIFIER_NODE);

  /* Identifiers can be compared directly for equality.  */
  if (attr1 == attr2)
    return true;

  /* If they are not equal, they may still be one in the form
     'text' while the other one is in the form '__text__'.  TODO:
     If we were storing attributes in normalized 'text' form, then
     this could all go away and we could take full advantage of
     the fact that we're comparing identifiers. :-)  */
  const size_t attr1_len = IDENTIFIER_LENGTH (attr1);
  const size_t attr2_len = IDENTIFIER_LENGTH (attr2);

  if (attr2_len == attr1_len + 4)
    {
      const char *p = IDENTIFIER_POINTER (attr2);
      const char *q = IDENTIFIER_POINTER (attr1);
      if (p[0] == '_' && p[1] == '_'
	  && p[attr2_len - 2] == '_' && p[attr2_len - 1] == '_'
	  && strncmp (q, p + 2, attr1_len) == 0)
	return true;;
    }
  else if (attr2_len + 4 == attr1_len)
    {
      const char *p = IDENTIFIER_POINTER (attr2);
      const char *q = IDENTIFIER_POINTER (attr1);
      if (q[0] == '_' && q[1] == '_'
	  && q[attr1_len - 2] == '_' && q[attr1_len - 1] == '_'
	  && strncmp (q + 2, p, attr2_len) == 0)
	return true;
    }

  return false;
}

/* Compare two attributes for their value identity.  Return true if the
   attribute values are known to be equal; otherwise return false.  */

bool
attribute_value_equal (const_tree attr1, const_tree attr2)
{
  if (TREE_VALUE (attr1) == TREE_VALUE (attr2))
    return true;

  if (TREE_VALUE (attr1) != NULL_TREE
      && TREE_CODE (TREE_VALUE (attr1)) == TREE_LIST
      && TREE_VALUE (attr2) != NULL_TREE
      && TREE_CODE (TREE_VALUE (attr2)) == TREE_LIST)
    {
      /* Handle attribute format.  */
      if (is_attribute_p ("format", TREE_PURPOSE (attr1)))
	{
	  attr1 = TREE_VALUE (attr1);
	  attr2 = TREE_VALUE (attr2);
	  /* Compare the archetypes (printf/scanf/strftime/...).  */
	  if (!cmp_attrib_identifiers (TREE_VALUE (attr1),
				       TREE_VALUE (attr2)))
	    return false;
	  /* Archetypes are the same.  Compare the rest.  */
	  return (simple_cst_list_equal (TREE_CHAIN (attr1),
					 TREE_CHAIN (attr2)) == 1);
	}
      return (simple_cst_list_equal (TREE_VALUE (attr1),
				     TREE_VALUE (attr2)) == 1);
    }

  if ((flag_openmp || flag_openmp_simd)
      && TREE_VALUE (attr1) && TREE_VALUE (attr2)
      && TREE_CODE (TREE_VALUE (attr1)) == OMP_CLAUSE
      && TREE_CODE (TREE_VALUE (attr2)) == OMP_CLAUSE)
    return omp_declare_simd_clauses_equal (TREE_VALUE (attr1),
					   TREE_VALUE (attr2));

  return (simple_cst_equal (TREE_VALUE (attr1), TREE_VALUE (attr2)) == 1);
}

/* Return 0 if the attributes for two types are incompatible, 1 if they
   are compatible, and 2 if they are nearly compatible (which causes a
   warning to be generated).  */
int
comp_type_attributes (const_tree type1, const_tree type2)
{
  const_tree a1 = TYPE_ATTRIBUTES (type1);
  const_tree a2 = TYPE_ATTRIBUTES (type2);
  const_tree a;

  if (a1 == a2)
    return 1;
  for (a = a1; a != NULL_TREE; a = TREE_CHAIN (a))
    {
      const struct attribute_spec *as;
      const_tree attr;

      as = lookup_attribute_spec (get_attribute_name (a));
      if (!as || as->affects_type_identity == false)
        continue;

      attr = lookup_attribute (as->name, CONST_CAST_TREE (a2));
      if (!attr || !attribute_value_equal (a, attr))
        break;
    }
  if (!a)
    {
      for (a = a2; a != NULL_TREE; a = TREE_CHAIN (a))
	{
	  const struct attribute_spec *as;

	  as = lookup_attribute_spec (get_attribute_name (a));
	  if (!as || as->affects_type_identity == false)
	    continue;

	  if (!lookup_attribute (as->name, CONST_CAST_TREE (a1)))
	    break;
	  /* We don't need to compare trees again, as we did this
	     already in first loop.  */
	}
      /* All types - affecting identity - are equal, so
         there is no need to call target hook for comparison.  */
      if (!a)
        return 1;
    }
  if (lookup_attribute ("transaction_safe", CONST_CAST_TREE (a)))
    return 0;
  /* As some type combinations - like default calling-convention - might
     be compatible, we have to call the target hook to get the final result.  */
  return targetm.comp_type_attributes (type1, type2);
}

/* Return a type like TTYPE except that its TYPE_ATTRIBUTE
   is ATTRIBUTE.

   Record such modified types already made so we don't make duplicates.  */

tree
build_type_attribute_variant (tree ttype, tree attribute)
{
  return build_type_attribute_qual_variant (ttype, attribute,
					    TYPE_QUALS (ttype));
}


/* Reset the expression *EXPR_P, a size or position.

   ??? We could reset all non-constant sizes or positions.  But it's cheap
   enough to not do so and refrain from adding workarounds to dwarf2out.c.

   We need to reset self-referential sizes or positions because they cannot
   be gimplified and thus can contain a CALL_EXPR after the gimplification
   is finished, which will run afoul of LTO streaming.  And they need to be
   reset to something essentially dummy but not constant, so as to preserve
   the properties of the object they are attached to.  */

static inline void
free_lang_data_in_one_sizepos (tree *expr_p)
{
  tree expr = *expr_p;
  if (CONTAINS_PLACEHOLDER_P (expr))
    *expr_p = build0 (PLACEHOLDER_EXPR, TREE_TYPE (expr));
}


/* Reset all the fields in a binfo node BINFO.  We only keep
   BINFO_VTABLE, which is used by gimple_fold_obj_type_ref.  */

static void
free_lang_data_in_binfo (tree binfo)
{
  unsigned i;
  tree t;

  gcc_assert (TREE_CODE (binfo) == TREE_BINFO);

  BINFO_VIRTUALS (binfo) = NULL_TREE;
  BINFO_BASE_ACCESSES (binfo) = NULL;
  BINFO_INHERITANCE_CHAIN (binfo) = NULL_TREE;
  BINFO_SUBVTT_INDEX (binfo) = NULL_TREE;

  FOR_EACH_VEC_ELT (*BINFO_BASE_BINFOS (binfo), i, t)
    free_lang_data_in_binfo (t);
}


/* Reset all language specific information still present in TYPE.  */

static void
free_lang_data_in_type (tree type)
{
  gcc_assert (TYPE_P (type));

  /* Give the FE a chance to remove its own data first.  */
  lang_hooks.free_lang_data (type);

  TREE_LANG_FLAG_0 (type) = 0;
  TREE_LANG_FLAG_1 (type) = 0;
  TREE_LANG_FLAG_2 (type) = 0;
  TREE_LANG_FLAG_3 (type) = 0;
  TREE_LANG_FLAG_4 (type) = 0;
  TREE_LANG_FLAG_5 (type) = 0;
  TREE_LANG_FLAG_6 (type) = 0;

  if (TREE_CODE (type) == FUNCTION_TYPE)
    {
      /* Remove the const and volatile qualifiers from arguments.  The
	 C++ front end removes them, but the C front end does not,
	 leading to false ODR violation errors when merging two
	 instances of the same function signature compiled by
	 different front ends.  */
      tree p;

      for (p = TYPE_ARG_TYPES (type); p; p = TREE_CHAIN (p))
	{
	  tree arg_type = TREE_VALUE (p);

	  if (TYPE_READONLY (arg_type) || TYPE_VOLATILE (arg_type))
	    {
	      int quals = TYPE_QUALS (arg_type)
			  & ~TYPE_QUAL_CONST
			  & ~TYPE_QUAL_VOLATILE;
	      TREE_VALUE (p) = build_qualified_type (arg_type, quals);
	      free_lang_data_in_type (TREE_VALUE (p));
	    }
	  /* C++ FE uses TREE_PURPOSE to store initial values.  */
	  TREE_PURPOSE (p) = NULL;
	}
      /* Java uses TYPE_MINVAL for TYPE_ARGUMENT_SIGNATURE.  */
      TYPE_MINVAL (type) = NULL;
    }
  if (TREE_CODE (type) == METHOD_TYPE)
    {
      tree p;

      for (p = TYPE_ARG_TYPES (type); p; p = TREE_CHAIN (p))
	{
	  /* C++ FE uses TREE_PURPOSE to store initial values.  */
	  TREE_PURPOSE (p) = NULL;
	}
      /* Java uses TYPE_MINVAL for TYPE_ARGUMENT_SIGNATURE.  */
      TYPE_MINVAL (type) = NULL;
    }

  /* Remove members that are not actually FIELD_DECLs from the field
     list of an aggregate.  These occur in C++.  */
  if (RECORD_OR_UNION_TYPE_P (type))
    {
      tree prev, member;

      /* Note that TYPE_FIELDS can be shared across distinct
	 TREE_TYPEs.  Therefore, if the first field of TYPE_FIELDS is
	 to be removed, we cannot set its TREE_CHAIN to NULL.
	 Otherwise, we would not be able to find all the other fields
	 in the other instances of this TREE_TYPE.

	 This was causing an ICE in testsuite/g++.dg/lto/20080915.C.  */
      prev = NULL_TREE;
      member = TYPE_FIELDS (type);
      while (member)
	{
	  if (TREE_CODE (member) == FIELD_DECL
	      || (TREE_CODE (member) == TYPE_DECL
		  && !DECL_IGNORED_P (member)
		  && debug_info_level > DINFO_LEVEL_TERSE
		  && !is_redundant_typedef (member)))
	    {
	      if (prev)
		TREE_CHAIN (prev) = member;
	      else
		TYPE_FIELDS (type) = member;
	      prev = member;
	    }

	  member = TREE_CHAIN (member);
	}

      if (prev)
	TREE_CHAIN (prev) = NULL_TREE;
      else
	TYPE_FIELDS (type) = NULL_TREE;

      /* FIXME: C FE uses TYPE_VFIELD to record C_TYPE_INCOMPLETE_VARS
 	 and danagle the pointer from time to time.  */
      if (TYPE_VFIELD (type) && TREE_CODE (TYPE_VFIELD (type)) != FIELD_DECL)
        TYPE_VFIELD (type) = NULL_TREE;

      /* Remove TYPE_METHODS list.  While it would be nice to keep it
 	 to enable ODR warnings about different method lists, doing so
	 seems to impractically increase size of LTO data streamed.
	 Keep the information if TYPE_METHODS was non-NULL. This is used
	 by function.c and pretty printers.  */
      if (TYPE_METHODS (type))
        TYPE_METHODS (type) = error_mark_node;
      if (TYPE_BINFO (type))
	{
	  free_lang_data_in_binfo (TYPE_BINFO (type));
	  /* We need to preserve link to bases and virtual table for all
	     polymorphic types to make devirtualization machinery working.
	     Debug output cares only about bases, but output also
	     virtual table pointers so merging of -fdevirtualize and
	     -fno-devirtualize units is easier.  */
	  if ((!BINFO_VTABLE (TYPE_BINFO (type))
	       || !flag_devirtualize)
	      && ((!BINFO_N_BASE_BINFOS (TYPE_BINFO (type))
		   && !BINFO_VTABLE (TYPE_BINFO (type)))
		  || debug_info_level != DINFO_LEVEL_NONE))
	    TYPE_BINFO (type) = NULL;
	}
    }
  else
    {
      /* For non-aggregate types, clear out the language slot (which
	 overloads TYPE_BINFO).  */
      TYPE_LANG_SLOT_1 (type) = NULL_TREE;

      if (INTEGRAL_TYPE_P (type)
	  || SCALAR_FLOAT_TYPE_P (type)
	  || FIXED_POINT_TYPE_P (type))
	{
	  free_lang_data_in_one_sizepos (&TYPE_MIN_VALUE (type));
	  free_lang_data_in_one_sizepos (&TYPE_MAX_VALUE (type));
	}
    }

  free_lang_data_in_one_sizepos (&TYPE_SIZE (type));
  free_lang_data_in_one_sizepos (&TYPE_SIZE_UNIT (type));

  if (TYPE_CONTEXT (type)
      && TREE_CODE (TYPE_CONTEXT (type)) == BLOCK)
    {
      tree ctx = TYPE_CONTEXT (type);
      do
	{
	  ctx = BLOCK_SUPERCONTEXT (ctx);
	}
      while (ctx && TREE_CODE (ctx) == BLOCK);
      TYPE_CONTEXT (type) = ctx;
    }
}


/* Return true if DECL may need an assembler name to be set.  */

static inline bool
need_assembler_name_p (tree decl)
{
  /* We use DECL_ASSEMBLER_NAME to hold mangled type names for One Definition
     Rule merging.  This makes type_odr_p to return true on those types during
     LTO and by comparing the mangled name, we can say what types are intended
     to be equivalent across compilation unit.

     We do not store names of type_in_anonymous_namespace_p.

     Record, union and enumeration type have linkage that allows use
     to check type_in_anonymous_namespace_p. We do not mangle compound types
     that always can be compared structurally.

     Similarly for builtin types, we compare properties of their main variant.
     A special case are integer types where mangling do make differences
     between char/signed char/unsigned char etc.  Storing name for these makes
     e.g.  -fno-signed-char/-fsigned-char mismatches to be handled well.
     See cp/mangle.c:write_builtin_type for details.  */

  if (flag_lto_odr_type_mering
      && TREE_CODE (decl) == TYPE_DECL
      && DECL_NAME (decl)
      && decl == TYPE_NAME (TREE_TYPE (decl))
      && TYPE_MAIN_VARIANT (TREE_TYPE (decl)) == TREE_TYPE (decl)
      && !TYPE_ARTIFICIAL (TREE_TYPE (decl))
      && (type_with_linkage_p (TREE_TYPE (decl))
	  || TREE_CODE (TREE_TYPE (decl)) == INTEGER_TYPE)
      && !variably_modified_type_p (TREE_TYPE (decl), NULL_TREE))
    return !DECL_ASSEMBLER_NAME_SET_P (decl);
  /* Only FUNCTION_DECLs and VAR_DECLs are considered.  */
  if (TREE_CODE (decl) != FUNCTION_DECL
      && TREE_CODE (decl) != VAR_DECL)
    return false;

  /* If DECL already has its assembler name set, it does not need a
     new one.  */
  if (!HAS_DECL_ASSEMBLER_NAME_P (decl)
      || DECL_ASSEMBLER_NAME_SET_P (decl))
    return false;

  /* Abstract decls do not need an assembler name.  */
  if (DECL_ABSTRACT_P (decl))
    return false;

  /* For VAR_DECLs, only static, public and external symbols need an
     assembler name.  */
  if (TREE_CODE (decl) == VAR_DECL
      && !TREE_STATIC (decl)
      && !TREE_PUBLIC (decl)
      && !DECL_EXTERNAL (decl))
    return false;

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* Do not set assembler name on builtins.  Allow RTL expansion to
	 decide whether to expand inline or via a regular call.  */
      if (DECL_BUILT_IN (decl)
	  && DECL_BUILT_IN_CLASS (decl) != BUILT_IN_FRONTEND)
	return false;

      /* Functions represented in the callgraph need an assembler name.  */
      if (cgraph_node::get (decl) != NULL)
	return true;

      /* Unused and not public functions don't need an assembler name.  */
      if (!TREE_USED (decl) && !TREE_PUBLIC (decl))
	return false;
    }

  return true;
}


/* Reset all language specific information still present in symbol
   DECL.  */

static void
free_lang_data_in_decl (tree decl)
{
  gcc_assert (DECL_P (decl));

  /* Give the FE a chance to remove its own data first.  */
  lang_hooks.free_lang_data (decl);

  TREE_LANG_FLAG_0 (decl) = 0;
  TREE_LANG_FLAG_1 (decl) = 0;
  TREE_LANG_FLAG_2 (decl) = 0;
  TREE_LANG_FLAG_3 (decl) = 0;
  TREE_LANG_FLAG_4 (decl) = 0;
  TREE_LANG_FLAG_5 (decl) = 0;
  TREE_LANG_FLAG_6 (decl) = 0;

  free_lang_data_in_one_sizepos (&DECL_SIZE (decl));
  free_lang_data_in_one_sizepos (&DECL_SIZE_UNIT (decl));
  if (TREE_CODE (decl) == FIELD_DECL)
    {
      free_lang_data_in_one_sizepos (&DECL_FIELD_OFFSET (decl));
      if (TREE_CODE (DECL_CONTEXT (decl)) == QUAL_UNION_TYPE)
	DECL_QUALIFIER (decl) = NULL_TREE;
    }

 if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      struct cgraph_node *node;
      if (!(node = cgraph_node::get (decl))
	  || (!node->definition && !node->clones))
	{
	  if (node)
	    node->release_body ();
	  else
	    {
	      release_function_body (decl);
	      DECL_ARGUMENTS (decl) = NULL;
	      DECL_RESULT (decl) = NULL;
	      DECL_INITIAL (decl) = error_mark_node;
	    }
	}
      if (gimple_has_body_p (decl) || (node && node->thunk.thunk_p))
	{
	  tree t;

	  /* If DECL has a gimple body, then the context for its
	     arguments must be DECL.  Otherwise, it doesn't really
	     matter, as we will not be emitting any code for DECL.  In
	     general, there may be other instances of DECL created by
	     the front end and since PARM_DECLs are generally shared,
	     their DECL_CONTEXT changes as the replicas of DECL are
	     created.  The only time where DECL_CONTEXT is important
	     is for the FUNCTION_DECLs that have a gimple body (since
	     the PARM_DECL will be used in the function's body).  */
	  for (t = DECL_ARGUMENTS (decl); t; t = TREE_CHAIN (t))
	    DECL_CONTEXT (t) = decl;
	  if (!DECL_FUNCTION_SPECIFIC_TARGET (decl))
	    DECL_FUNCTION_SPECIFIC_TARGET (decl)
	      = target_option_default_node;
	  if (!DECL_FUNCTION_SPECIFIC_OPTIMIZATION (decl))
	    DECL_FUNCTION_SPECIFIC_OPTIMIZATION (decl)
	      = optimization_default_node;
	}

      /* DECL_SAVED_TREE holds the GENERIC representation for DECL.
	 At this point, it is not needed anymore.  */
      DECL_SAVED_TREE (decl) = NULL_TREE;

      /* Clear the abstract origin if it refers to a method.  Otherwise
         dwarf2out.c will ICE as we clear TYPE_METHODS and thus the
	 origin will not be output correctly.  */
      if (DECL_ABSTRACT_ORIGIN (decl)
	  && DECL_CONTEXT (DECL_ABSTRACT_ORIGIN (decl))
	  && RECORD_OR_UNION_TYPE_P
	       (DECL_CONTEXT (DECL_ABSTRACT_ORIGIN (decl))))
	DECL_ABSTRACT_ORIGIN (decl) = NULL_TREE;

      /* Sometimes the C++ frontend doesn't manage to transform a temporary
         DECL_VINDEX referring to itself into a vtable slot number as it
	 should.  Happens with functions that are copied and then forgotten
	 about.  Just clear it, it won't matter anymore.  */
      if (DECL_VINDEX (decl) && !tree_fits_shwi_p (DECL_VINDEX (decl)))
	DECL_VINDEX (decl) = NULL_TREE;
    }
  else if (TREE_CODE (decl) == VAR_DECL)
    {
      if ((DECL_EXTERNAL (decl)
	   && (!TREE_STATIC (decl) || !TREE_READONLY (decl)))
	  || (decl_function_context (decl) && !TREE_STATIC (decl)))
	DECL_INITIAL (decl) = NULL_TREE;
    }
  else if (TREE_CODE (decl) == TYPE_DECL)
    {
      DECL_VISIBILITY (decl) = VISIBILITY_DEFAULT;
      DECL_VISIBILITY_SPECIFIED (decl) = 0;
      DECL_INITIAL (decl) = NULL_TREE;
    }
  else if (TREE_CODE (decl) == FIELD_DECL)
    DECL_INITIAL (decl) = NULL_TREE;
  else if (TREE_CODE (decl) == TRANSLATION_UNIT_DECL
           && DECL_INITIAL (decl)
           && TREE_CODE (DECL_INITIAL (decl)) == BLOCK)
    {
      /* Strip builtins from the translation-unit BLOCK.  We still have targets
	 without builtin_decl_explicit support and also builtins are shared
	 nodes and thus we can't use TREE_CHAIN in multiple lists.  */
      tree *nextp = &BLOCK_VARS (DECL_INITIAL (decl));
      while (*nextp)
        {
          tree var = *nextp;
          if (TREE_CODE (var) == FUNCTION_DECL
              && DECL_BUILT_IN (var))
	    *nextp = TREE_CHAIN (var);
	  else
	    nextp = &TREE_CHAIN (var);
        }
    }
}


/* Data used when collecting DECLs and TYPEs for language data removal.  */

struct free_lang_data_d
{
  /* Worklist to avoid excessive recursion.  */
  vec<tree> worklist;

  /* Set of traversed objects.  Used to avoid duplicate visits.  */
  hash_set<tree> *pset;

  /* Array of symbols to process with free_lang_data_in_decl.  */
  vec<tree> decls;

  /* Array of types to process with free_lang_data_in_type.  */
  vec<tree> types;
};


/* Save all language fields needed to generate proper debug information
   for DECL.  This saves most fields cleared out by free_lang_data_in_decl.  */

static void
save_debug_info_for_decl (tree t)
{
  /*struct saved_debug_info_d *sdi;*/

  gcc_assert (debug_info_level > DINFO_LEVEL_TERSE && t && DECL_P (t));

  /* FIXME.  Partial implementation for saving debug info removed.  */
}


/* Save all language fields needed to generate proper debug information
   for TYPE.  This saves most fields cleared out by free_lang_data_in_type.  */

static void
save_debug_info_for_type (tree t)
{
  /*struct saved_debug_info_d *sdi;*/

  gcc_assert (debug_info_level > DINFO_LEVEL_TERSE && t && TYPE_P (t));

  /* FIXME.  Partial implementation for saving debug info removed.  */
}


/* Add type or decl T to one of the list of tree nodes that need their
   language data removed.  The lists are held inside FLD.  */

static void
add_tree_to_fld_list (tree t, struct free_lang_data_d *fld)
{
  if (DECL_P (t))
    {
      fld->decls.safe_push (t);
      if (debug_info_level > DINFO_LEVEL_TERSE)
	save_debug_info_for_decl (t);
    }
  else if (TYPE_P (t))
    {
      fld->types.safe_push (t);
      if (debug_info_level > DINFO_LEVEL_TERSE)
	save_debug_info_for_type (t);
    }
  else
    gcc_unreachable ();
}

/* Push tree node T into FLD->WORKLIST.  */

static inline void
fld_worklist_push (tree t, struct free_lang_data_d *fld)
{
  if (t && !is_lang_specific (t) && !fld->pset->contains (t))
    fld->worklist.safe_push ((t));
}


/* Operand callback helper for free_lang_data_in_node.  *TP is the
   subtree operand being considered.  */

static tree
find_decls_types_r (tree *tp, int *ws, void *data)
{
  tree t = *tp;
  struct free_lang_data_d *fld = (struct free_lang_data_d *) data;

  if (TREE_CODE (t) == TREE_LIST)
    return NULL_TREE;

  /* Language specific nodes will be removed, so there is no need
     to gather anything under them.  */
  if (is_lang_specific (t))
    {
      *ws = 0;
      return NULL_TREE;
    }

  if (DECL_P (t))
    {
      /* Note that walk_tree does not traverse every possible field in
	 decls, so we have to do our own traversals here.  */
      add_tree_to_fld_list (t, fld);

      fld_worklist_push (DECL_NAME (t), fld);
      fld_worklist_push (DECL_CONTEXT (t), fld);
      fld_worklist_push (DECL_SIZE (t), fld);
      fld_worklist_push (DECL_SIZE_UNIT (t), fld);

      /* We are going to remove everything under DECL_INITIAL for
	 TYPE_DECLs.  No point walking them.  */
      if (TREE_CODE (t) != TYPE_DECL)
	fld_worklist_push (DECL_INITIAL (t), fld);

      fld_worklist_push (DECL_ATTRIBUTES (t), fld);
      fld_worklist_push (DECL_ABSTRACT_ORIGIN (t), fld);

      if (TREE_CODE (t) == FUNCTION_DECL)
	{
	  fld_worklist_push (DECL_ARGUMENTS (t), fld);
	  fld_worklist_push (DECL_RESULT (t), fld);
	}
      else if (TREE_CODE (t) == TYPE_DECL)
	{
	  fld_worklist_push (DECL_ORIGINAL_TYPE (t), fld);
	}
      else if (TREE_CODE (t) == FIELD_DECL)
	{
	  fld_worklist_push (DECL_FIELD_OFFSET (t), fld);
	  fld_worklist_push (DECL_BIT_FIELD_TYPE (t), fld);
	  fld_worklist_push (DECL_FIELD_BIT_OFFSET (t), fld);
	  fld_worklist_push (DECL_FCONTEXT (t), fld);
	}

      if ((TREE_CODE (t) == VAR_DECL || TREE_CODE (t) == PARM_DECL)
	  && DECL_HAS_VALUE_EXPR_P (t))
	fld_worklist_push (DECL_VALUE_EXPR (t), fld);

      if (TREE_CODE (t) != FIELD_DECL
	  && TREE_CODE (t) != TYPE_DECL)
	fld_worklist_push (TREE_CHAIN (t), fld);
      *ws = 0;
    }
  else if (TYPE_P (t))
    {
      /* Note that walk_tree does not traverse every possible field in
	 types, so we have to do our own traversals here.  */
      add_tree_to_fld_list (t, fld);

      if (!RECORD_OR_UNION_TYPE_P (t))
	fld_worklist_push (TYPE_CACHED_VALUES (t), fld);
      fld_worklist_push (TYPE_SIZE (t), fld);
      fld_worklist_push (TYPE_SIZE_UNIT (t), fld);
      fld_worklist_push (TYPE_ATTRIBUTES (t), fld);
      fld_worklist_push (TYPE_POINTER_TO (t), fld);
      fld_worklist_push (TYPE_REFERENCE_TO (t), fld);
      fld_worklist_push (TYPE_NAME (t), fld);
      /* Do not walk TYPE_NEXT_PTR_TO or TYPE_NEXT_REF_TO.  We do not stream
	 them and thus do not and want not to reach unused pointer types
	 this way.  */
      if (!POINTER_TYPE_P (t))
	fld_worklist_push (TYPE_MINVAL (t), fld);
      if (!RECORD_OR_UNION_TYPE_P (t))
	fld_worklist_push (TYPE_MAXVAL (t), fld);
      fld_worklist_push (TYPE_MAIN_VARIANT (t), fld);
      /* Do not walk TYPE_NEXT_VARIANT.  We do not stream it and thus
         do not and want not to reach unused variants this way.  */
      if (TYPE_CONTEXT (t))
	{
	  tree ctx = TYPE_CONTEXT (t);
	  /* We adjust BLOCK TYPE_CONTEXTs to the innermost non-BLOCK one.
	     So push that instead.  */
	  while (ctx && TREE_CODE (ctx) == BLOCK)
	    ctx = BLOCK_SUPERCONTEXT (ctx);
	  fld_worklist_push (ctx, fld);
	}
      /* Do not walk TYPE_CANONICAL.  We do not stream it and thus do not
	 and want not to reach unused types this way.  */

      if (RECORD_OR_UNION_TYPE_P (t) && TYPE_BINFO (t))
	{
	  unsigned i;
	  tree tem;
	  FOR_EACH_VEC_ELT (*BINFO_BASE_BINFOS (TYPE_BINFO (t)), i, tem)
	    fld_worklist_push (TREE_TYPE (tem), fld);
	  tem = BINFO_VIRTUALS (TYPE_BINFO (t));
	  if (tem
	      /* The Java FE overloads BINFO_VIRTUALS for its own purpose.  */
	      && TREE_CODE (tem) == TREE_LIST)
	    do
	      {
		fld_worklist_push (TREE_VALUE (tem), fld);
		tem = TREE_CHAIN (tem);
	      }
	    while (tem);
	}
      if (RECORD_OR_UNION_TYPE_P (t))
	{
	  tree tem;
	  /* Push all TYPE_FIELDS - there can be interleaving interesting
	     and non-interesting things.  */
	  tem = TYPE_FIELDS (t);
	  while (tem)
	    {
	      if (TREE_CODE (tem) == FIELD_DECL
		  || (TREE_CODE (tem) == TYPE_DECL
		      && !DECL_IGNORED_P (tem)
		      && debug_info_level > DINFO_LEVEL_TERSE
		      && !is_redundant_typedef (tem)))
		fld_worklist_push (tem, fld);
	      tem = TREE_CHAIN (tem);
	    }
	}

      fld_worklist_push (TYPE_STUB_DECL (t), fld);
      *ws = 0;
    }
  else if (TREE_CODE (t) == BLOCK)
    {
      tree tem;
      for (tem = BLOCK_VARS (t); tem; tem = TREE_CHAIN (tem))
	fld_worklist_push (tem, fld);
      for (tem = BLOCK_SUBBLOCKS (t); tem; tem = BLOCK_CHAIN (tem))
	fld_worklist_push (tem, fld);
      fld_worklist_push (BLOCK_ABSTRACT_ORIGIN (t), fld);
    }

  if (TREE_CODE (t) != IDENTIFIER_NODE
      && CODE_CONTAINS_STRUCT (TREE_CODE (t), TS_TYPED))
    fld_worklist_push (TREE_TYPE (t), fld);

  return NULL_TREE;
}


/* Find decls and types in T.  */

static void
find_decls_types (tree t, struct free_lang_data_d *fld)
{
  while (1)
    {
      if (!fld->pset->contains (t))
	walk_tree (&t, find_decls_types_r, fld, fld->pset);
      if (fld->worklist.is_empty ())
	break;
      t = fld->worklist.pop ();
    }
}

/* Translate all the types in LIST with the corresponding runtime
   types.  */

static tree
get_eh_types_for_runtime (tree list)
{
  tree head, prev;

  if (list == NULL_TREE)
    return NULL_TREE;

  head = build_tree_list (0, lookup_type_for_runtime (TREE_VALUE (list)));
  prev = head;
  list = TREE_CHAIN (list);
  while (list)
    {
      tree n = build_tree_list (0, lookup_type_for_runtime (TREE_VALUE (list)));
      TREE_CHAIN (prev) = n;
      prev = TREE_CHAIN (prev);
      list = TREE_CHAIN (list);
    }

  return head;
}


/* Find decls and types referenced in EH region R and store them in
   FLD->DECLS and FLD->TYPES.  */

static void
find_decls_types_in_eh_region (eh_region r, struct free_lang_data_d *fld)
{
  switch (r->type)
    {
    case ERT_CLEANUP:
      break;

    case ERT_TRY:
      {
	eh_catch c;

	/* The types referenced in each catch must first be changed to the
	   EH types used at runtime.  This removes references to FE types
	   in the region.  */
	for (c = r->u.eh_try.first_catch; c ; c = c->next_catch)
	  {
	    c->type_list = get_eh_types_for_runtime (c->type_list);
	    walk_tree (&c->type_list, find_decls_types_r, fld, fld->pset);
	  }
      }
      break;

    case ERT_ALLOWED_EXCEPTIONS:
      r->u.allowed.type_list
	= get_eh_types_for_runtime (r->u.allowed.type_list);
      walk_tree (&r->u.allowed.type_list, find_decls_types_r, fld, fld->pset);
      break;

    case ERT_MUST_NOT_THROW:
      walk_tree (&r->u.must_not_throw.failure_decl,
		 find_decls_types_r, fld, fld->pset);
      break;
    }
}


/* Find decls and types referenced in cgraph node N and store them in
   FLD->DECLS and FLD->TYPES.  Unlike pass_referenced_vars, this will
   look for *every* kind of DECL and TYPE node reachable from N,
   including those embedded inside types and decls (i.e,, TYPE_DECLs,
   NAMESPACE_DECLs, etc).  */

static void
find_decls_types_in_node (struct cgraph_node *n, struct free_lang_data_d *fld)
{
  basic_block bb;
  struct function *fn;
  unsigned ix;
  tree t;

  find_decls_types (n->decl, fld);

  if (!gimple_has_body_p (n->decl))
    return;

  gcc_assert (current_function_decl == NULL_TREE && cfun == NULL);

  fn = DECL_STRUCT_FUNCTION (n->decl);

  /* Traverse locals. */
  FOR_EACH_LOCAL_DECL (fn, ix, t)
    find_decls_types (t, fld);

  /* Traverse EH regions in FN.  */
  {
    eh_region r;
    FOR_ALL_EH_REGION_FN (r, fn)
      find_decls_types_in_eh_region (r, fld);
  }

  /* Traverse every statement in FN.  */
  FOR_EACH_BB_FN (bb, fn)
    {
      gphi_iterator psi;
      gimple_stmt_iterator si;
      unsigned i;

      for (psi = gsi_start_phis (bb); !gsi_end_p (psi); gsi_next (&psi))
	{
	  gphi *phi = psi.phi ();

	  for (i = 0; i < gimple_phi_num_args (phi); i++)
	    {
	      tree *arg_p = gimple_phi_arg_def_ptr (phi, i);
	      find_decls_types (*arg_p, fld);
	    }
	}

      for (si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next (&si))
	{
	  gimple *stmt = gsi_stmt (si);

	  if (is_gimple_call (stmt))
	    find_decls_types (gimple_call_fntype (stmt), fld);

	  for (i = 0; i < gimple_num_ops (stmt); i++)
	    {
	      tree arg = gimple_op (stmt, i);
	      find_decls_types (arg, fld);
	    }
	}
    }
}


/* Find decls and types referenced in varpool node N and store them in
   FLD->DECLS and FLD->TYPES.  Unlike pass_referenced_vars, this will
   look for *every* kind of DECL and TYPE node reachable from N,
   including those embedded inside types and decls (i.e,, TYPE_DECLs,
   NAMESPACE_DECLs, etc).  */

static void
find_decls_types_in_var (varpool_node *v, struct free_lang_data_d *fld)
{
  find_decls_types (v->decl, fld);
}

/* If T needs an assembler name, have one created for it.  */

void
assign_assembler_name_if_neeeded (tree t)
{
  if (need_assembler_name_p (t))
    {
      /* When setting DECL_ASSEMBLER_NAME, the C++ mangler may emit
	 diagnostics that use input_location to show locus
	 information.  The problem here is that, at this point,
	 input_location is generally anchored to the end of the file
	 (since the parser is long gone), so we don't have a good
	 position to pin it to.

	 To alleviate this problem, this uses the location of T's
	 declaration.  Examples of this are
	 testsuite/g++.dg/template/cond2.C and
	 testsuite/g++.dg/template/pr35240.C.  */
      location_t saved_location = input_location;
      input_location = DECL_SOURCE_LOCATION (t);

      decl_assembler_name (t);

      input_location = saved_location;
    }
}


/* Free language specific information for every operand and expression
   in every node of the call graph.  This process operates in three stages:

   1- Every callgraph node and varpool node is traversed looking for
      decls and types embedded in them.  This is a more exhaustive
      search than that done by find_referenced_vars, because it will
      also collect individual fields, decls embedded in types, etc.

   2- All the decls found are sent to free_lang_data_in_decl.

   3- All the types found are sent to free_lang_data_in_type.

   The ordering between decls and types is important because
   free_lang_data_in_decl sets assembler names, which includes
   mangling.  So types cannot be freed up until assembler names have
   been set up.  */

static void
free_lang_data_in_cgraph (void)
{
  struct cgraph_node *n;
  varpool_node *v;
  struct free_lang_data_d fld;
  tree t;
  unsigned i;
  alias_pair *p;

  /* Initialize sets and arrays to store referenced decls and types.  */
  fld.pset = new hash_set<tree>;
  fld.worklist.create (0);
  fld.decls.create (100);
  fld.types.create (100);

  /* Find decls and types in the body of every function in the callgraph.  */
  FOR_EACH_FUNCTION (n)
    find_decls_types_in_node (n, &fld);

  FOR_EACH_VEC_SAFE_ELT (alias_pairs, i, p)
    find_decls_types (p->decl, &fld);

  /* Find decls and types in every varpool symbol.  */
  FOR_EACH_VARIABLE (v)
    find_decls_types_in_var (v, &fld);

  /* Set the assembler name on every decl found.  We need to do this
     now because free_lang_data_in_decl will invalidate data needed
     for mangling.  This breaks mangling on interdependent decls.  */
  FOR_EACH_VEC_ELT (fld.decls, i, t)
    assign_assembler_name_if_neeeded (t);

  /* Traverse every decl found freeing its language data.  */
  FOR_EACH_VEC_ELT (fld.decls, i, t)
    free_lang_data_in_decl (t);

  /* Traverse every type found freeing its language data.  */
  FOR_EACH_VEC_ELT (fld.types, i, t)
    free_lang_data_in_type (t);
  if (flag_checking)
    {
      FOR_EACH_VEC_ELT (fld.types, i, t)
	verify_type (t);
    }

  delete fld.pset;
  fld.worklist.release ();
  fld.decls.release ();
  fld.types.release ();
}


/* Free resources that are used by FE but are not needed once they are done. */

static unsigned
free_lang_data (void)
{
  unsigned i;

  /* If we are the LTO frontend we have freed lang-specific data already.  */
  if (in_lto_p
      || (!flag_generate_lto && !flag_generate_offload))
    return 0;

  /* Allocate and assign alias sets to the standard integer types
     while the slots are still in the way the frontends generated them.  */
  for (i = 0; i < itk_none; ++i)
    if (integer_types[i])
      TYPE_ALIAS_SET (integer_types[i]) = get_alias_set (integer_types[i]);

  /* Traverse the IL resetting language specific information for
     operands, expressions, etc.  */
  free_lang_data_in_cgraph ();

  /* Create gimple variants for common types.  */
  ptrdiff_type_node = integer_type_node;
  fileptr_type_node = ptr_type_node;

  /* Reset some langhooks.  Do not reset types_compatible_p, it may
     still be used indirectly via the get_alias_set langhook.  */
  lang_hooks.dwarf_name = lhd_dwarf_name;
  lang_hooks.decl_printable_name = gimple_decl_printable_name;
  lang_hooks.gimplify_expr = lhd_gimplify_expr;

  /* We do not want the default decl_assembler_name implementation,
     rather if we have fixed everything we want a wrapper around it
     asserting that all non-local symbols already got their assembler
     name and only produce assembler names for local symbols.  Or rather
     make sure we never call decl_assembler_name on local symbols and
     devise a separate, middle-end private scheme for it.  */

  /* Reset diagnostic machinery.  */
  tree_diagnostics_defaults (global_dc);

  return 0;
}


namespace {

const pass_data pass_data_ipa_free_lang_data =
{
  SIMPLE_IPA_PASS, /* type */
  "*free_lang_data", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_FREE_LANG_DATA, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_free_lang_data : public simple_ipa_opt_pass
{
public:
  pass_ipa_free_lang_data (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_free_lang_data, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *) { return free_lang_data (); }

}; // class pass_ipa_free_lang_data

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_free_lang_data (gcc::context *ctxt)
{
  return new pass_ipa_free_lang_data (ctxt);
}

/* The backbone of is_attribute_p().  ATTR_LEN is the string length of
   ATTR_NAME.  Also used internally by remove_attribute().  */
bool
private_is_attribute_p (const char *attr_name, size_t attr_len, const_tree ident)
{
  size_t ident_len = IDENTIFIER_LENGTH (ident);

  if (ident_len == attr_len)
    {
      if (strcmp (attr_name, IDENTIFIER_POINTER (ident)) == 0)
	return true;
    }
  else if (ident_len == attr_len + 4)
    {
      /* There is the possibility that ATTR is 'text' and IDENT is
	 '__text__'.  */
      const char *p = IDENTIFIER_POINTER (ident);      
      if (p[0] == '_' && p[1] == '_'
	  && p[ident_len - 2] == '_' && p[ident_len - 1] == '_'
	  && strncmp (attr_name, p + 2, attr_len) == 0)
	return true;
    }

  return false;
}

/* The backbone of lookup_attribute().  ATTR_LEN is the string length
   of ATTR_NAME, and LIST is not NULL_TREE.  */
tree
private_lookup_attribute (const char *attr_name, size_t attr_len, tree list)
{
  while (list)
    {
      size_t ident_len = IDENTIFIER_LENGTH (get_attribute_name (list));

      if (ident_len == attr_len)
	{
	  if (!strcmp (attr_name,
		       IDENTIFIER_POINTER (get_attribute_name (list))))
	    break;
	}
      /* TODO: If we made sure that attributes were stored in the
	 canonical form without '__...__' (ie, as in 'text' as opposed
	 to '__text__') then we could avoid the following case.  */
      else if (ident_len == attr_len + 4)
	{
	  const char *p = IDENTIFIER_POINTER (get_attribute_name (list));
	  if (p[0] == '_' && p[1] == '_'
	      && p[ident_len - 2] == '_' && p[ident_len - 1] == '_'
	      && strncmp (attr_name, p + 2, attr_len) == 0)
	    break;
	}
      list = TREE_CHAIN (list);
    }

  return list;
}

/* Given an attribute name ATTR_NAME and a list of attributes LIST,
   return a pointer to the attribute's list first element if the attribute
   starts with ATTR_NAME. ATTR_NAME must be in the form 'text' (not
   '__text__').  */

tree
private_lookup_attribute_by_prefix (const char *attr_name, size_t attr_len,
				    tree list)
{
  while (list)
    {
      size_t ident_len = IDENTIFIER_LENGTH (get_attribute_name (list));

      if (attr_len > ident_len)
	{
	  list = TREE_CHAIN (list);
	  continue;
	}

      const char *p = IDENTIFIER_POINTER (get_attribute_name (list));

      if (strncmp (attr_name, p, attr_len) == 0)
	break;

      /* TODO: If we made sure that attributes were stored in the
	 canonical form without '__...__' (ie, as in 'text' as opposed
	 to '__text__') then we could avoid the following case.  */
      if (p[0] == '_' && p[1] == '_' &&
	  strncmp (attr_name, p + 2, attr_len) == 0)
	break;

      list = TREE_CHAIN (list);
    }

  return list;
}


/* A variant of lookup_attribute() that can be used with an identifier
   as the first argument, and where the identifier can be either
   'text' or '__text__'.

   Given an attribute ATTR_IDENTIFIER, and a list of attributes LIST,
   return a pointer to the attribute's list element if the attribute
   is part of the list, or NULL_TREE if not found.  If the attribute
   appears more than once, this only returns the first occurrence; the
   TREE_CHAIN of the return value should be passed back in if further
   occurrences are wanted.  ATTR_IDENTIFIER must be an identifier but
   can be in the form 'text' or '__text__'.  */
static tree
lookup_ident_attribute (tree attr_identifier, tree list)
{
  gcc_checking_assert (TREE_CODE (attr_identifier) == IDENTIFIER_NODE);

  while (list)
    {
      gcc_checking_assert (TREE_CODE (get_attribute_name (list))
			   == IDENTIFIER_NODE);

      if (cmp_attrib_identifiers (attr_identifier,
				  get_attribute_name (list)))
	/* Found it.  */
	break;
      list = TREE_CHAIN (list);
    }

  return list;
}

/* Remove any instances of attribute ATTR_NAME in LIST and return the
   modified list.  */

tree
remove_attribute (const char *attr_name, tree list)
{
  tree *p;
  size_t attr_len = strlen (attr_name);

  gcc_checking_assert (attr_name[0] != '_');

  for (p = &list; *p; )
    {
      tree l = *p;
      /* TODO: If we were storing attributes in normalized form, here
	 we could use a simple strcmp().  */
      if (private_is_attribute_p (attr_name, attr_len, get_attribute_name (l)))
	*p = TREE_CHAIN (l);
      else
	p = &TREE_CHAIN (l);
    }

  return list;
}

/* Return an attribute list that is the union of a1 and a2.  */

tree
merge_attributes (tree a1, tree a2)
{
  tree attributes;

  /* Either one unset?  Take the set one.  */

  if ((attributes = a1) == 0)
    attributes = a2;

  /* One that completely contains the other?  Take it.  */

  else if (a2 != 0 && ! attribute_list_contained (a1, a2))
    {
      if (attribute_list_contained (a2, a1))
	attributes = a2;
      else
	{
	  /* Pick the longest list, and hang on the other list.  */

	  if (list_length (a1) < list_length (a2))
	    attributes = a2, a2 = a1;

	  for (; a2 != 0; a2 = TREE_CHAIN (a2))
	    {
	      tree a;
	      for (a = lookup_ident_attribute (get_attribute_name (a2),
					       attributes);
		   a != NULL_TREE && !attribute_value_equal (a, a2);
		   a = lookup_ident_attribute (get_attribute_name (a2),
					       TREE_CHAIN (a)))
		;
	      if (a == NULL_TREE)
		{
		  a1 = copy_node (a2);
		  TREE_CHAIN (a1) = attributes;
		  attributes = a1;
		}
	    }
	}
    }
  return attributes;
}

/* Given types T1 and T2, merge their attributes and return
  the result.  */

tree
merge_type_attributes (tree t1, tree t2)
{
  return merge_attributes (TYPE_ATTRIBUTES (t1),
			   TYPE_ATTRIBUTES (t2));
}

/* Given decls OLDDECL and NEWDECL, merge their attributes and return
   the result.  */

tree
merge_decl_attributes (tree olddecl, tree newdecl)
{
  return merge_attributes (DECL_ATTRIBUTES (olddecl),
			   DECL_ATTRIBUTES (newdecl));
}

#if TARGET_DLLIMPORT_DECL_ATTRIBUTES

/* Specialization of merge_decl_attributes for various Windows targets.

   This handles the following situation:

     __declspec (dllimport) int foo;
     int foo;

   The second instance of `foo' nullifies the dllimport.  */

tree
merge_dllimport_decl_attributes (tree old, tree new_tree)
{
  tree a;
  int delete_dllimport_p = 1;

  /* What we need to do here is remove from `old' dllimport if it doesn't
     appear in `new'.  dllimport behaves like extern: if a declaration is
     marked dllimport and a definition appears later, then the object
     is not dllimport'd.  We also remove a `new' dllimport if the old list
     contains dllexport:  dllexport always overrides dllimport, regardless
     of the order of declaration.  */
  if (!VAR_OR_FUNCTION_DECL_P (new_tree))
    delete_dllimport_p = 0;
  else if (DECL_DLLIMPORT_P (new_tree)
     	   && lookup_attribute ("dllexport", DECL_ATTRIBUTES (old)))
    {
      DECL_DLLIMPORT_P (new_tree) = 0;
      warning (OPT_Wattributes, "%q+D already declared with dllexport attribute: "
	      "dllimport ignored", new_tree);
    }
  else if (DECL_DLLIMPORT_P (old) && !DECL_DLLIMPORT_P (new_tree))
    {
      /* Warn about overriding a symbol that has already been used, e.g.:
           extern int __attribute__ ((dllimport)) foo;
	   int* bar () {return &foo;}
	   int foo;
      */
      if (TREE_USED (old))
	{
	  warning (0, "%q+D redeclared without dllimport attribute "
		   "after being referenced with dll linkage", new_tree);
	  /* If we have used a variable's address with dllimport linkage,
	      keep the old DECL_DLLIMPORT_P flag: the ADDR_EXPR using the
	      decl may already have had TREE_CONSTANT computed.
	      We still remove the attribute so that assembler code refers
	      to '&foo rather than '_imp__foo'.  */
	  if (TREE_CODE (old) == VAR_DECL && TREE_ADDRESSABLE (old))
	    DECL_DLLIMPORT_P (new_tree) = 1;
	}

      /* Let an inline definition silently override the external reference,
	 but otherwise warn about attribute inconsistency.  */
      else if (TREE_CODE (new_tree) == VAR_DECL
	       || !DECL_DECLARED_INLINE_P (new_tree))
	warning (OPT_Wattributes, "%q+D redeclared without dllimport attribute: "
		  "previous dllimport ignored", new_tree);
    }
  else
    delete_dllimport_p = 0;

  a = merge_attributes (DECL_ATTRIBUTES (old), DECL_ATTRIBUTES (new_tree));

  if (delete_dllimport_p)
    a = remove_attribute ("dllimport", a);

  return a;
}

/* Handle a "dllimport" or "dllexport" attribute; arguments as in
   struct attribute_spec.handler.  */

tree
handle_dll_attribute (tree * pnode, tree name, tree args, int flags,
		      bool *no_add_attrs)
{
  tree node = *pnode;
  bool is_dllimport;

  /* These attributes may apply to structure and union types being created,
     but otherwise should pass to the declaration involved.  */
  if (!DECL_P (node))
    {
      if (flags & ((int) ATTR_FLAG_DECL_NEXT | (int) ATTR_FLAG_FUNCTION_NEXT
		   | (int) ATTR_FLAG_ARRAY_NEXT))
	{
	  *no_add_attrs = true;
	  return tree_cons (name, args, NULL_TREE);
	}
      if (TREE_CODE (node) == RECORD_TYPE
	  || TREE_CODE (node) == UNION_TYPE)
	{
	  node = TYPE_NAME (node);
	  if (!node)
	    return NULL_TREE;
	}
      else
	{
	  warning (OPT_Wattributes, "%qE attribute ignored",
		   name);
	  *no_add_attrs = true;
	  return NULL_TREE;
	}
    }

  if (TREE_CODE (node) != FUNCTION_DECL
      && TREE_CODE (node) != VAR_DECL
      && TREE_CODE (node) != TYPE_DECL)
    {
      *no_add_attrs = true;
      warning (OPT_Wattributes, "%qE attribute ignored",
	       name);
      return NULL_TREE;
    }

  if (TREE_CODE (node) == TYPE_DECL
      && TREE_CODE (TREE_TYPE (node)) != RECORD_TYPE
      && TREE_CODE (TREE_TYPE (node)) != UNION_TYPE)
    {
      *no_add_attrs = true;
      warning (OPT_Wattributes, "%qE attribute ignored",
	       name);
      return NULL_TREE;
    }

  is_dllimport = is_attribute_p ("dllimport", name);

  /* Report error on dllimport ambiguities seen now before they cause
     any damage.  */
  if (is_dllimport)
    {
      /* Honor any target-specific overrides. */
      if (!targetm.valid_dllimport_attribute_p (node))
	*no_add_attrs = true;

     else if (TREE_CODE (node) == FUNCTION_DECL
	        && DECL_DECLARED_INLINE_P (node))
	{
	  warning (OPT_Wattributes, "inline function %q+D declared as "
		  " dllimport: attribute ignored", node);
	  *no_add_attrs = true;
	}
      /* Like MS, treat definition of dllimported variables and
	 non-inlined functions on declaration as syntax errors. */
     else if (TREE_CODE (node) == FUNCTION_DECL && DECL_INITIAL (node))
	{
	  error ("function %q+D definition is marked dllimport", node);
	  *no_add_attrs = true;
	}

     else if (TREE_CODE (node) == VAR_DECL)
	{
	  if (DECL_INITIAL (node))
	    {
	      error ("variable %q+D definition is marked dllimport",
		     node);
	      *no_add_attrs = true;
	    }

	  /* `extern' needn't be specified with dllimport.
	     Specify `extern' now and hope for the best.  Sigh.  */
	  DECL_EXTERNAL (node) = 1;
	  /* Also, implicitly give dllimport'd variables declared within
	     a function global scope, unless declared static.  */
	  if (current_function_decl != NULL_TREE && !TREE_STATIC (node))
	    TREE_PUBLIC (node) = 1;
	}

      if (*no_add_attrs == false)
        DECL_DLLIMPORT_P (node) = 1;
    }
  else if (TREE_CODE (node) == FUNCTION_DECL
	   && DECL_DECLARED_INLINE_P (node)
	   && flag_keep_inline_dllexport)
    /* An exported function, even if inline, must be emitted.  */
    DECL_EXTERNAL (node) = 0;

  /*  Report error if symbol is not accessible at global scope.  */
  if (!TREE_PUBLIC (node)
      && (TREE_CODE (node) == VAR_DECL
	  || TREE_CODE (node) == FUNCTION_DECL))
    {
      error ("external linkage required for symbol %q+D because of "
	     "%qE attribute", node, name);
      *no_add_attrs = true;
    }

  /* A dllexport'd entity must have default visibility so that other
     program units (shared libraries or the main executable) can see
     it.  A dllimport'd entity must have default visibility so that
     the linker knows that undefined references within this program
     unit can be resolved by the dynamic linker.  */
  if (!*no_add_attrs)
    {
      if (DECL_VISIBILITY_SPECIFIED (node)
	  && DECL_VISIBILITY (node) != VISIBILITY_DEFAULT)
	error ("%qE implies default visibility, but %qD has already "
	       "been declared with a different visibility",
	       name, node);
      DECL_VISIBILITY (node) = VISIBILITY_DEFAULT;
      DECL_VISIBILITY_SPECIFIED (node) = 1;
    }

  return NULL_TREE;
}

#endif /* TARGET_DLLIMPORT_DECL_ATTRIBUTES  */

/* Set the type qualifiers for TYPE to TYPE_QUALS, which is a bitmask
   of the various TYPE_QUAL values.  */

static void
set_type_quals (tree type, int type_quals)
{
  TYPE_READONLY (type) = (type_quals & TYPE_QUAL_CONST) != 0;
  TYPE_VOLATILE (type) = (type_quals & TYPE_QUAL_VOLATILE) != 0;
  TYPE_RESTRICT (type) = (type_quals & TYPE_QUAL_RESTRICT) != 0;
  TYPE_ATOMIC (type) = (type_quals & TYPE_QUAL_ATOMIC) != 0;
  TYPE_ADDR_SPACE (type) = DECODE_QUAL_ADDR_SPACE (type_quals);
}

/* Returns true iff unqualified CAND and BASE are equivalent.  */

bool
check_base_type (const_tree cand, const_tree base)
{
  return (TYPE_NAME (cand) == TYPE_NAME (base)
	  /* Apparently this is needed for Objective-C.  */
	  && TYPE_CONTEXT (cand) == TYPE_CONTEXT (base)
	  /* Check alignment.  */
	  && TYPE_ALIGN (cand) == TYPE_ALIGN (base)
	  && attribute_list_equal (TYPE_ATTRIBUTES (cand),
				   TYPE_ATTRIBUTES (base)));
}

/* Returns true iff CAND is equivalent to BASE with TYPE_QUALS.  */

bool
check_qualified_type (const_tree cand, const_tree base, int type_quals)
{
  return (TYPE_QUALS (cand) == type_quals
	  && check_base_type (cand, base));
}

/* Returns true iff CAND is equivalent to BASE with ALIGN.  */

static bool
check_aligned_type (const_tree cand, const_tree base, unsigned int align)
{
  return (TYPE_QUALS (cand) == TYPE_QUALS (base)
	  && TYPE_NAME (cand) == TYPE_NAME (base)
	  /* Apparently this is needed for Objective-C.  */
	  && TYPE_CONTEXT (cand) == TYPE_CONTEXT (base)
	  /* Check alignment.  */
	  && TYPE_ALIGN (cand) == align
	  && attribute_list_equal (TYPE_ATTRIBUTES (cand),
				   TYPE_ATTRIBUTES (base)));
}

/* This function checks to see if TYPE matches the size one of the built-in 
   atomic types, and returns that core atomic type.  */

static tree
find_atomic_core_type (tree type)
{
  tree base_atomic_type;

  /* Only handle complete types.  */
  if (TYPE_SIZE (type) == NULL_TREE)
    return NULL_TREE;

  HOST_WIDE_INT type_size = tree_to_uhwi (TYPE_SIZE (type));
  switch (type_size)
    {
    case 8:
      base_atomic_type = atomicQI_type_node;
      break;

    case 16:
      base_atomic_type = atomicHI_type_node;
      break;

    case 32:
      base_atomic_type = atomicSI_type_node;
      break;

    case 64:
      base_atomic_type = atomicDI_type_node;
      break;

    case 128:
      base_atomic_type = atomicTI_type_node;
      break;

    default:
      base_atomic_type = NULL_TREE;
    }

  return base_atomic_type;
}

/* Return a version of the TYPE, qualified as indicated by the
   TYPE_QUALS, if one exists.  If no qualified version exists yet,
   return NULL_TREE.  */

tree
get_qualified_type (tree type, int type_quals)
{
  tree t;

  if (TYPE_QUALS (type) == type_quals)
    return type;

  /* Search the chain of variants to see if there is already one there just
     like the one we need to have.  If so, use that existing one.  We must
     preserve the TYPE_NAME, since there is code that depends on this.  */
  for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
    if (check_qualified_type (t, type, type_quals))
      return t;

  return NULL_TREE;
}

/* Like get_qualified_type, but creates the type if it does not
   exist.  This function never returns NULL_TREE.  */

tree
build_qualified_type (tree type, int type_quals)
{
  tree t;

  /* See if we already have the appropriate qualified variant.  */
  t = get_qualified_type (type, type_quals);

  /* If not, build it.  */
  if (!t)
    {
      t = build_variant_type_copy (type);
      set_type_quals (t, type_quals);

      if (((type_quals & TYPE_QUAL_ATOMIC) == TYPE_QUAL_ATOMIC))
	{
	  /* See if this object can map to a basic atomic type.  */
	  tree atomic_type = find_atomic_core_type (type);
	  if (atomic_type)
	    {
	      /* Ensure the alignment of this type is compatible with
		 the required alignment of the atomic type.  */
	      if (TYPE_ALIGN (atomic_type) > TYPE_ALIGN (t))
		SET_TYPE_ALIGN (t, TYPE_ALIGN (atomic_type));
	    }
	}

      if (TYPE_STRUCTURAL_EQUALITY_P (type))
	/* Propagate structural equality. */
	SET_TYPE_STRUCTURAL_EQUALITY (t);
      else if (TYPE_CANONICAL (type) != type)
	/* Build the underlying canonical type, since it is different
	   from TYPE. */
	{
	  tree c = build_qualified_type (TYPE_CANONICAL (type), type_quals);
	  TYPE_CANONICAL (t) = TYPE_CANONICAL (c);
	}
      else
	/* T is its own canonical type. */
	TYPE_CANONICAL (t) = t;

    }

  return t;
}

/* Create a variant of type T with alignment ALIGN.  */

tree
build_aligned_type (tree type, unsigned int align)
{
  tree t;

  if (TYPE_PACKED (type)
      || TYPE_ALIGN (type) == align)
    return type;

  for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
    if (check_aligned_type (t, type, align))
      return t;

  t = build_variant_type_copy (type);
  SET_TYPE_ALIGN (t, align);

  return t;
}

/* Create a new distinct copy of TYPE.  The new type is made its own
   MAIN_VARIANT. If TYPE requires structural equality checks, the
   resulting type requires structural equality checks; otherwise, its
   TYPE_CANONICAL points to itself. */

tree
build_distinct_type_copy (tree type)
{
  tree t = copy_node (type);

  TYPE_POINTER_TO (t) = 0;
  TYPE_REFERENCE_TO (t) = 0;

  /* Set the canonical type either to a new equivalence class, or
     propagate the need for structural equality checks. */
  if (TYPE_STRUCTURAL_EQUALITY_P (type))
    SET_TYPE_STRUCTURAL_EQUALITY (t);
  else
    TYPE_CANONICAL (t) = t;

  /* Make it its own variant.  */
  TYPE_MAIN_VARIANT (t) = t;
  TYPE_NEXT_VARIANT (t) = 0;

  /* We do not record methods in type copies nor variants
     so we do not need to keep them up to date when new method
     is inserted.  */
  if (RECORD_OR_UNION_TYPE_P (t))
    TYPE_METHODS (t) = NULL_TREE;

  /* Note that it is now possible for TYPE_MIN_VALUE to be a value
     whose TREE_TYPE is not t.  This can also happen in the Ada
     frontend when using subtypes.  */

  return t;
}

/* Create a new variant of TYPE, equivalent but distinct.  This is so
   the caller can modify it. TYPE_CANONICAL for the return type will
   be equivalent to TYPE_CANONICAL of TYPE, indicating that the types
   are considered equal by the language itself (or that both types
   require structural equality checks). */

tree
build_variant_type_copy (tree type)
{
  tree t, m = TYPE_MAIN_VARIANT (type);

  t = build_distinct_type_copy (type);

  /* Since we're building a variant, assume that it is a non-semantic
     variant. This also propagates TYPE_STRUCTURAL_EQUALITY_P. */
  TYPE_CANONICAL (t) = TYPE_CANONICAL (type);
  /* Type variants have no alias set defined.  */
  TYPE_ALIAS_SET (t) = -1;

  /* Add the new type to the chain of variants of TYPE.  */
  TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (m);
  TYPE_NEXT_VARIANT (m) = t;
  TYPE_MAIN_VARIANT (t) = m;

  return t;
}

/* Return true if the from tree in both tree maps are equal.  */

int
tree_map_base_eq (const void *va, const void *vb)
{
  const struct tree_map_base  *const a = (const struct tree_map_base *) va,
    *const b = (const struct tree_map_base *) vb;
  return (a->from == b->from);
}

/* Hash a from tree in a tree_base_map.  */

unsigned int
tree_map_base_hash (const void *item)
{
  return htab_hash_pointer (((const struct tree_map_base *)item)->from);
}

/* Return true if this tree map structure is marked for garbage collection
   purposes.  We simply return true if the from tree is marked, so that this
   structure goes away when the from tree goes away.  */

int
tree_map_base_marked_p (const void *p)
{
  return ggc_marked_p (((const struct tree_map_base *) p)->from);
}

/* Hash a from tree in a tree_map.  */

unsigned int
tree_map_hash (const void *item)
{
  return (((const struct tree_map *) item)->hash);
}

/* Hash a from tree in a tree_decl_map.  */

unsigned int
tree_decl_map_hash (const void *item)
{
  return DECL_UID (((const struct tree_decl_map *) item)->base.from);
}

/* Return the initialization priority for DECL.  */

priority_type
decl_init_priority_lookup (tree decl)
{
  symtab_node *snode = symtab_node::get (decl);

  if (!snode)
    return DEFAULT_INIT_PRIORITY;
  return
    snode->get_init_priority ();
}

/* Return the finalization priority for DECL.  */

priority_type
decl_fini_priority_lookup (tree decl)
{
  cgraph_node *node = cgraph_node::get (decl);

  if (!node)
    return DEFAULT_INIT_PRIORITY;
  return
    node->get_fini_priority ();
}

/* Set the initialization priority for DECL to PRIORITY.  */

void
decl_init_priority_insert (tree decl, priority_type priority)
{
  struct symtab_node *snode;

  if (priority == DEFAULT_INIT_PRIORITY)
    {
      snode = symtab_node::get (decl);
      if (!snode)
	return;
    }
  else if (TREE_CODE (decl) == VAR_DECL)
    snode = varpool_node::get_create (decl);
  else
    snode = cgraph_node::get_create (decl);
  snode->set_init_priority (priority);
}

/* Set the finalization priority for DECL to PRIORITY.  */

void
decl_fini_priority_insert (tree decl, priority_type priority)
{
  struct cgraph_node *node;

  if (priority == DEFAULT_INIT_PRIORITY)
    {
      node = cgraph_node::get (decl);
      if (!node)
	return;
    }
  else
    node = cgraph_node::get_create (decl);
  node->set_fini_priority (priority);
}

/* Print out the statistics for the DECL_DEBUG_EXPR hash table.  */

static void
print_debug_expr_statistics (void)
{
  fprintf (stderr, "DECL_DEBUG_EXPR  hash: size %ld, %ld elements, %f collisions\n",
	   (long) debug_expr_for_decl->size (),
	   (long) debug_expr_for_decl->elements (),
	   debug_expr_for_decl->collisions ());
}

/* Print out the statistics for the DECL_VALUE_EXPR hash table.  */

static void
print_value_expr_statistics (void)
{
  fprintf (stderr, "DECL_VALUE_EXPR  hash: size %ld, %ld elements, %f collisions\n",
	   (long) value_expr_for_decl->size (),
	   (long) value_expr_for_decl->elements (),
	   value_expr_for_decl->collisions ());
}

/* Lookup a debug expression for FROM, and return it if we find one.  */

tree
decl_debug_expr_lookup (tree from)
{
  struct tree_decl_map *h, in;
  in.base.from = from;

  h = debug_expr_for_decl->find_with_hash (&in, DECL_UID (from));
  if (h)
    return h->to;
  return NULL_TREE;
}

/* Insert a mapping FROM->TO in the debug expression hashtable.  */

void
decl_debug_expr_insert (tree from, tree to)
{
  struct tree_decl_map *h;

  h = ggc_alloc<tree_decl_map> ();
  h->base.from = from;
  h->to = to;
  *debug_expr_for_decl->find_slot_with_hash (h, DECL_UID (from), INSERT) = h;
}

/* Lookup a value expression for FROM, and return it if we find one.  */

tree
decl_value_expr_lookup (tree from)
{
  struct tree_decl_map *h, in;
  in.base.from = from;

  h = value_expr_for_decl->find_with_hash (&in, DECL_UID (from));
  if (h)
    return h->to;
  return NULL_TREE;
}

/* Insert a mapping FROM->TO in the value expression hashtable.  */

void
decl_value_expr_insert (tree from, tree to)
{
  struct tree_decl_map *h;

  h = ggc_alloc<tree_decl_map> ();
  h->base.from = from;
  h->to = to;
  *value_expr_for_decl->find_slot_with_hash (h, DECL_UID (from), INSERT) = h;
}

/* Lookup a vector of debug arguments for FROM, and return it if we
   find one.  */

vec<tree, va_gc> **
decl_debug_args_lookup (tree from)
{
  struct tree_vec_map *h, in;

  if (!DECL_HAS_DEBUG_ARGS_P (from))
    return NULL;
  gcc_checking_assert (debug_args_for_decl != NULL);
  in.base.from = from;
  h = debug_args_for_decl->find_with_hash (&in, DECL_UID (from));
  if (h)
    return &h->to;
  return NULL;
}

/* Insert a mapping FROM->empty vector of debug arguments in the value
   expression hashtable.  */

vec<tree, va_gc> **
decl_debug_args_insert (tree from)
{
  struct tree_vec_map *h;
  tree_vec_map **loc;

  if (DECL_HAS_DEBUG_ARGS_P (from))
    return decl_debug_args_lookup (from);
  if (debug_args_for_decl == NULL)
    debug_args_for_decl = hash_table<tree_vec_map_cache_hasher>::create_ggc (64);
  h = ggc_alloc<tree_vec_map> ();
  h->base.from = from;
  h->to = NULL;
  loc = debug_args_for_decl->find_slot_with_hash (h, DECL_UID (from), INSERT);
  *loc = h;
  DECL_HAS_DEBUG_ARGS_P (from) = 1;
  return &h->to;
}

/* Hashing of types so that we don't make duplicates.
   The entry point is `type_hash_canon'.  */

/* Compute a hash code for a list of types (chain of TREE_LIST nodes
   with types in the TREE_VALUE slots), by adding the hash codes
   of the individual types.  */

static void
type_hash_list (const_tree list, inchash::hash &hstate)
{
  const_tree tail;

  for (tail = list; tail; tail = TREE_CHAIN (tail))
    if (TREE_VALUE (tail) != error_mark_node)
      hstate.add_object (TYPE_HASH (TREE_VALUE (tail)));
}

/* These are the Hashtable callback functions.  */

/* Returns true iff the types are equivalent.  */

bool
type_cache_hasher::equal (type_hash *a, type_hash *b)
{
  /* First test the things that are the same for all types.  */
  if (a->hash != b->hash
      || TREE_CODE (a->type) != TREE_CODE (b->type)
      || TREE_TYPE (a->type) != TREE_TYPE (b->type)
      || !attribute_list_equal (TYPE_ATTRIBUTES (a->type),
				 TYPE_ATTRIBUTES (b->type))
      || (TREE_CODE (a->type) != COMPLEX_TYPE
          && TYPE_NAME (a->type) != TYPE_NAME (b->type)))
    return 0;

  /* Be careful about comparing arrays before and after the element type
     has been completed; don't compare TYPE_ALIGN unless both types are
     complete.  */
  if (COMPLETE_TYPE_P (a->type) && COMPLETE_TYPE_P (b->type)
      && (TYPE_ALIGN (a->type) != TYPE_ALIGN (b->type)
	  || TYPE_MODE (a->type) != TYPE_MODE (b->type)))
    return 0;

  switch (TREE_CODE (a->type))
    {
    case VOID_TYPE:
    case COMPLEX_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case NULLPTR_TYPE:
      return 1;

    case VECTOR_TYPE:
      return TYPE_VECTOR_SUBPARTS (a->type) == TYPE_VECTOR_SUBPARTS (b->type);

    case ENUMERAL_TYPE:
      if (TYPE_VALUES (a->type) != TYPE_VALUES (b->type)
	  && !(TYPE_VALUES (a->type)
	       && TREE_CODE (TYPE_VALUES (a->type)) == TREE_LIST
	       && TYPE_VALUES (b->type)
	       && TREE_CODE (TYPE_VALUES (b->type)) == TREE_LIST
	       && type_list_equal (TYPE_VALUES (a->type),
				   TYPE_VALUES (b->type))))
	return 0;

      /* ... fall through ... */

    case INTEGER_TYPE:
    case REAL_TYPE:
    case BOOLEAN_TYPE:
      if (TYPE_PRECISION (a->type) != TYPE_PRECISION (b->type))
	return false;
      return ((TYPE_MAX_VALUE (a->type) == TYPE_MAX_VALUE (b->type)
	       || tree_int_cst_equal (TYPE_MAX_VALUE (a->type),
				      TYPE_MAX_VALUE (b->type)))
	      && (TYPE_MIN_VALUE (a->type) == TYPE_MIN_VALUE (b->type)
		  || tree_int_cst_equal (TYPE_MIN_VALUE (a->type),
					 TYPE_MIN_VALUE (b->type))));

    case FIXED_POINT_TYPE:
      return TYPE_SATURATING (a->type) == TYPE_SATURATING (b->type);

    case OFFSET_TYPE:
      return TYPE_OFFSET_BASETYPE (a->type) == TYPE_OFFSET_BASETYPE (b->type);

    case METHOD_TYPE:
      if (TYPE_METHOD_BASETYPE (a->type) == TYPE_METHOD_BASETYPE (b->type)
	  && (TYPE_ARG_TYPES (a->type) == TYPE_ARG_TYPES (b->type)
	      || (TYPE_ARG_TYPES (a->type)
		  && TREE_CODE (TYPE_ARG_TYPES (a->type)) == TREE_LIST
		  && TYPE_ARG_TYPES (b->type)
		  && TREE_CODE (TYPE_ARG_TYPES (b->type)) == TREE_LIST
		  && type_list_equal (TYPE_ARG_TYPES (a->type),
				      TYPE_ARG_TYPES (b->type)))))
        break;
      return 0;
    case ARRAY_TYPE:
      return TYPE_DOMAIN (a->type) == TYPE_DOMAIN (b->type);

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      return (TYPE_FIELDS (a->type) == TYPE_FIELDS (b->type)
	      || (TYPE_FIELDS (a->type)
		  && TREE_CODE (TYPE_FIELDS (a->type)) == TREE_LIST
		  && TYPE_FIELDS (b->type)
		  && TREE_CODE (TYPE_FIELDS (b->type)) == TREE_LIST
		  && type_list_equal (TYPE_FIELDS (a->type),
				      TYPE_FIELDS (b->type))));

    case FUNCTION_TYPE:
      if (TYPE_ARG_TYPES (a->type) == TYPE_ARG_TYPES (b->type)
	  || (TYPE_ARG_TYPES (a->type)
	      && TREE_CODE (TYPE_ARG_TYPES (a->type)) == TREE_LIST
	      && TYPE_ARG_TYPES (b->type)
	      && TREE_CODE (TYPE_ARG_TYPES (b->type)) == TREE_LIST
	      && type_list_equal (TYPE_ARG_TYPES (a->type),
				  TYPE_ARG_TYPES (b->type))))
	break;
      return 0;

    default:
      return 0;
    }

  if (lang_hooks.types.type_hash_eq != NULL)
    return lang_hooks.types.type_hash_eq (a->type, b->type);

  return 1;
}

/* Given TYPE, and HASHCODE its hash code, return the canonical
   object for an identical type if one already exists.
   Otherwise, return TYPE, and record it as the canonical object.

   To use this function, first create a type of the sort you want.
   Then compute its hash code from the fields of the type that
   make it different from other similar types.
   Then call this function and use the value.  */

tree
type_hash_canon (unsigned int hashcode, tree type)
{
  type_hash in;
  type_hash **loc;

  /* The hash table only contains main variants, so ensure that's what we're
     being passed.  */
  gcc_assert (TYPE_MAIN_VARIANT (type) == type);

  /* The TYPE_ALIGN field of a type is set by layout_type(), so we
     must call that routine before comparing TYPE_ALIGNs.  */
  layout_type (type);

  in.hash = hashcode;
  in.type = type;

  loc = type_hash_table->find_slot_with_hash (&in, hashcode, INSERT);
  if (*loc)
    {
      tree t1 = ((type_hash *) *loc)->type;
      gcc_assert (TYPE_MAIN_VARIANT (t1) == t1);
      free_node (type);
      return t1;
    }
  else
    {
      struct type_hash *h;

      h = ggc_alloc<type_hash> ();
      h->hash = hashcode;
      h->type = type;
      *loc = h;

      return type;
    }
}

static void
print_type_hash_statistics (void)
{
  fprintf (stderr, "Type hash: size %ld, %ld elements, %f collisions\n",
	   (long) type_hash_table->size (),
	   (long) type_hash_table->elements (),
	   type_hash_table->collisions ());
}

/* Compute a hash code for a list of attributes (chain of TREE_LIST nodes
   with names in the TREE_PURPOSE slots and args in the TREE_VALUE slots),
   by adding the hash codes of the individual attributes.  */

static void
attribute_hash_list (const_tree list, inchash::hash &hstate)
{
  const_tree tail;

  for (tail = list; tail; tail = TREE_CHAIN (tail))
    /* ??? Do we want to add in TREE_VALUE too? */
    hstate.add_object (IDENTIFIER_HASH_VALUE (get_attribute_name (tail)));
}

/* Given two lists of attributes, return true if list l2 is
   equivalent to l1.  */

int
attribute_list_equal (const_tree l1, const_tree l2)
{
  if (l1 == l2)
    return 1;

  return attribute_list_contained (l1, l2)
	 && attribute_list_contained (l2, l1);
}

/* Given two lists of attributes, return true if list L2 is
   completely contained within L1.  */
/* ??? This would be faster if attribute names were stored in a canonicalized
   form.  Otherwise, if L1 uses `foo' and L2 uses `__foo__', the long method
   must be used to show these elements are equivalent (which they are).  */
/* ??? It's not clear that attributes with arguments will always be handled
   correctly.  */

int
attribute_list_contained (const_tree l1, const_tree l2)
{
  const_tree t1, t2;

  /* First check the obvious, maybe the lists are identical.  */
  if (l1 == l2)
    return 1;

  /* Maybe the lists are similar.  */
  for (t1 = l1, t2 = l2;
       t1 != 0 && t2 != 0
        && get_attribute_name (t1) == get_attribute_name (t2)
        && TREE_VALUE (t1) == TREE_VALUE (t2);
       t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2))
    ;

  /* Maybe the lists are equal.  */
  if (t1 == 0 && t2 == 0)
    return 1;

  for (; t2 != 0; t2 = TREE_CHAIN (t2))
    {
      const_tree attr;
      /* This CONST_CAST is okay because lookup_attribute does not
	 modify its argument and the return value is assigned to a
	 const_tree.  */
      for (attr = lookup_ident_attribute (get_attribute_name (t2),
					  CONST_CAST_TREE (l1));
	   attr != NULL_TREE && !attribute_value_equal (t2, attr);
	   attr = lookup_ident_attribute (get_attribute_name (t2),
					  TREE_CHAIN (attr)))
	;

      if (attr == NULL_TREE)
	return 0;
    }

  return 1;
}

/* Given two lists of types
   (chains of TREE_LIST nodes with types in the TREE_VALUE slots)
   return 1 if the lists contain the same types in the same order.
   Also, the TREE_PURPOSEs must match.  */

int
type_list_equal (const_tree l1, const_tree l2)
{
  const_tree t1, t2;

  for (t1 = l1, t2 = l2; t1 && t2; t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2))
    if (TREE_VALUE (t1) != TREE_VALUE (t2)
	|| (TREE_PURPOSE (t1) != TREE_PURPOSE (t2)
	    && ! (1 == simple_cst_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2))
		  && (TREE_TYPE (TREE_PURPOSE (t1))
		      == TREE_TYPE (TREE_PURPOSE (t2))))))
      return 0;

  return t1 == t2;
}

/* Returns the number of arguments to the FUNCTION_TYPE or METHOD_TYPE
   given by TYPE.  If the argument list accepts variable arguments,
   then this function counts only the ordinary arguments.  */

int
type_num_arguments (const_tree type)
{
  int i = 0;
  tree t;

  for (t = TYPE_ARG_TYPES (type); t; t = TREE_CHAIN (t))
    /* If the function does not take a variable number of arguments,
       the last element in the list will have type `void'.  */
    if (VOID_TYPE_P (TREE_VALUE (t)))
      break;
    else
      ++i;

  return i;
}

/* Nonzero if integer constants T1 and T2
   represent the same constant value.  */

int
tree_int_cst_equal (const_tree t1, const_tree t2)
{
  if (t1 == t2)
    return 1;

  if (t1 == 0 || t2 == 0)
    return 0;

  if (TREE_CODE (t1) == INTEGER_CST
      && TREE_CODE (t2) == INTEGER_CST
      && wi::to_widest (t1) == wi::to_widest (t2))
    return 1;

  return 0;
}

/* Return true if T is an INTEGER_CST whose numerical value (extended
   according to TYPE_UNSIGNED) fits in a signed HOST_WIDE_INT.  */

bool
tree_fits_shwi_p (const_tree t)
{
  return (t != NULL_TREE
	  && TREE_CODE (t) == INTEGER_CST
	  && wi::fits_shwi_p (wi::to_widest (t)));
}

/* Return true if T is an INTEGER_CST whose numerical value (extended
   according to TYPE_UNSIGNED) fits in an unsigned HOST_WIDE_INT.  */

bool
tree_fits_uhwi_p (const_tree t)
{
  return (t != NULL_TREE
	  && TREE_CODE (t) == INTEGER_CST
	  && wi::fits_uhwi_p (wi::to_widest (t)));
}

/* T is an INTEGER_CST whose numerical value (extended according to
   TYPE_UNSIGNED) fits in a signed HOST_WIDE_INT.  Return that
   HOST_WIDE_INT.  */

HOST_WIDE_INT
tree_to_shwi (const_tree t)
{
  gcc_assert (tree_fits_shwi_p (t));
  return TREE_INT_CST_LOW (t);
}

/* T is an INTEGER_CST whose numerical value (extended according to
   TYPE_UNSIGNED) fits in an unsigned HOST_WIDE_INT.  Return that
   HOST_WIDE_INT.  */

unsigned HOST_WIDE_INT
tree_to_uhwi (const_tree t)
{
  gcc_assert (tree_fits_uhwi_p (t));
  return TREE_INT_CST_LOW (t);
}

/* Return the most significant (sign) bit of T.  */

int
tree_int_cst_sign_bit (const_tree t)
{
  unsigned bitno = TYPE_PRECISION (TREE_TYPE (t)) - 1;

  return wi::extract_uhwi (t, bitno, 1);
}

/* Return an indication of the sign of the integer constant T.
   The return value is -1 if T < 0, 0 if T == 0, and 1 if T > 0.
   Note that -1 will never be returned if T's type is unsigned.  */

int
tree_int_cst_sgn (const_tree t)
{
  if (wi::eq_p (t, 0))
    return 0;
  else if (TYPE_UNSIGNED (TREE_TYPE (t)))
    return 1;
  else if (wi::neg_p (t))
    return -1;
  else
    return 1;
}

/* Return the minimum number of bits needed to represent VALUE in a
   signed or unsigned type, UNSIGNEDP says which.  */

unsigned int
tree_int_cst_min_precision (tree value, signop sgn)
{
  /* If the value is negative, compute its negative minus 1.  The latter
     adjustment is because the absolute value of the largest negative value
     is one larger than the largest positive value.  This is equivalent to
     a bit-wise negation, so use that operation instead.  */

  if (tree_int_cst_sgn (value) < 0)
    value = fold_build1 (BIT_NOT_EXPR, TREE_TYPE (value), value);

  /* Return the number of bits needed, taking into account the fact
     that we need one more bit for a signed than unsigned type.
     If value is 0 or -1, the minimum precision is 1 no matter
     whether unsignedp is true or false.  */

  if (integer_zerop (value))
    return 1;
  else
    return tree_floor_log2 (value) + 1 + (sgn == SIGNED ? 1 : 0) ;
}

/* Return truthvalue of whether T1 is the same tree structure as T2.
   Return 1 if they are the same.
   Return 0 if they are understandably different.
   Return -1 if either contains tree structure not understood by
   this function.  */

int
simple_cst_equal (const_tree t1, const_tree t2)
{
  enum tree_code code1, code2;
  int cmp;
  int i;

  if (t1 == t2)
    return 1;
  if (t1 == 0 || t2 == 0)
    return 0;

  code1 = TREE_CODE (t1);
  code2 = TREE_CODE (t2);

  if (CONVERT_EXPR_CODE_P (code1) || code1 == NON_LVALUE_EXPR)
    {
      if (CONVERT_EXPR_CODE_P (code2)
	  || code2 == NON_LVALUE_EXPR)
	return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      else
	return simple_cst_equal (TREE_OPERAND (t1, 0), t2);
    }

  else if (CONVERT_EXPR_CODE_P (code2)
	   || code2 == NON_LVALUE_EXPR)
    return simple_cst_equal (t1, TREE_OPERAND (t2, 0));

  if (code1 != code2)
    return 0;

  switch (code1)
    {
    case INTEGER_CST:
      return wi::to_widest (t1) == wi::to_widest (t2);

    case REAL_CST:
      return real_identical (&TREE_REAL_CST (t1), &TREE_REAL_CST (t2));

    case FIXED_CST:
      return FIXED_VALUES_IDENTICAL (TREE_FIXED_CST (t1), TREE_FIXED_CST (t2));

    case STRING_CST:
      return (TREE_STRING_LENGTH (t1) == TREE_STRING_LENGTH (t2)
	      && ! memcmp (TREE_STRING_POINTER (t1), TREE_STRING_POINTER (t2),
			 TREE_STRING_LENGTH (t1)));

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx;
	vec<constructor_elt, va_gc> *v1 = CONSTRUCTOR_ELTS (t1);
	vec<constructor_elt, va_gc> *v2 = CONSTRUCTOR_ELTS (t2);

	if (vec_safe_length (v1) != vec_safe_length (v2))
	  return false;

        for (idx = 0; idx < vec_safe_length (v1); ++idx)
	  /* ??? Should we handle also fields here? */
	  if (!simple_cst_equal ((*v1)[idx].value, (*v2)[idx].value))
	    return false;
	return true;
      }

    case SAVE_EXPR:
      return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

    case CALL_EXPR:
      cmp = simple_cst_equal (CALL_EXPR_FN (t1), CALL_EXPR_FN (t2));
      if (cmp <= 0)
	return cmp;
      if (call_expr_nargs (t1) != call_expr_nargs (t2))
	return 0;
      {
	const_tree arg1, arg2;
	const_call_expr_arg_iterator iter1, iter2;
	for (arg1 = first_const_call_expr_arg (t1, &iter1),
	       arg2 = first_const_call_expr_arg (t2, &iter2);
	     arg1 && arg2;
	     arg1 = next_const_call_expr_arg (&iter1),
	       arg2 = next_const_call_expr_arg (&iter2))
	  {
	    cmp = simple_cst_equal (arg1, arg2);
	    if (cmp <= 0)
	      return cmp;
	  }
	return arg1 == arg2;
      }

    case TARGET_EXPR:
      /* Special case: if either target is an unallocated VAR_DECL,
	 it means that it's going to be unified with whatever the
	 TARGET_EXPR is really supposed to initialize, so treat it
	 as being equivalent to anything.  */
      if ((TREE_CODE (TREE_OPERAND (t1, 0)) == VAR_DECL
	   && DECL_NAME (TREE_OPERAND (t1, 0)) == NULL_TREE
	   && !DECL_RTL_SET_P (TREE_OPERAND (t1, 0)))
	  || (TREE_CODE (TREE_OPERAND (t2, 0)) == VAR_DECL
	      && DECL_NAME (TREE_OPERAND (t2, 0)) == NULL_TREE
	      && !DECL_RTL_SET_P (TREE_OPERAND (t2, 0))))
	cmp = 1;
      else
	cmp = simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

      if (cmp <= 0)
	return cmp;

      return simple_cst_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));

    case WITH_CLEANUP_EXPR:
      cmp = simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      if (cmp <= 0)
	return cmp;

      return simple_cst_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t1, 1));

    case COMPONENT_REF:
      if (TREE_OPERAND (t1, 1) == TREE_OPERAND (t2, 1))
	return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

      return 0;

    case VAR_DECL:
    case PARM_DECL:
    case CONST_DECL:
    case FUNCTION_DECL:
      return 0;

    default:
      break;
    }

  /* This general rule works for most tree codes.  All exceptions should be
     handled above.  If this is a language-specific tree code, we can't
     trust what might be in the operand, so say we don't know
     the situation.  */
  if ((int) code1 >= (int) LAST_AND_UNUSED_TREE_CODE)
    return -1;

  switch (TREE_CODE_CLASS (code1))
    {
    case tcc_unary:
    case tcc_binary:
    case tcc_comparison:
    case tcc_expression:
    case tcc_reference:
    case tcc_statement:
      cmp = 1;
      for (i = 0; i < TREE_CODE_LENGTH (code1); i++)
	{
	  cmp = simple_cst_equal (TREE_OPERAND (t1, i), TREE_OPERAND (t2, i));
	  if (cmp <= 0)
	    return cmp;
	}

      return cmp;

    default:
      return -1;
    }
}

/* Compare the value of T, an INTEGER_CST, with U, an unsigned integer value.
   Return -1, 0, or 1 if the value of T is less than, equal to, or greater
   than U, respectively.  */

int
compare_tree_int (const_tree t, unsigned HOST_WIDE_INT u)
{
  if (tree_int_cst_sgn (t) < 0)
    return -1;
  else if (!tree_fits_uhwi_p (t))
    return 1;
  else if (TREE_INT_CST_LOW (t) == u)
    return 0;
  else if (TREE_INT_CST_LOW (t) < u)
    return -1;
  else
    return 1;
}

/* Return true if SIZE represents a constant size that is in bounds of
   what the middle-end and the backend accepts (covering not more than
   half of the address-space).  */

bool
valid_constant_size_p (const_tree size)
{
  if (! tree_fits_uhwi_p (size)
      || TREE_OVERFLOW (size)
      || tree_int_cst_sign_bit (size) != 0)
    return false;
  return true;
}

/* Return the precision of the type, or for a complex or vector type the
   precision of the type of its elements.  */

unsigned int
element_precision (const_tree type)
{
  if (!TYPE_P (type))
    type = TREE_TYPE (type);
  enum tree_code code = TREE_CODE (type);
  if (code == COMPLEX_TYPE || code == VECTOR_TYPE)
    type = TREE_TYPE (type);

  return TYPE_PRECISION (type);
}

/* Return true if CODE represents an associative tree code.  Otherwise
   return false.  */
bool
associative_tree_code (enum tree_code code)
{
  switch (code)
    {
    case BIT_IOR_EXPR:
    case BIT_AND_EXPR:
    case BIT_XOR_EXPR:
    case PLUS_EXPR:
    case MULT_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
      return true;

    default:
      break;
    }
  return false;
}

/* Return true if CODE represents a commutative tree code.  Otherwise
   return false.  */
bool
commutative_tree_code (enum tree_code code)
{
  switch (code)
    {
    case PLUS_EXPR:
    case MULT_EXPR:
    case MULT_HIGHPART_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case NE_EXPR:
    case EQ_EXPR:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_XOR_EXPR:
    case TRUTH_OR_EXPR:
    case WIDEN_MULT_EXPR:
    case VEC_WIDEN_MULT_HI_EXPR:
    case VEC_WIDEN_MULT_LO_EXPR:
    case VEC_WIDEN_MULT_EVEN_EXPR:
    case VEC_WIDEN_MULT_ODD_EXPR:
      return true;

    default:
      break;
    }
  return false;
}

/* Return true if CODE represents a ternary tree code for which the
   first two operands are commutative.  Otherwise return false.  */
bool
commutative_ternary_tree_code (enum tree_code code)
{
  switch (code)
    {
    case WIDEN_MULT_PLUS_EXPR:
    case WIDEN_MULT_MINUS_EXPR:
    case DOT_PROD_EXPR:
    case FMA_EXPR:
      return true;

    default:
      break;
    }
  return false;
}

/* Returns true if CODE can overflow.  */

bool
operation_can_overflow (enum tree_code code)
{
  switch (code)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case LSHIFT_EXPR:
      /* Can overflow in various ways.  */
      return true;
    case TRUNC_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
      /* For INT_MIN / -1.  */
      return true;
    case NEGATE_EXPR:
    case ABS_EXPR:
      /* For -INT_MIN.  */
      return true;
    default:
      /* These operators cannot overflow.  */
      return false;
    }
}

/* Returns true if CODE operating on operands of type TYPE doesn't overflow, or
   ftrapv doesn't generate trapping insns for CODE.  */

bool
operation_no_trapping_overflow (tree type, enum tree_code code)
{
  gcc_checking_assert (ANY_INTEGRAL_TYPE_P (type));

  /* We don't generate instructions that trap on overflow for complex or vector
     types.  */
  if (!INTEGRAL_TYPE_P (type))
    return true;

  if (!TYPE_OVERFLOW_TRAPS (type))
    return true;

  switch (code)
    {
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
      /* These operators can overflow, and -ftrapv generates trapping code for
	 these.  */
      return false;
    case TRUNC_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case LSHIFT_EXPR:
      /* These operators can overflow, but -ftrapv does not generate trapping
	 code for these.  */
      return true;
    default:
      /* These operators cannot overflow.  */
      return true;
    }
}

namespace inchash
{

/* Generate a hash value for an expression.  This can be used iteratively
   by passing a previous result as the HSTATE argument.

   This function is intended to produce the same hash for expressions which
   would compare equal using operand_equal_p.  */
void
add_expr (const_tree t, inchash::hash &hstate, unsigned int flags)
{
  int i;
  enum tree_code code;
  enum tree_code_class tclass;

  if (t == NULL_TREE)
    {
      hstate.merge_hash (0);
      return;
    }

  if (!(flags & OEP_ADDRESS_OF))
    STRIP_NOPS (t);

  code = TREE_CODE (t);

  switch (code)
    {
    /* Alas, constants aren't shared, so we can't rely on pointer
       identity.  */
    case VOID_CST:
      hstate.merge_hash (0);
      return;
    case INTEGER_CST:
      gcc_checking_assert (!(flags & OEP_ADDRESS_OF));
      for (i = 0; i < TREE_INT_CST_EXT_NUNITS (t); i++)
	hstate.add_wide_int (TREE_INT_CST_ELT (t, i));
      return;
    case REAL_CST:
      {
	unsigned int val2;
	if (!HONOR_SIGNED_ZEROS (t) && real_zerop (t))
	  val2 = rvc_zero;
	else
	  val2 = real_hash (TREE_REAL_CST_PTR (t));
	hstate.merge_hash (val2);
	return;
      }
    case FIXED_CST:
      {
	unsigned int val2 = fixed_hash (TREE_FIXED_CST_PTR (t));
	hstate.merge_hash (val2);
	return;
      }
    case STRING_CST:
      hstate.add ((const void *) TREE_STRING_POINTER (t),
		  TREE_STRING_LENGTH (t));
      return;
    case COMPLEX_CST:
      inchash::add_expr (TREE_REALPART (t), hstate, flags);
      inchash::add_expr (TREE_IMAGPART (t), hstate, flags);
      return;
    case VECTOR_CST:
      {
	unsigned i;
	for (i = 0; i < VECTOR_CST_NELTS (t); ++i)
	  inchash::add_expr (VECTOR_CST_ELT (t, i), hstate, flags);
	return;
      }
    case SSA_NAME:
      /* We can just compare by pointer.  */
      hstate.add_wide_int (SSA_NAME_VERSION (t));
      return;
    case PLACEHOLDER_EXPR:
      /* The node itself doesn't matter.  */
      return;
    case BLOCK:
    case OMP_CLAUSE:
      /* Ignore.  */
      return;
    case TREE_LIST:
      /* A list of expressions, for a CALL_EXPR or as the elements of a
	 VECTOR_CST.  */
      for (; t; t = TREE_CHAIN (t))
	inchash::add_expr (TREE_VALUE (t), hstate, flags);
      return;
    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx;
	tree field, value;
	flags &= ~OEP_ADDRESS_OF;
	FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (t), idx, field, value)
	  {
	    inchash::add_expr (field, hstate, flags);
	    inchash::add_expr (value, hstate, flags);
	  }
	return;
      }
    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	for (i = tsi_start (CONST_CAST_TREE (t));
	     !tsi_end_p (i); tsi_next (&i))
	  inchash::add_expr (tsi_stmt (i), hstate, flags);
	return;
      }
    case FUNCTION_DECL:
      /* When referring to a built-in FUNCTION_DECL, use the __builtin__ form.
	 Otherwise nodes that compare equal according to operand_equal_p might
	 get different hash codes.  However, don't do this for machine specific
	 or front end builtins, since the function code is overloaded in those
	 cases.  */
      if (DECL_BUILT_IN_CLASS (t) == BUILT_IN_NORMAL
	  && builtin_decl_explicit_p (DECL_FUNCTION_CODE (t)))
	{
	  t = builtin_decl_explicit (DECL_FUNCTION_CODE (t));
	  code = TREE_CODE (t);
	}
      /* FALL THROUGH */
    default:
      tclass = TREE_CODE_CLASS (code);

      if (tclass == tcc_declaration)
	{
	  /* DECL's have a unique ID */
	  hstate.add_wide_int (DECL_UID (t));
	}
      else if (tclass == tcc_comparison && !commutative_tree_code (code))
	{
	  /* For comparisons that can be swapped, use the lower
	     tree code.  */
	  enum tree_code ccode = swap_tree_comparison (code);
	  if (code < ccode)
	    ccode = code;
	  hstate.add_object (ccode);
	  inchash::add_expr (TREE_OPERAND (t, ccode != code), hstate, flags);
	  inchash::add_expr (TREE_OPERAND (t, ccode == code), hstate, flags);
	}
      else if (CONVERT_EXPR_CODE_P (code))
	{
	  /* NOP_EXPR and CONVERT_EXPR are considered equal by
	     operand_equal_p.  */
	  enum tree_code ccode = NOP_EXPR;
	  hstate.add_object (ccode);

	  /* Don't hash the type, that can lead to having nodes which
	     compare equal according to operand_equal_p, but which
	     have different hash codes.  Make sure to include signedness
	     in the hash computation.  */
	  hstate.add_int (TYPE_UNSIGNED (TREE_TYPE (t)));
	  inchash::add_expr (TREE_OPERAND (t, 0), hstate, flags);
	}
      /* For OEP_ADDRESS_OF, hash MEM_EXPR[&decl, 0] the same as decl.  */
      else if (code == MEM_REF
	       && (flags & OEP_ADDRESS_OF) != 0
	       && TREE_CODE (TREE_OPERAND (t, 0)) == ADDR_EXPR
	       && DECL_P (TREE_OPERAND (TREE_OPERAND (t, 0), 0))
	       && integer_zerop (TREE_OPERAND (t, 1)))
	inchash::add_expr (TREE_OPERAND (TREE_OPERAND (t, 0), 0),
			   hstate, flags);
      /* Don't ICE on FE specific trees, or their arguments etc.
	 during operand_equal_p hash verification.  */
      else if (!IS_EXPR_CODE_CLASS (tclass))
	gcc_assert (flags & OEP_HASH_CHECK);
      else
	{
	  unsigned int sflags = flags;

	  hstate.add_object (code);

	  switch (code)
	    {
	    case ADDR_EXPR:
	      gcc_checking_assert (!(flags & OEP_ADDRESS_OF));
	      flags |= OEP_ADDRESS_OF;
	      sflags = flags;
	      break;

	    case INDIRECT_REF:
	    case MEM_REF:
	    case TARGET_MEM_REF:
	      flags &= ~OEP_ADDRESS_OF;
	      sflags = flags;
	      break;

	    case ARRAY_REF:
	    case ARRAY_RANGE_REF:
	    case COMPONENT_REF:
	    case BIT_FIELD_REF:
	      sflags &= ~OEP_ADDRESS_OF;
	      break;

	    case COND_EXPR:
	      flags &= ~OEP_ADDRESS_OF;
	      break;

	    case FMA_EXPR:
	    case WIDEN_MULT_PLUS_EXPR:
	    case WIDEN_MULT_MINUS_EXPR:
	      {
		/* The multiplication operands are commutative.  */
		inchash::hash one, two;
		inchash::add_expr (TREE_OPERAND (t, 0), one, flags);
		inchash::add_expr (TREE_OPERAND (t, 1), two, flags);
		hstate.add_commutative (one, two);
		inchash::add_expr (TREE_OPERAND (t, 2), two, flags);
		return;
	      }

	    case CALL_EXPR:
	      if (CALL_EXPR_FN (t) == NULL_TREE)
		hstate.add_int (CALL_EXPR_IFN (t));
	      break;

	    case TARGET_EXPR:
	      /* For TARGET_EXPR, just hash on the TARGET_EXPR_SLOT.
		 Usually different TARGET_EXPRs just should use
		 different temporaries in their slots.  */
	      inchash::add_expr (TARGET_EXPR_SLOT (t), hstate, flags);
	      return;

	    default:
	      break;
	    }

	  /* Don't hash the type, that can lead to having nodes which
	     compare equal according to operand_equal_p, but which
	     have different hash codes.  */
	  if (code == NON_LVALUE_EXPR)
	    {
	      /* Make sure to include signness in the hash computation.  */
	      hstate.add_int (TYPE_UNSIGNED (TREE_TYPE (t)));
	      inchash::add_expr (TREE_OPERAND (t, 0), hstate, flags);
	    }

	  else if (commutative_tree_code (code))
	    {
	      /* It's a commutative expression.  We want to hash it the same
		 however it appears.  We do this by first hashing both operands
		 and then rehashing based on the order of their independent
		 hashes.  */
	      inchash::hash one, two;
	      inchash::add_expr (TREE_OPERAND (t, 0), one, flags);
	      inchash::add_expr (TREE_OPERAND (t, 1), two, flags);
	      hstate.add_commutative (one, two);
	    }
	  else
	    for (i = TREE_OPERAND_LENGTH (t) - 1; i >= 0; --i)
	      inchash::add_expr (TREE_OPERAND (t, i), hstate,
				 i == 0 ? flags : sflags);
	}
      return;
    }
}

}

/* Constructors for pointer, array and function types.
   (RECORD_TYPE, UNION_TYPE and ENUMERAL_TYPE nodes are
   constructed by language-dependent code, not here.)  */

/* Construct, lay out and return the type of pointers to TO_TYPE with
   mode MODE.  If CAN_ALIAS_ALL is TRUE, indicate this type can
   reference all of memory. If such a type has already been
   constructed, reuse it.  */

tree
build_pointer_type_for_mode (tree to_type, machine_mode mode,
			     bool can_alias_all)
{
  tree t;
  bool could_alias = can_alias_all;

  if (to_type == error_mark_node)
    return error_mark_node;

  /* If the pointed-to type has the may_alias attribute set, force
     a TYPE_REF_CAN_ALIAS_ALL pointer to be generated.  */
  if (lookup_attribute ("may_alias", TYPE_ATTRIBUTES (to_type)))
    can_alias_all = true;

  /* In some cases, languages will have things that aren't a POINTER_TYPE
     (such as a RECORD_TYPE for fat pointers in Ada) as TYPE_POINTER_TO.
     In that case, return that type without regard to the rest of our
     operands.

     ??? This is a kludge, but consistent with the way this function has
     always operated and there doesn't seem to be a good way to avoid this
     at the moment.  */
  if (TYPE_POINTER_TO (to_type) != 0
      && TREE_CODE (TYPE_POINTER_TO (to_type)) != POINTER_TYPE)
    return TYPE_POINTER_TO (to_type);

  /* First, if we already have a type for pointers to TO_TYPE and it's
     the proper mode, use it.  */
  for (t = TYPE_POINTER_TO (to_type); t; t = TYPE_NEXT_PTR_TO (t))
    if (TYPE_MODE (t) == mode && TYPE_REF_CAN_ALIAS_ALL (t) == can_alias_all)
      return t;

  t = make_node (POINTER_TYPE);

  TREE_TYPE (t) = to_type;
  SET_TYPE_MODE (t, mode);
  TYPE_REF_CAN_ALIAS_ALL (t) = can_alias_all;
  TYPE_NEXT_PTR_TO (t) = TYPE_POINTER_TO (to_type);
  TYPE_POINTER_TO (to_type) = t;

  /* During LTO we do not set TYPE_CANONICAL of pointers and references.  */
  if (TYPE_STRUCTURAL_EQUALITY_P (to_type) || in_lto_p)
    SET_TYPE_STRUCTURAL_EQUALITY (t);
  else if (TYPE_CANONICAL (to_type) != to_type || could_alias)
    TYPE_CANONICAL (t)
      = build_pointer_type_for_mode (TYPE_CANONICAL (to_type),
				     mode, false);

  /* Lay out the type.  This function has many callers that are concerned
     with expression-construction, and this simplifies them all.  */
  layout_type (t);

  return t;
}

/* By default build pointers in ptr_mode.  */

tree
build_pointer_type (tree to_type)
{
  addr_space_t as = to_type == error_mark_node? ADDR_SPACE_GENERIC
					      : TYPE_ADDR_SPACE (to_type);
  machine_mode pointer_mode = targetm.addr_space.pointer_mode (as);
  return build_pointer_type_for_mode (to_type, pointer_mode, false);
}

/* Same as build_pointer_type_for_mode, but for REFERENCE_TYPE.  */

tree
build_reference_type_for_mode (tree to_type, machine_mode mode,
			       bool can_alias_all)
{
  tree t;
  bool could_alias = can_alias_all;

  if (to_type == error_mark_node)
    return error_mark_node;

  /* If the pointed-to type has the may_alias attribute set, force
     a TYPE_REF_CAN_ALIAS_ALL pointer to be generated.  */
  if (lookup_attribute ("may_alias", TYPE_ATTRIBUTES (to_type)))
    can_alias_all = true;

  /* In some cases, languages will have things that aren't a REFERENCE_TYPE
     (such as a RECORD_TYPE for fat pointers in Ada) as TYPE_REFERENCE_TO.
     In that case, return that type without regard to the rest of our
     operands.

     ??? This is a kludge, but consistent with the way this function has
     always operated and there doesn't seem to be a good way to avoid this
     at the moment.  */
  if (TYPE_REFERENCE_TO (to_type) != 0
      && TREE_CODE (TYPE_REFERENCE_TO (to_type)) != REFERENCE_TYPE)
    return TYPE_REFERENCE_TO (to_type);

  /* First, if we already have a type for pointers to TO_TYPE and it's
     the proper mode, use it.  */
  for (t = TYPE_REFERENCE_TO (to_type); t; t = TYPE_NEXT_REF_TO (t))
    if (TYPE_MODE (t) == mode && TYPE_REF_CAN_ALIAS_ALL (t) == can_alias_all)
      return t;

  t = make_node (REFERENCE_TYPE);

  TREE_TYPE (t) = to_type;
  SET_TYPE_MODE (t, mode);
  TYPE_REF_CAN_ALIAS_ALL (t) = can_alias_all;
  TYPE_NEXT_REF_TO (t) = TYPE_REFERENCE_TO (to_type);
  TYPE_REFERENCE_TO (to_type) = t;

  /* During LTO we do not set TYPE_CANONICAL of pointers and references.  */
  if (TYPE_STRUCTURAL_EQUALITY_P (to_type) || in_lto_p)
    SET_TYPE_STRUCTURAL_EQUALITY (t);
  else if (TYPE_CANONICAL (to_type) != to_type || could_alias)
    TYPE_CANONICAL (t)
      = build_reference_type_for_mode (TYPE_CANONICAL (to_type),
				       mode, false);

  layout_type (t);

  return t;
}


/* Build the node for the type of references-to-TO_TYPE by default
   in ptr_mode.  */

tree
build_reference_type (tree to_type)
{
  addr_space_t as = to_type == error_mark_node? ADDR_SPACE_GENERIC
					      : TYPE_ADDR_SPACE (to_type);
  machine_mode pointer_mode = targetm.addr_space.pointer_mode (as);
  return build_reference_type_for_mode (to_type, pointer_mode, false);
}

#define MAX_INT_CACHED_PREC \
  (HOST_BITS_PER_WIDE_INT > 64 ? HOST_BITS_PER_WIDE_INT : 64)
static GTY(()) tree nonstandard_integer_type_cache[2 * MAX_INT_CACHED_PREC + 2];

/* Builds a signed or unsigned integer type of precision PRECISION.
   Used for C bitfields whose precision does not match that of
   built-in target types.  */
tree
build_nonstandard_integer_type (unsigned HOST_WIDE_INT precision,
				int unsignedp)
{
  tree itype, ret;

  if (unsignedp)
    unsignedp = MAX_INT_CACHED_PREC + 1;
    
  if (precision <= MAX_INT_CACHED_PREC)
    {
      itype = nonstandard_integer_type_cache[precision + unsignedp];
      if (itype)
	return itype;
    }

  itype = make_node (INTEGER_TYPE);
  TYPE_PRECISION (itype) = precision;

  if (unsignedp)
    fixup_unsigned_type (itype);
  else
    fixup_signed_type (itype);

  ret = itype;
  if (tree_fits_uhwi_p (TYPE_MAX_VALUE (itype)))
    ret = type_hash_canon (tree_to_uhwi (TYPE_MAX_VALUE (itype)), itype);
  if (precision <= MAX_INT_CACHED_PREC)
    nonstandard_integer_type_cache[precision + unsignedp] = ret;

  return ret;
}

#define MAX_BOOL_CACHED_PREC \
  (HOST_BITS_PER_WIDE_INT > 64 ? HOST_BITS_PER_WIDE_INT : 64)
static GTY(()) tree nonstandard_boolean_type_cache[MAX_BOOL_CACHED_PREC + 1];

/* Builds a boolean type of precision PRECISION.
   Used for boolean vectors to choose proper vector element size.  */
tree
build_nonstandard_boolean_type (unsigned HOST_WIDE_INT precision)
{
  tree type;

  if (precision <= MAX_BOOL_CACHED_PREC)
    {
      type = nonstandard_boolean_type_cache[precision];
      if (type)
	return type;
    }

  type = make_node (BOOLEAN_TYPE);
  TYPE_PRECISION (type) = precision;
  fixup_signed_type (type);

  if (precision <= MAX_INT_CACHED_PREC)
    nonstandard_boolean_type_cache[precision] = type;

  return type;
}

/* Create a range of some discrete type TYPE (an INTEGER_TYPE, ENUMERAL_TYPE
   or BOOLEAN_TYPE) with low bound LOWVAL and high bound HIGHVAL.  If SHARED
   is true, reuse such a type that has already been constructed.  */

static tree
build_range_type_1 (tree type, tree lowval, tree highval, bool shared)
{
  tree itype = make_node (INTEGER_TYPE);
  inchash::hash hstate;

  TREE_TYPE (itype) = type;

  TYPE_MIN_VALUE (itype) = fold_convert (type, lowval);
  TYPE_MAX_VALUE (itype) = highval ? fold_convert (type, highval) : NULL;

  TYPE_PRECISION (itype) = TYPE_PRECISION (type);
  SET_TYPE_MODE (itype, TYPE_MODE (type));
  TYPE_SIZE (itype) = TYPE_SIZE (type);
  TYPE_SIZE_UNIT (itype) = TYPE_SIZE_UNIT (type);
  SET_TYPE_ALIGN (itype, TYPE_ALIGN (type));
  TYPE_USER_ALIGN (itype) = TYPE_USER_ALIGN (type);

  if (!shared)
    return itype;

  if ((TYPE_MIN_VALUE (itype)
       && TREE_CODE (TYPE_MIN_VALUE (itype)) != INTEGER_CST)
      || (TYPE_MAX_VALUE (itype)
	  && TREE_CODE (TYPE_MAX_VALUE (itype)) != INTEGER_CST))
    {
      /* Since we cannot reliably merge this type, we need to compare it using
	 structural equality checks.  */
      SET_TYPE_STRUCTURAL_EQUALITY (itype);
      return itype;
    }

  inchash::add_expr (TYPE_MIN_VALUE (itype), hstate);
  inchash::add_expr (TYPE_MAX_VALUE (itype), hstate);
  hstate.merge_hash (TYPE_HASH (type));
  itype = type_hash_canon (hstate.end (), itype);

  return itype;
}

/* Wrapper around build_range_type_1 with SHARED set to true.  */

tree
build_range_type (tree type, tree lowval, tree highval)
{
  return build_range_type_1 (type, lowval, highval, true);
}

/* Wrapper around build_range_type_1 with SHARED set to false.  */

tree
build_nonshared_range_type (tree type, tree lowval, tree highval)
{
  return build_range_type_1 (type, lowval, highval, false);
}

/* Create a type of integers to be the TYPE_DOMAIN of an ARRAY_TYPE.
   MAXVAL should be the maximum value in the domain
   (one less than the length of the array).

   The maximum value that MAXVAL can have is INT_MAX for a HOST_WIDE_INT.
   We don't enforce this limit, that is up to caller (e.g. language front end).
   The limit exists because the result is a signed type and we don't handle
   sizes that use more than one HOST_WIDE_INT.  */

tree
build_index_type (tree maxval)
{
  return build_range_type (sizetype, size_zero_node, maxval);
}

/* Return true if the debug information for TYPE, a subtype, should be emitted
   as a subrange type.  If so, set LOWVAL to the low bound and HIGHVAL to the
   high bound, respectively.  Sometimes doing so unnecessarily obfuscates the
   debug info and doesn't reflect the source code.  */

bool
subrange_type_for_debug_p (const_tree type, tree *lowval, tree *highval)
{
  tree base_type = TREE_TYPE (type), low, high;

  /* Subrange types have a base type which is an integral type.  */
  if (!INTEGRAL_TYPE_P (base_type))
    return false;

  /* Get the real bounds of the subtype.  */
  if (lang_hooks.types.get_subrange_bounds)
    lang_hooks.types.get_subrange_bounds (type, &low, &high);
  else
    {
      low = TYPE_MIN_VALUE (type);
      high = TYPE_MAX_VALUE (type);
    }

  /* If the type and its base type have the same representation and the same
     name, then the type is not a subrange but a copy of the base type.  */
  if ((TREE_CODE (base_type) == INTEGER_TYPE
       || TREE_CODE (base_type) == BOOLEAN_TYPE)
      && int_size_in_bytes (type) == int_size_in_bytes (base_type)
      && tree_int_cst_equal (low, TYPE_MIN_VALUE (base_type))
      && tree_int_cst_equal (high, TYPE_MAX_VALUE (base_type))
      && TYPE_IDENTIFIER (type) == TYPE_IDENTIFIER (base_type))
    return false;

  if (lowval)
    *lowval = low;
  if (highval)
    *highval = high;
  return true;
}

/* Construct, lay out and return the type of arrays of elements with ELT_TYPE
   and number of elements specified by the range of values of INDEX_TYPE.
   If SHARED is true, reuse such a type that has already been constructed.  */

static tree
build_array_type_1 (tree elt_type, tree index_type, bool shared)
{
  tree t;

  if (TREE_CODE (elt_type) == FUNCTION_TYPE)
    {
      error ("arrays of functions are not meaningful");
      elt_type = integer_type_node;
    }

  t = make_node (ARRAY_TYPE);
  TREE_TYPE (t) = elt_type;
  TYPE_DOMAIN (t) = index_type;
  TYPE_ADDR_SPACE (t) = TYPE_ADDR_SPACE (elt_type);
  layout_type (t);

  /* If the element type is incomplete at this point we get marked for
     structural equality.  Do not record these types in the canonical
     type hashtable.  */
  if (TYPE_STRUCTURAL_EQUALITY_P (t))
    return t;

  if (shared)
    {
      inchash::hash hstate;
      hstate.add_object (TYPE_HASH (elt_type));
      if (index_type)
	hstate.add_object (TYPE_HASH (index_type));
      t = type_hash_canon (hstate.end (), t);
    }

  if (TYPE_CANONICAL (t) == t)
    {
      if (TYPE_STRUCTURAL_EQUALITY_P (elt_type)
	  || (index_type && TYPE_STRUCTURAL_EQUALITY_P (index_type))
	  || in_lto_p)
	SET_TYPE_STRUCTURAL_EQUALITY (t);
      else if (TYPE_CANONICAL (elt_type) != elt_type
	       || (index_type && TYPE_CANONICAL (index_type) != index_type))
	TYPE_CANONICAL (t)
	  = build_array_type_1 (TYPE_CANONICAL (elt_type),
				index_type
				? TYPE_CANONICAL (index_type) : NULL_TREE,
				shared);
    }

  return t;
}

/* Wrapper around build_array_type_1 with SHARED set to true.  */

tree
build_array_type (tree elt_type, tree index_type)
{
  return build_array_type_1 (elt_type, index_type, true);
}

/* Wrapper around build_array_type_1 with SHARED set to false.  */

tree
build_nonshared_array_type (tree elt_type, tree index_type)
{
  return build_array_type_1 (elt_type, index_type, false);
}

/* Return a representation of ELT_TYPE[NELTS], using indices of type
   sizetype.  */

tree
build_array_type_nelts (tree elt_type, unsigned HOST_WIDE_INT nelts)
{
  return build_array_type (elt_type, build_index_type (size_int (nelts - 1)));
}

/* Recursively examines the array elements of TYPE, until a non-array
   element type is found.  */

tree
strip_array_types (tree type)
{
  while (TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);

  return type;
}

/* Computes the canonical argument types from the argument type list
   ARGTYPES.

   Upon return, *ANY_STRUCTURAL_P will be true iff either it was true
   on entry to this function, or if any of the ARGTYPES are
   structural.

   Upon return, *ANY_NONCANONICAL_P will be true iff either it was
   true on entry to this function, or if any of the ARGTYPES are
   non-canonical.

   Returns a canonical argument list, which may be ARGTYPES when the
   canonical argument list is unneeded (i.e., *ANY_STRUCTURAL_P is
   true) or would not differ from ARGTYPES.  */

static tree
maybe_canonicalize_argtypes (tree argtypes,
			     bool *any_structural_p,
			     bool *any_noncanonical_p)
{
  tree arg;
  bool any_noncanonical_argtypes_p = false;

  for (arg = argtypes; arg && !(*any_structural_p); arg = TREE_CHAIN (arg))
    {
      if (!TREE_VALUE (arg) || TREE_VALUE (arg) == error_mark_node)
	/* Fail gracefully by stating that the type is structural.  */
	*any_structural_p = true;
      else if (TYPE_STRUCTURAL_EQUALITY_P (TREE_VALUE (arg)))
	*any_structural_p = true;
      else if (TYPE_CANONICAL (TREE_VALUE (arg)) != TREE_VALUE (arg)
	       || TREE_PURPOSE (arg))
	/* If the argument has a default argument, we consider it
	   non-canonical even though the type itself is canonical.
	   That way, different variants of function and method types
	   with default arguments will all point to the variant with
	   no defaults as their canonical type.  */
        any_noncanonical_argtypes_p = true;
    }

  if (*any_structural_p)
    return argtypes;

  if (any_noncanonical_argtypes_p)
    {
      /* Build the canonical list of argument types.  */
      tree canon_argtypes = NULL_TREE;
      bool is_void = false;

      for (arg = argtypes; arg; arg = TREE_CHAIN (arg))
        {
          if (arg == void_list_node)
            is_void = true;
          else
            canon_argtypes = tree_cons (NULL_TREE,
                                        TYPE_CANONICAL (TREE_VALUE (arg)),
                                        canon_argtypes);
        }

      canon_argtypes = nreverse (canon_argtypes);
      if (is_void)
        canon_argtypes = chainon (canon_argtypes, void_list_node);

      /* There is a non-canonical type.  */
      *any_noncanonical_p = true;
      return canon_argtypes;
    }

  /* The canonical argument types are the same as ARGTYPES.  */
  return argtypes;
}

/* Construct, lay out and return
   the type of functions returning type VALUE_TYPE
   given arguments of types ARG_TYPES.
   ARG_TYPES is a chain of TREE_LIST nodes whose TREE_VALUEs
   are data type nodes for the arguments of the function.
   If such a type has already been constructed, reuse it.  */

tree
build_function_type (tree value_type, tree arg_types)
{
  tree t;
  inchash::hash hstate;
  bool any_structural_p, any_noncanonical_p;
  tree canon_argtypes;

  if (TREE_CODE (value_type) == FUNCTION_TYPE)
    {
      error ("function return type cannot be function");
      value_type = integer_type_node;
    }

  /* Make a node of the sort we want.  */
  t = make_node (FUNCTION_TYPE);
  TREE_TYPE (t) = value_type;
  TYPE_ARG_TYPES (t) = arg_types;

  /* If we already have such a type, use the old one.  */
  hstate.add_object (TYPE_HASH (value_type));
  type_hash_list (arg_types, hstate);
  t = type_hash_canon (hstate.end (), t);

  /* Set up the canonical type. */
  any_structural_p   = TYPE_STRUCTURAL_EQUALITY_P (value_type);
  any_noncanonical_p = TYPE_CANONICAL (value_type) != value_type;
  canon_argtypes = maybe_canonicalize_argtypes (arg_types,
						&any_structural_p,
						&any_noncanonical_p);
  if (any_structural_p)
    SET_TYPE_STRUCTURAL_EQUALITY (t);
  else if (any_noncanonical_p)
    TYPE_CANONICAL (t) = build_function_type (TYPE_CANONICAL (value_type),
					      canon_argtypes);

  if (!COMPLETE_TYPE_P (t))
    layout_type (t);
  return t;
}

/* Build a function type.  The RETURN_TYPE is the type returned by the
   function.  If VAARGS is set, no void_type_node is appended to the
   list.  ARGP must be always be terminated be a NULL_TREE.  */

static tree
build_function_type_list_1 (bool vaargs, tree return_type, va_list argp)
{
  tree t, args, last;

  t = va_arg (argp, tree);
  for (args = NULL_TREE; t != NULL_TREE; t = va_arg (argp, tree))
    args = tree_cons (NULL_TREE, t, args);

  if (vaargs)
    {
      last = args;
      if (args != NULL_TREE)
	args = nreverse (args);
      gcc_assert (last != void_list_node);
    }
  else if (args == NULL_TREE)
    args = void_list_node;
  else
    {
      last = args;
      args = nreverse (args);
      TREE_CHAIN (last) = void_list_node;
    }
  args = build_function_type (return_type, args);

  return args;
}

/* Build a function type.  The RETURN_TYPE is the type returned by the
   function.  If additional arguments are provided, they are
   additional argument types.  The list of argument types must always
   be terminated by NULL_TREE.  */

tree
build_function_type_list (tree return_type, ...)
{
  tree args;
  va_list p;

  va_start (p, return_type);
  args = build_function_type_list_1 (false, return_type, p);
  va_end (p);
  return args;
}

/* Build a variable argument function type.  The RETURN_TYPE is the
   type returned by the function.  If additional arguments are provided,
   they are additional argument types.  The list of argument types must
   always be terminated by NULL_TREE.  */

tree
build_varargs_function_type_list (tree return_type, ...)
{
  tree args;
  va_list p;

  va_start (p, return_type);
  args = build_function_type_list_1 (true, return_type, p);
  va_end (p);

  return args;
}

/* Build a function type.  RETURN_TYPE is the type returned by the
   function; VAARGS indicates whether the function takes varargs.  The
   function takes N named arguments, the types of which are provided in
   ARG_TYPES.  */

static tree
build_function_type_array_1 (bool vaargs, tree return_type, int n,
			     tree *arg_types)
{
  int i;
  tree t = vaargs ? NULL_TREE : void_list_node;

  for (i = n - 1; i >= 0; i--)
    t = tree_cons (NULL_TREE, arg_types[i], t);

  return build_function_type (return_type, t);
}

/* Build a function type.  RETURN_TYPE is the type returned by the
   function.  The function takes N named arguments, the types of which
   are provided in ARG_TYPES.  */

tree
build_function_type_array (tree return_type, int n, tree *arg_types)
{
  return build_function_type_array_1 (false, return_type, n, arg_types);
}

/* Build a variable argument function type.  RETURN_TYPE is the type
   returned by the function.  The function takes N named arguments, the
   types of which are provided in ARG_TYPES.  */

tree
build_varargs_function_type_array (tree return_type, int n, tree *arg_types)
{
  return build_function_type_array_1 (true, return_type, n, arg_types);
}

/* Build a METHOD_TYPE for a member of BASETYPE.  The RETTYPE (a TYPE)
   and ARGTYPES (a TREE_LIST) are the return type and arguments types
   for the method.  An implicit additional parameter (of type
   pointer-to-BASETYPE) is added to the ARGTYPES.  */

tree
build_method_type_directly (tree basetype,
			    tree rettype,
			    tree argtypes)
{
  tree t;
  tree ptype;
  inchash::hash hstate;
  bool any_structural_p, any_noncanonical_p;
  tree canon_argtypes;

  /* Make a node of the sort we want.  */
  t = make_node (METHOD_TYPE);

  TYPE_METHOD_BASETYPE (t) = TYPE_MAIN_VARIANT (basetype);
  TREE_TYPE (t) = rettype;
  ptype = build_pointer_type (basetype);

  /* The actual arglist for this function includes a "hidden" argument
     which is "this".  Put it into the list of argument types.  */
  argtypes = tree_cons (NULL_TREE, ptype, argtypes);
  TYPE_ARG_TYPES (t) = argtypes;

  /* If we already have such a type, use the old one.  */
  hstate.add_object (TYPE_HASH (basetype));
  hstate.add_object (TYPE_HASH (rettype));
  type_hash_list (argtypes, hstate);
  t = type_hash_canon (hstate.end (), t);

  /* Set up the canonical type. */
  any_structural_p
    = (TYPE_STRUCTURAL_EQUALITY_P (basetype)
       || TYPE_STRUCTURAL_EQUALITY_P (rettype));
  any_noncanonical_p
    = (TYPE_CANONICAL (basetype) != basetype
       || TYPE_CANONICAL (rettype) != rettype);
  canon_argtypes = maybe_canonicalize_argtypes (TREE_CHAIN (argtypes),
						&any_structural_p,
						&any_noncanonical_p);
  if (any_structural_p)
    SET_TYPE_STRUCTURAL_EQUALITY (t);
  else if (any_noncanonical_p)
    TYPE_CANONICAL (t)
      = build_method_type_directly (TYPE_CANONICAL (basetype),
				    TYPE_CANONICAL (rettype),
				    canon_argtypes);
  if (!COMPLETE_TYPE_P (t))
    layout_type (t);

  return t;
}

/* Construct, lay out and return the type of methods belonging to class
   BASETYPE and whose arguments and values are described by TYPE.
   If that type exists already, reuse it.
   TYPE must be a FUNCTION_TYPE node.  */

tree
build_method_type (tree basetype, tree type)
{
  gcc_assert (TREE_CODE (type) == FUNCTION_TYPE);

  return build_method_type_directly (basetype,
				     TREE_TYPE (type),
				     TYPE_ARG_TYPES (type));
}

/* Construct, lay out and return the type of offsets to a value
   of type TYPE, within an object of type BASETYPE.
   If a suitable offset type exists already, reuse it.  */

tree
build_offset_type (tree basetype, tree type)
{
  tree t;
  inchash::hash hstate;

  /* Make a node of the sort we want.  */
  t = make_node (OFFSET_TYPE);

  TYPE_OFFSET_BASETYPE (t) = TYPE_MAIN_VARIANT (basetype);
  TREE_TYPE (t) = type;

  /* If we already have such a type, use the old one.  */
  hstate.add_object (TYPE_HASH (basetype));
  hstate.add_object (TYPE_HASH (type));
  t = type_hash_canon (hstate.end (), t);

  if (!COMPLETE_TYPE_P (t))
    layout_type (t);

  if (TYPE_CANONICAL (t) == t)
    {
      if (TYPE_STRUCTURAL_EQUALITY_P (basetype)
	  || TYPE_STRUCTURAL_EQUALITY_P (type))
	SET_TYPE_STRUCTURAL_EQUALITY (t);
      else if (TYPE_CANONICAL (TYPE_MAIN_VARIANT (basetype)) != basetype
	       || TYPE_CANONICAL (type) != type)
	TYPE_CANONICAL (t)
	  = build_offset_type (TYPE_CANONICAL (TYPE_MAIN_VARIANT (basetype)),
			       TYPE_CANONICAL (type));
    }

  return t;
}

/* Create a complex type whose components are COMPONENT_TYPE.  */

tree
build_complex_type (tree component_type)
{
  tree t;
  inchash::hash hstate;

  gcc_assert (INTEGRAL_TYPE_P (component_type)
	      || SCALAR_FLOAT_TYPE_P (component_type)
	      || FIXED_POINT_TYPE_P (component_type));

  /* Make a node of the sort we want.  */
  t = make_node (COMPLEX_TYPE);

  TREE_TYPE (t) = TYPE_MAIN_VARIANT (component_type);

  /* If we already have such a type, use the old one.  */
  hstate.add_object (TYPE_HASH (component_type));
  t = type_hash_canon (hstate.end (), t);

  if (!COMPLETE_TYPE_P (t))
    layout_type (t);

  if (TYPE_CANONICAL (t) == t)
    {
      if (TYPE_STRUCTURAL_EQUALITY_P (component_type))
	SET_TYPE_STRUCTURAL_EQUALITY (t);
      else if (TYPE_CANONICAL (component_type) != component_type)
	TYPE_CANONICAL (t)
	  = build_complex_type (TYPE_CANONICAL (component_type));
    }

  /* We need to create a name, since complex is a fundamental type.  */
  if (! TYPE_NAME (t))
    {
      const char *name;
      if (component_type == char_type_node)
	name = "complex char";
      else if (component_type == signed_char_type_node)
	name = "complex signed char";
      else if (component_type == unsigned_char_type_node)
	name = "complex unsigned char";
      else if (component_type == short_integer_type_node)
	name = "complex short int";
      else if (component_type == short_unsigned_type_node)
	name = "complex short unsigned int";
      else if (component_type == integer_type_node)
	name = "complex int";
      else if (component_type == unsigned_type_node)
	name = "complex unsigned int";
      else if (component_type == long_integer_type_node)
	name = "complex long int";
      else if (component_type == long_unsigned_type_node)
	name = "complex long unsigned int";
      else if (component_type == long_long_integer_type_node)
	name = "complex long long int";
      else if (component_type == long_long_unsigned_type_node)
	name = "complex long long unsigned int";
      else
	name = 0;

      if (name != 0)
	TYPE_NAME (t) = build_decl (UNKNOWN_LOCATION, TYPE_DECL,
	    			    get_identifier (name), t);
    }

  return build_qualified_type (t, TYPE_QUALS (component_type));
}

/* If TYPE is a real or complex floating-point type and the target
   does not directly support arithmetic on TYPE then return the wider
   type to be used for arithmetic on TYPE.  Otherwise, return
   NULL_TREE.  */

tree
excess_precision_type (tree type)
{
  if (flag_excess_precision != EXCESS_PRECISION_FAST)
    {
      int flt_eval_method = TARGET_FLT_EVAL_METHOD;
      switch (TREE_CODE (type))
	{
	case REAL_TYPE:
	  switch (flt_eval_method)
	    {
	    case 1:
	      if (TYPE_MODE (type) == TYPE_MODE (float_type_node))
		return double_type_node;
	      break;
	    case 2:
	      if (TYPE_MODE (type) == TYPE_MODE (float_type_node)
		  || TYPE_MODE (type) == TYPE_MODE (double_type_node))
		return long_double_type_node;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  break;
	case COMPLEX_TYPE:
	  if (TREE_CODE (TREE_TYPE (type)) != REAL_TYPE)
	    return NULL_TREE;
	  switch (flt_eval_method)
	    {
	    case 1:
	      if (TYPE_MODE (TREE_TYPE (type)) == TYPE_MODE (float_type_node))
		return complex_double_type_node;
	      break;
	    case 2:
	      if (TYPE_MODE (TREE_TYPE (type)) == TYPE_MODE (float_type_node)
		  || (TYPE_MODE (TREE_TYPE (type))
		      == TYPE_MODE (double_type_node)))
		return complex_long_double_type_node;
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  break;
	default:
	  break;
	}
    }
  return NULL_TREE;
}

/* Return OP, stripped of any conversions to wider types as much as is safe.
   Converting the value back to OP's type makes a value equivalent to OP.

   If FOR_TYPE is nonzero, we return a value which, if converted to
   type FOR_TYPE, would be equivalent to converting OP to type FOR_TYPE.

   OP must have integer, real or enumeral type.  Pointers are not allowed!

   There are some cases where the obvious value we could return
   would regenerate to OP if converted to OP's type,
   but would not extend like OP to wider types.
   If FOR_TYPE indicates such extension is contemplated, we eschew such values.
   For example, if OP is (unsigned short)(signed char)-1,
   we avoid returning (signed char)-1 if FOR_TYPE is int,
   even though extending that to an unsigned short would regenerate OP,
   since the result of extending (signed char)-1 to (int)
   is different from (int) OP.  */

tree
get_unwidened (tree op, tree for_type)
{
  /* Set UNS initially if converting OP to FOR_TYPE is a zero-extension.  */
  tree type = TREE_TYPE (op);
  unsigned final_prec
    = TYPE_PRECISION (for_type != 0 ? for_type : type);
  int uns
    = (for_type != 0 && for_type != type
       && final_prec > TYPE_PRECISION (type)
       && TYPE_UNSIGNED (type));
  tree win = op;

  while (CONVERT_EXPR_P (op))
    {
      int bitschange;

      /* TYPE_PRECISION on vector types has different meaning
	 (TYPE_VECTOR_SUBPARTS) and casts from vectors are view conversions,
	 so avoid them here.  */
      if (TREE_CODE (TREE_TYPE (TREE_OPERAND (op, 0))) == VECTOR_TYPE)
	break;

      bitschange = TYPE_PRECISION (TREE_TYPE (op))
		   - TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op, 0)));

      /* Truncations are many-one so cannot be removed.
	 Unless we are later going to truncate down even farther.  */
      if (bitschange < 0
	  && final_prec > TYPE_PRECISION (TREE_TYPE (op)))
	break;

      /* See what's inside this conversion.  If we decide to strip it,
	 we will set WIN.  */
      op = TREE_OPERAND (op, 0);

      /* If we have not stripped any zero-extensions (uns is 0),
	 we can strip any kind of extension.
	 If we have previously stripped a zero-extension,
	 only zero-extensions can safely be stripped.
	 Any extension can be stripped if the bits it would produce
	 are all going to be discarded later by truncating to FOR_TYPE.  */

      if (bitschange > 0)
	{
	  if (! uns || final_prec <= TYPE_PRECISION (TREE_TYPE (op)))
	    win = op;
	  /* TYPE_UNSIGNED says whether this is a zero-extension.
	     Let's avoid computing it if it does not affect WIN
	     and if UNS will not be needed again.  */
	  if ((uns
	       || CONVERT_EXPR_P (op))
	      && TYPE_UNSIGNED (TREE_TYPE (op)))
	    {
	      uns = 1;
	      win = op;
	    }
	}
    }

  /* If we finally reach a constant see if it fits in for_type and
     in that case convert it.  */
  if (for_type
      && TREE_CODE (win) == INTEGER_CST
      && TREE_TYPE (win) != for_type
      && int_fits_type_p (win, for_type))
    win = fold_convert (for_type, win);

  return win;
}

/* Return OP or a simpler expression for a narrower value
   which can be sign-extended or zero-extended to give back OP.
   Store in *UNSIGNEDP_PTR either 1 if the value should be zero-extended
   or 0 if the value should be sign-extended.  */

tree
get_narrower (tree op, int *unsignedp_ptr)
{
  int uns = 0;
  int first = 1;
  tree win = op;
  bool integral_p = INTEGRAL_TYPE_P (TREE_TYPE (op));

  while (TREE_CODE (op) == NOP_EXPR)
    {
      int bitschange
	= (TYPE_PRECISION (TREE_TYPE (op))
	   - TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op, 0))));

      /* Truncations are many-one so cannot be removed.  */
      if (bitschange < 0)
	break;

      /* See what's inside this conversion.  If we decide to strip it,
	 we will set WIN.  */

      if (bitschange > 0)
	{
	  op = TREE_OPERAND (op, 0);
	  /* An extension: the outermost one can be stripped,
	     but remember whether it is zero or sign extension.  */
	  if (first)
	    uns = TYPE_UNSIGNED (TREE_TYPE (op));
	  /* Otherwise, if a sign extension has been stripped,
	     only sign extensions can now be stripped;
	     if a zero extension has been stripped, only zero-extensions.  */
	  else if (uns != TYPE_UNSIGNED (TREE_TYPE (op)))
	    break;
	  first = 0;
	}
      else /* bitschange == 0 */
	{
	  /* A change in nominal type can always be stripped, but we must
	     preserve the unsignedness.  */
	  if (first)
	    uns = TYPE_UNSIGNED (TREE_TYPE (op));
	  first = 0;
	  op = TREE_OPERAND (op, 0);
	  /* Keep trying to narrow, but don't assign op to win if it
	     would turn an integral type into something else.  */
	  if (INTEGRAL_TYPE_P (TREE_TYPE (op)) != integral_p)
	    continue;
	}

      win = op;
    }

  if (TREE_CODE (op) == COMPONENT_REF
      /* Since type_for_size always gives an integer type.  */
      && TREE_CODE (TREE_TYPE (op)) != REAL_TYPE
      && TREE_CODE (TREE_TYPE (op)) != FIXED_POINT_TYPE
      /* Ensure field is laid out already.  */
      && DECL_SIZE (TREE_OPERAND (op, 1)) != 0
      && tree_fits_uhwi_p (DECL_SIZE (TREE_OPERAND (op, 1))))
    {
      unsigned HOST_WIDE_INT innerprec
	= tree_to_uhwi (DECL_SIZE (TREE_OPERAND (op, 1)));
      int unsignedp = (DECL_UNSIGNED (TREE_OPERAND (op, 1))
		       || TYPE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op, 1))));
      tree type = lang_hooks.types.type_for_size (innerprec, unsignedp);

      /* We can get this structure field in a narrower type that fits it,
	 but the resulting extension to its nominal type (a fullword type)
	 must satisfy the same conditions as for other extensions.

	 Do this only for fields that are aligned (not bit-fields),
	 because when bit-field insns will be used there is no
	 advantage in doing this.  */

      if (innerprec < TYPE_PRECISION (TREE_TYPE (op))
	  && ! DECL_BIT_FIELD (TREE_OPERAND (op, 1))
	  && (first || uns == DECL_UNSIGNED (TREE_OPERAND (op, 1)))
	  && type != 0)
	{
	  if (first)
	    uns = DECL_UNSIGNED (TREE_OPERAND (op, 1));
	  win = fold_convert (type, op);
	}
    }

  *unsignedp_ptr = uns;
  return win;
}

/* Returns true if integer constant C has a value that is permissible
   for type TYPE (an INTEGER_TYPE).  */

bool
int_fits_type_p (const_tree c, const_tree type)
{
  tree type_low_bound, type_high_bound;
  bool ok_for_low_bound, ok_for_high_bound;
  signop sgn_c = TYPE_SIGN (TREE_TYPE (c));

retry:
  type_low_bound = TYPE_MIN_VALUE (type);
  type_high_bound = TYPE_MAX_VALUE (type);

  /* If at least one bound of the type is a constant integer, we can check
     ourselves and maybe make a decision. If no such decision is possible, but
     this type is a subtype, try checking against that.  Otherwise, use
     fits_to_tree_p, which checks against the precision.

     Compute the status for each possibly constant bound, and return if we see
     one does not match. Use ok_for_xxx_bound for this purpose, assigning -1
     for "unknown if constant fits", 0 for "constant known *not* to fit" and 1
     for "constant known to fit".  */

  /* Check if c >= type_low_bound.  */
  if (type_low_bound && TREE_CODE (type_low_bound) == INTEGER_CST)
    {
      if (tree_int_cst_lt (c, type_low_bound))
	return false;
      ok_for_low_bound = true;
    }
  else
    ok_for_low_bound = false;

  /* Check if c <= type_high_bound.  */
  if (type_high_bound && TREE_CODE (type_high_bound) == INTEGER_CST)
    {
      if (tree_int_cst_lt (type_high_bound, c))
	return false;
      ok_for_high_bound = true;
    }
  else
    ok_for_high_bound = false;

  /* If the constant fits both bounds, the result is known.  */
  if (ok_for_low_bound && ok_for_high_bound)
    return true;

  /* Perform some generic filtering which may allow making a decision
     even if the bounds are not constant.  First, negative integers
     never fit in unsigned types, */
  if (TYPE_UNSIGNED (type) && sgn_c == SIGNED && wi::neg_p (c))
    return false;

  /* Second, narrower types always fit in wider ones.  */
  if (TYPE_PRECISION (type) > TYPE_PRECISION (TREE_TYPE (c)))
    return true;

  /* Third, unsigned integers with top bit set never fit signed types.  */
  if (!TYPE_UNSIGNED (type) && sgn_c == UNSIGNED)
    {
      int prec = GET_MODE_PRECISION (TYPE_MODE (TREE_TYPE (c))) - 1;
      if (prec < TYPE_PRECISION (TREE_TYPE (c)))
	{
	  /* When a tree_cst is converted to a wide-int, the precision
	     is taken from the type.  However, if the precision of the
	     mode underneath the type is smaller than that, it is
	     possible that the value will not fit.  The test below
	     fails if any bit is set between the sign bit of the
	     underlying mode and the top bit of the type.  */
	  if (wi::ne_p (wi::zext (c, prec - 1), c))
	    return false;
	}
      else if (wi::neg_p (c))
	return false;
    }

  /* If we haven't been able to decide at this point, there nothing more we
     can check ourselves here.  Look at the base type if we have one and it
     has the same precision.  */
  if (TREE_CODE (type) == INTEGER_TYPE
      && TREE_TYPE (type) != 0
      && TYPE_PRECISION (type) == TYPE_PRECISION (TREE_TYPE (type)))
    {
      type = TREE_TYPE (type);
      goto retry;
    }

  /* Or to fits_to_tree_p, if nothing else.  */
  return wi::fits_to_tree_p (c, type);
}

/* Stores bounds of an integer TYPE in MIN and MAX.  If TYPE has non-constant
   bounds or is a POINTER_TYPE, the maximum and/or minimum values that can be
   represented (assuming two's-complement arithmetic) within the bit
   precision of the type are returned instead.  */

void
get_type_static_bounds (const_tree type, mpz_t min, mpz_t max)
{
  if (!POINTER_TYPE_P (type) && TYPE_MIN_VALUE (type)
      && TREE_CODE (TYPE_MIN_VALUE (type)) == INTEGER_CST)
    wi::to_mpz (TYPE_MIN_VALUE (type), min, TYPE_SIGN (type));
  else
    {
      if (TYPE_UNSIGNED (type))
	mpz_set_ui (min, 0);
      else
	{
	  wide_int mn = wi::min_value (TYPE_PRECISION (type), SIGNED);
	  wi::to_mpz (mn, min, SIGNED);
	}
    }

  if (!POINTER_TYPE_P (type) && TYPE_MAX_VALUE (type)
      && TREE_CODE (TYPE_MAX_VALUE (type)) == INTEGER_CST)
    wi::to_mpz (TYPE_MAX_VALUE (type), max, TYPE_SIGN (type));
  else
    {
      wide_int mn = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));
      wi::to_mpz (mn, max, TYPE_SIGN (type));
    }
}

/* Return true if VAR is an automatic variable defined in function FN.  */

bool
auto_var_in_fn_p (const_tree var, const_tree fn)
{
  return (DECL_P (var) && DECL_CONTEXT (var) == fn
	  && ((((TREE_CODE (var) == VAR_DECL && ! DECL_EXTERNAL (var))
		|| TREE_CODE (var) == PARM_DECL)
	       && ! TREE_STATIC (var))
	      || TREE_CODE (var) == LABEL_DECL
	      || TREE_CODE (var) == RESULT_DECL));
}

/* Subprogram of following function.  Called by walk_tree.

   Return *TP if it is an automatic variable or parameter of the
   function passed in as DATA.  */

static tree
find_var_from_fn (tree *tp, int *walk_subtrees, void *data)
{
  tree fn = (tree) data;

  if (TYPE_P (*tp))
    *walk_subtrees = 0;

  else if (DECL_P (*tp)
	   && auto_var_in_fn_p (*tp, fn))
    return *tp;

  return NULL_TREE;
}

/* Returns true if T is, contains, or refers to a type with variable
   size.  For METHOD_TYPEs and FUNCTION_TYPEs we exclude the
   arguments, but not the return type.  If FN is nonzero, only return
   true if a modifier of the type or position of FN is a variable or
   parameter inside FN.

   This concept is more general than that of C99 'variably modified types':
   in C99, a struct type is never variably modified because a VLA may not
   appear as a structure member.  However, in GNU C code like:

     struct S { int i[f()]; };

   is valid, and other languages may define similar constructs.  */

bool
variably_modified_type_p (tree type, tree fn)
{
  tree t;

/* Test if T is either variable (if FN is zero) or an expression containing
   a variable in FN.  If TYPE isn't gimplified, return true also if
   gimplify_one_sizepos would gimplify the expression into a local
   variable.  */
#define RETURN_TRUE_IF_VAR(T)						\
  do { tree _t = (T);							\
    if (_t != NULL_TREE							\
	&& _t != error_mark_node					\
	&& TREE_CODE (_t) != INTEGER_CST				\
	&& TREE_CODE (_t) != PLACEHOLDER_EXPR				\
	&& (!fn								\
	    || (!TYPE_SIZES_GIMPLIFIED (type)				\
		&& !is_gimple_sizepos (_t))				\
	    || walk_tree (&_t, find_var_from_fn, fn, NULL)))		\
      return true;  } while (0)

  if (type == error_mark_node)
    return false;

  /* If TYPE itself has variable size, it is variably modified.  */
  RETURN_TRUE_IF_VAR (TYPE_SIZE (type));
  RETURN_TRUE_IF_VAR (TYPE_SIZE_UNIT (type));

  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case VECTOR_TYPE:
      if (variably_modified_type_p (TREE_TYPE (type), fn))
	return true;
      break;

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      /* If TYPE is a function type, it is variably modified if the
	 return type is variably modified.  */
      if (variably_modified_type_p (TREE_TYPE (type), fn))
	  return true;
      break;

    case INTEGER_TYPE:
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
      /* Scalar types are variably modified if their end points
	 aren't constant.  */
      RETURN_TRUE_IF_VAR (TYPE_MIN_VALUE (type));
      RETURN_TRUE_IF_VAR (TYPE_MAX_VALUE (type));
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      /* We can't see if any of the fields are variably-modified by the
	 definition we normally use, since that would produce infinite
	 recursion via pointers.  */
      /* This is variably modified if some field's type is.  */
      for (t = TYPE_FIELDS (type); t; t = DECL_CHAIN (t))
	if (TREE_CODE (t) == FIELD_DECL)
	  {
	    RETURN_TRUE_IF_VAR (DECL_FIELD_OFFSET (t));
	    RETURN_TRUE_IF_VAR (DECL_SIZE (t));
	    RETURN_TRUE_IF_VAR (DECL_SIZE_UNIT (t));

	    if (TREE_CODE (type) == QUAL_UNION_TYPE)
	      RETURN_TRUE_IF_VAR (DECL_QUALIFIER (t));
	  }
      break;

    case ARRAY_TYPE:
      /* Do not call ourselves to avoid infinite recursion.  This is
	 variably modified if the element type is.  */
      RETURN_TRUE_IF_VAR (TYPE_SIZE (TREE_TYPE (type)));
      RETURN_TRUE_IF_VAR (TYPE_SIZE_UNIT (TREE_TYPE (type)));
      break;

    default:
      break;
    }

  /* The current language may have other cases to check, but in general,
     all other types are not variably modified.  */
  return lang_hooks.tree_inlining.var_mod_type_p (type, fn);

#undef RETURN_TRUE_IF_VAR
}

/* Given a DECL or TYPE, return the scope in which it was declared, or
   NULL_TREE if there is no containing scope.  */

tree
get_containing_scope (const_tree t)
{
  return (TYPE_P (t) ? TYPE_CONTEXT (t) : DECL_CONTEXT (t));
}

/* Return the innermost context enclosing DECL that is
   a FUNCTION_DECL, or zero if none.  */

tree
decl_function_context (const_tree decl)
{
  tree context;

  if (TREE_CODE (decl) == ERROR_MARK)
    return 0;

  /* C++ virtual functions use DECL_CONTEXT for the class of the vtable
     where we look up the function at runtime.  Such functions always take
     a first argument of type 'pointer to real context'.

     C++ should really be fixed to use DECL_CONTEXT for the real context,
     and use something else for the "virtual context".  */
  else if (TREE_CODE (decl) == FUNCTION_DECL && DECL_VINDEX (decl))
    context
      = TYPE_MAIN_VARIANT
	(TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (decl)))));
  else
    context = DECL_CONTEXT (decl);

  while (context && TREE_CODE (context) != FUNCTION_DECL)
    {
      if (TREE_CODE (context) == BLOCK)
	context = BLOCK_SUPERCONTEXT (context);
      else
	context = get_containing_scope (context);
    }

  return context;
}

/* Return the innermost context enclosing DECL that is
   a RECORD_TYPE, UNION_TYPE or QUAL_UNION_TYPE, or zero if none.
   TYPE_DECLs and FUNCTION_DECLs are transparent to this function.  */

tree
decl_type_context (const_tree decl)
{
  tree context = DECL_CONTEXT (decl);

  while (context)
    switch (TREE_CODE (context))
      {
      case NAMESPACE_DECL:
      case TRANSLATION_UNIT_DECL:
	return NULL_TREE;

      case RECORD_TYPE:
      case UNION_TYPE:
      case QUAL_UNION_TYPE:
	return context;

      case TYPE_DECL:
      case FUNCTION_DECL:
	context = DECL_CONTEXT (context);
	break;

      case BLOCK:
	context = BLOCK_SUPERCONTEXT (context);
	break;

      default:
	gcc_unreachable ();
      }

  return NULL_TREE;
}

/* CALL is a CALL_EXPR.  Return the declaration for the function
   called, or NULL_TREE if the called function cannot be
   determined.  */

tree
get_callee_fndecl (const_tree call)
{
  tree addr;

  if (call == error_mark_node)
    return error_mark_node;

  /* It's invalid to call this function with anything but a
     CALL_EXPR.  */
  gcc_assert (TREE_CODE (call) == CALL_EXPR);

  /* The first operand to the CALL is the address of the function
     called.  */
  addr = CALL_EXPR_FN (call);

  /* If there is no function, return early.  */
  if (addr == NULL_TREE)
    return NULL_TREE;

  STRIP_NOPS (addr);

  /* If this is a readonly function pointer, extract its initial value.  */
  if (DECL_P (addr) && TREE_CODE (addr) != FUNCTION_DECL
      && TREE_READONLY (addr) && ! TREE_THIS_VOLATILE (addr)
      && DECL_INITIAL (addr))
    addr = DECL_INITIAL (addr);

  /* If the address is just `&f' for some function `f', then we know
     that `f' is being called.  */
  if (TREE_CODE (addr) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (addr, 0)) == FUNCTION_DECL)
    return TREE_OPERAND (addr, 0);

  /* We couldn't figure out what was being called.  */
  return NULL_TREE;
}

/* If CALL_EXPR CALL calls a normal built-in function or an internal function,
   return the associated function code, otherwise return CFN_LAST.  */

combined_fn
get_call_combined_fn (const_tree call)
{
  /* It's invalid to call this function with anything but a CALL_EXPR.  */
  gcc_assert (TREE_CODE (call) == CALL_EXPR);

  if (!CALL_EXPR_FN (call))
    return as_combined_fn (CALL_EXPR_IFN (call));

  tree fndecl = get_callee_fndecl (call);
  if (fndecl && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    return as_combined_fn (DECL_FUNCTION_CODE (fndecl));

  return CFN_LAST;
}

#define TREE_MEM_USAGE_SPACES 40

/* Print debugging information about tree nodes generated during the compile,
   and any language-specific information.  */

void
dump_tree_statistics (void)
{
  if (GATHER_STATISTICS)
    {
      int i;
      int total_nodes, total_bytes;
      fprintf (stderr, "\nKind                   Nodes      Bytes\n");
      mem_usage::print_dash_line (TREE_MEM_USAGE_SPACES);
      total_nodes = total_bytes = 0;
      for (i = 0; i < (int) all_kinds; i++)
	{
	  fprintf (stderr, "%-20s %7d %10d\n", tree_node_kind_names[i],
		   tree_node_counts[i], tree_node_sizes[i]);
	  total_nodes += tree_node_counts[i];
	  total_bytes += tree_node_sizes[i];
	}
      mem_usage::print_dash_line (TREE_MEM_USAGE_SPACES);
      fprintf (stderr, "%-20s %7d %10d\n", "Total", total_nodes, total_bytes);
      mem_usage::print_dash_line (TREE_MEM_USAGE_SPACES);
      fprintf (stderr, "Code                   Nodes\n");
      mem_usage::print_dash_line (TREE_MEM_USAGE_SPACES);
      for (i = 0; i < (int) MAX_TREE_CODES; i++)
	fprintf (stderr, "%-32s %7d\n", get_tree_code_name ((enum tree_code) i),
                 tree_code_counts[i]);
      mem_usage::print_dash_line (TREE_MEM_USAGE_SPACES);
      fprintf (stderr, "\n");
      ssanames_print_statistics ();
      fprintf (stderr, "\n");
      phinodes_print_statistics ();
      fprintf (stderr, "\n");
    }
  else
    fprintf (stderr, "(No per-node statistics)\n");

  print_type_hash_statistics ();
  print_debug_expr_statistics ();
  print_value_expr_statistics ();
  lang_hooks.print_statistics ();
}

#define FILE_FUNCTION_FORMAT "_GLOBAL__%s_%s"

/* Generate a crc32 of a byte.  */

static unsigned
crc32_unsigned_bits (unsigned chksum, unsigned value, unsigned bits)
{
  unsigned ix;

  for (ix = bits; ix--; value <<= 1)
    {
      unsigned feedback;
      
      feedback = (value ^ chksum) & 0x80000000 ? 0x04c11db7 : 0;
      chksum <<= 1;
      chksum ^= feedback;
    }
  return chksum;
}

/* Generate a crc32 of a 32-bit unsigned.  */

unsigned
crc32_unsigned (unsigned chksum, unsigned value)
{
  return crc32_unsigned_bits (chksum, value, 32);
}

/* Generate a crc32 of a byte.  */

unsigned
crc32_byte (unsigned chksum, char byte)
{
  return crc32_unsigned_bits (chksum, (unsigned) byte << 24, 8);
}

/* Generate a crc32 of a string.  */

unsigned
crc32_string (unsigned chksum, const char *string)
{
  do
    {
      chksum = crc32_byte (chksum, *string);
    }
  while (*string++);
  return chksum;
}

/* P is a string that will be used in a symbol.  Mask out any characters
   that are not valid in that context.  */

void
clean_symbol_name (char *p)
{
  for (; *p; p++)
    if (! (ISALNUM (*p)
#ifndef NO_DOLLAR_IN_LABEL	/* this for `$'; unlikely, but... -- kr */
	    || *p == '$'
#endif
#ifndef NO_DOT_IN_LABEL		/* this for `.'; unlikely, but...  */
	    || *p == '.'
#endif
	   ))
      *p = '_';
}

/* For anonymous aggregate types, we need some sort of name to
   hold on to.  In practice, this should not appear, but it should
   not be harmful if it does.  */
bool 
anon_aggrname_p(const_tree id_node)
{
#ifndef NO_DOT_IN_LABEL
 return (IDENTIFIER_POINTER (id_node)[0] == '.'
	 && IDENTIFIER_POINTER (id_node)[1] == '_');
#else /* NO_DOT_IN_LABEL */
#ifndef NO_DOLLAR_IN_LABEL
  return (IDENTIFIER_POINTER (id_node)[0] == '$' \
	  && IDENTIFIER_POINTER (id_node)[1] == '_');
#else /* NO_DOLLAR_IN_LABEL */
#define ANON_AGGRNAME_PREFIX "__anon_"
  return (!strncmp (IDENTIFIER_POINTER (id_node), ANON_AGGRNAME_PREFIX, 
		    sizeof (ANON_AGGRNAME_PREFIX) - 1));
#endif	/* NO_DOLLAR_IN_LABEL */
#endif	/* NO_DOT_IN_LABEL */
}

/* Return a format for an anonymous aggregate name.  */
const char *
anon_aggrname_format()
{
#ifndef NO_DOT_IN_LABEL
 return "._%d";
#else /* NO_DOT_IN_LABEL */
#ifndef NO_DOLLAR_IN_LABEL
  return "$_%d";
#else /* NO_DOLLAR_IN_LABEL */
  return "__anon_%d";
#endif	/* NO_DOLLAR_IN_LABEL */
#endif	/* NO_DOT_IN_LABEL */
}

/* Generate a name for a special-purpose function.
   The generated name may need to be unique across the whole link.
   Changes to this function may also require corresponding changes to
   xstrdup_mask_random.
   TYPE is some string to identify the purpose of this function to the
   linker or collect2; it must start with an uppercase letter,
   one of:
   I - for constructors
   D - for destructors
   N - for C++ anonymous namespaces
   F - for DWARF unwind frame information.  */

tree
get_file_function_name (const char *type)
{
  char *buf;
  const char *p;
  char *q;

  /* If we already have a name we know to be unique, just use that.  */
  if (first_global_object_name)
    p = q = ASTRDUP (first_global_object_name);
  /* If the target is handling the constructors/destructors, they
     will be local to this file and the name is only necessary for
     debugging purposes. 
     We also assign sub_I and sub_D sufixes to constructors called from
     the global static constructors.  These are always local.  */
  else if (((type[0] == 'I' || type[0] == 'D') && targetm.have_ctors_dtors)
	   || (strncmp (type, "sub_", 4) == 0
	       && (type[4] == 'I' || type[4] == 'D')))
    {
      const char *file = main_input_filename;
      if (! file)
	file = LOCATION_FILE (input_location);
      /* Just use the file's basename, because the full pathname
	 might be quite long.  */
      p = q = ASTRDUP (lbasename (file));
    }
  else
    {
      /* Otherwise, the name must be unique across the entire link.
	 We don't have anything that we know to be unique to this translation
	 unit, so use what we do have and throw in some randomness.  */
      unsigned len;
      const char *name = weak_global_object_name;
      const char *file = main_input_filename;

      if (! name)
	name = "";
      if (! file)
	file = LOCATION_FILE (input_location);

      len = strlen (file);
      q = (char *) alloca (9 + 17 + len + 1);
      memcpy (q, file, len + 1);

      snprintf (q + len, 9 + 17 + 1, "_%08X_" HOST_WIDE_INT_PRINT_HEX, 
		crc32_string (0, name), get_random_seed (false));

      p = q;
    }

  clean_symbol_name (q);
  buf = (char *) alloca (sizeof (FILE_FUNCTION_FORMAT) + strlen (p)
			 + strlen (type));

  /* Set up the name of the file-level functions we may need.
     Use a global object (which is already required to be unique over
     the program) rather than the file name (which imposes extra
     constraints).  */
  sprintf (buf, FILE_FUNCTION_FORMAT, type, p);

  return get_identifier (buf);
}

#if defined ENABLE_TREE_CHECKING && (GCC_VERSION >= 2007)

/* Complain that the tree code of NODE does not match the expected 0
   terminated list of trailing codes. The trailing code list can be
   empty, for a more vague error message.  FILE, LINE, and FUNCTION
   are of the caller.  */

void
tree_check_failed (const_tree node, const char *file,
		   int line, const char *function, ...)
{
  va_list args;
  const char *buffer;
  unsigned length = 0;
  enum tree_code code;

  va_start (args, function);
  while ((code = (enum tree_code) va_arg (args, int)))
    length += 4 + strlen (get_tree_code_name (code));
  va_end (args);
  if (length)
    {
      char *tmp;
      va_start (args, function);
      length += strlen ("expected ");
      buffer = tmp = (char *) alloca (length);
      length = 0;
      while ((code = (enum tree_code) va_arg (args, int)))
	{
	  const char *prefix = length ? " or " : "expected ";

	  strcpy (tmp + length, prefix);
	  length += strlen (prefix);
	  strcpy (tmp + length, get_tree_code_name (code));
	  length += strlen (get_tree_code_name (code));
	}
      va_end (args);
    }
  else
    buffer = "unexpected node";

  internal_error ("tree check: %s, have %s in %s, at %s:%d",
		  buffer, get_tree_code_name (TREE_CODE (node)),
		  function, trim_filename (file), line);
}

/* Complain that the tree code of NODE does match the expected 0
   terminated list of trailing codes. FILE, LINE, and FUNCTION are of
   the caller.  */

void
tree_not_check_failed (const_tree node, const char *file,
		       int line, const char *function, ...)
{
  va_list args;
  char *buffer;
  unsigned length = 0;
  enum tree_code code;

  va_start (args, function);
  while ((code = (enum tree_code) va_arg (args, int)))
    length += 4 + strlen (get_tree_code_name (code));
  va_end (args);
  va_start (args, function);
  buffer = (char *) alloca (length);
  length = 0;
  while ((code = (enum tree_code) va_arg (args, int)))
    {
      if (length)
	{
	  strcpy (buffer + length, " or ");
	  length += 4;
	}
      strcpy (buffer + length, get_tree_code_name (code));
      length += strlen (get_tree_code_name (code));
    }
  va_end (args);

  internal_error ("tree check: expected none of %s, have %s in %s, at %s:%d",
		  buffer, get_tree_code_name (TREE_CODE (node)),
		  function, trim_filename (file), line);
}

/* Similar to tree_check_failed, except that we check for a class of tree
   code, given in CL.  */

void
tree_class_check_failed (const_tree node, const enum tree_code_class cl,
			 const char *file, int line, const char *function)
{
  internal_error
    ("tree check: expected class %qs, have %qs (%s) in %s, at %s:%d",
     TREE_CODE_CLASS_STRING (cl),
     TREE_CODE_CLASS_STRING (TREE_CODE_CLASS (TREE_CODE (node))),
     get_tree_code_name (TREE_CODE (node)), function, trim_filename (file), line);
}

/* Similar to tree_check_failed, except that instead of specifying a
   dozen codes, use the knowledge that they're all sequential.  */

void
tree_range_check_failed (const_tree node, const char *file, int line,
			 const char *function, enum tree_code c1,
			 enum tree_code c2)
{
  char *buffer;
  unsigned length = 0;
  unsigned int c;

  for (c = c1; c <= c2; ++c)
    length += 4 + strlen (get_tree_code_name ((enum tree_code) c));

  length += strlen ("expected ");
  buffer = (char *) alloca (length);
  length = 0;

  for (c = c1; c <= c2; ++c)
    {
      const char *prefix = length ? " or " : "expected ";

      strcpy (buffer + length, prefix);
      length += strlen (prefix);
      strcpy (buffer + length, get_tree_code_name ((enum tree_code) c));
      length += strlen (get_tree_code_name ((enum tree_code) c));
    }

  internal_error ("tree check: %s, have %s in %s, at %s:%d",
		  buffer, get_tree_code_name (TREE_CODE (node)),
		  function, trim_filename (file), line);
}


/* Similar to tree_check_failed, except that we check that a tree does
   not have the specified code, given in CL.  */

void
tree_not_class_check_failed (const_tree node, const enum tree_code_class cl,
			     const char *file, int line, const char *function)
{
  internal_error
    ("tree check: did not expect class %qs, have %qs (%s) in %s, at %s:%d",
     TREE_CODE_CLASS_STRING (cl),
     TREE_CODE_CLASS_STRING (TREE_CODE_CLASS (TREE_CODE (node))),
     get_tree_code_name (TREE_CODE (node)), function, trim_filename (file), line);
}


/* Similar to tree_check_failed but applied to OMP_CLAUSE codes.  */

void
omp_clause_check_failed (const_tree node, const char *file, int line,
                         const char *function, enum omp_clause_code code)
{
  internal_error ("tree check: expected omp_clause %s, have %s in %s, at %s:%d",
		  omp_clause_code_name[code], get_tree_code_name (TREE_CODE (node)),
		  function, trim_filename (file), line);
}


/* Similar to tree_range_check_failed but applied to OMP_CLAUSE codes.  */

void
omp_clause_range_check_failed (const_tree node, const char *file, int line,
			       const char *function, enum omp_clause_code c1,
			       enum omp_clause_code c2)
{
  char *buffer;
  unsigned length = 0;
  unsigned int c;

  for (c = c1; c <= c2; ++c)
    length += 4 + strlen (omp_clause_code_name[c]);

  length += strlen ("expected ");
  buffer = (char *) alloca (length);
  length = 0;

  for (c = c1; c <= c2; ++c)
    {
      const char *prefix = length ? " or " : "expected ";

      strcpy (buffer + length, prefix);
      length += strlen (prefix);
      strcpy (buffer + length, omp_clause_code_name[c]);
      length += strlen (omp_clause_code_name[c]);
    }

  internal_error ("tree check: %s, have %s in %s, at %s:%d",
		  buffer, omp_clause_code_name[TREE_CODE (node)],
		  function, trim_filename (file), line);
}


#undef DEFTREESTRUCT
#define DEFTREESTRUCT(VAL, NAME) NAME,

static const char *ts_enum_names[] = {
#include "treestruct.def"
};
#undef DEFTREESTRUCT

#define TS_ENUM_NAME(EN) (ts_enum_names[(EN)])

/* Similar to tree_class_check_failed, except that we check for
   whether CODE contains the tree structure identified by EN.  */

void
tree_contains_struct_check_failed (const_tree node,
				   const enum tree_node_structure_enum en,
				   const char *file, int line,
				   const char *function)
{
  internal_error
    ("tree check: expected tree that contains %qs structure, have %qs in %s, at %s:%d",
     TS_ENUM_NAME (en),
     get_tree_code_name (TREE_CODE (node)), function, trim_filename (file), line);
}


/* Similar to above, except that the check is for the bounds of a TREE_VEC's
   (dynamically sized) vector.  */

void
tree_int_cst_elt_check_failed (int idx, int len, const char *file, int line,
			       const char *function)
{
  internal_error
    ("tree check: accessed elt %d of tree_int_cst with %d elts in %s, at %s:%d",
     idx + 1, len, function, trim_filename (file), line);
}

/* Similar to above, except that the check is for the bounds of a TREE_VEC's
   (dynamically sized) vector.  */

void
tree_vec_elt_check_failed (int idx, int len, const char *file, int line,
			   const char *function)
{
  internal_error
    ("tree check: accessed elt %d of tree_vec with %d elts in %s, at %s:%d",
     idx + 1, len, function, trim_filename (file), line);
}

/* Similar to above, except that the check is for the bounds of the operand
   vector of an expression node EXP.  */

void
tree_operand_check_failed (int idx, const_tree exp, const char *file,
			   int line, const char *function)
{
  enum tree_code code = TREE_CODE (exp);
  internal_error
    ("tree check: accessed operand %d of %s with %d operands in %s, at %s:%d",
     idx + 1, get_tree_code_name (code), TREE_OPERAND_LENGTH (exp),
     function, trim_filename (file), line);
}

/* Similar to above, except that the check is for the number of
   operands of an OMP_CLAUSE node.  */

void
omp_clause_operand_check_failed (int idx, const_tree t, const char *file,
			         int line, const char *function)
{
  internal_error
    ("tree check: accessed operand %d of omp_clause %s with %d operands "
     "in %s, at %s:%d", idx + 1, omp_clause_code_name[OMP_CLAUSE_CODE (t)],
     omp_clause_num_ops [OMP_CLAUSE_CODE (t)], function,
     trim_filename (file), line);
}
#endif /* ENABLE_TREE_CHECKING */

/* Create a new vector type node holding SUBPARTS units of type INNERTYPE,
   and mapped to the machine mode MODE.  Initialize its fields and build
   the information necessary for debugging output.  */

static tree
make_vector_type (tree innertype, int nunits, machine_mode mode)
{
  tree t;
  inchash::hash hstate;
  tree mv_innertype = TYPE_MAIN_VARIANT (innertype);

  t = make_node (VECTOR_TYPE);
  TREE_TYPE (t) = mv_innertype;
  SET_TYPE_VECTOR_SUBPARTS (t, nunits);
  SET_TYPE_MODE (t, mode);

  if (TYPE_STRUCTURAL_EQUALITY_P (mv_innertype) || in_lto_p)
    SET_TYPE_STRUCTURAL_EQUALITY (t);
  else if ((TYPE_CANONICAL (mv_innertype) != innertype
	    || mode != VOIDmode)
	   && !VECTOR_BOOLEAN_TYPE_P (t))
    TYPE_CANONICAL (t)
      = make_vector_type (TYPE_CANONICAL (mv_innertype), nunits, VOIDmode);

  layout_type (t);

  hstate.add_wide_int (VECTOR_TYPE);
  hstate.add_wide_int (nunits);
  hstate.add_wide_int (mode);
  hstate.add_object (TYPE_HASH (TREE_TYPE (t)));
  t = type_hash_canon (hstate.end (), t);

  /* We have built a main variant, based on the main variant of the
     inner type. Use it to build the variant we return.  */
  if ((TYPE_ATTRIBUTES (innertype) || TYPE_QUALS (innertype))
      && TREE_TYPE (t) != innertype)
    return build_type_attribute_qual_variant (t,
					      TYPE_ATTRIBUTES (innertype),
					      TYPE_QUALS (innertype));

  return t;
}

static tree
make_or_reuse_type (unsigned size, int unsignedp)
{
  int i;

  if (size == INT_TYPE_SIZE)
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (size == CHAR_TYPE_SIZE)
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (size == SHORT_TYPE_SIZE)
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (size == LONG_TYPE_SIZE)
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (size == LONG_LONG_TYPE_SIZE)
    return (unsignedp ? long_long_unsigned_type_node
            : long_long_integer_type_node);

  for (i = 0; i < NUM_INT_N_ENTS; i ++)
    if (size == int_n_data[i].bitsize
	&& int_n_enabled_p[i])
      return (unsignedp ? int_n_trees[i].unsigned_type
	      : int_n_trees[i].signed_type);

  if (unsignedp)
    return make_unsigned_type (size);
  else
    return make_signed_type (size);
}

/* Create or reuse a fract type by SIZE, UNSIGNEDP, and SATP.  */

static tree
make_or_reuse_fract_type (unsigned size, int unsignedp, int satp)
{
  if (satp)
    {
      if (size == SHORT_FRACT_TYPE_SIZE)
	return unsignedp ? sat_unsigned_short_fract_type_node
			 : sat_short_fract_type_node;
      if (size == FRACT_TYPE_SIZE)
	return unsignedp ? sat_unsigned_fract_type_node : sat_fract_type_node;
      if (size == LONG_FRACT_TYPE_SIZE)
	return unsignedp ? sat_unsigned_long_fract_type_node
			 : sat_long_fract_type_node;
      if (size == LONG_LONG_FRACT_TYPE_SIZE)
	return unsignedp ? sat_unsigned_long_long_fract_type_node
			 : sat_long_long_fract_type_node;
    }
  else
    {
      if (size == SHORT_FRACT_TYPE_SIZE)
	return unsignedp ? unsigned_short_fract_type_node
			 : short_fract_type_node;
      if (size == FRACT_TYPE_SIZE)
	return unsignedp ? unsigned_fract_type_node : fract_type_node;
      if (size == LONG_FRACT_TYPE_SIZE)
	return unsignedp ? unsigned_long_fract_type_node
			 : long_fract_type_node;
      if (size == LONG_LONG_FRACT_TYPE_SIZE)
	return unsignedp ? unsigned_long_long_fract_type_node
			 : long_long_fract_type_node;
    }

  return make_fract_type (size, unsignedp, satp);
}

/* Create or reuse an accum type by SIZE, UNSIGNEDP, and SATP.  */

static tree
make_or_reuse_accum_type (unsigned size, int unsignedp, int satp)
{
  if (satp)
    {
      if (size == SHORT_ACCUM_TYPE_SIZE)
	return unsignedp ? sat_unsigned_short_accum_type_node
			 : sat_short_accum_type_node;
      if (size == ACCUM_TYPE_SIZE)
	return unsignedp ? sat_unsigned_accum_type_node : sat_accum_type_node;
      if (size == LONG_ACCUM_TYPE_SIZE)
	return unsignedp ? sat_unsigned_long_accum_type_node
			 : sat_long_accum_type_node;
      if (size == LONG_LONG_ACCUM_TYPE_SIZE)
	return unsignedp ? sat_unsigned_long_long_accum_type_node
			 : sat_long_long_accum_type_node;
    }
  else
    {
      if (size == SHORT_ACCUM_TYPE_SIZE)
	return unsignedp ? unsigned_short_accum_type_node
			 : short_accum_type_node;
      if (size == ACCUM_TYPE_SIZE)
	return unsignedp ? unsigned_accum_type_node : accum_type_node;
      if (size == LONG_ACCUM_TYPE_SIZE)
	return unsignedp ? unsigned_long_accum_type_node
			 : long_accum_type_node;
      if (size == LONG_LONG_ACCUM_TYPE_SIZE)
	return unsignedp ? unsigned_long_long_accum_type_node
			 : long_long_accum_type_node;
    }

  return make_accum_type (size, unsignedp, satp);
}


/* Create an atomic variant node for TYPE.  This routine is called
   during initialization of data types to create the 5 basic atomic
   types. The generic build_variant_type function requires these to
   already be set up in order to function properly, so cannot be
   called from there.  If ALIGN is non-zero, then ensure alignment is
   overridden to this value.  */

static tree
build_atomic_base (tree type, unsigned int align)
{
  tree t;

  /* Make sure its not already registered.  */
  if ((t = get_qualified_type (type, TYPE_QUAL_ATOMIC)))
    return t;
  
  t = build_variant_type_copy (type);
  set_type_quals (t, TYPE_QUAL_ATOMIC);

  if (align)
    SET_TYPE_ALIGN (t, align);

  return t;
}

/* Create nodes for all integer types (and error_mark_node) using the sizes
   of C datatypes.  SIGNED_CHAR specifies whether char is signed.  */

void
build_common_tree_nodes (bool signed_char)
{
  int i;

  error_mark_node = make_node (ERROR_MARK);
  TREE_TYPE (error_mark_node) = error_mark_node;

  initialize_sizetypes ();

  /* Define both `signed char' and `unsigned char'.  */
  signed_char_type_node = make_signed_type (CHAR_TYPE_SIZE);
  TYPE_STRING_FLAG (signed_char_type_node) = 1;
  unsigned_char_type_node = make_unsigned_type (CHAR_TYPE_SIZE);
  TYPE_STRING_FLAG (unsigned_char_type_node) = 1;

  /* Define `char', which is like either `signed char' or `unsigned char'
     but not the same as either.  */
  char_type_node
    = (signed_char
       ? make_signed_type (CHAR_TYPE_SIZE)
       : make_unsigned_type (CHAR_TYPE_SIZE));
  TYPE_STRING_FLAG (char_type_node) = 1;

  short_integer_type_node = make_signed_type (SHORT_TYPE_SIZE);
  short_unsigned_type_node = make_unsigned_type (SHORT_TYPE_SIZE);
  integer_type_node = make_signed_type (INT_TYPE_SIZE);
  unsigned_type_node = make_unsigned_type (INT_TYPE_SIZE);
  long_integer_type_node = make_signed_type (LONG_TYPE_SIZE);
  long_unsigned_type_node = make_unsigned_type (LONG_TYPE_SIZE);
  long_long_integer_type_node = make_signed_type (LONG_LONG_TYPE_SIZE);
  long_long_unsigned_type_node = make_unsigned_type (LONG_LONG_TYPE_SIZE);

  for (i = 0; i < NUM_INT_N_ENTS; i ++)
    {
      int_n_trees[i].signed_type = make_signed_type (int_n_data[i].bitsize);
      int_n_trees[i].unsigned_type = make_unsigned_type (int_n_data[i].bitsize);
      TYPE_SIZE (int_n_trees[i].signed_type) = bitsize_int (int_n_data[i].bitsize);
      TYPE_SIZE (int_n_trees[i].unsigned_type) = bitsize_int (int_n_data[i].bitsize);

      if (int_n_data[i].bitsize > LONG_LONG_TYPE_SIZE
	  && int_n_enabled_p[i])
	{
	  integer_types[itk_intN_0 + i * 2] = int_n_trees[i].signed_type;
	  integer_types[itk_unsigned_intN_0 + i * 2] = int_n_trees[i].unsigned_type;
	}
    }

  /* Define a boolean type.  This type only represents boolean values but
     may be larger than char depending on the value of BOOL_TYPE_SIZE.  */
  boolean_type_node = make_unsigned_type (BOOL_TYPE_SIZE);
  TREE_SET_CODE (boolean_type_node, BOOLEAN_TYPE);
  TYPE_PRECISION (boolean_type_node) = 1;
  TYPE_MAX_VALUE (boolean_type_node) = build_int_cst (boolean_type_node, 1);

  /* Define what type to use for size_t.  */
  if (strcmp (SIZE_TYPE, "unsigned int") == 0)
    size_type_node = unsigned_type_node;
  else if (strcmp (SIZE_TYPE, "long unsigned int") == 0)
    size_type_node = long_unsigned_type_node;
  else if (strcmp (SIZE_TYPE, "long long unsigned int") == 0)
    size_type_node = long_long_unsigned_type_node;
  else if (strcmp (SIZE_TYPE, "short unsigned int") == 0)
    size_type_node = short_unsigned_type_node;
  else
    {
      int i;

      size_type_node = NULL_TREE;
      for (i = 0; i < NUM_INT_N_ENTS; i++)
	if (int_n_enabled_p[i])
	  {
	    char name[50];
	    sprintf (name, "__int%d unsigned", int_n_data[i].bitsize);

	    if (strcmp (name, SIZE_TYPE) == 0)
	      {
		size_type_node = int_n_trees[i].unsigned_type;
	      }
	  }
      if (size_type_node == NULL_TREE)
	gcc_unreachable ();
    }

  /* Fill in the rest of the sized types.  Reuse existing type nodes
     when possible.  */
  intQI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (QImode), 0);
  intHI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (HImode), 0);
  intSI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (SImode), 0);
  intDI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (DImode), 0);
  intTI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (TImode), 0);

  unsigned_intQI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (QImode), 1);
  unsigned_intHI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (HImode), 1);
  unsigned_intSI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (SImode), 1);
  unsigned_intDI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (DImode), 1);
  unsigned_intTI_type_node = make_or_reuse_type (GET_MODE_BITSIZE (TImode), 1);

  /* Don't call build_qualified type for atomics.  That routine does
     special processing for atomics, and until they are initialized
     it's better not to make that call.
     
     Check to see if there is a target override for atomic types.  */

  atomicQI_type_node = build_atomic_base (unsigned_intQI_type_node,
					targetm.atomic_align_for_mode (QImode));
  atomicHI_type_node = build_atomic_base (unsigned_intHI_type_node,
					targetm.atomic_align_for_mode (HImode));
  atomicSI_type_node = build_atomic_base (unsigned_intSI_type_node,
					targetm.atomic_align_for_mode (SImode));
  atomicDI_type_node = build_atomic_base (unsigned_intDI_type_node,
					targetm.atomic_align_for_mode (DImode));
  atomicTI_type_node = build_atomic_base (unsigned_intTI_type_node,
					targetm.atomic_align_for_mode (TImode));
  	
  access_public_node = get_identifier ("public");
  access_protected_node = get_identifier ("protected");
  access_private_node = get_identifier ("private");

  /* Define these next since types below may used them.  */
  integer_zero_node = build_int_cst (integer_type_node, 0);
  integer_one_node = build_int_cst (integer_type_node, 1);
  integer_three_node = build_int_cst (integer_type_node, 3);
  integer_minus_one_node = build_int_cst (integer_type_node, -1);

  size_zero_node = size_int (0);
  size_one_node = size_int (1);
  bitsize_zero_node = bitsize_int (0);
  bitsize_one_node = bitsize_int (1);
  bitsize_unit_node = bitsize_int (BITS_PER_UNIT);

  boolean_false_node = TYPE_MIN_VALUE (boolean_type_node);
  boolean_true_node = TYPE_MAX_VALUE (boolean_type_node);

  void_type_node = make_node (VOID_TYPE);
  layout_type (void_type_node);

  pointer_bounds_type_node = targetm.chkp_bound_type ();

  /* We are not going to have real types in C with less than byte alignment,
     so we might as well not have any types that claim to have it.  */
  SET_TYPE_ALIGN (void_type_node, BITS_PER_UNIT);
  TYPE_USER_ALIGN (void_type_node) = 0;

  void_node = make_node (VOID_CST);
  TREE_TYPE (void_node) = void_type_node;

  null_pointer_node = build_int_cst (build_pointer_type (void_type_node), 0);
  layout_type (TREE_TYPE (null_pointer_node));

  ptr_type_node = build_pointer_type (void_type_node);
  const_ptr_type_node
    = build_pointer_type (build_type_variant (void_type_node, 1, 0));
  fileptr_type_node = ptr_type_node;

  pointer_sized_int_node = build_nonstandard_integer_type (POINTER_SIZE, 1);

  float_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (float_type_node) = FLOAT_TYPE_SIZE;
  layout_type (float_type_node);

  double_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (double_type_node) = DOUBLE_TYPE_SIZE;
  layout_type (double_type_node);

  long_double_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (long_double_type_node) = LONG_DOUBLE_TYPE_SIZE;
  layout_type (long_double_type_node);

  float_ptr_type_node = build_pointer_type (float_type_node);
  double_ptr_type_node = build_pointer_type (double_type_node);
  long_double_ptr_type_node = build_pointer_type (long_double_type_node);
  integer_ptr_type_node = build_pointer_type (integer_type_node);

  /* Fixed size integer types.  */
  uint16_type_node = make_or_reuse_type (16, 1);
  uint32_type_node = make_or_reuse_type (32, 1);
  uint64_type_node = make_or_reuse_type (64, 1);

  /* Decimal float types. */
  dfloat32_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (dfloat32_type_node) = DECIMAL32_TYPE_SIZE;
  layout_type (dfloat32_type_node);
  SET_TYPE_MODE (dfloat32_type_node, SDmode);
  dfloat32_ptr_type_node = build_pointer_type (dfloat32_type_node);

  dfloat64_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (dfloat64_type_node) = DECIMAL64_TYPE_SIZE;
  layout_type (dfloat64_type_node);
  SET_TYPE_MODE (dfloat64_type_node, DDmode);
  dfloat64_ptr_type_node = build_pointer_type (dfloat64_type_node);

  dfloat128_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (dfloat128_type_node) = DECIMAL128_TYPE_SIZE;
  layout_type (dfloat128_type_node);
  SET_TYPE_MODE (dfloat128_type_node, TDmode);
  dfloat128_ptr_type_node = build_pointer_type (dfloat128_type_node);

  complex_integer_type_node = build_complex_type (integer_type_node);
  complex_float_type_node = build_complex_type (float_type_node);
  complex_double_type_node = build_complex_type (double_type_node);
  complex_long_double_type_node = build_complex_type (long_double_type_node);

/* Make fixed-point nodes based on sat/non-sat and signed/unsigned.  */
#define MAKE_FIXED_TYPE_NODE(KIND,SIZE) \
  sat_ ## KIND ## _type_node = \
    make_sat_signed_ ## KIND ## _type (SIZE); \
  sat_unsigned_ ## KIND ## _type_node = \
    make_sat_unsigned_ ## KIND ## _type (SIZE); \
  KIND ## _type_node = make_signed_ ## KIND ## _type (SIZE); \
  unsigned_ ## KIND ## _type_node = \
    make_unsigned_ ## KIND ## _type (SIZE);

#define MAKE_FIXED_TYPE_NODE_WIDTH(KIND,WIDTH,SIZE) \
  sat_ ## WIDTH ## KIND ## _type_node = \
    make_sat_signed_ ## KIND ## _type (SIZE); \
  sat_unsigned_ ## WIDTH ## KIND ## _type_node = \
    make_sat_unsigned_ ## KIND ## _type (SIZE); \
  WIDTH ## KIND ## _type_node = make_signed_ ## KIND ## _type (SIZE); \
  unsigned_ ## WIDTH ## KIND ## _type_node = \
    make_unsigned_ ## KIND ## _type (SIZE);

/* Make fixed-point type nodes based on four different widths.  */
#define MAKE_FIXED_TYPE_NODE_FAMILY(N1,N2) \
  MAKE_FIXED_TYPE_NODE_WIDTH (N1, short_, SHORT_ ## N2 ## _TYPE_SIZE) \
  MAKE_FIXED_TYPE_NODE (N1, N2 ## _TYPE_SIZE) \
  MAKE_FIXED_TYPE_NODE_WIDTH (N1, long_, LONG_ ## N2 ## _TYPE_SIZE) \
  MAKE_FIXED_TYPE_NODE_WIDTH (N1, long_long_, LONG_LONG_ ## N2 ## _TYPE_SIZE)

/* Make fixed-point mode nodes based on sat/non-sat and signed/unsigned.  */
#define MAKE_FIXED_MODE_NODE(KIND,NAME,MODE) \
  NAME ## _type_node = \
    make_or_reuse_signed_ ## KIND ## _type (GET_MODE_BITSIZE (MODE ## mode)); \
  u ## NAME ## _type_node = \
    make_or_reuse_unsigned_ ## KIND ## _type \
      (GET_MODE_BITSIZE (U ## MODE ## mode)); \
  sat_ ## NAME ## _type_node = \
    make_or_reuse_sat_signed_ ## KIND ## _type \
      (GET_MODE_BITSIZE (MODE ## mode)); \
  sat_u ## NAME ## _type_node = \
    make_or_reuse_sat_unsigned_ ## KIND ## _type \
      (GET_MODE_BITSIZE (U ## MODE ## mode));

  /* Fixed-point type and mode nodes.  */
  MAKE_FIXED_TYPE_NODE_FAMILY (fract, FRACT)
  MAKE_FIXED_TYPE_NODE_FAMILY (accum, ACCUM)
  MAKE_FIXED_MODE_NODE (fract, qq, QQ)
  MAKE_FIXED_MODE_NODE (fract, hq, HQ)
  MAKE_FIXED_MODE_NODE (fract, sq, SQ)
  MAKE_FIXED_MODE_NODE (fract, dq, DQ)
  MAKE_FIXED_MODE_NODE (fract, tq, TQ)
  MAKE_FIXED_MODE_NODE (accum, ha, HA)
  MAKE_FIXED_MODE_NODE (accum, sa, SA)
  MAKE_FIXED_MODE_NODE (accum, da, DA)
  MAKE_FIXED_MODE_NODE (accum, ta, TA)

  {
    tree t = targetm.build_builtin_va_list ();

    /* Many back-ends define record types without setting TYPE_NAME.
       If we copied the record type here, we'd keep the original
       record type without a name.  This breaks name mangling.  So,
       don't copy record types and let c_common_nodes_and_builtins()
       declare the type to be __builtin_va_list.  */
    if (TREE_CODE (t) != RECORD_TYPE)
      t = build_variant_type_copy (t);

    va_list_type_node = t;
  }
}

/* Modify DECL for given flags.
   TM_PURE attribute is set only on types, so the function will modify
   DECL's type when ECF_TM_PURE is used.  */

void
set_call_expr_flags (tree decl, int flags)
{
  if (flags & ECF_NOTHROW)
    TREE_NOTHROW (decl) = 1;
  if (flags & ECF_CONST)
    TREE_READONLY (decl) = 1;
  if (flags & ECF_PURE)
    DECL_PURE_P (decl) = 1;
  if (flags & ECF_LOOPING_CONST_OR_PURE)
    DECL_LOOPING_CONST_OR_PURE_P (decl) = 1;
  if (flags & ECF_NOVOPS)
    DECL_IS_NOVOPS (decl) = 1;
  if (flags & ECF_NORETURN)
    TREE_THIS_VOLATILE (decl) = 1;
  if (flags & ECF_MALLOC)
    DECL_IS_MALLOC (decl) = 1;
  if (flags & ECF_RETURNS_TWICE)
    DECL_IS_RETURNS_TWICE (decl) = 1;
  if (flags & ECF_LEAF)
    DECL_ATTRIBUTES (decl) = tree_cons (get_identifier ("leaf"),
					NULL, DECL_ATTRIBUTES (decl));
  if (flags & ECF_RET1)
    DECL_ATTRIBUTES (decl)
      = tree_cons (get_identifier ("fn spec"),
		   build_tree_list (NULL_TREE, build_string (1, "1")),
		   DECL_ATTRIBUTES (decl));
  if ((flags & ECF_TM_PURE) && flag_tm)
    apply_tm_attr (decl, get_identifier ("transaction_pure"));
  /* Looping const or pure is implied by noreturn.
     There is currently no way to declare looping const or looping pure alone.  */
  gcc_assert (!(flags & ECF_LOOPING_CONST_OR_PURE)
	      || ((flags & ECF_NORETURN) && (flags & (ECF_CONST | ECF_PURE))));
}


/* A subroutine of build_common_builtin_nodes.  Define a builtin function.  */

static void
local_define_builtin (const char *name, tree type, enum built_in_function code,
                      const char *library_name, int ecf_flags)
{
  tree decl;

  decl = add_builtin_function (name, type, code, BUILT_IN_NORMAL,
			       library_name, NULL_TREE);
  set_call_expr_flags (decl, ecf_flags);

  set_builtin_decl (code, decl, true);
}

/* Call this function after instantiating all builtins that the language
   front end cares about.  This will build the rest of the builtins
   and internal functions that are relied upon by the tree optimizers and
   the middle-end.  */

void
build_common_builtin_nodes (void)
{
  tree tmp, ftype;
  int ecf_flags;

  if (!builtin_decl_explicit_p (BUILT_IN_UNREACHABLE)
      || !builtin_decl_explicit_p (BUILT_IN_ABORT))
    {
      ftype = build_function_type (void_type_node, void_list_node);
      if (!builtin_decl_explicit_p (BUILT_IN_UNREACHABLE))
	local_define_builtin ("__builtin_unreachable", ftype,
			      BUILT_IN_UNREACHABLE,
			      "__builtin_unreachable",
			      ECF_NOTHROW | ECF_LEAF | ECF_NORETURN
			      | ECF_CONST);
      if (!builtin_decl_explicit_p (BUILT_IN_ABORT))
	local_define_builtin ("__builtin_abort", ftype, BUILT_IN_ABORT,
			      "abort",
			      ECF_LEAF | ECF_NORETURN | ECF_CONST);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_MEMCPY)
      || !builtin_decl_explicit_p (BUILT_IN_MEMMOVE))
    {
      ftype = build_function_type_list (ptr_type_node,
					ptr_type_node, const_ptr_type_node,
					size_type_node, NULL_TREE);

      if (!builtin_decl_explicit_p (BUILT_IN_MEMCPY))
	local_define_builtin ("__builtin_memcpy", ftype, BUILT_IN_MEMCPY,
			      "memcpy", ECF_NOTHROW | ECF_LEAF | ECF_RET1);
      if (!builtin_decl_explicit_p (BUILT_IN_MEMMOVE))
	local_define_builtin ("__builtin_memmove", ftype, BUILT_IN_MEMMOVE,
			      "memmove", ECF_NOTHROW | ECF_LEAF | ECF_RET1);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_MEMCMP))
    {
      ftype = build_function_type_list (integer_type_node, const_ptr_type_node,
					const_ptr_type_node, size_type_node,
					NULL_TREE);
      local_define_builtin ("__builtin_memcmp", ftype, BUILT_IN_MEMCMP,
			    "memcmp", ECF_PURE | ECF_NOTHROW | ECF_LEAF);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_MEMSET))
    {
      ftype = build_function_type_list (ptr_type_node,
					ptr_type_node, integer_type_node,
					size_type_node, NULL_TREE);
      local_define_builtin ("__builtin_memset", ftype, BUILT_IN_MEMSET,
			    "memset", ECF_NOTHROW | ECF_LEAF | ECF_RET1);
    }

  /* If we're checking the stack, `alloca' can throw.  */
  const int alloca_flags
    = ECF_MALLOC | ECF_LEAF | (flag_stack_check ? 0 : ECF_NOTHROW);

  if (!builtin_decl_explicit_p (BUILT_IN_ALLOCA))
    {
      ftype = build_function_type_list (ptr_type_node,
					size_type_node, NULL_TREE);
      local_define_builtin ("__builtin_alloca", ftype, BUILT_IN_ALLOCA,
			    "alloca", alloca_flags);
    }

  ftype = build_function_type_list (ptr_type_node, size_type_node,
				    size_type_node, NULL_TREE);
  local_define_builtin ("__builtin_alloca_with_align", ftype,
			BUILT_IN_ALLOCA_WITH_ALIGN,
			"__builtin_alloca_with_align",
			alloca_flags);

  ftype = build_function_type_list (void_type_node,
				    ptr_type_node, ptr_type_node,
				    ptr_type_node, NULL_TREE);
  local_define_builtin ("__builtin_init_trampoline", ftype,
			BUILT_IN_INIT_TRAMPOLINE,
			"__builtin_init_trampoline", ECF_NOTHROW | ECF_LEAF);
  local_define_builtin ("__builtin_init_heap_trampoline", ftype,
			BUILT_IN_INIT_HEAP_TRAMPOLINE,
			"__builtin_init_heap_trampoline",
			ECF_NOTHROW | ECF_LEAF);

  ftype = build_function_type_list (ptr_type_node, ptr_type_node, NULL_TREE);
  local_define_builtin ("__builtin_adjust_trampoline", ftype,
			BUILT_IN_ADJUST_TRAMPOLINE,
			"__builtin_adjust_trampoline",
			ECF_CONST | ECF_NOTHROW);

  ftype = build_function_type_list (void_type_node,
				    ptr_type_node, ptr_type_node, NULL_TREE);
  local_define_builtin ("__builtin_nonlocal_goto", ftype,
			BUILT_IN_NONLOCAL_GOTO,
			"__builtin_nonlocal_goto",
			ECF_NORETURN | ECF_NOTHROW);

  ftype = build_function_type_list (void_type_node,
				    ptr_type_node, ptr_type_node, NULL_TREE);
  local_define_builtin ("__builtin_setjmp_setup", ftype,
			BUILT_IN_SETJMP_SETUP,
			"__builtin_setjmp_setup", ECF_NOTHROW);

  ftype = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);
  local_define_builtin ("__builtin_setjmp_receiver", ftype,
			BUILT_IN_SETJMP_RECEIVER,
			"__builtin_setjmp_receiver", ECF_NOTHROW | ECF_LEAF);

  ftype = build_function_type_list (ptr_type_node, NULL_TREE);
  local_define_builtin ("__builtin_stack_save", ftype, BUILT_IN_STACK_SAVE,
			"__builtin_stack_save", ECF_NOTHROW | ECF_LEAF);

  ftype = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);
  local_define_builtin ("__builtin_stack_restore", ftype,
			BUILT_IN_STACK_RESTORE,
			"__builtin_stack_restore", ECF_NOTHROW | ECF_LEAF);

  ftype = build_function_type_list (integer_type_node, const_ptr_type_node,
				    const_ptr_type_node, size_type_node,
				    NULL_TREE);
  local_define_builtin ("__builtin_memcmp_eq", ftype, BUILT_IN_MEMCMP_EQ,
			"__builtin_memcmp_eq",
			ECF_PURE | ECF_NOTHROW | ECF_LEAF);

  /* If there's a possibility that we might use the ARM EABI, build the
    alternate __cxa_end_cleanup node used to resume from C++ and Java.  */
  if (targetm.arm_eabi_unwinder)
    {
      ftype = build_function_type_list (void_type_node, NULL_TREE);
      local_define_builtin ("__builtin_cxa_end_cleanup", ftype,
			    BUILT_IN_CXA_END_CLEANUP,
			    "__cxa_end_cleanup", ECF_NORETURN | ECF_LEAF);
    }

  ftype = build_function_type_list (void_type_node, ptr_type_node, NULL_TREE);
  local_define_builtin ("__builtin_unwind_resume", ftype,
			BUILT_IN_UNWIND_RESUME,
			((targetm_common.except_unwind_info (&global_options)
			  == UI_SJLJ)
			 ? "_Unwind_SjLj_Resume" : "_Unwind_Resume"),
			ECF_NORETURN);

  if (builtin_decl_explicit (BUILT_IN_RETURN_ADDRESS) == NULL_TREE)
    {
      ftype = build_function_type_list (ptr_type_node, integer_type_node,
					NULL_TREE);
      local_define_builtin ("__builtin_return_address", ftype,
			    BUILT_IN_RETURN_ADDRESS,
			    "__builtin_return_address",
			    ECF_NOTHROW);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_PROFILE_FUNC_ENTER)
      || !builtin_decl_explicit_p (BUILT_IN_PROFILE_FUNC_EXIT))
    {
      ftype = build_function_type_list (void_type_node, ptr_type_node,
					ptr_type_node, NULL_TREE);
      if (!builtin_decl_explicit_p (BUILT_IN_PROFILE_FUNC_ENTER))
	local_define_builtin ("__cyg_profile_func_enter", ftype,
			      BUILT_IN_PROFILE_FUNC_ENTER,
			      "__cyg_profile_func_enter", 0);
      if (!builtin_decl_explicit_p (BUILT_IN_PROFILE_FUNC_EXIT))
	local_define_builtin ("__cyg_profile_func_exit", ftype,
			      BUILT_IN_PROFILE_FUNC_EXIT,
			      "__cyg_profile_func_exit", 0);
    }

  /* The exception object and filter values from the runtime.  The argument
     must be zero before exception lowering, i.e. from the front end.  After
     exception lowering, it will be the region number for the exception
     landing pad.  These functions are PURE instead of CONST to prevent
     them from being hoisted past the exception edge that will initialize
     its value in the landing pad.  */
  ftype = build_function_type_list (ptr_type_node,
				    integer_type_node, NULL_TREE);
  ecf_flags = ECF_PURE | ECF_NOTHROW | ECF_LEAF;
  /* Only use TM_PURE if we have TM language support.  */
  if (builtin_decl_explicit_p (BUILT_IN_TM_LOAD_1))
    ecf_flags |= ECF_TM_PURE;
  local_define_builtin ("__builtin_eh_pointer", ftype, BUILT_IN_EH_POINTER,
			"__builtin_eh_pointer", ecf_flags);

  tmp = lang_hooks.types.type_for_mode (targetm.eh_return_filter_mode (), 0);
  ftype = build_function_type_list (tmp, integer_type_node, NULL_TREE);
  local_define_builtin ("__builtin_eh_filter", ftype, BUILT_IN_EH_FILTER,
			"__builtin_eh_filter", ECF_PURE | ECF_NOTHROW | ECF_LEAF);

  ftype = build_function_type_list (void_type_node,
				    integer_type_node, integer_type_node,
				    NULL_TREE);
  local_define_builtin ("__builtin_eh_copy_values", ftype,
			BUILT_IN_EH_COPY_VALUES,
			"__builtin_eh_copy_values", ECF_NOTHROW);

  /* Complex multiplication and division.  These are handled as builtins
     rather than optabs because emit_library_call_value doesn't support
     complex.  Further, we can do slightly better with folding these
     beasties if the real and complex parts of the arguments are separate.  */
  {
    int mode;

    for (mode = MIN_MODE_COMPLEX_FLOAT; mode <= MAX_MODE_COMPLEX_FLOAT; ++mode)
      {
	char mode_name_buf[4], *q;
	const char *p;
	enum built_in_function mcode, dcode;
	tree type, inner_type;
	const char *prefix = "__";

	if (targetm.libfunc_gnu_prefix)
	  prefix = "__gnu_";

	type = lang_hooks.types.type_for_mode ((machine_mode) mode, 0);
	if (type == NULL)
	  continue;
	inner_type = TREE_TYPE (type);

	ftype = build_function_type_list (type, inner_type, inner_type,
					  inner_type, inner_type, NULL_TREE);

        mcode = ((enum built_in_function)
		 (BUILT_IN_COMPLEX_MUL_MIN + mode - MIN_MODE_COMPLEX_FLOAT));
        dcode = ((enum built_in_function)
		 (BUILT_IN_COMPLEX_DIV_MIN + mode - MIN_MODE_COMPLEX_FLOAT));

        for (p = GET_MODE_NAME (mode), q = mode_name_buf; *p; p++, q++)
	  *q = TOLOWER (*p);
	*q = '\0';

	built_in_names[mcode] = concat (prefix, "mul", mode_name_buf, "3",
					NULL);
        local_define_builtin (built_in_names[mcode], ftype, mcode,
			      built_in_names[mcode],
			      ECF_CONST | ECF_NOTHROW | ECF_LEAF);

	built_in_names[dcode] = concat (prefix, "div", mode_name_buf, "3",
					NULL);
        local_define_builtin (built_in_names[dcode], ftype, dcode,
			      built_in_names[dcode],
			      ECF_CONST | ECF_NOTHROW | ECF_LEAF);
      }
  }

  init_internal_fns ();
}

/* HACK.  GROSS.  This is absolutely disgusting.  I wish there was a
   better way.

   If we requested a pointer to a vector, build up the pointers that
   we stripped off while looking for the inner type.  Similarly for
   return values from functions.

   The argument TYPE is the top of the chain, and BOTTOM is the
   new type which we will point to.  */

tree
reconstruct_complex_type (tree type, tree bottom)
{
  tree inner, outer;

  if (TREE_CODE (type) == POINTER_TYPE)
    {
      inner = reconstruct_complex_type (TREE_TYPE (type), bottom);
      outer = build_pointer_type_for_mode (inner, TYPE_MODE (type),
					   TYPE_REF_CAN_ALIAS_ALL (type));
    }
  else if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      inner = reconstruct_complex_type (TREE_TYPE (type), bottom);
      outer = build_reference_type_for_mode (inner, TYPE_MODE (type),
					     TYPE_REF_CAN_ALIAS_ALL (type));
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      inner = reconstruct_complex_type (TREE_TYPE (type), bottom);
      outer = build_array_type (inner, TYPE_DOMAIN (type));
    }
  else if (TREE_CODE (type) == FUNCTION_TYPE)
    {
      inner = reconstruct_complex_type (TREE_TYPE (type), bottom);
      outer = build_function_type (inner, TYPE_ARG_TYPES (type));
    }
  else if (TREE_CODE (type) == METHOD_TYPE)
    {
      inner = reconstruct_complex_type (TREE_TYPE (type), bottom);
      /* The build_method_type_directly() routine prepends 'this' to argument list,
         so we must compensate by getting rid of it.  */
      outer
	= build_method_type_directly
	    (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (type))),
	     inner,
	     TREE_CHAIN (TYPE_ARG_TYPES (type)));
    }
  else if (TREE_CODE (type) == OFFSET_TYPE)
    {
      inner = reconstruct_complex_type (TREE_TYPE (type), bottom);
      outer = build_offset_type (TYPE_OFFSET_BASETYPE (type), inner);
    }
  else
    return bottom;

  return build_type_attribute_qual_variant (outer, TYPE_ATTRIBUTES (type),
					    TYPE_QUALS (type));
}

/* Returns a vector tree node given a mode (integer, vector, or BLKmode) and
   the inner type.  */
tree
build_vector_type_for_mode (tree innertype, machine_mode mode)
{
  int nunits;

  switch (GET_MODE_CLASS (mode))
    {
    case MODE_VECTOR_INT:
    case MODE_VECTOR_FLOAT:
    case MODE_VECTOR_FRACT:
    case MODE_VECTOR_UFRACT:
    case MODE_VECTOR_ACCUM:
    case MODE_VECTOR_UACCUM:
      nunits = GET_MODE_NUNITS (mode);
      break;

    case MODE_INT:
      /* Check that there are no leftover bits.  */
      gcc_assert (GET_MODE_BITSIZE (mode)
		  % TREE_INT_CST_LOW (TYPE_SIZE (innertype)) == 0);

      nunits = GET_MODE_BITSIZE (mode)
	       / TREE_INT_CST_LOW (TYPE_SIZE (innertype));
      break;

    default:
      gcc_unreachable ();
    }

  return make_vector_type (innertype, nunits, mode);
}

/* Similarly, but takes the inner type and number of units, which must be
   a power of two.  */

tree
build_vector_type (tree innertype, int nunits)
{
  return make_vector_type (innertype, nunits, VOIDmode);
}

/* Build truth vector with specified length and number of units.  */

tree
build_truth_vector_type (unsigned nunits, unsigned vector_size)
{
  machine_mode mask_mode = targetm.vectorize.get_mask_mode (nunits,
							    vector_size);

  gcc_assert (mask_mode != VOIDmode);

  unsigned HOST_WIDE_INT vsize;
  if (mask_mode == BLKmode)
    vsize = vector_size * BITS_PER_UNIT;
  else
    vsize = GET_MODE_BITSIZE (mask_mode);

  unsigned HOST_WIDE_INT esize = vsize / nunits;
  gcc_assert (esize * nunits == vsize);

  tree bool_type = build_nonstandard_boolean_type (esize);

  return make_vector_type (bool_type, nunits, mask_mode);
}

/* Returns a vector type corresponding to a comparison of VECTYPE.  */

tree
build_same_sized_truth_vector_type (tree vectype)
{
  if (VECTOR_BOOLEAN_TYPE_P (vectype))
    return vectype;

  unsigned HOST_WIDE_INT size = GET_MODE_SIZE (TYPE_MODE (vectype));

  if (!size)
    size = tree_to_uhwi (TYPE_SIZE_UNIT (vectype));

  return build_truth_vector_type (TYPE_VECTOR_SUBPARTS (vectype), size);
}

/* Similarly, but builds a variant type with TYPE_VECTOR_OPAQUE set.  */

tree
build_opaque_vector_type (tree innertype, int nunits)
{
  tree t = make_vector_type (innertype, nunits, VOIDmode);
  tree cand;
  /* We always build the non-opaque variant before the opaque one,
     so if it already exists, it is TYPE_NEXT_VARIANT of this one.  */
  cand = TYPE_NEXT_VARIANT (t);
  if (cand
      && TYPE_VECTOR_OPAQUE (cand)
      && check_qualified_type (cand, t, TYPE_QUALS (t)))
    return cand;
  /* Othewise build a variant type and make sure to queue it after
     the non-opaque type.  */
  cand = build_distinct_type_copy (t);
  TYPE_VECTOR_OPAQUE (cand) = true;
  TYPE_CANONICAL (cand) = TYPE_CANONICAL (t);
  TYPE_NEXT_VARIANT (cand) = TYPE_NEXT_VARIANT (t);
  TYPE_NEXT_VARIANT (t) = cand;
  TYPE_MAIN_VARIANT (cand) = TYPE_MAIN_VARIANT (t);
  return cand;
}


/* Given an initializer INIT, return TRUE if INIT is zero or some
   aggregate of zeros.  Otherwise return FALSE.  */
bool
initializer_zerop (const_tree init)
{
  tree elt;

  STRIP_NOPS (init);

  switch (TREE_CODE (init))
    {
    case INTEGER_CST:
      return integer_zerop (init);

    case REAL_CST:
      /* ??? Note that this is not correct for C4X float formats.  There,
	 a bit pattern of all zeros is 1.0; 0.0 is encoded with the most
	 negative exponent.  */
      return real_zerop (init)
	&& ! REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (init));

    case FIXED_CST:
      return fixed_zerop (init);

    case COMPLEX_CST:
      return integer_zerop (init)
	|| (real_zerop (init)
	    && ! REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (TREE_REALPART (init)))
	    && ! REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (TREE_IMAGPART (init))));

    case VECTOR_CST:
      {
	unsigned i;
	for (i = 0; i < VECTOR_CST_NELTS (init); ++i)
	  if (!initializer_zerop (VECTOR_CST_ELT (init, i)))
	    return false;
	return true;
      }

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx;

	if (TREE_CLOBBER_P (init))
	  return false;
	FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (init), idx, elt)
	  if (!initializer_zerop (elt))
	    return false;
	return true;
      }

    case STRING_CST:
      {
	int i;

	/* We need to loop through all elements to handle cases like
	   "\0" and "\0foobar".  */
	for (i = 0; i < TREE_STRING_LENGTH (init); ++i)
	  if (TREE_STRING_POINTER (init)[i] != '\0')
	    return false;

	return true;
      }

    default:
      return false;
    }
}

/* Check if vector VEC consists of all the equal elements and
   that the number of elements corresponds to the type of VEC.
   The function returns first element of the vector
   or NULL_TREE if the vector is not uniform.  */
tree
uniform_vector_p (const_tree vec)
{
  tree first, t;
  unsigned i;

  if (vec == NULL_TREE)
    return NULL_TREE;

  gcc_assert (VECTOR_TYPE_P (TREE_TYPE (vec)));

  if (TREE_CODE (vec) == VECTOR_CST)
    {
      first = VECTOR_CST_ELT (vec, 0);
      for (i = 1; i < VECTOR_CST_NELTS (vec); ++i)
	if (!operand_equal_p (first, VECTOR_CST_ELT (vec, i), 0))
	  return NULL_TREE;

      return first;
    }

  else if (TREE_CODE (vec) == CONSTRUCTOR)
    {
      first = error_mark_node;

      FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (vec), i, t)
        {
          if (i == 0)
            {
              first = t;
              continue;
            }
	  if (!operand_equal_p (first, t, 0))
	    return NULL_TREE;
        }
      if (i != TYPE_VECTOR_SUBPARTS (TREE_TYPE (vec)))
	return NULL_TREE;

      return first;
    }

  return NULL_TREE;
}

/* Build an empty statement at location LOC.  */

tree
build_empty_stmt (location_t loc)
{
  tree t = build1 (NOP_EXPR, void_type_node, size_zero_node);
  SET_EXPR_LOCATION (t, loc);
  return t;
}


/* Build an OpenMP clause with code CODE.  LOC is the location of the
   clause.  */

tree
build_omp_clause (location_t loc, enum omp_clause_code code)
{
  tree t;
  int size, length;

  length = omp_clause_num_ops[code];
  size = (sizeof (struct tree_omp_clause) + (length - 1) * sizeof (tree));

  record_node_allocation_statistics (OMP_CLAUSE, size);

  t = (tree) ggc_internal_alloc (size);
  memset (t, 0, size);
  TREE_SET_CODE (t, OMP_CLAUSE);
  OMP_CLAUSE_SET_CODE (t, code);
  OMP_CLAUSE_LOCATION (t) = loc;

  return t;
}

/* Build a tcc_vl_exp object with code CODE and room for LEN operands.  LEN
   includes the implicit operand count in TREE_OPERAND 0, and so must be >= 1.
   Except for the CODE and operand count field, other storage for the
   object is initialized to zeros.  */

tree
build_vl_exp_stat (enum tree_code code, int len MEM_STAT_DECL)
{
  tree t;
  int length = (len - 1) * sizeof (tree) + sizeof (struct tree_exp);

  gcc_assert (TREE_CODE_CLASS (code) == tcc_vl_exp);
  gcc_assert (len >= 1);

  record_node_allocation_statistics (code, length);

  t = ggc_alloc_cleared_tree_node_stat (length PASS_MEM_STAT);

  TREE_SET_CODE (t, code);

  /* Can't use TREE_OPERAND to store the length because if checking is
     enabled, it will try to check the length before we store it.  :-P  */
  t->exp.operands[0] = build_int_cst (sizetype, len);

  return t;
}

/* Helper function for build_call_* functions; build a CALL_EXPR with
   indicated RETURN_TYPE, FN, and NARGS, but do not initialize any of
   the argument slots.  */

static tree
build_call_1 (tree return_type, tree fn, int nargs)
{
  tree t;

  t = build_vl_exp (CALL_EXPR, nargs + 3);
  TREE_TYPE (t) = return_type;
  CALL_EXPR_FN (t) = fn;
  CALL_EXPR_STATIC_CHAIN (t) = NULL;

  return t;
}

/* Build a CALL_EXPR of class tcc_vl_exp with the indicated RETURN_TYPE and
   FN and a null static chain slot.  NARGS is the number of call arguments
   which are specified as "..." arguments.  */

tree
build_call_nary (tree return_type, tree fn, int nargs, ...)
{
  tree ret;
  va_list args;
  va_start (args, nargs);
  ret = build_call_valist (return_type, fn, nargs, args);
  va_end (args);
  return ret;
}

/* Build a CALL_EXPR of class tcc_vl_exp with the indicated RETURN_TYPE and
   FN and a null static chain slot.  NARGS is the number of call arguments
   which are specified as a va_list ARGS.  */

tree
build_call_valist (tree return_type, tree fn, int nargs, va_list args)
{
  tree t;
  int i;

  t = build_call_1 (return_type, fn, nargs);
  for (i = 0; i < nargs; i++)
    CALL_EXPR_ARG (t, i) = va_arg (args, tree);
  process_call_operands (t);
  return t;
}

/* Build a CALL_EXPR of class tcc_vl_exp with the indicated RETURN_TYPE and
   FN and a null static chain slot.  NARGS is the number of call arguments
   which are specified as a tree array ARGS.  */

tree
build_call_array_loc (location_t loc, tree return_type, tree fn,
		      int nargs, const tree *args)
{
  tree t;
  int i;

  t = build_call_1 (return_type, fn, nargs);
  for (i = 0; i < nargs; i++)
    CALL_EXPR_ARG (t, i) = args[i];
  process_call_operands (t);
  SET_EXPR_LOCATION (t, loc);
  return t;
}

/* Like build_call_array, but takes a vec.  */

tree
build_call_vec (tree return_type, tree fn, vec<tree, va_gc> *args)
{
  tree ret, t;
  unsigned int ix;

  ret = build_call_1 (return_type, fn, vec_safe_length (args));
  FOR_EACH_VEC_SAFE_ELT (args, ix, t)
    CALL_EXPR_ARG (ret, ix) = t;
  process_call_operands (ret);
  return ret;
}

/* Conveniently construct a function call expression.  FNDECL names the
   function to be called and N arguments are passed in the array
   ARGARRAY.  */

tree
build_call_expr_loc_array (location_t loc, tree fndecl, int n, tree *argarray)
{
  tree fntype = TREE_TYPE (fndecl);
  tree fn = build1 (ADDR_EXPR, build_pointer_type (fntype), fndecl);
 
  return fold_build_call_array_loc (loc, TREE_TYPE (fntype), fn, n, argarray);
}

/* Conveniently construct a function call expression.  FNDECL names the
   function to be called and the arguments are passed in the vector
   VEC.  */

tree
build_call_expr_loc_vec (location_t loc, tree fndecl, vec<tree, va_gc> *vec)
{
  return build_call_expr_loc_array (loc, fndecl, vec_safe_length (vec),
				    vec_safe_address (vec));
}


/* Conveniently construct a function call expression.  FNDECL names the
   function to be called, N is the number of arguments, and the "..."
   parameters are the argument expressions.  */

tree
build_call_expr_loc (location_t loc, tree fndecl, int n, ...)
{
  va_list ap;
  tree *argarray = XALLOCAVEC (tree, n);
  int i;

  va_start (ap, n);
  for (i = 0; i < n; i++)
    argarray[i] = va_arg (ap, tree);
  va_end (ap);
  return build_call_expr_loc_array (loc, fndecl, n, argarray);
}

/* Like build_call_expr_loc (UNKNOWN_LOCATION, ...).  Duplicated because
   varargs macros aren't supported by all bootstrap compilers.  */

tree
build_call_expr (tree fndecl, int n, ...)
{
  va_list ap;
  tree *argarray = XALLOCAVEC (tree, n);
  int i;

  va_start (ap, n);
  for (i = 0; i < n; i++)
    argarray[i] = va_arg (ap, tree);
  va_end (ap);
  return build_call_expr_loc_array (UNKNOWN_LOCATION, fndecl, n, argarray);
}

/* Build an internal call to IFN, with arguments ARGS[0:N-1] and with return
   type TYPE.  This is just like CALL_EXPR, except its CALL_EXPR_FN is NULL.
   It will get gimplified later into an ordinary internal function.  */

tree
build_call_expr_internal_loc_array (location_t loc, internal_fn ifn,
				    tree type, int n, const tree *args)
{
  tree t = build_call_1 (type, NULL_TREE, n);
  for (int i = 0; i < n; ++i)
    CALL_EXPR_ARG (t, i) = args[i];
  SET_EXPR_LOCATION (t, loc);
  CALL_EXPR_IFN (t) = ifn;
  return t;
}

/* Build internal call expression.  This is just like CALL_EXPR, except
   its CALL_EXPR_FN is NULL.  It will get gimplified later into ordinary
   internal function.  */

tree
build_call_expr_internal_loc (location_t loc, enum internal_fn ifn,
			      tree type, int n, ...)
{
  va_list ap;
  tree *argarray = XALLOCAVEC (tree, n);
  int i;

  va_start (ap, n);
  for (i = 0; i < n; i++)
    argarray[i] = va_arg (ap, tree);
  va_end (ap);
  return build_call_expr_internal_loc_array (loc, ifn, type, n, argarray);
}

/* Return a function call to FN, if the target is guaranteed to support it,
   or null otherwise.

   N is the number of arguments, passed in the "...", and TYPE is the
   type of the return value.  */

tree
maybe_build_call_expr_loc (location_t loc, combined_fn fn, tree type,
			   int n, ...)
{
  va_list ap;
  tree *argarray = XALLOCAVEC (tree, n);
  int i;

  va_start (ap, n);
  for (i = 0; i < n; i++)
    argarray[i] = va_arg (ap, tree);
  va_end (ap);
  if (internal_fn_p (fn))
    {
      internal_fn ifn = as_internal_fn (fn);
      if (direct_internal_fn_p (ifn))
	{
	  tree_pair types = direct_internal_fn_types (ifn, type, argarray);
	  if (!direct_internal_fn_supported_p (ifn, types,
					       OPTIMIZE_FOR_BOTH))
	    return NULL_TREE;
	}
      return build_call_expr_internal_loc_array (loc, ifn, type, n, argarray);
    }
  else
    {
      tree fndecl = builtin_decl_implicit (as_builtin_fn (fn));
      if (!fndecl)
	return NULL_TREE;
      return build_call_expr_loc_array (loc, fndecl, n, argarray);
    }
}

/* Create a new constant string literal and return a char* pointer to it.
   The STRING_CST value is the LEN characters at STR.  */
tree
build_string_literal (int len, const char *str)
{
  tree t, elem, index, type;

  t = build_string (len, str);
  elem = build_type_variant (char_type_node, 1, 0);
  index = build_index_type (size_int (len - 1));
  type = build_array_type (elem, index);
  TREE_TYPE (t) = type;
  TREE_CONSTANT (t) = 1;
  TREE_READONLY (t) = 1;
  TREE_STATIC (t) = 1;

  type = build_pointer_type (elem);
  t = build1 (ADDR_EXPR, type,
	      build4 (ARRAY_REF, elem,
		      t, integer_zero_node, NULL_TREE, NULL_TREE));
  return t;
}



/* Return true if T (assumed to be a DECL) must be assigned a memory
   location.  */

bool
needs_to_live_in_memory (const_tree t)
{
  return (TREE_ADDRESSABLE (t)
	  || is_global_var (t)
	  || (TREE_CODE (t) == RESULT_DECL
	      && !DECL_BY_REFERENCE (t)
	      && aggregate_value_p (t, current_function_decl)));
}

/* Return value of a constant X and sign-extend it.  */

HOST_WIDE_INT
int_cst_value (const_tree x)
{
  unsigned bits = TYPE_PRECISION (TREE_TYPE (x));
  unsigned HOST_WIDE_INT val = TREE_INT_CST_LOW (x);

  /* Make sure the sign-extended value will fit in a HOST_WIDE_INT.  */
  gcc_assert (cst_and_fits_in_hwi (x));

  if (bits < HOST_BITS_PER_WIDE_INT)
    {
      bool negative = ((val >> (bits - 1)) & 1) != 0;
      if (negative)
	val |= (~(unsigned HOST_WIDE_INT) 0) << (bits - 1) << 1;
      else
	val &= ~((~(unsigned HOST_WIDE_INT) 0) << (bits - 1) << 1);
    }

  return val;
}

/* If TYPE is an integral or pointer type, return an integer type with
   the same precision which is unsigned iff UNSIGNEDP is true, or itself
   if TYPE is already an integer type of signedness UNSIGNEDP.  */

tree
signed_or_unsigned_type_for (int unsignedp, tree type)
{
  if (TREE_CODE (type) == INTEGER_TYPE && TYPE_UNSIGNED (type) == unsignedp)
    return type;

  if (TREE_CODE (type) == VECTOR_TYPE)
    {
      tree inner = TREE_TYPE (type);
      tree inner2 = signed_or_unsigned_type_for (unsignedp, inner);
      if (!inner2)
	return NULL_TREE;
      if (inner == inner2)
	return type;
      return build_vector_type (inner2, TYPE_VECTOR_SUBPARTS (type));
    }

  if (!INTEGRAL_TYPE_P (type)
      && !POINTER_TYPE_P (type)
      && TREE_CODE (type) != OFFSET_TYPE)
    return NULL_TREE;

  return build_nonstandard_integer_type (TYPE_PRECISION (type), unsignedp);
}

/* If TYPE is an integral or pointer type, return an integer type with
   the same precision which is unsigned, or itself if TYPE is already an
   unsigned integer type.  */

tree
unsigned_type_for (tree type)
{
  return signed_or_unsigned_type_for (1, type);
}

/* If TYPE is an integral or pointer type, return an integer type with
   the same precision which is signed, or itself if TYPE is already a
   signed integer type.  */

tree
signed_type_for (tree type)
{
  return signed_or_unsigned_type_for (0, type);
}

/* If TYPE is a vector type, return a signed integer vector type with the
   same width and number of subparts. Otherwise return boolean_type_node.  */

tree
truth_type_for (tree type)
{
  if (TREE_CODE (type) == VECTOR_TYPE)
    {
      if (VECTOR_BOOLEAN_TYPE_P (type))
	return type;
      return build_truth_vector_type (TYPE_VECTOR_SUBPARTS (type),
				      GET_MODE_SIZE (TYPE_MODE (type)));
    }
  else
    return boolean_type_node;
}

/* Returns the largest value obtainable by casting something in INNER type to
   OUTER type.  */

tree
upper_bound_in_type (tree outer, tree inner)
{
  unsigned int det = 0;
  unsigned oprec = TYPE_PRECISION (outer);
  unsigned iprec = TYPE_PRECISION (inner);
  unsigned prec;

  /* Compute a unique number for every combination.  */
  det |= (oprec > iprec) ? 4 : 0;
  det |= TYPE_UNSIGNED (outer) ? 2 : 0;
  det |= TYPE_UNSIGNED (inner) ? 1 : 0;

  /* Determine the exponent to use.  */
  switch (det)
    {
    case 0:
    case 1:
      /* oprec <= iprec, outer: signed, inner: don't care.  */
      prec = oprec - 1;
      break;
    case 2:
    case 3:
      /* oprec <= iprec, outer: unsigned, inner: don't care.  */
      prec = oprec;
      break;
    case 4:
      /* oprec > iprec, outer: signed, inner: signed.  */
      prec = iprec - 1;
      break;
    case 5:
      /* oprec > iprec, outer: signed, inner: unsigned.  */
      prec = iprec;
      break;
    case 6:
      /* oprec > iprec, outer: unsigned, inner: signed.  */
      prec = oprec;
      break;
    case 7:
      /* oprec > iprec, outer: unsigned, inner: unsigned.  */
      prec = iprec;
      break;
    default:
      gcc_unreachable ();
    }

  return wide_int_to_tree (outer,
			   wi::mask (prec, false, TYPE_PRECISION (outer)));
}

/* Returns the smallest value obtainable by casting something in INNER type to
   OUTER type.  */

tree
lower_bound_in_type (tree outer, tree inner)
{
  unsigned oprec = TYPE_PRECISION (outer);
  unsigned iprec = TYPE_PRECISION (inner);

  /* If OUTER type is unsigned, we can definitely cast 0 to OUTER type
     and obtain 0.  */
  if (TYPE_UNSIGNED (outer)
      /* If we are widening something of an unsigned type, OUTER type
	 contains all values of INNER type.  In particular, both INNER
	 and OUTER types have zero in common.  */
      || (oprec > iprec && TYPE_UNSIGNED (inner)))
    return build_int_cst (outer, 0);
  else
    {
      /* If we are widening a signed type to another signed type, we
	 want to obtain -2^^(iprec-1).  If we are keeping the
	 precision or narrowing to a signed type, we want to obtain
	 -2^(oprec-1).  */
      unsigned prec = oprec > iprec ? iprec : oprec;
      return wide_int_to_tree (outer,
			       wi::mask (prec - 1, true,
					 TYPE_PRECISION (outer)));
    }
}

/* Return nonzero if two operands that are suitable for PHI nodes are
   necessarily equal.  Specifically, both ARG0 and ARG1 must be either
   SSA_NAME or invariant.  Note that this is strictly an optimization.
   That is, callers of this function can directly call operand_equal_p
   and get the same result, only slower.  */

int
operand_equal_for_phi_arg_p (const_tree arg0, const_tree arg1)
{
  if (arg0 == arg1)
    return 1;
  if (TREE_CODE (arg0) == SSA_NAME || TREE_CODE (arg1) == SSA_NAME)
    return 0;
  return operand_equal_p (arg0, arg1, 0);
}

/* Returns number of zeros at the end of binary representation of X.  */

tree
num_ending_zeros (const_tree x)
{
  return build_int_cst (TREE_TYPE (x), wi::ctz (x));
}


#define WALK_SUBTREE(NODE)				\
  do							\
    {							\
      result = walk_tree_1 (&(NODE), func, data, pset, lh);	\
      if (result)					\
	return result;					\
    }							\
  while (0)

/* This is a subroutine of walk_tree that walks field of TYPE that are to
   be walked whenever a type is seen in the tree.  Rest of operands and return
   value are as for walk_tree.  */

static tree
walk_type_fields (tree type, walk_tree_fn func, void *data,
		  hash_set<tree> *pset, walk_tree_lh lh)
{
  tree result = NULL_TREE;

  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case VECTOR_TYPE:
      /* We have to worry about mutually recursive pointers.  These can't
	 be written in C.  They can in Ada.  It's pathological, but
	 there's an ACATS test (c38102a) that checks it.  Deal with this
	 by checking if we're pointing to another pointer, that one
	 points to another pointer, that one does too, and we have no htab.
	 If so, get a hash table.  We check three levels deep to avoid
	 the cost of the hash table if we don't need one.  */
      if (POINTER_TYPE_P (TREE_TYPE (type))
	  && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (type)))
	  && POINTER_TYPE_P (TREE_TYPE (TREE_TYPE (TREE_TYPE (type))))
	  && !pset)
	{
	  result = walk_tree_without_duplicates (&TREE_TYPE (type),
						 func, data);
	  if (result)
	    return result;

	  break;
	}

      /* ... fall through ... */

    case COMPLEX_TYPE:
      WALK_SUBTREE (TREE_TYPE (type));
      break;

    case METHOD_TYPE:
      WALK_SUBTREE (TYPE_METHOD_BASETYPE (type));

      /* Fall through.  */

    case FUNCTION_TYPE:
      WALK_SUBTREE (TREE_TYPE (type));
      {
	tree arg;

	/* We never want to walk into default arguments.  */
	for (arg = TYPE_ARG_TYPES (type); arg; arg = TREE_CHAIN (arg))
	  WALK_SUBTREE (TREE_VALUE (arg));
      }
      break;

    case ARRAY_TYPE:
      /* Don't follow this nodes's type if a pointer for fear that
	 we'll have infinite recursion.  If we have a PSET, then we
	 need not fear.  */
      if (pset
	  || (!POINTER_TYPE_P (TREE_TYPE (type))
	      && TREE_CODE (TREE_TYPE (type)) != OFFSET_TYPE))
	WALK_SUBTREE (TREE_TYPE (type));
      WALK_SUBTREE (TYPE_DOMAIN (type));
      break;

    case OFFSET_TYPE:
      WALK_SUBTREE (TREE_TYPE (type));
      WALK_SUBTREE (TYPE_OFFSET_BASETYPE (type));
      break;

    default:
      break;
    }

  return NULL_TREE;
}

/* Apply FUNC to all the sub-trees of TP in a pre-order traversal.  FUNC is
   called with the DATA and the address of each sub-tree.  If FUNC returns a
   non-NULL value, the traversal is stopped, and the value returned by FUNC
   is returned.  If PSET is non-NULL it is used to record the nodes visited,
   and to avoid visiting a node more than once.  */

tree
walk_tree_1 (tree *tp, walk_tree_fn func, void *data,
	     hash_set<tree> *pset, walk_tree_lh lh)
{
  enum tree_code code;
  int walk_subtrees;
  tree result;

#define WALK_SUBTREE_TAIL(NODE)				\
  do							\
    {							\
       tp = & (NODE);					\
       goto tail_recurse;				\
    }							\
  while (0)

 tail_recurse:
  /* Skip empty subtrees.  */
  if (!*tp)
    return NULL_TREE;

  /* Don't walk the same tree twice, if the user has requested
     that we avoid doing so.  */
  if (pset && pset->add (*tp))
    return NULL_TREE;

  /* Call the function.  */
  walk_subtrees = 1;
  result = (*func) (tp, &walk_subtrees, data);

  /* If we found something, return it.  */
  if (result)
    return result;

  code = TREE_CODE (*tp);

  /* Even if we didn't, FUNC may have decided that there was nothing
     interesting below this point in the tree.  */
  if (!walk_subtrees)
    {
      /* But we still need to check our siblings.  */
      if (code == TREE_LIST)
	WALK_SUBTREE_TAIL (TREE_CHAIN (*tp));
      else if (code == OMP_CLAUSE)
	WALK_SUBTREE_TAIL (OMP_CLAUSE_CHAIN (*tp));
      else
	return NULL_TREE;
    }

  if (lh)
    {
      result = (*lh) (tp, &walk_subtrees, func, data, pset);
      if (result || !walk_subtrees)
        return result;
    }

  switch (code)
    {
    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case INTEGER_CST:
    case REAL_CST:
    case FIXED_CST:
    case VECTOR_CST:
    case STRING_CST:
    case BLOCK:
    case PLACEHOLDER_EXPR:
    case SSA_NAME:
    case FIELD_DECL:
    case RESULT_DECL:
      /* None of these have subtrees other than those already walked
	 above.  */
      break;

    case TREE_LIST:
      WALK_SUBTREE (TREE_VALUE (*tp));
      WALK_SUBTREE_TAIL (TREE_CHAIN (*tp));
      break;

    case TREE_VEC:
      {
	int len = TREE_VEC_LENGTH (*tp);

	if (len == 0)
	  break;

	/* Walk all elements but the first.  */
	while (--len)
	  WALK_SUBTREE (TREE_VEC_ELT (*tp, len));

	/* Now walk the first one as a tail call.  */
	WALK_SUBTREE_TAIL (TREE_VEC_ELT (*tp, 0));
      }

    case COMPLEX_CST:
      WALK_SUBTREE (TREE_REALPART (*tp));
      WALK_SUBTREE_TAIL (TREE_IMAGPART (*tp));

    case CONSTRUCTOR:
      {
	unsigned HOST_WIDE_INT idx;
	constructor_elt *ce;

	for (idx = 0; vec_safe_iterate (CONSTRUCTOR_ELTS (*tp), idx, &ce);
	     idx++)
	  WALK_SUBTREE (ce->value);
      }
      break;

    case SAVE_EXPR:
      WALK_SUBTREE_TAIL (TREE_OPERAND (*tp, 0));

    case BIND_EXPR:
      {
	tree decl;
	for (decl = BIND_EXPR_VARS (*tp); decl; decl = DECL_CHAIN (decl))
	  {
	    /* Walk the DECL_INITIAL and DECL_SIZE.  We don't want to walk
	       into declarations that are just mentioned, rather than
	       declared; they don't really belong to this part of the tree.
	       And, we can see cycles: the initializer for a declaration
	       can refer to the declaration itself.  */
	    WALK_SUBTREE (DECL_INITIAL (decl));
	    WALK_SUBTREE (DECL_SIZE (decl));
	    WALK_SUBTREE (DECL_SIZE_UNIT (decl));
	  }
	WALK_SUBTREE_TAIL (BIND_EXPR_BODY (*tp));
      }

    case STATEMENT_LIST:
      {
	tree_stmt_iterator i;
	for (i = tsi_start (*tp); !tsi_end_p (i); tsi_next (&i))
	  WALK_SUBTREE (*tsi_stmt_ptr (i));
      }
      break;

    case OMP_CLAUSE:
      switch (OMP_CLAUSE_CODE (*tp))
	{
	case OMP_CLAUSE_GANG:
	case OMP_CLAUSE__GRIDDIM_:
	  WALK_SUBTREE (OMP_CLAUSE_OPERAND (*tp, 1));
	  /* FALLTHRU */

	case OMP_CLAUSE_ASYNC:
	case OMP_CLAUSE_WAIT:
	case OMP_CLAUSE_WORKER:
	case OMP_CLAUSE_VECTOR:
	case OMP_CLAUSE_NUM_GANGS:
	case OMP_CLAUSE_NUM_WORKERS:
	case OMP_CLAUSE_VECTOR_LENGTH:
	case OMP_CLAUSE_PRIVATE:
	case OMP_CLAUSE_SHARED:
	case OMP_CLAUSE_FIRSTPRIVATE:
	case OMP_CLAUSE_COPYIN:
	case OMP_CLAUSE_COPYPRIVATE:
	case OMP_CLAUSE_FINAL:
	case OMP_CLAUSE_IF:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_SCHEDULE:
	case OMP_CLAUSE_UNIFORM:
	case OMP_CLAUSE_DEPEND:
	case OMP_CLAUSE_NUM_TEAMS:
	case OMP_CLAUSE_THREAD_LIMIT:
	case OMP_CLAUSE_DEVICE:
	case OMP_CLAUSE_DIST_SCHEDULE:
	case OMP_CLAUSE_SAFELEN:
	case OMP_CLAUSE_SIMDLEN:
	case OMP_CLAUSE_ORDERED:
	case OMP_CLAUSE_PRIORITY:
	case OMP_CLAUSE_GRAINSIZE:
	case OMP_CLAUSE_NUM_TASKS:
	case OMP_CLAUSE_HINT:
	case OMP_CLAUSE_TO_DECLARE:
	case OMP_CLAUSE_LINK:
	case OMP_CLAUSE_USE_DEVICE_PTR:
	case OMP_CLAUSE_IS_DEVICE_PTR:
	case OMP_CLAUSE__LOOPTEMP_:
	case OMP_CLAUSE__SIMDUID_:
	case OMP_CLAUSE__CILK_FOR_COUNT_:
	  WALK_SUBTREE (OMP_CLAUSE_OPERAND (*tp, 0));
	  /* FALLTHRU */

	case OMP_CLAUSE_INDEPENDENT:
	case OMP_CLAUSE_NOWAIT:
	case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_UNTIED:
	case OMP_CLAUSE_MERGEABLE:
	case OMP_CLAUSE_PROC_BIND:
	case OMP_CLAUSE_INBRANCH:
	case OMP_CLAUSE_NOTINBRANCH:
	case OMP_CLAUSE_FOR:
	case OMP_CLAUSE_PARALLEL:
	case OMP_CLAUSE_SECTIONS:
	case OMP_CLAUSE_TASKGROUP:
	case OMP_CLAUSE_NOGROUP:
	case OMP_CLAUSE_THREADS:
	case OMP_CLAUSE_SIMD:
	case OMP_CLAUSE_DEFAULTMAP:
	case OMP_CLAUSE_AUTO:
	case OMP_CLAUSE_SEQ:
	case OMP_CLAUSE_TILE:
	  WALK_SUBTREE_TAIL (OMP_CLAUSE_CHAIN (*tp));

	case OMP_CLAUSE_LASTPRIVATE:
	  WALK_SUBTREE (OMP_CLAUSE_DECL (*tp));
	  WALK_SUBTREE (OMP_CLAUSE_LASTPRIVATE_STMT (*tp));
	  WALK_SUBTREE_TAIL (OMP_CLAUSE_CHAIN (*tp));

	case OMP_CLAUSE_COLLAPSE:
	  {
	    int i;
	    for (i = 0; i < 3; i++)
	      WALK_SUBTREE (OMP_CLAUSE_OPERAND (*tp, i));
	    WALK_SUBTREE_TAIL (OMP_CLAUSE_CHAIN (*tp));
	  }

	case OMP_CLAUSE_LINEAR:
	  WALK_SUBTREE (OMP_CLAUSE_DECL (*tp));
	  WALK_SUBTREE (OMP_CLAUSE_LINEAR_STEP (*tp));
	  WALK_SUBTREE (OMP_CLAUSE_LINEAR_STMT (*tp));
	  WALK_SUBTREE_TAIL (OMP_CLAUSE_CHAIN (*tp));

	case OMP_CLAUSE_ALIGNED:
	case OMP_CLAUSE_FROM:
	case OMP_CLAUSE_TO:
	case OMP_CLAUSE_MAP:
	case OMP_CLAUSE__CACHE_:
	  WALK_SUBTREE (OMP_CLAUSE_DECL (*tp));
	  WALK_SUBTREE (OMP_CLAUSE_OPERAND (*tp, 1));
	  WALK_SUBTREE_TAIL (OMP_CLAUSE_CHAIN (*tp));

	case OMP_CLAUSE_REDUCTION:
	  {
	    int i;
	    for (i = 0; i < 5; i++)
	      WALK_SUBTREE (OMP_CLAUSE_OPERAND (*tp, i));
	    WALK_SUBTREE_TAIL (OMP_CLAUSE_CHAIN (*tp));
	  }

	default:
	  gcc_unreachable ();
	}
      break;

    case TARGET_EXPR:
      {
	int i, len;

	/* TARGET_EXPRs are peculiar: operands 1 and 3 can be the same.
	   But, we only want to walk once.  */
	len = (TREE_OPERAND (*tp, 3) == TREE_OPERAND (*tp, 1)) ? 2 : 3;
	for (i = 0; i < len; ++i)
	  WALK_SUBTREE (TREE_OPERAND (*tp, i));
	WALK_SUBTREE_TAIL (TREE_OPERAND (*tp, len));
      }

    case DECL_EXPR:
      /* If this is a TYPE_DECL, walk into the fields of the type that it's
	 defining.  We only want to walk into these fields of a type in this
	 case and not in the general case of a mere reference to the type.

	 The criterion is as follows: if the field can be an expression, it
	 must be walked only here.  This should be in keeping with the fields
	 that are directly gimplified in gimplify_type_sizes in order for the
	 mark/copy-if-shared/unmark machinery of the gimplifier to work with
	 variable-sized types.

	 Note that DECLs get walked as part of processing the BIND_EXPR.  */
      if (TREE_CODE (DECL_EXPR_DECL (*tp)) == TYPE_DECL)
	{
	  tree *type_p = &TREE_TYPE (DECL_EXPR_DECL (*tp));
	  if (TREE_CODE (*type_p) == ERROR_MARK)
	    return NULL_TREE;

	  /* Call the function for the type.  See if it returns anything or
	     doesn't want us to continue.  If we are to continue, walk both
	     the normal fields and those for the declaration case.  */
	  result = (*func) (type_p, &walk_subtrees, data);
	  if (result || !walk_subtrees)
	    return result;

	  /* But do not walk a pointed-to type since it may itself need to
	     be walked in the declaration case if it isn't anonymous.  */
	  if (!POINTER_TYPE_P (*type_p))
	    {
	      result = walk_type_fields (*type_p, func, data, pset, lh);
	      if (result)
		return result;
	    }

	  /* If this is a record type, also walk the fields.  */
	  if (RECORD_OR_UNION_TYPE_P (*type_p))
	    {
	      tree field;

	      for (field = TYPE_FIELDS (*type_p); field;
		   field = DECL_CHAIN (field))
		{
		  /* We'd like to look at the type of the field, but we can
		     easily get infinite recursion.  So assume it's pointed
		     to elsewhere in the tree.  Also, ignore things that
		     aren't fields.  */
		  if (TREE_CODE (field) != FIELD_DECL)
		    continue;

		  WALK_SUBTREE (DECL_FIELD_OFFSET (field));
		  WALK_SUBTREE (DECL_SIZE (field));
		  WALK_SUBTREE (DECL_SIZE_UNIT (field));
		  if (TREE_CODE (*type_p) == QUAL_UNION_TYPE)
		    WALK_SUBTREE (DECL_QUALIFIER (field));
		}
	    }

	  /* Same for scalar types.  */
	  else if (TREE_CODE (*type_p) == BOOLEAN_TYPE
		   || TREE_CODE (*type_p) == ENUMERAL_TYPE
		   || TREE_CODE (*type_p) == INTEGER_TYPE
		   || TREE_CODE (*type_p) == FIXED_POINT_TYPE
		   || TREE_CODE (*type_p) == REAL_TYPE)
	    {
	      WALK_SUBTREE (TYPE_MIN_VALUE (*type_p));
	      WALK_SUBTREE (TYPE_MAX_VALUE (*type_p));
	    }

	  WALK_SUBTREE (TYPE_SIZE (*type_p));
	  WALK_SUBTREE_TAIL (TYPE_SIZE_UNIT (*type_p));
	}
      /* FALLTHRU */

    default:
      if (IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code)))
	{
	  int i, len;

	  /* Walk over all the sub-trees of this operand.  */
	  len = TREE_OPERAND_LENGTH (*tp);

	  /* Go through the subtrees.  We need to do this in forward order so
	     that the scope of a FOR_EXPR is handled properly.  */
	  if (len)
	    {
	      for (i = 0; i < len - 1; ++i)
		WALK_SUBTREE (TREE_OPERAND (*tp, i));
	      WALK_SUBTREE_TAIL (TREE_OPERAND (*tp, len - 1));
	    }
	}
      /* If this is a type, walk the needed fields in the type.  */
      else if (TYPE_P (*tp))
	return walk_type_fields (*tp, func, data, pset, lh);
      break;
    }

  /* We didn't find what we were looking for.  */
  return NULL_TREE;

#undef WALK_SUBTREE_TAIL
}
#undef WALK_SUBTREE

/* Like walk_tree, but does not walk duplicate nodes more than once.  */

tree
walk_tree_without_duplicates_1 (tree *tp, walk_tree_fn func, void *data,
				walk_tree_lh lh)
{
  tree result;

  hash_set<tree> pset;
  result = walk_tree_1 (tp, func, data, &pset, lh);
  return result;
}


tree
tree_block (tree t)
{
  const enum tree_code_class c = TREE_CODE_CLASS (TREE_CODE (t));

  if (IS_EXPR_CODE_CLASS (c))
    return LOCATION_BLOCK (t->exp.locus);
  gcc_unreachable ();
  return NULL;
}

void
tree_set_block (tree t, tree b)
{
  const enum tree_code_class c = TREE_CODE_CLASS (TREE_CODE (t));

  if (IS_EXPR_CODE_CLASS (c))
    {
      t->exp.locus = set_block (t->exp.locus, b);
    }
  else
    gcc_unreachable ();
}

/* Create a nameless artificial label and put it in the current
   function context.  The label has a location of LOC.  Returns the
   newly created label.  */

tree
create_artificial_label (location_t loc)
{
  tree lab = build_decl (loc,
      			 LABEL_DECL, NULL_TREE, void_type_node);

  DECL_ARTIFICIAL (lab) = 1;
  DECL_IGNORED_P (lab) = 1;
  DECL_CONTEXT (lab) = current_function_decl;
  return lab;
}

/*  Given a tree, try to return a useful variable name that we can use
    to prefix a temporary that is being assigned the value of the tree.
    I.E. given  <temp> = &A, return A.  */

const char *
get_name (tree t)
{
  tree stripped_decl;

  stripped_decl = t;
  STRIP_NOPS (stripped_decl);
  if (DECL_P (stripped_decl) && DECL_NAME (stripped_decl))
    return IDENTIFIER_POINTER (DECL_NAME (stripped_decl));
  else if (TREE_CODE (stripped_decl) == SSA_NAME)
    {
      tree name = SSA_NAME_IDENTIFIER (stripped_decl);
      if (!name)
	return NULL;
      return IDENTIFIER_POINTER (name);
    }
  else
    {
      switch (TREE_CODE (stripped_decl))
	{
	case ADDR_EXPR:
	  return get_name (TREE_OPERAND (stripped_decl, 0));
	default:
	  return NULL;
	}
    }
}

/* Return true if TYPE has a variable argument list.  */

bool
stdarg_p (const_tree fntype)
{
  function_args_iterator args_iter;
  tree n = NULL_TREE, t;

  if (!fntype)
    return false;

  FOREACH_FUNCTION_ARGS (fntype, t, args_iter)
    {
      n = t;
    }

  return n != NULL_TREE && n != void_type_node;
}

/* Return true if TYPE has a prototype.  */

bool
prototype_p (const_tree fntype)
{
  tree t;

  gcc_assert (fntype != NULL_TREE);

  t = TYPE_ARG_TYPES (fntype);
  return (t != NULL_TREE);
}

/* If BLOCK is inlined from an __attribute__((__artificial__))
   routine, return pointer to location from where it has been
   called.  */
location_t *
block_nonartificial_location (tree block)
{
  location_t *ret = NULL;

  while (block && TREE_CODE (block) == BLOCK
	 && BLOCK_ABSTRACT_ORIGIN (block))
    {
      tree ao = BLOCK_ABSTRACT_ORIGIN (block);

      while (TREE_CODE (ao) == BLOCK
	     && BLOCK_ABSTRACT_ORIGIN (ao)
	     && BLOCK_ABSTRACT_ORIGIN (ao) != ao)
	ao = BLOCK_ABSTRACT_ORIGIN (ao);

      if (TREE_CODE (ao) == FUNCTION_DECL)
	{
	  /* If AO is an artificial inline, point RET to the
	     call site locus at which it has been inlined and continue
	     the loop, in case AO's caller is also an artificial
	     inline.  */
	  if (DECL_DECLARED_INLINE_P (ao)
	      && lookup_attribute ("artificial", DECL_ATTRIBUTES (ao)))
	    ret = &BLOCK_SOURCE_LOCATION (block);
	  else
	    break;
	}
      else if (TREE_CODE (ao) != BLOCK)
	break;

      block = BLOCK_SUPERCONTEXT (block);
    }
  return ret;
}


/* If EXP is inlined from an __attribute__((__artificial__))
   function, return the location of the original call expression.  */

location_t
tree_nonartificial_location (tree exp)
{
  location_t *loc = block_nonartificial_location (TREE_BLOCK (exp));

  if (loc)
    return *loc;
  else
    return EXPR_LOCATION (exp);
}


/* These are the hash table functions for the hash table of OPTIMIZATION_NODEq
   nodes.  */

/* Return the hash code X, an OPTIMIZATION_NODE or TARGET_OPTION code.  */

hashval_t
cl_option_hasher::hash (tree x)
{
  const_tree const t = x;
  const char *p;
  size_t i;
  size_t len = 0;
  hashval_t hash = 0;

  if (TREE_CODE (t) == OPTIMIZATION_NODE)
    {
      p = (const char *)TREE_OPTIMIZATION (t);
      len = sizeof (struct cl_optimization);
    }

  else if (TREE_CODE (t) == TARGET_OPTION_NODE)
    return cl_target_option_hash (TREE_TARGET_OPTION (t));

  else
    gcc_unreachable ();

  /* assume most opt flags are just 0/1, some are 2-3, and a few might be
     something else.  */
  for (i = 0; i < len; i++)
    if (p[i])
      hash = (hash << 4) ^ ((i << 2) | p[i]);

  return hash;
}

/* Return nonzero if the value represented by *X (an OPTIMIZATION or
   TARGET_OPTION tree node) is the same as that given by *Y, which is the
   same.  */

bool
cl_option_hasher::equal (tree x, tree y)
{
  const_tree const xt = x;
  const_tree const yt = y;
  const char *xp;
  const char *yp;
  size_t len;

  if (TREE_CODE (xt) != TREE_CODE (yt))
    return 0;

  if (TREE_CODE (xt) == OPTIMIZATION_NODE)
    {
      xp = (const char *)TREE_OPTIMIZATION (xt);
      yp = (const char *)TREE_OPTIMIZATION (yt);
      len = sizeof (struct cl_optimization);
    }

  else if (TREE_CODE (xt) == TARGET_OPTION_NODE)
    {
      return cl_target_option_eq (TREE_TARGET_OPTION (xt),
				  TREE_TARGET_OPTION (yt));
    }

  else
    gcc_unreachable ();

  return (memcmp (xp, yp, len) == 0);
}

/* Build an OPTIMIZATION_NODE based on the options in OPTS.  */

tree
build_optimization_node (struct gcc_options *opts)
{
  tree t;

  /* Use the cache of optimization nodes.  */

  cl_optimization_save (TREE_OPTIMIZATION (cl_optimization_node),
			opts);

  tree *slot = cl_option_hash_table->find_slot (cl_optimization_node, INSERT);
  t = *slot;
  if (!t)
    {
      /* Insert this one into the hash table.  */
      t = cl_optimization_node;
      *slot = t;

      /* Make a new node for next time round.  */
      cl_optimization_node = make_node (OPTIMIZATION_NODE);
    }

  return t;
}

/* Build a TARGET_OPTION_NODE based on the options in OPTS.  */

tree
build_target_option_node (struct gcc_options *opts)
{
  tree t;

  /* Use the cache of optimization nodes.  */

  cl_target_option_save (TREE_TARGET_OPTION (cl_target_option_node),
			 opts);

  tree *slot = cl_option_hash_table->find_slot (cl_target_option_node, INSERT);
  t = *slot;
  if (!t)
    {
      /* Insert this one into the hash table.  */
      t = cl_target_option_node;
      *slot = t;

      /* Make a new node for next time round.  */
      cl_target_option_node = make_node (TARGET_OPTION_NODE);
    }

  return t;
}

/* Clear TREE_TARGET_GLOBALS of all TARGET_OPTION_NODE trees,
   so that they aren't saved during PCH writing.  */

void
prepare_target_option_nodes_for_pch (void)
{
  hash_table<cl_option_hasher>::iterator iter = cl_option_hash_table->begin ();
  for (; iter != cl_option_hash_table->end (); ++iter)
    if (TREE_CODE (*iter) == TARGET_OPTION_NODE)
      TREE_TARGET_GLOBALS (*iter) = NULL;
}

/* Determine the "ultimate origin" of a block.  The block may be an inlined
   instance of an inlined instance of a block which is local to an inline
   function, so we have to trace all of the way back through the origin chain
   to find out what sort of node actually served as the original seed for the
   given block.  */

tree
block_ultimate_origin (const_tree block)
{
  tree immediate_origin = BLOCK_ABSTRACT_ORIGIN (block);

  /* BLOCK_ABSTRACT_ORIGIN can point to itself; ignore that if
     we're trying to output the abstract instance of this function.  */
  if (BLOCK_ABSTRACT (block) && immediate_origin == block)
    return NULL_TREE;

  if (immediate_origin == NULL_TREE)
    return NULL_TREE;
  else
    {
      tree ret_val;
      tree lookahead = immediate_origin;

      do
	{
	  ret_val = lookahead;
	  lookahead = (TREE_CODE (ret_val) == BLOCK
		       ? BLOCK_ABSTRACT_ORIGIN (ret_val) : NULL);
	}
      while (lookahead != NULL && lookahead != ret_val);

      /* The block's abstract origin chain may not be the *ultimate* origin of
	 the block. It could lead to a DECL that has an abstract origin set.
	 If so, we want that DECL's abstract origin (which is what DECL_ORIGIN
	 will give us if it has one).  Note that DECL's abstract origins are
	 supposed to be the most distant ancestor (or so decl_ultimate_origin
	 claims), so we don't need to loop following the DECL origins.  */
      if (DECL_P (ret_val))
	return DECL_ORIGIN (ret_val);

      return ret_val;
    }
}

/* Return true iff conversion from INNER_TYPE to OUTER_TYPE generates
   no instruction.  */

bool
tree_nop_conversion_p (const_tree outer_type, const_tree inner_type)
{
  /* Do not strip casts into or out of differing address spaces.  */
  if (POINTER_TYPE_P (outer_type)
      && TYPE_ADDR_SPACE (TREE_TYPE (outer_type)) != ADDR_SPACE_GENERIC)
    {
      if (!POINTER_TYPE_P (inner_type)
	  || (TYPE_ADDR_SPACE (TREE_TYPE (outer_type))
	      != TYPE_ADDR_SPACE (TREE_TYPE (inner_type))))
	return false;
    }
  else if (POINTER_TYPE_P (inner_type)
	   && TYPE_ADDR_SPACE (TREE_TYPE (inner_type)) != ADDR_SPACE_GENERIC)
    {
      /* We already know that outer_type is not a pointer with
	 a non-generic address space.  */
      return false;
    }

  /* Use precision rather then machine mode when we can, which gives
     the correct answer even for submode (bit-field) types.  */
  if ((INTEGRAL_TYPE_P (outer_type)
       || POINTER_TYPE_P (outer_type)
       || TREE_CODE (outer_type) == OFFSET_TYPE)
      && (INTEGRAL_TYPE_P (inner_type)
	  || POINTER_TYPE_P (inner_type)
	  || TREE_CODE (inner_type) == OFFSET_TYPE))
    return TYPE_PRECISION (outer_type) == TYPE_PRECISION (inner_type);

  /* Otherwise fall back on comparing machine modes (e.g. for
     aggregate types, floats).  */
  return TYPE_MODE (outer_type) == TYPE_MODE (inner_type);
}

/* Return true iff conversion in EXP generates no instruction.  Mark
   it inline so that we fully inline into the stripping functions even
   though we have two uses of this function.  */

static inline bool
tree_nop_conversion (const_tree exp)
{
  tree outer_type, inner_type;

  if (!CONVERT_EXPR_P (exp)
      && TREE_CODE (exp) != NON_LVALUE_EXPR)
    return false;
  if (TREE_OPERAND (exp, 0) == error_mark_node)
    return false;

  outer_type = TREE_TYPE (exp);
  inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));

  if (!inner_type)
    return false;

  return tree_nop_conversion_p (outer_type, inner_type);
}

/* Return true iff conversion in EXP generates no instruction.  Don't
   consider conversions changing the signedness.  */

static bool
tree_sign_nop_conversion (const_tree exp)
{
  tree outer_type, inner_type;

  if (!tree_nop_conversion (exp))
    return false;

  outer_type = TREE_TYPE (exp);
  inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));

  return (TYPE_UNSIGNED (outer_type) == TYPE_UNSIGNED (inner_type)
	  && POINTER_TYPE_P (outer_type) == POINTER_TYPE_P (inner_type));
}

/* Strip conversions from EXP according to tree_nop_conversion and
   return the resulting expression.  */

tree
tree_strip_nop_conversions (tree exp)
{
  while (tree_nop_conversion (exp))
    exp = TREE_OPERAND (exp, 0);
  return exp;
}

/* Strip conversions from EXP according to tree_sign_nop_conversion
   and return the resulting expression.  */

tree
tree_strip_sign_nop_conversions (tree exp)
{
  while (tree_sign_nop_conversion (exp))
    exp = TREE_OPERAND (exp, 0);
  return exp;
}

/* Avoid any floating point extensions from EXP.  */
tree
strip_float_extensions (tree exp)
{
  tree sub, expt, subt;

  /*  For floating point constant look up the narrowest type that can hold
      it properly and handle it like (type)(narrowest_type)constant.
      This way we can optimize for instance a=a*2.0 where "a" is float
      but 2.0 is double constant.  */
  if (TREE_CODE (exp) == REAL_CST && !DECIMAL_FLOAT_TYPE_P (TREE_TYPE (exp)))
    {
      REAL_VALUE_TYPE orig;
      tree type = NULL;

      orig = TREE_REAL_CST (exp);
      if (TYPE_PRECISION (TREE_TYPE (exp)) > TYPE_PRECISION (float_type_node)
	  && exact_real_truncate (TYPE_MODE (float_type_node), &orig))
	type = float_type_node;
      else if (TYPE_PRECISION (TREE_TYPE (exp))
	       > TYPE_PRECISION (double_type_node)
	       && exact_real_truncate (TYPE_MODE (double_type_node), &orig))
	type = double_type_node;
      if (type)
	return build_real_truncate (type, orig);
    }

  if (!CONVERT_EXPR_P (exp))
    return exp;

  sub = TREE_OPERAND (exp, 0);
  subt = TREE_TYPE (sub);
  expt = TREE_TYPE (exp);

  if (!FLOAT_TYPE_P (subt))
    return exp;

  if (DECIMAL_FLOAT_TYPE_P (expt) != DECIMAL_FLOAT_TYPE_P (subt))
    return exp;

  if (TYPE_PRECISION (subt) > TYPE_PRECISION (expt))
    return exp;

  return strip_float_extensions (sub);
}

/* Strip out all handled components that produce invariant
   offsets.  */

const_tree
strip_invariant_refs (const_tree op)
{
  while (handled_component_p (op))
    {
      switch (TREE_CODE (op))
	{
	case ARRAY_REF:
	case ARRAY_RANGE_REF:
	  if (!is_gimple_constant (TREE_OPERAND (op, 1))
	      || TREE_OPERAND (op, 2) != NULL_TREE
	      || TREE_OPERAND (op, 3) != NULL_TREE)
	    return NULL;
	  break;

	case COMPONENT_REF:
	  if (TREE_OPERAND (op, 2) != NULL_TREE)
	    return NULL;
	  break;

	default:;
	}
      op = TREE_OPERAND (op, 0);
    }

  return op;
}

static GTY(()) tree gcc_eh_personality_decl;

/* Return the GCC personality function decl.  */

tree
lhd_gcc_personality (void)
{
  if (!gcc_eh_personality_decl)
    gcc_eh_personality_decl = build_personality_function ("gcc");
  return gcc_eh_personality_decl;
}

/* TARGET is a call target of GIMPLE call statement
   (obtained by gimple_call_fn).  Return true if it is
   OBJ_TYPE_REF representing an virtual call of C++ method.
   (As opposed to OBJ_TYPE_REF representing objc calls
   through a cast where middle-end devirtualization machinery
   can't apply.) */

bool
virtual_method_call_p (const_tree target)
{
  if (TREE_CODE (target) != OBJ_TYPE_REF)
    return false;
  tree t = TREE_TYPE (target);
  gcc_checking_assert (TREE_CODE (t) == POINTER_TYPE);
  t = TREE_TYPE (t);
  if (TREE_CODE (t) == FUNCTION_TYPE)
    return false;
  gcc_checking_assert (TREE_CODE (t) == METHOD_TYPE);
  /* If we do not have BINFO associated, it means that type was built
     without devirtualization enabled.  Do not consider this a virtual
     call.  */
  if (!TYPE_BINFO (obj_type_ref_class (target)))
    return false;
  return true;
}

/* REF is OBJ_TYPE_REF, return the class the ref corresponds to.  */

tree
obj_type_ref_class (const_tree ref)
{
  gcc_checking_assert (TREE_CODE (ref) == OBJ_TYPE_REF);
  ref = TREE_TYPE (ref);
  gcc_checking_assert (TREE_CODE (ref) == POINTER_TYPE);
  ref = TREE_TYPE (ref);
  /* We look for type THIS points to.  ObjC also builds
     OBJ_TYPE_REF with non-method calls, Their first parameter
     ID however also corresponds to class type. */
  gcc_checking_assert (TREE_CODE (ref) == METHOD_TYPE
		       || TREE_CODE (ref) == FUNCTION_TYPE);
  ref = TREE_VALUE (TYPE_ARG_TYPES (ref));
  gcc_checking_assert (TREE_CODE (ref) == POINTER_TYPE);
  return TREE_TYPE (ref);
}

/* Lookup sub-BINFO of BINFO of TYPE at offset POS.  */

static tree
lookup_binfo_at_offset (tree binfo, tree type, HOST_WIDE_INT pos)
{
  unsigned int i;
  tree base_binfo, b;

  for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
    if (pos == tree_to_shwi (BINFO_OFFSET (base_binfo))
	&& types_same_for_odr (TREE_TYPE (base_binfo), type))
      return base_binfo;
    else if ((b = lookup_binfo_at_offset (base_binfo, type, pos)) != NULL)
      return b;
  return NULL;
}

/* Try to find a base info of BINFO that would have its field decl at offset
   OFFSET within the BINFO type and which is of EXPECTED_TYPE.  If it can be
   found, return, otherwise return NULL_TREE.  */

tree
get_binfo_at_offset (tree binfo, HOST_WIDE_INT offset, tree expected_type)
{
  tree type = BINFO_TYPE (binfo);

  while (true)
    {
      HOST_WIDE_INT pos, size;
      tree fld;
      int i;

      if (types_same_for_odr (type, expected_type))
	  return binfo;
      if (offset < 0)
	return NULL_TREE;

      for (fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	{
	  if (TREE_CODE (fld) != FIELD_DECL || !DECL_ARTIFICIAL (fld))
	    continue;

	  pos = int_bit_position (fld);
	  size = tree_to_uhwi (DECL_SIZE (fld));
	  if (pos <= offset && (pos + size) > offset)
	    break;
	}
      if (!fld || TREE_CODE (TREE_TYPE (fld)) != RECORD_TYPE)
	return NULL_TREE;

      /* Offset 0 indicates the primary base, whose vtable contents are
	 represented in the binfo for the derived class.  */
      else if (offset != 0)
	{
	  tree found_binfo = NULL, base_binfo;
	  /* Offsets in BINFO are in bytes relative to the whole structure
	     while POS is in bits relative to the containing field.  */
	  int binfo_offset = (tree_to_shwi (BINFO_OFFSET (binfo)) + pos
			     / BITS_PER_UNIT);

	  for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); i++)
	    if (tree_to_shwi (BINFO_OFFSET (base_binfo)) == binfo_offset
		&& types_same_for_odr (TREE_TYPE (base_binfo), TREE_TYPE (fld)))
	      {
		found_binfo = base_binfo;
		break;
	      }
	  if (found_binfo)
	    binfo = found_binfo;
	  else
	    binfo = lookup_binfo_at_offset (binfo, TREE_TYPE (fld),
					    binfo_offset);
	 }

      type = TREE_TYPE (fld);
      offset -= pos;
    }
}

/* Returns true if X is a typedef decl.  */

bool
is_typedef_decl (const_tree x)
{
  return (x && TREE_CODE (x) == TYPE_DECL
          && DECL_ORIGINAL_TYPE (x) != NULL_TREE);
}

/* Returns true iff TYPE is a type variant created for a typedef. */

bool
typedef_variant_p (const_tree type)
{
  return is_typedef_decl (TYPE_NAME (type));
}

/* Warn about a use of an identifier which was marked deprecated.  */
void
warn_deprecated_use (tree node, tree attr)
{
  const char *msg;

  if (node == 0 || !warn_deprecated_decl)
    return;

  if (!attr)
    {
      if (DECL_P (node))
	attr = DECL_ATTRIBUTES (node);
      else if (TYPE_P (node))
	{
	  tree decl = TYPE_STUB_DECL (node);
	  if (decl)
	    attr = lookup_attribute ("deprecated",
				     TYPE_ATTRIBUTES (TREE_TYPE (decl)));
	}
    }

  if (attr)
    attr = lookup_attribute ("deprecated", attr);

  if (attr)
    msg = TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (attr)));
  else
    msg = NULL;

  bool w;
  if (DECL_P (node))
    {
      if (msg)
	w = warning (OPT_Wdeprecated_declarations,
		     "%qD is deprecated: %s", node, msg);
      else
	w = warning (OPT_Wdeprecated_declarations,
		     "%qD is deprecated", node);
      if (w)
	inform (DECL_SOURCE_LOCATION (node), "declared here");
    }
  else if (TYPE_P (node))
    {
      tree what = NULL_TREE;
      tree decl = TYPE_STUB_DECL (node);

      if (TYPE_NAME (node))
	{
	  if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
	    what = TYPE_NAME (node);
	  else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
		   && DECL_NAME (TYPE_NAME (node)))
	    what = DECL_NAME (TYPE_NAME (node));
	}

      if (decl)
	{
	  if (what)
	    {
	      if (msg)
		w = warning (OPT_Wdeprecated_declarations,
			     "%qE is deprecated: %s", what, msg);
	      else
		w = warning (OPT_Wdeprecated_declarations,
			     "%qE is deprecated", what);
	    }
	  else
	    {
	      if (msg)
		w = warning (OPT_Wdeprecated_declarations,
			     "type is deprecated: %s", msg);
	      else
		w = warning (OPT_Wdeprecated_declarations,
			     "type is deprecated");
	    }
	  if (w)
	    inform (DECL_SOURCE_LOCATION (decl), "declared here");
	}
      else
	{
	  if (what)
	    {
	      if (msg)
		warning (OPT_Wdeprecated_declarations, "%qE is deprecated: %s",
			 what, msg);
	      else
		warning (OPT_Wdeprecated_declarations, "%qE is deprecated", what);
	    }
	  else
	    {
	      if (msg)
		warning (OPT_Wdeprecated_declarations, "type is deprecated: %s",
			 msg);
	      else
		warning (OPT_Wdeprecated_declarations, "type is deprecated");
	    }
	}
    }
}

/* Return true if REF has a COMPONENT_REF with a bit-field field declaration
   somewhere in it.  */

bool
contains_bitfld_component_ref_p (const_tree ref)
{
  while (handled_component_p (ref))
    {
      if (TREE_CODE (ref) == COMPONENT_REF
          && DECL_BIT_FIELD (TREE_OPERAND (ref, 1)))
        return true;
      ref = TREE_OPERAND (ref, 0);
    }

  return false;
}

/* Try to determine whether a TRY_CATCH expression can fall through.
   This is a subroutine of block_may_fallthru.  */

static bool
try_catch_may_fallthru (const_tree stmt)
{
  tree_stmt_iterator i;

  /* If the TRY block can fall through, the whole TRY_CATCH can
     fall through.  */
  if (block_may_fallthru (TREE_OPERAND (stmt, 0)))
    return true;

  i = tsi_start (TREE_OPERAND (stmt, 1));
  switch (TREE_CODE (tsi_stmt (i)))
    {
    case CATCH_EXPR:
      /* We expect to see a sequence of CATCH_EXPR trees, each with a
	 catch expression and a body.  The whole TRY_CATCH may fall
	 through iff any of the catch bodies falls through.  */
      for (; !tsi_end_p (i); tsi_next (&i))
	{
	  if (block_may_fallthru (CATCH_BODY (tsi_stmt (i))))
	    return true;
	}
      return false;

    case EH_FILTER_EXPR:
      /* The exception filter expression only matters if there is an
	 exception.  If the exception does not match EH_FILTER_TYPES,
	 we will execute EH_FILTER_FAILURE, and we will fall through
	 if that falls through.  If the exception does match
	 EH_FILTER_TYPES, the stack unwinder will continue up the
	 stack, so we will not fall through.  We don't know whether we
	 will throw an exception which matches EH_FILTER_TYPES or not,
	 so we just ignore EH_FILTER_TYPES and assume that we might
	 throw an exception which doesn't match.  */
      return block_may_fallthru (EH_FILTER_FAILURE (tsi_stmt (i)));

    default:
      /* This case represents statements to be executed when an
	 exception occurs.  Those statements are implicitly followed
	 by a RESX statement to resume execution after the exception.
	 So in this case the TRY_CATCH never falls through.  */
      return false;
    }
}

/* Try to determine if we can fall out of the bottom of BLOCK.  This guess
   need not be 100% accurate; simply be conservative and return true if we
   don't know.  This is used only to avoid stupidly generating extra code.
   If we're wrong, we'll just delete the extra code later.  */

bool
block_may_fallthru (const_tree block)
{
  /* This CONST_CAST is okay because expr_last returns its argument
     unmodified and we assign it to a const_tree.  */
  const_tree stmt = expr_last (CONST_CAST_TREE (block));

  switch (stmt ? TREE_CODE (stmt) : ERROR_MARK)
    {
    case GOTO_EXPR:
    case RETURN_EXPR:
      /* Easy cases.  If the last statement of the block implies
	 control transfer, then we can't fall through.  */
      return false;

    case SWITCH_EXPR:
      /* If SWITCH_LABELS is set, this is lowered, and represents a
	 branch to a selected label and hence can not fall through.
	 Otherwise SWITCH_BODY is set, and the switch can fall
	 through.  */
      return SWITCH_LABELS (stmt) == NULL_TREE;

    case COND_EXPR:
      if (block_may_fallthru (COND_EXPR_THEN (stmt)))
	return true;
      return block_may_fallthru (COND_EXPR_ELSE (stmt));

    case BIND_EXPR:
      return block_may_fallthru (BIND_EXPR_BODY (stmt));

    case TRY_CATCH_EXPR:
      return try_catch_may_fallthru (stmt);

    case TRY_FINALLY_EXPR:
      /* The finally clause is always executed after the try clause,
	 so if it does not fall through, then the try-finally will not
	 fall through.  Otherwise, if the try clause does not fall
	 through, then when the finally clause falls through it will
	 resume execution wherever the try clause was going.  So the
	 whole try-finally will only fall through if both the try
	 clause and the finally clause fall through.  */
      return (block_may_fallthru (TREE_OPERAND (stmt, 0))
	      && block_may_fallthru (TREE_OPERAND (stmt, 1)));

    case MODIFY_EXPR:
      if (TREE_CODE (TREE_OPERAND (stmt, 1)) == CALL_EXPR)
	stmt = TREE_OPERAND (stmt, 1);
      else
	return true;
      /* FALLTHRU */

    case CALL_EXPR:
      /* Functions that do not return do not fall through.  */
      return (call_expr_flags (stmt) & ECF_NORETURN) == 0;

    case CLEANUP_POINT_EXPR:
      return block_may_fallthru (TREE_OPERAND (stmt, 0));

    case TARGET_EXPR:
      return block_may_fallthru (TREE_OPERAND (stmt, 1));

    case ERROR_MARK:
      return true;

    default:
      return lang_hooks.block_may_fallthru (stmt);
    }
}

/* True if we are using EH to handle cleanups.  */
static bool using_eh_for_cleanups_flag = false;

/* This routine is called from front ends to indicate eh should be used for
   cleanups.  */
void
using_eh_for_cleanups (void)
{
  using_eh_for_cleanups_flag = true;
}

/* Query whether EH is used for cleanups.  */
bool
using_eh_for_cleanups_p (void)
{
  return using_eh_for_cleanups_flag;
}

/* Wrapper for tree_code_name to ensure that tree code is valid */
const char *
get_tree_code_name (enum tree_code code)
{
  const char *invalid = "<invalid tree code>";

  if (code >= MAX_TREE_CODES)
    return invalid;

  return tree_code_name[code];
}

/* Drops the TREE_OVERFLOW flag from T.  */

tree
drop_tree_overflow (tree t)
{
  gcc_checking_assert (TREE_OVERFLOW (t));

  /* For tree codes with a sharing machinery re-build the result.  */
  if (TREE_CODE (t) == INTEGER_CST)
    return wide_int_to_tree (TREE_TYPE (t), t);

  /* Otherwise, as all tcc_constants are possibly shared, copy the node
     and drop the flag.  */
  t = copy_node (t);
  TREE_OVERFLOW (t) = 0;
  return t;
}

/* Given a memory reference expression T, return its base address.
   The base address of a memory reference expression is the main
   object being referenced.  For instance, the base address for
   'array[i].fld[j]' is 'array'.  You can think of this as stripping
   away the offset part from a memory address.

   This function calls handled_component_p to strip away all the inner
   parts of the memory reference until it reaches the base object.  */

tree
get_base_address (tree t)
{
  while (handled_component_p (t))
    t = TREE_OPERAND (t, 0);

  if ((TREE_CODE (t) == MEM_REF
       || TREE_CODE (t) == TARGET_MEM_REF)
      && TREE_CODE (TREE_OPERAND (t, 0)) == ADDR_EXPR)
    t = TREE_OPERAND (TREE_OPERAND (t, 0), 0);

  /* ???  Either the alias oracle or all callers need to properly deal
     with WITH_SIZE_EXPRs before we can look through those.  */
  if (TREE_CODE (t) == WITH_SIZE_EXPR)
    return NULL_TREE;

  return t;
}

/* Return a tree of sizetype representing the size, in bytes, of the element
   of EXP, an ARRAY_REF or an ARRAY_RANGE_REF.  */

tree
array_ref_element_size (tree exp)
{
  tree aligned_size = TREE_OPERAND (exp, 3);
  tree elmt_type = TREE_TYPE (TREE_TYPE (TREE_OPERAND (exp, 0)));
  location_t loc = EXPR_LOCATION (exp);

  /* If a size was specified in the ARRAY_REF, it's the size measured
     in alignment units of the element type.  So multiply by that value.  */
  if (aligned_size)
    {
      /* ??? tree_ssa_useless_type_conversion will eliminate casts to
	 sizetype from another type of the same width and signedness.  */
      if (TREE_TYPE (aligned_size) != sizetype)
	aligned_size = fold_convert_loc (loc, sizetype, aligned_size);
      return size_binop_loc (loc, MULT_EXPR, aligned_size,
			     size_int (TYPE_ALIGN_UNIT (elmt_type)));
    }

  /* Otherwise, take the size from that of the element type.  Substitute
     any PLACEHOLDER_EXPR that we have.  */
  else
    return SUBSTITUTE_PLACEHOLDER_IN_EXPR (TYPE_SIZE_UNIT (elmt_type), exp);
}

/* Return a tree representing the lower bound of the array mentioned in
   EXP, an ARRAY_REF or an ARRAY_RANGE_REF.  */

tree
array_ref_low_bound (tree exp)
{
  tree domain_type = TYPE_DOMAIN (TREE_TYPE (TREE_OPERAND (exp, 0)));

  /* If a lower bound is specified in EXP, use it.  */
  if (TREE_OPERAND (exp, 2))
    return TREE_OPERAND (exp, 2);

  /* Otherwise, if there is a domain type and it has a lower bound, use it,
     substituting for a PLACEHOLDER_EXPR as needed.  */
  if (domain_type && TYPE_MIN_VALUE (domain_type))
    return SUBSTITUTE_PLACEHOLDER_IN_EXPR (TYPE_MIN_VALUE (domain_type), exp);

  /* Otherwise, return a zero of the appropriate type.  */
  return build_int_cst (TREE_TYPE (TREE_OPERAND (exp, 1)), 0);
}

/* Return a tree representing the upper bound of the array mentioned in
   EXP, an ARRAY_REF or an ARRAY_RANGE_REF.  */

tree
array_ref_up_bound (tree exp)
{
  tree domain_type = TYPE_DOMAIN (TREE_TYPE (TREE_OPERAND (exp, 0)));

  /* If there is a domain type and it has an upper bound, use it, substituting
     for a PLACEHOLDER_EXPR as needed.  */
  if (domain_type && TYPE_MAX_VALUE (domain_type))
    return SUBSTITUTE_PLACEHOLDER_IN_EXPR (TYPE_MAX_VALUE (domain_type), exp);

  /* Otherwise fail.  */
  return NULL_TREE;
}

/* Returns true if REF is an array reference to an array at the end of
   a structure.  If this is the case, the array may be allocated larger
   than its upper bound implies.  */

bool
array_at_struct_end_p (tree ref)
{
  if (TREE_CODE (ref) != ARRAY_REF
      && TREE_CODE (ref) != ARRAY_RANGE_REF)
    return false;

  while (handled_component_p (ref))
    {
      /* If the reference chain contains a component reference to a
         non-union type and there follows another field the reference
	 is not at the end of a structure.  */
      if (TREE_CODE (ref) == COMPONENT_REF
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (ref, 0))) == RECORD_TYPE)
	{
	  tree nextf = DECL_CHAIN (TREE_OPERAND (ref, 1));
	  while (nextf && TREE_CODE (nextf) != FIELD_DECL)
	    nextf = DECL_CHAIN (nextf);
	  if (nextf)
	    return false;
	}

      ref = TREE_OPERAND (ref, 0);
    }

  tree size = NULL;

  if (TREE_CODE (ref) == MEM_REF
      && TREE_CODE (TREE_OPERAND (ref, 0)) == ADDR_EXPR)
    {
      size = TYPE_SIZE (TREE_TYPE (ref));
      ref = TREE_OPERAND (TREE_OPERAND (ref, 0), 0);
    }

  /* If the reference is based on a declared entity, the size of the array
     is constrained by its given domain.  (Do not trust commons PR/69368).  */
  if (DECL_P (ref)
      /* Be sure the size of MEM_REF target match.  For example:

	   char buf[10];
	   struct foo *str = (struct foo *)&buf;

	   str->trailin_array[2] = 1;

	 is valid because BUF allocate enough space.  */

      && (!size || (DECL_SIZE (ref) != NULL
		    && operand_equal_p (DECL_SIZE (ref), size, 0)))
      && !(flag_unconstrained_commons
	   && TREE_CODE (ref) == VAR_DECL && DECL_COMMON (ref)))
    return false;

  return true;
}

/* Return a tree representing the offset, in bytes, of the field referenced
   by EXP.  This does not include any offset in DECL_FIELD_BIT_OFFSET.  */

tree
component_ref_field_offset (tree exp)
{
  tree aligned_offset = TREE_OPERAND (exp, 2);
  tree field = TREE_OPERAND (exp, 1);
  location_t loc = EXPR_LOCATION (exp);

  /* If an offset was specified in the COMPONENT_REF, it's the offset measured
     in units of DECL_OFFSET_ALIGN / BITS_PER_UNIT.  So multiply by that
     value.  */
  if (aligned_offset)
    {
      /* ??? tree_ssa_useless_type_conversion will eliminate casts to
	 sizetype from another type of the same width and signedness.  */
      if (TREE_TYPE (aligned_offset) != sizetype)
	aligned_offset = fold_convert_loc (loc, sizetype, aligned_offset);
      return size_binop_loc (loc, MULT_EXPR, aligned_offset,
			     size_int (DECL_OFFSET_ALIGN (field)
				       / BITS_PER_UNIT));
    }

  /* Otherwise, take the offset from that of the field.  Substitute
     any PLACEHOLDER_EXPR that we have.  */
  else
    return SUBSTITUTE_PLACEHOLDER_IN_EXPR (DECL_FIELD_OFFSET (field), exp);
}

/* Return the machine mode of T.  For vectors, returns the mode of the
   inner type.  The main use case is to feed the result to HONOR_NANS,
   avoiding the BLKmode that a direct TYPE_MODE (T) might return.  */

machine_mode
element_mode (const_tree t)
{
  if (!TYPE_P (t))
    t = TREE_TYPE (t);
  if (VECTOR_TYPE_P (t) || TREE_CODE (t) == COMPLEX_TYPE)
    t = TREE_TYPE (t);
  return TYPE_MODE (t);
}
 

/* Veirfy that basic properties of T match TV and thus T can be a variant of
   TV.  TV should be the more specified variant (i.e. the main variant).  */

static bool
verify_type_variant (const_tree t, tree tv)
{
  /* Type variant can differ by:

     - TYPE_QUALS: TYPE_READONLY, TYPE_VOLATILE, TYPE_ATOMIC, TYPE_RESTRICT,
                   ENCODE_QUAL_ADDR_SPACE. 
     - main variant may be TYPE_COMPLETE_P and variant types !TYPE_COMPLETE_P
       in this case some values may not be set in the variant types
       (see TYPE_COMPLETE_P checks).
     - it is possible to have TYPE_ARTIFICIAL variant of non-artifical type
     - by TYPE_NAME and attributes (i.e. when variant originate by typedef)
     - TYPE_CANONICAL (TYPE_ALIAS_SET is the same among variants)
     - by the alignment: TYPE_ALIGN and TYPE_USER_ALIGN
     - during LTO by TYPE_CONTEXT if type is TYPE_FILE_SCOPE_P
       this is necessary to make it possible to merge types form different TUs
     - arrays, pointers and references may have TREE_TYPE that is a variant
       of TREE_TYPE of their main variants.
     - aggregates may have new TYPE_FIELDS list that list variants of
       the main variant TYPE_FIELDS.
     - vector types may differ by TYPE_VECTOR_OPAQUE
     - TYPE_METHODS is always NULL for vairant types and maintained for
       main variant only.
   */

  /* Convenience macro for matching individual fields.  */
#define verify_variant_match(flag)					    \
  do {									    \
    if (flag (tv) != flag (t))						    \
      {									    \
	error ("type variant differs by " #flag ".");			    \
	debug_tree (tv);						    \
	return false;							    \
      }									    \
  } while (false)

  /* tree_base checks.  */

  verify_variant_match (TREE_CODE);
  /* FIXME: Ada builds non-artificial variants of artificial types.  */
  if (TYPE_ARTIFICIAL (tv) && 0)
    verify_variant_match (TYPE_ARTIFICIAL);
  if (POINTER_TYPE_P (tv))
    verify_variant_match (TYPE_REF_CAN_ALIAS_ALL);
  /* FIXME: TYPE_SIZES_GIMPLIFIED may differs for Ada build.  */
  verify_variant_match (TYPE_UNSIGNED);
  verify_variant_match (TYPE_ALIGN_OK);
  verify_variant_match (TYPE_PACKED);
  if (TREE_CODE (t) == REFERENCE_TYPE)
    verify_variant_match (TYPE_REF_IS_RVALUE);
  if (AGGREGATE_TYPE_P (t))
    verify_variant_match (TYPE_REVERSE_STORAGE_ORDER);
  else
    verify_variant_match (TYPE_SATURATING);
  /* FIXME: This check trigger during libstdc++ build.  */
  if (RECORD_OR_UNION_TYPE_P (t) && COMPLETE_TYPE_P (t) && 0)
    verify_variant_match (TYPE_FINAL_P);

  /* tree_type_common checks.  */

  if (COMPLETE_TYPE_P (t))
    {
      verify_variant_match (TYPE_SIZE);
      verify_variant_match (TYPE_MODE);
      if (TYPE_SIZE_UNIT (t) != TYPE_SIZE_UNIT (tv)
	  /* FIXME: ideally we should compare pointer equality, but java FE
	     produce variants where size is INTEGER_CST of different type (int
	     wrt size_type) during libjava biuld.  */
	  && !operand_equal_p (TYPE_SIZE_UNIT (t), TYPE_SIZE_UNIT (tv), 0))
	{
	  error ("type variant has different TYPE_SIZE_UNIT");
	  debug_tree (tv);
	  error ("type variant's TYPE_SIZE_UNIT");
	  debug_tree (TYPE_SIZE_UNIT (tv));
	  error ("type's TYPE_SIZE_UNIT");
	  debug_tree (TYPE_SIZE_UNIT (t));
	  return false;
	}
    }
  verify_variant_match (TYPE_PRECISION);
  verify_variant_match (TYPE_NEEDS_CONSTRUCTING);
  if (RECORD_OR_UNION_TYPE_P (t))
    verify_variant_match (TYPE_TRANSPARENT_AGGR);
  else if (TREE_CODE (t) == ARRAY_TYPE)
    verify_variant_match (TYPE_NONALIASED_COMPONENT);
  /* During LTO we merge variant lists from diferent translation units
     that may differ BY TYPE_CONTEXT that in turn may point 
     to TRANSLATION_UNIT_DECL.
     Ada also builds variants of types with different TYPE_CONTEXT.   */
  if ((!in_lto_p || !TYPE_FILE_SCOPE_P (t)) && 0)
    verify_variant_match (TYPE_CONTEXT);
  verify_variant_match (TYPE_STRING_FLAG);
  if (TYPE_ALIAS_SET_KNOWN_P (t))
    {
      error ("type variant with TYPE_ALIAS_SET_KNOWN_P");
      debug_tree (tv);
      return false;
    }

  /* tree_type_non_common checks.  */

  /* FIXME: C FE uses TYPE_VFIELD to record C_TYPE_INCOMPLETE_VARS
     and dangle the pointer from time to time.  */
  if (RECORD_OR_UNION_TYPE_P (t) && TYPE_VFIELD (t) != TYPE_VFIELD (tv)
      && (in_lto_p || !TYPE_VFIELD (tv)
	  || TREE_CODE (TYPE_VFIELD (tv)) != TREE_LIST))
    {
      error ("type variant has different TYPE_VFIELD");
      debug_tree (tv);
      return false;
    }
  if ((TREE_CODE (t) == ENUMERAL_TYPE && COMPLETE_TYPE_P (t))
       || TREE_CODE (t) == INTEGER_TYPE
       || TREE_CODE (t) == BOOLEAN_TYPE
       || TREE_CODE (t) == REAL_TYPE
       || TREE_CODE (t) == FIXED_POINT_TYPE)
    {
      verify_variant_match (TYPE_MAX_VALUE);
      verify_variant_match (TYPE_MIN_VALUE);
    }
  if (TREE_CODE (t) == METHOD_TYPE)
    verify_variant_match (TYPE_METHOD_BASETYPE);
  if (RECORD_OR_UNION_TYPE_P (t) && TYPE_METHODS (t))
    {
      error ("type variant has TYPE_METHODS");
      debug_tree (tv);
      return false;
    }
  if (TREE_CODE (t) == OFFSET_TYPE)
    verify_variant_match (TYPE_OFFSET_BASETYPE);
  if (TREE_CODE (t) == ARRAY_TYPE)
    verify_variant_match (TYPE_ARRAY_MAX_SIZE);
  /* FIXME: Be lax and allow TYPE_BINFO to be missing in variant types
     or even type's main variant.  This is needed to make bootstrap pass
     and the bug seems new in GCC 5.
     C++ FE should be updated to make this consistent and we should check
     that TYPE_BINFO is always NULL for !COMPLETE_TYPE_P and otherwise there
     is a match with main variant.

     Also disable the check for Java for now because of parser hack that builds
     first an dummy BINFO and then sometimes replace it by real BINFO in some
     of the copies.  */
  if (RECORD_OR_UNION_TYPE_P (t) && TYPE_BINFO (t) && TYPE_BINFO (tv)
      && TYPE_BINFO (t) != TYPE_BINFO (tv)
      /* FIXME: Java sometimes keep dump TYPE_BINFOs on variant types.
	 Since there is no cheap way to tell C++/Java type w/o LTO, do checking
	 at LTO time only.  */
      && (in_lto_p && odr_type_p (t)))
    {
      error ("type variant has different TYPE_BINFO");
      debug_tree (tv);
      error ("type variant's TYPE_BINFO");
      debug_tree (TYPE_BINFO (tv));
      error ("type's TYPE_BINFO");
      debug_tree (TYPE_BINFO (t));
      return false;
    }

  /* Check various uses of TYPE_VALUES_RAW.  */
  if (TREE_CODE (t) == ENUMERAL_TYPE)
    verify_variant_match (TYPE_VALUES);
  else if (TREE_CODE (t) == ARRAY_TYPE)
    verify_variant_match (TYPE_DOMAIN);
  /* Permit incomplete variants of complete type.  While FEs may complete
     all variants, this does not happen for C++ templates in all cases.  */
  else if (RECORD_OR_UNION_TYPE_P (t)
	   && COMPLETE_TYPE_P (t)
	   && TYPE_FIELDS (t) != TYPE_FIELDS (tv))
    {
      tree f1, f2;

      /* Fortran builds qualified variants as new records with items of
	 qualified type. Verify that they looks same.  */
      for (f1 = TYPE_FIELDS (t), f2 = TYPE_FIELDS (tv);
	   f1 && f2;
	   f1 = TREE_CHAIN (f1), f2 = TREE_CHAIN (f2))
	if (TREE_CODE (f1) != FIELD_DECL || TREE_CODE (f2) != FIELD_DECL
	    || (TYPE_MAIN_VARIANT (TREE_TYPE (f1))
		 != TYPE_MAIN_VARIANT (TREE_TYPE (f2))
		/* FIXME: gfc_nonrestricted_type builds all types as variants
		   with exception of pointer types.  It deeply copies the type
		   which means that we may end up with a variant type
		   referring non-variant pointer.  We may change it to
		   produce types as variants, too, like
		   objc_get_protocol_qualified_type does.  */
		&& !POINTER_TYPE_P (TREE_TYPE (f1)))
	    || DECL_FIELD_OFFSET (f1) != DECL_FIELD_OFFSET (f2)
	    || DECL_FIELD_BIT_OFFSET (f1) != DECL_FIELD_BIT_OFFSET (f2))
	  break;
      if (f1 || f2)
	{
	  error ("type variant has different TYPE_FIELDS");
	  debug_tree (tv);
	  error ("first mismatch is field");
	  debug_tree (f1);
	  error ("and field");
	  debug_tree (f2);
          return false;
	}
    }
  else if ((TREE_CODE (t) == FUNCTION_TYPE || TREE_CODE (t) == METHOD_TYPE))
    verify_variant_match (TYPE_ARG_TYPES);
  /* For C++ the qualified variant of array type is really an array type
     of qualified TREE_TYPE.
     objc builds variants of pointer where pointer to type is a variant, too
     in objc_get_protocol_qualified_type.  */
  if (TREE_TYPE (t) != TREE_TYPE (tv)
      && ((TREE_CODE (t) != ARRAY_TYPE
	   && !POINTER_TYPE_P (t))
	  || TYPE_MAIN_VARIANT (TREE_TYPE (t))
	     != TYPE_MAIN_VARIANT (TREE_TYPE (tv))))
    {
      error ("type variant has different TREE_TYPE");
      debug_tree (tv);
      error ("type variant's TREE_TYPE");
      debug_tree (TREE_TYPE (tv));
      error ("type's TREE_TYPE");
      debug_tree (TREE_TYPE (t));
      return false;
    }
  if (type_with_alias_set_p (t)
      && !gimple_canonical_types_compatible_p (t, tv, false))
    {
      error ("type is not compatible with its vairant");
      debug_tree (tv);
      error ("type variant's TREE_TYPE");
      debug_tree (TREE_TYPE (tv));
      error ("type's TREE_TYPE");
      debug_tree (TREE_TYPE (t));
      return false;
    }
  return true;
#undef verify_variant_match
}


/* The TYPE_CANONICAL merging machinery.  It should closely resemble
   the middle-end types_compatible_p function.  It needs to avoid
   claiming types are different for types that should be treated
   the same with respect to TBAA.  Canonical types are also used
   for IL consistency checks via the useless_type_conversion_p
   predicate which does not handle all type kinds itself but falls
   back to pointer-comparison of TYPE_CANONICAL for aggregates
   for example.  */

/* Return true if TYPE_UNSIGNED of TYPE should be ignored for canonical
   type calculation because we need to allow inter-operability between signed
   and unsigned variants.  */

bool
type_with_interoperable_signedness (const_tree type)
{
  /* Fortran standard require C_SIGNED_CHAR to be interoperable with both
     signed char and unsigned char.  Similarly fortran FE builds
     C_SIZE_T as signed type, while C defines it unsigned.  */

  return tree_code_for_canonical_type_merging (TREE_CODE (type))
	   == INTEGER_TYPE
         && (TYPE_PRECISION (type) == TYPE_PRECISION (signed_char_type_node)
	     || TYPE_PRECISION (type) == TYPE_PRECISION (size_type_node));
}

/* Return true iff T1 and T2 are structurally identical for what
   TBAA is concerned.  
   This function is used both by lto.c canonical type merging and by the
   verifier.  If TRUST_TYPE_CANONICAL we do not look into structure of types
   that have TYPE_CANONICAL defined and assume them equivalent.  This is useful
   only for LTO because only in these cases TYPE_CANONICAL equivalence
   correspond to one defined by gimple_canonical_types_compatible_p.  */

bool
gimple_canonical_types_compatible_p (const_tree t1, const_tree t2,
				     bool trust_type_canonical)
{
  /* Type variants should be same as the main variant.  When not doing sanity
     checking to verify this fact, go to main variants and save some work.  */
  if (trust_type_canonical)
    {
      t1 = TYPE_MAIN_VARIANT (t1);
      t2 = TYPE_MAIN_VARIANT (t2);
    }

  /* Check first for the obvious case of pointer identity.  */
  if (t1 == t2)
    return true;

  /* Check that we have two types to compare.  */
  if (t1 == NULL_TREE || t2 == NULL_TREE)
    return false;

  /* We consider complete types always compatible with incomplete type.
     This does not make sense for canonical type calculation and thus we
     need to ensure that we are never called on it.

     FIXME: For more correctness the function probably should have three modes
	1) mode assuming that types are complete mathcing their structure
	2) mode allowing incomplete types but producing equivalence classes
	   and thus ignoring all info from complete types
	3) mode allowing incomplete types to match complete but checking
	   compatibility between complete types.

     1 and 2 can be used for canonical type calculation. 3 is the real
     definition of type compatibility that can be used i.e. for warnings during
     declaration merging.  */

  gcc_assert (!trust_type_canonical
	      || (type_with_alias_set_p (t1) && type_with_alias_set_p (t2)));
  /* If the types have been previously registered and found equal
     they still are.  */

  if (TYPE_CANONICAL (t1) && TYPE_CANONICAL (t2)
      && trust_type_canonical)
    {
      /* Do not use TYPE_CANONICAL of pointer types.  For LTO streamed types
	 they are always NULL, but they are set to non-NULL for types
	 constructed by build_pointer_type and variants.  In this case the
	 TYPE_CANONICAL is more fine grained than the equivalnce we test (where
	 all pointers are considered equal.  Be sure to not return false
	 negatives.  */
      gcc_checking_assert (canonical_type_used_p (t1)
			   && canonical_type_used_p (t2));
      return TYPE_CANONICAL (t1) == TYPE_CANONICAL (t2);
    }

  /* Can't be the same type if the types don't have the same code.  */
  enum tree_code code = tree_code_for_canonical_type_merging (TREE_CODE (t1));
  if (code != tree_code_for_canonical_type_merging (TREE_CODE (t2)))
    return false;

  /* Qualifiers do not matter for canonical type comparison purposes.  */

  /* Void types and nullptr types are always the same.  */
  if (TREE_CODE (t1) == VOID_TYPE
      || TREE_CODE (t1) == NULLPTR_TYPE)
    return true;

  /* Can't be the same type if they have different mode.  */
  if (TYPE_MODE (t1) != TYPE_MODE (t2))
    return false;

  /* Non-aggregate types can be handled cheaply.  */
  if (INTEGRAL_TYPE_P (t1)
      || SCALAR_FLOAT_TYPE_P (t1)
      || FIXED_POINT_TYPE_P (t1)
      || TREE_CODE (t1) == VECTOR_TYPE
      || TREE_CODE (t1) == COMPLEX_TYPE
      || TREE_CODE (t1) == OFFSET_TYPE
      || POINTER_TYPE_P (t1))
    {
      /* Can't be the same type if they have different recision.  */
      if (TYPE_PRECISION (t1) != TYPE_PRECISION (t2))
	return false;

      /* In some cases the signed and unsigned types are required to be
	 inter-operable.  */
      if (TYPE_UNSIGNED (t1) != TYPE_UNSIGNED (t2)
	  && !type_with_interoperable_signedness (t1))
	return false;

      /* Fortran's C_SIGNED_CHAR is !TYPE_STRING_FLAG but needs to be
	 interoperable with "signed char".  Unless all frontends are revisited
	 to agree on these types, we must ignore the flag completely.  */

      /* Fortran standard define C_PTR type that is compatible with every
 	 C pointer.  For this reason we need to glob all pointers into one.
	 Still pointers in different address spaces are not compatible.  */
      if (POINTER_TYPE_P (t1))
	{
	  if (TYPE_ADDR_SPACE (TREE_TYPE (t1))
	      != TYPE_ADDR_SPACE (TREE_TYPE (t2)))
	    return false;
	}

      /* Tail-recurse to components.  */
      if (TREE_CODE (t1) == VECTOR_TYPE
	  || TREE_CODE (t1) == COMPLEX_TYPE)
	return gimple_canonical_types_compatible_p (TREE_TYPE (t1),
						    TREE_TYPE (t2),
						    trust_type_canonical);

      return true;
    }

  /* Do type-specific comparisons.  */
  switch (TREE_CODE (t1))
    {
    case ARRAY_TYPE:
      /* Array types are the same if the element types are the same and
	 the number of elements are the same.  */
      if (!gimple_canonical_types_compatible_p (TREE_TYPE (t1), TREE_TYPE (t2),
						trust_type_canonical)
	  || TYPE_STRING_FLAG (t1) != TYPE_STRING_FLAG (t2)
	  || TYPE_REVERSE_STORAGE_ORDER (t1) != TYPE_REVERSE_STORAGE_ORDER (t2)
	  || TYPE_NONALIASED_COMPONENT (t1) != TYPE_NONALIASED_COMPONENT (t2))
	return false;
      else
	{
	  tree i1 = TYPE_DOMAIN (t1);
	  tree i2 = TYPE_DOMAIN (t2);

	  /* For an incomplete external array, the type domain can be
 	     NULL_TREE.  Check this condition also.  */
	  if (i1 == NULL_TREE && i2 == NULL_TREE)
	    return true;
	  else if (i1 == NULL_TREE || i2 == NULL_TREE)
	    return false;
	  else
	    {
	      tree min1 = TYPE_MIN_VALUE (i1);
	      tree min2 = TYPE_MIN_VALUE (i2);
	      tree max1 = TYPE_MAX_VALUE (i1);
	      tree max2 = TYPE_MAX_VALUE (i2);

	      /* The minimum/maximum values have to be the same.  */
	      if ((min1 == min2
		   || (min1 && min2
		       && ((TREE_CODE (min1) == PLACEHOLDER_EXPR
			    && TREE_CODE (min2) == PLACEHOLDER_EXPR)
		           || operand_equal_p (min1, min2, 0))))
		  && (max1 == max2
		      || (max1 && max2
			  && ((TREE_CODE (max1) == PLACEHOLDER_EXPR
			       && TREE_CODE (max2) == PLACEHOLDER_EXPR)
			      || operand_equal_p (max1, max2, 0)))))
		return true;
	      else
		return false;
	    }
	}

    case METHOD_TYPE:
    case FUNCTION_TYPE:
      /* Function types are the same if the return type and arguments types
	 are the same.  */
      if (!gimple_canonical_types_compatible_p (TREE_TYPE (t1), TREE_TYPE (t2),
						trust_type_canonical))
	return false;

      if (TYPE_ARG_TYPES (t1) == TYPE_ARG_TYPES (t2))
	return true;
      else
	{
	  tree parms1, parms2;

	  for (parms1 = TYPE_ARG_TYPES (t1), parms2 = TYPE_ARG_TYPES (t2);
	       parms1 && parms2;
	       parms1 = TREE_CHAIN (parms1), parms2 = TREE_CHAIN (parms2))
	    {
	      if (!gimple_canonical_types_compatible_p
		     (TREE_VALUE (parms1), TREE_VALUE (parms2),
		      trust_type_canonical))
		return false;
	    }

	  if (parms1 || parms2)
	    return false;

	  return true;
	}

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	tree f1, f2;

	/* Don't try to compare variants of an incomplete type, before
	   TYPE_FIELDS has been copied around.  */
	if (!COMPLETE_TYPE_P (t1) && !COMPLETE_TYPE_P (t2))
	  return true;


	if (TYPE_REVERSE_STORAGE_ORDER (t1) != TYPE_REVERSE_STORAGE_ORDER (t2))
	  return false;

	/* For aggregate types, all the fields must be the same.  */
	for (f1 = TYPE_FIELDS (t1), f2 = TYPE_FIELDS (t2);
	     f1 || f2;
	     f1 = TREE_CHAIN (f1), f2 = TREE_CHAIN (f2))
	  {
	    /* Skip non-fields.  */
	    while (f1 && TREE_CODE (f1) != FIELD_DECL)
	      f1 = TREE_CHAIN (f1);
	    while (f2 && TREE_CODE (f2) != FIELD_DECL)
	      f2 = TREE_CHAIN (f2);
	    if (!f1 || !f2)
	      break;
	    /* The fields must have the same name, offset and type.  */
	    if (DECL_NONADDRESSABLE_P (f1) != DECL_NONADDRESSABLE_P (f2)
		|| !gimple_compare_field_offset (f1, f2)
		|| !gimple_canonical_types_compatible_p
		      (TREE_TYPE (f1), TREE_TYPE (f2),
		       trust_type_canonical))
	      return false;
	  }

	/* If one aggregate has more fields than the other, they
	   are not the same.  */
	if (f1 || f2)
	  return false;

	return true;
      }

    default:
      /* Consider all types with language specific trees in them mutually
	 compatible.  This is executed only from verify_type and false
         positives can be tolerated.  */
      gcc_assert (!in_lto_p);
      return true;
    }
}

/* Verify type T.  */

void
verify_type (const_tree t)
{
  bool error_found = false;
  tree mv = TYPE_MAIN_VARIANT (t);
  if (!mv)
    {
      error ("Main variant is not defined");
      error_found = true;
    }
  else if (mv != TYPE_MAIN_VARIANT (mv))
    {
      error ("TYPE_MAIN_VARIANT has different TYPE_MAIN_VARIANT");
      debug_tree (mv);
      error_found = true;
    }
  else if (t != mv && !verify_type_variant (t, mv))
    error_found = true;

  tree ct = TYPE_CANONICAL (t);
  if (!ct)
    ;
  else if (TYPE_CANONICAL (t) != ct)
    {
      error ("TYPE_CANONICAL has different TYPE_CANONICAL");
      debug_tree (ct);
      error_found = true;
    }
  /* Method and function types can not be used to address memory and thus
     TYPE_CANONICAL really matters only for determining useless conversions.

     FIXME: C++ FE produce declarations of builtin functions that are not
     compatible with main variants.  */
  else if (TREE_CODE (t) == FUNCTION_TYPE)
    ;
  else if (t != ct
	   /* FIXME: gimple_canonical_types_compatible_p can not compare types
	      with variably sized arrays because their sizes possibly
	      gimplified to different variables.  */
	   && !variably_modified_type_p (ct, NULL)
	   && !gimple_canonical_types_compatible_p (t, ct, false))
    {
      error ("TYPE_CANONICAL is not compatible");
      debug_tree (ct);
      error_found = true;
    }

  if (COMPLETE_TYPE_P (t) && TYPE_CANONICAL (t)
      && TYPE_MODE (t) != TYPE_MODE (TYPE_CANONICAL (t)))
    {
      error ("TYPE_MODE of TYPE_CANONICAL is not compatible");
      debug_tree (ct);
      error_found = true;
    }
  /* FIXME: this is violated by the C++ FE as discussed in PR70029, when
     FUNCTION_*_QUALIFIED flags are set.  */
  if (0 && TYPE_MAIN_VARIANT (t) == t && ct && TYPE_MAIN_VARIANT (ct) != ct)
   {
      error ("TYPE_CANONICAL of main variant is not main variant");
      debug_tree (ct);
      debug_tree (TYPE_MAIN_VARIANT (ct));
      error_found = true;
   }


  /* Check various uses of TYPE_MINVAL.  */
  if (RECORD_OR_UNION_TYPE_P (t))
    {
      /* FIXME: C FE uses TYPE_VFIELD to record C_TYPE_INCOMPLETE_VARS
	 and danagle the pointer from time to time.  */
      if (TYPE_VFIELD (t)
	  && TREE_CODE (TYPE_VFIELD (t)) != FIELD_DECL
	  && TREE_CODE (TYPE_VFIELD (t)) != TREE_LIST)
	{
	  error ("TYPE_VFIELD is not FIELD_DECL nor TREE_LIST");
	  debug_tree (TYPE_VFIELD (t));
	  error_found = true;
	}
    }
  else if (TREE_CODE (t) == POINTER_TYPE)
    {
      if (TYPE_NEXT_PTR_TO (t)
	  && TREE_CODE (TYPE_NEXT_PTR_TO (t)) != POINTER_TYPE)
	{
	  error ("TYPE_NEXT_PTR_TO is not POINTER_TYPE");
	  debug_tree (TYPE_NEXT_PTR_TO (t));
	  error_found = true;
	}
    }
  else if (TREE_CODE (t) == REFERENCE_TYPE)
    {
      if (TYPE_NEXT_REF_TO (t)
	  && TREE_CODE (TYPE_NEXT_REF_TO (t)) != REFERENCE_TYPE)
	{
	  error ("TYPE_NEXT_REF_TO is not REFERENCE_TYPE");
	  debug_tree (TYPE_NEXT_REF_TO (t));
	  error_found = true;
	}
    }
  else if (INTEGRAL_TYPE_P (t) || TREE_CODE (t) == REAL_TYPE
	   || TREE_CODE (t) == FIXED_POINT_TYPE)
    {
      /* FIXME: The following check should pass:
	  useless_type_conversion_p (const_cast <tree> (t),
				     TREE_TYPE (TYPE_MIN_VALUE (t))
	 but does not for C sizetypes in LTO.  */
    }
  /* Java uses TYPE_MINVAL for TYPE_ARGUMENT_SIGNATURE.  */
  else if (TYPE_MINVAL (t)
	   && ((TREE_CODE (t) != METHOD_TYPE && TREE_CODE (t) != FUNCTION_TYPE)
	       || in_lto_p))
    {
      error ("TYPE_MINVAL non-NULL");
      debug_tree (TYPE_MINVAL (t));
      error_found = true;
    }

  /* Check various uses of TYPE_MAXVAL.  */
  if (RECORD_OR_UNION_TYPE_P (t))
    {
      if (TYPE_METHODS (t) && TREE_CODE (TYPE_METHODS (t)) != FUNCTION_DECL
	  && TREE_CODE (TYPE_METHODS (t)) != TEMPLATE_DECL
	  && TYPE_METHODS (t) != error_mark_node)
	{
	  error ("TYPE_METHODS is not FUNCTION_DECL, TEMPLATE_DECL nor error_mark_node");
	  debug_tree (TYPE_METHODS (t));
	  error_found = true;
	}
    }
  else if (TREE_CODE (t) == FUNCTION_TYPE || TREE_CODE (t) == METHOD_TYPE)
    {
      if (TYPE_METHOD_BASETYPE (t)
	  && TREE_CODE (TYPE_METHOD_BASETYPE (t)) != RECORD_TYPE
	  && TREE_CODE (TYPE_METHOD_BASETYPE (t)) != UNION_TYPE)
	{
	  error ("TYPE_METHOD_BASETYPE is not record nor union");
	  debug_tree (TYPE_METHOD_BASETYPE (t));
	  error_found = true;
	}
    }
  else if (TREE_CODE (t) == OFFSET_TYPE)
    {
      if (TYPE_OFFSET_BASETYPE (t)
	  && TREE_CODE (TYPE_OFFSET_BASETYPE (t)) != RECORD_TYPE
	  && TREE_CODE (TYPE_OFFSET_BASETYPE (t)) != UNION_TYPE)
	{
	  error ("TYPE_OFFSET_BASETYPE is not record nor union");
	  debug_tree (TYPE_OFFSET_BASETYPE (t));
	  error_found = true;
	}
    }
  else if (INTEGRAL_TYPE_P (t) || TREE_CODE (t) == REAL_TYPE
	   || TREE_CODE (t) == FIXED_POINT_TYPE)
    {
      /* FIXME: The following check should pass:
	  useless_type_conversion_p (const_cast <tree> (t),
				     TREE_TYPE (TYPE_MAX_VALUE (t))
	 but does not for C sizetypes in LTO.  */
    }
  else if (TREE_CODE (t) == ARRAY_TYPE)
    {
      if (TYPE_ARRAY_MAX_SIZE (t)
	  && TREE_CODE (TYPE_ARRAY_MAX_SIZE (t)) != INTEGER_CST)
        {
	  error ("TYPE_ARRAY_MAX_SIZE not INTEGER_CST");
	  debug_tree (TYPE_ARRAY_MAX_SIZE (t));
	  error_found = true;
        } 
    }
  else if (TYPE_MAXVAL (t))
    {
      error ("TYPE_MAXVAL non-NULL");
      debug_tree (TYPE_MAXVAL (t));
      error_found = true;
    }

  /* Check various uses of TYPE_BINFO.  */
  if (RECORD_OR_UNION_TYPE_P (t))
    {
      if (!TYPE_BINFO (t))
	;
      else if (TREE_CODE (TYPE_BINFO (t)) != TREE_BINFO)
	{
	  error ("TYPE_BINFO is not TREE_BINFO");
	  debug_tree (TYPE_BINFO (t));
	  error_found = true;
	}
      /* FIXME: Java builds invalid empty binfos that do not have
         TREE_TYPE set.  */
      else if (TREE_TYPE (TYPE_BINFO (t)) != TYPE_MAIN_VARIANT (t) && 0)
	{
	  error ("TYPE_BINFO type is not TYPE_MAIN_VARIANT");
	  debug_tree (TREE_TYPE (TYPE_BINFO (t)));
	  error_found = true;
	}
    }
  else if (TYPE_LANG_SLOT_1 (t) && in_lto_p)
    {
      error ("TYPE_LANG_SLOT_1 (binfo) field is non-NULL");
      debug_tree (TYPE_LANG_SLOT_1 (t));
      error_found = true;
    }

  /* Check various uses of TYPE_VALUES_RAW.  */
  if (TREE_CODE (t) == ENUMERAL_TYPE)
    for (tree l = TYPE_VALUES (t); l; l = TREE_CHAIN (l))
      {
	tree value = TREE_VALUE (l);
	tree name = TREE_PURPOSE (l);

	/* C FE porduce INTEGER_CST of INTEGER_TYPE, while C++ FE uses
 	   CONST_DECL of ENUMERAL TYPE.  */
	if (TREE_CODE (value) != INTEGER_CST && TREE_CODE (value) != CONST_DECL)
	  {
	    error ("Enum value is not CONST_DECL or INTEGER_CST");
	    debug_tree (value);
	    debug_tree (name);
	    error_found = true;
	  }
	if (TREE_CODE (TREE_TYPE (value)) != INTEGER_TYPE
	    && !useless_type_conversion_p (const_cast <tree> (t), TREE_TYPE (value)))
	  {
	    error ("Enum value type is not INTEGER_TYPE nor convertible to the enum");
	    debug_tree (value);
	    debug_tree (name);
	    error_found = true;
	  }
	if (TREE_CODE (name) != IDENTIFIER_NODE)
	  {
	    error ("Enum value name is not IDENTIFIER_NODE");
	    debug_tree (value);
	    debug_tree (name);
	    error_found = true;
	  }
      }
  else if (TREE_CODE (t) == ARRAY_TYPE)
    {
      if (TYPE_DOMAIN (t) && TREE_CODE (TYPE_DOMAIN (t)) != INTEGER_TYPE)
	{
	  error ("Array TYPE_DOMAIN is not integer type");
	  debug_tree (TYPE_DOMAIN (t));
	  error_found = true;
	}
    }
  else if (RECORD_OR_UNION_TYPE_P (t))
    {
      if (TYPE_FIELDS (t) && !COMPLETE_TYPE_P (t) && in_lto_p)
	{
	  error ("TYPE_FIELDS defined in incomplete type");
	  error_found = true;
	}
      for (tree fld = TYPE_FIELDS (t); fld; fld = TREE_CHAIN (fld))
	{
	  /* TODO: verify properties of decls.  */
	  if (TREE_CODE (fld) == FIELD_DECL)
	    ;
	  else if (TREE_CODE (fld) == TYPE_DECL)
	    ;
	  else if (TREE_CODE (fld) == CONST_DECL)
	    ;
	  else if (TREE_CODE (fld) == VAR_DECL)
	    ;
	  else if (TREE_CODE (fld) == TEMPLATE_DECL)
	    ;
	  else if (TREE_CODE (fld) == USING_DECL)
	    ;
	  else
	    {
	      error ("Wrong tree in TYPE_FIELDS list");
	      debug_tree (fld);
	      error_found = true;
	    }
	}
    }
  else if (TREE_CODE (t) == INTEGER_TYPE
	   || TREE_CODE (t) == BOOLEAN_TYPE
	   || TREE_CODE (t) == OFFSET_TYPE
	   || TREE_CODE (t) == REFERENCE_TYPE
	   || TREE_CODE (t) == NULLPTR_TYPE
	   || TREE_CODE (t) == POINTER_TYPE)
    {
      if (TYPE_CACHED_VALUES_P (t) != (TYPE_CACHED_VALUES (t) != NULL))
	{
	  error ("TYPE_CACHED_VALUES_P is %i while TYPE_CACHED_VALUES is %p",
		 TYPE_CACHED_VALUES_P (t), (void *)TYPE_CACHED_VALUES (t));
	  error_found = true;
	}
      else if (TYPE_CACHED_VALUES_P (t) && TREE_CODE (TYPE_CACHED_VALUES (t)) != TREE_VEC)
	{
	  error ("TYPE_CACHED_VALUES is not TREE_VEC");
	  debug_tree (TYPE_CACHED_VALUES (t));
	  error_found = true;
	}
      /* Verify just enough of cache to ensure that no one copied it to new type.
 	 All copying should go by copy_node that should clear it.  */
      else if (TYPE_CACHED_VALUES_P (t))
	{
	  int i;
	  for (i = 0; i < TREE_VEC_LENGTH (TYPE_CACHED_VALUES (t)); i++)
	    if (TREE_VEC_ELT (TYPE_CACHED_VALUES (t), i)
		&& TREE_TYPE (TREE_VEC_ELT (TYPE_CACHED_VALUES (t), i)) != t)
	      {
		error ("wrong TYPE_CACHED_VALUES entry");
		debug_tree (TREE_VEC_ELT (TYPE_CACHED_VALUES (t), i));
		error_found = true;
		break;
	      }
	}
    }
  else if (TREE_CODE (t) == FUNCTION_TYPE || TREE_CODE (t) == METHOD_TYPE)
    for (tree l = TYPE_ARG_TYPES (t); l; l = TREE_CHAIN (l))
      {
	/* C++ FE uses TREE_PURPOSE to store initial values.  */
	if (TREE_PURPOSE (l) && in_lto_p)
	  {
	    error ("TREE_PURPOSE is non-NULL in TYPE_ARG_TYPES list");
	    debug_tree (l);
	    error_found = true;
	  }
	if (!TYPE_P (TREE_VALUE (l)))
	  {
	    error ("Wrong entry in TYPE_ARG_TYPES list");
	    debug_tree (l);
	    error_found = true;
	  }
      }
  else if (!is_lang_specific (t) && TYPE_VALUES_RAW (t))
    {
      error ("TYPE_VALUES_RAW field is non-NULL");
      debug_tree (TYPE_VALUES_RAW (t));
      error_found = true;
    }
  if (TREE_CODE (t) != INTEGER_TYPE
      && TREE_CODE (t) != BOOLEAN_TYPE
      && TREE_CODE (t) != OFFSET_TYPE
      && TREE_CODE (t) != REFERENCE_TYPE
      && TREE_CODE (t) != NULLPTR_TYPE
      && TREE_CODE (t) != POINTER_TYPE
      && TYPE_CACHED_VALUES_P (t))
    {
      error ("TYPE_CACHED_VALUES_P is set while it should not");
      error_found = true;
    }
  if (TYPE_STRING_FLAG (t)
      && TREE_CODE (t) != ARRAY_TYPE && TREE_CODE (t) != INTEGER_TYPE)
    {
      error ("TYPE_STRING_FLAG is set on wrong type code");
      error_found = true;
    }
  else if (TYPE_STRING_FLAG (t))
    {
      const_tree b = t;
      if (TREE_CODE (b) == ARRAY_TYPE)
	b = TREE_TYPE (t);
      /* Java builds arrays with TYPE_STRING_FLAG of promoted_char_type
	 that is 32bits.  */
      if (TREE_CODE (b) != INTEGER_TYPE)
	{
	  error ("TYPE_STRING_FLAG is set on type that does not look like "
		 "char nor array of chars");
	  error_found = true;
	}
    }
  
  /* ipa-devirt makes an assumption that TYPE_METHOD_BASETYPE is always
     TYPE_MAIN_VARIANT and it would be odd to add methods only to variatns
     of a type. */
  if (TREE_CODE (t) == METHOD_TYPE
      && TYPE_MAIN_VARIANT (TYPE_METHOD_BASETYPE (t)) != TYPE_METHOD_BASETYPE (t))
    {
	error ("TYPE_METHOD_BASETYPE is not main variant");
	error_found = true;
    }

  if (error_found)
    {
      debug_tree (const_cast <tree> (t));
      internal_error ("verify_type failed");
    }
}


/* Return true if ARG is marked with the nonnull attribute in the
   current function signature.  */

bool
nonnull_arg_p (const_tree arg)
{
  tree t, attrs, fntype;
  unsigned HOST_WIDE_INT arg_num;

  gcc_assert (TREE_CODE (arg) == PARM_DECL
	      && (POINTER_TYPE_P (TREE_TYPE (arg))
		  || TREE_CODE (TREE_TYPE (arg)) == OFFSET_TYPE));

  /* The static chain decl is always non null.  */
  if (arg == cfun->static_chain_decl)
    return true;

  /* THIS argument of method is always non-NULL.  */
  if (TREE_CODE (TREE_TYPE (cfun->decl)) == METHOD_TYPE
      && arg == DECL_ARGUMENTS (cfun->decl)
      && flag_delete_null_pointer_checks)
    return true;

  /* Values passed by reference are always non-NULL.  */
  if (TREE_CODE (TREE_TYPE (arg)) == REFERENCE_TYPE
      && flag_delete_null_pointer_checks)
    return true;

  fntype = TREE_TYPE (cfun->decl);
  for (attrs = TYPE_ATTRIBUTES (fntype); attrs; attrs = TREE_CHAIN (attrs))
    {
      attrs = lookup_attribute ("nonnull", attrs);

      /* If "nonnull" wasn't specified, we know nothing about the argument.  */
      if (attrs == NULL_TREE)
	return false;

      /* If "nonnull" applies to all the arguments, then ARG is non-null.  */
      if (TREE_VALUE (attrs) == NULL_TREE)
	return true;

      /* Get the position number for ARG in the function signature.  */
      for (arg_num = 1, t = DECL_ARGUMENTS (cfun->decl);
	   t;
	   t = DECL_CHAIN (t), arg_num++)
	{
	  if (t == arg)
	    break;
	}

      gcc_assert (t == arg);

      /* Now see if ARG_NUM is mentioned in the nonnull list.  */
      for (t = TREE_VALUE (attrs); t; t = TREE_CHAIN (t))
	{
	  if (compare_tree_int (TREE_VALUE (t), arg_num) == 0)
	    return true;
	}
    }

  return false;
}

/* Given location LOC, strip away any packed range information
   or ad-hoc information.  */

location_t
get_pure_location (location_t loc)
{
  if (IS_ADHOC_LOC (loc))
    loc
      = line_table->location_adhoc_data_map.data[loc & MAX_SOURCE_LOCATION].locus;

  if (loc >= LINEMAPS_MACRO_LOWEST_LOCATION (line_table))
    return loc;

  if (loc < RESERVED_LOCATION_COUNT)
    return loc;

  const line_map *map = linemap_lookup (line_table, loc);
  const line_map_ordinary *ordmap = linemap_check_ordinary (map);

  return loc & ~((1 << ordmap->m_range_bits) - 1);
}

/* Combine LOC and BLOCK to a combined adhoc loc, retaining any range
   information.  */

location_t
set_block (location_t loc, tree block)
{
  location_t pure_loc = get_pure_location (loc);
  source_range src_range = get_range_from_loc (line_table, loc);
  return COMBINE_LOCATION_DATA (line_table, pure_loc, src_range, block);
}

location_t
set_source_range (tree expr, location_t start, location_t finish)
{
  source_range src_range;
  src_range.m_start = start;
  src_range.m_finish = finish;
  return set_source_range (expr, src_range);
}

location_t
set_source_range (tree expr, source_range src_range)
{
  if (!EXPR_P (expr))
    return UNKNOWN_LOCATION;

  location_t pure_loc = get_pure_location (EXPR_LOCATION (expr));
  location_t adhoc = COMBINE_LOCATION_DATA (line_table,
					    pure_loc,
					    src_range,
					    NULL);
  SET_EXPR_LOCATION (expr, adhoc);
  return adhoc;
}

location_t
make_location (location_t caret, location_t start, location_t finish)
{
  location_t pure_loc = get_pure_location (caret);
  source_range src_range;
  src_range.m_start = start;
  src_range.m_finish = finish;
  location_t combined_loc = COMBINE_LOCATION_DATA (line_table,
						   pure_loc,
						   src_range,
						   NULL);
  return combined_loc;
}

/* Return the name of combined function FN, for debugging purposes.  */

const char *
combined_fn_name (combined_fn fn)
{
  if (builtin_fn_p (fn))
    {
      tree fndecl = builtin_decl_explicit (as_builtin_fn (fn));
      return IDENTIFIER_POINTER (DECL_NAME (fndecl));
    }
  else
    return internal_fn_name (as_internal_fn (fn));
}

#if CHECKING_P

namespace selftest {

/* Selftests for tree.  */

/* Verify that integer constants are sane.  */

static void
test_integer_constants ()
{
  ASSERT_TRUE (integer_type_node != NULL);
  ASSERT_TRUE (build_int_cst (integer_type_node, 0) != NULL);

  tree type = integer_type_node;

  tree zero = build_zero_cst (type);
  ASSERT_EQ (INTEGER_CST, TREE_CODE (zero));
  ASSERT_EQ (type, TREE_TYPE (zero));

  tree one = build_int_cst (type, 1);
  ASSERT_EQ (INTEGER_CST, TREE_CODE (one));
  ASSERT_EQ (type, TREE_TYPE (zero));
}

/* Verify identifiers.  */

static void
test_identifiers ()
{
  tree identifier = get_identifier ("foo");
  ASSERT_EQ (3, IDENTIFIER_LENGTH (identifier));
  ASSERT_STREQ ("foo", IDENTIFIER_POINTER (identifier));
}

/* Verify LABEL_DECL.  */

static void
test_labels ()
{
  tree identifier = get_identifier ("err");
  tree label_decl = build_decl (UNKNOWN_LOCATION, LABEL_DECL,
				identifier, void_type_node);
  ASSERT_EQ (-1, LABEL_DECL_UID (label_decl));
  ASSERT_FALSE (FORCED_LABEL (label_decl));
}

/* Run all of the selftests within this file.  */

void
tree_c_tests ()
{
  test_integer_constants ();
  test_identifiers ();
  test_labels ();
}

} // namespace selftest

#endif /* CHECKING_P */

#include "gt-tree.h"
