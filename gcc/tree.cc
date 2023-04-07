/* Language-independent node constructors for parse phase of GNU compiler.
   Copyright (C) 1987-2023 Free Software Foundation, Inc.

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
#include "langhooks-def.h"
#include "tree-diagnostic.h"
#include "except.h"
#include "builtins.h"
#include "print-tree.h"
#include "ipa-utils.h"
#include "selftest.h"
#include "stringpool.h"
#include "attribs.h"
#include "rtl.h"
#include "regs.h"
#include "tree-vector-builder.h"
#include "gimple-fold.h"
#include "escaped_string.h"
#include "gimple-range.h"
#include "gomp-constants.h"
#include "dfp.h"
#include "asan.h"
#include "ubsan.h"

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

static uint64_t tree_code_counts[MAX_TREE_CODES];
uint64_t tree_node_counts[(int) all_kinds];
uint64_t tree_node_sizes[(int) all_kinds];

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
static GTY(()) unsigned next_type_uid = 1;
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

/* Class and variable for making sure that there is a single POLY_INT_CST
   for a given value.  */
struct poly_int_cst_hasher : ggc_cache_ptr_hash<tree_node>
{
  typedef std::pair<tree, const poly_wide_int *> compare_type;
  static hashval_t hash (tree t);
  static bool equal (tree x, const compare_type &y);
};

static GTY ((cache)) hash_table<poly_int_cst_hasher> *poly_int_cst_hash_table;

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

static GTY ((cache))
     hash_table<tree_vec_map_cache_hasher> *debug_args_for_decl;

static void set_type_quals (tree, int);
static void print_type_hash_statistics (void);
static void print_debug_expr_statistics (void);
static void print_value_expr_statistics (void);

tree global_trees[TI_MAX];
tree integer_types[itk_none];

bool int_n_enabled_p[NUM_INT_N_ENTS];
struct int_n_trees_t int_n_trees [NUM_INT_N_ENTS];

bool tree_contains_struct[MAX_TREE_CODES][64];

/* Number of operands for each OMP clause.  */
unsigned const char omp_clause_num_ops[] =
{
  0, /* OMP_CLAUSE_ERROR  */
  1, /* OMP_CLAUSE_PRIVATE  */
  1, /* OMP_CLAUSE_SHARED  */
  1, /* OMP_CLAUSE_FIRSTPRIVATE  */
  2, /* OMP_CLAUSE_LASTPRIVATE  */
  5, /* OMP_CLAUSE_REDUCTION  */
  5, /* OMP_CLAUSE_TASK_REDUCTION  */
  5, /* OMP_CLAUSE_IN_REDUCTION  */
  1, /* OMP_CLAUSE_COPYIN  */
  1, /* OMP_CLAUSE_COPYPRIVATE  */
  3, /* OMP_CLAUSE_LINEAR  */
  1, /* OMP_CLAUSE_AFFINITY  */
  2, /* OMP_CLAUSE_ALIGNED  */
  3, /* OMP_CLAUSE_ALLOCATE  */
  1, /* OMP_CLAUSE_DEPEND  */
  1, /* OMP_CLAUSE_NONTEMPORAL  */
  1, /* OMP_CLAUSE_UNIFORM  */
  1, /* OMP_CLAUSE_ENTER  */
  1, /* OMP_CLAUSE_LINK  */
  1, /* OMP_CLAUSE_DETACH  */
  1, /* OMP_CLAUSE_USE_DEVICE_PTR  */
  1, /* OMP_CLAUSE_USE_DEVICE_ADDR  */
  1, /* OMP_CLAUSE_IS_DEVICE_PTR  */
  1, /* OMP_CLAUSE_INCLUSIVE  */
  1, /* OMP_CLAUSE_EXCLUSIVE  */
  2, /* OMP_CLAUSE_FROM  */
  2, /* OMP_CLAUSE_TO  */
  2, /* OMP_CLAUSE_MAP  */
  1, /* OMP_CLAUSE_HAS_DEVICE_ADDR  */
  1, /* OMP_CLAUSE_DOACROSS  */
  2, /* OMP_CLAUSE__CACHE_  */
  2, /* OMP_CLAUSE_GANG  */
  1, /* OMP_CLAUSE_ASYNC  */
  1, /* OMP_CLAUSE_WAIT  */
  0, /* OMP_CLAUSE_AUTO  */
  0, /* OMP_CLAUSE_SEQ  */
  1, /* OMP_CLAUSE__LOOPTEMP_  */
  1, /* OMP_CLAUSE__REDUCTEMP_  */
  1, /* OMP_CLAUSE__CONDTEMP_  */
  1, /* OMP_CLAUSE__SCANTEMP_  */
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
  2, /* OMP_CLAUSE_NUM_TEAMS  */
  1, /* OMP_CLAUSE_THREAD_LIMIT  */
  0, /* OMP_CLAUSE_PROC_BIND  */
  1, /* OMP_CLAUSE_SAFELEN  */
  1, /* OMP_CLAUSE_SIMDLEN  */
  0, /* OMP_CLAUSE_DEVICE_TYPE  */
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
  0, /* OMP_CLAUSE_DEFAULTMAP  */
  0, /* OMP_CLAUSE_ORDER  */
  0, /* OMP_CLAUSE_BIND  */
  1, /* OMP_CLAUSE_FILTER  */
  1, /* OMP_CLAUSE__SIMDUID_  */
  0, /* OMP_CLAUSE__SIMT_  */
  0, /* OMP_CLAUSE_INDEPENDENT  */
  1, /* OMP_CLAUSE_WORKER  */
  1, /* OMP_CLAUSE_VECTOR  */
  1, /* OMP_CLAUSE_NUM_GANGS  */
  1, /* OMP_CLAUSE_NUM_WORKERS  */
  1, /* OMP_CLAUSE_VECTOR_LENGTH  */
  3, /* OMP_CLAUSE_TILE  */
  0, /* OMP_CLAUSE_IF_PRESENT */
  0, /* OMP_CLAUSE_FINALIZE */
  0, /* OMP_CLAUSE_NOHOST */
};

const char * const omp_clause_code_name[] =
{
  "error_clause",
  "private",
  "shared",
  "firstprivate",
  "lastprivate",
  "reduction",
  "task_reduction",
  "in_reduction",
  "copyin",
  "copyprivate",
  "linear",
  "affinity",
  "aligned",
  "allocate",
  "depend",
  "nontemporal",
  "uniform",
  "enter",
  "link",
  "detach",
  "use_device_ptr",
  "use_device_addr",
  "is_device_ptr",
  "inclusive",
  "exclusive",
  "from",
  "to",
  "map",
  "has_device_addr",
  "doacross",
  "_cache_",
  "gang",
  "async",
  "wait",
  "auto",
  "seq",
  "_looptemp_",
  "_reductemp_",
  "_condtemp_",
  "_scantemp_",
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
  "device_type",
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
  "order",
  "bind",
  "filter",
  "_simduid_",
  "_simt_",
  "independent",
  "worker",
  "vector",
  "num_gangs",
  "num_workers",
  "vector_length",
  "tile",
  "if_present",
  "finalize",
  "nohost",
};

/* Unless specific to OpenACC, we tend to internally maintain OpenMP-centric
   clause names, but for use in diagnostics etc. would like to use the "user"
   clause names.  */

const char *
user_omp_clause_code_name (tree clause, bool oacc)
{
  /* For OpenACC, the 'OMP_CLAUSE_MAP_KIND' of an 'OMP_CLAUSE_MAP' is used to
     distinguish clauses as seen by the user.  See also where front ends do
     'build_omp_clause' with 'OMP_CLAUSE_MAP'.  */
  if (oacc && OMP_CLAUSE_CODE (clause) == OMP_CLAUSE_MAP)
    switch (OMP_CLAUSE_MAP_KIND (clause))
      {
      case GOMP_MAP_FORCE_ALLOC:
      case GOMP_MAP_ALLOC: return "create";
      case GOMP_MAP_FORCE_TO:
      case GOMP_MAP_TO: return "copyin";
      case GOMP_MAP_FORCE_FROM:
      case GOMP_MAP_FROM: return "copyout";
      case GOMP_MAP_FORCE_TOFROM:
      case GOMP_MAP_TOFROM: return "copy";
      case GOMP_MAP_RELEASE: return "delete";
      case GOMP_MAP_FORCE_PRESENT: return "present";
      case GOMP_MAP_ATTACH: return "attach";
      case GOMP_MAP_FORCE_DETACH:
      case GOMP_MAP_DETACH: return "detach";
      case GOMP_MAP_DEVICE_RESIDENT: return "device_resident";
      case GOMP_MAP_LINK: return "link";
      case GOMP_MAP_FORCE_DEVICEPTR: return "deviceptr";
      default: break;
      }

  return omp_clause_code_name[OMP_CLAUSE_CODE (clause)];
}


/* Return the tree node structure used by tree code CODE.  */

static inline enum tree_node_structure_enum
tree_node_structure_for_code (enum tree_code code)
{
  switch (TREE_CODE_CLASS (code))
    {
    case tcc_declaration:
      switch (code)
	{
	case CONST_DECL:	return TS_CONST_DECL;
	case DEBUG_EXPR_DECL:	return TS_DECL_WRTL;
	case FIELD_DECL:	return TS_FIELD_DECL;
	case FUNCTION_DECL:	return TS_FUNCTION_DECL;
	case LABEL_DECL:	return TS_LABEL_DECL;
	case PARM_DECL:		return TS_PARM_DECL;
	case RESULT_DECL:	return TS_RESULT_DECL;
	case TRANSLATION_UNIT_DECL: return TS_TRANSLATION_UNIT_DECL;
	case TYPE_DECL:		return TS_TYPE_DECL;
	case VAR_DECL:		return TS_VAR_DECL;
	default: 		return TS_DECL_NON_COMMON;
	}

    case tcc_type:		return TS_TYPE_NON_COMMON;

    case tcc_binary:
    case tcc_comparison:
    case tcc_expression:
    case tcc_reference:
    case tcc_statement:
    case tcc_unary:
    case tcc_vl_exp:		return TS_EXP;

    default:  /* tcc_constant and tcc_exceptional */
      break;
    }

  switch (code)
    {
      /* tcc_constant cases.  */
    case COMPLEX_CST:		return TS_COMPLEX;
    case FIXED_CST:		return TS_FIXED_CST;
    case INTEGER_CST:		return TS_INT_CST;
    case POLY_INT_CST:		return TS_POLY_INT_CST;
    case REAL_CST:		return TS_REAL_CST;
    case STRING_CST:		return TS_STRING;
    case VECTOR_CST:		return TS_VECTOR;
    case VOID_CST:		return TS_TYPED;

      /* tcc_exceptional cases.  */
    case BLOCK:			return TS_BLOCK;
    case CONSTRUCTOR:		return TS_CONSTRUCTOR;
    case ERROR_MARK:		return TS_COMMON;
    case IDENTIFIER_NODE:	return TS_IDENTIFIER;
    case OMP_CLAUSE:		return TS_OMP_CLAUSE;
    case OPTIMIZATION_NODE:	return TS_OPTIMIZATION;
    case PLACEHOLDER_EXPR:	return TS_COMMON;
    case SSA_NAME:		return TS_SSA_NAME;
    case STATEMENT_LIST:	return TS_STATEMENT_LIST;
    case TARGET_OPTION_NODE:	return TS_TARGET_OPTION;
    case TREE_BINFO:		return TS_BINFO;
    case TREE_LIST:		return TS_LIST;
    case TREE_VEC:		return TS_VEC;

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
	case TS_OPTIMIZATION:
	case TS_TARGET_OPTION:
	  MARK_TS_BASE (code);
	  break;

	case TS_COMMON:
	case TS_INT_CST:
	case TS_POLY_INT_CST:
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


/* Init tree.cc.  */

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

  poly_int_cst_hash_table = hash_table<poly_int_cst_hasher>::create_ggc (64);

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
  return DECL_ASSEMBLER_NAME_RAW (decl);
}

/* The DECL_ASSEMBLER_NAME_RAW of DECL is being explicitly set to NAME
   (either of which may be NULL).  Inform the FE, if this changes the
   name.  */

void
overwrite_decl_assembler_name (tree decl, tree name)
{
  if (DECL_ASSEMBLER_NAME_RAW (decl) != name)
    lang_hooks.overwrite_decl_assembler_name (decl, name);
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
     See cp/mangle.cc:write_builtin_type for details.  */

  if (TREE_CODE (decl) == TYPE_DECL)
    {
      if (DECL_NAME (decl)
	  && decl == TYPE_NAME (TREE_TYPE (decl))
	  && TYPE_MAIN_VARIANT (TREE_TYPE (decl)) == TREE_TYPE (decl)
	  && !TYPE_ARTIFICIAL (TREE_TYPE (decl))
	  && ((TREE_CODE (TREE_TYPE (decl)) != RECORD_TYPE
	       && TREE_CODE (TREE_TYPE (decl)) != UNION_TYPE)
	      || TYPE_CXX_ODR_P (TREE_TYPE (decl)))
	  && (type_with_linkage_p (TREE_TYPE (decl))
	      || TREE_CODE (TREE_TYPE (decl)) == INTEGER_TYPE)
	  && !variably_modified_type_p (TREE_TYPE (decl), NULL_TREE))
	return !DECL_ASSEMBLER_NAME_SET_P (decl);
      return false;
    }
  /* Only FUNCTION_DECLs and VAR_DECLs are considered.  */
  if (!VAR_OR_FUNCTION_DECL_P (decl))
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
  if (VAR_P (decl)
      && !TREE_STATIC (decl)
      && !TREE_PUBLIC (decl)
      && !DECL_EXTERNAL (decl))
    return false;

  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* Do not set assembler name on builtins.  Allow RTL expansion to
	 decide whether to expand inline or via a regular call.  */
      if (fndecl_built_in_p (decl)
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

/* If T needs an assembler name, have one created for it.  */

void
assign_assembler_name_if_needed (tree t)
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
  else if (VAR_P (node))
    snode = varpool_node::get_create (node);
  else
    snode = cgraph_node::get_create (node);
  snode->set_section (value);
}

/* Set section name of NODE to match the section name of OTHER.

   set_decl_section_name (decl, other) is equivalent to
   set_decl_section_name (decl, DECL_SECTION_NAME (other)), but possibly more
   efficient.  */
void
set_decl_section_name (tree decl, const_tree other)
{
  struct symtab_node *other_node = symtab_node::get (other);
  if (other_node)
    {
      struct symtab_node *decl_node;
      if (VAR_P (decl))
    decl_node = varpool_node::get_create (decl);
      else
    decl_node = cgraph_node::get_create (decl);
      decl_node->set_section (*other_node);
    }
  else
    {
      struct symtab_node *decl_node = symtab_node::get (decl);
      if (!decl_node)
    return;
      decl_node->set_section (NULL);
    }
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
      switch (code)
	{
	case FIELD_DECL:	return sizeof (tree_field_decl);
	case PARM_DECL:		return sizeof (tree_parm_decl);
	case VAR_DECL:		return sizeof (tree_var_decl);
	case LABEL_DECL:	return sizeof (tree_label_decl);
	case RESULT_DECL:	return sizeof (tree_result_decl);
	case CONST_DECL:	return sizeof (tree_const_decl);
	case TYPE_DECL:		return sizeof (tree_type_decl);
	case FUNCTION_DECL:	return sizeof (tree_function_decl);
	case DEBUG_EXPR_DECL:	return sizeof (tree_decl_with_rtl);
	case TRANSLATION_UNIT_DECL: return sizeof (tree_translation_unit_decl);
	case NAMESPACE_DECL:
	case IMPORTED_DECL:
	case NAMELIST_DECL:	return sizeof (tree_decl_non_common);
	default:
	  gcc_checking_assert (code >= NUM_TREE_CODES);
	  return lang_hooks.tree_size (code);
	}

    case tcc_type:  /* a type node */
      switch (code)
	{
	case OFFSET_TYPE:
	case ENUMERAL_TYPE:
	case BOOLEAN_TYPE:
	case INTEGER_TYPE:
	case REAL_TYPE:
	case OPAQUE_TYPE:
	case POINTER_TYPE:
	case REFERENCE_TYPE:
	case NULLPTR_TYPE:
	case FIXED_POINT_TYPE:
	case COMPLEX_TYPE:
	case VECTOR_TYPE:
	case ARRAY_TYPE:
	case RECORD_TYPE:
	case UNION_TYPE:
	case QUAL_UNION_TYPE:
	case VOID_TYPE:
	case FUNCTION_TYPE:
	case METHOD_TYPE:
	case LANG_TYPE:		return sizeof (tree_type_non_common);
	default:
	  gcc_checking_assert (code >= NUM_TREE_CODES);
	  return lang_hooks.tree_size (code);
	}

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
	case VOID_CST:		return sizeof (tree_typed);
	case INTEGER_CST:	gcc_unreachable ();
	case POLY_INT_CST:	return sizeof (tree_poly_int_cst);
	case REAL_CST:		return sizeof (tree_real_cst);
	case FIXED_CST:		return sizeof (tree_fixed_cst);
	case COMPLEX_CST:	return sizeof (tree_complex);
	case VECTOR_CST:	gcc_unreachable ();
	case STRING_CST:	gcc_unreachable ();
	default:
	  gcc_checking_assert (code >= NUM_TREE_CODES);
	  return lang_hooks.tree_size (code);
	}

    case tcc_exceptional:  /* something random, like an identifier.  */
      switch (code)
	{
	case IDENTIFIER_NODE:	return lang_hooks.identifier_size;
	case TREE_LIST:		return sizeof (tree_list);

	case ERROR_MARK:
	case PLACEHOLDER_EXPR:	return sizeof (tree_common);

	case TREE_VEC:		gcc_unreachable ();
	case OMP_CLAUSE:	gcc_unreachable ();

	case SSA_NAME:		return sizeof (tree_ssa_name);

	case STATEMENT_LIST:	return sizeof (tree_statement_list);
	case BLOCK:		return sizeof (struct tree_block);
	case CONSTRUCTOR:	return sizeof (tree_constructor);
	case OPTIMIZATION_NODE: return sizeof (tree_optimization_option);
	case TARGET_OPTION_NODE: return sizeof (tree_target_option);

	default:
	  gcc_checking_assert (code >= NUM_TREE_CODES);
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
	      + (vector_cst_encoded_nelts (node) - 1) * sizeof (tree));

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

/* Return tree node kind based on tree CODE.  */

static tree_node_kind
get_stats_node_kind (enum tree_code code)
{
  enum tree_code_class type = TREE_CODE_CLASS (code);

  switch (type)
    {
    case tcc_declaration:  /* A decl node */
      return d_kind;
    case tcc_type:  /* a type node */
      return t_kind;
    case tcc_statement:  /* an expression with side effects */
      return s_kind;
    case tcc_reference:  /* a reference */
      return r_kind;
    case tcc_expression:  /* an expression */
    case tcc_comparison:  /* a comparison expression */
    case tcc_unary:  /* a unary arithmetic expression */
    case tcc_binary:  /* a binary arithmetic expression */
      return e_kind;
    case tcc_constant:  /* a constant */
      return c_kind;
    case tcc_exceptional:  /* something random, like an identifier.  */
      switch (code)
	{
	case IDENTIFIER_NODE:
	  return id_kind;
	case TREE_VEC:
	  return vec_kind;
	case TREE_BINFO:
	  return binfo_kind;
	case SSA_NAME:
	  return ssa_name_kind;
	case BLOCK:
	  return b_kind;
	case CONSTRUCTOR:
	  return constr_kind;
	case OMP_CLAUSE:
	  return omp_clause_kind;
	default:
	  return x_kind;
	}
      break;
    case tcc_vl_exp:
      return e_kind;
    default:
      gcc_unreachable ();
    }
}

/* Record interesting allocation statistics for a tree node with CODE
   and LENGTH.  */

static void
record_node_allocation_statistics (enum tree_code code, size_t length)
{
  if (!GATHER_STATISTICS)
    return;

  tree_node_kind kind = get_stats_node_kind (code);

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
make_node (enum tree_code code MEM_STAT_DECL)
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
      if (code != DEBUG_BEGIN_STMT)
	TREE_SIDE_EFFECTS (t) = 1;
      break;

    case tcc_declaration:
      if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
	{
	  if (code == FUNCTION_DECL)
	    {
	      SET_DECL_ALIGN (t, FUNCTION_ALIGNMENT (FUNCTION_BOUNDARY));
	      SET_DECL_MODE (t, FUNCTION_MODE);
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
      enum tree_node_kind kind = get_stats_node_kind (code);

      gcc_checking_assert (tree_code_counts[(int) TREE_CODE (node)] != 0);
      gcc_checking_assert (tree_node_counts[(int) kind] != 0);
      gcc_checking_assert (tree_node_sizes[(int) kind] >= tree_size (node));

      tree_code_counts[(int) TREE_CODE (node)]--;
      tree_node_counts[(int) kind]--;
      tree_node_sizes[(int) kind] -= tree_size (node);
    }
  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    vec_free (CONSTRUCTOR_ELTS (node));
  else if (code == BLOCK)
    vec_free (BLOCK_NONLOCALIZED_VARS (node));
  else if (code == TREE_BINFO)
    vec_free (BINFO_BASE_ACCESSES (node));
  else if (code == OPTIMIZATION_NODE)
    cl_optimization_option_free (TREE_OPTIMIZATION (node));
  else if (code == TARGET_OPTION_NODE)
    cl_target_option_free (TREE_TARGET_OPTION (node));
  ggc_free (node);
}

/* Return a new node with the same contents as NODE except that its
   TREE_CHAIN, if it has one, is zero and it has a fresh uid.  */

tree
copy_node (tree node MEM_STAT_DECL)
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
      if ((TREE_CODE (node) == PARM_DECL || VAR_P (node))
	  && DECL_HAS_VALUE_EXPR_P (node))
	{
	  SET_DECL_VALUE_EXPR (t, DECL_VALUE_EXPR (node));
	  DECL_HAS_VALUE_EXPR_P (t) = 1;
	}
      /* DECL_DEBUG_EXPR is copied explicitly by callers.  */
      if (VAR_P (node))
	{
	  DECL_HAS_DEBUG_EXPR_P (t) = 0;
	  t->decl_with_vis.symtab_node = NULL;
	}
      if (VAR_P (node) && DECL_HAS_INIT_PRIORITY_P (node))
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
      TYPE_SYMTAB_ADDRESS (t) = 0;
      TYPE_SYMTAB_DIE (t) = 0;

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

/* Return a new POLY_INT_CST with coefficients COEFFS and type TYPE.  */

static tree
build_new_poly_int_cst (tree type, tree (&coeffs)[NUM_POLY_INT_COEFFS]
			CXX_MEM_STAT_INFO)
{
  size_t length = sizeof (struct tree_poly_int_cst);
  record_node_allocation_statistics (POLY_INT_CST, length);

  tree t = ggc_alloc_cleared_tree_node_stat (length PASS_MEM_STAT);

  TREE_SET_CODE (t, POLY_INT_CST);
  TREE_CONSTANT (t) = 1;
  TREE_TYPE (t) = type;
  for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; ++i)
    POLY_INT_CST_COEFF (t, i) = coeffs[i];
  return t;
}

/* Create a constant tree that contains CST sign-extended to TYPE.  */

tree
build_int_cst (tree type, poly_int64 cst)
{
  /* Support legacy code.  */
  if (!type)
    type = integer_type_node;

  return wide_int_to_tree (type, wi::shwi (cst, TYPE_PRECISION (type)));
}

/* Create a constant tree that contains CST zero-extended to TYPE.  */

tree
build_int_cstu (tree type, poly_uint64 cst)
{
  return wide_int_to_tree (type, wi::uhwi (cst, TYPE_PRECISION (type)));
}

/* Create a constant tree that contains CST sign-extended to TYPE.  */

tree
build_int_cst_type (tree type, poly_int64 cst)
{
  gcc_assert (type);
  return wide_int_to_tree (type, wi::shwi (cst, TYPE_PRECISION (type)));
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
force_fit_type (tree type, const poly_wide_int_ref &cst,
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
	  poly_wide_int tmp = poly_wide_int::from (cst, TYPE_PRECISION (type),
						   sign);
	  tree t;
	  if (tmp.is_constant ())
	    t = build_new_int_cst (type, tmp.coeffs[0]);
	  else
	    {
	      tree coeffs[NUM_POLY_INT_COEFFS];
	      for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; ++i)
		{
		  coeffs[i] = build_new_int_cst (type, tmp.coeffs[i]);
		  TREE_OVERFLOW (coeffs[i]) = 1;
		}
	      t = build_new_poly_int_cst (type, coeffs);
	    }
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

/* Cache wide_int CST into the TYPE_CACHED_VALUES cache for TYPE.
   SLOT is the slot entry to store it in, and MAX_SLOTS is the maximum
   number of slots that can be cached for the type.  */

static inline tree
cache_wide_int_in_type_cache (tree type, const wide_int &cst,
			      int slot, int max_slots)
{
  gcc_checking_assert (slot >= 0);
  /* Initialize cache.  */
  if (!TYPE_CACHED_VALUES_P (type))
    {
      TYPE_CACHED_VALUES_P (type) = 1;
      TYPE_CACHED_VALUES (type) = make_tree_vec (max_slots);
    }
  tree t = TREE_VEC_ELT (TYPE_CACHED_VALUES (type), slot);
  if (!t)
    {
      /* Create a new shared int.  */
      t = build_new_int_cst (type, cst);
      TREE_VEC_ELT (TYPE_CACHED_VALUES (type), slot) = t;
    }
  return t;
}

/* Create an INT_CST node of TYPE and value CST.
   The returned node is always shared.  For small integers we use a
   per-type vector cache, for larger ones we use a single hash table.
   The value is extended from its precision according to the sign of
   the type to be a multiple of HOST_BITS_PER_WIDE_INT.  This defines
   the upper bits and ensures that hashing and value equality based
   upon the underlying HOST_WIDE_INTs works without masking.  */

static tree
wide_int_to_tree_1 (tree type, const wide_int_ref &pcst)
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
      if (pcst.elt (l - 1) == HOST_WIDE_INT_M1)
	gcc_checking_assert (pcst.elt (l - 2) >= 0);
    }

  wide_int cst = wide_int::from (pcst, prec, sgn);
  unsigned int ext_len = get_int_cst_ext_nunits (type, cst);

  enum tree_code code = TREE_CODE (type);
  if (code == POINTER_TYPE || code == REFERENCE_TYPE)
    {
      /* Cache NULL pointer and zero bounds.  */
      if (cst == 0)
	ix = 0;
      /* Cache upper bounds of pointers.  */
      else if (cst == wi::max_value (prec, sgn))
	ix = 1;
      /* Cache 1 which is used for a non-zero range.  */
      else if (cst == 1)
	ix = 2;

      if (ix >= 0)
	{
	  t = cache_wide_int_in_type_cache (type, cst, ix, 3);
	  /* Make sure no one is clobbering the shared constant.  */
	  gcc_checking_assert (TREE_TYPE (t) == type
			       && cst == wi::to_wide (t));
	  return t;
	}
    }
  if (ext_len == 1)
    {
      /* We just need to store a single HOST_WIDE_INT.  */
      HOST_WIDE_INT hwi;
      if (TYPE_UNSIGNED (type))
	hwi = cst.to_uhwi ();
      else
	hwi = cst.to_shwi ();

      switch (code)
	{
	case NULLPTR_TYPE:
	  gcc_assert (hwi == 0);
	  /* Fallthru.  */

	case POINTER_TYPE:
	case REFERENCE_TYPE:
	  /* Ignore pointers, as they were already handled above.  */
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
	      limit = param_integer_share_limit;
	      if (IN_RANGE (hwi, 0, param_integer_share_limit - 1))
		ix = hwi;
	    }
	  else
	    {
	      /* Cache [-1, N).  */
	      limit = param_integer_share_limit + 1;
	      if (IN_RANGE (hwi, -1, param_integer_share_limit - 1))
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
	  t = cache_wide_int_in_type_cache (type, cst, ix, limit);
	  /* Make sure no one is clobbering the shared constant.  */
	  gcc_checking_assert (TREE_TYPE (t) == type
			       && TREE_INT_CST_NUNITS (t) == 1
			       && TREE_INT_CST_OFFSET_NUNITS (t) == 1
			       && TREE_INT_CST_EXT_NUNITS (t) == 1
			       && TREE_INT_CST_ELT (t, 0) == hwi);
	  return t;
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
      else
	ggc_free (nt);
    }

  return t;
}

hashval_t
poly_int_cst_hasher::hash (tree t)
{
  inchash::hash hstate;

  hstate.add_int (TYPE_UID (TREE_TYPE (t)));
  for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; ++i)
    hstate.add_wide_int (wi::to_wide (POLY_INT_CST_COEFF (t, i)));

  return hstate.end ();
}

bool
poly_int_cst_hasher::equal (tree x, const compare_type &y)
{
  if (TREE_TYPE (x) != y.first)
    return false;
  for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; ++i)
    if (wi::to_wide (POLY_INT_CST_COEFF (x, i)) != y.second->coeffs[i])
      return false;
  return true;
}

/* Build a POLY_INT_CST node with type TYPE and with the elements in VALUES.
   The elements must also have type TYPE.  */

tree
build_poly_int_cst (tree type, const poly_wide_int_ref &values)
{
  unsigned int prec = TYPE_PRECISION (type);
  gcc_assert (prec <= values.coeffs[0].get_precision ());
  poly_wide_int c = poly_wide_int::from (values, prec, SIGNED);

  inchash::hash h;
  h.add_int (TYPE_UID (type));
  for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; ++i)
    h.add_wide_int (c.coeffs[i]);
  poly_int_cst_hasher::compare_type comp (type, &c);
  tree *slot = poly_int_cst_hash_table->find_slot_with_hash (comp, h.end (),
							     INSERT);
  if (*slot == NULL_TREE)
    {
      tree coeffs[NUM_POLY_INT_COEFFS];
      for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; ++i)
	coeffs[i] = wide_int_to_tree_1 (type, c.coeffs[i]);
      *slot = build_new_poly_int_cst (type, coeffs);
    }
  return *slot;
}

/* Create a constant tree with value VALUE in type TYPE.  */

tree
wide_int_to_tree (tree type, const poly_wide_int_ref &value)
{
  if (value.is_constant ())
    return wide_int_to_tree_1 (type, value.coeffs[0]);
  return build_poly_int_cst (type, value);
}

/* Insert INTEGER_CST T into a cache of integer constants.  And return
   the cached constant (which may or may not be T).  If MIGHT_DUPLICATE
   is false, and T falls into the type's 'smaller values' range, there
   cannot be an existing entry.  Otherwise, if MIGHT_DUPLICATE is true,
   or the value is large, should an existing entry exist, it is
   returned (rather than inserting T).  */

tree
cache_integer_cst (tree t, bool might_duplicate ATTRIBUTE_UNUSED)
{
  tree type = TREE_TYPE (t);
  int ix = -1;
  int limit = 0;
  int prec = TYPE_PRECISION (type);

  gcc_assert (!TREE_OVERFLOW (t));

  /* The caching indices here must match those in
     wide_int_to_type_1.  */
  switch (TREE_CODE (type))
    {
    case NULLPTR_TYPE:
      gcc_checking_assert (integer_zerop (t));
      /* Fallthru.  */

    case POINTER_TYPE:
    case REFERENCE_TYPE:
      {
	if (integer_zerop (t))
	  ix = 0;
	else if (integer_onep (t))
	  ix = 2;

	if (ix >= 0)
	  limit = 3;
      }
      break;

    case BOOLEAN_TYPE:
      /* Cache false or true.  */
      limit = 2;
      if (wi::ltu_p (wi::to_wide (t), 2))
	ix = TREE_INT_CST_ELT (t, 0);
      break;

    case INTEGER_TYPE:
    case OFFSET_TYPE:
      if (TYPE_UNSIGNED (type))
	{
	  /* Cache 0..N */
	  limit = param_integer_share_limit;

	  /* This is a little hokie, but if the prec is smaller than
	     what is necessary to hold param_integer_share_limit, then the
	     obvious test will not get the correct answer.  */
	  if (prec < HOST_BITS_PER_WIDE_INT)
	    {
	      if (tree_to_uhwi (t)
		  < (unsigned HOST_WIDE_INT) param_integer_share_limit)
		ix = tree_to_uhwi (t);
	    }
	  else if (wi::ltu_p (wi::to_wide (t), param_integer_share_limit))
	    ix = tree_to_uhwi (t);
	}
      else
	{
	  /* Cache -1..N */
	  limit = param_integer_share_limit + 1;

	  if (integer_minus_onep (t))
	    ix = 0;
	  else if (!wi::neg_p (wi::to_wide (t)))
	    {
	      if (prec < HOST_BITS_PER_WIDE_INT)
		{
		  if (tree_to_shwi (t) < param_integer_share_limit)
		    ix = tree_to_shwi (t) + 1;
		}
	      else if (wi::ltu_p (wi::to_wide (t), param_integer_share_limit))
		ix = tree_to_shwi (t) + 1;
	    }
	}
      break;

    case ENUMERAL_TYPE:
      /* The slot used by TYPE_CACHED_VALUES is used for the enum
	 members.  */
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

      if (tree r = TREE_VEC_ELT (TYPE_CACHED_VALUES (type), ix))
	{
	  gcc_checking_assert (might_duplicate);
	  t = r;
	}
      else
	TREE_VEC_ELT (TYPE_CACHED_VALUES (type), ix) = t;
    }
  else
    {
      /* Use the cache of larger shared ints.  */
      tree *slot = int_cst_hash_table->find_slot (t, INSERT);
      if (tree r = *slot)
	{
	  /* If there is already an entry for the number verify it's the
	     same value.  */
	  gcc_checking_assert (wi::to_wide (tree (r)) == wi::to_wide (t));
	  /* And return the cached value.  */
	  t = r;
	}
      else
	/* Otherwise insert this one into the hash table.  */
	*slot = t;
    }

  return t;
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
	  && (tree_fits_shwi_p (x) || tree_fits_uhwi_p (x)));
}

/* Build a newly constructed VECTOR_CST with the given values of
   (VECTOR_CST_)LOG2_NPATTERNS and (VECTOR_CST_)NELTS_PER_PATTERN.  */

tree
make_vector (unsigned log2_npatterns,
	     unsigned int nelts_per_pattern MEM_STAT_DECL)
{
  gcc_assert (IN_RANGE (nelts_per_pattern, 1, 3));
  tree t;
  unsigned npatterns = 1 << log2_npatterns;
  unsigned encoded_nelts = npatterns * nelts_per_pattern;
  unsigned length = (sizeof (struct tree_vector)
		     + (encoded_nelts - 1) * sizeof (tree));

  record_node_allocation_statistics (VECTOR_CST, length);

  t = ggc_alloc_cleared_tree_node_stat (length PASS_MEM_STAT);

  TREE_SET_CODE (t, VECTOR_CST);
  TREE_CONSTANT (t) = 1;
  VECTOR_CST_LOG2_NPATTERNS (t) = log2_npatterns;
  VECTOR_CST_NELTS_PER_PATTERN (t) = nelts_per_pattern;

  return t;
}

/* Return a new VECTOR_CST node whose type is TYPE and whose values
   are extracted from V, a vector of CONSTRUCTOR_ELT.  */

tree
build_vector_from_ctor (tree type, const vec<constructor_elt, va_gc> *v)
{
  if (vec_safe_length (v) == 0)
    return build_zero_cst (type);

  unsigned HOST_WIDE_INT idx, nelts;
  tree value;

  /* We can't construct a VECTOR_CST for a variable number of elements.  */
  nelts = TYPE_VECTOR_SUBPARTS (type).to_constant ();
  tree_vector_builder vec (type, nelts, 1);
  FOR_EACH_CONSTRUCTOR_VALUE (v, idx, value)
    {
      if (TREE_CODE (value) == VECTOR_CST)
	{
	  /* If NELTS is constant then this must be too.  */
	  unsigned int sub_nelts = VECTOR_CST_NELTS (value).to_constant ();
	  for (unsigned i = 0; i < sub_nelts; ++i)
	    vec.quick_push (VECTOR_CST_ELT (value, i));
	}
      else
	vec.quick_push (value);
    }
  while (vec.length () < nelts)
    vec.quick_push (build_zero_cst (TREE_TYPE (type)));

  return vec.build ();
}

/* Build a vector of type VECTYPE where all the elements are SCs.  */
tree
build_vector_from_val (tree vectype, tree sc)
{
  unsigned HOST_WIDE_INT i, nunits;

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
      tree_vector_builder v (vectype, 1, 1);
      v.quick_push (sc);
      return v.build ();
    }
  else if (!TYPE_VECTOR_SUBPARTS (vectype).is_constant (&nunits))
    return fold_build1 (VEC_DUPLICATE_EXPR, vectype, sc);
  else
    {
      vec<constructor_elt, va_gc> *v;
      vec_alloc (v, nunits);
      for (i = 0; i < nunits; ++i)
	CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, sc);
      return build_constructor (vectype, v);
    }
}

/* If TYPE is not a vector type, just return SC, otherwise return
   build_vector_from_val (TYPE, SC).  */

tree
build_uniform_cst (tree type, tree sc)
{
  if (!VECTOR_TYPE_P (type))
    return sc;

  return build_vector_from_val (type, sc);
}

/* Build a vector series of type TYPE in which element I has the value
   BASE + I * STEP.  The result is a constant if BASE and STEP are constant
   and a VEC_SERIES_EXPR otherwise.  */

tree
build_vec_series (tree type, tree base, tree step)
{
  if (integer_zerop (step))
    return build_vector_from_val (type, base);
  if (TREE_CODE (base) == INTEGER_CST && TREE_CODE (step) == INTEGER_CST)
    {
      tree_vector_builder builder (type, 1, 3);
      tree elt1 = wide_int_to_tree (TREE_TYPE (base),
				    wi::to_wide (base) + wi::to_wide (step));
      tree elt2 = wide_int_to_tree (TREE_TYPE (base),
				    wi::to_wide (elt1) + wi::to_wide (step));
      builder.quick_push (base);
      builder.quick_push (elt1);
      builder.quick_push (elt2);
      return builder.build ();
    }
  return build2 (VEC_SERIES_EXPR, type, base, step);
}

/* Return a vector with the same number of units and number of bits
   as VEC_TYPE, but in which the elements are a linear series of unsigned
   integers { BASE, BASE + STEP, BASE + STEP * 2, ... }.  */

tree
build_index_vector (tree vec_type, poly_uint64 base, poly_uint64 step)
{
  tree index_vec_type = vec_type;
  tree index_elt_type = TREE_TYPE (vec_type);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vec_type);
  if (!INTEGRAL_TYPE_P (index_elt_type) || !TYPE_UNSIGNED (index_elt_type))
    {
      index_elt_type = build_nonstandard_integer_type
	(GET_MODE_BITSIZE (SCALAR_TYPE_MODE (index_elt_type)), true);
      index_vec_type = build_vector_type (index_elt_type, nunits);
    }

  tree_vector_builder v (index_vec_type, 1, 3);
  for (unsigned int i = 0; i < 3; ++i)
    v.quick_push (build_int_cstu (index_elt_type, base + i * step));
  return v.build ();
}

/* Return a VECTOR_CST of type VEC_TYPE in which the first NUM_A
   elements are A and the rest are B.  */

tree
build_vector_a_then_b (tree vec_type, unsigned int num_a, tree a, tree b)
{
  gcc_assert (known_le (num_a, TYPE_VECTOR_SUBPARTS (vec_type)));
  unsigned int count = constant_lower_bound (TYPE_VECTOR_SUBPARTS (vec_type));
  /* Optimize the constant case.  */
  if ((count & 1) == 0 && TYPE_VECTOR_SUBPARTS (vec_type).is_constant ())
    count /= 2;
  tree_vector_builder builder (vec_type, count, 2);
  for (unsigned int i = 0; i < count * 2; ++i)
    builder.quick_push (i < num_a ? a : b);
  return builder.build ();
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
build_constructor (tree type, vec<constructor_elt, va_gc> *vals MEM_STAT_DECL)
{
  tree c = make_node (CONSTRUCTOR PASS_MEM_STAT);

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

/* Return a new CONSTRUCTOR node whose type is TYPE and whose values
   are in a vector pointed to by VALS.  Note that the TREE_PURPOSE
   fields in the constructor remain null.  */

tree
build_constructor_from_vec (tree type, const vec<tree, va_gc> *vals)
{
  vec<constructor_elt, va_gc> *v = NULL;

  for (tree t : vals)
    CONSTRUCTOR_APPEND_ELT (v, NULL_TREE, t);

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

/* Return a node of type TYPE for which TREE_CLOBBER_P is true.  */

tree
build_clobber (tree type, enum clobber_kind kind)
{
  tree clobber = build_constructor (type, NULL);
  TREE_THIS_VOLATILE (clobber) = true;
  CLOBBER_KIND (clobber) = kind;
  return clobber;
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
  int overflow = 0;

  /* dconst{0,1,2,m1,half} are used in various places in
     the middle-end and optimizers, allow them here
     even for decimal floating point types as an exception
     by converting them to decimal.  */
  if (DECIMAL_FLOAT_MODE_P (TYPE_MODE (type))
      && (d.cl == rvc_normal || d.cl == rvc_zero)
      && !d.decimal)
    {
      if (memcmp (&d, &dconst1, sizeof (d)) == 0)
	decimal_real_from_string (&d, "1");
      else if (memcmp (&d, &dconst2, sizeof (d)) == 0)
	decimal_real_from_string (&d, "2");
      else if (memcmp (&d, &dconstm1, sizeof (d)) == 0)
	decimal_real_from_string (&d, "-1");
      else if (memcmp (&d, &dconsthalf, sizeof (d)) == 0)
	decimal_real_from_string (&d, "0.5");
      else if (memcmp (&d, &dconst0, sizeof (d)) == 0)
	{
	  /* Make sure to give zero the minimum quantum exponent for
	     the type (which corresponds to all bits zero).  */
	  const struct real_format *fmt = REAL_MODE_FORMAT (TYPE_MODE (type));
	  char buf[16];
	  sprintf (buf, "0e%d", fmt->emin - fmt->p);
	  decimal_real_from_string (&d, buf);
	}
      else
	gcc_unreachable ();
    }

  /* ??? Used to check for overflow here via CHECK_FLOAT_TYPE.
     Consider doing it via real_convert now.  */

  v = make_node (REAL_CST);
  TREE_TYPE (v) = type;
  memcpy (TREE_REAL_CST_PTR (v), &d, sizeof (REAL_VALUE_TYPE));
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

  real_from_integer (&d, type ? TYPE_MODE (type) : VOIDmode, wi::to_wide (i),
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

/* Return a new REAL_CST node whose type is TYPE
   and whose value is the integer value I which has sign SGN.  */

tree
build_real_from_wide (tree type, const wide_int_ref &i, signop sgn)
{
  REAL_VALUE_TYPE d;

  /* Clear all bits of the real value type so that we can later do
     bitwise comparisons to see if two values are the same.  */
  memset (&d, 0, sizeof d);

  real_from_integer (&d, TYPE_MODE (type), i, sgn);
  return build_real (type, d);
}

/* Return a newly constructed STRING_CST node whose value is the LEN
   characters at STR when STR is nonnull, or all zeros otherwise.
   Note that for a C string literal, LEN should include the trailing NUL.
   The TREE_TYPE is not initialized.  */

tree
build_string (unsigned len, const char *str /*= NULL */)
{
  /* Do not waste bytes provided by padding of struct tree_string.  */
  unsigned size = len + offsetof (struct tree_string, str) + 1;

  record_node_allocation_statistics (STRING_CST, size);

  tree s = (tree) ggc_internal_alloc (size);

  memset (s, 0, sizeof (struct tree_typed));
  TREE_SET_CODE (s, STRING_CST);
  TREE_CONSTANT (s) = 1;
  TREE_STRING_LENGTH (s) = len;
  if (str)
    memcpy (s->string.str, str, len);
  else
    memset (s->string.str, 0, len);
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
  gcc_assert (CONSTANT_CLASS_P (real));
  gcc_assert (CONSTANT_CLASS_P (imag));

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
  REAL_VALUE_TYPE rzero = dconst0;

  rzero.sign = neg;
  return build_complex (type, build_real (TREE_TYPE (type), dconstinf),
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
      return build_fixed (type,
			  fixed_from_double_int (double_int_minus_one,
						 SCALAR_TYPE_MODE (type)));

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

/* If floating-point type TYPE has an IEEE-style sign bit, return an
   unsigned constant in which only the sign bit is set.  Return null
   otherwise.  */

tree
sign_mask_for (tree type)
{
  /* Avoid having to choose between a real-only sign and a pair of signs.
     This could be relaxed if the choice becomes obvious later.  */
  if (TREE_CODE (type) == COMPLEX_TYPE)
    return NULL_TREE;

  auto eltmode = as_a<scalar_float_mode> (element_mode (type));
  auto bits = REAL_MODE_FORMAT (eltmode)->ieee_bits;
  if (!bits || !pow2p_hwi (bits))
    return NULL_TREE;

  tree inttype = unsigned_type_for (type);
  if (!inttype)
    return NULL_TREE;

  auto mask = wi::set_bit_in_zero (bits - 1, bits);
  if (TREE_CODE (inttype) == VECTOR_TYPE)
    {
      tree elt = wide_int_to_tree (TREE_TYPE (inttype), mask);
      return build_vector_from_val (inttype, elt);
    }
  return wide_int_to_tree (inttype, mask);
}

/* Build a BINFO with LEN language slots.  */

tree
make_tree_binfo (unsigned base_binfos MEM_STAT_DECL)
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
make_int_cst (int len, int ext_len MEM_STAT_DECL)
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
make_tree_vec (int len MEM_STAT_DECL)
{
  tree t;
  size_t length = (len - 1) * sizeof (tree) + sizeof (struct tree_vec);

  record_node_allocation_statistics (TREE_VEC, length);

  t = ggc_alloc_cleared_tree_node_stat (length PASS_MEM_STAT);

  TREE_SET_CODE (t, TREE_VEC);
  TREE_VEC_LENGTH (t) = len;

  return t;
}

/* Grow a TREE_VEC node to new length LEN.  */

tree
grow_tree_vec (tree v, int len MEM_STAT_DECL)
{
  gcc_assert (TREE_CODE (v) == TREE_VEC);

  int oldlen = TREE_VEC_LENGTH (v);
  gcc_assert (len > oldlen);

  size_t oldlength = (oldlen - 1) * sizeof (tree) + sizeof (struct tree_vec);
  size_t length = (len - 1) * sizeof (tree) + sizeof (struct tree_vec);

  record_node_allocation_statistics (TREE_VEC, length - oldlength);

  v = (tree) ggc_realloc (v, length PASS_MEM_STAT);

  TREE_VEC_LENGTH (v) = len;

  return v;
}

/* Return 1 if EXPR is the constant zero, whether it is integral, float or
   fixed, and scalar, complex or vector.  */

bool
zerop (const_tree expr)
{
  return (integer_zerop (expr)
	  || real_zerop (expr)
	  || fixed_zerop (expr));
}

/* Return 1 if EXPR is the integer constant zero or a complex constant
   of zero, or a location wrapper for such a constant.  */

bool
integer_zerop (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);

  switch (TREE_CODE (expr))
    {
    case INTEGER_CST:
      return wi::to_wide (expr) == 0;
    case COMPLEX_CST:
      return (integer_zerop (TREE_REALPART (expr))
	      && integer_zerop (TREE_IMAGPART (expr)));
    case VECTOR_CST:
      return (VECTOR_CST_NPATTERNS (expr) == 1
	      && VECTOR_CST_DUPLICATE_P (expr)
	      && integer_zerop (VECTOR_CST_ENCODED_ELT (expr, 0)));
    default:
      return false;
    }
}

/* Return 1 if EXPR is the integer constant one or the corresponding
   complex constant, or a location wrapper for such a constant.  */

bool
integer_onep (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);

  switch (TREE_CODE (expr))
    {
    case INTEGER_CST:
      return wi::eq_p (wi::to_widest (expr), 1);
    case COMPLEX_CST:
      return (integer_onep (TREE_REALPART (expr))
	      && integer_zerop (TREE_IMAGPART (expr)));
    case VECTOR_CST:
      return (VECTOR_CST_NPATTERNS (expr) == 1
	      && VECTOR_CST_DUPLICATE_P (expr)
	      && integer_onep (VECTOR_CST_ENCODED_ELT (expr, 0)));
    default:
      return false;
    }
}

/* Return 1 if EXPR is the integer constant one.  For complex and vector,
   return 1 if every piece is the integer constant one.
   Also return 1 for location wrappers for such a constant.  */

bool
integer_each_onep (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);

  if (TREE_CODE (expr) == COMPLEX_CST)
    return (integer_onep (TREE_REALPART (expr))
	    && integer_onep (TREE_IMAGPART (expr)));
  else
    return integer_onep (expr);
}

/* Return 1 if EXPR is an integer containing all 1's in as much precision as
   it contains, or a complex or vector whose subparts are such integers,
   or a location wrapper for such a constant.  */

bool
integer_all_onesp (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);

  if (TREE_CODE (expr) == COMPLEX_CST
      && integer_all_onesp (TREE_REALPART (expr))
      && integer_all_onesp (TREE_IMAGPART (expr)))
    return true;

  else if (TREE_CODE (expr) == VECTOR_CST)
    return (VECTOR_CST_NPATTERNS (expr) == 1
	    && VECTOR_CST_DUPLICATE_P (expr)
	    && integer_all_onesp (VECTOR_CST_ENCODED_ELT (expr, 0)));

  else if (TREE_CODE (expr) != INTEGER_CST)
    return false;

  return (wi::max_value (TYPE_PRECISION (TREE_TYPE (expr)), UNSIGNED)
	  == wi::to_wide (expr));
}

/* Return 1 if EXPR is the integer constant minus one, or a location wrapper
   for such a constant.  */

bool
integer_minus_onep (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);

  if (TREE_CODE (expr) == COMPLEX_CST)
    return (integer_all_onesp (TREE_REALPART (expr))
	    && integer_zerop (TREE_IMAGPART (expr)));
  else
    return integer_all_onesp (expr);
}

/* Return 1 if EXPR is an integer constant that is a power of 2 (i.e., has only
   one bit on), or a location wrapper for such a constant.  */

bool
integer_pow2p (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);

  if (TREE_CODE (expr) == COMPLEX_CST
      && integer_pow2p (TREE_REALPART (expr))
      && integer_zerop (TREE_IMAGPART (expr)))
    return true;

  if (TREE_CODE (expr) != INTEGER_CST)
    return false;

  return wi::popcount (wi::to_wide (expr)) == 1;
}

/* Return 1 if EXPR is an integer constant other than zero or a
   complex constant other than zero, or a location wrapper for such a
   constant.  */

bool
integer_nonzerop (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);

  return ((TREE_CODE (expr) == INTEGER_CST
	   && wi::to_wide (expr) != 0)
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && (integer_nonzerop (TREE_REALPART (expr))
		  || integer_nonzerop (TREE_IMAGPART (expr)))));
}

/* Return 1 if EXPR is the integer constant one.  For vector,
   return 1 if every piece is the integer constant minus one
   (representing the value TRUE).
   Also return 1 for location wrappers for such a constant.  */

bool
integer_truep (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);

  if (TREE_CODE (expr) == VECTOR_CST)
    return integer_all_onesp (expr);
  return integer_onep (expr);
}

/* Return 1 if EXPR is the fixed-point constant zero, or a location wrapper
   for such a constant.  */

bool
fixed_zerop (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);

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

  return wi::exact_log2 (wi::to_wide (expr));
}

/* Similar, but return the largest integer Y such that 2 ** Y is less
   than or equal to EXPR.  */

int
tree_floor_log2 (const_tree expr)
{
  if (TREE_CODE (expr) == COMPLEX_CST)
    return tree_log2 (TREE_REALPART (expr));

  return wi::floor_log2 (wi::to_wide (expr));
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
      ret1 = wi::ctz (wi::to_wide (expr));
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
   decimal float constants, so don't return 1 for them.
   Also return 1 for location wrappers around such a constant.  */

bool
real_zerop (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);

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
	/* Don't simply check for a duplicate because the predicate
	   accepts both +0.0 and -0.0.  */
	unsigned count = vector_cst_encoded_nelts (expr);
	for (unsigned int i = 0; i < count; ++i)
	  if (!real_zerop (VECTOR_CST_ENCODED_ELT (expr, i)))
	    return false;
	return true;
      }
    default:
      return false;
    }
}

/* Return 1 if EXPR is the real constant one in real or complex form.
   Trailing zeroes matter for decimal float constants, so don't return
   1 for them.
   Also return 1 for location wrappers around such a constant.  */

bool
real_onep (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);

  switch (TREE_CODE (expr))
    {
    case REAL_CST:
      return real_equal (&TREE_REAL_CST (expr), &dconst1)
	     && !(DECIMAL_FLOAT_MODE_P (TYPE_MODE (TREE_TYPE (expr))));
    case COMPLEX_CST:
      return real_onep (TREE_REALPART (expr))
	     && real_zerop (TREE_IMAGPART (expr));
    case VECTOR_CST:
      return (VECTOR_CST_NPATTERNS (expr) == 1
	      && VECTOR_CST_DUPLICATE_P (expr)
	      && real_onep (VECTOR_CST_ENCODED_ELT (expr, 0)));
    default:
      return false;
    }
}

/* Return 1 if EXPR is the real constant minus one.  Trailing zeroes
   matter for decimal float constants, so don't return 1 for them.
   Also return 1 for location wrappers around such a constant.  */

bool
real_minus_onep (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);

  switch (TREE_CODE (expr))
    {
    case REAL_CST:
      return real_equal (&TREE_REAL_CST (expr), &dconstm1)
	     && !(DECIMAL_FLOAT_MODE_P (TYPE_MODE (TREE_TYPE (expr))));
    case COMPLEX_CST:
      return real_minus_onep (TREE_REALPART (expr))
	     && real_zerop (TREE_IMAGPART (expr));
    case VECTOR_CST:
      return (VECTOR_CST_NPATTERNS (expr) == 1
	      && VECTOR_CST_DUPLICATE_P (expr)
	      && real_minus_onep (VECTOR_CST_ENCODED_ELT (expr, 0)));
    default:
      return false;
    }
}

/* Return true if T could be a floating point zero.  */

bool
real_maybe_zerop (const_tree expr)
{
  switch (TREE_CODE (expr))
    {
    case REAL_CST:
      /* Can't use real_zerop here, as it always returns false for decimal
	 floats.  And can't use TREE_REAL_CST (expr).cl == rvc_zero
	 either, as decimal zeros are rvc_normal.  */
      return real_equal (&TREE_REAL_CST (expr), &dconst0);
    case COMPLEX_CST:
      return (real_maybe_zerop (TREE_REALPART (expr))
	      || real_maybe_zerop (TREE_IMAGPART (expr)));
    case VECTOR_CST:
      {
	unsigned count = vector_cst_encoded_nelts (expr);
	for (unsigned int i = 0; i < count; ++i)
	  if (real_maybe_zerop (VECTOR_CST_ENCODED_ELT (expr, i)))
	    return true;
	return false;
      }
    default:
      /* Perhaps for SSA_NAMEs we could query frange.  */
      return true;
    }
}

/* Nonzero if EXP is a constant or a cast of a constant.  */

bool
really_constant_p (const_tree exp)
{
  /* This is not quite the same as STRIP_NOPS.  It does more.  */
  while (CONVERT_EXPR_P (exp)
	 || TREE_CODE (exp) == NON_LVALUE_EXPR)
    exp = TREE_OPERAND (exp, 0);
  return TREE_CONSTANT (exp);
}

/* Return true if T holds a polynomial pointer difference, storing it in
   *VALUE if so.  A true return means that T's precision is no greater
   than 64 bits, which is the largest address space we support, so *VALUE
   never loses precision.  However, the signedness of the result does
   not necessarily match the signedness of T: sometimes an unsigned type
   like sizetype is used to encode a value that is actually negative.  */

bool
ptrdiff_tree_p (const_tree t, poly_int64_pod *value)
{
  if (!t)
    return false;
  if (TREE_CODE (t) == INTEGER_CST)
    {
      if (!cst_and_fits_in_hwi (t))
	return false;
      *value = int_cst_value (t);
      return true;
    }
  if (POLY_INT_CST_P (t))
    {
      for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; ++i)
	if (!cst_and_fits_in_hwi (POLY_INT_CST_COEFF (t, i)))
	  return false;
      for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; ++i)
	value->coeffs[i] = int_cst_value (POLY_INT_CST_COEFF (t, i));
      return true;
    }
  return false;
}

poly_int64
tree_to_poly_int64 (const_tree t)
{
  gcc_assert (tree_fits_poly_int64_p (t));
  if (POLY_INT_CST_P (t))
    return poly_int_cst_value (t).force_shwi ();
  return TREE_INT_CST_LOW (t);
}

poly_uint64
tree_to_poly_uint64 (const_tree t)
{
  gcc_assert (tree_fits_poly_uint64_p (t));
  if (POLY_INT_CST_P (t))
    return poly_int_cst_value (t).force_uhwi ();
  return TREE_INT_CST_LOW (t);
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

bool
chain_member (const_tree elem, const_tree chain)
{
  while (chain)
    {
      if (elem == chain)
	return true;
      chain = DECL_CHAIN (chain);
    }

  return false;
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

/* Returns the last FIELD_DECL in the TYPE_FIELDS of the RECORD_TYPE or
   UNION_TYPE TYPE, or NULL_TREE if none.  */

tree
last_field (const_tree type)
{
  tree last = NULL_TREE;

  for (tree fld = TYPE_FIELDS (type); fld; fld = TREE_CHAIN (fld))
    {
      if (TREE_CODE (fld) != FIELD_DECL)
	continue;

      last = fld;
    }

  return last;
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
build_tree_list (tree parm, tree value MEM_STAT_DECL)
{
  tree t = make_node (TREE_LIST PASS_MEM_STAT);
  TREE_PURPOSE (t) = parm;
  TREE_VALUE (t) = value;
  return t;
}

/* Build a chain of TREE_LIST nodes from a vector.  */

tree
build_tree_list_vec (const vec<tree, va_gc> *vec MEM_STAT_DECL)
{
  tree ret = NULL_TREE;
  tree *pp = &ret;
  unsigned int i;
  tree t;
  FOR_EACH_VEC_SAFE_ELT (vec, i, t)
    {
      *pp = build_tree_list (NULL, t PASS_MEM_STAT);
      pp = &TREE_CHAIN (*pp);
    }
  return ret;
}

/* Return a newly created TREE_LIST node whose
   purpose and value fields are PURPOSE and VALUE
   and whose TREE_CHAIN is CHAIN.  */

tree 
tree_cons (tree purpose, tree value, tree chain MEM_STAT_DECL)
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
    {
      /* zero sized arrays are represented from C FE as complete types with
	 NULL TYPE_MAX_VALUE and zero TYPE_SIZE, while C++ FE represents
	 them as min 0, max -1.  */
      if (COMPLETE_TYPE_P (type)
	  && integer_zerop (TYPE_SIZE (type))
	  && integer_zerop (min))
	return build_int_cst (TREE_TYPE (min), -1);

      return error_mark_node;
    }

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
  tree inner;

  /* If the tree evaluates to a constant, then we don't want to hide that
     fact (i.e. this allows further folding, and direct checks for constants).
     However, a read-only object that has side effects cannot be bypassed.
     Since it is no problem to reevaluate literals, we just return the
     literal node.  */
  inner = skip_simple_arithmetic (expr);
  if (TREE_CODE (inner) == ERROR_MARK)
    return inner;

  if (tree_invariant_p_1 (inner))
    return expr;

  /* If INNER contains a PLACEHOLDER_EXPR, we must evaluate it each time, since
     it means that the size or offset of some field of an object depends on
     the value within another field.

     Note that it must not be the case that EXPR contains both a PLACEHOLDER_EXPR
     and some variable since it would then need to be both evaluated once and
     evaluated more than once.  Front-ends must assure this case cannot
     happen by surrounding any such subexpressions in their own SAVE_EXPR
     and forcing evaluation at the proper time.  */
  if (contains_placeholder_p (inner))
    return expr;

  expr = build1_loc (EXPR_LOCATION (expr), SAVE_EXPR, TREE_TYPE (expr), expr);

  /* This expression might be placed ahead of a jump to ensure that the
     value was computed on both sides of the jump.  So make sure it isn't
     eliminated as dead.  */
  TREE_SIDE_EFFECTS (expr) = 1;
  return expr;
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
    case OPAQUE_TYPE:
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

        /* Fall through.  */

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

        /* Fall through.  */

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

	  /* If we are trying to replace F with a constant or with another
	     instance of one of the arguments of the call, inline back
	     functions which do nothing else than computing a value from
	     the arguments they are passed.  This makes it possible to
	     fold partially or entirely the replacement expression.  */
	  if (code == CALL_EXPR)
	    {
	      bool maybe_inline = false;
	      if (CONSTANT_CLASS_P (r))
		maybe_inline = true;
	      else
		for (i = 3; i < TREE_OPERAND_LENGTH (exp); i++)
		  if (operand_equal_p (TREE_OPERAND (exp, i), r, 0))
		    {
		      maybe_inline = true;
		      break;
		    }
	      if (maybe_inline)
		{
		  tree t = maybe_inline_call_in_expr (exp);
		  if (t)
		    return SUBSTITUTE_IN_EXPR (t, f, r);
		}
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
      /* Always wrap STATEMENT_LIST into SAVE_EXPR, even if it doesn't
	 have side-effects.  */
      if (code == STATEMENT_LIST)
	return save_expr (e);
      /* FALLTHRU */
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
  protected_set_expr_location (result, EXPR_LOCATION (ref));

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
build0 (enum tree_code code, tree tt MEM_STAT_DECL)
{
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 0);

  t = make_node (code PASS_MEM_STAT);
  TREE_TYPE (t) = tt;

  return t;
}

tree
build1 (enum tree_code code, tree type, tree node MEM_STAT_DECL)
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
    {
      if (code != DEBUG_BEGIN_STMT)
	TREE_SIDE_EFFECTS (t) = 1;
    }
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
build2 (enum tree_code code, tree tt, tree arg0, tree arg1 MEM_STAT_DECL)
{
  bool constant, read_only, side_effects, div_by_zero;
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

  t = make_node (code PASS_MEM_STAT);
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

  switch (code)
    {
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case TRUNC_MOD_EXPR:
      div_by_zero = integer_zerop (arg1);
      break;
    default:
      div_by_zero = false;
    }

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
      /* Don't mark X / 0 as constant.  */
      TREE_CONSTANT (t) = constant && !div_by_zero;
      TREE_THIS_VOLATILE (t)
	= (TREE_CODE_CLASS (code) == tcc_reference
	   && arg0 && TREE_THIS_VOLATILE (arg0));
    }

  return t;
}


tree
build3 (enum tree_code code, tree tt, tree arg0, tree arg1,
	tree arg2 MEM_STAT_DECL)
{
  bool constant, read_only, side_effects;
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 3);
  gcc_assert (TREE_CODE_CLASS (code) != tcc_vl_exp);

  t = make_node (code PASS_MEM_STAT);
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
build4 (enum tree_code code, tree tt, tree arg0, tree arg1,
	tree arg2, tree arg3 MEM_STAT_DECL)
{
  bool constant, read_only, side_effects;
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 4);

  t = make_node (code PASS_MEM_STAT);
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
build5 (enum tree_code code, tree tt, tree arg0, tree arg1,
	tree arg2, tree arg3, tree arg4 MEM_STAT_DECL)
{
  bool constant, read_only, side_effects;
  tree t;

  gcc_assert (TREE_CODE_LENGTH (code) == 5);

  t = make_node (code PASS_MEM_STAT);
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
  poly_int64 offset = 0;
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
      if (TREE_CODE (ptr) == MEM_REF)
	{
	  offset += mem_ref_offset (ptr).force_shwi ();
	  ptr = TREE_OPERAND (ptr, 0);
	}
      else
	ptr = build_fold_addr_expr (ptr);
      gcc_assert (is_gimple_reg (ptr) || is_gimple_min_invariant (ptr));
    }
  tem = build2 (MEM_REF, TREE_TYPE (ptype),
		ptr, build_int_cst (ptype, offset));
  SET_EXPR_LOCATION (tem, loc);
  return tem;
}

/* Return the constant offset of a MEM_REF or TARGET_MEM_REF tree T.  */

poly_offset_int
mem_ref_offset (const_tree t)
{
  return poly_offset_int::from (wi::to_poly_wide (TREE_OPERAND (t, 1)),
				SIGNED);
}

/* Return an invariant ADDR_EXPR of type TYPE taking the address of BASE
   offsetted by OFFSET units.  */

tree
build_invariant_address (tree type, tree base, poly_int64 offset)
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

/* Create a DECL_... node of code CODE, name NAME  (if non-null)
   and data type TYPE.
   We do NOT enter this node in any sort of symbol table.

   LOC is the location of the decl.

   layout_decl is used to set up the decl's storage layout.
   Other slots are initialized to 0 or null pointers.  */

tree
build_decl (location_t loc, enum tree_code code, tree name,
    		 tree type MEM_STAT_DECL)
{
  tree t;

  t = make_node (code PASS_MEM_STAT);
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

/* Create and return a DEBUG_EXPR_DECL node of the given TYPE.  */

tree
build_debug_expr_decl (tree type)
{
  tree vexpr = make_node (DEBUG_EXPR_DECL);
  DECL_ARTIFICIAL (vexpr) = 1;
  TREE_TYPE (vexpr) = type;
  SET_DECL_MODE (vexpr, TYPE_MODE (type));
  return vexpr;
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
  else if (t && TREE_CODE (t) == STATEMENT_LIST)
    {
      t = expr_single (t);
      if (t && CAN_HAVE_LOCATION_P (t))
	SET_EXPR_LOCATION (t, loc);
    }
}

/* Like PROTECTED_SET_EXPR_LOCATION, but only do that if T has
   UNKNOWN_LOCATION.  */

void
protected_set_expr_location_if_unset (tree t, location_t loc)
{
  t = expr_single (t);
  if (t && !EXPR_HAS_LOCATION (t))
    protected_set_expr_location (t, loc);
}

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

/* Returns true iff CAND and BASE have equivalent language-specific
   qualifiers.  */

bool
check_lang_type (const_tree cand, const_tree base)
{
  if (lang_hooks.types.type_hash_eq == NULL)
    return true;
  /* type_hash_eq currently only applies to these types.  */
  if (TREE_CODE (cand) != FUNCTION_TYPE
      && TREE_CODE (cand) != METHOD_TYPE)
    return true;
  return lang_hooks.types.type_hash_eq (cand, base);
}

/* This function checks to see if TYPE matches the size one of the built-in 
   atomic types, and returns that core atomic type.  */

static tree
find_atomic_core_type (const_tree type)
{
  tree base_atomic_type;

  /* Only handle complete types.  */
  if (!tree_fits_uhwi_p (TYPE_SIZE (type)))
    return NULL_TREE;

  switch (tree_to_uhwi (TYPE_SIZE (type)))
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

/* Returns true iff unqualified CAND and BASE are equivalent.  */

bool
check_base_type (const_tree cand, const_tree base)
{
  if (TYPE_NAME (cand) != TYPE_NAME (base)
      /* Apparently this is needed for Objective-C.  */
      || TYPE_CONTEXT (cand) != TYPE_CONTEXT (base)
      || !attribute_list_equal (TYPE_ATTRIBUTES (cand),
			        TYPE_ATTRIBUTES (base)))
    return false;
  /* Check alignment.  */
  if (TYPE_ALIGN (cand) == TYPE_ALIGN (base)
      && TYPE_USER_ALIGN (cand) == TYPE_USER_ALIGN (base))
    return true;
  /* Atomic types increase minimal alignment.  We must to do so as well
     or we get duplicated canonical types. See PR88686.  */
  if ((TYPE_QUALS (cand) & TYPE_QUAL_ATOMIC))
    {
      /* See if this object can map to a basic atomic type.  */
      tree atomic_type = find_atomic_core_type (cand);
      if (atomic_type && TYPE_ALIGN (atomic_type) == TYPE_ALIGN (cand))
       return true;
    }
  return false;
}

/* Returns true iff CAND is equivalent to BASE with TYPE_QUALS.  */

bool
check_qualified_type (const_tree cand, const_tree base, int type_quals)
{
  return (TYPE_QUALS (cand) == type_quals
	  && check_base_type (cand, base)
	  && check_lang_type (cand, base));
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
	  /* Check this is a user-aligned type as build_aligned_type
	     would create.  */
	  && TYPE_USER_ALIGN (cand)
	  && attribute_list_equal (TYPE_ATTRIBUTES (cand),
				   TYPE_ATTRIBUTES (base))
	  && check_lang_type (cand, base));
}

/* Return a version of the TYPE, qualified as indicated by the
   TYPE_QUALS, if one exists.  If no qualified version exists yet,
   return NULL_TREE.  */

tree
get_qualified_type (tree type, int type_quals)
{
  if (TYPE_QUALS (type) == type_quals)
    return type;

  tree mv = TYPE_MAIN_VARIANT (type);
  if (check_qualified_type (mv, type, type_quals))
    return mv;

  /* Search the chain of variants to see if there is already one there just
     like the one we need to have.  If so, use that existing one.  We must
     preserve the TYPE_NAME, since there is code that depends on this.  */
  for (tree *tp = &TYPE_NEXT_VARIANT (mv); *tp; tp = &TYPE_NEXT_VARIANT (*tp))
    if (check_qualified_type (*tp, type, type_quals))
      {
	/* Put the found variant at the head of the variant list so
	   frequently searched variants get found faster.  The C++ FE
	   benefits greatly from this.  */
	tree t = *tp;
	*tp = TYPE_NEXT_VARIANT (t);
	TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (mv);
	TYPE_NEXT_VARIANT (mv) = t;
	return t;
      }

  return NULL_TREE;
}

/* Like get_qualified_type, but creates the type if it does not
   exist.  This function never returns NULL_TREE.  */

tree
build_qualified_type (tree type, int type_quals MEM_STAT_DECL)
{
  tree t;

  /* See if we already have the appropriate qualified variant.  */
  t = get_qualified_type (type, type_quals);

  /* If not, build it.  */
  if (!t)
    {
      t = build_variant_type_copy (type PASS_MEM_STAT);
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
  TYPE_USER_ALIGN (t) = 1;

  return t;
}

/* Create a new distinct copy of TYPE.  The new type is made its own
   MAIN_VARIANT. If TYPE requires structural equality checks, the
   resulting type requires structural equality checks; otherwise, its
   TYPE_CANONICAL points to itself. */

tree
build_distinct_type_copy (tree type MEM_STAT_DECL)
{
  tree t = copy_node (type PASS_MEM_STAT);

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
build_variant_type_copy (tree type MEM_STAT_DECL)
{
  tree t, m = TYPE_MAIN_VARIANT (type);

  t = build_distinct_type_copy (type PASS_MEM_STAT);

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
  else if (VAR_P (decl))
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

  /* Uses of FROM shouldn't look like they happen at the location of TO.  */
  to = protected_set_expr_location_unshare (to, UNKNOWN_LOCATION);

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

/* Generate the default hash code for TYPE.  This is designed for
   speed, rather than maximum entropy.  */

hashval_t
type_hash_canon_hash (tree type)
{
  inchash::hash hstate;

  hstate.add_int (TREE_CODE (type));

  if (TREE_TYPE (type))
    hstate.add_object (TYPE_HASH (TREE_TYPE (type)));

  for (tree t = TYPE_ATTRIBUTES (type); t; t = TREE_CHAIN (t))
    /* Just the identifier is adequate to distinguish.  */
    hstate.add_object (IDENTIFIER_HASH_VALUE (get_attribute_name (t)));

  switch (TREE_CODE (type))
    {
    case METHOD_TYPE:
      hstate.add_object (TYPE_HASH (TYPE_METHOD_BASETYPE (type)));
      /* FALLTHROUGH. */
    case FUNCTION_TYPE:
      for (tree t = TYPE_ARG_TYPES (type); t; t = TREE_CHAIN (t))
	if (TREE_VALUE (t) != error_mark_node)
	  hstate.add_object (TYPE_HASH (TREE_VALUE (t)));
      break;

    case OFFSET_TYPE:
      hstate.add_object (TYPE_HASH (TYPE_OFFSET_BASETYPE (type)));
      break;

    case ARRAY_TYPE:
      {
	if (TYPE_DOMAIN (type))
	  hstate.add_object (TYPE_HASH (TYPE_DOMAIN (type)));
	if (!AGGREGATE_TYPE_P (TREE_TYPE (type)))
	  {
	    unsigned typeless = TYPE_TYPELESS_STORAGE (type);
	    hstate.add_object (typeless);
	  }
      }
      break;

    case INTEGER_TYPE:
      {
	tree t = TYPE_MAX_VALUE (type);
	if (!t)
	  t = TYPE_MIN_VALUE (type);
	for (int i = 0; i < TREE_INT_CST_NUNITS (t); i++)
	  hstate.add_object (TREE_INT_CST_ELT (t, i));
	break;
      }
      
    case REAL_TYPE:
    case FIXED_POINT_TYPE:
      {
	unsigned prec = TYPE_PRECISION (type);
	hstate.add_object (prec);
	break;
      }

    case VECTOR_TYPE:
      hstate.add_poly_int (TYPE_VECTOR_SUBPARTS (type));
      break;

    default:
      break;
    }

  return hstate.end ();
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
    case OPAQUE_TYPE:
    case COMPLEX_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case NULLPTR_TYPE:
      return 1;

    case VECTOR_TYPE:
      return known_eq (TYPE_VECTOR_SUBPARTS (a->type),
		       TYPE_VECTOR_SUBPARTS (b->type));

    case ENUMERAL_TYPE:
      if (TYPE_VALUES (a->type) != TYPE_VALUES (b->type)
	  && !(TYPE_VALUES (a->type)
	       && TREE_CODE (TYPE_VALUES (a->type)) == TREE_LIST
	       && TYPE_VALUES (b->type)
	       && TREE_CODE (TYPE_VALUES (b->type)) == TREE_LIST
	       && type_list_equal (TYPE_VALUES (a->type),
				   TYPE_VALUES (b->type))))
	return 0;

      /* fall through */

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
      /* Don't compare TYPE_TYPELESS_STORAGE flag on aggregates,
	 where the flag should be inherited from the element type
	 and can change after ARRAY_TYPEs are created; on non-aggregates
	 compare it and hash it, scalars will never have that flag set
	 and we need to differentiate between arrays created by different
	 front-ends or middle-end created arrays.  */
      return (TYPE_DOMAIN (a->type) == TYPE_DOMAIN (b->type)
	      && (AGGREGATE_TYPE_P (TREE_TYPE (a->type))
		  || (TYPE_TYPELESS_STORAGE (a->type)
		      == TYPE_TYPELESS_STORAGE (b->type))));

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
      if ((TYPE_ARG_TYPES (a->type) == TYPE_ARG_TYPES (b->type)
	   && (TYPE_NO_NAMED_ARGS_STDARG_P (a->type)
	       == TYPE_NO_NAMED_ARGS_STDARG_P (b->type)))
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
      gcc_assert (TYPE_MAIN_VARIANT (t1) == t1
		  && t1 != type);
      if (TYPE_UID (type) + 1 == next_type_uid)
	--next_type_uid;
      /* Free also min/max values and the cache for integer
	 types.  This can't be done in free_node, as LTO frees
	 those on its own.  */
      if (TREE_CODE (type) == INTEGER_TYPE)
	{
	  if (TYPE_MIN_VALUE (type)
	      && TREE_TYPE (TYPE_MIN_VALUE (type)) == type)
	    {
	      /* Zero is always in TYPE_CACHED_VALUES.  */
	      if (! TYPE_UNSIGNED (type))
		int_cst_hash_table->remove_elt (TYPE_MIN_VALUE (type));
	      ggc_free (TYPE_MIN_VALUE (type));
	    }
	  if (TYPE_MAX_VALUE (type)
	      && TREE_TYPE (TYPE_MAX_VALUE (type)) == type)
	    {
	      int_cst_hash_table->remove_elt (TYPE_MAX_VALUE (type));
	      ggc_free (TYPE_MAX_VALUE (type));
	    }
	  if (TYPE_CACHED_VALUES_P (type))
	    ggc_free (TYPE_CACHED_VALUES (type));
	}
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

/* Given two lists of types
   (chains of TREE_LIST nodes with types in the TREE_VALUE slots)
   return 1 if the lists contain the same types in the same order.
   Also, the TREE_PURPOSEs must match.  */

bool
type_list_equal (const_tree l1, const_tree l2)
{
  const_tree t1, t2;

  for (t1 = l1, t2 = l2; t1 && t2; t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2))
    if (TREE_VALUE (t1) != TREE_VALUE (t2)
	|| (TREE_PURPOSE (t1) != TREE_PURPOSE (t2)
	    && ! (1 == simple_cst_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2))
		  && (TREE_TYPE (TREE_PURPOSE (t1))
		      == TREE_TYPE (TREE_PURPOSE (t2))))))
      return false;

  return t1 == t2;
}

/* Returns the number of arguments to the FUNCTION_TYPE or METHOD_TYPE
   given by TYPE.  If the argument list accepts variable arguments,
   then this function counts only the ordinary arguments.  */

int
type_num_arguments (const_tree fntype)
{
  int i = 0;

  for (tree t = TYPE_ARG_TYPES (fntype); t; t = TREE_CHAIN (t))
    /* If the function does not take a variable number of arguments,
       the last element in the list will have type `void'.  */
    if (VOID_TYPE_P (TREE_VALUE (t)))
      break;
    else
      ++i;

  return i;
}

/* Return the type of the function TYPE's argument ARGNO if known.
   For vararg function's where ARGNO refers to one of the variadic
   arguments return null.  Otherwise, return a void_type_node for
   out-of-bounds ARGNO.  */

tree
type_argument_type (const_tree fntype, unsigned argno)
{
  /* Treat zero the same as an out-of-bounds argument number.  */
  if (!argno)
    return void_type_node;

  function_args_iterator iter;

  tree argtype;
  unsigned i = 1;
  FOREACH_FUNCTION_ARGS (fntype, argtype, iter)
    {
      /* A vararg function's argument list ends in a null.  Otherwise,
	 an ordinary function's argument list ends with void.  Return
	 null if ARGNO refers to a vararg argument, void_type_node if
	 it's out of bounds, and the formal argument type otherwise.  */
      if (!argtype)
	break;

      if (i == argno || VOID_TYPE_P (argtype))
	return argtype;

      ++i;
    }

  return NULL_TREE;
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

  STRIP_ANY_LOCATION_WRAPPER (t1);
  STRIP_ANY_LOCATION_WRAPPER (t2);

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

/* Return true if T is an INTEGER_CST or POLY_INT_CST whose numerical
   value (extended according to TYPE_UNSIGNED) fits in a poly_int64.  */

bool
tree_fits_poly_int64_p (const_tree t)
{
  if (t == NULL_TREE)
    return false;
  if (POLY_INT_CST_P (t))
    {
      for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; i++)
	if (!wi::fits_shwi_p (wi::to_wide (POLY_INT_CST_COEFF (t, i))))
	  return false;
      return true;
    }
  return (TREE_CODE (t) == INTEGER_CST
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

/* Return true if T is an INTEGER_CST or POLY_INT_CST whose numerical
   value (extended according to TYPE_UNSIGNED) fits in a poly_uint64.  */

bool
tree_fits_poly_uint64_p (const_tree t)
{
  if (t == NULL_TREE)
    return false;
  if (POLY_INT_CST_P (t))
    {
      for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; i++)
	if (!wi::fits_uhwi_p (wi::to_widest (POLY_INT_CST_COEFF (t, i))))
	  return false;
      return true;
    }
  return (TREE_CODE (t) == INTEGER_CST
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

  return wi::extract_uhwi (wi::to_wide (t), bitno, 1);
}

/* Return an indication of the sign of the integer constant T.
   The return value is -1 if T < 0, 0 if T == 0, and 1 if T > 0.
   Note that -1 will never be returned if T's type is unsigned.  */

int
tree_int_cst_sgn (const_tree t)
{
  if (wi::to_wide (t) == 0)
    return 0;
  else if (TYPE_UNSIGNED (TREE_TYPE (t)))
    return 1;
  else if (wi::neg_p (wi::to_wide (t)))
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

  /* For location wrappers to be the same, they must be at the same
     source location (and wrap the same thing).  */
  if (location_wrapper_p (t1) && location_wrapper_p (t2))
    {
      if (EXPR_LOCATION (t1) != EXPR_LOCATION (t2))
	return 0;
      return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
    }

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
      if (POLY_INT_CST_P (t1))
	/* A false return means maybe_ne rather than known_ne.  */
	return known_eq (poly_widest_int::from (poly_int_cst_value (t1),
						TYPE_SIGN (TREE_TYPE (t1))),
			 poly_widest_int::from (poly_int_cst_value (t2),
						TYPE_SIGN (TREE_TYPE (t2))));
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
   half of the address-space).
   When PERR is non-null, set *PERR on failure to the description of
   why SIZE is not valid.  */

bool
valid_constant_size_p (const_tree size, cst_size_error *perr /* = NULL */)
{
  if (POLY_INT_CST_P (size))
    {
      if (TREE_OVERFLOW (size))
	return false;
      for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; ++i)
	if (!valid_constant_size_p (POLY_INT_CST_COEFF (size, i)))
	  return false;
      return true;
    }

  cst_size_error error;
  if (!perr)
    perr = &error;

  if (TREE_CODE (size) != INTEGER_CST)
    {
      *perr = cst_size_not_constant;
      return false;
    }

  if (TREE_OVERFLOW_P (size))
    {
      *perr = cst_size_overflow;
      return false;
    }

  if (tree_int_cst_sgn (size) < 0)
    {
      *perr = cst_size_negative;
      return false;
    }
  if (!tree_fits_uhwi_p (size)
      || (wi::to_widest (TYPE_MAX_VALUE (sizetype))
	  < wi::to_widest (size) * 2))
    {
      *perr = cst_size_too_big;
      return false;
    }

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

/* Constructors for pointer, array and function types.
   (RECORD_TYPE, UNION_TYPE and ENUMERAL_TYPE nodes are
   constructed by language-dependent code, not here.)  */

/* Construct, lay out and return the type of pointers to TO_TYPE with
   mode MODE.  If MODE is VOIDmode, a pointer mode for the address
   space of TO_TYPE will be picked.  If CAN_ALIAS_ALL is TRUE,
   indicate this type can reference all of memory. If such a type has
   already been constructed, reuse it.  */

tree
build_pointer_type_for_mode (tree to_type, machine_mode mode,
			     bool can_alias_all)
{
  tree t;
  bool could_alias = can_alias_all;

  if (to_type == error_mark_node)
    return error_mark_node;

  if (mode == VOIDmode)
    {
      addr_space_t as = TYPE_ADDR_SPACE (to_type);
      mode = targetm.addr_space.pointer_mode (as);
    }

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
  return build_pointer_type_for_mode (to_type, VOIDmode, false);
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

  if (mode == VOIDmode)
    {
      addr_space_t as = TYPE_ADDR_SPACE (to_type);
      mode = targetm.addr_space.pointer_mode (as);
    }

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
  return build_reference_type_for_mode (to_type, VOIDmode, false);
}

#define MAX_INT_CACHED_PREC \
  (HOST_BITS_PER_WIDE_INT > 64 ? HOST_BITS_PER_WIDE_INT : 64)
static GTY(()) tree nonstandard_integer_type_cache[2 * MAX_INT_CACHED_PREC + 2];

static void
clear_nonstandard_integer_type_cache (void)
{
  for (size_t i = 0 ; i < 2 * MAX_INT_CACHED_PREC + 2 ; i++)
  {
    nonstandard_integer_type_cache[i] = NULL;
  }
}

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

  inchash::hash hstate;
  inchash::add_expr (TYPE_MAX_VALUE (itype), hstate);
  ret = type_hash_canon (hstate.end (), itype);
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

  TREE_TYPE (itype) = type;

  TYPE_MIN_VALUE (itype) = fold_convert (type, lowval);
  TYPE_MAX_VALUE (itype) = highval ? fold_convert (type, highval) : NULL;

  TYPE_PRECISION (itype) = TYPE_PRECISION (type);
  SET_TYPE_MODE (itype, TYPE_MODE (type));
  TYPE_SIZE (itype) = TYPE_SIZE (type);
  TYPE_SIZE_UNIT (itype) = TYPE_SIZE_UNIT (type);
  SET_TYPE_ALIGN (itype, TYPE_ALIGN (type));
  TYPE_USER_ALIGN (itype) = TYPE_USER_ALIGN (type);
  SET_TYPE_WARN_IF_NOT_ALIGN (itype, TYPE_WARN_IF_NOT_ALIGN (type));

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

  hashval_t hash = type_hash_canon_hash (itype);
  itype = type_hash_canon (hash, itype);

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
   If TYPELESS_STORAGE is true, TYPE_TYPELESS_STORAGE flag is set on the type.
   If SHARED is true, reuse such a type that has already been constructed.
   If SET_CANONICAL is true, compute TYPE_CANONICAL from the element type.  */

tree
build_array_type_1 (tree elt_type, tree index_type, bool typeless_storage,
		    bool shared, bool set_canonical)
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
  TYPE_TYPELESS_STORAGE (t) = typeless_storage;
  layout_type (t);

  if (shared)
    {
      hashval_t hash = type_hash_canon_hash (t);
      t = type_hash_canon (hash, t);
    }

  if (TYPE_CANONICAL (t) == t && set_canonical)
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
				typeless_storage, shared, set_canonical);
    }

  return t;
}

/* Wrapper around build_array_type_1 with SHARED set to true.  */

tree
build_array_type (tree elt_type, tree index_type, bool typeless_storage)
{
  return
    build_array_type_1 (elt_type, index_type, typeless_storage, true, true);
}

/* Wrapper around build_array_type_1 with SHARED set to false.  */

tree
build_nonshared_array_type (tree elt_type, tree index_type)
{
  return build_array_type_1 (elt_type, index_type, false, false, true);
}

/* Return a representation of ELT_TYPE[NELTS], using indices of type
   sizetype.  */

tree
build_array_type_nelts (tree elt_type, poly_uint64 nelts)
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
   NO_NAMED_ARGS_STDARG_P is true if this is a prototyped
   variable-arguments function with (...) prototype (no named arguments).
   If such a type has already been constructed, reuse it.  */

tree
build_function_type (tree value_type, tree arg_types,
		     bool no_named_args_stdarg_p)
{
  tree t;
  inchash::hash hstate;
  bool any_structural_p, any_noncanonical_p;
  tree canon_argtypes;

  gcc_assert (arg_types != error_mark_node);

  if (TREE_CODE (value_type) == FUNCTION_TYPE)
    {
      error ("function return type cannot be function");
      value_type = integer_type_node;
    }

  /* Make a node of the sort we want.  */
  t = make_node (FUNCTION_TYPE);
  TREE_TYPE (t) = value_type;
  TYPE_ARG_TYPES (t) = arg_types;
  if (no_named_args_stdarg_p)
    {
      gcc_assert (arg_types == NULL_TREE);
      TYPE_NO_NAMED_ARGS_STDARG_P (t) = 1;
    }

  /* If we already have such a type, use the old one.  */
  hashval_t hash = type_hash_canon_hash (t);
  t = type_hash_canon (hash, t);

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
  args = build_function_type (return_type, args, vaargs && args == NULL_TREE);

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

  return build_function_type (return_type, t, vaargs && n == 0);
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
  hashval_t hash = type_hash_canon_hash (t);
  t = type_hash_canon (hash, t);

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

  /* Make a node of the sort we want.  */
  t = make_node (OFFSET_TYPE);

  TYPE_OFFSET_BASETYPE (t) = TYPE_MAIN_VARIANT (basetype);
  TREE_TYPE (t) = type;

  /* If we already have such a type, use the old one.  */
  hashval_t hash = type_hash_canon_hash (t);
  t = type_hash_canon (hash, t);

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

/* Create a complex type whose components are COMPONENT_TYPE.

   If NAMED is true, the type is given a TYPE_NAME.  We do not always
   do so because this creates a DECL node and thus make the DECL_UIDs
   dependent on the type canonicalization hashtable, which is GC-ed,
   so the DECL_UIDs would not be stable wrt garbage collection.  */

tree
build_complex_type (tree component_type, bool named)
{
  gcc_assert (INTEGRAL_TYPE_P (component_type)
	      || SCALAR_FLOAT_TYPE_P (component_type)
	      || FIXED_POINT_TYPE_P (component_type));

  /* Make a node of the sort we want.  */
  tree probe = make_node (COMPLEX_TYPE);

  TREE_TYPE (probe) = TYPE_MAIN_VARIANT (component_type);

  /* If we already have such a type, use the old one.  */
  hashval_t hash = type_hash_canon_hash (probe);
  tree t = type_hash_canon (hash, probe);

  if (t == probe)
    {
      /* We created a new type.  The hash insertion will have laid
	 out the type.  We need to check the canonicalization and
	 maybe set the name.  */
      gcc_checking_assert (COMPLETE_TYPE_P (t)
			   && !TYPE_NAME (t)
			   && TYPE_CANONICAL (t) == t);

      if (TYPE_STRUCTURAL_EQUALITY_P (TREE_TYPE (t)))
	SET_TYPE_STRUCTURAL_EQUALITY (t);
      else if (TYPE_CANONICAL (TREE_TYPE (t)) != TREE_TYPE (t))
	TYPE_CANONICAL (t)
	  = build_complex_type (TYPE_CANONICAL (TREE_TYPE (t)), named);

      /* We need to create a name, since complex is a fundamental type.  */
      if (named)
	{
	  const char *name = NULL;

	  if (TREE_TYPE (t) == char_type_node)
	    name = "complex char";
	  else if (TREE_TYPE (t) == signed_char_type_node)
	    name = "complex signed char";
	  else if (TREE_TYPE (t) == unsigned_char_type_node)
	    name = "complex unsigned char";
	  else if (TREE_TYPE (t) == short_integer_type_node)
	    name = "complex short int";
	  else if (TREE_TYPE (t) == short_unsigned_type_node)
	    name = "complex short unsigned int";
	  else if (TREE_TYPE (t) == integer_type_node)
	    name = "complex int";
	  else if (TREE_TYPE (t) == unsigned_type_node)
	    name = "complex unsigned int";
	  else if (TREE_TYPE (t) == long_integer_type_node)
	    name = "complex long int";
	  else if (TREE_TYPE (t) == long_unsigned_type_node)
	    name = "complex long unsigned int";
	  else if (TREE_TYPE (t) == long_long_integer_type_node)
	    name = "complex long long int";
	  else if (TREE_TYPE (t) == long_long_unsigned_type_node)
	    name = "complex long long unsigned int";

	  if (name != NULL)
	    TYPE_NAME (t) = build_decl (UNKNOWN_LOCATION, TYPE_DECL,
					get_identifier (name), t);
	}
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
  /* The target can give two different responses to the question of
     which excess precision mode it would like depending on whether we
     are in -fexcess-precision=standard or -fexcess-precision=fast.  */

  enum excess_precision_type requested_type
    = (flag_excess_precision == EXCESS_PRECISION_FAST
       ? EXCESS_PRECISION_TYPE_FAST
       : (flag_excess_precision == EXCESS_PRECISION_FLOAT16
	  ? EXCESS_PRECISION_TYPE_FLOAT16 : EXCESS_PRECISION_TYPE_STANDARD));

  enum flt_eval_method target_flt_eval_method
    = targetm.c.excess_precision (requested_type);

  /* The target should not ask for unpredictable float evaluation (though
     it might advertise that implicitly the evaluation is unpredictable,
     but we don't care about that here, it will have been reported
     elsewhere).  If it does ask for unpredictable evaluation, we have
     nothing to do here.  */
  gcc_assert (target_flt_eval_method != FLT_EVAL_METHOD_UNPREDICTABLE);

  /* Nothing to do.  The target has asked for all types we know about
     to be computed with their native precision and range.  */
  if (target_flt_eval_method == FLT_EVAL_METHOD_PROMOTE_TO_FLOAT16)
    return NULL_TREE;

  /* The target will promote this type in a target-dependent way, so excess
     precision ought to leave it alone.  */
  if (targetm.promoted_type (type) != NULL_TREE)
    return NULL_TREE;

  machine_mode float16_type_mode = (float16_type_node
				    ? TYPE_MODE (float16_type_node)
				    : VOIDmode);
  machine_mode bfloat16_type_mode = (bfloat16_type_node
				     ? TYPE_MODE (bfloat16_type_node)
				     : VOIDmode);
  machine_mode float_type_mode = TYPE_MODE (float_type_node);
  machine_mode double_type_mode = TYPE_MODE (double_type_node);

  switch (TREE_CODE (type))
    {
    case REAL_TYPE:
      {
	machine_mode type_mode = TYPE_MODE (type);
	switch (target_flt_eval_method)
	  {
	  case FLT_EVAL_METHOD_PROMOTE_TO_FLOAT:
	    if (type_mode == float16_type_mode
		|| type_mode == bfloat16_type_mode)
	      return float_type_node;
	    break;
	  case FLT_EVAL_METHOD_PROMOTE_TO_DOUBLE:
	    if (type_mode == float16_type_mode
		|| type_mode == bfloat16_type_mode
		|| type_mode == float_type_mode)
	      return double_type_node;
	    break;
	  case FLT_EVAL_METHOD_PROMOTE_TO_LONG_DOUBLE:
	    if (type_mode == float16_type_mode
		|| type_mode == bfloat16_type_mode
		|| type_mode == float_type_mode
		|| type_mode == double_type_mode)
	      return long_double_type_node;
	    break;
	  default:
	    gcc_unreachable ();
	  }
	break;
      }
    case COMPLEX_TYPE:
      {
	if (TREE_CODE (TREE_TYPE (type)) != REAL_TYPE)
	  return NULL_TREE;
	machine_mode type_mode = TYPE_MODE (TREE_TYPE (type));
	switch (target_flt_eval_method)
	  {
	  case FLT_EVAL_METHOD_PROMOTE_TO_FLOAT:
	    if (type_mode == float16_type_mode
		|| type_mode == bfloat16_type_mode)
	      return complex_float_type_node;
	    break;
	  case FLT_EVAL_METHOD_PROMOTE_TO_DOUBLE:
	    if (type_mode == float16_type_mode
		|| type_mode == bfloat16_type_mode
		|| type_mode == float_type_mode)
	      return complex_double_type_node;
	    break;
	  case FLT_EVAL_METHOD_PROMOTE_TO_LONG_DOUBLE:
	    if (type_mode == float16_type_mode
		|| type_mode == bfloat16_type_mode
		|| type_mode == float_type_mode
		|| type_mode == double_type_mode)
	      return complex_long_double_type_node;
	    break;
	  default:
	    gcc_unreachable ();
	  }
	break;
      }
    default:
      break;
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

  /* If we finally reach a constant see if it fits in sth smaller and
     in that case convert it.  */
  if (TREE_CODE (win) == INTEGER_CST)
    {
      tree wtype = TREE_TYPE (win);
      unsigned prec = wi::min_precision (wi::to_wide (win), TYPE_SIGN (wtype));
      if (for_type)
	prec = MAX (prec, final_prec);
      if (prec < TYPE_PRECISION (wtype))
	{
	  tree t = lang_hooks.types.type_for_size (prec, TYPE_UNSIGNED (wtype));
	  if (t && TYPE_PRECISION (t) < TYPE_PRECISION (wtype))
	    win = fold_convert (t, win);
	}
    }

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

  if (TREE_CODE (op) == COMPOUND_EXPR)
    {
      do
	op = TREE_OPERAND (op, 1);
      while (TREE_CODE (op) == COMPOUND_EXPR);
      tree ret = get_narrower (op, unsignedp_ptr);
      if (ret == op)
	return win;
      auto_vec <tree, 16> v;
      unsigned int i;
      for (op = win; TREE_CODE (op) == COMPOUND_EXPR;
	   op = TREE_OPERAND (op, 1))
	v.safe_push (op);
      FOR_EACH_VEC_ELT_REVERSE (v, i, op)
	ret = build2_loc (EXPR_LOCATION (op), COMPOUND_EXPR,
			  TREE_TYPE (ret), TREE_OPERAND (op, 0),
			  ret);
      return ret;
    }
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

/* Return true if integer constant C has a value that is permissible
   for TYPE, an integral type.  */

bool
int_fits_type_p (const_tree c, const_tree type)
{
  tree type_low_bound, type_high_bound;
  bool ok_for_low_bound, ok_for_high_bound;
  signop sgn_c = TYPE_SIGN (TREE_TYPE (c));

  /* Non-standard boolean types can have arbitrary precision but various
     transformations assume that they can only take values 0 and +/-1.  */
  if (TREE_CODE (type) == BOOLEAN_TYPE)
    return wi::fits_to_boolean_p (wi::to_wide (c), type);

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
  if (TYPE_UNSIGNED (type) && sgn_c == SIGNED && wi::neg_p (wi::to_wide (c)))
    return false;

  /* Second, narrower types always fit in wider ones.  */
  if (TYPE_PRECISION (type) > TYPE_PRECISION (TREE_TYPE (c)))
    return true;

  /* Third, unsigned integers with top bit set never fit signed types.  */
  if (!TYPE_UNSIGNED (type) && sgn_c == UNSIGNED)
    {
      int prec = GET_MODE_PRECISION (SCALAR_INT_TYPE_MODE (TREE_TYPE (c))) - 1;
      if (prec < TYPE_PRECISION (TREE_TYPE (c)))
	{
	  /* When a tree_cst is converted to a wide-int, the precision
	     is taken from the type.  However, if the precision of the
	     mode underneath the type is smaller than that, it is
	     possible that the value will not fit.  The test below
	     fails if any bit is set between the sign bit of the
	     underlying mode and the top bit of the type.  */
	  if (wi::zext (wi::to_wide (c), prec - 1) != wi::to_wide (c))
	    return false;
	}
      else if (wi::neg_p (wi::to_wide (c)))
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
  return wi::fits_to_tree_p (wi::to_wide (c), type);
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
    wi::to_mpz (wi::to_wide (TYPE_MIN_VALUE (type)), min, TYPE_SIGN (type));
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
    wi::to_mpz (wi::to_wide (TYPE_MAX_VALUE (type)), max, TYPE_SIGN (type));
  else
    {
      wide_int mn = wi::max_value (TYPE_PRECISION (type), TYPE_SIGN (type));
      wi::to_mpz (mn, max, TYPE_SIGN (type));
    }
}

/* Return true if VAR is an automatic variable.  */

bool
auto_var_p (const_tree var)
{
  return ((((VAR_P (var) && ! DECL_EXTERNAL (var))
	    || TREE_CODE (var) == PARM_DECL)
	   && ! TREE_STATIC (var))
	  || TREE_CODE (var) == RESULT_DECL);
}

/* Return true if VAR is an automatic variable defined in function FN.  */

bool
auto_var_in_fn_p (const_tree var, const_tree fn)
{
  return (DECL_P (var) && DECL_CONTEXT (var) == fn
	  && (auto_var_p (var)
	      || TREE_CODE (var) == LABEL_DECL));
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
	&& !CONSTANT_CLASS_P (_t)					\
	&& TREE_CODE (_t) != PLACEHOLDER_EXPR				\
	&& (!fn								\
	    || (!TYPE_SIZES_GIMPLIFIED (type)				\
		&& (TREE_CODE (_t) != VAR_DECL				\
		    && !CONTAINS_PLACEHOLDER_P (_t)))			\
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
      /* Ada can have pointer types refering to themselves indirectly.  */
      if (TREE_VISITED (type))
	return false;
      TREE_VISITED (type) = true;
      if (variably_modified_type_p (TREE_TYPE (type), fn))
	{
	  TREE_VISITED (type) = false;
	  return true;
	}
      TREE_VISITED (type) = false;
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

	    /* If the type is a qualified union, then the DECL_QUALIFIER
	       of fields can also be an expression containing a variable.  */
	    if (TREE_CODE (type) == QUAL_UNION_TYPE)
	      RETURN_TRUE_IF_VAR (DECL_QUALIFIER (t));

	    /* If the field is a qualified union, then it's only a container
	       for what's inside so we look into it.  That's necessary in LTO
	       mode because the sizes of the field tested above have been set
	       to PLACEHOLDER_EXPRs by free_lang_data.  */
	    if (TREE_CODE (TREE_TYPE (t)) == QUAL_UNION_TYPE
		&& variably_modified_type_p (TREE_TYPE (t), fn))
	      return true;
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

/* Returns the ultimate TRANSLATION_UNIT_DECL context of DECL or NULL.  */

const_tree
get_ultimate_context (const_tree decl)
{
  while (decl && TREE_CODE (decl) != TRANSLATION_UNIT_DECL)
    {
      if (TREE_CODE (decl) == BLOCK)
	decl = BLOCK_SUPERCONTEXT (decl);
      else
	decl = get_containing_scope (decl);
    }
  return decl;
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
  else if (TREE_CODE (decl) == FUNCTION_DECL && DECL_VIRTUAL_P (decl))
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

/* Return true when STMTs arguments and return value match those of FNDECL,
   a decl of a builtin function.  */

static bool
tree_builtin_call_types_compatible_p (const_tree call, tree fndecl)
{
  gcc_checking_assert (DECL_BUILT_IN_CLASS (fndecl) != NOT_BUILT_IN);

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    if (tree decl = builtin_decl_explicit (DECL_FUNCTION_CODE (fndecl)))
      fndecl = decl;

  bool gimple_form = (cfun && (cfun->curr_properties & PROP_gimple)) != 0;
  if (gimple_form
      ? !useless_type_conversion_p (TREE_TYPE (call),
				    TREE_TYPE (TREE_TYPE (fndecl)))
      : (TYPE_MAIN_VARIANT (TREE_TYPE (call))
	 != TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (fndecl)))))
    return false;

  tree targs = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  unsigned nargs = call_expr_nargs (call);
  for (unsigned i = 0; i < nargs; ++i, targs = TREE_CHAIN (targs))
    {
      /* Variadic args follow.  */
      if (!targs)
	return true;
      tree arg = CALL_EXPR_ARG (call, i);
      tree type = TREE_VALUE (targs);
      if (gimple_form
	  ? !useless_type_conversion_p (type, TREE_TYPE (arg))
	  : TYPE_MAIN_VARIANT (type) != TYPE_MAIN_VARIANT (TREE_TYPE (arg)))
	{
	  /* For pointer arguments be more forgiving, e.g. due to
	     FILE * vs. fileptr_type_node, or say char * vs. const char *
	     differences etc.  */
	  if (!gimple_form
	      && POINTER_TYPE_P (type)
	      && POINTER_TYPE_P (TREE_TYPE (arg))
	      && tree_nop_conversion_p (type, TREE_TYPE (arg)))
	    continue;
	  /* char/short integral arguments are promoted to int
	     by several frontends if targetm.calls.promote_prototypes
	     is true.  Allow such promotion too.  */
	  if (INTEGRAL_TYPE_P (type)
	      && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)
	      && INTEGRAL_TYPE_P (TREE_TYPE (arg))
	      && !TYPE_UNSIGNED (TREE_TYPE (arg))
	      && targetm.calls.promote_prototypes (TREE_TYPE (fndecl))
	      && (gimple_form
		  ? useless_type_conversion_p (integer_type_node,
					       TREE_TYPE (arg))
		  : tree_nop_conversion_p (integer_type_node,
					   TREE_TYPE (arg))))
	    continue;
	  return false;
	}
    }
  if (targs && !VOID_TYPE_P (TREE_VALUE (targs)))
    return false;
  return true;
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
  if (fndecl
      && fndecl_built_in_p (fndecl, BUILT_IN_NORMAL)
      && tree_builtin_call_types_compatible_p (call, fndecl))
    return as_combined_fn (DECL_FUNCTION_CODE (fndecl));

  return CFN_LAST;
}

/* Comparator of indices based on tree_node_counts.  */

static int
tree_nodes_cmp (const void *p1, const void *p2)
{
  const unsigned *n1 = (const unsigned *)p1;
  const unsigned *n2 = (const unsigned *)p2;

  return tree_node_counts[*n1] - tree_node_counts[*n2];
}

/* Comparator of indices based on tree_code_counts.  */

static int
tree_codes_cmp (const void *p1, const void *p2)
{
  const unsigned *n1 = (const unsigned *)p1;
  const unsigned *n2 = (const unsigned *)p2;

  return tree_code_counts[*n1] - tree_code_counts[*n2];
}

#define TREE_MEM_USAGE_SPACES 40

/* Print debugging information about tree nodes generated during the compile,
   and any language-specific information.  */

void
dump_tree_statistics (void)
{
  if (GATHER_STATISTICS)
    {
      uint64_t total_nodes, total_bytes;
      fprintf (stderr, "\nKind                   Nodes      Bytes\n");
      mem_usage::print_dash_line (TREE_MEM_USAGE_SPACES);
      total_nodes = total_bytes = 0;

      {
	auto_vec<unsigned> indices (all_kinds);
	for (unsigned i = 0; i < all_kinds; i++)
	  indices.quick_push (i);
	indices.qsort (tree_nodes_cmp);

	for (unsigned i = 0; i < (int) all_kinds; i++)
	  {
	    unsigned j = indices[i];
	    fprintf (stderr, "%-20s %6" PRIu64 "%c %9" PRIu64 "%c\n",
		     tree_node_kind_names[j], SIZE_AMOUNT (tree_node_counts[j]),
		     SIZE_AMOUNT (tree_node_sizes[j]));
	    total_nodes += tree_node_counts[j];
	    total_bytes += tree_node_sizes[j];
	  }
	mem_usage::print_dash_line (TREE_MEM_USAGE_SPACES);
	fprintf (stderr, "%-20s %6" PRIu64 "%c %9" PRIu64 "%c\n", "Total",
		 SIZE_AMOUNT (total_nodes), SIZE_AMOUNT (total_bytes));
	mem_usage::print_dash_line (TREE_MEM_USAGE_SPACES);
      }

      {
	fprintf (stderr, "Code                              Nodes\n");
	mem_usage::print_dash_line (TREE_MEM_USAGE_SPACES);

	auto_vec<unsigned> indices (MAX_TREE_CODES);
	for (unsigned i = 0; i < MAX_TREE_CODES; i++)
	  indices.quick_push (i);
	indices.qsort (tree_codes_cmp);

	for (unsigned i = 0; i < MAX_TREE_CODES; i++)
	  {
	    unsigned j = indices[i];
	    fprintf (stderr, "%-32s %6" PRIu64 "%c\n",
		     get_tree_code_name ((enum tree_code) j),
		     SIZE_AMOUNT (tree_code_counts[j]));
	  }
	mem_usage::print_dash_line (TREE_MEM_USAGE_SPACES);
	fprintf (stderr, "\n");
	ssanames_print_statistics ();
	fprintf (stderr, "\n");
	phinodes_print_statistics ();
	fprintf (stderr, "\n");
      }
    }
  else
    fprintf (stderr, "(No per-node statistics)\n");

  print_type_hash_statistics ();
  print_debug_expr_statistics ();
  print_value_expr_statistics ();
  lang_hooks.print_statistics ();
}

#define FILE_FUNCTION_FORMAT "_GLOBAL__%s_%s"

/* Generate a crc32 of the low BYTES bytes of VALUE.  */

unsigned
crc32_unsigned_n (unsigned chksum, unsigned value, unsigned bytes)
{
  /* This relies on the raw feedback's top 4 bits being zero.  */
#define FEEDBACK(X) ((X) * 0x04c11db7)
#define SYNDROME(X) (FEEDBACK ((X) & 1) ^ FEEDBACK ((X) & 2) \
		     ^ FEEDBACK ((X) & 4) ^ FEEDBACK ((X) & 8))
  static const unsigned syndromes[16] =
    {
      SYNDROME(0x0), SYNDROME(0x1), SYNDROME(0x2), SYNDROME(0x3),
      SYNDROME(0x4), SYNDROME(0x5), SYNDROME(0x6), SYNDROME(0x7),
      SYNDROME(0x8), SYNDROME(0x9), SYNDROME(0xa), SYNDROME(0xb),
      SYNDROME(0xc), SYNDROME(0xd), SYNDROME(0xe), SYNDROME(0xf),
    };
#undef FEEDBACK
#undef SYNDROME

  value <<= (32 - bytes * 8);
  for (unsigned ix = bytes * 2; ix--; value <<= 4)
    {
      unsigned feedback = syndromes[((value ^ chksum) >> 28) & 0xf];

      chksum = (chksum << 4) ^ feedback;
    }

  return chksum;
}

/* Generate a crc32 of a string.  */

unsigned
crc32_string (unsigned chksum, const char *string)
{
  do
    chksum = crc32_byte (chksum, *string);
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

static GTY(()) unsigned anon_cnt = 0; /* Saved for PCH.  */

/* Create a unique anonymous identifier.  The identifier is still a
   valid assembly label.  */

tree
make_anon_name ()
{
  const char *fmt = 
#if !defined (NO_DOT_IN_LABEL)
    "."
#elif !defined (NO_DOLLAR_IN_LABEL)
    "$"
#else
    "_"
#endif
    "_anon_%d";

  char buf[24];
  int len = snprintf (buf, sizeof (buf), fmt, anon_cnt++);
  gcc_checking_assert (len < int (sizeof (buf)));

  tree id = get_identifier_with_length (buf, len);
  IDENTIFIER_ANON_P (id) = true;

  return id;
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
	   || (startswith (type, "sub_")
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
      q = (char *) alloca (9 + 19 + len + 1);
      memcpy (q, file, len + 1);

      snprintf (q + len, 9 + 19 + 1, "_%08X_" HOST_WIDE_INT_PRINT_HEX,
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
  internal_error ("tree check: expected %<omp_clause %s%>, have %qs "
		  "in %s, at %s:%d",
		  omp_clause_code_name[code],
		  get_tree_code_name (TREE_CODE (node)),
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
    ("tree check: accessed elt %d of %<tree_int_cst%> with %d elts in %s, "
     "at %s:%d",
     idx + 1, len, function, trim_filename (file), line);
}

/* Similar to above, except that the check is for the bounds of a TREE_VEC's
   (dynamically sized) vector.  */

void
tree_vec_elt_check_failed (int idx, int len, const char *file, int line,
			   const char *function)
{
  internal_error
    ("tree check: accessed elt %d of %<tree_vec%> with %d elts in %s, at %s:%d",
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
    ("tree check: accessed operand %d of %<omp_clause %s%> with %d operands "
     "in %s, at %s:%d", idx + 1, omp_clause_code_name[OMP_CLAUSE_CODE (t)],
     omp_clause_num_ops [OMP_CLAUSE_CODE (t)], function,
     trim_filename (file), line);
}
#endif /* ENABLE_TREE_CHECKING */

/* Create a new vector type node holding NUNITS units of type INNERTYPE,
   and mapped to the machine mode MODE.  Initialize its fields and build
   the information necessary for debugging output.  */

static tree
make_vector_type (tree innertype, poly_int64 nunits, machine_mode mode)
{
  tree t;
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

  hashval_t hash = type_hash_canon_hash (t);
  t = type_hash_canon (hash, t);

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

/* Information about the _FloatN and _FloatNx types.  This must be in
   the same order as the corresponding TI_* enum values.  */
const floatn_type_info floatn_nx_types[NUM_FLOATN_NX_TYPES] =
  {
    { 16, false },
    { 32, false },
    { 64, false },
    { 128, false },
    { 32, true },
    { 64, true },
    { 128, true },
  };


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

      if (int_n_enabled_p[i])
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
	    char name[50], altname[50];
	    sprintf (name, "__int%d unsigned", int_n_data[i].bitsize);
	    sprintf (altname, "__int%d__ unsigned", int_n_data[i].bitsize);

	    if (strcmp (name, SIZE_TYPE) == 0
		|| strcmp (altname, SIZE_TYPE) == 0)
	      {
		size_type_node = int_n_trees[i].unsigned_type;
	      }
	  }
      if (size_type_node == NULL_TREE)
	gcc_unreachable ();
    }

  /* Define what type to use for ptrdiff_t.  */
  if (strcmp (PTRDIFF_TYPE, "int") == 0)
    ptrdiff_type_node = integer_type_node;
  else if (strcmp (PTRDIFF_TYPE, "long int") == 0)
    ptrdiff_type_node = long_integer_type_node;
  else if (strcmp (PTRDIFF_TYPE, "long long int") == 0)
    ptrdiff_type_node = long_long_integer_type_node;
  else if (strcmp (PTRDIFF_TYPE, "short int") == 0)
    ptrdiff_type_node = short_integer_type_node;
  else
    {
      ptrdiff_type_node = NULL_TREE;
      for (int i = 0; i < NUM_INT_N_ENTS; i++)
	if (int_n_enabled_p[i])
	  {
	    char name[50], altname[50];
	    sprintf (name, "__int%d", int_n_data[i].bitsize);
	    sprintf (altname, "__int%d__", int_n_data[i].bitsize);

	    if (strcmp (name, PTRDIFF_TYPE) == 0
		|| strcmp (altname, PTRDIFF_TYPE) == 0)
	      ptrdiff_type_node = int_n_trees[i].signed_type;
	  }
      if (ptrdiff_type_node == NULL_TREE)
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

  /* We are not going to have real types in C with less than byte alignment,
     so we might as well not have any types that claim to have it.  */
  SET_TYPE_ALIGN (void_type_node, BITS_PER_UNIT);
  TYPE_USER_ALIGN (void_type_node) = 0;

  void_node = make_node (VOID_CST);
  TREE_TYPE (void_node) = void_type_node;

  void_list_node = build_tree_list (NULL_TREE, void_type_node);

  null_pointer_node = build_int_cst (build_pointer_type (void_type_node), 0);
  layout_type (TREE_TYPE (null_pointer_node));

  ptr_type_node = build_pointer_type (void_type_node);
  const_ptr_type_node
    = build_pointer_type (build_type_variant (void_type_node, 1, 0));
  for (unsigned i = 0; i < ARRAY_SIZE (builtin_structptr_types); ++i)
    builtin_structptr_types[i].node = builtin_structptr_types[i].base;

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

  for (i = 0; i < NUM_FLOATN_NX_TYPES; i++)
    {
      int n = floatn_nx_types[i].n;
      bool extended = floatn_nx_types[i].extended;
      scalar_float_mode mode;
      if (!targetm.floatn_mode (n, extended).exists (&mode))
	continue;
      int precision = GET_MODE_PRECISION (mode);
      /* Work around the rs6000 KFmode having precision 113 not
	 128.  */
      const struct real_format *fmt = REAL_MODE_FORMAT (mode);
      gcc_assert (fmt->b == 2 && fmt->emin + fmt->emax == 3);
      int min_precision = fmt->p + ceil_log2 (fmt->emax - fmt->emin);
      if (!extended)
	gcc_assert (min_precision == n);
      if (precision < min_precision)
	precision = min_precision;
      FLOATN_NX_TYPE_NODE (i) = make_node (REAL_TYPE);
      TYPE_PRECISION (FLOATN_NX_TYPE_NODE (i)) = precision;
      layout_type (FLOATN_NX_TYPE_NODE (i));
      SET_TYPE_MODE (FLOATN_NX_TYPE_NODE (i), mode);
    }
  float128t_type_node = float128_type_node;
#ifdef HAVE_BFmode
  if (REAL_MODE_FORMAT (BFmode) == &arm_bfloat_half_format
      && targetm.scalar_mode_supported_p (BFmode)
      && targetm.libgcc_floating_mode_supported_p (BFmode))
    {
      bfloat16_type_node = make_node (REAL_TYPE);
      TYPE_PRECISION (bfloat16_type_node) = GET_MODE_PRECISION (BFmode);
      layout_type (bfloat16_type_node);
      SET_TYPE_MODE (bfloat16_type_node, BFmode);
    }
#endif

  float_ptr_type_node = build_pointer_type (float_type_node);
  double_ptr_type_node = build_pointer_type (double_type_node);
  long_double_ptr_type_node = build_pointer_type (long_double_type_node);
  integer_ptr_type_node = build_pointer_type (integer_type_node);

  /* Fixed size integer types.  */
  uint16_type_node = make_or_reuse_type (16, 1);
  uint32_type_node = make_or_reuse_type (32, 1);
  uint64_type_node = make_or_reuse_type (64, 1);
  if (targetm.scalar_mode_supported_p (TImode))
    uint128_type_node = make_or_reuse_type (128, 1);

  /* Decimal float types. */
  if (targetm.decimal_float_supported_p ())
    {
      dfloat32_type_node = make_node (REAL_TYPE);
      TYPE_PRECISION (dfloat32_type_node) = DECIMAL32_TYPE_SIZE;
      SET_TYPE_MODE (dfloat32_type_node, SDmode);
      layout_type (dfloat32_type_node);

      dfloat64_type_node = make_node (REAL_TYPE);
      TYPE_PRECISION (dfloat64_type_node) = DECIMAL64_TYPE_SIZE;
      SET_TYPE_MODE (dfloat64_type_node, DDmode);
      layout_type (dfloat64_type_node);

      dfloat128_type_node = make_node (REAL_TYPE);
      TYPE_PRECISION (dfloat128_type_node) = DECIMAL128_TYPE_SIZE;
      SET_TYPE_MODE (dfloat128_type_node, TDmode);
      layout_type (dfloat128_type_node);
    }

  complex_integer_type_node = build_complex_type (integer_type_node, true);
  complex_float_type_node = build_complex_type (float_type_node, true);
  complex_double_type_node = build_complex_type (double_type_node, true);
  complex_long_double_type_node = build_complex_type (long_double_type_node,
						      true);

  for (i = 0; i < NUM_FLOATN_NX_TYPES; i++)
    {
      if (FLOATN_NX_TYPE_NODE (i) != NULL_TREE)
	COMPLEX_FLOATN_NX_TYPE_NODE (i)
	  = build_complex_type (FLOATN_NX_TYPE_NODE (i));
    }

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

  /* SCEV analyzer global shared trees.  */
  chrec_dont_know = make_node (SCEV_NOT_KNOWN);
  TREE_TYPE (chrec_dont_know) = void_type_node;
  chrec_known = make_node (SCEV_KNOWN);
  TREE_TYPE (chrec_known) = void_type_node;
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
  if (flags & ECF_COLD)
    DECL_ATTRIBUTES (decl) = tree_cons (get_identifier ("cold"),
					NULL, DECL_ATTRIBUTES (decl));
  if (flags & ECF_RET1)
    DECL_ATTRIBUTES (decl)
      = tree_cons (get_identifier ("fn spec"),
		   build_tree_list (NULL_TREE, build_string (2, "1 ")),
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

  if (!builtin_decl_explicit_p (BUILT_IN_CLEAR_PADDING))
    {
      ftype = build_function_type_list (void_type_node,
					ptr_type_node,
					ptr_type_node,
					integer_type_node,
					NULL_TREE);
      local_define_builtin ("__builtin_clear_padding", ftype,
			    BUILT_IN_CLEAR_PADDING,
			    "__builtin_clear_padding",
			    ECF_LEAF | ECF_NOTHROW);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_UNREACHABLE)
      || !builtin_decl_explicit_p (BUILT_IN_TRAP)
      || !builtin_decl_explicit_p (BUILT_IN_UNREACHABLE_TRAP)
      || !builtin_decl_explicit_p (BUILT_IN_ABORT))
    {
      ftype = build_function_type (void_type_node, void_list_node);
      if (!builtin_decl_explicit_p (BUILT_IN_UNREACHABLE))
	local_define_builtin ("__builtin_unreachable", ftype,
			      BUILT_IN_UNREACHABLE,
			      "__builtin_unreachable",
			      ECF_NOTHROW | ECF_LEAF | ECF_NORETURN
			      | ECF_CONST | ECF_COLD);
      if (!builtin_decl_explicit_p (BUILT_IN_UNREACHABLE_TRAP))
	local_define_builtin ("__builtin_unreachable trap", ftype,
			      BUILT_IN_UNREACHABLE_TRAP,
			      "__builtin_unreachable trap",
			      ECF_NOTHROW | ECF_LEAF | ECF_NORETURN
			      | ECF_CONST | ECF_COLD);
      if (!builtin_decl_explicit_p (BUILT_IN_ABORT))
	local_define_builtin ("__builtin_abort", ftype, BUILT_IN_ABORT,
			      "abort",
			      ECF_LEAF | ECF_NORETURN | ECF_CONST | ECF_COLD);
      if (!builtin_decl_explicit_p (BUILT_IN_TRAP))
	local_define_builtin ("__builtin_trap", ftype, BUILT_IN_TRAP,
			      "__builtin_trap",
			      ECF_NORETURN | ECF_NOTHROW | ECF_LEAF | ECF_COLD);
    }

  if (!builtin_decl_explicit_p (BUILT_IN_MEMCPY)
      || !builtin_decl_explicit_p (BUILT_IN_MEMMOVE))
    {
      ftype = build_function_type_list (ptr_type_node,
					ptr_type_node, const_ptr_type_node,
					size_type_node, NULL_TREE);

      if (!builtin_decl_explicit_p (BUILT_IN_MEMCPY))
	local_define_builtin ("__builtin_memcpy", ftype, BUILT_IN_MEMCPY,
			      "memcpy", ECF_NOTHROW | ECF_LEAF);
      if (!builtin_decl_explicit_p (BUILT_IN_MEMMOVE))
	local_define_builtin ("__builtin_memmove", ftype, BUILT_IN_MEMMOVE,
			      "memmove", ECF_NOTHROW | ECF_LEAF);
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
			    "memset", ECF_NOTHROW | ECF_LEAF);
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

  ftype = build_function_type_list (ptr_type_node, size_type_node,
				    size_type_node, size_type_node, NULL_TREE);
  local_define_builtin ("__builtin_alloca_with_align_and_max", ftype,
			BUILT_IN_ALLOCA_WITH_ALIGN_AND_MAX,
			"__builtin_alloca_with_align_and_max",
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
  local_define_builtin ("__builtin_init_descriptor", ftype,
			BUILT_IN_INIT_DESCRIPTOR,
			"__builtin_init_descriptor", ECF_NOTHROW | ECF_LEAF);

  ftype = build_function_type_list (ptr_type_node, ptr_type_node, NULL_TREE);
  local_define_builtin ("__builtin_adjust_trampoline", ftype,
			BUILT_IN_ADJUST_TRAMPOLINE,
			"__builtin_adjust_trampoline",
			ECF_CONST | ECF_NOTHROW);
  local_define_builtin ("__builtin_adjust_descriptor", ftype,
			BUILT_IN_ADJUST_DESCRIPTOR,
			"__builtin_adjust_descriptor",
			ECF_CONST | ECF_NOTHROW);

  ftype = build_function_type_list (void_type_node,
				    ptr_type_node, ptr_type_node, NULL_TREE);
  if (!builtin_decl_explicit_p (BUILT_IN_CLEAR_CACHE))
    local_define_builtin ("__builtin___clear_cache", ftype,
			  BUILT_IN_CLEAR_CACHE,
			  "__clear_cache",
			  ECF_NOTHROW);

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

  local_define_builtin ("__builtin_strncmp_eq", ftype, BUILT_IN_STRNCMP_EQ,
			"__builtin_strncmp_eq",
			ECF_PURE | ECF_NOTHROW | ECF_LEAF);

  local_define_builtin ("__builtin_strcmp_eq", ftype, BUILT_IN_STRCMP_EQ,
			"__builtin_strcmp_eq",
			ECF_PURE | ECF_NOTHROW | ECF_LEAF);

  /* If there's a possibility that we might use the ARM EABI, build the
    alternate __cxa_end_cleanup node used to resume from C++.  */
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

	/* For -ftrapping-math these should throw from a former
	   -fnon-call-exception stmt.  */
	built_in_names[mcode] = concat (prefix, "mul", mode_name_buf, "3",
					NULL);
        local_define_builtin (built_in_names[mcode], ftype, mcode,
			      built_in_names[mcode],
			      ECF_CONST | ECF_LEAF);

	built_in_names[dcode] = concat (prefix, "div", mode_name_buf, "3",
					NULL);
        local_define_builtin (built_in_names[dcode], ftype, dcode,
			      built_in_names[dcode],
			      ECF_CONST | ECF_LEAF);
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
      outer = build_function_type (inner, TYPE_ARG_TYPES (type),
				   TYPE_NO_NAMED_ARGS_STDARG_P (type));
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
  poly_int64 nunits;
  unsigned int bitsize;

  switch (GET_MODE_CLASS (mode))
    {
    case MODE_VECTOR_BOOL:
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
      bitsize = GET_MODE_BITSIZE (as_a <scalar_int_mode> (mode));
      gcc_assert (bitsize % TREE_INT_CST_LOW (TYPE_SIZE (innertype)) == 0);
      nunits = bitsize / TREE_INT_CST_LOW (TYPE_SIZE (innertype));
      break;

    default:
      gcc_unreachable ();
    }

  return make_vector_type (innertype, nunits, mode);
}

/* Similarly, but takes the inner type and number of units, which must be
   a power of two.  */

tree
build_vector_type (tree innertype, poly_int64 nunits)
{
  return make_vector_type (innertype, nunits, VOIDmode);
}

/* Build a truth vector with NUNITS units, giving it mode MASK_MODE.  */

tree
build_truth_vector_type_for_mode (poly_uint64 nunits, machine_mode mask_mode)
{
  gcc_assert (mask_mode != BLKmode);

  unsigned HOST_WIDE_INT esize;
  if (VECTOR_MODE_P (mask_mode))
    {
      poly_uint64 vsize = GET_MODE_BITSIZE (mask_mode);
      esize = vector_element_size (vsize, nunits);
    }
  else
    esize = 1;

  tree bool_type = build_nonstandard_boolean_type (esize);

  return make_vector_type (bool_type, nunits, mask_mode);
}

/* Build a vector type that holds one boolean result for each element of
   vector type VECTYPE.  The public interface for this operation is
   truth_type_for.  */

static tree
build_truth_vector_type_for (tree vectype)
{
  machine_mode vector_mode = TYPE_MODE (vectype);
  poly_uint64 nunits = TYPE_VECTOR_SUBPARTS (vectype);

  machine_mode mask_mode;
  if (VECTOR_MODE_P (vector_mode)
      && targetm.vectorize.get_mask_mode (vector_mode).exists (&mask_mode))
    return build_truth_vector_type_for_mode (nunits, mask_mode);

  poly_uint64 vsize = tree_to_poly_uint64 (TYPE_SIZE (vectype));
  unsigned HOST_WIDE_INT esize = vector_element_size (vsize, nunits);
  tree bool_type = build_nonstandard_boolean_type (esize);

  return make_vector_type (bool_type, nunits, VOIDmode);
}

/* Like build_vector_type, but builds a variant type with TYPE_VECTOR_OPAQUE
   set.  */

tree
build_opaque_vector_type (tree innertype, poly_int64 nunits)
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

/* Return the value of element I of VECTOR_CST T as a wide_int.  */

static poly_wide_int
vector_cst_int_elt (const_tree t, unsigned int i)
{
  /* First handle elements that are directly encoded.  */
  unsigned int encoded_nelts = vector_cst_encoded_nelts (t);
  if (i < encoded_nelts)
    return wi::to_poly_wide (VECTOR_CST_ENCODED_ELT (t, i));

  /* Identify the pattern that contains element I and work out the index of
     the last encoded element for that pattern.  */
  unsigned int npatterns = VECTOR_CST_NPATTERNS (t);
  unsigned int pattern = i % npatterns;
  unsigned int count = i / npatterns;
  unsigned int final_i = encoded_nelts - npatterns + pattern;

  /* If there are no steps, the final encoded value is the right one.  */
  if (!VECTOR_CST_STEPPED_P (t))
    return wi::to_poly_wide (VECTOR_CST_ENCODED_ELT (t, final_i));

  /* Otherwise work out the value from the last two encoded elements.  */
  tree v1 = VECTOR_CST_ENCODED_ELT (t, final_i - npatterns);
  tree v2 = VECTOR_CST_ENCODED_ELT (t, final_i);
  poly_wide_int diff = wi::to_poly_wide (v2) - wi::to_poly_wide (v1);
  return wi::to_poly_wide (v2) + (count - 2) * diff;
}

/* Return the value of element I of VECTOR_CST T.  */

tree
vector_cst_elt (const_tree t, unsigned int i)
{
  /* First handle elements that are directly encoded.  */
  unsigned int encoded_nelts = vector_cst_encoded_nelts (t);
  if (i < encoded_nelts)
    return VECTOR_CST_ENCODED_ELT (t, i);

  /* If there are no steps, the final encoded value is the right one.  */
  if (!VECTOR_CST_STEPPED_P (t))
    {
      /* Identify the pattern that contains element I and work out the index of
	 the last encoded element for that pattern.  */
      unsigned int npatterns = VECTOR_CST_NPATTERNS (t);
      unsigned int pattern = i % npatterns;
      unsigned int final_i = encoded_nelts - npatterns + pattern;
      return VECTOR_CST_ENCODED_ELT (t, final_i);
    }

  /* Otherwise work out the value from the last two encoded elements.  */
  return wide_int_to_tree (TREE_TYPE (TREE_TYPE (t)),
			   vector_cst_int_elt (t, i));
}

/* Given an initializer INIT, return TRUE if INIT is zero or some
   aggregate of zeros.  Otherwise return FALSE.  If NONZERO is not
   null, set *NONZERO if and only if INIT is known not to be all
   zeros.  The combination of return value of false and *NONZERO
   false implies that INIT may but need not be all zeros.  Other
   combinations indicate definitive answers.  */

bool
initializer_zerop (const_tree init, bool *nonzero /* = NULL */)
{
  bool dummy;
  if (!nonzero)
    nonzero = &dummy;

  /* Conservatively clear NONZERO and set it only if INIT is definitely
     not all zero.  */
  *nonzero = false;

  STRIP_NOPS (init);

  unsigned HOST_WIDE_INT off = 0;

  switch (TREE_CODE (init))
    {
    case INTEGER_CST:
      if (integer_zerop (init))
	return true;

      *nonzero = true;
      return false;

    case REAL_CST:
      /* ??? Note that this is not correct for C4X float formats.  There,
	 a bit pattern of all zeros is 1.0; 0.0 is encoded with the most
	 negative exponent.  */
      if (real_zerop (init)
	  && !REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (init)))
	return true;

      *nonzero = true;
      return false;

    case FIXED_CST:
      if (fixed_zerop (init))
	return true;

      *nonzero = true;
      return false;

    case COMPLEX_CST:
      if (integer_zerop (init)
	  || (real_zerop (init)
	      && !REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (TREE_REALPART (init)))
	      && !REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (TREE_IMAGPART (init)))))
	return true;

      *nonzero = true;
      return false;

    case VECTOR_CST:
      if (VECTOR_CST_NPATTERNS (init) == 1
	  && VECTOR_CST_DUPLICATE_P (init)
	  && initializer_zerop (VECTOR_CST_ENCODED_ELT (init, 0)))
	return true;

      *nonzero = true;
      return false;

    case CONSTRUCTOR:
      {
	if (TREE_CLOBBER_P (init))
	  return false;

	unsigned HOST_WIDE_INT idx;
	tree elt;

	FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (init), idx, elt)
	  if (!initializer_zerop (elt, nonzero))
	    return false;

	return true;
      }

    case MEM_REF:
      {
	tree arg = TREE_OPERAND (init, 0);
	if (TREE_CODE (arg) != ADDR_EXPR)
	  return false;
	tree offset = TREE_OPERAND (init, 1);
	if (TREE_CODE (offset) != INTEGER_CST
	    || !tree_fits_uhwi_p (offset))
	  return false;
	off = tree_to_uhwi (offset);
	if (INT_MAX < off)
	  return false;
	arg = TREE_OPERAND (arg, 0);
	if (TREE_CODE (arg) != STRING_CST)
	  return false;
	init = arg;
      }
      /* Fall through.  */

    case STRING_CST:
      {
	gcc_assert (off <= INT_MAX);

	int i = off;
	int n = TREE_STRING_LENGTH (init);
	if (n <= i)
	  return false;

	/* We need to loop through all elements to handle cases like
	   "\0" and "\0foobar".  */
	for (i = 0; i < n; ++i)
	  if (TREE_STRING_POINTER (init)[i] != '\0')
	    {
	      *nonzero = true;
	      return false;
	    }

	return true;
      }

    default:
      return false;
    }
}

/* Return true if EXPR is an initializer expression in which every element
   is a constant that is numerically equal to 0 or 1.  The elements do not
   need to be equal to each other.  */

bool
initializer_each_zero_or_onep (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);

  switch (TREE_CODE (expr))
    {
    case INTEGER_CST:
      return integer_zerop (expr) || integer_onep (expr);

    case REAL_CST:
      return real_zerop (expr) || real_onep (expr);

    case VECTOR_CST:
      {
	unsigned HOST_WIDE_INT nelts = vector_cst_encoded_nelts (expr);
	if (VECTOR_CST_STEPPED_P (expr)
	    && !TYPE_VECTOR_SUBPARTS (TREE_TYPE (expr)).is_constant (&nelts))
	  return false;

	for (unsigned int i = 0; i < nelts; ++i)
	  {
	    tree elt = vector_cst_elt (expr, i);
	    if (!initializer_each_zero_or_onep (elt))
	      return false;
	  }

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
  unsigned HOST_WIDE_INT i, nelts;

  if (vec == NULL_TREE)
    return NULL_TREE;

  gcc_assert (VECTOR_TYPE_P (TREE_TYPE (vec)));

  if (TREE_CODE (vec) == VEC_DUPLICATE_EXPR)
    return TREE_OPERAND (vec, 0);

  else if (TREE_CODE (vec) == VECTOR_CST)
    {
      if (VECTOR_CST_NPATTERNS (vec) == 1 && VECTOR_CST_DUPLICATE_P (vec))
	return VECTOR_CST_ENCODED_ELT (vec, 0);
      return NULL_TREE;
    }

  else if (TREE_CODE (vec) == CONSTRUCTOR
	   && TYPE_VECTOR_SUBPARTS (TREE_TYPE (vec)).is_constant (&nelts))
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
      if (i != nelts)
	return NULL_TREE;

      if (TREE_CODE (first) == CONSTRUCTOR || TREE_CODE (first) == VECTOR_CST)
	return uniform_vector_p (first);
      return first;
    }

  return NULL_TREE;
}

/* If the argument is INTEGER_CST, return it.  If the argument is vector
   with all elements the same INTEGER_CST, return that INTEGER_CST.  Otherwise
   return NULL_TREE.
   Look through location wrappers. */

tree
uniform_integer_cst_p (tree t)
{
  STRIP_ANY_LOCATION_WRAPPER (t);

  if (TREE_CODE (t) == INTEGER_CST)
    return t;

  if (VECTOR_TYPE_P (TREE_TYPE (t)))
    {
      t = uniform_vector_p (t);
      if (t && TREE_CODE (t) == INTEGER_CST)
	return t;
    }

  return NULL_TREE;
}

/* Checks to see if T is a constant or a constant vector and if each element E
   adheres to ~E + 1 == pow2 then return ~E otherwise NULL_TREE.  */

tree
bitmask_inv_cst_vector_p (tree t)
{

  tree_code code = TREE_CODE (t);
  tree type = TREE_TYPE (t);

  if (!INTEGRAL_TYPE_P (type)
      && !VECTOR_INTEGER_TYPE_P (type))
    return NULL_TREE;

  unsigned HOST_WIDE_INT nelts = 1;
  tree cst;
  unsigned int idx = 0;
  bool uniform = uniform_integer_cst_p (t);
  tree newtype = unsigned_type_for (type);
  tree_vector_builder builder;
  if (code == INTEGER_CST)
    cst = t;
  else
    {
      if (!VECTOR_CST_NELTS (t).is_constant (&nelts))
	return NULL_TREE;

      cst = vector_cst_elt (t, 0);
      builder.new_vector (newtype, nelts, 1);
    }

  tree ty = unsigned_type_for (TREE_TYPE (cst));

  do
    {
      if (idx > 0)
	cst = vector_cst_elt (t, idx);
      wide_int icst = wi::to_wide (cst);
      wide_int inv =  wi::bit_not (icst);
      icst = wi::add (1, inv);
      if (wi::popcount (icst) != 1)
	return NULL_TREE;

      tree newcst = wide_int_to_tree (ty, inv);

      if (uniform)
	return build_uniform_cst (newtype, newcst);

      builder.quick_push (newcst);
    }
  while (++idx < nelts);

  return builder.build ();
}

/* If VECTOR_CST T has a single nonzero element, return the index of that
   element, otherwise return -1.  */

int
single_nonzero_element (const_tree t)
{
  unsigned HOST_WIDE_INT nelts;
  unsigned int repeat_nelts;
  if (VECTOR_CST_NELTS (t).is_constant (&nelts))
    repeat_nelts = nelts;
  else if (VECTOR_CST_NELTS_PER_PATTERN (t) == 2)
    {
      nelts = vector_cst_encoded_nelts (t);
      repeat_nelts = VECTOR_CST_NPATTERNS (t);
    }
  else
    return -1;

  int res = -1;
  for (unsigned int i = 0; i < nelts; ++i)
    {
      tree elt = vector_cst_elt (t, i);
      if (!integer_zerop (elt) && !real_zerop (elt))
	{
	  if (res >= 0 || i >= repeat_nelts)
	    return -1;
	  res = i;
	}
    }
  return res;
}

/* Build an empty statement at location LOC.  */

tree
build_empty_stmt (location_t loc)
{
  tree t = build1 (NOP_EXPR, void_type_node, size_zero_node);
  SET_EXPR_LOCATION (t, loc);
  return t;
}


/* Build an OMP clause with code CODE.  LOC is the location of the
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
build_vl_exp (enum tree_code code, int len MEM_STAT_DECL)
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
build_call_vec (tree return_type, tree fn, const vec<tree, va_gc> *args)
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
  process_call_operands (t);
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

/* Return a function call to the appropriate builtin alloca variant.

   SIZE is the size to be allocated.  ALIGN, if non-zero, is the requested
   alignment of the allocated area.  MAX_SIZE, if non-negative, is an upper
   bound for SIZE in case it is not a fixed value.  */

tree
build_alloca_call_expr (tree size, unsigned int align, HOST_WIDE_INT max_size)
{
  if (max_size >= 0)
    {
      tree t = builtin_decl_explicit (BUILT_IN_ALLOCA_WITH_ALIGN_AND_MAX);
      return
	build_call_expr (t, 3, size, size_int (align), size_int (max_size));
    }
  else if (align > 0)
    {
      tree t = builtin_decl_explicit (BUILT_IN_ALLOCA_WITH_ALIGN);
      return build_call_expr (t, 2, size, size_int (align));
    }
  else
    {
      tree t = builtin_decl_explicit (BUILT_IN_ALLOCA);
      return build_call_expr (t, 1, size);
    }
}

/* The built-in decl to use to mark code points believed to be unreachable.
   Typically __builtin_unreachable, but __builtin_trap if
   -fsanitize=unreachable -fsanitize-trap=unreachable.  If only
   -fsanitize=unreachable, we rely on sanopt to replace calls with the
   appropriate ubsan function.  When building a call directly, use
   {gimple_},build_builtin_unreachable instead.  */

tree
builtin_decl_unreachable ()
{
  enum built_in_function fncode = BUILT_IN_UNREACHABLE;

  if (sanitize_flags_p (SANITIZE_UNREACHABLE)
      ? (flag_sanitize_trap & SANITIZE_UNREACHABLE)
      : flag_unreachable_traps)
    fncode = BUILT_IN_UNREACHABLE_TRAP;
  /* For non-trapping sanitize, we will rewrite __builtin_unreachable () later,
     in the sanopt pass.  */

  return builtin_decl_explicit (fncode);
}

/* Build a call to __builtin_unreachable, possibly rewritten by
   -fsanitize=unreachable.  Use this rather than the above when practical.  */

tree
build_builtin_unreachable (location_t loc)
{
  tree data = NULL_TREE;
  tree fn = sanitize_unreachable_fn (&data, loc);
  return build_call_expr_loc (loc, fn, data != NULL_TREE, data);
}

/* Create a new constant string literal of type ELTYPE[SIZE] (or LEN
   if SIZE == -1) and return a tree node representing char* pointer to
   it as an ADDR_EXPR (ARRAY_REF (ELTYPE, ...)).  When STR is nonnull
   the STRING_CST value is the LEN bytes at STR (the representation
   of the string, which may be wide).  Otherwise it's all zeros.  */

tree
build_string_literal (unsigned len, const char *str /* = NULL */,
		      tree eltype /* = char_type_node */,
		      unsigned HOST_WIDE_INT size /* = -1 */)
{
  tree t = build_string (len, str);
  /* Set the maximum valid index based on the string length or SIZE.  */
  unsigned HOST_WIDE_INT maxidx
    = (size == HOST_WIDE_INT_M1U ? len : size) - 1;

  tree index = build_index_type (size_int (maxidx));
  eltype = build_type_variant (eltype, 1, 0);
  tree type = build_array_type (eltype, index);
  TREE_TYPE (t) = type;
  TREE_CONSTANT (t) = 1;
  TREE_READONLY (t) = 1;
  TREE_STATIC (t) = 1;

  type = build_pointer_type (eltype);
  t = build1 (ADDR_EXPR, type,
	      build4 (ARRAY_REF, eltype,
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
	val |= HOST_WIDE_INT_M1U << (bits - 1) << 1;
      else
	val &= ~(HOST_WIDE_INT_M1U << (bits - 1) << 1);
    }

  return val;
}

/* If TYPE is an integral or pointer type, return an integer type with
   the same precision which is unsigned iff UNSIGNEDP is true, or itself
   if TYPE is already an integer type of signedness UNSIGNEDP.
   If TYPE is a floating-point type, return an integer type with the same
   bitsize and with the signedness given by UNSIGNEDP; this is useful
   when doing bit-level operations on a floating-point value.  */

tree
signed_or_unsigned_type_for (int unsignedp, tree type)
{
  if (ANY_INTEGRAL_TYPE_P (type) && TYPE_UNSIGNED (type) == unsignedp)
    return type;

  if (TREE_CODE (type) == VECTOR_TYPE)
    {
      tree inner = TREE_TYPE (type);
      tree inner2 = signed_or_unsigned_type_for (unsignedp, inner);
      if (!inner2)
	return NULL_TREE;
      if (inner == inner2)
	return type;
      machine_mode new_mode;
      if (VECTOR_MODE_P (TYPE_MODE (type))
	  && related_int_vector_mode (TYPE_MODE (type)).exists (&new_mode))
	return build_vector_type_for_mode (inner2, new_mode);
      return build_vector_type (inner2, TYPE_VECTOR_SUBPARTS (type));
    }

  if (TREE_CODE (type) == COMPLEX_TYPE)
    {
      tree inner = TREE_TYPE (type);
      tree inner2 = signed_or_unsigned_type_for (unsignedp, inner);
      if (!inner2)
	return NULL_TREE;
      if (inner == inner2)
	return type;
      return build_complex_type (inner2);
    }

  unsigned int bits;
  if (INTEGRAL_TYPE_P (type)
      || POINTER_TYPE_P (type)
      || TREE_CODE (type) == OFFSET_TYPE)
    bits = TYPE_PRECISION (type);
  else if (TREE_CODE (type) == REAL_TYPE)
    bits = GET_MODE_BITSIZE (SCALAR_TYPE_MODE (type));
  else
    return NULL_TREE;

  return build_nonstandard_integer_type (bits, unsignedp);
}

/* If TYPE is an integral or pointer type, return an integer type with
   the same precision which is unsigned, or itself if TYPE is already an
   unsigned integer type.  If TYPE is a floating-point type, return an
   unsigned integer type with the same bitsize as TYPE.  */

tree
unsigned_type_for (tree type)
{
  return signed_or_unsigned_type_for (1, type);
}

/* If TYPE is an integral or pointer type, return an integer type with
   the same precision which is signed, or itself if TYPE is already a
   signed integer type.  If TYPE is a floating-point type, return a
   signed integer type with the same bitsize as TYPE.  */

tree
signed_type_for (tree type)
{
  return signed_or_unsigned_type_for (0, type);
}

/* - For VECTOR_TYPEs:
    - The truth type must be a VECTOR_BOOLEAN_TYPE.
    - The number of elements must match (known_eq).
    - targetm.vectorize.get_mask_mode exists, and exactly
      the same mode as the truth type.
   - Otherwise, the truth type must be a BOOLEAN_TYPE
     or useless_type_conversion_p to BOOLEAN_TYPE.  */
bool
is_truth_type_for (tree type, tree truth_type)
{
  machine_mode mask_mode = TYPE_MODE (truth_type);
  machine_mode vmode = TYPE_MODE (type);
  machine_mode tmask_mode;

  if (TREE_CODE (type) == VECTOR_TYPE)
    {
      if (VECTOR_BOOLEAN_TYPE_P (truth_type)
	  && known_eq (TYPE_VECTOR_SUBPARTS (type),
		       TYPE_VECTOR_SUBPARTS (truth_type))
	  && targetm.vectorize.get_mask_mode (vmode).exists (&tmask_mode)
	  && tmask_mode == mask_mode)
	return true;

      return false;
    }

  return useless_type_conversion_p (boolean_type_node, truth_type);
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
      return build_truth_vector_type_for (type);
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
  return build_int_cst (TREE_TYPE (x), wi::ctz (wi::to_wide (x)));
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

      /* fall through */

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

    case TREE_VEC:
      {
	int len = TREE_VEC_LENGTH (*tp);

	if (len == 0)
	  break;

	/* Walk all elements but the last.  */
	for (int i = 0; i < len - 1; ++i)
	  WALK_SUBTREE (TREE_VEC_ELT (*tp, i));

	/* Now walk the last one as a tail call.  */
	WALK_SUBTREE_TAIL (TREE_VEC_ELT (*tp, len - 1));
      }

    case VECTOR_CST:
      {
	unsigned len = vector_cst_encoded_nelts (*tp);
	if (len == 0)
	  break;
	/* Walk all elements but the last.  */
	for (unsigned i = 0; i < len - 1; ++i)
	  WALK_SUBTREE (VECTOR_CST_ENCODED_ELT (*tp, i));
	/* Now walk the last one as a tail call.  */
	WALK_SUBTREE_TAIL (VECTOR_CST_ENCODED_ELT (*tp, len - 1));
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
      {
	int len = omp_clause_num_ops[OMP_CLAUSE_CODE (*tp)];
	for (int i = 0; i < len; i++)
	  WALK_SUBTREE (OMP_CLAUSE_OPERAND (*tp, i));
	WALK_SUBTREE_TAIL (OMP_CLAUSE_CHAIN (*tp));
      }

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
	  /* Call the function for the decl so e.g. copy_tree_body_r can
	     replace it with the remapped one.  */
	  result = (*func) (&DECL_EXPR_DECL (*tp), &walk_subtrees, data);
	  if (result || !walk_subtrees)
	    return result;

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

  if (TYPE_NO_NAMED_ARGS_STDARG_P (fntype))
    return true;

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

  if (TYPE_NO_NAMED_ARGS_STDARG_P (fntype))
    return true;

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

/* Return the location into which EXP has been inlined.  Analogous
   to tree_nonartificial_location() above but not limited to artificial
   functions declared inline.  If SYSTEM_HEADER is true, return
   the macro expansion point of the location if it's in a system header */

location_t
tree_inlined_location (tree exp, bool system_header /* = true */)
{
  location_t loc = UNKNOWN_LOCATION;

  tree block = TREE_BLOCK (exp);

  while (block && TREE_CODE (block) == BLOCK
	 && BLOCK_ABSTRACT_ORIGIN (block))
    {
      tree ao = BLOCK_ABSTRACT_ORIGIN (block);
      if (TREE_CODE (ao) == FUNCTION_DECL)
	loc = BLOCK_SOURCE_LOCATION (block);
      else if (TREE_CODE (ao) != BLOCK)
	break;

      block = BLOCK_SUPERCONTEXT (block);
    }

  if (loc == UNKNOWN_LOCATION)
    {
      loc = EXPR_LOCATION (exp);
      if (system_header)
	/* Only consider macro expansion when the block traversal failed
	   to find a location.  Otherwise it's not relevant.  */
	return expansion_point_location_if_in_system_header (loc);
    }

  return loc;
}

/* These are the hash table functions for the hash table of OPTIMIZATION_NODE
   nodes.  */

/* Return the hash code X, an OPTIMIZATION_NODE or TARGET_OPTION code.  */

hashval_t
cl_option_hasher::hash (tree x)
{
  const_tree const t = x;

  if (TREE_CODE (t) == OPTIMIZATION_NODE)
    return cl_optimization_hash (TREE_OPTIMIZATION (t));
  else if (TREE_CODE (t) == TARGET_OPTION_NODE)
    return cl_target_option_hash (TREE_TARGET_OPTION (t));
  else
    gcc_unreachable ();
}

/* Return nonzero if the value represented by *X (an OPTIMIZATION or
   TARGET_OPTION tree node) is the same as that given by *Y, which is the
   same.  */

bool
cl_option_hasher::equal (tree x, tree y)
{
  const_tree const xt = x;
  const_tree const yt = y;

  if (TREE_CODE (xt) != TREE_CODE (yt))
    return 0;

  if (TREE_CODE (xt) == OPTIMIZATION_NODE)
    return cl_optimization_option_eq (TREE_OPTIMIZATION (xt),
				      TREE_OPTIMIZATION (yt));
  else if (TREE_CODE (xt) == TARGET_OPTION_NODE)
    return cl_target_option_eq (TREE_TARGET_OPTION (xt),
				TREE_TARGET_OPTION (yt));
  else
    gcc_unreachable ();
}

/* Build an OPTIMIZATION_NODE based on the options in OPTS and OPTS_SET.  */

tree
build_optimization_node (struct gcc_options *opts,
			 struct gcc_options *opts_set)
{
  tree t;

  /* Use the cache of optimization nodes.  */

  cl_optimization_save (TREE_OPTIMIZATION (cl_optimization_node),
			opts, opts_set);

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

/* Build a TARGET_OPTION_NODE based on the options in OPTS and OPTS_SET.  */

tree
build_target_option_node (struct gcc_options *opts,
			  struct gcc_options *opts_set)
{
  tree t;

  /* Use the cache of optimization nodes.  */

  cl_target_option_save (TREE_TARGET_OPTION (cl_target_option_node),
			 opts, opts_set);

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

/* Determine the "ultimate origin" of a block.  */

tree
block_ultimate_origin (const_tree block)
{
  tree origin = BLOCK_ABSTRACT_ORIGIN (block);

  if (origin == NULL_TREE)
    return NULL_TREE;
  else
    {
      gcc_checking_assert ((DECL_P (origin)
			    && DECL_ORIGIN (origin) == origin)
			   || BLOCK_ORIGIN (origin) == origin);
      return origin;
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

  if (location_wrapper_p (exp))
    return true;
  if (!CONVERT_EXPR_P (exp)
      && TREE_CODE (exp) != NON_LVALUE_EXPR)
    return false;

  outer_type = TREE_TYPE (exp);
  inner_type = TREE_TYPE (TREE_OPERAND (exp, 0));
  if (!inner_type || inner_type == error_mark_node)
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

/* Strip handled components with zero offset from OP.  */

tree
strip_zero_offset_components (tree op)
{
  while (TREE_CODE (op) == COMPONENT_REF
	 && integer_zerop (DECL_FIELD_OFFSET (TREE_OPERAND (op, 1)))
	 && integer_zerop (DECL_FIELD_BIT_OFFSET (TREE_OPERAND (op, 1))))
    op = TREE_OPERAND (op, 0);
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
   can't apply.)  FOR_DUMP_P is true when being called from
   the dump routines.  */

bool
virtual_method_call_p (const_tree target, bool for_dump_p)
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
  if (!TYPE_BINFO (obj_type_ref_class (target, for_dump_p)))
    return false;
  return true;
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
get_binfo_at_offset (tree binfo, poly_int64 offset, tree expected_type)
{
  tree type = BINFO_TYPE (binfo);

  while (true)
    {
      HOST_WIDE_INT pos, size;
      tree fld;
      int i;

      if (types_same_for_odr (type, expected_type))
	  return binfo;
      if (maybe_lt (offset, 0))
	return NULL_TREE;

      for (fld = TYPE_FIELDS (type); fld; fld = DECL_CHAIN (fld))
	{
	  if (TREE_CODE (fld) != FIELD_DECL || !DECL_ARTIFICIAL (fld))
	    continue;

	  pos = int_bit_position (fld);
	  size = tree_to_uhwi (DECL_SIZE (fld));
	  if (known_in_range_p (offset, pos, size))
	    break;
	}
      if (!fld || TREE_CODE (TREE_TYPE (fld)) != RECORD_TYPE)
	return NULL_TREE;

      /* Offset 0 indicates the primary base, whose vtable contents are
	 represented in the binfo for the derived class.  */
      else if (maybe_ne (offset, 0))
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

/* PR 84195: Replace control characters in "unescaped" with their
   escaped equivalents.  Allow newlines if -fmessage-length has
   been set to a non-zero value.  This is done here, rather than
   where the attribute is recorded as the message length can
   change between these two locations.  */

void
escaped_string::escape (const char *unescaped)
{
  char *escaped;
  size_t i, new_i, len;

  if (m_owned)
    free (m_str);

  m_str = const_cast<char *> (unescaped);
  m_owned = false;

  if (unescaped == NULL || *unescaped == 0)
    return;

  len = strlen (unescaped);
  escaped = NULL;
  new_i = 0;

  for (i = 0; i < len; i++)
    {
      char c = unescaped[i];

      if (!ISCNTRL (c))
	{
	  if (escaped)
	    escaped[new_i++] = c;
	  continue;
	}

      if (c != '\n' || !pp_is_wrapping_line (global_dc->printer))
	{
	  if (escaped == NULL)
	    {
	      /* We only allocate space for a new string if we
		 actually encounter a control character that
		 needs replacing.  */
	      escaped = (char *) xmalloc (len * 2 + 1);
	      strncpy (escaped, unescaped, i);
	      new_i = i;
	    }

	  escaped[new_i++] = '\\';

	  switch (c)
	    {
	    case '\a': escaped[new_i++] = 'a'; break;
	    case '\b': escaped[new_i++] = 'b'; break;
	    case '\f': escaped[new_i++] = 'f'; break;
	    case '\n': escaped[new_i++] = 'n'; break;
	    case '\r': escaped[new_i++] = 'r'; break;
	    case '\t': escaped[new_i++] = 't'; break;
	    case '\v': escaped[new_i++] = 'v'; break;
	    default:   escaped[new_i++] = '?'; break;
	    }
	}
      else if (escaped)
	escaped[new_i++] = c;
    }

  if (escaped)
    {
      escaped[new_i] = 0;
      m_str = escaped;
      m_owned = true;
    }
}

/* Warn about a use of an identifier which was marked deprecated.  Returns
   whether a warning was given.  */

bool
warn_deprecated_use (tree node, tree attr)
{
  escaped_string msg;

  if (node == 0 || !warn_deprecated_decl)
    return false;

  if (!attr)
    {
      if (DECL_P (node))
	attr = DECL_ATTRIBUTES (node);
      else if (TYPE_P (node))
	{
	  tree decl = TYPE_STUB_DECL (node);
	  if (decl)
	    attr = TYPE_ATTRIBUTES (TREE_TYPE (decl));
	  else if ((decl = TYPE_STUB_DECL (TYPE_MAIN_VARIANT (node)))
		   != NULL_TREE)
	    {
	      node = TREE_TYPE (decl);
	      attr = TYPE_ATTRIBUTES (node);
	    }
	}
    }

  if (attr)
    attr = lookup_attribute ("deprecated", attr);

  if (attr)
    msg.escape (TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (attr))));

  bool w = false;
  if (DECL_P (node))
    {
      auto_diagnostic_group d;
      if (msg)
	w = warning (OPT_Wdeprecated_declarations,
		     "%qD is deprecated: %s", node, (const char *) msg);
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

      auto_diagnostic_group d;
      if (what)
	{
	  if (msg)
	    w = warning (OPT_Wdeprecated_declarations,
			 "%qE is deprecated: %s", what, (const char *) msg);
	  else
	    w = warning (OPT_Wdeprecated_declarations,
			 "%qE is deprecated", what);
	}
      else
	{
	  if (msg)
	    w = warning (OPT_Wdeprecated_declarations,
			 "type is deprecated: %s", (const char *) msg);
	  else
	    w = warning (OPT_Wdeprecated_declarations,
			 "type is deprecated");
	}

      if (w && decl)
	inform (DECL_SOURCE_LOCATION (decl), "declared here");
    }

  return w;
}

/* Error out with an identifier which was marked 'unavailable'. */
void
error_unavailable_use (tree node, tree attr)
{
  escaped_string msg;

  if (node == 0)
    return;

  if (!attr)
    {
      if (DECL_P (node))
	attr = DECL_ATTRIBUTES (node);
      else if (TYPE_P (node))
	{
	  tree decl = TYPE_STUB_DECL (node);
	  if (decl)
	    attr = lookup_attribute ("unavailable",
				     TYPE_ATTRIBUTES (TREE_TYPE (decl)));
	}
    }

  if (attr)
    attr = lookup_attribute ("unavailable", attr);

  if (attr)
    msg.escape (TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (attr))));

  if (DECL_P (node))
    {
      auto_diagnostic_group d;
      if (msg)
	error ("%qD is unavailable: %s", node, (const char *) msg);
      else
	error ("%qD is unavailable", node);
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

      auto_diagnostic_group d;
      if (what)
	{
	  if (msg)
	    error ("%qE is unavailable: %s", what, (const char *) msg);
	  else
	    error ("%qE is unavailable", what);
	}
      else
	{
	  if (msg)
	    error ("type is unavailable: %s", (const char *) msg);
	  else
	    error ("type is unavailable");
	}

      if (decl)
	inform (DECL_SOURCE_LOCATION (decl), "declared here");
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
      /* If there is a default: label or case labels cover all possible
	 SWITCH_COND values, then the SWITCH_EXPR will transfer control
	 to some case label in all cases and all we care is whether the
	 SWITCH_BODY falls through.  */
      if (SWITCH_ALL_CASES_P (stmt))
	return block_may_fallthru (SWITCH_BODY (stmt));
      return true;

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

    case EH_ELSE_EXPR:
      return block_may_fallthru (TREE_OPERAND (stmt, 0));

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

  /* The tree_code enum promotes to signed, but we could be getting
     invalid values, so force an unsigned comparison.  */
  if (unsigned (code) >= MAX_TREE_CODES)
    {
      if ((unsigned)code == 0xa5a5)
	return "ggc_freed";
      return invalid;
    }

  return tree_code_name[code];
}

/* Drops the TREE_OVERFLOW flag from T.  */

tree
drop_tree_overflow (tree t)
{
  gcc_checking_assert (TREE_OVERFLOW (t));

  /* For tree codes with a sharing machinery re-build the result.  */
  if (poly_int_tree_p (t))
    return wide_int_to_tree (TREE_TYPE (t), wi::to_poly_wide (t));

  /* For VECTOR_CST, remove the overflow bits from the encoded elements
     and canonicalize the result.  */
  if (TREE_CODE (t) == VECTOR_CST)
    {
      tree_vector_builder builder;
      builder.new_unary_operation (TREE_TYPE (t), t, true);
      unsigned int count = builder.encoded_nelts ();
      for (unsigned int i = 0; i < count; ++i)
	{
	  tree elt = VECTOR_CST_ELT (t, i);
	  if (TREE_OVERFLOW (elt))
	    elt = drop_tree_overflow (elt);
	  builder.quick_push (elt);
	}
      return builder.build ();
    }

  /* Otherwise, as all tcc_constants are possibly shared, copy the node
     and drop the flag.  */
  t = copy_node (t);
  TREE_OVERFLOW (t) = 0;

  /* For constants that contain nested constants, drop the flag
     from those as well.  */
  if (TREE_CODE (t) == COMPLEX_CST)
    {
      if (TREE_OVERFLOW (TREE_REALPART (t)))
	TREE_REALPART (t) = drop_tree_overflow (TREE_REALPART (t));
      if (TREE_OVERFLOW (TREE_IMAGPART (t)))
	TREE_IMAGPART (t) = drop_tree_overflow (TREE_IMAGPART (t));
    }

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
  if (TREE_CODE (t) == WITH_SIZE_EXPR)
    t = TREE_OPERAND (t, 0);
  while (handled_component_p (t))
    t = TREE_OPERAND (t, 0);

  if ((TREE_CODE (t) == MEM_REF
       || TREE_CODE (t) == TARGET_MEM_REF)
      && TREE_CODE (TREE_OPERAND (t, 0)) == ADDR_EXPR)
    t = TREE_OPERAND (TREE_OPERAND (t, 0), 0);

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
  tree idxtype = TREE_TYPE (TREE_OPERAND (exp, 1));
  return (idxtype == error_mark_node
	  ? integer_zero_node : build_int_cst (idxtype, 0));
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

/* Returns true if REF is an array reference, a component reference,
   or a memory reference to an array whose actual size might be larger
   than its upper bound implies, there are multiple cases:
   A. a ref to a flexible array member at the end of a structure;
   B. a ref to an array with a different type against the original decl;
      for example:

   short a[16] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
   (*((char(*)[16])&a[0]))[i+8]

   C. a ref to an array that was passed as a parameter;
      for example:

   int test (uint8_t *p, uint32_t t[1][1], int n) {
   for (int i = 0; i < 4; i++, p++)
     t[i][0] = ...;

   If non-null, set IS_TRAILING_ARRAY to true if the ref is the above case A.
*/

bool
array_ref_flexible_size_p (tree ref, bool *is_trailing_array /* = NULL */)
{
  /* The TYPE for this array referece.  */
  tree atype = NULL_TREE;
  /* The FIELD_DECL for the array field in the containing structure.  */
  tree afield_decl = NULL_TREE;
  /* Whether this array is the trailing array of a structure.  */
  bool is_trailing_array_tmp = false;
  if (!is_trailing_array)
    is_trailing_array = &is_trailing_array_tmp;

  if (TREE_CODE (ref) == ARRAY_REF
      || TREE_CODE (ref) == ARRAY_RANGE_REF)
    {
      atype = TREE_TYPE (TREE_OPERAND (ref, 0));
      ref = TREE_OPERAND (ref, 0);
    }
  else if (TREE_CODE (ref) == COMPONENT_REF
	   && TREE_CODE (TREE_TYPE (TREE_OPERAND (ref, 1))) == ARRAY_TYPE)
    {
      atype = TREE_TYPE (TREE_OPERAND (ref, 1));
      afield_decl = TREE_OPERAND (ref, 1);
    }
  else if (TREE_CODE (ref) == MEM_REF)
    {
      tree arg = TREE_OPERAND (ref, 0);
      if (TREE_CODE (arg) == ADDR_EXPR)
	arg = TREE_OPERAND (arg, 0);
      tree argtype = TREE_TYPE (arg);
      if (TREE_CODE (argtype) == RECORD_TYPE)
	{
	  if (tree fld = last_field (argtype))
	    {
	      atype = TREE_TYPE (fld);
	      afield_decl = fld;
	      if (TREE_CODE (atype) != ARRAY_TYPE)
		return false;
	      if (VAR_P (arg) && DECL_SIZE (fld))
		return false;
	    }
	  else
	    return false;
	}
      else
	return false;
    }
  else
    return false;

  if (TREE_CODE (ref) == STRING_CST)
    return false;

  tree ref_to_array = ref;
  while (handled_component_p (ref))
    {
      /* If the reference chain contains a component reference to a
         non-union type and there follows another field the reference
	 is not at the end of a structure.  */
      if (TREE_CODE (ref) == COMPONENT_REF)
	{
	  if (TREE_CODE (TREE_TYPE (TREE_OPERAND (ref, 0))) == RECORD_TYPE)
	    {
	      tree nextf = DECL_CHAIN (TREE_OPERAND (ref, 1));
	      while (nextf && TREE_CODE (nextf) != FIELD_DECL)
		nextf = DECL_CHAIN (nextf);
	      if (nextf)
		return false;
	    }
	}
      /* If we have a multi-dimensional array we do not consider
         a non-innermost dimension as flex array if the whole
	 multi-dimensional array is at struct end.
	 Same for an array of aggregates with a trailing array
	 member.  */
      else if (TREE_CODE (ref) == ARRAY_REF)
	return false;
      else if (TREE_CODE (ref) == ARRAY_RANGE_REF)
	;
      /* If we view an underlying object as sth else then what we
         gathered up to now is what we have to rely on.  */
      else if (TREE_CODE (ref) == VIEW_CONVERT_EXPR)
	break;
      else
	gcc_unreachable ();

      ref = TREE_OPERAND (ref, 0);
    }

  gcc_assert (!afield_decl
	      || (afield_decl && TREE_CODE (afield_decl) == FIELD_DECL));

  /* The array now is at struct end.  Treat flexible array member as
     always subject to extend, even into just padding constrained by
     an underlying decl.  */
  if (! TYPE_SIZE (atype)
      || ! TYPE_DOMAIN (atype)
      || ! TYPE_MAX_VALUE (TYPE_DOMAIN (atype)))
    {
      *is_trailing_array = afield_decl && TREE_CODE (afield_decl) == FIELD_DECL;
      return afield_decl ? !DECL_NOT_FLEXARRAY (afield_decl) : true;
    }

  /* If the reference is based on a declared entity, the size of the array
     is constrained by its given domain.  (Do not trust commons PR/69368).  */
  ref = get_base_address (ref);
  if (ref
      && DECL_P (ref)
      && !(flag_unconstrained_commons
	   && VAR_P (ref) && DECL_COMMON (ref))
      && DECL_SIZE_UNIT (ref)
      && TREE_CODE (DECL_SIZE_UNIT (ref)) == INTEGER_CST)
    {
      /* If the object itself is the array it is not at struct end.  */
      if (DECL_P (ref_to_array))
	return false;

      /* Check whether the array domain covers all of the available
         padding.  */
      poly_int64 offset;
      if (TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (atype))) != INTEGER_CST
	  || TREE_CODE (TYPE_MAX_VALUE (TYPE_DOMAIN (atype))) != INTEGER_CST
          || TREE_CODE (TYPE_MIN_VALUE (TYPE_DOMAIN (atype))) != INTEGER_CST)
	{
	  *is_trailing_array
	    = afield_decl && TREE_CODE (afield_decl) == FIELD_DECL;
	  return afield_decl ? !DECL_NOT_FLEXARRAY (afield_decl) : true;
	}
      if (! get_addr_base_and_unit_offset (ref_to_array, &offset))
	{
	  *is_trailing_array
	    = afield_decl && TREE_CODE (afield_decl) == FIELD_DECL;
	  return afield_decl ? !DECL_NOT_FLEXARRAY (afield_decl) : true;
	}

      /* If at least one extra element fits it is a flexarray.  */
      if (known_le ((wi::to_offset (TYPE_MAX_VALUE (TYPE_DOMAIN (atype)))
		     - wi::to_offset (TYPE_MIN_VALUE (TYPE_DOMAIN (atype)))
		     + 2)
		    * wi::to_offset (TYPE_SIZE_UNIT (TREE_TYPE (atype))),
		    wi::to_offset (DECL_SIZE_UNIT (ref)) - offset))
	{
	  *is_trailing_array
	    = afield_decl && TREE_CODE (afield_decl) == FIELD_DECL;
	  return afield_decl ? !DECL_NOT_FLEXARRAY (afield_decl) : true;
	}

      return false;
    }

  *is_trailing_array = afield_decl && TREE_CODE (afield_decl) == FIELD_DECL;
  return afield_decl ? !DECL_NOT_FLEXARRAY (afield_decl) : true;
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

/* Given the initializer INIT, return the initializer for the field
   DECL if it exists, otherwise null.  Used to obtain the initializer
   for a flexible array member and determine its size.  */

static tree
get_initializer_for (tree init, tree decl)
{
  STRIP_NOPS (init);

  tree fld, fld_init;
  unsigned HOST_WIDE_INT i;
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (init), i, fld, fld_init)
    {
      if (decl == fld)
	return fld_init;

      if (TREE_CODE (fld) == CONSTRUCTOR)
	{
	  fld_init = get_initializer_for (fld_init, decl);
	  if (fld_init)
	    return fld_init;
	}
    }

  return NULL_TREE;
}

/* Determines the special array member type for the array reference REF.  */
special_array_member
component_ref_sam_type (tree ref)
{
  special_array_member sam_type = special_array_member::none;

  tree member = TREE_OPERAND (ref, 1);
  tree memsize = DECL_SIZE_UNIT (member);
  if (memsize)
    {
      tree memtype = TREE_TYPE (member);
      if (TREE_CODE (memtype) != ARRAY_TYPE)
	return sam_type;

      bool trailing = false;
      (void) array_ref_flexible_size_p (ref, &trailing);
      bool zero_elts = integer_zerop (memsize);
      if (zero_elts && integer_zerop (TYPE_SIZE_UNIT (TREE_TYPE (memtype))))
	{
	  /* If array element has zero size, verify if it is a flexible
	     array member or zero length array.  Clear zero_elts if
	     it has one or more members or is a VLA member.  */
	  if (tree dom = TYPE_DOMAIN (memtype))
	    if (tree min = TYPE_MIN_VALUE (dom))
	      if (tree max = TYPE_MAX_VALUE (dom))
		if (TREE_CODE (min) != INTEGER_CST
		    || TREE_CODE (max) != INTEGER_CST
		    || !((integer_zerop (min) && integer_all_onesp (max))
			 || tree_int_cst_lt (max, min)))
		  zero_elts = false;
	}
      if (!trailing && !zero_elts)
	/* MEMBER is an interior array with more than one element.  */
	return special_array_member::int_n;

      if (zero_elts)
	{
	  if (trailing)
	    return special_array_member::trail_0;
	  else
	    return special_array_member::int_0;
	}

      if (!zero_elts)
	if (tree dom = TYPE_DOMAIN (memtype))
	  if (tree min = TYPE_MIN_VALUE (dom))
	    if (tree max = TYPE_MAX_VALUE (dom))
	      if (TREE_CODE (min) == INTEGER_CST
		  && TREE_CODE (max) == INTEGER_CST)
		{
		  offset_int minidx = wi::to_offset (min);
		  offset_int maxidx = wi::to_offset (max);
		  offset_int neltsm1 = maxidx - minidx;
		  if (neltsm1 > 0)
		    /* MEMBER is a trailing array with more than
		       one elements.  */
		    return special_array_member::trail_n;

		  if (neltsm1 == 0)
		    return special_array_member::trail_1;
		}
    }

  return sam_type;
}

/* Determines the size of the member referenced by the COMPONENT_REF
   REF, using its initializer expression if necessary in order to
   determine the size of an initialized flexible array member.
   If non-null, set *SAM to the type of special array member.
   Returns the size as sizetype (which might be zero for an object
   with an uninitialized flexible array member) or null if the size
   cannot be determined.  */

tree
component_ref_size (tree ref, special_array_member *sam /* = NULL */)
{
  gcc_assert (TREE_CODE (ref) == COMPONENT_REF);

  special_array_member sambuf;
  if (!sam)
    sam = &sambuf;
  *sam = component_ref_sam_type (ref);

  /* The object/argument referenced by the COMPONENT_REF and its type.  */
  tree arg = TREE_OPERAND (ref, 0);
  tree argtype = TREE_TYPE (arg);
  /* The referenced member.  */
  tree member = TREE_OPERAND (ref, 1);

  tree memsize = DECL_SIZE_UNIT (member);
  if (memsize)
    {
      tree memtype = TREE_TYPE (member);
      if (TREE_CODE (memtype) != ARRAY_TYPE)
	/* DECL_SIZE may be less than TYPE_SIZE in C++ when referring
	   to the type of a class with a virtual base which doesn't
	   reflect the size of the virtual's members (see pr97595).
	   If that's the case fail for now and implement something
	   more robust in the future.  */
	return (tree_int_cst_equal (memsize, TYPE_SIZE_UNIT (memtype))
		? memsize : NULL_TREE);

      /* 2-or-more elements arrays are treated as normal arrays by default.  */
      if (*sam == special_array_member::int_n
	  || *sam == special_array_member::trail_n)
	return memsize;

      tree afield_decl = TREE_OPERAND (ref, 1);
      gcc_assert (TREE_CODE (afield_decl) == FIELD_DECL);
      /* If the trailing array is a not a flexible array member, treat it as
	 a normal array.  */
      if (DECL_NOT_FLEXARRAY (afield_decl)
	  && *sam != special_array_member::int_0)
	return memsize;

      if (*sam == special_array_member::int_0)
	memsize = NULL_TREE;

      /* For a reference to a flexible array member of a union
	 use the size of the union instead of the size of the member.  */
      if (TREE_CODE (argtype) == UNION_TYPE)
	memsize = TYPE_SIZE_UNIT (argtype);
    }

  /* MEMBER is either a bona fide flexible array member, or a zero-elements
     array member, or an array of length one treated as such.  */

  /* If the reference is to a declared object and the member a true
     flexible array, try to determine its size from its initializer.  */
  poly_int64 baseoff = 0;
  tree base = get_addr_base_and_unit_offset (ref, &baseoff);
  if (!base || !VAR_P (base))
    {
      if (*sam != special_array_member::int_0)
	return NULL_TREE;

      if (TREE_CODE (arg) != COMPONENT_REF)
	return NULL_TREE;

      base = arg;
      while (TREE_CODE (base) == COMPONENT_REF)
	base = TREE_OPERAND (base, 0);
      baseoff = tree_to_poly_int64 (byte_position (TREE_OPERAND (ref, 1)));
    }

  /* BASE is the declared object of which MEMBER is either a member
     or that is cast to ARGTYPE (e.g., a char buffer used to store
     an ARGTYPE object).  */
  tree basetype = TREE_TYPE (base);

  /* Determine the base type of the referenced object.  If it's
     the same as ARGTYPE and MEMBER has a known size, return it.  */
  tree bt = basetype;
  if (*sam != special_array_member::int_0)
    while (TREE_CODE (bt) == ARRAY_TYPE)
      bt = TREE_TYPE (bt);
  bool typematch = useless_type_conversion_p (argtype, bt);
  if (memsize && typematch)
    return memsize;

  memsize = NULL_TREE;

  if (typematch)
    /* MEMBER is a true flexible array member.  Compute its size from
       the initializer of the BASE object if it has one.  */
    if (tree init = DECL_P (base) ? DECL_INITIAL (base) : NULL_TREE)
      if (init != error_mark_node)
	{
	  init = get_initializer_for (init, member);
	  if (init)
	    {
	      memsize = TYPE_SIZE_UNIT (TREE_TYPE (init));
	      if (tree refsize = TYPE_SIZE_UNIT (argtype))
		{
		  /* Use the larger of the initializer size and the tail
		     padding in the enclosing struct.  */
		  poly_int64 rsz = tree_to_poly_int64 (refsize);
		  rsz -= baseoff;
		  if (known_lt (tree_to_poly_int64 (memsize), rsz))
		    memsize = wide_int_to_tree (TREE_TYPE (memsize), rsz);
		}

	      baseoff = 0;
	    }
	}

  if (!memsize)
    {
      if (typematch)
	{
	  if (DECL_P (base)
	      && DECL_EXTERNAL (base)
	      && bt == basetype
	      && *sam != special_array_member::int_0)
	    /* The size of a flexible array member of an extern struct
	       with no initializer cannot be determined (it's defined
	       in another translation unit and can have an initializer
	       with an arbitrary number of elements).  */
	    return NULL_TREE;

	  /* Use the size of the base struct or, for interior zero-length
	     arrays, the size of the enclosing type.  */
	  memsize = TYPE_SIZE_UNIT (bt);
	}
      else if (DECL_P (base))
	/* Use the size of the BASE object (possibly an array of some
	   other type such as char used to store the struct).  */
	memsize = DECL_SIZE_UNIT (base);
      else
	return NULL_TREE;
    }

  /* If the flexible array member has a known size use the greater
     of it and the tail padding in the enclosing struct.
     Otherwise, when the size of the flexible array member is unknown
     and the referenced object is not a struct, use the size of its
     type when known.  This detects sizes of array buffers when cast
     to struct types with flexible array members.  */
  if (memsize)
    {
      if (!tree_fits_poly_int64_p (memsize))
	return NULL_TREE;
      poly_int64 memsz64 = memsize ? tree_to_poly_int64 (memsize) : 0;
      if (known_lt (baseoff, memsz64))
	{
	  memsz64 -= baseoff;
	  return wide_int_to_tree (TREE_TYPE (memsize), memsz64);
	}
      return size_zero_node;
    }

  /* Return "don't know" for an external non-array object since its
     flexible array member can be initialized to have any number of
     elements.  Otherwise, return zero because the flexible array
     member has no elements.  */
  return (DECL_P (base)
	  && DECL_EXTERNAL (base)
	  && (!typematch
	      || TREE_CODE (basetype) != ARRAY_TYPE)
	  ? NULL_TREE : size_zero_node);
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

/* Vector types need to re-check the target flags each time we report
   the machine mode.  We need to do this because attribute target can
   change the result of vector_mode_supported_p and have_regs_of_mode
   on a per-function basis.  Thus the TYPE_MODE of a VECTOR_TYPE can
   change on a per-function basis.  */
/* ??? Possibly a better solution is to run through all the types
   referenced by a function and re-compute the TYPE_MODE once, rather
   than make the TYPE_MODE macro call a function.  */

machine_mode
vector_type_mode (const_tree t)
{
  machine_mode mode;

  gcc_assert (TREE_CODE (t) == VECTOR_TYPE);

  mode = t->type_common.mode;
  if (VECTOR_MODE_P (mode)
      && (!targetm.vector_mode_supported_p (mode)
	  || !have_regs_of_mode[mode]))
    {
      scalar_int_mode innermode;

      /* For integers, try mapping it to a same-sized scalar mode.  */
      if (is_int_mode (TREE_TYPE (t)->type_common.mode, &innermode))
	{
	  poly_int64 size = (TYPE_VECTOR_SUBPARTS (t)
			     * GET_MODE_BITSIZE (innermode));
	  scalar_int_mode mode;
	  if (int_mode_for_size (size, 0).exists (&mode)
	      && have_regs_of_mode[mode])
	    return mode;
	}

      return BLKmode;
    }

  return mode;
}

/* Return the size in bits of each element of vector type TYPE.  */

unsigned int
vector_element_bits (const_tree type)
{
  gcc_checking_assert (VECTOR_TYPE_P (type));
  if (VECTOR_BOOLEAN_TYPE_P (type))
    return TYPE_PRECISION (TREE_TYPE (type));
  return tree_to_uhwi (TYPE_SIZE (TREE_TYPE (type)));
}

/* Calculate the size in bits of each element of vector type TYPE
   and return the result as a tree of type bitsizetype.  */

tree
vector_element_bits_tree (const_tree type)
{
  gcc_checking_assert (VECTOR_TYPE_P (type));
  if (VECTOR_BOOLEAN_TYPE_P (type))
    return bitsize_int (vector_element_bits (type));
  return TYPE_SIZE (TREE_TYPE (type));
}

/* Verify that basic properties of T match TV and thus T can be a variant of
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
   */

  /* Convenience macro for matching individual fields.  */
#define verify_variant_match(flag)					    \
  do {									    \
    if (flag (tv) != flag (t))						    \
      {									    \
	error ("type variant differs by %s", #flag);			    \
	debug_tree (tv);						    \
	return false;							    \
      }									    \
  } while (false)

  /* tree_base checks.  */

  verify_variant_match (TREE_CODE);
  /* FIXME: Ada builds non-artificial variants of artificial types.  */
#if 0
  if (TYPE_ARTIFICIAL (tv))
    verify_variant_match (TYPE_ARTIFICIAL);
#endif
  if (POINTER_TYPE_P (tv))
    verify_variant_match (TYPE_REF_CAN_ALIAS_ALL);
  /* FIXME: TYPE_SIZES_GIMPLIFIED may differs for Ada build.  */
  verify_variant_match (TYPE_UNSIGNED);
  verify_variant_match (TYPE_PACKED);
  if (TREE_CODE (t) == REFERENCE_TYPE)
    verify_variant_match (TYPE_REF_IS_RVALUE);
  if (AGGREGATE_TYPE_P (t))
    verify_variant_match (TYPE_REVERSE_STORAGE_ORDER);
  else
    verify_variant_match (TYPE_SATURATING);
  /* FIXME: This check trigger during libstdc++ build.  */
#if 0
  if (RECORD_OR_UNION_TYPE_P (t) && COMPLETE_TYPE_P (t))
    verify_variant_match (TYPE_FINAL_P);
#endif

  /* tree_type_common checks.  */

  if (COMPLETE_TYPE_P (t))
    {
      verify_variant_match (TYPE_MODE);
      if (TREE_CODE (TYPE_SIZE (t)) != PLACEHOLDER_EXPR
	  && TREE_CODE (TYPE_SIZE (tv)) != PLACEHOLDER_EXPR)
	verify_variant_match (TYPE_SIZE);
      if (TREE_CODE (TYPE_SIZE_UNIT (t)) != PLACEHOLDER_EXPR
	  && TREE_CODE (TYPE_SIZE_UNIT (tv)) != PLACEHOLDER_EXPR
	  && TYPE_SIZE_UNIT (t) != TYPE_SIZE_UNIT (tv))
	{
	  gcc_assert (!operand_equal_p (TYPE_SIZE_UNIT (t),
					TYPE_SIZE_UNIT (tv), 0));
	  error ("type variant has different %<TYPE_SIZE_UNIT%>");
	  debug_tree (tv);
	  error ("type variant%'s %<TYPE_SIZE_UNIT%>");
	  debug_tree (TYPE_SIZE_UNIT (tv));
	  error ("type%'s %<TYPE_SIZE_UNIT%>");
	  debug_tree (TYPE_SIZE_UNIT (t));
	  return false;
	}
      verify_variant_match (TYPE_NEEDS_CONSTRUCTING);
    }
  verify_variant_match (TYPE_PRECISION);
  if (RECORD_OR_UNION_TYPE_P (t))
    verify_variant_match (TYPE_TRANSPARENT_AGGR);
  else if (TREE_CODE (t) == ARRAY_TYPE)
    verify_variant_match (TYPE_NONALIASED_COMPONENT);
  /* During LTO we merge variant lists from diferent translation units
     that may differ BY TYPE_CONTEXT that in turn may point 
     to TRANSLATION_UNIT_DECL.
     Ada also builds variants of types with different TYPE_CONTEXT.   */
#if 0
  if (!in_lto_p || !TYPE_FILE_SCOPE_P (t))
    verify_variant_match (TYPE_CONTEXT);
#endif
  if (TREE_CODE (t) == ARRAY_TYPE || TREE_CODE (t) == INTEGER_TYPE)
    verify_variant_match (TYPE_STRING_FLAG);
  if (TREE_CODE (t) == RECORD_TYPE || TREE_CODE (t) == UNION_TYPE)
    verify_variant_match (TYPE_CXX_ODR_P);
  if (TYPE_ALIAS_SET_KNOWN_P (t))
    {
      error ("type variant with %<TYPE_ALIAS_SET_KNOWN_P%>");
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
      error ("type variant has different %<TYPE_VFIELD%>");
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
      error ("type variant has different %<TYPE_BINFO%>");
      debug_tree (tv);
      error ("type variant%'s %<TYPE_BINFO%>");
      debug_tree (TYPE_BINFO (tv));
      error ("type%'s %<TYPE_BINFO%>");
      debug_tree (TYPE_BINFO (t));
      return false;
    }

  /* Check various uses of TYPE_VALUES_RAW.  */
  if (TREE_CODE (t) == ENUMERAL_TYPE
      && TYPE_VALUES (t))
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
	  error ("type variant has different %<TYPE_FIELDS%>");
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
      error ("type variant has different %<TREE_TYPE%>");
      debug_tree (tv);
      error ("type variant%'s %<TREE_TYPE%>");
      debug_tree (TREE_TYPE (tv));
      error ("type%'s %<TREE_TYPE%>");
      debug_tree (TREE_TYPE (t));
      return false;
    }
  if (type_with_alias_set_p (t)
      && !gimple_canonical_types_compatible_p (t, tv, false))
    {
      error ("type is not compatible with its variant");
      debug_tree (tv);
      error ("type variant%'s %<TREE_TYPE%>");
      debug_tree (TREE_TYPE (tv));
      error ("type%'s %<TREE_TYPE%>");
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
   This function is used both by lto.cc canonical type merging and by the
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

  /* For types where we do ODR based TBAA the canonical type is always
     set correctly, so we know that types are different if their
     canonical types does not match.  */
  if (trust_type_canonical
      && (odr_type_p (t1) && odr_based_tbaa_p (t1))
	  != (odr_type_p (t2) && odr_based_tbaa_p (t2)))
    return false;

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

      if (TYPE_ARG_TYPES (t1) == TYPE_ARG_TYPES (t2)
	  && (TYPE_NO_NAMED_ARGS_STDARG_P (t1)
	      == TYPE_NO_NAMED_ARGS_STDARG_P (t2)))
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
	    /* Skip non-fields and zero-sized fields.  */
	    while (f1 && (TREE_CODE (f1) != FIELD_DECL
			  || (DECL_SIZE (f1)
			      && integer_zerop (DECL_SIZE (f1)))))
	      f1 = TREE_CHAIN (f1);
	    while (f2 && (TREE_CODE (f2) != FIELD_DECL
			  || (DECL_SIZE (f2)
			      && integer_zerop (DECL_SIZE (f2)))))
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

/* For OPAQUE_TYPE T, it should have only size and alignment information
   and its mode should be of class MODE_OPAQUE.  This function verifies
   these properties of T match TV which is the main variant of T and TC
   which is the canonical of T.  */

static void
verify_opaque_type (const_tree t, tree tv, tree tc)
{
  gcc_assert (OPAQUE_TYPE_P (t));
  gcc_assert (tv && tv == TYPE_MAIN_VARIANT (tv));
  gcc_assert (tc && tc == TYPE_CANONICAL (tc));

  /* For an opaque type T1, check if some of its properties match
     the corresponding ones of the other opaque type T2, emit some
     error messages for those inconsistent ones.  */
  auto check_properties_for_opaque_type = [](const_tree t1, tree t2,
					     const char *kind_msg)
  {
    if (!OPAQUE_TYPE_P (t2))
      {
	error ("type %s is not an opaque type", kind_msg);
	debug_tree (t2);
	return;
      }
    if (!OPAQUE_MODE_P (TYPE_MODE (t2)))
      {
	error ("type %s is not with opaque mode", kind_msg);
	debug_tree (t2);
	return;
      }
    if (TYPE_MODE (t1) != TYPE_MODE (t2))
      {
	error ("type %s differs by %<TYPE_MODE%>", kind_msg);
	debug_tree (t2);
	return;
      }
    poly_uint64 t1_size = tree_to_poly_uint64 (TYPE_SIZE (t1));
    poly_uint64 t2_size = tree_to_poly_uint64 (TYPE_SIZE (t2));
    if (maybe_ne (t1_size, t2_size))
      {
	error ("type %s differs by %<TYPE_SIZE%>", kind_msg);
	debug_tree (t2);
	return;
      }
    if (TYPE_ALIGN (t1) != TYPE_ALIGN (t2))
      {
	error ("type %s differs by %<TYPE_ALIGN%>", kind_msg);
	debug_tree (t2);
	return;
      }
    if (TYPE_USER_ALIGN (t1) != TYPE_USER_ALIGN (t2))
      {
	error ("type %s differs by %<TYPE_USER_ALIGN%>", kind_msg);
	debug_tree (t2);
	return;
      }
  };

  if (t != tv)
    check_properties_for_opaque_type (t, tv, "variant");

  if (t != tc)
    check_properties_for_opaque_type (t, tc, "canonical");
}

/* Verify type T.  */

void
verify_type (const_tree t)
{
  bool error_found = false;
  tree mv = TYPE_MAIN_VARIANT (t);
  tree ct = TYPE_CANONICAL (t);

  if (OPAQUE_TYPE_P (t))
    {
      verify_opaque_type (t, mv, ct);
      return;
    }

  if (!mv)
    {
      error ("main variant is not defined");
      error_found = true;
    }
  else if (mv != TYPE_MAIN_VARIANT (mv))
    {
      error ("%<TYPE_MAIN_VARIANT%> has different %<TYPE_MAIN_VARIANT%>");
      debug_tree (mv);
      error_found = true;
    }
  else if (t != mv && !verify_type_variant (t, mv))
    error_found = true;

  if (!ct)
    ;
  else if (TYPE_CANONICAL (ct) != ct)
    {
      error ("%<TYPE_CANONICAL%> has different %<TYPE_CANONICAL%>");
      debug_tree (ct);
      error_found = true;
    }
  /* Method and function types cannot be used to address memory and thus
     TYPE_CANONICAL really matters only for determining useless conversions.

     FIXME: C++ FE produce declarations of builtin functions that are not
     compatible with main variants.  */
  else if (TREE_CODE (t) == FUNCTION_TYPE)
    ;
  else if (t != ct
	   /* FIXME: gimple_canonical_types_compatible_p cannot compare types
	      with variably sized arrays because their sizes possibly
	      gimplified to different variables.  */
	   && !variably_modified_type_p (ct, NULL)
	   && !gimple_canonical_types_compatible_p (t, ct, false)
	   && COMPLETE_TYPE_P (t))
    {
      error ("%<TYPE_CANONICAL%> is not compatible");
      debug_tree (ct);
      error_found = true;
    }

  if (COMPLETE_TYPE_P (t) && TYPE_CANONICAL (t)
      && TYPE_MODE (t) != TYPE_MODE (TYPE_CANONICAL (t)))
    {
      error ("%<TYPE_MODE%> of %<TYPE_CANONICAL%> is not compatible");
      debug_tree (ct);
      error_found = true;
    }
  if (TYPE_MAIN_VARIANT (t) == t && ct && TYPE_MAIN_VARIANT (ct) != ct)
   {
      error ("%<TYPE_CANONICAL%> of main variant is not main variant");
      debug_tree (ct);
      debug_tree (TYPE_MAIN_VARIANT (ct));
      error_found = true;
   }


  /* Check various uses of TYPE_MIN_VALUE_RAW.  */
  if (RECORD_OR_UNION_TYPE_P (t))
    {
      /* FIXME: C FE uses TYPE_VFIELD to record C_TYPE_INCOMPLETE_VARS
	 and danagle the pointer from time to time.  */
      if (TYPE_VFIELD (t)
	  && TREE_CODE (TYPE_VFIELD (t)) != FIELD_DECL
	  && TREE_CODE (TYPE_VFIELD (t)) != TREE_LIST)
	{
	  error ("%<TYPE_VFIELD%> is not %<FIELD_DECL%> nor %<TREE_LIST%>");
	  debug_tree (TYPE_VFIELD (t));
	  error_found = true;
	}
    }
  else if (TREE_CODE (t) == POINTER_TYPE)
    {
      if (TYPE_NEXT_PTR_TO (t)
	  && TREE_CODE (TYPE_NEXT_PTR_TO (t)) != POINTER_TYPE)
	{
	  error ("%<TYPE_NEXT_PTR_TO%> is not %<POINTER_TYPE%>");
	  debug_tree (TYPE_NEXT_PTR_TO (t));
	  error_found = true;
	}
    }
  else if (TREE_CODE (t) == REFERENCE_TYPE)
    {
      if (TYPE_NEXT_REF_TO (t)
	  && TREE_CODE (TYPE_NEXT_REF_TO (t)) != REFERENCE_TYPE)
	{
	  error ("%<TYPE_NEXT_REF_TO%> is not %<REFERENCE_TYPE%>");
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

  /* Check various uses of TYPE_MAXVAL_RAW.  */
  if (RECORD_OR_UNION_TYPE_P (t))
    {
      if (!TYPE_BINFO (t))
	;
      else if (TREE_CODE (TYPE_BINFO (t)) != TREE_BINFO)
	{
	  error ("%<TYPE_BINFO%> is not %<TREE_BINFO%>");
	  debug_tree (TYPE_BINFO (t));
	  error_found = true;
	}
      else if (TREE_TYPE (TYPE_BINFO (t)) != TYPE_MAIN_VARIANT (t))
	{
	  error ("%<TYPE_BINFO%> type is not %<TYPE_MAIN_VARIANT%>");
	  debug_tree (TREE_TYPE (TYPE_BINFO (t)));
	  error_found = true;
	}
    }
  else if (TREE_CODE (t) == FUNCTION_TYPE || TREE_CODE (t) == METHOD_TYPE)
    {
      if (TYPE_METHOD_BASETYPE (t)
	  && TREE_CODE (TYPE_METHOD_BASETYPE (t)) != RECORD_TYPE
	  && TREE_CODE (TYPE_METHOD_BASETYPE (t)) != UNION_TYPE)
	{
	  error ("%<TYPE_METHOD_BASETYPE%> is not record nor union");
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
	  error ("%<TYPE_OFFSET_BASETYPE%> is not record nor union");
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
	  error ("%<TYPE_ARRAY_MAX_SIZE%> not %<INTEGER_CST%>");
	  debug_tree (TYPE_ARRAY_MAX_SIZE (t));
	  error_found = true;
        } 
    }
  else if (TYPE_MAX_VALUE_RAW (t))
    {
      error ("%<TYPE_MAX_VALUE_RAW%> non-NULL");
      debug_tree (TYPE_MAX_VALUE_RAW (t));
      error_found = true;
    }

  if (TYPE_LANG_SLOT_1 (t) && in_lto_p)
    {
      error ("%<TYPE_LANG_SLOT_1 (binfo)%> field is non-NULL");
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
	    error ("enum value is not %<CONST_DECL%> or %<INTEGER_CST%>");
	    debug_tree (value);
	    debug_tree (name);
	    error_found = true;
	  }
	if (TREE_CODE (TREE_TYPE (value)) != INTEGER_TYPE
	    && TREE_CODE (TREE_TYPE (value)) != BOOLEAN_TYPE
	    && !useless_type_conversion_p (const_cast <tree> (t), TREE_TYPE (value)))
	  {
	    error ("enum value type is not %<INTEGER_TYPE%> nor convertible "
		   "to the enum");
	    debug_tree (value);
	    debug_tree (name);
	    error_found = true;
	  }
	if (TREE_CODE (name) != IDENTIFIER_NODE)
	  {
	    error ("enum value name is not %<IDENTIFIER_NODE%>");
	    debug_tree (value);
	    debug_tree (name);
	    error_found = true;
	  }
      }
  else if (TREE_CODE (t) == ARRAY_TYPE)
    {
      if (TYPE_DOMAIN (t) && TREE_CODE (TYPE_DOMAIN (t)) != INTEGER_TYPE)
	{
	  error ("array %<TYPE_DOMAIN%> is not integer type");
	  debug_tree (TYPE_DOMAIN (t));
	  error_found = true;
	}
    }
  else if (RECORD_OR_UNION_TYPE_P (t))
    {
      if (TYPE_FIELDS (t) && !COMPLETE_TYPE_P (t) && in_lto_p)
	{
	  error ("%<TYPE_FIELDS%> defined in incomplete type");
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
	  else if (VAR_P (fld))
	    ;
	  else if (TREE_CODE (fld) == TEMPLATE_DECL)
	    ;
	  else if (TREE_CODE (fld) == USING_DECL)
	    ;
	  else if (TREE_CODE (fld) == FUNCTION_DECL)
	    ;
	  else
	    {
	      error ("wrong tree in %<TYPE_FIELDS%> list");
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
	  error ("%<TYPE_CACHED_VALUES_P%> is %i while %<TYPE_CACHED_VALUES%> "
		 "is %p",
		 TYPE_CACHED_VALUES_P (t), (void *)TYPE_CACHED_VALUES (t));
	  error_found = true;
	}
      else if (TYPE_CACHED_VALUES_P (t) && TREE_CODE (TYPE_CACHED_VALUES (t)) != TREE_VEC)
	{
	  error ("%<TYPE_CACHED_VALUES%> is not %<TREE_VEC%>");
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
		error ("wrong %<TYPE_CACHED_VALUES%> entry");
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
	    error ("%<TREE_PURPOSE%> is non-NULL in %<TYPE_ARG_TYPES%> list");
	    debug_tree (l);
	    error_found = true;
	  }
	if (!TYPE_P (TREE_VALUE (l)))
	  {
	    error ("wrong entry in %<TYPE_ARG_TYPES%> list");
	    debug_tree (l);
	    error_found = true;
	  }
      }
  else if (!is_lang_specific (t) && TYPE_VALUES_RAW (t))
    {
      error ("%<TYPE_VALUES_RAW%> field is non-NULL");
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
      error ("%<TYPE_CACHED_VALUES_P%> is set while it should not be");
      error_found = true;
    }
  
  /* ipa-devirt makes an assumption that TYPE_METHOD_BASETYPE is always
     TYPE_MAIN_VARIANT and it would be odd to add methods only to variatns
     of a type. */
  if (TREE_CODE (t) == METHOD_TYPE
      && TYPE_MAIN_VARIANT (TYPE_METHOD_BASETYPE (t)) != TYPE_METHOD_BASETYPE (t))
    {
	error ("%<TYPE_METHOD_BASETYPE%> is not main variant");
	error_found = true;
    }

  if (error_found)
    {
      debug_tree (const_cast <tree> (t));
      internal_error ("%qs failed", __func__);
    }
}


/* Return 1 if ARG interpreted as signed in its precision is known to be
   always positive or 2 if ARG is known to be always negative, or 3 if
   ARG may be positive or negative.  */

int
get_range_pos_neg (tree arg)
{
  if (arg == error_mark_node)
    return 3;

  int prec = TYPE_PRECISION (TREE_TYPE (arg));
  int cnt = 0;
  if (TREE_CODE (arg) == INTEGER_CST)
    {
      wide_int w = wi::sext (wi::to_wide (arg), prec);
      if (wi::neg_p (w))
	return 2;
      else
	return 1;
    }
  while (CONVERT_EXPR_P (arg)
	 && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (arg, 0)))
	 && TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg, 0))) <= prec)
    {
      arg = TREE_OPERAND (arg, 0);
      /* Narrower value zero extended into wider type
	 will always result in positive values.  */
      if (TYPE_UNSIGNED (TREE_TYPE (arg))
	  && TYPE_PRECISION (TREE_TYPE (arg)) < prec)
	return 1;
      prec = TYPE_PRECISION (TREE_TYPE (arg));
      if (++cnt > 30)
	return 3;
    }

  if (TREE_CODE (arg) != SSA_NAME)
    return 3;
  value_range r;
  while (!get_global_range_query ()->range_of_expr (r, arg) || r.kind () != VR_RANGE)
    {
      gimple *g = SSA_NAME_DEF_STMT (arg);
      if (is_gimple_assign (g)
	  && CONVERT_EXPR_CODE_P (gimple_assign_rhs_code (g)))
	{
	  tree t = gimple_assign_rhs1 (g);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (t))
	      && TYPE_PRECISION (TREE_TYPE (t)) <= prec)
	    {
	      if (TYPE_UNSIGNED (TREE_TYPE (t))
		  && TYPE_PRECISION (TREE_TYPE (t)) < prec)
		return 1;
	      prec = TYPE_PRECISION (TREE_TYPE (t));
	      arg = t;
	      if (++cnt > 30)
		return 3;
	      continue;
	    }
	}
      return 3;
    }
  if (TYPE_UNSIGNED (TREE_TYPE (arg)))
    {
      /* For unsigned values, the "positive" range comes
	 below the "negative" range.  */
      if (!wi::neg_p (wi::sext (r.upper_bound (), prec), SIGNED))
	return 1;
      if (wi::neg_p (wi::sext (r.lower_bound (), prec), SIGNED))
	return 2;
    }
  else
    {
      if (!wi::neg_p (wi::sext (r.lower_bound (), prec), SIGNED))
	return 1;
      if (wi::neg_p (wi::sext (r.upper_bound (), prec), SIGNED))
	return 2;
    }
  return 3;
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

/* Combine LOC and BLOCK to a combined adhoc loc, retaining any range
   information.  */

location_t
set_block (location_t loc, tree block)
{
  location_t pure_loc = get_pure_location (loc);
  source_range src_range = get_range_from_loc (line_table, loc);
  unsigned discriminator = get_discriminator_from_loc (line_table, loc);
  return COMBINE_LOCATION_DATA (line_table, pure_loc, src_range, block, discriminator);
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

  location_t expr_location = EXPR_LOCATION (expr);
  location_t pure_loc = get_pure_location (expr_location);
  unsigned discriminator = get_discriminator_from_loc (expr_location);
  location_t adhoc = COMBINE_LOCATION_DATA (line_table,
					    pure_loc,
					    src_range,
					    NULL,
					    discriminator);
  SET_EXPR_LOCATION (expr, adhoc);
  return adhoc;
}

/* Return EXPR, potentially wrapped with a node expression LOC,
   if !CAN_HAVE_LOCATION_P (expr).

   NON_LVALUE_EXPR is used for wrapping constants, apart from STRING_CST.
   VIEW_CONVERT_EXPR is used for wrapping non-constants and STRING_CST.

   Wrapper nodes can be identified using location_wrapper_p.  */

tree
maybe_wrap_with_location (tree expr, location_t loc)
{
  if (expr == NULL)
    return NULL;
  if (loc == UNKNOWN_LOCATION)
    return expr;
  if (CAN_HAVE_LOCATION_P (expr))
    return expr;
  /* We should only be adding wrappers for constants and for decls,
     or for some exceptional tree nodes (e.g. BASELINK in the C++ FE).  */
  gcc_assert (CONSTANT_CLASS_P (expr)
	      || DECL_P (expr)
	      || EXCEPTIONAL_CLASS_P (expr));

  /* For now, don't add wrappers to exceptional tree nodes, to minimize
     any impact of the wrapper nodes.  */
  if (EXCEPTIONAL_CLASS_P (expr) || error_operand_p (expr))
    return expr;

  /* Compiler-generated temporary variables don't need a wrapper.  */
  if (DECL_P (expr) && DECL_ARTIFICIAL (expr) && DECL_IGNORED_P (expr))
    return expr;

  /* If any auto_suppress_location_wrappers are active, don't create
     wrappers.  */
  if (suppress_location_wrappers > 0)
    return expr;

  tree_code code
    = (((CONSTANT_CLASS_P (expr) && TREE_CODE (expr) != STRING_CST)
	|| (TREE_CODE (expr) == CONST_DECL && !TREE_STATIC (expr)))
       ? NON_LVALUE_EXPR : VIEW_CONVERT_EXPR);
  tree wrapper = build1_loc (loc, code, TREE_TYPE (expr), expr);
  /* Mark this node as being a wrapper.  */
  EXPR_LOCATION_WRAPPER_P (wrapper) = 1;
  return wrapper;
}

int suppress_location_wrappers;

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

/* Return a bitmap with a bit set corresponding to each argument in
   a function call type FNTYPE declared with attribute nonnull,
   or null if none of the function's argument are nonnull.  The caller
   must free the bitmap.  */

bitmap
get_nonnull_args (const_tree fntype)
{
  if (fntype == NULL_TREE)
    return NULL;

  bitmap argmap = NULL;
  if (TREE_CODE (fntype) == METHOD_TYPE)
    {
      /* The this pointer in C++ non-static member functions is
	 implicitly nonnull whether or not it's declared as such.  */
      argmap = BITMAP_ALLOC (NULL);
      bitmap_set_bit (argmap, 0);
    }

  tree attrs = TYPE_ATTRIBUTES (fntype);
  if (!attrs)
    return argmap;

  /* A function declaration can specify multiple attribute nonnull,
     each with zero or more arguments.  The loop below creates a bitmap
     representing a union of all the arguments.  An empty (but non-null)
     bitmap means that all arguments have been declaraed nonnull.  */
  for ( ; attrs; attrs = TREE_CHAIN (attrs))
    {
      attrs = lookup_attribute ("nonnull", attrs);
      if (!attrs)
	break;

      if (!argmap)
	argmap = BITMAP_ALLOC (NULL);

      if (!TREE_VALUE (attrs))
	{
	  /* Clear the bitmap in case a previous attribute nonnull
	     set it and this one overrides it for all arguments.  */
	  bitmap_clear (argmap);
	  return argmap;
	}

      /* Iterate over the indices of the format arguments declared nonnull
	 and set a bit for each.  */
      for (tree idx = TREE_VALUE (attrs); idx; idx = TREE_CHAIN (idx))
	{
	  unsigned int val = TREE_INT_CST_LOW (TREE_VALUE (idx)) - 1;
	  bitmap_set_bit (argmap, val);
	}
    }

  return argmap;
}

/* Returns true if TYPE is a type where it and all of its subobjects
   (recursively) are of structure, union, or array type.  */

bool
is_empty_type (const_tree type)
{
  if (RECORD_OR_UNION_TYPE_P (type))
    {
      for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL
	    && !DECL_PADDING_P (field)
	    && !is_empty_type (TREE_TYPE (field)))
	  return false;
      return true;
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    return (integer_minus_onep (array_type_nelts (type))
	    || TYPE_DOMAIN (type) == NULL_TREE
	    || is_empty_type (TREE_TYPE (type)));
  return false;
}

/* Implement TARGET_EMPTY_RECORD_P.  Return true if TYPE is an empty type
   that shouldn't be passed via stack.  */

bool
default_is_empty_record (const_tree type)
{
  if (!abi_version_at_least (12))
    return false;

  if (type == error_mark_node)
    return false;

  if (TREE_ADDRESSABLE (type))
    return false;

  return is_empty_type (TYPE_MAIN_VARIANT (type));
}

/* Determine whether TYPE is a structure with a flexible array member,
   or a union containing such a structure (possibly recursively).  */

bool
flexible_array_type_p (const_tree type)
{
  tree x, last;
  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
      last = NULL_TREE;
      for (x = TYPE_FIELDS (type); x != NULL_TREE; x = DECL_CHAIN (x))
	if (TREE_CODE (x) == FIELD_DECL)
	  last = x;
      if (last == NULL_TREE)
	return false;
      if (TREE_CODE (TREE_TYPE (last)) == ARRAY_TYPE
	  && TYPE_SIZE (TREE_TYPE (last)) == NULL_TREE
	  && TYPE_DOMAIN (TREE_TYPE (last)) != NULL_TREE
	  && TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (last))) == NULL_TREE)
	return true;
      return false;
    case UNION_TYPE:
      for (x = TYPE_FIELDS (type); x != NULL_TREE; x = DECL_CHAIN (x))
	{
	  if (TREE_CODE (x) == FIELD_DECL
	      && flexible_array_type_p (TREE_TYPE (x)))
	    return true;
	}
      return false;
    default:
      return false;
  }
}

/* Like int_size_in_bytes, but handle empty records specially.  */

HOST_WIDE_INT
arg_int_size_in_bytes (const_tree type)
{
  return TYPE_EMPTY_P (type) ? 0 : int_size_in_bytes (type);
}

/* Like size_in_bytes, but handle empty records specially.  */

tree
arg_size_in_bytes (const_tree type)
{
  return TYPE_EMPTY_P (type) ? size_zero_node : size_in_bytes (type);
}

/* Return true if an expression with CODE has to have the same result type as
   its first operand.  */

bool
expr_type_first_operand_type_p (tree_code code)
{
  switch (code)
    {
    case NEGATE_EXPR:
    case ABS_EXPR:
    case BIT_NOT_EXPR:
    case PAREN_EXPR:
    case CONJ_EXPR:

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      return true;

    default:
      return false;
    }
}

/* Return a typenode for the "standard" C type with a given name.  */
tree
get_typenode_from_name (const char *name)
{
  if (name == NULL || *name == '\0')
    return NULL_TREE;

  if (strcmp (name, "char") == 0)
    return char_type_node;
  if (strcmp (name, "unsigned char") == 0)
    return unsigned_char_type_node;
  if (strcmp (name, "signed char") == 0)
    return signed_char_type_node;

  if (strcmp (name, "short int") == 0)
    return short_integer_type_node;
  if (strcmp (name, "short unsigned int") == 0)
    return short_unsigned_type_node;

  if (strcmp (name, "int") == 0)
    return integer_type_node;
  if (strcmp (name, "unsigned int") == 0)
    return unsigned_type_node;

  if (strcmp (name, "long int") == 0)
    return long_integer_type_node;
  if (strcmp (name, "long unsigned int") == 0)
    return long_unsigned_type_node;

  if (strcmp (name, "long long int") == 0)
    return long_long_integer_type_node;
  if (strcmp (name, "long long unsigned int") == 0)
    return long_long_unsigned_type_node;

  gcc_unreachable ();
}

/* List of pointer types used to declare builtins before we have seen their
   real declaration.

   Keep the size up to date in tree.h !  */
const builtin_structptr_type builtin_structptr_types[6] = 
{
  { fileptr_type_node, ptr_type_node, "FILE" },
  { const_tm_ptr_type_node, const_ptr_type_node, "tm" },
  { fenv_t_ptr_type_node, ptr_type_node, "fenv_t" },
  { const_fenv_t_ptr_type_node, const_ptr_type_node, "fenv_t" },
  { fexcept_t_ptr_type_node, ptr_type_node, "fexcept_t" },
  { const_fexcept_t_ptr_type_node, const_ptr_type_node, "fexcept_t" }
};

/* Return the maximum object size.  */

tree
max_object_size (void)
{
  /* To do: Make this a configurable parameter.  */
  return TYPE_MAX_VALUE (ptrdiff_type_node);
}

/* A wrapper around TARGET_VERIFY_TYPE_CONTEXT that makes the silent_p
   parameter default to false and that weeds out error_mark_node.  */

bool
verify_type_context (location_t loc, type_context_kind context,
		     const_tree type, bool silent_p)
{
  if (type == error_mark_node)
    return true;

  gcc_assert (TYPE_P (type));
  return (!targetm.verify_type_context
	  || targetm.verify_type_context (loc, context, type, silent_p));
}

/* Return true if NEW_ASM and DELETE_ASM name a valid pair of new and
   delete operators.  Return false if they may or may not name such
   a pair and, when nonnull, set *PCERTAIN to true if they certainly
   do not.  */

bool
valid_new_delete_pair_p (tree new_asm, tree delete_asm,
			 bool *pcertain /* = NULL */)
{
  bool certain;
  if (!pcertain)
    pcertain = &certain;

  const char *new_name = IDENTIFIER_POINTER (new_asm);
  const char *delete_name = IDENTIFIER_POINTER (delete_asm);
  unsigned int new_len = IDENTIFIER_LENGTH (new_asm);
  unsigned int delete_len = IDENTIFIER_LENGTH (delete_asm);

  /* The following failures are due to invalid names so they're not
     considered certain mismatches.  */
  *pcertain = false;

  if (new_len < 5 || delete_len < 6)
    return false;
  if (new_name[0] == '_')
    ++new_name, --new_len;
  if (new_name[0] == '_')
    ++new_name, --new_len;
  if (delete_name[0] == '_')
    ++delete_name, --delete_len;
  if (delete_name[0] == '_')
    ++delete_name, --delete_len;
  if (new_len < 4 || delete_len < 5)
    return false;

  /* The following failures are due to names of user-defined operators
     so they're also not considered certain mismatches.  */

  /* *_len is now just the length after initial underscores.  */
  if (new_name[0] != 'Z' || new_name[1] != 'n')
    return false;
  if (delete_name[0] != 'Z' || delete_name[1] != 'd')
    return false;

  /* The following failures are certain mismatches.  */
  *pcertain = true;

  /* _Znw must match _Zdl, _Zna must match _Zda.  */
  if ((new_name[2] != 'w' || delete_name[2] != 'l')
      && (new_name[2] != 'a' || delete_name[2] != 'a'))
    return false;
  /* 'j', 'm' and 'y' correspond to size_t.  */
  if (new_name[3] != 'j' && new_name[3] != 'm' && new_name[3] != 'y')
    return false;
  if (delete_name[3] != 'P' || delete_name[4] != 'v')
    return false;
  if (new_len == 4
      || (new_len == 18 && !memcmp (new_name + 4, "RKSt9nothrow_t", 14)))
    {
      /* _ZnXY or _ZnXYRKSt9nothrow_t matches
	 _ZdXPv, _ZdXPvY and _ZdXPvRKSt9nothrow_t.  */
      if (delete_len == 5)
	return true;
      if (delete_len == 6 && delete_name[5] == new_name[3])
	return true;
      if (delete_len == 19 && !memcmp (delete_name + 5, "RKSt9nothrow_t", 14))
	return true;
    }
  else if ((new_len == 19 && !memcmp (new_name + 4, "St11align_val_t", 15))
	   || (new_len == 33
	       && !memcmp (new_name + 4, "St11align_val_tRKSt9nothrow_t", 29)))
    {
      /* _ZnXYSt11align_val_t or _ZnXYSt11align_val_tRKSt9nothrow_t matches
	 _ZdXPvSt11align_val_t or _ZdXPvYSt11align_val_t or  or
	 _ZdXPvSt11align_val_tRKSt9nothrow_t.  */
      if (delete_len == 20 && !memcmp (delete_name + 5, "St11align_val_t", 15))
	return true;
      if (delete_len == 21
	  && delete_name[5] == new_name[3]
	  && !memcmp (delete_name + 6, "St11align_val_t", 15))
	return true;
      if (delete_len == 34
	  && !memcmp (delete_name + 5, "St11align_val_tRKSt9nothrow_t", 29))
	return true;
    }

  /* The negative result is conservative.  */
  *pcertain = false;
  return false;
}

/* Return the zero-based number corresponding to the argument being
   deallocated if FNDECL is a deallocation function or an out-of-bounds
   value if it isn't.  */

unsigned
fndecl_dealloc_argno (tree fndecl)
{
  /* A call to operator delete isn't recognized as one to a built-in.  */
  if (DECL_IS_OPERATOR_DELETE_P (fndecl))
    {
      if (DECL_IS_REPLACEABLE_OPERATOR (fndecl))
	return 0;

      /* Avoid placement delete that's not been inlined.  */
      tree fname = DECL_ASSEMBLER_NAME (fndecl);
      if (id_equal (fname, "_ZdlPvS_")       // ordinary form
	  || id_equal (fname, "_ZdaPvS_"))   // array form
	return UINT_MAX;
      return 0;
    }

  /* TODO: Handle user-defined functions with attribute malloc?  Handle
     known non-built-ins like fopen?  */
  if (fndecl_built_in_p (fndecl, BUILT_IN_NORMAL))
    {
      switch (DECL_FUNCTION_CODE (fndecl))
	{
	case BUILT_IN_FREE:
	case BUILT_IN_REALLOC:
	  return 0;
	default:
	  break;
	}
      return UINT_MAX;
    }

  tree attrs = DECL_ATTRIBUTES (fndecl);
  if (!attrs)
    return UINT_MAX;

  for (tree atfree = attrs;
       (atfree = lookup_attribute ("*dealloc", atfree));
       atfree = TREE_CHAIN (atfree))
    {
      tree alloc = TREE_VALUE (atfree);
      if (!alloc)
	continue;

      tree pos = TREE_CHAIN (alloc);
      if (!pos)
	return 0;

      pos = TREE_VALUE (pos);
      return TREE_INT_CST_LOW (pos) - 1;
    }

  return UINT_MAX;
}

/* If EXPR refers to a character array or pointer declared attribute
   nonstring, return a decl for that array or pointer and set *REF
   to the referenced enclosing object or pointer.  Otherwise return
   null.  */

tree
get_attr_nonstring_decl (tree expr, tree *ref)
{
  tree decl = expr;
  tree var = NULL_TREE;
  if (TREE_CODE (decl) == SSA_NAME)
    {
      gimple *def = SSA_NAME_DEF_STMT (decl);

      if (is_gimple_assign (def))
	{
	  tree_code code = gimple_assign_rhs_code (def);
	  if (code == ADDR_EXPR
	      || code == COMPONENT_REF
	      || code == VAR_DECL)
	    decl = gimple_assign_rhs1 (def);
	}
      else
	var = SSA_NAME_VAR (decl);
    }

  if (TREE_CODE (decl) == ADDR_EXPR)
    decl = TREE_OPERAND (decl, 0);

  /* To simplify calling code, store the referenced DECL regardless of
     the attribute determined below, but avoid storing the SSA_NAME_VAR
     obtained above (it's not useful for dataflow purposes).  */
  if (ref)
    *ref = decl;

  /* Use the SSA_NAME_VAR that was determined above to see if it's
     declared nonstring.  Otherwise drill down into the referenced
     DECL.  */
  if (var)
    decl = var;
  else if (TREE_CODE (decl) == ARRAY_REF)
    decl = TREE_OPERAND (decl, 0);
  else if (TREE_CODE (decl) == COMPONENT_REF)
    decl = TREE_OPERAND (decl, 1);
  else if (TREE_CODE (decl) == MEM_REF)
    return get_attr_nonstring_decl (TREE_OPERAND (decl, 0), ref);

  if (DECL_P (decl)
      && lookup_attribute ("nonstring", DECL_ATTRIBUTES (decl)))
    return decl;

  return NULL_TREE;
}

/* Return length of attribute names string,
   if arglist chain > 1, -1 otherwise.  */

int
get_target_clone_attr_len (tree arglist)
{
  tree arg;
  int str_len_sum = 0;
  int argnum = 0;

  for (arg = arglist; arg; arg = TREE_CHAIN (arg))
    {
      const char *str = TREE_STRING_POINTER (TREE_VALUE (arg));
      size_t len = strlen (str);
      str_len_sum += len + 1;
      for (const char *p = strchr (str, ','); p; p = strchr (p + 1, ','))
	argnum++;
      argnum++;
    }
  if (argnum <= 1)
    return -1;
  return str_len_sum;
}

void
tree_cc_finalize (void)
{
  clear_nonstandard_integer_type_cache ();
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

/* Return a new VECTOR_CST node whose type is TYPE and whose values
   are given by VALS.  */

static tree
build_vector (tree type, const vec<tree> &vals MEM_STAT_DECL)
{
  gcc_assert (known_eq (vals.length (), TYPE_VECTOR_SUBPARTS (type)));
  tree_vector_builder builder (type, vals.length (), 1);
  builder.splice (vals);
  return builder.build ();
}

/* Check that VECTOR_CST ACTUAL contains the elements in EXPECTED.  */

static void
check_vector_cst (const vec<tree> &expected, tree actual)
{
  ASSERT_KNOWN_EQ (expected.length (),
		   TYPE_VECTOR_SUBPARTS (TREE_TYPE (actual)));
  for (unsigned int i = 0; i < expected.length (); ++i)
    ASSERT_EQ (wi::to_wide (expected[i]),
	       wi::to_wide (vector_cst_elt (actual, i)));
}

/* Check that VECTOR_CST ACTUAL contains NPATTERNS duplicated elements,
   and that its elements match EXPECTED.  */

static void
check_vector_cst_duplicate (const vec<tree> &expected, tree actual,
			    unsigned int npatterns)
{
  ASSERT_EQ (npatterns, VECTOR_CST_NPATTERNS (actual));
  ASSERT_EQ (1, VECTOR_CST_NELTS_PER_PATTERN (actual));
  ASSERT_EQ (npatterns, vector_cst_encoded_nelts (actual));
  ASSERT_TRUE (VECTOR_CST_DUPLICATE_P (actual));
  ASSERT_FALSE (VECTOR_CST_STEPPED_P (actual));
  check_vector_cst (expected, actual);
}

/* Check that VECTOR_CST ACTUAL contains NPATTERNS foreground elements
   and NPATTERNS background elements, and that its elements match
   EXPECTED.  */

static void
check_vector_cst_fill (const vec<tree> &expected, tree actual,
		       unsigned int npatterns)
{
  ASSERT_EQ (npatterns, VECTOR_CST_NPATTERNS (actual));
  ASSERT_EQ (2, VECTOR_CST_NELTS_PER_PATTERN (actual));
  ASSERT_EQ (2 * npatterns, vector_cst_encoded_nelts (actual));
  ASSERT_FALSE (VECTOR_CST_DUPLICATE_P (actual));
  ASSERT_FALSE (VECTOR_CST_STEPPED_P (actual));
  check_vector_cst (expected, actual);
}

/* Check that VECTOR_CST ACTUAL contains NPATTERNS stepped patterns,
   and that its elements match EXPECTED.  */

static void
check_vector_cst_stepped (const vec<tree> &expected, tree actual,
			  unsigned int npatterns)
{
  ASSERT_EQ (npatterns, VECTOR_CST_NPATTERNS (actual));
  ASSERT_EQ (3, VECTOR_CST_NELTS_PER_PATTERN (actual));
  ASSERT_EQ (3 * npatterns, vector_cst_encoded_nelts (actual));
  ASSERT_FALSE (VECTOR_CST_DUPLICATE_P (actual));
  ASSERT_TRUE (VECTOR_CST_STEPPED_P (actual));
  check_vector_cst (expected, actual);
}

/* Test the creation of VECTOR_CSTs.  */

static void
test_vector_cst_patterns (ALONE_CXX_MEM_STAT_INFO)
{
  auto_vec<tree, 8> elements (8);
  elements.quick_grow (8);
  tree element_type = build_nonstandard_integer_type (16, true);
  tree vector_type = build_vector_type (element_type, 8);

  /* Test a simple linear series with a base of 0 and a step of 1:
     { 0, 1, 2, 3, 4, 5, 6, 7 }.  */
  for (unsigned int i = 0; i < 8; ++i)
    elements[i] = build_int_cst (element_type, i);
  tree vector = build_vector (vector_type, elements PASS_MEM_STAT);
  check_vector_cst_stepped (elements, vector, 1);

  /* Try the same with the first element replaced by 100:
     { 100, 1, 2, 3, 4, 5, 6, 7 }.  */
  elements[0] = build_int_cst (element_type, 100);
  vector = build_vector (vector_type, elements PASS_MEM_STAT);
  check_vector_cst_stepped (elements, vector, 1);

  /* Try a series that wraps around.
     { 100, 65531, 65532, 65533, 65534, 65535, 0, 1 }.  */
  for (unsigned int i = 1; i < 8; ++i)
    elements[i] = build_int_cst (element_type, (65530 + i) & 0xffff);
  vector = build_vector (vector_type, elements PASS_MEM_STAT);
  check_vector_cst_stepped (elements, vector, 1);

  /* Try a downward series:
     { 100, 79, 78, 77, 76, 75, 75, 73 }.  */
  for (unsigned int i = 1; i < 8; ++i)
    elements[i] = build_int_cst (element_type, 80 - i);
  vector = build_vector (vector_type, elements PASS_MEM_STAT);
  check_vector_cst_stepped (elements, vector, 1);

  /* Try two interleaved series with different bases and steps:
     { 100, 53, 66, 206, 62, 212, 58, 218 }.  */
  elements[1] = build_int_cst (element_type, 53);
  for (unsigned int i = 2; i < 8; i += 2)
    {
      elements[i] = build_int_cst (element_type, 70 - i * 2);
      elements[i + 1] = build_int_cst (element_type, 200 + i * 3);
    }
  vector = build_vector (vector_type, elements PASS_MEM_STAT);
  check_vector_cst_stepped (elements, vector, 2);

  /* Try a duplicated value:
     { 100, 100, 100, 100, 100, 100, 100, 100 }.  */
  for (unsigned int i = 1; i < 8; ++i)
    elements[i] = elements[0];
  vector = build_vector (vector_type, elements PASS_MEM_STAT);
  check_vector_cst_duplicate (elements, vector, 1);

  /* Try an interleaved duplicated value:
     { 100, 55, 100, 55, 100, 55, 100, 55 }.  */
  elements[1] = build_int_cst (element_type, 55);
  for (unsigned int i = 2; i < 8; ++i)
    elements[i] = elements[i - 2];
  vector = build_vector (vector_type, elements PASS_MEM_STAT);
  check_vector_cst_duplicate (elements, vector, 2);

  /* Try a duplicated value with 2 exceptions
     { 41, 97, 100, 55, 100, 55, 100, 55 }.  */
  elements[0] = build_int_cst (element_type, 41);
  elements[1] = build_int_cst (element_type, 97);
  vector = build_vector (vector_type, elements PASS_MEM_STAT);
  check_vector_cst_fill (elements, vector, 2);

  /* Try with and without a step
     { 41, 97, 100, 21, 100, 35, 100, 49 }.  */
  for (unsigned int i = 3; i < 8; i += 2)
    elements[i] = build_int_cst (element_type, i * 7);
  vector = build_vector (vector_type, elements PASS_MEM_STAT);
  check_vector_cst_stepped (elements, vector, 2);

  /* Try a fully-general constant:
     { 41, 97, 100, 21, 100, 9990, 100, 49 }.  */
  elements[5] = build_int_cst (element_type, 9990);
  vector = build_vector (vector_type, elements PASS_MEM_STAT);
  check_vector_cst_fill (elements, vector, 4);
}

/* Verify that STRIP_NOPS (NODE) is EXPECTED.
   Helper function for test_location_wrappers, to deal with STRIP_NOPS
   modifying its argument in-place.  */

static void
check_strip_nops (tree node, tree expected)
{
  STRIP_NOPS (node);
  ASSERT_EQ (expected, node);
}

/* Verify location wrappers.  */

static void
test_location_wrappers ()
{
  location_t loc = BUILTINS_LOCATION;

  ASSERT_EQ (NULL_TREE, maybe_wrap_with_location (NULL_TREE, loc));

  /* Wrapping a constant.  */
  tree int_cst = build_int_cst (integer_type_node, 42);
  ASSERT_FALSE (CAN_HAVE_LOCATION_P (int_cst));
  ASSERT_FALSE (location_wrapper_p (int_cst));

  tree wrapped_int_cst = maybe_wrap_with_location (int_cst, loc);
  ASSERT_TRUE (location_wrapper_p (wrapped_int_cst));
  ASSERT_EQ (loc, EXPR_LOCATION (wrapped_int_cst));
  ASSERT_EQ (int_cst, tree_strip_any_location_wrapper (wrapped_int_cst));

  /* We shouldn't add wrapper nodes for UNKNOWN_LOCATION.  */
  ASSERT_EQ (int_cst, maybe_wrap_with_location (int_cst, UNKNOWN_LOCATION));

  /* We shouldn't add wrapper nodes for nodes that CAN_HAVE_LOCATION_P.  */
  tree cast = build1 (NOP_EXPR, char_type_node, int_cst);
  ASSERT_TRUE (CAN_HAVE_LOCATION_P (cast));
  ASSERT_EQ (cast, maybe_wrap_with_location (cast, loc));

  /* Wrapping a STRING_CST.  */
  tree string_cst = build_string (4, "foo");
  ASSERT_FALSE (CAN_HAVE_LOCATION_P (string_cst));
  ASSERT_FALSE (location_wrapper_p (string_cst));

  tree wrapped_string_cst = maybe_wrap_with_location (string_cst, loc);
  ASSERT_TRUE (location_wrapper_p (wrapped_string_cst));
  ASSERT_EQ (VIEW_CONVERT_EXPR, TREE_CODE (wrapped_string_cst));
  ASSERT_EQ (loc, EXPR_LOCATION (wrapped_string_cst));
  ASSERT_EQ (string_cst, tree_strip_any_location_wrapper (wrapped_string_cst));


  /* Wrapping a variable.  */
  tree int_var = build_decl (UNKNOWN_LOCATION, VAR_DECL,
			     get_identifier ("some_int_var"),
			     integer_type_node);
  ASSERT_FALSE (CAN_HAVE_LOCATION_P (int_var));
  ASSERT_FALSE (location_wrapper_p (int_var));

  tree wrapped_int_var = maybe_wrap_with_location (int_var, loc);
  ASSERT_TRUE (location_wrapper_p (wrapped_int_var));
  ASSERT_EQ (loc, EXPR_LOCATION (wrapped_int_var));
  ASSERT_EQ (int_var, tree_strip_any_location_wrapper (wrapped_int_var));

  /* Verify that "reinterpret_cast<int>(some_int_var)" is not a location
     wrapper.  */
  tree r_cast = build1 (NON_LVALUE_EXPR, integer_type_node, int_var);
  ASSERT_FALSE (location_wrapper_p (r_cast));
  ASSERT_EQ (r_cast, tree_strip_any_location_wrapper (r_cast));

  /* Verify that STRIP_NOPS removes wrappers.  */
  check_strip_nops (wrapped_int_cst, int_cst);
  check_strip_nops (wrapped_string_cst, string_cst);
  check_strip_nops (wrapped_int_var, int_var);
}

/* Test various tree predicates.  Verify that location wrappers don't
   affect the results.  */

static void
test_predicates ()
{
  /* Build various constants and wrappers around them.  */

  location_t loc = BUILTINS_LOCATION;

  tree i_0 = build_int_cst (integer_type_node, 0);
  tree wr_i_0 = maybe_wrap_with_location (i_0, loc);

  tree i_1 = build_int_cst (integer_type_node, 1);
  tree wr_i_1 = maybe_wrap_with_location (i_1, loc);

  tree i_m1 = build_int_cst (integer_type_node, -1);
  tree wr_i_m1 = maybe_wrap_with_location (i_m1, loc);

  tree f_0 = build_real_from_int_cst (float_type_node, i_0);
  tree wr_f_0 = maybe_wrap_with_location (f_0, loc);
  tree f_1 = build_real_from_int_cst (float_type_node, i_1);
  tree wr_f_1 = maybe_wrap_with_location (f_1, loc);
  tree f_m1 = build_real_from_int_cst (float_type_node, i_m1);
  tree wr_f_m1 = maybe_wrap_with_location (f_m1, loc);

  tree c_i_0 = build_complex (NULL_TREE, i_0, i_0);
  tree c_i_1 = build_complex (NULL_TREE, i_1, i_0);
  tree c_i_m1 = build_complex (NULL_TREE, i_m1, i_0);

  tree c_f_0 = build_complex (NULL_TREE, f_0, f_0);
  tree c_f_1 = build_complex (NULL_TREE, f_1, f_0);
  tree c_f_m1 = build_complex (NULL_TREE, f_m1, f_0);

  /* TODO: vector constants.  */

  /* Test integer_onep.  */
  ASSERT_FALSE (integer_onep (i_0));
  ASSERT_FALSE (integer_onep (wr_i_0));
  ASSERT_TRUE (integer_onep (i_1));
  ASSERT_TRUE (integer_onep (wr_i_1));
  ASSERT_FALSE (integer_onep (i_m1));
  ASSERT_FALSE (integer_onep (wr_i_m1));
  ASSERT_FALSE (integer_onep (f_0));
  ASSERT_FALSE (integer_onep (wr_f_0));
  ASSERT_FALSE (integer_onep (f_1));
  ASSERT_FALSE (integer_onep (wr_f_1));
  ASSERT_FALSE (integer_onep (f_m1));
  ASSERT_FALSE (integer_onep (wr_f_m1));
  ASSERT_FALSE (integer_onep (c_i_0));
  ASSERT_TRUE (integer_onep (c_i_1));
  ASSERT_FALSE (integer_onep (c_i_m1));
  ASSERT_FALSE (integer_onep (c_f_0));
  ASSERT_FALSE (integer_onep (c_f_1));
  ASSERT_FALSE (integer_onep (c_f_m1));

  /* Test integer_zerop.  */
  ASSERT_TRUE (integer_zerop (i_0));
  ASSERT_TRUE (integer_zerop (wr_i_0));
  ASSERT_FALSE (integer_zerop (i_1));
  ASSERT_FALSE (integer_zerop (wr_i_1));
  ASSERT_FALSE (integer_zerop (i_m1));
  ASSERT_FALSE (integer_zerop (wr_i_m1));
  ASSERT_FALSE (integer_zerop (f_0));
  ASSERT_FALSE (integer_zerop (wr_f_0));
  ASSERT_FALSE (integer_zerop (f_1));
  ASSERT_FALSE (integer_zerop (wr_f_1));
  ASSERT_FALSE (integer_zerop (f_m1));
  ASSERT_FALSE (integer_zerop (wr_f_m1));
  ASSERT_TRUE (integer_zerop (c_i_0));
  ASSERT_FALSE (integer_zerop (c_i_1));
  ASSERT_FALSE (integer_zerop (c_i_m1));
  ASSERT_FALSE (integer_zerop (c_f_0));
  ASSERT_FALSE (integer_zerop (c_f_1));
  ASSERT_FALSE (integer_zerop (c_f_m1));

  /* Test integer_all_onesp.  */
  ASSERT_FALSE (integer_all_onesp (i_0));
  ASSERT_FALSE (integer_all_onesp (wr_i_0));
  ASSERT_FALSE (integer_all_onesp (i_1));
  ASSERT_FALSE (integer_all_onesp (wr_i_1));
  ASSERT_TRUE (integer_all_onesp (i_m1));
  ASSERT_TRUE (integer_all_onesp (wr_i_m1));
  ASSERT_FALSE (integer_all_onesp (f_0));
  ASSERT_FALSE (integer_all_onesp (wr_f_0));
  ASSERT_FALSE (integer_all_onesp (f_1));
  ASSERT_FALSE (integer_all_onesp (wr_f_1));
  ASSERT_FALSE (integer_all_onesp (f_m1));
  ASSERT_FALSE (integer_all_onesp (wr_f_m1));
  ASSERT_FALSE (integer_all_onesp (c_i_0));
  ASSERT_FALSE (integer_all_onesp (c_i_1));
  ASSERT_FALSE (integer_all_onesp (c_i_m1));
  ASSERT_FALSE (integer_all_onesp (c_f_0));
  ASSERT_FALSE (integer_all_onesp (c_f_1));
  ASSERT_FALSE (integer_all_onesp (c_f_m1));

  /* Test integer_minus_onep.  */
  ASSERT_FALSE (integer_minus_onep (i_0));
  ASSERT_FALSE (integer_minus_onep (wr_i_0));
  ASSERT_FALSE (integer_minus_onep (i_1));
  ASSERT_FALSE (integer_minus_onep (wr_i_1));
  ASSERT_TRUE (integer_minus_onep (i_m1));
  ASSERT_TRUE (integer_minus_onep (wr_i_m1));
  ASSERT_FALSE (integer_minus_onep (f_0));
  ASSERT_FALSE (integer_minus_onep (wr_f_0));
  ASSERT_FALSE (integer_minus_onep (f_1));
  ASSERT_FALSE (integer_minus_onep (wr_f_1));
  ASSERT_FALSE (integer_minus_onep (f_m1));
  ASSERT_FALSE (integer_minus_onep (wr_f_m1));
  ASSERT_FALSE (integer_minus_onep (c_i_0));
  ASSERT_FALSE (integer_minus_onep (c_i_1));
  ASSERT_TRUE (integer_minus_onep (c_i_m1));
  ASSERT_FALSE (integer_minus_onep (c_f_0));
  ASSERT_FALSE (integer_minus_onep (c_f_1));
  ASSERT_FALSE (integer_minus_onep (c_f_m1));

  /* Test integer_each_onep.  */
  ASSERT_FALSE (integer_each_onep (i_0));
  ASSERT_FALSE (integer_each_onep (wr_i_0));
  ASSERT_TRUE (integer_each_onep (i_1));
  ASSERT_TRUE (integer_each_onep (wr_i_1));
  ASSERT_FALSE (integer_each_onep (i_m1));
  ASSERT_FALSE (integer_each_onep (wr_i_m1));
  ASSERT_FALSE (integer_each_onep (f_0));
  ASSERT_FALSE (integer_each_onep (wr_f_0));
  ASSERT_FALSE (integer_each_onep (f_1));
  ASSERT_FALSE (integer_each_onep (wr_f_1));
  ASSERT_FALSE (integer_each_onep (f_m1));
  ASSERT_FALSE (integer_each_onep (wr_f_m1));
  ASSERT_FALSE (integer_each_onep (c_i_0));
  ASSERT_FALSE (integer_each_onep (c_i_1));
  ASSERT_FALSE (integer_each_onep (c_i_m1));
  ASSERT_FALSE (integer_each_onep (c_f_0));
  ASSERT_FALSE (integer_each_onep (c_f_1));
  ASSERT_FALSE (integer_each_onep (c_f_m1));

  /* Test integer_truep.  */
  ASSERT_FALSE (integer_truep (i_0));
  ASSERT_FALSE (integer_truep (wr_i_0));
  ASSERT_TRUE (integer_truep (i_1));
  ASSERT_TRUE (integer_truep (wr_i_1));
  ASSERT_FALSE (integer_truep (i_m1));
  ASSERT_FALSE (integer_truep (wr_i_m1));
  ASSERT_FALSE (integer_truep (f_0));
  ASSERT_FALSE (integer_truep (wr_f_0));
  ASSERT_FALSE (integer_truep (f_1));
  ASSERT_FALSE (integer_truep (wr_f_1));
  ASSERT_FALSE (integer_truep (f_m1));
  ASSERT_FALSE (integer_truep (wr_f_m1));
  ASSERT_FALSE (integer_truep (c_i_0));
  ASSERT_TRUE (integer_truep (c_i_1));
  ASSERT_FALSE (integer_truep (c_i_m1));
  ASSERT_FALSE (integer_truep (c_f_0));
  ASSERT_FALSE (integer_truep (c_f_1));
  ASSERT_FALSE (integer_truep (c_f_m1));

  /* Test integer_nonzerop.  */
  ASSERT_FALSE (integer_nonzerop (i_0));
  ASSERT_FALSE (integer_nonzerop (wr_i_0));
  ASSERT_TRUE (integer_nonzerop (i_1));
  ASSERT_TRUE (integer_nonzerop (wr_i_1));
  ASSERT_TRUE (integer_nonzerop (i_m1));
  ASSERT_TRUE (integer_nonzerop (wr_i_m1));
  ASSERT_FALSE (integer_nonzerop (f_0));
  ASSERT_FALSE (integer_nonzerop (wr_f_0));
  ASSERT_FALSE (integer_nonzerop (f_1));
  ASSERT_FALSE (integer_nonzerop (wr_f_1));
  ASSERT_FALSE (integer_nonzerop (f_m1));
  ASSERT_FALSE (integer_nonzerop (wr_f_m1));
  ASSERT_FALSE (integer_nonzerop (c_i_0));
  ASSERT_TRUE (integer_nonzerop (c_i_1));
  ASSERT_TRUE (integer_nonzerop (c_i_m1));
  ASSERT_FALSE (integer_nonzerop (c_f_0));
  ASSERT_FALSE (integer_nonzerop (c_f_1));
  ASSERT_FALSE (integer_nonzerop (c_f_m1));

  /* Test real_zerop.  */
  ASSERT_FALSE (real_zerop (i_0));
  ASSERT_FALSE (real_zerop (wr_i_0));
  ASSERT_FALSE (real_zerop (i_1));
  ASSERT_FALSE (real_zerop (wr_i_1));
  ASSERT_FALSE (real_zerop (i_m1));
  ASSERT_FALSE (real_zerop (wr_i_m1));
  ASSERT_TRUE (real_zerop (f_0));
  ASSERT_TRUE (real_zerop (wr_f_0));
  ASSERT_FALSE (real_zerop (f_1));
  ASSERT_FALSE (real_zerop (wr_f_1));
  ASSERT_FALSE (real_zerop (f_m1));
  ASSERT_FALSE (real_zerop (wr_f_m1));
  ASSERT_FALSE (real_zerop (c_i_0));
  ASSERT_FALSE (real_zerop (c_i_1));
  ASSERT_FALSE (real_zerop (c_i_m1));
  ASSERT_TRUE (real_zerop (c_f_0));
  ASSERT_FALSE (real_zerop (c_f_1));
  ASSERT_FALSE (real_zerop (c_f_m1));

  /* Test real_onep.  */
  ASSERT_FALSE (real_onep (i_0));
  ASSERT_FALSE (real_onep (wr_i_0));
  ASSERT_FALSE (real_onep (i_1));
  ASSERT_FALSE (real_onep (wr_i_1));
  ASSERT_FALSE (real_onep (i_m1));
  ASSERT_FALSE (real_onep (wr_i_m1));
  ASSERT_FALSE (real_onep (f_0));
  ASSERT_FALSE (real_onep (wr_f_0));
  ASSERT_TRUE (real_onep (f_1));
  ASSERT_TRUE (real_onep (wr_f_1));
  ASSERT_FALSE (real_onep (f_m1));
  ASSERT_FALSE (real_onep (wr_f_m1));
  ASSERT_FALSE (real_onep (c_i_0));
  ASSERT_FALSE (real_onep (c_i_1));
  ASSERT_FALSE (real_onep (c_i_m1));
  ASSERT_FALSE (real_onep (c_f_0));
  ASSERT_TRUE (real_onep (c_f_1));
  ASSERT_FALSE (real_onep (c_f_m1));

  /* Test real_minus_onep.  */
  ASSERT_FALSE (real_minus_onep (i_0));
  ASSERT_FALSE (real_minus_onep (wr_i_0));
  ASSERT_FALSE (real_minus_onep (i_1));
  ASSERT_FALSE (real_minus_onep (wr_i_1));
  ASSERT_FALSE (real_minus_onep (i_m1));
  ASSERT_FALSE (real_minus_onep (wr_i_m1));
  ASSERT_FALSE (real_minus_onep (f_0));
  ASSERT_FALSE (real_minus_onep (wr_f_0));
  ASSERT_FALSE (real_minus_onep (f_1));
  ASSERT_FALSE (real_minus_onep (wr_f_1));
  ASSERT_TRUE (real_minus_onep (f_m1));
  ASSERT_TRUE (real_minus_onep (wr_f_m1));
  ASSERT_FALSE (real_minus_onep (c_i_0));
  ASSERT_FALSE (real_minus_onep (c_i_1));
  ASSERT_FALSE (real_minus_onep (c_i_m1));
  ASSERT_FALSE (real_minus_onep (c_f_0));
  ASSERT_FALSE (real_minus_onep (c_f_1));
  ASSERT_TRUE (real_minus_onep (c_f_m1));

  /* Test zerop.  */
  ASSERT_TRUE (zerop (i_0));
  ASSERT_TRUE (zerop (wr_i_0));
  ASSERT_FALSE (zerop (i_1));
  ASSERT_FALSE (zerop (wr_i_1));
  ASSERT_FALSE (zerop (i_m1));
  ASSERT_FALSE (zerop (wr_i_m1));
  ASSERT_TRUE (zerop (f_0));
  ASSERT_TRUE (zerop (wr_f_0));
  ASSERT_FALSE (zerop (f_1));
  ASSERT_FALSE (zerop (wr_f_1));
  ASSERT_FALSE (zerop (f_m1));
  ASSERT_FALSE (zerop (wr_f_m1));
  ASSERT_TRUE (zerop (c_i_0));
  ASSERT_FALSE (zerop (c_i_1));
  ASSERT_FALSE (zerop (c_i_m1));
  ASSERT_TRUE (zerop (c_f_0));
  ASSERT_FALSE (zerop (c_f_1));
  ASSERT_FALSE (zerop (c_f_m1));

  /* Test tree_expr_nonnegative_p.  */
  ASSERT_TRUE (tree_expr_nonnegative_p (i_0));
  ASSERT_TRUE (tree_expr_nonnegative_p (wr_i_0));
  ASSERT_TRUE (tree_expr_nonnegative_p (i_1));
  ASSERT_TRUE (tree_expr_nonnegative_p (wr_i_1));
  ASSERT_FALSE (tree_expr_nonnegative_p (i_m1));
  ASSERT_FALSE (tree_expr_nonnegative_p (wr_i_m1));
  ASSERT_TRUE (tree_expr_nonnegative_p (f_0));
  ASSERT_TRUE (tree_expr_nonnegative_p (wr_f_0));
  ASSERT_TRUE (tree_expr_nonnegative_p (f_1));
  ASSERT_TRUE (tree_expr_nonnegative_p (wr_f_1));
  ASSERT_FALSE (tree_expr_nonnegative_p (f_m1));
  ASSERT_FALSE (tree_expr_nonnegative_p (wr_f_m1));
  ASSERT_FALSE (tree_expr_nonnegative_p (c_i_0));
  ASSERT_FALSE (tree_expr_nonnegative_p (c_i_1));
  ASSERT_FALSE (tree_expr_nonnegative_p (c_i_m1));
  ASSERT_FALSE (tree_expr_nonnegative_p (c_f_0));
  ASSERT_FALSE (tree_expr_nonnegative_p (c_f_1));
  ASSERT_FALSE (tree_expr_nonnegative_p (c_f_m1));

  /* Test tree_expr_nonzero_p.  */
  ASSERT_FALSE (tree_expr_nonzero_p (i_0));
  ASSERT_FALSE (tree_expr_nonzero_p (wr_i_0));
  ASSERT_TRUE (tree_expr_nonzero_p (i_1));
  ASSERT_TRUE (tree_expr_nonzero_p (wr_i_1));
  ASSERT_TRUE (tree_expr_nonzero_p (i_m1));
  ASSERT_TRUE (tree_expr_nonzero_p (wr_i_m1));

  /* Test integer_valued_real_p.  */
  ASSERT_FALSE (integer_valued_real_p (i_0));
  ASSERT_TRUE (integer_valued_real_p (f_0));
  ASSERT_TRUE (integer_valued_real_p (wr_f_0));
  ASSERT_TRUE (integer_valued_real_p (f_1));
  ASSERT_TRUE (integer_valued_real_p (wr_f_1));

  /* Test integer_pow2p.  */
  ASSERT_FALSE (integer_pow2p (i_0));
  ASSERT_TRUE (integer_pow2p (i_1));
  ASSERT_TRUE (integer_pow2p (wr_i_1));

  /* Test uniform_integer_cst_p.  */
  ASSERT_TRUE (uniform_integer_cst_p (i_0));
  ASSERT_TRUE (uniform_integer_cst_p (wr_i_0));
  ASSERT_TRUE (uniform_integer_cst_p (i_1));
  ASSERT_TRUE (uniform_integer_cst_p (wr_i_1));
  ASSERT_TRUE (uniform_integer_cst_p (i_m1));
  ASSERT_TRUE (uniform_integer_cst_p (wr_i_m1));
  ASSERT_FALSE (uniform_integer_cst_p (f_0));
  ASSERT_FALSE (uniform_integer_cst_p (wr_f_0));
  ASSERT_FALSE (uniform_integer_cst_p (f_1));
  ASSERT_FALSE (uniform_integer_cst_p (wr_f_1));
  ASSERT_FALSE (uniform_integer_cst_p (f_m1));
  ASSERT_FALSE (uniform_integer_cst_p (wr_f_m1));
  ASSERT_FALSE (uniform_integer_cst_p (c_i_0));
  ASSERT_FALSE (uniform_integer_cst_p (c_i_1));
  ASSERT_FALSE (uniform_integer_cst_p (c_i_m1));
  ASSERT_FALSE (uniform_integer_cst_p (c_f_0));
  ASSERT_FALSE (uniform_integer_cst_p (c_f_1));
  ASSERT_FALSE (uniform_integer_cst_p (c_f_m1));
}

/* Check that string escaping works correctly.  */

static void
test_escaped_strings (void)
{
  int saved_cutoff;
  escaped_string msg;

  msg.escape (NULL);
  /* ASSERT_STREQ does not accept NULL as a valid test
     result, so we have to use ASSERT_EQ instead.  */
  ASSERT_EQ (NULL, (const char *) msg);

  msg.escape ("");
  ASSERT_STREQ ("", (const char *) msg);

  msg.escape ("foobar");
  ASSERT_STREQ ("foobar", (const char *) msg);

  /* Ensure that we have -fmessage-length set to 0.  */
  saved_cutoff = pp_line_cutoff (global_dc->printer);
  pp_line_cutoff (global_dc->printer) = 0;

  msg.escape ("foo\nbar");
  ASSERT_STREQ ("foo\\nbar", (const char *) msg);

  msg.escape ("\a\b\f\n\r\t\v");
  ASSERT_STREQ ("\\a\\b\\f\\n\\r\\t\\v", (const char *) msg);

  /* Now repeat the tests with -fmessage-length set to 5.  */
  pp_line_cutoff (global_dc->printer) = 5;

  /* Note that the newline is not translated into an escape.  */
  msg.escape ("foo\nbar");
  ASSERT_STREQ ("foo\nbar", (const char *) msg);

  msg.escape ("\a\b\f\n\r\t\v");
  ASSERT_STREQ ("\\a\\b\\f\n\\r\\t\\v", (const char *) msg);

  /* Restore the original message length setting.  */
  pp_line_cutoff (global_dc->printer) = saved_cutoff;
}

/* Run all of the selftests within this file.  */

void
tree_cc_tests ()
{
  test_integer_constants ();
  test_identifiers ();
  test_labels ();
  test_vector_cst_patterns ();
  test_location_wrappers ();
  test_predicates ();
  test_escaped_strings ();
}

} // namespace selftest

#endif /* CHECKING_P */

#include "gt-tree.h"
