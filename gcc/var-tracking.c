/* Variable tracking routines for the GNU compiler.
   Copyright (C) 2002-2014 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* This file contains the variable tracking pass.  It computes where
   variables are located (which registers or where in memory) at each position
   in instruction stream and emits notes describing the locations.
   Debug information (DWARF2 location lists) is finally generated from
   these notes.
   With this debug information, it is possible to show variables
   even when debugging optimized code.

   How does the variable tracking pass work?

   First, it scans RTL code for uses, stores and clobbers (register/memory
   references in instructions), for call insns and for stack adjustments
   separately for each basic block and saves them to an array of micro
   operations.
   The micro operations of one instruction are ordered so that
   pre-modifying stack adjustment < use < use with no var < call insn <
     < clobber < set < post-modifying stack adjustment

   Then, a forward dataflow analysis is performed to find out how locations
   of variables change through code and to propagate the variable locations
   along control flow graph.
   The IN set for basic block BB is computed as a union of OUT sets of BB's
   predecessors, the OUT set for BB is copied from the IN set for BB and
   is changed according to micro operations in BB.

   The IN and OUT sets for basic blocks consist of a current stack adjustment
   (used for adjusting offset of variables addressed using stack pointer),
   the table of structures describing the locations of parts of a variable
   and for each physical register a linked list for each physical register.
   The linked list is a list of variable parts stored in the register,
   i.e. it is a list of triplets (reg, decl, offset) where decl is
   REG_EXPR (reg) and offset is REG_OFFSET (reg).  The linked list is used for
   effective deleting appropriate variable parts when we set or clobber the
   register.

   There may be more than one variable part in a register.  The linked lists
   should be pretty short so it is a good data structure here.
   For example in the following code, register allocator may assign same
   register to variables A and B, and both of them are stored in the same
   register in CODE:

     if (cond)
       set A;
     else
       set B;
     CODE;
     if (cond)
       use A;
     else
       use B;

   Finally, the NOTE_INSN_VAR_LOCATION notes describing the variable locations
   are emitted to appropriate positions in RTL code.  Each such a note describes
   the location of one variable at the point in instruction stream where the
   note is.  There is no need to emit a note for each variable before each
   instruction, we only emit these notes where the location of variable changes
   (this means that we also emit notes for changes between the OUT set of the
   previous block and the IN set of the current block).

   The notes consist of two parts:
   1. the declaration (from REG_EXPR or MEM_EXPR)
   2. the location of a variable - it is either a simple register/memory
      reference (for simple variables, for example int),
      or a parallel of register/memory references (for a large variables
      which consist of several parts, for example long long).

*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "varasm.h"
#include "stor-layout.h"
#include "pointer-set.h"
#include "hash-table.h"
#include "basic-block.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "insn-config.h"
#include "reload.h"
#include "sbitmap.h"
#include "alloc-pool.h"
#include "fibheap.h"
#include "regs.h"
#include "expr.h"
#include "tree-pass.h"
#include "bitmap.h"
#include "tree-dfa.h"
#include "tree-ssa.h"
#include "cselib.h"
#include "target.h"
#include "params.h"
#include "diagnostic.h"
#include "tree-pretty-print.h"
#include "recog.h"
#include "tm_p.h"
#include "alias.h"

/* var-tracking.c assumes that tree code with the same value as VALUE rtx code
   has no chance to appear in REG_EXPR/MEM_EXPRs and isn't a decl.
   Currently the value is the same as IDENTIFIER_NODE, which has such
   a property.  If this compile time assertion ever fails, make sure that
   the new tree code that equals (int) VALUE has the same property.  */
extern char check_value_val[(int) VALUE == (int) IDENTIFIER_NODE ? 1 : -1];

/* Type of micro operation.  */
enum micro_operation_type
{
  MO_USE,	/* Use location (REG or MEM).  */
  MO_USE_NO_VAR,/* Use location which is not associated with a variable
		   or the variable is not trackable.  */
  MO_VAL_USE,	/* Use location which is associated with a value.  */
  MO_VAL_LOC,   /* Use location which appears in a debug insn.  */
  MO_VAL_SET,	/* Set location associated with a value.  */
  MO_SET,	/* Set location.  */
  MO_COPY,	/* Copy the same portion of a variable from one
		   location to another.  */
  MO_CLOBBER,	/* Clobber location.  */
  MO_CALL,	/* Call insn.  */
  MO_ADJUST	/* Adjust stack pointer.  */

};

static const char * const ATTRIBUTE_UNUSED
micro_operation_type_name[] = {
  "MO_USE",
  "MO_USE_NO_VAR",
  "MO_VAL_USE",
  "MO_VAL_LOC",
  "MO_VAL_SET",
  "MO_SET",
  "MO_COPY",
  "MO_CLOBBER",
  "MO_CALL",
  "MO_ADJUST"
};

/* Where shall the note be emitted?  BEFORE or AFTER the instruction.
   Notes emitted as AFTER_CALL are to take effect during the call,
   rather than after the call.  */
enum emit_note_where
{
  EMIT_NOTE_BEFORE_INSN,
  EMIT_NOTE_AFTER_INSN,
  EMIT_NOTE_AFTER_CALL_INSN
};

/* Structure holding information about micro operation.  */
typedef struct micro_operation_def
{
  /* Type of micro operation.  */
  enum micro_operation_type type;

  /* The instruction which the micro operation is in, for MO_USE,
     MO_USE_NO_VAR, MO_CALL and MO_ADJUST, or the subsequent
     instruction or note in the original flow (before any var-tracking
     notes are inserted, to simplify emission of notes), for MO_SET
     and MO_CLOBBER.  */
  rtx insn;

  union {
    /* Location.  For MO_SET and MO_COPY, this is the SET that
       performs the assignment, if known, otherwise it is the target
       of the assignment.  For MO_VAL_USE and MO_VAL_SET, it is a
       CONCAT of the VALUE and the LOC associated with it.  For
       MO_VAL_LOC, it is a CONCAT of the VALUE and the VAR_LOCATION
       associated with it.  */
    rtx loc;

    /* Stack adjustment.  */
    HOST_WIDE_INT adjust;
  } u;
} micro_operation;


/* A declaration of a variable, or an RTL value being handled like a
   declaration.  */
typedef void *decl_or_value;

/* Return true if a decl_or_value DV is a DECL or NULL.  */
static inline bool
dv_is_decl_p (decl_or_value dv)
{
  return !dv || (int) TREE_CODE ((tree) dv) != (int) VALUE;
}

/* Return true if a decl_or_value is a VALUE rtl.  */
static inline bool
dv_is_value_p (decl_or_value dv)
{
  return dv && !dv_is_decl_p (dv);
}

/* Return the decl in the decl_or_value.  */
static inline tree
dv_as_decl (decl_or_value dv)
{
  gcc_checking_assert (dv_is_decl_p (dv));
  return (tree) dv;
}

/* Return the value in the decl_or_value.  */
static inline rtx
dv_as_value (decl_or_value dv)
{
  gcc_checking_assert (dv_is_value_p (dv));
  return (rtx)dv;
}

/* Return the opaque pointer in the decl_or_value.  */
static inline void *
dv_as_opaque (decl_or_value dv)
{
  return dv;
}


/* Description of location of a part of a variable.  The content of a physical
   register is described by a chain of these structures.
   The chains are pretty short (usually 1 or 2 elements) and thus
   chain is the best data structure.  */
typedef struct attrs_def
{
  /* Pointer to next member of the list.  */
  struct attrs_def *next;

  /* The rtx of register.  */
  rtx loc;

  /* The declaration corresponding to LOC.  */
  decl_or_value dv;

  /* Offset from start of DECL.  */
  HOST_WIDE_INT offset;
} *attrs;

/* Structure for chaining the locations.  */
typedef struct location_chain_def
{
  /* Next element in the chain.  */
  struct location_chain_def *next;

  /* The location (REG, MEM or VALUE).  */
  rtx loc;

  /* The "value" stored in this location.  */
  rtx set_src;

  /* Initialized? */
  enum var_init_status init;
} *location_chain;

/* A vector of loc_exp_dep holds the active dependencies of a one-part
   DV on VALUEs, i.e., the VALUEs expanded so as to form the current
   location of DV.  Each entry is also part of VALUE' s linked-list of
   backlinks back to DV.  */
typedef struct loc_exp_dep_s
{
  /* The dependent DV.  */
  decl_or_value dv;
  /* The dependency VALUE or DECL_DEBUG.  */
  rtx value;
  /* The next entry in VALUE's backlinks list.  */
  struct loc_exp_dep_s *next;
  /* A pointer to the pointer to this entry (head or prev's next) in
     the doubly-linked list.  */
  struct loc_exp_dep_s **pprev;
} loc_exp_dep;


/* This data structure holds information about the depth of a variable
   expansion.  */
typedef struct expand_depth_struct
{
  /* This measures the complexity of the expanded expression.  It
     grows by one for each level of expansion that adds more than one
     operand.  */
  int complexity;
  /* This counts the number of ENTRY_VALUE expressions in an
     expansion.  We want to minimize their use.  */
  int entryvals;
} expand_depth;

/* This data structure is allocated for one-part variables at the time
   of emitting notes.  */
struct onepart_aux
{
  /* Doubly-linked list of dependent DVs.  These are DVs whose cur_loc
     computation used the expansion of this variable, and that ought
     to be notified should this variable change.  If the DV's cur_loc
     expanded to NULL, all components of the loc list are regarded as
     active, so that any changes in them give us a chance to get a
     location.  Otherwise, only components of the loc that expanded to
     non-NULL are regarded as active dependencies.  */
  loc_exp_dep *backlinks;
  /* This holds the LOC that was expanded into cur_loc.  We need only
     mark a one-part variable as changed if the FROM loc is removed,
     or if it has no known location and a loc is added, or if it gets
     a change notification from any of its active dependencies.  */
  rtx from;
  /* The depth of the cur_loc expression.  */
  expand_depth depth;
  /* Dependencies actively used when expand FROM into cur_loc.  */
  vec<loc_exp_dep, va_heap, vl_embed> deps;
};

/* Structure describing one part of variable.  */
typedef struct variable_part_def
{
  /* Chain of locations of the part.  */
  location_chain loc_chain;

  /* Location which was last emitted to location list.  */
  rtx cur_loc;

  union variable_aux
  {
    /* The offset in the variable, if !var->onepart.  */
    HOST_WIDE_INT offset;

    /* Pointer to auxiliary data, if var->onepart and emit_notes.  */
    struct onepart_aux *onepaux;
  } aux;
} variable_part;

/* Maximum number of location parts.  */
#define MAX_VAR_PARTS 16

/* Enumeration type used to discriminate various types of one-part
   variables.  */
typedef enum onepart_enum
{
  /* Not a one-part variable.  */
  NOT_ONEPART = 0,
  /* A one-part DECL that is not a DEBUG_EXPR_DECL.  */
  ONEPART_VDECL = 1,
  /* A DEBUG_EXPR_DECL.  */
  ONEPART_DEXPR = 2,
  /* A VALUE.  */
  ONEPART_VALUE = 3
} onepart_enum_t;

/* Structure describing where the variable is located.  */
typedef struct variable_def
{
  /* The declaration of the variable, or an RTL value being handled
     like a declaration.  */
  decl_or_value dv;

  /* Reference count.  */
  int refcount;

  /* Number of variable parts.  */
  char n_var_parts;

  /* What type of DV this is, according to enum onepart_enum.  */
  ENUM_BITFIELD (onepart_enum) onepart : CHAR_BIT;

  /* True if this variable_def struct is currently in the
     changed_variables hash table.  */
  bool in_changed_variables;

  /* The variable parts.  */
  variable_part var_part[1];
} *variable;
typedef const struct variable_def *const_variable;

/* Pointer to the BB's information specific to variable tracking pass.  */
#define VTI(BB) ((variable_tracking_info) (BB)->aux)

/* Macro to access MEM_OFFSET as an HOST_WIDE_INT.  Evaluates MEM twice.  */
#define INT_MEM_OFFSET(mem) (MEM_OFFSET_KNOWN_P (mem) ? MEM_OFFSET (mem) : 0)

#if ENABLE_CHECKING && (GCC_VERSION >= 2007)

/* Access VAR's Ith part's offset, checking that it's not a one-part
   variable.  */
#define VAR_PART_OFFSET(var, i) __extension__			\
(*({  variable const __v = (var);				\
      gcc_checking_assert (!__v->onepart);			\
      &__v->var_part[(i)].aux.offset; }))

/* Access VAR's one-part auxiliary data, checking that it is a
   one-part variable.  */
#define VAR_LOC_1PAUX(var) __extension__			\
(*({  variable const __v = (var);				\
      gcc_checking_assert (__v->onepart);			\
      &__v->var_part[0].aux.onepaux; }))

#else
#define VAR_PART_OFFSET(var, i) ((var)->var_part[(i)].aux.offset)
#define VAR_LOC_1PAUX(var) ((var)->var_part[0].aux.onepaux)
#endif

/* These are accessor macros for the one-part auxiliary data.  When
   convenient for users, they're guarded by tests that the data was
   allocated.  */
#define VAR_LOC_DEP_LST(var) (VAR_LOC_1PAUX (var)		  \
			      ? VAR_LOC_1PAUX (var)->backlinks	  \
			      : NULL)
#define VAR_LOC_DEP_LSTP(var) (VAR_LOC_1PAUX (var)		  \
			       ? &VAR_LOC_1PAUX (var)->backlinks  \
			       : NULL)
#define VAR_LOC_FROM(var) (VAR_LOC_1PAUX (var)->from)
#define VAR_LOC_DEPTH(var) (VAR_LOC_1PAUX (var)->depth)
#define VAR_LOC_DEP_VEC(var) (VAR_LOC_1PAUX (var)		  \
			      ? &VAR_LOC_1PAUX (var)->deps	  \
			      : NULL)



typedef unsigned int dvuid;

/* Return the uid of DV.  */

static inline dvuid
dv_uid (decl_or_value dv)
{
  if (dv_is_value_p (dv))
    return CSELIB_VAL_PTR (dv_as_value (dv))->uid;
  else
    return DECL_UID (dv_as_decl (dv));
}

/* Compute the hash from the uid.  */

static inline hashval_t
dv_uid2hash (dvuid uid)
{
  return uid;
}

/* The hash function for a mask table in a shared_htab chain.  */

static inline hashval_t
dv_htab_hash (decl_or_value dv)
{
  return dv_uid2hash (dv_uid (dv));
}

static void variable_htab_free (void *);

/* Variable hashtable helpers.  */

struct variable_hasher
{
  typedef variable_def value_type;
  typedef void compare_type;
  static inline hashval_t hash (const value_type *);
  static inline bool equal (const value_type *, const compare_type *);
  static inline void remove (value_type *);
};

/* The hash function for variable_htab, computes the hash value
   from the declaration of variable X.  */

inline hashval_t
variable_hasher::hash (const value_type *v)
{
  return dv_htab_hash (v->dv);
}

/* Compare the declaration of variable X with declaration Y.  */

inline bool
variable_hasher::equal (const value_type *v, const compare_type *y)
{
  decl_or_value dv = CONST_CAST2 (decl_or_value, const void *, y);

  return (dv_as_opaque (v->dv) == dv_as_opaque (dv));
}

/* Free the element of VARIABLE_HTAB (its type is struct variable_def).  */

inline void
variable_hasher::remove (value_type *var)
{
  variable_htab_free (var);
}

typedef hash_table <variable_hasher> variable_table_type;
typedef variable_table_type::iterator variable_iterator_type;

/* Structure for passing some other parameters to function
   emit_note_insn_var_location.  */
typedef struct emit_note_data_def
{
  /* The instruction which the note will be emitted before/after.  */
  rtx insn;

  /* Where the note will be emitted (before/after insn)?  */
  enum emit_note_where where;

  /* The variables and values active at this point.  */
  variable_table_type vars;
} emit_note_data;

/* Structure holding a refcounted hash table.  If refcount > 1,
   it must be first unshared before modified.  */
typedef struct shared_hash_def
{
  /* Reference count.  */
  int refcount;

  /* Actual hash table.  */
  variable_table_type htab;
} *shared_hash;

/* Structure holding the IN or OUT set for a basic block.  */
typedef struct dataflow_set_def
{
  /* Adjustment of stack offset.  */
  HOST_WIDE_INT stack_adjust;

  /* Attributes for registers (lists of attrs).  */
  attrs regs[FIRST_PSEUDO_REGISTER];

  /* Variable locations.  */
  shared_hash vars;

  /* Vars that is being traversed.  */
  shared_hash traversed_vars;
} dataflow_set;

/* The structure (one for each basic block) containing the information
   needed for variable tracking.  */
typedef struct variable_tracking_info_def
{
  /* The vector of micro operations.  */
  vec<micro_operation> mos;

  /* The IN and OUT set for dataflow analysis.  */
  dataflow_set in;
  dataflow_set out;

  /* The permanent-in dataflow set for this block.  This is used to
     hold values for which we had to compute entry values.  ??? This
     should probably be dynamically allocated, to avoid using more
     memory in non-debug builds.  */
  dataflow_set *permp;

  /* Has the block been visited in DFS?  */
  bool visited;

  /* Has the block been flooded in VTA?  */
  bool flooded;

} *variable_tracking_info;

/* Alloc pool for struct attrs_def.  */
static alloc_pool attrs_pool;

/* Alloc pool for struct variable_def with MAX_VAR_PARTS entries.  */
static alloc_pool var_pool;

/* Alloc pool for struct variable_def with a single var_part entry.  */
static alloc_pool valvar_pool;

/* Alloc pool for struct location_chain_def.  */
static alloc_pool loc_chain_pool;

/* Alloc pool for struct shared_hash_def.  */
static alloc_pool shared_hash_pool;

/* Alloc pool for struct loc_exp_dep_s for NOT_ONEPART variables.  */
static alloc_pool loc_exp_dep_pool;

/* Changed variables, notes will be emitted for them.  */
static variable_table_type changed_variables;

/* Shall notes be emitted?  */
static bool emit_notes;

/* Values whose dynamic location lists have gone empty, but whose
   cselib location lists are still usable.  Use this to hold the
   current location, the backlinks, etc, during emit_notes.  */
static variable_table_type dropped_values;

/* Empty shared hashtable.  */
static shared_hash empty_shared_hash;

/* Scratch register bitmap used by cselib_expand_value_rtx.  */
static bitmap scratch_regs = NULL;

#ifdef HAVE_window_save
typedef struct GTY(()) parm_reg {
  rtx outgoing;
  rtx incoming;
} parm_reg_t;


/* Vector of windowed parameter registers, if any.  */
static vec<parm_reg_t, va_gc> *windowed_parm_regs = NULL;
#endif

/* Variable used to tell whether cselib_process_insn called our hook.  */
static bool cselib_hook_called;

/* Local function prototypes.  */
static void stack_adjust_offset_pre_post (rtx, HOST_WIDE_INT *,
					  HOST_WIDE_INT *);
static void insn_stack_adjust_offset_pre_post (rtx, HOST_WIDE_INT *,
					       HOST_WIDE_INT *);
static bool vt_stack_adjustments (void);

static void init_attrs_list_set (attrs *);
static void attrs_list_clear (attrs *);
static attrs attrs_list_member (attrs, decl_or_value, HOST_WIDE_INT);
static void attrs_list_insert (attrs *, decl_or_value, HOST_WIDE_INT, rtx);
static void attrs_list_copy (attrs *, attrs);
static void attrs_list_union (attrs *, attrs);

static variable_def **unshare_variable (dataflow_set *set, variable_def **slot,
					variable var, enum var_init_status);
static void vars_copy (variable_table_type, variable_table_type);
static tree var_debug_decl (tree);
static void var_reg_set (dataflow_set *, rtx, enum var_init_status, rtx);
static void var_reg_delete_and_set (dataflow_set *, rtx, bool,
				    enum var_init_status, rtx);
static void var_reg_delete (dataflow_set *, rtx, bool);
static void var_regno_delete (dataflow_set *, int);
static void var_mem_set (dataflow_set *, rtx, enum var_init_status, rtx);
static void var_mem_delete_and_set (dataflow_set *, rtx, bool,
				    enum var_init_status, rtx);
static void var_mem_delete (dataflow_set *, rtx, bool);

static void dataflow_set_init (dataflow_set *);
static void dataflow_set_clear (dataflow_set *);
static void dataflow_set_copy (dataflow_set *, dataflow_set *);
static int variable_union_info_cmp_pos (const void *, const void *);
static void dataflow_set_union (dataflow_set *, dataflow_set *);
static location_chain find_loc_in_1pdv (rtx, variable, variable_table_type);
static bool canon_value_cmp (rtx, rtx);
static int loc_cmp (rtx, rtx);
static bool variable_part_different_p (variable_part *, variable_part *);
static bool onepart_variable_different_p (variable, variable);
static bool variable_different_p (variable, variable);
static bool dataflow_set_different (dataflow_set *, dataflow_set *);
static void dataflow_set_destroy (dataflow_set *);

static bool contains_symbol_ref (rtx);
static bool track_expr_p (tree, bool);
static bool same_variable_part_p (rtx, tree, HOST_WIDE_INT);
static int add_uses (rtx *, void *);
static void add_uses_1 (rtx *, void *);
static void add_stores (rtx, const_rtx, void *);
static bool compute_bb_dataflow (basic_block);
static bool vt_find_locations (void);

static void dump_attrs_list (attrs);
static void dump_var (variable);
static void dump_vars (variable_table_type);
static void dump_dataflow_set (dataflow_set *);
static void dump_dataflow_sets (void);

static void set_dv_changed (decl_or_value, bool);
static void variable_was_changed (variable, dataflow_set *);
static variable_def **set_slot_part (dataflow_set *, rtx, variable_def **,
				     decl_or_value, HOST_WIDE_INT,
				     enum var_init_status, rtx);
static void set_variable_part (dataflow_set *, rtx,
			       decl_or_value, HOST_WIDE_INT,
			       enum var_init_status, rtx, enum insert_option);
static variable_def **clobber_slot_part (dataflow_set *, rtx,
					 variable_def **, HOST_WIDE_INT, rtx);
static void clobber_variable_part (dataflow_set *, rtx,
				   decl_or_value, HOST_WIDE_INT, rtx);
static variable_def **delete_slot_part (dataflow_set *, rtx, variable_def **,
					HOST_WIDE_INT);
static void delete_variable_part (dataflow_set *, rtx,
				  decl_or_value, HOST_WIDE_INT);
static void emit_notes_in_bb (basic_block, dataflow_set *);
static void vt_emit_notes (void);

static bool vt_get_decl_and_offset (rtx, tree *, HOST_WIDE_INT *);
static void vt_add_function_parameters (void);
static bool vt_initialize (void);
static void vt_finalize (void);

/* Given a SET, calculate the amount of stack adjustment it contains
   PRE- and POST-modifying stack pointer.
   This function is similar to stack_adjust_offset.  */

static void
stack_adjust_offset_pre_post (rtx pattern, HOST_WIDE_INT *pre,
			      HOST_WIDE_INT *post)
{
  rtx src = SET_SRC (pattern);
  rtx dest = SET_DEST (pattern);
  enum rtx_code code;

  if (dest == stack_pointer_rtx)
    {
      /* (set (reg sp) (plus (reg sp) (const_int))) */
      code = GET_CODE (src);
      if (! (code == PLUS || code == MINUS)
	  || XEXP (src, 0) != stack_pointer_rtx
	  || !CONST_INT_P (XEXP (src, 1)))
	return;

      if (code == MINUS)
	*post += INTVAL (XEXP (src, 1));
      else
	*post -= INTVAL (XEXP (src, 1));
    }
  else if (MEM_P (dest))
    {
      /* (set (mem (pre_dec (reg sp))) (foo)) */
      src = XEXP (dest, 0);
      code = GET_CODE (src);

      switch (code)
	{
	case PRE_MODIFY:
	case POST_MODIFY:
	  if (XEXP (src, 0) == stack_pointer_rtx)
	    {
	      rtx val = XEXP (XEXP (src, 1), 1);
	      /* We handle only adjustments by constant amount.  */
	      gcc_assert (GET_CODE (XEXP (src, 1)) == PLUS &&
			  CONST_INT_P (val));

	      if (code == PRE_MODIFY)
		*pre -= INTVAL (val);
	      else
		*post -= INTVAL (val);
	      break;
	    }
	  return;

	case PRE_DEC:
	  if (XEXP (src, 0) == stack_pointer_rtx)
	    {
	      *pre += GET_MODE_SIZE (GET_MODE (dest));
	      break;
	    }
	  return;

	case POST_DEC:
	  if (XEXP (src, 0) == stack_pointer_rtx)
	    {
	      *post += GET_MODE_SIZE (GET_MODE (dest));
	      break;
	    }
	  return;

	case PRE_INC:
	  if (XEXP (src, 0) == stack_pointer_rtx)
	    {
	      *pre -= GET_MODE_SIZE (GET_MODE (dest));
	      break;
	    }
	  return;

	case POST_INC:
	  if (XEXP (src, 0) == stack_pointer_rtx)
	    {
	      *post -= GET_MODE_SIZE (GET_MODE (dest));
	      break;
	    }
	  return;

	default:
	  return;
	}
    }
}

/* Given an INSN, calculate the amount of stack adjustment it contains
   PRE- and POST-modifying stack pointer.  */

static void
insn_stack_adjust_offset_pre_post (rtx insn, HOST_WIDE_INT *pre,
				   HOST_WIDE_INT *post)
{
  rtx pattern;

  *pre = 0;
  *post = 0;

  pattern = PATTERN (insn);
  if (RTX_FRAME_RELATED_P (insn))
    {
      rtx expr = find_reg_note (insn, REG_FRAME_RELATED_EXPR, NULL_RTX);
      if (expr)
	pattern = XEXP (expr, 0);
    }

  if (GET_CODE (pattern) == SET)
    stack_adjust_offset_pre_post (pattern, pre, post);
  else if (GET_CODE (pattern) == PARALLEL
	   || GET_CODE (pattern) == SEQUENCE)
    {
      int i;

      /* There may be stack adjustments inside compound insns.  Search
	 for them.  */
      for ( i = XVECLEN (pattern, 0) - 1; i >= 0; i--)
	if (GET_CODE (XVECEXP (pattern, 0, i)) == SET)
	  stack_adjust_offset_pre_post (XVECEXP (pattern, 0, i), pre, post);
    }
}

/* Compute stack adjustments for all blocks by traversing DFS tree.
   Return true when the adjustments on all incoming edges are consistent.
   Heavily borrowed from pre_and_rev_post_order_compute.  */

static bool
vt_stack_adjustments (void)
{
  edge_iterator *stack;
  int sp;

  /* Initialize entry block.  */
  VTI (ENTRY_BLOCK_PTR_FOR_FN (cfun))->visited = true;
  VTI (ENTRY_BLOCK_PTR_FOR_FN (cfun))->in.stack_adjust =
 INCOMING_FRAME_SP_OFFSET;
  VTI (ENTRY_BLOCK_PTR_FOR_FN (cfun))->out.stack_adjust =
 INCOMING_FRAME_SP_OFFSET;

  /* Allocate stack for back-tracking up CFG.  */
  stack = XNEWVEC (edge_iterator, n_basic_blocks_for_fn (cfun) + 1);
  sp = 0;

  /* Push the first edge on to the stack.  */
  stack[sp++] = ei_start (ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs);

  while (sp)
    {
      edge_iterator ei;
      basic_block src;
      basic_block dest;

      /* Look at the edge on the top of the stack.  */
      ei = stack[sp - 1];
      src = ei_edge (ei)->src;
      dest = ei_edge (ei)->dest;

      /* Check if the edge destination has been visited yet.  */
      if (!VTI (dest)->visited)
	{
	  rtx insn;
	  HOST_WIDE_INT pre, post, offset;
	  VTI (dest)->visited = true;
	  VTI (dest)->in.stack_adjust = offset = VTI (src)->out.stack_adjust;

	  if (dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
	    for (insn = BB_HEAD (dest);
		 insn != NEXT_INSN (BB_END (dest));
		 insn = NEXT_INSN (insn))
	      if (INSN_P (insn))
		{
		  insn_stack_adjust_offset_pre_post (insn, &pre, &post);
		  offset += pre + post;
		}

	  VTI (dest)->out.stack_adjust = offset;

	  if (EDGE_COUNT (dest->succs) > 0)
	    /* Since the DEST node has been visited for the first
	       time, check its successors.  */
	    stack[sp++] = ei_start (dest->succs);
	}
      else
	{
	  /* We can end up with different stack adjustments for the exit block
	     of a shrink-wrapped function if stack_adjust_offset_pre_post
	     doesn't understand the rtx pattern used to restore the stack
	     pointer in the epilogue.  For example, on s390(x), the stack
	     pointer is often restored via a load-multiple instruction
	     and so no stack_adjust offset is recorded for it.  This means
	     that the stack offset at the end of the epilogue block is the
	     the same as the offset before the epilogue, whereas other paths
	     to the exit block will have the correct stack_adjust.

	     It is safe to ignore these differences because (a) we never
	     use the stack_adjust for the exit block in this pass and
	     (b) dwarf2cfi checks whether the CFA notes in a shrink-wrapped
	     function are correct.

	     We must check whether the adjustments on other edges are
	     the same though.  */
	  if (dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
	      && VTI (dest)->in.stack_adjust != VTI (src)->out.stack_adjust)
	    {
	      free (stack);
	      return false;
	    }

	  if (! ei_one_before_end_p (ei))
	    /* Go to the next edge.  */
	    ei_next (&stack[sp - 1]);
	  else
	    /* Return to previous level if there are no more edges.  */
	    sp--;
	}
    }

  free (stack);
  return true;
}

/* arg_pointer_rtx resp. frame_pointer_rtx if stack_pointer_rtx or
   hard_frame_pointer_rtx is being mapped to it and offset for it.  */
static rtx cfa_base_rtx;
static HOST_WIDE_INT cfa_base_offset;

/* Compute a CFA-based value for an ADJUSTMENT made to stack_pointer_rtx
   or hard_frame_pointer_rtx.  */

static inline rtx
compute_cfa_pointer (HOST_WIDE_INT adjustment)
{
  return plus_constant (Pmode, cfa_base_rtx, adjustment + cfa_base_offset);
}

/* Adjustment for hard_frame_pointer_rtx to cfa base reg,
   or -1 if the replacement shouldn't be done.  */
static HOST_WIDE_INT hard_frame_pointer_adjustment = -1;

/* Data for adjust_mems callback.  */

struct adjust_mem_data
{
  bool store;
  enum machine_mode mem_mode;
  HOST_WIDE_INT stack_adjust;
  rtx side_effects;
};

/* Helper for adjust_mems.  Return 1 if *loc is unsuitable for
   transformation of wider mode arithmetics to narrower mode,
   -1 if it is suitable and subexpressions shouldn't be
   traversed and 0 if it is suitable and subexpressions should
   be traversed.  Called through for_each_rtx.  */

static int
use_narrower_mode_test (rtx *loc, void *data)
{
  rtx subreg = (rtx) data;

  if (CONSTANT_P (*loc))
    return -1;
  switch (GET_CODE (*loc))
    {
    case REG:
      if (cselib_lookup (*loc, GET_MODE (SUBREG_REG (subreg)), 0, VOIDmode))
	return 1;
      if (!validate_subreg (GET_MODE (subreg), GET_MODE (*loc),
			    *loc, subreg_lowpart_offset (GET_MODE (subreg),
							 GET_MODE (*loc))))
	return 1;
      return -1;
    case PLUS:
    case MINUS:
    case MULT:
      return 0;
    case ASHIFT:
      if (for_each_rtx (&XEXP (*loc, 0), use_narrower_mode_test, data))
	return 1;
      else
	return -1;
    default:
      return 1;
    }
}

/* Transform X into narrower mode MODE from wider mode WMODE.  */

static rtx
use_narrower_mode (rtx x, enum machine_mode mode, enum machine_mode wmode)
{
  rtx op0, op1;
  if (CONSTANT_P (x))
    return lowpart_subreg (mode, x, wmode);
  switch (GET_CODE (x))
    {
    case REG:
      return lowpart_subreg (mode, x, wmode);
    case PLUS:
    case MINUS:
    case MULT:
      op0 = use_narrower_mode (XEXP (x, 0), mode, wmode);
      op1 = use_narrower_mode (XEXP (x, 1), mode, wmode);
      return simplify_gen_binary (GET_CODE (x), mode, op0, op1);
    case ASHIFT:
      op0 = use_narrower_mode (XEXP (x, 0), mode, wmode);
      return simplify_gen_binary (ASHIFT, mode, op0, XEXP (x, 1));
    default:
      gcc_unreachable ();
    }
}

/* Helper function for adjusting used MEMs.  */

static rtx
adjust_mems (rtx loc, const_rtx old_rtx, void *data)
{
  struct adjust_mem_data *amd = (struct adjust_mem_data *) data;
  rtx mem, addr = loc, tem;
  enum machine_mode mem_mode_save;
  bool store_save;
  switch (GET_CODE (loc))
    {
    case REG:
      /* Don't do any sp or fp replacements outside of MEM addresses
         on the LHS.  */
      if (amd->mem_mode == VOIDmode && amd->store)
	return loc;
      if (loc == stack_pointer_rtx
	  && !frame_pointer_needed
	  && cfa_base_rtx)
	return compute_cfa_pointer (amd->stack_adjust);
      else if (loc == hard_frame_pointer_rtx
	       && frame_pointer_needed
	       && hard_frame_pointer_adjustment != -1
	       && cfa_base_rtx)
	return compute_cfa_pointer (hard_frame_pointer_adjustment);
      gcc_checking_assert (loc != virtual_incoming_args_rtx);
      return loc;
    case MEM:
      mem = loc;
      if (!amd->store)
	{
	  mem = targetm.delegitimize_address (mem);
	  if (mem != loc && !MEM_P (mem))
	    return simplify_replace_fn_rtx (mem, old_rtx, adjust_mems, data);
	}

      addr = XEXP (mem, 0);
      mem_mode_save = amd->mem_mode;
      amd->mem_mode = GET_MODE (mem);
      store_save = amd->store;
      amd->store = false;
      addr = simplify_replace_fn_rtx (addr, old_rtx, adjust_mems, data);
      amd->store = store_save;
      amd->mem_mode = mem_mode_save;
      if (mem == loc)
	addr = targetm.delegitimize_address (addr);
      if (addr != XEXP (mem, 0))
	mem = replace_equiv_address_nv (mem, addr);
      if (!amd->store)
	mem = avoid_constant_pool_reference (mem);
      return mem;
    case PRE_INC:
    case PRE_DEC:
      addr = gen_rtx_PLUS (GET_MODE (loc), XEXP (loc, 0),
			   gen_int_mode (GET_CODE (loc) == PRE_INC
					 ? GET_MODE_SIZE (amd->mem_mode)
					 : -GET_MODE_SIZE (amd->mem_mode),
					 GET_MODE (loc)));
    case POST_INC:
    case POST_DEC:
      if (addr == loc)
	addr = XEXP (loc, 0);
      gcc_assert (amd->mem_mode != VOIDmode && amd->mem_mode != BLKmode);
      addr = simplify_replace_fn_rtx (addr, old_rtx, adjust_mems, data);
      tem = gen_rtx_PLUS (GET_MODE (loc), XEXP (loc, 0),
			  gen_int_mode ((GET_CODE (loc) == PRE_INC
					 || GET_CODE (loc) == POST_INC)
					? GET_MODE_SIZE (amd->mem_mode)
					: -GET_MODE_SIZE (amd->mem_mode),
					GET_MODE (loc)));
      store_save = amd->store;
      amd->store = false;
      tem = simplify_replace_fn_rtx (tem, old_rtx, adjust_mems, data);
      amd->store = store_save;
      amd->side_effects = alloc_EXPR_LIST (0,
					   gen_rtx_SET (VOIDmode,
							XEXP (loc, 0), tem),
					   amd->side_effects);
      return addr;
    case PRE_MODIFY:
      addr = XEXP (loc, 1);
    case POST_MODIFY:
      if (addr == loc)
	addr = XEXP (loc, 0);
      gcc_assert (amd->mem_mode != VOIDmode);
      addr = simplify_replace_fn_rtx (addr, old_rtx, adjust_mems, data);
      store_save = amd->store;
      amd->store = false;
      tem = simplify_replace_fn_rtx (XEXP (loc, 1), old_rtx,
				     adjust_mems, data);
      amd->store = store_save;
      amd->side_effects = alloc_EXPR_LIST (0,
					   gen_rtx_SET (VOIDmode,
							XEXP (loc, 0), tem),
					   amd->side_effects);
      return addr;
    case SUBREG:
      /* First try without delegitimization of whole MEMs and
	 avoid_constant_pool_reference, which is more likely to succeed.  */
      store_save = amd->store;
      amd->store = true;
      addr = simplify_replace_fn_rtx (SUBREG_REG (loc), old_rtx, adjust_mems,
				      data);
      amd->store = store_save;
      mem = simplify_replace_fn_rtx (addr, old_rtx, adjust_mems, data);
      if (mem == SUBREG_REG (loc))
	{
	  tem = loc;
	  goto finish_subreg;
	}
      tem = simplify_gen_subreg (GET_MODE (loc), mem,
				 GET_MODE (SUBREG_REG (loc)),
				 SUBREG_BYTE (loc));
      if (tem)
	goto finish_subreg;
      tem = simplify_gen_subreg (GET_MODE (loc), addr,
				 GET_MODE (SUBREG_REG (loc)),
				 SUBREG_BYTE (loc));
      if (tem == NULL_RTX)
	tem = gen_rtx_raw_SUBREG (GET_MODE (loc), addr, SUBREG_BYTE (loc));
    finish_subreg:
      if (MAY_HAVE_DEBUG_INSNS
	  && GET_CODE (tem) == SUBREG
	  && (GET_CODE (SUBREG_REG (tem)) == PLUS
	      || GET_CODE (SUBREG_REG (tem)) == MINUS
	      || GET_CODE (SUBREG_REG (tem)) == MULT
	      || GET_CODE (SUBREG_REG (tem)) == ASHIFT)
	  && GET_MODE_CLASS (GET_MODE (tem)) == MODE_INT
	  && GET_MODE_CLASS (GET_MODE (SUBREG_REG (tem))) == MODE_INT
	  && GET_MODE_SIZE (GET_MODE (tem))
	     < GET_MODE_SIZE (GET_MODE (SUBREG_REG (tem)))
	  && subreg_lowpart_p (tem)
	  && !for_each_rtx (&SUBREG_REG (tem), use_narrower_mode_test, tem))
	return use_narrower_mode (SUBREG_REG (tem), GET_MODE (tem),
				  GET_MODE (SUBREG_REG (tem)));
      return tem;
    case ASM_OPERANDS:
      /* Don't do any replacements in second and following
	 ASM_OPERANDS of inline-asm with multiple sets.
	 ASM_OPERANDS_INPUT_VEC, ASM_OPERANDS_INPUT_CONSTRAINT_VEC
	 and ASM_OPERANDS_LABEL_VEC need to be equal between
	 all the ASM_OPERANDs in the insn and adjust_insn will
	 fix this up.  */
      if (ASM_OPERANDS_OUTPUT_IDX (loc) != 0)
	return loc;
      break;
    default:
      break;
    }
  return NULL_RTX;
}

/* Helper function for replacement of uses.  */

static void
adjust_mem_uses (rtx *x, void *data)
{
  rtx new_x = simplify_replace_fn_rtx (*x, NULL_RTX, adjust_mems, data);
  if (new_x != *x)
    validate_change (NULL_RTX, x, new_x, true);
}

/* Helper function for replacement of stores.  */

static void
adjust_mem_stores (rtx loc, const_rtx expr, void *data)
{
  if (MEM_P (loc))
    {
      rtx new_dest = simplify_replace_fn_rtx (SET_DEST (expr), NULL_RTX,
					      adjust_mems, data);
      if (new_dest != SET_DEST (expr))
	{
	  rtx xexpr = CONST_CAST_RTX (expr);
	  validate_change (NULL_RTX, &SET_DEST (xexpr), new_dest, true);
	}
    }
}

/* Simplify INSN.  Remove all {PRE,POST}_{INC,DEC,MODIFY} rtxes,
   replace them with their value in the insn and add the side-effects
   as other sets to the insn.  */

static void
adjust_insn (basic_block bb, rtx insn)
{
  struct adjust_mem_data amd;
  rtx set;

#ifdef HAVE_window_save
  /* If the target machine has an explicit window save instruction, the
     transformation OUTGOING_REGNO -> INCOMING_REGNO is done there.  */
  if (RTX_FRAME_RELATED_P (insn)
      && find_reg_note (insn, REG_CFA_WINDOW_SAVE, NULL_RTX))
    {
      unsigned int i, nregs = vec_safe_length (windowed_parm_regs);
      rtx rtl = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (nregs * 2));
      parm_reg_t *p;

      FOR_EACH_VEC_SAFE_ELT (windowed_parm_regs, i, p)
	{
	  XVECEXP (rtl, 0, i * 2)
	    = gen_rtx_SET (VOIDmode, p->incoming, p->outgoing);
	  /* Do not clobber the attached DECL, but only the REG.  */
	  XVECEXP (rtl, 0, i * 2 + 1)
	    = gen_rtx_CLOBBER (GET_MODE (p->outgoing),
			       gen_raw_REG (GET_MODE (p->outgoing),
					    REGNO (p->outgoing)));
	}

      validate_change (NULL_RTX, &PATTERN (insn), rtl, true);
      return;
    }
#endif

  amd.mem_mode = VOIDmode;
  amd.stack_adjust = -VTI (bb)->out.stack_adjust;
  amd.side_effects = NULL_RTX;

  amd.store = true;
  note_stores (PATTERN (insn), adjust_mem_stores, &amd);

  amd.store = false;
  if (GET_CODE (PATTERN (insn)) == PARALLEL
      && asm_noperands (PATTERN (insn)) > 0
      && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == SET)
    {
      rtx body, set0;
      int i;

      /* inline-asm with multiple sets is tiny bit more complicated,
	 because the 3 vectors in ASM_OPERANDS need to be shared between
	 all ASM_OPERANDS in the instruction.  adjust_mems will
	 not touch ASM_OPERANDS other than the first one, asm_noperands
	 test above needs to be called before that (otherwise it would fail)
	 and afterwards this code fixes it up.  */
      note_uses (&PATTERN (insn), adjust_mem_uses, &amd);
      body = PATTERN (insn);
      set0 = XVECEXP (body, 0, 0);
      gcc_checking_assert (GET_CODE (set0) == SET
			   && GET_CODE (SET_SRC (set0)) == ASM_OPERANDS
			   && ASM_OPERANDS_OUTPUT_IDX (SET_SRC (set0)) == 0);
      for (i = 1; i < XVECLEN (body, 0); i++)
	if (GET_CODE (XVECEXP (body, 0, i)) != SET)
	  break;
	else
	  {
	    set = XVECEXP (body, 0, i);
	    gcc_checking_assert (GET_CODE (SET_SRC (set)) == ASM_OPERANDS
				 && ASM_OPERANDS_OUTPUT_IDX (SET_SRC (set))
				    == i);
	    if (ASM_OPERANDS_INPUT_VEC (SET_SRC (set))
		!= ASM_OPERANDS_INPUT_VEC (SET_SRC (set0))
		|| ASM_OPERANDS_INPUT_CONSTRAINT_VEC (SET_SRC (set))
		   != ASM_OPERANDS_INPUT_CONSTRAINT_VEC (SET_SRC (set0))
		|| ASM_OPERANDS_LABEL_VEC (SET_SRC (set))
		   != ASM_OPERANDS_LABEL_VEC (SET_SRC (set0)))
	      {
		rtx newsrc = shallow_copy_rtx (SET_SRC (set));
		ASM_OPERANDS_INPUT_VEC (newsrc)
		  = ASM_OPERANDS_INPUT_VEC (SET_SRC (set0));
		ASM_OPERANDS_INPUT_CONSTRAINT_VEC (newsrc)
		  = ASM_OPERANDS_INPUT_CONSTRAINT_VEC (SET_SRC (set0));
		ASM_OPERANDS_LABEL_VEC (newsrc)
		  = ASM_OPERANDS_LABEL_VEC (SET_SRC (set0));
		validate_change (NULL_RTX, &SET_SRC (set), newsrc, true);
	      }
	  }
    }
  else
    note_uses (&PATTERN (insn), adjust_mem_uses, &amd);

  /* For read-only MEMs containing some constant, prefer those
     constants.  */
  set = single_set (insn);
  if (set && MEM_P (SET_SRC (set)) && MEM_READONLY_P (SET_SRC (set)))
    {
      rtx note = find_reg_equal_equiv_note (insn);

      if (note && CONSTANT_P (XEXP (note, 0)))
	validate_change (NULL_RTX, &SET_SRC (set), XEXP (note, 0), true);
    }

  if (amd.side_effects)
    {
      rtx *pat, new_pat, s;
      int i, oldn, newn;

      pat = &PATTERN (insn);
      if (GET_CODE (*pat) == COND_EXEC)
	pat = &COND_EXEC_CODE (*pat);
      if (GET_CODE (*pat) == PARALLEL)
	oldn = XVECLEN (*pat, 0);
      else
	oldn = 1;
      for (s = amd.side_effects, newn = 0; s; newn++)
	s = XEXP (s, 1);
      new_pat = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (oldn + newn));
      if (GET_CODE (*pat) == PARALLEL)
	for (i = 0; i < oldn; i++)
	  XVECEXP (new_pat, 0, i) = XVECEXP (*pat, 0, i);
      else
	XVECEXP (new_pat, 0, 0) = *pat;
      for (s = amd.side_effects, i = oldn; i < oldn + newn; i++, s = XEXP (s, 1))
	XVECEXP (new_pat, 0, i) = XEXP (s, 0);
      free_EXPR_LIST_list (&amd.side_effects);
      validate_change (NULL_RTX, pat, new_pat, true);
    }
}

/* Return the DEBUG_EXPR of a DEBUG_EXPR_DECL or the VALUE in DV.  */
static inline rtx
dv_as_rtx (decl_or_value dv)
{
  tree decl;

  if (dv_is_value_p (dv))
    return dv_as_value (dv);

  decl = dv_as_decl (dv);

  gcc_checking_assert (TREE_CODE (decl) == DEBUG_EXPR_DECL);
  return DECL_RTL_KNOWN_SET (decl);
}

/* Return nonzero if a decl_or_value must not have more than one
   variable part.  The returned value discriminates among various
   kinds of one-part DVs ccording to enum onepart_enum.  */
static inline onepart_enum_t
dv_onepart_p (decl_or_value dv)
{
  tree decl;

  if (!MAY_HAVE_DEBUG_INSNS)
    return NOT_ONEPART;

  if (dv_is_value_p (dv))
    return ONEPART_VALUE;

  decl = dv_as_decl (dv);

  if (TREE_CODE (decl) == DEBUG_EXPR_DECL)
    return ONEPART_DEXPR;

  if (target_for_debug_bind (decl) != NULL_TREE)
    return ONEPART_VDECL;

  return NOT_ONEPART;
}

/* Return the variable pool to be used for a dv of type ONEPART.  */
static inline alloc_pool
onepart_pool (onepart_enum_t onepart)
{
  return onepart ? valvar_pool : var_pool;
}

/* Build a decl_or_value out of a decl.  */
static inline decl_or_value
dv_from_decl (tree decl)
{
  decl_or_value dv;
  dv = decl;
  gcc_checking_assert (dv_is_decl_p (dv));
  return dv;
}

/* Build a decl_or_value out of a value.  */
static inline decl_or_value
dv_from_value (rtx value)
{
  decl_or_value dv;
  dv = value;
  gcc_checking_assert (dv_is_value_p (dv));
  return dv;
}

/* Return a value or the decl of a debug_expr as a decl_or_value.  */
static inline decl_or_value
dv_from_rtx (rtx x)
{
  decl_or_value dv;

  switch (GET_CODE (x))
    {
    case DEBUG_EXPR:
      dv = dv_from_decl (DEBUG_EXPR_TREE_DECL (x));
      gcc_checking_assert (DECL_RTL_KNOWN_SET (DEBUG_EXPR_TREE_DECL (x)) == x);
      break;

    case VALUE:
      dv = dv_from_value (x);
      break;

    default:
      gcc_unreachable ();
    }

  return dv;
}

extern void debug_dv (decl_or_value dv);

DEBUG_FUNCTION void
debug_dv (decl_or_value dv)
{
  if (dv_is_value_p (dv))
    debug_rtx (dv_as_value (dv));
  else
    debug_generic_stmt (dv_as_decl (dv));
}

static void loc_exp_dep_clear (variable var);

/* Free the element of VARIABLE_HTAB (its type is struct variable_def).  */

static void
variable_htab_free (void *elem)
{
  int i;
  variable var = (variable) elem;
  location_chain node, next;

  gcc_checking_assert (var->refcount > 0);

  var->refcount--;
  if (var->refcount > 0)
    return;

  for (i = 0; i < var->n_var_parts; i++)
    {
      for (node = var->var_part[i].loc_chain; node; node = next)
	{
	  next = node->next;
	  pool_free (loc_chain_pool, node);
	}
      var->var_part[i].loc_chain = NULL;
    }
  if (var->onepart && VAR_LOC_1PAUX (var))
    {
      loc_exp_dep_clear (var);
      if (VAR_LOC_DEP_LST (var))
	VAR_LOC_DEP_LST (var)->pprev = NULL;
      XDELETE (VAR_LOC_1PAUX (var));
      /* These may be reused across functions, so reset
	 e.g. NO_LOC_P.  */
      if (var->onepart == ONEPART_DEXPR)
	set_dv_changed (var->dv, true);
    }
  pool_free (onepart_pool (var->onepart), var);
}

/* Initialize the set (array) SET of attrs to empty lists.  */

static void
init_attrs_list_set (attrs *set)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    set[i] = NULL;
}

/* Make the list *LISTP empty.  */

static void
attrs_list_clear (attrs *listp)
{
  attrs list, next;

  for (list = *listp; list; list = next)
    {
      next = list->next;
      pool_free (attrs_pool, list);
    }
  *listp = NULL;
}

/* Return true if the pair of DECL and OFFSET is the member of the LIST.  */

static attrs
attrs_list_member (attrs list, decl_or_value dv, HOST_WIDE_INT offset)
{
  for (; list; list = list->next)
    if (dv_as_opaque (list->dv) == dv_as_opaque (dv) && list->offset == offset)
      return list;
  return NULL;
}

/* Insert the triplet DECL, OFFSET, LOC to the list *LISTP.  */

static void
attrs_list_insert (attrs *listp, decl_or_value dv,
		   HOST_WIDE_INT offset, rtx loc)
{
  attrs list;

  list = (attrs) pool_alloc (attrs_pool);
  list->loc = loc;
  list->dv = dv;
  list->offset = offset;
  list->next = *listp;
  *listp = list;
}

/* Copy all nodes from SRC and create a list *DSTP of the copies.  */

static void
attrs_list_copy (attrs *dstp, attrs src)
{
  attrs n;

  attrs_list_clear (dstp);
  for (; src; src = src->next)
    {
      n = (attrs) pool_alloc (attrs_pool);
      n->loc = src->loc;
      n->dv = src->dv;
      n->offset = src->offset;
      n->next = *dstp;
      *dstp = n;
    }
}

/* Add all nodes from SRC which are not in *DSTP to *DSTP.  */

static void
attrs_list_union (attrs *dstp, attrs src)
{
  for (; src; src = src->next)
    {
      if (!attrs_list_member (*dstp, src->dv, src->offset))
	attrs_list_insert (dstp, src->dv, src->offset, src->loc);
    }
}

/* Combine nodes that are not onepart nodes from SRC and SRC2 into
   *DSTP.  */

static void
attrs_list_mpdv_union (attrs *dstp, attrs src, attrs src2)
{
  gcc_assert (!*dstp);
  for (; src; src = src->next)
    {
      if (!dv_onepart_p (src->dv))
	attrs_list_insert (dstp, src->dv, src->offset, src->loc);
    }
  for (src = src2; src; src = src->next)
    {
      if (!dv_onepart_p (src->dv)
	  && !attrs_list_member (*dstp, src->dv, src->offset))
	attrs_list_insert (dstp, src->dv, src->offset, src->loc);
    }
}

/* Shared hashtable support.  */

/* Return true if VARS is shared.  */

static inline bool
shared_hash_shared (shared_hash vars)
{
  return vars->refcount > 1;
}

/* Return the hash table for VARS.  */

static inline variable_table_type
shared_hash_htab (shared_hash vars)
{
  return vars->htab;
}

/* Return true if VAR is shared, or maybe because VARS is shared.  */

static inline bool
shared_var_p (variable var, shared_hash vars)
{
  /* Don't count an entry in the changed_variables table as a duplicate.  */
  return ((var->refcount > 1 + (int) var->in_changed_variables)
	  || shared_hash_shared (vars));
}

/* Copy variables into a new hash table.  */

static shared_hash
shared_hash_unshare (shared_hash vars)
{
  shared_hash new_vars = (shared_hash) pool_alloc (shared_hash_pool);
  gcc_assert (vars->refcount > 1);
  new_vars->refcount = 1;
  new_vars->htab.create (vars->htab.elements () + 3);
  vars_copy (new_vars->htab, vars->htab);
  vars->refcount--;
  return new_vars;
}

/* Increment reference counter on VARS and return it.  */

static inline shared_hash
shared_hash_copy (shared_hash vars)
{
  vars->refcount++;
  return vars;
}

/* Decrement reference counter and destroy hash table if not shared
   anymore.  */

static void
shared_hash_destroy (shared_hash vars)
{
  gcc_checking_assert (vars->refcount > 0);
  if (--vars->refcount == 0)
    {
      vars->htab.dispose ();
      pool_free (shared_hash_pool, vars);
    }
}

/* Unshare *PVARS if shared and return slot for DV.  If INS is
   INSERT, insert it if not already present.  */

static inline variable_def **
shared_hash_find_slot_unshare_1 (shared_hash *pvars, decl_or_value dv,
				 hashval_t dvhash, enum insert_option ins)
{
  if (shared_hash_shared (*pvars))
    *pvars = shared_hash_unshare (*pvars);
  return shared_hash_htab (*pvars).find_slot_with_hash (dv, dvhash, ins);
}

static inline variable_def **
shared_hash_find_slot_unshare (shared_hash *pvars, decl_or_value dv,
			       enum insert_option ins)
{
  return shared_hash_find_slot_unshare_1 (pvars, dv, dv_htab_hash (dv), ins);
}

/* Return slot for DV, if it is already present in the hash table.
   If it is not present, insert it only VARS is not shared, otherwise
   return NULL.  */

static inline variable_def **
shared_hash_find_slot_1 (shared_hash vars, decl_or_value dv, hashval_t dvhash)
{
  return shared_hash_htab (vars).find_slot_with_hash (dv, dvhash,
						      shared_hash_shared (vars)
						      ? NO_INSERT : INSERT);
}

static inline variable_def **
shared_hash_find_slot (shared_hash vars, decl_or_value dv)
{
  return shared_hash_find_slot_1 (vars, dv, dv_htab_hash (dv));
}

/* Return slot for DV only if it is already present in the hash table.  */

static inline variable_def **
shared_hash_find_slot_noinsert_1 (shared_hash vars, decl_or_value dv,
				  hashval_t dvhash)
{
  return shared_hash_htab (vars).find_slot_with_hash (dv, dvhash, NO_INSERT);
}

static inline variable_def **
shared_hash_find_slot_noinsert (shared_hash vars, decl_or_value dv)
{
  return shared_hash_find_slot_noinsert_1 (vars, dv, dv_htab_hash (dv));
}

/* Return variable for DV or NULL if not already present in the hash
   table.  */

static inline variable
shared_hash_find_1 (shared_hash vars, decl_or_value dv, hashval_t dvhash)
{
  return shared_hash_htab (vars).find_with_hash (dv, dvhash);
}

static inline variable
shared_hash_find (shared_hash vars, decl_or_value dv)
{
  return shared_hash_find_1 (vars, dv, dv_htab_hash (dv));
}

/* Return true if TVAL is better than CVAL as a canonival value.  We
   choose lowest-numbered VALUEs, using the RTX address as a
   tie-breaker.  The idea is to arrange them into a star topology,
   such that all of them are at most one step away from the canonical
   value, and the canonical value has backlinks to all of them, in
   addition to all the actual locations.  We don't enforce this
   topology throughout the entire dataflow analysis, though.
 */

static inline bool
canon_value_cmp (rtx tval, rtx cval)
{
  return !cval
    || CSELIB_VAL_PTR (tval)->uid < CSELIB_VAL_PTR (cval)->uid;
}

static bool dst_can_be_shared;

/* Return a copy of a variable VAR and insert it to dataflow set SET.  */

static variable_def **
unshare_variable (dataflow_set *set, variable_def **slot, variable var,
		  enum var_init_status initialized)
{
  variable new_var;
  int i;

  new_var = (variable) pool_alloc (onepart_pool (var->onepart));
  new_var->dv = var->dv;
  new_var->refcount = 1;
  var->refcount--;
  new_var->n_var_parts = var->n_var_parts;
  new_var->onepart = var->onepart;
  new_var->in_changed_variables = false;

  if (! flag_var_tracking_uninit)
    initialized = VAR_INIT_STATUS_INITIALIZED;

  for (i = 0; i < var->n_var_parts; i++)
    {
      location_chain node;
      location_chain *nextp;

      if (i == 0 && var->onepart)
	{
	  /* One-part auxiliary data is only used while emitting
	     notes, so propagate it to the new variable in the active
	     dataflow set.  If we're not emitting notes, this will be
	     a no-op.  */
	  gcc_checking_assert (!VAR_LOC_1PAUX (var) || emit_notes);
	  VAR_LOC_1PAUX (new_var) = VAR_LOC_1PAUX (var);
	  VAR_LOC_1PAUX (var) = NULL;
	}
      else
	VAR_PART_OFFSET (new_var, i) = VAR_PART_OFFSET (var, i);
      nextp = &new_var->var_part[i].loc_chain;
      for (node = var->var_part[i].loc_chain; node; node = node->next)
	{
	  location_chain new_lc;

	  new_lc = (location_chain) pool_alloc (loc_chain_pool);
	  new_lc->next = NULL;
	  if (node->init > initialized)
	    new_lc->init = node->init;
	  else
	    new_lc->init = initialized;
	  if (node->set_src && !(MEM_P (node->set_src)))
	    new_lc->set_src = node->set_src;
	  else
	    new_lc->set_src = NULL;
	  new_lc->loc = node->loc;

	  *nextp = new_lc;
	  nextp = &new_lc->next;
	}

      new_var->var_part[i].cur_loc = var->var_part[i].cur_loc;
    }

  dst_can_be_shared = false;
  if (shared_hash_shared (set->vars))
    slot = shared_hash_find_slot_unshare (&set->vars, var->dv, NO_INSERT);
  else if (set->traversed_vars && set->vars != set->traversed_vars)
    slot = shared_hash_find_slot_noinsert (set->vars, var->dv);
  *slot = new_var;
  if (var->in_changed_variables)
    {
      variable_def **cslot
	= changed_variables.find_slot_with_hash (var->dv,
				    dv_htab_hash (var->dv), NO_INSERT);
      gcc_assert (*cslot == (void *) var);
      var->in_changed_variables = false;
      variable_htab_free (var);
      *cslot = new_var;
      new_var->in_changed_variables = true;
    }
  return slot;
}

/* Copy all variables from hash table SRC to hash table DST.  */

static void
vars_copy (variable_table_type dst, variable_table_type src)
{
  variable_iterator_type hi;
  variable var;

  FOR_EACH_HASH_TABLE_ELEMENT (src, var, variable, hi)
    {
      variable_def **dstp;
      var->refcount++;
      dstp = dst.find_slot_with_hash (var->dv, dv_htab_hash (var->dv), INSERT);
      *dstp = var;
    }
}

/* Map a decl to its main debug decl.  */

static inline tree
var_debug_decl (tree decl)
{
  if (decl && TREE_CODE (decl) == VAR_DECL
      && DECL_HAS_DEBUG_EXPR_P (decl))
    {
      tree debugdecl = DECL_DEBUG_EXPR (decl);
      if (DECL_P (debugdecl))
	decl = debugdecl;
    }

  return decl;
}

/* Set the register LOC to contain DV, OFFSET.  */

static void
var_reg_decl_set (dataflow_set *set, rtx loc, enum var_init_status initialized,
		  decl_or_value dv, HOST_WIDE_INT offset, rtx set_src,
		  enum insert_option iopt)
{
  attrs node;
  bool decl_p = dv_is_decl_p (dv);

  if (decl_p)
    dv = dv_from_decl (var_debug_decl (dv_as_decl (dv)));

  for (node = set->regs[REGNO (loc)]; node; node = node->next)
    if (dv_as_opaque (node->dv) == dv_as_opaque (dv)
	&& node->offset == offset)
      break;
  if (!node)
    attrs_list_insert (&set->regs[REGNO (loc)], dv, offset, loc);
  set_variable_part (set, loc, dv, offset, initialized, set_src, iopt);
}

/* Set the register to contain REG_EXPR (LOC), REG_OFFSET (LOC).  */

static void
var_reg_set (dataflow_set *set, rtx loc, enum var_init_status initialized,
	     rtx set_src)
{
  tree decl = REG_EXPR (loc);
  HOST_WIDE_INT offset = REG_OFFSET (loc);

  var_reg_decl_set (set, loc, initialized,
		    dv_from_decl (decl), offset, set_src, INSERT);
}

static enum var_init_status
get_init_value (dataflow_set *set, rtx loc, decl_or_value dv)
{
  variable var;
  int i;
  enum var_init_status ret_val = VAR_INIT_STATUS_UNKNOWN;

  if (! flag_var_tracking_uninit)
    return VAR_INIT_STATUS_INITIALIZED;

  var = shared_hash_find (set->vars, dv);
  if (var)
    {
      for (i = 0; i < var->n_var_parts && ret_val == VAR_INIT_STATUS_UNKNOWN; i++)
	{
	  location_chain nextp;
	  for (nextp = var->var_part[i].loc_chain; nextp; nextp = nextp->next)
	    if (rtx_equal_p (nextp->loc, loc))
	      {
		ret_val = nextp->init;
		break;
	      }
	}
    }

  return ret_val;
}

/* Delete current content of register LOC in dataflow set SET and set
   the register to contain REG_EXPR (LOC), REG_OFFSET (LOC).  If
   MODIFY is true, any other live copies of the same variable part are
   also deleted from the dataflow set, otherwise the variable part is
   assumed to be copied from another location holding the same
   part.  */

static void
var_reg_delete_and_set (dataflow_set *set, rtx loc, bool modify,
			enum var_init_status initialized, rtx set_src)
{
  tree decl = REG_EXPR (loc);
  HOST_WIDE_INT offset = REG_OFFSET (loc);
  attrs node, next;
  attrs *nextp;

  decl = var_debug_decl (decl);

  if (initialized == VAR_INIT_STATUS_UNKNOWN)
    initialized = get_init_value (set, loc, dv_from_decl (decl));

  nextp = &set->regs[REGNO (loc)];
  for (node = *nextp; node; node = next)
    {
      next = node->next;
      if (dv_as_opaque (node->dv) != decl || node->offset != offset)
	{
	  delete_variable_part (set, node->loc, node->dv, node->offset);
	  pool_free (attrs_pool, node);
	  *nextp = next;
	}
      else
	{
	  node->loc = loc;
	  nextp = &node->next;
	}
    }
  if (modify)
    clobber_variable_part (set, loc, dv_from_decl (decl), offset, set_src);
  var_reg_set (set, loc, initialized, set_src);
}

/* Delete the association of register LOC in dataflow set SET with any
   variables that aren't onepart.  If CLOBBER is true, also delete any
   other live copies of the same variable part, and delete the
   association with onepart dvs too.  */

static void
var_reg_delete (dataflow_set *set, rtx loc, bool clobber)
{
  attrs *nextp = &set->regs[REGNO (loc)];
  attrs node, next;

  if (clobber)
    {
      tree decl = REG_EXPR (loc);
      HOST_WIDE_INT offset = REG_OFFSET (loc);

      decl = var_debug_decl (decl);

      clobber_variable_part (set, NULL, dv_from_decl (decl), offset, NULL);
    }

  for (node = *nextp; node; node = next)
    {
      next = node->next;
      if (clobber || !dv_onepart_p (node->dv))
	{
	  delete_variable_part (set, node->loc, node->dv, node->offset);
	  pool_free (attrs_pool, node);
	  *nextp = next;
	}
      else
	nextp = &node->next;
    }
}

/* Delete content of register with number REGNO in dataflow set SET.  */

static void
var_regno_delete (dataflow_set *set, int regno)
{
  attrs *reg = &set->regs[regno];
  attrs node, next;

  for (node = *reg; node; node = next)
    {
      next = node->next;
      delete_variable_part (set, node->loc, node->dv, node->offset);
      pool_free (attrs_pool, node);
    }
  *reg = NULL;
}

/* Return true if I is the negated value of a power of two.  */
static bool
negative_power_of_two_p (HOST_WIDE_INT i)
{
  unsigned HOST_WIDE_INT x = -(unsigned HOST_WIDE_INT)i;
  return x == (x & -x);
}

/* Strip constant offsets and alignments off of LOC.  Return the base
   expression.  */

static rtx
vt_get_canonicalize_base (rtx loc)
{
  while ((GET_CODE (loc) == PLUS
	  || GET_CODE (loc) == AND)
	 && GET_CODE (XEXP (loc, 1)) == CONST_INT
	 && (GET_CODE (loc) != AND
	     || negative_power_of_two_p (INTVAL (XEXP (loc, 1)))))
    loc = XEXP (loc, 0);

  return loc;
}

/* This caches canonicalized addresses for VALUEs, computed using
   information in the global cselib table.  */
static struct pointer_map_t *global_get_addr_cache;

/* This caches canonicalized addresses for VALUEs, computed using
   information from the global cache and information pertaining to a
   basic block being analyzed.  */
static struct pointer_map_t *local_get_addr_cache;

static rtx vt_canonicalize_addr (dataflow_set *, rtx);

/* Return the canonical address for LOC, that must be a VALUE, using a
   cached global equivalence or computing it and storing it in the
   global cache.  */

static rtx
get_addr_from_global_cache (rtx const loc)
{
  rtx x;
  void **slot;

  gcc_checking_assert (GET_CODE (loc) == VALUE);
  
  slot = pointer_map_insert (global_get_addr_cache, loc);
  if (*slot)
    return (rtx)*slot;

  x = canon_rtx (get_addr (loc));

  /* Tentative, avoiding infinite recursion.  */
  *slot = x;

  if (x != loc)
    {
      rtx nx = vt_canonicalize_addr (NULL, x);
      if (nx != x)
	{
	  /* The table may have moved during recursion, recompute
	     SLOT.  */
	  slot = pointer_map_contains (global_get_addr_cache, loc);
	  *slot = x = nx;
	}
    }

  return x;
}

/* Return the canonical address for LOC, that must be a VALUE, using a
   cached local equivalence or computing it and storing it in the
   local cache.  */

static rtx
get_addr_from_local_cache (dataflow_set *set, rtx const loc)
{
  rtx x;
  void **slot;
  decl_or_value dv;
  variable var;
  location_chain l;

  gcc_checking_assert (GET_CODE (loc) == VALUE);
  
  slot = pointer_map_insert (local_get_addr_cache, loc);
  if (*slot)
    return (rtx)*slot;

  x = get_addr_from_global_cache (loc);
  
  /* Tentative, avoiding infinite recursion.  */
  *slot = x;

  /* Recurse to cache local expansion of X, or if we need to search
     for a VALUE in the expansion.  */
  if (x != loc)
    {
      rtx nx = vt_canonicalize_addr (set, x);
      if (nx != x)
	{
	  slot = pointer_map_contains (local_get_addr_cache, loc);
	  *slot = x = nx;
	}
      return x;
    }

  dv = dv_from_rtx (x);
  var = shared_hash_find (set->vars, dv);
  if (!var)
    return x;

  /* Look for an improved equivalent expression.  */
  for (l = var->var_part[0].loc_chain; l; l = l->next)
    {
      rtx base = vt_get_canonicalize_base (l->loc);
      if (GET_CODE (base) == VALUE
	  && canon_value_cmp (base, loc))
	{
	  rtx nx = vt_canonicalize_addr (set, l->loc);
	  if (x != nx)
	    {
	      slot = pointer_map_contains (local_get_addr_cache, loc);
	      *slot = x = nx;
	    }
	  break;
	}
    }

  return x;
}

/* Canonicalize LOC using equivalences from SET in addition to those
   in the cselib static table.  It expects a VALUE-based expression,
   and it will only substitute VALUEs with other VALUEs or
   function-global equivalences, so that, if two addresses have base
   VALUEs that are locally or globally related in ways that
   memrefs_conflict_p cares about, they will both canonicalize to
   expressions that have the same base VALUE.

   The use of VALUEs as canonical base addresses enables the canonical
   RTXs to remain unchanged globally, if they resolve to a constant,
   or throughout a basic block otherwise, so that they can be cached
   and the cache needs not be invalidated when REGs, MEMs or such
   change.  */

static rtx
vt_canonicalize_addr (dataflow_set *set, rtx oloc)
{
  HOST_WIDE_INT ofst = 0;
  enum machine_mode mode = GET_MODE (oloc);
  rtx loc = oloc;
  rtx x;
  bool retry = true;

  while (retry)
    {
      while (GET_CODE (loc) == PLUS
	     && GET_CODE (XEXP (loc, 1)) == CONST_INT)
	{
	  ofst += INTVAL (XEXP (loc, 1));
	  loc = XEXP (loc, 0);
	}

      /* Alignment operations can't normally be combined, so just
	 canonicalize the base and we're done.  We'll normally have
	 only one stack alignment anyway.  */
      if (GET_CODE (loc) == AND
	  && GET_CODE (XEXP (loc, 1)) == CONST_INT
	  && negative_power_of_two_p (INTVAL (XEXP (loc, 1))))
	{
	  x = vt_canonicalize_addr (set, XEXP (loc, 0));
	  if (x != XEXP (loc, 0))
	    loc = gen_rtx_AND (mode, x, XEXP (loc, 1));
	  retry = false;
	}

      if (GET_CODE (loc) == VALUE)
	{
	  if (set)
	    loc = get_addr_from_local_cache (set, loc);
	  else
	    loc = get_addr_from_global_cache (loc);

	  /* Consolidate plus_constants.  */
	  while (ofst && GET_CODE (loc) == PLUS
		 && GET_CODE (XEXP (loc, 1)) == CONST_INT)
	    {
	      ofst += INTVAL (XEXP (loc, 1));
	      loc = XEXP (loc, 0);
	    }

	  retry = false;
	}
      else
	{
	  x = canon_rtx (loc);
	  if (retry)
	    retry = (x != loc);
	  loc = x;
	}
    }

  /* Add OFST back in.  */
  if (ofst)
    {
      /* Don't build new RTL if we can help it.  */
      if (GET_CODE (oloc) == PLUS
	  && XEXP (oloc, 0) == loc
	  && INTVAL (XEXP (oloc, 1)) == ofst)
	return oloc;

      loc = plus_constant (mode, loc, ofst);
    }

  return loc;
}

/* Return true iff there's a true dependence between MLOC and LOC.
   MADDR must be a canonicalized version of MLOC's address.  */

static inline bool
vt_canon_true_dep (dataflow_set *set, rtx mloc, rtx maddr, rtx loc)
{
  if (GET_CODE (loc) != MEM)
    return false;

  rtx addr = vt_canonicalize_addr (set, XEXP (loc, 0));
  if (!canon_true_dependence (mloc, GET_MODE (mloc), maddr, loc, addr))
    return false;

  return true;
}

/* Hold parameters for the hashtab traversal function
   drop_overlapping_mem_locs, see below.  */

struct overlapping_mems
{
  dataflow_set *set;
  rtx loc, addr;
};

/* Remove all MEMs that overlap with COMS->LOC from the location list
   of a hash table entry for a value.  COMS->ADDR must be a
   canonicalized form of COMS->LOC's address, and COMS->LOC must be
   canonicalized itself.  */

int
drop_overlapping_mem_locs (variable_def **slot, overlapping_mems *coms)
{
  dataflow_set *set = coms->set;
  rtx mloc = coms->loc, addr = coms->addr;
  variable var = *slot;

  if (var->onepart == ONEPART_VALUE)
    {
      location_chain loc, *locp;
      bool changed = false;
      rtx cur_loc;

      gcc_assert (var->n_var_parts == 1);

      if (shared_var_p (var, set->vars))
	{
	  for (loc = var->var_part[0].loc_chain; loc; loc = loc->next)
	    if (vt_canon_true_dep (set, mloc, addr, loc->loc))
	      break;

	  if (!loc)
	    return 1;

	  slot = unshare_variable (set, slot, var, VAR_INIT_STATUS_UNKNOWN);
	  var = *slot;
	  gcc_assert (var->n_var_parts == 1);
	}

      if (VAR_LOC_1PAUX (var))
	cur_loc = VAR_LOC_FROM (var);
      else
	cur_loc = var->var_part[0].cur_loc;

      for (locp = &var->var_part[0].loc_chain, loc = *locp;
	   loc; loc = *locp)
	{
	  if (!vt_canon_true_dep (set, mloc, addr, loc->loc))
	    {
	      locp = &loc->next;
	      continue;
	    }

	  *locp = loc->next;
	  /* If we have deleted the location which was last emitted
	     we have to emit new location so add the variable to set
	     of changed variables.  */
	  if (cur_loc == loc->loc)
	    {
	      changed = true;
	      var->var_part[0].cur_loc = NULL;
	      if (VAR_LOC_1PAUX (var))
		VAR_LOC_FROM (var) = NULL;
	    }
	  pool_free (loc_chain_pool, loc);
	}

      if (!var->var_part[0].loc_chain)
	{
	  var->n_var_parts--;
	  changed = true;
	}
      if (changed)
	variable_was_changed (var, set);
    }

  return 1;
}

/* Remove from SET all VALUE bindings to MEMs that overlap with LOC.  */

static void
clobber_overlapping_mems (dataflow_set *set, rtx loc)
{
  struct overlapping_mems coms;

  gcc_checking_assert (GET_CODE (loc) == MEM);

  coms.set = set;
  coms.loc = canon_rtx (loc);
  coms.addr = vt_canonicalize_addr (set, XEXP (loc, 0));

  set->traversed_vars = set->vars;
  shared_hash_htab (set->vars)
    .traverse <overlapping_mems*, drop_overlapping_mem_locs> (&coms);
  set->traversed_vars = NULL;
}

/* Set the location of DV, OFFSET as the MEM LOC.  */

static void
var_mem_decl_set (dataflow_set *set, rtx loc, enum var_init_status initialized,
		  decl_or_value dv, HOST_WIDE_INT offset, rtx set_src,
		  enum insert_option iopt)
{
  if (dv_is_decl_p (dv))
    dv = dv_from_decl (var_debug_decl (dv_as_decl (dv)));

  set_variable_part (set, loc, dv, offset, initialized, set_src, iopt);
}

/* Set the location part of variable MEM_EXPR (LOC) in dataflow set
   SET to LOC.
   Adjust the address first if it is stack pointer based.  */

static void
var_mem_set (dataflow_set *set, rtx loc, enum var_init_status initialized,
	     rtx set_src)
{
  tree decl = MEM_EXPR (loc);
  HOST_WIDE_INT offset = INT_MEM_OFFSET (loc);

  var_mem_decl_set (set, loc, initialized,
		    dv_from_decl (decl), offset, set_src, INSERT);
}

/* Delete and set the location part of variable MEM_EXPR (LOC) in
   dataflow set SET to LOC.  If MODIFY is true, any other live copies
   of the same variable part are also deleted from the dataflow set,
   otherwise the variable part is assumed to be copied from another
   location holding the same part.
   Adjust the address first if it is stack pointer based.  */

static void
var_mem_delete_and_set (dataflow_set *set, rtx loc, bool modify,
			enum var_init_status initialized, rtx set_src)
{
  tree decl = MEM_EXPR (loc);
  HOST_WIDE_INT offset = INT_MEM_OFFSET (loc);

  clobber_overlapping_mems (set, loc);
  decl = var_debug_decl (decl);

  if (initialized == VAR_INIT_STATUS_UNKNOWN)
    initialized = get_init_value (set, loc, dv_from_decl (decl));

  if (modify)
    clobber_variable_part (set, NULL, dv_from_decl (decl), offset, set_src);
  var_mem_set (set, loc, initialized, set_src);
}

/* Delete the location part LOC from dataflow set SET.  If CLOBBER is
   true, also delete any other live copies of the same variable part.
   Adjust the address first if it is stack pointer based.  */

static void
var_mem_delete (dataflow_set *set, rtx loc, bool clobber)
{
  tree decl = MEM_EXPR (loc);
  HOST_WIDE_INT offset = INT_MEM_OFFSET (loc);

  clobber_overlapping_mems (set, loc);
  decl = var_debug_decl (decl);
  if (clobber)
    clobber_variable_part (set, NULL, dv_from_decl (decl), offset, NULL);
  delete_variable_part (set, loc, dv_from_decl (decl), offset);
}

/* Return true if LOC should not be expanded for location expressions,
   or used in them.  */

static inline bool
unsuitable_loc (rtx loc)
{
  switch (GET_CODE (loc))
    {
    case PC:
    case SCRATCH:
    case CC0:
    case ASM_INPUT:
    case ASM_OPERANDS:
      return true;

    default:
      return false;
    }
}

/* Bind VAL to LOC in SET.  If MODIFIED, detach LOC from any values
   bound to it.  */

static inline void
val_bind (dataflow_set *set, rtx val, rtx loc, bool modified)
{
  if (REG_P (loc))
    {
      if (modified)
	var_regno_delete (set, REGNO (loc));
      var_reg_decl_set (set, loc, VAR_INIT_STATUS_INITIALIZED,
			dv_from_value (val), 0, NULL_RTX, INSERT);
    }
  else if (MEM_P (loc))
    {
      struct elt_loc_list *l = CSELIB_VAL_PTR (val)->locs;

      if (modified)
	clobber_overlapping_mems (set, loc);

      if (l && GET_CODE (l->loc) == VALUE)
	l = canonical_cselib_val (CSELIB_VAL_PTR (l->loc))->locs;

      /* If this MEM is a global constant, we don't need it in the
	 dynamic tables.  ??? We should test this before emitting the
	 micro-op in the first place.  */
      while (l)
	if (GET_CODE (l->loc) == MEM && XEXP (l->loc, 0) == XEXP (loc, 0))
	  break;
	else
	  l = l->next;

      if (!l)
	var_mem_decl_set (set, loc, VAR_INIT_STATUS_INITIALIZED,
			  dv_from_value (val), 0, NULL_RTX, INSERT);
    }
  else
    {
      /* Other kinds of equivalences are necessarily static, at least
	 so long as we do not perform substitutions while merging
	 expressions.  */
      gcc_unreachable ();
      set_variable_part (set, loc, dv_from_value (val), 0,
			 VAR_INIT_STATUS_INITIALIZED, NULL_RTX, INSERT);
    }
}

/* Bind a value to a location it was just stored in.  If MODIFIED
   holds, assume the location was modified, detaching it from any
   values bound to it.  */

static void
val_store (dataflow_set *set, rtx val, rtx loc, rtx insn, bool modified)
{
  cselib_val *v = CSELIB_VAL_PTR (val);

  gcc_assert (cselib_preserved_value_p (v));

  if (dump_file)
    {
      fprintf (dump_file, "%i: ", insn ? INSN_UID (insn) : 0);
      print_inline_rtx (dump_file, loc, 0);
      fprintf (dump_file, " evaluates to ");
      print_inline_rtx (dump_file, val, 0);
      if (v->locs)
	{
	  struct elt_loc_list *l;
	  for (l = v->locs; l; l = l->next)
	    {
	      fprintf (dump_file, "\n%i: ", INSN_UID (l->setting_insn));
	      print_inline_rtx (dump_file, l->loc, 0);
	    }
	}
      fprintf (dump_file, "\n");
    }

  gcc_checking_assert (!unsuitable_loc (loc));

  val_bind (set, val, loc, modified);
}

/* Clear (canonical address) slots that reference X.  */

static bool
local_get_addr_clear_given_value (const void *v ATTRIBUTE_UNUSED,
				  void **slot, void *x)
{
  if (vt_get_canonicalize_base ((rtx)*slot) == x)
    *slot = NULL;
  return true;
}

/* Reset this node, detaching all its equivalences.  Return the slot
   in the variable hash table that holds dv, if there is one.  */

static void
val_reset (dataflow_set *set, decl_or_value dv)
{
  variable var = shared_hash_find (set->vars, dv) ;
  location_chain node;
  rtx cval;

  if (!var || !var->n_var_parts)
    return;

  gcc_assert (var->n_var_parts == 1);

  if (var->onepart == ONEPART_VALUE)
    {
      rtx x = dv_as_value (dv);
      void **slot;
      
      /* Relationships in the global cache don't change, so reset the
	 local cache entry only.  */
      slot = pointer_map_contains (local_get_addr_cache, x);
      if (slot)
	{
	  /* If the value resolved back to itself, odds are that other
	     values may have cached it too.  These entries now refer
	     to the old X, so detach them too.  Entries that used the
	     old X but resolved to something else remain ok as long as
	     that something else isn't also reset.  */
	  if (*slot == x)
	    pointer_map_traverse (local_get_addr_cache,
				  local_get_addr_clear_given_value, x);
	  *slot = NULL;
	}
    }

  cval = NULL;
  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (GET_CODE (node->loc) == VALUE
	&& canon_value_cmp (node->loc, cval))
      cval = node->loc;

  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (GET_CODE (node->loc) == VALUE && cval != node->loc)
      {
	/* Redirect the equivalence link to the new canonical
	   value, or simply remove it if it would point at
	   itself.  */
	if (cval)
	  set_variable_part (set, cval, dv_from_value (node->loc),
			     0, node->init, node->set_src, NO_INSERT);
	delete_variable_part (set, dv_as_value (dv),
			      dv_from_value (node->loc), 0);
      }

  if (cval)
    {
      decl_or_value cdv = dv_from_value (cval);

      /* Keep the remaining values connected, accummulating links
	 in the canonical value.  */
      for (node = var->var_part[0].loc_chain; node; node = node->next)
	{
	  if (node->loc == cval)
	    continue;
	  else if (GET_CODE (node->loc) == REG)
	    var_reg_decl_set (set, node->loc, node->init, cdv, 0,
			      node->set_src, NO_INSERT);
	  else if (GET_CODE (node->loc) == MEM)
	    var_mem_decl_set (set, node->loc, node->init, cdv, 0,
			      node->set_src, NO_INSERT);
	  else
	    set_variable_part (set, node->loc, cdv, 0,
			       node->init, node->set_src, NO_INSERT);
	}
    }

  /* We remove this last, to make sure that the canonical value is not
     removed to the point of requiring reinsertion.  */
  if (cval)
    delete_variable_part (set, dv_as_value (dv), dv_from_value (cval), 0);

  clobber_variable_part (set, NULL, dv, 0, NULL);
}

/* Find the values in a given location and map the val to another
   value, if it is unique, or add the location as one holding the
   value.  */

static void
val_resolve (dataflow_set *set, rtx val, rtx loc, rtx insn)
{
  decl_or_value dv = dv_from_value (val);

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      if (insn)
	fprintf (dump_file, "%i: ", INSN_UID (insn));
      else
	fprintf (dump_file, "head: ");
      print_inline_rtx (dump_file, val, 0);
      fputs (" is at ", dump_file);
      print_inline_rtx (dump_file, loc, 0);
      fputc ('\n', dump_file);
    }

  val_reset (set, dv);

  gcc_checking_assert (!unsuitable_loc (loc));

  if (REG_P (loc))
    {
      attrs node, found = NULL;

      for (node = set->regs[REGNO (loc)]; node; node = node->next)
	if (dv_is_value_p (node->dv)
	    && GET_MODE (dv_as_value (node->dv)) == GET_MODE (loc))
	  {
	    found = node;

	    /* Map incoming equivalences.  ??? Wouldn't it be nice if
	     we just started sharing the location lists?  Maybe a
	     circular list ending at the value itself or some
	     such.  */
	    set_variable_part (set, dv_as_value (node->dv),
			       dv_from_value (val), node->offset,
			       VAR_INIT_STATUS_INITIALIZED, NULL_RTX, INSERT);
	    set_variable_part (set, val, node->dv, node->offset,
			       VAR_INIT_STATUS_INITIALIZED, NULL_RTX, INSERT);
	  }

      /* If we didn't find any equivalence, we need to remember that
	 this value is held in the named register.  */
      if (found)
	return;
    }
  /* ??? Attempt to find and merge equivalent MEMs or other
     expressions too.  */

  val_bind (set, val, loc, false);
}

/* Initialize dataflow set SET to be empty.
   VARS_SIZE is the initial size of hash table VARS.  */

static void
dataflow_set_init (dataflow_set *set)
{
  init_attrs_list_set (set->regs);
  set->vars = shared_hash_copy (empty_shared_hash);
  set->stack_adjust = 0;
  set->traversed_vars = NULL;
}

/* Delete the contents of dataflow set SET.  */

static void
dataflow_set_clear (dataflow_set *set)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    attrs_list_clear (&set->regs[i]);

  shared_hash_destroy (set->vars);
  set->vars = shared_hash_copy (empty_shared_hash);
}

/* Copy the contents of dataflow set SRC to DST.  */

static void
dataflow_set_copy (dataflow_set *dst, dataflow_set *src)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    attrs_list_copy (&dst->regs[i], src->regs[i]);

  shared_hash_destroy (dst->vars);
  dst->vars = shared_hash_copy (src->vars);
  dst->stack_adjust = src->stack_adjust;
}

/* Information for merging lists of locations for a given offset of variable.
 */
struct variable_union_info
{
  /* Node of the location chain.  */
  location_chain lc;

  /* The sum of positions in the input chains.  */
  int pos;

  /* The position in the chain of DST dataflow set.  */
  int pos_dst;
};

/* Buffer for location list sorting and its allocated size.  */
static struct variable_union_info *vui_vec;
static int vui_allocated;

/* Compare function for qsort, order the structures by POS element.  */

static int
variable_union_info_cmp_pos (const void *n1, const void *n2)
{
  const struct variable_union_info *const i1 =
    (const struct variable_union_info *) n1;
  const struct variable_union_info *const i2 =
    ( const struct variable_union_info *) n2;

  if (i1->pos != i2->pos)
    return i1->pos - i2->pos;

  return (i1->pos_dst - i2->pos_dst);
}

/* Compute union of location parts of variable *SLOT and the same variable
   from hash table DATA.  Compute "sorted" union of the location chains
   for common offsets, i.e. the locations of a variable part are sorted by
   a priority where the priority is the sum of the positions in the 2 chains
   (if a location is only in one list the position in the second list is
   defined to be larger than the length of the chains).
   When we are updating the location parts the newest location is in the
   beginning of the chain, so when we do the described "sorted" union
   we keep the newest locations in the beginning.  */

static int
variable_union (variable src, dataflow_set *set)
{
  variable dst;
  variable_def **dstp;
  int i, j, k;

  dstp = shared_hash_find_slot (set->vars, src->dv);
  if (!dstp || !*dstp)
    {
      src->refcount++;

      dst_can_be_shared = false;
      if (!dstp)
	dstp = shared_hash_find_slot_unshare (&set->vars, src->dv, INSERT);

      *dstp = src;

      /* Continue traversing the hash table.  */
      return 1;
    }
  else
    dst = *dstp;

  gcc_assert (src->n_var_parts);
  gcc_checking_assert (src->onepart == dst->onepart);

  /* We can combine one-part variables very efficiently, because their
     entries are in canonical order.  */
  if (src->onepart)
    {
      location_chain *nodep, dnode, snode;

      gcc_assert (src->n_var_parts == 1
		  && dst->n_var_parts == 1);

      snode = src->var_part[0].loc_chain;
      gcc_assert (snode);

    restart_onepart_unshared:
      nodep = &dst->var_part[0].loc_chain;
      dnode = *nodep;
      gcc_assert (dnode);

      while (snode)
	{
	  int r = dnode ? loc_cmp (dnode->loc, snode->loc) : 1;

	  if (r > 0)
	    {
	      location_chain nnode;

	      if (shared_var_p (dst, set->vars))
		{
		  dstp = unshare_variable (set, dstp, dst,
					   VAR_INIT_STATUS_INITIALIZED);
		  dst = *dstp;
		  goto restart_onepart_unshared;
		}

	      *nodep = nnode = (location_chain) pool_alloc (loc_chain_pool);
	      nnode->loc = snode->loc;
	      nnode->init = snode->init;
	      if (!snode->set_src || MEM_P (snode->set_src))
		nnode->set_src = NULL;
	      else
		nnode->set_src = snode->set_src;
	      nnode->next = dnode;
	      dnode = nnode;
	    }
	  else if (r == 0)
	    gcc_checking_assert (rtx_equal_p (dnode->loc, snode->loc));

	  if (r >= 0)
	    snode = snode->next;

	  nodep = &dnode->next;
	  dnode = *nodep;
	}

      return 1;
    }

  gcc_checking_assert (!src->onepart);

  /* Count the number of location parts, result is K.  */
  for (i = 0, j = 0, k = 0;
       i < src->n_var_parts && j < dst->n_var_parts; k++)
    {
      if (VAR_PART_OFFSET (src, i) == VAR_PART_OFFSET (dst, j))
	{
	  i++;
	  j++;
	}
      else if (VAR_PART_OFFSET (src, i) < VAR_PART_OFFSET (dst, j))
	i++;
      else
	j++;
    }
  k += src->n_var_parts - i;
  k += dst->n_var_parts - j;

  /* We track only variables whose size is <= MAX_VAR_PARTS bytes
     thus there are at most MAX_VAR_PARTS different offsets.  */
  gcc_checking_assert (dst->onepart ? k == 1 : k <= MAX_VAR_PARTS);

  if (dst->n_var_parts != k && shared_var_p (dst, set->vars))
    {
      dstp = unshare_variable (set, dstp, dst, VAR_INIT_STATUS_UNKNOWN);
      dst = *dstp;
    }

  i = src->n_var_parts - 1;
  j = dst->n_var_parts - 1;
  dst->n_var_parts = k;

  for (k--; k >= 0; k--)
    {
      location_chain node, node2;

      if (i >= 0 && j >= 0
	  && VAR_PART_OFFSET (src, i) == VAR_PART_OFFSET (dst, j))
	{
	  /* Compute the "sorted" union of the chains, i.e. the locations which
	     are in both chains go first, they are sorted by the sum of
	     positions in the chains.  */
	  int dst_l, src_l;
	  int ii, jj, n;
	  struct variable_union_info *vui;

	  /* If DST is shared compare the location chains.
	     If they are different we will modify the chain in DST with
	     high probability so make a copy of DST.  */
	  if (shared_var_p (dst, set->vars))
	    {
	      for (node = src->var_part[i].loc_chain,
		   node2 = dst->var_part[j].loc_chain; node && node2;
		   node = node->next, node2 = node2->next)
		{
		  if (!((REG_P (node2->loc)
			 && REG_P (node->loc)
			 && REGNO (node2->loc) == REGNO (node->loc))
			|| rtx_equal_p (node2->loc, node->loc)))
		    {
		      if (node2->init < node->init)
		        node2->init = node->init;
		      break;
		    }
		}
	      if (node || node2)
		{
		  dstp = unshare_variable (set, dstp, dst,
					   VAR_INIT_STATUS_UNKNOWN);
		  dst = (variable)*dstp;
		}
	    }

	  src_l = 0;
	  for (node = src->var_part[i].loc_chain; node; node = node->next)
	    src_l++;
	  dst_l = 0;
	  for (node = dst->var_part[j].loc_chain; node; node = node->next)
	    dst_l++;

	  if (dst_l == 1)
	    {
	      /* The most common case, much simpler, no qsort is needed.  */
	      location_chain dstnode = dst->var_part[j].loc_chain;
	      dst->var_part[k].loc_chain = dstnode;
	      VAR_PART_OFFSET (dst, k) = VAR_PART_OFFSET (dst, j);
	      node2 = dstnode;
	      for (node = src->var_part[i].loc_chain; node; node = node->next)
		if (!((REG_P (dstnode->loc)
		       && REG_P (node->loc)
		       && REGNO (dstnode->loc) == REGNO (node->loc))
		      || rtx_equal_p (dstnode->loc, node->loc)))
		  {
		    location_chain new_node;

		    /* Copy the location from SRC.  */
		    new_node = (location_chain) pool_alloc (loc_chain_pool);
		    new_node->loc = node->loc;
		    new_node->init = node->init;
		    if (!node->set_src || MEM_P (node->set_src))
		      new_node->set_src = NULL;
		    else
		      new_node->set_src = node->set_src;
		    node2->next = new_node;
		    node2 = new_node;
		  }
	      node2->next = NULL;
	    }
	  else
	    {
	      if (src_l + dst_l > vui_allocated)
		{
		  vui_allocated = MAX (vui_allocated * 2, src_l + dst_l);
		  vui_vec = XRESIZEVEC (struct variable_union_info, vui_vec,
					vui_allocated);
		}
	      vui = vui_vec;

	      /* Fill in the locations from DST.  */
	      for (node = dst->var_part[j].loc_chain, jj = 0; node;
		   node = node->next, jj++)
		{
		  vui[jj].lc = node;
		  vui[jj].pos_dst = jj;

		  /* Pos plus value larger than a sum of 2 valid positions.  */
		  vui[jj].pos = jj + src_l + dst_l;
		}

	      /* Fill in the locations from SRC.  */
	      n = dst_l;
	      for (node = src->var_part[i].loc_chain, ii = 0; node;
		   node = node->next, ii++)
		{
		  /* Find location from NODE.  */
		  for (jj = 0; jj < dst_l; jj++)
		    {
		      if ((REG_P (vui[jj].lc->loc)
			   && REG_P (node->loc)
			   && REGNO (vui[jj].lc->loc) == REGNO (node->loc))
			  || rtx_equal_p (vui[jj].lc->loc, node->loc))
			{
			  vui[jj].pos = jj + ii;
			  break;
			}
		    }
		  if (jj >= dst_l)	/* The location has not been found.  */
		    {
		      location_chain new_node;

		      /* Copy the location from SRC.  */
		      new_node = (location_chain) pool_alloc (loc_chain_pool);
		      new_node->loc = node->loc;
		      new_node->init = node->init;
		      if (!node->set_src || MEM_P (node->set_src))
			new_node->set_src = NULL;
		      else
			new_node->set_src = node->set_src;
		      vui[n].lc = new_node;
		      vui[n].pos_dst = src_l + dst_l;
		      vui[n].pos = ii + src_l + dst_l;
		      n++;
		    }
		}

	      if (dst_l == 2)
		{
		  /* Special case still very common case.  For dst_l == 2
		     all entries dst_l ... n-1 are sorted, with for i >= dst_l
		     vui[i].pos == i + src_l + dst_l.  */
		  if (vui[0].pos > vui[1].pos)
		    {
		      /* Order should be 1, 0, 2... */
		      dst->var_part[k].loc_chain = vui[1].lc;
		      vui[1].lc->next = vui[0].lc;
		      if (n >= 3)
			{
			  vui[0].lc->next = vui[2].lc;
			  vui[n - 1].lc->next = NULL;
			}
		      else
			vui[0].lc->next = NULL;
		      ii = 3;
		    }
		  else
		    {
		      dst->var_part[k].loc_chain = vui[0].lc;
		      if (n >= 3 && vui[2].pos < vui[1].pos)
			{
			  /* Order should be 0, 2, 1, 3... */
			  vui[0].lc->next = vui[2].lc;
			  vui[2].lc->next = vui[1].lc;
			  if (n >= 4)
			    {
			      vui[1].lc->next = vui[3].lc;
			      vui[n - 1].lc->next = NULL;
			    }
			  else
			    vui[1].lc->next = NULL;
			  ii = 4;
			}
		      else
			{
			  /* Order should be 0, 1, 2... */
			  ii = 1;
			  vui[n - 1].lc->next = NULL;
			}
		    }
		  for (; ii < n; ii++)
		    vui[ii - 1].lc->next = vui[ii].lc;
		}
	      else
		{
		  qsort (vui, n, sizeof (struct variable_union_info),
			 variable_union_info_cmp_pos);

		  /* Reconnect the nodes in sorted order.  */
		  for (ii = 1; ii < n; ii++)
		    vui[ii - 1].lc->next = vui[ii].lc;
		  vui[n - 1].lc->next = NULL;
		  dst->var_part[k].loc_chain = vui[0].lc;
		}

	      VAR_PART_OFFSET (dst, k) = VAR_PART_OFFSET (dst, j);
	    }
	  i--;
	  j--;
	}
      else if ((i >= 0 && j >= 0
		&& VAR_PART_OFFSET (src, i) < VAR_PART_OFFSET (dst, j))
	       || i < 0)
	{
	  dst->var_part[k] = dst->var_part[j];
	  j--;
	}
      else if ((i >= 0 && j >= 0
		&& VAR_PART_OFFSET (src, i) > VAR_PART_OFFSET (dst, j))
	       || j < 0)
	{
	  location_chain *nextp;

	  /* Copy the chain from SRC.  */
	  nextp = &dst->var_part[k].loc_chain;
	  for (node = src->var_part[i].loc_chain; node; node = node->next)
	    {
	      location_chain new_lc;

	      new_lc = (location_chain) pool_alloc (loc_chain_pool);
	      new_lc->next = NULL;
	      new_lc->init = node->init;
	      if (!node->set_src || MEM_P (node->set_src))
		new_lc->set_src = NULL;
	      else
		new_lc->set_src = node->set_src;
	      new_lc->loc = node->loc;

	      *nextp = new_lc;
	      nextp = &new_lc->next;
	    }

	  VAR_PART_OFFSET (dst, k) = VAR_PART_OFFSET (src, i);
	  i--;
	}
      dst->var_part[k].cur_loc = NULL;
    }

  if (flag_var_tracking_uninit)
    for (i = 0; i < src->n_var_parts && i < dst->n_var_parts; i++)
      {
	location_chain node, node2;
	for (node = src->var_part[i].loc_chain; node; node = node->next)
	  for (node2 = dst->var_part[i].loc_chain; node2; node2 = node2->next)
	    if (rtx_equal_p (node->loc, node2->loc))
	      {
		if (node->init > node2->init)
		  node2->init = node->init;
	      }
      }

  /* Continue traversing the hash table.  */
  return 1;
}

/* Compute union of dataflow sets SRC and DST and store it to DST.  */

static void
dataflow_set_union (dataflow_set *dst, dataflow_set *src)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    attrs_list_union (&dst->regs[i], src->regs[i]);

  if (dst->vars == empty_shared_hash)
    {
      shared_hash_destroy (dst->vars);
      dst->vars = shared_hash_copy (src->vars);
    }
  else
    {
      variable_iterator_type hi;
      variable var;

      FOR_EACH_HASH_TABLE_ELEMENT (shared_hash_htab (src->vars),
				   var, variable, hi)
	variable_union (var, dst);
    }
}

/* Whether the value is currently being expanded.  */
#define VALUE_RECURSED_INTO(x) \
  (RTL_FLAG_CHECK2 ("VALUE_RECURSED_INTO", (x), VALUE, DEBUG_EXPR)->used)

/* Whether no expansion was found, saving useless lookups.
   It must only be set when VALUE_CHANGED is clear.  */
#define NO_LOC_P(x) \
  (RTL_FLAG_CHECK2 ("NO_LOC_P", (x), VALUE, DEBUG_EXPR)->return_val)

/* Whether cur_loc in the value needs to be (re)computed.  */
#define VALUE_CHANGED(x) \
  (RTL_FLAG_CHECK1 ("VALUE_CHANGED", (x), VALUE)->frame_related)
/* Whether cur_loc in the decl needs to be (re)computed.  */
#define DECL_CHANGED(x) TREE_VISITED (x)

/* Record (if NEWV) that DV needs to have its cur_loc recomputed.  For
   user DECLs, this means they're in changed_variables.  Values and
   debug exprs may be left with this flag set if no user variable
   requires them to be evaluated.  */

static inline void
set_dv_changed (decl_or_value dv, bool newv)
{
  switch (dv_onepart_p (dv))
    {
    case ONEPART_VALUE:
      if (newv)
	NO_LOC_P (dv_as_value (dv)) = false;
      VALUE_CHANGED (dv_as_value (dv)) = newv;
      break;

    case ONEPART_DEXPR:
      if (newv)
	NO_LOC_P (DECL_RTL_KNOWN_SET (dv_as_decl (dv))) = false;
      /* Fall through...  */

    default:
      DECL_CHANGED (dv_as_decl (dv)) = newv;
      break;
    }
}

/* Return true if DV needs to have its cur_loc recomputed.  */

static inline bool
dv_changed_p (decl_or_value dv)
{
  return (dv_is_value_p (dv)
	  ? VALUE_CHANGED (dv_as_value (dv))
	  : DECL_CHANGED (dv_as_decl (dv)));
}

/* Return a location list node whose loc is rtx_equal to LOC, in the
   location list of a one-part variable or value VAR, or in that of
   any values recursively mentioned in the location lists.  VARS must
   be in star-canonical form.  */

static location_chain
find_loc_in_1pdv (rtx loc, variable var, variable_table_type vars)
{
  location_chain node;
  enum rtx_code loc_code;

  if (!var)
    return NULL;

  gcc_checking_assert (var->onepart);

  if (!var->n_var_parts)
    return NULL;

  gcc_checking_assert (loc != dv_as_opaque (var->dv));

  loc_code = GET_CODE (loc);
  for (node = var->var_part[0].loc_chain; node; node = node->next)
    {
      decl_or_value dv;
      variable rvar;

      if (GET_CODE (node->loc) != loc_code)
	{
	  if (GET_CODE (node->loc) != VALUE)
	    continue;
	}
      else if (loc == node->loc)
	return node;
      else if (loc_code != VALUE)
	{
	  if (rtx_equal_p (loc, node->loc))
	    return node;
	  continue;
	}

      /* Since we're in star-canonical form, we don't need to visit
	 non-canonical nodes: one-part variables and non-canonical
	 values would only point back to the canonical node.  */
      if (dv_is_value_p (var->dv)
	  && !canon_value_cmp (node->loc, dv_as_value (var->dv)))
	{
	  /* Skip all subsequent VALUEs.  */
	  while (node->next && GET_CODE (node->next->loc) == VALUE)
	    {
	      node = node->next;
	      gcc_checking_assert (!canon_value_cmp (node->loc,
						     dv_as_value (var->dv)));
	      if (loc == node->loc)
		return node;
	    }
	  continue;
	}

      gcc_checking_assert (node == var->var_part[0].loc_chain);
      gcc_checking_assert (!node->next);

      dv = dv_from_value (node->loc);
      rvar = vars.find_with_hash (dv, dv_htab_hash (dv));
      return find_loc_in_1pdv (loc, rvar, vars);
    }

  /* ??? Gotta look in cselib_val locations too.  */

  return NULL;
}

/* Hash table iteration argument passed to variable_merge.  */
struct dfset_merge
{
  /* The set in which the merge is to be inserted.  */
  dataflow_set *dst;
  /* The set that we're iterating in.  */
  dataflow_set *cur;
  /* The set that may contain the other dv we are to merge with.  */
  dataflow_set *src;
  /* Number of onepart dvs in src.  */
  int src_onepart_cnt;
};

/* Insert LOC in *DNODE, if it's not there yet.  The list must be in
   loc_cmp order, and it is maintained as such.  */

static void
insert_into_intersection (location_chain *nodep, rtx loc,
			  enum var_init_status status)
{
  location_chain node;
  int r;

  for (node = *nodep; node; nodep = &node->next, node = *nodep)
    if ((r = loc_cmp (node->loc, loc)) == 0)
      {
	node->init = MIN (node->init, status);
	return;
      }
    else if (r > 0)
      break;

  node = (location_chain) pool_alloc (loc_chain_pool);

  node->loc = loc;
  node->set_src = NULL;
  node->init = status;
  node->next = *nodep;
  *nodep = node;
}

/* Insert in DEST the intersection of the locations present in both
   S1NODE and S2VAR, directly or indirectly.  S1NODE is from a
   variable in DSM->cur, whereas S2VAR is from DSM->src.  dvar is in
   DSM->dst.  */

static void
intersect_loc_chains (rtx val, location_chain *dest, struct dfset_merge *dsm,
		      location_chain s1node, variable s2var)
{
  dataflow_set *s1set = dsm->cur;
  dataflow_set *s2set = dsm->src;
  location_chain found;

  if (s2var)
    {
      location_chain s2node;

      gcc_checking_assert (s2var->onepart);

      if (s2var->n_var_parts)
	{
	  s2node = s2var->var_part[0].loc_chain;

	  for (; s1node && s2node;
	       s1node = s1node->next, s2node = s2node->next)
	    if (s1node->loc != s2node->loc)
	      break;
	    else if (s1node->loc == val)
	      continue;
	    else
	      insert_into_intersection (dest, s1node->loc,
					MIN (s1node->init, s2node->init));
	}
    }

  for (; s1node; s1node = s1node->next)
    {
      if (s1node->loc == val)
	continue;

      if ((found = find_loc_in_1pdv (s1node->loc, s2var,
				     shared_hash_htab (s2set->vars))))
	{
	  insert_into_intersection (dest, s1node->loc,
				    MIN (s1node->init, found->init));
	  continue;
	}

      if (GET_CODE (s1node->loc) == VALUE
	  && !VALUE_RECURSED_INTO (s1node->loc))
	{
	  decl_or_value dv = dv_from_value (s1node->loc);
	  variable svar = shared_hash_find (s1set->vars, dv);
	  if (svar)
	    {
	      if (svar->n_var_parts == 1)
		{
		  VALUE_RECURSED_INTO (s1node->loc) = true;
		  intersect_loc_chains (val, dest, dsm,
					svar->var_part[0].loc_chain,
					s2var);
		  VALUE_RECURSED_INTO (s1node->loc) = false;
		}
	    }
	}

      /* ??? gotta look in cselib_val locations too.  */

      /* ??? if the location is equivalent to any location in src,
	 searched recursively

	   add to dst the values needed to represent the equivalence

     telling whether locations S is equivalent to another dv's
     location list:

       for each location D in the list

         if S and D satisfy rtx_equal_p, then it is present

	 else if D is a value, recurse without cycles

	 else if S and D have the same CODE and MODE

	   for each operand oS and the corresponding oD

	     if oS and oD are not equivalent, then S an D are not equivalent

	     else if they are RTX vectors

	       if any vector oS element is not equivalent to its respective oD,
	       then S and D are not equivalent

   */


    }
}

/* Return -1 if X should be before Y in a location list for a 1-part
   variable, 1 if Y should be before X, and 0 if they're equivalent
   and should not appear in the list.  */

static int
loc_cmp (rtx x, rtx y)
{
  int i, j, r;
  RTX_CODE code = GET_CODE (x);
  const char *fmt;

  if (x == y)
    return 0;

  if (REG_P (x))
    {
      if (!REG_P (y))
	return -1;
      gcc_assert (GET_MODE (x) == GET_MODE (y));
      if (REGNO (x) == REGNO (y))
	return 0;
      else if (REGNO (x) < REGNO (y))
	return -1;
      else
	return 1;
    }

  if (REG_P (y))
    return 1;

  if (MEM_P (x))
    {
      if (!MEM_P (y))
	return -1;
      gcc_assert (GET_MODE (x) == GET_MODE (y));
      return loc_cmp (XEXP (x, 0), XEXP (y, 0));
    }

  if (MEM_P (y))
    return 1;

  if (GET_CODE (x) == VALUE)
    {
      if (GET_CODE (y) != VALUE)
	return -1;
      /* Don't assert the modes are the same, that is true only
	 when not recursing.  (subreg:QI (value:SI 1:1) 0)
	 and (subreg:QI (value:DI 2:2) 0) can be compared,
	 even when the modes are different.  */
      if (canon_value_cmp (x, y))
	return -1;
      else
	return 1;
    }

  if (GET_CODE (y) == VALUE)
    return 1;

  /* Entry value is the least preferable kind of expression.  */
  if (GET_CODE (x) == ENTRY_VALUE)
    {
      if (GET_CODE (y) != ENTRY_VALUE)
	return 1;
      gcc_assert (GET_MODE (x) == GET_MODE (y));
      return loc_cmp (ENTRY_VALUE_EXP (x), ENTRY_VALUE_EXP (y));
    }

  if (GET_CODE (y) == ENTRY_VALUE)
    return -1;

  if (GET_CODE (x) == GET_CODE (y))
    /* Compare operands below.  */;
  else if (GET_CODE (x) < GET_CODE (y))
    return -1;
  else
    return 1;

  gcc_assert (GET_MODE (x) == GET_MODE (y));

  if (GET_CODE (x) == DEBUG_EXPR)
    {
      if (DEBUG_TEMP_UID (DEBUG_EXPR_TREE_DECL (x))
	  < DEBUG_TEMP_UID (DEBUG_EXPR_TREE_DECL (y)))
	return -1;
      gcc_checking_assert (DEBUG_TEMP_UID (DEBUG_EXPR_TREE_DECL (x))
			   > DEBUG_TEMP_UID (DEBUG_EXPR_TREE_DECL (y)));
      return 1;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    switch (fmt[i])
      {
      case 'w':
	if (XWINT (x, i) == XWINT (y, i))
	  break;
	else if (XWINT (x, i) < XWINT (y, i))
	  return -1;
	else
	  return 1;

      case 'n':
      case 'i':
	if (XINT (x, i) == XINT (y, i))
	  break;
	else if (XINT (x, i) < XINT (y, i))
	  return -1;
	else
	  return 1;

      case 'V':
      case 'E':
	/* Compare the vector length first.  */
	if (XVECLEN (x, i) == XVECLEN (y, i))
	  /* Compare the vectors elements.  */;
	else if (XVECLEN (x, i) < XVECLEN (y, i))
	  return -1;
	else
	  return 1;

	for (j = 0; j < XVECLEN (x, i); j++)
	  if ((r = loc_cmp (XVECEXP (x, i, j),
			    XVECEXP (y, i, j))))
	    return r;
	break;

      case 'e':
	if ((r = loc_cmp (XEXP (x, i), XEXP (y, i))))
	  return r;
	break;

      case 'S':
      case 's':
	if (XSTR (x, i) == XSTR (y, i))
	  break;
	if (!XSTR (x, i))
	  return -1;
	if (!XSTR (y, i))
	  return 1;
	if ((r = strcmp (XSTR (x, i), XSTR (y, i))) == 0)
	  break;
	else if (r < 0)
	  return -1;
	else
	  return 1;

      case 'u':
	/* These are just backpointers, so they don't matter.  */
	break;

      case '0':
      case 't':
	break;

	/* It is believed that rtx's at this level will never
	   contain anything but integers and other rtx's,
	   except for within LABEL_REFs and SYMBOL_REFs.  */
      default:
	gcc_unreachable ();
      }
  if (CONST_WIDE_INT_P (x))
    {
      /* Compare the vector length first.  */
      if (CONST_WIDE_INT_NUNITS (x) >= CONST_WIDE_INT_NUNITS (y))
	return 1;
      else if (CONST_WIDE_INT_NUNITS (x) < CONST_WIDE_INT_NUNITS (y))
	return -1;

      /* Compare the vectors elements.  */;
      for (j = CONST_WIDE_INT_NUNITS (x) - 1; j >= 0 ; j--)
	{
	  if (CONST_WIDE_INT_ELT (x, j) < CONST_WIDE_INT_ELT (y, j))
	    return -1;
	  if (CONST_WIDE_INT_ELT (x, j) > CONST_WIDE_INT_ELT (y, j))
	    return 1;
	}
    }

  return 0;
}

#if ENABLE_CHECKING
/* Check the order of entries in one-part variables.   */

int
canonicalize_loc_order_check (variable_def **slot,
			      dataflow_set *data ATTRIBUTE_UNUSED)
{
  variable var = *slot;
  location_chain node, next;

#ifdef ENABLE_RTL_CHECKING
  int i;
  for (i = 0; i < var->n_var_parts; i++)
    gcc_assert (var->var_part[0].cur_loc == NULL);
  gcc_assert (!var->in_changed_variables);
#endif

  if (!var->onepart)
    return 1;

  gcc_assert (var->n_var_parts == 1);
  node = var->var_part[0].loc_chain;
  gcc_assert (node);

  while ((next = node->next))
    {
      gcc_assert (loc_cmp (node->loc, next->loc) < 0);
      node = next;
    }

  return 1;
}
#endif

/* Mark with VALUE_RECURSED_INTO values that have neighbors that are
   more likely to be chosen as canonical for an equivalence set.
   Ensure less likely values can reach more likely neighbors, making
   the connections bidirectional.  */

int
canonicalize_values_mark (variable_def **slot, dataflow_set *set)
{
  variable var = *slot;
  decl_or_value dv = var->dv;
  rtx val;
  location_chain node;

  if (!dv_is_value_p (dv))
    return 1;

  gcc_checking_assert (var->n_var_parts == 1);

  val = dv_as_value (dv);

  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (GET_CODE (node->loc) == VALUE)
      {
	if (canon_value_cmp (node->loc, val))
	  VALUE_RECURSED_INTO (val) = true;
	else
	  {
	    decl_or_value odv = dv_from_value (node->loc);
	    variable_def **oslot;
	    oslot = shared_hash_find_slot_noinsert (set->vars, odv);

	    set_slot_part (set, val, oslot, odv, 0,
			   node->init, NULL_RTX);

	    VALUE_RECURSED_INTO (node->loc) = true;
	  }
      }

  return 1;
}

/* Remove redundant entries from equivalence lists in onepart
   variables, canonicalizing equivalence sets into star shapes.  */

int
canonicalize_values_star (variable_def **slot, dataflow_set *set)
{
  variable var = *slot;
  decl_or_value dv = var->dv;
  location_chain node;
  decl_or_value cdv;
  rtx val, cval;
  variable_def **cslot;
  bool has_value;
  bool has_marks;

  if (!var->onepart)
    return 1;

  gcc_checking_assert (var->n_var_parts == 1);

  if (dv_is_value_p (dv))
    {
      cval = dv_as_value (dv);
      if (!VALUE_RECURSED_INTO (cval))
	return 1;
      VALUE_RECURSED_INTO (cval) = false;
    }
  else
    cval = NULL_RTX;

 restart:
  val = cval;
  has_value = false;
  has_marks = false;

  gcc_assert (var->n_var_parts == 1);

  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (GET_CODE (node->loc) == VALUE)
      {
	has_value = true;
	if (VALUE_RECURSED_INTO (node->loc))
	  has_marks = true;
	if (canon_value_cmp (node->loc, cval))
	  cval = node->loc;
      }

  if (!has_value)
    return 1;

  if (cval == val)
    {
      if (!has_marks || dv_is_decl_p (dv))
	return 1;

      /* Keep it marked so that we revisit it, either after visiting a
	 child node, or after visiting a new parent that might be
	 found out.  */
      VALUE_RECURSED_INTO (val) = true;

      for (node = var->var_part[0].loc_chain; node; node = node->next)
	if (GET_CODE (node->loc) == VALUE
	    && VALUE_RECURSED_INTO (node->loc))
	  {
	    cval = node->loc;
	  restart_with_cval:
	    VALUE_RECURSED_INTO (cval) = false;
	    dv = dv_from_value (cval);
	    slot = shared_hash_find_slot_noinsert (set->vars, dv);
	    if (!slot)
	      {
		gcc_assert (dv_is_decl_p (var->dv));
		/* The canonical value was reset and dropped.
		   Remove it.  */
		clobber_variable_part (set, NULL, var->dv, 0, NULL);
		return 1;
	      }
	    var = *slot;
	    gcc_assert (dv_is_value_p (var->dv));
	    if (var->n_var_parts == 0)
	      return 1;
	    gcc_assert (var->n_var_parts == 1);
	    goto restart;
	  }

      VALUE_RECURSED_INTO (val) = false;

      return 1;
    }

  /* Push values to the canonical one.  */
  cdv = dv_from_value (cval);
  cslot = shared_hash_find_slot_noinsert (set->vars, cdv);

  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (node->loc != cval)
      {
	cslot = set_slot_part (set, node->loc, cslot, cdv, 0,
			       node->init, NULL_RTX);
	if (GET_CODE (node->loc) == VALUE)
	  {
	    decl_or_value ndv = dv_from_value (node->loc);

	    set_variable_part (set, cval, ndv, 0, node->init, NULL_RTX,
			       NO_INSERT);

	    if (canon_value_cmp (node->loc, val))
	      {
		/* If it could have been a local minimum, it's not any more,
		   since it's now neighbor to cval, so it may have to push
		   to it.  Conversely, if it wouldn't have prevailed over
		   val, then whatever mark it has is fine: if it was to
		   push, it will now push to a more canonical node, but if
		   it wasn't, then it has already pushed any values it might
		   have to.  */
		VALUE_RECURSED_INTO (node->loc) = true;
		/* Make sure we visit node->loc by ensuring we cval is
		   visited too.  */
		VALUE_RECURSED_INTO (cval) = true;
	      }
	    else if (!VALUE_RECURSED_INTO (node->loc))
	      /* If we have no need to "recurse" into this node, it's
		 already "canonicalized", so drop the link to the old
		 parent.  */
	      clobber_variable_part (set, cval, ndv, 0, NULL);
	  }
	else if (GET_CODE (node->loc) == REG)
	  {
	    attrs list = set->regs[REGNO (node->loc)], *listp;

	    /* Change an existing attribute referring to dv so that it
	       refers to cdv, removing any duplicate this might
	       introduce, and checking that no previous duplicates
	       existed, all in a single pass.  */

	    while (list)
	      {
		if (list->offset == 0
		    && (dv_as_opaque (list->dv) == dv_as_opaque (dv)
			|| dv_as_opaque (list->dv) == dv_as_opaque (cdv)))
		  break;

		list = list->next;
	      }

	    gcc_assert (list);
	    if (dv_as_opaque (list->dv) == dv_as_opaque (dv))
	      {
		list->dv = cdv;
		for (listp = &list->next; (list = *listp); listp = &list->next)
		  {
		    if (list->offset)
		      continue;

		    if (dv_as_opaque (list->dv) == dv_as_opaque (cdv))
		      {
			*listp = list->next;
			pool_free (attrs_pool, list);
			list = *listp;
			break;
		      }

		    gcc_assert (dv_as_opaque (list->dv) != dv_as_opaque (dv));
		  }
	      }
	    else if (dv_as_opaque (list->dv) == dv_as_opaque (cdv))
	      {
		for (listp = &list->next; (list = *listp); listp = &list->next)
		  {
		    if (list->offset)
		      continue;

		    if (dv_as_opaque (list->dv) == dv_as_opaque (dv))
		      {
			*listp = list->next;
			pool_free (attrs_pool, list);
			list = *listp;
			break;
		      }

		    gcc_assert (dv_as_opaque (list->dv) != dv_as_opaque (cdv));
		  }
	      }
	    else
	      gcc_unreachable ();

#if ENABLE_CHECKING
	    while (list)
	      {
		if (list->offset == 0
		    && (dv_as_opaque (list->dv) == dv_as_opaque (dv)
			|| dv_as_opaque (list->dv) == dv_as_opaque (cdv)))
		  gcc_unreachable ();

		list = list->next;
	      }
#endif
	  }
      }

  if (val)
    set_slot_part (set, val, cslot, cdv, 0,
		   VAR_INIT_STATUS_INITIALIZED, NULL_RTX);

  slot = clobber_slot_part (set, cval, slot, 0, NULL);

  /* Variable may have been unshared.  */
  var = *slot;
  gcc_checking_assert (var->n_var_parts && var->var_part[0].loc_chain->loc == cval
		       && var->var_part[0].loc_chain->next == NULL);

  if (VALUE_RECURSED_INTO (cval))
    goto restart_with_cval;

  return 1;
}

/* Bind one-part variables to the canonical value in an equivalence
   set.  Not doing this causes dataflow convergence failure in rare
   circumstances, see PR42873.  Unfortunately we can't do this
   efficiently as part of canonicalize_values_star, since we may not
   have determined or even seen the canonical value of a set when we
   get to a variable that references another member of the set.  */

int
canonicalize_vars_star (variable_def **slot, dataflow_set *set)
{
  variable var = *slot;
  decl_or_value dv = var->dv;
  location_chain node;
  rtx cval;
  decl_or_value cdv;
  variable_def **cslot;
  variable cvar;
  location_chain cnode;

  if (!var->onepart || var->onepart == ONEPART_VALUE)
    return 1;

  gcc_assert (var->n_var_parts == 1);

  node = var->var_part[0].loc_chain;

  if (GET_CODE (node->loc) != VALUE)
    return 1;

  gcc_assert (!node->next);
  cval = node->loc;

  /* Push values to the canonical one.  */
  cdv = dv_from_value (cval);
  cslot = shared_hash_find_slot_noinsert (set->vars, cdv);
  if (!cslot)
    return 1;
  cvar = *cslot;
  gcc_assert (cvar->n_var_parts == 1);

  cnode = cvar->var_part[0].loc_chain;

  /* CVAL is canonical if its value list contains non-VALUEs or VALUEs
     that are not “more canonical” than it.  */
  if (GET_CODE (cnode->loc) != VALUE
      || !canon_value_cmp (cnode->loc, cval))
    return 1;

  /* CVAL was found to be non-canonical.  Change the variable to point
     to the canonical VALUE.  */
  gcc_assert (!cnode->next);
  cval = cnode->loc;

  slot = set_slot_part (set, cval, slot, dv, 0,
			node->init, node->set_src);
  clobber_slot_part (set, cval, slot, 0, node->set_src);

  return 1;
}

/* Combine variable or value in *S1SLOT (in DSM->cur) with the
   corresponding entry in DSM->src.  Multi-part variables are combined
   with variable_union, whereas onepart dvs are combined with
   intersection.  */

static int
variable_merge_over_cur (variable s1var, struct dfset_merge *dsm)
{
  dataflow_set *dst = dsm->dst;
  variable_def **dstslot;
  variable s2var, dvar = NULL;
  decl_or_value dv = s1var->dv;
  onepart_enum_t onepart = s1var->onepart;
  rtx val;
  hashval_t dvhash;
  location_chain node, *nodep;

  /* If the incoming onepart variable has an empty location list, then
     the intersection will be just as empty.  For other variables,
     it's always union.  */
  gcc_checking_assert (s1var->n_var_parts
		       && s1var->var_part[0].loc_chain);

  if (!onepart)
    return variable_union (s1var, dst);

  gcc_checking_assert (s1var->n_var_parts == 1);

  dvhash = dv_htab_hash (dv);
  if (dv_is_value_p (dv))
    val = dv_as_value (dv);
  else
    val = NULL;

  s2var = shared_hash_find_1 (dsm->src->vars, dv, dvhash);
  if (!s2var)
    {
      dst_can_be_shared = false;
      return 1;
    }

  dsm->src_onepart_cnt--;
  gcc_assert (s2var->var_part[0].loc_chain
	      && s2var->onepart == onepart
	      && s2var->n_var_parts == 1);

  dstslot = shared_hash_find_slot_noinsert_1 (dst->vars, dv, dvhash);
  if (dstslot)
    {
      dvar = *dstslot;
      gcc_assert (dvar->refcount == 1
		  && dvar->onepart == onepart
		  && dvar->n_var_parts == 1);
      nodep = &dvar->var_part[0].loc_chain;
    }
  else
    {
      nodep = &node;
      node = NULL;
    }

  if (!dstslot && !onepart_variable_different_p (s1var, s2var))
    {
      dstslot = shared_hash_find_slot_unshare_1 (&dst->vars, dv,
						 dvhash, INSERT);
      *dstslot = dvar = s2var;
      dvar->refcount++;
    }
  else
    {
      dst_can_be_shared = false;

      intersect_loc_chains (val, nodep, dsm,
			    s1var->var_part[0].loc_chain, s2var);

      if (!dstslot)
	{
	  if (node)
	    {
	      dvar = (variable) pool_alloc (onepart_pool (onepart));
	      dvar->dv = dv;
	      dvar->refcount = 1;
	      dvar->n_var_parts = 1;
	      dvar->onepart = onepart;
	      dvar->in_changed_variables = false;
	      dvar->var_part[0].loc_chain = node;
	      dvar->var_part[0].cur_loc = NULL;
	      if (onepart)
		VAR_LOC_1PAUX (dvar) = NULL;
	      else
		VAR_PART_OFFSET (dvar, 0) = 0;

	      dstslot
		= shared_hash_find_slot_unshare_1 (&dst->vars, dv, dvhash,
						   INSERT);
	      gcc_assert (!*dstslot);
	      *dstslot = dvar;
	    }
	  else
	    return 1;
	}
    }

  nodep = &dvar->var_part[0].loc_chain;
  while ((node = *nodep))
    {
      location_chain *nextp = &node->next;

      if (GET_CODE (node->loc) == REG)
	{
	  attrs list;

	  for (list = dst->regs[REGNO (node->loc)]; list; list = list->next)
	    if (GET_MODE (node->loc) == GET_MODE (list->loc)
		&& dv_is_value_p (list->dv))
	      break;

	  if (!list)
	    attrs_list_insert (&dst->regs[REGNO (node->loc)],
			       dv, 0, node->loc);
	  /* If this value became canonical for another value that had
	     this register, we want to leave it alone.  */
	  else if (dv_as_value (list->dv) != val)
	    {
	      dstslot = set_slot_part (dst, dv_as_value (list->dv),
				       dstslot, dv, 0,
				       node->init, NULL_RTX);
	      dstslot = delete_slot_part (dst, node->loc, dstslot, 0);

	      /* Since nextp points into the removed node, we can't
		 use it.  The pointer to the next node moved to nodep.
		 However, if the variable we're walking is unshared
		 during our walk, we'll keep walking the location list
		 of the previously-shared variable, in which case the
		 node won't have been removed, and we'll want to skip
		 it.  That's why we test *nodep here.  */
	      if (*nodep != node)
		nextp = nodep;
	    }
	}
      else
	/* Canonicalization puts registers first, so we don't have to
	   walk it all.  */
	break;
      nodep = nextp;
    }

  if (dvar != *dstslot)
    dvar = *dstslot;
  nodep = &dvar->var_part[0].loc_chain;

  if (val)
    {
      /* Mark all referenced nodes for canonicalization, and make sure
	 we have mutual equivalence links.  */
      VALUE_RECURSED_INTO (val) = true;
      for (node = *nodep; node; node = node->next)
	if (GET_CODE (node->loc) == VALUE)
	  {
	    VALUE_RECURSED_INTO (node->loc) = true;
	    set_variable_part (dst, val, dv_from_value (node->loc), 0,
			       node->init, NULL, INSERT);
	  }

      dstslot = shared_hash_find_slot_noinsert_1 (dst->vars, dv, dvhash);
      gcc_assert (*dstslot == dvar);
      canonicalize_values_star (dstslot, dst);
      gcc_checking_assert (dstslot
			   == shared_hash_find_slot_noinsert_1 (dst->vars,
								dv, dvhash));
      dvar = *dstslot;
    }
  else
    {
      bool has_value = false, has_other = false;

      /* If we have one value and anything else, we're going to
	 canonicalize this, so make sure all values have an entry in
	 the table and are marked for canonicalization.  */
      for (node = *nodep; node; node = node->next)
	{
	  if (GET_CODE (node->loc) == VALUE)
	    {
	      /* If this was marked during register canonicalization,
		 we know we have to canonicalize values.  */
	      if (has_value)
		has_other = true;
	      has_value = true;
	      if (has_other)
		break;
	    }
	  else
	    {
	      has_other = true;
	      if (has_value)
		break;
	    }
	}

      if (has_value && has_other)
	{
	  for (node = *nodep; node; node = node->next)
	    {
	      if (GET_CODE (node->loc) == VALUE)
		{
		  decl_or_value dv = dv_from_value (node->loc);
		  variable_def **slot = NULL;

		  if (shared_hash_shared (dst->vars))
		    slot = shared_hash_find_slot_noinsert (dst->vars, dv);
		  if (!slot)
		    slot = shared_hash_find_slot_unshare (&dst->vars, dv,
							  INSERT);
		  if (!*slot)
		    {
		      variable var = (variable) pool_alloc (onepart_pool
							    (ONEPART_VALUE));
		      var->dv = dv;
		      var->refcount = 1;
		      var->n_var_parts = 1;
		      var->onepart = ONEPART_VALUE;
		      var->in_changed_variables = false;
		      var->var_part[0].loc_chain = NULL;
		      var->var_part[0].cur_loc = NULL;
		      VAR_LOC_1PAUX (var) = NULL;
		      *slot = var;
		    }

		  VALUE_RECURSED_INTO (node->loc) = true;
		}
	    }

	  dstslot = shared_hash_find_slot_noinsert_1 (dst->vars, dv, dvhash);
	  gcc_assert (*dstslot == dvar);
	  canonicalize_values_star (dstslot, dst);
	  gcc_checking_assert (dstslot
			       == shared_hash_find_slot_noinsert_1 (dst->vars,
								    dv, dvhash));
	  dvar = *dstslot;
	}
    }

  if (!onepart_variable_different_p (dvar, s2var))
    {
      variable_htab_free (dvar);
      *dstslot = dvar = s2var;
      dvar->refcount++;
    }
  else if (s2var != s1var && !onepart_variable_different_p (dvar, s1var))
    {
      variable_htab_free (dvar);
      *dstslot = dvar = s1var;
      dvar->refcount++;
      dst_can_be_shared = false;
    }
  else
    dst_can_be_shared = false;

  return 1;
}

/* Copy s2slot (in DSM->src) to DSM->dst if the variable is a
   multi-part variable.  Unions of multi-part variables and
   intersections of one-part ones will be handled in
   variable_merge_over_cur().  */

static int
variable_merge_over_src (variable s2var, struct dfset_merge *dsm)
{
  dataflow_set *dst = dsm->dst;
  decl_or_value dv = s2var->dv;

  if (!s2var->onepart)
    {
      variable_def **dstp = shared_hash_find_slot (dst->vars, dv);
      *dstp = s2var;
      s2var->refcount++;
      return 1;
    }

  dsm->src_onepart_cnt++;
  return 1;
}

/* Combine dataflow set information from SRC2 into DST, using PDST
   to carry over information across passes.  */

static void
dataflow_set_merge (dataflow_set *dst, dataflow_set *src2)
{
  dataflow_set cur = *dst;
  dataflow_set *src1 = &cur;
  struct dfset_merge dsm;
  int i;
  size_t src1_elems, src2_elems;
  variable_iterator_type hi;
  variable var;

  src1_elems = shared_hash_htab (src1->vars).elements ();
  src2_elems = shared_hash_htab (src2->vars).elements ();
  dataflow_set_init (dst);
  dst->stack_adjust = cur.stack_adjust;
  shared_hash_destroy (dst->vars);
  dst->vars = (shared_hash) pool_alloc (shared_hash_pool);
  dst->vars->refcount = 1;
  dst->vars->htab.create (MAX (src1_elems, src2_elems));

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    attrs_list_mpdv_union (&dst->regs[i], src1->regs[i], src2->regs[i]);

  dsm.dst = dst;
  dsm.src = src2;
  dsm.cur = src1;
  dsm.src_onepart_cnt = 0;

  FOR_EACH_HASH_TABLE_ELEMENT (shared_hash_htab (dsm.src->vars),
			       var, variable, hi)
    variable_merge_over_src (var, &dsm);
  FOR_EACH_HASH_TABLE_ELEMENT (shared_hash_htab (dsm.cur->vars),
			       var, variable, hi)
    variable_merge_over_cur (var, &dsm);

  if (dsm.src_onepart_cnt)
    dst_can_be_shared = false;

  dataflow_set_destroy (src1);
}

/* Mark register equivalences.  */

static void
dataflow_set_equiv_regs (dataflow_set *set)
{
  int i;
  attrs list, *listp;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      rtx canon[NUM_MACHINE_MODES];

      /* If the list is empty or one entry, no need to canonicalize
	 anything.  */
      if (set->regs[i] == NULL || set->regs[i]->next == NULL)
	continue;

      memset (canon, 0, sizeof (canon));

      for (list = set->regs[i]; list; list = list->next)
	if (list->offset == 0 && dv_is_value_p (list->dv))
	  {
	    rtx val = dv_as_value (list->dv);
	    rtx *cvalp = &canon[(int)GET_MODE (val)];
	    rtx cval = *cvalp;

	    if (canon_value_cmp (val, cval))
	      *cvalp = val;
	  }

      for (list = set->regs[i]; list; list = list->next)
	if (list->offset == 0 && dv_onepart_p (list->dv))
	  {
	    rtx cval = canon[(int)GET_MODE (list->loc)];

	    if (!cval)
	      continue;

	    if (dv_is_value_p (list->dv))
	      {
		rtx val = dv_as_value (list->dv);

		if (val == cval)
		  continue;

		VALUE_RECURSED_INTO (val) = true;
		set_variable_part (set, val, dv_from_value (cval), 0,
				   VAR_INIT_STATUS_INITIALIZED,
				   NULL, NO_INSERT);
	      }

	    VALUE_RECURSED_INTO (cval) = true;
	    set_variable_part (set, cval, list->dv, 0,
			       VAR_INIT_STATUS_INITIALIZED, NULL, NO_INSERT);
	  }

      for (listp = &set->regs[i]; (list = *listp);
	   listp = list ? &list->next : listp)
	if (list->offset == 0 && dv_onepart_p (list->dv))
	  {
	    rtx cval = canon[(int)GET_MODE (list->loc)];
	    variable_def **slot;

	    if (!cval)
	      continue;

	    if (dv_is_value_p (list->dv))
	      {
		rtx val = dv_as_value (list->dv);
		if (!VALUE_RECURSED_INTO (val))
		  continue;
	      }

	    slot = shared_hash_find_slot_noinsert (set->vars, list->dv);
	    canonicalize_values_star (slot, set);
	    if (*listp != list)
	      list = NULL;
	  }
    }
}

/* Remove any redundant values in the location list of VAR, which must
   be unshared and 1-part.  */

static void
remove_duplicate_values (variable var)
{
  location_chain node, *nodep;

  gcc_assert (var->onepart);
  gcc_assert (var->n_var_parts == 1);
  gcc_assert (var->refcount == 1);

  for (nodep = &var->var_part[0].loc_chain; (node = *nodep); )
    {
      if (GET_CODE (node->loc) == VALUE)
	{
	  if (VALUE_RECURSED_INTO (node->loc))
	    {
	      /* Remove duplicate value node.  */
	      *nodep = node->next;
	      pool_free (loc_chain_pool, node);
	      continue;
	    }
	  else
	    VALUE_RECURSED_INTO (node->loc) = true;
	}
      nodep = &node->next;
    }

  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (GET_CODE (node->loc) == VALUE)
      {
	gcc_assert (VALUE_RECURSED_INTO (node->loc));
	VALUE_RECURSED_INTO (node->loc) = false;
      }
}


/* Hash table iteration argument passed to variable_post_merge.  */
struct dfset_post_merge
{
  /* The new input set for the current block.  */
  dataflow_set *set;
  /* Pointer to the permanent input set for the current block, or
     NULL.  */
  dataflow_set **permp;
};

/* Create values for incoming expressions associated with one-part
   variables that don't have value numbers for them.  */

int
variable_post_merge_new_vals (variable_def **slot, dfset_post_merge *dfpm)
{
  dataflow_set *set = dfpm->set;
  variable var = *slot;
  location_chain node;

  if (!var->onepart || !var->n_var_parts)
    return 1;

  gcc_assert (var->n_var_parts == 1);

  if (dv_is_decl_p (var->dv))
    {
      bool check_dupes = false;

    restart:
      for (node = var->var_part[0].loc_chain; node; node = node->next)
	{
	  if (GET_CODE (node->loc) == VALUE)
	    gcc_assert (!VALUE_RECURSED_INTO (node->loc));
	  else if (GET_CODE (node->loc) == REG)
	    {
	      attrs att, *attp, *curp = NULL;

	      if (var->refcount != 1)
		{
		  slot = unshare_variable (set, slot, var,
					   VAR_INIT_STATUS_INITIALIZED);
		  var = *slot;
		  goto restart;
		}

	      for (attp = &set->regs[REGNO (node->loc)]; (att = *attp);
		   attp = &att->next)
		if (att->offset == 0
		    && GET_MODE (att->loc) == GET_MODE (node->loc))
		  {
		    if (dv_is_value_p (att->dv))
		      {
			rtx cval = dv_as_value (att->dv);
			node->loc = cval;
			check_dupes = true;
			break;
		      }
		    else if (dv_as_opaque (att->dv) == dv_as_opaque (var->dv))
		      curp = attp;
		  }

	      if (!curp)
		{
		  curp = attp;
		  while (*curp)
		    if ((*curp)->offset == 0
			&& GET_MODE ((*curp)->loc) == GET_MODE (node->loc)
			&& dv_as_opaque ((*curp)->dv) == dv_as_opaque (var->dv))
		      break;
		    else
		      curp = &(*curp)->next;
		  gcc_assert (*curp);
		}

	      if (!att)
		{
		  decl_or_value cdv;
		  rtx cval;

		  if (!*dfpm->permp)
		    {
		      *dfpm->permp = XNEW (dataflow_set);
		      dataflow_set_init (*dfpm->permp);
		    }

		  for (att = (*dfpm->permp)->regs[REGNO (node->loc)];
		       att; att = att->next)
		    if (GET_MODE (att->loc) == GET_MODE (node->loc))
		      {
			gcc_assert (att->offset == 0
				    && dv_is_value_p (att->dv));
			val_reset (set, att->dv);
			break;
		      }

		  if (att)
		    {
		      cdv = att->dv;
		      cval = dv_as_value (cdv);
		    }
		  else
		    {
		      /* Create a unique value to hold this register,
			 that ought to be found and reused in
			 subsequent rounds.  */
		      cselib_val *v;
		      gcc_assert (!cselib_lookup (node->loc,
						  GET_MODE (node->loc), 0,
						  VOIDmode));
		      v = cselib_lookup (node->loc, GET_MODE (node->loc), 1,
					 VOIDmode);
		      cselib_preserve_value (v);
		      cselib_invalidate_rtx (node->loc);
		      cval = v->val_rtx;
		      cdv = dv_from_value (cval);
		      if (dump_file)
			fprintf (dump_file,
				 "Created new value %u:%u for reg %i\n",
				 v->uid, v->hash, REGNO (node->loc));
		    }

		  var_reg_decl_set (*dfpm->permp, node->loc,
				    VAR_INIT_STATUS_INITIALIZED,
				    cdv, 0, NULL, INSERT);

		  node->loc = cval;
		  check_dupes = true;
		}

	      /* Remove attribute referring to the decl, which now
		 uses the value for the register, already existing or
		 to be added when we bring perm in.  */
	      att = *curp;
	      *curp = att->next;
	      pool_free (attrs_pool, att);
	    }
	}

      if (check_dupes)
	remove_duplicate_values (var);
    }

  return 1;
}

/* Reset values in the permanent set that are not associated with the
   chosen expression.  */

int
variable_post_merge_perm_vals (variable_def **pslot, dfset_post_merge *dfpm)
{
  dataflow_set *set = dfpm->set;
  variable pvar = *pslot, var;
  location_chain pnode;
  decl_or_value dv;
  attrs att;

  gcc_assert (dv_is_value_p (pvar->dv)
	      && pvar->n_var_parts == 1);
  pnode = pvar->var_part[0].loc_chain;
  gcc_assert (pnode
	      && !pnode->next
	      && REG_P (pnode->loc));

  dv = pvar->dv;

  var = shared_hash_find (set->vars, dv);
  if (var)
    {
      /* Although variable_post_merge_new_vals may have made decls
	 non-star-canonical, values that pre-existed in canonical form
	 remain canonical, and newly-created values reference a single
	 REG, so they are canonical as well.  Since VAR has the
	 location list for a VALUE, using find_loc_in_1pdv for it is
	 fine, since VALUEs don't map back to DECLs.  */
      if (find_loc_in_1pdv (pnode->loc, var, shared_hash_htab (set->vars)))
	return 1;
      val_reset (set, dv);
    }

  for (att = set->regs[REGNO (pnode->loc)]; att; att = att->next)
    if (att->offset == 0
	&& GET_MODE (att->loc) == GET_MODE (pnode->loc)
	&& dv_is_value_p (att->dv))
      break;

  /* If there is a value associated with this register already, create
     an equivalence.  */
  if (att && dv_as_value (att->dv) != dv_as_value (dv))
    {
      rtx cval = dv_as_value (att->dv);
      set_variable_part (set, cval, dv, 0, pnode->init, NULL, INSERT);
      set_variable_part (set, dv_as_value (dv), att->dv, 0, pnode->init,
			 NULL, INSERT);
    }
  else if (!att)
    {
      attrs_list_insert (&set->regs[REGNO (pnode->loc)],
			 dv, 0, pnode->loc);
      variable_union (pvar, set);
    }

  return 1;
}

/* Just checking stuff and registering register attributes for
   now.  */

static void
dataflow_post_merge_adjust (dataflow_set *set, dataflow_set **permp)
{
  struct dfset_post_merge dfpm;

  dfpm.set = set;
  dfpm.permp = permp;

  shared_hash_htab (set->vars)
    .traverse <dfset_post_merge*, variable_post_merge_new_vals> (&dfpm);
  if (*permp)
    shared_hash_htab ((*permp)->vars)
      .traverse <dfset_post_merge*, variable_post_merge_perm_vals> (&dfpm);
  shared_hash_htab (set->vars)
    .traverse <dataflow_set *, canonicalize_values_star> (set);
  shared_hash_htab (set->vars)
    .traverse <dataflow_set *, canonicalize_vars_star> (set);
}

/* Return a node whose loc is a MEM that refers to EXPR in the
   location list of a one-part variable or value VAR, or in that of
   any values recursively mentioned in the location lists.  */

static location_chain
find_mem_expr_in_1pdv (tree expr, rtx val, variable_table_type vars)
{
  location_chain node;
  decl_or_value dv;
  variable var;
  location_chain where = NULL;

  if (!val)
    return NULL;

  gcc_assert (GET_CODE (val) == VALUE
	      && !VALUE_RECURSED_INTO (val));

  dv = dv_from_value (val);
  var = vars.find_with_hash (dv, dv_htab_hash (dv));

  if (!var)
    return NULL;

  gcc_assert (var->onepart);

  if (!var->n_var_parts)
    return NULL;

  VALUE_RECURSED_INTO (val) = true;

  for (node = var->var_part[0].loc_chain; node; node = node->next)
    if (MEM_P (node->loc)
	&& MEM_EXPR (node->loc) == expr
	&& INT_MEM_OFFSET (node->loc) == 0)
      {
	where = node;
	break;
      }
    else if (GET_CODE (node->loc) == VALUE
	     && !VALUE_RECURSED_INTO (node->loc)
	     && (where = find_mem_expr_in_1pdv (expr, node->loc, vars)))
      break;

  VALUE_RECURSED_INTO (val) = false;

  return where;
}

/* Return TRUE if the value of MEM may vary across a call.  */

static bool
mem_dies_at_call (rtx mem)
{
  tree expr = MEM_EXPR (mem);
  tree decl;

  if (!expr)
    return true;

  decl = get_base_address (expr);

  if (!decl)
    return true;

  if (!DECL_P (decl))
    return true;

  return (may_be_aliased (decl)
	  || (!TREE_READONLY (decl) && is_global_var (decl)));
}

/* Remove all MEMs from the location list of a hash table entry for a
   one-part variable, except those whose MEM attributes map back to
   the variable itself, directly or within a VALUE.  */

int
dataflow_set_preserve_mem_locs (variable_def **slot, dataflow_set *set)
{
  variable var = *slot;

  if (var->onepart == ONEPART_VDECL || var->onepart == ONEPART_DEXPR)
    {
      tree decl = dv_as_decl (var->dv);
      location_chain loc, *locp;
      bool changed = false;

      if (!var->n_var_parts)
	return 1;

      gcc_assert (var->n_var_parts == 1);

      if (shared_var_p (var, set->vars))
	{
	  for (loc = var->var_part[0].loc_chain; loc; loc = loc->next)
	    {
	      /* We want to remove dying MEMs that doesn't refer to DECL.  */
	      if (GET_CODE (loc->loc) == MEM
		  && (MEM_EXPR (loc->loc) != decl
		      || INT_MEM_OFFSET (loc->loc) != 0)
		  && !mem_dies_at_call (loc->loc))
		break;
	      /* We want to move here MEMs that do refer to DECL.  */
	      else if (GET_CODE (loc->loc) == VALUE
		       && find_mem_expr_in_1pdv (decl, loc->loc,
						 shared_hash_htab (set->vars)))
		break;
	    }

	  if (!loc)
	    return 1;

	  slot = unshare_variable (set, slot, var, VAR_INIT_STATUS_UNKNOWN);
	  var = *slot;
	  gcc_assert (var->n_var_parts == 1);
	}

      for (locp = &var->var_part[0].loc_chain, loc = *locp;
	   loc; loc = *locp)
	{
	  rtx old_loc = loc->loc;
	  if (GET_CODE (old_loc) == VALUE)
	    {
	      location_chain mem_node
		= find_mem_expr_in_1pdv (decl, loc->loc,
					 shared_hash_htab (set->vars));

	      /* ??? This picks up only one out of multiple MEMs that
		 refer to the same variable.  Do we ever need to be
		 concerned about dealing with more than one, or, given
		 that they should all map to the same variable
		 location, their addresses will have been merged and
		 they will be regarded as equivalent?  */
	      if (mem_node)
		{
		  loc->loc = mem_node->loc;
		  loc->set_src = mem_node->set_src;
		  loc->init = MIN (loc->init, mem_node->init);
		}
	    }

	  if (GET_CODE (loc->loc) != MEM
	      || (MEM_EXPR (loc->loc) == decl
		  && INT_MEM_OFFSET (loc->loc) == 0)
	      || !mem_dies_at_call (loc->loc))
	    {
	      if (old_loc != loc->loc && emit_notes)
		{
		  if (old_loc == var->var_part[0].cur_loc)
		    {
		      changed = true;
		      var->var_part[0].cur_loc = NULL;
		    }
		}
	      locp = &loc->next;
	      continue;
	    }

	  if (emit_notes)
	    {
	      if (old_loc == var->var_part[0].cur_loc)
		{
		  changed = true;
		  var->var_part[0].cur_loc = NULL;
		}
	    }
	  *locp = loc->next;
	  pool_free (loc_chain_pool, loc);
	}

      if (!var->var_part[0].loc_chain)
	{
	  var->n_var_parts--;
	  changed = true;
	}
      if (changed)
	variable_was_changed (var, set);
    }

  return 1;
}

/* Remove all MEMs from the location list of a hash table entry for a
   value.  */

int
dataflow_set_remove_mem_locs (variable_def **slot, dataflow_set *set)
{
  variable var = *slot;

  if (var->onepart == ONEPART_VALUE)
    {
      location_chain loc, *locp;
      bool changed = false;
      rtx cur_loc;

      gcc_assert (var->n_var_parts == 1);

      if (shared_var_p (var, set->vars))
	{
	  for (loc = var->var_part[0].loc_chain; loc; loc = loc->next)
	    if (GET_CODE (loc->loc) == MEM
		&& mem_dies_at_call (loc->loc))
	      break;

	  if (!loc)
	    return 1;

	  slot = unshare_variable (set, slot, var, VAR_INIT_STATUS_UNKNOWN);
	  var = *slot;
	  gcc_assert (var->n_var_parts == 1);
	}

      if (VAR_LOC_1PAUX (var))
	cur_loc = VAR_LOC_FROM (var);
      else
	cur_loc = var->var_part[0].cur_loc;

      for (locp = &var->var_part[0].loc_chain, loc = *locp;
	   loc; loc = *locp)
	{
	  if (GET_CODE (loc->loc) != MEM
	      || !mem_dies_at_call (loc->loc))
	    {
	      locp = &loc->next;
	      continue;
	    }

	  *locp = loc->next;
	  /* If we have deleted the location which was last emitted
	     we have to emit new location so add the variable to set
	     of changed variables.  */
	  if (cur_loc == loc->loc)
	    {
	      changed = true;
	      var->var_part[0].cur_loc = NULL;
	      if (VAR_LOC_1PAUX (var))
		VAR_LOC_FROM (var) = NULL;
	    }
	  pool_free (loc_chain_pool, loc);
	}

      if (!var->var_part[0].loc_chain)
	{
	  var->n_var_parts--;
	  changed = true;
	}
      if (changed)
	variable_was_changed (var, set);
    }

  return 1;
}

/* Remove all variable-location information about call-clobbered
   registers, as well as associations between MEMs and VALUEs.  */

static void
dataflow_set_clear_at_call (dataflow_set *set)
{
  unsigned int r;
  hard_reg_set_iterator hrsi;

  EXECUTE_IF_SET_IN_HARD_REG_SET (regs_invalidated_by_call, 0, r, hrsi)
    var_regno_delete (set, r);

  if (MAY_HAVE_DEBUG_INSNS)
    {
      set->traversed_vars = set->vars;
      shared_hash_htab (set->vars)
	.traverse <dataflow_set *, dataflow_set_preserve_mem_locs> (set);
      set->traversed_vars = set->vars;
      shared_hash_htab (set->vars)
	.traverse <dataflow_set *, dataflow_set_remove_mem_locs> (set);
      set->traversed_vars = NULL;
    }
}

static bool
variable_part_different_p (variable_part *vp1, variable_part *vp2)
{
  location_chain lc1, lc2;

  for (lc1 = vp1->loc_chain; lc1; lc1 = lc1->next)
    {
      for (lc2 = vp2->loc_chain; lc2; lc2 = lc2->next)
	{
	  if (REG_P (lc1->loc) && REG_P (lc2->loc))
	    {
	      if (REGNO (lc1->loc) == REGNO (lc2->loc))
		break;
	    }
	  if (rtx_equal_p (lc1->loc, lc2->loc))
	    break;
	}
      if (!lc2)
	return true;
    }
  return false;
}

/* Return true if one-part variables VAR1 and VAR2 are different.
   They must be in canonical order.  */

static bool
onepart_variable_different_p (variable var1, variable var2)
{
  location_chain lc1, lc2;

  if (var1 == var2)
    return false;

  gcc_assert (var1->n_var_parts == 1
	      && var2->n_var_parts == 1);

  lc1 = var1->var_part[0].loc_chain;
  lc2 = var2->var_part[0].loc_chain;

  gcc_assert (lc1 && lc2);

  while (lc1 && lc2)
    {
      if (loc_cmp (lc1->loc, lc2->loc))
	return true;
      lc1 = lc1->next;
      lc2 = lc2->next;
    }

  return lc1 != lc2;
}

/* Return true if variables VAR1 and VAR2 are different.  */

static bool
variable_different_p (variable var1, variable var2)
{
  int i;

  if (var1 == var2)
    return false;

  if (var1->onepart != var2->onepart)
    return true;

  if (var1->n_var_parts != var2->n_var_parts)
    return true;

  if (var1->onepart && var1->n_var_parts)
    {
      gcc_checking_assert (dv_as_opaque (var1->dv) == dv_as_opaque (var2->dv)
			   && var1->n_var_parts == 1);
      /* One-part values have locations in a canonical order.  */
      return onepart_variable_different_p (var1, var2);
    }

  for (i = 0; i < var1->n_var_parts; i++)
    {
      if (VAR_PART_OFFSET (var1, i) != VAR_PART_OFFSET (var2, i))
	return true;
      if (variable_part_different_p (&var1->var_part[i], &var2->var_part[i]))
	return true;
      if (variable_part_different_p (&var2->var_part[i], &var1->var_part[i]))
	return true;
    }
  return false;
}

/* Return true if dataflow sets OLD_SET and NEW_SET differ.  */

static bool
dataflow_set_different (dataflow_set *old_set, dataflow_set *new_set)
{
  variable_iterator_type hi;
  variable var1;

  if (old_set->vars == new_set->vars)
    return false;

  if (shared_hash_htab (old_set->vars).elements ()
      != shared_hash_htab (new_set->vars).elements ())
    return true;

  FOR_EACH_HASH_TABLE_ELEMENT (shared_hash_htab (old_set->vars),
			       var1, variable, hi)
    {
      variable_table_type htab = shared_hash_htab (new_set->vars);
      variable var2 = htab.find_with_hash (var1->dv, dv_htab_hash (var1->dv));
      if (!var2)
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "dataflow difference found: removal of:\n");
	      dump_var (var1);
	    }
	  return true;
	}

      if (variable_different_p (var1, var2))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    {
	      fprintf (dump_file, "dataflow difference found: "
		       "old and new follow:\n");
	      dump_var (var1);
	      dump_var (var2);
	    }
	  return true;
	}
    }

  /* No need to traverse the second hashtab, if both have the same number
     of elements and the second one had all entries found in the first one,
     then it can't have any extra entries.  */
  return false;
}

/* Free the contents of dataflow set SET.  */

static void
dataflow_set_destroy (dataflow_set *set)
{
  int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    attrs_list_clear (&set->regs[i]);

  shared_hash_destroy (set->vars);
  set->vars = NULL;
}

/* Return true if RTL X contains a SYMBOL_REF.  */

static bool
contains_symbol_ref (rtx x)
{
  const char *fmt;
  RTX_CODE code;
  int i;

  if (!x)
    return false;

  code = GET_CODE (x);
  if (code == SYMBOL_REF)
    return true;

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (contains_symbol_ref (XEXP (x, i)))
	    return true;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (contains_symbol_ref (XVECEXP (x, i, j)))
	      return true;
	}
    }

  return false;
}

/* Shall EXPR be tracked?  */

static bool
track_expr_p (tree expr, bool need_rtl)
{
  rtx decl_rtl;
  tree realdecl;

  if (TREE_CODE (expr) == DEBUG_EXPR_DECL)
    return DECL_RTL_SET_P (expr);

  /* If EXPR is not a parameter or a variable do not track it.  */
  if (TREE_CODE (expr) != VAR_DECL && TREE_CODE (expr) != PARM_DECL)
    return 0;

  /* It also must have a name...  */
  if (!DECL_NAME (expr) && need_rtl)
    return 0;

  /* ... and a RTL assigned to it.  */
  decl_rtl = DECL_RTL_IF_SET (expr);
  if (!decl_rtl && need_rtl)
    return 0;

  /* If this expression is really a debug alias of some other declaration, we
     don't need to track this expression if the ultimate declaration is
     ignored.  */
  realdecl = expr;
  if (TREE_CODE (realdecl) == VAR_DECL && DECL_HAS_DEBUG_EXPR_P (realdecl))
    {
      realdecl = DECL_DEBUG_EXPR (realdecl);
      if (!DECL_P (realdecl))
	{
	  if (handled_component_p (realdecl)
	      || (TREE_CODE (realdecl) == MEM_REF
		  && TREE_CODE (TREE_OPERAND (realdecl, 0)) == ADDR_EXPR))
	    {
	      HOST_WIDE_INT bitsize, bitpos, maxsize;
	      tree innerdecl
		= get_ref_base_and_extent (realdecl, &bitpos, &bitsize,
					   &maxsize);
	      if (!DECL_P (innerdecl)
		  || DECL_IGNORED_P (innerdecl)
		  /* Do not track declarations for parts of tracked parameters
		     since we want to track them as a whole instead.  */
		  || (TREE_CODE (innerdecl) == PARM_DECL
		      && DECL_MODE (innerdecl) != BLKmode
		      && TREE_CODE (TREE_TYPE (innerdecl)) != UNION_TYPE)
		  || TREE_STATIC (innerdecl)
		  || bitsize <= 0
		  || bitpos + bitsize > 256
		  || bitsize != maxsize)
		return 0;
	      else
		realdecl = expr;
	    }
	  else
	    return 0;
	}
    }

  /* Do not track EXPR if REALDECL it should be ignored for debugging
     purposes.  */
  if (DECL_IGNORED_P (realdecl))
    return 0;

  /* Do not track global variables until we are able to emit correct location
     list for them.  */
  if (TREE_STATIC (realdecl))
    return 0;

  /* When the EXPR is a DECL for alias of some variable (see example)
     the TREE_STATIC flag is not used.  Disable tracking all DECLs whose
     DECL_RTL contains SYMBOL_REF.

     Example:
     extern char **_dl_argv_internal __attribute__ ((alias ("_dl_argv")));
     char **_dl_argv;
  */
  if (decl_rtl && MEM_P (decl_rtl)
      && contains_symbol_ref (XEXP (decl_rtl, 0)))
    return 0;

  /* If RTX is a memory it should not be very large (because it would be
     an array or struct).  */
  if (decl_rtl && MEM_P (decl_rtl))
    {
      /* Do not track structures and arrays.  */
      if (GET_MODE (decl_rtl) == BLKmode
	  || AGGREGATE_TYPE_P (TREE_TYPE (realdecl)))
	return 0;
      if (MEM_SIZE_KNOWN_P (decl_rtl)
	  && MEM_SIZE (decl_rtl) > MAX_VAR_PARTS)
	return 0;
    }

  DECL_CHANGED (expr) = 0;
  DECL_CHANGED (realdecl) = 0;
  return 1;
}

/* Determine whether a given LOC refers to the same variable part as
   EXPR+OFFSET.  */

static bool
same_variable_part_p (rtx loc, tree expr, HOST_WIDE_INT offset)
{
  tree expr2;
  HOST_WIDE_INT offset2;

  if (! DECL_P (expr))
    return false;

  if (REG_P (loc))
    {
      expr2 = REG_EXPR (loc);
      offset2 = REG_OFFSET (loc);
    }
  else if (MEM_P (loc))
    {
      expr2 = MEM_EXPR (loc);
      offset2 = INT_MEM_OFFSET (loc);
    }
  else
    return false;

  if (! expr2 || ! DECL_P (expr2))
    return false;

  expr = var_debug_decl (expr);
  expr2 = var_debug_decl (expr2);

  return (expr == expr2 && offset == offset2);
}

/* LOC is a REG or MEM that we would like to track if possible.
   If EXPR is null, we don't know what expression LOC refers to,
   otherwise it refers to EXPR + OFFSET.  STORE_REG_P is true if
   LOC is an lvalue register.

   Return true if EXPR is nonnull and if LOC, or some lowpart of it,
   is something we can track.  When returning true, store the mode of
   the lowpart we can track in *MODE_OUT (if nonnull) and its offset
   from EXPR in *OFFSET_OUT (if nonnull).  */

static bool
track_loc_p (rtx loc, tree expr, HOST_WIDE_INT offset, bool store_reg_p,
	     enum machine_mode *mode_out, HOST_WIDE_INT *offset_out)
{
  enum machine_mode mode;

  if (expr == NULL || !track_expr_p (expr, true))
    return false;

  /* If REG was a paradoxical subreg, its REG_ATTRS will describe the
     whole subreg, but only the old inner part is really relevant.  */
  mode = GET_MODE (loc);
  if (REG_P (loc) && !HARD_REGISTER_NUM_P (ORIGINAL_REGNO (loc)))
    {
      enum machine_mode pseudo_mode;

      pseudo_mode = PSEUDO_REGNO_MODE (ORIGINAL_REGNO (loc));
      if (GET_MODE_SIZE (mode) > GET_MODE_SIZE (pseudo_mode))
	{
	  offset += byte_lowpart_offset (pseudo_mode, mode);
	  mode = pseudo_mode;
	}
    }

  /* If LOC is a paradoxical lowpart of EXPR, refer to EXPR itself.
     Do the same if we are storing to a register and EXPR occupies
     the whole of register LOC; in that case, the whole of EXPR is
     being changed.  We exclude complex modes from the second case
     because the real and imaginary parts are represented as separate
     pseudo registers, even if the whole complex value fits into one
     hard register.  */
  if ((GET_MODE_SIZE (mode) > GET_MODE_SIZE (DECL_MODE (expr))
       || (store_reg_p
	   && !COMPLEX_MODE_P (DECL_MODE (expr))
	   && hard_regno_nregs[REGNO (loc)][DECL_MODE (expr)] == 1))
      && offset + byte_lowpart_offset (DECL_MODE (expr), mode) == 0)
    {
      mode = DECL_MODE (expr);
      offset = 0;
    }

  if (offset < 0 || offset >= MAX_VAR_PARTS)
    return false;

  if (mode_out)
    *mode_out = mode;
  if (offset_out)
    *offset_out = offset;
  return true;
}

/* Return the MODE lowpart of LOC, or null if LOC is not something we
   want to track.  When returning nonnull, make sure that the attributes
   on the returned value are updated.  */

static rtx
var_lowpart (enum machine_mode mode, rtx loc)
{
  unsigned int offset, reg_offset, regno;

  if (GET_MODE (loc) == mode)
    return loc;

  if (!REG_P (loc) && !MEM_P (loc))
    return NULL;

  offset = byte_lowpart_offset (mode, GET_MODE (loc));

  if (MEM_P (loc))
    return adjust_address_nv (loc, mode, offset);

  reg_offset = subreg_lowpart_offset (mode, GET_MODE (loc));
  regno = REGNO (loc) + subreg_regno_offset (REGNO (loc), GET_MODE (loc),
					     reg_offset, mode);
  return gen_rtx_REG_offset (loc, mode, regno, offset);
}

/* Carry information about uses and stores while walking rtx.  */

struct count_use_info
{
  /* The insn where the RTX is.  */
  rtx insn;

  /* The basic block where insn is.  */
  basic_block bb;

  /* The array of n_sets sets in the insn, as determined by cselib.  */
  struct cselib_set *sets;
  int n_sets;

  /* True if we're counting stores, false otherwise.  */
  bool store_p;
};

/* Find a VALUE corresponding to X.   */

static inline cselib_val *
find_use_val (rtx x, enum machine_mode mode, struct count_use_info *cui)
{
  int i;

  if (cui->sets)
    {
      /* This is called after uses are set up and before stores are
	 processed by cselib, so it's safe to look up srcs, but not
	 dsts.  So we look up expressions that appear in srcs or in
	 dest expressions, but we search the sets array for dests of
	 stores.  */
      if (cui->store_p)
	{
	  /* Some targets represent memset and memcpy patterns
	     by (set (mem:BLK ...) (reg:[QHSD]I ...)) or
	     (set (mem:BLK ...) (const_int ...)) or
	     (set (mem:BLK ...) (mem:BLK ...)).  Don't return anything
	     in that case, otherwise we end up with mode mismatches.  */
	  if (mode == BLKmode && MEM_P (x))
	    return NULL;
	  for (i = 0; i < cui->n_sets; i++)
	    if (cui->sets[i].dest == x)
	      return cui->sets[i].src_elt;
	}
      else
	return cselib_lookup (x, mode, 0, VOIDmode);
    }

  return NULL;
}

/* Replace all registers and addresses in an expression with VALUE
   expressions that map back to them, unless the expression is a
   register.  If no mapping is or can be performed, returns NULL.  */

static rtx
replace_expr_with_values (rtx loc)
{
  if (REG_P (loc) || GET_CODE (loc) == ENTRY_VALUE)
    return NULL;
  else if (MEM_P (loc))
    {
      cselib_val *addr = cselib_lookup (XEXP (loc, 0),
					get_address_mode (loc), 0,
					GET_MODE (loc));
      if (addr)
	return replace_equiv_address_nv (loc, addr->val_rtx);
      else
	return NULL;
    }
  else
    return cselib_subst_to_values (loc, VOIDmode);
}

/* Return true if *X is a DEBUG_EXPR.  Usable as an argument to
   for_each_rtx to tell whether there are any DEBUG_EXPRs within
   RTX.  */

static int
rtx_debug_expr_p (rtx *x, void *data ATTRIBUTE_UNUSED)
{
  rtx loc = *x;

  return GET_CODE (loc) == DEBUG_EXPR;
}

/* Determine what kind of micro operation to choose for a USE.  Return
   MO_CLOBBER if no micro operation is to be generated.  */

static enum micro_operation_type
use_type (rtx loc, struct count_use_info *cui, enum machine_mode *modep)
{
  tree expr;

  if (cui && cui->sets)
    {
      if (GET_CODE (loc) == VAR_LOCATION)
	{
	  if (track_expr_p (PAT_VAR_LOCATION_DECL (loc), false))
	    {
	      rtx ploc = PAT_VAR_LOCATION_LOC (loc);
	      if (! VAR_LOC_UNKNOWN_P (ploc))
		{
		  cselib_val *val = cselib_lookup (ploc, GET_MODE (loc), 1,
						   VOIDmode);

		  /* ??? flag_float_store and volatile mems are never
		     given values, but we could in theory use them for
		     locations.  */
		  gcc_assert (val || 1);
		}
	      return MO_VAL_LOC;
	    }
	  else
	    return MO_CLOBBER;
	}

      if (REG_P (loc) || MEM_P (loc))
	{
	  if (modep)
	    *modep = GET_MODE (loc);
	  if (cui->store_p)
	    {
	      if (REG_P (loc)
		  || (find_use_val (loc, GET_MODE (loc), cui)
		      && cselib_lookup (XEXP (loc, 0),
					get_address_mode (loc), 0,
					GET_MODE (loc))))
		return MO_VAL_SET;
	    }
	  else
	    {
	      cselib_val *val = find_use_val (loc, GET_MODE (loc), cui);

	      if (val && !cselib_preserved_value_p (val))
		return MO_VAL_USE;
	    }
	}
    }

  if (REG_P (loc))
    {
      gcc_assert (REGNO (loc) < FIRST_PSEUDO_REGISTER);

      if (loc == cfa_base_rtx)
	return MO_CLOBBER;
      expr = REG_EXPR (loc);

      if (!expr)
	return MO_USE_NO_VAR;
      else if (target_for_debug_bind (var_debug_decl (expr)))
	return MO_CLOBBER;
      else if (track_loc_p (loc, expr, REG_OFFSET (loc),
			    false, modep, NULL))
	return MO_USE;
      else
	return MO_USE_NO_VAR;
    }
  else if (MEM_P (loc))
    {
      expr = MEM_EXPR (loc);

      if (!expr)
	return MO_CLOBBER;
      else if (target_for_debug_bind (var_debug_decl (expr)))
	return MO_CLOBBER;
      else if (track_loc_p (loc, expr, INT_MEM_OFFSET (loc),
			    false, modep, NULL)
	       /* Multi-part variables shouldn't refer to one-part
		  variable names such as VALUEs (never happens) or
		  DEBUG_EXPRs (only happens in the presence of debug
		  insns).  */
	       && (!MAY_HAVE_DEBUG_INSNS
		   || !for_each_rtx (&XEXP (loc, 0), rtx_debug_expr_p, NULL)))
	return MO_USE;
      else
	return MO_CLOBBER;
    }

  return MO_CLOBBER;
}

/* Log to OUT information about micro-operation MOPT involving X in
   INSN of BB.  */

static inline void
log_op_type (rtx x, basic_block bb, rtx insn,
	     enum micro_operation_type mopt, FILE *out)
{
  fprintf (out, "bb %i op %i insn %i %s ",
	   bb->index, VTI (bb)->mos.length (),
	   INSN_UID (insn), micro_operation_type_name[mopt]);
  print_inline_rtx (out, x, 2);
  fputc ('\n', out);
}

/* Tell whether the CONCAT used to holds a VALUE and its location
   needs value resolution, i.e., an attempt of mapping the location
   back to other incoming values.  */
#define VAL_NEEDS_RESOLUTION(x) \
  (RTL_FLAG_CHECK1 ("VAL_NEEDS_RESOLUTION", (x), CONCAT)->volatil)
/* Whether the location in the CONCAT is a tracked expression, that
   should also be handled like a MO_USE.  */
#define VAL_HOLDS_TRACK_EXPR(x) \
  (RTL_FLAG_CHECK1 ("VAL_HOLDS_TRACK_EXPR", (x), CONCAT)->used)
/* Whether the location in the CONCAT should be handled like a MO_COPY
   as well.  */
#define VAL_EXPR_IS_COPIED(x) \
  (RTL_FLAG_CHECK1 ("VAL_EXPR_IS_COPIED", (x), CONCAT)->jump)
/* Whether the location in the CONCAT should be handled like a
   MO_CLOBBER as well.  */
#define VAL_EXPR_IS_CLOBBERED(x) \
  (RTL_FLAG_CHECK1 ("VAL_EXPR_IS_CLOBBERED", (x), CONCAT)->unchanging)

/* All preserved VALUEs.  */
static vec<rtx> preserved_values;

/* Ensure VAL is preserved and remember it in a vector for vt_emit_notes.  */

static void
preserve_value (cselib_val *val)
{
  cselib_preserve_value (val);
  preserved_values.safe_push (val->val_rtx);
}

/* Helper function for MO_VAL_LOC handling.  Return non-zero if
   any rtxes not suitable for CONST use not replaced by VALUEs
   are discovered.  */

static int
non_suitable_const (rtx *x, void *data ATTRIBUTE_UNUSED)
{
  if (*x == NULL_RTX)
    return 0;

  switch (GET_CODE (*x))
    {
    case REG:
    case DEBUG_EXPR:
    case PC:
    case SCRATCH:
    case CC0:
    case ASM_INPUT:
    case ASM_OPERANDS:
      return 1;
    case MEM:
      return !MEM_READONLY_P (*x);
    default:
      return 0;
    }
}

/* Add uses (register and memory references) LOC which will be tracked
   to VTI (bb)->mos.  INSN is instruction which the LOC is part of.  */

static int
add_uses (rtx *ploc, void *data)
{
  rtx loc = *ploc;
  enum machine_mode mode = VOIDmode;
  struct count_use_info *cui = (struct count_use_info *)data;
  enum micro_operation_type type = use_type (loc, cui, &mode);

  if (type != MO_CLOBBER)
    {
      basic_block bb = cui->bb;
      micro_operation mo;

      mo.type = type;
      mo.u.loc = type == MO_USE ? var_lowpart (mode, loc) : loc;
      mo.insn = cui->insn;

      if (type == MO_VAL_LOC)
	{
	  rtx oloc = loc;
	  rtx vloc = PAT_VAR_LOCATION_LOC (oloc);
	  cselib_val *val;

	  gcc_assert (cui->sets);

	  if (MEM_P (vloc)
	      && !REG_P (XEXP (vloc, 0))
	      && !MEM_P (XEXP (vloc, 0)))
	    {
	      rtx mloc = vloc;
	      enum machine_mode address_mode = get_address_mode (mloc);
	      cselib_val *val
		= cselib_lookup (XEXP (mloc, 0), address_mode, 0,
				 GET_MODE (mloc));

	      if (val && !cselib_preserved_value_p (val))
		preserve_value (val);
	    }

	  if (CONSTANT_P (vloc)
	      && (GET_CODE (vloc) != CONST
		  || for_each_rtx (&vloc, non_suitable_const, NULL)))
	    /* For constants don't look up any value.  */;
	  else if (!VAR_LOC_UNKNOWN_P (vloc) && !unsuitable_loc (vloc)
		   && (val = find_use_val (vloc, GET_MODE (oloc), cui)))
	    {
	      enum machine_mode mode2;
	      enum micro_operation_type type2;
	      rtx nloc = NULL;
	      bool resolvable = REG_P (vloc) || MEM_P (vloc);

	      if (resolvable)
		nloc = replace_expr_with_values (vloc);

	      if (nloc)
		{
		  oloc = shallow_copy_rtx (oloc);
		  PAT_VAR_LOCATION_LOC (oloc) = nloc;
		}

	      oloc = gen_rtx_CONCAT (mode, val->val_rtx, oloc);

	      type2 = use_type (vloc, 0, &mode2);

	      gcc_assert (type2 == MO_USE || type2 == MO_USE_NO_VAR
			  || type2 == MO_CLOBBER);

	      if (type2 == MO_CLOBBER
		  && !cselib_preserved_value_p (val))
		{
		  VAL_NEEDS_RESOLUTION (oloc) = resolvable;
		  preserve_value (val);
		}
	    }
	  else if (!VAR_LOC_UNKNOWN_P (vloc))
	    {
	      oloc = shallow_copy_rtx (oloc);
	      PAT_VAR_LOCATION_LOC (oloc) = gen_rtx_UNKNOWN_VAR_LOC ();
	    }

	  mo.u.loc = oloc;
	}
      else if (type == MO_VAL_USE)
	{
	  enum machine_mode mode2 = VOIDmode;
	  enum micro_operation_type type2;
	  cselib_val *val = find_use_val (loc, GET_MODE (loc), cui);
	  rtx vloc, oloc = loc, nloc;

	  gcc_assert (cui->sets);

	  if (MEM_P (oloc)
	      && !REG_P (XEXP (oloc, 0))
	      && !MEM_P (XEXP (oloc, 0)))
	    {
	      rtx mloc = oloc;
	      enum machine_mode address_mode = get_address_mode (mloc);
	      cselib_val *val
		= cselib_lookup (XEXP (mloc, 0), address_mode, 0,
				 GET_MODE (mloc));

	      if (val && !cselib_preserved_value_p (val))
		preserve_value (val);
	    }

	  type2 = use_type (loc, 0, &mode2);

	  gcc_assert (type2 == MO_USE || type2 == MO_USE_NO_VAR
		      || type2 == MO_CLOBBER);

	  if (type2 == MO_USE)
	    vloc = var_lowpart (mode2, loc);
	  else
	    vloc = oloc;

	  /* The loc of a MO_VAL_USE may have two forms:

	     (concat val src): val is at src, a value-based
	     representation.

	     (concat (concat val use) src): same as above, with use as
	     the MO_USE tracked value, if it differs from src.

	  */

	  gcc_checking_assert (REG_P (loc) || MEM_P (loc));
	  nloc = replace_expr_with_values (loc);
	  if (!nloc)
	    nloc = oloc;

	  if (vloc != nloc)
	    oloc = gen_rtx_CONCAT (mode2, val->val_rtx, vloc);
	  else
	    oloc = val->val_rtx;

	  mo.u.loc = gen_rtx_CONCAT (mode, oloc, nloc);

	  if (type2 == MO_USE)
	    VAL_HOLDS_TRACK_EXPR (mo.u.loc) = 1;
	  if (!cselib_preserved_value_p (val))
	    {
	      VAL_NEEDS_RESOLUTION (mo.u.loc) = 1;
	      preserve_value (val);
	    }
	}
      else
	gcc_assert (type == MO_USE || type == MO_USE_NO_VAR);

      if (dump_file && (dump_flags & TDF_DETAILS))
	log_op_type (mo.u.loc, cui->bb, cui->insn, mo.type, dump_file);
      VTI (bb)->mos.safe_push (mo);
    }

  return 0;
}

/* Helper function for finding all uses of REG/MEM in X in insn INSN.  */

static void
add_uses_1 (rtx *x, void *cui)
{
  for_each_rtx (x, add_uses, cui);
}

/* This is the value used during expansion of locations.  We want it
   to be unbounded, so that variables expanded deep in a recursion
   nest are fully evaluated, so that their values are cached
   correctly.  We avoid recursion cycles through other means, and we
   don't unshare RTL, so excess complexity is not a problem.  */
#define EXPR_DEPTH (INT_MAX)
/* We use this to keep too-complex expressions from being emitted as
   location notes, and then to debug information.  Users can trade
   compile time for ridiculously complex expressions, although they're
   seldom useful, and they may often have to be discarded as not
   representable anyway.  */
#define EXPR_USE_DEPTH (PARAM_VALUE (PARAM_MAX_VARTRACK_EXPR_DEPTH))

/* Attempt to reverse the EXPR operation in the debug info and record
   it in the cselib table.  Say for reg1 = reg2 + 6 even when reg2 is
   no longer live we can express its value as VAL - 6.  */

static void
reverse_op (rtx val, const_rtx expr, rtx insn)
{
  rtx src, arg, ret;
  cselib_val *v;
  struct elt_loc_list *l;
  enum rtx_code code;
  int count;

  if (GET_CODE (expr) != SET)
    return;

  if (!REG_P (SET_DEST (expr)) || GET_MODE (val) != GET_MODE (SET_DEST (expr)))
    return;

  src = SET_SRC (expr);
  switch (GET_CODE (src))
    {
    case PLUS:
    case MINUS:
    case XOR:
    case NOT:
    case NEG:
      if (!REG_P (XEXP (src, 0)))
	return;
      break;
    case SIGN_EXTEND:
    case ZERO_EXTEND:
      if (!REG_P (XEXP (src, 0)) && !MEM_P (XEXP (src, 0)))
	return;
      break;
    default:
      return;
    }

  if (!SCALAR_INT_MODE_P (GET_MODE (src)) || XEXP (src, 0) == cfa_base_rtx)
    return;

  v = cselib_lookup (XEXP (src, 0), GET_MODE (XEXP (src, 0)), 0, VOIDmode);
  if (!v || !cselib_preserved_value_p (v))
    return;

  /* Use canonical V to avoid creating multiple redundant expressions
     for different VALUES equivalent to V.  */
  v = canonical_cselib_val (v);

  /* Adding a reverse op isn't useful if V already has an always valid
     location.  Ignore ENTRY_VALUE, while it is always constant, we should
     prefer non-ENTRY_VALUE locations whenever possible.  */
  for (l = v->locs, count = 0; l; l = l->next, count++)
    if (CONSTANT_P (l->loc)
	&& (GET_CODE (l->loc) != CONST || !references_value_p (l->loc, 0)))
      return;
    /* Avoid creating too large locs lists.  */
    else if (count == PARAM_VALUE (PARAM_MAX_VARTRACK_REVERSE_OP_SIZE))
      return;

  switch (GET_CODE (src))
    {
    case NOT:
    case NEG:
      if (GET_MODE (v->val_rtx) != GET_MODE (val))
	return;
      ret = gen_rtx_fmt_e (GET_CODE (src), GET_MODE (val), val);
      break;
    case SIGN_EXTEND:
    case ZERO_EXTEND:
      ret = gen_lowpart_SUBREG (GET_MODE (v->val_rtx), val);
      break;
    case XOR:
      code = XOR;
      goto binary;
    case PLUS:
      code = MINUS;
      goto binary;
    case MINUS:
      code = PLUS;
      goto binary;
    binary:
      if (GET_MODE (v->val_rtx) != GET_MODE (val))
	return;
      arg = XEXP (src, 1);
      if (!CONST_INT_P (arg) && GET_CODE (arg) != SYMBOL_REF)
	{
	  arg = cselib_expand_value_rtx (arg, scratch_regs, 5);
	  if (arg == NULL_RTX)
	    return;
	  if (!CONST_INT_P (arg) && GET_CODE (arg) != SYMBOL_REF)
	    return;
	}
      ret = simplify_gen_binary (code, GET_MODE (val), val, arg);
      if (ret == val)
	/* Ensure ret isn't VALUE itself (which can happen e.g. for
	   (plus (reg1) (reg2)) when reg2 is known to be 0), as that
	   breaks a lot of routines during var-tracking.  */
	ret = gen_rtx_fmt_ee (PLUS, GET_MODE (val), val, const0_rtx);
      break;
    default:
      gcc_unreachable ();
    }

  cselib_add_permanent_equiv (v, ret, insn);
}

/* Add stores (register and memory references) LOC which will be tracked
   to VTI (bb)->mos.  EXPR is the RTL expression containing the store.
   CUIP->insn is instruction which the LOC is part of.  */

static void
add_stores (rtx loc, const_rtx expr, void *cuip)
{
  enum machine_mode mode = VOIDmode, mode2;
  struct count_use_info *cui = (struct count_use_info *)cuip;
  basic_block bb = cui->bb;
  micro_operation mo;
  rtx oloc = loc, nloc, src = NULL;
  enum micro_operation_type type = use_type (loc, cui, &mode);
  bool track_p = false;
  cselib_val *v;
  bool resolve, preserve;

  if (type == MO_CLOBBER)
    return;

  mode2 = mode;

  if (REG_P (loc))
    {
      gcc_assert (loc != cfa_base_rtx);
      if ((GET_CODE (expr) == CLOBBER && type != MO_VAL_SET)
	  || !(track_p = use_type (loc, NULL, &mode2) == MO_USE)
	  || GET_CODE (expr) == CLOBBER)
	{
	  mo.type = MO_CLOBBER;
	  mo.u.loc = loc;
	  if (GET_CODE (expr) == SET
	      && SET_DEST (expr) == loc
	      && !unsuitable_loc (SET_SRC (expr))
	      && find_use_val (loc, mode, cui))
	    {
	      gcc_checking_assert (type == MO_VAL_SET);
	      mo.u.loc = gen_rtx_SET (VOIDmode, loc, SET_SRC (expr));
	    }
	}
      else
	{
	  if (GET_CODE (expr) == SET
	      && SET_DEST (expr) == loc
	      && GET_CODE (SET_SRC (expr)) != ASM_OPERANDS)
	    src = var_lowpart (mode2, SET_SRC (expr));
	  loc = var_lowpart (mode2, loc);

	  if (src == NULL)
	    {
	      mo.type = MO_SET;
	      mo.u.loc = loc;
	    }
	  else
	    {
	      rtx xexpr = gen_rtx_SET (VOIDmode, loc, src);
	      if (same_variable_part_p (src, REG_EXPR (loc), REG_OFFSET (loc)))
		{
		  /* If this is an instruction copying (part of) a parameter
		     passed by invisible reference to its register location,
		     pretend it's a SET so that the initial memory location
		     is discarded, as the parameter register can be reused
		     for other purposes and we do not track locations based
		     on generic registers.  */
		  if (MEM_P (src)
		      && REG_EXPR (loc)
		      && TREE_CODE (REG_EXPR (loc)) == PARM_DECL
		      && DECL_MODE (REG_EXPR (loc)) != BLKmode
		      && MEM_P (DECL_INCOMING_RTL (REG_EXPR (loc)))
		      && XEXP (DECL_INCOMING_RTL (REG_EXPR (loc)), 0)
			 != arg_pointer_rtx)
		    mo.type = MO_SET;
		  else
		    mo.type = MO_COPY;
		}
	      else
		mo.type = MO_SET;
	      mo.u.loc = xexpr;
	    }
	}
      mo.insn = cui->insn;
    }
  else if (MEM_P (loc)
	   && ((track_p = use_type (loc, NULL, &mode2) == MO_USE)
	       || cui->sets))
    {
      if (MEM_P (loc) && type == MO_VAL_SET
	  && !REG_P (XEXP (loc, 0))
	  && !MEM_P (XEXP (loc, 0)))
	{
	  rtx mloc = loc;
	  enum machine_mode address_mode = get_address_mode (mloc);
	  cselib_val *val = cselib_lookup (XEXP (mloc, 0),
					   address_mode, 0,
					   GET_MODE (mloc));

	  if (val && !cselib_preserved_value_p (val))
	    preserve_value (val);
	}

      if (GET_CODE (expr) == CLOBBER || !track_p)
	{
	  mo.type = MO_CLOBBER;
	  mo.u.loc = track_p ? var_lowpart (mode2, loc) : loc;
	}
      else
	{
	  if (GET_CODE (expr) == SET
	      && SET_DEST (expr) == loc
	      && GET_CODE (SET_SRC (expr)) != ASM_OPERANDS)
	    src = var_lowpart (mode2, SET_SRC (expr));
	  loc = var_lowpart (mode2, loc);

	  if (src == NULL)
	    {
	      mo.type = MO_SET;
	      mo.u.loc = loc;
	    }
	  else
	    {
	      rtx xexpr = gen_rtx_SET (VOIDmode, loc, src);
	      if (same_variable_part_p (SET_SRC (xexpr),
					MEM_EXPR (loc),
					INT_MEM_OFFSET (loc)))
		mo.type = MO_COPY;
	      else
		mo.type = MO_SET;
	      mo.u.loc = xexpr;
	    }
	}
      mo.insn = cui->insn;
    }
  else
    return;

  if (type != MO_VAL_SET)
    goto log_and_return;

  v = find_use_val (oloc, mode, cui);

  if (!v)
    goto log_and_return;

  resolve = preserve = !cselib_preserved_value_p (v);

  /* We cannot track values for multiple-part variables, so we track only
     locations for tracked parameters passed either by invisible reference
     or directly in multiple locations.  */
  if (track_p
      && REG_P (loc)
      && REG_EXPR (loc)
      && TREE_CODE (REG_EXPR (loc)) == PARM_DECL
      && DECL_MODE (REG_EXPR (loc)) != BLKmode
      && TREE_CODE (TREE_TYPE (REG_EXPR (loc))) != UNION_TYPE
      && ((MEM_P (DECL_INCOMING_RTL (REG_EXPR (loc)))
	   && XEXP (DECL_INCOMING_RTL (REG_EXPR (loc)), 0) != arg_pointer_rtx)
          || (GET_CODE (DECL_INCOMING_RTL (REG_EXPR (loc))) == PARALLEL
	      && XVECLEN (DECL_INCOMING_RTL (REG_EXPR (loc)), 0) > 1)))
    {
      /* Although we don't use the value here, it could be used later by the
	 mere virtue of its existence as the operand of the reverse operation
	 that gave rise to it (typically extension/truncation).  Make sure it
	 is preserved as required by vt_expand_var_loc_chain.  */
      if (preserve)
	preserve_value (v);
      goto log_and_return;
    }

  if (loc == stack_pointer_rtx
      && hard_frame_pointer_adjustment != -1
      && preserve)
    cselib_set_value_sp_based (v);

  nloc = replace_expr_with_values (oloc);
  if (nloc)
    oloc = nloc;

  if (GET_CODE (PATTERN (cui->insn)) == COND_EXEC)
    {
      cselib_val *oval = cselib_lookup (oloc, GET_MODE (oloc), 0, VOIDmode);

      gcc_assert (oval != v);
      gcc_assert (REG_P (oloc) || MEM_P (oloc));

      if (oval && !cselib_preserved_value_p (oval))
	{
	  micro_operation moa;

	  preserve_value (oval);

	  moa.type = MO_VAL_USE;
	  moa.u.loc = gen_rtx_CONCAT (mode, oval->val_rtx, oloc);
	  VAL_NEEDS_RESOLUTION (moa.u.loc) = 1;
	  moa.insn = cui->insn;

	  if (dump_file && (dump_flags & TDF_DETAILS))
	    log_op_type (moa.u.loc, cui->bb, cui->insn,
			 moa.type, dump_file);
	  VTI (bb)->mos.safe_push (moa);
	}

      resolve = false;
    }
  else if (resolve && GET_CODE (mo.u.loc) == SET)
    {
      if (REG_P (SET_SRC (expr)) || MEM_P (SET_SRC (expr)))
	nloc = replace_expr_with_values (SET_SRC (expr));
      else
	nloc = NULL_RTX;

      /* Avoid the mode mismatch between oexpr and expr.  */
      if (!nloc && mode != mode2)
	{
	  nloc = SET_SRC (expr);
	  gcc_assert (oloc == SET_DEST (expr));
	}

      if (nloc && nloc != SET_SRC (mo.u.loc))
	oloc = gen_rtx_SET (GET_MODE (mo.u.loc), oloc, nloc);
      else
	{
	  if (oloc == SET_DEST (mo.u.loc))
	    /* No point in duplicating.  */
	    oloc = mo.u.loc;
	  if (!REG_P (SET_SRC (mo.u.loc)))
	    resolve = false;
	}
    }
  else if (!resolve)
    {
      if (GET_CODE (mo.u.loc) == SET
	  && oloc == SET_DEST (mo.u.loc))
	/* No point in duplicating.  */
	oloc = mo.u.loc;
    }
  else
    resolve = false;

  loc = gen_rtx_CONCAT (mode, v->val_rtx, oloc);

  if (mo.u.loc != oloc)
    loc = gen_rtx_CONCAT (GET_MODE (mo.u.loc), loc, mo.u.loc);

  /* The loc of a MO_VAL_SET may have various forms:

     (concat val dst): dst now holds val

     (concat val (set dst src)): dst now holds val, copied from src

     (concat (concat val dstv) dst): dst now holds val; dstv is dst
     after replacing mems and non-top-level regs with values.

     (concat (concat val dstv) (set dst src)): dst now holds val,
     copied from src.  dstv is a value-based representation of dst, if
     it differs from dst.  If resolution is needed, src is a REG, and
     its mode is the same as that of val.

     (concat (concat val (set dstv srcv)) (set dst src)): src
     copied to dst, holding val.  dstv and srcv are value-based
     representations of dst and src, respectively.

  */

  if (GET_CODE (PATTERN (cui->insn)) != COND_EXEC)
    reverse_op (v->val_rtx, expr, cui->insn);

  mo.u.loc = loc;

  if (track_p)
    VAL_HOLDS_TRACK_EXPR (loc) = 1;
  if (preserve)
    {
      VAL_NEEDS_RESOLUTION (loc) = resolve;
      preserve_value (v);
    }
  if (mo.type == MO_CLOBBER)
    VAL_EXPR_IS_CLOBBERED (loc) = 1;
  if (mo.type == MO_COPY)
    VAL_EXPR_IS_COPIED (loc) = 1;

  mo.type = MO_VAL_SET;

 log_and_return:
  if (dump_file && (dump_flags & TDF_DETAILS))
    log_op_type (mo.u.loc, cui->bb, cui->insn, mo.type, dump_file);
  VTI (bb)->mos.safe_push (mo);
}

/* Arguments to the call.  */
static rtx call_arguments;

/* Compute call_arguments.  */

static void
prepare_call_arguments (basic_block bb, rtx insn)
{
  rtx link, x, call;
  rtx prev, cur, next;
  rtx this_arg = NULL_RTX;
  tree type = NULL_TREE, t, fndecl = NULL_TREE;
  tree obj_type_ref = NULL_TREE;
  CUMULATIVE_ARGS args_so_far_v;
  cumulative_args_t args_so_far;

  memset (&args_so_far_v, 0, sizeof (args_so_far_v));
  args_so_far = pack_cumulative_args (&args_so_far_v);
  call = get_call_rtx_from (insn);
  if (call)
    {
      if (GET_CODE (XEXP (XEXP (call, 0), 0)) == SYMBOL_REF)
	{
	  rtx symbol = XEXP (XEXP (call, 0), 0);
	  if (SYMBOL_REF_DECL (symbol))
	    fndecl = SYMBOL_REF_DECL (symbol);
	}
      if (fndecl == NULL_TREE)
	fndecl = MEM_EXPR (XEXP (call, 0));
      if (fndecl
	  && TREE_CODE (TREE_TYPE (fndecl)) != FUNCTION_TYPE
	  && TREE_CODE (TREE_TYPE (fndecl)) != METHOD_TYPE)
	fndecl = NULL_TREE;
      if (fndecl && TYPE_ARG_TYPES (TREE_TYPE (fndecl)))
	type = TREE_TYPE (fndecl);
      if (fndecl && TREE_CODE (fndecl) != FUNCTION_DECL)
	{
	  if (TREE_CODE (fndecl) == INDIRECT_REF
	      && TREE_CODE (TREE_OPERAND (fndecl, 0)) == OBJ_TYPE_REF)
	    obj_type_ref = TREE_OPERAND (fndecl, 0);
	  fndecl = NULL_TREE;
	}
      if (type)
	{
	  for (t = TYPE_ARG_TYPES (type); t && t != void_list_node;
	       t = TREE_CHAIN (t))
	    if (TREE_CODE (TREE_VALUE (t)) == REFERENCE_TYPE
		&& INTEGRAL_TYPE_P (TREE_TYPE (TREE_VALUE (t))))
	      break;
	  if ((t == NULL || t == void_list_node) && obj_type_ref == NULL_TREE)
	    type = NULL;
	  else
	    {
	      int nargs ATTRIBUTE_UNUSED = list_length (TYPE_ARG_TYPES (type));
	      link = CALL_INSN_FUNCTION_USAGE (insn);
#ifndef PCC_STATIC_STRUCT_RETURN
	      if (aggregate_value_p (TREE_TYPE (type), type)
		  && targetm.calls.struct_value_rtx (type, 0) == 0)
		{
		  tree struct_addr = build_pointer_type (TREE_TYPE (type));
		  enum machine_mode mode = TYPE_MODE (struct_addr);
		  rtx reg;
		  INIT_CUMULATIVE_ARGS (args_so_far_v, type, NULL_RTX, fndecl,
					nargs + 1);
		  reg = targetm.calls.function_arg (args_so_far, mode,
						    struct_addr, true);
		  targetm.calls.function_arg_advance (args_so_far, mode,
						      struct_addr, true);
		  if (reg == NULL_RTX)
		    {
		      for (; link; link = XEXP (link, 1))
			if (GET_CODE (XEXP (link, 0)) == USE
			    && MEM_P (XEXP (XEXP (link, 0), 0)))
			  {
			    link = XEXP (link, 1);
			    break;
			  }
		    }
		}
	      else
#endif
		INIT_CUMULATIVE_ARGS (args_so_far_v, type, NULL_RTX, fndecl,
				      nargs);
	      if (obj_type_ref && TYPE_ARG_TYPES (type) != void_list_node)
		{
		  enum machine_mode mode;
		  t = TYPE_ARG_TYPES (type);
		  mode = TYPE_MODE (TREE_VALUE (t));
		  this_arg = targetm.calls.function_arg (args_so_far, mode,
							 TREE_VALUE (t), true);
		  if (this_arg && !REG_P (this_arg))
		    this_arg = NULL_RTX;
		  else if (this_arg == NULL_RTX)
		    {
		      for (; link; link = XEXP (link, 1))
			if (GET_CODE (XEXP (link, 0)) == USE
			    && MEM_P (XEXP (XEXP (link, 0), 0)))
			  {
			    this_arg = XEXP (XEXP (link, 0), 0);
			    break;
			  }
		    }
		}
	    }
	}
    }
  t = type ? TYPE_ARG_TYPES (type) : NULL_TREE;

  for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
    if (GET_CODE (XEXP (link, 0)) == USE)
      {
	rtx item = NULL_RTX;
	x = XEXP (XEXP (link, 0), 0);
	if (GET_MODE (link) == VOIDmode
	    || GET_MODE (link) == BLKmode
	    || (GET_MODE (link) != GET_MODE (x)
		&& (GET_MODE_CLASS (GET_MODE (link)) != MODE_INT
		    || GET_MODE_CLASS (GET_MODE (x)) != MODE_INT)))
	  /* Can't do anything for these, if the original type mode
	     isn't known or can't be converted.  */;
	else if (REG_P (x))
	  {
	    cselib_val *val = cselib_lookup (x, GET_MODE (x), 0, VOIDmode);
	    if (val && cselib_preserved_value_p (val))
	      item = val->val_rtx;
	    else if (GET_MODE_CLASS (GET_MODE (x)) == MODE_INT)
	      {
		enum machine_mode mode = GET_MODE (x);

		while ((mode = GET_MODE_WIDER_MODE (mode)) != VOIDmode
		       && GET_MODE_BITSIZE (mode) <= BITS_PER_WORD)
		  {
		    rtx reg = simplify_subreg (mode, x, GET_MODE (x), 0);

		    if (reg == NULL_RTX || !REG_P (reg))
		      continue;
		    val = cselib_lookup (reg, mode, 0, VOIDmode);
		    if (val && cselib_preserved_value_p (val))
		      {
			item = val->val_rtx;
			break;
		      }
		  }
	      }
	  }
	else if (MEM_P (x))
	  {
	    rtx mem = x;
	    cselib_val *val;

	    if (!frame_pointer_needed)
	      {
		struct adjust_mem_data amd;
		amd.mem_mode = VOIDmode;
		amd.stack_adjust = -VTI (bb)->out.stack_adjust;
		amd.side_effects = NULL_RTX;
		amd.store = true;
		mem = simplify_replace_fn_rtx (mem, NULL_RTX, adjust_mems,
					       &amd);
		gcc_assert (amd.side_effects == NULL_RTX);
	      }
	    val = cselib_lookup (mem, GET_MODE (mem), 0, VOIDmode);
	    if (val && cselib_preserved_value_p (val))
	      item = val->val_rtx;
	    else if (GET_MODE_CLASS (GET_MODE (mem)) != MODE_INT)
	      {
		/* For non-integer stack argument see also if they weren't
		   initialized by integers.  */
		enum machine_mode imode = int_mode_for_mode (GET_MODE (mem));
		if (imode != GET_MODE (mem) && imode != BLKmode)
		  {
		    val = cselib_lookup (adjust_address_nv (mem, imode, 0),
					 imode, 0, VOIDmode);
		    if (val && cselib_preserved_value_p (val))
		      item = lowpart_subreg (GET_MODE (x), val->val_rtx,
					     imode);
		  }
	      }
	  }
	if (item)
	  {
	    rtx x2 = x;
	    if (GET_MODE (item) != GET_MODE (link))
	      item = lowpart_subreg (GET_MODE (link), item, GET_MODE (item));
	    if (GET_MODE (x2) != GET_MODE (link))
	      x2 = lowpart_subreg (GET_MODE (link), x2, GET_MODE (x2));
	    item = gen_rtx_CONCAT (GET_MODE (link), x2, item);
	    call_arguments
	      = gen_rtx_EXPR_LIST (VOIDmode, item, call_arguments);
	  }
	if (t && t != void_list_node)
	  {
	    tree argtype = TREE_VALUE (t);
	    enum machine_mode mode = TYPE_MODE (argtype);
	    rtx reg;
	    if (pass_by_reference (&args_so_far_v, mode, argtype, true))
	      {
		argtype = build_pointer_type (argtype);
		mode = TYPE_MODE (argtype);
	      }
	    reg = targetm.calls.function_arg (args_so_far, mode,
					      argtype, true);
	    if (TREE_CODE (argtype) == REFERENCE_TYPE
		&& INTEGRAL_TYPE_P (TREE_TYPE (argtype))
		&& reg
		&& REG_P (reg)
		&& GET_MODE (reg) == mode
		&& GET_MODE_CLASS (mode) == MODE_INT
		&& REG_P (x)
		&& REGNO (x) == REGNO (reg)
		&& GET_MODE (x) == mode
		&& item)
	      {
		enum machine_mode indmode
		  = TYPE_MODE (TREE_TYPE (argtype));
		rtx mem = gen_rtx_MEM (indmode, x);
		cselib_val *val = cselib_lookup (mem, indmode, 0, VOIDmode);
		if (val && cselib_preserved_value_p (val))
		  {
		    item = gen_rtx_CONCAT (indmode, mem, val->val_rtx);
		    call_arguments = gen_rtx_EXPR_LIST (VOIDmode, item,
							call_arguments);
		  }
		else
		  {
		    struct elt_loc_list *l;
		    tree initial;

		    /* Try harder, when passing address of a constant
		       pool integer it can be easily read back.  */
		    item = XEXP (item, 1);
		    if (GET_CODE (item) == SUBREG)
		      item = SUBREG_REG (item);
		    gcc_assert (GET_CODE (item) == VALUE);
		    val = CSELIB_VAL_PTR (item);
		    for (l = val->locs; l; l = l->next)
		      if (GET_CODE (l->loc) == SYMBOL_REF
			  && TREE_CONSTANT_POOL_ADDRESS_P (l->loc)
			  && SYMBOL_REF_DECL (l->loc)
			  && DECL_INITIAL (SYMBOL_REF_DECL (l->loc)))
			{
			  initial = DECL_INITIAL (SYMBOL_REF_DECL (l->loc));
			  if (tree_fits_shwi_p (initial))
			    {
			      item = GEN_INT (tree_to_shwi (initial));
			      item = gen_rtx_CONCAT (indmode, mem, item);
			      call_arguments
				= gen_rtx_EXPR_LIST (VOIDmode, item,
						     call_arguments);
			    }
			  break;
			}
		  }
	      }
	    targetm.calls.function_arg_advance (args_so_far, mode,
						argtype, true);
	    t = TREE_CHAIN (t);
	  }
      }

  /* Add debug arguments.  */
  if (fndecl
      && TREE_CODE (fndecl) == FUNCTION_DECL
      && DECL_HAS_DEBUG_ARGS_P (fndecl))
    {
      vec<tree, va_gc> **debug_args = decl_debug_args_lookup (fndecl);
      if (debug_args)
	{
	  unsigned int ix;
	  tree param;
	  for (ix = 0; vec_safe_iterate (*debug_args, ix, &param); ix += 2)
	    {
	      rtx item;
	      tree dtemp = (**debug_args)[ix + 1];
	      enum machine_mode mode = DECL_MODE (dtemp);
	      item = gen_rtx_DEBUG_PARAMETER_REF (mode, param);
	      item = gen_rtx_CONCAT (mode, item, DECL_RTL_KNOWN_SET (dtemp));
	      call_arguments = gen_rtx_EXPR_LIST (VOIDmode, item,
						  call_arguments);
	    }
	}
    }

  /* Reverse call_arguments chain.  */
  prev = NULL_RTX;
  for (cur = call_arguments; cur; cur = next)
    {
      next = XEXP (cur, 1);
      XEXP (cur, 1) = prev;
      prev = cur;
    }
  call_arguments = prev;

  x = get_call_rtx_from (insn);
  if (x)
    {
      x = XEXP (XEXP (x, 0), 0);
      if (GET_CODE (x) == SYMBOL_REF)
	/* Don't record anything.  */;
      else if (CONSTANT_P (x))
	{
	  x = gen_rtx_CONCAT (GET_MODE (x) == VOIDmode ? Pmode : GET_MODE (x),
			      pc_rtx, x);
	  call_arguments
	    = gen_rtx_EXPR_LIST (VOIDmode, x, call_arguments);
	}
      else
	{
	  cselib_val *val = cselib_lookup (x, GET_MODE (x), 0, VOIDmode);
	  if (val && cselib_preserved_value_p (val))
	    {
	      x = gen_rtx_CONCAT (GET_MODE (x), pc_rtx, val->val_rtx);
	      call_arguments
		= gen_rtx_EXPR_LIST (VOIDmode, x, call_arguments);
	    }
	}
    }
  if (this_arg)
    {
      enum machine_mode mode
	= TYPE_MODE (TREE_TYPE (OBJ_TYPE_REF_EXPR (obj_type_ref)));
      rtx clobbered = gen_rtx_MEM (mode, this_arg);
      HOST_WIDE_INT token
	= tree_to_shwi (OBJ_TYPE_REF_TOKEN (obj_type_ref));
      if (token)
	clobbered = plus_constant (mode, clobbered,
				   token * GET_MODE_SIZE (mode));
      clobbered = gen_rtx_MEM (mode, clobbered);
      x = gen_rtx_CONCAT (mode, gen_rtx_CLOBBER (VOIDmode, pc_rtx), clobbered);
      call_arguments
	= gen_rtx_EXPR_LIST (VOIDmode, x, call_arguments);
    }
}

/* Callback for cselib_record_sets_hook, that records as micro
   operations uses and stores in an insn after cselib_record_sets has
   analyzed the sets in an insn, but before it modifies the stored
   values in the internal tables, unless cselib_record_sets doesn't
   call it directly (perhaps because we're not doing cselib in the
   first place, in which case sets and n_sets will be 0).  */

static void
add_with_sets (rtx insn, struct cselib_set *sets, int n_sets)
{
  basic_block bb = BLOCK_FOR_INSN (insn);
  int n1, n2;
  struct count_use_info cui;
  micro_operation *mos;

  cselib_hook_called = true;

  cui.insn = insn;
  cui.bb = bb;
  cui.sets = sets;
  cui.n_sets = n_sets;

  n1 = VTI (bb)->mos.length ();
  cui.store_p = false;
  note_uses (&PATTERN (insn), add_uses_1, &cui);
  n2 = VTI (bb)->mos.length () - 1;
  mos = VTI (bb)->mos.address ();

  /* Order the MO_USEs to be before MO_USE_NO_VARs and MO_VAL_USE, and
     MO_VAL_LOC last.  */
  while (n1 < n2)
    {
      while (n1 < n2 && mos[n1].type == MO_USE)
	n1++;
      while (n1 < n2 && mos[n2].type != MO_USE)
	n2--;
      if (n1 < n2)
	{
	  micro_operation sw;

	  sw = mos[n1];
	  mos[n1] = mos[n2];
	  mos[n2] = sw;
	}
    }

  n2 = VTI (bb)->mos.length () - 1;
  while (n1 < n2)
    {
      while (n1 < n2 && mos[n1].type != MO_VAL_LOC)
	n1++;
      while (n1 < n2 && mos[n2].type == MO_VAL_LOC)
	n2--;
      if (n1 < n2)
	{
	  micro_operation sw;

	  sw = mos[n1];
	  mos[n1] = mos[n2];
	  mos[n2] = sw;
	}
    }

  if (CALL_P (insn))
    {
      micro_operation mo;

      mo.type = MO_CALL;
      mo.insn = insn;
      mo.u.loc = call_arguments;
      call_arguments = NULL_RTX;

      if (dump_file && (dump_flags & TDF_DETAILS))
	log_op_type (PATTERN (insn), bb, insn, mo.type, dump_file);
      VTI (bb)->mos.safe_push (mo);
    }

  n1 = VTI (bb)->mos.length ();
  /* This will record NEXT_INSN (insn), such that we can
     insert notes before it without worrying about any
     notes that MO_USEs might emit after the insn.  */
  cui.store_p = true;
  note_stores (PATTERN (insn), add_stores, &cui);
  n2 = VTI (bb)->mos.length () - 1;
  mos = VTI (bb)->mos.address ();

  /* Order the MO_VAL_USEs first (note_stores does nothing
     on DEBUG_INSNs, so there are no MO_VAL_LOCs from this
     insn), then MO_CLOBBERs, then MO_SET/MO_COPY/MO_VAL_SET.  */
  while (n1 < n2)
    {
      while (n1 < n2 && mos[n1].type == MO_VAL_USE)
	n1++;
      while (n1 < n2 && mos[n2].type != MO_VAL_USE)
	n2--;
      if (n1 < n2)
	{
	  micro_operation sw;

	  sw = mos[n1];
	  mos[n1] = mos[n2];
	  mos[n2] = sw;
	}
    }

  n2 = VTI (bb)->mos.length () - 1;
  while (n1 < n2)
    {
      while (n1 < n2 && mos[n1].type == MO_CLOBBER)
	n1++;
      while (n1 < n2 && mos[n2].type != MO_CLOBBER)
	n2--;
      if (n1 < n2)
	{
	  micro_operation sw;

	  sw = mos[n1];
	  mos[n1] = mos[n2];
	  mos[n2] = sw;
	}
    }
}

static enum var_init_status
find_src_status (dataflow_set *in, rtx src)
{
  tree decl = NULL_TREE;
  enum var_init_status status = VAR_INIT_STATUS_UNINITIALIZED;

  if (! flag_var_tracking_uninit)
    status = VAR_INIT_STATUS_INITIALIZED;

  if (src && REG_P (src))
    decl = var_debug_decl (REG_EXPR (src));
  else if (src && MEM_P (src))
    decl = var_debug_decl (MEM_EXPR (src));

  if (src && decl)
    status = get_init_value (in, src, dv_from_decl (decl));

  return status;
}

/* SRC is the source of an assignment.  Use SET to try to find what
   was ultimately assigned to SRC.  Return that value if known,
   otherwise return SRC itself.  */

static rtx
find_src_set_src (dataflow_set *set, rtx src)
{
  tree decl = NULL_TREE;   /* The variable being copied around.          */
  rtx set_src = NULL_RTX;  /* The value for "decl" stored in "src".      */
  variable var;
  location_chain nextp;
  int i;
  bool found;

  if (src && REG_P (src))
    decl = var_debug_decl (REG_EXPR (src));
  else if (src && MEM_P (src))
    decl = var_debug_decl (MEM_EXPR (src));

  if (src && decl)
    {
      decl_or_value dv = dv_from_decl (decl);

      var = shared_hash_find (set->vars, dv);
      if (var)
	{
	  found = false;
	  for (i = 0; i < var->n_var_parts && !found; i++)
	    for (nextp = var->var_part[i].loc_chain; nextp && !found;
		 nextp = nextp->next)
	      if (rtx_equal_p (nextp->loc, src))
		{
		  set_src = nextp->set_src;
		  found = true;
		}

	}
    }

  return set_src;
}

/* Compute the changes of variable locations in the basic block BB.  */

static bool
compute_bb_dataflow (basic_block bb)
{
  unsigned int i;
  micro_operation *mo;
  bool changed;
  dataflow_set old_out;
  dataflow_set *in = &VTI (bb)->in;
  dataflow_set *out = &VTI (bb)->out;

  dataflow_set_init (&old_out);
  dataflow_set_copy (&old_out, out);
  dataflow_set_copy (out, in);

  if (MAY_HAVE_DEBUG_INSNS)
    local_get_addr_cache = pointer_map_create ();

  FOR_EACH_VEC_ELT (VTI (bb)->mos, i, mo)
    {
      rtx insn = mo->insn;

      switch (mo->type)
	{
	  case MO_CALL:
	    dataflow_set_clear_at_call (out);
	    break;

	  case MO_USE:
	    {
	      rtx loc = mo->u.loc;

	      if (REG_P (loc))
		var_reg_set (out, loc, VAR_INIT_STATUS_UNINITIALIZED, NULL);
	      else if (MEM_P (loc))
		var_mem_set (out, loc, VAR_INIT_STATUS_UNINITIALIZED, NULL);
	    }
	    break;

	  case MO_VAL_LOC:
	    {
	      rtx loc = mo->u.loc;
	      rtx val, vloc;
	      tree var;

	      if (GET_CODE (loc) == CONCAT)
		{
		  val = XEXP (loc, 0);
		  vloc = XEXP (loc, 1);
		}
	      else
		{
		  val = NULL_RTX;
		  vloc = loc;
		}

	      var = PAT_VAR_LOCATION_DECL (vloc);

	      clobber_variable_part (out, NULL_RTX,
				     dv_from_decl (var), 0, NULL_RTX);
	      if (val)
		{
		  if (VAL_NEEDS_RESOLUTION (loc))
		    val_resolve (out, val, PAT_VAR_LOCATION_LOC (vloc), insn);
		  set_variable_part (out, val, dv_from_decl (var), 0,
				     VAR_INIT_STATUS_INITIALIZED, NULL_RTX,
				     INSERT);
		}
	      else if (!VAR_LOC_UNKNOWN_P (PAT_VAR_LOCATION_LOC (vloc)))
		set_variable_part (out, PAT_VAR_LOCATION_LOC (vloc),
				   dv_from_decl (var), 0,
				   VAR_INIT_STATUS_INITIALIZED, NULL_RTX,
				   INSERT);
	    }
	    break;

	  case MO_VAL_USE:
	    {
	      rtx loc = mo->u.loc;
	      rtx val, vloc, uloc;

	      vloc = uloc = XEXP (loc, 1);
	      val = XEXP (loc, 0);

	      if (GET_CODE (val) == CONCAT)
		{
		  uloc = XEXP (val, 1);
		  val = XEXP (val, 0);
		}

	      if (VAL_NEEDS_RESOLUTION (loc))
		val_resolve (out, val, vloc, insn);
	      else
		val_store (out, val, uloc, insn, false);

	      if (VAL_HOLDS_TRACK_EXPR (loc))
		{
		  if (GET_CODE (uloc) == REG)
		    var_reg_set (out, uloc, VAR_INIT_STATUS_UNINITIALIZED,
				 NULL);
		  else if (GET_CODE (uloc) == MEM)
		    var_mem_set (out, uloc, VAR_INIT_STATUS_UNINITIALIZED,
				 NULL);
		}
	    }
	    break;

	  case MO_VAL_SET:
	    {
	      rtx loc = mo->u.loc;
	      rtx val, vloc, uloc;
	      rtx dstv, srcv;

	      vloc = loc;
	      uloc = XEXP (vloc, 1);
	      val = XEXP (vloc, 0);
	      vloc = uloc;

	      if (GET_CODE (uloc) == SET)
		{
		  dstv = SET_DEST (uloc);
		  srcv = SET_SRC (uloc);
		}
	      else
		{
		  dstv = uloc;
		  srcv = NULL;
		}

	      if (GET_CODE (val) == CONCAT)
		{
		  dstv = vloc = XEXP (val, 1);
		  val = XEXP (val, 0);
		}

	      if (GET_CODE (vloc) == SET)
		{
		  srcv = SET_SRC (vloc);

		  gcc_assert (val != srcv);
		  gcc_assert (vloc == uloc || VAL_NEEDS_RESOLUTION (loc));

		  dstv = vloc = SET_DEST (vloc);

		  if (VAL_NEEDS_RESOLUTION (loc))
		    val_resolve (out, val, srcv, insn);
		}
	      else if (VAL_NEEDS_RESOLUTION (loc))
		{
		  gcc_assert (GET_CODE (uloc) == SET
			      && GET_CODE (SET_SRC (uloc)) == REG);
		  val_resolve (out, val, SET_SRC (uloc), insn);
		}

	      if (VAL_HOLDS_TRACK_EXPR (loc))
		{
		  if (VAL_EXPR_IS_CLOBBERED (loc))
		    {
		      if (REG_P (uloc))
			var_reg_delete (out, uloc, true);
		      else if (MEM_P (uloc))
			{
			  gcc_assert (MEM_P (dstv));
			  gcc_assert (MEM_ATTRS (dstv) == MEM_ATTRS (uloc));
			  var_mem_delete (out, dstv, true);
			}
		    }
		  else
		    {
		      bool copied_p = VAL_EXPR_IS_COPIED (loc);
		      rtx src = NULL, dst = uloc;
		      enum var_init_status status = VAR_INIT_STATUS_INITIALIZED;

		      if (GET_CODE (uloc) == SET)
			{
			  src = SET_SRC (uloc);
			  dst = SET_DEST (uloc);
			}

		      if (copied_p)
			{
			  if (flag_var_tracking_uninit)
			    {
			      status = find_src_status (in, src);

			      if (status == VAR_INIT_STATUS_UNKNOWN)
				status = find_src_status (out, src);
			    }

			  src = find_src_set_src (in, src);
			}

		      if (REG_P (dst))
			var_reg_delete_and_set (out, dst, !copied_p,
						status, srcv);
		      else if (MEM_P (dst))
			{
			  gcc_assert (MEM_P (dstv));
			  gcc_assert (MEM_ATTRS (dstv) == MEM_ATTRS (dst));
			  var_mem_delete_and_set (out, dstv, !copied_p,
						  status, srcv);
			}
		    }
		}
	      else if (REG_P (uloc))
		var_regno_delete (out, REGNO (uloc));
	      else if (MEM_P (uloc))
		{
		  gcc_checking_assert (GET_CODE (vloc) == MEM);
		  gcc_checking_assert (dstv == vloc);
		  if (dstv != vloc)
		    clobber_overlapping_mems (out, vloc);
		}

	      val_store (out, val, dstv, insn, true);
	    }
	    break;

	  case MO_SET:
	    {
	      rtx loc = mo->u.loc;
	      rtx set_src = NULL;

	      if (GET_CODE (loc) == SET)
		{
		  set_src = SET_SRC (loc);
		  loc = SET_DEST (loc);
		}

	      if (REG_P (loc))
		var_reg_delete_and_set (out, loc, true, VAR_INIT_STATUS_INITIALIZED,
					set_src);
	      else if (MEM_P (loc))
		var_mem_delete_and_set (out, loc, true, VAR_INIT_STATUS_INITIALIZED,
					set_src);
	    }
	    break;

	  case MO_COPY:
	    {
	      rtx loc = mo->u.loc;
	      enum var_init_status src_status;
	      rtx set_src = NULL;

	      if (GET_CODE (loc) == SET)
		{
		  set_src = SET_SRC (loc);
		  loc = SET_DEST (loc);
		}

	      if (! flag_var_tracking_uninit)
		src_status = VAR_INIT_STATUS_INITIALIZED;
	      else
		{
		  src_status = find_src_status (in, set_src);

		  if (src_status == VAR_INIT_STATUS_UNKNOWN)
		    src_status = find_src_status (out, set_src);
		}

	      set_src = find_src_set_src (in, set_src);

	      if (REG_P (loc))
		var_reg_delete_and_set (out, loc, false, src_status, set_src);
	      else if (MEM_P (loc))
		var_mem_delete_and_set (out, loc, false, src_status, set_src);
	    }
	    break;

	  case MO_USE_NO_VAR:
	    {
	      rtx loc = mo->u.loc;

	      if (REG_P (loc))
		var_reg_delete (out, loc, false);
	      else if (MEM_P (loc))
		var_mem_delete (out, loc, false);
	    }
	    break;

	  case MO_CLOBBER:
	    {
	      rtx loc = mo->u.loc;

	      if (REG_P (loc))
		var_reg_delete (out, loc, true);
	      else if (MEM_P (loc))
		var_mem_delete (out, loc, true);
	    }
	    break;

	  case MO_ADJUST:
	    out->stack_adjust += mo->u.adjust;
	    break;
	}
    }

  if (MAY_HAVE_DEBUG_INSNS)
    {
      pointer_map_destroy (local_get_addr_cache);
      local_get_addr_cache = NULL;

      dataflow_set_equiv_regs (out);
      shared_hash_htab (out->vars)
	.traverse <dataflow_set *, canonicalize_values_mark> (out);
      shared_hash_htab (out->vars)
	.traverse <dataflow_set *, canonicalize_values_star> (out);
#if ENABLE_CHECKING
      shared_hash_htab (out->vars)
	.traverse <dataflow_set *, canonicalize_loc_order_check> (out);
#endif
    }
  changed = dataflow_set_different (&old_out, out);
  dataflow_set_destroy (&old_out);
  return changed;
}

/* Find the locations of variables in the whole function.  */

static bool
vt_find_locations (void)
{
  fibheap_t worklist, pending, fibheap_swap;
  sbitmap visited, in_worklist, in_pending, sbitmap_swap;
  basic_block bb;
  edge e;
  int *bb_order;
  int *rc_order;
  int i;
  int htabsz = 0;
  int htabmax = PARAM_VALUE (PARAM_MAX_VARTRACK_SIZE);
  bool success = true;

  timevar_push (TV_VAR_TRACKING_DATAFLOW);
  /* Compute reverse completion order of depth first search of the CFG
     so that the data-flow runs faster.  */
  rc_order = XNEWVEC (int, n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS);
  bb_order = XNEWVEC (int, last_basic_block_for_fn (cfun));
  pre_and_rev_post_order_compute (NULL, rc_order, false);
  for (i = 0; i < n_basic_blocks_for_fn (cfun) - NUM_FIXED_BLOCKS; i++)
    bb_order[rc_order[i]] = i;
  free (rc_order);

  worklist = fibheap_new ();
  pending = fibheap_new ();
  visited = sbitmap_alloc (last_basic_block_for_fn (cfun));
  in_worklist = sbitmap_alloc (last_basic_block_for_fn (cfun));
  in_pending = sbitmap_alloc (last_basic_block_for_fn (cfun));
  bitmap_clear (in_worklist);

  FOR_EACH_BB_FN (bb, cfun)
    fibheap_insert (pending, bb_order[bb->index], bb);
  bitmap_ones (in_pending);

  while (success && !fibheap_empty (pending))
    {
      fibheap_swap = pending;
      pending = worklist;
      worklist = fibheap_swap;
      sbitmap_swap = in_pending;
      in_pending = in_worklist;
      in_worklist = sbitmap_swap;

      bitmap_clear (visited);

      while (!fibheap_empty (worklist))
	{
	  bb = (basic_block) fibheap_extract_min (worklist);
	  bitmap_clear_bit (in_worklist, bb->index);
	  gcc_assert (!bitmap_bit_p (visited, bb->index));
	  if (!bitmap_bit_p (visited, bb->index))
	    {
	      bool changed;
	      edge_iterator ei;
	      int oldinsz, oldoutsz;

	      bitmap_set_bit (visited, bb->index);

	      if (VTI (bb)->in.vars)
		{
		  htabsz
		    -= shared_hash_htab (VTI (bb)->in.vars).size ()
			+ shared_hash_htab (VTI (bb)->out.vars).size ();
		  oldinsz = shared_hash_htab (VTI (bb)->in.vars).elements ();
		  oldoutsz = shared_hash_htab (VTI (bb)->out.vars).elements ();
		}
	      else
		oldinsz = oldoutsz = 0;

	      if (MAY_HAVE_DEBUG_INSNS)
		{
		  dataflow_set *in = &VTI (bb)->in, *first_out = NULL;
		  bool first = true, adjust = false;

		  /* Calculate the IN set as the intersection of
		     predecessor OUT sets.  */

		  dataflow_set_clear (in);
		  dst_can_be_shared = true;

		  FOR_EACH_EDGE (e, ei, bb->preds)
		    if (!VTI (e->src)->flooded)
		      gcc_assert (bb_order[bb->index]
				  <= bb_order[e->src->index]);
		    else if (first)
		      {
			dataflow_set_copy (in, &VTI (e->src)->out);
			first_out = &VTI (e->src)->out;
			first = false;
		      }
		    else
		      {
			dataflow_set_merge (in, &VTI (e->src)->out);
			adjust = true;
		      }

		  if (adjust)
		    {
		      dataflow_post_merge_adjust (in, &VTI (bb)->permp);
#if ENABLE_CHECKING
		      /* Merge and merge_adjust should keep entries in
			 canonical order.  */
		      shared_hash_htab (in->vars)
			.traverse <dataflow_set *,
				   canonicalize_loc_order_check> (in);
#endif
		      if (dst_can_be_shared)
			{
			  shared_hash_destroy (in->vars);
			  in->vars = shared_hash_copy (first_out->vars);
			}
		    }

		  VTI (bb)->flooded = true;
		}
	      else
		{
		  /* Calculate the IN set as union of predecessor OUT sets.  */
		  dataflow_set_clear (&VTI (bb)->in);
		  FOR_EACH_EDGE (e, ei, bb->preds)
		    dataflow_set_union (&VTI (bb)->in, &VTI (e->src)->out);
		}

	      changed = compute_bb_dataflow (bb);
	      htabsz += shared_hash_htab (VTI (bb)->in.vars).size ()
			 + shared_hash_htab (VTI (bb)->out.vars).size ();

	      if (htabmax && htabsz > htabmax)
		{
		  if (MAY_HAVE_DEBUG_INSNS)
		    inform (DECL_SOURCE_LOCATION (cfun->decl),
			    "variable tracking size limit exceeded with "
			    "-fvar-tracking-assignments, retrying without");
		  else
		    inform (DECL_SOURCE_LOCATION (cfun->decl),
			    "variable tracking size limit exceeded");
		  success = false;
		  break;
		}

	      if (changed)
		{
		  FOR_EACH_EDGE (e, ei, bb->succs)
		    {
		      if (e->dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
			continue;

		      if (bitmap_bit_p (visited, e->dest->index))
			{
			  if (!bitmap_bit_p (in_pending, e->dest->index))
			    {
			      /* Send E->DEST to next round.  */
			      bitmap_set_bit (in_pending, e->dest->index);
			      fibheap_insert (pending,
					      bb_order[e->dest->index],
					      e->dest);
			    }
			}
		      else if (!bitmap_bit_p (in_worklist, e->dest->index))
			{
			  /* Add E->DEST to current round.  */
			  bitmap_set_bit (in_worklist, e->dest->index);
			  fibheap_insert (worklist, bb_order[e->dest->index],
					  e->dest);
			}
		    }
		}

	      if (dump_file)
		fprintf (dump_file,
			 "BB %i: in %i (was %i), out %i (was %i), rem %i + %i, tsz %i\n",
			 bb->index,
			 (int)shared_hash_htab (VTI (bb)->in.vars).size (),
			 oldinsz,
			 (int)shared_hash_htab (VTI (bb)->out.vars).size (),
			 oldoutsz,
			 (int)worklist->nodes, (int)pending->nodes, htabsz);

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "BB %i IN:\n", bb->index);
		  dump_dataflow_set (&VTI (bb)->in);
		  fprintf (dump_file, "BB %i OUT:\n", bb->index);
		  dump_dataflow_set (&VTI (bb)->out);
		}
	    }
	}
    }

  if (success && MAY_HAVE_DEBUG_INSNS)
    FOR_EACH_BB_FN (bb, cfun)
      gcc_assert (VTI (bb)->flooded);

  free (bb_order);
  fibheap_delete (worklist);
  fibheap_delete (pending);
  sbitmap_free (visited);
  sbitmap_free (in_worklist);
  sbitmap_free (in_pending);

  timevar_pop (TV_VAR_TRACKING_DATAFLOW);
  return success;
}

/* Print the content of the LIST to dump file.  */

static void
dump_attrs_list (attrs list)
{
  for (; list; list = list->next)
    {
      if (dv_is_decl_p (list->dv))
	print_mem_expr (dump_file, dv_as_decl (list->dv));
      else
	print_rtl_single (dump_file, dv_as_value (list->dv));
      fprintf (dump_file, "+" HOST_WIDE_INT_PRINT_DEC, list->offset);
    }
  fprintf (dump_file, "\n");
}

/* Print the information about variable *SLOT to dump file.  */

int
dump_var_tracking_slot (variable_def **slot, void *data ATTRIBUTE_UNUSED)
{
  variable var = *slot;

  dump_var (var);

  /* Continue traversing the hash table.  */
  return 1;
}

/* Print the information about variable VAR to dump file.  */

static void
dump_var (variable var)
{
  int i;
  location_chain node;

  if (dv_is_decl_p (var->dv))
    {
      const_tree decl = dv_as_decl (var->dv);

      if (DECL_NAME (decl))
	{
	  fprintf (dump_file, "  name: %s",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));
	  if (dump_flags & TDF_UID)
	    fprintf (dump_file, "D.%u", DECL_UID (decl));
	}
      else if (TREE_CODE (decl) == DEBUG_EXPR_DECL)
	fprintf (dump_file, "  name: D#%u", DEBUG_TEMP_UID (decl));
      else
	fprintf (dump_file, "  name: D.%u", DECL_UID (decl));
      fprintf (dump_file, "\n");
    }
  else
    {
      fputc (' ', dump_file);
      print_rtl_single (dump_file, dv_as_value (var->dv));
    }

  for (i = 0; i < var->n_var_parts; i++)
    {
      fprintf (dump_file, "    offset %ld\n",
	       (long)(var->onepart ? 0 : VAR_PART_OFFSET (var, i)));
      for (node = var->var_part[i].loc_chain; node; node = node->next)
	{
	  fprintf (dump_file, "      ");
	  if (node->init == VAR_INIT_STATUS_UNINITIALIZED)
	    fprintf (dump_file, "[uninit]");
	  print_rtl_single (dump_file, node->loc);
	}
    }
}

/* Print the information about variables from hash table VARS to dump file.  */

static void
dump_vars (variable_table_type vars)
{
  if (vars.elements () > 0)
    {
      fprintf (dump_file, "Variables:\n");
      vars.traverse <void *, dump_var_tracking_slot> (NULL);
    }
}

/* Print the dataflow set SET to dump file.  */

static void
dump_dataflow_set (dataflow_set *set)
{
  int i;

  fprintf (dump_file, "Stack adjustment: " HOST_WIDE_INT_PRINT_DEC "\n",
	   set->stack_adjust);
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (set->regs[i])
	{
	  fprintf (dump_file, "Reg %d:", i);
	  dump_attrs_list (set->regs[i]);
	}
    }
  dump_vars (shared_hash_htab (set->vars));
  fprintf (dump_file, "\n");
}

/* Print the IN and OUT sets for each basic block to dump file.  */

static void
dump_dataflow_sets (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      fprintf (dump_file, "\nBasic block %d:\n", bb->index);
      fprintf (dump_file, "IN:\n");
      dump_dataflow_set (&VTI (bb)->in);
      fprintf (dump_file, "OUT:\n");
      dump_dataflow_set (&VTI (bb)->out);
    }
}

/* Return the variable for DV in dropped_values, inserting one if
   requested with INSERT.  */

static inline variable
variable_from_dropped (decl_or_value dv, enum insert_option insert)
{
  variable_def **slot;
  variable empty_var;
  onepart_enum_t onepart;

  slot = dropped_values.find_slot_with_hash (dv, dv_htab_hash (dv), insert);

  if (!slot)
    return NULL;

  if (*slot)
    return *slot;

  gcc_checking_assert (insert == INSERT);

  onepart = dv_onepart_p (dv);

  gcc_checking_assert (onepart == ONEPART_VALUE || onepart == ONEPART_DEXPR);

  empty_var = (variable) pool_alloc (onepart_pool (onepart));
  empty_var->dv = dv;
  empty_var->refcount = 1;
  empty_var->n_var_parts = 0;
  empty_var->onepart = onepart;
  empty_var->in_changed_variables = false;
  empty_var->var_part[0].loc_chain = NULL;
  empty_var->var_part[0].cur_loc = NULL;
  VAR_LOC_1PAUX (empty_var) = NULL;
  set_dv_changed (dv, true);

  *slot = empty_var;

  return empty_var;
}

/* Recover the one-part aux from dropped_values.  */

static struct onepart_aux *
recover_dropped_1paux (variable var)
{
  variable dvar;

  gcc_checking_assert (var->onepart);

  if (VAR_LOC_1PAUX (var))
    return VAR_LOC_1PAUX (var);

  if (var->onepart == ONEPART_VDECL)
    return NULL;

  dvar = variable_from_dropped (var->dv, NO_INSERT);

  if (!dvar)
    return NULL;

  VAR_LOC_1PAUX (var) = VAR_LOC_1PAUX (dvar);
  VAR_LOC_1PAUX (dvar) = NULL;

  return VAR_LOC_1PAUX (var);
}

/* Add variable VAR to the hash table of changed variables and
   if it has no locations delete it from SET's hash table.  */

static void
variable_was_changed (variable var, dataflow_set *set)
{
  hashval_t hash = dv_htab_hash (var->dv);

  if (emit_notes)
    {
      variable_def **slot;

      /* Remember this decl or VALUE has been added to changed_variables.  */
      set_dv_changed (var->dv, true);

      slot = changed_variables.find_slot_with_hash (var->dv, hash, INSERT);

      if (*slot)
	{
	  variable old_var = *slot;
	  gcc_assert (old_var->in_changed_variables);
	  old_var->in_changed_variables = false;
	  if (var != old_var && var->onepart)
	    {
	      /* Restore the auxiliary info from an empty variable
		 previously created for changed_variables, so it is
		 not lost.  */
	      gcc_checking_assert (!VAR_LOC_1PAUX (var));
	      VAR_LOC_1PAUX (var) = VAR_LOC_1PAUX (old_var);
	      VAR_LOC_1PAUX (old_var) = NULL;
	    }
	  variable_htab_free (*slot);
	}

      if (set && var->n_var_parts == 0)
	{
	  onepart_enum_t onepart = var->onepart;
	  variable empty_var = NULL;
	  variable_def **dslot = NULL;

	  if (onepart == ONEPART_VALUE || onepart == ONEPART_DEXPR)
	    {
	      dslot = dropped_values.find_slot_with_hash (var->dv,
						dv_htab_hash (var->dv),
						INSERT);
	      empty_var = *dslot;

	      if (empty_var)
		{
		  gcc_checking_assert (!empty_var->in_changed_variables);
		  if (!VAR_LOC_1PAUX (var))
		    {
		      VAR_LOC_1PAUX (var) = VAR_LOC_1PAUX (empty_var);
		      VAR_LOC_1PAUX (empty_var) = NULL;
		    }
		  else
		    gcc_checking_assert (!VAR_LOC_1PAUX (empty_var));
		}
	    }

	  if (!empty_var)
	    {
	      empty_var = (variable) pool_alloc (onepart_pool (onepart));
	      empty_var->dv = var->dv;
	      empty_var->refcount = 1;
	      empty_var->n_var_parts = 0;
	      empty_var->onepart = onepart;
	      if (dslot)
		{
		  empty_var->refcount++;
		  *dslot = empty_var;
		}
	    }
	  else
	    empty_var->refcount++;
	  empty_var->in_changed_variables = true;
	  *slot = empty_var;
	  if (onepart)
	    {
	      empty_var->var_part[0].loc_chain = NULL;
	      empty_var->var_part[0].cur_loc = NULL;
	      VAR_LOC_1PAUX (empty_var) = VAR_LOC_1PAUX (var);
	      VAR_LOC_1PAUX (var) = NULL;
	    }
	  goto drop_var;
	}
      else
	{
	  if (var->onepart && !VAR_LOC_1PAUX (var))
	    recover_dropped_1paux (var);
	  var->refcount++;
	  var->in_changed_variables = true;
	  *slot = var;
	}
    }
  else
    {
      gcc_assert (set);
      if (var->n_var_parts == 0)
	{
	  variable_def **slot;

	drop_var:
	  slot = shared_hash_find_slot_noinsert (set->vars, var->dv);
	  if (slot)
	    {
	      if (shared_hash_shared (set->vars))
		slot = shared_hash_find_slot_unshare (&set->vars, var->dv,
						      NO_INSERT);
	      shared_hash_htab (set->vars).clear_slot (slot);
	    }
	}
    }
}

/* Look for the index in VAR->var_part corresponding to OFFSET.
   Return -1 if not found.  If INSERTION_POINT is non-NULL, the
   referenced int will be set to the index that the part has or should
   have, if it should be inserted.  */

static inline int
find_variable_location_part (variable var, HOST_WIDE_INT offset,
			     int *insertion_point)
{
  int pos, low, high;

  if (var->onepart)
    {
      if (offset != 0)
	return -1;

      if (insertion_point)
	*insertion_point = 0;

      return var->n_var_parts - 1;
    }

  /* Find the location part.  */
  low = 0;
  high = var->n_var_parts;
  while (low != high)
    {
      pos = (low + high) / 2;
      if (VAR_PART_OFFSET (var, pos) < offset)
	low = pos + 1;
      else
	high = pos;
    }
  pos = low;

  if (insertion_point)
    *insertion_point = pos;

  if (pos < var->n_var_parts && VAR_PART_OFFSET (var, pos) == offset)
    return pos;

  return -1;
}

static variable_def **
set_slot_part (dataflow_set *set, rtx loc, variable_def **slot,
	       decl_or_value dv, HOST_WIDE_INT offset,
	       enum var_init_status initialized, rtx set_src)
{
  int pos;
  location_chain node, next;
  location_chain *nextp;
  variable var;
  onepart_enum_t onepart;

  var = *slot;

  if (var)
    onepart = var->onepart;
  else
    onepart = dv_onepart_p (dv);

  gcc_checking_assert (offset == 0 || !onepart);
  gcc_checking_assert (loc != dv_as_opaque (dv));

  if (! flag_var_tracking_uninit)
    initialized = VAR_INIT_STATUS_INITIALIZED;

  if (!var)
    {
      /* Create new variable information.  */
      var = (variable) pool_alloc (onepart_pool (onepart));
      var->dv = dv;
      var->refcount = 1;
      var->n_var_parts = 1;
      var->onepart = onepart;
      var->in_changed_variables = false;
      if (var->onepart)
	VAR_LOC_1PAUX (var) = NULL;
      else
	VAR_PART_OFFSET (var, 0) = offset;
      var->var_part[0].loc_chain = NULL;
      var->var_part[0].cur_loc = NULL;
      *slot = var;
      pos = 0;
      nextp = &var->var_part[0].loc_chain;
    }
  else if (onepart)
    {
      int r = -1, c = 0;

      gcc_assert (dv_as_opaque (var->dv) == dv_as_opaque (dv));

      pos = 0;

      if (GET_CODE (loc) == VALUE)
	{
	  for (nextp = &var->var_part[0].loc_chain; (node = *nextp);
	       nextp = &node->next)
	    if (GET_CODE (node->loc) == VALUE)
	      {
		if (node->loc == loc)
		  {
		    r = 0;
		    break;
		  }
		if (canon_value_cmp (node->loc, loc))
		  c++;
		else
		  {
		    r = 1;
		    break;
		  }
	      }
	    else if (REG_P (node->loc) || MEM_P (node->loc))
	      c++;
	    else
	      {
		r = 1;
		break;
	      }
	}
      else if (REG_P (loc))
	{
	  for (nextp = &var->var_part[0].loc_chain; (node = *nextp);
	       nextp = &node->next)
	    if (REG_P (node->loc))
	      {
		if (REGNO (node->loc) < REGNO (loc))
		  c++;
		else
		  {
		    if (REGNO (node->loc) == REGNO (loc))
		      r = 0;
		    else
		      r = 1;
		    break;
		  }
	      }
	    else
	      {
		r = 1;
		break;
	      }
	}
      else if (MEM_P (loc))
	{
	  for (nextp = &var->var_part[0].loc_chain; (node = *nextp);
	       nextp = &node->next)
	    if (REG_P (node->loc))
	      c++;
	    else if (MEM_P (node->loc))
	      {
		if ((r = loc_cmp (XEXP (node->loc, 0), XEXP (loc, 0))) >= 0)
		  break;
		else
		  c++;
	      }
	    else
	      {
		r = 1;
		break;
	      }
	}
      else
	for (nextp = &var->var_part[0].loc_chain; (node = *nextp);
	     nextp = &node->next)
	  if ((r = loc_cmp (node->loc, loc)) >= 0)
	    break;
	  else
	    c++;

      if (r == 0)
	return slot;

      if (shared_var_p (var, set->vars))
	{
	  slot = unshare_variable (set, slot, var, initialized);
	  var = *slot;
	  for (nextp = &var->var_part[0].loc_chain; c;
	       nextp = &(*nextp)->next)
	    c--;
	  gcc_assert ((!node && !*nextp) || node->loc == (*nextp)->loc);
	}
    }
  else
    {
      int inspos = 0;

      gcc_assert (dv_as_decl (var->dv) == dv_as_decl (dv));

      pos = find_variable_location_part (var, offset, &inspos);

      if (pos >= 0)
	{
	  node = var->var_part[pos].loc_chain;

	  if (node
	      && ((REG_P (node->loc) && REG_P (loc)
		   && REGNO (node->loc) == REGNO (loc))
		  || rtx_equal_p (node->loc, loc)))
	    {
	      /* LOC is in the beginning of the chain so we have nothing
		 to do.  */
	      if (node->init < initialized)
		node->init = initialized;
	      if (set_src != NULL)
		node->set_src = set_src;

	      return slot;
	    }
	  else
	    {
	      /* We have to make a copy of a shared variable.  */
	      if (shared_var_p (var, set->vars))
		{
		  slot = unshare_variable (set, slot, var, initialized);
		  var = *slot;
		}
	    }
	}
      else
	{
	  /* We have not found the location part, new one will be created.  */

	  /* We have to make a copy of the shared variable.  */
	  if (shared_var_p (var, set->vars))
	    {
	      slot = unshare_variable (set, slot, var, initialized);
	      var = *slot;
	    }

	  /* We track only variables whose size is <= MAX_VAR_PARTS bytes
	     thus there are at most MAX_VAR_PARTS different offsets.  */
	  gcc_assert (var->n_var_parts < MAX_VAR_PARTS
		      && (!var->n_var_parts || !onepart));

	  /* We have to move the elements of array starting at index
	     inspos to the next position.  */
	  for (pos = var->n_var_parts; pos > inspos; pos--)
	    var->var_part[pos] = var->var_part[pos - 1];

	  var->n_var_parts++;
	  gcc_checking_assert (!onepart);
	  VAR_PART_OFFSET (var, pos) = offset;
	  var->var_part[pos].loc_chain = NULL;
	  var->var_part[pos].cur_loc = NULL;
	}

      /* Delete the location from the list.  */
      nextp = &var->var_part[pos].loc_chain;
      for (node = var->var_part[pos].loc_chain; node; node = next)
	{
	  next = node->next;
	  if ((REG_P (node->loc) && REG_P (loc)
	       && REGNO (node->loc) == REGNO (loc))
	      || rtx_equal_p (node->loc, loc))
	    {
	      /* Save these values, to assign to the new node, before
		 deleting this one.  */
	      if (node->init > initialized)
		initialized = node->init;
	      if (node->set_src != NULL && set_src == NULL)
		set_src = node->set_src;
	      if (var->var_part[pos].cur_loc == node->loc)
		var->var_part[pos].cur_loc = NULL;
	      pool_free (loc_chain_pool, node);
	      *nextp = next;
	      break;
	    }
	  else
	    nextp = &node->next;
	}

      nextp = &var->var_part[pos].loc_chain;
    }

  /* Add the location to the beginning.  */
  node = (location_chain) pool_alloc (loc_chain_pool);
  node->loc = loc;
  node->init = initialized;
  node->set_src = set_src;
  node->next = *nextp;
  *nextp = node;

  /* If no location was emitted do so.  */
  if (var->var_part[pos].cur_loc == NULL)
    variable_was_changed (var, set);

  return slot;
}

/* Set the part of variable's location in the dataflow set SET.  The
   variable part is specified by variable's declaration in DV and
   offset OFFSET and the part's location by LOC.  IOPT should be
   NO_INSERT if the variable is known to be in SET already and the
   variable hash table must not be resized, and INSERT otherwise.  */

static void
set_variable_part (dataflow_set *set, rtx loc,
		   decl_or_value dv, HOST_WIDE_INT offset,
		   enum var_init_status initialized, rtx set_src,
		   enum insert_option iopt)
{
  variable_def **slot;

  if (iopt == NO_INSERT)
    slot = shared_hash_find_slot_noinsert (set->vars, dv);
  else
    {
      slot = shared_hash_find_slot (set->vars, dv);
      if (!slot)
	slot = shared_hash_find_slot_unshare (&set->vars, dv, iopt);
    }
  set_slot_part (set, loc, slot, dv, offset, initialized, set_src);
}

/* Remove all recorded register locations for the given variable part
   from dataflow set SET, except for those that are identical to loc.
   The variable part is specified by variable's declaration or value
   DV and offset OFFSET.  */

static variable_def **
clobber_slot_part (dataflow_set *set, rtx loc, variable_def **slot,
		   HOST_WIDE_INT offset, rtx set_src)
{
  variable var = *slot;
  int pos = find_variable_location_part (var, offset, NULL);

  if (pos >= 0)
    {
      location_chain node, next;

      /* Remove the register locations from the dataflow set.  */
      next = var->var_part[pos].loc_chain;
      for (node = next; node; node = next)
	{
	  next = node->next;
	  if (node->loc != loc
	      && (!flag_var_tracking_uninit
		  || !set_src
		  || MEM_P (set_src)
		  || !rtx_equal_p (set_src, node->set_src)))
	    {
	      if (REG_P (node->loc))
		{
		  attrs anode, anext;
		  attrs *anextp;

		  /* Remove the variable part from the register's
		     list, but preserve any other variable parts
		     that might be regarded as live in that same
		     register.  */
		  anextp = &set->regs[REGNO (node->loc)];
		  for (anode = *anextp; anode; anode = anext)
		    {
		      anext = anode->next;
		      if (dv_as_opaque (anode->dv) == dv_as_opaque (var->dv)
			  && anode->offset == offset)
			{
			  pool_free (attrs_pool, anode);
			  *anextp = anext;
			}
		      else
			anextp = &anode->next;
		    }
		}

	      slot = delete_slot_part (set, node->loc, slot, offset);
	    }
	}
    }

  return slot;
}

/* Remove all recorded register locations for the given variable part
   from dataflow set SET, except for those that are identical to loc.
   The variable part is specified by variable's declaration or value
   DV and offset OFFSET.  */

static void
clobber_variable_part (dataflow_set *set, rtx loc, decl_or_value dv,
		       HOST_WIDE_INT offset, rtx set_src)
{
  variable_def **slot;

  if (!dv_as_opaque (dv)
      || (!dv_is_value_p (dv) && ! DECL_P (dv_as_decl (dv))))
    return;

  slot = shared_hash_find_slot_noinsert (set->vars, dv);
  if (!slot)
    return;

  clobber_slot_part (set, loc, slot, offset, set_src);
}

/* Delete the part of variable's location from dataflow set SET.  The
   variable part is specified by its SET->vars slot SLOT and offset
   OFFSET and the part's location by LOC.  */

static variable_def **
delete_slot_part (dataflow_set *set, rtx loc, variable_def **slot,
		  HOST_WIDE_INT offset)
{
  variable var = *slot;
  int pos = find_variable_location_part (var, offset, NULL);

  if (pos >= 0)
    {
      location_chain node, next;
      location_chain *nextp;
      bool changed;
      rtx cur_loc;

      if (shared_var_p (var, set->vars))
	{
	  /* If the variable contains the location part we have to
	     make a copy of the variable.  */
	  for (node = var->var_part[pos].loc_chain; node;
	       node = node->next)
	    {
	      if ((REG_P (node->loc) && REG_P (loc)
		   && REGNO (node->loc) == REGNO (loc))
		  || rtx_equal_p (node->loc, loc))
		{
		  slot = unshare_variable (set, slot, var,
					   VAR_INIT_STATUS_UNKNOWN);
		  var = *slot;
		  break;
		}
	    }
	}

      if (pos == 0 && var->onepart && VAR_LOC_1PAUX (var))
	cur_loc = VAR_LOC_FROM (var);
      else
	cur_loc = var->var_part[pos].cur_loc;

      /* Delete the location part.  */
      changed = false;
      nextp = &var->var_part[pos].loc_chain;
      for (node = *nextp; node; node = next)
	{
	  next = node->next;
	  if ((REG_P (node->loc) && REG_P (loc)
	       && REGNO (node->loc) == REGNO (loc))
	      || rtx_equal_p (node->loc, loc))
	    {
	      /* If we have deleted the location which was last emitted
		 we have to emit new location so add the variable to set
		 of changed variables.  */
	      if (cur_loc == node->loc)
		{
		  changed = true;
		  var->var_part[pos].cur_loc = NULL;
		  if (pos == 0 && var->onepart && VAR_LOC_1PAUX (var))
		    VAR_LOC_FROM (var) = NULL;
		}
	      pool_free (loc_chain_pool, node);
	      *nextp = next;
	      break;
	    }
	  else
	    nextp = &node->next;
	}

      if (var->var_part[pos].loc_chain == NULL)
	{
	  changed = true;
	  var->n_var_parts--;
	  while (pos < var->n_var_parts)
	    {
	      var->var_part[pos] = var->var_part[pos + 1];
	      pos++;
	    }
	}
      if (changed)
	variable_was_changed (var, set);
    }

  return slot;
}

/* Delete the part of variable's location from dataflow set SET.  The
   variable part is specified by variable's declaration or value DV
   and offset OFFSET and the part's location by LOC.  */

static void
delete_variable_part (dataflow_set *set, rtx loc, decl_or_value dv,
		      HOST_WIDE_INT offset)
{
  variable_def **slot = shared_hash_find_slot_noinsert (set->vars, dv);
  if (!slot)
    return;

  delete_slot_part (set, loc, slot, offset);
}


/* Structure for passing some other parameters to function
   vt_expand_loc_callback.  */
struct expand_loc_callback_data
{
  /* The variables and values active at this point.  */
  variable_table_type vars;

  /* Stack of values and debug_exprs under expansion, and their
     children.  */
  auto_vec<rtx, 4> expanding;

  /* Stack of values and debug_exprs whose expansion hit recursion
     cycles.  They will have VALUE_RECURSED_INTO marked when added to
     this list.  This flag will be cleared if any of its dependencies
     resolves to a valid location.  So, if the flag remains set at the
     end of the search, we know no valid location for this one can
     possibly exist.  */
  auto_vec<rtx, 4> pending;

  /* The maximum depth among the sub-expressions under expansion.
     Zero indicates no expansion so far.  */
  expand_depth depth;
};

/* Allocate the one-part auxiliary data structure for VAR, with enough
   room for COUNT dependencies.  */

static void
loc_exp_dep_alloc (variable var, int count)
{
  size_t allocsize;

  gcc_checking_assert (var->onepart);

  /* We can be called with COUNT == 0 to allocate the data structure
     without any dependencies, e.g. for the backlinks only.  However,
     if we are specifying a COUNT, then the dependency list must have
     been emptied before.  It would be possible to adjust pointers or
     force it empty here, but this is better done at an earlier point
     in the algorithm, so we instead leave an assertion to catch
     errors.  */
  gcc_checking_assert (!count
		       || VAR_LOC_DEP_VEC (var) == NULL
		       || VAR_LOC_DEP_VEC (var)->is_empty ());

  if (VAR_LOC_1PAUX (var) && VAR_LOC_DEP_VEC (var)->space (count))
    return;

  allocsize = offsetof (struct onepart_aux, deps)
	      + vec<loc_exp_dep, va_heap, vl_embed>::embedded_size (count);

  if (VAR_LOC_1PAUX (var))
    {
      VAR_LOC_1PAUX (var) = XRESIZEVAR (struct onepart_aux,
					VAR_LOC_1PAUX (var), allocsize);
      /* If the reallocation moves the onepaux structure, the
	 back-pointer to BACKLINKS in the first list member will still
	 point to its old location.  Adjust it.  */
      if (VAR_LOC_DEP_LST (var))
	VAR_LOC_DEP_LST (var)->pprev = VAR_LOC_DEP_LSTP (var);
    }
  else
    {
      VAR_LOC_1PAUX (var) = XNEWVAR (struct onepart_aux, allocsize);
      *VAR_LOC_DEP_LSTP (var) = NULL;
      VAR_LOC_FROM (var) = NULL;
      VAR_LOC_DEPTH (var).complexity = 0;
      VAR_LOC_DEPTH (var).entryvals = 0;
    }
  VAR_LOC_DEP_VEC (var)->embedded_init (count);
}

/* Remove all entries from the vector of active dependencies of VAR,
   removing them from the back-links lists too.  */

static void
loc_exp_dep_clear (variable var)
{
  while (VAR_LOC_DEP_VEC (var) && !VAR_LOC_DEP_VEC (var)->is_empty ())
    {
      loc_exp_dep *led = &VAR_LOC_DEP_VEC (var)->last ();
      if (led->next)
	led->next->pprev = led->pprev;
      if (led->pprev)
	*led->pprev = led->next;
      VAR_LOC_DEP_VEC (var)->pop ();
    }
}

/* Insert an active dependency from VAR on X to the vector of
   dependencies, and add the corresponding back-link to X's list of
   back-links in VARS.  */

static void
loc_exp_insert_dep (variable var, rtx x, variable_table_type vars)
{
  decl_or_value dv;
  variable xvar;
  loc_exp_dep *led;

  dv = dv_from_rtx (x);

  /* ??? Build a vector of variables parallel to EXPANDING, to avoid
     an additional look up?  */
  xvar = vars.find_with_hash (dv, dv_htab_hash (dv));

  if (!xvar)
    {
      xvar = variable_from_dropped (dv, NO_INSERT);
      gcc_checking_assert (xvar);
    }

  /* No point in adding the same backlink more than once.  This may
     arise if say the same value appears in two complex expressions in
     the same loc_list, or even more than once in a single
     expression.  */
  if (VAR_LOC_DEP_LST (xvar) && VAR_LOC_DEP_LST (xvar)->dv == var->dv)
    return;

  if (var->onepart == NOT_ONEPART)
    led = (loc_exp_dep *) pool_alloc (loc_exp_dep_pool);
  else
    {
      loc_exp_dep empty;
      memset (&empty, 0, sizeof (empty));
      VAR_LOC_DEP_VEC (var)->quick_push (empty);
      led = &VAR_LOC_DEP_VEC (var)->last ();
    }
  led->dv = var->dv;
  led->value = x;

  loc_exp_dep_alloc (xvar, 0);
  led->pprev = VAR_LOC_DEP_LSTP (xvar);
  led->next = *led->pprev;
  if (led->next)
    led->next->pprev = &led->next;
  *led->pprev = led;
}

/* Create active dependencies of VAR on COUNT values starting at
   VALUE, and corresponding back-links to the entries in VARS.  Return
   true if we found any pending-recursion results.  */

static bool
loc_exp_dep_set (variable var, rtx result, rtx *value, int count,
		 variable_table_type vars)
{
  bool pending_recursion = false;

  gcc_checking_assert (VAR_LOC_DEP_VEC (var) == NULL
		       || VAR_LOC_DEP_VEC (var)->is_empty ());

  /* Set up all dependencies from last_child (as set up at the end of
     the loop above) to the end.  */
  loc_exp_dep_alloc (var, count);

  while (count--)
    {
      rtx x = *value++;

      if (!pending_recursion)
	pending_recursion = !result && VALUE_RECURSED_INTO (x);

      loc_exp_insert_dep (var, x, vars);
    }

  return pending_recursion;
}

/* Notify the back-links of IVAR that are pending recursion that we
   have found a non-NIL value for it, so they are cleared for another
   attempt to compute a current location.  */

static void
notify_dependents_of_resolved_value (variable ivar, variable_table_type vars)
{
  loc_exp_dep *led, *next;

  for (led = VAR_LOC_DEP_LST (ivar); led; led = next)
    {
      decl_or_value dv = led->dv;
      variable var;

      next = led->next;

      if (dv_is_value_p (dv))
	{
	  rtx value = dv_as_value (dv);

	  /* If we have already resolved it, leave it alone.  */
	  if (!VALUE_RECURSED_INTO (value))
	    continue;

	  /* Check that VALUE_RECURSED_INTO, true from the test above,
	     implies NO_LOC_P.  */
	  gcc_checking_assert (NO_LOC_P (value));

	  /* We won't notify variables that are being expanded,
	     because their dependency list is cleared before
	     recursing.  */
	  NO_LOC_P (value) = false;
	  VALUE_RECURSED_INTO (value) = false;

	  gcc_checking_assert (dv_changed_p (dv));
	}
      else
	{
	  gcc_checking_assert (dv_onepart_p (dv) != NOT_ONEPART);
	  if (!dv_changed_p (dv))
	    continue;
      }

      var = vars.find_with_hash (dv, dv_htab_hash (dv));

      if (!var)
	var = variable_from_dropped (dv, NO_INSERT);

      if (var)
	notify_dependents_of_resolved_value (var, vars);

      if (next)
	next->pprev = led->pprev;
      if (led->pprev)
	*led->pprev = next;
      led->next = NULL;
      led->pprev = NULL;
    }
}

static rtx vt_expand_loc_callback (rtx x, bitmap regs,
				   int max_depth, void *data);

/* Return the combined depth, when one sub-expression evaluated to
   BEST_DEPTH and the previous known depth was SAVED_DEPTH.  */

static inline expand_depth
update_depth (expand_depth saved_depth, expand_depth best_depth)
{
  /* If we didn't find anything, stick with what we had.  */
  if (!best_depth.complexity)
    return saved_depth;

  /* If we found hadn't found anything, use the depth of the current
     expression.  Do NOT add one extra level, we want to compute the
     maximum depth among sub-expressions.  We'll increment it later,
     if appropriate.  */
  if (!saved_depth.complexity)
    return best_depth;

  /* Combine the entryval count so that regardless of which one we
     return, the entryval count is accurate.  */
  best_depth.entryvals = saved_depth.entryvals
    = best_depth.entryvals + saved_depth.entryvals;

  if (saved_depth.complexity < best_depth.complexity)
    return best_depth;
  else
    return saved_depth;
}

/* Expand VAR to a location RTX, updating its cur_loc.  Use REGS and
   DATA for cselib expand callback.  If PENDRECP is given, indicate in
   it whether any sub-expression couldn't be fully evaluated because
   it is pending recursion resolution.  */

static inline rtx
vt_expand_var_loc_chain (variable var, bitmap regs, void *data, bool *pendrecp)
{
  struct expand_loc_callback_data *elcd
    = (struct expand_loc_callback_data *) data;
  location_chain loc, next;
  rtx result = NULL;
  int first_child, result_first_child, last_child;
  bool pending_recursion;
  rtx loc_from = NULL;
  struct elt_loc_list *cloc = NULL;
  expand_depth depth = { 0, 0 }, saved_depth = elcd->depth;
  int wanted_entryvals, found_entryvals = 0;

  /* Clear all backlinks pointing at this, so that we're not notified
     while we're active.  */
  loc_exp_dep_clear (var);

 retry:
  if (var->onepart == ONEPART_VALUE)
    {
      cselib_val *val = CSELIB_VAL_PTR (dv_as_value (var->dv));

      gcc_checking_assert (cselib_preserved_value_p (val));

      cloc = val->locs;
    }

  first_child = result_first_child = last_child
    = elcd->expanding.length ();

  wanted_entryvals = found_entryvals;

  /* Attempt to expand each available location in turn.  */
  for (next = loc = var->n_var_parts ? var->var_part[0].loc_chain : NULL;
       loc || cloc; loc = next)
    {
      result_first_child = last_child;

      if (!loc)
	{
	  loc_from = cloc->loc;
	  next = loc;
	  cloc = cloc->next;
	  if (unsuitable_loc (loc_from))
	    continue;
	}
      else
	{
	  loc_from = loc->loc;
	  next = loc->next;
	}

      gcc_checking_assert (!unsuitable_loc (loc_from));

      elcd->depth.complexity = elcd->depth.entryvals = 0;
      result = cselib_expand_value_rtx_cb (loc_from, regs, EXPR_DEPTH,
					   vt_expand_loc_callback, data);
      last_child = elcd->expanding.length ();

      if (result)
	{
	  depth = elcd->depth;

	  gcc_checking_assert (depth.complexity
			       || result_first_child == last_child);

	  if (last_child - result_first_child != 1)
	    {
	      if (!depth.complexity && GET_CODE (result) == ENTRY_VALUE)
		depth.entryvals++;
	      depth.complexity++;
	    }

	  if (depth.complexity <= EXPR_USE_DEPTH)
	    {
	      if (depth.entryvals <= wanted_entryvals)
		break;
	      else if (!found_entryvals || depth.entryvals < found_entryvals)
		found_entryvals = depth.entryvals;
	    }

	  result = NULL;
	}

      /* Set it up in case we leave the loop.  */
      depth.complexity = depth.entryvals = 0;
      loc_from = NULL;
      result_first_child = first_child;
    }

  if (!loc_from && wanted_entryvals < found_entryvals)
    {
      /* We found entries with ENTRY_VALUEs and skipped them.  Since
	 we could not find any expansions without ENTRY_VALUEs, but we
	 found at least one with them, go back and get an entry with
	 the minimum number ENTRY_VALUE count that we found.  We could
	 avoid looping, but since each sub-loc is already resolved,
	 the re-expansion should be trivial.  ??? Should we record all
	 attempted locs as dependencies, so that we retry the
	 expansion should any of them change, in the hope it can give
	 us a new entry without an ENTRY_VALUE?  */
      elcd->expanding.truncate (first_child);
      goto retry;
    }

  /* Register all encountered dependencies as active.  */
  pending_recursion = loc_exp_dep_set
    (var, result, elcd->expanding.address () + result_first_child,
     last_child - result_first_child, elcd->vars);

  elcd->expanding.truncate (first_child);

  /* Record where the expansion came from.  */
  gcc_checking_assert (!result || !pending_recursion);
  VAR_LOC_FROM (var) = loc_from;
  VAR_LOC_DEPTH (var) = depth;

  gcc_checking_assert (!depth.complexity == !result);

  elcd->depth = update_depth (saved_depth, depth);

  /* Indicate whether any of the dependencies are pending recursion
     resolution.  */
  if (pendrecp)
    *pendrecp = pending_recursion;

  if (!pendrecp || !pending_recursion)
    var->var_part[0].cur_loc = result;

  return result;
}

/* Callback for cselib_expand_value, that looks for expressions
   holding the value in the var-tracking hash tables.  Return X for
   standard processing, anything else is to be used as-is.  */

static rtx
vt_expand_loc_callback (rtx x, bitmap regs,
			int max_depth ATTRIBUTE_UNUSED,
			void *data)
{
  struct expand_loc_callback_data *elcd
    = (struct expand_loc_callback_data *) data;
  decl_or_value dv;
  variable var;
  rtx result, subreg;
  bool pending_recursion = false;
  bool from_empty = false;

  switch (GET_CODE (x))
    {
    case SUBREG:
      subreg = cselib_expand_value_rtx_cb (SUBREG_REG (x), regs,
					   EXPR_DEPTH,
					   vt_expand_loc_callback, data);

      if (!subreg)
	return NULL;

      result = simplify_gen_subreg (GET_MODE (x), subreg,
				    GET_MODE (SUBREG_REG (x)),
				    SUBREG_BYTE (x));

      /* Invalid SUBREGs are ok in debug info.  ??? We could try
	 alternate expansions for the VALUE as well.  */
      if (!result)
	result = gen_rtx_raw_SUBREG (GET_MODE (x), subreg, SUBREG_BYTE (x));

      return result;

    case DEBUG_EXPR:
    case VALUE:
      dv = dv_from_rtx (x);
      break;

    default:
      return x;
    }

  elcd->expanding.safe_push (x);

  /* Check that VALUE_RECURSED_INTO implies NO_LOC_P.  */
  gcc_checking_assert (!VALUE_RECURSED_INTO (x) || NO_LOC_P (x));

  if (NO_LOC_P (x))
    {
      gcc_checking_assert (VALUE_RECURSED_INTO (x) || !dv_changed_p (dv));
      return NULL;
    }

  var = elcd->vars.find_with_hash (dv, dv_htab_hash (dv));

  if (!var)
    {
      from_empty = true;
      var = variable_from_dropped (dv, INSERT);
    }

  gcc_checking_assert (var);

  if (!dv_changed_p (dv))
    {
      gcc_checking_assert (!NO_LOC_P (x));
      gcc_checking_assert (var->var_part[0].cur_loc);
      gcc_checking_assert (VAR_LOC_1PAUX (var));
      gcc_checking_assert (VAR_LOC_1PAUX (var)->depth.complexity);

      elcd->depth = update_depth (elcd->depth, VAR_LOC_1PAUX (var)->depth);

      return var->var_part[0].cur_loc;
    }

  VALUE_RECURSED_INTO (x) = true;
  /* This is tentative, but it makes some tests simpler.  */
  NO_LOC_P (x) = true;

  gcc_checking_assert (var->n_var_parts == 1 || from_empty);

  result = vt_expand_var_loc_chain (var, regs, data, &pending_recursion);

  if (pending_recursion)
    {
      gcc_checking_assert (!result);
      elcd->pending.safe_push (x);
    }
  else
    {
      NO_LOC_P (x) = !result;
      VALUE_RECURSED_INTO (x) = false;
      set_dv_changed (dv, false);

      if (result)
	notify_dependents_of_resolved_value (var, elcd->vars);
    }

  return result;
}

/* While expanding variables, we may encounter recursion cycles
   because of mutual (possibly indirect) dependencies between two
   particular variables (or values), say A and B.  If we're trying to
   expand A when we get to B, which in turn attempts to expand A, if
   we can't find any other expansion for B, we'll add B to this
   pending-recursion stack, and tentatively return NULL for its
   location.  This tentative value will be used for any other
   occurrences of B, unless A gets some other location, in which case
   it will notify B that it is worth another try at computing a
   location for it, and it will use the location computed for A then.
   At the end of the expansion, the tentative NULL locations become
   final for all members of PENDING that didn't get a notification.
   This function performs this finalization of NULL locations.  */

static void
resolve_expansions_pending_recursion (vec<rtx, va_heap> *pending)
{
  while (!pending->is_empty ())
    {
      rtx x = pending->pop ();
      decl_or_value dv;

      if (!VALUE_RECURSED_INTO (x))
	continue;

      gcc_checking_assert (NO_LOC_P (x));
      VALUE_RECURSED_INTO (x) = false;
      dv = dv_from_rtx (x);
      gcc_checking_assert (dv_changed_p (dv));
      set_dv_changed (dv, false);
    }
}

/* Initialize expand_loc_callback_data D with variable hash table V.
   It must be a macro because of alloca (vec stack).  */
#define INIT_ELCD(d, v)						\
  do								\
    {								\
      (d).vars = (v);						\
      (d).depth.complexity = (d).depth.entryvals = 0;		\
    }								\
  while (0)
/* Finalize expand_loc_callback_data D, resolved to location L.  */
#define FINI_ELCD(d, l)						\
  do								\
    {								\
      resolve_expansions_pending_recursion (&(d).pending);	\
      (d).pending.release ();					\
      (d).expanding.release ();					\
								\
      if ((l) && MEM_P (l))					\
	(l) = targetm.delegitimize_address (l);			\
    }								\
  while (0)

/* Expand VALUEs and DEBUG_EXPRs in LOC to a location, using the
   equivalences in VARS, updating their CUR_LOCs in the process.  */

static rtx
vt_expand_loc (rtx loc, variable_table_type vars)
{
  struct expand_loc_callback_data data;
  rtx result;

  if (!MAY_HAVE_DEBUG_INSNS)
    return loc;

  INIT_ELCD (data, vars);

  result = cselib_expand_value_rtx_cb (loc, scratch_regs, EXPR_DEPTH,
				       vt_expand_loc_callback, &data);

  FINI_ELCD (data, result);

  return result;
}

/* Expand the one-part VARiable to a location, using the equivalences
   in VARS, updating their CUR_LOCs in the process.  */

static rtx
vt_expand_1pvar (variable var, variable_table_type vars)
{
  struct expand_loc_callback_data data;
  rtx loc;

  gcc_checking_assert (var->onepart && var->n_var_parts == 1);

  if (!dv_changed_p (var->dv))
    return var->var_part[0].cur_loc;

  INIT_ELCD (data, vars);

  loc = vt_expand_var_loc_chain (var, scratch_regs, &data, NULL);

  gcc_checking_assert (data.expanding.is_empty ());

  FINI_ELCD (data, loc);

  return loc;
}

/* Emit the NOTE_INSN_VAR_LOCATION for variable *VARP.  DATA contains
   additional parameters: WHERE specifies whether the note shall be emitted
   before or after instruction INSN.  */

int
emit_note_insn_var_location (variable_def **varp, emit_note_data *data)
{
  variable var = *varp;
  rtx insn = data->insn;
  enum emit_note_where where = data->where;
  variable_table_type vars = data->vars;
  rtx note, note_vl;
  int i, j, n_var_parts;
  bool complete;
  enum var_init_status initialized = VAR_INIT_STATUS_UNINITIALIZED;
  HOST_WIDE_INT last_limit;
  tree type_size_unit;
  HOST_WIDE_INT offsets[MAX_VAR_PARTS];
  rtx loc[MAX_VAR_PARTS];
  tree decl;
  location_chain lc;

  gcc_checking_assert (var->onepart == NOT_ONEPART
		       || var->onepart == ONEPART_VDECL);

  decl = dv_as_decl (var->dv);

  complete = true;
  last_limit = 0;
  n_var_parts = 0;
  if (!var->onepart)
    for (i = 0; i < var->n_var_parts; i++)
      if (var->var_part[i].cur_loc == NULL && var->var_part[i].loc_chain)
	var->var_part[i].cur_loc = var->var_part[i].loc_chain->loc;
  for (i = 0; i < var->n_var_parts; i++)
    {
      enum machine_mode mode, wider_mode;
      rtx loc2;
      HOST_WIDE_INT offset;

      if (i == 0 && var->onepart)
	{
	  gcc_checking_assert (var->n_var_parts == 1);
	  offset = 0;
	  initialized = VAR_INIT_STATUS_INITIALIZED;
	  loc2 = vt_expand_1pvar (var, vars);
	}
      else
	{
	  if (last_limit < VAR_PART_OFFSET (var, i))
	    {
	      complete = false;
	      break;
	    }
	  else if (last_limit > VAR_PART_OFFSET (var, i))
	    continue;
	  offset = VAR_PART_OFFSET (var, i);
	  loc2 = var->var_part[i].cur_loc;
	  if (loc2 && GET_CODE (loc2) == MEM
	      && GET_CODE (XEXP (loc2, 0)) == VALUE)
	    {
	      rtx depval = XEXP (loc2, 0);

	      loc2 = vt_expand_loc (loc2, vars);

	      if (loc2)
		loc_exp_insert_dep (var, depval, vars);
	    }
	  if (!loc2)
	    {
	      complete = false;
	      continue;
	    }
	  gcc_checking_assert (GET_CODE (loc2) != VALUE);
	  for (lc = var->var_part[i].loc_chain; lc; lc = lc->next)
	    if (var->var_part[i].cur_loc == lc->loc)
	      {
		initialized = lc->init;
		break;
	      }
	  gcc_assert (lc);
	}

      offsets[n_var_parts] = offset;
      if (!loc2)
	{
	  complete = false;
	  continue;
	}
      loc[n_var_parts] = loc2;
      mode = GET_MODE (var->var_part[i].cur_loc);
      if (mode == VOIDmode && var->onepart)
	mode = DECL_MODE (decl);
      last_limit = offsets[n_var_parts] + GET_MODE_SIZE (mode);

      /* Attempt to merge adjacent registers or memory.  */
      wider_mode = GET_MODE_WIDER_MODE (mode);
      for (j = i + 1; j < var->n_var_parts; j++)
	if (last_limit <= VAR_PART_OFFSET (var, j))
	  break;
      if (j < var->n_var_parts
	  && wider_mode != VOIDmode
	  && var->var_part[j].cur_loc
	  && mode == GET_MODE (var->var_part[j].cur_loc)
	  && (REG_P (loc[n_var_parts]) || MEM_P (loc[n_var_parts]))
	  && last_limit == (var->onepart ? 0 : VAR_PART_OFFSET (var, j))
	  && (loc2 = vt_expand_loc (var->var_part[j].cur_loc, vars))
	  && GET_CODE (loc[n_var_parts]) == GET_CODE (loc2))
	{
	  rtx new_loc = NULL;

	  if (REG_P (loc[n_var_parts])
	      && hard_regno_nregs[REGNO (loc[n_var_parts])][mode] * 2
		 == hard_regno_nregs[REGNO (loc[n_var_parts])][wider_mode]
	      && end_hard_regno (mode, REGNO (loc[n_var_parts]))
		 == REGNO (loc2))
	    {
	      if (! WORDS_BIG_ENDIAN && ! BYTES_BIG_ENDIAN)
		new_loc = simplify_subreg (wider_mode, loc[n_var_parts],
					   mode, 0);
	      else if (WORDS_BIG_ENDIAN && BYTES_BIG_ENDIAN)
		new_loc = simplify_subreg (wider_mode, loc2, mode, 0);
	      if (new_loc)
		{
		  if (!REG_P (new_loc)
		      || REGNO (new_loc) != REGNO (loc[n_var_parts]))
		    new_loc = NULL;
		  else
		    REG_ATTRS (new_loc) = REG_ATTRS (loc[n_var_parts]);
		}
	    }
	  else if (MEM_P (loc[n_var_parts])
		   && GET_CODE (XEXP (loc2, 0)) == PLUS
		   && REG_P (XEXP (XEXP (loc2, 0), 0))
		   && CONST_INT_P (XEXP (XEXP (loc2, 0), 1)))
	    {
	      if ((REG_P (XEXP (loc[n_var_parts], 0))
		   && rtx_equal_p (XEXP (loc[n_var_parts], 0),
				   XEXP (XEXP (loc2, 0), 0))
		   && INTVAL (XEXP (XEXP (loc2, 0), 1))
		      == GET_MODE_SIZE (mode))
		  || (GET_CODE (XEXP (loc[n_var_parts], 0)) == PLUS
		      && CONST_INT_P (XEXP (XEXP (loc[n_var_parts], 0), 1))
		      && rtx_equal_p (XEXP (XEXP (loc[n_var_parts], 0), 0),
				      XEXP (XEXP (loc2, 0), 0))
		      && INTVAL (XEXP (XEXP (loc[n_var_parts], 0), 1))
			 + GET_MODE_SIZE (mode)
			 == INTVAL (XEXP (XEXP (loc2, 0), 1))))
		new_loc = adjust_address_nv (loc[n_var_parts],
					     wider_mode, 0);
	    }

	  if (new_loc)
	    {
	      loc[n_var_parts] = new_loc;
	      mode = wider_mode;
	      last_limit = offsets[n_var_parts] + GET_MODE_SIZE (mode);
	      i = j;
	    }
	}
      ++n_var_parts;
    }
  type_size_unit = TYPE_SIZE_UNIT (TREE_TYPE (decl));
  if ((unsigned HOST_WIDE_INT) last_limit < TREE_INT_CST_LOW (type_size_unit))
    complete = false;

  if (! flag_var_tracking_uninit)
    initialized = VAR_INIT_STATUS_INITIALIZED;

  note_vl = NULL_RTX;
  if (!complete)
    note_vl = gen_rtx_VAR_LOCATION (VOIDmode, decl, NULL_RTX, initialized);
  else if (n_var_parts == 1)
    {
      rtx expr_list;

      if (offsets[0] || GET_CODE (loc[0]) == PARALLEL)
	expr_list = gen_rtx_EXPR_LIST (VOIDmode, loc[0], GEN_INT (offsets[0]));
      else
	expr_list = loc[0];

      note_vl = gen_rtx_VAR_LOCATION (VOIDmode, decl, expr_list, initialized);
    }
  else if (n_var_parts)
    {
      rtx parallel;

      for (i = 0; i < n_var_parts; i++)
	loc[i]
	  = gen_rtx_EXPR_LIST (VOIDmode, loc[i], GEN_INT (offsets[i]));

      parallel = gen_rtx_PARALLEL (VOIDmode,
				   gen_rtvec_v (n_var_parts, loc));
      note_vl = gen_rtx_VAR_LOCATION (VOIDmode, decl,
				      parallel, initialized);
    }

  if (where != EMIT_NOTE_BEFORE_INSN)
    {
      note = emit_note_after (NOTE_INSN_VAR_LOCATION, insn);
      if (where == EMIT_NOTE_AFTER_CALL_INSN)
	NOTE_DURING_CALL_P (note) = true;
    }
  else
    {
      /* Make sure that the call related notes come first.  */
      while (NEXT_INSN (insn)
	     && NOTE_P (insn)
	     && ((NOTE_KIND (insn) == NOTE_INSN_VAR_LOCATION
		  && NOTE_DURING_CALL_P (insn))
		 || NOTE_KIND (insn) == NOTE_INSN_CALL_ARG_LOCATION))
	insn = NEXT_INSN (insn);
      if (NOTE_P (insn)
	  && ((NOTE_KIND (insn) == NOTE_INSN_VAR_LOCATION
	       && NOTE_DURING_CALL_P (insn))
	      || NOTE_KIND (insn) == NOTE_INSN_CALL_ARG_LOCATION))
	note = emit_note_after (NOTE_INSN_VAR_LOCATION, insn);
      else
	note = emit_note_before (NOTE_INSN_VAR_LOCATION, insn);
    }
  NOTE_VAR_LOCATION (note) = note_vl;

  set_dv_changed (var->dv, false);
  gcc_assert (var->in_changed_variables);
  var->in_changed_variables = false;
  changed_variables.clear_slot (varp);

  /* Continue traversing the hash table.  */
  return 1;
}

/* While traversing changed_variables, push onto DATA (a stack of RTX
   values) entries that aren't user variables.  */

int
var_track_values_to_stack (variable_def **slot,
			   vec<rtx, va_heap> *changed_values_stack)
{
  variable var = *slot;

  if (var->onepart == ONEPART_VALUE)
    changed_values_stack->safe_push (dv_as_value (var->dv));
  else if (var->onepart == ONEPART_DEXPR)
    changed_values_stack->safe_push (DECL_RTL_KNOWN_SET (dv_as_decl (var->dv)));

  return 1;
}

/* Remove from changed_variables the entry whose DV corresponds to
   value or debug_expr VAL.  */
static void
remove_value_from_changed_variables (rtx val)
{
  decl_or_value dv = dv_from_rtx (val);
  variable_def **slot;
  variable var;

  slot = changed_variables.find_slot_with_hash (dv, dv_htab_hash (dv),
						NO_INSERT);
  var = *slot;
  var->in_changed_variables = false;
  changed_variables.clear_slot (slot);
}

/* If VAL (a value or debug_expr) has backlinks to variables actively
   dependent on it in HTAB or in CHANGED_VARIABLES, mark them as
   changed, adding to CHANGED_VALUES_STACK any dependencies that may
   have dependencies of their own to notify.  */

static void
notify_dependents_of_changed_value (rtx val, variable_table_type htab,
				    vec<rtx, va_heap> *changed_values_stack)
{
  variable_def **slot;
  variable var;
  loc_exp_dep *led;
  decl_or_value dv = dv_from_rtx (val);

  slot = changed_variables.find_slot_with_hash (dv, dv_htab_hash (dv),
						NO_INSERT);
  if (!slot)
    slot = htab.find_slot_with_hash (dv, dv_htab_hash (dv), NO_INSERT);
  if (!slot)
    slot = dropped_values.find_slot_with_hash (dv, dv_htab_hash (dv),
					       NO_INSERT);
  var = *slot;

  while ((led = VAR_LOC_DEP_LST (var)))
    {
      decl_or_value ldv = led->dv;
      variable ivar;

      /* Deactivate and remove the backlink, as it was “used up”.  It
	 makes no sense to attempt to notify the same entity again:
	 either it will be recomputed and re-register an active
	 dependency, or it will still have the changed mark.  */
      if (led->next)
	led->next->pprev = led->pprev;
      if (led->pprev)
	*led->pprev = led->next;
      led->next = NULL;
      led->pprev = NULL;

      if (dv_changed_p (ldv))
	continue;

      switch (dv_onepart_p (ldv))
	{
	case ONEPART_VALUE:
	case ONEPART_DEXPR:
	  set_dv_changed (ldv, true);
	  changed_values_stack->safe_push (dv_as_rtx (ldv));
	  break;

	case ONEPART_VDECL:
	  ivar = htab.find_with_hash (ldv, dv_htab_hash (ldv));
	  gcc_checking_assert (!VAR_LOC_DEP_LST (ivar));
	  variable_was_changed (ivar, NULL);
	  break;

	case NOT_ONEPART:
	  pool_free (loc_exp_dep_pool, led);
	  ivar = htab.find_with_hash (ldv, dv_htab_hash (ldv));
	  if (ivar)
	    {
	      int i = ivar->n_var_parts;
	      while (i--)
		{
		  rtx loc = ivar->var_part[i].cur_loc;

		  if (loc && GET_CODE (loc) == MEM
		      && XEXP (loc, 0) == val)
		    {
		      variable_was_changed (ivar, NULL);
		      break;
		    }
		}
	    }
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

/* Take out of changed_variables any entries that don't refer to use
   variables.  Back-propagate change notifications from values and
   debug_exprs to their active dependencies in HTAB or in
   CHANGED_VARIABLES.  */

static void
process_changed_values (variable_table_type htab)
{
  int i, n;
  rtx val;
  auto_vec<rtx, 20> changed_values_stack;

  /* Move values from changed_variables to changed_values_stack.  */
  changed_variables
    .traverse <vec<rtx, va_heap>*, var_track_values_to_stack>
      (&changed_values_stack);

  /* Back-propagate change notifications in values while popping
     them from the stack.  */
  for (n = i = changed_values_stack.length ();
       i > 0; i = changed_values_stack.length ())
    {
      val = changed_values_stack.pop ();
      notify_dependents_of_changed_value (val, htab, &changed_values_stack);

      /* This condition will hold when visiting each of the entries
	 originally in changed_variables.  We can't remove them
	 earlier because this could drop the backlinks before we got a
	 chance to use them.  */
      if (i == n)
	{
	  remove_value_from_changed_variables (val);
	  n--;
	}
    }
}

/* Emit NOTE_INSN_VAR_LOCATION note for each variable from a chain
   CHANGED_VARIABLES and delete this chain.  WHERE specifies whether
   the notes shall be emitted before of after instruction INSN.  */

static void
emit_notes_for_changes (rtx insn, enum emit_note_where where,
			shared_hash vars)
{
  emit_note_data data;
  variable_table_type htab = shared_hash_htab (vars);

  if (!changed_variables.elements ())
    return;

  if (MAY_HAVE_DEBUG_INSNS)
    process_changed_values (htab);

  data.insn = insn;
  data.where = where;
  data.vars = htab;

  changed_variables
    .traverse <emit_note_data*, emit_note_insn_var_location> (&data);
}

/* Add variable *SLOT to the chain CHANGED_VARIABLES if it differs from the
   same variable in hash table DATA or is not there at all.  */

int
emit_notes_for_differences_1 (variable_def **slot, variable_table_type new_vars)
{
  variable old_var, new_var;

  old_var = *slot;
  new_var = new_vars.find_with_hash (old_var->dv, dv_htab_hash (old_var->dv));

  if (!new_var)
    {
      /* Variable has disappeared.  */
      variable empty_var = NULL;

      if (old_var->onepart == ONEPART_VALUE
	  || old_var->onepart == ONEPART_DEXPR)
	{
	  empty_var = variable_from_dropped (old_var->dv, NO_INSERT);
	  if (empty_var)
	    {
	      gcc_checking_assert (!empty_var->in_changed_variables);
	      if (!VAR_LOC_1PAUX (old_var))
		{
		  VAR_LOC_1PAUX (old_var) = VAR_LOC_1PAUX (empty_var);
		  VAR_LOC_1PAUX (empty_var) = NULL;
		}
	      else
		gcc_checking_assert (!VAR_LOC_1PAUX (empty_var));
	    }
	}

      if (!empty_var)
	{
	  empty_var = (variable) pool_alloc (onepart_pool (old_var->onepart));
	  empty_var->dv = old_var->dv;
	  empty_var->refcount = 0;
	  empty_var->n_var_parts = 0;
	  empty_var->onepart = old_var->onepart;
	  empty_var->in_changed_variables = false;
	}

      if (empty_var->onepart)
	{
	  /* Propagate the auxiliary data to (ultimately)
	     changed_variables.  */
	  empty_var->var_part[0].loc_chain = NULL;
	  empty_var->var_part[0].cur_loc = NULL;
	  VAR_LOC_1PAUX (empty_var) = VAR_LOC_1PAUX (old_var);
	  VAR_LOC_1PAUX (old_var) = NULL;
	}
      variable_was_changed (empty_var, NULL);
      /* Continue traversing the hash table.  */
      return 1;
    }
  /* Update cur_loc and one-part auxiliary data, before new_var goes
     through variable_was_changed.  */
  if (old_var != new_var && new_var->onepart)
    {
      gcc_checking_assert (VAR_LOC_1PAUX (new_var) == NULL);
      VAR_LOC_1PAUX (new_var) = VAR_LOC_1PAUX (old_var);
      VAR_LOC_1PAUX (old_var) = NULL;
      new_var->var_part[0].cur_loc = old_var->var_part[0].cur_loc;
    }
  if (variable_different_p (old_var, new_var))
    variable_was_changed (new_var, NULL);

  /* Continue traversing the hash table.  */
  return 1;
}

/* Add variable *SLOT to the chain CHANGED_VARIABLES if it is not in hash
   table DATA.  */

int
emit_notes_for_differences_2 (variable_def **slot, variable_table_type old_vars)
{
  variable old_var, new_var;

  new_var = *slot;
  old_var = old_vars.find_with_hash (new_var->dv, dv_htab_hash (new_var->dv));
  if (!old_var)
    {
      int i;
      for (i = 0; i < new_var->n_var_parts; i++)
	new_var->var_part[i].cur_loc = NULL;
      variable_was_changed (new_var, NULL);
    }

  /* Continue traversing the hash table.  */
  return 1;
}

/* Emit notes before INSN for differences between dataflow sets OLD_SET and
   NEW_SET.  */

static void
emit_notes_for_differences (rtx insn, dataflow_set *old_set,
			    dataflow_set *new_set)
{
  shared_hash_htab (old_set->vars)
    .traverse <variable_table_type, emit_notes_for_differences_1>
      (shared_hash_htab (new_set->vars));
  shared_hash_htab (new_set->vars)
    .traverse <variable_table_type, emit_notes_for_differences_2>
      (shared_hash_htab (old_set->vars));
  emit_notes_for_changes (insn, EMIT_NOTE_BEFORE_INSN, new_set->vars);
}

/* Return the next insn after INSN that is not a NOTE_INSN_VAR_LOCATION.  */

static rtx
next_non_note_insn_var_location (rtx insn)
{
  while (insn)
    {
      insn = NEXT_INSN (insn);
      if (insn == 0
	  || !NOTE_P (insn)
	  || NOTE_KIND (insn) != NOTE_INSN_VAR_LOCATION)
	break;
    }

  return insn;
}

/* Emit the notes for changes of location parts in the basic block BB.  */

static void
emit_notes_in_bb (basic_block bb, dataflow_set *set)
{
  unsigned int i;
  micro_operation *mo;

  dataflow_set_clear (set);
  dataflow_set_copy (set, &VTI (bb)->in);

  FOR_EACH_VEC_ELT (VTI (bb)->mos, i, mo)
    {
      rtx insn = mo->insn;
      rtx next_insn = next_non_note_insn_var_location (insn);

      switch (mo->type)
	{
	  case MO_CALL:
	    dataflow_set_clear_at_call (set);
	    emit_notes_for_changes (insn, EMIT_NOTE_AFTER_CALL_INSN, set->vars);
	    {
	      rtx arguments = mo->u.loc, *p = &arguments, note;
	      while (*p)
		{
		  XEXP (XEXP (*p, 0), 1)
		    = vt_expand_loc (XEXP (XEXP (*p, 0), 1),
				     shared_hash_htab (set->vars));
		  /* If expansion is successful, keep it in the list.  */
		  if (XEXP (XEXP (*p, 0), 1))
		    p = &XEXP (*p, 1);
		  /* Otherwise, if the following item is data_value for it,
		     drop it too too.  */
		  else if (XEXP (*p, 1)
			   && REG_P (XEXP (XEXP (*p, 0), 0))
			   && MEM_P (XEXP (XEXP (XEXP (*p, 1), 0), 0))
			   && REG_P (XEXP (XEXP (XEXP (XEXP (*p, 1), 0), 0),
					   0))
			   && REGNO (XEXP (XEXP (*p, 0), 0))
			      == REGNO (XEXP (XEXP (XEXP (XEXP (*p, 1), 0),
						    0), 0)))
		    *p = XEXP (XEXP (*p, 1), 1);
		  /* Just drop this item.  */
		  else
		    *p = XEXP (*p, 1);
		}
	      note = emit_note_after (NOTE_INSN_CALL_ARG_LOCATION, insn);
	      NOTE_VAR_LOCATION (note) = arguments;
	    }
	    break;

	  case MO_USE:
	    {
	      rtx loc = mo->u.loc;

	      if (REG_P (loc))
		var_reg_set (set, loc, VAR_INIT_STATUS_UNINITIALIZED, NULL);
	      else
		var_mem_set (set, loc, VAR_INIT_STATUS_UNINITIALIZED, NULL);

	      emit_notes_for_changes (insn, EMIT_NOTE_BEFORE_INSN, set->vars);
	    }
	    break;

	  case MO_VAL_LOC:
	    {
	      rtx loc = mo->u.loc;
	      rtx val, vloc;
	      tree var;

	      if (GET_CODE (loc) == CONCAT)
		{
		  val = XEXP (loc, 0);
		  vloc = XEXP (loc, 1);
		}
	      else
		{
		  val = NULL_RTX;
		  vloc = loc;
		}

	      var = PAT_VAR_LOCATION_DECL (vloc);

	      clobber_variable_part (set, NULL_RTX,
				     dv_from_decl (var), 0, NULL_RTX);
	      if (val)
		{
		  if (VAL_NEEDS_RESOLUTION (loc))
		    val_resolve (set, val, PAT_VAR_LOCATION_LOC (vloc), insn);
		  set_variable_part (set, val, dv_from_decl (var), 0,
				     VAR_INIT_STATUS_INITIALIZED, NULL_RTX,
				     INSERT);
		}
	      else if (!VAR_LOC_UNKNOWN_P (PAT_VAR_LOCATION_LOC (vloc)))
		set_variable_part (set, PAT_VAR_LOCATION_LOC (vloc),
				   dv_from_decl (var), 0,
				   VAR_INIT_STATUS_INITIALIZED, NULL_RTX,
				   INSERT);

	      emit_notes_for_changes (insn, EMIT_NOTE_AFTER_INSN, set->vars);
	    }
	    break;

	  case MO_VAL_USE:
	    {
	      rtx loc = mo->u.loc;
	      rtx val, vloc, uloc;

	      vloc = uloc = XEXP (loc, 1);
	      val = XEXP (loc, 0);

	      if (GET_CODE (val) == CONCAT)
		{
		  uloc = XEXP (val, 1);
		  val = XEXP (val, 0);
		}

	      if (VAL_NEEDS_RESOLUTION (loc))
		val_resolve (set, val, vloc, insn);
	      else
		val_store (set, val, uloc, insn, false);

	      if (VAL_HOLDS_TRACK_EXPR (loc))
		{
		  if (GET_CODE (uloc) == REG)
		    var_reg_set (set, uloc, VAR_INIT_STATUS_UNINITIALIZED,
				 NULL);
		  else if (GET_CODE (uloc) == MEM)
		    var_mem_set (set, uloc, VAR_INIT_STATUS_UNINITIALIZED,
				 NULL);
		}

	      emit_notes_for_changes (insn, EMIT_NOTE_BEFORE_INSN, set->vars);
	    }
	    break;

	  case MO_VAL_SET:
	    {
	      rtx loc = mo->u.loc;
	      rtx val, vloc, uloc;
	      rtx dstv, srcv;

	      vloc = loc;
	      uloc = XEXP (vloc, 1);
	      val = XEXP (vloc, 0);
	      vloc = uloc;

	      if (GET_CODE (uloc) == SET)
		{
		  dstv = SET_DEST (uloc);
		  srcv = SET_SRC (uloc);
		}
	      else
		{
		  dstv = uloc;
		  srcv = NULL;
		}

	      if (GET_CODE (val) == CONCAT)
		{
		  dstv = vloc = XEXP (val, 1);
		  val = XEXP (val, 0);
		}

	      if (GET_CODE (vloc) == SET)
		{
		  srcv = SET_SRC (vloc);

		  gcc_assert (val != srcv);
		  gcc_assert (vloc == uloc || VAL_NEEDS_RESOLUTION (loc));

		  dstv = vloc = SET_DEST (vloc);

		  if (VAL_NEEDS_RESOLUTION (loc))
		    val_resolve (set, val, srcv, insn);
		}
	      else if (VAL_NEEDS_RESOLUTION (loc))
		{
		  gcc_assert (GET_CODE (uloc) == SET
			      && GET_CODE (SET_SRC (uloc)) == REG);
		  val_resolve (set, val, SET_SRC (uloc), insn);
		}

	      if (VAL_HOLDS_TRACK_EXPR (loc))
		{
		  if (VAL_EXPR_IS_CLOBBERED (loc))
		    {
		      if (REG_P (uloc))
			var_reg_delete (set, uloc, true);
		      else if (MEM_P (uloc))
			{
			  gcc_assert (MEM_P (dstv));
			  gcc_assert (MEM_ATTRS (dstv) == MEM_ATTRS (uloc));
			  var_mem_delete (set, dstv, true);
			}
		    }
		  else
		    {
		      bool copied_p = VAL_EXPR_IS_COPIED (loc);
		      rtx src = NULL, dst = uloc;
		      enum var_init_status status = VAR_INIT_STATUS_INITIALIZED;

		      if (GET_CODE (uloc) == SET)
			{
			  src = SET_SRC (uloc);
			  dst = SET_DEST (uloc);
			}

		      if (copied_p)
			{
			  status = find_src_status (set, src);

			  src = find_src_set_src (set, src);
			}

		      if (REG_P (dst))
			var_reg_delete_and_set (set, dst, !copied_p,
						status, srcv);
		      else if (MEM_P (dst))
			{
			  gcc_assert (MEM_P (dstv));
			  gcc_assert (MEM_ATTRS (dstv) == MEM_ATTRS (dst));
			  var_mem_delete_and_set (set, dstv, !copied_p,
						  status, srcv);
			}
		    }
		}
	      else if (REG_P (uloc))
		var_regno_delete (set, REGNO (uloc));
	      else if (MEM_P (uloc))
		{
		  gcc_checking_assert (GET_CODE (vloc) == MEM);
		  gcc_checking_assert (vloc == dstv);
		  if (vloc != dstv)
		    clobber_overlapping_mems (set, vloc);
		}

	      val_store (set, val, dstv, insn, true);

	      emit_notes_for_changes (next_insn, EMIT_NOTE_BEFORE_INSN,
				      set->vars);
	    }
	    break;

	  case MO_SET:
	    {
	      rtx loc = mo->u.loc;
	      rtx set_src = NULL;

	      if (GET_CODE (loc) == SET)
		{
		  set_src = SET_SRC (loc);
		  loc = SET_DEST (loc);
		}

	      if (REG_P (loc))
		var_reg_delete_and_set (set, loc, true, VAR_INIT_STATUS_INITIALIZED,
					set_src);
	      else
		var_mem_delete_and_set (set, loc, true, VAR_INIT_STATUS_INITIALIZED,
					set_src);

	      emit_notes_for_changes (next_insn, EMIT_NOTE_BEFORE_INSN,
				      set->vars);
	    }
	    break;

	  case MO_COPY:
	    {
	      rtx loc = mo->u.loc;
	      enum var_init_status src_status;
	      rtx set_src = NULL;

	      if (GET_CODE (loc) == SET)
		{
		  set_src = SET_SRC (loc);
		  loc = SET_DEST (loc);
		}

	      src_status = find_src_status (set, set_src);
	      set_src = find_src_set_src (set, set_src);

	      if (REG_P (loc))
		var_reg_delete_and_set (set, loc, false, src_status, set_src);
	      else
		var_mem_delete_and_set (set, loc, false, src_status, set_src);

	      emit_notes_for_changes (next_insn, EMIT_NOTE_BEFORE_INSN,
				      set->vars);
	    }
	    break;

	  case MO_USE_NO_VAR:
	    {
	      rtx loc = mo->u.loc;

	      if (REG_P (loc))
		var_reg_delete (set, loc, false);
	      else
		var_mem_delete (set, loc, false);

	      emit_notes_for_changes (insn, EMIT_NOTE_AFTER_INSN, set->vars);
	    }
	    break;

	  case MO_CLOBBER:
	    {
	      rtx loc = mo->u.loc;

	      if (REG_P (loc))
		var_reg_delete (set, loc, true);
	      else
		var_mem_delete (set, loc, true);

	      emit_notes_for_changes (next_insn, EMIT_NOTE_BEFORE_INSN,
				      set->vars);
	    }
	    break;

	  case MO_ADJUST:
	    set->stack_adjust += mo->u.adjust;
	    break;
	}
    }
}

/* Emit notes for the whole function.  */

static void
vt_emit_notes (void)
{
  basic_block bb;
  dataflow_set cur;

  gcc_assert (!changed_variables.elements ());

  /* Free memory occupied by the out hash tables, as they aren't used
     anymore.  */
  FOR_EACH_BB_FN (bb, cfun)
    dataflow_set_clear (&VTI (bb)->out);

  /* Enable emitting notes by functions (mainly by set_variable_part and
     delete_variable_part).  */
  emit_notes = true;

  if (MAY_HAVE_DEBUG_INSNS)
    {
      dropped_values.create (cselib_get_next_uid () * 2);
      loc_exp_dep_pool = create_alloc_pool ("loc_exp_dep pool",
					    sizeof (loc_exp_dep), 64);
    }

  dataflow_set_init (&cur);

  FOR_EACH_BB_FN (bb, cfun)
    {
      /* Emit the notes for changes of variable locations between two
	 subsequent basic blocks.  */
      emit_notes_for_differences (BB_HEAD (bb), &cur, &VTI (bb)->in);

      if (MAY_HAVE_DEBUG_INSNS)
	local_get_addr_cache = pointer_map_create ();

      /* Emit the notes for the changes in the basic block itself.  */
      emit_notes_in_bb (bb, &cur);

      if (MAY_HAVE_DEBUG_INSNS)
	pointer_map_destroy (local_get_addr_cache);
      local_get_addr_cache = NULL;

      /* Free memory occupied by the in hash table, we won't need it
	 again.  */
      dataflow_set_clear (&VTI (bb)->in);
    }
#ifdef ENABLE_CHECKING
  shared_hash_htab (cur.vars)
    .traverse <variable_table_type, emit_notes_for_differences_1>
      (shared_hash_htab (empty_shared_hash));
#endif
  dataflow_set_destroy (&cur);

  if (MAY_HAVE_DEBUG_INSNS)
    dropped_values.dispose ();

  emit_notes = false;
}

/* If there is a declaration and offset associated with register/memory RTL
   assign declaration to *DECLP and offset to *OFFSETP, and return true.  */

static bool
vt_get_decl_and_offset (rtx rtl, tree *declp, HOST_WIDE_INT *offsetp)
{
  if (REG_P (rtl))
    {
      if (REG_ATTRS (rtl))
	{
	  *declp = REG_EXPR (rtl);
	  *offsetp = REG_OFFSET (rtl);
	  return true;
	}
    }
  else if (GET_CODE (rtl) == PARALLEL)
    {
      tree decl = NULL_TREE;
      HOST_WIDE_INT offset = MAX_VAR_PARTS;
      int len = XVECLEN (rtl, 0), i;

      for (i = 0; i < len; i++)
	{
	  rtx reg = XEXP (XVECEXP (rtl, 0, i), 0);
	  if (!REG_P (reg) || !REG_ATTRS (reg))
	    break;
	  if (!decl)
	    decl = REG_EXPR (reg);
	  if (REG_EXPR (reg) != decl)
	    break;
	  if (REG_OFFSET (reg) < offset)
	    offset = REG_OFFSET (reg);
	}

      if (i == len)
	{
	  *declp = decl;
	  *offsetp = offset;
	  return true;
	}
    }
  else if (MEM_P (rtl))
    {
      if (MEM_ATTRS (rtl))
	{
	  *declp = MEM_EXPR (rtl);
	  *offsetp = INT_MEM_OFFSET (rtl);
	  return true;
	}
    }
  return false;
}

/* Record the value for the ENTRY_VALUE of RTL as a global equivalence
   of VAL.  */

static void
record_entry_value (cselib_val *val, rtx rtl)
{
  rtx ev = gen_rtx_ENTRY_VALUE (GET_MODE (rtl));

  ENTRY_VALUE_EXP (ev) = rtl;

  cselib_add_permanent_equiv (val, ev, get_insns ());
}

/* Insert function parameter PARM in IN and OUT sets of ENTRY_BLOCK.  */

static void
vt_add_function_parameter (tree parm)
{
  rtx decl_rtl = DECL_RTL_IF_SET (parm);
  rtx incoming = DECL_INCOMING_RTL (parm);
  tree decl;
  enum machine_mode mode;
  HOST_WIDE_INT offset;
  dataflow_set *out;
  decl_or_value dv;

  if (TREE_CODE (parm) != PARM_DECL)
    return;

  if (!decl_rtl || !incoming)
    return;

  if (GET_MODE (decl_rtl) == BLKmode || GET_MODE (incoming) == BLKmode)
    return;

  /* If there is a DRAP register or a pseudo in internal_arg_pointer,
     rewrite the incoming location of parameters passed on the stack
     into MEMs based on the argument pointer, so that incoming doesn't
     depend on a pseudo.  */
  if (MEM_P (incoming)
      && (XEXP (incoming, 0) == crtl->args.internal_arg_pointer
	  || (GET_CODE (XEXP (incoming, 0)) == PLUS
	      && XEXP (XEXP (incoming, 0), 0)
		 == crtl->args.internal_arg_pointer
	      && CONST_INT_P (XEXP (XEXP (incoming, 0), 1)))))
    {
      HOST_WIDE_INT off = -FIRST_PARM_OFFSET (current_function_decl);
      if (GET_CODE (XEXP (incoming, 0)) == PLUS)
	off += INTVAL (XEXP (XEXP (incoming, 0), 1));
      incoming
	= replace_equiv_address_nv (incoming,
				    plus_constant (Pmode,
						   arg_pointer_rtx, off));
    }

#ifdef HAVE_window_save
  /* DECL_INCOMING_RTL uses the INCOMING_REGNO of parameter registers.
     If the target machine has an explicit window save instruction, the
     actual entry value is the corresponding OUTGOING_REGNO instead.  */
  if (HAVE_window_save && !crtl->uses_only_leaf_regs)
    {
      if (REG_P (incoming)
	  && HARD_REGISTER_P (incoming)
	  && OUTGOING_REGNO (REGNO (incoming)) != REGNO (incoming))
	{
	  parm_reg_t p;
	  p.incoming = incoming;
	  incoming
	    = gen_rtx_REG_offset (incoming, GET_MODE (incoming),
				  OUTGOING_REGNO (REGNO (incoming)), 0);
	  p.outgoing = incoming;
	  vec_safe_push (windowed_parm_regs, p);
	}
      else if (GET_CODE (incoming) == PARALLEL)
	{
	  rtx outgoing
	    = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (XVECLEN (incoming, 0)));
	  int i;

	  for (i = 0; i < XVECLEN (incoming, 0); i++)
	    {
	      rtx reg = XEXP (XVECEXP (incoming, 0, i), 0);
	      parm_reg_t p;
	      p.incoming = reg;
	      reg = gen_rtx_REG_offset (reg, GET_MODE (reg),
					OUTGOING_REGNO (REGNO (reg)), 0);
	      p.outgoing = reg;
	      XVECEXP (outgoing, 0, i)
		= gen_rtx_EXPR_LIST (VOIDmode, reg,
				     XEXP (XVECEXP (incoming, 0, i), 1));
	      vec_safe_push (windowed_parm_regs, p);
	    }

	  incoming = outgoing;
	}
      else if (MEM_P (incoming)
	       && REG_P (XEXP (incoming, 0))
	       && HARD_REGISTER_P (XEXP (incoming, 0)))
	{
	  rtx reg = XEXP (incoming, 0);
	  if (OUTGOING_REGNO (REGNO (reg)) != REGNO (reg))
	    {
	      parm_reg_t p;
	      p.incoming = reg;
	      reg = gen_raw_REG (GET_MODE (reg), OUTGOING_REGNO (REGNO (reg)));
	      p.outgoing = reg;
	      vec_safe_push (windowed_parm_regs, p);
	      incoming = replace_equiv_address_nv (incoming, reg);
	    }
	}
    }
#endif

  if (!vt_get_decl_and_offset (incoming, &decl, &offset))
    {
      if (MEM_P (incoming))
	{
	  /* This means argument is passed by invisible reference.  */
	  offset = 0;
	  decl = parm;
	}
      else
	{
	  if (!vt_get_decl_and_offset (decl_rtl, &decl, &offset))
	    return;
	  offset += byte_lowpart_offset (GET_MODE (incoming),
					 GET_MODE (decl_rtl));
	}
    }

  if (!decl)
    return;

  if (parm != decl)
    {
      /* If that DECL_RTL wasn't a pseudo that got spilled to
	 memory, bail out.  Otherwise, the spill slot sharing code
	 will force the memory to reference spill_slot_decl (%sfp),
	 so we don't match above.  That's ok, the pseudo must have
	 referenced the entire parameter, so just reset OFFSET.  */
      if (decl != get_spill_slot_decl (false))
        return;
      offset = 0;
    }

  if (!track_loc_p (incoming, parm, offset, false, &mode, &offset))
    return;

  out = &VTI (ENTRY_BLOCK_PTR_FOR_FN (cfun))->out;

  dv = dv_from_decl (parm);

  if (target_for_debug_bind (parm)
      /* We can't deal with these right now, because this kind of
	 variable is single-part.  ??? We could handle parallels
	 that describe multiple locations for the same single
	 value, but ATM we don't.  */
      && GET_CODE (incoming) != PARALLEL)
    {
      cselib_val *val;
      rtx lowpart;

      /* ??? We shouldn't ever hit this, but it may happen because
	 arguments passed by invisible reference aren't dealt with
	 above: incoming-rtl will have Pmode rather than the
	 expected mode for the type.  */
      if (offset)
	return;

      lowpart = var_lowpart (mode, incoming);
      if (!lowpart)
	return;

      val = cselib_lookup_from_insn (lowpart, mode, true,
				     VOIDmode, get_insns ());

      /* ??? Float-typed values in memory are not handled by
	 cselib.  */
      if (val)
	{
	  preserve_value (val);
	  set_variable_part (out, val->val_rtx, dv, offset,
			     VAR_INIT_STATUS_INITIALIZED, NULL, INSERT);
	  dv = dv_from_value (val->val_rtx);
	}

      if (MEM_P (incoming))
	{
	  val = cselib_lookup_from_insn (XEXP (incoming, 0), mode, true,
					 VOIDmode, get_insns ());
	  if (val)
	    {
	      preserve_value (val);
	      incoming = replace_equiv_address_nv (incoming, val->val_rtx);
	    }
	}
    }

  if (REG_P (incoming))
    {
      incoming = var_lowpart (mode, incoming);
      gcc_assert (REGNO (incoming) < FIRST_PSEUDO_REGISTER);
      attrs_list_insert (&out->regs[REGNO (incoming)], dv, offset,
			 incoming);
      set_variable_part (out, incoming, dv, offset,
			 VAR_INIT_STATUS_INITIALIZED, NULL, INSERT);
      if (dv_is_value_p (dv))
	{
	  record_entry_value (CSELIB_VAL_PTR (dv_as_value (dv)), incoming);
	  if (TREE_CODE (TREE_TYPE (parm)) == REFERENCE_TYPE
	      && INTEGRAL_TYPE_P (TREE_TYPE (TREE_TYPE (parm))))
	    {
	      enum machine_mode indmode
		= TYPE_MODE (TREE_TYPE (TREE_TYPE (parm)));
	      rtx mem = gen_rtx_MEM (indmode, incoming);
	      cselib_val *val = cselib_lookup_from_insn (mem, indmode, true,
							 VOIDmode,
							 get_insns ());
	      if (val)
		{
		  preserve_value (val);
		  record_entry_value (val, mem);
		  set_variable_part (out, mem, dv_from_value (val->val_rtx), 0,
				     VAR_INIT_STATUS_INITIALIZED, NULL, INSERT);
		}
	    }
	}
    }
  else if (GET_CODE (incoming) == PARALLEL && !dv_onepart_p (dv))
    {
      int i;

      for (i = 0; i < XVECLEN (incoming, 0); i++)
	{
	  rtx reg = XEXP (XVECEXP (incoming, 0, i), 0);
	  offset = REG_OFFSET (reg);
	  gcc_assert (REGNO (reg) < FIRST_PSEUDO_REGISTER);
	  attrs_list_insert (&out->regs[REGNO (reg)], dv, offset, reg);
	  set_variable_part (out, reg, dv, offset,
			     VAR_INIT_STATUS_INITIALIZED, NULL, INSERT);
	}
    }
  else if (MEM_P (incoming))
    {
      incoming = var_lowpart (mode, incoming);
      set_variable_part (out, incoming, dv, offset,
			 VAR_INIT_STATUS_INITIALIZED, NULL, INSERT);
    }
}

/* Insert function parameters to IN and OUT sets of ENTRY_BLOCK.  */

static void
vt_add_function_parameters (void)
{
  tree parm;

  for (parm = DECL_ARGUMENTS (current_function_decl);
       parm; parm = DECL_CHAIN (parm))
    vt_add_function_parameter (parm);

  if (DECL_HAS_VALUE_EXPR_P (DECL_RESULT (current_function_decl)))
    {
      tree vexpr = DECL_VALUE_EXPR (DECL_RESULT (current_function_decl));

      if (TREE_CODE (vexpr) == INDIRECT_REF)
	vexpr = TREE_OPERAND (vexpr, 0);

      if (TREE_CODE (vexpr) == PARM_DECL
	  && DECL_ARTIFICIAL (vexpr)
	  && !DECL_IGNORED_P (vexpr)
	  && DECL_NAMELESS (vexpr))
	vt_add_function_parameter (vexpr);
    }
}

/* Initialize cfa_base_rtx, create a preserved VALUE for it and
   ensure it isn't flushed during cselib_reset_table.
   Can be called only if frame_pointer_rtx resp. arg_pointer_rtx
   has been eliminated.  */

static void
vt_init_cfa_base (void)
{
  cselib_val *val;

#ifdef FRAME_POINTER_CFA_OFFSET
  cfa_base_rtx = frame_pointer_rtx;
  cfa_base_offset = -FRAME_POINTER_CFA_OFFSET (current_function_decl);
#else
  cfa_base_rtx = arg_pointer_rtx;
  cfa_base_offset = -ARG_POINTER_CFA_OFFSET (current_function_decl);
#endif
  if (cfa_base_rtx == hard_frame_pointer_rtx
      || !fixed_regs[REGNO (cfa_base_rtx)])
    {
      cfa_base_rtx = NULL_RTX;
      return;
    }
  if (!MAY_HAVE_DEBUG_INSNS)
    return;

  /* Tell alias analysis that cfa_base_rtx should share
     find_base_term value with stack pointer or hard frame pointer.  */
  if (!frame_pointer_needed)
    vt_equate_reg_base_value (cfa_base_rtx, stack_pointer_rtx);
  else if (!crtl->stack_realign_tried)
    vt_equate_reg_base_value (cfa_base_rtx, hard_frame_pointer_rtx);

  val = cselib_lookup_from_insn (cfa_base_rtx, GET_MODE (cfa_base_rtx), 1,
				 VOIDmode, get_insns ());
  preserve_value (val);
  cselib_preserve_cfa_base_value (val, REGNO (cfa_base_rtx));
}

/* Allocate and initialize the data structures for variable tracking
   and parse the RTL to get the micro operations.  */

static bool
vt_initialize (void)
{
  basic_block bb;
  HOST_WIDE_INT fp_cfa_offset = -1;

  alloc_aux_for_blocks (sizeof (struct variable_tracking_info_def));

  attrs_pool = create_alloc_pool ("attrs_def pool",
				  sizeof (struct attrs_def), 1024);
  var_pool = create_alloc_pool ("variable_def pool",
				sizeof (struct variable_def)
				+ (MAX_VAR_PARTS - 1)
				* sizeof (((variable)NULL)->var_part[0]), 64);
  loc_chain_pool = create_alloc_pool ("location_chain_def pool",
				      sizeof (struct location_chain_def),
				      1024);
  shared_hash_pool = create_alloc_pool ("shared_hash_def pool",
					sizeof (struct shared_hash_def), 256);
  empty_shared_hash = (shared_hash) pool_alloc (shared_hash_pool);
  empty_shared_hash->refcount = 1;
  empty_shared_hash->htab.create (1);
  changed_variables.create (10);

  /* Init the IN and OUT sets.  */
  FOR_ALL_BB_FN (bb, cfun)
    {
      VTI (bb)->visited = false;
      VTI (bb)->flooded = false;
      dataflow_set_init (&VTI (bb)->in);
      dataflow_set_init (&VTI (bb)->out);
      VTI (bb)->permp = NULL;
    }

  if (MAY_HAVE_DEBUG_INSNS)
    {
      cselib_init (CSELIB_RECORD_MEMORY | CSELIB_PRESERVE_CONSTANTS);
      scratch_regs = BITMAP_ALLOC (NULL);
      valvar_pool = create_alloc_pool ("small variable_def pool",
				       sizeof (struct variable_def), 256);
      preserved_values.create (256);
      global_get_addr_cache = pointer_map_create ();
    }
  else
    {
      scratch_regs = NULL;
      valvar_pool = NULL;
      global_get_addr_cache = NULL;
    }

  if (MAY_HAVE_DEBUG_INSNS)
    {
      rtx reg, expr;
      int ofst;
      cselib_val *val;

#ifdef FRAME_POINTER_CFA_OFFSET
      reg = frame_pointer_rtx;
      ofst = FRAME_POINTER_CFA_OFFSET (current_function_decl);
#else
      reg = arg_pointer_rtx;
      ofst = ARG_POINTER_CFA_OFFSET (current_function_decl);
#endif

      ofst -= INCOMING_FRAME_SP_OFFSET;

      val = cselib_lookup_from_insn (reg, GET_MODE (reg), 1,
				     VOIDmode, get_insns ());
      preserve_value (val);
      if (reg != hard_frame_pointer_rtx && fixed_regs[REGNO (reg)])
	cselib_preserve_cfa_base_value (val, REGNO (reg));
      expr = plus_constant (GET_MODE (stack_pointer_rtx),
			    stack_pointer_rtx, -ofst);
      cselib_add_permanent_equiv (val, expr, get_insns ());

      if (ofst)
	{
	  val = cselib_lookup_from_insn (stack_pointer_rtx,
					 GET_MODE (stack_pointer_rtx), 1,
					 VOIDmode, get_insns ());
	  preserve_value (val);
	  expr = plus_constant (GET_MODE (reg), reg, ofst);
	  cselib_add_permanent_equiv (val, expr, get_insns ());
	}
    }

  /* In order to factor out the adjustments made to the stack pointer or to
     the hard frame pointer and thus be able to use DW_OP_fbreg operations
     instead of individual location lists, we're going to rewrite MEMs based
     on them into MEMs based on the CFA by de-eliminating stack_pointer_rtx
     or hard_frame_pointer_rtx to the virtual CFA pointer frame_pointer_rtx
     resp. arg_pointer_rtx.  We can do this either when there is no frame
     pointer in the function and stack adjustments are consistent for all
     basic blocks or when there is a frame pointer and no stack realignment.
     But we first have to check that frame_pointer_rtx resp. arg_pointer_rtx
     has been eliminated.  */
  if (!frame_pointer_needed)
    {
      rtx reg, elim;

      if (!vt_stack_adjustments ())
	return false;

#ifdef FRAME_POINTER_CFA_OFFSET
      reg = frame_pointer_rtx;
#else
      reg = arg_pointer_rtx;
#endif
      elim = eliminate_regs (reg, VOIDmode, NULL_RTX);
      if (elim != reg)
	{
	  if (GET_CODE (elim) == PLUS)
	    elim = XEXP (elim, 0);
	  if (elim == stack_pointer_rtx)
	    vt_init_cfa_base ();
	}
    }
  else if (!crtl->stack_realign_tried)
    {
      rtx reg, elim;

#ifdef FRAME_POINTER_CFA_OFFSET
      reg = frame_pointer_rtx;
      fp_cfa_offset = FRAME_POINTER_CFA_OFFSET (current_function_decl);
#else
      reg = arg_pointer_rtx;
      fp_cfa_offset = ARG_POINTER_CFA_OFFSET (current_function_decl);
#endif
      elim = eliminate_regs (reg, VOIDmode, NULL_RTX);
      if (elim != reg)
	{
	  if (GET_CODE (elim) == PLUS)
	    {
	      fp_cfa_offset -= INTVAL (XEXP (elim, 1));
	      elim = XEXP (elim, 0);
	    }
	  if (elim != hard_frame_pointer_rtx)
	    fp_cfa_offset = -1;
	}
      else
	fp_cfa_offset = -1;
    }

  /* If the stack is realigned and a DRAP register is used, we're going to
     rewrite MEMs based on it representing incoming locations of parameters
     passed on the stack into MEMs based on the argument pointer.  Although
     we aren't going to rewrite other MEMs, we still need to initialize the
     virtual CFA pointer in order to ensure that the argument pointer will
     be seen as a constant throughout the function.

     ??? This doesn't work if FRAME_POINTER_CFA_OFFSET is defined.  */
  else if (stack_realign_drap)
    {
      rtx reg, elim;

#ifdef FRAME_POINTER_CFA_OFFSET
      reg = frame_pointer_rtx;
#else
      reg = arg_pointer_rtx;
#endif
      elim = eliminate_regs (reg, VOIDmode, NULL_RTX);
      if (elim != reg)
	{
	  if (GET_CODE (elim) == PLUS)
	    elim = XEXP (elim, 0);
	  if (elim == hard_frame_pointer_rtx)
	    vt_init_cfa_base ();
	}
    }

  hard_frame_pointer_adjustment = -1;

  vt_add_function_parameters ();

  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx insn;
      HOST_WIDE_INT pre, post = 0;
      basic_block first_bb, last_bb;

      if (MAY_HAVE_DEBUG_INSNS)
	{
	  cselib_record_sets_hook = add_with_sets;
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "first value: %i\n",
		     cselib_get_next_uid ());
	}

      first_bb = bb;
      for (;;)
	{
	  edge e;
	  if (bb->next_bb == EXIT_BLOCK_PTR_FOR_FN (cfun)
	      || ! single_pred_p (bb->next_bb))
	    break;
	  e = find_edge (bb, bb->next_bb);
	  if (! e || (e->flags & EDGE_FALLTHRU) == 0)
	    break;
	  bb = bb->next_bb;
	}
      last_bb = bb;

      /* Add the micro-operations to the vector.  */
      FOR_BB_BETWEEN (bb, first_bb, last_bb->next_bb, next_bb)
	{
	  HOST_WIDE_INT offset = VTI (bb)->out.stack_adjust;
	  VTI (bb)->out.stack_adjust = VTI (bb)->in.stack_adjust;
	  for (insn = BB_HEAD (bb); insn != NEXT_INSN (BB_END (bb));
	       insn = NEXT_INSN (insn))
	    {
	      if (INSN_P (insn))
		{
		  if (!frame_pointer_needed)
		    {
		      insn_stack_adjust_offset_pre_post (insn, &pre, &post);
		      if (pre)
			{
			  micro_operation mo;
			  mo.type = MO_ADJUST;
			  mo.u.adjust = pre;
			  mo.insn = insn;
			  if (dump_file && (dump_flags & TDF_DETAILS))
			    log_op_type (PATTERN (insn), bb, insn,
					 MO_ADJUST, dump_file);
			  VTI (bb)->mos.safe_push (mo);
			  VTI (bb)->out.stack_adjust += pre;
			}
		    }

		  cselib_hook_called = false;
		  adjust_insn (bb, insn);
		  if (MAY_HAVE_DEBUG_INSNS)
		    {
		      if (CALL_P (insn))
			prepare_call_arguments (bb, insn);
		      cselib_process_insn (insn);
		      if (dump_file && (dump_flags & TDF_DETAILS))
			{
			  print_rtl_single (dump_file, insn);
			  dump_cselib_table (dump_file);
			}
		    }
		  if (!cselib_hook_called)
		    add_with_sets (insn, 0, 0);
		  cancel_changes (0);

		  if (!frame_pointer_needed && post)
		    {
		      micro_operation mo;
		      mo.type = MO_ADJUST;
		      mo.u.adjust = post;
		      mo.insn = insn;
		      if (dump_file && (dump_flags & TDF_DETAILS))
			log_op_type (PATTERN (insn), bb, insn,
				     MO_ADJUST, dump_file);
		      VTI (bb)->mos.safe_push (mo);
		      VTI (bb)->out.stack_adjust += post;
		    }

		  if (fp_cfa_offset != -1
		      && hard_frame_pointer_adjustment == -1
		      && fp_setter_insn (insn))
		    {
		      vt_init_cfa_base ();
		      hard_frame_pointer_adjustment = fp_cfa_offset;
		      /* Disassociate sp from fp now.  */
		      if (MAY_HAVE_DEBUG_INSNS)
			{
			  cselib_val *v;
			  cselib_invalidate_rtx (stack_pointer_rtx);
			  v = cselib_lookup (stack_pointer_rtx, Pmode, 1,
					     VOIDmode);
			  if (v && !cselib_preserved_value_p (v))
			    {
			      cselib_set_value_sp_based (v);
			      preserve_value (v);
			    }
			}
		    }
		}
	    }
	  gcc_assert (offset == VTI (bb)->out.stack_adjust);
	}

      bb = last_bb;

      if (MAY_HAVE_DEBUG_INSNS)
	{
	  cselib_preserve_only_values ();
	  cselib_reset_table (cselib_get_next_uid ());
	  cselib_record_sets_hook = NULL;
	}
    }

  hard_frame_pointer_adjustment = -1;
  VTI (ENTRY_BLOCK_PTR_FOR_FN (cfun))->flooded = true;
  cfa_base_rtx = NULL_RTX;
  return true;
}

/* This is *not* reset after each function.  It gives each
   NOTE_INSN_DELETED_DEBUG_LABEL in the entire compilation
   a unique label number.  */

static int debug_label_num = 1;

/* Get rid of all debug insns from the insn stream.  */

static void
delete_debug_insns (void)
{
  basic_block bb;
  rtx insn, next;

  if (!MAY_HAVE_DEBUG_INSNS)
    return;

  FOR_EACH_BB_FN (bb, cfun)
    {
      FOR_BB_INSNS_SAFE (bb, insn, next)
	if (DEBUG_INSN_P (insn))
	  {
	    tree decl = INSN_VAR_LOCATION_DECL (insn);
	    if (TREE_CODE (decl) == LABEL_DECL
		&& DECL_NAME (decl)
		&& !DECL_RTL_SET_P (decl))
	      {
		PUT_CODE (insn, NOTE);
		NOTE_KIND (insn) = NOTE_INSN_DELETED_DEBUG_LABEL;
		NOTE_DELETED_LABEL_NAME (insn)
		  = IDENTIFIER_POINTER (DECL_NAME (decl));
		SET_DECL_RTL (decl, insn);
		CODE_LABEL_NUMBER (insn) = debug_label_num++;
	      }
	    else
	      delete_insn (insn);
	  }
    }
}

/* Run a fast, BB-local only version of var tracking, to take care of
   information that we don't do global analysis on, such that not all
   information is lost.  If SKIPPED holds, we're skipping the global
   pass entirely, so we should try to use information it would have
   handled as well..  */

static void
vt_debug_insns_local (bool skipped ATTRIBUTE_UNUSED)
{
  /* ??? Just skip it all for now.  */
  delete_debug_insns ();
}

/* Free the data structures needed for variable tracking.  */

static void
vt_finalize (void)
{
  basic_block bb;

  FOR_EACH_BB_FN (bb, cfun)
    {
      VTI (bb)->mos.release ();
    }

  FOR_ALL_BB_FN (bb, cfun)
    {
      dataflow_set_destroy (&VTI (bb)->in);
      dataflow_set_destroy (&VTI (bb)->out);
      if (VTI (bb)->permp)
	{
	  dataflow_set_destroy (VTI (bb)->permp);
	  XDELETE (VTI (bb)->permp);
	}
    }
  free_aux_for_blocks ();
  empty_shared_hash->htab.dispose ();
  changed_variables.dispose ();
  free_alloc_pool (attrs_pool);
  free_alloc_pool (var_pool);
  free_alloc_pool (loc_chain_pool);
  free_alloc_pool (shared_hash_pool);

  if (MAY_HAVE_DEBUG_INSNS)
    {
      if (global_get_addr_cache)
	pointer_map_destroy (global_get_addr_cache);
      global_get_addr_cache = NULL;
      if (loc_exp_dep_pool)
	free_alloc_pool (loc_exp_dep_pool);
      loc_exp_dep_pool = NULL;
      free_alloc_pool (valvar_pool);
      preserved_values.release ();
      cselib_finish ();
      BITMAP_FREE (scratch_regs);
      scratch_regs = NULL;
    }

#ifdef HAVE_window_save
  vec_free (windowed_parm_regs);
#endif

  if (vui_vec)
    XDELETEVEC (vui_vec);
  vui_vec = NULL;
  vui_allocated = 0;
}

/* The entry point to variable tracking pass.  */

static inline unsigned int
variable_tracking_main_1 (void)
{
  bool success;

  if (flag_var_tracking_assignments < 0)
    {
      delete_debug_insns ();
      return 0;
    }

  if (n_basic_blocks_for_fn (cfun) > 500 &&
      n_edges_for_fn (cfun) / n_basic_blocks_for_fn (cfun) >= 20)
    {
      vt_debug_insns_local (true);
      return 0;
    }

  mark_dfs_back_edges ();
  if (!vt_initialize ())
    {
      vt_finalize ();
      vt_debug_insns_local (true);
      return 0;
    }

  success = vt_find_locations ();

  if (!success && flag_var_tracking_assignments > 0)
    {
      vt_finalize ();

      delete_debug_insns ();

      /* This is later restored by our caller.  */
      flag_var_tracking_assignments = 0;

      success = vt_initialize ();
      gcc_assert (success);

      success = vt_find_locations ();
    }

  if (!success)
    {
      vt_finalize ();
      vt_debug_insns_local (false);
      return 0;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      dump_dataflow_sets ();
      dump_reg_info (dump_file);
      dump_flow_info (dump_file, dump_flags);
    }

  timevar_push (TV_VAR_TRACKING_EMIT);
  vt_emit_notes ();
  timevar_pop (TV_VAR_TRACKING_EMIT);

  vt_finalize ();
  vt_debug_insns_local (false);
  return 0;
}

unsigned int
variable_tracking_main (void)
{
  unsigned int ret;
  int save = flag_var_tracking_assignments;

  ret = variable_tracking_main_1 ();

  flag_var_tracking_assignments = save;

  return ret;
}

namespace {

const pass_data pass_data_variable_tracking =
{
  RTL_PASS, /* type */
  "vartrack", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  true, /* has_execute */
  TV_VAR_TRACKING, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_variable_tracking : public rtl_opt_pass
{
public:
  pass_variable_tracking (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_variable_tracking, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return (flag_var_tracking && !targetm.delay_vartrack);
    }

  virtual unsigned int execute (function *)
    {
      return variable_tracking_main ();
    }

}; // class pass_variable_tracking

} // anon namespace

rtl_opt_pass *
make_pass_variable_tracking (gcc::context *ctxt)
{
  return new pass_variable_tracking (ctxt);
}
