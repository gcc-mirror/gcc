/* Language-independent node constructors for parse phase of GNU compiler.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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

/* This file contains the low level primitives for operating on tree nodes,
   including allocation, list operations, interning of identifiers,
   construction of data type nodes and statement nodes,
   and construction of type conversion nodes.  It also contains
   tables index by tree code that describe how to take apart
   nodes of that code.

   It is intended to be language-independent, but occasionally
   calls language-dependent routines defined (for C) in typecheck.c.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "flags.h"
#include "tree.h"
#include "real.h"
#include "tm_p.h"
#include "function.h"
#include "obstack.h"
#include "toplev.h"
#include "ggc.h"
#include "hashtab.h"
#include "output.h"
#include "target.h"
#include "langhooks.h"

/* obstack.[ch] explicitly declined to prototype this.  */
extern int _obstack_allocated_p (struct obstack *h, void *obj);

#ifdef GATHER_STATISTICS
/* Statistics-gathering stuff.  */

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
  "perm_tree_lists",
  "temp_tree_lists",
  "vecs",
  "random kinds",
  "lang_decl kinds",
  "lang_type kinds"
};
#endif /* GATHER_STATISTICS */

/* Unique id for next decl created.  */
static GTY(()) int next_decl_uid;
/* Unique id for next type created.  */
static GTY(()) int next_type_uid = 1;

/* Since we cannot rehash a type after it is in the table, we have to
   keep the hash code.  */

struct type_hash GTY(())
{
  unsigned long hash;
  tree type;
};

/* Initial size of the hash table (rounded to next prime).  */
#define TYPE_HASH_INITIAL_SIZE 1000

/* Now here is the hash table.  When recording a type, it is added to
   the slot whose index is the hash code.  Note that the hash table is
   used for several kinds of types (function types, array types and
   array index range types, for now).  While all these live in the
   same table, they are completely independent, and the hash code is
   computed differently for each of these.  */

static GTY ((if_marked ("type_hash_marked_p"), param_is (struct type_hash)))
     htab_t type_hash_table;

static void set_type_quals (tree, int);
static int type_hash_eq (const void *, const void *);
static hashval_t type_hash_hash (const void *);
static void print_type_hash_statistics (void);
static void finish_vector_type (tree);
static int type_hash_marked_p (const void *);

tree global_trees[TI_MAX];
tree integer_types[itk_none];

/* Init tree.c.  */

void
init_ttree (void)
{
  /* Initialize the hash table of types.  */
  type_hash_table = htab_create_ggc (TYPE_HASH_INITIAL_SIZE, type_hash_hash,
				     type_hash_eq, 0);
}


/* The name of the object as the assembler will see it (but before any
   translations made by ASM_OUTPUT_LABELREF).  Often this is the same
   as DECL_NAME.  It is an IDENTIFIER_NODE.  */
tree
decl_assembler_name (tree decl)
{
  if (!DECL_ASSEMBLER_NAME_SET_P (decl))
    (*lang_hooks.set_decl_assembler_name) (decl);
  return DECL_CHECK (decl)->decl.assembler_name;
}

/* Compute the number of bytes occupied by 'node'.  This routine only
   looks at TREE_CODE and, if the code is TREE_VEC, TREE_VEC_LENGTH.  */
size_t
tree_size (tree node)
{
  enum tree_code code = TREE_CODE (node);

  switch (TREE_CODE_CLASS (code))
    {
    case 'd':  /* A decl node */
      return sizeof (struct tree_decl);

    case 't':  /* a type node */
      return sizeof (struct tree_type);

    case 'b':  /* a lexical block node */
      return sizeof (struct tree_block);

    case 'r':  /* a reference */
    case 'e':  /* an expression */
    case 's':  /* an expression with side effects */
    case '<':  /* a comparison expression */
    case '1':  /* a unary arithmetic expression */
    case '2':  /* a binary arithmetic expression */
      return (sizeof (struct tree_exp)
	      + TREE_CODE_LENGTH (code) * sizeof (char *) - sizeof (char *));

    case 'c':  /* a constant */
      switch (code)
	{
	case INTEGER_CST:	return sizeof (struct tree_int_cst);
	case REAL_CST:		return sizeof (struct tree_real_cst);
	case COMPLEX_CST:	return sizeof (struct tree_complex);
	case VECTOR_CST:	return sizeof (struct tree_vector);
	case STRING_CST:	return sizeof (struct tree_string);
	default:
	  return (*lang_hooks.tree_size) (code);
	}

    case 'x':  /* something random, like an identifier.  */
      switch (code)
	{
	case IDENTIFIER_NODE:	return lang_hooks.identifier_size;
	case TREE_LIST:		return sizeof (struct tree_list);
	case TREE_VEC:		return (sizeof (struct tree_vec)
					+ TREE_VEC_LENGTH(node) * sizeof(char *)
					- sizeof (char *));

	case ERROR_MARK:
	case PLACEHOLDER_EXPR:	return sizeof (struct tree_common);

	default:
	  return (*lang_hooks.tree_size) (code);
	}

    default:
      abort ();
    }
}

/* Return a newly allocated node of code CODE.
   For decl and type nodes, some other fields are initialized.
   The rest of the node is initialized to zero.

   Achoo!  I got a code in the node.  */

tree
make_node (enum tree_code code)
{
  tree t;
  int type = TREE_CODE_CLASS (code);
  size_t length;
#ifdef GATHER_STATISTICS
  tree_node_kind kind;
#endif
  struct tree_common ttmp;

  /* We can't allocate a TREE_VEC without knowing how many elements
     it will have.  */
  if (code == TREE_VEC)
    abort ();

  TREE_SET_CODE ((tree)&ttmp, code);
  length = tree_size ((tree)&ttmp);

#ifdef GATHER_STATISTICS
  switch (type)
    {
    case 'd':  /* A decl node */
      kind = d_kind;
      break;

    case 't':  /* a type node */
      kind = t_kind;
      break;

    case 'b':  /* a lexical block */
      kind = b_kind;
      break;

    case 's':  /* an expression with side effects */
      kind = s_kind;
      break;

    case 'r':  /* a reference */
      kind = r_kind;
      break;

    case 'e':  /* an expression */
    case '<':  /* a comparison expression */
    case '1':  /* a unary arithmetic expression */
    case '2':  /* a binary arithmetic expression */
      kind = e_kind;
      break;

    case 'c':  /* a constant */
      kind = c_kind;
      break;

    case 'x':  /* something random, like an identifier.  */
      if (code == IDENTIFIER_NODE)
	kind = id_kind;
      else if (code == TREE_VEC)
	kind = vec_kind;
      else
	kind = x_kind;
      break;

    default:
      abort ();
    }

  tree_node_counts[(int) kind]++;
  tree_node_sizes[(int) kind] += length;
#endif

  t = ggc_alloc_tree (length);

  memset (t, 0, length);

  TREE_SET_CODE (t, code);

  switch (type)
    {
    case 's':
      TREE_SIDE_EFFECTS (t) = 1;
      break;

    case 'd':
      if (code != FUNCTION_DECL)
	DECL_ALIGN (t) = 1;
      DECL_USER_ALIGN (t) = 0;
      DECL_IN_SYSTEM_HEADER (t) = in_system_header;
      DECL_SOURCE_LOCATION (t) = input_location;
      DECL_UID (t) = next_decl_uid++;

      /* We have not yet computed the alias set for this declaration.  */
      DECL_POINTER_ALIAS_SET (t) = -1;
      break;

    case 't':
      TYPE_UID (t) = next_type_uid++;
      TYPE_ALIGN (t) = char_type_node ? TYPE_ALIGN (char_type_node) : 0;
      TYPE_USER_ALIGN (t) = 0;
      TYPE_MAIN_VARIANT (t) = t;

      /* Default to no attributes for type, but let target change that.  */
      TYPE_ATTRIBUTES (t) = NULL_TREE;
      (*targetm.set_default_type_attributes) (t);

      /* We have not yet computed the alias set for this type.  */
      TYPE_ALIAS_SET (t) = -1;
      break;

    case 'c':
      TREE_CONSTANT (t) = 1;
      break;

    case 'e':
      switch (code)
	{
	case INIT_EXPR:
	case MODIFY_EXPR:
	case VA_ARG_EXPR:
	case RTL_EXPR:
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
    }

  return t;
}

/* Return a new node with the same contents as NODE except that its
   TREE_CHAIN is zero and it has a fresh uid.  */

tree
copy_node (tree node)
{
  tree t;
  enum tree_code code = TREE_CODE (node);
  size_t length;

  length = tree_size (node);
  t = ggc_alloc_tree (length);
  memcpy (t, node, length);

  TREE_CHAIN (t) = 0;
  TREE_ASM_WRITTEN (t) = 0;

  if (TREE_CODE_CLASS (code) == 'd')
    DECL_UID (t) = next_decl_uid++;
  else if (TREE_CODE_CLASS (code) == 't')
    {
      TYPE_UID (t) = next_type_uid++;
      /* The following is so that the debug code for
	 the copy is different from the original type.
	 The two statements usually duplicate each other
	 (because they clear fields of the same union),
	 but the optimizer should catch that.  */
      TYPE_SYMTAB_POINTER (t) = 0;
      TYPE_SYMTAB_ADDRESS (t) = 0;
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


/* Return a newly constructed INTEGER_CST node whose constant value
   is specified by the two ints LOW and HI.
   The TREE_TYPE is set to `int'.

   This function should be used via the `build_int_2' macro.  */

tree
build_int_2_wide (unsigned HOST_WIDE_INT low, HOST_WIDE_INT hi)
{
  tree t = make_node (INTEGER_CST);

  TREE_INT_CST_LOW (t) = low;
  TREE_INT_CST_HIGH (t) = hi;
  TREE_TYPE (t) = integer_type_node;
  return t;
}

/* Return a new VECTOR_CST node whose type is TYPE and whose values
   are in a list pointed by VALS.  */

tree
build_vector (tree type, tree vals)
{
  tree v = make_node (VECTOR_CST);
  int over1 = 0, over2 = 0;
  tree link;

  TREE_VECTOR_CST_ELTS (v) = vals;
  TREE_TYPE (v) = type;

  /* Iterate through elements and check for overflow.  */
  for (link = vals; link; link = TREE_CHAIN (link))
    {
      tree value = TREE_VALUE (link);

      over1 |= TREE_OVERFLOW (value);
      over2 |= TREE_CONSTANT_OVERFLOW (value);
    }

  TREE_OVERFLOW (v) = over1;
  TREE_CONSTANT_OVERFLOW (v) = over2;

  return v;
}

/* Return a new CONSTRUCTOR node whose type is TYPE and whose values
   are in a list pointed to by VALS.  */
tree
build_constructor (tree type, tree vals)
{
  tree c = make_node (CONSTRUCTOR);
  TREE_TYPE (c) = type;
  CONSTRUCTOR_ELTS (c) = vals;

  /* ??? May not be necessary.  Mirrors what build does.  */
  if (vals)
    {
      TREE_SIDE_EFFECTS (c) = TREE_SIDE_EFFECTS (vals);
      TREE_READONLY (c) = TREE_READONLY (vals);
      TREE_CONSTANT (c) = TREE_CONSTANT (vals);
    }
  else
    TREE_CONSTANT (c) = 0;  /* safe side */

  return c;
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
  dp = ggc_alloc (sizeof (REAL_VALUE_TYPE));
  memcpy (dp, &d, sizeof (REAL_VALUE_TYPE));

  TREE_TYPE (v) = type;
  TREE_REAL_CST_PTR (v) = dp;
  TREE_OVERFLOW (v) = TREE_CONSTANT_OVERFLOW (v) = overflow;
  return v;
}

/* Return a new REAL_CST node whose type is TYPE
   and whose value is the integer value of the INTEGER_CST node I.  */

REAL_VALUE_TYPE
real_value_from_int_cst (tree type, tree i)
{
  REAL_VALUE_TYPE d;

  /* Clear all bits of the real value type so that we can later do
     bitwise comparisons to see if two values are the same.  */
  memset (&d, 0, sizeof d);

  real_from_integer (&d, type ? TYPE_MODE (type) : VOIDmode,
		     TREE_INT_CST_LOW (i), TREE_INT_CST_HIGH (i),
		     TREE_UNSIGNED (TREE_TYPE (i)));
  return d;
}

/* Given a tree representing an integer constant I, return a tree
   representing the same value as a floating-point constant of type TYPE.  */

tree
build_real_from_int_cst (tree type, tree i)
{
  tree v;
  int overflow = TREE_OVERFLOW (i);

  v = build_real (type, real_value_from_int_cst (type, i));

  TREE_OVERFLOW (v) |= overflow;
  TREE_CONSTANT_OVERFLOW (v) |= overflow;
  return v;
}

/* Return a newly constructed STRING_CST node whose value is
   the LEN characters at STR.
   The TREE_TYPE is not initialized.  */

tree
build_string (int len, const char *str)
{
  tree s = make_node (STRING_CST);

  TREE_STRING_LENGTH (s) = len;
  TREE_STRING_POINTER (s) = ggc_alloc_string (str, len);

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
  TREE_CONSTANT_OVERFLOW (t)
    = TREE_CONSTANT_OVERFLOW (real) | TREE_CONSTANT_OVERFLOW (imag);
  return t;
}

/* Build a newly constructed TREE_VEC node of length LEN.  */

tree
make_tree_vec (int len)
{
  tree t;
  int length = (len - 1) * sizeof (tree) + sizeof (struct tree_vec);

#ifdef GATHER_STATISTICS
  tree_node_counts[(int) vec_kind]++;
  tree_node_sizes[(int) vec_kind] += length;
#endif

  t = ggc_alloc_tree (length);

  memset (t, 0, length);
  TREE_SET_CODE (t, TREE_VEC);
  TREE_VEC_LENGTH (t) = len;

  return t;
}

/* Return 1 if EXPR is the integer constant zero or a complex constant
   of zero.  */

int
integer_zerop (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == INTEGER_CST
	   && ! TREE_CONSTANT_OVERFLOW (expr)
	   && TREE_INT_CST_LOW (expr) == 0
	   && TREE_INT_CST_HIGH (expr) == 0)
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && integer_zerop (TREE_REALPART (expr))
	      && integer_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is the integer constant one or the corresponding
   complex constant.  */

int
integer_onep (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == INTEGER_CST
	   && ! TREE_CONSTANT_OVERFLOW (expr)
	   && TREE_INT_CST_LOW (expr) == 1
	   && TREE_INT_CST_HIGH (expr) == 0)
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && integer_onep (TREE_REALPART (expr))
	      && integer_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is an integer containing all 1's in as much precision as
   it contains.  Likewise for the corresponding complex constant.  */

int
integer_all_onesp (tree expr)
{
  int prec;
  int uns;

  STRIP_NOPS (expr);

  if (TREE_CODE (expr) == COMPLEX_CST
      && integer_all_onesp (TREE_REALPART (expr))
      && integer_zerop (TREE_IMAGPART (expr)))
    return 1;

  else if (TREE_CODE (expr) != INTEGER_CST
	   || TREE_CONSTANT_OVERFLOW (expr))
    return 0;

  uns = TREE_UNSIGNED (TREE_TYPE (expr));
  if (!uns)
    return (TREE_INT_CST_LOW (expr) == ~(unsigned HOST_WIDE_INT) 0
	    && TREE_INT_CST_HIGH (expr) == -1);

  /* Note that using TYPE_PRECISION here is wrong.  We care about the
     actual bits, not the (arbitrary) range of the type.  */
  prec = GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (expr)));
  if (prec >= HOST_BITS_PER_WIDE_INT)
    {
      HOST_WIDE_INT high_value;
      int shift_amount;

      shift_amount = prec - HOST_BITS_PER_WIDE_INT;

      if (shift_amount > HOST_BITS_PER_WIDE_INT)
	/* Can not handle precisions greater than twice the host int size.  */
	abort ();
      else if (shift_amount == HOST_BITS_PER_WIDE_INT)
	/* Shifting by the host word size is undefined according to the ANSI
	   standard, so we must handle this as a special case.  */
	high_value = -1;
      else
	high_value = ((HOST_WIDE_INT) 1 << shift_amount) - 1;

      return (TREE_INT_CST_LOW (expr) == ~(unsigned HOST_WIDE_INT) 0
	      && TREE_INT_CST_HIGH (expr) == high_value);
    }
  else
    return TREE_INT_CST_LOW (expr) == ((unsigned HOST_WIDE_INT) 1 << prec) - 1;
}

/* Return 1 if EXPR is an integer constant that is a power of 2 (i.e., has only
   one bit on).  */

int
integer_pow2p (tree expr)
{
  int prec;
  HOST_WIDE_INT high, low;

  STRIP_NOPS (expr);

  if (TREE_CODE (expr) == COMPLEX_CST
      && integer_pow2p (TREE_REALPART (expr))
      && integer_zerop (TREE_IMAGPART (expr)))
    return 1;

  if (TREE_CODE (expr) != INTEGER_CST || TREE_CONSTANT_OVERFLOW (expr))
    return 0;

  prec = (POINTER_TYPE_P (TREE_TYPE (expr))
	  ? POINTER_SIZE : TYPE_PRECISION (TREE_TYPE (expr)));
  high = TREE_INT_CST_HIGH (expr);
  low = TREE_INT_CST_LOW (expr);

  /* First clear all bits that are beyond the type's precision in case
     we've been sign extended.  */

  if (prec == 2 * HOST_BITS_PER_WIDE_INT)
    ;
  else if (prec > HOST_BITS_PER_WIDE_INT)
    high &= ~((HOST_WIDE_INT) (-1) << (prec - HOST_BITS_PER_WIDE_INT));
  else
    {
      high = 0;
      if (prec < HOST_BITS_PER_WIDE_INT)
	low &= ~((HOST_WIDE_INT) (-1) << prec);
    }

  if (high == 0 && low == 0)
    return 0;

  return ((high == 0 && (low & (low - 1)) == 0)
	  || (low == 0 && (high & (high - 1)) == 0));
}

/* Return 1 if EXPR is an integer constant other than zero or a
   complex constant other than zero.  */

int
integer_nonzerop (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == INTEGER_CST
	   && ! TREE_CONSTANT_OVERFLOW (expr)
	   && (TREE_INT_CST_LOW (expr) != 0
	       || TREE_INT_CST_HIGH (expr) != 0))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && (integer_nonzerop (TREE_REALPART (expr))
		  || integer_nonzerop (TREE_IMAGPART (expr)))));
}

/* Return the power of two represented by a tree node known to be a
   power of two.  */

int
tree_log2 (tree expr)
{
  int prec;
  HOST_WIDE_INT high, low;

  STRIP_NOPS (expr);

  if (TREE_CODE (expr) == COMPLEX_CST)
    return tree_log2 (TREE_REALPART (expr));

  prec = (POINTER_TYPE_P (TREE_TYPE (expr))
	  ? POINTER_SIZE : TYPE_PRECISION (TREE_TYPE (expr)));

  high = TREE_INT_CST_HIGH (expr);
  low = TREE_INT_CST_LOW (expr);

  /* First clear all bits that are beyond the type's precision in case
     we've been sign extended.  */

  if (prec == 2 * HOST_BITS_PER_WIDE_INT)
    ;
  else if (prec > HOST_BITS_PER_WIDE_INT)
    high &= ~((HOST_WIDE_INT) (-1) << (prec - HOST_BITS_PER_WIDE_INT));
  else
    {
      high = 0;
      if (prec < HOST_BITS_PER_WIDE_INT)
	low &= ~((HOST_WIDE_INT) (-1) << prec);
    }

  return (high != 0 ? HOST_BITS_PER_WIDE_INT + exact_log2 (high)
	  : exact_log2 (low));
}

/* Similar, but return the largest integer Y such that 2 ** Y is less
   than or equal to EXPR.  */

int
tree_floor_log2 (tree expr)
{
  int prec;
  HOST_WIDE_INT high, low;

  STRIP_NOPS (expr);

  if (TREE_CODE (expr) == COMPLEX_CST)
    return tree_log2 (TREE_REALPART (expr));

  prec = (POINTER_TYPE_P (TREE_TYPE (expr))
	  ? POINTER_SIZE : TYPE_PRECISION (TREE_TYPE (expr)));

  high = TREE_INT_CST_HIGH (expr);
  low = TREE_INT_CST_LOW (expr);

  /* First clear all bits that are beyond the type's precision in case
     we've been sign extended.  Ignore if type's precision hasn't been set
     since what we are doing is setting it.  */

  if (prec == 2 * HOST_BITS_PER_WIDE_INT || prec == 0)
    ;
  else if (prec > HOST_BITS_PER_WIDE_INT)
    high &= ~((HOST_WIDE_INT) (-1) << (prec - HOST_BITS_PER_WIDE_INT));
  else
    {
      high = 0;
      if (prec < HOST_BITS_PER_WIDE_INT)
	low &= ~((HOST_WIDE_INT) (-1) << prec);
    }

  return (high != 0 ? HOST_BITS_PER_WIDE_INT + floor_log2 (high)
	  : floor_log2 (low));
}

/* Return 1 if EXPR is the real constant zero.  */

int
real_zerop (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == REAL_CST
	   && ! TREE_CONSTANT_OVERFLOW (expr)
	   && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconst0))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && real_zerop (TREE_REALPART (expr))
	      && real_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is the real constant one in real or complex form.  */

int
real_onep (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == REAL_CST
	   && ! TREE_CONSTANT_OVERFLOW (expr)
	   && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconst1))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && real_onep (TREE_REALPART (expr))
	      && real_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is the real constant two.  */

int
real_twop (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == REAL_CST
	   && ! TREE_CONSTANT_OVERFLOW (expr)
	   && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconst2))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && real_twop (TREE_REALPART (expr))
	      && real_zerop (TREE_IMAGPART (expr))));
}

/* Return 1 if EXPR is the real constant minus one.  */

int
real_minus_onep (tree expr)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == REAL_CST
	   && ! TREE_CONSTANT_OVERFLOW (expr)
	   && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), dconstm1))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && real_minus_onep (TREE_REALPART (expr))
	      && real_zerop (TREE_IMAGPART (expr))));
}

/* Nonzero if EXP is a constant or a cast of a constant.  */

int
really_constant_p (tree exp)
{
  /* This is not quite the same as STRIP_NOPS.  It does more.  */
  while (TREE_CODE (exp) == NOP_EXPR
	 || TREE_CODE (exp) == CONVERT_EXPR
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
purpose_member (tree elem, tree list)
{
  while (list)
    {
      if (elem == TREE_PURPOSE (list))
	return list;
      list = TREE_CHAIN (list);
    }
  return NULL_TREE;
}

/* Return first list element whose BINFO_TYPE is ELEM.
   Return 0 if ELEM is not in LIST.  */

tree
binfo_member (tree elem, tree list)
{
  while (list)
    {
      if (elem == BINFO_TYPE (list))
	return list;
      list = TREE_CHAIN (list);
    }
  return NULL_TREE;
}

/* Return nonzero if ELEM is part of the chain CHAIN.  */

int
chain_member (tree elem, tree chain)
{
  while (chain)
    {
      if (elem == chain)
	return 1;
      chain = TREE_CHAIN (chain);
    }

  return 0;
}

/* Return the length of a chain of nodes chained through TREE_CHAIN.
   We expect a null pointer to mark the end of the chain.
   This is the Lisp primitive `length'.  */

int
list_length (tree t)
{
  tree tail;
  int len = 0;

  for (tail = t; tail; tail = TREE_CHAIN (tail))
    len++;

  return len;
}

/* Returns the number of FIELD_DECLs in TYPE.  */

int
fields_length (tree type)
{
  tree t = TYPE_FIELDS (type);
  int count = 0;

  for (; t; t = TREE_CHAIN (t))
    if (TREE_CODE (t) == FIELD_DECL)
      ++count;

  return count;
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
      if (t2 == t1)
	abort ();  /* Circularity created.  */
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
      next = TREE_CHAIN (decl);
      TREE_CHAIN (decl) = prev;
      prev = decl;
    }
  return prev;
}

/* Return a newly created TREE_LIST node whose
   purpose and value fields are PARM and VALUE.  */

tree
build_tree_list (tree parm, tree value)
{
  tree t = make_node (TREE_LIST);
  TREE_PURPOSE (t) = parm;
  TREE_VALUE (t) = value;
  return t;
}

/* Return a newly created TREE_LIST node whose
   purpose and value fields are PURPOSE and VALUE
   and whose TREE_CHAIN is CHAIN.  */

tree
tree_cons (tree purpose, tree value, tree chain)
{
  tree node;

  node = ggc_alloc_tree (sizeof (struct tree_list));

  memset (node, 0, sizeof (struct tree_common));

#ifdef GATHER_STATISTICS
  tree_node_counts[(int) x_kind]++;
  tree_node_sizes[(int) x_kind] += sizeof (struct tree_list);
#endif

  TREE_SET_CODE (node, TREE_LIST);
  TREE_CHAIN (node) = chain;
  TREE_PURPOSE (node) = purpose;
  TREE_VALUE (node) = value;
  return node;
}

/* Return the first expression in a sequence of COMPOUND_EXPRs.  */

tree
expr_first (tree expr)
{
  if (expr == NULL_TREE)
    return expr;
  while (TREE_CODE (expr) == COMPOUND_EXPR)
    expr = TREE_OPERAND (expr, 0);
  return expr;
}

/* Return the last expression in a sequence of COMPOUND_EXPRs.  */

tree
expr_last (tree expr)
{
  if (expr == NULL_TREE)
    return expr;
  while (TREE_CODE (expr) == COMPOUND_EXPR)
    expr = TREE_OPERAND (expr, 1);
  return expr;
}

/* Return the number of subexpressions in a sequence of COMPOUND_EXPRs.  */

int
expr_length (tree expr)
{
  int len = 0;

  if (expr == NULL_TREE)
    return 0;
  for (; TREE_CODE (expr) == COMPOUND_EXPR; expr = TREE_OPERAND (expr, 1))
    len += expr_length (TREE_OPERAND (expr, 0));
  ++len;
  return len;
}

/* Return the size nominally occupied by an object of type TYPE
   when it resides in memory.  The value is measured in units of bytes,
   and its data type is that normally used for type sizes
   (which is the first type created by make_signed_type or
   make_unsigned_type).  */

tree
size_in_bytes (tree type)
{
  tree t;

  if (type == error_mark_node)
    return integer_zero_node;

  type = TYPE_MAIN_VARIANT (type);
  t = TYPE_SIZE_UNIT (type);

  if (t == 0)
    {
      (*lang_hooks.types.incomplete_type_error) (NULL_TREE, type);
      return size_zero_node;
    }

  if (TREE_CODE (t) == INTEGER_CST)
    force_fit_type (t, 0);

  return t;
}

/* Return the size of TYPE (in bytes) as a wide integer
   or return -1 if the size can vary or is larger than an integer.  */

HOST_WIDE_INT
int_size_in_bytes (tree type)
{
  tree t;

  if (type == error_mark_node)
    return 0;

  type = TYPE_MAIN_VARIANT (type);
  t = TYPE_SIZE_UNIT (type);
  if (t == 0
      || TREE_CODE (t) != INTEGER_CST
      || TREE_OVERFLOW (t)
      || TREE_INT_CST_HIGH (t) != 0
      /* If the result would appear negative, it's too big to represent.  */
      || (HOST_WIDE_INT) TREE_INT_CST_LOW (t) < 0)
    return -1;

  return TREE_INT_CST_LOW (t);
}

/* Return the bit position of FIELD, in bits from the start of the record.
   This is a tree of type bitsizetype.  */

tree
bit_position (tree field)
{
  return bit_from_pos (DECL_FIELD_OFFSET (field),
		       DECL_FIELD_BIT_OFFSET (field));
}

/* Likewise, but return as an integer.  Abort if it cannot be represented
   in that way (since it could be a signed value, we don't have the option
   of returning -1 like int_size_in_byte can.  */

HOST_WIDE_INT
int_bit_position (tree field)
{
  return tree_low_cst (bit_position (field), 0);
}

/* Return the byte position of FIELD, in bytes from the start of the record.
   This is a tree of type sizetype.  */

tree
byte_position (tree field)
{
  return byte_from_pos (DECL_FIELD_OFFSET (field),
			DECL_FIELD_BIT_OFFSET (field));
}

/* Likewise, but return as an integer.  Abort if it cannot be represented
   in that way (since it could be a signed value, we don't have the option
   of returning -1 like int_size_in_byte can.  */

HOST_WIDE_INT
int_byte_position (tree field)
{
  return tree_low_cst (byte_position (field), 0);
}

/* Return the strictest alignment, in bits, that T is known to have.  */

unsigned int
expr_align (tree t)
{
  unsigned int align0, align1;

  switch (TREE_CODE (t))
    {
    case NOP_EXPR:  case CONVERT_EXPR:  case NON_LVALUE_EXPR:
      /* If we have conversions, we know that the alignment of the
	 object must meet each of the alignments of the types.  */
      align0 = expr_align (TREE_OPERAND (t, 0));
      align1 = TYPE_ALIGN (TREE_TYPE (t));
      return MAX (align0, align1);

    case SAVE_EXPR:         case COMPOUND_EXPR:       case MODIFY_EXPR:
    case INIT_EXPR:         case TARGET_EXPR:         case WITH_CLEANUP_EXPR:
    case WITH_RECORD_EXPR:  case CLEANUP_POINT_EXPR:  case UNSAVE_EXPR:
      /* These don't change the alignment of an object.  */
      return expr_align (TREE_OPERAND (t, 0));

    case COND_EXPR:
      /* The best we can do is say that the alignment is the least aligned
	 of the two arms.  */
      align0 = expr_align (TREE_OPERAND (t, 1));
      align1 = expr_align (TREE_OPERAND (t, 2));
      return MIN (align0, align1);

    case LABEL_DECL:     case CONST_DECL:
    case VAR_DECL:       case PARM_DECL:   case RESULT_DECL:
      if (DECL_ALIGN (t) != 0)
	return DECL_ALIGN (t);
      break;

    case FUNCTION_DECL:
      return FUNCTION_BOUNDARY;

    default:
      break;
    }

  /* Otherwise take the alignment from that of the type.  */
  return TYPE_ALIGN (TREE_TYPE (t));
}

/* Return, as a tree node, the number of elements for TYPE (which is an
   ARRAY_TYPE) minus one. This counts only elements of the top array.  */

tree
array_type_nelts (tree type)
{
  tree index_type, min, max;

  /* If they did it with unspecified bounds, then we should have already
     given an error about it before we got here.  */
  if (! TYPE_DOMAIN (type))
    return error_mark_node;

  index_type = TYPE_DOMAIN (type);
  min = TYPE_MIN_VALUE (index_type);
  max = TYPE_MAX_VALUE (index_type);

  return (integer_zerop (min)
	  ? max
	  : fold (build (MINUS_EXPR, TREE_TYPE (max), max, min)));
}

/* Return nonzero if arg is static -- a reference to an object in
   static storage.  This is not the same as the C meaning of `static'.  */

int
staticp (tree arg)
{
  switch (TREE_CODE (arg))
    {
    case FUNCTION_DECL:
      /* Nested functions aren't static, since taking their address
	 involves a trampoline.  */
      return ((decl_function_context (arg) == 0 || DECL_NO_STATIC_CHAIN (arg))
	      && ! DECL_NON_ADDR_CONST_P (arg));

    case VAR_DECL:
      return ((TREE_STATIC (arg) || DECL_EXTERNAL (arg))
	      && ! DECL_THREAD_LOCAL (arg)
	      && ! DECL_NON_ADDR_CONST_P (arg));

    case CONSTRUCTOR:
      return TREE_STATIC (arg);

    case LABEL_DECL:
    case STRING_CST:
      return 1;

      /* If we are referencing a bitfield, we can't evaluate an
	 ADDR_EXPR at compile time and so it isn't a constant.  */
    case COMPONENT_REF:
      return (! DECL_BIT_FIELD (TREE_OPERAND (arg, 1))
	      && staticp (TREE_OPERAND (arg, 0)));

    case BIT_FIELD_REF:
      return 0;

#if 0
       /* This case is technically correct, but results in setting
	  TREE_CONSTANT on ADDR_EXPRs that cannot be evaluated at
	  compile time.  */
    case INDIRECT_REF:
      return TREE_CONSTANT (TREE_OPERAND (arg, 0));
#endif

    case ARRAY_REF:
    case ARRAY_RANGE_REF:
      if (TREE_CODE (TYPE_SIZE (TREE_TYPE (arg))) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg, 1)) == INTEGER_CST)
	return staticp (TREE_OPERAND (arg, 0));

    default:
      if ((unsigned int) TREE_CODE (arg)
	  >= (unsigned int) LAST_AND_UNUSED_TREE_CODE)
	return (*lang_hooks.staticp) (arg);
      else
	return 0;
    }
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
  if (TREE_CONSTANT (inner)
      || (TREE_READONLY (inner) && ! TREE_SIDE_EFFECTS (inner))
      || TREE_CODE (inner) == SAVE_EXPR
      || TREE_CODE (inner) == ERROR_MARK)
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

  t = build (SAVE_EXPR, TREE_TYPE (expr), t, current_function_decl, NULL_TREE);

  /* This expression might be placed ahead of a jump to ensure that the
     value was computed on both sides of the jump.  So make sure it isn't
     eliminated as dead.  */
  TREE_SIDE_EFFECTS (t) = 1;
  TREE_READONLY (t) = 1;
  return t;
}

/* Look inside EXPR and into any simple arithmetic operations.  Return
   the innermost non-arithmetic node.  */

tree
skip_simple_arithmetic (tree expr)
{
  tree inner;

  /* We don't care about whether this can be used as an lvalue in this
     context.  */
  while (TREE_CODE (expr) == NON_LVALUE_EXPR)
    expr = TREE_OPERAND (expr, 0);

  /* If we have simple operations applied to a SAVE_EXPR or to a SAVE_EXPR and
     a constant, it will be more efficient to not make another SAVE_EXPR since
     it will allow better simplification and GCSE will be able to merge the
     computations if they actually occur.  */
  inner = expr;
  while (1)
    {
      if (TREE_CODE_CLASS (TREE_CODE (inner)) == '1')
	inner = TREE_OPERAND (inner, 0);
      else if (TREE_CODE_CLASS (TREE_CODE (inner)) == '2')
	{
	  if (TREE_CONSTANT (TREE_OPERAND (inner, 1)))
	    inner = TREE_OPERAND (inner, 0);
	  else if (TREE_CONSTANT (TREE_OPERAND (inner, 0)))
	    inner = TREE_OPERAND (inner, 1);
	  else
	    break;
	}
      else
	break;
    }

  return inner;
}

/* Return TRUE if EXPR is a SAVE_EXPR or wraps simple arithmetic around a
   SAVE_EXPR.  Return FALSE otherwise.  */

bool
saved_expr_p (tree expr)
{
  return TREE_CODE (skip_simple_arithmetic (expr)) == SAVE_EXPR;
}

/* Arrange for an expression to be expanded multiple independent
   times.  This is useful for cleanup actions, as the backend can
   expand them multiple times in different places.  */

tree
unsave_expr (tree expr)
{
  tree t;

  /* If this is already protected, no sense in protecting it again.  */
  if (TREE_CODE (expr) == UNSAVE_EXPR)
    return expr;

  t = build1 (UNSAVE_EXPR, TREE_TYPE (expr), expr);
  TREE_SIDE_EFFECTS (t) = TREE_SIDE_EFFECTS (expr);
  return t;
}

/* Returns the index of the first non-tree operand for CODE, or the number
   of operands if all are trees.  */

int
first_rtl_op (enum tree_code code)
{
  switch (code)
    {
    case SAVE_EXPR:
      return 2;
    case GOTO_SUBROUTINE_EXPR:
    case RTL_EXPR:
      return 0;
    case WITH_CLEANUP_EXPR:
      return 2;
    default:
      return TREE_CODE_LENGTH (code);
    }
}

/* Return which tree structure is used by T.  */

enum tree_node_structure_enum
tree_node_structure (tree t)
{
  enum tree_code code = TREE_CODE (t);

  switch (TREE_CODE_CLASS (code))
    {
    case 'd':	return TS_DECL;
    case 't':	return TS_TYPE;
    case 'b':	return TS_BLOCK;
    case 'r': case '<': case '1': case '2': case 'e': case 's':
      return TS_EXP;
    default:  /* 'c' and 'x' */
      break;
    }
  switch (code)
    {
      /* 'c' cases.  */
    case INTEGER_CST:		return TS_INT_CST;
    case REAL_CST:		return TS_REAL_CST;
    case COMPLEX_CST:		return TS_COMPLEX;
    case VECTOR_CST:		return TS_VECTOR;
    case STRING_CST:		return TS_STRING;
      /* 'x' cases.  */
    case ERROR_MARK:		return TS_COMMON;
    case IDENTIFIER_NODE:	return TS_IDENTIFIER;
    case TREE_LIST:		return TS_LIST;
    case TREE_VEC:		return TS_VEC;
    case PLACEHOLDER_EXPR:	return TS_COMMON;

    default:
      abort ();
    }
}

/* Perform any modifications to EXPR required when it is unsaved.  Does
   not recurse into EXPR's subtrees.  */

void
unsave_expr_1 (tree expr)
{
  switch (TREE_CODE (expr))
    {
    case SAVE_EXPR:
      if (! SAVE_EXPR_PERSISTENT_P (expr))
	SAVE_EXPR_RTL (expr) = 0;
      break;

    case TARGET_EXPR:
      /* Don't mess with a TARGET_EXPR that hasn't been expanded.
         It's OK for this to happen if it was part of a subtree that
         isn't immediately expanded, such as operand 2 of another
         TARGET_EXPR.  */
      if (TREE_OPERAND (expr, 1))
	break;

      TREE_OPERAND (expr, 1) = TREE_OPERAND (expr, 3);
      TREE_OPERAND (expr, 3) = NULL_TREE;
      break;

    case RTL_EXPR:
      /* I don't yet know how to emit a sequence multiple times.  */
      if (RTL_EXPR_SEQUENCE (expr) != 0)
	abort ();
      break;

    default:
      break;
    }
}

/* Default lang hook for "unsave_expr_now".  */

tree
lhd_unsave_expr_now (tree expr)
{
  enum tree_code code;

  /* There's nothing to do for NULL_TREE.  */
  if (expr == 0)
    return expr;

  unsave_expr_1 (expr);

  code = TREE_CODE (expr);
  switch (TREE_CODE_CLASS (code))
    {
    case 'c':  /* a constant */
    case 't':  /* a type node */
    case 'd':  /* A decl node */
    case 'b':  /* A block node */
      break;

    case 'x':  /* miscellaneous: e.g., identifier, TREE_LIST or ERROR_MARK.  */
      if (code == TREE_LIST)
	{
	  lhd_unsave_expr_now (TREE_VALUE (expr));
	  lhd_unsave_expr_now (TREE_CHAIN (expr));
	}
      break;

    case 'e':  /* an expression */
    case 'r':  /* a reference */
    case 's':  /* an expression with side effects */
    case '<':  /* a comparison expression */
    case '2':  /* a binary arithmetic expression */
    case '1':  /* a unary arithmetic expression */
      {
	int i;

	for (i = first_rtl_op (code) - 1; i >= 0; i--)
	  lhd_unsave_expr_now (TREE_OPERAND (expr, i));
      }
      break;

    default:
      abort ();
    }

  return expr;
}

/* Return 0 if it is safe to evaluate EXPR multiple times,
   return 1 if it is safe if EXPR is unsaved afterward, or
   return 2 if it is completely unsafe.

   This assumes that CALL_EXPRs and TARGET_EXPRs are never replicated in
   an expression tree, so that it safe to unsave them and the surrounding
   context will be correct.

   SAVE_EXPRs basically *only* appear replicated in an expression tree,
   occasionally across the whole of a function.  It is therefore only
   safe to unsave a SAVE_EXPR if you know that all occurrences appear
   below the UNSAVE_EXPR.

   RTL_EXPRs consume their rtl during evaluation.  It is therefore
   never possible to unsave them.  */

int
unsafe_for_reeval (tree expr)
{
  int unsafeness = 0;
  enum tree_code code;
  int i, tmp, tmp2;
  tree exp;
  int first_rtl;

  if (expr == NULL_TREE)
    return 1;

  code = TREE_CODE (expr);
  first_rtl = first_rtl_op (code);

  switch (code)
    {
    case SAVE_EXPR:
    case RTL_EXPR:
    case TRY_CATCH_EXPR:
      return 2;

    case TREE_LIST:
      for (exp = expr; exp != 0; exp = TREE_CHAIN (exp))
	{
	  tmp = unsafe_for_reeval (TREE_VALUE (exp));
	  unsafeness = MAX (tmp, unsafeness);
	}

      return unsafeness;

    case CALL_EXPR:
      tmp2 = unsafe_for_reeval (TREE_OPERAND (expr, 0));
      tmp = unsafe_for_reeval (TREE_OPERAND (expr, 1));
      return MAX (MAX (tmp, 1), tmp2);

    case TARGET_EXPR:
      unsafeness = 1;
      break;

    case EXIT_BLOCK_EXPR:
      /* EXIT_BLOCK_LABELED_BLOCK, a.k.a. TREE_OPERAND (expr, 0), holds
	 a reference to an ancestor LABELED_BLOCK, so we need to avoid
	 unbounded recursion in the 'e' traversal code below.  */
      exp = EXIT_BLOCK_RETURN (expr);
      return exp ? unsafe_for_reeval (exp) : 0;

    default:
      tmp = (*lang_hooks.unsafe_for_reeval) (expr);
      if (tmp >= 0)
	return tmp;
      break;
    }

  switch (TREE_CODE_CLASS (code))
    {
    case 'c':  /* a constant */
    case 't':  /* a type node */
    case 'x':  /* something random, like an identifier or an ERROR_MARK.  */
    case 'd':  /* A decl node */
    case 'b':  /* A block node */
      return 0;

    case 'e':  /* an expression */
    case 'r':  /* a reference */
    case 's':  /* an expression with side effects */
    case '<':  /* a comparison expression */
    case '2':  /* a binary arithmetic expression */
    case '1':  /* a unary arithmetic expression */
      for (i = first_rtl - 1; i >= 0; i--)
	{
	  tmp = unsafe_for_reeval (TREE_OPERAND (expr, i));
	  unsafeness = MAX (tmp, unsafeness);
	}

      return unsafeness;

    default:
      return 2;
    }
}

/* Return 1 if EXP contains a PLACEHOLDER_EXPR; i.e., if it represents a size
   or offset that depends on a field within a record.  */

bool
contains_placeholder_p (tree exp)
{
  enum tree_code code;
  int result;

  if (!exp)
    return 0;

  /* If we have a WITH_RECORD_EXPR, it "cancels" any PLACEHOLDER_EXPR
     in it since it is supplying a value for it.  */
  code = TREE_CODE (exp);
  if (code == WITH_RECORD_EXPR)
    return 0;
  else if (code == PLACEHOLDER_EXPR)
    return 1;

  switch (TREE_CODE_CLASS (code))
    {
    case 'r':
      /* Don't look at any PLACEHOLDER_EXPRs that might be in index or bit
	 position computations since they will be converted into a
	 WITH_RECORD_EXPR involving the reference, which will assume
	 here will be valid.  */
      return CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 0));

    case 'x':
      if (code == TREE_LIST)
	return (CONTAINS_PLACEHOLDER_P (TREE_VALUE (exp))
		|| CONTAINS_PLACEHOLDER_P (TREE_CHAIN (exp)));
      break;

    case '1':
    case '2':  case '<':
    case 'e':
      switch (code)
	{
	case COMPOUND_EXPR:
	  /* Ignoring the first operand isn't quite right, but works best.  */
	  return CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 1));

	case RTL_EXPR:
	case CONSTRUCTOR:
	  return 0;

	case COND_EXPR:
	  return (CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 0))
		  || CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 1))
		  || CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 2)));

	case SAVE_EXPR:
	  /* If we already know this doesn't have a placeholder, don't
	     check again.  */
	  if (SAVE_EXPR_NOPLACEHOLDER (exp) || SAVE_EXPR_RTL (exp) != 0)
	    return 0;

	  SAVE_EXPR_NOPLACEHOLDER (exp) = 1;
	  result = CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 0));
	  if (result)
	    SAVE_EXPR_NOPLACEHOLDER (exp) = 0;

	  return result;

	case CALL_EXPR:
	  return CONTAINS_PLACEHOLDER_P (TREE_OPERAND (exp, 1));

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

    default:
      return 0;
    }
  return 0;
}

/* Return 1 if any part of the computation of TYPE involves a PLACEHOLDER_EXPR.
   This includes size, bounds, qualifiers (for QUAL_UNION_TYPE) and field
   positions.  */

bool
type_contains_placeholder_p (tree type)
{
  /* If the size contains a placeholder or the parent type (component type in
     the case of arrays) type involves a placeholder, this type does.  */
  if (CONTAINS_PLACEHOLDER_P (TYPE_SIZE (type))
      || CONTAINS_PLACEHOLDER_P (TYPE_SIZE_UNIT (type))
      || (TREE_TYPE (type) != 0
	  && type_contains_placeholder_p (TREE_TYPE (type))))
    return 1;

  /* Now do type-specific checks.  Note that the last part of the check above
     greatly limits what we have to do below.  */
  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
    case CHAR_TYPE:
    case POINTER_TYPE:
    case OFFSET_TYPE:
    case REFERENCE_TYPE:
    case METHOD_TYPE:
    case FILE_TYPE:
    case FUNCTION_TYPE:
      return 0;

    case INTEGER_TYPE:
    case REAL_TYPE:
      /* Here we just check the bounds.  */
      return (CONTAINS_PLACEHOLDER_P (TYPE_MIN_VALUE (type))
	      || CONTAINS_PLACEHOLDER_P (TYPE_MAX_VALUE (type)));

    case ARRAY_TYPE:
    case SET_TYPE:
      /* We're already checked the component type (TREE_TYPE), so just check
	 the index type.  */
      return type_contains_placeholder_p (TYPE_DOMAIN (type));

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	static tree seen_types = 0;
	tree field;
	bool ret = 0;

	/* We have to be careful here that we don't end up in infinite
	   recursions due to a field of a type being a pointer to that type
	   or to a mutually-recursive type.  So we store a list of record
	   types that we've seen and see if this type is in them.  To save
	   memory, we don't use a list for just one type.  Here we check
	   whether we've seen this type before and store it if not.  */
	if (seen_types == 0)
	  seen_types = type;
	else if (TREE_CODE (seen_types) != TREE_LIST)
	  {
	    if (seen_types == type)
	      return 0;

	    seen_types = tree_cons (NULL_TREE, type,
				    build_tree_list (NULL_TREE, seen_types));
	  }
	else
	  {
	    if (value_member (type, seen_types) != 0)
	      return 0;

	    seen_types = tree_cons (NULL_TREE, type, seen_types);
	  }

	for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	  if (TREE_CODE (field) == FIELD_DECL
	      && (CONTAINS_PLACEHOLDER_P (DECL_FIELD_OFFSET (field))
		  || (TREE_CODE (type) == QUAL_UNION_TYPE
		      && CONTAINS_PLACEHOLDER_P (DECL_QUALIFIER (field)))
		  || type_contains_placeholder_p (TREE_TYPE (field))))
	    {
	      ret = true;
	      break;
	    }

	/* Now remove us from seen_types and return the result.  */
	if (seen_types == type)
	  seen_types = 0;
	else
	  seen_types = TREE_CHAIN (seen_types);

	return ret;
      }

    default:
      abort ();
    }
}

/* Return 1 if EXP contains any expressions that produce cleanups for an
   outer scope to deal with.  Used by fold.  */

int
has_cleanups (tree exp)
{
  int i, nops, cmp;

  if (! TREE_SIDE_EFFECTS (exp))
    return 0;

  switch (TREE_CODE (exp))
    {
    case TARGET_EXPR:
    case GOTO_SUBROUTINE_EXPR:
    case WITH_CLEANUP_EXPR:
      return 1;

    case CLEANUP_POINT_EXPR:
      return 0;

    case CALL_EXPR:
      for (exp = TREE_OPERAND (exp, 1); exp; exp = TREE_CHAIN (exp))
	{
	  cmp = has_cleanups (TREE_VALUE (exp));
	  if (cmp)
	    return cmp;
	}
      return 0;

    default:
      break;
    }

  /* This general rule works for most tree codes.  All exceptions should be
     handled above.  If this is a language-specific tree code, we can't
     trust what might be in the operand, so say we don't know
     the situation.  */
  if ((int) TREE_CODE (exp) >= (int) LAST_AND_UNUSED_TREE_CODE)
    return -1;

  nops = first_rtl_op (TREE_CODE (exp));
  for (i = 0; i < nops; i++)
    if (TREE_OPERAND (exp, i) != 0)
      {
	int type = TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (exp, i)));
	if (type == 'e' || type == '<' || type == '1' || type == '2'
	    || type == 'r' || type == 's')
	  {
	    cmp = has_cleanups (TREE_OPERAND (exp, i));
	    if (cmp)
	      return cmp;
	  }
      }

  return 0;
}

/* Given a tree EXP, a FIELD_DECL F, and a replacement value R,
   return a tree with all occurrences of references to F in a
   PLACEHOLDER_EXPR replaced by R.   Note that we assume here that EXP
   contains only arithmetic expressions or a CALL_EXPR with a
   PLACEHOLDER_EXPR occurring only in its arglist.  */

tree
substitute_in_expr (tree exp, tree f, tree r)
{
  enum tree_code code = TREE_CODE (exp);
  tree op0, op1, op2;
  tree new;
  tree inner;

  switch (TREE_CODE_CLASS (code))
    {
    case 'c':
    case 'd':
      return exp;

    case 'x':
      if (code == PLACEHOLDER_EXPR)
	return exp;
      else if (code == TREE_LIST)
	{
	  op0 = (TREE_CHAIN (exp) == 0
		 ? 0 : substitute_in_expr (TREE_CHAIN (exp), f, r));
	  op1 = substitute_in_expr (TREE_VALUE (exp), f, r);
	  if (op0 == TREE_CHAIN (exp) && op1 == TREE_VALUE (exp))
	    return exp;

	  return tree_cons (TREE_PURPOSE (exp), op1, op0);
	}

      abort ();

    case '1':
    case '2':
    case '<':
    case 'e':
      switch (TREE_CODE_LENGTH (code))
	{
	case 1:
	  op0 = substitute_in_expr (TREE_OPERAND (exp, 0), f, r);
	  if (op0 == TREE_OPERAND (exp, 0))
	    return exp;

	  if (code == NON_LVALUE_EXPR)
	    return op0;

	  new = fold (build1 (code, TREE_TYPE (exp), op0));
	  break;

	case 2:
	  /* An RTL_EXPR cannot contain a PLACEHOLDER_EXPR; a CONSTRUCTOR
	     could, but we don't support it.  */
	  if (code == RTL_EXPR)
	    return exp;
	  else if (code == CONSTRUCTOR)
	    abort ();

	  op0 = TREE_OPERAND (exp, 0);
	  op1 = TREE_OPERAND (exp, 1);
	  if (CONTAINS_PLACEHOLDER_P (op0))
	    op0 = substitute_in_expr (op0, f, r);
	  if (CONTAINS_PLACEHOLDER_P (op1))
	    op1 = substitute_in_expr (op1, f, r);

	  if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1))
	    return exp;

	  new = fold (build (code, TREE_TYPE (exp), op0, op1));
	  break;

	case 3:
	  /* It cannot be that anything inside a SAVE_EXPR contains a
	     PLACEHOLDER_EXPR.  */
	  if (code == SAVE_EXPR)
	    return exp;

	  else if (code == CALL_EXPR)
	    {
	      op1 = substitute_in_expr (TREE_OPERAND (exp, 1), f, r);
	      if (op1 == TREE_OPERAND (exp, 1))
		return exp;

	      return build (code, TREE_TYPE (exp),
			    TREE_OPERAND (exp, 0), op1, NULL_TREE);
	    }

	  else if (code != COND_EXPR)
	    abort ();

	  op0 = TREE_OPERAND (exp, 0);
	  op1 = TREE_OPERAND (exp, 1);
	  op2 = TREE_OPERAND (exp, 2);

	  if (CONTAINS_PLACEHOLDER_P (op0))
	    op0 = substitute_in_expr (op0, f, r);
	  if (CONTAINS_PLACEHOLDER_P (op1))
	    op1 = substitute_in_expr (op1, f, r);
	  if (CONTAINS_PLACEHOLDER_P (op2))
	    op2 = substitute_in_expr (op2, f, r);

	  if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
	      && op2 == TREE_OPERAND (exp, 2))
	    return exp;

	  new = fold (build (code, TREE_TYPE (exp), op0, op1, op2));
	  break;

	default:
	  abort ();
	}

      break;

    case 'r':
      switch (code)
	{
	case COMPONENT_REF:
	  /* If this expression is getting a value from a PLACEHOLDER_EXPR
	     and it is the right field, replace it with R.  */
	  for (inner = TREE_OPERAND (exp, 0);
	       TREE_CODE_CLASS (TREE_CODE (inner)) == 'r';
	       inner = TREE_OPERAND (inner, 0))
	    ;
	  if (TREE_CODE (inner) == PLACEHOLDER_EXPR
	      && TREE_OPERAND (exp, 1) == f)
	    return r;

	  /* If this expression hasn't been completed let, leave it
	     alone.  */
	  if (TREE_CODE (inner) == PLACEHOLDER_EXPR
	      && TREE_TYPE (inner) == 0)
	    return exp;

	  op0 = substitute_in_expr (TREE_OPERAND (exp, 0), f, r);
	  if (op0 == TREE_OPERAND (exp, 0))
	    return exp;

	  new = fold (build (code, TREE_TYPE (exp), op0,
			     TREE_OPERAND (exp, 1)));
	  break;

	case BIT_FIELD_REF:
	  op0 = substitute_in_expr (TREE_OPERAND (exp, 0), f, r);
	  op1 = substitute_in_expr (TREE_OPERAND (exp, 1), f, r);
	  op2 = substitute_in_expr (TREE_OPERAND (exp, 2), f, r);
	  if (op0 == TREE_OPERAND (exp, 0) && op1 == TREE_OPERAND (exp, 1)
	      && op2 == TREE_OPERAND (exp, 2))
	    return exp;

	  new = fold (build (code, TREE_TYPE (exp), op0, op1, op2));
	  break;

	case INDIRECT_REF:
	case BUFFER_REF:
	  op0 = substitute_in_expr (TREE_OPERAND (exp, 0), f, r);
	  if (op0 == TREE_OPERAND (exp, 0))
	    return exp;

	  new = fold (build1 (code, TREE_TYPE (exp), op0));
	  break;

	default:
	  abort ();
	}
      break;

    default:
      abort ();
    }

  TREE_READONLY (new) = TREE_READONLY (exp);
  return new;
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

    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_CEIL_EXPR:
      result = build_nt (code, stabilize_reference (TREE_OPERAND (ref, 0)));
      break;

    case INDIRECT_REF:
      result = build_nt (INDIRECT_REF,
			 stabilize_reference_1 (TREE_OPERAND (ref, 0)));
      break;

    case COMPONENT_REF:
      result = build_nt (COMPONENT_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 TREE_OPERAND (ref, 1));
      break;

    case BIT_FIELD_REF:
      result = build_nt (BIT_FIELD_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 1)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 2)));
      break;

    case ARRAY_REF:
      result = build_nt (ARRAY_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 1)));
      break;

    case ARRAY_RANGE_REF:
      result = build_nt (ARRAY_RANGE_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 stabilize_reference_1 (TREE_OPERAND (ref, 1)));
      break;

    case COMPOUND_EXPR:
      /* We cannot wrap the first expression in a SAVE_EXPR, as then
	 it wouldn't be ignored.  This matters when dealing with
	 volatiles.  */
      return stabilize_reference_1 (ref);

    case RTL_EXPR:
      result = build1 (INDIRECT_REF, TREE_TYPE (ref),
		       save_expr (build1 (ADDR_EXPR,
					  build_pointer_type (TREE_TYPE (ref)),
					  ref)));
      break;

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

tree
stabilize_reference_1 (tree e)
{
  tree result;
  enum tree_code code = TREE_CODE (e);

  /* We cannot ignore const expressions because it might be a reference
     to a const array but whose index contains side-effects.  But we can
     ignore things that are actual constant or that already have been
     handled by this function.  */

  if (TREE_CONSTANT (e) || code == SAVE_EXPR)
    return e;

  switch (TREE_CODE_CLASS (code))
    {
    case 'x':
    case 't':
    case 'd':
    case 'b':
    case '<':
    case 's':
    case 'e':
    case 'r':
      /* If the expression has side-effects, then encase it in a SAVE_EXPR
	 so that it will only be evaluated once.  */
      /* The reference (r) and comparison (<) classes could be handled as
	 below, but it is generally faster to only evaluate them once.  */
      if (TREE_SIDE_EFFECTS (e))
	return save_expr (e);
      return e;

    case 'c':
      /* Constants need no processing.  In fact, we should never reach
	 here.  */
      return e;

    case '2':
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

    case '1':
      /* Recursively stabilize each operand.  */
      result = build_nt (code, stabilize_reference_1 (TREE_OPERAND (e, 0)));
      break;

    default:
      abort ();
    }

  TREE_TYPE (result) = TREE_TYPE (e);
  TREE_READONLY (result) = TREE_READONLY (e);
  TREE_SIDE_EFFECTS (result) = TREE_SIDE_EFFECTS (e);
  TREE_THIS_VOLATILE (result) = TREE_THIS_VOLATILE (e);

  return result;
}

/* Low-level constructors for expressions.  */

/* Build an expression of code CODE, data type TYPE,
   and operands as specified by the arguments ARG1 and following arguments.
   Expressions and reference nodes can be created this way.
   Constants, decls, types and misc nodes cannot be.  */

tree
build (enum tree_code code, tree tt, ...)
{
  tree t;
  int length;
  int i;
  int fro;
  int constant;
  va_list p;
  tree node;

  va_start (p, tt);

  t = make_node (code);
  length = TREE_CODE_LENGTH (code);
  TREE_TYPE (t) = tt;

  /* Below, we automatically set TREE_SIDE_EFFECTS and TREE_READONLY for the
     result based on those same flags for the arguments.  But if the
     arguments aren't really even `tree' expressions, we shouldn't be trying
     to do this.  */
  fro = first_rtl_op (code);

  /* Expressions without side effects may be constant if their
     arguments are as well.  */
  constant = (TREE_CODE_CLASS (code) == '<'
	      || TREE_CODE_CLASS (code) == '1'
	      || TREE_CODE_CLASS (code) == '2'
	      || TREE_CODE_CLASS (code) == 'c');

  if (length == 2)
    {
      /* This is equivalent to the loop below, but faster.  */
      tree arg0 = va_arg (p, tree);
      tree arg1 = va_arg (p, tree);

      TREE_OPERAND (t, 0) = arg0;
      TREE_OPERAND (t, 1) = arg1;
      TREE_READONLY (t) = 1;
      if (arg0 && fro > 0)
	{
	  if (TREE_SIDE_EFFECTS (arg0))
	    TREE_SIDE_EFFECTS (t) = 1;
	  if (!TREE_READONLY (arg0))
	    TREE_READONLY (t) = 0;
	  if (!TREE_CONSTANT (arg0))
	    constant = 0;
	}

      if (arg1 && fro > 1)
	{
	  if (TREE_SIDE_EFFECTS (arg1))
	    TREE_SIDE_EFFECTS (t) = 1;
	  if (!TREE_READONLY (arg1))
	    TREE_READONLY (t) = 0;
	  if (!TREE_CONSTANT (arg1))
	    constant = 0;
	}
    }
  else if (length == 1)
    {
      tree arg0 = va_arg (p, tree);

      /* The only one-operand cases we handle here are those with side-effects.
	 Others are handled with build1.  So don't bother checked if the
	 arg has side-effects since we'll already have set it.

	 ??? This really should use build1 too.  */
      if (TREE_CODE_CLASS (code) != 's')
	abort ();
      TREE_OPERAND (t, 0) = arg0;
    }
  else
    {
      for (i = 0; i < length; i++)
	{
	  tree operand = va_arg (p, tree);

	  TREE_OPERAND (t, i) = operand;
	  if (operand && fro > i)
	    {
	      if (TREE_SIDE_EFFECTS (operand))
		TREE_SIDE_EFFECTS (t) = 1;
	      if (!TREE_CONSTANT (operand))
		constant = 0;
	    }
	}
    }
  va_end (p);

  TREE_CONSTANT (t) = constant;
  
  if (code == CALL_EXPR && !TREE_SIDE_EFFECTS (t))
    {
      /* Calls have side-effects, except those to const or
	 pure functions.  */
      i = call_expr_flags (t);
      if (!(i & (ECF_CONST | ECF_PURE)))
	TREE_SIDE_EFFECTS (t) = 1;

      /* And even those have side-effects if their arguments do.  */
      else for (node = TREE_OPERAND (t, 1); node; node = TREE_CHAIN (node))
	if (TREE_SIDE_EFFECTS (TREE_VALUE (node)))
	  {
	    TREE_SIDE_EFFECTS (t) = 1;
	    break;
	  }
    }

  return t;
}

/* Same as above, but only builds for unary operators.
   Saves lions share of calls to `build'; cuts down use
   of varargs, which is expensive for RISC machines.  */

tree
build1 (enum tree_code code, tree type, tree node)
{
  int length = sizeof (struct tree_exp);
#ifdef GATHER_STATISTICS
  tree_node_kind kind;
#endif
  tree t;

#ifdef GATHER_STATISTICS
  switch (TREE_CODE_CLASS (code))
    {
    case 's':  /* an expression with side effects */
      kind = s_kind;
      break;
    case 'r':  /* a reference */
      kind = r_kind;
      break;
    default:
      kind = e_kind;
      break;
    }

  tree_node_counts[(int) kind]++;
  tree_node_sizes[(int) kind] += length;
#endif

#ifdef ENABLE_CHECKING
  if (TREE_CODE_CLASS (code) == '2'
      || TREE_CODE_CLASS (code) == '<'
      || TREE_CODE_LENGTH (code) != 1)
    abort ();
#endif /* ENABLE_CHECKING */

  t = ggc_alloc_tree (length);

  memset (t, 0, sizeof (struct tree_common));

  TREE_SET_CODE (t, code);

  TREE_TYPE (t) = type;
  TREE_COMPLEXITY (t) = 0;
  TREE_OPERAND (t, 0) = node;
  if (node && first_rtl_op (code) != 0)
    {
      TREE_SIDE_EFFECTS (t) = TREE_SIDE_EFFECTS (node);
      TREE_READONLY (t) = TREE_READONLY (node);
    }

  if (TREE_CODE_CLASS (code) == 's')
    TREE_SIDE_EFFECTS (t) = 1;
  else switch (code)
    {
    case INIT_EXPR:
    case MODIFY_EXPR:
    case VA_ARG_EXPR:
    case RTL_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
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
	{
	  /* The address of a volatile decl or reference does not have
	     side-effects.  But be careful not to ignore side-effects from
	     other sources deeper in the expression--if node is a _REF and
	     one of its operands has side-effects, so do we.  */
	  if (TREE_THIS_VOLATILE (node))
	    {
	      TREE_SIDE_EFFECTS (t) = 0;
	      if (!DECL_P (node))
		{
		  int i = first_rtl_op (TREE_CODE (node)) - 1;
		  for (; i >= 0; --i)
		    {
		      if (TREE_SIDE_EFFECTS (TREE_OPERAND (node, i)))
			TREE_SIDE_EFFECTS (t) = 1;
		    }
		}
	    }
	}
      break;

    default:
      if (TREE_CODE_CLASS (code) == '1' && node && TREE_CONSTANT (node))
	TREE_CONSTANT (t) = 1;
      break;
    }

  return t;
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

  va_start (p, code);

  t = make_node (code);
  length = TREE_CODE_LENGTH (code);

  for (i = 0; i < length; i++)
    TREE_OPERAND (t, i) = va_arg (p, tree);

  va_end (p);
  return t;
}

/* Create a DECL_... node of code CODE, name NAME and data type TYPE.
   We do NOT enter this node in any sort of symbol table.

   layout_decl is used to set up the decl's storage layout.
   Other slots are initialized to 0 or null pointers.  */

tree
build_decl (enum tree_code code, tree name, tree type)
{
  tree t;

  t = make_node (code);

/*  if (type == error_mark_node)
    type = integer_type_node; */
/* That is not done, deliberately, so that having error_mark_node
   as the type can suppress useless errors in the use of this variable.  */

  DECL_NAME (t) = name;
  TREE_TYPE (t) = type;

  if (code == VAR_DECL || code == PARM_DECL || code == RESULT_DECL)
    layout_decl (t, 0);
  else if (code == FUNCTION_DECL)
    DECL_MODE (t) = FUNCTION_MODE;

  return t;
}

/* BLOCK nodes are used to represent the structure of binding contours
   and declarations, once those contours have been exited and their contents
   compiled.  This information is used for outputting debugging info.  */

tree
build_block (tree vars, tree tags ATTRIBUTE_UNUSED, tree subblocks,
	     tree supercontext, tree chain)
{
  tree block = make_node (BLOCK);

  BLOCK_VARS (block) = vars;
  BLOCK_SUBBLOCKS (block) = subblocks;
  BLOCK_SUPERCONTEXT (block) = supercontext;
  BLOCK_CHAIN (block) = chain;
  return block;
}

/* EXPR_WITH_FILE_LOCATION are used to keep track of the exact
   location where an expression or an identifier were encountered. It
   is necessary for languages where the frontend parser will handle
   recursively more than one file (Java is one of them).  */

tree
build_expr_wfl (tree node, const char *file, int line, int col)
{
  static const char *last_file = 0;
  static tree last_filenode = NULL_TREE;
  tree wfl = make_node (EXPR_WITH_FILE_LOCATION);

  EXPR_WFL_NODE (wfl) = node;
  EXPR_WFL_SET_LINECOL (wfl, line, col);
  if (file != last_file)
    {
      last_file = file;
      last_filenode = file ? get_identifier (file) : NULL_TREE;
    }

  EXPR_WFL_FILENAME_NODE (wfl) = last_filenode;
  if (node)
    {
      TREE_SIDE_EFFECTS (wfl) = TREE_SIDE_EFFECTS (node);
      TREE_TYPE (wfl) = TREE_TYPE (node);
    }

  return wfl;
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
   is ATTRIBUTE.

   Record such modified types already made so we don't make duplicates.  */

tree
build_type_attribute_variant (tree ttype, tree attribute)
{
  if (! attribute_list_equal (TYPE_ATTRIBUTES (ttype), attribute))
    {
      unsigned int hashcode;
      tree ntype;

      ntype = copy_node (ttype);

      TYPE_POINTER_TO (ntype) = 0;
      TYPE_REFERENCE_TO (ntype) = 0;
      TYPE_ATTRIBUTES (ntype) = attribute;

      /* Create a new main variant of TYPE.  */
      TYPE_MAIN_VARIANT (ntype) = ntype;
      TYPE_NEXT_VARIANT (ntype) = 0;
      set_type_quals (ntype, TYPE_UNQUALIFIED);

      hashcode = (TYPE_HASH (TREE_CODE (ntype))
		  + TYPE_HASH (TREE_TYPE (ntype))
		  + attribute_hash_list (attribute));

      switch (TREE_CODE (ntype))
	{
	case FUNCTION_TYPE:
	  hashcode += TYPE_HASH (TYPE_ARG_TYPES (ntype));
	  break;
	case ARRAY_TYPE:
	  hashcode += TYPE_HASH (TYPE_DOMAIN (ntype));
	  break;
	case INTEGER_TYPE:
	  hashcode += TYPE_HASH (TYPE_MAX_VALUE (ntype));
	  break;
	case REAL_TYPE:
	  hashcode += TYPE_HASH (TYPE_PRECISION (ntype));
	  break;
	default:
	  break;
	}

      ntype = type_hash_canon (hashcode, ntype);
      ttype = build_qualified_type (ntype, TYPE_QUALS (ttype));
    }

  return ttype;
}

/* Return nonzero if IDENT is a valid name for attribute ATTR,
   or zero if not.

   We try both `text' and `__text__', ATTR may be either one.  */
/* ??? It might be a reasonable simplification to require ATTR to be only
   `text'.  One might then also require attribute lists to be stored in
   their canonicalized form.  */

int
is_attribute_p (const char *attr, tree ident)
{
  int ident_len, attr_len;
  const char *p;

  if (TREE_CODE (ident) != IDENTIFIER_NODE)
    return 0;

  if (strcmp (attr, IDENTIFIER_POINTER (ident)) == 0)
    return 1;

  p = IDENTIFIER_POINTER (ident);
  ident_len = strlen (p);
  attr_len = strlen (attr);

  /* If ATTR is `__text__', IDENT must be `text'; and vice versa.  */
  if (attr[0] == '_')
    {
      if (attr[1] != '_'
	  || attr[attr_len - 2] != '_'
	  || attr[attr_len - 1] != '_')
	abort ();
      if (ident_len == attr_len - 4
	  && strncmp (attr + 2, p, attr_len - 4) == 0)
	return 1;
    }
  else
    {
      if (ident_len == attr_len + 4
	  && p[0] == '_' && p[1] == '_'
	  && p[ident_len - 2] == '_' && p[ident_len - 1] == '_'
	  && strncmp (attr, p + 2, attr_len) == 0)
	return 1;
    }

  return 0;
}

/* Given an attribute name and a list of attributes, return a pointer to the
   attribute's list element if the attribute is part of the list, or NULL_TREE
   if not found.  If the attribute appears more than once, this only
   returns the first occurrence; the TREE_CHAIN of the return value should
   be passed back in if further occurrences are wanted.  */

tree
lookup_attribute (const char *attr_name, tree list)
{
  tree l;

  for (l = list; l; l = TREE_CHAIN (l))
    {
      if (TREE_CODE (TREE_PURPOSE (l)) != IDENTIFIER_NODE)
	abort ();
      if (is_attribute_p (attr_name, TREE_PURPOSE (l)))
	return l;
    }

  return NULL_TREE;
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
	      for (a = lookup_attribute (IDENTIFIER_POINTER (TREE_PURPOSE (a2)),
					 attributes);
		   a != NULL_TREE;
		   a = lookup_attribute (IDENTIFIER_POINTER (TREE_PURPOSE (a2)),
					 TREE_CHAIN (a)))
		{
		  if (simple_cst_equal (TREE_VALUE (a), TREE_VALUE (a2)) == 1)
		    break;
		}
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

#ifdef TARGET_DLLIMPORT_DECL_ATTRIBUTES

/* Specialization of merge_decl_attributes for various Windows targets.

   This handles the following situation:

     __declspec (dllimport) int foo;
     int foo;

   The second instance of `foo' nullifies the dllimport.  */

tree
merge_dllimport_decl_attributes (tree old, tree new)
{
  tree a;
  int delete_dllimport_p;

  old = DECL_ATTRIBUTES (old);
  new = DECL_ATTRIBUTES (new);

  /* What we need to do here is remove from `old' dllimport if it doesn't
     appear in `new'.  dllimport behaves like extern: if a declaration is
     marked dllimport and a definition appears later, then the object
     is not dllimport'd.  */
  if (lookup_attribute ("dllimport", old) != NULL_TREE
      && lookup_attribute ("dllimport", new) == NULL_TREE)
    delete_dllimport_p = 1;
  else
    delete_dllimport_p = 0;

  a = merge_attributes (old, new);

  if (delete_dllimport_p)
    {
      tree prev, t;

      /* Scan the list for dllimport and delete it.  */
      for (prev = NULL_TREE, t = a; t; prev = t, t = TREE_CHAIN (t))
	{
	  if (is_attribute_p ("dllimport", TREE_PURPOSE (t)))
	    {
	      if (prev == NULL_TREE)
		a = TREE_CHAIN (a);
	      else
		TREE_CHAIN (prev) = TREE_CHAIN (t);
	      break;
	    }
	}
    }

  return a;
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
}

/* Return a version of the TYPE, qualified as indicated by the
   TYPE_QUALS, if one exists.  If no qualified version exists yet,
   return NULL_TREE.  */

tree
get_qualified_type (tree type, int type_quals)
{
  tree t;

  /* Search the chain of variants to see if there is already one there just
     like the one we need to have.  If so, use that existing one.  We must
     preserve the TYPE_NAME, since there is code that depends on this.  */
  for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
    if (TYPE_QUALS (t) == type_quals && TYPE_NAME (t) == TYPE_NAME (type)
        && TYPE_CONTEXT (t) == TYPE_CONTEXT (type)
	&& attribute_list_equal (TYPE_ATTRIBUTES (t), TYPE_ATTRIBUTES (type)))
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
      t = build_type_copy (type);
      set_type_quals (t, type_quals);
    }

  return t;
}

/* Create a new variant of TYPE, equivalent but distinct.
   This is so the caller can modify it.  */

tree
build_type_copy (tree type)
{
  tree t, m = TYPE_MAIN_VARIANT (type);

  t = copy_node (type);

  TYPE_POINTER_TO (t) = 0;
  TYPE_REFERENCE_TO (t) = 0;

  /* Add this type to the chain of variants of TYPE.  */
  TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (m);
  TYPE_NEXT_VARIANT (m) = t;

  return t;
}

/* Hashing of types so that we don't make duplicates.
   The entry point is `type_hash_canon'.  */

/* Compute a hash code for a list of types (chain of TREE_LIST nodes
   with types in the TREE_VALUE slots), by adding the hash codes
   of the individual types.  */

unsigned int
type_hash_list (tree list)
{
  unsigned int hashcode;
  tree tail;

  for (hashcode = 0, tail = list; tail; tail = TREE_CHAIN (tail))
    hashcode += TYPE_HASH (TREE_VALUE (tail));

  return hashcode;
}

/* These are the Hashtable callback functions.  */

/* Returns true if the types are equal.  */

static int
type_hash_eq (const void *va, const void *vb)
{
  const struct type_hash *a = va, *b = vb;
  if (a->hash == b->hash
      && TREE_CODE (a->type) == TREE_CODE (b->type)
      && TREE_TYPE (a->type) == TREE_TYPE (b->type)
      && attribute_list_equal (TYPE_ATTRIBUTES (a->type),
			       TYPE_ATTRIBUTES (b->type))
      && TYPE_ALIGN (a->type) == TYPE_ALIGN (b->type)
      && (TYPE_MAX_VALUE (a->type) == TYPE_MAX_VALUE (b->type)
	  || tree_int_cst_equal (TYPE_MAX_VALUE (a->type),
				 TYPE_MAX_VALUE (b->type)))
      && (TYPE_MIN_VALUE (a->type) == TYPE_MIN_VALUE (b->type)
	  || tree_int_cst_equal (TYPE_MIN_VALUE (a->type),
				 TYPE_MIN_VALUE (b->type)))
      /* Note that TYPE_DOMAIN is TYPE_ARG_TYPES for FUNCTION_TYPE.  */
      && (TYPE_DOMAIN (a->type) == TYPE_DOMAIN (b->type)
	  || (TYPE_DOMAIN (a->type)
	      && TREE_CODE (TYPE_DOMAIN (a->type)) == TREE_LIST
	      && TYPE_DOMAIN (b->type)
	      && TREE_CODE (TYPE_DOMAIN (b->type)) == TREE_LIST
	      && type_list_equal (TYPE_DOMAIN (a->type),
				  TYPE_DOMAIN (b->type)))))
    return 1;
  return 0;
}

/* Return the cached hash value.  */

static hashval_t
type_hash_hash (const void *item)
{
  return ((const struct type_hash *) item)->hash;
}

/* Look in the type hash table for a type isomorphic to TYPE.
   If one is found, return it.  Otherwise return 0.  */

tree
type_hash_lookup (unsigned int hashcode, tree type)
{
  struct type_hash *h, in;

  /* The TYPE_ALIGN field of a type is set by layout_type(), so we
     must call that routine before comparing TYPE_ALIGNs.  */
  layout_type (type);

  in.hash = hashcode;
  in.type = type;

  h = htab_find_with_hash (type_hash_table, &in, hashcode);
  if (h)
    return h->type;
  return NULL_TREE;
}

/* Add an entry to the type-hash-table
   for a type TYPE whose hash code is HASHCODE.  */

void
type_hash_add (unsigned int hashcode, tree type)
{
  struct type_hash *h;
  void **loc;

  h = ggc_alloc (sizeof (struct type_hash));
  h->hash = hashcode;
  h->type = type;
  loc = htab_find_slot_with_hash (type_hash_table, h, hashcode, INSERT);
  *(struct type_hash **) loc = h;
}

/* Given TYPE, and HASHCODE its hash code, return the canonical
   object for an identical type if one already exists.
   Otherwise, return TYPE, and record it as the canonical object
   if it is a permanent object.

   To use this function, first create a type of the sort you want.
   Then compute its hash code from the fields of the type that
   make it different from other similar types.
   Then call this function and use the value.
   This function frees the type you pass in if it is a duplicate.  */

/* Set to 1 to debug without canonicalization.  Never set by program.  */
int debug_no_type_hash = 0;

tree
type_hash_canon (unsigned int hashcode, tree type)
{
  tree t1;

  if (debug_no_type_hash)
    return type;

  /* See if the type is in the hash table already.  If so, return it.
     Otherwise, add the type.  */
  t1 = type_hash_lookup (hashcode, type);
  if (t1 != 0)
    {
#ifdef GATHER_STATISTICS
      tree_node_counts[(int) t_kind]--;
      tree_node_sizes[(int) t_kind] -= sizeof (struct tree_type);
#endif
      return t1;
    }
  else
    {
      type_hash_add (hashcode, type);
      return type;
    }
}

/* See if the data pointed to by the type hash table is marked.  We consider
   it marked if the type is marked or if a debug type number or symbol
   table entry has been made for the type.  This reduces the amount of
   debugging output and eliminates that dependency of the debug output on
   the number of garbage collections.  */

static int
type_hash_marked_p (const void *p)
{
  tree type = ((struct type_hash *) p)->type;

  return ggc_marked_p (type) || TYPE_SYMTAB_POINTER (type);
}

static void
print_type_hash_statistics (void)
{
  fprintf (stderr, "Type hash: size %ld, %ld elements, %f collisions\n",
	   (long) htab_size (type_hash_table),
	   (long) htab_elements (type_hash_table),
	   htab_collisions (type_hash_table));
}

/* Compute a hash code for a list of attributes (chain of TREE_LIST nodes
   with names in the TREE_PURPOSE slots and args in the TREE_VALUE slots),
   by adding the hash codes of the individual attributes.  */

unsigned int
attribute_hash_list (tree list)
{
  unsigned int hashcode;
  tree tail;

  for (hashcode = 0, tail = list; tail; tail = TREE_CHAIN (tail))
    /* ??? Do we want to add in TREE_VALUE too? */
    hashcode += TYPE_HASH (TREE_PURPOSE (tail));
  return hashcode;
}

/* Given two lists of attributes, return true if list l2 is
   equivalent to l1.  */

int
attribute_list_equal (tree l1, tree l2)
{
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
attribute_list_contained (tree l1, tree l2)
{
  tree t1, t2;

  /* First check the obvious, maybe the lists are identical.  */
  if (l1 == l2)
    return 1;

  /* Maybe the lists are similar.  */
  for (t1 = l1, t2 = l2;
       t1 != 0 && t2 != 0
        && TREE_PURPOSE (t1) == TREE_PURPOSE (t2)
        && TREE_VALUE (t1) == TREE_VALUE (t2);
       t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2));

  /* Maybe the lists are equal.  */
  if (t1 == 0 && t2 == 0)
    return 1;

  for (; t2 != 0; t2 = TREE_CHAIN (t2))
    {
      tree attr;
      for (attr = lookup_attribute (IDENTIFIER_POINTER (TREE_PURPOSE (t2)), l1);
	   attr != NULL_TREE;
	   attr = lookup_attribute (IDENTIFIER_POINTER (TREE_PURPOSE (t2)),
				    TREE_CHAIN (attr)))
	{
	  if (simple_cst_equal (TREE_VALUE (t2), TREE_VALUE (attr)) == 1)
	    break;
	}

      if (attr == 0)
	return 0;

      if (simple_cst_equal (TREE_VALUE (t2), TREE_VALUE (attr)) != 1)
	return 0;
    }

  return 1;
}

/* Given two lists of types
   (chains of TREE_LIST nodes with types in the TREE_VALUE slots)
   return 1 if the lists contain the same types in the same order.
   Also, the TREE_PURPOSEs must match.  */

int
type_list_equal (tree l1, tree l2)
{
  tree t1, t2;

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
type_num_arguments (tree type)
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
tree_int_cst_equal (tree t1, tree t2)
{
  if (t1 == t2)
    return 1;

  if (t1 == 0 || t2 == 0)
    return 0;

  if (TREE_CODE (t1) == INTEGER_CST
      && TREE_CODE (t2) == INTEGER_CST
      && TREE_INT_CST_LOW (t1) == TREE_INT_CST_LOW (t2)
      && TREE_INT_CST_HIGH (t1) == TREE_INT_CST_HIGH (t2))
    return 1;

  return 0;
}

/* Nonzero if integer constants T1 and T2 represent values that satisfy <.
   The precise way of comparison depends on their data type.  */

int
tree_int_cst_lt (tree t1, tree t2)
{
  if (t1 == t2)
    return 0;

  if (TREE_UNSIGNED (TREE_TYPE (t1)) != TREE_UNSIGNED (TREE_TYPE (t2)))
    {
      int t1_sgn = tree_int_cst_sgn (t1);
      int t2_sgn = tree_int_cst_sgn (t2);

      if (t1_sgn < t2_sgn)
	return 1;
      else if (t1_sgn > t2_sgn)
	return 0;
      /* Otherwise, both are non-negative, so we compare them as
	 unsigned just in case one of them would overflow a signed
	 type.  */
    }
  else if (! TREE_UNSIGNED (TREE_TYPE (t1)))
    return INT_CST_LT (t1, t2);

  return INT_CST_LT_UNSIGNED (t1, t2);
}

/* Returns -1 if T1 < T2, 0 if T1 == T2, and 1 if T1 > T2.  */

int
tree_int_cst_compare (tree t1, tree t2)
{
  if (tree_int_cst_lt (t1, t2))
    return -1;
  else if (tree_int_cst_lt (t2, t1))
    return 1;
  else
    return 0;
}

/* Return 1 if T is an INTEGER_CST that can be manipulated efficiently on
   the host.  If POS is zero, the value can be represented in a single
   HOST_WIDE_INT.  If POS is nonzero, the value must be positive and can
   be represented in a single unsigned HOST_WIDE_INT.  */

int
host_integerp (tree t, int pos)
{
  return (TREE_CODE (t) == INTEGER_CST
	  && ! TREE_OVERFLOW (t)
	  && ((TREE_INT_CST_HIGH (t) == 0
	       && (HOST_WIDE_INT) TREE_INT_CST_LOW (t) >= 0)
	      || (! pos && TREE_INT_CST_HIGH (t) == -1
		  && (HOST_WIDE_INT) TREE_INT_CST_LOW (t) < 0
		  && ! TREE_UNSIGNED (TREE_TYPE (t)))
	      || (pos && TREE_INT_CST_HIGH (t) == 0)));
}

/* Return the HOST_WIDE_INT least significant bits of T if it is an
   INTEGER_CST and there is no overflow.  POS is nonzero if the result must
   be positive.  Abort if we cannot satisfy the above conditions.  */

HOST_WIDE_INT
tree_low_cst (tree t, int pos)
{
  if (host_integerp (t, pos))
    return TREE_INT_CST_LOW (t);
  else
    abort ();
}

/* Return the most significant bit of the integer constant T.  */

int
tree_int_cst_msb (tree t)
{
  int prec;
  HOST_WIDE_INT h;
  unsigned HOST_WIDE_INT l;

  /* Note that using TYPE_PRECISION here is wrong.  We care about the
     actual bits, not the (arbitrary) range of the type.  */
  prec = GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (t))) - 1;
  rshift_double (TREE_INT_CST_LOW (t), TREE_INT_CST_HIGH (t), prec,
		 2 * HOST_BITS_PER_WIDE_INT, &l, &h, 0);
  return (l & 1) == 1;
}

/* Return an indication of the sign of the integer constant T.
   The return value is -1 if T < 0, 0 if T == 0, and 1 if T > 0.
   Note that -1 will never be returned it T's type is unsigned.  */

int
tree_int_cst_sgn (tree t)
{
  if (TREE_INT_CST_LOW (t) == 0 && TREE_INT_CST_HIGH (t) == 0)
    return 0;
  else if (TREE_UNSIGNED (TREE_TYPE (t)))
    return 1;
  else if (TREE_INT_CST_HIGH (t) < 0)
    return -1;
  else
    return 1;
}

/* Compare two constructor-element-type constants.  Return 1 if the lists
   are known to be equal; otherwise return 0.  */

int
simple_cst_list_equal (tree l1, tree l2)
{
  while (l1 != NULL_TREE && l2 != NULL_TREE)
    {
      if (simple_cst_equal (TREE_VALUE (l1), TREE_VALUE (l2)) != 1)
	return 0;

      l1 = TREE_CHAIN (l1);
      l2 = TREE_CHAIN (l2);
    }

  return l1 == l2;
}

/* Return truthvalue of whether T1 is the same tree structure as T2.
   Return 1 if they are the same.
   Return 0 if they are understandably different.
   Return -1 if either contains tree structure not understood by
   this function.  */

int
simple_cst_equal (tree t1, tree t2)
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

  if (code1 == NOP_EXPR || code1 == CONVERT_EXPR || code1 == NON_LVALUE_EXPR)
    {
      if (code2 == NOP_EXPR || code2 == CONVERT_EXPR
	  || code2 == NON_LVALUE_EXPR)
	return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      else
	return simple_cst_equal (TREE_OPERAND (t1, 0), t2);
    }

  else if (code2 == NOP_EXPR || code2 == CONVERT_EXPR
	   || code2 == NON_LVALUE_EXPR)
    return simple_cst_equal (t1, TREE_OPERAND (t2, 0));

  if (code1 != code2)
    return 0;

  switch (code1)
    {
    case INTEGER_CST:
      return (TREE_INT_CST_LOW (t1) == TREE_INT_CST_LOW (t2)
	      && TREE_INT_CST_HIGH (t1) == TREE_INT_CST_HIGH (t2));

    case REAL_CST:
      return REAL_VALUES_IDENTICAL (TREE_REAL_CST (t1), TREE_REAL_CST (t2));

    case STRING_CST:
      return (TREE_STRING_LENGTH (t1) == TREE_STRING_LENGTH (t2)
	      && ! memcmp (TREE_STRING_POINTER (t1), TREE_STRING_POINTER (t2),
			 TREE_STRING_LENGTH (t1)));

    case CONSTRUCTOR:
      if (CONSTRUCTOR_ELTS (t1) == CONSTRUCTOR_ELTS (t2))
	return 1;
      else
	abort ();

    case SAVE_EXPR:
      return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

    case CALL_EXPR:
      cmp = simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      if (cmp <= 0)
	return cmp;
      return
	simple_cst_list_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));

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
    case '1':
    case '2':
    case '<':
    case 'e':
    case 'r':
    case 's':
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
compare_tree_int (tree t, unsigned HOST_WIDE_INT u)
{
  if (tree_int_cst_sgn (t) < 0)
    return -1;
  else if (TREE_INT_CST_HIGH (t) != 0)
    return 1;
  else if (TREE_INT_CST_LOW (t) == u)
    return 0;
  else if (TREE_INT_CST_LOW (t) < u)
    return -1;
  else
    return 1;
}

/* Generate a hash value for an expression.  This can be used iteratively
   by passing a previous result as the "val" argument.

   This function is intended to produce the same hash for expressions which
   would compare equal using operand_equal_p.  */

hashval_t
iterative_hash_expr (tree t, hashval_t val)
{
  int i;
  enum tree_code code;
  char class;

  if (t == NULL_TREE)
    return iterative_hash_object (t, val);

  code = TREE_CODE (t);
  class = TREE_CODE_CLASS (code);

  if (class == 'd')
    {
      /* Decls we can just compare by pointer.  */
      val = iterative_hash_object (t, val);
    }
  else if (class == 'c')
    {
      /* Alas, constants aren't shared, so we can't rely on pointer
	 identity.  */
      if (code == INTEGER_CST)
	{
	  val = iterative_hash_object (TREE_INT_CST_LOW (t), val);
	  val = iterative_hash_object (TREE_INT_CST_HIGH (t), val);
	}
      else if (code == REAL_CST)
	val = iterative_hash (TREE_REAL_CST_PTR (t),
			      sizeof (REAL_VALUE_TYPE), val);
      else if (code == STRING_CST)
	val = iterative_hash (TREE_STRING_POINTER (t),
			      TREE_STRING_LENGTH (t), val);
      else if (code == COMPLEX_CST)
	{
	  val = iterative_hash_expr (TREE_REALPART (t), val);
	  val = iterative_hash_expr (TREE_IMAGPART (t), val);
	}
      else if (code == VECTOR_CST)
	val = iterative_hash_expr (TREE_VECTOR_CST_ELTS (t), val);
      else
	abort ();
    }
  else if (IS_EXPR_CODE_CLASS (class))
    {
      val = iterative_hash_object (code, val);

      if (code == NOP_EXPR || code == CONVERT_EXPR
	  || code == NON_LVALUE_EXPR)
	val = iterative_hash_object (TREE_TYPE (t), val);

      if (code == PLUS_EXPR || code == MULT_EXPR || code == MIN_EXPR
	  || code == MAX_EXPR || code == BIT_IOR_EXPR || code == BIT_XOR_EXPR
	  || code == BIT_AND_EXPR || code == NE_EXPR || code == EQ_EXPR)
	{
	  /* It's a commutative expression.  We want to hash it the same
	     however it appears.  We do this by first hashing both operands
	     and then rehashing based on the order of their independent
	     hashes.  */
	  hashval_t one = iterative_hash_expr (TREE_OPERAND (t, 0), 0);
	  hashval_t two = iterative_hash_expr (TREE_OPERAND (t, 1), 0);
	  hashval_t t;

	  if (one > two)
	    t = one, one = two, two = t;

	  val = iterative_hash_object (one, val);
	  val = iterative_hash_object (two, val);
	}
      else
	for (i = first_rtl_op (code) - 1; i >= 0; --i)
	  val = iterative_hash_expr (TREE_OPERAND (t, i), val);
    }
  else if (code == TREE_LIST)
    {
      /* A list of expressions, for a CALL_EXPR or as the elements of a
	 VECTOR_CST.  */
      for (; t; t = TREE_CHAIN (t))
	val = iterative_hash_expr (TREE_VALUE (t), val);
    }
  else
    abort ();

  return val;
}

/* Constructors for pointer, array and function types.
   (RECORD_TYPE, UNION_TYPE and ENUMERAL_TYPE nodes are
   constructed by language-dependent code, not here.)  */

/* Construct, lay out and return the type of pointers to TO_TYPE
   with mode MODE. If such a type has already been constructed,
   reuse it.  */

tree
build_pointer_type_for_mode (tree to_type, enum machine_mode mode)
{
  tree t = TYPE_POINTER_TO (to_type);

  /* First, if we already have a type for pointers to TO_TYPE, use it.  */
  if (t != 0 && mode == ptr_mode)
    return t;

  t = make_node (POINTER_TYPE);

  TREE_TYPE (t) = to_type;
  TYPE_MODE (t) = mode;

  /* Record this type as the pointer to TO_TYPE.  */
  if (mode == ptr_mode)
  TYPE_POINTER_TO (to_type) = t;

  /* Lay out the type.  This function has many callers that are concerned
     with expression-construction, and this simplifies them all.
     Also, it guarantees the TYPE_SIZE is in the same obstack as the type.  */
  layout_type (t);

  return t;
}

/* By default build pointers in ptr_mode.  */

tree
build_pointer_type (tree to_type)
{
  return build_pointer_type_for_mode (to_type, ptr_mode);
}

/* Construct, lay out and return the type of references to TO_TYPE
   with mode MODE. If such a type has already been constructed,
   reuse it.  */

tree
build_reference_type_for_mode (tree to_type, enum machine_mode mode)
{
  tree t = TYPE_REFERENCE_TO (to_type);

  /* First, if we already have a type for pointers to TO_TYPE, use it.  */
  if (t != 0 && mode == ptr_mode)
    return t;

  t = make_node (REFERENCE_TYPE);

  TREE_TYPE (t) = to_type;
  TYPE_MODE (t) = mode;

  /* Record this type as the pointer to TO_TYPE.  */
  if (mode == ptr_mode)
  TYPE_REFERENCE_TO (to_type) = t;

  layout_type (t);

  return t;
}


/* Build the node for the type of references-to-TO_TYPE by default
   in ptr_mode.  */

tree
build_reference_type (tree to_type)
{
  return build_reference_type_for_mode (to_type, ptr_mode);
}

/* Build a type that is compatible with t but has no cv quals anywhere
   in its type, thus

   const char *const *const *  ->  char ***.  */

tree
build_type_no_quals (tree t)
{
  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
      return build_pointer_type (build_type_no_quals (TREE_TYPE (t)));
    case REFERENCE_TYPE:
      return build_reference_type (build_type_no_quals (TREE_TYPE (t)));
    default:
      return TYPE_MAIN_VARIANT (t);
    }
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
  tree itype = make_node (INTEGER_TYPE);

  TREE_TYPE (itype) = sizetype;
  TYPE_PRECISION (itype) = TYPE_PRECISION (sizetype);
  TYPE_MIN_VALUE (itype) = size_zero_node;
  TYPE_MAX_VALUE (itype) = convert (sizetype, maxval);
  TYPE_MODE (itype) = TYPE_MODE (sizetype);
  TYPE_SIZE (itype) = TYPE_SIZE (sizetype);
  TYPE_SIZE_UNIT (itype) = TYPE_SIZE_UNIT (sizetype);
  TYPE_ALIGN (itype) = TYPE_ALIGN (sizetype);
  TYPE_USER_ALIGN (itype) = TYPE_USER_ALIGN (sizetype);

  if (host_integerp (maxval, 1))
    return type_hash_canon (tree_low_cst (maxval, 1), itype);
  else
    return itype;
}

/* Create a range of some discrete type TYPE (an INTEGER_TYPE,
   ENUMERAL_TYPE, BOOLEAN_TYPE, or CHAR_TYPE), with
   low bound LOWVAL and high bound HIGHVAL.
   if TYPE==NULL_TREE, sizetype is used.  */

tree
build_range_type (tree type, tree lowval, tree highval)
{
  tree itype = make_node (INTEGER_TYPE);

  TREE_TYPE (itype) = type;
  if (type == NULL_TREE)
    type = sizetype;

  TYPE_MIN_VALUE (itype) = convert (type, lowval);
  TYPE_MAX_VALUE (itype) = highval ? convert (type, highval) : NULL;

  TYPE_PRECISION (itype) = TYPE_PRECISION (type);
  TYPE_MODE (itype) = TYPE_MODE (type);
  TYPE_SIZE (itype) = TYPE_SIZE (type);
  TYPE_SIZE_UNIT (itype) = TYPE_SIZE_UNIT (type);
  TYPE_ALIGN (itype) = TYPE_ALIGN (type);
  TYPE_USER_ALIGN (itype) = TYPE_USER_ALIGN (type);

  if (host_integerp (lowval, 0) && highval != 0 && host_integerp (highval, 0))
    return type_hash_canon (tree_low_cst (highval, 0)
			    - tree_low_cst (lowval, 0),
			    itype);
  else
    return itype;
}

/* Just like build_index_type, but takes lowval and highval instead
   of just highval (maxval).  */

tree
build_index_2_type (tree lowval, tree highval)
{
  return build_range_type (sizetype, lowval, highval);
}

/* Construct, lay out and return the type of arrays of elements with ELT_TYPE
   and number of elements specified by the range of values of INDEX_TYPE.
   If such a type has already been constructed, reuse it.  */

tree
build_array_type (tree elt_type, tree index_type)
{
  tree t;
  unsigned int hashcode;

  if (TREE_CODE (elt_type) == FUNCTION_TYPE)
    {
      error ("arrays of functions are not meaningful");
      elt_type = integer_type_node;
    }

  /* Make sure TYPE_POINTER_TO (elt_type) is filled in.  */
  build_pointer_type (elt_type);

  /* Allocate the array after the pointer type,
     in case we free it in type_hash_canon.  */
  t = make_node (ARRAY_TYPE);
  TREE_TYPE (t) = elt_type;
  TYPE_DOMAIN (t) = index_type;

  if (index_type == 0)
    {
      return t;
    }

  hashcode = TYPE_HASH (elt_type) + TYPE_HASH (index_type);
  t = type_hash_canon (hashcode, t);

  if (!COMPLETE_TYPE_P (t))
    layout_type (t);
  return t;
}

/* Return the TYPE of the elements comprising
   the innermost dimension of ARRAY.  */

tree
get_inner_array_type (tree array)
{
  tree type = TREE_TYPE (array);

  while (TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);

  return type;
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
  unsigned int hashcode;

  if (TREE_CODE (value_type) == FUNCTION_TYPE)
    {
      error ("function return type cannot be function");
      value_type = integer_type_node;
    }

  /* Make a node of the sort we want.  */
  t = make_node (FUNCTION_TYPE);
  TREE_TYPE (t) = value_type;
  TYPE_ARG_TYPES (t) = arg_types;

  /* If we already have such a type, use the old one and free this one.  */
  hashcode = TYPE_HASH (value_type) + type_hash_list (arg_types);
  t = type_hash_canon (hashcode, t);

  if (!COMPLETE_TYPE_P (t))
    layout_type (t);
  return t;
}

/* Build a function type.  The RETURN_TYPE is the type returned by the
   function.  If additional arguments are provided, they are
   additional argument types.  The list of argument types must always
   be terminated by NULL_TREE.  */

tree
build_function_type_list (tree return_type, ...)
{
  tree t, args, last;
  va_list p;

  va_start (p, return_type);

  t = va_arg (p, tree);
  for (args = NULL_TREE; t != NULL_TREE; t = va_arg (p, tree))
    args = tree_cons (NULL_TREE, t, args);

  last = args;
  args = nreverse (args);
  TREE_CHAIN (last) = void_list_node;
  args = build_function_type (return_type, args);

  va_end (p);
  return args;
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
  int hashcode;

  /* Make a node of the sort we want.  */
  t = make_node (METHOD_TYPE);

  TYPE_METHOD_BASETYPE (t) = TYPE_MAIN_VARIANT (basetype);
  TREE_TYPE (t) = rettype;
  ptype = build_pointer_type (basetype);

  /* The actual arglist for this function includes a "hidden" argument
     which is "this".  Put it into the list of argument types.  */
  argtypes = tree_cons (NULL_TREE, ptype, argtypes);
  TYPE_ARG_TYPES (t) = argtypes;

  /* If we already have such a type, use the old one and free this one.
     Note that it also frees up the above cons cell if found.  */
  hashcode = TYPE_HASH (basetype) + TYPE_HASH (rettype) +
    type_hash_list (argtypes);

  t = type_hash_canon (hashcode, t);

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
  if (TREE_CODE (type) != FUNCTION_TYPE)
    abort ();

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
  unsigned int hashcode;

  /* Make a node of the sort we want.  */
  t = make_node (OFFSET_TYPE);

  TYPE_OFFSET_BASETYPE (t) = TYPE_MAIN_VARIANT (basetype);
  TREE_TYPE (t) = type;

  /* If we already have such a type, use the old one and free this one.  */
  hashcode = TYPE_HASH (basetype) + TYPE_HASH (type);
  t = type_hash_canon (hashcode, t);

  if (!COMPLETE_TYPE_P (t))
    layout_type (t);

  return t;
}

/* Create a complex type whose components are COMPONENT_TYPE.  */

tree
build_complex_type (tree component_type)
{
  tree t;
  unsigned int hashcode;

  /* Make a node of the sort we want.  */
  t = make_node (COMPLEX_TYPE);

  TREE_TYPE (t) = TYPE_MAIN_VARIANT (component_type);
  set_type_quals (t, TYPE_QUALS (component_type));

  /* If we already have such a type, use the old one and free this one.  */
  hashcode = TYPE_HASH (component_type);
  t = type_hash_canon (hashcode, t);

  if (!COMPLETE_TYPE_P (t))
    layout_type (t);

  /* If we are writing Dwarf2 output we need to create a name,
     since complex is a fundamental type.  */
  if ((write_symbols == DWARF2_DEBUG || write_symbols == VMS_AND_DWARF2_DEBUG)
      && ! TYPE_NAME (t))
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
	TYPE_NAME (t) = get_identifier (name);
    }

  return t;
}

/* Return OP, stripped of any conversions to wider types as much as is safe.
   Converting the value back to OP's type makes a value equivalent to OP.

   If FOR_TYPE is nonzero, we return a value which, if converted to
   type FOR_TYPE, would be equivalent to converting OP to type FOR_TYPE.

   If FOR_TYPE is nonzero, unaligned bit-field references may be changed to the
   narrowest type that can hold the value, even if they don't exactly fit.
   Otherwise, bit-field references are changed to a narrower type
   only if they can be fetched directly from memory in that type.

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
       && TREE_UNSIGNED (type));
  tree win = op;

  while (TREE_CODE (op) == NOP_EXPR)
    {
      int bitschange
	= TYPE_PRECISION (TREE_TYPE (op))
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
	  /* TREE_UNSIGNED says whether this is a zero-extension.
	     Let's avoid computing it if it does not affect WIN
	     and if UNS will not be needed again.  */
	  if ((uns || TREE_CODE (op) == NOP_EXPR)
	      && TREE_UNSIGNED (TREE_TYPE (op)))
	    {
	      uns = 1;
	      win = op;
	    }
	}
    }

  if (TREE_CODE (op) == COMPONENT_REF
      /* Since type_for_size always gives an integer type.  */
      && TREE_CODE (type) != REAL_TYPE
      /* Don't crash if field not laid out yet.  */
      && DECL_SIZE (TREE_OPERAND (op, 1)) != 0
      && host_integerp (DECL_SIZE (TREE_OPERAND (op, 1)), 1))
    {
      unsigned int innerprec
	= tree_low_cst (DECL_SIZE (TREE_OPERAND (op, 1)), 1);
      int unsignedp = (TREE_UNSIGNED (TREE_OPERAND (op, 1))
		       || TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op, 1))));
      type = (*lang_hooks.types.type_for_size) (innerprec, unsignedp);

      /* We can get this structure field in the narrowest type it fits in.
	 If FOR_TYPE is 0, do this only for a field that matches the
	 narrower type exactly and is aligned for it
	 The resulting extension to its nominal type (a fullword type)
	 must fit the same conditions as for other extensions.  */

      if (type != 0
	  && INT_CST_LT_UNSIGNED (TYPE_SIZE (type), TYPE_SIZE (TREE_TYPE (op)))
	  && (for_type || ! DECL_BIT_FIELD (TREE_OPERAND (op, 1)))
	  && (! uns || final_prec <= innerprec || unsignedp))
	{
	  win = build (COMPONENT_REF, type, TREE_OPERAND (op, 0),
		       TREE_OPERAND (op, 1));
	  TREE_SIDE_EFFECTS (win) = TREE_SIDE_EFFECTS (op);
	  TREE_THIS_VOLATILE (win) = TREE_THIS_VOLATILE (op);
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
	    uns = TREE_UNSIGNED (TREE_TYPE (op));
	  /* Otherwise, if a sign extension has been stripped,
	     only sign extensions can now be stripped;
	     if a zero extension has been stripped, only zero-extensions.  */
	  else if (uns != TREE_UNSIGNED (TREE_TYPE (op)))
	    break;
	  first = 0;
	}
      else /* bitschange == 0 */
	{
	  /* A change in nominal type can always be stripped, but we must
	     preserve the unsignedness.  */
	  if (first)
	    uns = TREE_UNSIGNED (TREE_TYPE (op));
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
      /* Ensure field is laid out already.  */
      && DECL_SIZE (TREE_OPERAND (op, 1)) != 0)
    {
      unsigned HOST_WIDE_INT innerprec
	= tree_low_cst (DECL_SIZE (TREE_OPERAND (op, 1)), 1);
      int unsignedp = (TREE_UNSIGNED (TREE_OPERAND (op, 1))
		       || TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op, 1))));
      tree type = (*lang_hooks.types.type_for_size) (innerprec, unsignedp);

      /* We can get this structure field in a narrower type that fits it,
	 but the resulting extension to its nominal type (a fullword type)
	 must satisfy the same conditions as for other extensions.

	 Do this only for fields that are aligned (not bit-fields),
	 because when bit-field insns will be used there is no
	 advantage in doing this.  */

      if (innerprec < TYPE_PRECISION (TREE_TYPE (op))
	  && ! DECL_BIT_FIELD (TREE_OPERAND (op, 1))
	  && (first || uns == TREE_UNSIGNED (TREE_OPERAND (op, 1)))
	  && type != 0)
	{
	  if (first)
	    uns = TREE_UNSIGNED (TREE_OPERAND (op, 1));
	  win = build (COMPONENT_REF, type, TREE_OPERAND (op, 0),
		       TREE_OPERAND (op, 1));
	  TREE_SIDE_EFFECTS (win) = TREE_SIDE_EFFECTS (op);
	  TREE_THIS_VOLATILE (win) = TREE_THIS_VOLATILE (op);
	}
    }
  *unsignedp_ptr = uns;
  return win;
}

/* Nonzero if integer constant C has a value that is permissible
   for type TYPE (an INTEGER_TYPE).  */

int
int_fits_type_p (tree c, tree type)
{
  tree type_low_bound = TYPE_MIN_VALUE (type);
  tree type_high_bound = TYPE_MAX_VALUE (type);
  int ok_for_low_bound, ok_for_high_bound;

  /* Perform some generic filtering first, which may allow making a decision
     even if the bounds are not constant.  First, negative integers never fit
     in unsigned types, */
  if ((TREE_UNSIGNED (type) && tree_int_cst_sgn (c) < 0)
      /* Also, unsigned integers with top bit set never fit signed types.  */
      || (! TREE_UNSIGNED (type)
	  && TREE_UNSIGNED (TREE_TYPE (c)) && tree_int_cst_msb (c)))
    return 0;

  /* If at least one bound of the type is a constant integer, we can check
     ourselves and maybe make a decision. If no such decision is possible, but
     this type is a subtype, try checking against that.  Otherwise, use
     force_fit_type, which checks against the precision.

     Compute the status for each possibly constant bound, and return if we see
     one does not match. Use ok_for_xxx_bound for this purpose, assigning -1
     for "unknown if constant fits", 0 for "constant known *not* to fit" and 1
     for "constant known to fit".  */

  ok_for_low_bound = -1;
  ok_for_high_bound = -1;

  /* Check if C >= type_low_bound.  */
  if (type_low_bound && TREE_CODE (type_low_bound) == INTEGER_CST)
    {
      ok_for_low_bound = ! tree_int_cst_lt (c, type_low_bound);
      if (! ok_for_low_bound)
	return 0;
    }

  /* Check if c <= type_high_bound.  */
  if (type_high_bound && TREE_CODE (type_high_bound) == INTEGER_CST)
    {
      ok_for_high_bound = ! tree_int_cst_lt (type_high_bound, c);
      if (! ok_for_high_bound)
	return 0;
    }

  /* If the constant fits both bounds, the result is known.  */
  if (ok_for_low_bound == 1 && ok_for_high_bound == 1)
    return 1;

  /* If we haven't been able to decide at this point, there nothing more we
     can check ourselves here. Look at the base type if we have one.  */
  else if (TREE_CODE (type) == INTEGER_TYPE && TREE_TYPE (type) != 0)
    return int_fits_type_p (c, TREE_TYPE (type));

  /* Or to force_fit_type, if nothing else.  */
  else
    {
      c = copy_node (c);
      TREE_TYPE (c) = type;
      return !force_fit_type (c, 0);
    }
}

/* Returns true if T is, contains, or refers to a type with variable
   size.  This concept is more general than that of C99 'variably
   modified types': in C99, a struct type is never variably modified
   because a VLA may not appear as a structure member.  However, in
   GNU C code like:

     struct S { int i[f()]; };

   is valid, and other languages may define similar constructs.  */

bool
variably_modified_type_p (tree type)
{
  tree t;

  if (type == error_mark_node)
    return false;

  /* If TYPE itself has variable size, it is variably modified.

     We do not yet have a representation of the C99 '[*]' syntax.
     When a representation is chosen, this function should be modified
     to test for that case as well.  */
  t = TYPE_SIZE (type);
  if (t && t != error_mark_node && TREE_CODE (t) != INTEGER_CST)
    return true;

  switch (TREE_CODE (type))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case ARRAY_TYPE:
      /* If TYPE is a pointer or reference, it is variably modified if
	 the type pointed to is variably modified.  Similarly for arrays;
	 note that VLAs are handled by the TYPE_SIZE check above.  */
      return variably_modified_type_p (TREE_TYPE (type));

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      /* If TYPE is a function type, it is variably modified if any of the
         parameters or the return type are variably modified.  */
      {
	tree parm;

	if (variably_modified_type_p (TREE_TYPE (type)))
	  return true;
	for (parm = TYPE_ARG_TYPES (type);
	     parm && parm != void_list_node;
	     parm = TREE_CHAIN (parm))
	  if (variably_modified_type_p (TREE_VALUE (parm)))
	    return true;
      }
      break;

    case INTEGER_TYPE:
      /* Scalar types are variably modified if their end points
	 aren't constant.  */
      t = TYPE_MIN_VALUE (type);
      if (t && t != error_mark_node && TREE_CODE (t) != INTEGER_CST)
	return true;
      t = TYPE_MAX_VALUE (type);
      if (t && t != error_mark_node && TREE_CODE (t) != INTEGER_CST)
	return true;
      return false;

    default:
      break;
    }

  /* The current language may have other cases to check, but in general,
     all other types are not variably modified.  */
  return (*lang_hooks.tree_inlining.var_mod_type_p) (type);
}

/* Given a DECL or TYPE, return the scope in which it was declared, or
   NULL_TREE if there is no containing scope.  */

tree
get_containing_scope (tree t)
{
  return (TYPE_P (t) ? TYPE_CONTEXT (t) : DECL_CONTEXT (t));
}

/* Return the innermost context enclosing DECL that is
   a FUNCTION_DECL, or zero if none.  */

tree
decl_function_context (tree decl)
{
  tree context;

  if (TREE_CODE (decl) == ERROR_MARK)
    return 0;

  if (TREE_CODE (decl) == SAVE_EXPR)
    context = SAVE_EXPR_CONTEXT (decl);

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
decl_type_context (tree decl)
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
	abort ();
      }

  return NULL_TREE;
}

/* CALL is a CALL_EXPR.  Return the declaration for the function
   called, or NULL_TREE if the called function cannot be
   determined.  */

tree
get_callee_fndecl (tree call)
{
  tree addr;

  /* It's invalid to call this function with anything but a
     CALL_EXPR.  */
  if (TREE_CODE (call) != CALL_EXPR)
    abort ();

  /* The first operand to the CALL is the address of the function
     called.  */
  addr = TREE_OPERAND (call, 0);

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
  
  /* We couldn't figure out what was being called.  Maybe the front
     end has some idea.  */
  return (*lang_hooks.lang_get_callee_fndecl) (call);
}

/* Print debugging information about tree nodes generated during the compile,
   and any language-specific information.  */

void
dump_tree_statistics (void)
{
#ifdef GATHER_STATISTICS
  int i;
  int total_nodes, total_bytes;
#endif

  fprintf (stderr, "\n??? tree nodes created\n\n");
#ifdef GATHER_STATISTICS
  fprintf (stderr, "Kind                   Nodes      Bytes\n");
  fprintf (stderr, "---------------------------------------\n");
  total_nodes = total_bytes = 0;
  for (i = 0; i < (int) all_kinds; i++)
    {
      fprintf (stderr, "%-20s %7d %10d\n", tree_node_kind_names[i],
	       tree_node_counts[i], tree_node_sizes[i]);
      total_nodes += tree_node_counts[i];
      total_bytes += tree_node_sizes[i];
    }
  fprintf (stderr, "---------------------------------------\n");
  fprintf (stderr, "%-20s %7d %10d\n", "Total", total_nodes, total_bytes);
  fprintf (stderr, "---------------------------------------\n");
#else
  fprintf (stderr, "(No per-node statistics)\n");
#endif
  print_type_hash_statistics ();
  (*lang_hooks.print_statistics) ();
}

#define FILE_FUNCTION_FORMAT "_GLOBAL__%s_%s"

/* Generate a crc32 of a string.  */

unsigned
crc32_string (unsigned chksum, const char *string)
{
  do
    {
      unsigned value = *string << 24;
      unsigned ix;
      
      for (ix = 8; ix--; value <<= 1)
  	{
  	  unsigned feedback;
  	  
  	  feedback = (value ^ chksum) & 0x80000000 ? 0x04c11db7 : 0;
 	  chksum <<= 1;
 	  chksum ^= feedback;
  	}
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

/* Generate a name for a function unique to this translation unit.
   TYPE is some string to identify the purpose of this function to the
   linker or collect2.  */

tree
get_file_function_name_long (const char *type)
{
  char *buf;
  const char *p;
  char *q;

  if (first_global_object_name)
    p = first_global_object_name;
  else
    {
      /* We don't have anything that we know to be unique to this translation
	 unit, so use what we do have and throw in some randomness.  */
      unsigned len;
      const char *name = weak_global_object_name;
      const char *file = main_input_filename;

      if (! name)
	name = "";
      if (! file)
	file = input_filename;

      len = strlen (file);
      q = alloca (9 * 2 + len + 1);
      memcpy (q, file, len + 1);
      clean_symbol_name (q);

      sprintf (q + len, "_%08X_%08X", crc32_string (0, name),
	       crc32_string (0, flag_random_seed));

      p = q;
    }

  buf = alloca (sizeof (FILE_FUNCTION_FORMAT) + strlen (p) + strlen (type));

  /* Set up the name of the file-level functions we may need.
     Use a global object (which is already required to be unique over
     the program) rather than the file name (which imposes extra
     constraints).  */
  sprintf (buf, FILE_FUNCTION_FORMAT, type, p);

  return get_identifier (buf);
}

/* If KIND=='I', return a suitable global initializer (constructor) name.
   If KIND=='D', return a suitable global clean-up (destructor) name.  */

tree
get_file_function_name (int kind)
{
  char p[2];

  p[0] = kind;
  p[1] = 0;

  return get_file_function_name_long (p);
}

/* Expand (the constant part of) a SET_TYPE CONSTRUCTOR node.
   The result is placed in BUFFER (which has length BIT_SIZE),
   with one bit in each char ('\000' or '\001').

   If the constructor is constant, NULL_TREE is returned.
   Otherwise, a TREE_LIST of the non-constant elements is emitted.  */

tree
get_set_constructor_bits (tree init, char *buffer, int bit_size)
{
  int i;
  tree vals;
  HOST_WIDE_INT domain_min
    = tree_low_cst (TYPE_MIN_VALUE (TYPE_DOMAIN (TREE_TYPE (init))), 0);
  tree non_const_bits = NULL_TREE;

  for (i = 0; i < bit_size; i++)
    buffer[i] = 0;

  for (vals = TREE_OPERAND (init, 1);
       vals != NULL_TREE; vals = TREE_CHAIN (vals))
    {
      if (!host_integerp (TREE_VALUE (vals), 0)
	  || (TREE_PURPOSE (vals) != NULL_TREE
	      && !host_integerp (TREE_PURPOSE (vals), 0)))
	non_const_bits
	  = tree_cons (TREE_PURPOSE (vals), TREE_VALUE (vals), non_const_bits);
      else if (TREE_PURPOSE (vals) != NULL_TREE)
	{
	  /* Set a range of bits to ones.  */
	  HOST_WIDE_INT lo_index
	    = tree_low_cst (TREE_PURPOSE (vals), 0) - domain_min;
	  HOST_WIDE_INT hi_index
	    = tree_low_cst (TREE_VALUE (vals), 0) - domain_min;

	  if (lo_index < 0 || lo_index >= bit_size
	      || hi_index < 0 || hi_index >= bit_size)
	    abort ();
	  for (; lo_index <= hi_index; lo_index++)
	    buffer[lo_index] = 1;
	}
      else
	{
	  /* Set a single bit to one.  */
	  HOST_WIDE_INT index
	    = tree_low_cst (TREE_VALUE (vals), 0) - domain_min;
	  if (index < 0 || index >= bit_size)
	    {
	      error ("invalid initializer for bit string");
	      return NULL_TREE;
	    }
	  buffer[index] = 1;
	}
    }
  return non_const_bits;
}

/* Expand (the constant part of) a SET_TYPE CONSTRUCTOR node.
   The result is placed in BUFFER (which is an array of bytes).
   If the constructor is constant, NULL_TREE is returned.
   Otherwise, a TREE_LIST of the non-constant elements is emitted.  */

tree
get_set_constructor_bytes (tree init, unsigned char *buffer, int wd_size)
{
  int i;
  int set_word_size = BITS_PER_UNIT;
  int bit_size = wd_size * set_word_size;
  int bit_pos = 0;
  unsigned char *bytep = buffer;
  char *bit_buffer = alloca (bit_size);
  tree non_const_bits = get_set_constructor_bits (init, bit_buffer, bit_size);

  for (i = 0; i < wd_size; i++)
    buffer[i] = 0;

  for (i = 0; i < bit_size; i++)
    {
      if (bit_buffer[i])
	{
	  if (BYTES_BIG_ENDIAN)
	    *bytep |= (1 << (set_word_size - 1 - bit_pos));
	  else
	    *bytep |= 1 << bit_pos;
	}
      bit_pos++;
      if (bit_pos >= set_word_size)
	bit_pos = 0, bytep++;
    }
  return non_const_bits;
}

#if defined ENABLE_TREE_CHECKING && (GCC_VERSION >= 2007)
/* Complain that the tree code of NODE does not match the expected CODE.
   FILE, LINE, and FUNCTION are of the caller.  */

void
tree_check_failed (const tree node, enum tree_code code, const char *file,
		   int line, const char *function)
{
  internal_error ("tree check: expected %s, have %s in %s, at %s:%d",
		  tree_code_name[code], tree_code_name[TREE_CODE (node)],
		  function, trim_filename (file), line);
}

/* Similar to above, except that we check for a class of tree
   code, given in CL.  */

void
tree_class_check_failed (const tree node, int cl, const char *file,
			 int line, const char *function)
{
  internal_error
    ("tree check: expected class '%c', have '%c' (%s) in %s, at %s:%d",
     cl, TREE_CODE_CLASS (TREE_CODE (node)),
     tree_code_name[TREE_CODE (node)], function, trim_filename (file), line);
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
   vector of an expression node.  */

void
tree_operand_check_failed (int idx, enum tree_code code, const char *file,
			   int line, const char *function)
{
  internal_error
    ("tree check: accessed operand %d of %s with %d operands in %s, at %s:%d",
     idx + 1, tree_code_name[code], TREE_CODE_LENGTH (code),
     function, trim_filename (file), line);
}
#endif /* ENABLE_TREE_CHECKING */

/* For a new vector type node T, build the information necessary for
   debugging output.  */

static void
finish_vector_type (tree t)
{
  layout_type (t);

  {
    tree index = build_int_2 (TYPE_VECTOR_SUBPARTS (t) - 1, 0);
    tree array = build_array_type (TREE_TYPE (t),
				   build_index_type (index));
    tree rt = make_node (RECORD_TYPE);

    TYPE_FIELDS (rt) = build_decl (FIELD_DECL, get_identifier ("f"), array);
    DECL_CONTEXT (TYPE_FIELDS (rt)) = rt;
    layout_type (rt);
    TYPE_DEBUG_REPRESENTATION_TYPE (t) = rt;
    /* In dwarfout.c, type lookup uses TYPE_UID numbers.  We want to output
       the representation type, and we want to find that die when looking up
       the vector type.  This is most easily achieved by making the TYPE_UID
       numbers equal.  */
    TYPE_UID (rt) = TYPE_UID (t);
  }
}

static tree
make_or_reuse_type (unsigned size, int unsignedp)
{
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

  if (unsignedp)
    return make_unsigned_type (size);
  else
    return make_signed_type (size);
}

/* Create nodes for all integer types (and error_mark_node) using the sizes
   of C datatypes.  The caller should call set_sizetype soon after calling
   this function to select one of the types as sizetype.  */

void
build_common_tree_nodes (int signed_char)
{
  error_mark_node = make_node (ERROR_MARK);
  TREE_TYPE (error_mark_node) = error_mark_node;

  initialize_sizetypes ();

  /* Define both `signed char' and `unsigned char'.  */
  signed_char_type_node = make_signed_type (CHAR_TYPE_SIZE);
  unsigned_char_type_node = make_unsigned_type (CHAR_TYPE_SIZE);

  /* Define `char', which is like either `signed char' or `unsigned char'
     but not the same as either.  */
  char_type_node
    = (signed_char
       ? make_signed_type (CHAR_TYPE_SIZE)
       : make_unsigned_type (CHAR_TYPE_SIZE));

  short_integer_type_node = make_signed_type (SHORT_TYPE_SIZE);
  short_unsigned_type_node = make_unsigned_type (SHORT_TYPE_SIZE);
  integer_type_node = make_signed_type (INT_TYPE_SIZE);
  unsigned_type_node = make_unsigned_type (INT_TYPE_SIZE);
  long_integer_type_node = make_signed_type (LONG_TYPE_SIZE);
  long_unsigned_type_node = make_unsigned_type (LONG_TYPE_SIZE);
  long_long_integer_type_node = make_signed_type (LONG_LONG_TYPE_SIZE);
  long_long_unsigned_type_node = make_unsigned_type (LONG_LONG_TYPE_SIZE);

  /* Define a boolean type.  This type only represents boolean values but
     may be larger than char depending on the value of BOOL_TYPE_SIZE.
     Front ends which want to override this size (i.e. Java) can redefine
     boolean_type_node before calling build_common_tree_nodes_2.  */
  boolean_type_node = make_unsigned_type (BOOL_TYPE_SIZE);
  TREE_SET_CODE (boolean_type_node, BOOLEAN_TYPE);
  TYPE_MAX_VALUE (boolean_type_node) = build_int_2 (1, 0);
  TREE_TYPE (TYPE_MAX_VALUE (boolean_type_node)) = boolean_type_node;
  TYPE_PRECISION (boolean_type_node) = 1;

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
 
  access_public_node = get_identifier ("public");
  access_protected_node = get_identifier ("protected");
  access_private_node = get_identifier ("private");
}

/* Call this function after calling build_common_tree_nodes and set_sizetype.
   It will create several other common tree nodes.  */

void
build_common_tree_nodes_2 (int short_double)
{
  /* Define these next since types below may used them.  */
  integer_zero_node = build_int_2 (0, 0);
  integer_one_node = build_int_2 (1, 0);
  integer_minus_one_node = build_int_2 (-1, -1);

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
  TYPE_ALIGN (void_type_node) = BITS_PER_UNIT;
  TYPE_USER_ALIGN (void_type_node) = 0;

  null_pointer_node = build_int_2 (0, 0);
  TREE_TYPE (null_pointer_node) = build_pointer_type (void_type_node);
  layout_type (TREE_TYPE (null_pointer_node));

  ptr_type_node = build_pointer_type (void_type_node);
  const_ptr_type_node
    = build_pointer_type (build_type_variant (void_type_node, 1, 0));

  float_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (float_type_node) = FLOAT_TYPE_SIZE;
  layout_type (float_type_node);

  double_type_node = make_node (REAL_TYPE);
  if (short_double)
    TYPE_PRECISION (double_type_node) = FLOAT_TYPE_SIZE;
  else
    TYPE_PRECISION (double_type_node) = DOUBLE_TYPE_SIZE;
  layout_type (double_type_node);

  long_double_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (long_double_type_node) = LONG_DOUBLE_TYPE_SIZE;
  layout_type (long_double_type_node);

  float_ptr_type_node = build_pointer_type (float_type_node);
  double_ptr_type_node = build_pointer_type (double_type_node);
  long_double_ptr_type_node = build_pointer_type (long_double_type_node);
  integer_ptr_type_node = build_pointer_type (integer_type_node);

  complex_integer_type_node = make_node (COMPLEX_TYPE);
  TREE_TYPE (complex_integer_type_node) = integer_type_node;
  layout_type (complex_integer_type_node);

  complex_float_type_node = make_node (COMPLEX_TYPE);
  TREE_TYPE (complex_float_type_node) = float_type_node;
  layout_type (complex_float_type_node);

  complex_double_type_node = make_node (COMPLEX_TYPE);
  TREE_TYPE (complex_double_type_node) = double_type_node;
  layout_type (complex_double_type_node);

  complex_long_double_type_node = make_node (COMPLEX_TYPE);
  TREE_TYPE (complex_long_double_type_node) = long_double_type_node;
  layout_type (complex_long_double_type_node);

  {
    tree t = (*targetm.build_builtin_va_list) ();

    /* Many back-ends define record types without setting TYPE_NAME.
       If we copied the record type here, we'd keep the original
       record type without a name.  This breaks name mangling.  So,
       don't copy record types and let c_common_nodes_and_builtins()
       declare the type to be __builtin_va_list.  */
    if (TREE_CODE (t) != RECORD_TYPE)
      t = build_type_copy (t);

    va_list_type_node = t;
  }

  unsigned_V4SI_type_node
    = make_vector (V4SImode, unsigned_intSI_type_node, 1);
  unsigned_V2HI_type_node
    = make_vector (V2HImode, unsigned_intHI_type_node, 1);
  unsigned_V2SI_type_node
    = make_vector (V2SImode, unsigned_intSI_type_node, 1);
  unsigned_V2DI_type_node
    = make_vector (V2DImode, unsigned_intDI_type_node, 1);
  unsigned_V4HI_type_node
    = make_vector (V4HImode, unsigned_intHI_type_node, 1);
  unsigned_V8QI_type_node
    = make_vector (V8QImode, unsigned_intQI_type_node, 1);
  unsigned_V8HI_type_node
    = make_vector (V8HImode, unsigned_intHI_type_node, 1);
  unsigned_V16QI_type_node
    = make_vector (V16QImode, unsigned_intQI_type_node, 1);
  unsigned_V1DI_type_node
    = make_vector (V1DImode, unsigned_intDI_type_node, 1);

  V16SF_type_node = make_vector (V16SFmode, float_type_node, 0);
  V4SF_type_node = make_vector (V4SFmode, float_type_node, 0);
  V4SI_type_node = make_vector (V4SImode, intSI_type_node, 0);
  V2HI_type_node = make_vector (V2HImode, intHI_type_node, 0);
  V2SI_type_node = make_vector (V2SImode, intSI_type_node, 0);
  V2DI_type_node = make_vector (V2DImode, intDI_type_node, 0);
  V4HI_type_node = make_vector (V4HImode, intHI_type_node, 0);
  V8QI_type_node = make_vector (V8QImode, intQI_type_node, 0);
  V8HI_type_node = make_vector (V8HImode, intHI_type_node, 0);
  V2SF_type_node = make_vector (V2SFmode, float_type_node, 0);
  V2DF_type_node = make_vector (V2DFmode, double_type_node, 0);
  V16QI_type_node = make_vector (V16QImode, intQI_type_node, 0);
  V1DI_type_node = make_vector (V1DImode, intDI_type_node, 0);
  V4DF_type_node = make_vector (V4DFmode, double_type_node, 0);
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

  if (POINTER_TYPE_P (type))
    {
      inner = reconstruct_complex_type (TREE_TYPE (type), bottom);
      outer = build_pointer_type (inner);
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
      outer = build_method_type_directly (TYPE_METHOD_BASETYPE (type),
					 inner,
					 TYPE_ARG_TYPES (type));
    }
  else
    return bottom;

  TREE_READONLY (outer) = TREE_READONLY (type);
  TREE_THIS_VOLATILE (outer) = TREE_THIS_VOLATILE (type);

  return outer;
}

/* Returns a vector tree node given a vector mode, the inner type, and
   the signness.  */

tree
make_vector (enum machine_mode mode, tree innertype, int unsignedp)
{
  tree t;

  t = make_node (VECTOR_TYPE);
  TREE_TYPE (t) = innertype;
  TYPE_MODE (t) = mode;
  TREE_UNSIGNED (TREE_TYPE (t)) = unsignedp;
  finish_vector_type (t);

  return t;
}

/* Given an initializer INIT, return TRUE if INIT is zero or some
   aggregate of zeros.  Otherwise return FALSE.  */

bool
initializer_zerop (tree init)
{
  STRIP_NOPS (init);

  switch (TREE_CODE (init))
    {
    case INTEGER_CST:
      return integer_zerop (init);
    case REAL_CST:
      return real_zerop (init)
	&& ! REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (init));
    case COMPLEX_CST:
      return integer_zerop (init)
	|| (real_zerop (init)
	    && ! REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (TREE_REALPART (init)))
	    && ! REAL_VALUE_MINUS_ZERO (TREE_REAL_CST (TREE_IMAGPART (init))));
    case CONSTRUCTOR:
      {
	/* Set is empty if it has no elements.  */
        if ((TREE_CODE (TREE_TYPE (init)) == SET_TYPE)
             && CONSTRUCTOR_ELTS (init))
	  return false;

	if (AGGREGATE_TYPE_P (TREE_TYPE (init)))
	  {
	    tree aggr_init = CONSTRUCTOR_ELTS (init);

	    while (aggr_init)
	      {
		if (! initializer_zerop (TREE_VALUE (aggr_init)))
		  return false;
		aggr_init = TREE_CHAIN (aggr_init);
	      }
	    return true;
	  }
	return false;
      }
    default:
      return false;
    }
}

#include "gt-tree.h"
