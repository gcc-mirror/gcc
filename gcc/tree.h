/* Front-end tree definitions for GNU compiler.
   Copyright (C) 1989, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2002 Free Software Foundation, Inc.

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

#include "machmode.h"
#include "version.h"

/* Codes of tree nodes */

#define DEFTREECODE(SYM, STRING, TYPE, NARGS)   SYM,

enum tree_code {
#include "tree.def"

  LAST_AND_UNUSED_TREE_CODE	/* A convenient way to get a value for
				   NUM_TREE_CODE.  */
};

#undef DEFTREECODE

/* Number of language-independent tree codes.  */
#define NUM_TREE_CODES ((int) LAST_AND_UNUSED_TREE_CODE)

/* Indexed by enum tree_code, contains a character which is
   `<' for a comparison expression, `1', for a unary arithmetic
   expression, `2' for a binary arithmetic expression, `e' for
   other types of expressions, `r' for a reference, `c' for a
   constant, `d' for a decl, `t' for a type, `s' for a statement,
   and `x' for anything else (TREE_LIST, IDENTIFIER, etc).  */

#define MAX_TREE_CODES 256
extern char tree_code_type[MAX_TREE_CODES];
#define TREE_CODE_CLASS(CODE)	tree_code_type[(int) (CODE)]

/* Returns non-zero iff CLASS is the tree-code class of an
   expression.  */

#define IS_EXPR_CODE_CLASS(CLASS) \
  ((CLASS) == '<' || (CLASS) == '1' || (CLASS) == '2' || (CLASS) == 'e')

/* Number of argument-words in each kind of tree-node.  */

extern int tree_code_length[MAX_TREE_CODES];
#define TREE_CODE_LENGTH(CODE)	tree_code_length[(int) (CODE)]

/* Names of tree components.  */

extern const char *tree_code_name[MAX_TREE_CODES];

/* Classify which part of the compiler has defined a given builtin function.
   Note that we assume below that this is no more than two bits.  */
enum built_in_class
{
  NOT_BUILT_IN = 0,
  BUILT_IN_FRONTEND,
  BUILT_IN_MD,
  BUILT_IN_NORMAL
};

/* Names for the above.  */
extern const char *const built_in_class_names[4];

/* Codes that identify the various built in functions
   so that expand_call can identify them quickly.  */

#define DEF_BUILTIN(ENUM, N, C, T, LT, B, F, NA) ENUM,
enum built_in_function
{
#include "builtins.def"

  /* Upper bound on non-language-specific builtins.  */
  END_BUILTINS
};
#undef DEF_BUILTIN

/* Names for the above.  */
extern const char *const built_in_names[(int) END_BUILTINS];

/* An array of _DECL trees for the above.  */
extern tree built_in_decls[(int) END_BUILTINS];

/* The definition of tree nodes fills the next several pages.  */

/* A tree node can represent a data type, a variable, an expression
   or a statement.  Each node has a TREE_CODE which says what kind of
   thing it represents.  Some common codes are:
   INTEGER_TYPE -- represents a type of integers.
   ARRAY_TYPE -- represents a type of pointer.
   VAR_DECL -- represents a declared variable.
   INTEGER_CST -- represents a constant integer value.
   PLUS_EXPR -- represents a sum (an expression).

   As for the contents of a tree node: there are some fields
   that all nodes share.  Each TREE_CODE has various special-purpose
   fields as well.  The fields of a node are never accessed directly,
   always through accessor macros.  */

/* Every kind of tree node starts with this structure,
   so all nodes have these fields.

   See the accessor macros, defined below, for documentation of the
   fields.  */

struct tree_common
{
  tree chain;
  tree type;

  ENUM_BITFIELD(tree_code) code : 8;

  unsigned side_effects_flag : 1;
  unsigned constant_flag : 1;
  unsigned addressable_flag : 1;
  unsigned volatile_flag : 1;
  unsigned readonly_flag : 1;
  unsigned unsigned_flag : 1;
  unsigned asm_written_flag: 1;
  unsigned unused_0 : 1;

  unsigned used_flag : 1;
  unsigned nothrow_flag : 1;
  unsigned static_flag : 1;
  unsigned public_flag : 1;
  unsigned private_flag : 1;
  unsigned protected_flag : 1;
  unsigned bounded_flag : 1;
  unsigned deprecated_flag : 1;

  unsigned lang_flag_0 : 1;
  unsigned lang_flag_1 : 1;
  unsigned lang_flag_2 : 1;
  unsigned lang_flag_3 : 1;
  unsigned lang_flag_4 : 1;
  unsigned lang_flag_5 : 1;
  unsigned lang_flag_6 : 1;
  unsigned unused_1 : 1;
};

/* The following table lists the uses of each of the above flags and
   for which types of nodes they are defined.  Note that expressions
   include decls.

   addressable_flag:

       TREE_ADDRESSABLE in
   	   VAR_DECL, FUNCTION_DECL, FIELD_DECL, CONSTRUCTOR, LABEL_DECL,
	   ..._TYPE, IDENTIFIER_NODE.
	   In a STMT_EXPR, it means we want the result of the enclosed
	   expression.

   static_flag:

       TREE_STATIC in
           VAR_DECL, FUNCTION_DECL, CONSTRUCTOR, ADDR_EXPR
       TREE_NO_UNUSED_WARNING in
           CONVERT_EXPR, NOP_EXPR, COMPOUND_EXPR
       TREE_VIA_VIRTUAL in
           TREE_LIST or TREE_VEC
       TREE_CONSTANT_OVERFLOW in
           INTEGER_CST, REAL_CST, COMPLEX_CST, VECTOR_CST
       TREE_SYMBOL_REFERENCED in
           IDENTIFIER_NODE
       CLEANUP_EH_ONLY in
           TARGET_EXPR, WITH_CLEANUP_EXPR, CLEANUP_STMT,
	   TREE_LIST elements of a block's cleanup list.

   public_flag:

       TREE_OVERFLOW in
           INTEGER_CST, REAL_CST, COMPLEX_CST, VECTOR_CST
       TREE_PUBLIC in
           VAR_DECL or FUNCTION_DECL or IDENTIFIER_NODE
       TREE_VIA_PUBLIC in
           TREE_LIST or TREE_VEC
       EXPR_WFL_EMIT_LINE_NOTE in
           EXPR_WITH_FILE_LOCATION

   private_flag:

       TREE_VIA_PRIVATE in
           TREE_LIST or TREE_VEC
       TREE_PRIVATE in
           ..._DECL

   protected_flag:

       TREE_VIA_PROTECTED in
           TREE_LIST
	   TREE_VEC
       TREE_PROTECTED in
           BLOCK
	   ..._DECL

   side_effects_flag:

       TREE_SIDE_EFFECTS in
           all expressions

   volatile_flag:

       TREE_THIS_VOLATILE in
           all expressions
       TYPE_VOLATILE in
           ..._TYPE

   readonly_flag:

       TREE_READONLY in
           all expressions
       TYPE_READONLY in
           ..._TYPE

   constant_flag:

       TREE_CONSTANT in
           all expressions

   unsigned_flag:

       TREE_UNSIGNED in
           INTEGER_TYPE, ENUMERAL_TYPE, FIELD_DECL
       DECL_BUILT_IN_NONANSI in
           FUNCTION_DECL
       SAVE_EXPR_NOPLACEHOLDER in
	   SAVE_EXPR

   asm_written_flag:

       TREE_ASM_WRITTEN in
           VAR_DECL, FUNCTION_DECL, RECORD_TYPE, UNION_TYPE, QUAL_UNION_TYPE
	   BLOCK

   used_flag:

       TREE_USED in
           expressions, IDENTIFIER_NODE

   nothrow_flag:

       TREE_NOTHROW in
           CALL_EXPR, FUNCTION_DECL

   bounded_flag:

       TREE_BOUNDED in
	   expressions, VAR_DECL, PARM_DECL, FIELD_DECL, FUNCTION_DECL,
	   IDENTIFIER_NODE
       TYPE_BOUNDED in
	   ..._TYPE

   deprecated_flag:

	TREE_DEPRECATED in
	   ..._DECL
*/

/* Define accessors for the fields that all tree nodes have
   (though some fields are not used for all kinds of nodes).  */

/* The tree-code says what kind of node it is.
   Codes are defined in tree.def.  */
#define TREE_CODE(NODE) ((enum tree_code) (NODE)->common.code)
#define TREE_SET_CODE(NODE, VALUE) \
((NODE)->common.code = (ENUM_BITFIELD (tree_code)) (VALUE))

/* When checking is enabled, errors will be generated if a tree node
   is accessed incorrectly. The macros abort with a fatal error.  */
#if defined ENABLE_TREE_CHECKING && (GCC_VERSION >= 2007)

#define TREE_CHECK(t, code) __extension__				\
({  const tree __t = (t);						\
    if (TREE_CODE(__t) != (code))					\
      tree_check_failed (__t, code, __FILE__, __LINE__, __FUNCTION__);	\
    __t; })
#define TREE_CLASS_CHECK(t, class) __extension__			\
({  const tree __t = (t);						\
    if (TREE_CODE_CLASS(TREE_CODE(__t)) != (class))			\
      tree_class_check_failed (__t, class, __FILE__, __LINE__,		\
			       __FUNCTION__);				\
    __t; })

/* These checks have to be special cased.  */
#define CST_OR_CONSTRUCTOR_CHECK(t) __extension__			\
({  const tree __t = (t);						\
    enum tree_code const __c = TREE_CODE(__t);				\
    if (__c != CONSTRUCTOR && TREE_CODE_CLASS(__c) != 'c')		\
      tree_check_failed (__t, CONSTRUCTOR, __FILE__, __LINE__,		\
			 __FUNCTION__);					\
    __t; })
#define EXPR_CHECK(t) __extension__					\
({  const tree __t = (t);						\
    char const __c = TREE_CODE_CLASS(TREE_CODE(__t));			\
    if (__c != 'r' && __c != 's' && __c != '<'				\
	&& __c != '1' && __c != '2' && __c != 'e')			\
      tree_class_check_failed (__t, 'e', __FILE__, __LINE__,		\
			       __FUNCTION__);				\
    __t; })

extern void tree_check_failed PARAMS ((const tree, enum tree_code,
				       const char *, int, const char *))
    ATTRIBUTE_NORETURN;
extern void tree_class_check_failed PARAMS ((const tree, int,
					     const char *, int, const char *))
    ATTRIBUTE_NORETURN;

#else /* not ENABLE_TREE_CHECKING, or not gcc */

#define TREE_CHECK(t, code)		(t)
#define TREE_CLASS_CHECK(t, code)	(t)
#define CST_OR_CONSTRUCTOR_CHECK(t)	(t)
#define EXPR_CHECK(t)			(t)

#endif

#include "tree-check.h"

#define TYPE_CHECK(tree)	TREE_CLASS_CHECK  (tree, 't')
#define DECL_CHECK(tree)	TREE_CLASS_CHECK  (tree, 'd')
#define CST_CHECK(tree)		TREE_CLASS_CHECK  (tree, 'c')

/* In all nodes that are expressions, this is the data type of the expression.
   In POINTER_TYPE nodes, this is the type that the pointer points to.
   In ARRAY_TYPE nodes, this is the type of the elements.
   In VECTOR_TYPE nodes, this is the type of the elements.  */
#define TREE_TYPE(NODE) ((NODE)->common.type)

/* Here is how primitive or already-canonicalized types' hash codes
   are made.  */
#define TYPE_HASH(TYPE) ((size_t) (TYPE) & 0777777)

/* Nodes are chained together for many purposes.
   Types are chained together to record them for being output to the debugger
   (see the function `chain_type').
   Decls in the same scope are chained together to record the contents
   of the scope.
   Statement nodes for successive statements used to be chained together.
   Often lists of things are represented by TREE_LIST nodes that
   are chained together.  */

#define TREE_CHAIN(NODE) ((NODE)->common.chain)

/* Given an expression as a tree, strip any NON_LVALUE_EXPRs and NOP_EXPRs
   that don't change the machine mode.  */

#define STRIP_NOPS(EXP)						\
  while ((TREE_CODE (EXP) == NOP_EXPR				\
	  || TREE_CODE (EXP) == CONVERT_EXPR			\
	  || TREE_CODE (EXP) == NON_LVALUE_EXPR)		\
	 && TREE_OPERAND (EXP, 0) != error_mark_node		\
	 && (TYPE_MODE (TREE_TYPE (EXP))			\
	     == TYPE_MODE (TREE_TYPE (TREE_OPERAND (EXP, 0)))))	\
    (EXP) = TREE_OPERAND (EXP, 0)

/* Like STRIP_NOPS, but don't let the signedness change either.  */

#define STRIP_SIGN_NOPS(EXP) \
  while ((TREE_CODE (EXP) == NOP_EXPR				\
	  || TREE_CODE (EXP) == CONVERT_EXPR			\
	  || TREE_CODE (EXP) == NON_LVALUE_EXPR)		\
	 && TREE_OPERAND (EXP, 0) != error_mark_node		\
	 && (TYPE_MODE (TREE_TYPE (EXP))			\
	     == TYPE_MODE (TREE_TYPE (TREE_OPERAND (EXP, 0))))	\
	 && (TREE_UNSIGNED (TREE_TYPE (EXP))			\
	     == TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (EXP, 0))))) \
    (EXP) = TREE_OPERAND (EXP, 0)

/* Like STRIP_NOPS, but don't alter the TREE_TYPE either.  */

#define STRIP_TYPE_NOPS(EXP) \
  while ((TREE_CODE (EXP) == NOP_EXPR				\
	  || TREE_CODE (EXP) == CONVERT_EXPR			\
	  || TREE_CODE (EXP) == NON_LVALUE_EXPR)		\
	 && TREE_OPERAND (EXP, 0) != error_mark_node		\
	 && (TREE_TYPE (EXP)					\
	     == TREE_TYPE (TREE_OPERAND (EXP, 0))))		\
    (EXP) = TREE_OPERAND (EXP, 0)

/* Nonzero if TYPE represents an integral type.  Note that we do not
   include COMPLEX types here.  */

#define INTEGRAL_TYPE_P(TYPE)  \
  (TREE_CODE (TYPE) == INTEGER_TYPE || TREE_CODE (TYPE) == ENUMERAL_TYPE  \
   || TREE_CODE (TYPE) == BOOLEAN_TYPE || TREE_CODE (TYPE) == CHAR_TYPE)

/* Nonzero if TYPE represents a floating-point type, including complex
   floating-point types.  */

#define FLOAT_TYPE_P(TYPE)		\
  (TREE_CODE (TYPE) == REAL_TYPE	\
   || (TREE_CODE (TYPE) == COMPLEX_TYPE \
       && TREE_CODE (TREE_TYPE (TYPE)) == REAL_TYPE))

/* Nonzero if TYPE represents an aggregate (multi-component) type.  */

#define AGGREGATE_TYPE_P(TYPE) \
  (TREE_CODE (TYPE) == ARRAY_TYPE || TREE_CODE (TYPE) == RECORD_TYPE \
   || TREE_CODE (TYPE) == UNION_TYPE || TREE_CODE (TYPE) == QUAL_UNION_TYPE \
   || TREE_CODE (TYPE) == SET_TYPE)

/* Nonzero if TYPE represents an unbounded pointer or unbounded
   reference type.  (It should be renamed to INDIRECT_TYPE_P.)  */

#define POINTER_TYPE_P(TYPE) \
  (TREE_CODE (TYPE) == POINTER_TYPE || TREE_CODE (TYPE) == REFERENCE_TYPE)

/* Nonzero if TYPE represents a bounded pointer or bounded reference type.  */

#define BOUNDED_INDIRECT_TYPE_P(TYPE) \
  (TREE_CODE (TYPE) == RECORD_TYPE && TREE_TYPE (TYPE))

/* Nonzero if TYPE represents a bounded pointer type.  */

#define BOUNDED_POINTER_TYPE_P(TYPE) \
  (BOUNDED_INDIRECT_TYPE_P (TYPE) \
   && TREE_CODE (TYPE_BOUNDED_SUBTYPE (TYPE)) == POINTER_TYPE)

/* Nonzero if TYPE represents a bounded reference type.  Bounded
   reference types have two specific uses: (1) When a reference is
   seated to a variable-length RECORD_TYPE that has an array of
   indeterminate length as its final field.  For all other objects, it
   is sufficient to check bounds at the time the reference is seated,
   and assume that all future uses of the reference are safe, since
   the address of references cannot change.  (2) When a reference
   supertype is seated to an subtype object.  The bounds "remember"
   the true size of the complete object, so that subsequent upcasts of
   the address of the reference will be checked properly (is such a
   thing valid C++?).  */

#define BOUNDED_REFERENCE_TYPE_P(TYPE) \
  (BOUNDED_INDIRECT_TYPE_P (TYPE) \
   && TREE_CODE (TYPE_BOUNDED_SUBTYPE (TYPE)) == REFERENCE_TYPE)

/* Nonzero if TYPE represents a pointer or reference type, either
   bounded or unbounded.  */

#define MAYBE_BOUNDED_INDIRECT_TYPE_P(TYPE) \
  (POINTER_TYPE_P (TYPE) || BOUNDED_INDIRECT_TYPE_P (TYPE))

/* Nonzero if TYPE represents a pointer type, either bounded or unbounded.  */

#define MAYBE_BOUNDED_POINTER_TYPE_P(TYPE) \
  (TREE_CODE (TYPE) == POINTER_TYPE || BOUNDED_POINTER_TYPE_P (TYPE))

/* Nonzero if TYPE represents a reference type, either bounded or
   unbounded.  */

#define MAYBE_BOUNDED_REFERENCE_TYPE_P(TYPE) \
  (TREE_CODE (TYPE) == REFERENCE_TYPE || BOUNDED_REFERENCE_TYPE_P (TYPE))

/* Nonzero if this type is a complete type.  */
#define COMPLETE_TYPE_P(NODE) (TYPE_SIZE (NODE) != NULL_TREE)

/* Nonzero if this type is the (possibly qualified) void type.  */
#define VOID_TYPE_P(NODE) (TREE_CODE (NODE) == VOID_TYPE)

/* Nonzero if this type is complete or is cv void.  */
#define COMPLETE_OR_VOID_TYPE_P(NODE) \
  (COMPLETE_TYPE_P (NODE) || VOID_TYPE_P (NODE))

/* Nonzero if this type is complete or is an array with unspecified bound.  */
#define COMPLETE_OR_UNBOUND_ARRAY_TYPE_P(NODE) \
  (COMPLETE_TYPE_P (TREE_CODE (NODE) == ARRAY_TYPE ? TREE_TYPE (NODE) : (NODE)))

/* Nonzero if TYPE represents a type.  */

#define TYPE_P(TYPE)	(TREE_CODE_CLASS (TREE_CODE (TYPE)) == 't')

/* Define many boolean fields that all tree nodes have.  */

/* In VAR_DECL nodes, nonzero means address of this is needed.
   So it cannot be in a register.
   In a FUNCTION_DECL, nonzero means its address is needed.
   So it must be compiled even if it is an inline function.
   In a FIELD_DECL node, it means that the programmer is permitted to
   construct the address of this field.  This is used for aliasing
   purposes: see record_component_aliases.
   In CONSTRUCTOR nodes, it means object constructed must be in memory.
   In LABEL_DECL nodes, it means a goto for this label has been seen 
   from a place outside all binding contours that restore stack levels.
   In ..._TYPE nodes, it means that objects of this type must
   be fully addressable.  This means that pieces of this
   object cannot go into register parameters, for example.
   In IDENTIFIER_NODEs, this means that some extern decl for this name
   had its address taken.  That matters for inline functions.  */
#define TREE_ADDRESSABLE(NODE) ((NODE)->common.addressable_flag)

/* In a VAR_DECL, nonzero means allocate static storage.
   In a FUNCTION_DECL, nonzero if function has been defined.
   In a CONSTRUCTOR, nonzero means allocate static storage.  */
#define TREE_STATIC(NODE) ((NODE)->common.static_flag)

/* In a TARGET_EXPR, WITH_CLEANUP_EXPR, CLEANUP_STMT, or element of a
   block's cleanup list, means that the pertinent cleanup should only be
   executed if an exception is thrown, not on normal exit of its scope.  */
#define CLEANUP_EH_ONLY(NODE) ((NODE)->common.static_flag)

/* In a CONVERT_EXPR, NOP_EXPR or COMPOUND_EXPR, this means the node was
   made implicitly and should not lead to an "unused value" warning.  */
#define TREE_NO_UNUSED_WARNING(NODE) ((NODE)->common.static_flag)

/* Nonzero for a TREE_LIST or TREE_VEC node means that the derivation
   chain is via a `virtual' declaration.  */
#define TREE_VIA_VIRTUAL(NODE) ((NODE)->common.static_flag)

/* In an INTEGER_CST, REAL_CST, COMPLEX_CST, or VECTOR_CST this means
   there was an overflow in folding.  This is distinct from
   TREE_OVERFLOW because ANSI C requires a diagnostic when overflows
   occur in constant expressions.  */
#define TREE_CONSTANT_OVERFLOW(NODE) ((NODE)->common.static_flag)

/* In an IDENTIFIER_NODE, this means that assemble_name was called with
   this string as an argument.  */
#define TREE_SYMBOL_REFERENCED(NODE) \
  (IDENTIFIER_NODE_CHECK (NODE)->common.static_flag)

/* In an INTEGER_CST, REAL_CST, COMPLEX_CST, or VECTOR_CST, this means
   there was an overflow in folding, and no warning has been issued
   for this subexpression.  TREE_OVERFLOW implies
   TREE_CONSTANT_OVERFLOW, but not vice versa.  */
#define TREE_OVERFLOW(NODE) ((NODE)->common.public_flag)

/* In a VAR_DECL or FUNCTION_DECL,
   nonzero means name is to be accessible from outside this module.
   In an IDENTIFIER_NODE, nonzero means an external declaration
   accessible from outside this module was previously seen
   for this name in an inner scope.  */
#define TREE_PUBLIC(NODE) ((NODE)->common.public_flag)

/* Nonzero for TREE_LIST or TREE_VEC node means that the path to the
   base class is via a `public' declaration, which preserves public
   fields from the base class as public.  */
#define TREE_VIA_PUBLIC(NODE) ((NODE)->common.public_flag)

/* Ditto, for `private' declarations.  */
#define TREE_VIA_PRIVATE(NODE) ((NODE)->common.private_flag)

/* Nonzero for TREE_LIST or TREE_VEC node means that the path to the
   base class is via a `protected' declaration, which preserves
   protected fields from the base class as protected.
   OVERLOADED.  */
#define TREE_VIA_PROTECTED(NODE) ((NODE)->common.protected_flag)

/* In any expression, nonzero means it has side effects or reevaluation
   of the whole expression could produce a different value.
   This is set if any subexpression is a function call, a side effect
   or a reference to a volatile variable.
   In a ..._DECL, this is set only if the declaration said `volatile'.  */
#define TREE_SIDE_EFFECTS(NODE) ((NODE)->common.side_effects_flag)

/* Nonzero means this expression is volatile in the C sense:
   its address should be of type `volatile WHATEVER *'.
   In other words, the declared item is volatile qualified.
   This is used in _DECL nodes and _REF nodes.

   In a ..._TYPE node, means this type is volatile-qualified.
   But use TYPE_VOLATILE instead of this macro when the node is a type,
   because eventually we may make that a different bit.

   If this bit is set in an expression, so is TREE_SIDE_EFFECTS.  */
#define TREE_THIS_VOLATILE(NODE) ((NODE)->common.volatile_flag)

/* In a VAR_DECL, PARM_DECL or FIELD_DECL, or any kind of ..._REF node,
   nonzero means it may not be the lhs of an assignment.
   In a ..._TYPE node, means this type is const-qualified
   (but the macro TYPE_READONLY should be used instead of this macro
   when the node is a type).  */
#define TREE_READONLY(NODE) ((NODE)->common.readonly_flag)

/* Non-zero if NODE is a _DECL with TREE_READONLY set.  */
#define TREE_READONLY_DECL_P(NODE) (TREE_READONLY (NODE) && DECL_P (NODE))

/* Value of expression is constant.
   Always appears in all ..._CST nodes.
   May also appear in an arithmetic expression, an ADDR_EXPR or a CONSTRUCTOR
   if the value is constant.  */
#define TREE_CONSTANT(NODE) ((NODE)->common.constant_flag)

/* In INTEGER_TYPE or ENUMERAL_TYPE nodes, means an unsigned type.
   In FIELD_DECL nodes, means an unsigned bit field.
   The same bit is used in functions as DECL_BUILT_IN_NONANSI.  */
#define TREE_UNSIGNED(NODE) ((NODE)->common.unsigned_flag)

#define TYPE_TRAP_SIGNED(NODE) \
  (flag_trapv && ! TREE_UNSIGNED (TYPE_CHECK (NODE)))

/* Nonzero in a VAR_DECL means assembler code has been written.
   Nonzero in a FUNCTION_DECL means that the function has been compiled.
   This is interesting in an inline function, since it might not need
   to be compiled separately.
   Nonzero in a RECORD_TYPE, UNION_TYPE, QUAL_UNION_TYPE or ENUMERAL_TYPE
   if the sdb debugging info for the type has been written.
   In a BLOCK node, nonzero if reorder_blocks has already seen this block.  */
#define TREE_ASM_WRITTEN(NODE) ((NODE)->common.asm_written_flag)

/* Nonzero in a _DECL if the name is used in its scope.
   Nonzero in an expr node means inhibit warning if value is unused.
   In IDENTIFIER_NODEs, this means that some extern decl for this name
   was used.  */
#define TREE_USED(NODE) ((NODE)->common.used_flag)

/* In a FUNCTION_DECL, nonzero means a call to the function cannot throw
   an exception.  In a CALL_EXPR, nonzero means the call cannot throw.  */
#define TREE_NOTHROW(NODE) ((NODE)->common.nothrow_flag)

/* In a type, nonzero means that all objects of the type are guaranteed by the
   language or front-end to be properly aligned, so we can indicate that a MEM
   of this type is aligned at least to the alignment of the type, even if it
   doesn't appear that it is.  We see this, for example, in object-oriented
   languages where a tag field may show this is an object of a more-aligned
   variant of the more generic type.  */
#define TYPE_ALIGN_OK(NODE) (TYPE_CHECK (NODE)->common.nothrow_flag)

/* Used in classes in C++.  */
#define TREE_PRIVATE(NODE) ((NODE)->common.private_flag)
/* Used in classes in C++.
   In a BLOCK node, this is BLOCK_HANDLER_BLOCK.  */
#define TREE_PROTECTED(NODE) ((NODE)->common.protected_flag)

/* In a ..._TYPE node, nonzero means that the type's size and layout,
   (or the size and layout of its arguments and/or return value in the
   case of a FUNCTION_TYPE or METHOD_TYPE) was changed by the presence
   of pointer bounds.  Use TYPE_BOUNDED instead of this macro when the
   node is a type, because eventually we may make that a different
   bit.  TYPE_BOUNDED doesn't mean that this type is a bounded indirect
   type--use BOUNDED_POINTER_TYPE_P, BOUNDED_REFERENCE_TYPE_P,
   BOUNDED_INDIRECT_TYPE_P to test for that.

   In a FUNCTION_DECL, nonzero means that the size and layout of one
   of its arguments and/or return value was changed by the presence of
   pointer bounds.  This value can differ from the value of
   TYPE_BOUNDED (TREE_TYPE (fundecl)) if the function was implicitly
   declared, then later called with pointer args, or was declared with
   a variable argument list and is later called with pointer values in
   the variable argument list.

   In a VAR_DECL, PARM_DECL or FIELD_DECL, TREE_BOUNDED matches the value
   of the decl's type's BOUNDED_POINTER_TYPE_P.

   In a CONSTRUCTOR or other expression, nonzero means the value is a
   bounded pointer.  It is insufficient to determine the boundedness
   of an expression EXP with BOUNDED_POINTER_TYPE_P (TREE_TYPE (EXP)),
   since we allow pointer to be temporarily cast to integer for
   rounding up to an alignment boudary in a way that preserves the
   pointer's bounds.

   In an IDENTIFIER_NODE, nonzero means that the name is prefixed with
   BP_PREFIX (see varasm.c).  This occurs for the DECL_ASSEMBLER_NAME
   of a function that has bounded pointer(s) for its return type and/or
   argument type(s).  */

#define TREE_BOUNDED(NODE) ((NODE)->common.bounded_flag)

/* Nonzero in a IDENTIFIER_NODE if the use of the name is defined as a
   deprecated feature by __attribute__((deprecated)).  */
#define TREE_DEPRECATED(NODE) ((NODE)->common.deprecated_flag)

/* These flags are available for each language front end to use internally.  */
#define TREE_LANG_FLAG_0(NODE) ((NODE)->common.lang_flag_0)
#define TREE_LANG_FLAG_1(NODE) ((NODE)->common.lang_flag_1)
#define TREE_LANG_FLAG_2(NODE) ((NODE)->common.lang_flag_2)
#define TREE_LANG_FLAG_3(NODE) ((NODE)->common.lang_flag_3)
#define TREE_LANG_FLAG_4(NODE) ((NODE)->common.lang_flag_4)
#define TREE_LANG_FLAG_5(NODE) ((NODE)->common.lang_flag_5)
#define TREE_LANG_FLAG_6(NODE) ((NODE)->common.lang_flag_6)

/* Define additional fields and accessors for nodes representing constants.  */

/* In an INTEGER_CST node.  These two together make a 2-word integer.
   If the data type is signed, the value is sign-extended to 2 words
   even though not all of them may really be in use.
   In an unsigned constant shorter than 2 words, the extra bits are 0.  */
#define TREE_INT_CST(NODE) (INTEGER_CST_CHECK (NODE)->int_cst.int_cst)
#define TREE_INT_CST_LOW(NODE) (TREE_INT_CST (NODE).low)
#define TREE_INT_CST_HIGH(NODE) (TREE_INT_CST (NODE).high)

#define INT_CST_LT(A, B)				\
  (TREE_INT_CST_HIGH (A) < TREE_INT_CST_HIGH (B)	\
   || (TREE_INT_CST_HIGH (A) == TREE_INT_CST_HIGH (B)	\
       && TREE_INT_CST_LOW (A) < TREE_INT_CST_LOW (B)))

#define INT_CST_LT_UNSIGNED(A, B)				\
  (((unsigned HOST_WIDE_INT) TREE_INT_CST_HIGH (A)		\
    < (unsigned HOST_WIDE_INT) TREE_INT_CST_HIGH (B))		\
   || (((unsigned HOST_WIDE_INT) TREE_INT_CST_HIGH (A)		\
        == (unsigned HOST_WIDE_INT) TREE_INT_CST_HIGH (B))	\
       && TREE_INT_CST_LOW (A) < TREE_INT_CST_LOW (B)))
 
struct tree_int_cst
{
  struct tree_common common;
  rtx rtl;	/* acts as link to register transfer language
			   (rtl) info */
  /* A sub-struct is necessary here because the function `const_hash'
     wants to scan both words as a unit and taking the address of the
     sub-struct yields the properly inclusive bounded pointer.  */
  struct {
    unsigned HOST_WIDE_INT low;
    HOST_WIDE_INT high;
  } int_cst;
};

/* In REAL_CST, STRING_CST, COMPLEX_CST, VECTOR_CST nodes, and
   CONSTRUCTOR nodes, and generally in all kinds of constants that
   could be given labels (rather than being immediate).  */

#define TREE_CST_RTL(NODE) (CST_OR_CONSTRUCTOR_CHECK (NODE)->real_cst.rtl)

/* In a REAL_CST node.

   We can represent a real value as either a `double' or an array of
   longs.  */

#define TREE_REAL_CST(NODE) (REAL_CST_CHECK (NODE)->real_cst.real_cst)

#include "real.h"

struct tree_real_cst
{
  struct tree_common common;
  rtx rtl;	/* acts as link to register transfer language (rtl) info */
  REAL_VALUE_TYPE real_cst;
};

/* In a STRING_CST */
#define TREE_STRING_LENGTH(NODE) (STRING_CST_CHECK (NODE)->string.length)
#define TREE_STRING_POINTER(NODE) (STRING_CST_CHECK (NODE)->string.pointer)

struct tree_string
{
  struct tree_common common;
  rtx rtl;	/* acts as link to register transfer language (rtl) info */
  int length;
  const char *pointer;
};

/* In a COMPLEX_CST node.  */
#define TREE_REALPART(NODE) (COMPLEX_CST_CHECK (NODE)->complex.real)
#define TREE_IMAGPART(NODE) (COMPLEX_CST_CHECK (NODE)->complex.imag)

struct tree_complex
{
  struct tree_common common;
  rtx rtl;	/* acts as link to register transfer language (rtl) info */
  tree real;
  tree imag;
};

/* In a VECTOR_CST node.  */
#define TREE_VECTOR_CST_ELTS(NODE) (VECTOR_CST_CHECK (NODE)->vector.elements)

struct tree_vector
{
  struct tree_common common;
  rtx rtl;
  tree elements;
};

#include "hashtable.h"

/* Define fields and accessors for some special-purpose tree nodes.  */

#define IDENTIFIER_LENGTH(NODE) \
  (IDENTIFIER_NODE_CHECK (NODE)->identifier.id.len)
#define IDENTIFIER_POINTER(NODE) \
  ((const char *) IDENTIFIER_NODE_CHECK (NODE)->identifier.id.str)

/* Translate a hash table identifier pointer to a tree_identifier
   pointer, and vice versa.  */

#define HT_IDENT_TO_GCC_IDENT(NODE) \
  ((tree) ((char *) (NODE) - sizeof (struct tree_common)))
#define GCC_IDENT_TO_HT_IDENT(NODE) (&((struct tree_identifier *) (NODE))->id)

struct tree_identifier
{
  struct tree_common common;
  struct ht_identifier id;
};

/* In a TREE_LIST node.  */
#define TREE_PURPOSE(NODE) (TREE_LIST_CHECK (NODE)->list.purpose)
#define TREE_VALUE(NODE) (TREE_LIST_CHECK (NODE)->list.value)

struct tree_list
{
  struct tree_common common;
  tree purpose;
  tree value;
};

/* In a TREE_VEC node.  */
#define TREE_VEC_LENGTH(NODE) (TREE_VEC_CHECK (NODE)->vec.length)
#define TREE_VEC_ELT(NODE,I) (TREE_VEC_CHECK (NODE)->vec.a[I])
#define TREE_VEC_END(NODE) \
  ((void) TREE_VEC_CHECK (NODE), &((NODE)->vec.a[(NODE)->vec.length]))

struct tree_vec
{
  struct tree_common common;
  int length;
  tree a[1];
};

/* Define fields and accessors for some nodes that represent expressions.  */

/* In a SAVE_EXPR node.  */
#define SAVE_EXPR_CONTEXT(NODE) TREE_OPERAND (SAVE_EXPR_CHECK (NODE), 1)
#define SAVE_EXPR_RTL(NODE) (*(rtx *) &SAVE_EXPR_CHECK (NODE)->exp.operands[2])
#define SAVE_EXPR_NOPLACEHOLDER(NODE) TREE_UNSIGNED (SAVE_EXPR_CHECK (NODE))
/* Nonzero if the SAVE_EXPRs value should be kept, even if it occurs
   both in normal code and in a handler.  (Normally, in a handler, all
   SAVE_EXPRs are unsaved, meaning that there values are
   recalculated.)  */
#define SAVE_EXPR_PERSISTENT_P(NODE) TREE_ASM_WRITTEN (SAVE_EXPR_CHECK (NODE))

/* In a RTL_EXPR node.  */
#define RTL_EXPR_SEQUENCE(NODE) \
  (*(rtx *) &RTL_EXPR_CHECK (NODE)->exp.operands[0])
#define RTL_EXPR_RTL(NODE) (*(rtx *) &RTL_EXPR_CHECK (NODE)->exp.operands[1])

/* In a WITH_CLEANUP_EXPR node.  */
#define WITH_CLEANUP_EXPR_RTL(NODE) \
  (*(rtx *) &WITH_CLEANUP_EXPR_CHECK (NODE)->exp.operands[2])

/* In a CONSTRUCTOR node.  */
#define CONSTRUCTOR_ELTS(NODE) TREE_OPERAND (CONSTRUCTOR_CHECK (NODE), 1)

/* In ordinary expression nodes.  */
#define TREE_OPERAND(NODE, I) (EXPR_CHECK (NODE)->exp.operands[I])
#define TREE_COMPLEXITY(NODE) (EXPR_CHECK (NODE)->exp.complexity)

/* In a LABELED_BLOCK_EXPR node.  */
#define LABELED_BLOCK_LABEL(NODE) \
  TREE_OPERAND (LABELED_BLOCK_EXPR_CHECK (NODE), 0)
#define LABELED_BLOCK_BODY(NODE) \
  TREE_OPERAND (LABELED_BLOCK_EXPR_CHECK (NODE), 1)

/* In a EXIT_BLOCK_EXPR node.  */
#define EXIT_BLOCK_LABELED_BLOCK(NODE) \
  TREE_OPERAND (EXIT_BLOCK_EXPR_CHECK (NODE), 0)
#define EXIT_BLOCK_RETURN(NODE) TREE_OPERAND (EXIT_BLOCK_EXPR_CHECK (NODE), 1)

/* In a LOOP_EXPR node.  */
#define LOOP_EXPR_BODY(NODE) TREE_OPERAND (LOOP_EXPR_CHECK (NODE), 0)

/* In a EXPR_WITH_FILE_LOCATION node.  */
#define EXPR_WFL_EMIT_LINE_NOTE(NODE) \
  (EXPR_WITH_FILE_LOCATION_CHECK (NODE)->common.public_flag)
#define EXPR_WFL_NODE(NODE) \
  TREE_OPERAND (EXPR_WITH_FILE_LOCATION_CHECK (NODE), 0)
#define EXPR_WFL_FILENAME_NODE(NODE) \
  TREE_OPERAND (EXPR_WITH_FILE_LOCATION_CHECK (NODE), 1)
#define EXPR_WFL_FILENAME(NODE) \
  IDENTIFIER_POINTER (EXPR_WFL_FILENAME_NODE (NODE))
/* ??? Java uses this in all expressions.  */
#define EXPR_WFL_LINECOL(NODE) (EXPR_CHECK (NODE)->exp.complexity)
#define EXPR_WFL_LINENO(NODE) (EXPR_WFL_LINECOL (NODE) >> 12)
#define EXPR_WFL_COLNO(NODE) (EXPR_WFL_LINECOL (NODE) & 0xfff)
#define EXPR_WFL_SET_LINECOL(NODE, LINE, COL) \
  (EXPR_WFL_LINECOL(NODE) = ((LINE) << 12) | ((COL) & 0xfff))

struct tree_exp
{
  struct tree_common common;
  int complexity;
  tree operands[1];
};

/* In a BLOCK node.  */
#define BLOCK_VARS(NODE) (BLOCK_CHECK (NODE)->block.vars)
#define BLOCK_SUBBLOCKS(NODE) (BLOCK_CHECK (NODE)->block.subblocks)
#define BLOCK_SUPERCONTEXT(NODE) (BLOCK_CHECK (NODE)->block.supercontext)
/* Note: when changing this, make sure to find the places
   that use chainon or nreverse.  */
#define BLOCK_CHAIN(NODE) TREE_CHAIN (BLOCK_CHECK (NODE))
#define BLOCK_ABSTRACT_ORIGIN(NODE) (BLOCK_CHECK (NODE)->block.abstract_origin)
#define BLOCK_ABSTRACT(NODE) (BLOCK_CHECK (NODE)->block.abstract_flag)

/* Nonzero means that this block is prepared to handle exceptions
   listed in the BLOCK_VARS slot.  */
#define BLOCK_HANDLER_BLOCK(NODE) \
  (BLOCK_CHECK (NODE)->block.handler_block_flag)

/* An index number for this block.  These values are not guaranteed to
   be unique across functions -- whether or not they are depends on
   the debugging output format in use.  */
#define BLOCK_NUMBER(NODE) (BLOCK_CHECK (NODE)->block.block_num)

/* If block reordering splits a lexical block into discontiguous
   address ranges, we'll make a copy of the original block.

   Note that this is logically distinct from BLOCK_ABSTRACT_ORIGIN.
   In that case, we have one source block that has been replicated
   (through inlining or unrolling) into many logical blocks, and that
   these logical blocks have different physical variables in them.

   In this case, we have one logical block split into several
   non-contiguous address ranges.  Most debug formats can't actually
   represent this idea directly, so we fake it by creating multiple
   logical blocks with the same variables in them.  However, for those
   that do support non-contiguous regions, these allow the original
   logical block to be reconstructed, along with the set of address
   ranges.

   One of the logical block fragments is arbitrarily chosen to be
   the ORIGIN.  The other fragments will point to the origin via
   BLOCK_FRAGMENT_ORIGIN; the origin itself will have this pointer
   be null.  The list of fragments will be chained through 
   BLOCK_FRAGMENT_CHAIN from the origin.  */

#define BLOCK_FRAGMENT_ORIGIN(NODE) (BLOCK_CHECK (NODE)->block.fragment_origin)
#define BLOCK_FRAGMENT_CHAIN(NODE) (BLOCK_CHECK (NODE)->block.fragment_chain)

struct tree_block
{
  struct tree_common common;

  unsigned handler_block_flag : 1;
  unsigned abstract_flag : 1;
  unsigned block_num : 30;

  tree vars;
  tree subblocks;
  tree supercontext;
  tree abstract_origin;
  tree fragment_origin;
  tree fragment_chain;
};

/* Define fields and accessors for nodes representing data types.  */

/* See tree.def for documentation of the use of these fields.
   Look at the documentation of the various ..._TYPE tree codes.  */

#define TYPE_UID(NODE) (TYPE_CHECK (NODE)->type.uid)
#define TYPE_SIZE(NODE) (TYPE_CHECK (NODE)->type.size)
#define TYPE_SIZE_UNIT(NODE) (TYPE_CHECK (NODE)->type.size_unit)
#define TYPE_MODE(NODE) (TYPE_CHECK (NODE)->type.mode)
#define TYPE_VALUES(NODE) (TYPE_CHECK (NODE)->type.values)
#define TYPE_DOMAIN(NODE) (TYPE_CHECK (NODE)->type.values)
#define TYPE_FIELDS(NODE) (TYPE_CHECK (NODE)->type.values)
#define TYPE_METHODS(NODE) (TYPE_CHECK (NODE)->type.maxval)
#define TYPE_VFIELD(NODE) (TYPE_CHECK (NODE)->type.minval)
#define TYPE_ARG_TYPES(NODE) (TYPE_CHECK (NODE)->type.values)
#define TYPE_METHOD_BASETYPE(NODE) (TYPE_CHECK (NODE)->type.maxval)
#define TYPE_OFFSET_BASETYPE(NODE) (TYPE_CHECK (NODE)->type.maxval)
#define TYPE_POINTER_TO(NODE) (TYPE_CHECK (NODE)->type.pointer_to)
#define TYPE_REFERENCE_TO(NODE) (TYPE_CHECK (NODE)->type.reference_to)
#define TYPE_MIN_VALUE(NODE) (TYPE_CHECK (NODE)->type.minval)
#define TYPE_MAX_VALUE(NODE) (TYPE_CHECK (NODE)->type.maxval)
#define TYPE_PRECISION(NODE) (TYPE_CHECK (NODE)->type.precision)
#define TYPE_SYMTAB_ADDRESS(NODE) (TYPE_CHECK (NODE)->type.symtab.address)
#define TYPE_SYMTAB_POINTER(NODE) (TYPE_CHECK (NODE)->type.symtab.pointer)
#define TYPE_NAME(NODE) (TYPE_CHECK (NODE)->type.name)
#define TYPE_NEXT_VARIANT(NODE) (TYPE_CHECK (NODE)->type.next_variant)
#define TYPE_MAIN_VARIANT(NODE) (TYPE_CHECK (NODE)->type.main_variant)
#define TYPE_CONTEXT(NODE) (TYPE_CHECK (NODE)->type.context)
#define TYPE_LANG_SPECIFIC(NODE) (TYPE_CHECK (NODE)->type.lang_specific)

/* For a VECTOR_TYPE node, this describes a different type which is emitted
   in the debugging output.  We use this to describe a vector as a
   structure containing an array.  */
#define TYPE_DEBUG_REPRESENTATION_TYPE(NODE) (TYPE_CHECK (NODE)->type.values)

/* Indirect types present difficulties because they may be represented
   as either POINTER_TYPE/REFERENCE_TYPE nodes (unbounded) or as
   RECORD_TYPE nodes (bounded).  Bounded and unbounded pointers might
   be logically equivalent, but physically different.  Simple
   comparison of the main variant only tells if the types are
   logically equivalent.  Use this predicate to compare for physical
   equivalency.  */

/* Types have the same main variant, and have the same boundedness.  */
#define TYPE_MAIN_VARIANTS_PHYSICALLY_EQUAL_P(TYPE1, TYPE2)	\
  (TYPE_MAIN_VARIANT (TYPE1) == TYPE_MAIN_VARIANT (TYPE2)	\
   && TREE_CODE (TYPE1) == TREE_CODE (TYPE2))

/* Return the type variant that has no qualifiers (i.e., the main variant),
   except that the boundedness qualifier is preserved.  */
#define TYPE_MAIN_PHYSICAL_VARIANT(TYPE)		\
  (BOUNDED_POINTER_TYPE_P (TYPE)			\
   ? build_qualified_type (TYPE, TYPE_QUAL_BOUNDED)	\
   : TYPE_MAIN_VARIANT (TYPE))

/* For aggregate types, information about this type, as a base type
   for itself.  Used in a language-dependent way for types that are
   neither a RECORD_TYPE, QUAL_UNION_TYPE, nor a UNION_TYPE.  */
#define TYPE_BINFO(NODE) (TYPE_CHECK (NODE)->type.binfo)

/* The (language-specific) typed-based alias set for this type.
   Objects whose TYPE_ALIAS_SETs are different cannot alias each
   other.  If the TYPE_ALIAS_SET is -1, no alias set has yet been
   assigned to this type.  If the TYPE_ALIAS_SET is 0, objects of this
   type can alias objects of any type.  */
#define TYPE_ALIAS_SET(NODE) (TYPE_CHECK (NODE)->type.alias_set)

/* Nonzero iff the typed-based alias set for this type has been
   calculated.  */
#define TYPE_ALIAS_SET_KNOWN_P(NODE) (TYPE_CHECK (NODE)->type.alias_set != -1)

/* A TREE_LIST of IDENTIFIER nodes of the attributes that apply
   to this type.  */
#define TYPE_ATTRIBUTES(NODE) (TYPE_CHECK (NODE)->type.attributes)

/* The alignment necessary for objects of this type.
   The value is an int, measured in bits.  */
#define TYPE_ALIGN(NODE) (TYPE_CHECK (NODE)->type.align)

/* 1 if the alignment for this type was requested by "aligned" attribute,
   0 if it is the default for this type.  */
#define TYPE_USER_ALIGN(NODE) (TYPE_CHECK (NODE)->type.user_align)

/* The alignment for NODE, in bytes.  */
#define TYPE_ALIGN_UNIT(NODE) (TYPE_ALIGN (NODE) / BITS_PER_UNIT)

/* If your language allows you to declare types, and you want debug info
   for them, then you need to generate corresponding TYPE_DECL nodes.
   These "stub" TYPE_DECL nodes have no name, and simply point at the
   type node.  You then set the TYPE_STUB_DECL field of the type node
   to point back at the TYPE_DECL node.  This allows the debug routines
   to know that the two nodes represent the same type, so that we only
   get one debug info record for them.  */
#define TYPE_STUB_DECL(NODE) TREE_CHAIN (NODE)

/* In a RECORD_TYPE, UNION_TYPE or QUAL_UNION_TYPE, it means the type
   has BLKmode only because it lacks the alignment requirement for
   its size.  */
#define TYPE_NO_FORCE_BLK(NODE) (TYPE_CHECK (NODE)->type.no_force_blk_flag)

/* In an INTEGER_TYPE, it means the type represents a size.  We use
   this both for validity checking and to permit optimizations that
   are unsafe for other types.  Note that the C `size_t' type should
   *not* have this flag set.  The `size_t' type is simply a typedef
   for an ordinary integer type that happens to be the type of an
   expression returned by `sizeof'; `size_t' has no special
   properties.  Expressions whose type have TYPE_IS_SIZETYPE set are
   always actual sizes.  */
#define TYPE_IS_SIZETYPE(NODE) \
  (INTEGER_TYPE_CHECK (NODE)->type.no_force_blk_flag)

/* In a FUNCTION_TYPE, indicates that the function returns with the stack
   pointer depressed.  */
#define TYPE_RETURNS_STACK_DEPRESSED(NODE) \
  (FUNCTION_TYPE_CHECK (NODE)->type.no_force_blk_flag)

/* Nonzero in a type considered volatile as a whole.  */
#define TYPE_VOLATILE(NODE) (TYPE_CHECK (NODE)->common.volatile_flag)

/* Means this type is const-qualified.  */
#define TYPE_READONLY(NODE) (TYPE_CHECK (NODE)->common.readonly_flag)

/* If nonzero, this type is `restrict'-qualified, in the C sense of
   the term.  */
#define TYPE_RESTRICT(NODE) (TYPE_CHECK (NODE)->type.restrict_flag)

/* If nonzero, this type's size and layout, (or the size and layout of
   its arguments and/or return value in the case of a FUNCTION_TYPE or
   METHOD_TYPE) was changed by the presence of pointer bounds.  */
#define TYPE_BOUNDED(NODE) (TYPE_CHECK (NODE)->common.bounded_flag)

/* There is a TYPE_QUAL value for each type qualifier.  They can be
   combined by bitwise-or to form the complete set of qualifiers for a
   type.  */

#define TYPE_UNQUALIFIED   0x0
#define TYPE_QUAL_CONST    0x1
#define TYPE_QUAL_VOLATILE 0x2
#define TYPE_QUAL_RESTRICT 0x4
#define TYPE_QUAL_BOUNDED  0x8

/* The set of type qualifiers for this type.  */
#define TYPE_QUALS(NODE)					\
  ((TYPE_READONLY (NODE) * TYPE_QUAL_CONST)			\
   | (TYPE_VOLATILE (NODE) * TYPE_QUAL_VOLATILE)		\
   | (TYPE_RESTRICT (NODE) * TYPE_QUAL_RESTRICT)		\
   | (BOUNDED_INDIRECT_TYPE_P (NODE) * TYPE_QUAL_BOUNDED))

/* The set of qualifiers pertinent to an expression node.  */
#define TREE_EXPR_QUALS(NODE)				\
  ((TREE_READONLY (NODE) * TYPE_QUAL_CONST)		\
   | (TREE_THIS_VOLATILE (NODE) * TYPE_QUAL_VOLATILE)	\
   | (TREE_BOUNDED (NODE) * TYPE_QUAL_BOUNDED))

/* The set of qualifiers pertinent to a FUNCTION_DECL node.  */
#define TREE_FUNC_QUALS(NODE)				\
  ((TREE_READONLY (NODE) * TYPE_QUAL_CONST)		\
   | (TREE_THIS_VOLATILE (NODE) * TYPE_QUAL_VOLATILE))

/* These flags are available for each language front end to use internally.  */
#define TYPE_LANG_FLAG_0(NODE) (TYPE_CHECK (NODE)->type.lang_flag_0)
#define TYPE_LANG_FLAG_1(NODE) (TYPE_CHECK (NODE)->type.lang_flag_1)
#define TYPE_LANG_FLAG_2(NODE) (TYPE_CHECK (NODE)->type.lang_flag_2)
#define TYPE_LANG_FLAG_3(NODE) (TYPE_CHECK (NODE)->type.lang_flag_3)
#define TYPE_LANG_FLAG_4(NODE) (TYPE_CHECK (NODE)->type.lang_flag_4)
#define TYPE_LANG_FLAG_5(NODE) (TYPE_CHECK (NODE)->type.lang_flag_5)
#define TYPE_LANG_FLAG_6(NODE) (TYPE_CHECK (NODE)->type.lang_flag_6)

/* If set in an ARRAY_TYPE, indicates a string type (for languages
   that distinguish string from array of char).
   If set in a SET_TYPE, indicates a bitstring type.  */
#define TYPE_STRING_FLAG(NODE) (TYPE_CHECK (NODE)->type.string_flag)

/* If non-NULL, this is an upper bound of the size (in bytes) of an
   object of the given ARRAY_TYPE.  This allows temporaries to be
   allocated.  */
#define TYPE_ARRAY_MAX_SIZE(ARRAY_TYPE) \
  TYPE_MAX_VALUE (ARRAY_TYPE_CHECK (ARRAY_TYPE))

/* For a VECTOR_TYPE, this is the number of sub-parts of the vector.  */
#define TYPE_VECTOR_SUBPARTS(VECTOR_TYPE) \
  GET_MODE_NUNITS (VECTOR_TYPE_CHECK (VECTOR_TYPE)->type.mode)

  /* Indicates that objects of this type must be initialized by calling a
   function when they are created.  */
#define TYPE_NEEDS_CONSTRUCTING(NODE) \
  (TYPE_CHECK (NODE)->type.needs_constructing_flag)

/* Indicates that objects of this type (a UNION_TYPE), should be passed
   the same way that the first union alternative would be passed.  */
#define TYPE_TRANSPARENT_UNION(NODE)  \
  (UNION_TYPE_CHECK (NODE)->type.transparent_union_flag)

/* For an ARRAY_TYPE, indicates that it is not permitted to
   take the address of a component of the type.  */
#define TYPE_NONALIASED_COMPONENT(NODE) \
  (ARRAY_TYPE_CHECK (NODE)->type.transparent_union_flag)

/* Indicated that objects of this type should be laid out in as
   compact a way as possible.  */
#define TYPE_PACKED(NODE) (TYPE_CHECK (NODE)->type.packed_flag)

/* A bounded pointer or bounded reference type (collectively called
   indirect types) is represented as a RECORD_TYPE node containing
   three pointer fields whose type is the corresponding unbounded
   POINTER_TYPE or REFERENCE_TYPE.  A RECORD_TYPE node that represents
   a bounded indirect type differs from a normal RECORD_TYPE node in
   that its TREE_TYPE is non-NULL and has the pointed-to type just as
   a POINTER_TYPE or REFERENCE_TYPE node has.  The bounded RECORD_TYPE
   nodes are stored on the same type variant chain alongside the
   variants of the underlaying indirect types nodes.  The main variant
   of such chains is always the unbounded type.  */

/* Access the field decls of a bounded-pointer type.  */
#define TYPE_BOUNDED_VALUE(TYPE) TYPE_FIELDS (TYPE)
#define TYPE_BOUNDED_BASE(TYPE) TREE_CHAIN (TYPE_BOUNDED_VALUE (TYPE))
#define TYPE_BOUNDED_EXTENT(TYPE) TREE_CHAIN (TYPE_BOUNDED_BASE (TYPE))

/* Access the simple-pointer subtype of a bounded-pointer type.  */
#define TYPE_BOUNDED_SUBTYPE(TYPE) TREE_TYPE (TYPE_BOUNDED_VALUE (TYPE))

/* Find the unbounded counterpart to a type, or return TYPE if it is
   already unbounded.  */
#define TYPE_UNBOUNDED_VARIANT(TYPE) \
  (BOUNDED_POINTER_TYPE_P (TYPE) ? TYPE_BOUNDED_SUBTYPE (TYPE) : (TYPE))

/* This field comprises two bits, for values in the range 0..3:

   depth=0 means that type is a scalar, or an aggregate that contains
   only depth=0 types, or a function that has only depth=0 types for
   its return value and argument types.

   depth=1 means that type is a pointer to a depth=0 type, or an
   aggregate that contains only depth=0 and depth=1 types, or a
   function that has only depth=0 and depth=1 types for its return
   value and argument types.

   The meanings of depth=2 and depth=3 are obvious by induction.
   Varargs functions are depth=3.  The type `va_list' is depth=3.

   The purpose of measuring pointer depth of a type is to determine
   the eligibility of a function for an automatically-generated
   bounded-pointer thunk.  A depth=0 functions needs no thunk.  A
   depth=1 function is eligible for an automatic thunk.  Functions
   with depth 2 or more are too complex to get automatic thunks.

   Function decls also have a pointer_depth field, since we also
   consider the actual argument types for functions.  */

#define TYPE_POINTER_DEPTH(TYPE) (TYPE_CHECK (TYPE)->type.pointer_depth)

/* In a FUNCTION_TYPE node, this bit stores the value of
   default_pointer_boundedness at the time TYPE was created.  It is
   useful for choosing default boundedness of function arguments for
   non-prototype function decls and for varargs/stdarg lists.  */
#define TYPE_AMBIENT_BOUNDEDNESS(TYPE) \
  (FUNCTION_TYPE_CHECK (TYPE)->type.transparent_union_flag)

#define MAX_POINTER_DEPTH 2
#define VA_LIST_POINTER_DEPTH 3

struct tree_type
{
  struct tree_common common;
  tree values;
  tree size;
  tree size_unit;
  tree attributes;
  unsigned int uid;

  unsigned int precision : 9;
  ENUM_BITFIELD(machine_mode) mode : 7;

  unsigned string_flag : 1;
  unsigned no_force_blk_flag : 1;
  unsigned needs_constructing_flag : 1;
  unsigned transparent_union_flag : 1;
  unsigned packed_flag : 1;
  unsigned restrict_flag : 1;
  unsigned pointer_depth : 2;

  unsigned lang_flag_0 : 1;
  unsigned lang_flag_1 : 1;
  unsigned lang_flag_2 : 1;
  unsigned lang_flag_3 : 1;
  unsigned lang_flag_4 : 1;
  unsigned lang_flag_5 : 1;
  unsigned lang_flag_6 : 1;
  unsigned user_align : 1;

  unsigned int align;
  tree pointer_to;
  tree reference_to;
  union {int address; char *pointer; } symtab;
  tree name;
  tree minval;
  tree maxval;
  tree next_variant;
  tree main_variant;
  tree binfo;
  tree context;
  HOST_WIDE_INT alias_set;
  /* Points to a structure whose details depend on the language in use.  */
  struct lang_type *lang_specific;
};

/* Define accessor macros for information about type inheritance
   and basetypes.

   A "basetype" means a particular usage of a data type for inheritance
   in another type.  Each such basetype usage has its own "binfo"
   object to describe it.  The binfo object is a TREE_VEC node.

   Inheritance is represented by the binfo nodes allocated for a
   given type.  For example, given types C and D, such that D is
   inherited by C, 3 binfo nodes will be allocated: one for describing
   the binfo properties of C, similarly one for D, and one for
   describing the binfo properties of D as a base type for C.
   Thus, given a pointer to class C, one can get a pointer to the binfo
   of D acting as a basetype for C by looking at C's binfo's basetypes.  */

/* The actual data type node being inherited in this basetype.  */
#define BINFO_TYPE(NODE) TREE_TYPE (NODE)

/* The offset where this basetype appears in its containing type.
   BINFO_OFFSET slot holds the offset (in bytes)
   from the base of the complete object to the base of the part of the
   object that is allocated on behalf of this `type'.
   This is always 0 except when there is multiple inheritance.  */
   
#define BINFO_OFFSET(NODE) TREE_VEC_ELT ((NODE), 1)
#define TYPE_BINFO_OFFSET(NODE) BINFO_OFFSET (TYPE_BINFO (NODE))
#define BINFO_OFFSET_ZEROP(NODE) (integer_zerop (BINFO_OFFSET (NODE)))

/* The virtual function table belonging to this basetype.  Virtual
   function tables provide a mechanism for run-time method dispatching.
   The entries of a virtual function table are language-dependent.  */

#define BINFO_VTABLE(NODE) TREE_VEC_ELT ((NODE), 2)
#define TYPE_BINFO_VTABLE(NODE) BINFO_VTABLE (TYPE_BINFO (NODE))

/* The virtual functions in the virtual function table.  This is
   a TREE_LIST that is used as an initial approximation for building
   a virtual function table for this basetype.  */
#define BINFO_VIRTUALS(NODE) TREE_VEC_ELT ((NODE), 3)
#define TYPE_BINFO_VIRTUALS(NODE) BINFO_VIRTUALS (TYPE_BINFO (NODE))

/* A vector of binfos for the direct basetypes inherited by this
   basetype.

   If this basetype describes type D as inherited in C, and if the
   basetypes of D are E and F, then this vector contains binfos for
   inheritance of E and F by C.

   ??? This could probably be done by just allocating the
   base types at the end of this TREE_VEC (instead of using
   another TREE_VEC).  This would simplify the calculation
   of how many basetypes a given type had.  */
#define BINFO_BASETYPES(NODE) TREE_VEC_ELT ((NODE), 4)
#define TYPE_BINFO_BASETYPES(NODE) TREE_VEC_ELT (TYPE_BINFO (NODE), 4)

/* The number of basetypes for NODE.  */
#define BINFO_N_BASETYPES(NODE) \
  (BINFO_BASETYPES (NODE) ? TREE_VEC_LENGTH (BINFO_BASETYPES (NODE)) : 0)

/* Accessor macro to get to the Nth basetype of this basetype.  */
#define BINFO_BASETYPE(NODE,N) TREE_VEC_ELT (BINFO_BASETYPES (NODE), (N))
#define TYPE_BINFO_BASETYPE(NODE,N) \
  BINFO_TYPE (TREE_VEC_ELT (BINFO_BASETYPES (TYPE_BINFO (NODE)), (N)))

/* For a BINFO record describing a virtual base class, i.e., one where
   TREE_VIA_VIRTUAL is set, this field assists in locating the virtual
   base.  The actual contents are language-dependent.  Under the old
   ABI, the C++ front-end uses a FIELD_DECL whose contents are a
   pointer to the virtual base; under the new ABI this field is
   instead a INTEGER_CST giving an offset into the vtable where the
   offset to the virtual base can be found.  */
#define BINFO_VPTR_FIELD(NODE) TREE_VEC_ELT (NODE, 5)

/* The size of a base class subobject of this type.  Not all frontends
   currently allocate the space for these fields.  */
#define BINFO_SIZE(NODE) TREE_VEC_ELT (NODE, 6)
#define BINFO_SIZE_UNIT(NODE) TREE_VEC_ELT (NODE, 7)
#define TYPE_BINFO_SIZE(NODE) BINFO_SIZE (TYPE_BINFO (NODE))
#define TYPE_BINFO_SIZE_UNIT(NODE) BINFO_SIZE_UNIT (TYPE_BINFO (NODE))

/* Slot used to build a chain that represents a use of inheritance.
   For example, if X is derived from Y, and Y is derived from Z,
   then this field can be used to link the binfo node for X to
   the binfo node for X's Y to represent the use of inheritance
   from X to Y.  Similarly, this slot of the binfo node for X's Y
   can point to the Z from which Y is inherited (in X's inheritance
   hierarchy).  In this fashion, one can represent and traverse specific
   uses of inheritance using the binfo nodes themselves (instead of
   consing new space pointing to binfo nodes).
   It is up to the language-dependent front-ends to maintain
   this information as necessary.  */
#define BINFO_INHERITANCE_CHAIN(NODE) TREE_VEC_ELT ((NODE), 0)

/* Define fields and accessors for nodes representing declared names.  */

/* Nonzero if DECL represents a decl.  */
#define DECL_P(DECL)	(TREE_CODE_CLASS (TREE_CODE (DECL)) == 'd')

/* This is the name of the object as written by the user.
   It is an IDENTIFIER_NODE.  */
#define DECL_NAME(NODE) (DECL_CHECK (NODE)->decl.name)

/* The name of the object as the assembler will see it (but before any
   translations made by ASM_OUTPUT_LABELREF).  Often this is the same
   as DECL_NAME.  It is an IDENTIFIER_NODE.  */
#define DECL_ASSEMBLER_NAME(NODE)		\
  ((DECL_ASSEMBLER_NAME_SET_P (NODE)		\
    ? (void) 0					\
    : (*lang_set_decl_assembler_name) (NODE)),	\
   DECL_CHECK (NODE)->decl.assembler_name)

/* Returns non-zero if the DECL_ASSEMBLER_NAME for NODE has been set.  If zero,
   the NODE might still have a DECL_ASSEMBLER_NAME -- it just hasn't been set
   yet.  */
#define DECL_ASSEMBLER_NAME_SET_P(NODE) \
  (DECL_CHECK (NODE)->decl.assembler_name != NULL_TREE)

/* Set the DECL_ASSEMBLER_NAME for NODE to NAME.  */
#define SET_DECL_ASSEMBLER_NAME(NODE, NAME) \
  (DECL_CHECK (NODE)->decl.assembler_name = (NAME))

/* Copy the DECL_ASSEMBLER_NAME from DECL1 to DECL2.  Note that if DECL1's
   DECL_ASSEMBLER_NAME has not yet been set, using this macro will not cause
   the DECL_ASSEMBLER_NAME of either DECL to be set.  In other words, the
   semantics of using this macro, are different than saying:
     
     SET_DECL_ASSEMBLER_NAME(DECL2, DECL_ASSEMBLER_NAME (DECL1))

   which will try to set the DECL_ASSEMBLER_NAME for DECL1.  */

#define COPY_DECL_ASSEMBLER_NAME(DECL1, DECL2)				\
  (DECL_ASSEMBLER_NAME_SET_P (DECL1)					\
   ? (void) SET_DECL_ASSEMBLER_NAME (DECL2, 				\
                                     DECL_ASSEMBLER_NAME (DECL1))	\
   : (void) 0)

/* Records the section name in a section attribute.  Used to pass
   the name from decl_attributes to make_function_rtl and make_decl_rtl.  */
#define DECL_SECTION_NAME(NODE) (DECL_CHECK (NODE)->decl.section_name)

/*  For FIELD_DECLs, this is the
    RECORD_TYPE, UNION_TYPE, or QUAL_UNION_TYPE node that the field is
    a member of.  For VAR_DECL, PARM_DECL, FUNCTION_DECL, LABEL_DECL,
    and CONST_DECL nodes, this points to either the FUNCTION_DECL for the
    containing function, the RECORD_TYPE or UNION_TYPE for the containing
    type, or NULL_TREE if the given decl has "file scope".  */
#define DECL_CONTEXT(NODE) (DECL_CHECK (NODE)->decl.context)
#define DECL_FIELD_CONTEXT(NODE) (FIELD_DECL_CHECK (NODE)->decl.context)
/* In a DECL this is the field where attributes are stored.  */
#define DECL_ATTRIBUTES(NODE) (DECL_CHECK (NODE)->decl.attributes)
/* In a FIELD_DECL, this is the field position, counting in bytes, of the
   byte containing the bit closest to the beginning of the structure.  */
#define DECL_FIELD_OFFSET(NODE) (FIELD_DECL_CHECK (NODE)->decl.arguments)
/* In a FIELD_DECL, this is the offset, in bits, of the first bit of the
   field from DECL_FIELD_OFFSET.  */
#define DECL_FIELD_BIT_OFFSET(NODE) (FIELD_DECL_CHECK (NODE)->decl.u2.t)
/* In a FIELD_DECL, this indicates whether the field was a bit-field and
   if so, the type that was originally specified for it.
   TREE_TYPE may have been modified (in finish_struct).  */
#define DECL_BIT_FIELD_TYPE(NODE) (FIELD_DECL_CHECK (NODE)->decl.result)
/* In FUNCTION_DECL, a chain of ..._DECL nodes.
   VAR_DECL and PARM_DECL reserve the arguments slot for language-specific
   uses.  */
#define DECL_ARGUMENTS(NODE) (DECL_CHECK (NODE)->decl.arguments)
/* This field is used to reference anything in decl.result and is meant only
   for use by the garbage collector.  */
#define DECL_RESULT_FLD(NODE) (DECL_CHECK (NODE)->decl.result)
/* In FUNCTION_DECL, holds the decl for the return value.  */
#define DECL_RESULT(NODE) (FUNCTION_DECL_CHECK (NODE)->decl.result)
/* For a TYPE_DECL, holds the "original" type.  (TREE_TYPE has the copy.) */
#define DECL_ORIGINAL_TYPE(NODE) (TYPE_DECL_CHECK (NODE)->decl.result)
/* In PARM_DECL, holds the type as written (perhaps a function or array).  */
#define DECL_ARG_TYPE_AS_WRITTEN(NODE) (PARM_DECL_CHECK (NODE)->decl.result)
/* For a FUNCTION_DECL, holds the tree of BINDINGs.
   For a VAR_DECL, holds the initial value.
   For a PARM_DECL, not used--default
   values for parameters are encoded in the type of the function,
   not in the PARM_DECL slot.  */
#define DECL_INITIAL(NODE) (DECL_CHECK (NODE)->decl.initial)
/* For a PARM_DECL, records the data type used to pass the argument,
   which may be different from the type seen in the program.  */
#define DECL_ARG_TYPE(NODE) (PARM_DECL_CHECK (NODE)->decl.initial)
/* For a FIELD_DECL in a QUAL_UNION_TYPE, records the expression, which
   if nonzero, indicates that the field occupies the type.  */
#define DECL_QUALIFIER(NODE) (FIELD_DECL_CHECK (NODE)->decl.initial)
/* These two fields describe where in the source code the declaration
   was.  If the declaration appears in several places (as for a C
   function that is declared first and then defined later), this
   information should refer to the definition.  */
#define DECL_SOURCE_FILE(NODE) (DECL_CHECK (NODE)->decl.filename)
#define DECL_SOURCE_LINE(NODE) (DECL_CHECK (NODE)->decl.linenum)
/* Holds the size of the datum, in bits, as a tree expression.
   Need not be constant.  */
#define DECL_SIZE(NODE) (DECL_CHECK (NODE)->decl.size)
/* Likewise for the size in bytes.  */
#define DECL_SIZE_UNIT(NODE) (DECL_CHECK (NODE)->decl.size_unit)
/* Holds the alignment required for the datum, in bits.  */
#define DECL_ALIGN(NODE) (DECL_CHECK (NODE)->decl.u1.a.align)
/* The alignment of NODE, in bytes.  */
#define DECL_ALIGN_UNIT(NODE) (DECL_ALIGN (NODE) / BITS_PER_UNIT)
/* For FIELD_DECLs, off_align holds the number of low-order bits of
   DECL_FIELD_OFFSET which are known to be always zero.
   DECL_OFFSET_ALIGN thus returns the alignment that DECL_FIELD_OFFSET
   has.  */
#define DECL_OFFSET_ALIGN(NODE) \
  (((unsigned HOST_WIDE_INT)1) << FIELD_DECL_CHECK (NODE)->decl.u1.a.off_align)
/* Specify that DECL_ALIGN(NODE) is a multiple of X.  */
#define SET_DECL_OFFSET_ALIGN(NODE, X) \
  (FIELD_DECL_CHECK (NODE)->decl.u1.a.off_align	= exact_log2 ((X) & -(X)))
/* 1 if the alignment for this type was requested by "aligned" attribute,
   0 if it is the default for this type.  */
#define DECL_USER_ALIGN(NODE) (DECL_CHECK (NODE)->decl.user_align)
/* Holds the machine mode corresponding to the declaration of a variable or
   field.  Always equal to TYPE_MODE (TREE_TYPE (decl)) except for a
   FIELD_DECL.  */
#define DECL_MODE(NODE) (DECL_CHECK (NODE)->decl.mode)
/* Holds the RTL expression for the value of a variable or function.  If
   PROMOTED_MODE is defined, the mode of this expression may not be same
   as DECL_MODE.  In that case, DECL_MODE contains the mode corresponding
   to the variable's data type, while the mode
   of DECL_RTL is the mode actually used to contain the data.  

   This value can be evaluated lazily for functions, variables with
   static storage duration, and labels.  */
#define DECL_RTL(NODE)					\
  (DECL_CHECK (NODE)->decl.rtl				\
   ? (NODE)->decl.rtl					\
   : (make_decl_rtl (NODE, NULL), (NODE)->decl.rtl))
/* Set the DECL_RTL for NODE to RTL.  */
#define SET_DECL_RTL(NODE, RTL) (DECL_CHECK (NODE)->decl.rtl = (RTL))
/* Returns non-zero if the DECL_RTL for NODE has already been set.  */
#define DECL_RTL_SET_P(NODE)  (DECL_CHECK (NODE)->decl.rtl != NULL)
/* Copy the RTL from NODE1 to NODE2.  If the RTL was not set for
   NODE1, it will not be set for NODE2; this is a lazy copy.  */
#define COPY_DECL_RTL(NODE1, NODE2) \
  (DECL_CHECK (NODE2)->decl.rtl = DECL_CHECK (NODE1)->decl.rtl)
/* The DECL_RTL for NODE, if it is set, or NULL, if it is not set.  */
#define DECL_RTL_IF_SET(NODE) (DECL_RTL_SET_P (NODE) ? DECL_RTL (NODE) : NULL)

/* Holds an INSN_LIST of all of the live ranges in which the variable
   has been moved to a possibly different register.  */
#define DECL_LIVE_RANGE_RTL(NODE) (DECL_CHECK (NODE)->decl.live_range_rtl)

/* For PARM_DECL, holds an RTL for the stack slot or register
   where the data was actually passed.  */
#define DECL_INCOMING_RTL(NODE) (PARM_DECL_CHECK (NODE)->decl.u2.r)

/* For FUNCTION_DECL, if it is inline, holds the saved insn chain.  */
#define DECL_SAVED_INSNS(NODE) (FUNCTION_DECL_CHECK (NODE)->decl.u2.f)

/* For FUNCTION_DECL, if it is built-in,
   this identifies which built-in operation it is.  */
#define DECL_FUNCTION_CODE(NODE) (FUNCTION_DECL_CHECK (NODE)->decl.u1.f)

/* The DECL_VINDEX is used for FUNCTION_DECLS in two different ways.
   Before the struct containing the FUNCTION_DECL is laid out,
   DECL_VINDEX may point to a FUNCTION_DECL in a base class which
   is the FUNCTION_DECL which this FUNCTION_DECL will replace as a virtual
   function.  When the class is laid out, this pointer is changed
   to an INTEGER_CST node which is suitable for use as an index
   into the virtual function table.  */
#define DECL_VINDEX(NODE) (DECL_CHECK (NODE)->decl.vindex)

/* For FIELD_DECLS, DECL_FCONTEXT is the *first* baseclass in
   which this FIELD_DECL is defined.  This information is needed when
   writing debugging information about vfield and vbase decls for C++.  */
#define DECL_FCONTEXT(NODE) (FIELD_DECL_CHECK (NODE)->decl.vindex)

/* Every ..._DECL node gets a unique number.  */
#define DECL_UID(NODE) (DECL_CHECK (NODE)->decl.uid)

/* For any sort of a ..._DECL node, this points to the original (abstract)
   decl node which this decl is an instance of, or else it is NULL indicating
   that this decl is not an instance of some other decl.  For example,
   in a nested declaration of an inline function, this points back to the
   definition.  */
#define DECL_ABSTRACT_ORIGIN(NODE) (DECL_CHECK (NODE)->decl.abstract_origin)

/* Like DECL_ABSTRACT_ORIGIN, but returns NODE if there's no abstract
   origin.  This is useful when setting the DECL_ABSTRACT_ORIGIN.  */
#define DECL_ORIGIN(NODE) \
  (DECL_ABSTRACT_ORIGIN (NODE) ? DECL_ABSTRACT_ORIGIN (NODE) : (NODE))

/* Nonzero for any sort of ..._DECL node means this decl node represents an
   inline instance of some original (abstract) decl from an inline function;
   suppress any warnings about shadowing some other variable.  FUNCTION_DECL
   nodes can also have their abstract origin set to themselves.  */
#define DECL_FROM_INLINE(NODE) (DECL_ABSTRACT_ORIGIN (NODE) != NULL_TREE \
				&& DECL_ABSTRACT_ORIGIN (NODE) != (NODE))

/* Nonzero if a _DECL means that the name of this decl should be ignored
   for symbolic debug purposes.  */
#define DECL_IGNORED_P(NODE) (DECL_CHECK (NODE)->decl.ignored_flag)

/* Nonzero for a given ..._DECL node means that this node represents an
   "abstract instance" of the given declaration (e.g. in the original
   declaration of an inline function).  When generating symbolic debugging
   information, we mustn't try to generate any address information for nodes
   marked as "abstract instances" because we don't actually generate
   any code or allocate any data space for such instances.  */
#define DECL_ABSTRACT(NODE) (DECL_CHECK (NODE)->decl.abstract_flag)

/* Nonzero if a _DECL means that no warnings should be generated just
   because this decl is unused.  */
#define DECL_IN_SYSTEM_HEADER(NODE) \
  (DECL_CHECK (NODE)->decl.in_system_header_flag)

/* Nonzero for a given ..._DECL node means that this node should be
   put in .common, if possible.  If a DECL_INITIAL is given, and it
   is not error_mark_node, then the decl cannot be put in .common.  */
#define DECL_COMMON(NODE) (DECL_CHECK (NODE)->decl.common_flag)

/* Language-specific decl information.  */
#define DECL_LANG_SPECIFIC(NODE) (DECL_CHECK (NODE)->decl.lang_specific)

/* In a VAR_DECL or FUNCTION_DECL,
   nonzero means external reference:
   do not allocate storage, and refer to a definition elsewhere.  */
#define DECL_EXTERNAL(NODE) (DECL_CHECK (NODE)->decl.external_flag)

/* In a VAR_DECL for a RECORD_TYPE, sets number for non-init_priority
   initializatons.  */
#define DEFAULT_INIT_PRIORITY 65535
#define MAX_INIT_PRIORITY 65535
#define MAX_RESERVED_INIT_PRIORITY 100

/* In a TYPE_DECL
   nonzero means the detail info about this type is not dumped into stabs.
   Instead it will generate cross reference ('x') of names. 
   This uses the same flag as DECL_EXTERNAL.  */
#define TYPE_DECL_SUPPRESS_DEBUG(NODE) \
  (TYPE_DECL_CHECK (NODE)->decl.external_flag)

/* In VAR_DECL and PARM_DECL nodes, nonzero means declared `register'.  */
#define DECL_REGISTER(NODE) (DECL_CHECK (NODE)->decl.regdecl_flag)

/* In LABEL_DECL nodes, nonzero means that an error message about
   jumping into such a binding contour has been printed for this label.  */
#define DECL_ERROR_ISSUED(NODE) (LABEL_DECL_CHECK (NODE)->decl.regdecl_flag)

/* In a FIELD_DECL, indicates this field should be bit-packed.  */
#define DECL_PACKED(NODE) (FIELD_DECL_CHECK (NODE)->decl.regdecl_flag)

/* In a FUNCTION_DECL with a non-zero DECL_CONTEXT, indicates that a
   static chain is not needed.  */
#define DECL_NO_STATIC_CHAIN(NODE) \
  (FUNCTION_DECL_CHECK (NODE)->decl.regdecl_flag)

/* Nonzero in a ..._DECL means this variable is ref'd from a nested function.
   For VAR_DECL nodes, PARM_DECL nodes, and FUNCTION_DECL nodes.

   For LABEL_DECL nodes, nonzero if nonlocal gotos to the label are permitted.

   Also set in some languages for variables, etc., outside the normal
   lexical scope, such as class instance variables.  */
#define DECL_NONLOCAL(NODE) (DECL_CHECK (NODE)->decl.nonlocal_flag)

/* Nonzero in a FUNCTION_DECL means this function can be substituted
   where it is called.  */
#define DECL_INLINE(NODE) (FUNCTION_DECL_CHECK (NODE)->decl.inline_flag)

/* In a FUNCTION_DECL, nonzero if the function cannot be inlined.  */
#define DECL_UNINLINABLE(NODE) (FUNCTION_DECL_CHECK (NODE)->decl.uninlinable)

/* In a FUNCTION_DECL, the saved representation of the body of the
   entire function.  Usually a COMPOUND_STMT, but in C++ this may also
   be a RETURN_INIT, CTOR_INITIALIZER, or TRY_BLOCK.  */
#define DECL_SAVED_TREE(NODE) (FUNCTION_DECL_CHECK (NODE)->decl.saved_tree)

/* List of FUNCTION_DECLs inlined into this function's body.  */
#define DECL_INLINED_FNS(NODE) (FUNCTION_DECL_CHECK (NODE)->decl.inlined_fns)

/* Nonzero in a FUNCTION_DECL means this is a built-in function
   that is not specified by ansi C and that users are supposed to be allowed
   to redefine for any purpose whatever.  */
#define DECL_BUILT_IN_NONANSI(NODE) \
  (FUNCTION_DECL_CHECK (NODE)->common.unsigned_flag)

/* Nonzero in a FUNCTION_DECL means this function should be treated
   as if it were a malloc, meaning it returns a pointer that is
   not an alias.  */
#define DECL_IS_MALLOC(NODE) (FUNCTION_DECL_CHECK (NODE)->decl.malloc_flag)

/* Nonzero in a FUNCTION_DECL means this function should be treated
   as "pure" function (like const function, but may read global memory).  */
#define DECL_IS_PURE(NODE) (FUNCTION_DECL_CHECK (NODE)->decl.pure_flag)

/* Nonzero in a FIELD_DECL means it is a bit field, and must be accessed
   specially.  */
#define DECL_BIT_FIELD(NODE) (FIELD_DECL_CHECK (NODE)->decl.bit_field_flag)

/* In a LABEL_DECL, nonzero means label was defined inside a binding
   contour that restored a stack level and which is now exited.  */
#define DECL_TOO_LATE(NODE) (LABEL_DECL_CHECK (NODE)->decl.bit_field_flag)

/* Unused in FUNCTION_DECL.  */

/* In a VAR_DECL that's static,
   nonzero if the space is in the text section.  */
#define DECL_IN_TEXT_SECTION(NODE) (VAR_DECL_CHECK (NODE)->decl.bit_field_flag)

/* In a FUNCTION_DECL, nonzero means a built in function.  */
#define DECL_BUILT_IN(NODE) (DECL_BUILT_IN_CLASS (NODE) != NOT_BUILT_IN)

/* For a builtin function, identify which part of the compiler defined it.  */
#define DECL_BUILT_IN_CLASS(NODE) \
   (FUNCTION_DECL_CHECK (NODE)->decl.built_in_class)

/* Used in VAR_DECLs to indicate that the variable is a vtable.
   Used in FIELD_DECLs for vtable pointers.
   Used in FUNCTION_DECLs to indicate that the function is virtual.  */
#define DECL_VIRTUAL_P(NODE) (DECL_CHECK (NODE)->decl.virtual_flag)

/* Used to indicate that the linkage status of this DECL is not yet known,
   so it should not be output now.  */
#define DECL_DEFER_OUTPUT(NODE) (DECL_CHECK (NODE)->decl.defer_output)

/* Used in PARM_DECLs whose type are unions to indicate that the
   argument should be passed in the same way that the first union
   alternative would be passed.  */
#define DECL_TRANSPARENT_UNION(NODE) \
  (PARM_DECL_CHECK (NODE)->decl.transparent_union)

/* Used in FUNCTION_DECLs to indicate that they should be run automatically
   at the beginning or end of execution.  */
#define DECL_STATIC_CONSTRUCTOR(NODE) \
  (FUNCTION_DECL_CHECK (NODE)->decl.static_ctor_flag)

#define DECL_STATIC_DESTRUCTOR(NODE) \
(FUNCTION_DECL_CHECK (NODE)->decl.static_dtor_flag)

/* Used to indicate that this DECL represents a compiler-generated entity.  */
#define DECL_ARTIFICIAL(NODE) (DECL_CHECK (NODE)->decl.artificial_flag)

/* Used to indicate that this DECL has weak linkage.  */
#define DECL_WEAK(NODE) (DECL_CHECK (NODE)->decl.weak_flag)

/* Used in TREE_PUBLIC decls to indicate that copies of this DECL in
   multiple translation units should be merged.  */
#define DECL_ONE_ONLY(NODE) (DECL_CHECK (NODE)->decl.transparent_union)

/* Used in a DECL to indicate that, even if it TREE_PUBLIC, it need
   not be put out unless it is needed in this translation unit.
   Entities like this are shared across translation units (like weak
   entities), but are guaranteed to be generated by any translation
   unit that needs them, and therefore need not be put out anywhere
   where they are not needed.  DECL_COMDAT is just a hint to the
   back-end; it is up to front-ends which set this flag to ensure
   that there will never be any harm, other than bloat, in putting out
   something which is DECL_COMDAT.  */
#define DECL_COMDAT(NODE) (DECL_CHECK (NODE)->decl.comdat_flag)

/* Used in FUNCTION_DECLs to indicate that function entry and exit should
   be instrumented with calls to support routines.  */
#define DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT(NODE) \
  (FUNCTION_DECL_CHECK (NODE)->decl.no_instrument_function_entry_exit)

/* Used in FUNCTION_DECLs to indicate that limit-stack-* should be
   disabled in this function.  */
#define DECL_NO_LIMIT_STACK(NODE) \
  (FUNCTION_DECL_CHECK (NODE)->decl.no_limit_stack)

/* Additional flags for language-specific uses.  */
#define DECL_LANG_FLAG_0(NODE) (DECL_CHECK (NODE)->decl.lang_flag_0)
#define DECL_LANG_FLAG_1(NODE) (DECL_CHECK (NODE)->decl.lang_flag_1)
#define DECL_LANG_FLAG_2(NODE) (DECL_CHECK (NODE)->decl.lang_flag_2)
#define DECL_LANG_FLAG_3(NODE) (DECL_CHECK (NODE)->decl.lang_flag_3)
#define DECL_LANG_FLAG_4(NODE) (DECL_CHECK (NODE)->decl.lang_flag_4)
#define DECL_LANG_FLAG_5(NODE) (DECL_CHECK (NODE)->decl.lang_flag_5)
#define DECL_LANG_FLAG_6(NODE) (DECL_CHECK (NODE)->decl.lang_flag_6)
#define DECL_LANG_FLAG_7(NODE) (DECL_CHECK (NODE)->decl.lang_flag_7)

/* Used to indicate that the pointer to this DECL cannot be treated as
   an address constant.  */
#define DECL_NON_ADDR_CONST_P(NODE) (DECL_CHECK (NODE)->decl.non_addr_const_p)

/* Used in a FIELD_DECL to indicate that we cannot form the address of
   this component.  */
#define DECL_NONADDRESSABLE_P(NODE) \
  (FIELD_DECL_CHECK (NODE)->decl.non_addressable)

/* Used to indicate an alias set for the memory pointed to by this
   particular FIELD_DECL, PARM_DECL, or VAR_DECL, which must have
   pointer (or reference) type.  */
#define DECL_POINTER_ALIAS_SET(NODE) \
  (DECL_CHECK (NODE)->decl.pointer_alias_set)

/* Nonzero if an alias set has been assigned to this declaration.  */
#define DECL_POINTER_ALIAS_SET_KNOWN_P(NODE) \
  (DECL_POINTER_ALIAS_SET (NODE) != - 1)

/* The pointer_depth field comprises two bits for values in the range
   0..3.  The value is normally equal to TYPE_POINTER_DEPTH of decl's
   type node, but for functions it migth be greater.  For example,
   this can happen when the function is declared to accept a parameter
   of type void* (depth=1), but is actually called with an argument of
   type foo** (depth=2).  The function type will get the formal
   parameter's depth, but the function decl will get the actual
   argument's depth.  */
#define DECL_POINTER_DEPTH(DECL) (DECL_CHECK (DECL)->decl.pointer_depth)

struct function;

struct tree_decl
{
  struct tree_common common;
  const char *filename;
  int linenum;
  unsigned int uid;
  tree size;
  ENUM_BITFIELD(machine_mode) mode : 8;

  unsigned external_flag : 1;
  unsigned nonlocal_flag : 1;
  unsigned regdecl_flag : 1;
  unsigned inline_flag : 1;
  unsigned bit_field_flag : 1;
  unsigned virtual_flag : 1;
  unsigned ignored_flag : 1;
  unsigned abstract_flag : 1;

  unsigned in_system_header_flag : 1;
  unsigned common_flag : 1;
  unsigned defer_output : 1;
  unsigned transparent_union : 1;
  unsigned static_ctor_flag : 1;
  unsigned static_dtor_flag : 1;
  unsigned artificial_flag : 1;
  unsigned weak_flag : 1;

  unsigned non_addr_const_p : 1;
  unsigned no_instrument_function_entry_exit : 1;
  unsigned comdat_flag : 1;
  unsigned malloc_flag : 1;
  unsigned no_limit_stack : 1;
  ENUM_BITFIELD(built_in_class) built_in_class : 2;
  unsigned pure_flag : 1;

  unsigned pointer_depth : 2;
  unsigned non_addressable : 1;
  unsigned user_align : 1;
  unsigned uninlinable : 1;
  /* Three unused bits.  */

  unsigned lang_flag_0 : 1;
  unsigned lang_flag_1 : 1;
  unsigned lang_flag_2 : 1;
  unsigned lang_flag_3 : 1;
  unsigned lang_flag_4 : 1;
  unsigned lang_flag_5 : 1;
  unsigned lang_flag_6 : 1;
  unsigned lang_flag_7 : 1;

  union {
    /* In a FUNCTION_DECL for which DECL_BUILT_IN holds, this is
       DECL_FUNCTION_CODE.  */
    enum built_in_function f;
    /* In a FUNCITON_DECL for which DECL_BUILT_IN does not hold, this
       is used by language-dependent code.  */
    HOST_WIDE_INT i;
    /* DECL_ALIGN and DECL_OFFSET_ALIGN.  (These are not used for
       FUNCTION_DECLs).  */
    struct {unsigned int align : 24; unsigned int off_align : 8;} a;
  } u1;

  tree size_unit;
  tree name;
  tree context;
  tree arguments;	/* Also used for DECL_FIELD_OFFSET */
  tree result;	/* Also used for DECL_BIT_FIELD_TYPE */
  tree initial;	/* Also used for DECL_QUALIFIER */
  tree abstract_origin;
  tree assembler_name;
  tree section_name;
  tree attributes;
  rtx rtl;	/* RTL representation for object.  */
  rtx live_range_rtl;

  /* In FUNCTION_DECL, if it is inline, holds the saved insn chain.
     In FIELD_DECL, is DECL_FIELD_BIT_OFFSET.
     In PARM_DECL, holds an RTL for the stack slot
     of register where the data was actually passed.
     Used by Chill and Java in LABEL_DECL and by C++ and Java in VAR_DECL.  */
  union {
    struct function *f;
    rtx r;
    tree t;
    int i;
  } u2;

  /* In a FUNCTION_DECL, this is DECL_SAVED_TREE.  */
  tree saved_tree;

  /* In a FUNCTION_DECL, these are function data which is to be kept
     as long as FUNCTION_DECL is kept.  */
  tree inlined_fns;

  tree vindex;
  HOST_WIDE_INT pointer_alias_set;
  /* Points to a structure whose details depend on the language in use.  */
  struct lang_decl *lang_specific;
};

/* Define the overall contents of a tree node.
   It may be any of the structures declared above
   for various types of node.  */

union tree_node
{
  struct tree_common common;
  struct tree_int_cst int_cst;
  struct tree_real_cst real_cst;
  struct tree_vector vector;
  struct tree_string string;
  struct tree_complex complex;
  struct tree_identifier identifier;
  struct tree_decl decl;
  struct tree_type type;
  struct tree_list list;
  struct tree_vec vec;
  struct tree_exp exp;
  struct tree_block block;
 };

/* Standard named or nameless data types of the C compiler.  */

enum tree_index
{
  TI_ERROR_MARK,
  TI_INTQI_TYPE,
  TI_INTHI_TYPE,
  TI_INTSI_TYPE,
  TI_INTDI_TYPE,
  TI_INTTI_TYPE,

  TI_UINTQI_TYPE,
  TI_UINTHI_TYPE,
  TI_UINTSI_TYPE,
  TI_UINTDI_TYPE,
  TI_UINTTI_TYPE,
    
  TI_INTEGER_ZERO,
  TI_INTEGER_ONE,
  TI_INTEGER_MINUS_ONE,
  TI_NULL_POINTER,

  TI_SIZE_ZERO,
  TI_SIZE_ONE,

  TI_BITSIZE_ZERO,
  TI_BITSIZE_ONE,
  TI_BITSIZE_UNIT,

  TI_COMPLEX_INTEGER_TYPE,
  TI_COMPLEX_FLOAT_TYPE,
  TI_COMPLEX_DOUBLE_TYPE,
  TI_COMPLEX_LONG_DOUBLE_TYPE,

  TI_FLOAT_TYPE,
  TI_DOUBLE_TYPE,
  TI_LONG_DOUBLE_TYPE,

  TI_VOID_TYPE,
  TI_PTR_TYPE,
  TI_CONST_PTR_TYPE,
  TI_PTRDIFF_TYPE,
  TI_VA_LIST_TYPE,

  TI_VOID_LIST_NODE,

  TI_UV4SF_TYPE,
  TI_UV4SI_TYPE,
  TI_UV8HI_TYPE,
  TI_UV8QI_TYPE,
  TI_UV4HI_TYPE,
  TI_UV2SI_TYPE,
  TI_UV2SF_TYPE,
  TI_UV16QI_TYPE,

  TI_V4SF_TYPE,
  TI_V16SF_TYPE,
  TI_V4SI_TYPE,
  TI_V8HI_TYPE,
  TI_V8QI_TYPE,
  TI_V4HI_TYPE,
  TI_V2SI_TYPE,
  TI_V2SF_TYPE,
  TI_V16QI_TYPE,

  TI_MAIN_IDENTIFIER,

  TI_MAX
};

extern tree global_trees[TI_MAX];

#define error_mark_node			global_trees[TI_ERROR_MARK]
 
#define intQI_type_node			global_trees[TI_INTQI_TYPE]
#define intHI_type_node			global_trees[TI_INTHI_TYPE]
#define intSI_type_node			global_trees[TI_INTSI_TYPE]
#define intDI_type_node			global_trees[TI_INTDI_TYPE]
#define intTI_type_node			global_trees[TI_INTTI_TYPE]

#define unsigned_intQI_type_node	global_trees[TI_UINTQI_TYPE]
#define unsigned_intHI_type_node	global_trees[TI_UINTHI_TYPE]
#define unsigned_intSI_type_node	global_trees[TI_UINTSI_TYPE]
#define unsigned_intDI_type_node	global_trees[TI_UINTDI_TYPE]
#define unsigned_intTI_type_node	global_trees[TI_UINTTI_TYPE]

#define integer_zero_node		global_trees[TI_INTEGER_ZERO]
#define integer_one_node		global_trees[TI_INTEGER_ONE]
#define integer_minus_one_node		global_trees[TI_INTEGER_MINUS_ONE]
#define size_zero_node			global_trees[TI_SIZE_ZERO]
#define size_one_node			global_trees[TI_SIZE_ONE]
#define bitsize_zero_node		global_trees[TI_BITSIZE_ZERO]
#define bitsize_one_node		global_trees[TI_BITSIZE_ONE]
#define bitsize_unit_node		global_trees[TI_BITSIZE_UNIT]

#define null_pointer_node		global_trees[TI_NULL_POINTER]

#define float_type_node			global_trees[TI_FLOAT_TYPE]
#define double_type_node		global_trees[TI_DOUBLE_TYPE]
#define long_double_type_node		global_trees[TI_LONG_DOUBLE_TYPE]

#define complex_integer_type_node	global_trees[TI_COMPLEX_INTEGER_TYPE]
#define complex_float_type_node		global_trees[TI_COMPLEX_FLOAT_TYPE]
#define complex_double_type_node	global_trees[TI_COMPLEX_DOUBLE_TYPE]
#define complex_long_double_type_node	global_trees[TI_COMPLEX_LONG_DOUBLE_TYPE]

#define void_type_node			global_trees[TI_VOID_TYPE]
/* The C type `void *'.  */
#define ptr_type_node			global_trees[TI_PTR_TYPE]
/* The C type `const void *'.  */
#define const_ptr_type_node		global_trees[TI_CONST_PTR_TYPE]
#define ptrdiff_type_node		global_trees[TI_PTRDIFF_TYPE]
#define va_list_type_node		global_trees[TI_VA_LIST_TYPE]

/* The node that should be placed at the end of a parameter list to
   indicate that the function does not take a variable number of
   arguments.  The TREE_VALUE will be void_type_node and there will be
   no TREE_CHAIN.  Language-independent code should not assume
   anything else about this node.  */
#define void_list_node                  global_trees[TI_VOID_LIST_NODE]

#define main_identifier_node		global_trees[TI_MAIN_IDENTIFIER]
#define MAIN_NAME_P(NODE) (IDENTIFIER_NODE_CHECK (NODE) == main_identifier_node)

#define unsigned_V16QI_type_node	global_trees[TI_UV16QI_TYPE]
#define unsigned_V4SI_type_node		global_trees[TI_UV4SI_TYPE]
#define unsigned_V8QI_type_node		global_trees[TI_UV8QI_TYPE]
#define unsigned_V8HI_type_node		global_trees[TI_UV8HI_TYPE]
#define unsigned_V4HI_type_node		global_trees[TI_UV4HI_TYPE]
#define unsigned_V2SI_type_node		global_trees[TI_UV2SI_TYPE]

#define V16QI_type_node			global_trees[TI_V16QI_TYPE]
#define V4SF_type_node			global_trees[TI_V4SF_TYPE]
#define V4SI_type_node			global_trees[TI_V4SI_TYPE]
#define V8QI_type_node			global_trees[TI_V8QI_TYPE]
#define V8HI_type_node			global_trees[TI_V8HI_TYPE]
#define V4HI_type_node			global_trees[TI_V4HI_TYPE]
#define V2SI_type_node			global_trees[TI_V2SI_TYPE]
#define V2SF_type_node			global_trees[TI_V2SF_TYPE]
#define V16SF_type_node			global_trees[TI_V16SF_TYPE]

/* An enumeration of the standard C integer types.  These must be
   ordered so that shorter types appear before longer ones.  */
enum integer_type_kind 
{
  itk_char,
  itk_signed_char,
  itk_unsigned_char,
  itk_short,
  itk_unsigned_short,
  itk_int,
  itk_unsigned_int,
  itk_long,
  itk_unsigned_long,
  itk_long_long,
  itk_unsigned_long_long,
  itk_none
};

typedef enum integer_type_kind integer_type_kind;

/* The standard C integer types.  Use integer_type_kind to index into
   this array.  */
extern tree integer_types[itk_none];

#define char_type_node			integer_types[itk_char]
#define signed_char_type_node		integer_types[itk_signed_char]
#define unsigned_char_type_node		integer_types[itk_unsigned_char]
#define short_integer_type_node		integer_types[itk_short]
#define short_unsigned_type_node	integer_types[itk_unsigned_short]
#define integer_type_node		integer_types[itk_int]
#define unsigned_type_node		integer_types[itk_unsigned_int]
#define long_integer_type_node		integer_types[itk_long]
#define long_unsigned_type_node		integer_types[itk_unsigned_long]
#define long_long_integer_type_node	integer_types[itk_long_long]
#define long_long_unsigned_type_node	integer_types[itk_unsigned_long_long]


/* A pointer-to-function member type looks like:

     struct {
       __P __pfn;
       ptrdiff_t __delta;
     };

   If __pfn is NULL, it is a NULL pointer-to-member-function.

   (Because the vtable is always the first thing in the object, we
   don't need its offset.)  If the function is virtual, then PFN is
   one plus twice the index into the vtable; otherwise, it is just a
   pointer to the function.

   Unfortunately, using the lowest bit of PFN doesn't work in
   architectures that don't impose alignment requirements on function
   addresses, or that use the lowest bit to tell one ISA from another,
   for example.  For such architectures, we use the lowest bit of
   DELTA instead of the lowest bit of the PFN, and DELTA will be
   multiplied by 2.  */

enum ptrmemfunc_vbit_where_t
{
  ptrmemfunc_vbit_in_pfn,
  ptrmemfunc_vbit_in_delta
};

#define NULL_TREE (tree) NULL

/* Approximate positive square root of a host double.  This is for
   statistical reports, not code generation.  */
extern double approx_sqrt		PARAMS ((double));

extern char *permalloc			PARAMS ((int));
extern char *expralloc			PARAMS ((int));

/* Compute the number of bytes occupied by 'node'.  This routine only
   looks at TREE_CODE and, if the code is TREE_VEC, TREE_VEC_LENGTH.  */

extern size_t tree_size			PARAMS ((tree));

/* Lowest level primitive for allocating a node.
   The TREE_CODE is the only argument.  Contents are initialized
   to zero except for a few of the common fields.  */

extern tree make_node			PARAMS ((enum tree_code));
extern tree make_lang_type		PARAMS ((enum tree_code));
extern tree (*make_lang_type_fn)		PARAMS ((enum tree_code));

/* Make a copy of a node, with all the same contents except
   for TREE_PERMANENT.  (The copy is permanent
   iff nodes being made now are permanent.)  */

extern tree copy_node			PARAMS ((tree));

/* Make a copy of a chain of TREE_LIST nodes.  */

extern tree copy_list			PARAMS ((tree));

/* Make a TREE_VEC.  */

extern tree make_tree_vec		PARAMS ((int));

/* Return the (unique) IDENTIFIER_NODE node for a given name.
   The name is supplied as a char *.  */

extern tree get_identifier		PARAMS ((const char *));

/* Identical to get_identifier, except that the length is assumed
   known.  */

extern tree get_identifier_with_length  PARAMS ((const char *, unsigned int));

/* If an identifier with the name TEXT (a null-terminated string) has
   previously been referred to, return that node; otherwise return
   NULL_TREE.  */

extern tree maybe_get_identifier	PARAMS ((const char *));

/* Construct various types of nodes.  */

#define build_int_2(LO, HI)  \
  build_int_2_wide ((unsigned HOST_WIDE_INT) (LO), (HOST_WIDE_INT) (HI))

extern tree build			PARAMS ((enum tree_code, tree, ...));
extern tree build_nt			PARAMS ((enum tree_code, ...));

extern tree build_int_2_wide		PARAMS ((unsigned HOST_WIDE_INT, HOST_WIDE_INT));
extern tree build_vector                PARAMS ((tree, tree));
extern tree build_real			PARAMS ((tree, REAL_VALUE_TYPE));
extern tree build_real_from_int_cst 	PARAMS ((tree, tree));
extern tree build_complex		PARAMS ((tree, tree, tree));
extern tree build_string		PARAMS ((int, const char *));
extern tree build1			PARAMS ((enum tree_code, tree, tree));
extern tree build_tree_list		PARAMS ((tree, tree));
extern tree build_decl			PARAMS ((enum tree_code, tree, tree));
extern tree build_block			PARAMS ((tree, tree, tree, tree, tree));
extern tree build_expr_wfl              PARAMS ((tree, const char *, int, int));

/* Construct various nodes representing data types.  */

extern tree make_signed_type		PARAMS ((int));
extern tree make_unsigned_type		PARAMS ((int));
extern void initialize_sizetypes	PARAMS ((void));
extern void set_sizetype		PARAMS ((tree));
extern tree signed_or_unsigned_type 	PARAMS ((int, tree));
extern void fixup_unsigned_type		PARAMS ((tree));
extern tree build_pointer_type		PARAMS ((tree));
extern tree build_reference_type 	PARAMS ((tree));
extern tree build_type_no_quals 	PARAMS ((tree));
extern tree build_index_type		PARAMS ((tree));
extern tree build_index_2_type		PARAMS ((tree, tree));
extern tree build_array_type		PARAMS ((tree, tree));
extern tree build_function_type		PARAMS ((tree, tree));
extern tree build_method_type		PARAMS ((tree, tree));
extern tree build_offset_type		PARAMS ((tree, tree));
extern tree build_complex_type		PARAMS ((tree));
extern tree array_type_nelts		PARAMS ((tree));

extern tree value_member		PARAMS ((tree, tree));
extern tree purpose_member		PARAMS ((tree, tree));
extern tree binfo_member		PARAMS ((tree, tree));
extern unsigned int attribute_hash_list	PARAMS ((tree));
extern int attribute_list_equal		PARAMS ((tree, tree));
extern int attribute_list_contained	PARAMS ((tree, tree));
extern int tree_int_cst_equal		PARAMS ((tree, tree));
extern int tree_int_cst_lt		PARAMS ((tree, tree));
extern int tree_int_cst_compare         PARAMS ((tree, tree));
extern int host_integerp		PARAMS ((tree, int));
extern HOST_WIDE_INT tree_low_cst	PARAMS ((tree, int));
extern int tree_int_cst_msb		PARAMS ((tree));
extern int tree_int_cst_sgn		PARAMS ((tree));
extern int tree_expr_nonnegative_p	PARAMS ((tree));
extern int rtl_expr_nonnegative_p	PARAMS ((rtx));
extern int index_type_equal		PARAMS ((tree, tree));
extern tree get_inner_array_type	PARAMS ((tree));

/* From expmed.c.  Since rtl.h is included after tree.h, we can't
   put the prototype here.  Rtl.h does declare the prototype if
   tree.h had been included.  */

extern tree make_tree			PARAMS ((tree, rtx));

/* Return a type like TTYPE except that its TYPE_ATTRIBUTES
   is ATTRIBUTE.

   Such modified types already made are recorded so that duplicates
   are not made.  */

extern tree build_type_attribute_variant PARAMS ((tree, tree));
extern tree build_decl_attribute_variant PARAMS ((tree, tree));

/* Structure describing an attribute and a function to handle it.  */
struct attribute_spec
{
  /* The name of the attribute (without any leading or trailing __),
     or NULL to mark the end of a table of attributes.  */
  const char *const name;
  /* The minimum length of the list of arguments of the attribute.  */
  const int min_length;
  /* The maximum length of the list of arguments of the attribute
     (-1 for no maximum).  */
  const int max_length;
  /* Whether this attribute requires a DECL.  If it does, it will be passed
     from types of DECLs, function return types and array element types to
     the DECLs, function types and array types respectively; but when
     applied to a type in any other circumstances, it will be ignored with
     a warning.  (If greater control is desired for a given attribute,
     this should be false, and the flags argument to the handler may be
     used to gain greater control in that case.)  */
  const bool decl_required;
  /* Whether this attribute requires a type.  If it does, it will be passed
     from a DECL to the type of that DECL.  */
  const bool type_required;
  /* Whether this attribute requires a function (or method) type.  If it does,
     it will be passed from a function pointer type to the target type,
     and from a function return type (which is not itself a function
     pointer type) to the function type.  */
  const bool function_type_required;
  /* Function to handle this attribute.  NODE points to the node to which
     the attribute is to be applied.  If a DECL, it should be modified in
     place; if a TYPE, a copy should be created.  NAME is the name of the
     attribute (possibly with leading or trailing __).  ARGS is the TREE_LIST
     of the arguments (which may be NULL).  FLAGS gives further information
     about the context of the attribute.  Afterwards, the attributes will
     be added to the DECL_ATTRIBUTES or TYPE_ATTRIBUTES, as appropriate,
     unless *NO_ADD_ATTRS is set to true (which should be done on error,
     as well as in any other cases when the attributes should not be added
     to the DECL or TYPE).  Depending on FLAGS, any attributes to be
     applied to another type or DECL later may be returned;
     otherwise the return value should be NULL_TREE.  This pointer may be
     NULL if no special handling is required beyond the checks implied
     by the rest of this structure.  */
  tree (*const handler) PARAMS ((tree *node, tree name, tree args,
				 int flags, bool *no_add_attrs));
};

extern const struct attribute_spec default_target_attribute_table[];

/* Flags that may be passed in the third argument of decl_attributes, and
   to handler functions for attributes.  */
enum attribute_flags
{
  /* The type passed in is the type of a DECL, and any attributes that
     should be passed in again to be applied to the DECL rather than the
     type should be returned.  */
  ATTR_FLAG_DECL_NEXT = 1,
  /* The type passed in is a function return type, and any attributes that
     should be passed in again to be applied to the function type rather
     than the return type should be returned.  */
  ATTR_FLAG_FUNCTION_NEXT = 2,
  /* The type passed in is an array element type, and any attributes that
     should be passed in again to be applied to the array type rather
     than the element type should be returned.  */
  ATTR_FLAG_ARRAY_NEXT = 4,
  /* The type passed in is a structure, union or enumeration type being
     created, and should be modified in place.  */
  ATTR_FLAG_TYPE_IN_PLACE = 8,
  /* The attributes are being applied by default to a library function whose
     name indicates known behavior, and should be silently ignored if they
     are not in fact compatible with the function type.  */
  ATTR_FLAG_BUILT_IN = 16
};

/* Default versions of target-overridable functions.  */

extern tree merge_decl_attributes PARAMS ((tree, tree));
extern tree merge_type_attributes PARAMS ((tree, tree));
extern int default_comp_type_attributes PARAMS ((tree, tree));
extern void default_set_default_type_attributes PARAMS ((tree));
extern void default_insert_attributes PARAMS ((tree, tree *));
extern bool default_function_attribute_inlinable_p PARAMS ((tree));
extern bool default_ms_bitfield_layout_p PARAMS ((tree));

/* Split a list of declspecs and attributes into two.  */

extern void split_specs_attrs		PARAMS ((tree, tree *, tree *));

/* Strip attributes from a list of combined specs and attrs.  */

extern tree strip_attrs			PARAMS ((tree));

/* Return 1 if an attribute and its arguments are valid for a decl or type.  */

extern int valid_machine_attribute	PARAMS ((tree, tree, tree, tree));

/* Given a tree node and a string, return non-zero if the tree node is
   a valid attribute name for the string.  */

extern int is_attribute_p		PARAMS ((const char *, tree));

/* Given an attribute name and a list of attributes, return the list element
   of the attribute or NULL_TREE if not found.  */

extern tree lookup_attribute		PARAMS ((const char *, tree));

/* Given two attributes lists, return a list of their union.  */

extern tree merge_attributes		PARAMS ((tree, tree));

#ifdef TARGET_DLLIMPORT_DECL_ATTRIBUTES
/* Given two Windows decl attributes lists, possibly including
   dllimport, return a list of their union .  */
extern tree merge_dllimport_decl_attributes PARAMS ((tree, tree));
#endif

/* Return a version of the TYPE, qualified as indicated by the
   TYPE_QUALS, if one exists.  If no qualified version exists yet,
   return NULL_TREE.  */

extern tree get_qualified_type          PARAMS ((tree, int));

/* Like get_qualified_type, but creates the type if it does not
   exist.  This function never returns NULL_TREE.  */

extern tree build_qualified_type        PARAMS ((tree, int));

/* Like build_qualified_type, but only deals with the `const' and
   `volatile' qualifiers.  This interface is retained for backwards
   compatiblity with the various front-ends; new code should use
   build_qualified_type instead.  */

#define build_type_variant(TYPE, CONST_P, VOLATILE_P)			\
  build_qualified_type ((TYPE),						\
			((CONST_P) ? TYPE_QUAL_CONST : 0)		\
			| ((VOLATILE_P) ? TYPE_QUAL_VOLATILE : 0))

/* Make a copy of a type node.  */

extern tree build_type_copy		PARAMS ((tree));

/* Given a ..._TYPE node, calculate the TYPE_SIZE, TYPE_SIZE_UNIT,
   TYPE_ALIGN and TYPE_MODE fields.  If called more than once on one
   node, does nothing except for the first time.  */

extern void layout_type			PARAMS ((tree));

/* These functions allow a front-end to perform a manual layout of a
   RECORD_TYPE.  (For instance, if the placement of subsequent fields
   depends on the placement of fields so far.)  Begin by calling
   start_record_layout.  Then, call place_field for each of the
   fields.  Then, call finish_record_layout.  See layout_type for the
   default way in which these functions are used.  */

typedef struct record_layout_info_s
{
  /* The RECORD_TYPE that we are laying out.  */
  tree t;
  /* The offset into the record so far, in bytes, not including bits in
     BITPOS.  */
  tree offset;
  /* The last known alignment of SIZE.  */
  unsigned int offset_align;
  /* The bit position within the last OFFSET_ALIGN bits, in bits.  */
  tree bitpos;
  /* The alignment of the record so far, in bits.  */
  unsigned int record_align;
  /* The alignment of the record so far, not including padding, in bits.  */
  unsigned int unpacked_align;
  /* The alignment of the record so far, allowing for the record to be
     padded only at the end, in bits.  */
  unsigned int unpadded_align;
  /* The previous field layed out.  */
  tree prev_field;
  /* The static variables (i.e., class variables, as opposed to
     instance variables) encountered in T.  */
  tree pending_statics;
  int packed_maybe_necessary;
} *record_layout_info;

extern void set_lang_adjust_rli		PARAMS ((void (*) PARAMS
						 ((record_layout_info))));
extern record_layout_info start_record_layout PARAMS ((tree));
extern tree bit_from_pos		PARAMS ((tree, tree));
extern tree byte_from_pos		PARAMS ((tree, tree));
extern void pos_from_byte		PARAMS ((tree *, tree *, unsigned int,
						 tree));
extern void pos_from_bit		PARAMS ((tree *, tree *, unsigned int,
						 tree));
extern void normalize_offset		PARAMS ((tree *, tree *,
						 unsigned int));
extern tree rli_size_unit_so_far	PARAMS ((record_layout_info));
extern tree rli_size_so_far		PARAMS ((record_layout_info));
extern void normalize_rli		PARAMS ((record_layout_info));
extern void place_field			PARAMS ((record_layout_info, tree));
extern void compute_record_mode		PARAMS ((tree));
extern void finish_record_layout	PARAMS ((record_layout_info));

/* Given a hashcode and a ..._TYPE node (for which the hashcode was made),
   return a canonicalized ..._TYPE node, so that duplicates are not made.
   How the hash code is computed is up to the caller, as long as any two
   callers that could hash identical-looking type nodes agree.  */

extern tree type_hash_canon		PARAMS ((unsigned int, tree));

/* Given a VAR_DECL, PARM_DECL, RESULT_DECL or FIELD_DECL node,
   calculates the DECL_SIZE, DECL_SIZE_UNIT, DECL_ALIGN and DECL_MODE
   fields.  Call this only once for any given decl node.

   Second argument is the boundary that this field can be assumed to
   be starting at (in bits).  Zero means it can be assumed aligned
   on any boundary that may be needed.  */

extern void layout_decl			PARAMS ((tree, unsigned));

/* Return the mode for data of a given size SIZE and mode class CLASS.
   If LIMIT is nonzero, then don't use modes bigger than MAX_FIXED_MODE_SIZE.
   The value is BLKmode if no other mode is found.  This is like
   mode_for_size, but is passed a tree.  */

extern enum machine_mode mode_for_size_tree PARAMS ((tree, enum mode_class,
						     int));

/* Return an expr equal to X but certainly not valid as an lvalue.  */

extern tree non_lvalue			PARAMS ((tree));
extern tree pedantic_non_lvalue		PARAMS ((tree));

extern tree convert			PARAMS ((tree, tree));
extern unsigned int expr_align		PARAMS ((tree));
extern tree size_in_bytes		PARAMS ((tree));
extern HOST_WIDE_INT int_size_in_bytes	PARAMS ((tree));
extern tree bit_position		PARAMS ((tree));
extern HOST_WIDE_INT int_bit_position	PARAMS ((tree));
extern tree byte_position		PARAMS ((tree));
extern HOST_WIDE_INT int_byte_position	PARAMS ((tree));

/* Define data structures, macros, and functions for handling sizes
   and the various types used to represent sizes.  */

enum size_type_kind
{
  SIZETYPE,		/* Normal representation of sizes in bytes.  */
  SSIZETYPE,		/* Signed representation of sizes in bytes.  */
  USIZETYPE,		/* Unsigned representation of sizes in bytes.  */
  BITSIZETYPE,		/* Normal representation of sizes in bits.  */
  SBITSIZETYPE,		/* Signed representation of sizes in bits.  */
  UBITSIZETYPE,	        /* Unsifgned representation of sizes in bits.  */
  TYPE_KIND_LAST};

extern tree sizetype_tab[(int) TYPE_KIND_LAST];

#define sizetype sizetype_tab[(int) SIZETYPE]
#define bitsizetype sizetype_tab[(int) BITSIZETYPE]
#define ssizetype sizetype_tab[(int) SSIZETYPE]
#define usizetype sizetype_tab[(int) USIZETYPE]
#define sbitsizetype sizetype_tab[(int) SBITSIZETYPE]
#define ubitsizetype sizetype_tab[(int) UBITSIZETYPE]

extern tree size_binop			PARAMS ((enum tree_code, tree, tree));
extern tree size_diffop			PARAMS ((tree, tree));
extern tree size_int_wide		PARAMS ((HOST_WIDE_INT,
						 enum size_type_kind));
extern tree size_int_type_wide		PARAMS ((HOST_WIDE_INT, tree));

#define size_int_type(L, T) size_int_type_wide ((HOST_WIDE_INT) (L), T)
#define size_int(L) size_int_wide ((HOST_WIDE_INT) (L), SIZETYPE)
#define ssize_int(L) size_int_wide ((HOST_WIDE_INT) (L), SSIZETYPE)
#define bitsize_int(L) size_int_wide ((HOST_WIDE_INT) (L), BITSIZETYPE)
#define sbitsize_int(L) size_int_wide ((HOST_WIDE_INT) (L), SBITSIZETYPE)

extern tree round_up			PARAMS ((tree, int));
extern tree round_down			PARAMS ((tree, int));
extern tree get_pending_sizes		PARAMS ((void));
extern int is_pending_size		PARAMS ((tree));
extern void put_pending_size		PARAMS ((tree));
extern void put_pending_sizes		PARAMS ((tree));

/* Type for sizes of data-type.  */

#define BITS_PER_UNIT_LOG \
  ((BITS_PER_UNIT > 1) + (BITS_PER_UNIT > 2) + (BITS_PER_UNIT > 4) \
   + (BITS_PER_UNIT > 8) + (BITS_PER_UNIT > 16) + (BITS_PER_UNIT > 32) \
   + (BITS_PER_UNIT > 64) + (BITS_PER_UNIT > 128) + (BITS_PER_UNIT > 256))

/* If nonzero, an upper limit on alignment of structure fields, in bits.  */
extern unsigned int maximum_field_alignment;

/* If non-zero, the alignment of a bitstring or (power-)set value, in bits.  */
extern unsigned int set_alignment;

/* Concatenate two lists (chains of TREE_LIST nodes) X and Y
   by making the last node in X point to Y.
   Returns X, except if X is 0 returns Y.  */

extern tree chainon			PARAMS ((tree, tree));

/* Make a new TREE_LIST node from specified PURPOSE, VALUE and CHAIN.  */

extern tree tree_cons			PARAMS ((tree, tree, tree));

/* Return the last tree node in a chain.  */

extern tree tree_last			PARAMS ((tree));

/* Reverse the order of elements in a chain, and return the new head.  */

extern tree nreverse			PARAMS ((tree));

/* Returns the length of a chain of nodes
   (number of chain pointers to follow before reaching a null pointer).  */

extern int list_length			PARAMS ((tree));

/* Returns the number of FIELD_DECLs in a type.  */

extern int fields_length		PARAMS ((tree));

/* integer_zerop (tree x) is nonzero if X is an integer constant of value 0 */

extern int integer_zerop		PARAMS ((tree));

/* integer_onep (tree x) is nonzero if X is an integer constant of value 1 */

extern int integer_onep			PARAMS ((tree));

/* integer_all_onesp (tree x) is nonzero if X is an integer constant
   all of whose significant bits are 1.  */

extern int integer_all_onesp		PARAMS ((tree));

/* integer_pow2p (tree x) is nonzero is X is an integer constant with
   exactly one bit 1.  */

extern int integer_pow2p		PARAMS ((tree));

/* staticp (tree x) is nonzero if X is a reference to data allocated
   at a fixed address in memory.  */

extern int staticp			PARAMS ((tree));

/* Gets an error if argument X is not an lvalue.
   Also returns 1 if X is an lvalue, 0 if not.  */

extern int lvalue_or_else		PARAMS ((tree, const char *));

/* save_expr (EXP) returns an expression equivalent to EXP
   but it can be used multiple times within context CTX
   and only evaluate EXP once.  */

extern tree save_expr			PARAMS ((tree));

/* Returns the index of the first non-tree operand for CODE, or the number
   of operands if all are trees.  */

extern int first_rtl_op			PARAMS ((enum tree_code));

/* unsave_expr (EXP) returns an expression equivalent to EXP but it
   can be used multiple times and will evaluate EXP in its entirety
   each time.  */

extern tree unsave_expr			PARAMS ((tree));

/* Reset EXP in place so that it can be expaned again.  Does not
   recurse into subtrees.  */

extern void unsave_expr_1               PARAMS ((tree));

/* Like unsave_expr_1, but recurses into all subtrees.  */

extern tree unsave_expr_now		PARAMS ((tree));

/* If non-null, these are language-specific helper functions for
   unsave_expr_now.  If present, LANG_UNSAVE is called before its
   argument (an UNSAVE_EXPR) is to be unsaved, and all other
   processing in unsave_expr_now is aborted.  LANG_UNSAVE_EXPR_NOW is
   called from unsave_expr_1 for language-specific tree codes.  */
extern void (*lang_unsave)              PARAMS ((tree *));
extern void (*lang_unsave_expr_now)     PARAMS ((tree));

/* Return 0 if it is safe to evaluate EXPR multiple times,
   return 1 if it is safe if EXPR is unsaved afterward, or
   return 2 if it is completely unsafe.  */
extern int unsafe_for_reeval		PARAMS ((tree));

/* If non-null, these are language-specific helper functions for
   unsafe_for_reeval.  Return negative to not handle some tree.  */
extern int (*lang_unsafe_for_reeval)	PARAMS ((tree));

/* Return 1 if EXP contains a PLACEHOLDER_EXPR; i.e., if it represents a size
   or offset that depends on a field within a record.

   Note that we only allow such expressions within simple arithmetic
   or a COND_EXPR.  */

extern int contains_placeholder_p	PARAMS ((tree));

/* Return 1 if EXP contains any expressions that produce cleanups for an
   outer scope to deal with.  Used by fold.  */

extern int has_cleanups			PARAMS ((tree));

/* Given a tree EXP, a FIELD_DECL F, and a replacement value R,
   return a tree with all occurrences of references to F in a
   PLACEHOLDER_EXPR replaced by R.   Note that we assume here that EXP
   contains only arithmetic expressions.  */

extern tree substitute_in_expr		PARAMS ((tree, tree, tree));

/* variable_size (EXP) is like save_expr (EXP) except that it
   is for the special case of something that is part of a
   variable size for a data type.  It makes special arrangements
   to compute the value at the right time when the data type
   belongs to a function parameter.  */

extern tree variable_size		PARAMS ((tree));

/* stabilize_reference (EXP) returns an reference equivalent to EXP
   but it can be used multiple times
   and only evaluate the subexpressions once.  */

extern tree stabilize_reference		PARAMS ((tree));

/* Subroutine of stabilize_reference; this is called for subtrees of
   references.  Any expression with side-effects must be put in a SAVE_EXPR
   to ensure that it is only evaluated once.  */

extern tree stabilize_reference_1	PARAMS ((tree));

/* Return EXP, stripped of any conversions to wider types
   in such a way that the result of converting to type FOR_TYPE
   is the same as if EXP were converted to FOR_TYPE.
   If FOR_TYPE is 0, it signifies EXP's type.  */

extern tree get_unwidened		PARAMS ((tree, tree));

/* Return OP or a simpler expression for a narrower value
   which can be sign-extended or zero-extended to give back OP.
   Store in *UNSIGNEDP_PTR either 1 if the value should be zero-extended
   or 0 if the value should be sign-extended.  */

extern tree get_narrower		PARAMS ((tree, int *));

/* Given MODE and UNSIGNEDP, return a suitable type-tree
   with that mode.
   The definition of this resides in language-specific code
   as the repertoire of available types may vary.  */

extern tree type_for_mode		PARAMS ((enum machine_mode, int));

/* Given PRECISION and UNSIGNEDP, return a suitable type-tree
   for an integer type with at least that precision.
   The definition of this resides in language-specific code
   as the repertoire of available types may vary.  */

extern tree type_for_size		PARAMS ((unsigned, int));

/* Given an integer type T, return a type like T but unsigned.
   If T is unsigned, the value is T.
   The definition of this resides in language-specific code
   as the repertoire of available types may vary.  */

extern tree unsigned_type		PARAMS ((tree));

/* Given an integer type T, return a type like T but signed.
   If T is signed, the value is T.
   The definition of this resides in language-specific code
   as the repertoire of available types may vary.  */

extern tree signed_type			PARAMS ((tree));

/* This function must be defined in the language-specific files.
   expand_expr calls it to build the cleanup-expression for a TARGET_EXPR.
   This is defined in a language-specific file.  */

extern tree maybe_build_cleanup		PARAMS ((tree));

/* Given an expression EXP that may be a COMPONENT_REF or an ARRAY_REF,
   look for nested component-refs or array-refs at constant positions
   and find the ultimate containing object, which is returned.  */

extern tree get_inner_reference		PARAMS ((tree, HOST_WIDE_INT *,
						 HOST_WIDE_INT *, tree *,
						 enum machine_mode *, int *,
						 int *));

/* Return 1 if T is an expression that get_inner_reference handles.  */

extern int handled_component_p		PARAMS ((tree));

/* Given a DECL or TYPE, return the scope in which it was declared, or
   NUL_TREE if there is no containing scope.  */

extern tree get_containing_scope        PARAMS ((tree));

/* Return the FUNCTION_DECL which provides this _DECL with its context,
   or zero if none.  */
extern tree decl_function_context 	PARAMS ((tree));

/* Return the RECORD_TYPE, UNION_TYPE, or QUAL_UNION_TYPE which provides
   this _DECL with its context, or zero if none.  */
extern tree decl_type_context		PARAMS ((tree));

/* Given the FUNCTION_DECL for the current function,
   return zero if it is ok for this function to be inline.
   Otherwise return a warning message with a single %s
   for the function's name.  */

extern const char *function_cannot_inline_p 	PARAMS ((tree));

/* Return 1 if EXPR is the real constant zero.  */
extern int real_zerop PARAMS ((tree));

/* Declare commonly used variables for tree structure.  */

/* Points to the name of the input file from which the current input
   being parsed originally came (before it went into cpp).  */
extern const char *input_filename;

/* Current line number in input file.  */
extern int lineno;

/* Nonzero means lvalues are limited to those valid in pedantic ANSI C.
   Zero means allow extended lvalues.  */

extern int pedantic_lvalues;

/* Nonzero means can safely call expand_expr now;
   otherwise layout_type puts variable sizes onto `pending_sizes' instead.  */

extern int immediate_size_expand;

/* Points to the FUNCTION_DECL of the function whose body we are reading.  */

extern tree current_function_decl;

/* Nonzero means a FUNC_BEGIN label was emitted.  */
extern tree current_function_func_begin_label;

/* Nonzero means all ..._TYPE nodes should be allocated permanently.  */

extern int all_types_permanent;

/* Pointer to function to compute the name to use to print a declaration.
   DECL is the declaration in question.
   VERBOSITY determines what information will be printed:
     0: DECL_NAME, demangled as necessary.
     1: and scope information.
     2: and any other information that might be interesting, such as function
        parameter types in C++.  */

extern const char *(*decl_printable_name)	PARAMS ((tree, int));

/* Pointer to function to finish handling an incomplete decl at the
   end of compilation.  */

extern void (*incomplete_decl_finalize_hook)	PARAMS ((tree));

/* Declare a predefined function.  Return the declaration.  This function is
   provided by each language frontend.  */
extern tree builtin_function			PARAMS ((const char *, tree, int,
						       enum built_in_class,
						       const char *));

/* In tree.c */
extern char *perm_calloc			PARAMS ((int, long));
extern void clean_symbol_name			PARAMS ((char *));
extern tree get_file_function_name_long 	PARAMS ((const char *));
extern tree get_set_constructor_bits		PARAMS ((tree, char *, int));
extern tree get_set_constructor_bytes		PARAMS ((tree,
						       unsigned char *, int));
extern tree get_callee_fndecl                   PARAMS ((tree));
extern void set_decl_assembler_name             PARAMS ((tree));
extern int type_num_arguments                   PARAMS ((tree));

/* In stmt.c */

extern int in_control_zone_p			PARAMS ((void));
extern void expand_fixups			PARAMS ((rtx));
extern tree expand_start_stmt_expr		PARAMS ((int));
extern tree expand_end_stmt_expr		PARAMS ((tree));
extern void expand_expr_stmt			PARAMS ((tree));
extern void expand_expr_stmt_value		PARAMS ((tree, int, int));
extern int warn_if_unused_value			PARAMS ((tree));
extern void expand_decl_init			PARAMS ((tree));
extern void clear_last_expr			PARAMS ((void));
extern void expand_label			PARAMS ((tree));
extern void expand_goto				PARAMS ((tree));
extern void expand_asm				PARAMS ((tree));
extern void expand_start_cond			PARAMS ((tree, int));
extern void expand_end_cond			PARAMS ((void));
extern void expand_start_else			PARAMS ((void));
extern void expand_start_elseif			PARAMS ((tree));
extern struct nesting *expand_start_loop 	PARAMS ((int));
extern struct nesting *expand_start_loop_continue_elsewhere 	PARAMS ((int));
extern struct nesting *expand_start_null_loop 	PARAMS ((void));
extern void expand_loop_continue_here		PARAMS ((void));
extern void expand_end_loop			PARAMS ((void));
extern void expand_end_null_loop		PARAMS ((void));
extern int expand_continue_loop			PARAMS ((struct nesting *));
extern int expand_exit_loop			PARAMS ((struct nesting *));
extern int expand_exit_loop_if_false		PARAMS ((struct nesting *,
						         tree));
extern int expand_exit_loop_top_cond		PARAMS ((struct nesting *,
							 tree));
extern int expand_exit_something		PARAMS ((void));

extern void expand_return			PARAMS ((tree));
extern int optimize_tail_recursion		PARAMS ((tree, rtx));
extern void expand_start_bindings_and_block     PARAMS ((int, tree));
#define expand_start_bindings(flags) \
  expand_start_bindings_and_block(flags, NULL_TREE)
extern void expand_end_bindings			PARAMS ((tree, int, int));
extern void warn_about_unused_variables         PARAMS ((tree));
extern void start_cleanup_deferral		PARAMS ((void));
extern void end_cleanup_deferral		PARAMS ((void));
extern int is_body_block			PARAMS ((tree));

extern int conditional_context			PARAMS ((void));
extern struct nesting * current_nesting_level	PARAMS ((void));
extern tree last_cleanup_this_contour		PARAMS ((void));
extern void expand_start_case			PARAMS ((int, tree, tree,
						       const char *));
extern void expand_end_case_type		PARAMS ((tree, tree));
#define expand_end_case(cond) expand_end_case_type (cond, NULL)
extern int add_case_node                        PARAMS ((tree, tree,
							 tree, tree *));
extern int pushcase				PARAMS ((tree,
						       tree (*) (tree, tree),
						       tree, tree *));
extern int pushcase_range			PARAMS ((tree, tree,
						       tree (*) (tree, tree),
						       tree, tree *));
extern void using_eh_for_cleanups		PARAMS ((void));
extern int stmt_loop_nest_empty			PARAMS ((void));

/* In fold-const.c */

/* Fold constants as much as possible in an expression.
   Returns the simplified expression.
   Acts only on the top level of the expression;
   if the argument itself cannot be simplified, its
   subexpressions are not changed.  */

extern tree fold		PARAMS ((tree));

extern int force_fit_type	PARAMS ((tree, int));
extern int add_double		PARAMS ((unsigned HOST_WIDE_INT, HOST_WIDE_INT,
					 unsigned HOST_WIDE_INT, HOST_WIDE_INT,
					 unsigned HOST_WIDE_INT *,
					 HOST_WIDE_INT *));
extern int neg_double		PARAMS ((unsigned HOST_WIDE_INT, HOST_WIDE_INT,
					 unsigned HOST_WIDE_INT *,
					 HOST_WIDE_INT *));
extern int mul_double		PARAMS ((unsigned HOST_WIDE_INT,
					 HOST_WIDE_INT,
					 unsigned HOST_WIDE_INT, HOST_WIDE_INT,
					 unsigned HOST_WIDE_INT *,
					 HOST_WIDE_INT *));
extern void lshift_double	PARAMS ((unsigned HOST_WIDE_INT, HOST_WIDE_INT,
					 HOST_WIDE_INT, unsigned int,
					 unsigned HOST_WIDE_INT *,
					 HOST_WIDE_INT *, int));
extern void rshift_double	PARAMS ((unsigned HOST_WIDE_INT, HOST_WIDE_INT,
					 HOST_WIDE_INT, unsigned int,
					 unsigned HOST_WIDE_INT *,
					 HOST_WIDE_INT *, int));
extern void lrotate_double	PARAMS ((unsigned HOST_WIDE_INT, HOST_WIDE_INT,
					 HOST_WIDE_INT, unsigned int,
					 unsigned HOST_WIDE_INT *,
					 HOST_WIDE_INT *));
extern void rrotate_double	PARAMS ((unsigned HOST_WIDE_INT, HOST_WIDE_INT,
					 HOST_WIDE_INT, unsigned int,
					 unsigned HOST_WIDE_INT *,
					 HOST_WIDE_INT *));
extern int operand_equal_p	PARAMS ((tree, tree, int));
extern tree invert_truthvalue	PARAMS ((tree));

/* In builtins.c.  Given a type, apply default promotions wrt unnamed
   function arguments and return the new type.  Return NULL_TREE if no
   change.  Required by any language that supports variadic arguments.  */

extern tree (*lang_type_promotes_to)	PARAMS ((tree));
extern tree fold_builtin		PARAMS ((tree));

/* The language front-end must define these functions.  */

/* Function to replace the DECL_LANG_SPECIFIC field of a DECL with a copy.  */
extern void copy_lang_decl			PARAMS ((tree));

/* Function called with no arguments to parse and compile the input.  */
extern int yyparse				PARAMS ((void));
/* Functions for processing symbol declarations.  */
/* Function to enter a new lexical scope.
   Takes one argument: always zero when called from outside the front end.  */
extern void pushlevel				PARAMS ((int));
/* Function to exit a lexical scope.  It returns a BINDING for that scope.
   Takes three arguments:
     KEEP -- nonzero if there were declarations in this scope.
     REVERSE -- reverse the order of decls before returning them.
     FUNCTIONBODY -- nonzero if this level is the body of a function.  */
extern tree poplevel				PARAMS ((int, int, int));
/* Set the BLOCK node for the current scope level.  */
extern void set_block				PARAMS ((tree));
/* Function to add a decl to the current scope level.
   Takes one argument, a decl to add.
   Returns that decl, or, if the same symbol is already declared, may
   return a different decl for that name.  */
extern tree pushdecl				PARAMS ((tree));
/* Function to return the chain of decls so far in the current scope level.  */
extern tree getdecls				PARAMS ((void));
/* Function to return the chain of structure tags in the current scope level.  */
extern tree gettags				PARAMS ((void));

extern tree build_range_type PARAMS ((tree, tree, tree));

/* In alias.c */
extern void record_component_aliases		PARAMS ((tree));
extern HOST_WIDE_INT get_alias_set		PARAMS ((tree));
extern int alias_sets_conflict_p		PARAMS ((HOST_WIDE_INT,
							 HOST_WIDE_INT));
extern int readonly_fields_p			PARAMS ((tree));
extern int objects_must_conflict_p		PARAMS ((tree, tree));

/* Set the DECL_ASSEMBLER_NAME for a node.  If it is the sort of thing
   that the assembler should talk about, set DECL_ASSEMBLER_NAME to an
   appropriate IDENTIFIER_NODE.  Otherwise, set it to the
   ERROR_MARK_NODE to ensure that the assembler does not talk about
   it.  */
extern void (*lang_set_decl_assembler_name)     PARAMS ((tree));

struct obstack;

/* In tree.c */
extern int really_constant_p		PARAMS ((tree));
extern int int_fits_type_p		PARAMS ((tree, tree));
extern int tree_log2			PARAMS ((tree));
extern int tree_floor_log2		PARAMS ((tree));
extern void preserve_data		PARAMS ((void));
extern int object_permanent_p		PARAMS ((tree));
extern int type_precision		PARAMS ((tree));
extern int simple_cst_equal		PARAMS ((tree, tree));
extern int compare_tree_int		PARAMS ((tree,
						 unsigned HOST_WIDE_INT));
extern int type_list_equal		PARAMS ((tree, tree));
extern int chain_member			PARAMS ((tree, tree));
extern int chain_member_purpose		PARAMS ((tree, tree));
extern int chain_member_value		PARAMS ((tree, tree));
extern tree listify			PARAMS ((tree));
extern tree type_hash_lookup		PARAMS ((unsigned int, tree));
extern void type_hash_add		PARAMS ((unsigned int, tree));
extern unsigned int type_hash_list	PARAMS ((tree));
extern int simple_cst_list_equal	PARAMS ((tree, tree));
extern void dump_tree_statistics	PARAMS ((void));
extern void print_obstack_statistics	PARAMS ((const char *,
						struct obstack *));
#ifdef BUFSIZ
extern void print_obstack_name		PARAMS ((char *, FILE *,
						 const char *));
#endif
extern void expand_function_end		PARAMS ((const char *, int, int));
extern void expand_function_start	PARAMS ((tree, int));
extern void expand_pending_sizes        PARAMS ((tree));

extern int real_onep			PARAMS ((tree));
extern int real_twop			PARAMS ((tree));
extern void gcc_obstack_init		PARAMS ((struct obstack *));
extern void init_obstacks		PARAMS ((void));
extern void build_common_tree_nodes	PARAMS ((int));
extern void build_common_tree_nodes_2	PARAMS ((int));
extern void mark_tree_hashtable         PARAMS ((void *));

/* In function.c */
extern void setjmp_protect_args		PARAMS ((void));
extern void setjmp_protect		PARAMS ((tree));
extern void expand_main_function	PARAMS ((void));
extern void mark_varargs		PARAMS ((void));
extern void init_dummy_function_start	PARAMS ((void));
extern void expand_dummy_function_end	PARAMS ((void));
extern void init_function_for_compilation	PARAMS ((void));
extern void init_function_start		PARAMS ((tree, const char *, int));
extern void assign_parms		PARAMS ((tree));
extern void put_var_into_stack		PARAMS ((tree));
extern void flush_addressof		PARAMS ((tree));
extern void uninitialized_vars_warning	PARAMS ((tree));
extern void setjmp_args_warning		PARAMS ((void));
extern void mark_all_temps_used		PARAMS ((void));
extern void init_temp_slots		PARAMS ((void));
extern void combine_temp_slots		PARAMS ((void));
extern void free_temp_slots		PARAMS ((void));
extern void pop_temp_slots		PARAMS ((void));
extern void push_temp_slots		PARAMS ((void));
extern void preserve_temp_slots		PARAMS ((rtx));
extern void preserve_rtl_expr_temps	PARAMS ((tree));
extern int aggregate_value_p		PARAMS ((tree));
extern void free_temps_for_rtl_expr	PARAMS ((tree));
extern void instantiate_virtual_regs	PARAMS ((tree, rtx));
extern void unshare_all_rtl		PARAMS ((tree, rtx));
extern int max_parm_reg_num		PARAMS ((void));
extern void push_function_context	PARAMS ((void));
extern void pop_function_context	PARAMS ((void));
extern void push_function_context_to	PARAMS ((tree));
extern void pop_function_context_from	PARAMS ((tree));
extern void ggc_mark_struct_function	PARAMS ((struct function *));

/* In print-rtl.c */
#ifdef BUFSIZ
extern void print_rtl			PARAMS ((FILE *, rtx));
#endif

/* In print-tree.c */
extern void debug_tree			PARAMS ((tree));
#ifdef BUFSIZ
extern void print_node			PARAMS ((FILE *, const char *, tree,
						 int));
extern void print_node_brief		PARAMS ((FILE *, const char *, tree,
						 int));
extern void indent_to			PARAMS ((FILE *, int));
#endif

/* In expr.c */
extern int apply_args_register_offset		PARAMS ((int));
extern rtx expand_builtin_return_addr
	PARAMS ((enum built_in_function, int, rtx));
extern void check_max_integer_computation_mode	PARAMS ((tree));

/* In emit-rtl.c */
extern void start_sequence_for_rtl_expr		PARAMS ((tree));
extern rtx emit_line_note		PARAMS ((const char *, int));

/* In calls.c */

extern int setjmp_call_p		PARAMS ((tree));

/* In attribs.c.  */

/* Process the attributes listed in ATTRIBUTES and install them in *NODE,
   which is either a DECL (including a TYPE_DECL) or a TYPE.  If a DECL,
   it should be modified in place; if a TYPE, a copy should be created
   unless ATTR_FLAG_TYPE_IN_PLACE is set in FLAGS.  FLAGS gives further
   information, in the form of a bitwise OR of flags in enum attribute_flags
   from tree.h.  Depending on these flags, some attributes may be
   returned to be applied at a later stage (for example, to apply
   a decl attribute to the declaration rather than to its type).  */
extern tree decl_attributes		PARAMS ((tree *, tree, int));

/* The following function must be provided by front ends
   using attribs.c.  */

/* Possibly apply default attributes to a function (represented by
   a FUNCTION_DECL).  */
extern void insert_default_attributes PARAMS ((tree));

/* Table of machine-independent attributes for checking formats, if used.  */
extern const struct attribute_spec *format_attribute_table;

/* Table of machine-independent attributes for a particular language.  */
extern const struct attribute_spec *lang_attribute_table;

/* Flag saying whether common language attributes are to be supported.  */
extern int lang_attribute_common;

/* In front end.  */

extern int mark_addressable		PARAMS ((tree));
extern void incomplete_type_error	PARAMS ((tree, tree));
extern tree truthvalue_conversion	PARAMS ((tree));
extern int global_bindings_p		PARAMS ((void));
extern void insert_block		PARAMS ((tree));

/* In integrate.c */
extern void save_for_inline		PARAMS ((tree));
extern void set_decl_abstract_flags	PARAMS ((tree, int));
extern void output_inline_function	PARAMS ((tree));
extern void set_decl_origin_self	PARAMS ((tree));

/* In stor-layout.c */
extern void fixup_signed_type		PARAMS ((tree));
extern void internal_reference_types 	PARAMS ((void));

/* varasm.c */
extern void make_decl_rtl		PARAMS ((tree, const char *));
extern void make_decl_one_only		PARAMS ((tree));
extern int supports_one_only		PARAMS ((void));
extern void variable_section		PARAMS ((tree, int));

/* In fold-const.c */
extern int div_and_round_double		PARAMS ((enum tree_code, int,
						 unsigned HOST_WIDE_INT,
						 HOST_WIDE_INT,
						 unsigned HOST_WIDE_INT,
						 HOST_WIDE_INT,
						 unsigned HOST_WIDE_INT *,
						 HOST_WIDE_INT *,
						 unsigned HOST_WIDE_INT *,
						 HOST_WIDE_INT *));

/* In stmt.c */
extern void emit_nop			PARAMS ((void));
extern void expand_computed_goto	PARAMS ((tree));
extern bool parse_output_constraint     PARAMS ((const char **,
						 int, int, int,
						 bool *, bool *, bool *));
extern void expand_asm_operands		PARAMS ((tree, tree, tree, tree, int,
						 const char *, int));
extern int any_pending_cleanups		PARAMS ((int));
extern void init_stmt			PARAMS ((void));
extern void init_stmt_for_function	PARAMS ((void));
extern int drop_through_at_end_p	PARAMS ((void));
extern void expand_start_target_temps	PARAMS ((void));
extern void expand_end_target_temps	PARAMS ((void));
extern void expand_elseif		PARAMS ((tree));
extern void save_stack_pointer		PARAMS ((void));
extern void expand_decl			PARAMS ((tree));
extern int expand_decl_cleanup		PARAMS ((tree, tree));
extern int expand_decl_cleanup_eh	PARAMS ((tree, tree, int));
extern void expand_anon_union_decl	PARAMS ((tree, tree, tree));
extern void move_cleanups_up		PARAMS ((void));
extern void expand_start_case_dummy	PARAMS ((void));
extern void expand_end_case_dummy	PARAMS ((void));
extern tree case_index_expr_type	PARAMS ((void));
extern HOST_WIDE_INT all_cases_count	PARAMS ((tree, int *));
extern void check_for_full_enumeration_handling PARAMS ((tree));
extern void declare_nonlocal_label	PARAMS ((tree));

/* If KIND=='I', return a suitable global initializer (constructor) name.
   If KIND=='D', return a suitable global clean-up (destructor) name.  */
extern tree get_file_function_name PARAMS ((int));

/* Interface of the DWARF2 unwind info support.  */

/* Generate a new label for the CFI info to refer to.  */

extern char *dwarf2out_cfi_label	PARAMS ((void));

/* Entry point to update the canonical frame address (CFA).  */

extern void dwarf2out_def_cfa		PARAMS ((const char *, unsigned, long));

/* Add the CFI for saving a register window.  */

extern void dwarf2out_window_save	PARAMS ((const char *));

/* Add a CFI to update the running total of the size of arguments pushed
   onto the stack.  */

extern void dwarf2out_args_size		PARAMS ((const char *, long));

/* Entry point for saving a register to the stack.  */

extern void dwarf2out_reg_save		PARAMS ((const char *, unsigned, long));

/* Entry point for saving the return address in the stack.  */

extern void dwarf2out_return_save	PARAMS ((const char *, long));

/* Entry point for saving the return address in a register.  */

extern void dwarf2out_return_reg	PARAMS ((const char *, unsigned));

/* The type of a function that walks over tree structure.  */

typedef tree (*walk_tree_fn)		PARAMS ((tree *, int *, void *));

/* In tree-dump.c */

/* Different tree dump places.  When you add new tree dump places,
   extend the DUMP_FILES array in tree-dump.c */
enum tree_dump_index
{
  TDI_all,			/* dump the whole translation unit */
  TDI_class,			/* dump class hierarchy */
  TDI_original,			/* dump each function before optimizing it */
  TDI_optimized,		/* dump each function after optimizing it */
  TDI_inlined,			/* dump each function after inlining
				   within it.  */
  TDI_end
};

/* Bit masks to control tree dumping. Not all values are applicable to
   all tree dumps. Add new ones at the end. When you define new
   values, extend the DUMP_OPTIONS array in tree-dump.c */
#define TDF_ADDRESS	(1 << 0)	/* dump node addresses */
#define TDF_SLIM	(1 << 1)	/* don't go wild following links */

typedef struct dump_info *dump_info_p;

extern int dump_flag			PARAMS ((dump_info_p, int, tree));
extern int dump_enabled_p		PARAMS ((enum tree_dump_index));
extern FILE *dump_begin			PARAMS ((enum tree_dump_index, int *));
extern void dump_end			PARAMS ((enum tree_dump_index, FILE *));
extern void dump_node			PARAMS ((tree, int, FILE *));
extern int dump_switch_p                PARAMS ((const char *));
extern const char *dump_flag_name	PARAMS ((enum tree_dump_index));


/* Redefine abort to report an internal error w/o coredump, and
   reporting the location of the error in the source file.  This logic
   is duplicated in rtl.h and tree.h because every file that needs the
   special abort includes one or both.  toplev.h gets too few files,
   system.h gets too many.  */

extern void fancy_abort PARAMS ((const char *, int, const char *))
    ATTRIBUTE_NORETURN;
#define abort() fancy_abort (__FILE__, __LINE__, __FUNCTION__)
