/* Front-end tree definitions for GNU compiler.
   Copyright (C) 1989, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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

#ifndef GCC_TREE_H
#define GCC_TREE_H

#include "machmode.h"
#include "version.h"
#include "input.h"

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
extern const char tree_code_type[];
#define TREE_CODE_CLASS(CODE)	tree_code_type[(int) (CODE)]

/* Returns nonzero iff CLASS is the tree-code class of an
   expression.  */

#define IS_EXPR_CODE_CLASS(CLASS) (strchr ("<12ers", (CLASS)) != 0)

/* Returns nonzero iff NODE is an expression of some kind.  */

#define EXPR_P(NODE) IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (TREE_CODE (NODE)))

/* Number of argument-words in each kind of tree-node.  */

extern const unsigned char tree_code_length[];
#define TREE_CODE_LENGTH(CODE)	tree_code_length[(int) (CODE)]

/* Names of tree components.  */

extern const char *const tree_code_name[];

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

#define DEF_BUILTIN(ENUM, N, C, T, LT, B, F, NA, AT, IM) ENUM,
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
extern GTY(()) tree built_in_decls[(int) END_BUILTINS];
extern GTY(()) tree implicit_built_in_decls[(int) END_BUILTINS];

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

struct tree_common GTY(())
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
  unsigned deprecated_flag : 1;
  unsigned unused_1 : 1;

  unsigned lang_flag_0 : 1;
  unsigned lang_flag_1 : 1;
  unsigned lang_flag_2 : 1;
  unsigned lang_flag_3 : 1;
  unsigned lang_flag_4 : 1;
  unsigned lang_flag_5 : 1;
  unsigned lang_flag_6 : 1;
  unsigned unused_2 : 1;
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
       EXPR_WFL_EMIT_LINE_NOTE in
           EXPR_WITH_FILE_LOCATION

   private_flag:

       TREE_PRIVATE in
           ..._DECL
       CALL_EXPR_HAS_RETURN_SLOT_ADDR in
           CALL_EXPR

   protected_flag:

       TREE_PROTECTED in
           BLOCK
	   ..._DECL
       CALL_FROM_THUNK_P in
           CALL_EXPR 

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

       TYPE_ALIGN_OK in
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
#define TREE_SET_CODE(NODE, VALUE) ((NODE)->common.code = (VALUE))

/* When checking is enabled, errors will be generated if a tree node
   is accessed incorrectly. The macros abort with a fatal error.  */
#if defined ENABLE_TREE_CHECKING && (GCC_VERSION >= 2007)

#define TREE_CHECK(T, CODE) __extension__				\
({  const tree __t = (T);						\
    if (TREE_CODE (__t) != (CODE))					\
      tree_check_failed (__t, (CODE), __FILE__, __LINE__, __FUNCTION__); \
    __t; })

#define TREE_CLASS_CHECK(T, CLASS) __extension__			\
({  const tree __t = (T);						\
    if (TREE_CODE_CLASS (TREE_CODE(__t)) != (CLASS))			\
      tree_class_check_failed (__t, (CLASS), __FILE__, __LINE__,	\
			       __FUNCTION__);				\
    __t; })

/* These checks have to be special cased.  */
#define EXPR_CHECK(T) __extension__					\
({  const tree __t = (T);						\
    char const __c = TREE_CODE_CLASS (TREE_CODE (__t));			\
    if (!IS_EXPR_CODE_CLASS (__c))					\
      tree_class_check_failed (__t, 'e', __FILE__, __LINE__,		\
			       __FUNCTION__);				\
    __t; })

#define TREE_VEC_ELT_CHECK(T, I) __extension__				\
(*({const tree __t = (T);						\
    const int __i = (I);						\
    if (TREE_CODE (__t) != TREE_VEC)					\
      tree_check_failed (__t, TREE_VEC,					\
			 __FILE__, __LINE__, __FUNCTION__);		\
    if (__i < 0 || __i >= __t->vec.length)				\
      tree_vec_elt_check_failed (__i, __t->vec.length,			\
				 __FILE__, __LINE__, __FUNCTION__);	\
    &__t->vec.a[__i]; }))

/* Special checks for TREE_OPERANDs.  */
#define TREE_OPERAND_CHECK(T, I) __extension__				\
(*({const tree __t = EXPR_CHECK (T);					\
    const int __i = (I);						\
    if (__i < 0 || __i >= TREE_CODE_LENGTH (TREE_CODE (__t)))		\
      tree_operand_check_failed (__i, TREE_CODE (__t),			\
				 __FILE__, __LINE__, __FUNCTION__);	\
    &__t->exp.operands[__i]; }))

#define TREE_OPERAND_CHECK_CODE(T, CODE, I) __extension__		\
(*({const tree __t = (T);						\
    const int __i = (I);						\
    if (TREE_CODE (__t) != CODE)					\
      tree_check_failed (__t, CODE, __FILE__, __LINE__, __FUNCTION__);	\
    if (__i < 0 || __i >= TREE_CODE_LENGTH (CODE))			\
      tree_operand_check_failed (__i, (CODE),				\
				 __FILE__, __LINE__, __FUNCTION__);	\
    &__t->exp.operands[__i]; }))

#define TREE_RTL_OPERAND_CHECK(T, CODE, I) __extension__		\
(*(rtx *)								\
 ({const tree __t = (T);						\
    const int __i = (I);						\
    if (TREE_CODE (__t) != (CODE))					\
      tree_check_failed (__t, (CODE), __FILE__, __LINE__, __FUNCTION__); \
    if (__i < 0 || __i >= TREE_CODE_LENGTH ((CODE)))			\
      tree_operand_check_failed (__i, (CODE),				\
				 __FILE__, __LINE__, __FUNCTION__);	\
    &__t->exp.operands[__i]; }))

extern void tree_check_failed (const tree, enum tree_code,
			       const char *, int, const char *)
    ATTRIBUTE_NORETURN;
extern void tree_class_check_failed (const tree, int,
				     const char *, int, const char *)
    ATTRIBUTE_NORETURN;
extern void tree_vec_elt_check_failed (int, int, const char *,
				       int, const char *)
    ATTRIBUTE_NORETURN;

extern void tree_operand_check_failed (int, enum tree_code,
				       const char *, int, const char *)
    ATTRIBUTE_NORETURN;

#else /* not ENABLE_TREE_CHECKING, or not gcc */

#define TREE_CHECK(T, CODE)		(T)
#define TREE_CLASS_CHECK(T, CODE)	(T)
#define EXPR_CHECK(T)			(T)
#define TREE_VEC_ELT_CHECK(T, I)	((T)->vec.a[I])
#define TREE_OPERAND_CHECK(T, I)	((T)->exp.operands[I])
#define TREE_OPERAND_CHECK_CODE(T, CODE, I) ((T)->exp.operands[I])
#define TREE_RTL_OPERAND_CHECK(T, CODE, I)  (*(rtx *) &((T)->exp.operands[I]))

#endif

#include "tree-check.h"

#define TYPE_CHECK(T)		TREE_CLASS_CHECK (T, 't')
#define DECL_CHECK(T)		TREE_CLASS_CHECK (T, 'd')
#define CST_CHECK(T)		TREE_CLASS_CHECK (T, 'c')
#define STMT_CHECK(T)		TREE_CLASS_CHECK (T, 's')

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

/* Like STRIP_NOPS, but don't alter the TREE_TYPE main variant either.  */

#define STRIP_MAIN_TYPE_NOPS(EXP)					\
  while ((TREE_CODE (EXP) == NOP_EXPR					\
	  || TREE_CODE (EXP) == CONVERT_EXPR				\
	  || TREE_CODE (EXP) == NON_LVALUE_EXPR)			\
	 && TREE_OPERAND (EXP, 0) != error_mark_node			\
	 && (TYPE_MAIN_VARIANT (TREE_TYPE (EXP))			\
	     == TYPE_MAIN_VARIANT (TREE_TYPE (TREE_OPERAND (EXP, 0)))))	\
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

/* Nonzero if TYPE represents a scalar floating-point type.  */

#define SCALAR_FLOAT_TYPE_P(TYPE) (TREE_CODE (TYPE) == REAL_TYPE)

/* Nonzero if TYPE represents a complex floating-point type.  */

#define COMPLEX_FLOAT_TYPE_P(TYPE)	\
  (TREE_CODE (TYPE) == COMPLEX_TYPE	\
   && TREE_CODE (TREE_TYPE (TYPE)) == REAL_TYPE)

/* Nonzero if TYPE represents a floating-point type, including complex
   floating-point types.  */

#define FLOAT_TYPE_P(TYPE)		\
  (SCALAR_FLOAT_TYPE_P (TYPE) || COMPLEX_FLOAT_TYPE_P (TYPE))

/* Nonzero if TYPE represents an aggregate (multi-component) type.  */

#define AGGREGATE_TYPE_P(TYPE) \
  (TREE_CODE (TYPE) == ARRAY_TYPE || TREE_CODE (TYPE) == RECORD_TYPE \
   || TREE_CODE (TYPE) == UNION_TYPE || TREE_CODE (TYPE) == QUAL_UNION_TYPE \
   || TREE_CODE (TYPE) == SET_TYPE)

/* Nonzero if TYPE represents a pointer or reference type.
   (It should be renamed to INDIRECT_TYPE_P.)  */

#define POINTER_TYPE_P(TYPE) \
  (TREE_CODE (TYPE) == POINTER_TYPE || TREE_CODE (TYPE) == REFERENCE_TYPE)

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

/* Nonzero if NODE is a _DECL with TREE_READONLY set.  */
#define TREE_READONLY_DECL_P(NODE) (TREE_READONLY (NODE) && DECL_P (NODE))

/* Value of expression is constant.
   Always appears in all ..._CST nodes.
   May also appear in an arithmetic expression, an ADDR_EXPR or a CONSTRUCTOR
   if the value is constant.  */
#define TREE_CONSTANT(NODE) ((NODE)->common.constant_flag)

/* In INTEGER_TYPE or ENUMERAL_TYPE nodes, means an unsigned type.
   In FIELD_DECL nodes, means an unsigned bit field.  */
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

/* In a CALL_EXPR, means that the address of the return slot is part of the
   argument list.  */
#define CALL_EXPR_HAS_RETURN_SLOT_ADDR(NODE) ((NODE)->common.private_flag)

/* In a CALL_EXPR, means that the call is the jump from a thunk to the
   thunked-to function.  */
#define CALL_FROM_THUNK_P(NODE) ((NODE)->common.protected_flag)

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

/* Nonzero in an IDENTIFIER_NODE if the use of the name is defined as a
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

struct tree_int_cst GTY(())
{
  struct tree_common common;
  /* A sub-struct is necessary here because the function `const_hash'
     wants to scan both words as a unit and taking the address of the
     sub-struct yields the properly inclusive bounded pointer.  */
  struct tree_int_cst_lowhi {
    unsigned HOST_WIDE_INT low;
    HOST_WIDE_INT high;
  } int_cst;
};

/* In a REAL_CST node.  struct real_value is an opaque entity, with
   manipulators defined in real.h.  We don't want tree.h depending on
   real.h and transitively on tm.h.  */
struct real_value;

#define TREE_REAL_CST_PTR(NODE) (REAL_CST_CHECK (NODE)->real_cst.real_cst_ptr)
#define TREE_REAL_CST(NODE) (*TREE_REAL_CST_PTR (NODE))

struct tree_real_cst GTY(())
{
  struct tree_common common;
  struct real_value * real_cst_ptr;
};

/* In a STRING_CST */
#define TREE_STRING_LENGTH(NODE) (STRING_CST_CHECK (NODE)->string.length)
#define TREE_STRING_POINTER(NODE) (STRING_CST_CHECK (NODE)->string.pointer)

struct tree_string GTY(())
{
  struct tree_common common;
  int length;
  const char *pointer;
};

/* In a COMPLEX_CST node.  */
#define TREE_REALPART(NODE) (COMPLEX_CST_CHECK (NODE)->complex.real)
#define TREE_IMAGPART(NODE) (COMPLEX_CST_CHECK (NODE)->complex.imag)

struct tree_complex GTY(())
{
  struct tree_common common;
  tree real;
  tree imag;
};

/* In a VECTOR_CST node.  */
#define TREE_VECTOR_CST_ELTS(NODE) (VECTOR_CST_CHECK (NODE)->vector.elements)

struct tree_vector GTY(())
{
  struct tree_common common;
  tree elements;
};

#include "hashtable.h"

/* Define fields and accessors for some special-purpose tree nodes.  */

#define IDENTIFIER_LENGTH(NODE) \
  (IDENTIFIER_NODE_CHECK (NODE)->identifier.id.len)
#define IDENTIFIER_POINTER(NODE) \
  ((const char *) IDENTIFIER_NODE_CHECK (NODE)->identifier.id.str)
#define IDENTIFIER_HASH_VALUE(NODE) \
  (IDENTIFIER_NODE_CHECK (NODE)->identifier.id.hash_value)

/* Translate a hash table identifier pointer to a tree_identifier
   pointer, and vice versa.  */

#define HT_IDENT_TO_GCC_IDENT(NODE) \
  ((tree) ((char *) (NODE) - sizeof (struct tree_common)))
#define GCC_IDENT_TO_HT_IDENT(NODE) (&((struct tree_identifier *) (NODE))->id)

struct tree_identifier GTY(())
{
  struct tree_common common;
  struct ht_identifier id;
};

/* In a TREE_LIST node.  */
#define TREE_PURPOSE(NODE) (TREE_LIST_CHECK (NODE)->list.purpose)
#define TREE_VALUE(NODE) (TREE_LIST_CHECK (NODE)->list.value)

struct tree_list GTY(())
{
  struct tree_common common;
  tree purpose;
  tree value;
};

/* In a TREE_VEC node.  */
#define TREE_VEC_LENGTH(NODE) (TREE_VEC_CHECK (NODE)->vec.length)
#define TREE_VEC_END(NODE) \
  ((void) TREE_VEC_CHECK (NODE), &((NODE)->vec.a[(NODE)->vec.length]))

#define TREE_VEC_ELT(NODE,I) TREE_VEC_ELT_CHECK (NODE, I)

struct tree_vec GTY(())
{
  struct tree_common common;
  int length;
  tree GTY ((length ("TREE_VEC_LENGTH ((tree)&%h)"))) a[1];
};

/* Define fields and accessors for some nodes that represent expressions.  */

/* In a SAVE_EXPR node.  */
#define SAVE_EXPR_CONTEXT(NODE) TREE_OPERAND_CHECK_CODE (NODE, SAVE_EXPR, 1)
#define SAVE_EXPR_RTL(NODE) TREE_RTL_OPERAND_CHECK (NODE, SAVE_EXPR, 2)

#define SAVE_EXPR_NOPLACEHOLDER(NODE) TREE_UNSIGNED (SAVE_EXPR_CHECK (NODE))
/* Nonzero if the SAVE_EXPRs value should be kept, even if it occurs
   both in normal code and in a handler.  (Normally, in a handler, all
   SAVE_EXPRs are unsaved, meaning that their values are
   recalculated.)  */
#define SAVE_EXPR_PERSISTENT_P(NODE) TREE_ASM_WRITTEN (SAVE_EXPR_CHECK (NODE))

/* In a RTL_EXPR node.  */
#define RTL_EXPR_SEQUENCE(NODE) TREE_RTL_OPERAND_CHECK (NODE, RTL_EXPR, 0)
#define RTL_EXPR_RTL(NODE) TREE_RTL_OPERAND_CHECK (NODE, RTL_EXPR, 1)
#define RTL_EXPR_ALT_RTL(NODE) TREE_RTL_OPERAND_CHECK (NODE, RTL_EXPR, 2)

/* In a WITH_CLEANUP_EXPR node.  */
#define WITH_CLEANUP_EXPR_RTL(NODE) \
  TREE_RTL_OPERAND_CHECK (NODE, WITH_CLEANUP_EXPR, 2)

/* In a CONSTRUCTOR node.  */
#define CONSTRUCTOR_ELTS(NODE) TREE_OPERAND_CHECK_CODE (NODE, CONSTRUCTOR, 0)

/* In ordinary expression nodes.  */
#define TREE_OPERAND(NODE, I) TREE_OPERAND_CHECK (NODE, I)
#define TREE_COMPLEXITY(NODE) (EXPR_CHECK (NODE)->exp.complexity)

/* In a LABELED_BLOCK_EXPR node.  */
#define LABELED_BLOCK_LABEL(NODE) \
  TREE_OPERAND_CHECK_CODE (NODE, LABELED_BLOCK_EXPR, 0)
#define LABELED_BLOCK_BODY(NODE) \
  TREE_OPERAND_CHECK_CODE (NODE, LABELED_BLOCK_EXPR, 1)

/* In an EXIT_BLOCK_EXPR node.  */
#define EXIT_BLOCK_LABELED_BLOCK(NODE) \
  TREE_OPERAND_CHECK_CODE (NODE, EXIT_BLOCK_EXPR, 0)
#define EXIT_BLOCK_RETURN(NODE) TREE_OPERAND_CHECK_CODE (NODE, EXIT_BLOCK_EXPR, 1)

/* In a LOOP_EXPR node.  */
#define LOOP_EXPR_BODY(NODE) TREE_OPERAND_CHECK_CODE (NODE, LOOP_EXPR, 0)

/* In an EXPR_WITH_FILE_LOCATION node.  */
#define EXPR_WFL_EMIT_LINE_NOTE(NODE) \
  (EXPR_WITH_FILE_LOCATION_CHECK (NODE)->common.public_flag)
#define EXPR_WFL_NODE(NODE) \
  TREE_OPERAND_CHECK_CODE (NODE, EXPR_WITH_FILE_LOCATION, 0)
#define EXPR_WFL_FILENAME_NODE(NODE) \
  TREE_OPERAND_CHECK_CODE (NODE, EXPR_WITH_FILE_LOCATION, 1)
#define EXPR_WFL_FILENAME(NODE) \
  IDENTIFIER_POINTER (EXPR_WFL_FILENAME_NODE (NODE))
/* ??? Java uses this in all expressions.  */
#define EXPR_WFL_LINECOL(NODE) (EXPR_CHECK (NODE)->exp.complexity)
#define EXPR_WFL_LINENO(NODE) (EXPR_WFL_LINECOL (NODE) >> 12)
#define EXPR_WFL_COLNO(NODE) (EXPR_WFL_LINECOL (NODE) & 0xfff)
#define EXPR_WFL_SET_LINECOL(NODE, LINE, COL) \
  (EXPR_WFL_LINECOL(NODE) = ((LINE) << 12) | ((COL) & 0xfff))

/* In a TARGET_EXPR node.  */
#define TARGET_EXPR_SLOT(NODE) TREE_OPERAND_CHECK_CODE (NODE, TARGET_EXPR, 0)
#define TARGET_EXPR_INITIAL(NODE) TREE_OPERAND_CHECK_CODE (NODE, TARGET_EXPR, 1)
#define TARGET_EXPR_CLEANUP(NODE) TREE_OPERAND_CHECK_CODE (NODE, TARGET_EXPR, 2)

struct tree_exp GTY(())
{
  struct tree_common common;
  int complexity;
  tree GTY ((special ("tree_exp"),
	     desc ("TREE_CODE ((tree) &%0)")))
    operands[1];
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

struct tree_block GTY(())
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
#define TYPE_SYMTAB_DIE(NODE) (TYPE_CHECK (NODE)->type.symtab.die)
#define TYPE_NAME(NODE) (TYPE_CHECK (NODE)->type.name)
#define TYPE_NEXT_VARIANT(NODE) (TYPE_CHECK (NODE)->type.next_variant)
#define TYPE_MAIN_VARIANT(NODE) (TYPE_CHECK (NODE)->type.main_variant)
#define TYPE_CONTEXT(NODE) (TYPE_CHECK (NODE)->type.context)
#define TYPE_LANG_SPECIFIC(NODE) (TYPE_CHECK (NODE)->type.lang_specific)

/* For a VECTOR_TYPE node, this describes a different type which is emitted
   in the debugging output.  We use this to describe a vector as a
   structure containing an array.  */
#define TYPE_DEBUG_REPRESENTATION_TYPE(NODE) (TYPE_CHECK (NODE)->type.values)

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

/* There is a TYPE_QUAL value for each type qualifier.  They can be
   combined by bitwise-or to form the complete set of qualifiers for a
   type.  */

#define TYPE_UNQUALIFIED   0x0
#define TYPE_QUAL_CONST    0x1
#define TYPE_QUAL_VOLATILE 0x2
#define TYPE_QUAL_RESTRICT 0x4

/* The set of type qualifiers for this type.  */
#define TYPE_QUALS(NODE)					\
  ((TYPE_READONLY (NODE) * TYPE_QUAL_CONST)			\
   | (TYPE_VOLATILE (NODE) * TYPE_QUAL_VOLATILE)		\
   | (TYPE_RESTRICT (NODE) * TYPE_QUAL_RESTRICT))

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

struct die_struct;

struct tree_type GTY(())
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
  unsigned spare : 2;

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
  union tree_type_symtab {
    int GTY ((tag ("0"))) address;
    char * GTY ((tag ("1"))) pointer;
    struct die_struct * GTY ((tag ("2"))) die;
  } GTY ((desc ("debug_hooks == &sdb_debug_hooks ? 1 : debug_hooks == &dwarf2_debug_hooks ? 2 : 0"),
	  descbits ("2"))) symtab;
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
   base.  The actual contents are language-dependent.  In the C++
   front-end this field is an INTEGER_CST giving an offset into the
   vtable where the offset to the virtual base can be found.  */
#define BINFO_VPTR_FIELD(NODE) TREE_VEC_ELT (NODE, 5)

/* Indicates the accesses this binfo has to its bases. The values are
   access_public_node, access_protected_node or access_private_node.
   If this array is not present, public access is implied.  */
#define BINFO_BASEACCESSES(NODE) TREE_VEC_ELT ((NODE), 6)
#define BINFO_BASEACCESS(NODE,N) TREE_VEC_ELT (BINFO_BASEACCESSES(NODE), (N))

/* Number of language independent elements in a binfo.  Languages may
   add additional trailing elements.  */

#define BINFO_ELTS 7

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
#define DECL_ASSEMBLER_NAME(NODE) decl_assembler_name (NODE)

/* Returns nonzero if the DECL_ASSEMBLER_NAME for NODE has been set.  If zero,
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
   ? (void) SET_DECL_ASSEMBLER_NAME (DECL2,				\
				     DECL_ASSEMBLER_NAME (DECL1))	\
   : (void) 0)

/* Records the section name in a section attribute.  Used to pass
   the name from decl_attributes to make_function_rtl and make_decl_rtl.  */
#define DECL_SECTION_NAME(NODE) (DECL_CHECK (NODE)->decl.section_name)

/*  For FIELD_DECLs, this is the RECORD_TYPE, UNION_TYPE, or
    QUAL_UNION_TYPE node that the field is a member of.  For VAR_DECL,
    PARM_DECL, FUNCTION_DECL, LABEL_DECL, and CONST_DECL nodes, this
    points to either the FUNCTION_DECL for the containing function,
    the RECORD_TYPE or UNION_TYPE for the containing type, or
    NULL_TREE or a TRANSLATION_UNIT_DECL if the given decl has "file
    scope".  */
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
   For a TRANSLATION_UNIT_DECL, holds the namespace's BLOCK.
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
#define DECL_SOURCE_LOCATION(NODE) (DECL_CHECK (NODE)->decl.locus)
#define DECL_SOURCE_FILE(NODE) (DECL_SOURCE_LOCATION (NODE).file)
#define DECL_SOURCE_LINE(NODE) (DECL_SOURCE_LOCATION (NODE).line)
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
/* Holds the RTL expression for the value of a variable or function.
   This value can be evaluated lazily for functions, variables with
   static storage duration, and labels.  */
#define DECL_RTL(NODE)					\
  (DECL_CHECK (NODE)->decl.rtl				\
   ? (NODE)->decl.rtl					\
   : (make_decl_rtl (NODE, NULL), (NODE)->decl.rtl))
/* Set the DECL_RTL for NODE to RTL.  */
#define SET_DECL_RTL(NODE, RTL) set_decl_rtl (NODE, RTL)
/* Returns nonzero if the DECL_RTL for NODE has already been set.  */
#define DECL_RTL_SET_P(NODE)  (DECL_CHECK (NODE)->decl.rtl != NULL)
/* Copy the RTL from NODE1 to NODE2.  If the RTL was not set for
   NODE1, it will not be set for NODE2; this is a lazy copy.  */
#define COPY_DECL_RTL(NODE1, NODE2) \
  (DECL_CHECK (NODE2)->decl.rtl = DECL_CHECK (NODE1)->decl.rtl)
/* The DECL_RTL for NODE, if it is set, or NULL, if it is not set.  */
#define DECL_RTL_IF_SET(NODE) (DECL_RTL_SET_P (NODE) ? DECL_RTL (NODE) : NULL)

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
   initializations.  */
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

/* In a FUNCTION_DECL with a nonzero DECL_CONTEXT, indicates that a
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

/* Nonzero in a FUNCTION_DECL means that this function was declared inline,
   such as via the `inline' keyword in C/C++.  This flag controls the linkage
   semantics of 'inline'; whether or not the function is inlined is
   controlled by DECL_INLINE.  */
#define DECL_DECLARED_INLINE_P(NODE) \
  (FUNCTION_DECL_CHECK (NODE)->decl.declared_inline_flag)

/* Value of the decls's visibility attribute */
#define DECL_VISIBILITY(NODE) (DECL_CHECK (NODE)->decl.visibility)

/* In a FUNCTION_DECL, nonzero if the function cannot be inlined.  */
#define DECL_UNINLINABLE(NODE) (FUNCTION_DECL_CHECK (NODE)->decl.uninlinable)

/* In a VAR_DECL, nonzero if the data should be allocated from
   thread-local storage.  */
#define DECL_THREAD_LOCAL(NODE) (VAR_DECL_CHECK (NODE)->decl.thread_local_flag)

/* In a FUNCTION_DECL, the saved representation of the body of the
   entire function.  Usually a COMPOUND_STMT, but in C++ this may also
   be a RETURN_INIT, CTOR_INITIALIZER, or TRY_BLOCK.  */
#define DECL_SAVED_TREE(NODE) (FUNCTION_DECL_CHECK (NODE)->decl.saved_tree)

/* List of FUNCTION_DECLs inlined into this function's body.  */
#define DECL_INLINED_FNS(NODE) (FUNCTION_DECL_CHECK (NODE)->decl.inlined_fns)

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

/* Nonzero for a decl which is at file scope.  */
#define DECL_FILE_SCOPE_P(EXP) 					\
  (! DECL_CONTEXT (EXP)						\
   || TREE_CODE (DECL_CONTEXT (EXP)) == TRANSLATION_UNIT_DECL)

/* Enumerate visibility settings.  */

enum symbol_visibility
{
  VISIBILITY_DEFAULT,
  VISIBILITY_INTERNAL,
  VISIBILITY_HIDDEN,
  VISIBILITY_PROTECTED
};

struct function;

struct tree_decl GTY(())
{
  struct tree_common common;
  location_t locus;
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

  unsigned non_addressable : 1;
  unsigned user_align : 1;
  unsigned uninlinable : 1;
  unsigned thread_local_flag : 1;
  unsigned declared_inline_flag : 1;
  ENUM_BITFIELD(symbol_visibility) visibility : 2;
  unsigned unused : 1;
  /* one unused bit.  */

  unsigned lang_flag_0 : 1;
  unsigned lang_flag_1 : 1;
  unsigned lang_flag_2 : 1;
  unsigned lang_flag_3 : 1;
  unsigned lang_flag_4 : 1;
  unsigned lang_flag_5 : 1;
  unsigned lang_flag_6 : 1;
  unsigned lang_flag_7 : 1;

  union tree_decl_u1 {
    /* In a FUNCTION_DECL for which DECL_BUILT_IN holds, this is
       DECL_FUNCTION_CODE.  */
    enum built_in_function f;
    /* In a FUNCTION_DECL for which DECL_BUILT_IN does not hold, this
       is used by language-dependent code.  */
    HOST_WIDE_INT i;
    /* DECL_ALIGN and DECL_OFFSET_ALIGN.  (These are not used for
       FUNCTION_DECLs).  */
    struct tree_decl_u1_a {
      unsigned int align : 24;
      unsigned int off_align : 8;
    } a;
  } GTY ((skip (""))) u1;

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

  /* In FUNCTION_DECL, if it is inline, holds the saved insn chain.
     In FIELD_DECL, is DECL_FIELD_BIT_OFFSET.
     In PARM_DECL, holds an RTL for the stack slot
     of register where the data was actually passed.
     Used by Chill and Java in LABEL_DECL and by C++ and Java in VAR_DECL.  */
  union tree_decl_u2 {
    struct function * GTY ((tag ("FUNCTION_DECL"))) f;
    rtx GTY ((tag ("PARM_DECL"))) r;
    tree GTY ((tag ("FIELD_DECL"))) t;
    int GTY ((tag ("VAR_DECL"))) i;
  } GTY ((desc ("TREE_CODE((tree) &(%0))"))) u2;

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

enum tree_node_structure_enum {
  TS_COMMON,
  TS_INT_CST,
  TS_REAL_CST,
  TS_VECTOR,
  TS_STRING,
  TS_COMPLEX,
  TS_IDENTIFIER,
  TS_DECL,
  TS_TYPE,
  TS_LIST,
  TS_VEC,
  TS_EXP,
  TS_BLOCK,
  LAST_TS_ENUM
};

/* Define the overall contents of a tree node.
   It may be any of the structures declared above
   for various types of node.  */

union tree_node GTY ((ptr_alias (union lang_tree_node),
		      desc ("tree_node_structure (&%h)")))
{
  struct tree_common GTY ((tag ("TS_COMMON"))) common;
  struct tree_int_cst GTY ((tag ("TS_INT_CST"))) int_cst;
  struct tree_real_cst GTY ((tag ("TS_REAL_CST"))) real_cst;
  struct tree_vector GTY ((tag ("TS_VECTOR"))) vector;
  struct tree_string GTY ((tag ("TS_STRING"))) string;
  struct tree_complex GTY ((tag ("TS_COMPLEX"))) complex;
  struct tree_identifier GTY ((tag ("TS_IDENTIFIER"))) identifier;
  struct tree_decl GTY ((tag ("TS_DECL"))) decl;
  struct tree_type GTY ((tag ("TS_TYPE"))) type;
  struct tree_list GTY ((tag ("TS_LIST"))) list;
  struct tree_vec GTY ((tag ("TS_VEC"))) vec;
  struct tree_exp GTY ((tag ("TS_EXP"))) exp;
  struct tree_block GTY ((tag ("TS_BLOCK"))) block;
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

  TI_PUBLIC,
  TI_PROTECTED,
  TI_PRIVATE,

  TI_BOOLEAN_FALSE,
  TI_BOOLEAN_TRUE,

  TI_COMPLEX_INTEGER_TYPE,
  TI_COMPLEX_FLOAT_TYPE,
  TI_COMPLEX_DOUBLE_TYPE,
  TI_COMPLEX_LONG_DOUBLE_TYPE,

  TI_FLOAT_TYPE,
  TI_DOUBLE_TYPE,
  TI_LONG_DOUBLE_TYPE,

  TI_FLOAT_PTR_TYPE,
  TI_DOUBLE_PTR_TYPE,
  TI_LONG_DOUBLE_PTR_TYPE,
  TI_INTEGER_PTR_TYPE,

  TI_VOID_TYPE,
  TI_PTR_TYPE,
  TI_CONST_PTR_TYPE,
  TI_SIZE_TYPE,
  TI_PTRDIFF_TYPE,
  TI_VA_LIST_TYPE,
  TI_BOOLEAN_TYPE,

  TI_VOID_LIST_NODE,

  TI_UV4SF_TYPE,
  TI_UV4SI_TYPE,
  TI_UV8HI_TYPE,
  TI_UV8QI_TYPE,
  TI_UV4HI_TYPE,
  TI_UV2HI_TYPE,
  TI_UV2SI_TYPE,
  TI_UV2SF_TYPE,
  TI_UV2DI_TYPE,
  TI_UV1DI_TYPE,
  TI_UV16QI_TYPE,

  TI_V4SF_TYPE,
  TI_V16SF_TYPE,
  TI_V4SI_TYPE,
  TI_V8HI_TYPE,
  TI_V8QI_TYPE,
  TI_V4HI_TYPE,
  TI_V2HI_TYPE,
  TI_V2SI_TYPE,
  TI_V2SF_TYPE,
  TI_V2DF_TYPE,
  TI_V2DI_TYPE,
  TI_V1DI_TYPE,
  TI_V16QI_TYPE,
  TI_V4DF_TYPE,

  TI_MAIN_IDENTIFIER,

  TI_MAX
};

extern GTY(()) tree global_trees[TI_MAX];

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

/* Base access nodes.  */
#define access_public_node		global_trees[TI_PUBLIC]
#define access_protected_node	        global_trees[TI_PROTECTED]
#define access_private_node		global_trees[TI_PRIVATE]

#define null_pointer_node		global_trees[TI_NULL_POINTER]

#define float_type_node			global_trees[TI_FLOAT_TYPE]
#define double_type_node		global_trees[TI_DOUBLE_TYPE]
#define long_double_type_node		global_trees[TI_LONG_DOUBLE_TYPE]

#define float_ptr_type_node		global_trees[TI_FLOAT_PTR_TYPE]
#define double_ptr_type_node		global_trees[TI_DOUBLE_PTR_TYPE]
#define long_double_ptr_type_node	global_trees[TI_LONG_DOUBLE_PTR_TYPE]
#define integer_ptr_type_node		global_trees[TI_INTEGER_PTR_TYPE]

#define complex_integer_type_node	global_trees[TI_COMPLEX_INTEGER_TYPE]
#define complex_float_type_node		global_trees[TI_COMPLEX_FLOAT_TYPE]
#define complex_double_type_node	global_trees[TI_COMPLEX_DOUBLE_TYPE]
#define complex_long_double_type_node	global_trees[TI_COMPLEX_LONG_DOUBLE_TYPE]

#define void_type_node			global_trees[TI_VOID_TYPE]
/* The C type `void *'.  */
#define ptr_type_node			global_trees[TI_PTR_TYPE]
/* The C type `const void *'.  */
#define const_ptr_type_node		global_trees[TI_CONST_PTR_TYPE]
/* The C type `size_t'.  */
#define size_type_node                  global_trees[TI_SIZE_TYPE]
#define ptrdiff_type_node		global_trees[TI_PTRDIFF_TYPE]
#define va_list_type_node		global_trees[TI_VA_LIST_TYPE]

#define boolean_type_node		global_trees[TI_BOOLEAN_TYPE]
#define boolean_false_node		global_trees[TI_BOOLEAN_FALSE]
#define boolean_true_node		global_trees[TI_BOOLEAN_TRUE]

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
#define unsigned_V2HI_type_node		global_trees[TI_UV2HI_TYPE]
#define unsigned_V2SI_type_node		global_trees[TI_UV2SI_TYPE]
#define unsigned_V2DI_type_node		global_trees[TI_UV2DI_TYPE]
#define unsigned_V1DI_type_node		global_trees[TI_UV1DI_TYPE]

#define V16QI_type_node			global_trees[TI_V16QI_TYPE]
#define V4SF_type_node			global_trees[TI_V4SF_TYPE]
#define V4SI_type_node			global_trees[TI_V4SI_TYPE]
#define V8QI_type_node			global_trees[TI_V8QI_TYPE]
#define V8HI_type_node			global_trees[TI_V8HI_TYPE]
#define V4HI_type_node			global_trees[TI_V4HI_TYPE]
#define V2HI_type_node			global_trees[TI_V2HI_TYPE]
#define V2SI_type_node			global_trees[TI_V2SI_TYPE]
#define V2SF_type_node			global_trees[TI_V2SF_TYPE]
#define V2DI_type_node			global_trees[TI_V2DI_TYPE]
#define V2DF_type_node			global_trees[TI_V2DF_TYPE]
#define V16SF_type_node			global_trees[TI_V16SF_TYPE]
#define V1DI_type_node			global_trees[TI_V1DI_TYPE]
#define V4DF_type_node			global_trees[TI_V4DF_TYPE]

/* An enumeration of the standard C integer types.  These must be
   ordered so that shorter types appear before longer ones, and so
   that signed types appear before unsigned ones, for the correct
   functioning of interpret_integer() in c-lex.c.  */
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
extern GTY(()) tree integer_types[itk_none];

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

/* Set to the default thread-local storage (tls) model to use.  */

enum tls_model {
  TLS_MODEL_GLOBAL_DYNAMIC = 1,
  TLS_MODEL_LOCAL_DYNAMIC,
  TLS_MODEL_INITIAL_EXEC,
  TLS_MODEL_LOCAL_EXEC
};

extern enum tls_model flag_tls_default;


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

extern tree decl_assembler_name (tree);

/* Compute the number of bytes occupied by 'node'.  This routine only
   looks at TREE_CODE and, if the code is TREE_VEC, TREE_VEC_LENGTH.  */

extern size_t tree_size (tree);

/* Lowest level primitive for allocating a node.
   The TREE_CODE is the only argument.  Contents are initialized
   to zero except for a few of the common fields.  */

extern tree make_node (enum tree_code);

/* Make a copy of a node, with all the same contents.  */

extern tree copy_node (tree);

/* Make a copy of a chain of TREE_LIST nodes.  */

extern tree copy_list (tree);

/* Make a TREE_VEC.  */

extern tree make_tree_vec (int);

/* Return the (unique) IDENTIFIER_NODE node for a given name.
   The name is supplied as a char *.  */

extern tree get_identifier (const char *);

#if GCC_VERSION >= 3000
#define get_identifier(str) \
  (__builtin_constant_p (str)				\
    ? get_identifier_with_length ((str), strlen (str))  \
    : get_identifier (str))
#endif


/* Identical to get_identifier, except that the length is assumed
   known.  */

extern tree get_identifier_with_length (const char *, size_t);

/* If an identifier with the name TEXT (a null-terminated string) has
   previously been referred to, return that node; otherwise return
   NULL_TREE.  */

extern tree maybe_get_identifier (const char *);

/* Construct various types of nodes.  */

#define build_int_2(LO, HI)  \
  build_int_2_wide ((unsigned HOST_WIDE_INT) (LO), (HOST_WIDE_INT) (HI))

extern tree build (enum tree_code, tree, ...);
extern tree build_nt (enum tree_code, ...);

extern tree build_int_2_wide (unsigned HOST_WIDE_INT, HOST_WIDE_INT);
extern tree build_vector (tree, tree);
extern tree build_constructor (tree, tree);
extern tree build_real_from_int_cst (tree, tree);
extern tree build_complex (tree, tree, tree);
extern tree build_string (int, const char *);
extern tree build1 (enum tree_code, tree, tree);
extern tree build_tree_list (tree, tree);
extern tree build_decl (enum tree_code, tree, tree);
extern tree build_block (tree, tree, tree, tree, tree);
extern tree build_expr_wfl (tree, const char *, int, int);

/* Construct various nodes representing data types.  */

extern tree make_signed_type (int);
extern tree make_unsigned_type (int);
extern void initialize_sizetypes (void);
extern void set_sizetype (tree);
extern void fixup_unsigned_type (tree);
extern tree build_pointer_type_for_mode (tree, enum machine_mode);
extern tree build_pointer_type (tree);
extern tree build_reference_type_for_mode (tree, enum machine_mode);
extern tree build_reference_type (tree);
extern tree build_type_no_quals (tree);
extern tree build_index_type (tree);
extern tree build_index_2_type (tree, tree);
extern tree build_array_type (tree, tree);
extern tree build_function_type (tree, tree);
extern tree build_function_type_list (tree, ...);
extern tree build_method_type_directly (tree, tree, tree);
extern tree build_method_type (tree, tree);
extern tree build_offset_type (tree, tree);
extern tree build_complex_type (tree);
extern tree array_type_nelts (tree);

extern tree value_member (tree, tree);
extern tree purpose_member (tree, tree);
extern tree binfo_member (tree, tree);
extern unsigned int attribute_hash_list (tree);
extern int attribute_list_equal (tree, tree);
extern int attribute_list_contained (tree, tree);
extern int tree_int_cst_equal (tree, tree);
extern int tree_int_cst_lt (tree, tree);
extern int tree_int_cst_compare (tree, tree);
extern int host_integerp (tree, int);
extern HOST_WIDE_INT tree_low_cst (tree, int);
extern int tree_int_cst_msb (tree);
extern int tree_int_cst_sgn (tree);
extern int tree_expr_nonnegative_p (tree);
extern int rtl_expr_nonnegative_p (rtx);
extern tree get_inner_array_type (tree);

/* From expmed.c.  Since rtl.h is included after tree.h, we can't
   put the prototype here.  Rtl.h does declare the prototype if
   tree.h had been included.  */

extern tree make_tree (tree, rtx);

/* Return a type like TTYPE except that its TYPE_ATTRIBUTES
   is ATTRIBUTE.

   Such modified types already made are recorded so that duplicates
   are not made.  */

extern tree build_type_attribute_variant (tree, tree);
extern tree build_decl_attribute_variant (tree, tree);

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
  tree (*const handler) (tree *node, tree name, tree args,
				 int flags, bool *no_add_attrs);
};

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

extern tree merge_decl_attributes (tree, tree);
extern tree merge_type_attributes (tree, tree);
extern void default_register_cpp_builtins (struct cpp_reader *);

/* Split a list of declspecs and attributes into two.  */

extern void split_specs_attrs (tree, tree *, tree *);

/* Strip attributes from a list of combined specs and attrs.  */

extern tree strip_attrs (tree);

/* Return 1 if an attribute and its arguments are valid for a decl or type.  */

extern int valid_machine_attribute (tree, tree, tree, tree);

/* Given a tree node and a string, return nonzero if the tree node is
   a valid attribute name for the string.  */

extern int is_attribute_p (const char *, tree);

/* Given an attribute name and a list of attributes, return the list element
   of the attribute or NULL_TREE if not found.  */

extern tree lookup_attribute (const char *, tree);

/* Given two attributes lists, return a list of their union.  */

extern tree merge_attributes (tree, tree);

#ifdef TARGET_DLLIMPORT_DECL_ATTRIBUTES
/* Given two Windows decl attributes lists, possibly including
   dllimport, return a list of their union .  */
extern tree merge_dllimport_decl_attributes (tree, tree);
#endif

/* Return a version of the TYPE, qualified as indicated by the
   TYPE_QUALS, if one exists.  If no qualified version exists yet,
   return NULL_TREE.  */

extern tree get_qualified_type (tree, int);

/* Like get_qualified_type, but creates the type if it does not
   exist.  This function never returns NULL_TREE.  */

extern tree build_qualified_type (tree, int);

/* Like build_qualified_type, but only deals with the `const' and
   `volatile' qualifiers.  This interface is retained for backwards
   compatibility with the various front-ends; new code should use
   build_qualified_type instead.  */

#define build_type_variant(TYPE, CONST_P, VOLATILE_P)			\
  build_qualified_type ((TYPE),						\
			((CONST_P) ? TYPE_QUAL_CONST : 0)		\
			| ((VOLATILE_P) ? TYPE_QUAL_VOLATILE : 0))

/* Make a copy of a type node.  */

extern tree build_type_copy (tree);

/* Finish up a builtin RECORD_TYPE. Give it a name and provide its
   fields. Optionally specify an alignment, and then lsy it out.  */

extern void finish_builtin_struct (tree, const char *,
							 tree, tree);

/* Given a ..._TYPE node, calculate the TYPE_SIZE, TYPE_SIZE_UNIT,
   TYPE_ALIGN and TYPE_MODE fields.  If called more than once on one
   node, does nothing except for the first time.  */

extern void layout_type (tree);

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
  /* The alignment of the record so far, ignoring #pragma pack and
     __attribute__ ((packed)), in bits.  */
  unsigned int unpacked_align;
  /* The previous field layed out.  */
  tree prev_field;
  /* The static variables (i.e., class variables, as opposed to
     instance variables) encountered in T.  */
  tree pending_statics;
  /* Bits remaining in the current alignment group */
  int remaining_in_alignment;
  /* True if we've seen a packed field that didn't have normal
     alignment anyway.  */
  int packed_maybe_necessary;
} *record_layout_info;

extern void set_lang_adjust_rli (void (*) (record_layout_info));
extern record_layout_info start_record_layout (tree);
extern tree bit_from_pos (tree, tree);
extern tree byte_from_pos (tree, tree);
extern void pos_from_bit (tree *, tree *, unsigned int, tree);
extern void normalize_offset (tree *, tree *, unsigned int);
extern tree rli_size_unit_so_far (record_layout_info);
extern tree rli_size_so_far (record_layout_info);
extern void normalize_rli (record_layout_info);
extern void place_field (record_layout_info, tree);
extern void compute_record_mode (tree);
extern void finish_record_layout (record_layout_info, int);

/* Given a hashcode and a ..._TYPE node (for which the hashcode was made),
   return a canonicalized ..._TYPE node, so that duplicates are not made.
   How the hash code is computed is up to the caller, as long as any two
   callers that could hash identical-looking type nodes agree.  */

extern tree type_hash_canon (unsigned int, tree);

/* Given a VAR_DECL, PARM_DECL, RESULT_DECL or FIELD_DECL node,
   calculates the DECL_SIZE, DECL_SIZE_UNIT, DECL_ALIGN and DECL_MODE
   fields.  Call this only once for any given decl node.

   Second argument is the boundary that this field can be assumed to
   be starting at (in bits).  Zero means it can be assumed aligned
   on any boundary that may be needed.  */

extern void layout_decl (tree, unsigned);

/* Return the mode for data of a given size SIZE and mode class CLASS.
   If LIMIT is nonzero, then don't use modes bigger than MAX_FIXED_MODE_SIZE.
   The value is BLKmode if no other mode is found.  This is like
   mode_for_size, but is passed a tree.  */

extern enum machine_mode mode_for_size_tree (tree, enum mode_class, int);

/* Return an expr equal to X but certainly not valid as an lvalue.  */

extern tree non_lvalue (tree);
extern tree pedantic_non_lvalue (tree);

extern tree convert (tree, tree);
extern unsigned int expr_align (tree);
extern tree expr_first (tree);
extern tree expr_last (tree);
extern int expr_length (tree);
extern tree size_in_bytes (tree);
extern HOST_WIDE_INT int_size_in_bytes (tree);
extern tree bit_position (tree);
extern HOST_WIDE_INT int_bit_position (tree);
extern tree byte_position (tree);
extern HOST_WIDE_INT int_byte_position (tree);

/* Define data structures, macros, and functions for handling sizes
   and the various types used to represent sizes.  */

enum size_type_kind
{
  SIZETYPE,		/* Normal representation of sizes in bytes.  */
  SSIZETYPE,		/* Signed representation of sizes in bytes.  */
  USIZETYPE,		/* Unsigned representation of sizes in bytes.  */
  BITSIZETYPE,		/* Normal representation of sizes in bits.  */
  SBITSIZETYPE,		/* Signed representation of sizes in bits.  */
  UBITSIZETYPE,	        /* Unsigned representation of sizes in bits.  */
  TYPE_KIND_LAST};

extern GTY(()) tree sizetype_tab[(int) TYPE_KIND_LAST];

#define sizetype sizetype_tab[(int) SIZETYPE]
#define bitsizetype sizetype_tab[(int) BITSIZETYPE]
#define ssizetype sizetype_tab[(int) SSIZETYPE]
#define usizetype sizetype_tab[(int) USIZETYPE]
#define sbitsizetype sizetype_tab[(int) SBITSIZETYPE]
#define ubitsizetype sizetype_tab[(int) UBITSIZETYPE]

extern tree size_binop (enum tree_code, tree, tree);
extern tree size_diffop (tree, tree);
extern tree size_int_wide (HOST_WIDE_INT, enum size_type_kind);
extern tree size_int_type_wide (HOST_WIDE_INT, tree);

#define size_int_type(L, T) size_int_type_wide ((HOST_WIDE_INT) (L), T)
#define size_int(L) size_int_wide ((HOST_WIDE_INT) (L), SIZETYPE)
#define ssize_int(L) size_int_wide ((HOST_WIDE_INT) (L), SSIZETYPE)
#define bitsize_int(L) size_int_wide ((HOST_WIDE_INT) (L), BITSIZETYPE)
#define sbitsize_int(L) size_int_wide ((HOST_WIDE_INT) (L), SBITSIZETYPE)

extern tree round_up (tree, int);
extern tree round_down (tree, int);
extern tree get_pending_sizes (void);
extern int is_pending_size (tree);
extern void put_pending_size (tree);
extern void put_pending_sizes (tree);

/* Type for sizes of data-type.  */

#define BITS_PER_UNIT_LOG \
  ((BITS_PER_UNIT > 1) + (BITS_PER_UNIT > 2) + (BITS_PER_UNIT > 4) \
   + (BITS_PER_UNIT > 8) + (BITS_PER_UNIT > 16) + (BITS_PER_UNIT > 32) \
   + (BITS_PER_UNIT > 64) + (BITS_PER_UNIT > 128) + (BITS_PER_UNIT > 256))

/* If nonzero, an upper limit on alignment of structure fields, in bits.  */
extern unsigned int maximum_field_alignment;

/* If nonzero, the alignment of a bitstring or (power-)set value, in bits.  */
extern unsigned int set_alignment;

/* Concatenate two lists (chains of TREE_LIST nodes) X and Y
   by making the last node in X point to Y.
   Returns X, except if X is 0 returns Y.  */

extern tree chainon (tree, tree);

/* Make a new TREE_LIST node from specified PURPOSE, VALUE and CHAIN.  */

extern tree tree_cons (tree, tree, tree);

/* Return the last tree node in a chain.  */

extern tree tree_last (tree);

/* Reverse the order of elements in a chain, and return the new head.  */

extern tree nreverse (tree);

/* Returns the length of a chain of nodes
   (number of chain pointers to follow before reaching a null pointer).  */

extern int list_length (tree);

/* Returns the number of FIELD_DECLs in a type.  */

extern int fields_length (tree);

/* Given an initializer INIT, return TRUE if INIT is zero or some
   aggregate of zeros.  Otherwise return FALSE.  */

extern bool initializer_zerop (tree);

/* Given an initializer INIT, return TRUE if INIT is at least 3/4 zeros.
   Otherwise return FALSE.  */

extern int mostly_zeros_p (tree);

/* integer_zerop (tree x) is nonzero if X is an integer constant of value 0 */

extern int integer_zerop (tree);

/* integer_onep (tree x) is nonzero if X is an integer constant of value 1 */

extern int integer_onep (tree);

/* integer_all_onesp (tree x) is nonzero if X is an integer constant
   all of whose significant bits are 1.  */

extern int integer_all_onesp (tree);

/* integer_pow2p (tree x) is nonzero is X is an integer constant with
   exactly one bit 1.  */

extern int integer_pow2p (tree);

/* integer_nonzerop (tree x) is nonzero if X is an integer constant
   with a nonzero value.  */

extern int integer_nonzerop (tree);

/* staticp (tree x) is nonzero if X is a reference to data allocated
   at a fixed address in memory.  */

extern int staticp (tree);

/* Gets an error if argument X is not an lvalue.
   Also returns 1 if X is an lvalue, 0 if not.  */

extern int lvalue_or_else (tree, const char *);

/* save_expr (EXP) returns an expression equivalent to EXP
   but it can be used multiple times within context CTX
   and only evaluate EXP once.  */

extern tree save_expr (tree);

/* Look inside EXPR and into any simple arithmetic operations.  Return
   the innermost non-arithmetic node.  */

extern tree skip_simple_arithmetic (tree);

/* Return TRUE if EXPR is a SAVE_EXPR or wraps simple arithmetic around a
   SAVE_EXPR.  Return FALSE otherwise.  */

extern bool saved_expr_p (tree);

/* Returns the index of the first non-tree operand for CODE, or the number
   of operands if all are trees.  */

extern int first_rtl_op (enum tree_code);

/* Return which tree structure is used by T.  */

enum tree_node_structure_enum tree_node_structure (tree);

/* unsave_expr (EXP) returns an expression equivalent to EXP but it
   can be used multiple times and will evaluate EXP in its entirety
   each time.  */

extern tree unsave_expr (tree);

/* Reset EXP in place so that it can be expanded again.  Does not
   recurse into subtrees.  */

extern void unsave_expr_1 (tree);

/* Return 0 if it is safe to evaluate EXPR multiple times,
   return 1 if it is safe if EXPR is unsaved afterward, or
   return 2 if it is completely unsafe.  */
extern int unsafe_for_reeval (tree);

/* Return 1 if EXP contains a PLACEHOLDER_EXPR; i.e., if it represents a size
   or offset that depends on a field within a record.

   Note that we only allow such expressions within simple arithmetic
   or a COND_EXPR.  */

extern bool contains_placeholder_p (tree);

/* This macro calls the above function but short-circuits the common
   case of a constant to save time.  Also check for null.  */

#define CONTAINS_PLACEHOLDER_P(EXP) \
  ((EXP) != 0 && ! TREE_CONSTANT (EXP) && contains_placeholder_p (EXP))

/* Return 1 if any part of the computation of TYPE involves a PLACEHOLDER_EXPR.
   This includes size, bounds, qualifiers (for QUAL_UNION_TYPE) and field
   positions.  */

extern bool type_contains_placeholder_p (tree);

/* Return 1 if EXP contains any expressions that produce cleanups for an
   outer scope to deal with.  Used by fold.  */

extern int has_cleanups (tree);

/* Given a tree EXP, a FIELD_DECL F, and a replacement value R,
   return a tree with all occurrences of references to F in a
   PLACEHOLDER_EXPR replaced by R.   Note that we assume here that EXP
   contains only arithmetic expressions.  */

extern tree substitute_in_expr (tree, tree, tree);

/* variable_size (EXP) is like save_expr (EXP) except that it
   is for the special case of something that is part of a
   variable size for a data type.  It makes special arrangements
   to compute the value at the right time when the data type
   belongs to a function parameter.  */

extern tree variable_size (tree);

/* stabilize_reference (EXP) returns a reference equivalent to EXP
   but it can be used multiple times
   and only evaluate the subexpressions once.  */

extern tree stabilize_reference (tree);

/* Subroutine of stabilize_reference; this is called for subtrees of
   references.  Any expression with side-effects must be put in a SAVE_EXPR
   to ensure that it is only evaluated once.  */

extern tree stabilize_reference_1 (tree);

/* Return EXP, stripped of any conversions to wider types
   in such a way that the result of converting to type FOR_TYPE
   is the same as if EXP were converted to FOR_TYPE.
   If FOR_TYPE is 0, it signifies EXP's type.  */

extern tree get_unwidened (tree, tree);

/* Return OP or a simpler expression for a narrower value
   which can be sign-extended or zero-extended to give back OP.
   Store in *UNSIGNEDP_PTR either 1 if the value should be zero-extended
   or 0 if the value should be sign-extended.  */

extern tree get_narrower (tree, int *);

/* Given an expression EXP that may be a COMPONENT_REF or an ARRAY_REF,
   look for nested component-refs or array-refs at constant positions
   and find the ultimate containing object, which is returned.  */

extern tree get_inner_reference (tree, HOST_WIDE_INT *, HOST_WIDE_INT *,
				 tree *, enum machine_mode *, int *, int *);

/* Return 1 if T is an expression that get_inner_reference handles.  */

extern int handled_component_p (tree);

/* Given a DECL or TYPE, return the scope in which it was declared, or
   NUL_TREE if there is no containing scope.  */

extern tree get_containing_scope (tree);

/* Return the FUNCTION_DECL which provides this _DECL with its context,
   or zero if none.  */
extern tree decl_function_context (tree);

/* Return the RECORD_TYPE, UNION_TYPE, or QUAL_UNION_TYPE which provides
   this _DECL with its context, or zero if none.  */
extern tree decl_type_context (tree);

/* Given the FUNCTION_DECL for the current function,
   return zero if it is ok for this function to be inline.
   Otherwise return a warning message with a single %s
   for the function's name.  */

extern const char *function_cannot_inline_p (tree);

/* Return 1 if EXPR is the real constant zero.  */
extern int real_zerop (tree);

/* Declare commonly used variables for tree structure.  */

/* Nonzero means lvalues are limited to those valid in pedantic ANSI C.
   Zero means allow extended lvalues.  */

extern int pedantic_lvalues;

/* Nonzero means can safely call expand_expr now;
   otherwise layout_type puts variable sizes onto `pending_sizes' instead.  */

extern int immediate_size_expand;

/* Points to the FUNCTION_DECL of the function whose body we are reading.  */

extern GTY(()) tree current_function_decl;

/* Nonzero means a FUNC_BEGIN label was emitted.  */
extern GTY(()) tree current_function_func_begin_label;

/* Nonzero means all ..._TYPE nodes should be allocated permanently.  */

extern int all_types_permanent;

/* Exit a binding level.  This function is provided by each language
   frontend.  */
extern tree poplevel (int, int, int);

/* Declare a predefined function.  Return the declaration.  This function is
   provided by each language frontend.  */
extern tree builtin_function (const char *, tree, int, enum built_in_class,
			      const char *, tree);

/* In tree.c */
extern unsigned crc32_string (unsigned, const char *);
extern void clean_symbol_name (char *);
extern tree get_file_function_name_long (const char *);
extern tree get_set_constructor_bits (tree, char *, int);
extern tree get_set_constructor_bytes (tree, unsigned char *, int);
extern tree get_callee_fndecl (tree);
extern void change_decl_assembler_name (tree, tree);
extern int type_num_arguments (tree);
extern tree lhd_unsave_expr_now (tree);


/* In stmt.c */

extern void expand_fixups (rtx);
extern tree expand_start_stmt_expr (int);
extern tree expand_end_stmt_expr (tree);
extern void expand_expr_stmt (tree);
extern void expand_expr_stmt_value (tree, int, int);
extern int warn_if_unused_value (tree);
extern void expand_decl_init (tree);
extern void clear_last_expr (void);
extern void expand_label (tree);
extern void expand_goto (tree);
extern void expand_asm (tree, int);
extern void expand_start_cond (tree, int);
extern void expand_end_cond (void);
extern void expand_start_else (void);
extern void expand_start_elseif (tree);
extern struct nesting *expand_start_loop (int);
extern struct nesting *expand_start_loop_continue_elsewhere (int);
extern struct nesting *expand_start_null_loop (void);
extern void expand_loop_continue_here (void);
extern void expand_end_loop (void);
extern void expand_end_null_loop (void);
extern int expand_continue_loop (struct nesting *);
extern int expand_exit_loop (struct nesting *);
extern int expand_exit_loop_if_false (struct nesting *,tree);
extern int expand_exit_loop_top_cond (struct nesting *, tree);
extern int expand_exit_something (void);

extern void expand_return (tree);
extern int optimize_tail_recursion (tree, rtx);
extern void expand_start_bindings_and_block (int, tree);
#define expand_start_bindings(flags) \
  expand_start_bindings_and_block(flags, NULL_TREE)
extern void expand_end_bindings (tree, int, int);
extern void warn_about_unused_variables (tree);
extern void start_cleanup_deferral (void);
extern void end_cleanup_deferral (void);
extern int is_body_block (tree);

extern int conditional_context (void);
extern struct nesting * current_nesting_level (void);
extern tree last_cleanup_this_contour (void);
extern void expand_start_case (int, tree, tree, const char *);
extern void expand_end_case_type (tree, tree);
#define expand_end_case(cond) expand_end_case_type (cond, NULL)
extern int add_case_node (tree, tree, tree, tree *);
extern int pushcase (tree, tree (*) (tree, tree), tree, tree *);
extern int pushcase_range (tree, tree, tree (*) (tree, tree), tree, tree *);
extern void using_eh_for_cleanups (void);

/* In fold-const.c */

/* Fold constants as much as possible in an expression.
   Returns the simplified expression.
   Acts only on the top level of the expression;
   if the argument itself cannot be simplified, its
   subexpressions are not changed.  */

extern tree fold (tree);
extern tree fold_initializer (tree);
extern tree fold_single_bit_test (enum tree_code, tree, tree, tree);

extern int force_fit_type (tree, int);
extern int add_double (unsigned HOST_WIDE_INT, HOST_WIDE_INT,
		       unsigned HOST_WIDE_INT, HOST_WIDE_INT,
		       unsigned HOST_WIDE_INT *, HOST_WIDE_INT *);
extern int neg_double (unsigned HOST_WIDE_INT, HOST_WIDE_INT,
		       unsigned HOST_WIDE_INT *, HOST_WIDE_INT *);
extern int mul_double (unsigned HOST_WIDE_INT, HOST_WIDE_INT,
		       unsigned HOST_WIDE_INT, HOST_WIDE_INT,
		       unsigned HOST_WIDE_INT *, HOST_WIDE_INT *);
extern void lshift_double (unsigned HOST_WIDE_INT, HOST_WIDE_INT,
			   HOST_WIDE_INT, unsigned int,
			   unsigned HOST_WIDE_INT *, HOST_WIDE_INT *, int);
extern void rshift_double (unsigned HOST_WIDE_INT, HOST_WIDE_INT,
			   HOST_WIDE_INT, unsigned int,
			   unsigned HOST_WIDE_INT *, HOST_WIDE_INT *, int);
extern void lrotate_double (unsigned HOST_WIDE_INT, HOST_WIDE_INT,
			    HOST_WIDE_INT, unsigned int,
			    unsigned HOST_WIDE_INT *, HOST_WIDE_INT *);
extern void rrotate_double (unsigned HOST_WIDE_INT, HOST_WIDE_INT,
			    HOST_WIDE_INT, unsigned int,
			    unsigned HOST_WIDE_INT *, HOST_WIDE_INT *);

extern int div_and_round_double (enum tree_code, int, unsigned HOST_WIDE_INT,
				 HOST_WIDE_INT, unsigned HOST_WIDE_INT,
				 HOST_WIDE_INT, unsigned HOST_WIDE_INT *,
				 HOST_WIDE_INT *, unsigned HOST_WIDE_INT *,
				 HOST_WIDE_INT *);

extern int operand_equal_p (tree, tree, int);
extern tree omit_one_operand (tree, tree, tree);
extern tree invert_truthvalue (tree);

/* In builtins.c */
extern tree fold_builtin (tree);
extern enum built_in_function builtin_mathfn_code (tree);
extern tree build_function_call_expr (tree, tree);
extern tree mathfn_built_in (tree, enum built_in_function fn);

/* In convert.c */
extern tree strip_float_extensions (tree);

/* In alias.c */
extern void record_component_aliases (tree);
extern HOST_WIDE_INT get_alias_set (tree);
extern int alias_sets_conflict_p (HOST_WIDE_INT, HOST_WIDE_INT);
extern int readonly_fields_p (tree);
extern int objects_must_conflict_p (tree, tree);

/* In tree.c */
extern int really_constant_p (tree);
extern int int_fits_type_p (tree, tree);
extern bool variably_modified_type_p (tree);
extern int tree_log2 (tree);
extern int tree_floor_log2 (tree);
extern int simple_cst_equal (tree, tree);
extern unsigned int iterative_hash_expr (tree, unsigned int);
extern int compare_tree_int (tree, unsigned HOST_WIDE_INT);
extern int type_list_equal (tree, tree);
extern int chain_member (tree, tree);
extern tree type_hash_lookup (unsigned int, tree);
extern void type_hash_add (unsigned int, tree);
extern unsigned int type_hash_list (tree);
extern int simple_cst_list_equal (tree, tree);
extern void dump_tree_statistics (void);
extern void expand_function_end (void);
extern void expand_function_start (tree, int);
extern void expand_pending_sizes (tree);
extern tree make_vector (enum machine_mode, tree, int);
extern tree reconstruct_complex_type (tree, tree);

extern int real_onep (tree);
extern int real_twop (tree);
extern int real_minus_onep (tree);
extern void init_ttree (void);
extern void build_common_tree_nodes (int);
extern void build_common_tree_nodes_2 (int);
extern tree build_range_type (tree, tree, tree);

/* In function.c */
extern void setjmp_protect_args (void);
extern void setjmp_protect (tree);
extern void expand_main_function (void);
extern void init_dummy_function_start (void);
extern void expand_dummy_function_end (void);
extern void init_function_for_compilation (void);
extern void allocate_struct_function (tree);
extern void init_function_start (tree);
extern void assign_parms (tree);
extern void put_var_into_stack (tree, int);
extern void flush_addressof (tree);
extern void uninitialized_vars_warning (tree);
extern void setjmp_args_warning (void);
extern void mark_all_temps_used (void);
extern void init_temp_slots (void);
extern void combine_temp_slots (void);
extern void free_temp_slots (void);
extern void pop_temp_slots (void);
extern void push_temp_slots (void);
extern void preserve_temp_slots (rtx);
extern void preserve_rtl_expr_temps (tree);
extern int aggregate_value_p (tree, tree);
extern void free_temps_for_rtl_expr (tree);
extern void instantiate_virtual_regs (tree, rtx);
extern void unshare_all_rtl (tree, rtx);
extern void push_function_context (void);
extern void pop_function_context (void);
extern void push_function_context_to (tree);
extern void pop_function_context_from (tree);

/* In print-rtl.c */
#ifdef BUFSIZ
extern void print_rtl (FILE *, rtx);
#endif

/* In print-tree.c */
extern void debug_tree (tree);
#ifdef BUFSIZ
extern void print_node (FILE *, const char *, tree, int);
extern void print_node_brief (FILE *, const char *, tree, int);
extern void indent_to (FILE *, int);
#endif

/* In expr.c */
extern int apply_args_register_offset (int);
extern rtx expand_builtin_return_addr (enum built_in_function, int, rtx);
extern void check_max_integer_computation_mode (tree);

/* In emit-rtl.c */
extern void start_sequence_for_rtl_expr (tree);
extern rtx emit_line_note (location_t);

/* In calls.c */

/* Nonzero if this is a call to a `const' function.  */
#define ECF_CONST		1
/* Nonzero if this is a call to a `volatile' function.  */
#define ECF_NORETURN		2
/* Nonzero if this is a call to malloc or a related function.  */
#define ECF_MALLOC		4
/* Nonzero if it is plausible that this is a call to alloca.  */
#define ECF_MAY_BE_ALLOCA	8
/* Nonzero if this is a call to a function that won't throw an exception.  */
#define ECF_NOTHROW		16
/* Nonzero if this is a call to setjmp or a related function.  */
#define ECF_RETURNS_TWICE	32
/* Nonzero if this is a call to `longjmp'.  */
#define ECF_LONGJMP		64
/* Nonzero if this is a syscall that makes a new process in the image of
   the current one.  */
#define ECF_FORK_OR_EXEC	128
#define ECF_SIBCALL		256
/* Nonzero if this is a call to "pure" function (like const function,
   but may read memory.  */
#define ECF_PURE		512
/* Nonzero if this is a call to a function that returns with the stack
   pointer depressed.  */
#define ECF_SP_DEPRESSED	1024
/* Nonzero if this call is known to always return.  */
#define ECF_ALWAYS_RETURN	2048
/* Create libcall block around the call.  */
#define ECF_LIBCALL_BLOCK	4096

extern int flags_from_decl_or_type (tree);
extern int call_expr_flags (tree);

extern int setjmp_call_p (tree);
extern bool alloca_call_p (tree);

/* In attribs.c.  */

/* Process the attributes listed in ATTRIBUTES and install them in *NODE,
   which is either a DECL (including a TYPE_DECL) or a TYPE.  If a DECL,
   it should be modified in place; if a TYPE, a copy should be created
   unless ATTR_FLAG_TYPE_IN_PLACE is set in FLAGS.  FLAGS gives further
   information, in the form of a bitwise OR of flags in enum attribute_flags
   from tree.h.  Depending on these flags, some attributes may be
   returned to be applied at a later stage (for example, to apply
   a decl attribute to the declaration rather than to its type).  */
extern tree decl_attributes (tree *, tree, int);

/* In integrate.c */
extern void save_for_inline (tree);
extern void set_decl_abstract_flags (tree, int);
extern void output_inline_function (tree);
extern void set_decl_origin_self (tree);

/* In stor-layout.c */
extern void set_min_and_max_values_for_integral_type (tree, int, bool);
extern void fixup_signed_type (tree);
extern void internal_reference_types (void);

/* varasm.c */
extern void make_decl_rtl (tree, const char *);
extern void make_decl_one_only (tree);
extern int supports_one_only (void);
extern void variable_section (tree, int);
enum tls_model decl_tls_model (tree);
extern void resolve_unique_section (tree, int, int);
extern void mark_referenced (tree);
extern void notice_global_symbol (tree);
extern void process_pending_assemble_output_defs (void);

/* In stmt.c */
extern void emit_nop (void);
extern void expand_computed_goto (tree);
extern bool parse_output_constraint (const char **, int, int, int,
				     bool *, bool *, bool *);
extern bool parse_input_constraint (const char **, int, int, int, int,
				    const char * const *, bool *, bool *);
extern void expand_asm_operands (tree, tree, tree, tree, int, location_t);
extern tree resolve_asm_operand_names (tree, tree, tree);
extern int any_pending_cleanups (void);
extern void init_stmt_for_function (void);
extern void expand_start_target_temps (void);
extern void expand_end_target_temps (void);
extern void expand_elseif (tree);
extern void save_stack_pointer (void);
extern void expand_decl (tree);
extern int expand_decl_cleanup (tree, tree);
extern int expand_decl_cleanup_eh (tree, tree, int);
extern void expand_anon_union_decl (tree, tree, tree);
extern void expand_start_case_dummy (void);
extern HOST_WIDE_INT all_cases_count (tree, int *);
extern void check_for_full_enumeration_handling (tree);
extern void declare_nonlocal_label (tree);

/* If KIND=='I', return a suitable global initializer (constructor) name.
   If KIND=='D', return a suitable global clean-up (destructor) name.  */
extern tree get_file_function_name (int);

/* Interface of the DWARF2 unwind info support.  */

/* Generate a new label for the CFI info to refer to.  */

extern char *dwarf2out_cfi_label (void);

/* Entry point to update the canonical frame address (CFA).  */

extern void dwarf2out_def_cfa (const char *, unsigned, HOST_WIDE_INT);

/* Add the CFI for saving a register window.  */

extern void dwarf2out_window_save (const char *);

/* Add a CFI to update the running total of the size of arguments pushed
   onto the stack.  */

extern void dwarf2out_args_size (const char *, HOST_WIDE_INT);

/* Entry point for saving a register to the stack.  */

extern void dwarf2out_reg_save (const char *, unsigned, HOST_WIDE_INT);

/* Entry point for saving the return address in the stack.  */

extern void dwarf2out_return_save (const char *, HOST_WIDE_INT);

/* Entry point for saving the return address in a register.  */

extern void dwarf2out_return_reg (const char *, unsigned);

/* The type of a function that walks over tree structure.  */

typedef tree (*walk_tree_fn) (tree *, int *, void *);

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

extern int dump_flag (dump_info_p, int, tree);
extern int dump_enabled_p (enum tree_dump_index);
extern FILE *dump_begin (enum tree_dump_index, int *);
extern void dump_end (enum tree_dump_index, FILE *);
extern void dump_node (tree, int, FILE *);
extern int dump_switch_p (const char *);
extern const char *dump_flag_name (enum tree_dump_index);
/* Assign the RTX to declaration.  */

extern void set_decl_rtl (tree, rtx);

/* Redefine abort to report an internal error w/o coredump, and
   reporting the location of the error in the source file.  This logic
   is duplicated in rtl.h and tree.h because every file that needs the
   special abort includes one or both.  toplev.h gets too few files,
   system.h gets too many.  */

extern void fancy_abort (const char *, int, const char *)
    ATTRIBUTE_NORETURN;
#define abort() fancy_abort (__FILE__, __LINE__, __FUNCTION__)

/* Enum and arrays used for tree allocation stats. 
   Keep in sync with tree.c:tree_node_kind_names.  */
typedef enum
{
  d_kind,
  t_kind,
  b_kind,
  s_kind,
  r_kind,
  e_kind,
  c_kind,
  id_kind,
  perm_list_kind,
  temp_list_kind,
  vec_kind,
  x_kind,
  lang_decl,
  lang_type,
  all_kinds
} tree_node_kind;

extern int tree_node_counts[];
extern int tree_node_sizes[];
    
#endif  /* GCC_TREE_H  */
