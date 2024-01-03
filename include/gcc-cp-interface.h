/* Interface between GCC C++ FE and GDB

   Copyright (C) 2014-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef GCC_CP_INTERFACE_H
#define GCC_CP_INTERFACE_H

#include "gcc-interface.h"

/* This header defines the interface to the GCC API.  It must be both
   valid C and valid C++, because it is included by both programs.  */

#ifdef __cplusplus
extern "C" {
#endif

/* Forward declaration.  */

struct gcc_cp_context;

/*
 * Definitions and declarations for the C++ front end.
 */

/* Defined versions of the C++ front-end API.  */

enum gcc_cp_api_version
{
  GCC_CP_FE_VERSION_0 = 0
};

/* Qualifiers.  */

enum gcc_cp_qualifiers
{
  GCC_CP_QUALIFIER_CONST = 1,
  GCC_CP_QUALIFIER_VOLATILE = 2,
  GCC_CP_QUALIFIER_RESTRICT = 4
};

/* Ref qualifiers.  */

enum gcc_cp_ref_qualifiers {
  GCC_CP_REF_QUAL_NONE = 0,
  GCC_CP_REF_QUAL_LVALUE = 1,
  GCC_CP_REF_QUAL_RVALUE = 2
};

/* Opaque typedef for unbound class templates.  They are used for
   template arguments, and defaults for template template
   parameters.  */

typedef unsigned long long gcc_utempl;

/* Opaque typedef for expressions.  They are used for template
   arguments, defaults for non-type template parameters, and defaults
   for function arguments.  */

typedef unsigned long long gcc_expr;

typedef enum
  { GCC_CP_TPARG_VALUE, GCC_CP_TPARG_CLASS,
    GCC_CP_TPARG_TEMPL, GCC_CP_TPARG_PACK }
gcc_cp_template_arg_kind;

typedef union
{ gcc_expr value; gcc_type type; gcc_utempl templ; gcc_type pack; }
gcc_cp_template_arg;

/* An array of template arguments.  */

struct gcc_cp_template_args
{
  /* Number of elements.  */

  int n_elements;

  /* kind[i] indicates what kind of template argument type[i] is.  */

  char /* gcc_cp_template_arg_kind */ *kinds;

  /* The template arguments.  */

  gcc_cp_template_arg *elements;
};

/* An array of (default) function arguments.  */

struct gcc_cp_function_args
{
  /* Number of elements.  */

  int n_elements;

  /* The (default) values for each argument.  */

  gcc_expr *elements;
};

/* This enumerates the kinds of decls that GDB can create.  */

enum gcc_cp_symbol_kind
{
  /* A function.  */

  GCC_CP_SYMBOL_FUNCTION,

  /* A variable.  */

  GCC_CP_SYMBOL_VARIABLE,

  /* A typedef, or an alias declaration (including template ones).  */

  GCC_CP_SYMBOL_TYPEDEF,

  /* A label.  */

  GCC_CP_SYMBOL_LABEL,

  /* A class, forward declared in build_decl (to be later defined in
     start_class_definition), or, in a template parameter list scope,
     a declaration of a template class, closing the parameter
     list.  */

  GCC_CP_SYMBOL_CLASS,

  /* A union, forward declared in build_decl (to be later defined in
     start_class_definition).  */

  GCC_CP_SYMBOL_UNION,

  /* An enumeration type being introduced with start_new_enum_type.  */

  GCC_CP_SYMBOL_ENUM,

  /* A nonstatic data member being introduced with new_field.  */

  GCC_CP_SYMBOL_FIELD,

  /* A base class in a gcc_vbase_array.  */

  GCC_CP_SYMBOL_BASECLASS,

  /* A using declaration in new_using_decl.  */

  GCC_CP_SYMBOL_USING,

  /* A (lambda) closure class type.  In many regards this is just like
     a regular class, but it's not supposed to have base classes, some
     of the member functions that are usually implicitly-defined are
     deleted, and it should have an operator() member function that
     holds the lambda body.  We can't instantiate objects of lambda
     types from the snippet, but we can interact with them in such
     ways as passing them to functions that take their types, and
     calling their body.  */

  GCC_CP_SYMBOL_LAMBDA_CLOSURE,

  /* Marker to check that we haven't exceeded GCC_CP_SYMBOL_MASK.  */
  GCC_CP_SYMBOL_END,

  GCC_CP_SYMBOL_MASK = 15,

  /* When defining a class member, at least one of the
     GCC_CP_ACCESS_MASK bits must be set; when defining a namespace-
     or union-scoped symbol, none of them must be set.  */

  GCC_CP_ACCESS_PRIVATE,
  GCC_CP_ACCESS_PUBLIC = GCC_CP_ACCESS_PRIVATE << 1,
  GCC_CP_ACCESS_MASK = (GCC_CP_ACCESS_PUBLIC
			       | GCC_CP_ACCESS_PRIVATE),
  GCC_CP_ACCESS_PROTECTED = GCC_CP_ACCESS_MASK,
  GCC_CP_ACCESS_NONE = 0,

  GCC_CP_FLAG_BASE = GCC_CP_ACCESS_PRIVATE << 2,

  /* Flags to be used along with GCC_CP_SYMBOL_FUNCTION:  */

  /* This flag should be set for constructors, destructors and
     operators.  */
  GCC_CP_FLAG_SPECIAL_FUNCTION = GCC_CP_FLAG_BASE,

  /* We intentionally cannot express inline, constexpr, or virtual
     override for functions.  We can't inline or constexpr-replace
     without a source-level body.  The override keyword is only
     meaningful within the definition of the containing class.  */

  /* This indicates a "virtual" member function, explicitly or
     implicitly (due to a virtual function with the same name and
     prototype in a base class) declared as such.  */
  GCC_CP_FLAG_VIRTUAL_FUNCTION = GCC_CP_FLAG_BASE << 1,

  /* The following two flags should only be set when the flag above is
     set.  */

  /* This indicates a pure virtual member function, i.e., one that is
     declared with "= 0", even if a body is provided in the
     definition.  */
  GCC_CP_FLAG_PURE_VIRTUAL_FUNCTION = GCC_CP_FLAG_BASE << 2,

  /* This indicates a "final" virtual member function.  */
  GCC_CP_FLAG_FINAL_VIRTUAL_FUNCTION = GCC_CP_FLAG_BASE << 3,

  /* This indicates a special member function should have its default
     implementation.  This either means the function declaration
     contains the "= default" tokens, or that the member function was
     implicitly generated by the compiler, although the latter use is
     discouraged: just let the compiler implicitly introduce it.

     A member function defaulted after its first declaration has
     slightly different ABI implications from one implicitly generated
     or explicitly defaulted at the declaration (and definition)
     point.  To avoid silent (possibly harmless) violation of the one
     definition rule, it is recommended that this flag not be used for
     such functions, and that the address of the definition be
     supplied instead.  */
  GCC_CP_FLAG_DEFAULTED_FUNCTION = GCC_CP_FLAG_BASE << 4,

  /* This indicates a deleted member function, i.e., one that has been
     defined as "= delete" at its declaration point, or one that has
     been implicitly defined as deleted (with or without an explicit
     "= default" definition).

     This should not be used for implicitly-declared member functions
     that resolve to deleted definitions, as it may affect the
     implicit declaration of other member functions.  */
  GCC_CP_FLAG_DELETED_FUNCTION = GCC_CP_FLAG_BASE << 5,

  /* This indicates a constructor or type-conversion operator declared
     as "explicit".  */

  GCC_CP_FLAG_EXPLICIT_FUNCTION = GCC_CP_FLAG_BASE << 6,

  GCC_CP_FLAG_END_FUNCTION,
  GCC_CP_FLAG_MASK_FUNCTION = (((GCC_CP_FLAG_END_FUNCTION - 1) << 1)
			       - GCC_CP_FLAG_BASE),

  /* Flags to be used along with GCC_CP_SYMBOL_VARIABLE:  */

  /* This indicates a variable declared as "constexpr".  */

  GCC_CP_FLAG_CONSTEXPR_VARIABLE = GCC_CP_FLAG_BASE,

  /* This indicates a variable declared as "thread_local".  ??? What
     should the ADDRESS be?  */

  GCC_CP_FLAG_THREAD_LOCAL_VARIABLE = GCC_CP_FLAG_BASE << 1,

  GCC_CP_FLAG_END_VARIABLE,
  GCC_CP_FLAG_MASK_VARIABLE = (((GCC_CP_FLAG_END_VARIABLE - 1) << 1)
			       - GCC_CP_FLAG_BASE),

  /* Flags to be used when defining nonstatic data members of classes
     with new_field.  */

  /* Use this when no flags are present.  */
  GCC_CP_FLAG_FIELD_NOFLAG = 0,

  /* This indicates the field is declared as mutable.  */
  GCC_CP_FLAG_FIELD_MUTABLE = GCC_CP_FLAG_BASE,

  GCC_CP_FLAG_END_FIELD,
  GCC_CP_FLAG_MASK_FIELD = (((GCC_CP_FLAG_END_FIELD - 1) << 1)
			    - GCC_CP_FLAG_BASE),

  /* Flags to be used when defining an enum with
     start_new_enum_type.  */

  /* This indicates an enum type without any flags.  */
  GCC_CP_FLAG_ENUM_NOFLAG = 0,

  /* This indicates a scoped enum type.  */
  GCC_CP_FLAG_ENUM_SCOPED = GCC_CP_FLAG_BASE,

  GCC_CP_FLAG_END_ENUM,
  GCC_CP_FLAG_MASK_ENUM = (((GCC_CP_FLAG_END_ENUM - 1) << 1)
			       - GCC_CP_FLAG_BASE),


  /* Flags to be used when introducing a class or a class template
     with build_decl.  */

  /* This indicates an enum type without any flags.  */
  GCC_CP_FLAG_CLASS_NOFLAG = 0,

  /* This indicates the class is actually a struct.  This has no
     effect whatsoever on access control in this interface, since all
     class members must have explicit access control bits set, but it
     may affect error messages.  */
  GCC_CP_FLAG_CLASS_IS_STRUCT = GCC_CP_FLAG_BASE,

  GCC_CP_FLAG_END_CLASS,
  GCC_CP_FLAG_MASK_CLASS = (((GCC_CP_FLAG_END_CLASS - 1) << 1)
			       - GCC_CP_FLAG_BASE),


  /* Flags to be used when introducing a virtual base class in a
     gcc_vbase_array.  */

  /* This indicates an enum type without any flags.  */
  GCC_CP_FLAG_BASECLASS_NOFLAG = 0,

  /* This indicates the class is actually a struct.  This has no
     effect whatsoever on access control in this interface, since all
     class members must have explicit access control bits set, but it
     may affect error messages.  */
  GCC_CP_FLAG_BASECLASS_VIRTUAL = GCC_CP_FLAG_BASE,

  GCC_CP_FLAG_END_BASECLASS,
  GCC_CP_FLAG_MASK_BASECLASS = (((GCC_CP_FLAG_END_BASECLASS - 1) << 1)
				- GCC_CP_FLAG_BASE),


  GCC_CP_FLAG_MASK = (GCC_CP_FLAG_MASK_FUNCTION
		      | GCC_CP_FLAG_MASK_VARIABLE
		      | GCC_CP_FLAG_MASK_FIELD
		      | GCC_CP_FLAG_MASK_ENUM
		      | GCC_CP_FLAG_MASK_CLASS
		      | GCC_CP_FLAG_MASK_BASECLASS
		      )
};


/* An array of types used for creating lists of base classes.  */

struct gcc_vbase_array
{
  /* Number of elements.  */

  int n_elements;

  /* The base classes.  */

  gcc_type *elements;

  /* Flags for each base class.  Used to indicate access control and
     virtualness.  */

  enum gcc_cp_symbol_kind *flags;
};


/* This enumerates the types of symbols that GCC might request from
   GDB.  */

enum gcc_cp_oracle_request
{
  /* An identifier in namespace scope -- type, variable, function,
     namespace, template.  All namespace-scoped symbols with the
     requested name, in any namespace (including the global
     namespace), should be defined in response to this request.  */

  GCC_CP_ORACLE_IDENTIFIER
};

/* The type of the function called by GCC to ask GDB for a symbol's
   definition.  DATUM is an arbitrary value supplied when the oracle
   function is registered.  CONTEXT is the GCC context in which the
   request is being made.  REQUEST specifies what sort of symbol is
   being requested, and IDENTIFIER is the name of the symbol.  */

typedef void gcc_cp_oracle_function (void *datum,
				     struct gcc_cp_context *context,
				     enum gcc_cp_oracle_request request,
				     const char *identifier);

/* The type of the function called by GCC to ask GDB for a symbol's
   address.  This should return 0 if the address is not known.  */

typedef gcc_address gcc_cp_symbol_address_function (void *datum,
						    struct gcc_cp_context *ctxt,
						    const char *identifier);

/* The type of the function called by GCC to ask GDB to enter or leave
   the user expression scope.  */

typedef void gcc_cp_enter_leave_user_expr_scope_function (void *datum,
							  struct gcc_cp_context
							  *context);

/* The vtable used by the C front end.  */

struct gcc_cp_fe_vtable
{
  /* The version of the C interface.  The value is one of the
     gcc_cp_api_version constants.  */

  unsigned int cp_version;

  /* Set the callbacks for this context.

     The binding oracle is called whenever the C++ parser needs to
     look up a symbol.  This gives the caller a chance to lazily
     instantiate symbols using other parts of the gcc_cp_fe_interface
     API.  The symbol is looked up without a scope, and the oracle
     must supply a definition for ALL namespace-scoped definitions
     bound to the symbol.

     The address oracle is called whenever the C++ parser needs to
     look up a symbol.  This may be called for symbols not provided by
     the symbol oracle, such as built-in functions where GCC provides
     the declaration; other internal symbols, such as those related
     with thunks, rtti, and virtual tables are likely to be queried
     through this interface too.  The identifier is a mangled symbol
     name.

     DATUM is an arbitrary piece of data that is passed back verbatim
     to the callbacks in requests.  */

  void (*set_callbacks) (struct gcc_cp_context *self,
			 gcc_cp_oracle_function *binding_oracle,
			 gcc_cp_symbol_address_function *address_oracle,
			 gcc_cp_enter_leave_user_expr_scope_function *enter_scope,
			 gcc_cp_enter_leave_user_expr_scope_function *leave_scope,
			 void *datum);

#define GCC_METHOD0(R, N) \
  R (*N) (struct gcc_cp_context *);
#define GCC_METHOD1(R, N, A) \
  R (*N) (struct gcc_cp_context *, A);
#define GCC_METHOD2(R, N, A, B) \
  R (*N) (struct gcc_cp_context *, A, B);
#define GCC_METHOD3(R, N, A, B, C) \
  R (*N) (struct gcc_cp_context *, A, B, C);
#define GCC_METHOD4(R, N, A, B, C, D) \
  R (*N) (struct gcc_cp_context *, A, B, C, D);
#define GCC_METHOD5(R, N, A, B, C, D, E) \
  R (*N) (struct gcc_cp_context *, A, B, C, D, E);
#define GCC_METHOD7(R, N, A, B, C, D, E, F, G) \
  R (*N) (struct gcc_cp_context *, A, B, C, D, E, F, G);

#include "gcc-cp-fe.def"

#undef GCC_METHOD0
#undef GCC_METHOD1
#undef GCC_METHOD2
#undef GCC_METHOD3
#undef GCC_METHOD4
#undef GCC_METHOD5
#undef GCC_METHOD7

};

/* The C front end object.  */

struct gcc_cp_context
{
  /* Base class.  */

  struct gcc_base_context base;

  /* Our vtable.  This is a separate field because this is simpler
     than implementing a vtable inheritance scheme in C.  */

  const struct gcc_cp_fe_vtable *cp_ops;
};

/* The name of the .so that the compiler builds.  We dlopen this
   later.  */

#define GCC_CP_FE_LIBCC libcc1.so

/* The compiler exports a single initialization function.  This macro
   holds its name as a symbol.  */

#define GCC_CP_FE_CONTEXT gcc_cp_fe_context

/* The type of the initialization function.  The caller passes in the
   desired base version and desired C-specific version.  If the
   request can be satisfied, a compatible gcc_context object will be
   returned.  Otherwise, the function returns NULL.  */

typedef struct gcc_cp_context *gcc_cp_fe_context_function
    (enum gcc_base_api_version,
     enum gcc_cp_api_version);

#ifdef __cplusplus
}
#endif

#endif /* GCC_CP_INTERFACE_H */
