/* Demangler for g++ V3 ABI.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Written by Ian Lance Taylor <ian@wasabisystems.com>.

   This file is part of the libiberty library, which is part of GCC.

   This file is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   In addition to the permissions in the GNU General Public License, the
   Free Software Foundation gives you unlimited permission to link the
   compiled version of this file into combinations with other programs,
   and to distribute those combinations without any restriction coming
   from the use of this file.  (The General Public License restrictions
   do apply in other respects; for example, they cover modification of
   the file, and distribution when not linked into a combined
   executable.)

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. 
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "ansidecl.h"
#include "libiberty.h"
#include "demangle.h"

/* This code implements a demangler for the g++ V3 ABI.  The ABI is
   described on this web page:
       http://www.codesourcery.com/cxx-abi/abi.html#mangling

   This code was written while looking at the demangler written by
   Alex Samuel <samuel@codesourcery.com>.

   This code first pulls the mangled name apart into a list of
   components, and then walks the list generating the demangled
   name.  */

/* Avoid pulling in the ctype tables for this simple usage.  */
#define IS_DIGIT(c) ((c) >= '0' && (c) <= '9')

/* The prefix prepended by GCC to an identifier represnting the
   anonymous namespace.  */
#define ANONYMOUS_NAMESPACE_PREFIX "_GLOBAL_"
#define ANONYMOUS_NAMESPACE_PREFIX_LEN \
  (sizeof (ANONYMOUS_NAMESPACE_PREFIX) - 1)

/* Information we keep for operators.  */

struct d_operator_info
{
  /* Mangled name.  */
  const char *code;
  /* Real name.  */
  const char *name;
  /* Number of arguments.  */
  int args;
};

/* How to print the value of a builtin type.  */

enum d_builtin_type_print
{
  /* Print as (type)val.  */
  D_PRINT_DEFAULT,
  /* Print as integer.  */
  D_PRINT_INT,
  /* Print as long, with trailing `l'.  */
  D_PRINT_LONG,
  /* Print as bool.  */
  D_PRINT_BOOL,
  /* Print in usual way, but here to detect void.  */
  D_PRINT_VOID
};

/* Information we keep for a builtin type.  */

struct d_builtin_type_info
{
  /* Type name.  */
  const char *name;
  /* Type name when using Java.  */
  const char *java_name;
  /* How to print a value of this type.  */
  enum d_builtin_type_print print;
};

/* Component types found in mangled names.  */

enum d_comp_type
{
  /* A name.  */
  D_COMP_NAME,
  /* A qualified name.  */
  D_COMP_QUAL_NAME,
  /* A typed name.  */
  D_COMP_TYPED_NAME,
  /* A template.  */
  D_COMP_TEMPLATE,
  /* A template parameter.  */
  D_COMP_TEMPLATE_PARAM,
  /* A constructor.  */
  D_COMP_CTOR,
  /* A destructor.  */
  D_COMP_DTOR,
  /* A vtable.  */
  D_COMP_VTABLE,
  /* A VTT structure.  */
  D_COMP_VTT,
  /* A construction vtable.  */
  D_COMP_CONSTRUCTION_VTABLE,
  /* A typeinfo structure.  */
  D_COMP_TYPEINFO,
  /* A typeinfo name.  */
  D_COMP_TYPEINFO_NAME,
  /* A typeinfo function.  */
  D_COMP_TYPEINFO_FN,
  /* A thunk.  */
  D_COMP_THUNK,
  /* A virtual thunk.  */
  D_COMP_VIRTUAL_THUNK,
  /* A covariant thunk.  */
  D_COMP_COVARIANT_THUNK,
  /* A Java class.  */
  D_COMP_JAVA_CLASS,
  /* A guard variable.  */
  D_COMP_GUARD,
  /* A reference temporary.  */
  D_COMP_REFTEMP,
  /* A standard substitution.  */
  D_COMP_SUB_STD,
  /* The restrict qualifier.  */
  D_COMP_RESTRICT,
  /* The volatile qualifier.  */
  D_COMP_VOLATILE,
  /* The const qualifier.  */
  D_COMP_CONST,
  /* A vendor qualifier.  */
  D_COMP_VENDOR_TYPE_QUAL,
  /* A pointer.  */
  D_COMP_POINTER,
  /* A reference.  */
  D_COMP_REFERENCE,
  /* A complex type.  */
  D_COMP_COMPLEX,
  /* An imaginary type.  */
  D_COMP_IMAGINARY,
  /* A builtin type.  */
  D_COMP_BUILTIN_TYPE,
  /* A vendor's builtin type.  */
  D_COMP_VENDOR_TYPE,
  /* A function type.  */
  D_COMP_FUNCTION_TYPE,
  /* An array type.  */
  D_COMP_ARRAY_TYPE,
  /* A pointer to member type.  */
  D_COMP_PTRMEM_TYPE,
  /* An argument list.  */
  D_COMP_ARGLIST,
  /* A template argument list.  */
  D_COMP_TEMPLATE_ARGLIST,
  /* An operator.  */
  D_COMP_OPERATOR,
  /* An extended operator.  */
  D_COMP_EXTENDED_OPERATOR,
  /* A typecast.  */
  D_COMP_CAST,
  /* A unary expression.  */
  D_COMP_UNARY,
  /* A binary expression.  */
  D_COMP_BINARY,
  /* Arguments to a binary expression.  */
  D_COMP_BINARY_ARGS,
  /* A trinary expression.  */
  D_COMP_TRINARY,
  /* Arguments to a trinary expression.  */
  D_COMP_TRINARY_ARG1,
  D_COMP_TRINARY_ARG2,
  /* A literal.  */
  D_COMP_LITERAL
};

/* A component of the mangled name.  */

struct d_comp
{
  /* The type of this component.  */
  enum d_comp_type type;
  union
  {
    /* For D_COMP_NAME.  */
    struct
    {
      /* A pointer to the name (not NULL terminated) and it's
	 length.  */
      const char *s;
      int len;
    } s_name;

    /* For D_COMP_OPERATOR.  */
    struct
    {
      /* Operator.  */
      const struct d_operator_info *op;
    } s_operator;

    /* For D_COMP_EXTENDED_OPERATOR.  */
    struct
    {
      /* Number of arguments.  */
      int args;
      /* Name.  */
      struct d_comp *name;
    } s_extended_operator;

    /* For D_COMP_CTOR.  */
    struct
    {
      enum gnu_v3_ctor_kinds kind;
      struct d_comp *name;
    } s_ctor;

    /* For D_COMP_DTOR.  */
    struct
    {
      enum gnu_v3_dtor_kinds kind;
      struct d_comp *name;
    } s_dtor;

    /* For D_COMP_BUILTIN_TYPE.  */
    struct
    {
      const struct d_builtin_type_info *type;
    } s_builtin;

    /* For D_COMP_SUB_STD.  */
    struct
    {
      const char* string;
    } s_string;

    /* For D_COMP_TEMPLATE_PARAM.  */
    struct
    {
      long number;
    } s_number;

    /* For other types.  */
    struct
    {
      struct d_comp *left;
      struct d_comp *right;
    } s_binary;

  } u;
};

#define d_left(dc) ((dc)->u.s_binary.left)
#define d_right(dc) ((dc)->u.s_binary.right)

/* The information structure we pass around.  */

struct d_info
{
  /* The string we are demangling.  */
  const char *s;
  /* The options passed to the demangler.  */
  int options;
  /* The next character in the string to consider.  */
  const char *n;
  /* The array of components.  */
  struct d_comp *comps;
  /* The index of the next available component.  */
  int next_comp;
  /* The number of available component structures.  */
  int num_comps;
  /* The array of substitutions.  */
  struct d_comp **subs;
  /* The index of the next substitution.  */
  int next_sub;
  /* The number of available entries in the subs array.  */
  int num_subs;
  /* The last name we saw, for constructors and destructors.  */
  struct d_comp *last_name;
};

#define d_peek_char(di) (*((di)->n))
#define d_peek_next_char(di) ((di)->n[1])
#define d_advance(di, i) ((di)->n += (i))
#define d_next_char(di) (*((di)->n++))
#define d_str(di) ((di)->n)

/* A list of templates.  This is used while printing.  */

struct d_print_template
{
  /* Next template on the list.  */
  struct d_print_template *next;
  /* This template.  */
  const struct d_comp *template;
};

/* A list of type modifiers.  This is used while printing.  */

struct d_print_mod
{
  /* Next modifier on the list.  These are in the reverse of the order
     in which they appeared in the mangled string.  */
  struct d_print_mod *next;
  /* The modifier.  */
  const struct d_comp *mod;
  /* Whether this modifier was printed.  */
  int printed;
};

/* We use this structure to hold information during printing.  */

struct d_print_info
{
  /* The options passed to the demangler.  */
  int options;
  /* Buffer holding the result.  */
  char *buf;
  /* Current length of data in buffer.  */
  size_t len;
  /* Allocated size of buffer.  */
  size_t alc;
  /* The current list of templates, if any.  */
  struct d_print_template *templates;
  /* The current list of modifiers (e.g., pointer, reference, etc.),
     if any.  */
  struct d_print_mod *modifiers;
  /* Set to 1 if we had a memory allocation failure.  */
  int allocation_failure;
};

#define d_print_saw_error(dpi) ((dpi)->buf == NULL)

#define d_append_char(dpi, c) \
  do \
    { \
      if ((dpi)->buf != NULL && (dpi)->len < (dpi)->alc) \
        (dpi)->buf[(dpi)->len++] = (c); \
      else \
        d_print_append_char ((dpi), (c)); \
    } \
  while (0)

#define d_append_buffer(dpi, s, l) \
  do \
    { \
      if ((dpi)->buf != NULL && (dpi)->len + (l) <= (dpi)->alc) \
        { \
          memcpy ((dpi)->buf + (dpi)->len, (s), (l)); \
          (dpi)->len += l; \
        } \
      else \
        d_print_append_buffer ((dpi), (s), (l)); \
    } \
  while (0)

#define d_append_string(dpi, s) \
  do \
    { \
      size_t d_append_string_len = strlen (s); \
      d_append_buffer ((dpi), (s), d_append_string_len); \
    } \
  while (0)

#ifdef CP_DEMANGLE_DEBUG
static void d_dump PARAMS ((struct d_comp *, int));
#endif
static struct d_comp *d_make_empty PARAMS ((struct d_info *,
					    enum d_comp_type));
static struct d_comp *d_make_comp PARAMS ((struct d_info *, enum d_comp_type,
					   struct d_comp *, struct d_comp *));
static struct d_comp *d_make_name PARAMS ((struct d_info *, const char *,
					   int));
static struct d_comp *d_make_builtin_type PARAMS ((struct d_info *,
						   const struct d_builtin_type_info *));
static struct d_comp *d_make_operator PARAMS ((struct d_info *,
					       const struct d_operator_info *));
static struct d_comp *d_make_extended_operator PARAMS ((struct d_info *,
							int,
							struct d_comp *));
static struct d_comp *d_make_ctor PARAMS ((struct d_info *,
					   enum gnu_v3_ctor_kinds,
					   struct d_comp *));
static struct d_comp *d_make_dtor PARAMS ((struct d_info *,
					   enum gnu_v3_dtor_kinds,
					   struct d_comp *));
static struct d_comp *d_make_template_param PARAMS ((struct d_info *, long));
static struct d_comp *d_make_sub PARAMS ((struct d_info *, const char *));
static struct d_comp *d_mangled_name PARAMS ((struct d_info *));
static int has_return_type PARAMS ((struct d_comp *));
static int is_ctor_dtor_or_conversion PARAMS ((struct d_comp *));
static struct d_comp *d_encoding PARAMS ((struct d_info *, int));
static struct d_comp *d_name PARAMS ((struct d_info *));
static struct d_comp *d_nested_name PARAMS ((struct d_info *));
static struct d_comp *d_prefix PARAMS ((struct d_info *));
static struct d_comp *d_unqualified_name PARAMS ((struct d_info *));
static struct d_comp *d_source_name PARAMS ((struct d_info *));
static long d_number PARAMS ((struct d_info *));
static struct d_comp *d_identifier PARAMS ((struct d_info *, int));
static struct d_comp *d_operator_name PARAMS ((struct d_info *));
static struct d_comp *d_special_name PARAMS ((struct d_info *));
static int d_call_offset PARAMS ((struct d_info *, int));
static struct d_comp *d_ctor_dtor_name PARAMS ((struct d_info *));
static struct d_comp *d_type PARAMS ((struct d_info *));
static struct d_comp **d_cv_qualifiers PARAMS ((struct d_info *,
						struct d_comp **));
static struct d_comp *d_function_type PARAMS ((struct d_info *));
static struct d_comp *d_bare_function_type PARAMS ((struct d_info *, int));
static struct d_comp *d_class_enum_type PARAMS ((struct d_info *));
static struct d_comp *d_array_type PARAMS ((struct d_info *));
static struct d_comp *d_pointer_to_member_type PARAMS ((struct d_info *));
static struct d_comp *d_template_param PARAMS ((struct d_info *));
static struct d_comp *d_template_args PARAMS ((struct d_info *));
static struct d_comp *d_template_arg PARAMS ((struct d_info *));
static struct d_comp *d_expression PARAMS ((struct d_info *));
static struct d_comp *d_expr_primary PARAMS ((struct d_info *));
static struct d_comp *d_local_name PARAMS ((struct d_info *));
static int d_discriminator PARAMS ((struct d_info *));
static int d_add_substitution PARAMS ((struct d_info *, struct d_comp *));
static struct d_comp *d_substitution PARAMS ((struct d_info *));
static void d_print_resize PARAMS ((struct d_print_info *, size_t));
static void d_print_append_char PARAMS ((struct d_print_info *, int));
static void d_print_append_buffer PARAMS ((struct d_print_info *, const char *,
					   size_t));
static void d_print_error PARAMS ((struct d_print_info *));
static char *d_print PARAMS ((int, const struct d_comp *, size_t *));
static void d_print_comp PARAMS ((struct d_print_info *,
				  const struct d_comp *));
static void d_print_identifier PARAMS ((struct d_print_info *, const char *,
					int));
static void d_print_mod_list PARAMS ((struct d_print_info *,
				      struct d_print_mod *));
static void d_print_mod PARAMS ((struct d_print_info *,
				 const struct d_comp *));
static void d_print_function_type PARAMS ((struct d_print_info *,
					   const struct d_comp *,
					   struct d_print_mod *));
static void d_print_array_type PARAMS ((struct d_print_info *,
					const struct d_comp *,
					struct d_print_mod *));
static void d_print_expr_op PARAMS ((struct d_print_info *,
				     const struct d_comp *));
static void d_print_cast PARAMS ((struct d_print_info *,
				  const struct d_comp *));
static int d_init_info PARAMS ((const char *, int, size_t, struct d_info *));
static char *d_demangle PARAMS ((const char *, int, size_t *));

#ifdef CP_DEMANGLE_DEBUG

static void
d_dump (dc, indent)
     struct d_comp *dc;
     int indent;
{
  int i;

  if (dc == NULL)
    return;

  for (i = 0; i < indent; ++i)
    putchar (' ');

  switch (dc->type)
    {
    case D_COMP_NAME:
      printf ("name '%.*s'\n", dc->u.s_name.len, dc->u.s_name.s);
      return;
    case D_COMP_TEMPLATE_PARAM:
      printf ("template parameter %ld\n", dc->u.s_number.number);
      return;
    case D_COMP_CTOR:
      printf ("constructor %d\n", (int) dc->u.s_ctor.kind);
      d_dump (dc->u.s_ctor.name, indent + 2);
      return;
    case D_COMP_DTOR:
      printf ("destructor %d\n", (int) dc->u.s_dtor.kind);
      d_dump (dc->u.s_dtor.name, indent + 2);
      return;
    case D_COMP_SUB_STD:
      printf ("standard substitution %s\n", dc->u.s_string.string);
      return;
    case D_COMP_BUILTIN_TYPE:
      printf ("builtin type %s\n", dc->u.s_builtin.type->name);
      return;
    case D_COMP_OPERATOR:
      printf ("operator %s\n", dc->u.s_operator.op->name);
      return;
    case D_COMP_EXTENDED_OPERATOR:
      printf ("extended operator with %d args\n",
	      dc->u.s_extended_operator.args);
      d_dump (dc->u.s_extended_operator.name, indent + 2);
      return;

    case D_COMP_QUAL_NAME:
      printf ("qualified name\n");
      break;
    case D_COMP_TYPED_NAME:
      printf ("typed name\n");
      break;
    case D_COMP_TEMPLATE:
      printf ("template\n");
      break;
    case D_COMP_VTABLE:
      printf ("vtable\n");
      break;
    case D_COMP_VTT:
      printf ("VTT\n");
      break;
    case D_COMP_CONSTRUCTION_VTABLE:
      printf ("construction vtable\n");
      break;
    case D_COMP_TYPEINFO:
      printf ("typeinfo\n");
      break;
    case D_COMP_TYPEINFO_NAME:
      printf ("typeinfo name\n");
      break;
    case D_COMP_TYPEINFO_FN:
      printf ("typeinfo function\n");
      break;
    case D_COMP_THUNK:
      printf ("thunk\n");
      break;
    case D_COMP_VIRTUAL_THUNK:
      printf ("virtual thunk\n");
      break;
    case D_COMP_COVARIANT_THUNK:
      printf ("covariant thunk\n");
      break;
    case D_COMP_JAVA_CLASS:
      printf ("java class\n");
      break;
    case D_COMP_GUARD:
      printf ("guard\n");
      break;
    case D_COMP_REFTEMP:
      printf ("reference temporary\n");
      break;
    case D_COMP_RESTRICT:
      printf ("restrict\n");
      break;
    case D_COMP_VOLATILE:
      printf ("volatile\n");
      break;
    case D_COMP_CONST:
      printf ("const\n");
      break;
    case D_COMP_VENDOR_TYPE_QUAL:
      printf ("vendor type qualifier\n");
      break;
    case D_COMP_POINTER:
      printf ("pointer\n");
      break;
    case D_COMP_REFERENCE:
      printf ("reference\n");
      break;
    case D_COMP_COMPLEX:
      printf ("complex\n");
      break;
    case D_COMP_IMAGINARY:
      printf ("imaginary\n");
      break;
    case D_COMP_VENDOR_TYPE:
      printf ("vendor type\n");
      break;
    case D_COMP_FUNCTION_TYPE:
      printf ("function type\n");
      break;
    case D_COMP_ARRAY_TYPE:
      printf ("array type\n");
      break;
    case D_COMP_PTRMEM_TYPE:
      printf ("pointer to member type\n");
      break;
    case D_COMP_ARGLIST:
      printf ("argument list\n");
      break;
    case D_COMP_TEMPLATE_ARGLIST:
      printf ("template argument list\n");
      break;
    case D_COMP_CAST:
      printf ("cast\n");
      break;
    case D_COMP_UNARY:
      printf ("unary operator\n");
      break;
    case D_COMP_BINARY:
      printf ("binary operator\n");
      break;
    case D_COMP_BINARY_ARGS:
      printf ("binary operator arguments\n");
      break;
    case D_COMP_TRINARY:
      printf ("trinary operator\n");
      break;
    case D_COMP_TRINARY_ARG1:
      printf ("trinary operator arguments 1\n");
      break;
    case D_COMP_TRINARY_ARG2:
      printf ("trinary operator arguments 1\n");
      break;
    case D_COMP_LITERAL:
      printf ("literal\n");
      break;
    }

  d_dump (d_left (dc), indent + 2);
  d_dump (d_right (dc), indent + 2);
}

#endif /* CP_DEMANGLE_DEBUG */

/* Add a new component.  */

static struct d_comp *
d_make_empty (di, type)
     struct d_info *di;
     enum d_comp_type type;
{
  struct d_comp *p;

  if (di->next_comp >= di->num_comps)
    return NULL;
  p = &di->comps[di->next_comp];
  p->type = type;
  ++di->next_comp;
  return p;
}

/* Add a new generic component.  */

static struct d_comp *
d_make_comp (di, type, left, right)
     struct d_info *di;
     enum d_comp_type type;
     struct d_comp *left;
     struct d_comp *right;
{
  struct d_comp *p;

  /* We check for errors here.  A typical error would be a NULL return
     from a subroutine.  We catch here, and return NULL on upward.  */
  switch (type)
    {
      /* These types require two parameters.  */
    case D_COMP_QUAL_NAME:
    case D_COMP_TYPED_NAME:
    case D_COMP_TEMPLATE:
    case D_COMP_VENDOR_TYPE_QUAL:
    case D_COMP_PTRMEM_TYPE:
    case D_COMP_UNARY:
    case D_COMP_BINARY:
    case D_COMP_BINARY_ARGS:
    case D_COMP_TRINARY:
    case D_COMP_TRINARY_ARG1:
    case D_COMP_TRINARY_ARG2:
    case D_COMP_LITERAL:
      if (left == NULL || right == NULL)
	return NULL;
      break;

      /* These types only require one parameter.  */
    case D_COMP_VTABLE:
    case D_COMP_VTT:
    case D_COMP_CONSTRUCTION_VTABLE:
    case D_COMP_TYPEINFO:
    case D_COMP_TYPEINFO_NAME:
    case D_COMP_TYPEINFO_FN:
    case D_COMP_THUNK:
    case D_COMP_VIRTUAL_THUNK:
    case D_COMP_COVARIANT_THUNK:
    case D_COMP_JAVA_CLASS:
    case D_COMP_GUARD:
    case D_COMP_REFTEMP:
    case D_COMP_POINTER:
    case D_COMP_REFERENCE:
    case D_COMP_COMPLEX:
    case D_COMP_IMAGINARY:
    case D_COMP_VENDOR_TYPE:
    case D_COMP_ARGLIST:
    case D_COMP_TEMPLATE_ARGLIST:
    case D_COMP_CAST:
      if (left == NULL)
	return NULL;
      break;

      /* This needs a right parameter, but the left parameter can be
	 empty.  */
    case D_COMP_ARRAY_TYPE:
      if (right == NULL)
	return NULL;
      break;

      /* These are allowed to have no parameters--in some cases they
	 will be filled in later.  */
    case D_COMP_FUNCTION_TYPE:
    case D_COMP_RESTRICT:
    case D_COMP_VOLATILE:
    case D_COMP_CONST:
      break;

      /* Other types should not be seen here.  */
    default:
      return NULL;
    }

  p = d_make_empty (di, type);
  if (p != NULL)
    {
      p->u.s_binary.left = left;
      p->u.s_binary.right = right;
    }
  return p;
}

/* Add a new name component.  */

static struct d_comp *
d_make_name (di, s, len)
     struct d_info *di;
     const char *s;
     int len;
{
  struct d_comp *p;

  p = d_make_empty (di, D_COMP_NAME);
  if (p != NULL)
    {
      p->u.s_name.s = s;
      p->u.s_name.len = len;
    }
  return p;
}

/* Add a new builtin type component.  */

static struct d_comp *
d_make_builtin_type (di, type)
     struct d_info *di;
     const struct d_builtin_type_info *type;
{
  struct d_comp *p;

  p = d_make_empty (di, D_COMP_BUILTIN_TYPE);
  if (p != NULL)
    p->u.s_builtin.type = type;
  return p;
}

/* Add a new operator component.  */

static struct d_comp *
d_make_operator (di, op)
     struct d_info *di;
     const struct d_operator_info *op;
{
  struct d_comp *p;

  p = d_make_empty (di, D_COMP_OPERATOR);
  if (p != NULL)
    p->u.s_operator.op = op;
  return p;
}

/* Add a new extended operator component.  */

static struct d_comp *
d_make_extended_operator (di, args, name)
     struct d_info *di;
     int args;
     struct d_comp *name;
{
  struct d_comp *p;

  p = d_make_empty (di, D_COMP_EXTENDED_OPERATOR);
  if (p != NULL)
    {
      p->u.s_extended_operator.args = args;
      p->u.s_extended_operator.name = name;
    }
  return p;
}

/* Add a new constructor component.  */

static struct d_comp *
d_make_ctor (di, kind,  name)
     struct d_info *di;
     enum gnu_v3_ctor_kinds kind;
     struct d_comp *name;
{
  struct d_comp *p;

  p = d_make_empty (di, D_COMP_CTOR);
  if (p != NULL)
    {
      p->u.s_ctor.kind = kind;
      p->u.s_ctor.name = name;
    }
  return p;
}

/* Add a new destructor component.  */

static struct d_comp *
d_make_dtor (di, kind, name)
     struct d_info *di;
     enum gnu_v3_dtor_kinds kind;
     struct d_comp *name;
{
  struct d_comp *p;

  p = d_make_empty (di, D_COMP_DTOR);
  if (p != NULL)
    {
      p->u.s_dtor.kind = kind;
      p->u.s_dtor.name = name;
    }
  return p;
}

/* Add a new template parameter.  */

static struct d_comp *
d_make_template_param (di, i)
     struct d_info *di;
     long i;
{
  struct d_comp *p;

  p = d_make_empty (di, D_COMP_TEMPLATE_PARAM);
  if (p != NULL)
    p->u.s_number.number = i;
  return p;
}

/* Add a new standard substitution component.  */

static struct d_comp *
d_make_sub (di, name)
     struct d_info *di;
     const char *name;
{
  struct d_comp *p;

  p = d_make_empty (di, D_COMP_SUB_STD);
  if (p != NULL)
    p->u.s_string.string = name;
  return p;
}

/* <mangled-name> ::= _Z <encoding> */

static struct d_comp *
d_mangled_name (di)
     struct d_info *di;
{
  if (d_next_char (di) != '_')
    return NULL;
  if (d_next_char (di) != 'Z')
    return NULL;
  return d_encoding (di, 1);
}

/* Return whether a function should have a return type.  The argument
   is the function name, which may be qualified in various ways.  The
   rules are that template functions have return types with some
   exceptions, function types which are not part of a function name
   mangling have return types with some exceptions, and non-template
   function names do not have return types.  The exceptions are that
   constructors, destructors, and conversion operators do not have
   return types.  */

static int
has_return_type (dc)
     struct d_comp *dc;
{
  if (dc == NULL)
    return 0;
  switch (dc->type)
    {
    default:
      return 0;
    case D_COMP_TEMPLATE:
      return ! is_ctor_dtor_or_conversion (d_left (dc));
    case D_COMP_RESTRICT:
    case D_COMP_VOLATILE:
    case D_COMP_CONST:
    case D_COMP_VENDOR_TYPE_QUAL:
      return has_return_type (d_left (dc));
    }
}

/* Return whether a name is a constructor, a destructor, or a
   conversion operator.  */

static int
is_ctor_dtor_or_conversion (dc)
     struct d_comp *dc;
{
  if (dc == NULL)
    return 0;
  switch (dc->type)
    {
    default:
      return 0;
    case D_COMP_QUAL_NAME:
      return is_ctor_dtor_or_conversion (d_right (dc));
    case D_COMP_CTOR:
    case D_COMP_DTOR:
    case D_COMP_CAST:
      return 1;
    }
}

/* <encoding> ::= <(function) name> <bare-function-type>
              ::= <(data) name>
              ::= <special-name>

   TOP_LEVEL is non-zero when called at the top level, in which case
   if DMGL_PARAMS is not set we do not demangle the function
   parameters.  We only set this at the top level, because otherwise
   we would not correctly demangle names in local scopes.  */

static struct d_comp *
d_encoding (di, top_level)
     struct d_info *di;
     int top_level;
{
  char peek = d_peek_char (di);

  if (peek == 'G' || peek == 'T')
    return d_special_name (di);
  else
    {
      struct d_comp *dc;

      dc = d_name (di);
      peek = d_peek_char (di);
      if (peek == '\0'
	  || peek == 'E'
	  || (top_level && (di->options & DMGL_PARAMS) == 0))
	return dc;
      return d_make_comp (di, D_COMP_TYPED_NAME, dc,
			  d_bare_function_type (di, has_return_type (dc)));
    }
}

/* <name> ::= <nested-name>
          ::= <unscoped-name>
          ::= <unscoped-template-name> <template-args>
          ::= <local-name>

   <unscoped-name> ::= <unqualified-name>
                   ::= St <unqualified-name>

   <unscoped-template-name> ::= <unscoped-name>
                            ::= <substitution>
*/

static struct d_comp *
d_name (di)
     struct d_info *di;
{
  char peek = d_peek_char (di);
  struct d_comp *dc;

  switch (peek)
    {
    case 'N':
      return d_nested_name (di);

    case 'Z':
      return d_local_name (di);

    case 'S':
      {
	int subst;

	if (d_peek_next_char (di) != 't')
	  {
	    dc = d_substitution (di);
	    subst = 1;
	  }
	else
	  {
	    d_advance (di, 2);
	    dc = d_make_comp (di, D_COMP_QUAL_NAME, d_make_name (di, "std", 3),
			      d_unqualified_name (di));
	    subst = 0;
	  }

	if (d_peek_char (di) != 'I')
	  {
	    /* The grammar does not permit this case to occur if we
	       called d_substitution() above (i.e., subst == 1).  We
	       don't bother to check.  */
	  }
	else
	  {
	    /* This is <template-args>, which means that we just saw
	       <unscoped-template-name>, which is a substitution
	       candidate if we didn't just get it from a
	       substitution.  */
	    if (! subst)
	      {
		if (! d_add_substitution (di, dc))
		  return NULL;
	      }
	    dc = d_make_comp (di, D_COMP_TEMPLATE, dc, d_template_args (di));
	  }

	return dc;
      }

    default:
      dc = d_unqualified_name (di);
      if (d_peek_char (di) == 'I')
	{
	  /* This is <template-args>, which means that we just saw
	     <unscoped-template-name>, which is a substitution
	     candidate.  */
	  if (! d_add_substitution (di, dc))
	    return NULL;
	  dc = d_make_comp (di, D_COMP_TEMPLATE, dc, d_template_args (di));
	}
      return dc;
    }
}

/* <nested-name> ::= N [<CV-qualifiers>] <prefix> <unqualified-name> E
                 ::= N [<CV-qualifiers>] <template-prefix> <template-args> E
*/

static struct d_comp *
d_nested_name (di)
     struct d_info *di;
{
  struct d_comp *ret;
  struct d_comp **pret;

  if (d_next_char (di) != 'N')
    return NULL;

  pret = d_cv_qualifiers (di, &ret);
  if (pret == NULL)
    return NULL;

  *pret = d_prefix (di);
  if (*pret == NULL)
    return NULL;

  if (d_next_char (di) != 'E')
    return NULL;

  return ret;
}

/* <prefix> ::= <prefix> <unqualified-name>
            ::= <template-prefix> <template-args>
            ::= <template-param>
            ::=
            ::= <substitution>

   <template-prefix> ::= <prefix> <(template) unqualified-name>
                     ::= <template-param>
                     ::= <substitution>
*/

static struct d_comp *
d_prefix (di)
     struct d_info *di;
{
  struct d_comp *ret = NULL;

  while (1)
    {
      char peek;
      enum d_comp_type comb_type;
      struct d_comp *dc;

      peek = d_peek_char (di);
      if (peek == '\0')
	return NULL;

      /* The older code accepts a <local-name> here, but I don't see
	 that in the grammar.  The older code does not accept a
	 <template-param> here.  */

      comb_type = D_COMP_QUAL_NAME;
      if (IS_DIGIT (peek)
	  || (peek >= 'a' && peek <= 'z')
	  || peek == 'C'
	  || peek == 'D')
	dc = d_unqualified_name (di);
      else if (peek == 'S')
	dc = d_substitution (di);
      else if (peek == 'I')
	{
	  if (ret == NULL)
	    return NULL;
	  comb_type = D_COMP_TEMPLATE;
	  dc = d_template_args (di);
	}
      else if (peek == 'T')
	dc = d_template_param (di);
      else if (peek == 'E')
	return ret;
      else
	return NULL;

      if (ret == NULL)
	ret = dc;
      else
	ret = d_make_comp (di, comb_type, ret, dc);

      if (peek != 'S' && d_peek_char (di) != 'E')
	{
	  if (! d_add_substitution (di, ret))
	    return NULL;
	}
    }
}

/* <unqualified-name> ::= <operator-name>
                      ::= <ctor-dtor-name>
                      ::= <source-name>
*/

static struct d_comp *
d_unqualified_name (di)
     struct d_info *di;
{
  char peek;

  peek = d_peek_char (di);
  if (IS_DIGIT (peek))
    return d_source_name (di);
  else if (peek >= 'a' && peek <= 'z')
    return d_operator_name (di);
  else if (peek == 'C' || peek == 'D')
    return d_ctor_dtor_name (di);
  else
    return NULL;
}

/* <source-name> ::= <(positive length) number> <identifier>  */

static struct d_comp *
d_source_name (di)
     struct d_info *di;
{
  long len;
  struct d_comp *ret;

  len = d_number (di);
  if (len <= 0)
    return NULL;
  ret = d_identifier (di, len);
  di->last_name = ret;
  return ret;
}

/* number ::= [n] <(non-negative decimal integer)>  */

static long
d_number (di)
     struct d_info *di;
{
  int sign;
  char peek;
  long ret;

  sign = 1;
  peek = d_peek_char (di);
  if (peek == 'n')
    {
      sign = -1;
      d_advance (di, 1);
      peek = d_peek_char (di);
    }

  ret = 0;
  while (1)
    {
      if (! IS_DIGIT (peek))
	return ret * sign;
      ret = ret * 10 + peek - '0';
      d_advance (di, 1);
      peek = d_peek_char (di);
    }
}

/* identifier ::= <(unqualified source code identifier)>  */

static struct d_comp *
d_identifier (di, len)
     struct d_info *di;
     int len;
{
  const char *name;

  name = d_str (di);
  d_advance (di, len);

  /* Look for something which looks like a gcc encoding of an
     anonymous namespace, and replace it with a more user friendly
     name.  */
  if (len >= (int) ANONYMOUS_NAMESPACE_PREFIX_LEN + 2
      && memcmp (name, ANONYMOUS_NAMESPACE_PREFIX,
		 ANONYMOUS_NAMESPACE_PREFIX_LEN) == 0)
    {
      const char *s;

      s = name + ANONYMOUS_NAMESPACE_PREFIX_LEN;
      if ((*s == '.' || *s == '_' || *s == '$')
	  && s[1] == 'N')
	return d_make_name (di, "(anonymous namespace)",
			    sizeof "(anonymous namespace)" - 1);
    }

  return d_make_name (di, name, len);
}

/* operator_name ::= many different two character encodings.
                 ::= cv <type>
                 ::= v <digit> <source-name>
*/

static const struct d_operator_info d_operators[] =
{
  { "aN", "&=",        2 },
  { "aS", "=",         2 },
  { "aa", "&&",        2 },
  { "ad", "&",         1 },
  { "an", "&",         2 },
  { "cl", "()",        0 },
  { "cm", ",",         2 },
  { "co", "~",         1 },
  { "dV", "/=",        2 },
  { "da", "delete[]",  1 },
  { "de", "*",         1 },
  { "dl", "delete",    1 },
  { "dv", "/",         2 },
  { "eO", "^=",        2 },
  { "eo", "^",         2 },
  { "eq", "==",        2 },
  { "ge", ">=",        2 },
  { "gt", ">",         2 },
  { "ix", "[]",        2 },
  { "lS", "<<=",       2 },
  { "le", "<=",        2 },
  { "ls", "<<",        2 },
  { "lt", "<",         2 },
  { "mI", "-=",        2 },
  { "mL", "*=",        2 },
  { "mi", "-",         2 },
  { "ml", "*",         2 },
  { "mm", "--",        1 },
  { "na", "new[]",     1 },
  { "ne", "!=",        2 },
  { "ng", "-",         1 },
  { "nt", "!",         1 },
  { "nw", "new",       1 },
  { "oR", "|=",        2 },
  { "oo", "||",        2 },
  { "or", "|",         2 },
  { "pL", "+=",        2 },
  { "pl", "+",         2 },
  { "pm", "->*",       2 },
  { "pp", "++",        1 },
  { "ps", "+",         1 },
  { "pt", "->",        2 },
  { "qu", "?",         3 },
  { "rM", "%=",        2 },
  { "rS", ">>=",       2 },
  { "rm", "%",         2 },
  { "rs", ">>",        2 },
  { "st", "sizeof ",   1 },
  { "sz", "sizeof ",   1 }
};

static struct d_comp *
d_operator_name (di)
     struct d_info *di;
{
  char c1;
  char c2;

  c1 = d_next_char (di);
  c2 = d_next_char (di);
  if (c1 == 'v' && IS_DIGIT (c2))
    return d_make_extended_operator (di, c2 - '0', d_source_name (di));
  else if (c1 == 'c' && c2 == 'v')
    return d_make_comp (di, D_COMP_CAST, d_type (di), NULL);
  else
    {
      int low = 0;
      int high = sizeof (d_operators) / sizeof (d_operators[0]);

      while (1)
	{
	  int i;
	  const struct d_operator_info *p;

	  i = low + (high - low) / 2;
	  p = d_operators + i;

	  if (c1 == p->code[0] && c2 == p->code[1])
	    return d_make_operator (di, p);

	  if (c1 < p->code[0] || (c1 == p->code[0] && c2 < p->code[1]))
	    high = i;
	  else
	    low = i + 1;
	  if (low == high)
	    return NULL;
	}
    }
}

/* <special-name> ::= TV <type>
                  ::= TT <type>
                  ::= TI <type>
                  ::= TS <type>
                  ::= GV <(object) name>
                  ::= T <call-offset> <(base) encoding>
                  ::= Tc <call-offset> <call-offset> <(base) encoding>
   Also g++ extensions:
                  ::= TC <type> <(offset) number> _ <(base) type>
                  ::= TF <type>
                  ::= TJ <type>
                  ::= GR <name>
*/

static struct d_comp *
d_special_name (di)
     struct d_info *di;
{
  char c;

  c = d_next_char (di);
  if (c == 'T')
    {
      switch (d_next_char (di))
	{
	case 'V':
	  return d_make_comp (di, D_COMP_VTABLE, d_type (di), NULL);
	case 'T':
	  return d_make_comp (di, D_COMP_VTT, d_type (di), NULL);
	case 'I':
	  return d_make_comp (di, D_COMP_TYPEINFO, d_type (di), NULL);
	case 'S':
	  return d_make_comp (di, D_COMP_TYPEINFO_NAME, d_type (di), NULL);

	case 'h':
	  if (! d_call_offset (di, 'h'))
	    return NULL;
	  return d_make_comp (di, D_COMP_THUNK, d_encoding (di, 0), NULL);

	case 'v':
	  if (! d_call_offset (di, 'v'))
	    return NULL;
	  return d_make_comp (di, D_COMP_VIRTUAL_THUNK, d_encoding (di, 0),
			      NULL);

	case 'c':
	  if (! d_call_offset (di, '\0'))
	    return NULL;
	  if (! d_call_offset (di, '\0'))
	    return NULL;
	  return d_make_comp (di, D_COMP_COVARIANT_THUNK, d_encoding (di, 0),
			      NULL);

	case 'C':
	  {
	    struct d_comp *derived_type;
	    long offset;
	    struct d_comp *base_type;

	    derived_type = d_type (di);
	    offset = d_number (di);
	    if (offset < 0)
	      return NULL;
	    if (d_next_char (di) != '_')
	      return NULL;
	    base_type = d_type (di);
	    /* We don't display the offset.  FIXME: We should display
	       it in verbose mode.  */
	    return d_make_comp (di, D_COMP_CONSTRUCTION_VTABLE, base_type,
				derived_type);
	  }

	case 'F':
	  return d_make_comp (di, D_COMP_TYPEINFO_FN, d_type (di), NULL);
	case 'J':
	  return d_make_comp (di, D_COMP_JAVA_CLASS, d_type (di), NULL);

	default:
	  return NULL;
	}
    }
  else if (c == 'G')
    {
      switch (d_next_char (di))
	{
	case 'V':
	  return d_make_comp (di, D_COMP_GUARD, d_name (di), NULL);

	case 'R':
	  return d_make_comp (di, D_COMP_REFTEMP, d_name (di), NULL);

	default:
	  return NULL;
	}
    }
  else
    return NULL;
}

/* <call-offset> ::= h <nv-offset> _
                 ::= v <v-offset> _

   <nv-offset> ::= <(offset) number>

   <v-offset> ::= <(offset) number> _ <(virtual offset) number>

   The C parameter, if not '\0', is a character we just read which is
   the start of the <call-offset>.

   We don't display the offset information anywhere.  FIXME: We should
   display it in verbose mode.  */

static int
d_call_offset (di, c)
     struct d_info *di;
     int c;
{
  long offset;
  long virtual_offset;

  if (c == '\0')
    c = d_next_char (di);

  if (c == 'h')
    offset = d_number (di);
  else if (c == 'v')
    {
      offset = d_number (di);
      if (d_next_char (di) != '_')
	return 0;
      virtual_offset = d_number (di);
    }
  else
    return 0;

  if (d_next_char (di) != '_')
    return 0;

  return 1;
}

/* <ctor-dtor-name> ::= C1
                    ::= C2
                    ::= C3
                    ::= D0
                    ::= D1
                    ::= D2
*/

static struct d_comp *
d_ctor_dtor_name (di)
     struct d_info *di;
{
  switch (d_next_char (di))
    {
    case 'C':
      {
	enum gnu_v3_ctor_kinds kind;

	switch (d_next_char (di))
	  {
	  case '1':
	    kind = gnu_v3_complete_object_ctor;
	    break;
	  case '2':
	    kind = gnu_v3_base_object_ctor;
	    break;
	  case '3':
	    kind = gnu_v3_complete_object_allocating_ctor;
	    break;
	  default:
	    return NULL;
	  }
	return d_make_ctor (di, kind, di->last_name);
      }

    case 'D':
      {
	enum gnu_v3_dtor_kinds kind;

	switch (d_next_char (di))
	  {
	  case '0':
	    kind = gnu_v3_deleting_dtor;
	    break;
	  case '1':
	    kind = gnu_v3_complete_object_dtor;
	    break;
	  case '2':
	    kind = gnu_v3_base_object_dtor;
	    break;
	  default:
	    return NULL;
	  }
	return d_make_dtor (di, kind, di->last_name);
      }

    default:
      return NULL;
    }
}

/* <type> ::= <builtin-type>
          ::= <function-type>
          ::= <class-enum-type>
          ::= <array-type>
          ::= <pointer-to-member-type>
          ::= <template-param>
          ::= <template-template-param> <template-args>
          ::= <substitution>
          ::= <CV-qualifiers> <type>
          ::= P <type>
          ::= R <type>
          ::= C <type>
          ::= G <type>
          ::= U <source-name> <type>

   <builtin-type> ::= various one letter codes
                  ::= u <source-name>
*/

static const struct d_builtin_type_info d_builtin_types[26] =
{
  /* a */ { "signed char",	"signed char",		D_PRINT_INT },
  /* b */ { "bool",		"boolean",		D_PRINT_BOOL },
  /* c */ { "char",		"byte",			D_PRINT_INT },
  /* d */ { "double",		"double",		D_PRINT_DEFAULT },
  /* e */ { "long double",	"long double",		D_PRINT_DEFAULT },
  /* f */ { "float",		"float",		D_PRINT_DEFAULT },
  /* g */ { "__float128",	"__float128",		D_PRINT_DEFAULT },
  /* h */ { "unsigned char",	"unsigned char",	D_PRINT_INT },
  /* i */ { "int",		"int",			D_PRINT_INT },
  /* j */ { "unsigned int",	"unsigned",		D_PRINT_INT },
  /* k */ { NULL,		NULL,			D_PRINT_DEFAULT },
  /* l */ { "long",		"long",			D_PRINT_LONG },
  /* m */ { "unsigned long",	"unsigned long",	D_PRINT_LONG },
  /* n */ { "__int128",		"__int128",		D_PRINT_DEFAULT },
  /* o */ { "unsigned __int128", "unsigned __int128",	D_PRINT_DEFAULT },
  /* p */ { NULL,		NULL,			D_PRINT_DEFAULT },
  /* q */ { NULL,		NULL,			D_PRINT_DEFAULT },
  /* r */ { NULL,		NULL,			D_PRINT_DEFAULT },
  /* s */ { "short",		"short",		D_PRINT_INT },
  /* t */ { "unsigned short",	"unsigned short",	D_PRINT_INT },
  /* u */ { NULL,		NULL,			D_PRINT_DEFAULT },
  /* v */ { "void",		"void",			D_PRINT_VOID },
  /* w */ { "wchar_t",		"char",			D_PRINT_INT },
  /* x */ { "long long",	"long",			D_PRINT_DEFAULT },
  /* y */ { "unsigned long long", "unsigned long long",	D_PRINT_DEFAULT },
  /* z */ { "...",		"...",			D_PRINT_DEFAULT },
};

static struct d_comp *
d_type (di)
     struct d_info *di;
{
  char peek;
  struct d_comp *ret;
  int can_subst;

  /* The ABI specifies that when CV-qualifiers are used, the base type
     is substitutable, and the fully qualified type is substitutable,
     but the base type with a strict subset of the CV-qualifiers is
     not substitutable.  The natural recursive implementation of the
     CV-qualifiers would cause subsets to be substitutable, so instead
     we pull them all off now.

     FIXME: The ABI specifies that vendor qualifiers are handled just
     like the standard CV-qualifiers with respect to subsetting and
     substitution, but g++ does not appear to work this way.  */

  peek = d_peek_char (di);
  if (peek == 'r' || peek == 'V' || peek == 'K')
    {
      struct d_comp **pret;

      pret = d_cv_qualifiers (di, &ret);
      *pret = d_type (di);
      if (! d_add_substitution (di, ret))
	return NULL;
      return ret;
    }

  can_subst = 1;

  switch (peek)
    {
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
    case 'h': case 'i': case 'j':           case 'l': case 'm': case 'n':
    case 'o':                               case 's': case 't':
    case 'v': case 'w': case 'x': case 'y': case 'z':
      ret = d_make_builtin_type (di, &d_builtin_types[peek - 'a']);
      can_subst = 0;
      d_advance (di, 1);
      break;

    case 'u':
      d_advance (di, 1);
      ret = d_make_comp (di, D_COMP_VENDOR_TYPE, d_source_name (di), NULL);
      break;

    case 'F':
      ret = d_function_type (di);
      break;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    case 'N':
    case 'Z':
      ret = d_class_enum_type (di);
      break;

    case 'A':
      ret = d_array_type (di);
      break;

    case 'M':
      ret = d_pointer_to_member_type (di);
      break;

    case 'T':
      ret = d_template_param (di);
      if (d_peek_char (di) == 'I')
	{
	  /* This is <template-template-param> <template-args>.  The
	     <template-template-param> part is a substitution
	     candidate.  */
	  if (! d_add_substitution (di, ret))
	    return NULL;
	  ret = d_make_comp (di, D_COMP_TEMPLATE, ret, d_template_args (di));
	}
      break;

    case 'S':
      /* If this is a special substitution, then it is the start of
	 <class-enum-type>.  */
      {
	char peek_next;

	peek_next = d_peek_next_char (di);
	if (IS_DIGIT (peek_next)
	    || peek_next == '_'
	    || (peek_next >= 'A' && peek_next <= 'Z'))
	  {
	    ret = d_substitution (di);
	    /* The substituted name may have been a template name and
	       may be followed by tepmlate args.  */
	    if (d_peek_char (di) == 'I')
	      ret = d_make_comp (di, D_COMP_TEMPLATE, ret,
				 d_template_args (di));
	    else
	      can_subst = 0;
	  }
	else
	  {
	    ret = d_class_enum_type (di);
	    /* If the substitution was a complete type, then it is not
	       a new substitution candidate.  However, if the
	       substitution was followed by template arguments, then
	       the whole thing is a substitution candidate.  */
	    if (ret->type == D_COMP_SUB_STD)
	      can_subst = 0;
	  }
      }
      break;

    case 'P':
      d_advance (di, 1);
      ret = d_make_comp (di, D_COMP_POINTER, d_type (di), NULL);
      break;

    case 'R':
      d_advance (di, 1);
      ret = d_make_comp (di, D_COMP_REFERENCE, d_type (di), NULL);
      break;

    case 'C':
      d_advance (di, 1);
      ret = d_make_comp (di, D_COMP_COMPLEX, d_type (di), NULL);
      break;

    case 'G':
      d_advance (di, 1);
      ret = d_make_comp (di, D_COMP_IMAGINARY, d_type (di), NULL);
      break;

    case 'U':
      d_advance (di, 1);
      ret = d_source_name (di);
      ret = d_make_comp (di, D_COMP_VENDOR_TYPE_QUAL, d_type (di), ret);
      break;

    default:
      return NULL;
    }

  if (can_subst)
    {
      if (! d_add_substitution (di, ret))
	return NULL;
    }

  return ret;
}

/* <CV-qualifiers> ::= [r] [V] [K]  */

static struct d_comp **
d_cv_qualifiers (di, pret)
     struct d_info *di;
     struct d_comp **pret;
{
  char peek;

  peek = d_peek_char (di);
  while (peek == 'r' || peek == 'V' || peek == 'K')
    {
      enum d_comp_type t;

      d_advance (di, 1);
      if (peek == 'r')
	t = D_COMP_RESTRICT;
      else if (peek == 'V')
	t = D_COMP_VOLATILE;
      else
	t = D_COMP_CONST;

      *pret = d_make_comp (di, t, NULL, NULL);
      if (*pret == NULL)
	return NULL;
      pret = &d_left (*pret);

      peek = d_peek_char (di);
    }

  return pret;
}

/* <function-type> ::= F [Y] <bare-function-type> E  */

static struct d_comp *
d_function_type (di)
     struct d_info *di;
{
  struct d_comp *ret;

  if (d_next_char (di) != 'F')
    return NULL;
  if (d_peek_char (di) == 'Y')
    {
      /* Function has C linkage.  We don't print this information.
	 FIXME: We should print it in verbose mode.  */
      d_advance (di, 1);
    }
  ret = d_bare_function_type (di, 1);
  if (d_next_char (di) != 'E')
    return NULL;
  return ret;
}

/* <bare-function-type> ::= <type>+  */

static struct d_comp *
d_bare_function_type (di, has_return_type)
     struct d_info *di;
     int has_return_type;
{
  struct d_comp *return_type;
  struct d_comp *tl;
  struct d_comp **ptl;

  return_type = NULL;
  tl = NULL;
  ptl = &tl;
  while (1)
    {
      char peek;
      struct d_comp *type;

      peek = d_peek_char (di);
      if (peek == '\0' || peek == 'E')
	break;
      type = d_type (di);
      if (type == NULL)
	return NULL;
      if (has_return_type)
	{
	  return_type = type;
	  has_return_type = 0;
	}
      else
	{
	  *ptl = d_make_comp (di, D_COMP_ARGLIST, type, NULL);
	  ptl = &d_right (*ptl);
	}
    }

  /* There should be at least one parameter type besides the optional
     return type.  A function which takes no arguments will have a
     single parameter type void.  */
  if (tl == NULL)
    return NULL;

  /* If we have a single parameter type void, omit it.  */
  if (d_right (tl) == NULL
      && d_left (tl)->type == D_COMP_BUILTIN_TYPE
      && d_left (tl)->u.s_builtin.type->print == D_PRINT_VOID)
    tl = NULL;

  return d_make_comp (di, D_COMP_FUNCTION_TYPE, return_type, tl);
}

/* <class-enum-type> ::= <name>  */

static struct d_comp *
d_class_enum_type (di)
     struct d_info *di;
{
  return d_name (di);
}

/* <array-type> ::= A <(positive dimension) number> _ <(element) type>
                ::= A [<(dimension) expression>] _ <(element) type>
*/

static struct d_comp *
d_array_type (di)
     struct d_info *di;
{
  char peek;
  struct d_comp *dim;

  if (d_next_char (di) != 'A')
    return NULL;

  peek = d_peek_char (di);
  if (peek == '_')
    dim = NULL;
  else if (IS_DIGIT (peek))
    {
      const char *s;

      s = d_str (di);
      do
	{
	  d_advance (di, 1);
	  peek = d_peek_char (di);
	}
      while (IS_DIGIT (peek));
      dim = d_make_name (di, s, d_str (di) - s);
    }
  else
    {
      dim = d_expression (di);
      if (dim == NULL)
	return NULL;
    }

  if (d_next_char (di) != '_')
    return NULL;

  return d_make_comp (di, D_COMP_ARRAY_TYPE, dim, d_type (di));
}

/* <pointer-to-member-type> ::= M <(class) type> <(member) type>  */

static struct d_comp *
d_pointer_to_member_type (di)
     struct d_info *di;
{
  struct d_comp *cl;
  struct d_comp *mem;
  struct d_comp **pmem;

  if (d_next_char (di) != 'M')
    return NULL;

  cl = d_type (di);

  /* The ABI specifies that any type can be a substitution source, and
     that M is followed by two types, and that when a CV-qualified
     type is seen both the base type and the CV-qualified types are
     substitution sources.  The ABI also specifies that for a pointer
     to a CV-qualified member function, the qualifiers are attached to
     the second type.  Given the grammar, a plain reading of the ABI
     suggests that both the CV-qualified member function and the
     non-qualified member function are substitution sources.  However,
     g++ does not work that way.  g++ treats only the CV-qualified
     member function as a substitution source.  FIXME.  So to work
     with g++, we need to pull off the CV-qualifiers here, in order to
     avoid calling add_substitution() in d_type().  */

  pmem = d_cv_qualifiers (di, &mem);
  *pmem = d_type (di);

  return d_make_comp (di, D_COMP_PTRMEM_TYPE, cl, mem);
}

/* <template-param> ::= T_
                    ::= T <(parameter-2 non-negative) number> _
*/

static struct d_comp *
d_template_param (di)
     struct d_info *di;
{
  long param;

  if (d_next_char (di) != 'T')
    return NULL;

  if (d_peek_char (di) == '_')
    param = 0;
  else
    {
      param = d_number (di);
      if (param < 0)
	return NULL;
      param += 1;
    }

  if (d_next_char (di) != '_')
    return NULL;

  return d_make_template_param (di, param);
}

/* <template-args> ::= I <template-arg>+ E  */

static struct d_comp *
d_template_args (di)
     struct d_info *di;
{
  struct d_comp *hold_last_name;
  struct d_comp *al;
  struct d_comp **pal;

  /* Preserve the last name we saw--don't let the template arguments
     clobber it, as that would give us the wrong name for a subsequent
     constructor or destructor.  */
  hold_last_name = di->last_name;

  if (d_next_char (di) != 'I')
    return NULL;

  al = NULL;
  pal = &al;
  while (1)
    {
      struct d_comp *a;

      a = d_template_arg (di);
      if (a == NULL)
	return NULL;

      *pal = d_make_comp (di, D_COMP_TEMPLATE_ARGLIST, a, NULL);
      pal = &d_right (*pal);

      if (d_peek_char (di) == 'E')
	{
	  d_advance (di, 1);
	  break;
	}
    }

  di->last_name = hold_last_name;

  return al;
}

/* <template-arg> ::= <type>
                  ::= X <expression> E
                  ::= <expr-primary>
*/

static struct d_comp *
d_template_arg (di)
     struct d_info *di;
{
  struct d_comp *ret;

  switch (d_peek_char (di))
    {
    case 'X':
      d_advance (di, 1);
      ret = d_expression (di);
      if (d_next_char (di) != 'E')
	return NULL;
      return ret;

    case 'L':
      return d_expr_primary (di);

    default:
      return d_type (di);
    }
}

/* <expression> ::= <(unary) operator-name> <expression>
                ::= <(binary) operator-name> <expression> <expression>
                ::= <(trinary) operator-name> <expression> <expression> <expression>
                ::= st <type>
                ::= <template-param>
                ::= sr <type> <unqualified-name>
                ::= sr <type> <unqualified-name> <template-args>
                ::= <expr-primary>
*/

static struct d_comp *
d_expression (di)
     struct d_info *di;
{
  char peek;

  peek = d_peek_char (di);
  if (peek == 'L')
    return d_expr_primary (di);
  else if (peek == 'T')
    return d_template_param (di);
  else if (peek == 's' && d_peek_next_char (di) == 'r')
    {
      struct d_comp *type;
      struct d_comp *name;

      d_advance (di, 2);
      type = d_type (di);
      name = d_unqualified_name (di);
      if (d_peek_char (di) != 'I')
	return d_make_comp (di, D_COMP_QUAL_NAME, type, name);
      else
	return d_make_comp (di, D_COMP_QUAL_NAME, type,
			    d_make_comp (di, D_COMP_TEMPLATE, name,
					 d_template_args (di)));
    }
  else
    {
      struct d_comp *op;
      int args;

      op = d_operator_name (di);
      if (op == NULL)
	return NULL;

      if (op->type == D_COMP_OPERATOR
	  && strcmp (op->u.s_operator.op->code, "st") == 0)
	return d_make_comp (di, D_COMP_UNARY, op, d_type (di));

      switch (op->type)
	{
	default:
	  return NULL;
	case D_COMP_OPERATOR:
	  args = op->u.s_operator.op->args;
	  break;
	case D_COMP_EXTENDED_OPERATOR:
	  args = op->u.s_extended_operator.args;
	  break;
	case D_COMP_CAST:
	  args = 1;
	  break;
	}

      switch (args)
	{
	case 1:
	  return d_make_comp (di, D_COMP_UNARY, op, d_expression (di));
	case 2:
	  {
	    struct d_comp *left;

	    left = d_expression (di);
	    return d_make_comp (di, D_COMP_BINARY, op,
				d_make_comp (di, D_COMP_BINARY_ARGS, left,
					     d_expression (di)));
	  }
	case 3:
	  {
	    struct d_comp *first;
	    struct d_comp *second;

	    first = d_expression (di);
	    second = d_expression (di);
	    return d_make_comp (di, D_COMP_TRINARY, op,
				d_make_comp (di, D_COMP_TRINARY_ARG1, first,
					     d_make_comp (di,
							  D_COMP_TRINARY_ARG2,
							  second,
							  d_expression (di))));
	  }
	default:
	  return NULL;
	}
    }
}

/* <expr-primary> ::= L <type> <(value) number> E
                  ::= L <type> <(value) float> E
                  ::= L <mangled-name> E
*/

static struct d_comp *
d_expr_primary (di)
     struct d_info *di;
{
  struct d_comp *ret;

  if (d_next_char (di) != 'L')
    return NULL;
  if (d_peek_char (di) == '_')
    ret = d_mangled_name (di);
  else
    {
      struct d_comp *type;
      const char *s;

      type = d_type (di);

      /* Rather than try to interpret the literal value, we just
	 collect it as a string.  Note that it's possible to have a
	 floating point literal here.  The ABI specifies that the
	 format of such literals is machine independent.  That's fine,
	 but what's not fine is that versions of g++ up to 3.2 with
	 -fabi-version=1 used upper case letters in the hex constant,
	 and dumped out gcc's internal representation.  That makes it
	 hard to tell where the constant ends, and hard to dump the
	 constant in any readable form anyhow.  We don't attempt to
	 handle these cases.  */

      s = d_str (di);
      while (d_peek_char (di) != 'E')
	d_advance (di, 1);
      ret = d_make_comp (di, D_COMP_LITERAL, type,
			 d_make_name (di, s, d_str (di) - s));
    }
  if (d_next_char (di) != 'E')
    return NULL;
  return ret;
}

/* <local-name> ::= Z <(function) encoding> E <(entity) name> [<discriminator>]
                ::= Z <(function) encoding> E s [<discriminator>]
*/

static struct d_comp *
d_local_name (di)
     struct d_info *di;
{
  struct d_comp *function;

  if (d_next_char (di) != 'Z')
    return NULL;

  function = d_encoding (di, 0);

  if (d_next_char (di) != 'E')
    return NULL;

  if (d_peek_char (di) == 's')
    {
      d_advance (di, 1);
      if (! d_discriminator (di))
	return NULL;
      return d_make_comp (di, D_COMP_QUAL_NAME, function,
			  d_make_name (di, "string literal",
				       sizeof "string literal" - 1));
    }
  else
    {
      struct d_comp *name;

      name = d_name (di);
      if (! d_discriminator (di))
	return NULL;
      return d_make_comp (di, D_COMP_QUAL_NAME, function, name);
    }
}

/* <discriminator> ::= _ <(non-negative) number>

   We demangle the discriminator, but we don't print it out.  FIXME:
   We should print it out in verbose mode.  */

static int
d_discriminator (di)
     struct d_info *di;
{
  long discrim;

  if (d_peek_char (di) != '_')
    return 1;
  d_advance (di, 1);
  discrim = d_number (di);
  if (discrim < 0)
    return 0;
  return 1;
}

/* Add a new substitution.  */

static int
d_add_substitution (di, dc)
     struct d_info *di;
     struct d_comp *dc;
{
  if (di->next_sub >= di->num_subs)
    return 0;
  di->subs[di->next_sub] = dc;
  ++di->next_sub;
  return 1;
}

/* <substitution> ::= S <seq-id> _
                  ::= S_
                  ::= St
                  ::= Sa
                  ::= Sb
                  ::= Ss
                  ::= Si
                  ::= So
                  ::= Sd
*/

static struct d_comp *
d_substitution (di)
     struct d_info *di;
{
  char c;

  if (d_next_char (di) != 'S')
    return NULL;

  c = d_next_char (di);
  if (c == '_' || IS_DIGIT (c) || (c >= 'A' && c <= 'Z'))
    {
      int id;

      id = 0;
      if (c != '_')
	{
	  do
	    {
	      if (IS_DIGIT (c))
		id = id * 36 + c - '0';
	      else if (c >= 'A' && c <= 'Z')
		id = id * 36 + c - 'A' + 10;
	      else
		return NULL;
	      c = d_next_char (di);
	    }
	  while (c != '_');

	  ++id;
	}

      if (id >= di->next_sub)
	return NULL;

      return di->subs[id];
    }
  else
    {
      switch (c)
	{
	case 't':
	  return d_make_sub (di, "std");
	case 'a':
	  di->last_name = d_make_sub (di, "allocator");
	  return d_make_sub (di, "std::allocator");
	case 'b':
	  di->last_name = d_make_sub (di, "basic_string");
	  return d_make_sub (di, "std::basic_string");
	case 's':
	  di->last_name = d_make_sub (di, "string");
	  return d_make_sub (di, "std::string");
	case 'i':
	  di->last_name = d_make_sub (di, "istream");
	  return d_make_sub (di, "std::istream");
	case 'o':
	  di->last_name = d_make_sub (di, "ostream");
	  return d_make_sub (di, "std::ostream");
	case 'd':
	  di->last_name = d_make_sub (di, "iostream");
	  return d_make_sub (di, "std::iostream");
	default:
	  return NULL;
	}
    }
}

/* Resize the print buffer.  */

static void
d_print_resize (dpi, add)
     struct d_print_info *dpi;
     size_t add;
{
  size_t need;

  need = dpi->len + add;
  while (need > dpi->alc)
    {
      size_t newalc;
      char *newbuf;

      newalc = dpi->alc * 2;
      newbuf = realloc (dpi->buf, newalc);
      if (newbuf == NULL)
	{
	  free (dpi->buf);
	  dpi->buf = NULL;
	  dpi->allocation_failure = 1;
	  return;
	}
      dpi->buf = newbuf;
      dpi->alc = newalc;
    }
}

/* Append a character to the print buffer.  */

static void
d_print_append_char (dpi, c)
     struct d_print_info *dpi;
     int c;
{
  if (dpi->buf != NULL)
    {
      if (dpi->len >= dpi->alc)
	{
	  d_print_resize (dpi, 1);
	  if (dpi->buf == NULL)
	    return;
	}

      dpi->buf[dpi->len] = c;
      ++dpi->len;
    }
}

/* Append a buffer to the print buffer.  */

static void
d_print_append_buffer (dpi, s, l)
     struct d_print_info *dpi;
     const char *s;
     size_t l;
{
  if (dpi->buf != NULL)
    {
      if (dpi->len + l > dpi->alc)
	{
	  d_print_resize (dpi, l);
	  if (dpi->buf == NULL)
	    return;
	}

      memcpy (dpi->buf + dpi->len, s, l);
      dpi->len += l;
    }
}

/* Indicate that an error occurred during printing.  */

static void
d_print_error (dpi)
     struct d_print_info *dpi;
{
  free (dpi->buf);
  dpi->buf = NULL;
}

/* Turn components into a human readable string.  Returns a string
   allocated by malloc, or NULL on error.  On success, this sets *PALC
   to the size of the allocated buffer.  On failure, this sets *PALC
   to 0 for a bad parse, or to 1 for a memory allocation failure.  */

static char *
d_print (options, dc, palc)
     int options;
     const struct d_comp *dc;
     size_t *palc;
{
  struct d_print_info dpi;

  dpi.options = options;

  dpi.alc = 64;
  dpi.buf = malloc (dpi.alc);
  if (dpi.buf == NULL)
    {
      *palc = 1;
      return NULL;
    }

  dpi.len = 0;
  dpi.templates = NULL;
  dpi.modifiers = NULL;

  dpi.allocation_failure = 0;

  d_print_comp (&dpi, dc);

  d_append_char (&dpi, '\0');

  if (dpi.buf != NULL)
    *palc = dpi.alc;
  else
    *palc = dpi.allocation_failure;

  return dpi.buf;
}

/* Subroutine to handle components.  */

static void
d_print_comp (dpi, dc)
     struct d_print_info *dpi;
     const struct d_comp *dc;
{
  if (dc == NULL)
    {
      d_print_error (dpi);
      return;
    }
  if (d_print_saw_error (dpi))
    return;

  switch (dc->type)
    {
    case D_COMP_NAME:
      d_print_identifier (dpi, dc->u.s_name.s, dc->u.s_name.len);
      return;

    case D_COMP_QUAL_NAME:
      d_print_comp (dpi, d_left (dc));
      d_append_string (dpi, (dpi->options & DMGL_JAVA) == 0 ? "::" : ".");
      d_print_comp (dpi, d_right (dc));
      return;

    case D_COMP_TYPED_NAME:
      {
	const struct d_comp *typed_name;
	struct d_print_mod dpm;
	struct d_print_template dpt;

	/* Pass the name down to the type so that it can be printed in
	   the right place for the type.  If the name has
	   CV-qualifiers, they are really method qualifiers; pull them
	   off now and print them after everything else.  Note that we
	   don't handle D_COMP_VENDOR_TYPE_QUAL here; it's not
	   accepted by d_cv_qualifiers() either.  */
	typed_name = d_left (dc);
	while (typed_name != NULL
	       && (typed_name->type == D_COMP_RESTRICT
		   || typed_name->type == D_COMP_VOLATILE
		   || typed_name->type == D_COMP_CONST))
	  typed_name = d_left (typed_name);

	dpm.next = dpi->modifiers;
	dpi->modifiers = &dpm;
	dpm.mod = typed_name;
	dpm.printed = 0;

	/* If typed_name is a template, then it applies to the
	   function type as well.  */
	if (typed_name->type == D_COMP_TEMPLATE)
	  {
	    dpt.next = dpi->templates;
	    dpi->templates = &dpt;
	    dpt.template = typed_name;
	  }

	d_print_comp (dpi, d_right (dc));

	if (typed_name->type == D_COMP_TEMPLATE)
	  dpi->templates = dpt.next;

	/* If the modifier didn't get printed by the type, print it
	   now.  */
	if (! dpm.printed)
	  {
	    d_append_char (dpi, ' ');
	    d_print_comp (dpi, typed_name);
	  }

	dpi->modifiers = dpm.next;

	/* Now print any CV-qualifiers on the type.  */
	typed_name = d_left (dc);
	while (typed_name != NULL
	       && (typed_name->type == D_COMP_RESTRICT
		   || typed_name->type == D_COMP_VOLATILE
		   || typed_name->type == D_COMP_CONST))
	  {
	    d_print_mod (dpi, typed_name);
	    typed_name = d_left (typed_name);
	  }

	return;
      }

    case D_COMP_TEMPLATE:
      d_print_comp (dpi, d_left (dc));
      d_append_char (dpi, '<');
      d_print_comp (dpi, d_right (dc));
      /* Avoid generating two consecutive '>' characters, to avoid the
	 C++ syntactic ambiguity.  */
      if (dpi->buf[dpi->len - 1] == '>')
	d_append_char (dpi, ' ');
      d_append_char (dpi, '>');
      return;

    case D_COMP_TEMPLATE_PARAM:
      {
	long i;
	struct d_comp *a;
	struct d_print_template *hold_dpt;

	if (dpi->templates == NULL)
	  {
	    d_print_error (dpi);
	    return;
	  }
	i = dc->u.s_number.number;
	for (a = d_right (dpi->templates->template);
	     a != NULL;
	     a = d_right (a))
	  {
	    if (a->type != D_COMP_TEMPLATE_ARGLIST)
	      {
		d_print_error (dpi);
		return;
	      }
	    if (i <= 0)
	      break;
	    --i;
	  }
	if (i != 0 || a == NULL)
	  {
	    d_print_error (dpi);
	    return;
	  }

	/* While processing this parameter, we need to pop the list of
	   templates.  This is because the template parameter may
	   itself be a reference to a parameter of an outer
	   template.  */

	hold_dpt = dpi->templates;
	dpi->templates = hold_dpt->next;

	d_print_comp (dpi, d_left (a));

	dpi->templates = hold_dpt;

	return;
      }

    case D_COMP_CTOR:
      d_print_comp (dpi, dc->u.s_ctor.name);
      return;

    case D_COMP_DTOR:
      d_append_char (dpi, '~');
      d_print_comp (dpi, dc->u.s_dtor.name);
      return;

    case D_COMP_VTABLE:
      d_append_string (dpi, "vtable for ");
      d_print_comp (dpi, d_left (dc));
      return;

    case D_COMP_VTT:
      d_append_string (dpi, "VTT for ");
      d_print_comp (dpi, d_left (dc));
      return;

    case D_COMP_CONSTRUCTION_VTABLE:
      d_append_string (dpi, "construction vtable for ");
      d_print_comp (dpi, d_left (dc));
      d_append_string (dpi, "-in-");
      d_print_comp (dpi, d_right (dc));
      return;

    case D_COMP_TYPEINFO:
      d_append_string (dpi, "typeinfo for ");
      d_print_comp (dpi, d_left (dc));
      return;

    case D_COMP_TYPEINFO_NAME:
      d_append_string (dpi, "typeinfo name for ");
      d_print_comp (dpi, d_left (dc));
      return;

    case D_COMP_TYPEINFO_FN:
      d_append_string (dpi, "typeinfo fn for ");
      d_print_comp (dpi, d_left (dc));
      return;

    case D_COMP_THUNK:
      d_append_string (dpi, "non-virtual thunk to ");
      d_print_comp (dpi, d_left (dc));
      return;

    case D_COMP_VIRTUAL_THUNK:
      d_append_string (dpi, "virtual thunk to ");
      d_print_comp (dpi, d_left (dc));
      return;

    case D_COMP_COVARIANT_THUNK:
      d_append_string (dpi, "covariant return thunk to ");
      d_print_comp (dpi, d_left (dc));
      return;

    case D_COMP_JAVA_CLASS:
      d_append_string (dpi, "java Class for ");
      d_print_comp (dpi, d_left (dc));
      return;

    case D_COMP_GUARD:
      d_append_string (dpi, "guard variable for ");
      d_print_comp (dpi, d_left (dc));
      return;

    case D_COMP_REFTEMP:
      d_append_string (dpi, "reference temporary for ");
      d_print_comp (dpi, d_left (dc));
      return;

    case D_COMP_SUB_STD:
      d_append_string (dpi, dc->u.s_string.string);
      return;

    case D_COMP_RESTRICT:
    case D_COMP_VOLATILE:
    case D_COMP_CONST:
    case D_COMP_VENDOR_TYPE_QUAL:
    case D_COMP_POINTER:
    case D_COMP_REFERENCE:
    case D_COMP_COMPLEX:
    case D_COMP_IMAGINARY:
      {
	/* We keep a list of modifiers on the stack.  */
	struct d_print_mod dpm;

	dpm.next = dpi->modifiers;
	dpi->modifiers = &dpm;
	dpm.mod = dc;
	dpm.printed = 0;

	d_print_comp (dpi, d_left (dc));

	/* If the modifier didn't get printed by the type, print it
	   now.  */
	if (! dpm.printed)
	  d_print_mod (dpi, dc);

	dpi->modifiers = dpm.next;

	return;
      }

    case D_COMP_BUILTIN_TYPE:
      if ((dpi->options & DMGL_JAVA) == 0)
	d_append_string (dpi, dc->u.s_builtin.type->name);
      else
	d_append_string (dpi, dc->u.s_builtin.type->java_name);
      return;

    case D_COMP_VENDOR_TYPE:
      d_print_comp (dpi, d_left (dc));
      return;

    case D_COMP_FUNCTION_TYPE:
      {
	if (d_left (dc) != NULL)
	  {
	    struct d_print_mod dpm;

	    /* We must pass this type down as a modifier in order to
	       print it in the right location.  */

	    dpm.next = dpi->modifiers;
	    dpi->modifiers = &dpm;
	    dpm.mod = dc;
	    dpm.printed = 0;

	    d_print_comp (dpi, d_left (dc));

	    dpi->modifiers = dpm.next;

	    if (dpm.printed)
	      return;

	    d_append_char (dpi, ' ');
	  }

	d_print_function_type (dpi, dc, dpi->modifiers);

	return;
      }

    case D_COMP_ARRAY_TYPE:
      {
	struct d_print_mod dpm;

	/* We must pass this type down as a modifier in order to print
	   multi-dimensional arrays correctly.  */

	dpm.next = dpi->modifiers;
	dpi->modifiers = &dpm;
	dpm.mod = dc;
	dpm.printed = 0;

	d_print_comp (dpi, d_right (dc));

	dpi->modifiers = dpm.next;

	if (dpm.printed)
	  return;

	d_print_array_type (dpi, dc, dpi->modifiers);

	return;
      }

    case D_COMP_PTRMEM_TYPE:
      {
	const struct d_comp *target_type;
	struct d_print_mod dpm;

	/* Pass the name down to the type so that it can be printed in
	   the right place for the type.  If the type has
	   CV-qualifiers, they are really method qualifiers; pull them
	   off now and print them after everything else.  */
	target_type = d_right (dc);
	while (target_type != NULL
	       && (target_type->type == D_COMP_RESTRICT
		   || target_type->type == D_COMP_VOLATILE
		   || target_type->type == D_COMP_CONST))
	  target_type = d_left (target_type);

	dpm.next = dpi->modifiers;
	dpi->modifiers = &dpm;
	dpm.mod = dc;
	dpm.printed = 0;

	d_print_comp (dpi, target_type);

	/* If the modifier didn't get printed by the type, print it
	   now.  */
	if (! dpm.printed)
	  {
	    d_append_char (dpi, ' ');
	    d_print_comp (dpi, d_left (dc));
	    d_append_string (dpi, "::*");
	  }

	dpi->modifiers = dpm.next;

	/* Now print any CV-qualifiers on the type.  */
	target_type = d_right (dc);
	while (target_type != NULL
	       && (target_type->type == D_COMP_RESTRICT
		   || target_type->type == D_COMP_VOLATILE
		   || target_type->type == D_COMP_CONST))
	  {
	    d_print_mod (dpi, target_type);
	    target_type = d_left (target_type);
	  }

	return;
      }

    case D_COMP_ARGLIST:
    case D_COMP_TEMPLATE_ARGLIST:
      d_print_comp (dpi, d_left (dc));
      if (d_right (dc) != NULL)
	{
	  d_append_string (dpi, ", ");
	  d_print_comp (dpi, d_right (dc));
	}
      return;

    case D_COMP_OPERATOR:
      {
	char c;

	d_append_string (dpi, "operator");
	c = dc->u.s_operator.op->name[0];
	if (c >= 'a' && c <= 'z')
	  d_append_char (dpi, ' ');
	d_append_string (dpi, dc->u.s_operator.op->name);
	return;
      }

    case D_COMP_EXTENDED_OPERATOR:
      d_append_string (dpi, "operator ");
      d_print_comp (dpi, dc->u.s_extended_operator.name);
      return;

    case D_COMP_CAST:
      d_append_string (dpi, "operator ");
      d_print_cast (dpi, dc);
      return;

    case D_COMP_UNARY:
      if (d_left (dc)->type != D_COMP_CAST)
	d_print_expr_op (dpi, d_left (dc));
      else
	{
	  d_append_string (dpi, "((");
	  d_print_cast (dpi, d_left (dc));
	  d_append_char (dpi, ')');
	}
      d_append_char (dpi, '(');
      d_print_comp (dpi, d_right (dc));
      d_append_char (dpi, ')');
      if (d_left (dc)->type == D_COMP_CAST)
	d_append_char (dpi, ')');
      return;

    case D_COMP_BINARY:
      if (d_right (dc)->type != D_COMP_BINARY_ARGS)
	{
	  d_print_error (dpi);
	  return;
	}
      d_append_char (dpi, '(');
      d_print_comp (dpi, d_left (d_right (dc)));
      d_append_string (dpi, ") ");
      d_print_expr_op (dpi, d_left (dc));
      d_append_string (dpi, " (");
      d_print_comp (dpi, d_right (d_right (dc)));
      d_append_char (dpi, ')');
      return;

    case D_COMP_BINARY_ARGS:
      /* We should only see this as part of D_COMP_BINARY.  */
      d_print_error (dpi);
      return;

    case D_COMP_TRINARY:
      if (d_right (dc)->type != D_COMP_TRINARY_ARG1
	  || d_right (d_right (dc))->type != D_COMP_TRINARY_ARG2)
	{
	  d_print_error (dpi);
	  return;
	}
      d_append_char (dpi, '(');
      d_print_comp (dpi, d_left (d_right (dc)));
      d_append_string (dpi, ") ");
      d_print_expr_op (dpi, d_left (dc));
      d_append_string (dpi, " (");
      d_print_comp (dpi, d_left (d_right (d_right (dc))));
      d_append_string (dpi, ") : (");
      d_print_comp (dpi, d_right (d_right (d_right (dc))));
      d_append_char (dpi, ')');
      return;

    case D_COMP_TRINARY_ARG1:
    case D_COMP_TRINARY_ARG2:
      /* We should only see these are part of D_COMP_TRINARY.  */
      d_print_error (dpi);
      return;

    case D_COMP_LITERAL:
      /* For some builtin types, produce simpler output.  */
      if (d_left (dc)->type == D_COMP_BUILTIN_TYPE)
	{
	  switch (d_left (dc)->u.s_builtin.type->print)
	    {
	    case D_PRINT_INT:
	      if (d_right (dc)->type == D_COMP_NAME)
		{
		  d_print_comp (dpi, d_right (dc));
		  return;
		}
	      break;

	    case D_PRINT_LONG:
	      if (d_right (dc)->type == D_COMP_NAME)
		{
		  d_print_comp (dpi, d_right (dc));
		  d_append_char (dpi, 'l');
		  return;
		}
	      break;

	    case D_PRINT_BOOL:
	      if (d_right (dc)->type == D_COMP_NAME
		  && d_right (dc)->u.s_name.len == 1)
		{
		  switch (d_right (dc)->u.s_name.s[0])
		    {
		    case '0':
		      d_append_string (dpi, "false");
		      return;
		    case '1':
		      d_append_string (dpi, "true");
		      return;
		    default:
		      break;
		    }
		}
	      break;

	    default:
	      break;
	    }
	}

      d_append_char (dpi, '(');
      d_print_comp (dpi, d_left (dc));
      d_append_char (dpi, ')');
      d_print_comp (dpi, d_right (dc));
      return;

    default:
      d_print_error (dpi);
      return;
    }
}

/* Print an identifier.  */

static void
d_print_identifier (dpi, name, len)
     struct d_print_info *dpi;
     const char *name;
     int len;
{
  if ((dpi->options & DMGL_JAVA) == 0)
    d_append_buffer (dpi, name, len);
  else
    {
      const char *p;
      const char *end;

      /* For Java we try to handle encoded extended Unicode
	 characters.  The C++ ABI doesn't mention Unicode encoding, so
	 we don't it for C++.  Characters are encoded as
	 __U<hex-char>+_.  */
      end = name + len;
      for (p = name; p < end; ++p)
	{
	  if (end - p > 3
	      && p[0] == '_'
	      && p[1] == '_'
	      && p[2] == 'U')
	    {
	      unsigned long c;
	      const char *q;

	      c = 0;
	      for (q = p + 3; q < end; ++q)
		{
		  int dig;

		  if (*q >= '0' && *q <= '9')
		    dig = *q - '0';
		  else if (*q >= 'A' && *q <= 'F')
		    dig = *q - 'A' + 10;
		  else if (*q >= 'a' && *q <= 'f')
		    dig = *q - 'a' + 10;
		  else
		    break;

		  c = c * 16 + dig;
		}
	      /* If the Unicode character is larger than 256, we don't
		 try to deal with it here.  FIXME.  */
	      if (q < end && *q == '_' && c < 256)
		{
		  d_append_char (dpi, c);
		  p = q;
		  continue;
		}
	    }

	  d_append_char (dpi, *p);
	}
    }
}

/* Print a list of modifiers.  */

static void
d_print_mod_list (dpi, mods)
     struct d_print_info *dpi;
     struct d_print_mod *mods;
{
  if (mods == NULL || mods->printed || d_print_saw_error (dpi))
    return;

  if (mods->mod->type == D_COMP_FUNCTION_TYPE)
    {
      mods->printed = 1;
      d_print_function_type (dpi, mods->mod, mods->next);
      return;
    }
  else if (mods->mod->type == D_COMP_ARRAY_TYPE)
    {
      mods->printed = 1;
      d_print_array_type (dpi, mods->mod, mods->next);
      return;
    }

  mods->printed = 1;

  d_print_mod (dpi, mods->mod);

  d_print_mod_list (dpi, mods->next);
}
 
/* Print a modifier.  */

static void
d_print_mod (dpi, mod)
     struct d_print_info *dpi;
     const struct d_comp *mod;
{
  switch (mod->type)
    {
    case D_COMP_RESTRICT:
      d_append_string (dpi, " restrict");
      return;
    case D_COMP_VOLATILE:
      d_append_string (dpi, " volatile");
      return;
    case D_COMP_CONST:
      d_append_string (dpi, " const");
      return;
    case D_COMP_VENDOR_TYPE_QUAL:
      d_append_char (dpi, ' ');
      d_print_comp (dpi, d_right (mod));
      return;
    case D_COMP_POINTER:
      /* There is no pointer symbol in Java.  */
      if ((dpi->options & DMGL_JAVA) == 0)
	d_append_char (dpi, '*');
      return;
    case D_COMP_REFERENCE:
      d_append_char (dpi, '&');
      return;
    case D_COMP_COMPLEX:
      d_append_string (dpi, "complex ");
      return;
    case D_COMP_IMAGINARY:
      d_append_string (dpi, "imaginary ");
      return;
    case D_COMP_PTRMEM_TYPE:
      if (dpi->buf[dpi->len - 1] != '(')
	d_append_char (dpi, ' ');
      d_print_comp (dpi, d_left (mod));
      d_append_string (dpi, "::*");
      return;
    case D_COMP_TYPED_NAME:
      d_print_comp (dpi, d_left (mod));
      return;
    default:
      /* Otherwise, we have something that won't go back on the
	 modifier stack, so we can just print it.  */
      d_print_comp (dpi, mod);
      return;
    }
}

/* Print a function type, except for the return type.  */

static void
d_print_function_type (dpi, dc, mods)
     struct d_print_info *dpi;
     const struct d_comp *dc;
     struct d_print_mod *mods;
{
  if (mods != NULL)
    {
      int need_paren;
      int saw_mod;
      struct d_print_mod *p;

      need_paren = 0;
      saw_mod = 0;
      for (p = mods; p != NULL; p = p->next)
	{
	  if (p->printed)
	    break;

	  saw_mod = 1;
	  switch (p->mod->type)
	    {
	    case D_COMP_RESTRICT:
	    case D_COMP_VOLATILE:
	    case D_COMP_CONST:
	    case D_COMP_VENDOR_TYPE_QUAL:
	    case D_COMP_POINTER:
	    case D_COMP_REFERENCE:
	    case D_COMP_COMPLEX:
	    case D_COMP_IMAGINARY:
	    case D_COMP_PTRMEM_TYPE:
	      need_paren = 1;
	      break;
	    default:
	      break;
	    }
	  if (need_paren)
	    break;
	}

      if (d_left (dc) != NULL && ! saw_mod)
	need_paren = 1;

      if (need_paren)
	d_append_char (dpi, '(');

      d_print_mod_list (dpi, mods);

      if (need_paren)
	d_append_char (dpi, ')');
    }

  d_append_char (dpi, '(');

  if (d_right (dc) != NULL)
    d_print_comp (dpi, d_right (dc));

  d_append_char (dpi, ')');
}

/* Print an array type, except for the element type.  */

static void
d_print_array_type (dpi, dc, mods)
     struct d_print_info *dpi;
     const struct d_comp *dc;
     struct d_print_mod *mods;
{
  int need_space;

  need_space = 1;
  if (mods != NULL)
    {
      int need_paren;
      struct d_print_mod *p;

      need_paren = 0;
      for (p = mods; p != NULL; p = p->next)
	{
	  if (p->printed)
	    break;

	  if (p->mod->type == D_COMP_ARRAY_TYPE)
	    {
	      need_space = 0;
	      break;
	    }
	  else
	    {
	      need_paren = 1;
	      need_space = 1;
	      break;
	    }
	}

      if (need_paren)
	d_append_string (dpi, " (");

      d_print_mod_list (dpi, mods);

      if (need_paren)
	d_append_char (dpi, ')');
    }

  if (need_space)
    d_append_char (dpi, ' ');

  d_append_char (dpi, '[');

  if (d_left (dc) != NULL)
    d_print_comp (dpi, d_left (dc));

  d_append_char (dpi, ']');
}

/* Print an operator in an expression.  */

static void
d_print_expr_op (dpi, dc)
     struct d_print_info *dpi;
     const struct d_comp *dc;
{
  if (dc->type == D_COMP_OPERATOR)
    d_append_string (dpi, dc->u.s_operator.op->name);
  else
    d_print_comp (dpi, dc);
}

/* Print a cast.  */

static void
d_print_cast (dpi, dc)
     struct d_print_info *dpi;
     const struct d_comp *dc;
{
  if (d_left (dc)->type != D_COMP_TEMPLATE)
    d_print_comp (dpi, d_left (dc));
  else
    {
      struct d_print_template dpt;

      /* It appears that for a templated cast operator, we need to put
	 the template parameters in scope for the operator name, but
	 not for the parameters.  The effect is that we need to handle
	 the template printing here.  FIXME: Verify this.  */

      dpt.next = dpi->templates;
      dpi->templates = &dpt;
      dpt.template = d_left (dc);

      d_print_comp (dpi, d_left (d_left (dc)));

      dpi->templates = dpt.next;

      d_append_char (dpi, '<');
      d_print_comp (dpi, d_right (d_left (dc)));
      /* Avoid generating two consecutive '>' characters, to avoid
	 the C++ syntactic ambiguity.  */
      if (dpi->buf[dpi->len - 1] == '>')
	d_append_char (dpi, ' ');
      d_append_char (dpi, '>');
    }
}

/* Initialize the information structure we use to pass around
   information.  */

static int
d_init_info (mangled, options, len, di)
     const char *mangled;
     int options;
     size_t len;
     struct d_info *di;
{
  di->s = mangled;
  di->options = options;

  di->n = mangled;

  /* We can not need more components than twice the number of chars in
     the mangled string.  Most components correspond directly to
     chars, but the ARGLIST types are exceptions.  */
  di->num_comps = 2 * len;
  di->comps = (struct d_comp *) malloc (di->num_comps
					* sizeof (struct d_comp));
  di->next_comp = 0;

  /* Similarly, we can not need more substitutions than there are
     chars in the mangled string divided by 2, since it takes at least
     two chars to refer to a substitution.  */
  di->num_subs = (len + 1) / 2;
  di->subs = (struct d_comp **) malloc (di->num_subs
					* sizeof (struct d_comp *));
  di->next_sub = 0;

  di->last_name = NULL;

  if (di->comps == NULL || di->subs == NULL)
    {
      if (di->comps != NULL)
	free (di->comps);
      if (di->subs != NULL)
	free (di->subs);
      return 0;
    }

  return 1;
}

/* Entry point for the demangler.  If MANGLED is a g++ v3 ABI mangled
   name, return a buffer allocated with malloc holding the demangled
   name.  OPTIONS is the usual libiberty demangler options.  On
   success, this sets *PALC to the allocated size of the returned
   buffer.  On failure, this sets *PALC to 0 for a bad name, or 1 for
   a memory allocation failure.  On failure, this returns NULL.  */

static char *
d_demangle (mangled, options, palc)
     const char* mangled;
     int options;
     size_t *palc;
{
  size_t len;
  int type;
  struct d_info di;
  struct d_comp *dc;
  char *ret;

  *palc = 0;

  len = strlen (mangled);

  if (mangled[0] == '_' && mangled[1] == 'Z')
    type = 0;
  else if (strncmp (mangled, "_GLOBAL_", 8) == 0
	   && (mangled[8] == '.' || mangled[8] == '_' || mangled[8] == '$')
	   && (mangled[9] == 'D' || mangled[9] == 'I')
	   && mangled[10] == '_')
    {
      char *r;

      r = malloc (40 + len - 11);
      if (r == NULL)
	*palc = 1;
      else
	{
	  if (mangled[9] == 'I')
	    strcpy (r, "global constructors keyed to ");
	  else
	    strcpy (r, "global destructors keyed to ");
	  strcat (r, mangled + 11);
	}
      return r;
    }
  else
    {
      if ((options & DMGL_TYPES) == 0)
	return NULL;
      type = 1;
    }

  if (! d_init_info (mangled, options, len, &di))
    {
      *palc = 1;
      return NULL;
    }

  if (! type)
    dc = d_mangled_name (&di);
  else
    dc = d_type (&di);

#ifdef CP_DEMANGLE_DEBUG
  if (dc == NULL)
    printf ("failed demangling\n");
  else
    d_dump (dc, 0);
#endif

  free (di.subs);
  di.subs = NULL;

  ret = NULL;
  if (dc != NULL)
    ret = d_print (options, dc, palc);

  free (di.comps);

  return ret;
}

#if defined(IN_LIBGCC2) || defined(IN_GLIBCPP_V3)

extern char *__cxa_demangle PARAMS ((const char *, char *, size_t *, int *));

/* ia64 ABI-mandated entry point in the C++ runtime library for
   performing demangling.  MANGLED_NAME is a NUL-terminated character
   string containing the name to be demangled.

   OUTPUT_BUFFER is a region of memory, allocated with malloc, of
   *LENGTH bytes, into which the demangled name is stored.  If
   OUTPUT_BUFFER is not long enough, it is expanded using realloc.
   OUTPUT_BUFFER may instead be NULL; in that case, the demangled name
   is placed in a region of memory allocated with malloc.

   If LENGTH is non-NULL, the length of the buffer conaining the
   demangled name, is placed in *LENGTH.

   The return value is a pointer to the start of the NUL-terminated
   demangled name, or NULL if the demangling fails.  The caller is
   responsible for deallocating this memory using free.

   *STATUS is set to one of the following values:
      0: The demangling operation succeeded.
     -1: A memory allocation failure occurred.
     -2: MANGLED_NAME is not a valid name under the C++ ABI mangling rules.
     -3: One of the arguments is invalid.

   The demangling is performed using the C++ ABI mangling rules, with
   GNU extensions.  */

char *
__cxa_demangle (mangled_name, output_buffer, length, status)
     const char *mangled_name;
     char *output_buffer;
     size_t *length;
     int *status;
{
  char *demangled;
  size_t alc;

  if (status == NULL)
    return NULL;

  if (mangled_name == NULL)
    {
      *status = -3;
      return NULL;
    }

  if (output_buffer != NULL && length == NULL)
    {
      *status = -3;
      return NULL;
    }

  demangled = d_demangle (mangled_name, DMGL_TYPES, &alc);

  if (demangled == NULL)
    {
      if (alc == 1)
	*status = -1;
      else
	*status = -2;
      return NULL;
    }

  if (output_buffer == NULL)
    {
      if (length != NULL)
	*length = alc;
    }
  else
    {
      if (strlen (demangled) < *length)
	{
	  strcpy (output_buffer, demangled);
	  free (demangled);
	  demangled = output_buffer;
	}
      else
	{
	  free (output_buffer);
	  *length = alc;
	}
    }

  *status = 0;

  return demangled;
}

#else /* ! (IN_LIBGCC2 || IN_GLIBCPP_V3) */

/* Entry point for libiberty demangler.  If MANGLED is a g++ v3 ABI
   mangled name, return a buffer allocated with malloc holding the
   demangled name.  Otherwise, return NULL.  */

char *
cplus_demangle_v3 (mangled, options)
     const char* mangled;
     int options;
{
  size_t alc;

  return d_demangle (mangled, options, &alc);
}

/* Demangle a Java symbol.  Java uses a subset of the V3 ABI C++ mangling 
   conventions, but the output formatting is a little different.
   This instructs the C++ demangler not to emit pointer characters ("*"), and 
   to use Java's namespace separator symbol ("." instead of "::").  It then 
   does an additional pass over the demangled output to replace instances 
   of JArray<TYPE> with TYPE[].  */

char *
java_demangle_v3 (mangled)
     const char* mangled;
{
  size_t alc;
  char *demangled;
  int nesting;
  char *from;
  char *to;

  demangled = d_demangle (mangled, DMGL_JAVA, &alc);

  if (demangled == NULL)
    return NULL;

  nesting = 0;
  from = demangled;
  to = from;
  while (*from != '\0')
    {
      if (strncmp (from, "JArray<", 7) == 0)
	{
	  from += 7;
	  ++nesting;
	}
      else if (nesting > 0 && *from == '>')
	{
	  while (to > demangled && to[-1] == ' ')
	    --to;
	  *to++ = '[';
	  *to++ = ']';
	  --nesting;
	  ++from;
	}
      else
	*to++ = *from++;
    }

  *to = '\0';

  return demangled;
}

#endif /* IN_LIBGCC2 || IN_GLIBCPP_V3 */

#ifndef IN_GLIBCPP_V3

/* Demangle a string in order to find out whether it is a constructor
   or destructor.  Return non-zero on success.  Set *CTOR_KIND and
   *DTOR_KIND appropriately.  */

static int
is_ctor_or_dtor (mangled, ctor_kind, dtor_kind)
     const char *mangled;
     enum gnu_v3_ctor_kinds *ctor_kind;
     enum gnu_v3_dtor_kinds *dtor_kind;
{
  struct d_info di;
  struct d_comp *dc;

  *ctor_kind = (enum gnu_v3_ctor_kinds) 0;
  *dtor_kind = (enum gnu_v3_dtor_kinds) 0;

  if (! d_init_info (mangled, DMGL_GNU_V3, strlen (mangled), &di))
    return 0;

  dc = d_mangled_name (&di);

  if (dc == NULL)
    return 0;

  while (dc != NULL)
    {
      switch (dc->type)
	{
	default:
	  return 0;
	case D_COMP_TYPED_NAME:
	case D_COMP_TEMPLATE:
	case D_COMP_RESTRICT:
	case D_COMP_VOLATILE:
	case D_COMP_CONST:
	case D_COMP_VENDOR_TYPE_QUAL:
	  dc = d_left (dc);
	  break;
	case D_COMP_QUAL_NAME:
	  dc = d_right (dc);
	  break;
	case D_COMP_CTOR:
	  *ctor_kind = dc->u.s_ctor.kind;
	  return 1;
	case D_COMP_DTOR:
	  *dtor_kind = dc->u.s_dtor.kind;
	  return 1;
	}
    }

  return 0;
}

/* Return whether NAME is the mangled form of a g++ V3 ABI constructor
   name.  A non-zero return indicates the type of constructor.  */

enum gnu_v3_ctor_kinds
is_gnu_v3_mangled_ctor (name)
     const char *name;
{
  enum gnu_v3_ctor_kinds ctor_kind;
  enum gnu_v3_dtor_kinds dtor_kind;

  if (! is_ctor_or_dtor (name, &ctor_kind, &dtor_kind))
    return (enum gnu_v3_ctor_kinds) 0;
  return ctor_kind;
}


/* Return whether NAME is the mangled form of a g++ V3 ABI destructor
   name.  A non-zero return indicates the type of destructor.  */

enum gnu_v3_dtor_kinds
is_gnu_v3_mangled_dtor (name)
     const char *name;
{
  enum gnu_v3_ctor_kinds ctor_kind;
  enum gnu_v3_dtor_kinds dtor_kind;

  if (! is_ctor_or_dtor (name, &ctor_kind, &dtor_kind))
    return (enum gnu_v3_dtor_kinds) 0;
  return dtor_kind;
}

#endif /* IN_GLIBCPP_V3 */

#ifdef STANDALONE_DEMANGLER

#include "getopt.h"
#include "dyn-string.h"

static void print_usage PARAMS ((FILE* fp, int exit_value));

#define IS_ALPHA(CHAR)                                                  \
  (((CHAR) >= 'a' && (CHAR) <= 'z')                                     \
   || ((CHAR) >= 'A' && (CHAR) <= 'Z'))

/* Non-zero if CHAR is a character than can occur in a mangled name.  */
#define is_mangled_char(CHAR)                                           \
  (IS_ALPHA (CHAR) || IS_DIGIT (CHAR)                                   \
   || (CHAR) == '_' || (CHAR) == '.' || (CHAR) == '$')

/* The name of this program, as invoked.  */
const char* program_name;

/* Prints usage summary to FP and then exits with EXIT_VALUE.  */

static void
print_usage (fp, exit_value)
     FILE* fp;
     int exit_value;
{
  fprintf (fp, "Usage: %s [options] [names ...]\n", program_name);
  fprintf (fp, "Options:\n");
  fprintf (fp, "  -h,--help       Display this message.\n");
  fprintf (fp, "  -p,--no-params  Don't display function parameters\n");
  fprintf (fp, "  -v,--verbose    Produce verbose demanglings.\n");
  fprintf (fp, "If names are provided, they are demangled.  Otherwise filters standard input.\n");

  exit (exit_value);
}

/* Option specification for getopt_long.  */
static const struct option long_options[] = 
{
  { "help",	 no_argument, NULL, 'h' },
  { "no-params", no_argument, NULL, 'p' },
  { "verbose",   no_argument, NULL, 'v' },
  { NULL,        no_argument, NULL, 0   },
};

/* Main entry for a demangling filter executable.  It will demangle
   its command line arguments, if any.  If none are provided, it will
   filter stdin to stdout, replacing any recognized mangled C++ names
   with their demangled equivalents.  */

int
main (argc, argv)
     int argc;
     char *argv[];
{
  int i;
  int opt_char;
  int options = DMGL_PARAMS | DMGL_ANSI | DMGL_TYPES;

  /* Use the program name of this program, as invoked.  */
  program_name = argv[0];

  /* Parse options.  */
  do 
    {
      opt_char = getopt_long (argc, argv, "hpv", long_options, NULL);
      switch (opt_char)
	{
	case '?':  /* Unrecognized option.  */
	  print_usage (stderr, 1);
	  break;

	case 'h':
	  print_usage (stdout, 0);
	  break;

	case 'p':
	  options &= ~ DMGL_PARAMS;
	  break;

	case 'v':
	  options |= DMGL_VERBOSE;
	  break;
	}
    }
  while (opt_char != -1);

  if (optind == argc) 
    /* No command line arguments were provided.  Filter stdin.  */
    {
      dyn_string_t mangled = dyn_string_new (3);
      char *s;

      /* Read all of input.  */
      while (!feof (stdin))
	{
	  char c;

	  /* Pile characters into mangled until we hit one that can't
	     occur in a mangled name.  */
	  c = getchar ();
	  while (!feof (stdin) && is_mangled_char (c))
	    {
	      dyn_string_append_char (mangled, c);
	      if (feof (stdin))
		break;
	      c = getchar ();
	    }

	  if (dyn_string_length (mangled) > 0)
	    {
	      s = cplus_demangle_v3 (dyn_string_buf (mangled), options);

	      if (s != NULL)
		{
		  fputs (s, stdout);
		  free (s);
		}
	      else
		{
		  /* It might not have been a mangled name.  Print the
		     original text.  */
		  fputs (dyn_string_buf (mangled), stdout);
		}

	      dyn_string_clear (mangled);
	    }

	  /* If we haven't hit EOF yet, we've read one character that
	     can't occur in a mangled name, so print it out.  */
	  if (!feof (stdin))
	    putchar (c);
	}

      dyn_string_delete (mangled);
    }
  else
    /* Demangle command line arguments.  */
    {
      /* Loop over command line arguments.  */
      for (i = optind; i < argc; ++i)
	{
	  char *s;

	  /* Attempt to demangle.  */
	  s = cplus_demangle_v3 (argv[i], options);

	  /* If it worked, print the demangled name.  */
	  if (s != NULL)
	    {
	      printf ("%s\n", s);
	      free (s);
	    }
	  else
	    fprintf (stderr, "Failed: %s\n", argv[i]);
	}
    }

  return 0;
}

#endif /* STANDALONE_DEMANGLER */
