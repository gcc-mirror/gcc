/* Encoding of types for Objective C.
   Copyright (C) 1993, 1995, 1996, 1997, 1998, 2000, 2002
   Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup
   Bitfield support by Ovidiu Predescu

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

/* FIXME: This file has no business including tm.h.  */

#include "tconfig.h"
#include "coretypes.h"
#include "tm.h"
#include "objc-api.h"
#include "encoding.h"
#include <stdlib.h>

#undef  MAX
#define MAX(X, Y)                    \
  ({ typeof (X) __x = (X), __y = (Y); \
     (__x > __y ? __x : __y); })

#undef  MIN
#define MIN(X, Y)                    \
  ({ typeof (X) __x = (X), __y = (Y); \
     (__x < __y ? __x : __y); })

#undef  ROUND
#define ROUND(V, A) \
  ({ typeof (V) __v = (V); typeof (A) __a = (A); \
     __a * ((__v+__a - 1)/__a); })


/* Various hacks for objc_layout_record. These are used by the target
   macros. */

#define TREE_CODE(TYPE) *(TYPE)
#define TREE_TYPE(TREE) (TREE)

#define RECORD_TYPE     _C_STRUCT_B
#define UNION_TYPE      _C_UNION_B
#define QUAL_UNION_TYPE _C_UNION_B
#define ARRAY_TYPE      _C_ARY_B

#define REAL_TYPE       _C_DBL

#define VECTOR_TYPE	_C_VECTOR

#define TYPE_FIELDS(TYPE)     objc_skip_typespec (TYPE)

#define DECL_MODE(TYPE) *(TYPE)
#define TYPE_MODE(TYPE) *(TYPE)

#define DFmode          _C_DBL

#define get_inner_array_type(TYPE)      ((TYPE) + 1)

/* Some ports (eg ARM) allow the structure size boundary to be
   selected at compile-time.  We override the normal definition with
   one that has a constant value for this compilation.  */
#undef STRUCTURE_SIZE_BOUNDARY
#define STRUCTURE_SIZE_BOUNDARY (BITS_PER_UNIT * sizeof (struct{char a;}))

/* Some ROUND_TYPE_ALIGN macros use TARGET_foo, and consequently
   target_flags.  Define a dummy entry here to so we don't die.  */
/* ??? FIXME: As of 2002-06-21, the attribute `unused' doesn't seem to
   eliminate the warning.  */
static int __attribute__ ((__unused__)) target_flags = 0;


/*  FIXME: while this file has no business including tm.h, this
    definitely has no business defining this macro but it
    is only way around without really rewritting this file,
    should look after the branch of 3.4 to fix this.  */
#define rs6000_special_round_type_align(STRUCT, COMPUTED, SPECIFIED)	\
  ((TYPE_FIELDS (STRUCT) != 0						\
    && DECL_MODE (TYPE_FIELDS (STRUCT)) == DFmode)			\
   ? MAX (MAX (COMPUTED, SPECIFIED), 64)				\
   : MAX (COMPUTED, SPECIFIED))

/*
  return the size of an object specified by type
*/

int
objc_sizeof_type (const char *type)
{
  /* Skip the variable name if any */
  if (*type == '"')
    {
      for (type++; *type++ != '"';)
	/* do nothing */;
    }

  switch (*type) {
  case _C_ID:
    return sizeof (id);
    break;

  case _C_CLASS:
    return sizeof (Class);
    break;

  case _C_SEL:
    return sizeof (SEL);
    break;

  case _C_CHR:
    return sizeof (char);
    break;

  case _C_UCHR:
    return sizeof (unsigned char);
    break;

  case _C_SHT:
    return sizeof (short);
    break;

  case _C_USHT:
    return sizeof (unsigned short);
    break;

  case _C_INT:
    return sizeof (int);
    break;

  case _C_UINT:
    return sizeof (unsigned int);
    break;

  case _C_LNG:
    return sizeof (long);
    break;

  case _C_ULNG:
    return sizeof (unsigned long);
    break;

  case _C_LNG_LNG:
    return sizeof (long long);
    break;

  case _C_ULNG_LNG:
    return sizeof (unsigned long long);
    break;

  case _C_FLT:
    return sizeof (float);
    break;

  case _C_DBL:
    return sizeof (double);
    break;

  case _C_VOID:
    return sizeof (void);
    break;

  case _C_PTR:
  case _C_ATOM:
  case _C_CHARPTR:
    return sizeof (char *);
    break;

  case _C_ARY_B:
    {
      int len = atoi (type + 1);
      while (isdigit ((unsigned char)*++type))
	;
      return len * objc_aligned_size (type);
    }
    break;

  case _C_BFLD:
    {
      /* The new encoding of bitfields is: b 'position' 'type' 'size' */
      int position, size;
      int startByte, endByte;

      position = atoi (type + 1);
      while (isdigit ((unsigned char)*++type))
	;
      size = atoi (type + 1);

      startByte = position / BITS_PER_UNIT;
      endByte = (position + size) / BITS_PER_UNIT;
      return endByte - startByte;
    }

  case _C_STRUCT_B:
    {
      struct objc_struct_layout layout;
      unsigned int size;

      objc_layout_structure (type, &layout);
      while (objc_layout_structure_next_member (&layout))
        /* do nothing */ ;
      objc_layout_finish_structure (&layout, &size, NULL);

      return size;
    }

  case _C_UNION_B:
    {
      int max_size = 0;
      while (*type != _C_UNION_E && *type++ != '=')
	/* do nothing */;
      while (*type != _C_UNION_E)
	{
	  /* Skip the variable name if any */
	  if (*type == '"')
	    {
	      for (type++; *type++ != '"';)
		/* do nothing */;
	    }
	  max_size = MAX (max_size, objc_sizeof_type (type));
	  type = objc_skip_typespec (type);
	}
      return max_size;
    }

  default:
    {
      objc_error (nil, OBJC_ERR_BAD_TYPE, "unknown type %s\n", type);
      return 0;
    }
  }
}


/*
  Return the alignment of an object specified by type
*/

int
objc_alignof_type (const char *type)
{
  /* Skip the variable name if any */
  if (*type == '"')
    {
      for (type++; *type++ != '"';)
	/* do nothing */;
    }
  switch (*type) {
  case _C_ID:
    return __alignof__ (id);
    break;

  case _C_CLASS:
    return __alignof__ (Class);
    break;

  case _C_SEL:
    return __alignof__ (SEL);
    break;

  case _C_CHR:
    return __alignof__ (char);
    break;

  case _C_UCHR:
    return __alignof__ (unsigned char);
    break;

  case _C_SHT:
    return __alignof__ (short);
    break;

  case _C_USHT:
    return __alignof__ (unsigned short);
    break;

  case _C_INT:
    return __alignof__ (int);
    break;

  case _C_UINT:
    return __alignof__ (unsigned int);
    break;

  case _C_LNG:
    return __alignof__ (long);
    break;

  case _C_ULNG:
    return __alignof__ (unsigned long);
    break;

  case _C_LNG_LNG:
    return __alignof__ (long long);
    break;

  case _C_ULNG_LNG:
    return __alignof__ (unsigned long long);
    break;

  case _C_FLT:
    return __alignof__ (float);
    break;

  case _C_DBL:
    return __alignof__ (double);
    break;

  case _C_PTR:
  case _C_ATOM:
  case _C_CHARPTR:
    return __alignof__ (char *);
    break;

  case _C_ARY_B:
    while (isdigit ((unsigned char)*++type))
      /* do nothing */;
    return objc_alignof_type (type);

  case _C_STRUCT_B:
    {
      struct objc_struct_layout layout;
      unsigned int align;

      objc_layout_structure (type, &layout);
      while (objc_layout_structure_next_member (&layout))
        /* do nothing */;
      objc_layout_finish_structure (&layout, NULL, &align);

      return align;
    }

  case _C_UNION_B:
    {
      int maxalign = 0;
      while (*type != _C_UNION_E && *type++ != '=')
	/* do nothing */;
      while (*type != _C_UNION_E)
	{
	  /* Skip the variable name if any */
	  if (*type == '"')
	    {
	      for (type++; *type++ != '"';)
		/* do nothing */;
	    }
	  maxalign = MAX (maxalign, objc_alignof_type (type));
	  type = objc_skip_typespec (type);
	}
      return maxalign;
    }

  default:
    {
      objc_error (nil, OBJC_ERR_BAD_TYPE, "unknown type %s\n", type);
      return 0;
    }
  }
}

/*
  The aligned size if the size rounded up to the nearest alignment.
*/

int
objc_aligned_size (const char *type)
{
  int size, align;

  /* Skip the variable name */
  if (*type == '"')
    {
      for (type++; *type++ != '"';)
	/* do nothing */;
    }

  size = objc_sizeof_type (type);
  align = objc_alignof_type (type);

  return ROUND (size, align);
}

/*
  The size rounded up to the nearest integral of the wordsize, taken
  to be the size of a void *.
*/

int
objc_promoted_size (const char *type)
{
  int size, wordsize;

  /* Skip the variable name */
  if (*type == '"')
    {
      for (type++; *type++ != '"';)
	/* do nothing */;
    }

  size = objc_sizeof_type (type);
  wordsize = sizeof (void *);

  return ROUND (size, wordsize);
}

/*
  Skip type qualifiers.  These may eventually precede typespecs
  occurring in method prototype encodings.
*/

inline const char *
objc_skip_type_qualifiers (const char *type)
{
  while (*type == _C_CONST
	 || *type == _C_IN
	 || *type == _C_INOUT
	 || *type == _C_OUT
	 || *type == _C_BYCOPY
         || *type == _C_BYREF
	 || *type == _C_ONEWAY
	 || *type == _C_GCINVISIBLE)
    {
      type += 1;
    }
  return type;
}


/*
  Skip one typespec element.  If the typespec is prepended by type
  qualifiers, these are skipped as well.
*/

const char *
objc_skip_typespec (const char *type)
{
  /* Skip the variable name if any */
  if (*type == '"')
    {
      for (type++; *type++ != '"';)
	/* do nothing */;
    }

  type = objc_skip_type_qualifiers (type);

  switch (*type) {

  case _C_ID:
    /* An id may be annotated by the actual type if it is known
       with the @"ClassName" syntax */

    if (*++type != '"')
      return type;
    else
      {
	while (*++type != '"')
	  /* do nothing */;
	return type + 1;
      }

    /* The following are one character type codes */
  case _C_CLASS:
  case _C_SEL:
  case _C_CHR:
  case _C_UCHR:
  case _C_CHARPTR:
  case _C_ATOM:
  case _C_SHT:
  case _C_USHT:
  case _C_INT:
  case _C_UINT:
  case _C_LNG:
  case _C_ULNG:
  case _C_LNG_LNG:
  case _C_ULNG_LNG:
  case _C_FLT:
  case _C_DBL:
  case _C_VOID:
  case _C_UNDEF:
    return ++type;
    break;

  case _C_ARY_B:
    /* skip digits, typespec and closing ']' */

    while (isdigit ((unsigned char)*++type))
      ;
    type = objc_skip_typespec (type);
    if (*type == _C_ARY_E)
      return ++type;
    else
      {
	objc_error (nil, OBJC_ERR_BAD_TYPE, "bad array type %s\n", type);
	return 0;
      }

  case _C_BFLD:
    /* The new encoding of bitfields is: b 'position' 'type' 'size' */
    while (isdigit ((unsigned char)*++type))
      ;	/* skip position */
    while (isdigit ((unsigned char)*++type))
      ;	/* skip type and size */
    return type;

  case _C_STRUCT_B:
    /* skip name, and elements until closing '}'  */

    while (*type != _C_STRUCT_E && *type++ != '=')
      ;
    while (*type != _C_STRUCT_E)
      {
	type = objc_skip_typespec (type);
      }
    return ++type;

  case _C_UNION_B:
    /* skip name, and elements until closing ')'  */

    while (*type != _C_UNION_E && *type++ != '=')
      ;
    while (*type != _C_UNION_E)
      {
	type = objc_skip_typespec (type);
      }
    return ++type;

  case _C_PTR:
    /* Just skip the following typespec */

    return objc_skip_typespec (++type);

  default:
    {
      objc_error (nil, OBJC_ERR_BAD_TYPE, "unknown type %s\n", type);
      return 0;
    }
  }
}

/*
  Skip an offset as part of a method encoding.  This is prepended by a
  '+' if the argument is passed in registers.
*/
inline const char *
objc_skip_offset (const char *type)
{
  if (*type == '+')
    type++;
  while (isdigit ((unsigned char) *++type))
    ;
  return type;
}

/*
  Skip an argument specification of a method encoding.
*/
const char *
objc_skip_argspec (const char *type)
{
  type = objc_skip_typespec (type);
  type = objc_skip_offset (type);
  return type;
}

/*
  Return the number of arguments that the method MTH expects.
  Note that all methods need two implicit arguments `self' and
  `_cmd'.
*/
int
method_get_number_of_arguments (struct objc_method *mth)
{
  int i = 0;
  const char *type = mth->method_types;
  while (*type)
    {
      type = objc_skip_argspec (type);
      i += 1;
    }
  return i - 1;
}

/*
  Return the size of the argument block needed on the stack to invoke
  the method MTH.  This may be zero, if all arguments are passed in
  registers.
*/

int
method_get_sizeof_arguments (struct objc_method *mth)
{
  const char *type = objc_skip_typespec (mth->method_types);
  return atoi (type);
}

/*
  Return a pointer to the next argument of ARGFRAME.  type points to
  the last argument.  Typical use of this look like:

  {
    char *datum, *type;
    for (datum = method_get_first_argument (method, argframe, &type);
         datum; datum = method_get_next_argument (argframe, &type))
      {
        unsigned flags = objc_get_type_qualifiers (type);
        type = objc_skip_type_qualifiers (type);
	if (*type != _C_PTR)
          [portal encodeData: datum ofType: type];
	else
	  {
	    if ((flags & _F_IN) == _F_IN)
              [portal encodeData: *(char **) datum ofType: ++type];
	  }
      }
  }
*/

char *
method_get_next_argument (arglist_t argframe, const char **type)
{
  const char *t = objc_skip_argspec (*type);

  if (*t == '\0')
    return 0;

  *type = t;
  t = objc_skip_typespec (t);

  if (*t == '+')
    return argframe->arg_regs + atoi (++t);
  else
    return argframe->arg_ptr + atoi (t);
}

/*
  Return a pointer to the value of the first argument of the method
  described in M with the given argumentframe ARGFRAME.  The type
  is returned in TYPE.  type must be passed to successive calls of
  method_get_next_argument.
*/
char *
method_get_first_argument (struct objc_method *m,
			   arglist_t argframe,
			   const char **type)
{
  *type = m->method_types;
  return method_get_next_argument (argframe, type);
}

/*
   Return a pointer to the ARGth argument of the method
   M from the frame ARGFRAME.  The type of the argument
   is returned in the value-result argument TYPE
*/

char *
method_get_nth_argument (struct objc_method *m,
			 arglist_t argframe, int arg,
			 const char **type)
{
  const char *t = objc_skip_argspec (m->method_types);

  if (arg > method_get_number_of_arguments (m))
    return 0;

  while (arg--)
    t = objc_skip_argspec (t);

  *type = t;
  t = objc_skip_typespec (t);

  if (*t == '+')
    return argframe->arg_regs + atoi (++t);
  else
    return argframe->arg_ptr + atoi (t);
}

unsigned
objc_get_type_qualifiers (const char *type)
{
  unsigned res = 0;
  BOOL flag = YES;

  while (flag)
    switch (*type++)
      {
      case _C_CONST:	res |= _F_CONST; break;
      case _C_IN:	res |= _F_IN; break;
      case _C_INOUT:	res |= _F_INOUT; break;
      case _C_OUT:	res |= _F_OUT; break;
      case _C_BYCOPY:	res |= _F_BYCOPY; break;
      case _C_BYREF:  res |= _F_BYREF; break;
      case _C_ONEWAY:	res |= _F_ONEWAY; break;
      case _C_GCINVISIBLE: res |= _F_GCINVISIBLE; break;
      default: flag = NO;
    }

  return res;
}


/* The following three functions can be used to determine how a
   structure is laid out by the compiler. For example:

  struct objc_struct_layout layout;
  int i;

  objc_layout_structure (type, &layout);
  while (objc_layout_structure_next_member (&layout))
    {
      int position, align;
      const char *type;

      objc_layout_structure_get_info (&layout, &position, &align, &type);
      printf ("element %d has offset %d, alignment %d\n",
              i++, position, align);
    }

  These functions are used by objc_sizeof_type and objc_alignof_type
  functions to compute the size and alignment of structures. The
  previous method of computing the size and alignment of a structure
  was not working on some architectures, particulary on AIX, and in
  the presence of bitfields inside the structure. */
void
objc_layout_structure (const char *type,
                           struct objc_struct_layout *layout)
{
  const char *ntype;

  if (*type++ != _C_STRUCT_B)
    {
      objc_error (nil, OBJC_ERR_BAD_TYPE,
                 "record type expected in objc_layout_structure, got %s\n",
                 type);
    }

  layout->original_type = type;

  /* Skip "<name>=" if any. Avoid embedded structures and unions. */
  ntype = type;
  while (*ntype != _C_STRUCT_E && *ntype != _C_STRUCT_B && *ntype != _C_UNION_B
         && *ntype++ != '=')
    /* do nothing */;

  /* If there's a "<name>=", ntype - 1 points to '='; skip the the name */
  if (*(ntype - 1) == '=')
    type = ntype;

  layout->type = type;
  layout->prev_type = NULL;
  layout->record_size = 0;
  layout->record_align = BITS_PER_UNIT;

  layout->record_align = MAX (layout->record_align, STRUCTURE_SIZE_BOUNDARY);
}


BOOL
objc_layout_structure_next_member (struct objc_struct_layout *layout)
{
  register int desired_align = 0;

  /* The following are used only if the field is a bitfield */
  register const char *bfld_type = 0;
  register int bfld_type_size, bfld_type_align = 0, bfld_field_size = 0;

  /* The current type without the type qualifiers */
  const char *type;

  /* Add the size of the previous field to the size of the record.  */
  if (layout->prev_type)
    {
      type = objc_skip_type_qualifiers (layout->prev_type);

      if (*type != _C_BFLD)
        layout->record_size += objc_sizeof_type (type) * BITS_PER_UNIT;
      else {
        /* Get the bitfield's type */
        for (bfld_type = type + 1;
             isdigit ((unsigned char)*bfld_type);
             bfld_type++)
          /* do nothing */;

        bfld_type_size = objc_sizeof_type (bfld_type) * BITS_PER_UNIT;
        bfld_type_align = objc_alignof_type (bfld_type) * BITS_PER_UNIT;
        bfld_field_size = atoi (objc_skip_typespec (bfld_type));
        layout->record_size += bfld_field_size;
      }
    }

  if (*layout->type == _C_STRUCT_E)
    return NO;

  /* Skip the variable name if any */
  if (*layout->type == '"')
    {
      for (layout->type++; *layout->type++ != '"';)
        /* do nothing */;
    }

  type = objc_skip_type_qualifiers (layout->type);

  if (*type != _C_BFLD)
    desired_align = objc_alignof_type (type) * BITS_PER_UNIT;
  else
    {
      desired_align = 1;
      /* Skip the bitfield's offset */
      for (bfld_type = type + 1;
           isdigit ((unsigned char) *bfld_type);
           bfld_type++)
        /* do nothing */;

      bfld_type_size = objc_sizeof_type (bfld_type) * BITS_PER_UNIT;
      bfld_type_align = objc_alignof_type (bfld_type) * BITS_PER_UNIT;
      bfld_field_size = atoi (objc_skip_typespec (bfld_type));
    }

#ifdef BIGGEST_FIELD_ALIGNMENT
  desired_align = MIN (desired_align, BIGGEST_FIELD_ALIGNMENT);
#endif
#ifdef ADJUST_FIELD_ALIGN
  desired_align = ADJUST_FIELD_ALIGN (type, desired_align);
#endif

  /* Record must have at least as much alignment as any field.
     Otherwise, the alignment of the field within the record
     is meaningless.  */
#ifndef PCC_BITFIELD_TYPE_MATTERS
  layout->record_align = MAX (layout->record_align, desired_align);
#else	/* PCC_BITFIELD_TYPE_MATTERS */
  if (*type == _C_BFLD)
    {
      /* For these machines, a zero-length field does not
         affect the alignment of the structure as a whole.
         It does, however, affect the alignment of the next field
         within the structure.  */
      if (bfld_field_size)
        layout->record_align = MAX (layout->record_align, desired_align);
      else
        desired_align = objc_alignof_type (bfld_type) * BITS_PER_UNIT;

      /* A named bit field of declared type `int'
         forces the entire structure to have `int' alignment.
         Q1: How is encoded this thing and how to check for it?
         Q2: How to determine maximum_field_alignment at runtime? */

/*	  if (DECL_NAME (field) != 0) */
      {
        int type_align = bfld_type_align;
#if 0
        if (maximum_field_alignment != 0)
          type_align = MIN (type_align, maximum_field_alignment);
        else if (DECL_PACKED (field))
          type_align = MIN (type_align, BITS_PER_UNIT);
#endif

        layout->record_align = MAX (layout->record_align, type_align);
      }
    }
  else
    layout->record_align = MAX (layout->record_align, desired_align);
#endif	/* PCC_BITFIELD_TYPE_MATTERS */

  /* Does this field automatically have alignment it needs
     by virtue of the fields that precede it and the record's
     own alignment?  */

  if (*type == _C_BFLD)
    layout->record_size = atoi (type + 1);
  else if (layout->record_size % desired_align != 0)
    {
      /* No, we need to skip space before this field.
         Bump the cumulative size to multiple of field alignment.  */
      layout->record_size = ROUND (layout->record_size, desired_align);
    }

  /* Jump to the next field in record. */

  layout->prev_type = layout->type;
  layout->type = objc_skip_typespec (layout->type);      /* skip component */

  return YES;
}


void objc_layout_finish_structure (struct objc_struct_layout *layout,
                                   unsigned int *size,
                                   unsigned int *align)
{
  if (layout->type && *layout->type == _C_STRUCT_E)
    {
      /* Work out the alignment of the record as one expression and store
         in the record type.  Round it up to a multiple of the record's
         alignment. */

#if defined (ROUND_TYPE_ALIGN) && ! defined (__sparc__)
      layout->record_align = ROUND_TYPE_ALIGN (layout->original_type,
                                               1,
                                               layout->record_align);
#else
      layout->record_align = MAX (1, layout->record_align);
#endif

#ifdef ROUND_TYPE_SIZE
      layout->record_size = ROUND_TYPE_SIZE (layout->original_type,
                                             layout->record_size,
                                             layout->record_align);
#else
      /* Round the size up to be a multiple of the required alignment */
      layout->record_size = ROUND (layout->record_size, layout->record_align);
#endif

      layout->type = NULL;
    }
  if (size)
    *size = layout->record_size / BITS_PER_UNIT;
  if (align)
    *align = layout->record_align / BITS_PER_UNIT;
}


void objc_layout_structure_get_info (struct objc_struct_layout *layout,
                                     unsigned int *offset,
                                     unsigned int *align,
                                     const char **type)
{
  if (offset)
    *offset = layout->record_size / BITS_PER_UNIT;
  if (align)
    *align = layout->record_align / BITS_PER_UNIT;
  if (type)
    *type = layout->prev_type;
}
