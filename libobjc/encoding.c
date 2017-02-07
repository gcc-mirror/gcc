/* Encoding of types for Objective C.
   Copyright (C) 1993-2017 Free Software Foundation, Inc.
   Contributed by Kresten Krab Thorup
   Bitfield support by Ovidiu Predescu

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

/* FIXME: This file has no business including tm.h.  */

/* FIXME: This file contains functions that will abort the entire
   program if they fail.  Is that really needed ?  */

#include "config.h"
#include "objc-private/common.h"
#include "objc-private/error.h"
#include "tconfig.h"
#include "coretypes.h"
#include "tm.h"
#include "objc/runtime.h"
#include "objc-private/module-abi-8.h" /* For struct objc_method */
#include <stdlib.h>
#include <ctype.h>
#include <string.h>                    /* For memcpy.  */

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

#define TYPE_FIELDS(TYPE)           ({const char *_field = (TYPE)+1; \
    while (*_field != _C_STRUCT_E && *_field != _C_STRUCT_B \
           && *_field != _C_UNION_B && *_field++ != '=') \
    /* do nothing */; \
    _field;})

#define DECL_MODE(TYPE) *(TYPE)
#define TYPE_MODE(TYPE) *(TYPE)

#define DFmode          _C_DBL

#define strip_array_types(TYPE)      ({const char *_field = (TYPE); \
  while (*_field == _C_ARY_B)\
    {\
      while (isdigit ((unsigned char)*++_field))\
	;\
    }\
    _field;})

/* Some ports (eg ARM) allow the structure size boundary to be
   selected at compile-time.  We override the normal definition with
   one that has a constant value for this compilation.  */
#undef  STRUCTURE_SIZE_BOUNDARY
#define STRUCTURE_SIZE_BOUNDARY (__CHAR_BIT__ * sizeof (struct{char a;}))

/* Some ROUND_TYPE_ALIGN macros use TARGET_foo, and consequently
   target_flags.  Define a dummy entry here to so we don't die.
   We have to rename it because target_flags may already have been
   declared extern.  */
#define target_flags not_target_flags
static int __attribute__ ((__unused__)) not_target_flags = 0;

/* Some ROUND_TYPE_ALIGN use ALTIVEC_VECTOR_MODE (rs6000 darwin).
   Define a dummy ALTIVEC_VECTOR_MODE so it will not die.  */
#undef ALTIVEC_VECTOR_MODE
#define ALTIVEC_VECTOR_MODE(MODE) (0)

/* Replace TARGET_VSX, TARGET_ALTIVEC, and TARGET_64BIT with constants based on
   the current switches, rather than looking in the options structure.  */
#ifdef _ARCH_PPC
#undef TARGET_VSX
#undef TARGET_ALTIVEC
#undef TARGET_64BIT

#ifdef __VSX__
#define TARGET_VSX 1
#else
#define TARGET_VSX 0
#endif

#ifdef __ALTIVEC__
#define TARGET_ALTIVEC 1
#else
#define TARGET_ALTIVEC 0
#endif

#ifdef _ARCH_PPC64
#define TARGET_64BIT 1
#else
#define TARGET_64BIT 0
#endif
#endif

/* Furthermore, some (powerpc) targets also use TARGET_ALIGN_NATURAL
 in their alignment macros. Currently[4.5/6], rs6000.h points this
 to a static variable, initialized by target overrides. This is reset
 in linux64.h but not in darwin64.h.  The macro is not used by *86*.  */

#if __MACH__ 
# if __LP64__
#  undef TARGET_ALIGN_NATURAL
#  define TARGET_ALIGN_NATURAL 1
# endif

/* On Darwin32, we need to recurse until we find the starting stuct type.  */
static int 
_darwin_rs6000_special_round_type_align (const char *struc, int comp, int spec)
{
  const char *_stp , *_fields = TYPE_FIELDS (struc);
  if (!_fields)
    return MAX (comp, spec);
  _stp = strip_array_types (_fields);
  if (TYPE_MODE(_stp) == _C_COMPLEX)
   _stp++;
  switch (TYPE_MODE(_stp))
    {
      case RECORD_TYPE:
      case UNION_TYPE:
	return MAX (MAX (comp, spec), objc_alignof_type (_stp) * __CHAR_BIT__);
	break;
      case DFmode:
      case _C_LNG_LNG:
      case _C_ULNG_LNG:
	return MAX (MAX (comp, spec), 64);
	break;

      default:
	return MAX (comp, spec);
	break;
    }
}

/* See comment below.  */
#define darwin_rs6000_special_round_type_align(S,C,S2)			\
  (_darwin_rs6000_special_round_type_align ((char*)(S), (int)(C), (int)(S2)))
#endif

/*  FIXME: while this file has no business including tm.h, this
    definitely has no business defining this macro but it
    is only way around without really rewritting this file,
    should look after the branch of 3.4 to fix this.   */
#define rs6000_special_round_type_align(STRUCT, COMPUTED, SPECIFIED)	\
  ({ const char *_fields = TYPE_FIELDS (STRUCT);			\
  ((_fields != 0							\
    && TYPE_MODE (strip_array_types (TREE_TYPE (_fields))) == DFmode)	\
   ? MAX (MAX (COMPUTED, SPECIFIED), 64)				\
   : MAX (COMPUTED, SPECIFIED));})

#define rs6000_special_adjust_field_align_p(FIELD, COMPUTED) 0

/* Skip a variable name, enclosed in quotes (").  */
static inline
const char *
objc_skip_variable_name (const char *type)
{
  /* Skip the variable name if any.  */
  if (*type == '"')
    {
      /* FIXME: How do we know we won't read beyond the end of the
	 string.  Here and in the rest of the file!  */
      /* Skip '"'.  */
      type++;
      /* Skip to the next '"'.  */
      while (*type != '"')
	type++;
      /* Skip '"'.  */
      type++;
    }

  return type;
}

int
objc_sizeof_type (const char *type)
{
  type = objc_skip_variable_name (type);

  switch (*type) {
  case _C_BOOL:
    return sizeof (_Bool);
    break;

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

  case _C_LNG_DBL:
    return sizeof (long double);
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

  case _C_VECTOR:
    {
      /* Skip the '!'.  */
      type++;
      /* Skip the '['.  */
      type++;

      /* The size in bytes is the following number.  */
      int size = atoi (type);
      return size;
    }
    break;

  case _C_BFLD:
    {
      /* The GNU encoding of bitfields is: b 'position' 'type'
	 'size'.  */
      int position, size;
      int startByte, endByte;

      position = atoi (type + 1);
      while (isdigit ((unsigned char)*++type))
	;
      size = atoi (type + 1);

      startByte = position / __CHAR_BIT__;
      endByte = (position + size) / __CHAR_BIT__;
      return endByte - startByte;
    }

  case _C_UNION_B:
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
    
  case _C_COMPLEX:
    {
      type++; /* Skip after the 'j'. */
      switch (*type)
        {
	    case _C_CHR:
	      return sizeof (_Complex char);
	      break;

	    case _C_UCHR:
	      return sizeof (_Complex unsigned char);
	      break;

	    case _C_SHT:
	      return sizeof (_Complex short);
	      break;

	    case _C_USHT:
	      return sizeof (_Complex unsigned short);
	      break;

	    case _C_INT:
	      return sizeof (_Complex int);
	      break;

	    case _C_UINT:
	      return sizeof (_Complex unsigned int);
	      break;

	    case _C_LNG:
	      return sizeof (_Complex long);
	      break;

	    case _C_ULNG:
	      return sizeof (_Complex unsigned long);
	      break;

	    case _C_LNG_LNG:
	      return sizeof (_Complex long long);
	      break;

	    case _C_ULNG_LNG:
	      return sizeof (_Complex unsigned long long);
	      break;

	    case _C_FLT:
	      return sizeof (_Complex float);
	      break;

	    case _C_DBL:
	      return sizeof (_Complex double);
	      break;

	    case _C_LNG_DBL:
	      return sizeof (_Complex long double);
	      break;
	    
	    default:
	      {
		/* FIXME: Is this so bad that we have to abort the
		   entire program ?  (it applies to all the other
		   _objc_abort calls in this file).
		*/
		_objc_abort ("unknown complex type %s\n", type);
		return 0;
	      }
	}
    }

  default:
    {
      _objc_abort ("unknown type %s\n", type);
      return 0;
    }
  }
}

int
objc_alignof_type (const char *type)
{
  type = objc_skip_variable_name (type);

  switch (*type) {
  case _C_BOOL:
    return __alignof__ (_Bool);
    break;

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

  case _C_LNG_DBL:
    return __alignof__ (long double);
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

  case _C_VECTOR:
    {   
      /* Skip the '!'.  */
      type++;
      /* Skip the '['.  */
      type++;
      
      /* Skip the size.  */
      while (isdigit ((unsigned char)*type))
	type++;
      
      /* Skip the ','.  */
      type++;
      
      /* The alignment in bytes is the following number.  */
      return atoi (type);
    }
  case _C_STRUCT_B:
  case _C_UNION_B:
    {
      struct objc_struct_layout layout;
      unsigned int align;

      objc_layout_structure (type, &layout);
      while (objc_layout_structure_next_member (&layout))
        /* do nothing */;
      objc_layout_finish_structure (&layout, NULL, &align);

      return align;
    }
    
    
  case _C_COMPLEX:
    {
      type++; /* Skip after the 'j'. */
      switch (*type)
        {
	    case _C_CHR:
	      return __alignof__ (_Complex char);
	      break;

	    case _C_UCHR:
	      return __alignof__ (_Complex unsigned char);
	      break;

	    case _C_SHT:
	      return __alignof__ (_Complex short);
	      break;

	    case _C_USHT:
	      return __alignof__ (_Complex unsigned short);
	      break;

	    case _C_INT:
	      return __alignof__ (_Complex int);
	      break;

	    case _C_UINT:
	      return __alignof__ (_Complex unsigned int);
	      break;

	    case _C_LNG:
	      return __alignof__ (_Complex long);
	      break;

	    case _C_ULNG:
	      return __alignof__ (_Complex unsigned long);
	      break;

	    case _C_LNG_LNG:
	      return __alignof__ (_Complex long long);
	      break;

	    case _C_ULNG_LNG:
	      return __alignof__ (_Complex unsigned long long);
	      break;

	    case _C_FLT:
	      return __alignof__ (_Complex float);
	      break;

	    case _C_DBL:
	      return __alignof__ (_Complex double);
	      break;

	    case _C_LNG_DBL:
	      return __alignof__ (_Complex long double);
	      break;
	    
	    default:
	      {
		_objc_abort ("unknown complex type %s\n", type);
		return 0;
	      }
	}
    }

  default:
    {
      _objc_abort ("unknown type %s\n", type);
      return 0;
    }
  }
}

int
objc_aligned_size (const char *type)
{
  int size, align;

  type = objc_skip_variable_name (type);
  size = objc_sizeof_type (type);
  align = objc_alignof_type (type);

  return ROUND (size, align);
}

int
objc_promoted_size (const char *type)
{
  int size, wordsize;

  type = objc_skip_variable_name (type);
  size = objc_sizeof_type (type);
  wordsize = sizeof (void *);

  return ROUND (size, wordsize);
}

inline
const char *
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

inline
const char *
objc_skip_typespec (const char *type)
{
  type = objc_skip_variable_name (type);
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
  case _C_BOOL:
  case _C_ULNG:
  case _C_LNG_LNG:
  case _C_ULNG_LNG:
  case _C_FLT:
  case _C_DBL:
  case _C_LNG_DBL:
  case _C_VOID:
  case _C_UNDEF:
    return ++type;
    break;
    
  case _C_COMPLEX:
    return type + 2;
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
	_objc_abort ("bad array type %s\n", type);
	return 0;
      }

  case _C_VECTOR:
    /* Skip '!' */
    type++;
    /* Skip '[' */
    type++;
    /* Skip digits (size) */
    while (isdigit ((unsigned char)*type))
      type++;
    /* Skip ',' */
    type++;
    /* Skip digits (alignment) */
    while (isdigit ((unsigned char)*type))
      type++;
    /* Skip typespec.  */
    type = objc_skip_typespec (type);
    /* Skip closing ']'.  */
    if (*type == _C_ARY_E)
      return ++type;
    else
      {
	_objc_abort ("bad vector type %s\n", type);
	return 0;
      }

  case _C_BFLD:
    /* The GNU encoding of bitfields is: b 'position' 'type'
       'size'.  */
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
      _objc_abort ("unknown type %s\n", type);
      return 0;
    }
  }
}

inline
const char *
objc_skip_offset (const char *type)
{
  /* The offset is prepended by a '+' if the argument is passed in
     registers.  PS: The compiler stopped generating this '+' in
     version 3.4.  */
  if (*type == '+')
    type++;

  /* Some people claim that on some platforms, where the stack grows
     backwards, the compiler generates negative offsets (??).  Skip a
     '-' for such a negative offset.  */
  if (*type == '-')
    type++;

  /* Skip the digits that represent the offset.  */
  while (isdigit ((unsigned char) *type))
    type++;

  return type;
}

const char *
objc_skip_argspec (const char *type)
{
  type = objc_skip_typespec (type);
  type = objc_skip_offset (type);
  return type;
}

char *
method_copyReturnType (struct objc_method *method)
{
  if (method == NULL)
    return 0;
  else
    {
      char *returnValue;
      size_t returnValueSize;

      /* Determine returnValueSize.  */
      {
	/* Find the end of the first argument.  We want to return the
	   first argument spec, plus 1 byte for the \0 at the end.  */
	const char *type = method->method_types;
	if (*type == '\0')
	  return NULL;
	type = objc_skip_argspec (type);
	returnValueSize = type - method->method_types + 1;
      }

      /* Copy the first argument into returnValue.  */
      returnValue = malloc (sizeof (char) * returnValueSize);
      memcpy (returnValue, method->method_types, returnValueSize);
      returnValue[returnValueSize - 1] = '\0';

      return returnValue;
    }
}

char *
method_copyArgumentType (struct objc_method * method, unsigned int argumentNumber)
{
  if (method == NULL)
    return 0;
  else
    {
      char *returnValue;
      const char *returnValueStart;
      size_t returnValueSize;

      /* Determine returnValueStart and returnValueSize.  */
      {
	const char *type = method->method_types;

	/* Skip the first argument (return type).  */
	type = objc_skip_argspec (type);

	/* Now keep skipping arguments until we get to
	   argumentNumber.  */
	while (argumentNumber > 0)
	  {
	    /* We are supposed to skip an argument, but the string is
	       finished.  This means we were asked for a non-existing
	       argument.  */
	    if (*type == '\0')
	      return NULL;

	    type = objc_skip_argspec (type);
	    argumentNumber--;
	  }

	/* If the argument does not exist, return NULL.  */
	if (*type == '\0')
	  return NULL;

	returnValueStart = type;
	type = objc_skip_argspec (type);
	returnValueSize = type - returnValueStart + 1;
      }
      
      /* Copy the argument into returnValue.  */
      returnValue = malloc (sizeof (char) * returnValueSize);
      memcpy (returnValue, returnValueStart, returnValueSize);
      returnValue[returnValueSize - 1] = '\0';

      return returnValue;
    }
}

void method_getReturnType (struct objc_method * method, char *returnValue, 
			   size_t returnValueSize)
{
  if (returnValue == NULL  ||  returnValueSize == 0)
    return;

  /* Zero the string; we'll then write the argument type at the
     beginning of it, if needed.  */
  memset (returnValue, 0, returnValueSize);

  if (method == NULL)
    return;
  else
    {
      size_t argumentTypeSize;

      /* Determine argumentTypeSize.  */
      {
	/* Find the end of the first argument.  We want to return the
	   first argument spec.  */
	const char *type = method->method_types;
	if (*type == '\0')
	  return;
	type = objc_skip_argspec (type);
	argumentTypeSize = type - method->method_types;
	if (argumentTypeSize > returnValueSize)
	  argumentTypeSize = returnValueSize;
      }
      /* Copy the argument at the beginning of the string.  */
      memcpy (returnValue, method->method_types, argumentTypeSize);
    }
}

void method_getArgumentType (struct objc_method * method, unsigned int argumentNumber,
			     char *returnValue, size_t returnValueSize)
{
  if (returnValue == NULL  ||  returnValueSize == 0)
    return;

  /* Zero the string; we'll then write the argument type at the
     beginning of it, if needed.  */
  memset (returnValue, 0, returnValueSize);

  if (method == NULL)
    return;
  else
    {
      const char *returnValueStart;
      size_t argumentTypeSize;

      /* Determine returnValueStart and argumentTypeSize.  */
      {
	const char *type = method->method_types;

	/* Skip the first argument (return type).  */
	type = objc_skip_argspec (type);

	/* Now keep skipping arguments until we get to
	   argumentNumber.  */
	while (argumentNumber > 0)
	  {
	    /* We are supposed to skip an argument, but the string is
	       finished.  This means we were asked for a non-existing
	       argument.  */
	    if (*type == '\0')
	      return;

	    type = objc_skip_argspec (type);
	    argumentNumber--;
	  }

	/* If the argument does not exist, it's game over.  */
	if (*type == '\0')
	  return;

	returnValueStart = type;
	type = objc_skip_argspec (type);
	argumentTypeSize = type - returnValueStart;
	if (argumentTypeSize > returnValueSize)
	  argumentTypeSize = returnValueSize;
      }
      /* Copy the argument at the beginning of the string.  */
      memcpy (returnValue, returnValueStart, argumentTypeSize);
    }
}

unsigned int
method_getNumberOfArguments (struct objc_method *method)
{
  if (method == NULL)
    return 0;
  else
    {
      unsigned int i = 0;
      const char *type = method->method_types;
      while (*type)
	{
	  type = objc_skip_argspec (type);
	  i += 1;
	}

      if (i == 0)
	{
	  /* This could only happen if method_types is invalid; in
	     that case, return 0.  */
	  return 0;
	}
      else
	{
	  /* Remove the return type.  */
	  return (i - 1);
	}
    }
}

unsigned
objc_get_type_qualifiers (const char *type)
{
  unsigned res = 0;
  BOOL flag = YES;

  while (flag)
    switch (*type++)
      {
      case _C_CONST:       res |= _F_CONST; break;
      case _C_IN:          res |= _F_IN; break;
      case _C_INOUT:       res |= _F_INOUT; break;
      case _C_OUT:         res |= _F_OUT; break;
      case _C_BYCOPY:      res |= _F_BYCOPY; break;
      case _C_BYREF:       res |= _F_BYREF; break;
      case _C_ONEWAY:      res |= _F_ONEWAY; break;
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
  was not working on some architectures, particularly on AIX, and in
  the presence of bitfields inside the structure.  */
void
objc_layout_structure (const char *type,
		       struct objc_struct_layout *layout)
{
  const char *ntype;

  if (*type != _C_UNION_B && *type != _C_STRUCT_B)
    {
      _objc_abort ("record (or union) type expected in objc_layout_structure, got %s\n",
		   type);
    }

  type ++;
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
  layout->record_align = __CHAR_BIT__;

  layout->record_align = MAX (layout->record_align, STRUCTURE_SIZE_BOUNDARY);
}

BOOL
objc_layout_structure_next_member (struct objc_struct_layout *layout)
{
  register int desired_align = 0;

  /* The following are used only if the field is a bitfield */
  register const char *bfld_type = 0;
  register int bfld_type_align = 0, bfld_field_size = 0;

  /* The current type without the type qualifiers */
  const char *type;
  BOOL unionp = layout->original_type[-1] == _C_UNION_B;

  /* Add the size of the previous field to the size of the record.  */
  if (layout->prev_type)
    {
      type = objc_skip_type_qualifiers (layout->prev_type);
      if (unionp)
        layout->record_size = MAX (layout->record_size,
				   objc_sizeof_type (type) * __CHAR_BIT__);

      else if (*type != _C_BFLD)
	layout->record_size += objc_sizeof_type (type) * __CHAR_BIT__;
      else {
        /* Get the bitfield's type */
        for (bfld_type = type + 1;
             isdigit ((unsigned char)*bfld_type);
             bfld_type++)
          /* do nothing */;

	bfld_type_align = objc_alignof_type (bfld_type) * __CHAR_BIT__;
        bfld_field_size = atoi (objc_skip_typespec (bfld_type));
        layout->record_size += bfld_field_size;
      }
    }

  if ((unionp && *layout->type == _C_UNION_E)
      || (!unionp && *layout->type == _C_STRUCT_E))
    return NO;

  /* Skip the variable name if any */
  layout->type = objc_skip_variable_name (layout->type);
  type = objc_skip_type_qualifiers (layout->type);

  if (*type != _C_BFLD)
    desired_align = objc_alignof_type (type) * __CHAR_BIT__;
  else
    {
      desired_align = 1;
      /* Skip the bitfield's offset */
      for (bfld_type = type + 1;
           isdigit ((unsigned char) *bfld_type);
           bfld_type++)
        /* do nothing */;

      bfld_type_align = objc_alignof_type (bfld_type) * __CHAR_BIT__;
      bfld_field_size = atoi (objc_skip_typespec (bfld_type));
    }

  /* The following won't work for vectors.  */
#ifdef BIGGEST_FIELD_ALIGNMENT
  desired_align = MIN (desired_align, BIGGEST_FIELD_ALIGNMENT);
#endif
#ifdef ADJUST_FIELD_ALIGN
  desired_align = ADJUST_FIELD_ALIGN (type, type, desired_align);
#endif

  /* Record must have at least as much alignment as any field.
     Otherwise, the alignment of the field within the record
     is meaningless.  */
#ifndef HAVE_BITFIELD_TYPE_MATTERS
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
	desired_align = objc_alignof_type (bfld_type) * __CHAR_BIT__;

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
	  type_align = MIN (type_align, __CHAR_BIT__);
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
  BOOL unionp = layout->original_type[-1] == _C_UNION_B;
  if (layout->type
      && ((!unionp && *layout->type == _C_STRUCT_E)
       	  || (unionp && *layout->type == _C_UNION_E)))
    {
      /* Work out the alignment of the record as one expression and store
         in the record type.  Round it up to a multiple of the record's
         alignment. */
#if defined (ROUND_TYPE_ALIGN) && ! defined (__sparc__)
      layout->record_align = ROUND_TYPE_ALIGN (layout->original_type-1,
                                               1,
                                               layout->record_align);
#else
      layout->record_align = MAX (1, layout->record_align);
#endif

      /* Round the size up to be a multiple of the required alignment */
      layout->record_size = ROUND (layout->record_size, layout->record_align);

      layout->type = NULL;
    }
  if (size)
    *size = layout->record_size / __CHAR_BIT__;
  if (align)
    *align = layout->record_align / __CHAR_BIT__;
}

void objc_layout_structure_get_info (struct objc_struct_layout *layout,
                                     unsigned int *offset,
                                     unsigned int *align,
                                     const char **type)
{
  if (offset)
    *offset = layout->record_size / __CHAR_BIT__;
  if (align)
    *align = layout->record_align / __CHAR_BIT__;
  if (type)
    *type = layout->prev_type;
}
