/* Vector API for GNU compiler.
   Copyright (C) 2004 Free Software Foundation, Inc.
   Contributed by Nathan Sidwell <nathan@codesourcery.com>

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

#include "config.h"
#include "system.h"
#include "ggc.h"
#include "vec.h"
#include "errors.h"
#include "coretypes.h"
#include "tree.h"

struct vec_prefix 
{
  unsigned num;
  unsigned alloc;
  void *vec[1];
};

/* Ensure there are at least RESERVE free slots in VEC, if RESERVE >=
   0.  If RESERVE < 0 increase the current allocation exponentially.
   VEC can be NULL, to create a new vector.  */

void *
vec_gc_p_reserve (void *vec, int reserve MEM_STAT_DECL)
{
  return vec_gc_o_reserve (vec, reserve,
			   offsetof (struct vec_prefix, vec), sizeof (void *)
			   PASS_MEM_STAT);
}

/* Ensure there are at least RESERVE free slots in VEC, if RESERVE >=
   0.  If RESERVE < 0, increase the current allocation exponentially.
   VEC can be NULL, in which case a new vector is created.  The
   vector's trailing array is at VEC_OFFSET offset and consists of
   ELT_SIZE sized elements.  */

void *
vec_gc_o_reserve (void *vec, int reserve, size_t vec_offset, size_t elt_size
		   MEM_STAT_DECL)
{
  struct vec_prefix *pfx = vec;
  unsigned alloc = pfx ? pfx->num : 0;

  if (reserve >= 0)
    alloc += reserve;
  else if (alloc)
    alloc *= 2;
  else
    alloc = 4;

  if (pfx && pfx->alloc >= alloc)
    abort ();
  
  vec = ggc_realloc_stat (vec, vec_offset + alloc * elt_size PASS_MEM_STAT);
  ((struct vec_prefix *)vec)->alloc = alloc;
  if (!pfx)
    ((struct vec_prefix *)vec)->num = 0;
  
  return vec;
}

/* Explicitly release a vector.  */

void
vec_gc_free (void *vec)
{
  ggc_free (vec);
}

/* Ensure there are at least RESERVE free slots in VEC, if RESERVE >=
   0.  If RESERVE < 0 increase the current allocation exponentially.
   VEC can be NULL, to create a new vector.  */

void *
vec_heap_p_reserve (void *vec, int reserve MEM_STAT_DECL)
{
  return vec_heap_o_reserve (vec, reserve,
			     offsetof (struct vec_prefix, vec), sizeof (void *)
			     PASS_MEM_STAT);
}

/* Ensure there are at least RESERVE free slots in VEC, if RESERVE >=
   0.  If RESERVE < 0, increase the current allocation exponentially.
   VEC can be NULL, in which case a new vector is created.  The
   vector's trailing array is at VEC_OFFSET offset and consists of
   ELT_SIZE sized elements.  */

void *
vec_heap_o_reserve (void *vec, int reserve, size_t vec_offset, size_t elt_size
		    MEM_STAT_DECL)
{
  struct vec_prefix *pfx = vec;
  unsigned alloc = pfx ? pfx->num : 0;

  if (reserve >= 0)
    alloc += reserve;
  else if (alloc)
    alloc *= 2;
  else
    alloc = 4;

  if (pfx && pfx->alloc >= alloc)
    abort ();
  
  vec = xrealloc (vec, vec_offset + alloc * elt_size);
  ((struct vec_prefix *)vec)->alloc = alloc;
  if (!pfx)
    ((struct vec_prefix *)vec)->num = 0;
  
  return vec;
}

/* Explicitly release a vector.  */

void
vec_heap_free (void *vec)
{
  free (vec);
}

#if ENABLE_CHECKING
/* Issue a vector domain error, and then fall over.  */

void
vec_assert_fail (const char *op, const char *struct_name,
		 const char *file, unsigned int line, const char *function)
{
  internal_error ("vector %s %s domain error, in %s at %s:%u",
		  struct_name, op, function, trim_filename (file), line);
}
#endif
