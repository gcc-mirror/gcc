/* Virtual array support.
   Copyright (C) 1998 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to the Free
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#ifndef _VARRAY_H_
#define _VARRAY_H_

#ifndef HOST_WIDE_INT
#include "machmode.h"
#endif

#ifndef __GCC_SYSTEM_H__
#include "system.h"
#endif

/* Auxiliary structure used inside the varray structure, used for
   function integration data.  */

struct const_equiv_data {
  /* Map pseudo reg number in calling function to equivalent constant.  We
     cannot in general substitute constants into parameter pseudo registers,
     since some machine descriptions (many RISCs) won't always handle
     the resulting insns.  So if an incoming parameter has a constant
     equivalent, we record it here, and if the resulting insn is
     recognizable, we go with it.

     We also use this mechanism to convert references to incoming arguments
     and stacked variables.  copy_rtx_and_substitute will replace the virtual
     incoming argument and virtual stacked variables registers with new
     pseudos that contain pointers into the replacement area allocated for
     this inline instance.  These pseudos are then marked as being equivalent
     to the appropriate address and substituted if valid.  */
  rtx rtx;

  /* Record the valid age for each entry.  The entry is invalid if its
     age is less than const_age.  */
  unsigned age;
};

/* Union of various array types that are used.  */
typedef union varray_data_tag {
  char			 c[1];
  unsigned char		 uc[1];
  short			 s[1];
  unsigned short	 us[1];
  int			 i[1];
  unsigned int		 u[1];
  long			 l[1];
  unsigned long		 ul[1];
  HOST_WIDE_INT		 hint[1];
  unsigned HOST_WIDE_INT uhint[1];
  GENERIC_PTR		 generic[1];
  char			 *cptr[1];
  struct rtx_def	 *rtx[1];
  struct rtvec_def	 *rtvec[1];
  union tree_node	 *tree[1];
  struct bitmap_head_def *bitmap[1];
  struct sched_info_tag	 *sched[1];
  struct reg_info_def	 *reg[1];
  struct const_equiv_data const_equiv[1];
  struct basic_block_def *bb[1];
} varray_data;

/* Virtual array of pointers header.  */
typedef struct varray_head_tag {
  size_t	num_elements;	/* maximum element number allocated */
  size_t	element_size;	/* size of each data element */
  const char   *name;		/* name of the varray for reporting errors */
  varray_data	data;		/* data elements follow, must be last */
} *varray_type;

/* Allocate a virtual array with NUM elements, each of which is SIZE bytes
   long, named NAME.  Array elements are zeroed.  */
extern varray_type varray_init	PROTO ((size_t, size_t, const char *));

#define VARRAY_CHAR_INIT(va, num, name) \
  va = varray_init (num, sizeof (char), name)

#define VARRAY_UCHAR_INIT(va, num, name) \
  va = varray_init (num, sizeof (unsigned char), name)

#define VARRAY_SHORT_INIT(va, num, name) \
  va = varray_init (num, sizeof (short), name)

#define VARRAY_USHORT_INIT(va, num, name) \
  va = varray_init (num, sizeof (unsigned short), name)

#define VARRAY_INT_INIT(va, num, name) \
  va = varray_init (num, sizeof (int), name)

#define VARRAY_UINT_INIT(va, num, name) \
  va = varray_init (num, sizeof (unsigned int), name)

#define VARRAY_LONG_INIT(va, num, name) \
  va = varray_init (num, sizeof (long), name)

#define VARRAY_ULONG_INIT(va, num, name) \
  va = varray_init (num, sizeof (unsigned long), name)

#define VARRAY_WIDE_INT_INIT(va, num, name) \
  va = varray_init (num, sizeof (HOST_WIDE_INT), name)

#define VARRAY_UWIDE_INT_INIT(va, num, name) \
  va = varray_init (num, sizeof (unsigned HOST_WIDE_INT), name)

#define VARRAY_GENERIC_PTR_INIT(va, num, name) \
  va = varray_init (num, sizeof (GENERIC_PTR), name)

#define VARRAY_CHAR_PTR_INIT(va, num, name) \
  va = varray_init (num, sizeof (char *), name)

#define VARRAY_RTX_INIT(va, num, name) \
  va = varray_init (num, sizeof (struct rtx_def *), name)

#define VARRAY_RTVEC_INIT(va, num, name) \
  va = varray_init (num, sizeof (struct rtvec_def), name)

#define VARRAY_TREE_INIT(va, num, name) \
  va = varray_init (num, sizeof (union tree_node *), name)

#define VARRAY_BITMAP_INIT(va, num, name) \
  va = varray_init (num, sizeof (struct bitmap_head_def *), name)

#define VARRAY_SCHED_INIT(va, num, name) \
  va = varray_init (num, sizeof (struct sched_info_tag *), name)

#define VARRAY_REG_INIT(va, num, name) \
  va = varray_init (num, sizeof (struct reg_info_def *), name)

#define VARRAY_CONST_EQUIV_INIT(va, num, name) \
  va = varray_init (num, sizeof (struct const_equiv_data), name)

#define VARRAY_BB_INIT(va, num, name) \
  va = varray_init (num, sizeof (struct basic_block_def *), name)

/* Free up memory allocated by the virtual array, but do not free any of the
   elements involved.  */
#define VARRAY_FREE(vp) \
  do { if (vp) { free (vp); vp = (varray_type)0; } } while (0)

/* Grow/shrink the virtual array VA to N elements.  */
extern varray_type varray_grow	PROTO((varray_type, size_t));

#define VARRAY_GROW(VA, N) ((VA) = varray_grow (VA, N))

#define VARRAY_SIZE(VA)	((VA)->num_elements)

/* Check for VARRAY_xxx macros being in bound, return N for use as an
   index.  */
#ifdef ENABLE_CHECKING
#define VARRAY_CHECK(VA, N)						\
((((size_t)(N) < (VA)->num_elements)					\
  ? 0									\
  : (fatal ("Virtual array %s element %ld out of bounds, at %s:%d",	\
	    (VA)->name, (long)(N), __FILE__, __LINE__), 0)),		\
 (N))
#else
#define VARRAY_CHECK(VA, N) (N)
#endif

#define VARRAY_CHAR(VA, N)	((VA)->data.c[ VARRAY_CHECK (VA, N) ])
#define VARRAY_UCHAR(VA, N)	((VA)->data.uc[ VARRAY_CHECK (VA, N) ])
#define VARRAY_SHORT(VA, N)	((VA)->data.s[ VARRAY_CHECK (VA, N) ])
#define VARRAY_USHORT(VA, N)	((VA)->data.us[ VARRAY_CHECK (VA, N) ])
#define VARRAY_INT(VA, N)	((VA)->data.i[ VARRAY_CHECK (VA, N) ])
#define VARRAY_UINT(VA, N)	((VA)->data.u[ VARRAY_CHECK (VA, N) ])
#define VARRAY_LONG(VA, N)	((VA)->data.l[ VARRAY_CHECK (VA, N) ])
#define VARRAY_ULONG(VA, N)	((VA)->data.ul[ VARRAY_CHECK (VA, N) ])
#define VARRAY_WIDE_INT(VA, N)	((VA)->data.hint[ VARRAY_CHECK (VA, N) ])
#define VARRAY_UWIDE_INT(VA, N)	((VA)->data.uhint[ VARRAY_CHECK (VA, N) ])
#define VARRAY_GENERIC_PTR(VA,N) ((VA)->data.generic[ VARRAY_CHECK (VA, N) ])
#define VARRAY_CHAR_PTR(VA,N)	((VA)->data.cptr[ VARRAY_CHECK (VA, N) ])
#define VARRAY_RTX(VA, N)	((VA)->data.rtx[ VARRAY_CHECK (VA, N) ])
#define VARRAY_RTVEC(VA, N)	((VA)->data.rtvec[ VARRAY_CHECK (VA, N) ])
#define VARRAY_TREE(VA, N)	((VA)->data.tree[ VARRAY_CHECK (VA, N) ])
#define VARRAY_BITMAP(VA, N)	((VA)->data.bitmap[ VARRAY_CHECK (VA, N) ])
#define VARRAY_SCHED(VA, N)	((VA)->data.sched[ VARRAY_CHECK (VA, N) ])
#define VARRAY_REG(VA, N)	((VA)->data.reg[ VARRAY_CHECK (VA, N) ])
#define VARRAY_CONST_EQUIV(VA, N) ((VA)->data.const_equiv[VARRAY_CHECK (VA, N)])
#define VARRAY_BB(VA, N)	((VA)->data.bb[ VARRAY_CHECK (VA, N) ])

#endif /* _VARRAY_H_ */
