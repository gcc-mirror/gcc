/* Virtual array support.
   Copyright (C) 1998, 1999, 2000, 2002 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.  */

#ifndef GCC_VARRAY_H
#define GCC_VARRAY_H

#ifndef HOST_WIDE_INT
#include "machmode.h"
#endif

#ifndef GCC_SYSTEM_H
#include "system.h"
#endif

/* Auxiliary structure used inside the varray structure, used for
   function integration data.  */

struct const_equiv_data GTY(()) {
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
  struct rtx_def *rtx;

  /* Record the valid age for each entry.  The entry is invalid if its
     age is less than const_age.  */
  unsigned age;
};

/* Enum indicating what the varray contains.  
   If this is changed, `element_size' in varray.c needs to be updated.  */

enum varray_data_enum {
  VARRAY_DATA_C,
  VARRAY_DATA_UC,
  VARRAY_DATA_S,
  VARRAY_DATA_US,
  VARRAY_DATA_I,
  VARRAY_DATA_U,
  VARRAY_DATA_L,
  VARRAY_DATA_UL,
  VARRAY_DATA_HINT,
  VARRAY_DATA_UHINT,
  VARRAY_DATA_GENERIC,
  VARRAY_DATA_CPTR,
  VARRAY_DATA_RTX,
  VARRAY_DATA_RTVEC,
  VARRAY_DATA_TREE,
  VARRAY_DATA_BITMAP,
  VARRAY_DATA_REG,
  VARRAY_DATA_CONST_EQUIV,
  VARRAY_DATA_BB,
  VARRAY_DATA_TE,
  NUM_VARRAY_DATA
};

/* Union of various array types that are used.  */
typedef union varray_data_tag GTY (()) {
  char			  GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_C")))		c[1];
  unsigned char		  GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_UC")))	uc[1];
  short			  GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_S")))		s[1];
  unsigned short	  GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_US")))	us[1];
  int			  GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_I")))		i[1];
  unsigned int		  GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_U")))		u[1];
  long			  GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_L")))		l[1];
  unsigned long		  GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_UL")))	ul[1];
  HOST_WIDE_INT		  GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_HINT")))	hint[1];
  unsigned HOST_WIDE_INT  GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_UHINT")))	uhint[1];
  PTR			  GTY ((length ("%0.num_elements"), use_param (""),
				tag ("VARRAY_DATA_GENERIC")))	generic[1];
  char			 *GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_CPTR")))	cptr[1];
  struct rtx_def	 *GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_RTX")))	rtx[1];
  struct rtvec_def	 *GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_RTVEC")))	rtvec[1];
  union tree_node	 *GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_TREE")))	tree[1];
  struct bitmap_head_def *GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_BITMAP")))	bitmap[1];
  struct reg_info_def	 *GTY ((length ("%0.num_elements"), skip (""),
				tag ("VARRAY_DATA_REG")))	reg[1];
  struct const_equiv_data GTY ((length ("%0.num_elements"),
			tag ("VARRAY_DATA_CONST_EQUIV"))) 	const_equiv[1];
  struct basic_block_def *GTY ((length ("%0.num_elements"), skip (""),
				tag ("VARRAY_DATA_BB")))	bb[1];
  struct elt_list	 *GTY ((length ("%0.num_elements"),
				tag ("VARRAY_DATA_TE")))	te[1];
} varray_data;

/* Virtual array of pointers header.  */
struct varray_head_tag GTY(()) {
  size_t	num_elements;	/* Maximum element number allocated.  */
  size_t        elements_used;  /* The number of elements used, if
				   using VARRAY_PUSH/VARRAY_POP.  */
  enum varray_data_enum type;	/* The kind of elements in the varray.  */
  const char   *name;		/* name of the varray for reporting errors */
  varray_data	GTY ((desc ("%0.type"))) data;	/* The data elements follow, 
						   must be last.  */
};
typedef struct varray_head_tag *varray_type;

/* Allocate a virtual array with NUM elements, each of which is SIZE bytes
   long, named NAME.  Array elements are zeroed.  */
extern varray_type varray_init	PARAMS ((size_t, enum varray_data_enum, 
					 const char *));

#define VARRAY_CHAR_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_C, name)

#define VARRAY_UCHAR_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_UC, name)

#define VARRAY_SHORT_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_S, name)

#define VARRAY_USHORT_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_US, name)

#define VARRAY_INT_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_I, name)

#define VARRAY_UINT_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_U, name)

#define VARRAY_LONG_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_L, name)

#define VARRAY_ULONG_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_UL, name)

#define VARRAY_WIDE_INT_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_HINT, name)

#define VARRAY_UWIDE_INT_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_UHINT, name)

#define VARRAY_GENERIC_PTR_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_GENERIC, name)

#define VARRAY_CHAR_PTR_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_CPTR, name)

#define VARRAY_RTX_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_RTX, name)

#define VARRAY_RTVEC_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_RTVEC, name)

#define VARRAY_TREE_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_TREE, name)

#define VARRAY_BITMAP_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_BITMAP, name)

#define VARRAY_REG_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_REG, name)

#define VARRAY_CONST_EQUIV_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_CONST_EQUIV, name)

#define VARRAY_BB_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_BB, name)

#define VARRAY_ELT_LIST_INIT(va, num, name) \
  va = varray_init (num, VARRAY_DATA_TE, name)

/* Free up memory allocated by the virtual array, but do not free any of the
   elements involved.  */
#define VARRAY_FREE(vp) \
  do { if (vp) { free (vp); vp = (varray_type) 0; } } while (0)

/* Grow/shrink the virtual array VA to N elements.  */
extern varray_type varray_grow	PARAMS ((varray_type, size_t));

#define VARRAY_GROW(VA, N) ((VA) = varray_grow (VA, N))

#define VARRAY_SIZE(VA)	((VA)->num_elements)

#define VARRAY_ACTIVE_SIZE(VA)	((VA)->elements_used)
#define VARRAY_POP_ALL(VA)	((VA)->elements_used = 0)

#define VARRAY_CLEAR(VA) varray_clear(VA)

extern void varray_clear	PARAMS ((varray_type));

/* Check for VARRAY_xxx macros being in bound.  */
#if defined ENABLE_CHECKING && (GCC_VERSION >= 2007)
extern void varray_check_failed PARAMS ((varray_type, size_t,
					const char *, int,
					const char *)) ATTRIBUTE_NORETURN;
#define VARRAY_CHECK(VA, N, T) __extension__			\
(*({ varray_type const _va = (VA);				\
     const size_t _n = (N); 					\
     if (_n >= _va->num_elements)				\
       varray_check_failed (_va, _n, __FILE__, __LINE__, __FUNCTION__);	\
     &_va->data.T[_n]; }))
#else
#define VARRAY_CHECK(VA, N, T) ((VA)->data.T[N])
#endif

/* Push X onto VA.  T is the name of the field in varray_data
   corresponding to the type of X.  */
#define VARRAY_PUSH(VA, T, X)				\
  do							\
    {							\
      if ((VA)->elements_used >= (VA)->num_elements)	\
        VARRAY_GROW ((VA), 2 * (VA)->num_elements);	\
      (VA)->data.T[(VA)->elements_used++] = (X);	\
    }							\
  while (0)

/* Pop the top element of VA.  */
#define VARRAY_POP(VA) \
  ((VA)->elements_used--)

/* Return the top element of VA.  */
#define VARRAY_TOP(VA, T) \
  ((VA)->data.T[(VA)->elements_used - 1])

#define VARRAY_CHAR(VA, N)		VARRAY_CHECK (VA, N, c)
#define VARRAY_UCHAR(VA, N)		VARRAY_CHECK (VA, N, uc)
#define VARRAY_SHORT(VA, N)		VARRAY_CHECK (VA, N, s)
#define VARRAY_USHORT(VA, N)		VARRAY_CHECK (VA, N, us)
#define VARRAY_INT(VA, N)		VARRAY_CHECK (VA, N, i)
#define VARRAY_UINT(VA, N)		VARRAY_CHECK (VA, N, u)
#define VARRAY_LONG(VA, N)		VARRAY_CHECK (VA, N, l)
#define VARRAY_ULONG(VA, N)		VARRAY_CHECK (VA, N, ul)
#define VARRAY_WIDE_INT(VA, N)		VARRAY_CHECK (VA, N, hint)
#define VARRAY_UWIDE_INT(VA, N)		VARRAY_CHECK (VA, N, uhint)
#define VARRAY_GENERIC_PTR(VA,N)	VARRAY_CHECK (VA, N, generic)
#define VARRAY_CHAR_PTR(VA,N)		VARRAY_CHECK (VA, N, cptr)
#define VARRAY_RTX(VA, N)		VARRAY_CHECK (VA, N, rtx)
#define VARRAY_RTVEC(VA, N)		VARRAY_CHECK (VA, N, rtvec)
#define VARRAY_TREE(VA, N)		VARRAY_CHECK (VA, N, tree)
#define VARRAY_BITMAP(VA, N)		VARRAY_CHECK (VA, N, bitmap)
#define VARRAY_REG(VA, N)		VARRAY_CHECK (VA, N, reg)
#define VARRAY_CONST_EQUIV(VA, N)	VARRAY_CHECK (VA, N, const_equiv)
#define VARRAY_BB(VA, N)		VARRAY_CHECK (VA, N, bb)
#define VARRAY_ELT_LIST(VA, N)		VARRAY_CHECK (VA, N, te)

/* Push a new element on the end of VA, extending it if necessary.  */
#define VARRAY_PUSH_CHAR(VA, X)		VARRAY_PUSH (VA, c, X)
#define VARRAY_PUSH_UCHAR(VA, X)	VARRAY_PUSH (VA, uc, X)
#define VARRAY_PUSH_SHORT(VA, X)	VARRAY_PUSH (VA, s, X)
#define VARRAY_PUSH_USHORT(VA, X)	VARRAY_PUSH (VA, us, X)
#define VARRAY_PUSH_INT(VA, X)		VARRAY_PUSH (VA, i, X)
#define VARRAY_PUSH_UINT(VA, X)		VARRAY_PUSH (VA, u, X)
#define VARRAY_PUSH_LONG(VA, X)		VARRAY_PUSH (VA, l, X)
#define VARRAY_PUSH_ULONG(VA, X)	VARRAY_PUSH (VA, ul, X)
#define VARRAY_PUSH_WIDE_INT(VA, X)	VARRAY_PUSH (VA, hint, X)
#define VARRAY_PUSH_UWIDE_INT(VA, X)	VARRAY_PUSH (VA, uhint, X)
#define VARRAY_PUSH_GENERIC_PTR(VA, X)	VARRAY_PUSH (VA, generic, X)
#define VARRAY_PUSH_CHAR_PTR(VA, X)	VARRAY_PUSH (VA, cptr, X)
#define VARRAY_PUSH_RTX(VA, X)		VARRAY_PUSH (VA, rtx, X)
#define VARRAY_PUSH_RTVEC(VA, X)	VARRAY_PUSH (VA, rtvec, X)
#define VARRAY_PUSH_TREE(VA, X)		VARRAY_PUSH (VA, tree, X)
#define VARRAY_PUSH_BITMAP(VA, X)	VARRAY_PUSH (VA, bitmap, X)
#define VARRAY_PUSH_REG(VA, X)		VARRAY_PUSH (VA, reg, X)
#define VARRAY_PUSH_CONST_EQUIV(VA, X)	VARRAY_PUSH (VA, const_equiv, X)
#define VARRAY_PUSH_BB(VA, X)		VARRAY_PUSH (VA, bb, X)

/* Return the last element of VA.  */
#define VARRAY_TOP_CHAR(VA)		VARRAY_TOP (VA, c)
#define VARRAY_TOP_UCHAR(VA)	        VARRAY_TOP (VA, uc)
#define VARRAY_TOP_SHORT(VA)	        VARRAY_TOP (VA, s)
#define VARRAY_TOP_USHORT(VA)	        VARRAY_TOP (VA, us)
#define VARRAY_TOP_INT(VA)		VARRAY_TOP (VA, i)
#define VARRAY_TOP_UINT(VA)		VARRAY_TOP (VA, u)
#define VARRAY_TOP_LONG(VA)		VARRAY_TOP (VA, l)
#define VARRAY_TOP_ULONG(VA)	        VARRAY_TOP (VA, ul)
#define VARRAY_TOP_WIDE_INT(VA)	        VARRAY_TOP (VA, hint)
#define VARRAY_TOP_UWIDE_INT(VA)	VARRAY_TOP (VA, uhint)
#define VARRAY_TOP_GENERIC_PTR(VA)	VARRAY_TOP (VA, generic)
#define VARRAY_TOP_CHAR_PTR(VA)		VARRAY_TOP (VA, cptr)
#define VARRAY_TOP_RTX(VA)		VARRAY_TOP (VA, rtx)
#define VARRAY_TOP_RTVEC(VA)	        VARRAY_TOP (VA, rtvec)
#define VARRAY_TOP_TREE(VA)		VARRAY_TOP (VA, tree)
#define VARRAY_TOP_BITMAP(VA)	        VARRAY_TOP (VA, bitmap)
#define VARRAY_TOP_REG(VA)		VARRAY_TOP (VA, reg)
#define VARRAY_TOP_CONST_EQUIV(VA)	VARRAY_TOP (VA, const_equiv)
#define VARRAY_TOP_BB(VA)		VARRAY_TOP (VA, bb)

#endif /* ! GCC_VARRAY_H */
