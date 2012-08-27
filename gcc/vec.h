/* Vector API for GNU compiler.
   Copyright (C) 2004, 2005, 2007, 2008, 2009, 2010, 2011, 2012
   Free Software Foundation, Inc.
   Contributed by Nathan Sidwell <nathan@codesourcery.com>
   Re-implemented in C++ by Diego Novillo <dnovillo@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_VEC_H
#define GCC_VEC_H

#include "statistics.h"		/* For MEM_STAT_DECL.  */

/* The macros here implement a set of templated vector types and
   associated interfaces.  These templates are implemented with
   macros, as we're not in C++ land.  The interface functions are
   typesafe and use static inline functions, sometimes backed by
   out-of-line generic functions.  The vectors are designed to
   interoperate with the GTY machinery.

   Because of the different behavior of structure objects, scalar
   objects and of pointers, there are three flavors, one for each of
   these variants.  Both the structure object and pointer variants
   pass pointers to objects around -- in the former case the pointers
   are stored into the vector and in the latter case the pointers are
   dereferenced and the objects copied into the vector.  The scalar
   object variant is suitable for int-like objects, and the vector
   elements are returned by value.

   There are both 'index' and 'iterate' accessors.  The iterator
   returns a boolean iteration condition and updates the iteration
   variable passed by reference.  Because the iterator will be
   inlined, the address-of can be optimized away.

   The vectors are implemented using the trailing array idiom, thus
   they are not resizeable without changing the address of the vector
   object itself.  This means you cannot have variables or fields of
   vector type -- always use a pointer to a vector.  The one exception
   is the final field of a structure, which could be a vector type.
   You will have to use the embedded_size & embedded_init calls to
   create such objects, and they will probably not be resizeable (so
   don't use the 'safe' allocation variants).  The trailing array
   idiom is used (rather than a pointer to an array of data), because,
   if we allow NULL to also represent an empty vector, empty vectors
   occupy minimal space in the structure containing them.

   Each operation that increases the number of active elements is
   available in 'quick' and 'safe' variants.  The former presumes that
   there is sufficient allocated space for the operation to succeed
   (it dies if there is not).  The latter will reallocate the
   vector, if needed.  Reallocation causes an exponential increase in
   vector size.  If you know you will be adding N elements, it would
   be more efficient to use the reserve operation before adding the
   elements with the 'quick' operation.  This will ensure there are at
   least as many elements as you ask for, it will exponentially
   increase if there are too few spare slots.  If you want reserve a
   specific number of slots, but do not want the exponential increase
   (for instance, you know this is the last allocation), use the
   reserve_exact operation.  You can also create a vector of a
   specific size from the get go.

   You should prefer the push and pop operations, as they append and
   remove from the end of the vector. If you need to remove several
   items in one go, use the truncate operation.  The insert and remove
   operations allow you to change elements in the middle of the
   vector.  There are two remove operations, one which preserves the
   element ordering 'ordered_remove', and one which does not
   'unordered_remove'.  The latter function copies the end element
   into the removed slot, rather than invoke a memmove operation.  The
   'lower_bound' function will determine where to place an item in the
   array using insert that will maintain sorted order.

   When a vector type is defined, first a non-memory managed version
   is created.  You can then define either or both garbage collected
   and heap allocated versions.  The allocation mechanism is specified
   when the type is defined, and is therefore part of the type.  If
   you need both gc'd and heap allocated versions, you still must have
   *exactly* one definition of the common non-memory managed base vector.

   If you need to directly manipulate a vector, then the 'address'
   accessor will return the address of the start of the vector.  Also
   the 'space' predicate will tell you whether there is spare capacity
   in the vector.  You will not normally need to use these two functions.

   Vector types are defined using a DEF_VEC_{O,A,P,I}(TYPEDEF) macro, to
   get the non-memory allocation version, and then a
   DEF_VEC_ALLOC_{O,A,P,I}(TYPEDEF,ALLOC) macro to get memory managed
   vectors.  Variables of vector type are declared using a
   VEC(TYPEDEF,ALLOC) macro.  The ALLOC argument specifies the
   allocation strategy, and can be either 'gc' or 'heap' for garbage
   collected and heap allocated respectively.  It can be 'none' to get
   a vector that must be explicitly allocated (for instance as a
   trailing array of another structure).  The characters O, A, P and I
   indicate whether TYPEDEF is a pointer (P), object (O), atomic object
   (A) or integral (I) type.  Be careful to pick the correct one, as
   you'll get an awkward and inefficient API if you use the wrong one or
   a even a crash if you pick the atomic object version when the object
   version should have been chosen instead.  There is a check, which
   results in a compile-time warning, for the P and I versions, but there
   is no check for the O versions, as that is not possible in plain C.
   Due to the way GTY works, you must annotate any structures you wish to
   insert or reference from a vector with a GTY(()) tag.  You need to do
   this even if you never declare the GC allocated variants.

   An example of their use would be,

   DEF_VEC_P(tree);   // non-managed tree vector.
   DEF_VEC_ALLOC_P(tree,gc);	// gc'd vector of tree pointers.  This must
   			        // appear at file scope.

   struct my_struct {
     VEC(tree,gc) *v;      // A (pointer to) a vector of tree pointers.
   };

   struct my_struct *s;

   if (VEC_length(tree,s->v)) { we have some contents }
   VEC_safe_push(tree,gc,s->v,decl); // append some decl onto the end
   for (ix = 0; VEC_iterate(tree,s->v,ix,elt); ix++)
     { do something with elt }

*/

#if ENABLE_CHECKING
#define VEC_CHECK_INFO ,__FILE__,__LINE__,__FUNCTION__
#define VEC_CHECK_DECL ,const char *file_,unsigned line_,const char *function_
#define VEC_CHECK_PASS ,file_,line_,function_

#define VEC_ASSERT(EXPR,OP,T,A) \
  (void)((EXPR) ? 0 : (VEC_ASSERT_FAIL(OP,VEC(T,A)), 0))

extern void vec_assert_fail (const char *, const char * VEC_CHECK_DECL)
     ATTRIBUTE_NORETURN;
#define VEC_ASSERT_FAIL(OP,VEC) vec_assert_fail (OP,#VEC VEC_CHECK_PASS)
#else
#define VEC_CHECK_INFO
#define VEC_CHECK_DECL
#define VEC_CHECK_PASS
#define VEC_ASSERT(EXPR,OP,T,A) (void)(EXPR)
#endif

#define VEC(T,A) vec_t<T>

enum vec_allocation_t { heap, gc, stack };

struct vec_prefix
{
  unsigned num;
  unsigned alloc;
};

/* Vector type, user visible.  */
template<typename T>
struct GTY(()) vec_t
{
  vec_prefix prefix;
  T vec[1];
};

/* Garbage collection support for vec_t.  */

template<typename T>
void
gt_ggc_mx (vec_t<T> *v)
{
  extern void gt_ggc_mx (T&);
  for (unsigned i = 0; i < v->prefix.num; i++)
    gt_ggc_mx (v->vec[i]);
}


/* PCH support for vec_t.  */

template<typename T>
void
gt_pch_nx (vec_t<T> *v)
{
  extern void gt_pch_nx (T&);
  for (unsigned i = 0; i < v->prefix.num; i++)
    gt_pch_nx (v->vec[i]);
}

template<typename T>
void
gt_pch_nx (vec_t<T *> *v, gt_pointer_operator op, void *cookie)
{
  for (unsigned i = 0; i < v->prefix.num; i++)
    op (&(v->vec[i]), cookie);
}

template<typename T>
void
gt_pch_nx (vec_t<T> *v, gt_pointer_operator op, void *cookie)
{
  extern void gt_pch_nx (T *, gt_pointer_operator, void *);
  for (unsigned i = 0; i < v->prefix.num; i++)
    gt_pch_nx (&(v->vec[i]), op, cookie);
}


/* FIXME cxx-conversion.  Remove these definitions and update all
   calling sites.  */
/* Vector of integer-like object.  */
#define DEF_VEC_I(T)			struct vec_swallow_trailing_semi
#define DEF_VEC_ALLOC_I(T,A)		struct vec_swallow_trailing_semi

/* Vector of pointer to object.  */
#define DEF_VEC_P(T)			struct vec_swallow_trailing_semi
#define DEF_VEC_ALLOC_P(T,A)		struct vec_swallow_trailing_semi

/* Vector of object.  */
#define DEF_VEC_O(T)			struct vec_swallow_trailing_semi
#define DEF_VEC_ALLOC_O(T,A)		struct vec_swallow_trailing_semi

/* Vectors on the stack.  */
#define DEF_VEC_ALLOC_P_STACK(T)	struct vec_swallow_trailing_semi
#define DEF_VEC_ALLOC_O_STACK(T)	struct vec_swallow_trailing_semi
#define DEF_VEC_ALLOC_I_STACK(T)	struct vec_swallow_trailing_semi

/* Vectors of atomic types.  Atomic types do not need to have its
   elements marked for GC and PCH.  To avoid unnecessary traversals,
   we provide template instantiations for the GC/PCH functions that
   do not traverse the vector.

   FIXME cxx-conversion - Once vec_t users are converted this can
   be provided in some other way (e.g., adding an additional template
   parameter to the vec_t class).  */
#define DEF_VEC_A(TYPE)						\
template<typename T>						\
void								\
gt_ggc_mx (vec_t<TYPE> *v ATTRIBUTE_UNUSED)			\
{								\
}								\
								\
template<typename T>						\
void								\
gt_pch_nx (vec_t<TYPE> *v ATTRIBUTE_UNUSED)			\
{								\
}								\
								\
template<typename T>						\
void								\
gt_pch_nx (vec_t<TYPE> *v ATTRIBUTE_UNUSED,			\
	   gt_pointer_operator op ATTRIBUTE_UNUSED,		\
	   void *cookie ATTRIBUTE_UNUSED)			\
{								\
}								\
struct vec_swallow_trailing_semi

#define DEF_VEC_ALLOC_A(T,A)		struct vec_swallow_trailing_semi

/* Support functions for stack vectors.  */
extern void *vec_stack_p_reserve_exact_1 (int, void *);
extern void *vec_stack_o_reserve (void *, int, size_t, size_t MEM_STAT_DECL);
extern void *vec_stack_o_reserve_exact (void *, int, size_t, size_t
					 MEM_STAT_DECL);
extern void vec_stack_free (void *);

/* Reallocate an array of elements with prefix.  */
template<typename T, enum vec_allocation_t A>
extern vec_t<T> *vec_reserve (vec_t<T> *, int MEM_STAT_DECL);

template<typename T, enum vec_allocation_t A>
extern vec_t<T> *vec_reserve_exact (vec_t<T> *, int MEM_STAT_DECL);

extern void dump_vec_loc_statistics (void);
extern void ggc_free (void *);
extern void vec_heap_free (void *);


/* Macros to invoke API calls.  A single macro works for both pointer
   and object vectors, but the argument and return types might well be
   different.  In each macro, T is the typedef of the vector elements,
   and A is the allocation strategy.  The allocation strategy is only
   present when it is required.  Some of these macros pass the vector,
   V, by reference (by taking its address), this is noted in the
   descriptions.  */

/* Length of vector
   unsigned VEC_T_length(const VEC(T) *v);

   Return the number of active elements in V.  V can be NULL, in which
   case zero is returned.  */

#define VEC_length(T,V)	(VEC_length_1<T> (V))

template<typename T>
static inline unsigned
VEC_length_1 (const vec_t<T> *vec_)
{
  return vec_ ? vec_->prefix.num : 0;
}


/* Check if vector is empty
   int VEC_T_empty(const VEC(T) *v);

   Return nonzero if V is an empty vector (or V is NULL), zero otherwise.  */

#define VEC_empty(T,V)	(VEC_empty_1<T> (V))

template<typename T>
static inline bool
VEC_empty_1 (const vec_t<T> *vec_)
{
  return VEC_length (T, vec_) == 0;
}


/* Get the address of the array of elements
   T *VEC_T_address (VEC(T) v)

   If you need to directly manipulate the array (for instance, you
   want to feed it to qsort), use this accessor.  */

#define VEC_address(T,V)	(VEC_address_1<T> (V))

template<typename T>
static inline T *
VEC_address_1 (vec_t<T> *vec_)
{
  return vec_ ? vec_->vec : 0;
}


/* Get the final element of the vector.
   T VEC_T_last(VEC(T) *v); // Integer
   T VEC_T_last(VEC(T) *v); // Pointer
   T *VEC_T_last(VEC(T) *v); // Object

   Return the final element.  V must not be empty.  */

#define VEC_last(T,V)	(VEC_last_1<T> (V VEC_CHECK_INFO))

template<typename T>
static inline T&
VEC_last_1 (vec_t<T> *vec_ VEC_CHECK_DECL)
{
  VEC_ASSERT (vec_ && vec_->prefix.num, "last", T, base);
  return vec_->vec[vec_->prefix.num - 1];
}


/* Index into vector
   T VEC_T_index(VEC(T) *v, unsigned ix); // Integer
   T VEC_T_index(VEC(T) *v, unsigned ix); // Pointer
   T *VEC_T_index(VEC(T) *v, unsigned ix); // Object

   Return the IX'th element.  IX must be in the domain of V.  */

#define VEC_index(T,V,I) (VEC_index_1<T> (V, I VEC_CHECK_INFO))

template<typename T>
static inline T&
VEC_index_1 (vec_t<T> *vec_, unsigned ix_ VEC_CHECK_DECL)
{
  VEC_ASSERT (vec_ && ix_ < vec_->prefix.num, "index", T, base);
  return vec_->vec[ix_];
}

template<typename T>
static inline const T&
VEC_index_1 (const vec_t<T> *vec_, unsigned ix_ VEC_CHECK_DECL)
{
  VEC_ASSERT (vec_ && ix_ < vec_->prefix.num, "index", T, base);
  return vec_->vec[ix_];
}


/* Iterate over vector
   int VEC_T_iterate(VEC(T) *v, unsigned ix, T &ptr); // Integer
   int VEC_T_iterate(VEC(T) *v, unsigned ix, T &ptr); // Pointer
   int VEC_T_iterate(VEC(T) *v, unsigned ix, T *&ptr); // Object

   Return iteration condition and update PTR to point to the IX'th
   element.  At the end of iteration, sets PTR to NULL.  Use this to
   iterate over the elements of a vector as follows,

     for (ix = 0; VEC_iterate(T,v,ix,ptr); ix++)
       continue;  */

#define VEC_iterate(T,V,I,P)	(VEC_iterate_1<T> (V, I, &(P)))

template<typename T>
static inline bool
VEC_iterate_1 (const vec_t<T> *vec_, unsigned ix_, T *ptr)
{
  if (vec_ && ix_ < vec_->prefix.num)
    {
      *ptr = vec_->vec[ix_];
      return true;
    }
  else
    {
      *ptr = 0;
      return false;
    }
}

template<typename T>
static inline bool
VEC_iterate_1 (vec_t<T> *vec_, unsigned ix_, T **ptr)
{
  if (vec_ && ix_ < vec_->prefix.num)
    {
      *ptr = &vec_->vec[ix_];
      return true;
    }
  else
    {
      *ptr = 0;
      return false;
    }
}

/* Convenience macro for forward iteration.  */

#define FOR_EACH_VEC_ELT(T, V, I, P)		\
  for (I = 0; VEC_iterate (T, (V), (I), (P)); ++(I))

/* Likewise, but start from FROM rather than 0.  */

#define FOR_EACH_VEC_ELT_FROM(T, V, I, P, FROM)		\
  for (I = (FROM); VEC_iterate (T, (V), (I), (P)); ++(I))

/* Convenience macro for reverse iteration.  */

#define FOR_EACH_VEC_ELT_REVERSE(T,V,I,P) \
  for (I = VEC_length (T, (V)) - 1;           \
       VEC_iterate (T, (V), (I), (P));	  \
       (I)--)


/* Use these to determine the required size and initialization of a
   vector embedded within another structure (as the final member).

   size_t VEC_T_embedded_size(int reserve);
   void VEC_T_embedded_init(VEC(T) *v, int reserve);

   These allow the caller to perform the memory allocation.  */

#define VEC_embedded_size(T,N)	 (VEC_embedded_size_1<T> (N))

template<typename T>
static inline size_t
VEC_embedded_size_1 (int alloc_)
{
  return offsetof (vec_t<T>, vec) + alloc_ * sizeof (T);
}

#define VEC_embedded_init(T,O,N) (VEC_embedded_init_1<T> (O, N))

template<typename T>
static inline void
VEC_embedded_init_1 (vec_t<T> *vec_, int alloc_)
{
  vec_->prefix.num = 0;
  vec_->prefix.alloc = alloc_;
}


/* Allocate new vector.
   VEC(T,A) *VEC_T_A_alloc(int reserve);

   Allocate a new vector with space for RESERVE objects.  If RESERVE
   is zero, NO vector is created.

   We support a vector which starts out with space on the stack and
   switches to heap space when forced to reallocate.  This works a
   little differently.  In the case of stack vectors, VEC_alloc will
   expand to a call to VEC_alloc_1 that calls XALLOCAVAR to request the
   initial allocation.  This uses alloca to get the initial space.
   Since alloca can not be usefully called in an inline function,
   VEC_alloc must always be a macro.

   Only the initial allocation will be made using alloca, so pass a
   reasonable estimate that doesn't use too much stack space; don't
   pass zero.  Don't return a VEC(TYPE,stack) vector from the function
   which allocated it.  */

#define VEC_alloc(T,A,N)					\
  ((A == stack)							\
    ? VEC_alloc_1 (N,						\
 		   XALLOCAVAR (vec_t<T>, 			\
			       VEC_embedded_size_1<T> (N)))	\
    : VEC_alloc_1<T, A> (N MEM_STAT_INFO))

template<typename T, enum vec_allocation_t A>
static inline vec_t<T> *
VEC_alloc_1 (int alloc_ MEM_STAT_DECL)
{
  return vec_reserve_exact<T, A> (NULL, alloc_ PASS_MEM_STAT);
}

template<typename T>
static inline vec_t<T> *
VEC_alloc_1 (int alloc_, vec_t<T> *space)
{
  return (vec_t<T> *) vec_stack_p_reserve_exact_1 (alloc_, space);
}


/* Free a vector.
   void VEC_T_A_free(VEC(T,A) *&);

   Free a vector and set it to NULL.  */

#define VEC_free(T,A,V)		(VEC_free_1<T, A> (&V))

template<typename T, enum vec_allocation_t A>
static inline void
VEC_free_1 (vec_t<T> **vec_)
{
  if (*vec_)
    {
      if (A == heap)
	vec_heap_free (*vec_);
      else if (A == gc)
	ggc_free (*vec_);
      else if (A == stack)
	vec_stack_free (*vec_);
    }
  *vec_ = NULL;
}


/* Copy a vector.
   VEC(T,A) *VEC_T_A_copy(VEC(T) *);

   Copy the live elements of a vector into a new vector.  The new and
   old vectors need not be allocated by the same mechanism.  */

#define VEC_copy(T,A,V) (VEC_copy_1<T, A> (V MEM_STAT_INFO))

template<typename T, enum vec_allocation_t A>
static inline vec_t<T> *
VEC_copy_1 (vec_t<T> *vec_ MEM_STAT_DECL)
{
  size_t len_ = vec_ ? vec_->prefix.num : 0;
  vec_t<T> *new_vec_ = NULL;

  if (len_)
    {
      new_vec_ = vec_reserve_exact<T, A> (NULL, len_ PASS_MEM_STAT);
      new_vec_->prefix.num = len_;
      memcpy (new_vec_->vec, vec_->vec, sizeof (T) * len_);
    }
  return new_vec_;
}


/* Determine if a vector has additional capacity.

   int VEC_T_space (VEC(T) *v,int reserve)

   If V has space for RESERVE additional entries, return nonzero.  You
   usually only need to use this if you are doing your own vector
   reallocation, for instance on an embedded vector.  This returns
   nonzero in exactly the same circumstances that VEC_T_reserve
   will.  */

#define VEC_space(T,V,R)	(VEC_space_1<T> (V, R VEC_CHECK_INFO))

template<typename T>
static inline int
VEC_space_1 (vec_t<T> *vec_, int alloc_ VEC_CHECK_DECL)
{
  VEC_ASSERT (alloc_ >= 0, "space", T, base);
  return vec_
	 ? vec_->prefix.alloc - vec_->prefix.num >= (unsigned)alloc_
	 : !alloc_;
}


/* Reserve space.
   int VEC_T_A_reserve(VEC(T,A) *&v, int reserve);

   Ensure that V has at least RESERVE slots available.  This will
   create additional headroom.  Note this can cause V to be
   reallocated.  Returns nonzero iff reallocation actually
   occurred.  */

#define VEC_reserve(T,A,V,R)	\
  	(VEC_reserve_1<T, A> (&(V), (int)(R) VEC_CHECK_INFO MEM_STAT_INFO))

template<typename T, enum vec_allocation_t A>
static inline int
VEC_reserve_1 (vec_t<T> **vec_, int alloc_  VEC_CHECK_DECL MEM_STAT_DECL)
{
  int extend = !VEC_space_1 (*vec_, alloc_ VEC_CHECK_PASS);

  if (extend)
    *vec_ = vec_reserve<T, A> (*vec_, alloc_ PASS_MEM_STAT);

  return extend;
}


/* Reserve space exactly.
   int VEC_T_A_reserve_exact(VEC(T,A) *&v, int reserve);

   Ensure that V has at least RESERVE slots available.  This will not
   create additional headroom.  Note this can cause V to be
   reallocated.  Returns nonzero iff reallocation actually
   occurred.  */

#define VEC_reserve_exact(T,A,V,R)	\
	(VEC_reserve_exact_1<T, A> (&(V), R VEC_CHECK_INFO MEM_STAT_INFO))

template<typename T, enum vec_allocation_t A>
static inline int
VEC_reserve_exact_1 (vec_t<T> **vec_, int alloc_ VEC_CHECK_DECL MEM_STAT_DECL)
{
  int extend = !VEC_space_1 (*vec_, alloc_ VEC_CHECK_PASS);

  if (extend)
    *vec_ = vec_reserve_exact<T, A> (*vec_, alloc_ PASS_MEM_STAT);

  return extend;
}


/* Copy elements with no reallocation
   void VEC_T_splice (VEC(T) *dst, VEC(T) *src); // Integer
   void VEC_T_splice (VEC(T) *dst, VEC(T) *src); // Pointer
   void VEC_T_splice (VEC(T) *dst, VEC(T) *src); // Object

   Copy the elements in SRC to the end of DST as if by memcpy.  DST and
   SRC need not be allocated with the same mechanism, although they most
   often will be.  DST is assumed to have sufficient headroom
   available.  */

#define VEC_splice(T,DST,SRC)	(VEC_splice_1<T> (DST, SRC VEC_CHECK_INFO))

template<typename T>
static inline void
VEC_splice_1 (vec_t<T> *dst_, vec_t<T> *src_ VEC_CHECK_DECL)
{
  if (src_)
    {
      unsigned len_ = src_->prefix.num;
      VEC_ASSERT (dst_->prefix.num + len_ <= dst_->prefix.alloc, "splice",
		  T, base);

      memcpy (&dst_->vec[dst_->prefix.num], &src_->vec[0], len_ * sizeof (T));
      dst_->prefix.num += len_;
    }
}


/* Copy elements with reallocation
   void VEC_T_safe_splice (VEC(T,A) *&dst, VEC(T) *src); // Integer
   void VEC_T_safe_splice (VEC(T,A) *&dst, VEC(T) *src); // Pointer
   void VEC_T_safe_splice (VEC(T,A) *&dst, VEC(T) *src); // Object

   Copy the elements in SRC to the end of DST as if by memcpy.  DST and
   SRC need not be allocated with the same mechanism, although they most
   often will be.  DST need not have sufficient headroom and will be
   reallocated if needed.  */

#define VEC_safe_splice(T,A,DST,SRC)					\
	(VEC_safe_splice_1<T, A> (&(DST), SRC VEC_CHECK_INFO MEM_STAT_INFO))

template<typename T, enum vec_allocation_t A>
static inline void
VEC_safe_splice_1 (vec_t<T> **dst_, vec_t<T> *src_ VEC_CHECK_DECL MEM_STAT_DECL)
{
  if (src_)
    {
      VEC_reserve_exact_1<T, A> (dst_, src_->prefix.num
				 VEC_CHECK_PASS MEM_STAT_INFO);

      VEC_splice_1 (*dst_, src_ VEC_CHECK_PASS);
    }
}

  
/* Push object with no reallocation
   T *VEC_T_quick_push (VEC(T) *v, T obj); // Integer
   T *VEC_T_quick_push (VEC(T) *v, T obj); // Pointer
   T *VEC_T_quick_push (VEC(T) *v, T *obj); // Object

   Push a new element onto the end, returns a pointer to the slot
   filled in. For object vectors, the new value can be NULL, in which
   case NO initialization is performed.  There must
   be sufficient space in the vector.  */

#define VEC_quick_push(T,V,O)	(VEC_quick_push_1<T> (V, O VEC_CHECK_INFO))

template<typename T>
static inline T &
VEC_quick_push_1 (vec_t<T> *vec_, T obj_ VEC_CHECK_DECL)
{
  VEC_ASSERT (vec_->prefix.num < vec_->prefix.alloc, "push", T, base);
  vec_->vec[vec_->prefix.num] = obj_;
  T &val_ = vec_->vec[vec_->prefix.num];
  vec_->prefix.num++;
  return val_;
}

template<typename T>
static inline T *
VEC_quick_push_1 (vec_t<T> *vec_, const T *ptr_ VEC_CHECK_DECL)
{
  T *slot_;
  VEC_ASSERT (vec_->prefix.num < vec_->prefix.alloc, "push", T, base);
  slot_ = &vec_->vec[vec_->prefix.num++];
  if (ptr_)
    *slot_ = *ptr_;
  return slot_;
}


/* Push object with reallocation
   T *VEC_T_A_safe_push (VEC(T,A) *&v, T obj); // Integer
   T *VEC_T_A_safe_push (VEC(T,A) *&v, T obj); // Pointer
   T *VEC_T_A_safe_push (VEC(T,A) *&v, T *obj); // Object

   Push a new element onto the end, returns a pointer to the slot
   filled in. For object vectors, the new value can be NULL, in which
   case NO initialization is performed.  Reallocates V, if needed.  */

#define VEC_safe_push(T,A,V,O)		\
	(VEC_safe_push_1<T, A> (&(V), O VEC_CHECK_INFO MEM_STAT_INFO))

template<typename T, enum vec_allocation_t A>
static inline T &
VEC_safe_push_1 (vec_t<T> **vec_, T obj_ VEC_CHECK_DECL MEM_STAT_DECL)
{
  VEC_reserve_1<T, A> (vec_, 1 VEC_CHECK_PASS PASS_MEM_STAT);
  return VEC_quick_push_1 (*vec_, obj_ VEC_CHECK_PASS);
}

template<typename T, enum vec_allocation_t A>
static inline T *
VEC_safe_push_1 (vec_t<T> **vec_, const T *ptr_ VEC_CHECK_DECL MEM_STAT_DECL)
{
  VEC_reserve_1<T, A> (vec_, 1 VEC_CHECK_PASS PASS_MEM_STAT);
  return VEC_quick_push_1 (*vec_, ptr_ VEC_CHECK_PASS);
}


/* Pop element off end
   T VEC_T_pop (VEC(T) *v);		// Integer
   T VEC_T_pop (VEC(T) *v);		// Pointer
   void VEC_T_pop (VEC(T) *v);		// Object

   Pop the last element off the end. Returns the element popped, for
   pointer vectors.  */

#define VEC_pop(T,V)	(VEC_pop_1<T> (V VEC_CHECK_INFO))

template<typename T>
static inline T&
VEC_pop_1 (vec_t<T> *vec_ VEC_CHECK_DECL)
{
  VEC_ASSERT (vec_->prefix.num, "pop", T, base);
  return vec_->vec[--vec_->prefix.num];
}


/* Truncate to specific length
   void VEC_T_truncate (VEC(T) *v, unsigned len);

   Set the length as specified.  The new length must be less than or
   equal to the current length.  This is an O(1) operation.  */

#define VEC_truncate(T,V,I)	\
	(VEC_truncate_1<T> (V, (unsigned)(I) VEC_CHECK_INFO))

template<typename T>
static inline void
VEC_truncate_1 (vec_t<T> *vec_, unsigned size_ VEC_CHECK_DECL)
{
  VEC_ASSERT (vec_ ? vec_->prefix.num >= size_ : !size_, "truncate", T, base);
  if (vec_)
    vec_->prefix.num = size_;
}


/* Grow to a specific length.
   void VEC_T_A_safe_grow (VEC(T,A) *&v, int len);

   Grow the vector to a specific length.  The LEN must be as
   long or longer than the current length.  The new elements are
   uninitialized.  */

#define VEC_safe_grow(T,A,V,I)		\
	(VEC_safe_grow_1<T, A> (&(V), (int)(I) VEC_CHECK_INFO MEM_STAT_INFO))

template<typename T, enum vec_allocation_t A>
static inline void
VEC_safe_grow_1 (vec_t<T> **vec_, int size_ VEC_CHECK_DECL MEM_STAT_DECL)
{
  VEC_ASSERT (size_ >= 0 && VEC_length (T, *vec_) <= (unsigned)size_,
	      "grow", T, A);
  VEC_reserve_exact_1<T, A> (vec_,
			     size_ - (int)(*vec_ ? (*vec_)->prefix.num : 0)
			     VEC_CHECK_PASS PASS_MEM_STAT);
  (*vec_)->prefix.num = size_;
}


/* Grow to a specific length.
   void VEC_T_A_safe_grow_cleared (VEC(T,A) *&v, int len);

   Grow the vector to a specific length.  The LEN must be as
   long or longer than the current length.  The new elements are
   initialized to zero.  */

#define VEC_safe_grow_cleared(T,A,V,I)			\
	(VEC_safe_grow_cleared_1<T,A> (&(V), (int)(I)	\
				       VEC_CHECK_INFO MEM_STAT_INFO))

template<typename T, enum vec_allocation_t A>
static inline void
VEC_safe_grow_cleared_1 (vec_t<T> **vec_, int size_ VEC_CHECK_DECL
			 MEM_STAT_DECL)
{
  int oldsize = VEC_length (T, *vec_);
  VEC_safe_grow_1<T, A> (vec_, size_ VEC_CHECK_PASS PASS_MEM_STAT);
  memset (&(VEC_address (T, *vec_)[oldsize]), 0,
	  sizeof (T) * (size_ - oldsize));
}


/* Replace element
   T VEC_T_replace (VEC(T) *v, unsigned ix, T val); // Integer
   T VEC_T_replace (VEC(T) *v, unsigned ix, T val); // Pointer
   T *VEC_T_replace (VEC(T) *v, unsigned ix, T *val);  // Object

   Replace the IXth element of V with a new value, VAL.  For pointer
   vectors returns the original value. For object vectors returns a
   pointer to the new value.  For object vectors the new value can be
   NULL, in which case no overwriting of the slot is actually
   performed.  */

#define VEC_replace(T,V,I,O)		\
	(VEC_replace_1<T> (V, (unsigned)(I), O VEC_CHECK_INFO))

template<typename T>
static inline T&
VEC_replace_1 (vec_t<T> *vec_, unsigned ix_, T obj_ VEC_CHECK_DECL)
{
  VEC_ASSERT (ix_ < vec_->prefix.num, "replace", T, base);
  vec_->vec[ix_] = obj_;
  return vec_->vec[ix_];
}


/* Insert object with no reallocation
   void VEC_T_quick_insert (VEC(T) *v, unsigned ix, T val); // Integer
   void VEC_T_quick_insert (VEC(T) *v, unsigned ix, T val); // Pointer
   void VEC_T_quick_insert (VEC(T) *v, unsigned ix, T *val); // Object

   Insert an element, VAL, at the IXth position of V.  For vectors of
   object, the new value can be NULL, in which case no initialization
   of the inserted slot takes place. There must be sufficient space.  */

#define VEC_quick_insert(T,V,I,O)	\
	(VEC_quick_insert_1<T> (V,I,O VEC_CHECK_INFO))

template<typename T>
static inline void
VEC_quick_insert_1 (vec_t<T> *vec_, unsigned ix_, T obj_ VEC_CHECK_DECL)
{
  T *slot_;

  VEC_ASSERT (vec_->prefix.num < vec_->prefix.alloc, "insert", T, base);
  VEC_ASSERT (ix_ <= vec_->prefix.num, "insert", T, base);
  slot_ = &vec_->vec[ix_];
  memmove (slot_ + 1, slot_, (vec_->prefix.num++ - ix_) * sizeof (T));
  *slot_ = obj_;
}

template<typename T>
static inline void
VEC_quick_insert_1 (vec_t<T> *vec_, unsigned ix_, const T *ptr_ VEC_CHECK_DECL)
{
  T *slot_;

  VEC_ASSERT (vec_->prefix.num < vec_->prefix.alloc, "insert", T, base);
  VEC_ASSERT (ix_ <= vec_->prefix.num, "insert", T, base);
  slot_ = &vec_->vec[ix_];
  memmove (slot_ + 1, slot_, (vec_->prefix.num++ - ix_) * sizeof (T));
  if (ptr_)
    *slot_ = *ptr_;
}


/* Insert object with reallocation
   T *VEC_T_A_safe_insert (VEC(T,A) *&v, unsigned ix, T val); // Integer
   T *VEC_T_A_safe_insert (VEC(T,A) *&v, unsigned ix, T val); // Pointer
   T *VEC_T_A_safe_insert (VEC(T,A) *&v, unsigned ix, T *val); // Object

   Insert an element, VAL, at the IXth position of V. Return a pointer
   to the slot created.  For vectors of object, the new value can be
   NULL, in which case no initialization of the inserted slot takes
   place. Reallocate V, if necessary.  */

#define VEC_safe_insert(T,A,V,I,O)	\
	(VEC_safe_insert_1<T, A> (&(V),I,O VEC_CHECK_INFO MEM_STAT_INFO))

template<typename T, enum vec_allocation_t A>
static inline void
VEC_safe_insert_1 (vec_t<T> **vec_, unsigned ix_, T obj_
		   VEC_CHECK_DECL MEM_STAT_DECL)
{
  VEC_reserve_1<T, A> (vec_, 1 VEC_CHECK_PASS PASS_MEM_STAT);
  VEC_quick_insert_1 (*vec_, ix_, obj_ VEC_CHECK_PASS);
}

template<typename T, enum vec_allocation_t A>
static inline void
VEC_safe_insert_1 (vec_t<T> **vec_, unsigned ix_, T *ptr_
		   VEC_CHECK_DECL MEM_STAT_DECL)
{
  VEC_reserve_1<T, A> (vec_, 1 VEC_CHECK_PASS PASS_MEM_STAT);
  VEC_quick_insert_1 (*vec_, ix_, ptr_ VEC_CHECK_PASS);
}



/* Remove element retaining order
   void VEC_T_ordered_remove (VEC(T) *v, unsigned ix); // Integer
   void VEC_T_ordered_remove (VEC(T) *v, unsigned ix); // Pointer
   void VEC_T_ordered_remove (VEC(T) *v, unsigned ix); // Object

   Remove an element from the IXth position of V. Ordering of
   remaining elements is preserved.  This is an O(N) operation due to
   a memmove.  */

#define VEC_ordered_remove(T,V,I)	\
	(VEC_ordered_remove_1<T> (V,I VEC_CHECK_INFO))

template<typename T>
static inline void
VEC_ordered_remove_1 (vec_t<T> *vec_, unsigned ix_ VEC_CHECK_DECL)
{
  T *slot_;
  VEC_ASSERT (ix_ < vec_->prefix.num, "remove", T, base);
  slot_ = &vec_->vec[ix_];
  memmove (slot_, slot_ + 1, (--vec_->prefix.num - ix_) * sizeof (T));
}


/* Remove element destroying order
   void VEC_T_unordered_remove (VEC(T) *v, unsigned ix); // Integer
   void VEC_T_unordered_remove (VEC(T) *v, unsigned ix); // Pointer
   void VEC_T_unordered_remove (VEC(T) *v, unsigned ix); // Object

   Remove an element from the IXth position of V.  Ordering of
   remaining elements is destroyed.  This is an O(1) operation.  */

#define VEC_unordered_remove(T,V,I)	\
	(VEC_unordered_remove_1<T> (V,I VEC_CHECK_INFO))

template<typename T>
static inline void
VEC_unordered_remove_1 (vec_t<T> *vec_, unsigned ix_ VEC_CHECK_DECL)
{
  VEC_ASSERT (ix_ < vec_->prefix.num, "remove", T, base);
  vec_->vec[ix_] = vec_->vec[--vec_->prefix.num];
}


/* Remove a block of elements
   void VEC_T_block_remove (VEC(T) *v, unsigned ix, unsigned len);

   Remove LEN elements starting at the IXth.  Ordering is retained.
   This is an O(N) operation due to memmove.  */

#define VEC_block_remove(T,V,I,L)	\
	(VEC_block_remove_1<T> (V, I, L VEC_CHECK_INFO))

template<typename T>
static inline void
VEC_block_remove_1 (vec_t<T> *vec_, unsigned ix_, unsigned len_ VEC_CHECK_DECL)
{
  T *slot_;
  VEC_ASSERT (ix_ + len_ <= vec_->prefix.num, "block_remove", T, base);
  slot_ = &vec_->vec[ix_];
  vec_->prefix.num -= len_;
  memmove (slot_, slot_ + len_, (vec_->prefix.num - ix_) * sizeof (T));
}


/* Conveniently sort the contents of the vector with qsort.
   void VEC_qsort (VEC(T) *v, int (*cmp_func)(const void *, const void *))  */

#define VEC_qsort(T,V,CMP) qsort(VEC_address (T, V), VEC_length (T, V),	\
				 sizeof (T), CMP)


/* Find the first index in the vector not less than the object.
   unsigned VEC_T_lower_bound (VEC(T) *v, const T val,
                               bool (*lessthan) (const T, const T)); // Integer
   unsigned VEC_T_lower_bound (VEC(T) *v, const T val,
                               bool (*lessthan) (const T, const T)); // Pointer
   unsigned VEC_T_lower_bound (VEC(T) *v, const T *val,
                               bool (*lessthan) (const T*, const T*)); // Object

   Find the first position in which VAL could be inserted without
   changing the ordering of V.  LESSTHAN is a function that returns
   true if the first argument is strictly less than the second.  */

#define VEC_lower_bound(T,V,O,LT)	\
        (VEC_lower_bound_1<T> (V, O, LT VEC_CHECK_INFO))

template<typename T>
static inline unsigned
VEC_lower_bound_1 (vec_t<T> *vec_, T obj_,
		   bool (*lessthan_)(T, T) VEC_CHECK_DECL)
{
  unsigned int len_ = VEC_length (T, vec_);
  unsigned int half_, middle_;
  unsigned int first_ = 0;
  while (len_ > 0)
    {
      T middle_elem_;
      half_ = len_ >> 1;
      middle_ = first_;
      middle_ += half_;
      middle_elem_ = VEC_index_1 (vec_, middle_ VEC_CHECK_PASS);
      if (lessthan_ (middle_elem_, obj_))
	{
	  first_ = middle_;
	  ++first_;
	  len_ = len_ - half_ - 1;
	}
      else
	len_ = half_;
    }
  return first_;
}

template<typename T>
static inline unsigned
VEC_lower_bound_1 (vec_t<T> *vec_, const T *ptr_,
		   bool (*lessthan_)(const T*, const T*) VEC_CHECK_DECL)
{
  unsigned int len_ = VEC_length (T, vec_);
  unsigned int half_, middle_;
  unsigned int first_ = 0;
  while (len_ > 0)
    {
      T *middle_elem_;
      half_ = len_ >> 1;
      middle_ = first_;
      middle_ += half_;
      middle_elem_ = &VEC_index_1 (vec_, middle_ VEC_CHECK_PASS);
      if (lessthan_ (middle_elem_, ptr_))
	{
	  first_ = middle_;
	  ++first_;
	  len_ = len_ - half_ - 1;
	}
      else
	len_ = half_;
    }
  return first_;
}


void *vec_heap_o_reserve_1 (void *, int, size_t, size_t, bool MEM_STAT_DECL);
void *vec_gc_o_reserve_1 (void *, int, size_t, size_t, bool MEM_STAT_DECL);

/* Ensure there are at least RESERVE free slots in VEC_, growing
   exponentially.  If RESERVE < 0 grow exactly, else grow
   exponentially.  As a special case, if VEC_ is NULL, and RESERVE is
   0, no vector will be created. */

template<typename T, enum vec_allocation_t A>
vec_t<T> *
vec_reserve (vec_t<T> *vec_, int reserve MEM_STAT_DECL)
{
  if (A == gc)
    return (vec_t<T> *) vec_gc_o_reserve_1 (vec_, reserve,
					    offsetof (vec_t<T>, vec),
					    sizeof (T), false
					    PASS_MEM_STAT);
  else if (A == heap)
    return (vec_t<T> *) vec_heap_o_reserve_1 (vec_, reserve,
					      offsetof (vec_t<T>, vec),
					      sizeof (T), false
					      PASS_MEM_STAT);
  else
    return (vec_t<T> *) vec_stack_o_reserve (vec_, reserve,
					     offsetof (vec_t<T>, vec),
					     sizeof (T) PASS_MEM_STAT);
}


/* Ensure there are at least RESERVE free slots in VEC_, growing
   exactly.  If RESERVE < 0 grow exactly, else grow exponentially.  As
   a special case, if VEC_ is NULL, and RESERVE is 0, no vector will be
   created. */

template<typename T, enum vec_allocation_t A>
vec_t<T> *
vec_reserve_exact (vec_t<T> *vec_, int reserve MEM_STAT_DECL)
{
  if (A == gc)
    return (vec_t<T> *) vec_gc_o_reserve_1 (vec_, reserve,
					    sizeof (struct vec_prefix),
					    sizeof (T), true
					    PASS_MEM_STAT);
  else if (A == heap)
    return (vec_t<T> *) vec_heap_o_reserve_1 (vec_, reserve,
					      sizeof (struct vec_prefix),
					      sizeof (T), true
					      PASS_MEM_STAT);
  else if (A == stack)
    {
      /* Only allow stack vectors when re-growing them.  The initial
	 allocation of stack vectors must be done with VEC_alloc,
	 because it uses alloca() for the allocation.  */
      if (vec_ == NULL)
	{
	  fprintf (stderr, "Stack vectors must be initially allocated "
		   "with VEC_stack_alloc.\n");
	  gcc_unreachable ();
	}
      return (vec_t<T> *) vec_stack_o_reserve_exact (vec_, reserve,
						     sizeof (struct vec_prefix),
						     sizeof (T)
						     PASS_MEM_STAT);
    }
}

#endif /* GCC_VEC_H */
