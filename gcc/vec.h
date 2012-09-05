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

/* Templated vector type and associated interfaces.

   The interface functions are typesafe and use inline functions,
   sometimes backed by out-of-line generic functions.  The vectors are
   designed to interoperate with the GTY machinery.

   FIXME - Remove the following compatibility notes after a handler
   class for vec_t is implemented.

   To preserve compatibility with the existing API, some functions
   that manipulate vector elements implement two overloads: one taking
   a pointer to the element and others that take a pointer to a
   pointer to the element.

   This used to be implemented with three different families of macros
   and structures: structure objects, scalar objects and of pointers.
   Both the structure object and pointer variants passed pointers to
   objects around -- in the former case the pointers were stored into
   the vector and in the latter case the pointers were dereferenced and
   the objects copied into the vector.  The scalar object variant was
   suitable for int-like objects, and the vector elements were returned
   by value.

   There are both 'index' and 'iterate' accessors.  The index accessor
   is implemented by operator[].  The iterator returns a boolean
   iteration condition and updates the iteration variable passed by
   reference.  Because the iterator will be inlined, the address-of
   can be optimized away.

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
   when the vector is allocated.  This can occur via the VEC_alloc
   call or one of the VEC_safe_* functions that add elements to a
   vector.  If the vector is NULL, it will be allocated using the
   allocation strategy selected in the call.  The valid allocations
   are defined in enum vec_allocation_t.

   If you need to directly manipulate a vector, then the 'address'
   accessor will return the address of the start of the vector.  Also
   the 'space' predicate will tell you whether there is spare capacity
   in the vector.  You will not normally need to use these two functions.

   Variables of vector type are of type vec_t<ETYPE> where ETYPE is
   the type of the elements of the vector. Due to the way GTY works,
   you must annotate any structures you wish to insert or reference
   from a vector with a GTY(()) tag.  You need to do this even if you
   never use the GC allocated variants.

   An example of their use would be,

   struct my_struct {
     vec_t<tree> *v;      // A (pointer to) a vector of tree pointers.
   };

   struct my_struct *s;

   if (VEC_length(tree,s->v)) { we have some contents }
   VEC_safe_push(tree,gc,s->v,decl); // append some decl onto the end
   for (ix = 0; VEC_iterate(tree,s->v,ix,elt); ix++)
     { do something with elt }

*/

#if ENABLE_CHECKING
#define ALONE_VEC_CHECK_INFO __FILE__, __LINE__, __FUNCTION__
#define VEC_CHECK_INFO , ALONE_VEC_CHECK_INFO
#define ALONE_VEC_CHECK_DECL const char *file_, unsigned line_, const char *function_
#define VEC_CHECK_DECL , ALONE_VEC_CHECK_DECL
#define ALONE_VEC_CHECK_PASS file_, line_, function_
#define VEC_CHECK_PASS , ALONE_VEC_CHECK_PASS

#define VEC_ASSERT(EXPR,OP,T,A) \
  (void)((EXPR) ? 0 : (VEC_ASSERT_FAIL(OP,VEC(T,A)), 0))

extern void vec_assert_fail (const char *, const char * VEC_CHECK_DECL)
     ATTRIBUTE_NORETURN;
#define VEC_ASSERT_FAIL(OP,VEC) vec_assert_fail (OP,#VEC VEC_CHECK_PASS)
#else
#define ALONE_VEC_CHECK_INFO
#define VEC_CHECK_INFO
#define ALONE_VEC_CHECK_DECL void
#define VEC_CHECK_DECL
#define ALONE_VEC_CHECK_PASS
#define VEC_CHECK_PASS
#define VEC_ASSERT(EXPR,OP,T,A) (void)(EXPR)
#endif

#define VEC(T,A) vec_t<T>

enum vec_allocation_t { heap, gc, stack };

struct vec_prefix
{
  unsigned num_;
  unsigned alloc_;
};

/* Vector type, user visible.  */
template<typename T>
struct GTY(()) vec_t
{
  unsigned length (void) const;
  bool empty (void) const;
  T *address (void);
  T &last (ALONE_VEC_CHECK_DECL);
  const T &operator[] (unsigned) const;
  T &operator[] (unsigned);
  void embedded_init (int, int = 0);

  template<enum vec_allocation_t A>
  vec_t<T> *copy (ALONE_MEM_STAT_DECL);

  bool space (int VEC_CHECK_DECL);
  void splice (vec_t<T> * VEC_CHECK_DECL);
  T &quick_push (T VEC_CHECK_DECL);
  T *quick_push (const T * VEC_CHECK_DECL);
  T &pop (ALONE_VEC_CHECK_DECL);
  void truncate (unsigned VEC_CHECK_DECL);
  void replace (unsigned, T VEC_CHECK_DECL);
  void quick_insert (unsigned, T VEC_CHECK_DECL);
  void quick_insert (unsigned, const T * VEC_CHECK_DECL);
  void ordered_remove (unsigned VEC_CHECK_DECL);
  void unordered_remove (unsigned VEC_CHECK_DECL);
  void block_remove (unsigned, unsigned VEC_CHECK_DECL);

  unsigned lower_bound (T, bool (*)(T, T)) const;
  unsigned lower_bound (const T *, bool (*)(const T *, const T *)) const;

  /* Class-static member functions.  Some of these will become member
     functions of a future handler class wrapping vec_t.  */
  static size_t embedded_size (int);

  template<enum vec_allocation_t A>
  static vec_t<T> *alloc (int MEM_STAT_DECL);

  static vec_t<T> *alloc (int, vec_t<T> *);

  template<enum vec_allocation_t A>
  static void free (vec_t<T> **);

  template<enum vec_allocation_t A>
  static vec_t<T> *reserve_exact (vec_t<T> *, int MEM_STAT_DECL);

  template<enum vec_allocation_t A>
  static bool reserve_exact (vec_t<T> **, int VEC_CHECK_DECL MEM_STAT_DECL);

  template<enum vec_allocation_t A>
  static vec_t<T> *reserve (vec_t<T> *, int MEM_STAT_DECL);

  template<enum vec_allocation_t A>
  static bool reserve (vec_t<T> **, int VEC_CHECK_DECL MEM_STAT_DECL);

  template<enum vec_allocation_t A>
  static void safe_splice (vec_t<T> **, vec_t<T> * VEC_CHECK_DECL
			   MEM_STAT_DECL);

  template<enum vec_allocation_t A>
  static T &safe_push (vec_t<T> **, T VEC_CHECK_DECL MEM_STAT_DECL);

  template<enum vec_allocation_t A>
  static T *safe_push (vec_t<T> **, const T * VEC_CHECK_DECL MEM_STAT_DECL);

  template<enum vec_allocation_t A>
  static void safe_grow (vec_t<T> **, int VEC_CHECK_DECL MEM_STAT_DECL);

  template<enum vec_allocation_t A>
  static void safe_grow_cleared (vec_t<T> **, int VEC_CHECK_DECL MEM_STAT_DECL);

  template<enum vec_allocation_t A>
  static void safe_insert (vec_t<T> **, unsigned, T * VEC_CHECK_DECL
			   MEM_STAT_DECL);

  template<enum vec_allocation_t A>
  static void safe_insert (vec_t<T> **, unsigned, T obj VEC_CHECK_DECL
			   MEM_STAT_DECL);

  static bool iterate (const vec_t<T> *, unsigned, T *);
  static bool iterate (const vec_t<T> *, unsigned, T **);

  vec_prefix prefix_;
  T vec_[1];
};


/* Garbage collection support for vec_t.  */

template<typename T>
void
gt_ggc_mx (vec_t<T> *v)
{
  extern void gt_ggc_mx (T &);
  for (unsigned i = 0; i < v->length (); i++)
    gt_ggc_mx ((*v)[i]);
}


/* PCH support for vec_t.  */

template<typename T>
void
gt_pch_nx (vec_t<T> *v)
{
  extern void gt_pch_nx (T &);
  for (unsigned i = 0; i < v->length (); i++)
    gt_pch_nx ((*v)[i]);
}

template<typename T>
void
gt_pch_nx (vec_t<T *> *v, gt_pointer_operator op, void *cookie)
{
  for (unsigned i = 0; i < v->length (); i++)
    op (&((*v)[i]), cookie);
}

template<typename T>
void
gt_pch_nx (vec_t<T> *v, gt_pointer_operator op, void *cookie)
{
  extern void gt_pch_nx (T *, gt_pointer_operator, void *);
  for (unsigned i = 0; i < v->length (); i++)
    gt_pch_nx (&((*v)[i]), op, cookie);
}


/* FIXME.  Remove these definitions and update all calling sites after
   the handler class for vec_t is implemented.  */

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

extern void dump_vec_loc_statistics (void);
extern void ggc_free (void *);
extern void vec_heap_free (void *);


/* API compatibility macros (to be removed).  */
#define VEC_length(T,V)							\
	((V) ? (V)->length () : 0)

#define VEC_empty(T,V)							\
	((V) ? (V)->empty () : true)

#define VEC_address(T,V)						\
	vec_address<T> (V)

/* FIXME.  For now, we need to continue expanding VEC_address into a
   function call.  Otherwise, the warning machinery for -Wnonnull gets
   confused thinking that VEC_address may return null in calls to
   memcpy and qsort.  This will disappear once vec_address becomes
   a member function for a handler class wrapping vec_t.  */

template<typename T>
static inline T *
vec_address (vec_t<T> *vec)
{
  return vec ? vec->address() : NULL;
}

#define VEC_last(T,V)							\
	((V)->last (ALONE_VEC_CHECK_INFO))

#define VEC_index(T,V,I)						\
	((*(V))[I])

#define VEC_iterate(T,V,I,P)						\
	(vec_t<T>::iterate(V, I, &(P)))

#define VEC_embedded_size(T,N)						\
	(vec_t<T>::embedded_size (N))

#define VEC_embedded_init(T,V,N)					\
	((V)->embedded_init (N))

#define VEC_free(T,A,V)							\
	(vec_t<T>::free<A> (&(V)))

#define VEC_copy(T,A,V)							\
	((V)->copy<A> (ALONE_MEM_STAT_INFO))

#define VEC_space(T,V,R)						\
	((V) ? (V)->space (R VEC_CHECK_INFO) : (R) == 0)

#define VEC_reserve(T,A,V,R)						\
	(vec_t<T>::reserve<A> (&(V), (int)(R) VEC_CHECK_INFO MEM_STAT_INFO))

#define VEC_reserve_exact(T,A,V,R)					\
	(vec_t<T>::reserve_exact<A> (&(V), R VEC_CHECK_INFO MEM_STAT_INFO))

#define VEC_splice(T,DST,SRC)	        				\
	(DST)->splice (SRC VEC_CHECK_INFO)

#define VEC_safe_splice(T,A,DST,SRC)					\
	 vec_t<T>::safe_splice<A> (&(DST), SRC VEC_CHECK_INFO MEM_STAT_INFO)

#define VEC_quick_push(T,V,O)						\
	((V)->quick_push (O VEC_CHECK_INFO))

#define VEC_safe_push(T,A,V,O)						\
	(vec_t<T>::safe_push<A> (&(V), O VEC_CHECK_INFO MEM_STAT_INFO))

#define VEC_pop(T,V)							\
	((V)->pop (ALONE_VEC_CHECK_INFO))

#define VEC_truncate(T,V,I)						\
	(V								\
	 ? (V)->truncate ((unsigned)(I) VEC_CHECK_INFO)			\
	 : gcc_assert ((I) == 0))

#define VEC_safe_grow(T,A,V,I)						\
	(vec_t<T>::safe_grow<A> (&(V), (int)(I) VEC_CHECK_INFO MEM_STAT_INFO))

#define VEC_safe_grow_cleared(T,A,V,I)					\
	(vec_t<T>::safe_grow_cleared<A> (&(V), (int)(I)			\
				         VEC_CHECK_INFO MEM_STAT_INFO))

#define VEC_replace(T,V,I,O)						\
	((V)->replace ((unsigned)(I), O VEC_CHECK_INFO))

#define VEC_quick_insert(T,V,I,O)					\
	((V)->quick_insert (I,O VEC_CHECK_INFO))

#define VEC_safe_insert(T,A,V,I,O)					\
	(vec_t<T>::safe_insert<A> (&(V), I, O VEC_CHECK_INFO MEM_STAT_INFO))

#define VEC_ordered_remove(T,V,I)					\
	((V)->ordered_remove (I VEC_CHECK_INFO))

#define VEC_unordered_remove(T,V,I)					\
	((V)->unordered_remove (I VEC_CHECK_INFO))

#define VEC_block_remove(T,V,I,L)					\
	((V)->block_remove (I, L VEC_CHECK_INFO))

#define VEC_lower_bound(T,V,O,LT)					\
	((V)->lower_bound (O, LT))


/* Return the number of active elements in this vector.  */

template<typename T>
inline unsigned
vec_t<T>::length (void) const
{
  return prefix_.num_;
}


/* Return true if this vector has no active elements.  */

template<typename T>
inline bool
vec_t<T>::empty (void) const
{
  return length () == 0;
}


/* Return the address of the array of elements.  If you need to
   directly manipulate the array (for instance, you want to feed it
   to qsort), use this accessor.  */

template<typename T>
inline T *
vec_t<T>::address (void)
{
  return vec_;
}


/* Get the final element of the vector, which must not be empty.  */

template<typename T>
T &
vec_t<T>::last (ALONE_VEC_CHECK_DECL)
{
  VEC_ASSERT (prefix_.num_, "last", T, base);
  return (*this)[prefix_.num_ - 1];
}


/* Index into vector.  Return the IX'th element.  IX must be in the
   domain of the vector.  */

template<typename T>
const T &
vec_t<T>::operator[] (unsigned ix) const
{
  gcc_assert (ix < prefix_.num_);
  return vec_[ix];
}

template<typename T>
T &
vec_t<T>::operator[] (unsigned ix)
{
  gcc_assert (ix < prefix_.num_);
  return vec_[ix];
}


/* Return iteration condition and update PTR to point to the IX'th
   element of VEC.  Use this to iterate over the elements of a vector
   as follows,

     for (ix = 0; vec_t<T>::iterate(v, ix, &ptr); ix++)
       continue;
   
   FIXME.  This is a static member function because if VEC is NULL,
   PTR should be initialized to NULL.  This will become a regular
   member function of the handler class.  */

template<typename T>
bool
vec_t<T>::iterate (const vec_t<T> *vec, unsigned ix, T *ptr)
{
  if (vec && ix < vec->prefix_.num_)
    {
      *ptr = vec->vec_[ix];
      return true;
    }
  else
    {
      *ptr = 0;
      return false;
    }
}


/* Return iteration condition and update *PTR to point to the
   IX'th element of VEC.  Use this to iterate over the elements of a
   vector as follows,

     for (ix = 0; v->iterate(ix, &ptr); ix++)
       continue;

   This variant is for vectors of objects.  FIXME, to be removed
   once the distinction between vec_t<T> and vec_t<T *> disappears.  */

template<typename T>
bool
vec_t<T>::iterate (const vec_t<T> *vec, unsigned ix, T **ptr)
{
  if (vec && ix < vec->prefix_.num_)
    {
      *ptr = CONST_CAST (T *, &vec->vec_[ix]);
      return true;
    }
  else
    {
      *ptr = 0;
      return false;
    }
}


/* Convenience macro for forward iteration.  */

#define FOR_EACH_VEC_ELT(T, V, I, P)			\
  for (I = 0; VEC_iterate (T, (V), (I), (P)); ++(I))

/* Likewise, but start from FROM rather than 0.  */

#define FOR_EACH_VEC_ELT_FROM(T, V, I, P, FROM)		\
  for (I = (FROM); VEC_iterate (T, (V), (I), (P)); ++(I))

/* Convenience macro for reverse iteration.  */

#define FOR_EACH_VEC_ELT_REVERSE(T, V, I, P)		\
  for (I = VEC_length (T, (V)) - 1;			\
       VEC_iterate (T, (V), (I), (P));			\
       (I)--)


/* Return the number of bytes needed to embed an instance of vec_t inside
   another data structure.

   Use these methods to determine the required size and initialization
   of a vector V of type T embedded within another structure (as the
   final member):

   size_t vec_t<T>::embedded_size<T> (int reserve);
   void v->embedded_init(int reserve, int active);

   These allow the caller to perform the memory allocation.  */

template<typename T>
size_t
vec_t<T>::embedded_size (int nelems)
{
  return offsetof (vec_t<T>, vec_) + nelems * sizeof (T);
}


/* Initialize the vector to contain room for NELEMS elements and
   ACTIVE active elements.  */

template<typename T>
void
vec_t<T>::embedded_init (int nelems, int active)
{
  prefix_.num_ = active;
  prefix_.alloc_ = nelems;
}


/* Allocate a new vector with space for RESERVE objects.  If RESERVE
   is zero, NO vector is created.

   Note that this allocator must always be a macro:

   We support a vector which starts out with space on the stack and
   switches to heap space when forced to reallocate.  This works a
   little differently.  In the case of stack vectors, vec_alloc will
   expand to a call to vec_alloc_1 that calls XALLOCAVAR to request the
   initial allocation.  This uses alloca to get the initial space.
   Since alloca can not be usefully called in an inline function,
   vec_alloc must always be a macro.

   Important limitations of stack vectors:

   - Only the initial allocation will be made using alloca, so pass a
     reasonable estimate that doesn't use too much stack space; don't
     pass zero.

   - Don't return a stack-allocated vector from the function which
     allocated it.  */

#define VEC_alloc(T,A,N)						\
  ((A == stack)								\
    ? vec_t<T>::alloc (N, XALLOCAVAR (vec_t<T>, vec_t<T>::embedded_size (N)))\
    : vec_t<T>::alloc<A> (N MEM_STAT_INFO))

template<typename T>
template<enum vec_allocation_t A>
vec_t<T> *
vec_t<T>::alloc (int nelems MEM_STAT_DECL)
{
  return reserve_exact<A> ((vec_t<T> *) NULL, nelems PASS_MEM_STAT);
}

template<typename T>
vec_t<T> *
vec_t<T>::alloc (int nelems, vec_t<T> *space)
{
  return static_cast <vec_t<T> *> (vec_stack_p_reserve_exact_1 (nelems, space));
}


/* Free vector *V and set it to NULL.  */

template<typename T>
template<enum vec_allocation_t A>
void
vec_t<T>::free (vec_t<T> **v)
{
  if (*v)
    {
      if (A == heap)
	vec_heap_free (*v);
      else if (A == gc)
	ggc_free (*v);
      else if (A == stack)
	vec_stack_free (*v);
    }
  *v = NULL;
}


/* Return a copy of this vector.  The new and old vectors need not be
   allocated by the same mechanism.  */

template<typename T>
template<enum vec_allocation_t A>
vec_t<T> *
vec_t<T>::copy (ALONE_MEM_STAT_DECL)
{
  unsigned len = VEC_length (T, this);
  vec_t<T> *new_vec = NULL;

  if (len)
    {
      new_vec = reserve_exact<A> (static_cast<vec_t<T> *> (NULL),
				  len PASS_MEM_STAT);
      new_vec->embedded_init (len, len);
      memcpy (new_vec->address (), vec_, sizeof (T) * len);
    }

  return new_vec;
}


/* If this vector has space for RESERVE additional entries, return
   true.  You usually only need to use this if you are doing your
   own vector reallocation, for instance on an embedded vector.  This
   returns true in exactly the same circumstances that vec_reserve
   will.  */

template<typename T>
bool
vec_t<T>::space (int nelems VEC_CHECK_DECL)
{
  VEC_ASSERT (nelems >= 0, "space", T, base);
  return prefix_.alloc_ - prefix_.num_ >= static_cast <unsigned> (nelems);
}


/* Ensure that the vector **VEC has at least RESERVE slots available.  This
   will create additional headroom.  Note this can cause **VEC to
   be reallocated.  Returns true iff reallocation actually occurred.  */

template<typename T>
template<enum vec_allocation_t A>
bool
vec_t<T>::reserve (vec_t<T> **vec, int nelems VEC_CHECK_DECL MEM_STAT_DECL)
{
  bool extend = (*vec) ? !(*vec)->space (nelems VEC_CHECK_PASS) : nelems != 0;

  if (extend)
    *vec = reserve<A> (*vec, nelems PASS_MEM_STAT);

  return extend;
}


/* Ensure that **VEC has at least NELEMS slots available.  This will not
   create additional headroom.  Note this can cause VEC to be
   reallocated.  Returns true iff reallocation actually occurred.  */

template<typename T>
template<enum vec_allocation_t A>
bool
vec_t<T>::reserve_exact (vec_t<T> **vec, int nelems VEC_CHECK_DECL
			 MEM_STAT_DECL)
{
  bool extend = (*vec) ? !(*vec)->space (nelems VEC_CHECK_PASS) : nelems != 0;

  if (extend)
    *vec = reserve_exact<A> (*vec, nelems PASS_MEM_STAT);

  return extend;
}


/* Copy the elements from SRC to the end of this vector as if by memcpy.
   SRC and this vector need not be allocated with the same mechanism,
   although they most often will be.  This vector is assumed to have
   sufficient headroom available.  */

template<typename T>
void
vec_t<T>::splice (vec_t<T> *src VEC_CHECK_DECL)
{
  if (src)
    {
      unsigned len = VEC_length (T, src);
      VEC_ASSERT (VEC_length (T, this) + len <= prefix_.alloc_, "splice", T,
		  base);
      memcpy (address () + VEC_length (T, this),
	      src->address (),
	      len * sizeof (T));
      prefix_.num_ += len;
    }
}


/* Copy the elements in SRC to the end of DST as if by memcpy.  DST and
   SRC need not be allocated with the same mechanism, although they most
   often will be.  DST need not have sufficient headroom and will be
   reallocated if needed.  */

template<typename T>
template<enum vec_allocation_t A>
void
vec_t<T>::safe_splice (vec_t<T> **dst, vec_t<T> *src VEC_CHECK_DECL
		       MEM_STAT_DECL)
{
  if (src)
    {
      reserve_exact<A> (dst, VEC_length (T, src) VEC_CHECK_PASS MEM_STAT_INFO);
      (*dst)->splice (src VEC_CHECK_PASS);
    }
}

  
/* Push OBJ (a new element) onto the end, returns a reference to the slot
   filled in.  There must be sufficient space in the vector.  */

template<typename T>
T &
vec_t<T>::quick_push (T obj VEC_CHECK_DECL)
{
  VEC_ASSERT (prefix_.num_ < prefix_.alloc_, "push", T, base);
  vec_[prefix_.num_] = obj;
  T &val = vec_[prefix_.num_];
  prefix_.num_++;
  return val;
}


/* Push PTR (a new pointer to an element) onto the end, returns a
   pointer to the slot filled in. The new value can be NULL, in which
   case NO initialization is performed.  There must be sufficient
   space in the vector.  */

template<typename T>
T *
vec_t<T>::quick_push (const T *ptr VEC_CHECK_DECL)
{
  VEC_ASSERT (prefix_.num_ < prefix_.alloc_, "push", T, base);
  T *slot = &vec_[prefix_.num_++];
  if (ptr)
    *slot = *ptr;
  return slot;
}


/* Push a new element OBJ onto the end of VEC.  Returns a reference to
   the slot filled in.  Reallocates V, if needed.  */

template<typename T>
template<enum vec_allocation_t A>
T &
vec_t<T>::safe_push (vec_t<T> **vec, T obj VEC_CHECK_DECL MEM_STAT_DECL)
{
  reserve<A> (vec, 1 VEC_CHECK_PASS PASS_MEM_STAT);
  return (*vec)->quick_push (obj VEC_CHECK_PASS);
}


/* Push a pointer PTR to a new element onto the end of VEC.  Returns a
   pointer to the slot filled in. For object vectors, the new value
   can be NULL, in which case NO initialization is performed.
   Reallocates VEC, if needed.  */

template<typename T>
template<enum vec_allocation_t A>
T *
vec_t<T>::safe_push (vec_t<T> **vec, const T *ptr VEC_CHECK_DECL MEM_STAT_DECL)
{
  reserve<A> (vec, 1 VEC_CHECK_PASS PASS_MEM_STAT);
  return (*vec)->quick_push (ptr VEC_CHECK_PASS);
}


/* Pop and return the last element off the end of the vector.  */


template<typename T>
T &
vec_t<T>::pop (ALONE_VEC_CHECK_DECL)
{
  VEC_ASSERT (prefix_.num_, "pop", T, base);
  return vec_[--prefix_.num_];
}


/* Set the length of the vector to LEN.  The new length must be less
   than or equal to the current length.  This is an O(1) operation.  */

template<typename T>
void
vec_t<T>::truncate (unsigned size VEC_CHECK_DECL)
{
  VEC_ASSERT (prefix_.num_ >= size, "truncate", T, base);
  prefix_.num_ = size;
}


/* Grow the vector VEC to a specific length.  The LEN must be as
   long or longer than the current length.  The new elements are
   uninitialized.  */

template<typename T>
template<enum vec_allocation_t A>
void
vec_t<T>::safe_grow (vec_t<T> **vec, int size VEC_CHECK_DECL MEM_STAT_DECL)
{
  VEC_ASSERT (size >= 0 && VEC_length (T, *vec) <= (unsigned)size,
	      "grow", T, A);
  reserve_exact<A> (vec, size - (int)VEC_length (T, *vec)
		    VEC_CHECK_PASS PASS_MEM_STAT);
  (*vec)->prefix_.num_ = size;
}


/* Grow the vector *VEC to a specific length.  The LEN must be as
   long or longer than the current length.  The new elements are
   initialized to zero.  */

template<typename T>
template<enum vec_allocation_t A>
void
vec_t<T>::safe_grow_cleared (vec_t<T> **vec, int size VEC_CHECK_DECL
			     MEM_STAT_DECL)
{
  int oldsize = VEC_length (T, *vec);
  safe_grow<A> (vec, size VEC_CHECK_PASS PASS_MEM_STAT);
  memset (&((*vec)->address ()[oldsize]), 0, sizeof (T) * (size - oldsize));
}


/* Replace the IXth element of this vector with a new value, VAL.  */

template<typename T>
void
vec_t<T>::replace (unsigned ix, T obj VEC_CHECK_DECL)
{
  VEC_ASSERT (ix < prefix_.num_, "replace", T, base);
  vec_[ix] = obj;
}


/* Insert an element, OBJ, at the IXth position of VEC.  There must be
   sufficient space.  */

template<typename T>
void
vec_t<T>::quick_insert (unsigned ix, T obj VEC_CHECK_DECL)
{
  VEC_ASSERT (prefix_.num_ < prefix_.alloc_, "insert", T, base);
  VEC_ASSERT (ix <= prefix_.num_, "insert", T, base);
  T *slot = &vec_[ix];
  memmove (slot + 1, slot, (prefix_.num_++ - ix) * sizeof (T));
  *slot = obj;
}


/* Insert an element, *PTR, at the IXth position of V.  The new value
   can be NULL, in which case no initialization of the inserted slot
   takes place. There must be sufficient space.  */

template<typename T>
void
vec_t<T>::quick_insert (unsigned ix, const T *ptr VEC_CHECK_DECL)
{
  VEC_ASSERT (prefix_.num_ < prefix_.alloc_, "insert", T, base);
  VEC_ASSERT (ix <= prefix_.num_, "insert", T, base);
  T *slot = &vec_[ix];
  memmove (slot + 1, slot, (prefix_.num_++ - ix) * sizeof (T));
  if (ptr)
    *slot = *ptr;
}


/* Insert an element, VAL, at the IXth position of VEC. Reallocate
   VEC, if necessary.  */

template<typename T>
template<enum vec_allocation_t A>
void
vec_t<T>::safe_insert (vec_t<T> **vec, unsigned ix, T obj VEC_CHECK_DECL
		       MEM_STAT_DECL)
{
  reserve<A> (vec, 1 VEC_CHECK_PASS PASS_MEM_STAT);
  (*vec)->quick_insert (ix, obj VEC_CHECK_PASS);
}


/* Insert an element, *PTR, at the IXth position of VEC. Return a pointer
   to the slot created.  For vectors of object, the new value can be
   NULL, in which case no initialization of the inserted slot takes
   place. Reallocate V, if necessary.  */

template<typename T>
template<enum vec_allocation_t A>
void
vec_t<T>::safe_insert (vec_t<T> **vec, unsigned ix, T *ptr VEC_CHECK_DECL
		       MEM_STAT_DECL)
{
  reserve<A> (vec, 1 VEC_CHECK_PASS PASS_MEM_STAT);
  (*vec)->quick_insert (ix, ptr VEC_CHECK_PASS);
}


/* Remove an element from the IXth position of this vector.  Ordering of
   remaining elements is preserved.  This is an O(N) operation due to
   a memmove.  */

template<typename T>
void
vec_t<T>::ordered_remove (unsigned ix VEC_CHECK_DECL)
{
  VEC_ASSERT (ix < prefix_.num_, "remove", T, base);
  T *slot = &vec_[ix];
  memmove (slot, slot + 1, (--prefix_.num_ - ix) * sizeof (T));
}


/* Remove an element from the IXth position of VEC.  Ordering of
   remaining elements is destroyed.  This is an O(1) operation.  */

template<typename T>
void
vec_t<T>::unordered_remove (unsigned ix VEC_CHECK_DECL)
{
  VEC_ASSERT (ix < prefix_.num_, "remove", T, base);
  vec_[ix] = vec_[--prefix_.num_];
}


/* Remove LEN elements starting at the IXth.  Ordering is retained.
   This is an O(N) operation due to memmove.  */

template<typename T>
void
vec_t<T>::block_remove (unsigned ix, unsigned len VEC_CHECK_DECL)
{
  VEC_ASSERT (ix + len <= prefix_.num_, "block_remove", T, base);
  T *slot = &vec_[ix];
  prefix_.num_ -= len;
  memmove (slot, slot + len, (prefix_.num_ - ix) * sizeof (T));
}

/* Sort the contents of V with qsort.  Use CMP as the comparison function.  */
#define VEC_qsort(T,V,CMP)						\
	qsort (VEC_address (T, V), VEC_length (T, V), sizeof (T), CMP)


/* Find and return the first position in which OBJ could be inserted
   without changing the ordering of this vector.  LESSTHAN is a
   function that returns true if the first argument is strictly less
   than the second.  */

template<typename T>
unsigned
vec_t<T>::lower_bound (T obj, bool (*lessthan)(T, T)) const
{
  unsigned int len = VEC_length (T, this);
  unsigned int half, middle;
  unsigned int first = 0;
  while (len > 0)
    {
      half = len >> 1;
      middle = first;
      middle += half;
      T middle_elem = (*this)[middle];
      if (lessthan (middle_elem, obj))
	{
	  first = middle;
	  ++first;
	  len = len - half - 1;
	}
      else
	len = half;
    }
  return first;
}


/* Find and return the first position in which *PTR could be inserted
   without changing the ordering of this vector.  LESSTHAN is a
   function that returns true if the first argument is strictly less
   than the second.  */

template<typename T>
unsigned
vec_t<T>::lower_bound (const T *ptr,
		       bool (*lessthan)(const T *, const T *)) const
{
  unsigned int len = VEC_length (T, this);
  unsigned int half, middle;
  unsigned int first = 0;
  while (len > 0)
    {
      half = len >> 1;
      middle = first;
      middle += half;
      const T *middle_elem = &(*this)[middle];
      if (lessthan (middle_elem, ptr))
	{
	  first = middle;
	  ++first;
	  len = len - half - 1;
	}
      else
	len = half;
    }
  return first;
}


void *vec_heap_o_reserve_1 (void *, int, size_t, size_t, bool MEM_STAT_DECL);
void *vec_gc_o_reserve_1 (void *, int, size_t, size_t, bool MEM_STAT_DECL);

/* Ensure there are at least RESERVE free slots in VEC_, growing
   exponentially.  If RESERVE < 0 grow exactly, else grow
   exponentially.  As a special case, if VEC_ is NULL, and RESERVE is
   0, no vector will be created. */

template<typename T>
template<enum vec_allocation_t A>
vec_t<T> *
vec_t<T>::reserve (vec_t<T> *vec, int reserve MEM_STAT_DECL)
{
  void *res = NULL;
  size_t off = offsetof (vec_t<T>, vec_);
  size_t sz = sizeof (T);

  switch (A)
    {
      case gc:
	res = vec_gc_o_reserve_1 (vec, reserve, off, sz, false PASS_MEM_STAT);
	break;
      case heap:
	res = vec_heap_o_reserve_1 (vec, reserve, off, sz, false PASS_MEM_STAT);
	break;
      case stack:
	res = vec_stack_o_reserve (vec, reserve, off, sz PASS_MEM_STAT);
	break;
    }

  return static_cast <vec_t<T> *> (res);
}


/* Ensure there are at least RESERVE free slots in VEC, growing
   exactly.  If RESERVE < 0 grow exactly, else grow exponentially.  As
   a special case, if VEC is NULL, and RESERVE is 0, no vector will be
   created. */

template<typename T>
template<enum vec_allocation_t A>
vec_t<T> *
vec_t<T>::reserve_exact (vec_t<T> *vec, int reserve MEM_STAT_DECL)
{
  void *res = NULL;
  size_t off = sizeof (struct vec_prefix);
  size_t sz = sizeof (T);

  gcc_assert (offsetof (vec_t<T>, vec_) == sizeof (struct vec_prefix));

  switch (A)
    {
      case gc:
	res = vec_gc_o_reserve_1 (vec, reserve, off, sz, true PASS_MEM_STAT);
	break;
      case heap:
	res = vec_heap_o_reserve_1 (vec, reserve, off, sz, true PASS_MEM_STAT);
	break;
      case stack:
	res = vec_stack_o_reserve_exact (vec, reserve, off, sz PASS_MEM_STAT);
	break;
    }

  return static_cast <vec_t<T> *> (res);
}

#endif /* GCC_VEC_H */
