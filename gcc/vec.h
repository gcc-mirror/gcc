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

#ifndef GCC_VEC_H
#define GCC_VEC_H

/* The macros here implement a set of templated vector types and
   associated interfaces.  These templates are implemented with
   macros, as we're not in C++ land.  The interface functions are
   typesafe and use static inline functions, sometimes backed by
   out-of-line generic functions.  The vectors are designed to
   interoperate with the GTY machinery.

   Because of the different behaviour of objects and of pointers to
   objects, there are two flavours.  One to deal with a vector of
   pointers to objects, and one to deal with a vector of objects
   themselves.  Both of these pass pointers to objects around -- in
   the former case the pointers are stored into the vector and in the
   latter case the pointers are dereferenced and the objects copied
   into the vector.  Therefore, when using a vector of pointers, the
   objects pointed to must be long lived, but when dealing with a
   vector of objects, the source objects need not be.

   The vectors are implemented using the trailing array idiom, thus
   they are not resizeable without changing the address of the vector
   object itself.  This means you cannot have variables or fields of
   vector type -- always use a pointer to a vector.  The one exception
   is the final field of a structure, which could be a vector type.
   You will have to use the embedded_alloc call to create such
   objects, and they will probably not be resizeable (so don't use the
   'safe' allocation variants).  The trailing array idiom is used
   (rather than a pointer to an array of data), because, if we allow
   NULL to also represent an empty vector, empty vectors occupy
   minimal space in the structure containing them.

   Each operation that increases the number of active elements is
   available in 'quick' and 'safe' variants.  The former presumes that
   there is sufficient allocated space for the operation to succeed
   (it aborts if there is not).  The latter will reallocate the
   vector, if needed.  Reallocation causes an exponential increase in
   vector size.  If you know you will be adding N elements, it would
   be more efficient to use the reserve operation before adding the
   elements with the 'quick' operation.

   You should prefer the push and pop operations, as they append and
   remove from the end of the vector.  The insert and remove
   operations allow you to change elements in the middle of the
   vector.  There are two remove operations, one which preserves the
   element ordering 'ordered_remove', and one which does not
   'unordered_remove'.  The latter function copies the end element
   into the removed slot, rather than invoke a memmove operation.
   
   Vector types are defined using a DEF_VEC_x(TYPEDEF) macro, and
   variables of vector type are declared using a VEC(TYPEDEF)
   macro. The 'x' letter indicates whether TYPEDEF is a pointer (P) or
   object (O) type.

   An example of their use would be,

   DEF_VEC_P(tree);	// define a vector of tree pointers.  This must
   			// appear at file scope.

   struct my_struct {
     VEC(tree) *v;      // A (pointer to) a vector of tree pointers.
   };

   struct my_struct *s;

   if (VEC_length(tree,s)) { we have some contents }
   VEC_safe_push(tree,s,decl); // append some decl onto the end
   for (ix = 0; (t = VEC_iterate(tree,s,ix)); ix++)
     { do something with t }

*/

/* Macros to invoke API calls.  A single macro works for both pointer
   and object vectors, but the argument and return types might well be
   different.  In each macro, TDEF is the typedef of the vector
   elements.  Some of these macros pass the vector, V, by reference
   (by taking its address), this is noted in the descriptions.  */

/* Length of vector
   size_t VEC_T_length(const VEC(T) *v);

   Return the number of active elements in V.  V can be NULL, in which
   case zero is returned.  */
#define VEC_length(TDEF,V)		(VEC_OP(TDEF,length)(V))

/* Get the final element of the vector.
   T VEC_T_last(VEC(T) *v); // Pointer
   T *VEC_T_last(VEC(T) *v); // Object

   Return the final element.  If V is empty,  abort.  */
#define VEC_last(TDEF,V)		(VEC_OP(TDEF,last)(V))

/* Index into vector
   T VEC_T_index(VEC(T) *v, size_t ix); // Pointer
   T *VEC_T_index(VEC(T) *v, size_t ix); // Object

   Return the IX'th element.  If IX is outside the domain of V,
   abort.  */
#define VEC_index(TDEF,V,I)		(VEC_OP(TDEF,index)(V,I))

/* Iterate over vector
   T VEC_T_index(VEC(T) *v, size_t ix); // Pointer
   T *VEC_T_index(VEC(T) *v, size_t ix); // Object

   Return the IX'th element or NULL. Use this to iterate over the
   elements of a vector as follows,

     for (ix = 0; (ptr = VEC_iterate(T,v,ix)); ix++)
       continue;  */
#define VEC_iterate(TDEF,V,I)		(VEC_OP(TDEF,iterate)(V,I))

/* Allocate new vector.
   VEC(T) *VEC_T_alloc(size_t reserve);

   Allocate a new vector with space for RESERVE objects.  */
#define VEC_alloc(TDEF,A)		(VEC_OP(TDEF,alloc)(A))

/* Allocate new vector offset within a structure
   void *VEC_T_embedded_alloc(size_t offset, size_t reserve);

   Allocate a new vector which is at offset OFFSET within a structure,
   and with space for RESERVE objects.  Return a pointer to the start
   of the structure containing the vector.  Naturally, the vector must
   be the last member of the structure.  */
#define VEC_embedded_alloc(TDEF,O,A)	(VEC_OP(TDEF,embedded_alloc)(O,A))

/* Reserve space.
   void VEC_T_reserve(VEC(T) *&v, size_t reserve);

   Ensure that V has at least RESERVE slots available.  Note this can
   cause V to be reallocated.  */
#define VEC_reserve(TDEF,V,R)		(VEC_OP(TDEF,reserve)(&(V),R))

/* Push object with no reallocation
   T *VEC_T_quick_push (VEC(T) *v, T obj); // Pointer
   T *VEC_T_quick_push (VEC(T) *v, T *obj); // Object
   
   Push a new element onto the end, returns a pointer to the slot
   filled in. For object vectors, the new value can be NULL, in which
   case NO initialization is performed.  Aborts if there is
   insufficient space in the vector. */
#define VEC_quick_push(TDEF,V,O)	(VEC_OP(TDEF,quick_push)(V,O))

/* Push object with reallocation
   T *VEC_T_safe_push (VEC(T) *&v, T obj); // Pointer
   T *VEC_T_safe_push (VEC(T) *&v, T *obj); // Object
   
   Push a new element onto the end, returns a pointer to the slot
   filled in. For object vectors, the new value can be NULL, in which
   case NO initialization is performed.  Reallocates V, if needed.  */
#define VEC_safe_push(TDEF,V,O)		(VEC_OP(TDEF,safe_push)(&(V),O))

/* Pop element off end
   T VEC_T_pop (VEC(T) *v);		// Pointer
   void VEC_T_pop (VEC(T) *v);		// Object

   Pop the last element off the end. Returns the element popped, for
   pointer vectors.  */
#define VEC_pop(TDEF,V)			(VEC_OP(TDEF,pop)(V))

/* Replace element
   T VEC_T_replace (VEC(T) *v, size_t ix, T val); // Pointer
   T *VEC_T_replace (VEC(T) *v, size_t ix, T *val);  // Object
   
   Replace the IXth element of V with a new value, VAL.  For pointer
   vectors returns the original value. For object vectors returns a
   pointer to the new value.  For object vectors the new value can be
   NULL, in which case no overwriting of the slot is actually
   performed.  */
#define VEC_replace(TDEF,V,I,O)		(VEC_OP(TDEF,replace)(V,I,O))

/* Insert object with no reallocation
   T *VEC_T_quick_insert (VEC(T) *v, size_t ix, T val); // Pointer
   T *VEC_T_quick_insert (VEC(T) *v, size_t ix, T *val); // Object
   
   Insert an element, VAL, at the IXth position of V. Return a pointer
   to the slot created.  For vectors of object, the new value can be
   NULL, in which case no initialization of the inserted slot takes
   place. Aborts if there is insufficient space.  */
#define VEC_quick_insert(TDEF,V,I,O)	(VEC_OP(TDEF,quick_insert)(V,I,O))

/* Insert object with reallocation
   T *VEC_T_safe_insert (VEC(T) *&v, size_t ix, T val); // Pointer
   T *VEC_T_safe_insert (VEC(T) *&v, size_t ix, T *val); // Object
   
   Insert an element, VAL, at the IXth position of V. Return a pointer
   to the slot created.  For vectors of object, the new value can be
   NULL, in which case no initialization of the inserted slot takes
   place. Reallocate V, if necessary.  */
#define VEC_safe_insert(TDEF,V,I,O)	(VEC_OP(TDEF,safe_insert)(&(V),I,O))
     
/* Remove element retaining order
   T VEC_T_ordered_remove (VEC(T) *v, size_t ix); // Pointer
   void VEC_T_ordered_remove (VEC(T) *v, size_t ix); // Object
   
   Remove an element from the IXth position of V. Ordering of
   remaining elements is preserverd.  For pointer vectors returns the
   removed object.  This is an O(N) operation due to a memmove.  */
#define VEC_ordered_remove(TDEF,V,I)	(VEC_OP(TDEF,ordered_remove)(V,I))

/* Remove element destroying order
   T VEC_T_unordered_remove (VEC(T) *v, size_t ix); // Pointer
   void VEC_T_unordered_remove (VEC(T) *v, size_t ix); // Object
   
   Remove an element from the IXth position of V. Ordering of
   remaining elements is destroyed.  For pointer vectors returns the
   removed object.  This is an O(1) operation.  */
#define VEC_unordered_remove(TDEF,V,I)	(VEC_OP(TDEF,unordered_remove)(V,I))

#if !IN_GENGTYPE
#include "auto-host.h"

/* Reallocate an array of elements with prefix.  */
extern void *vec_p_reserve (void *, size_t);
extern void *vec_o_reserve (void *, size_t, size_t, size_t);
extern void *vec_embedded_alloc (size_t, size_t, size_t, size_t);

#if ENABLE_CHECKING
extern void vec_assert_fail (const char *, const char *,
			    const char *, size_t, const char *)
     ATTRIBUTE_NORETURN;
#define VEC_ASSERT_FAIL(OP,VEC) \
  vec_assert_fail (OP,#VEC,__FILE__,__LINE__,__FUNCTION__)
     
#define VEC_ASSERT(EXPR,OP,TDEF) \
  (void)((EXPR) ? 0 : (VEC_ASSERT_FAIL(OP,VEC(TDEF)), 0))
#else
#define VEC_ASSERT(EXPR,OP,TYPE) (void)(EXPR)
#endif

#define VEC(TDEF) VEC_##TDEF
#define VEC_OP(TDEF,OP) VEC_OP_(VEC(TDEF),OP)
#define VEC_OP_(VEC,OP) VEC_OP__(VEC,OP)
#define VEC_OP__(VEC,OP) VEC ## _ ## OP
#else  /* IN_GENGTYPE */
#define VEC(TDEF) VEC_ TDEF
#define VEC_STRINGIFY(X) VEC_STRINGIFY_(X)
#define VEC_STRINGIFY_(X) #X
#undef GTY
#endif /* IN_GENGTYPE */

#define VEC_TDEF(TDEF)							  \
typedef struct VEC (TDEF) GTY(())					  \
{									  \
  size_t num;								  \
  size_t alloc;								  \
  TDEF GTY ((length ("%h.num"))) vec[1];				  \
} VEC (TDEF)

/* Vector of pointer to object.  */
#if IN_GENGTYPE
{"DEF_VEC_P", VEC_STRINGIFY (VEC_TDEF (#)) ";", NULL},
#else
  
#define DEF_VEC_P(TDEF)							  \
VEC_TDEF (TDEF);							  \
									  \
static inline size_t VEC_OP (TDEF,length)				  \
     (const VEC (TDEF) *vec_) 						  \
{									  \
  return vec_ ? vec_->num : 0;						  \
}									  \
									  \
static inline TDEF VEC_OP (TDEF,last)					  \
     (const VEC (TDEF) *vec_)						  \
{									  \
  VEC_ASSERT (vec_ && vec_->num, "last", TDEF);				  \
  									  \
  return vec_->vec[vec_->num - 1];					  \
}									  \
									  \
static inline TDEF VEC_OP (TDEF,index)					  \
     (const VEC (TDEF) *vec_, size_t ix_)				  \
{									  \
  VEC_ASSERT (vec_ && ix_ < vec_->num, "index", TDEF);			  \
  									  \
  return vec_->vec[ix_];						  \
}									  \
									  \
static inline TDEF VEC_OP (TDEF,iterate)		  	     	  \
     (const VEC (TDEF) *vec_, size_t ix_)				  \
{									  \
  return vec_ && ix_ < vec_->num ? vec_->vec[ix_] : NULL;		  \
}									  \
									  \
static inline VEC (TDEF) *VEC_OP (TDEF,alloc)		       		  \
     (size_t alloc_)							  \
{									  \
  return vec_p_reserve (NULL, alloc_ - !alloc_);			  \
}									  \
									  \
static inline void *VEC_OP (TDEF,embedded_alloc)			  \
     (size_t offset_, size_t alloc_)					  \
{									  \
  return vec_embedded_alloc (offset_, offsetof (VEC(TDEF),vec),		  \
			     sizeof (TDEF), alloc_);			  \
}									  \
									  \
static inline void VEC_OP (TDEF,reserve)	       			  \
     (VEC (TDEF) **vec_, size_t alloc_)					  \
{									  \
  *vec_ = vec_p_reserve (*vec_, alloc_);				  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,quick_push)				  \
     (VEC (TDEF) *vec_, TDEF obj_)					  \
{									  \
  TDEF *slot_;								  \
  									  \
  VEC_ASSERT (vec_->num < vec_->alloc, "push", TDEF);			  \
  slot_ = &vec_->vec[vec_->num++];					  \
  *slot_ = obj_;							  \
  									  \
  return slot_;								  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,safe_push)				  \
     (VEC (TDEF) **vec_, TDEF obj_)					  \
{									  \
  if (!*vec_ || (*vec_)->num == (*vec_)->alloc)				  \
    VEC_OP (TDEF,reserve) (vec_, ~(size_t)0);				  \
									  \
  return VEC_OP (TDEF,quick_push) (*vec_, obj_);			  \
}									  \
									  \
static inline TDEF VEC_OP (TDEF,pop)					  \
     (VEC (TDEF) *vec_)			       				  \
{									  \
  TDEF obj_;								  \
									  \
  VEC_ASSERT (vec_->num, "pop", TDEF);					  \
  obj_ = vec_->vec[--vec_->num];					  \
									  \
  return obj_;								  \
}									  \
									  \
static inline TDEF VEC_OP (TDEF,replace)		  	     	  \
     (VEC (TDEF) *vec_, size_t ix_, TDEF obj_)				  \
{									  \
  TDEF old_obj_;							  \
									  \
  VEC_ASSERT (ix_ < vec_->num, "replace", TDEF);			  \
  old_obj_ = vec_->vec[ix_];						  \
  vec_->vec[ix_] = obj_;						  \
									  \
  return old_obj_;							  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,quick_insert)		     	  	  \
     (VEC (TDEF) *vec_, size_t ix_, TDEF obj_)				  \
{									  \
  TDEF *slot_;								  \
									  \
  VEC_ASSERT (vec_->num < vec_->alloc, "insert", TDEF);			  \
  VEC_ASSERT (ix_ <= vec_->num, "insert", TDEF);			  \
  slot_ = &vec_->vec[ix_];						  \
  memmove (slot_ + 1, slot_, vec_->num++ - ix_);			  \
  *slot_ = obj_;							  \
  									  \
  return slot_;								  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,safe_insert)		     	  	  \
     (VEC (TDEF) **vec_, size_t ix_, TDEF obj_)       			  \
{									  \
  if (!*vec_ || (*vec_)->num == (*vec_)->alloc)				  \
    VEC_OP (TDEF,reserve) (vec_, ~(size_t)0);				  \
									  \
  return VEC_OP (TDEF,quick_insert) (*vec_, ix_, obj_);			  \
}									  \
									  \
static inline TDEF VEC_OP (TDEF,ordered_remove)				  \
     (VEC (TDEF) *vec_, size_t ix_)					  \
{									  \
  TDEF *slot_;								  \
  TDEF obj_;								  \
									  \
  VEC_ASSERT (ix_ < vec_->num, "remove", TDEF);				  \
  slot_ = &vec_->vec[ix_];						  \
  obj_ = *slot_;							  \
  memmove (slot_, slot_ + 1, --vec_->num - ix_);       			  \
									  \
  return obj_;								  \
}									  \
									  \
static inline TDEF VEC_OP (TDEF,unordered_remove)			  \
     (VEC (TDEF) *vec_, size_t ix_)					  \
{									  \
  TDEF *slot_;								  \
  TDEF obj_;								  \
									  \
  VEC_ASSERT (ix_ < vec_->num, "remove", TDEF);				  \
  slot_ = &vec_->vec[ix_];						  \
  obj_ = *slot_;							  \
  *slot_ = vec_->vec[--vec_->num];					  \
									  \
  return obj_;								  \
}									  \
									  \
struct vec_swallow_trailing_semi
#endif

/* Vector of object.  */
#if IN_GENGTYPE
{"DEF_VEC_O", VEC_STRINGIFY (VEC_TDEF (#)) ";", NULL},
#else
  
#define DEF_VEC_O(TDEF)							  \
VEC_TDEF (TDEF);							  \
									  \
static inline size_t VEC_OP (TDEF,length)				  \
     (const VEC (TDEF) *vec_) 						  \
{									  \
  return vec_ ? vec_->num : 0;						  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,last)					  \
     (VEC (TDEF) *vec_)							  \
{									  \
  VEC_ASSERT (vec_ && vec_->num, "last", TDEF);				  \
  									  \
  return &vec_->vec[vec_->num - 1];					  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,index)					  \
     (VEC (TDEF) *vec_, size_t ix_)					  \
{									  \
  VEC_ASSERT (vec_ && ix_ < vec_->num, "index", TDEF);			  \
  									  \
  return &vec_->vec[ix_];						  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,iterate)				  \
     (VEC (TDEF) *vec_, size_t ix_)					  \
{									  \
  return vec_ && ix_ < vec_->num ? &vec_->vec[ix_] : NULL;		  \
}									  \
									  \
static inline VEC (TDEF) *VEC_OP (TDEF,alloc)      			  \
     (size_t alloc_)							  \
{									  \
  return vec_o_reserve (NULL, alloc_ - !alloc_,				  \
			offsetof (VEC(TDEF),vec), sizeof (TDEF));	  \
}									  \
									  \
static inline void *VEC_OP (TDEF,embedded_alloc)			  \
     (size_t offset_, size_t alloc_)					  \
{									  \
  return vec_embedded_alloc (offset_, offsetof (VEC(TDEF),vec),		  \
			     sizeof (TDEF), alloc_);			  \
}									  \
									  \
static inline void VEC_OP (TDEF,reserve)	       			  \
     (VEC (TDEF) **vec_, size_t alloc_)					  \
{									  \
  *vec_ = vec_o_reserve (*vec_, alloc_,					  \
			 offsetof (VEC(TDEF),vec), sizeof (TDEF));	  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,quick_push)				  \
     (VEC (TDEF) *vec_, const TDEF *obj_)				  \
{									  \
  TDEF *slot_;								  \
  									  \
  VEC_ASSERT (vec_->num < vec_->alloc, "push", TDEF);			  \
  slot_ = &vec_->vec[vec_->num++];					  \
  if (obj_)								  \
    *slot_ = *obj_;							  \
  									  \
  return slot_;								  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,safe_push)				  \
     (VEC (TDEF) **vec_, const TDEF *obj_)				  \
{									  \
  if (!*vec_ || (*vec_)->num == (*vec_)->alloc)				  \
    VEC_OP (TDEF,reserve) (vec_, ~(size_t)0);				  \
									  \
  return VEC_OP (TDEF,quick_push) (*vec_, obj_);			  \
}									  \
									  \
static inline void VEC_OP (TDEF,pop)					  \
     (VEC (TDEF) *vec_)							  \
{									  \
  VEC_ASSERT (vec_->num, "pop", TDEF);					  \
  vec_->vec[--vec_->num];						  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,replace)				  \
     (VEC (TDEF) *vec_, size_t ix_, const TDEF *obj_)			  \
{									  \
  TDEF *slot_;								  \
									  \
  VEC_ASSERT (ix_ < vec_->num, "replace", TDEF);			  \
  slot_ = &vec_->vec[ix_];						  \
  if (obj_)								  \
    *slot_ = *obj_;							  \
									  \
  return slot_;								  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,quick_insert)				  \
     (VEC (TDEF) *vec_, size_t ix_, const TDEF *obj_)			  \
{									  \
  TDEF *slot_;								  \
									  \
  VEC_ASSERT (vec_->num < vec_->alloc, "insert", TDEF);			  \
  VEC_ASSERT (ix_ <= vec_->num, "insert", TDEF);			  \
  slot_ = &vec_->vec[ix_];						  \
  memmove (slot_ + 1, slot_, vec_->num++ - ix_);			  \
  if (obj_)								  \
    *slot_ = *obj_;							  \
  									  \
  return slot_;								  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,safe_insert)		     	  	  \
     (VEC (TDEF) **vec_, size_t ix_, const TDEF *obj_)			  \
{									  \
  if (!*vec_ || (*vec_)->num == (*vec_)->alloc)				  \
    VEC_OP (TDEF,reserve) (vec_, ~(size_t)0);				  \
									  \
  return VEC_OP (TDEF,quick_insert) (*vec_, ix_, obj_);			  \
}									  \
									  \
static inline void VEC_OP (TDEF,ordered_remove)				  \
     (VEC (TDEF) *vec_, size_t ix_)					  \
{									  \
  TDEF *slot_;								  \
									  \
  VEC_ASSERT (ix_ < vec_->num, "remove", TDEF);				  \
  slot_ = &vec_->vec[ix_];						  \
  memmove (slot_, slot_ + 1, --vec_->num - ix_);       			  \
}									  \
									  \
static inline void VEC_OP (TDEF,unordered_remove)			  \
     (VEC (TDEF) *vec_, size_t ix_)					  \
{									  \
  VEC_ASSERT (ix_ < vec_->num, "remove", TDEF);				  \
  vec_->vec[ix_] = vec_->vec[--vec_->num];				  \
}									  \
									  \
struct vec_swallow_trailing_semi
#endif

#endif /* GCC_VEC_H */
