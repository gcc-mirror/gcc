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

   Because of the different behavior of objects and of pointers to
   objects, there are two flavors.  One to deal with a vector of
   pointers to objects, and one to deal with a vector of objects
   themselves.  Both of these pass pointers to objects around -- in
   the former case the pointers are stored into the vector and in the
   latter case the pointers are dereferenced and the objects copied
   into the vector.  Therefore, when using a vector of pointers, the
   objects pointed to must be long lived, but when dealing with a
   vector of objects, the source objects need not be.

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
   (it aborts if there is not).  The latter will reallocate the
   vector, if needed.  Reallocation causes an exponential increase in
   vector size.  If you know you will be adding N elements, it would
   be more efficient to use the reserve operation before adding the
   elements with the 'quick' operation.  You may also use the reserve
   operation with a -1 operand, to gain control over exactly when
   reallocation occurs.

   You should prefer the push and pop operations, as they append and
   remove from the end of the vector. If you need to remove several
   items in one go, use the truncate operation.  The insert and remove
   operations allow you to change elements in the middle of the
   vector.  There are two remove operations, one which preserves the
   element ordering 'ordered_remove', and one which does not
   'unordered_remove'.  The latter function copies the end element
   into the removed slot, rather than invoke a memmove operation.
   The 'lower_bound' function will determine where to place an item in the
   array using insert that will maintain sorted order.

   Both garbage collected and explicitly managed vector types are
   creatable.  The allocation mechanism is specified when the type is
   defined, and is therefore part of the type.
   
   If you need to directly manipulate a vector, then the 'address'
   accessor will return the address of the start of the vector.  Also
   the 'space' predicate will tell you whether there is spare capacity
   in the vector.  You will not normally need to use these two functions.
   
   Vector types are defined using a DEF_VEC_{GC,MALLOC}_{O,P}(TYPEDEF)
   macro, and variables of vector type are declared using a
   VEC(TYPEDEF) macro.  The tags GC and MALLOC specify the allocation
   method -- garbage collected or explicit malloc/free calls.  The
   characters O and P indicate whether TYPEDEF is a pointer (P) or
   object (O) type.

   An example of their use would be,

   DEF_VEC_GC_P(tree);	// define a gc'd vector of tree pointers.  This must
   			// appear at file scope.

   struct my_struct {
     VEC(tree) *v;      // A (pointer to) a vector of tree pointers.
   };

   struct my_struct *s;

   if (VEC_length(tree,s->v)) { we have some contents }
   VEC_safe_push(tree,s->v,decl); // append some decl onto the end
   for (ix = 0; VEC_iterate(tree,s->v,ix,elt); ix++)
     { do something with elt }

*/

/* Macros to invoke API calls.  A single macro works for both pointer
   and object vectors, but the argument and return types might well be
   different.  In each macro, TDEF is the typedef of the vector
   elements.  Some of these macros pass the vector, V, by reference
   (by taking its address), this is noted in the descriptions.  */

/* Length of vector
   unsigned VEC_T_length(const VEC(T) *v);

   Return the number of active elements in V.  V can be NULL, in which
   case zero is returned.  */

#define VEC_length(TDEF,V)	(VEC_OP(TDEF,length)(V))

/* Get the final element of the vector.
   T VEC_T_last(VEC(T) *v); // Pointer
   T *VEC_T_last(VEC(T) *v); // Object

   Return the final element.  If V is empty,  abort.  */

#define VEC_last(TDEF,V)	(VEC_OP(TDEF,last)(V VEC_CHECK_INFO))

/* Index into vector
   T VEC_T_index(VEC(T) *v, unsigned ix); // Pointer
   T *VEC_T_index(VEC(T) *v, unsigned ix); // Object

   Return the IX'th element.  If IX is outside the domain of V,
   abort.  */

#define VEC_index(TDEF,V,I)	(VEC_OP(TDEF,index)(V,I VEC_CHECK_INFO))

/* Iterate over vector
   int VEC_T_iterate(VEC(T) *v, unsigned ix, T &ptr); // Pointer
   int VEC_T_iterate(VEC(T) *v, unsigned ix, T *&ptr); // Object

   Return iteration condition and update PTR to point to the IX'th
   element.  At the end of iteration, sets PTR to NULL.  Use this to
   iterate over the elements of a vector as follows,

     for (ix = 0; VEC_iterate(T,v,ix,ptr); ix++)
       continue;  */

#define VEC_iterate(TDEF,V,I,P)	(VEC_OP(TDEF,iterate)(V,I,&(P)))

/* Allocate new vector.
   VEC(T) *VEC_T_alloc(int reserve);

   Allocate a new vector with space for RESERVE objects.  If RESERVE
   is <= 0, a default number of slots are created.  */

#define VEC_alloc(TDEF,A)	(VEC_OP(TDEF,alloc)(A MEM_STAT_INFO))

/* Free a vector.
   void VEC_T_alloc(VEC(T) *&);

   Free a vector and set it to NULL.  */

#define VEC_free(TDEF,V)	(VEC_OP(TDEF,free)(&V))

/* Use these to determine the required size and initialization of a
   vector embedded within another structure (as the final member).
   
   size_t VEC_T_embedded_size(int reserve);
   void VEC_T_embedded_init(VEC(T) *v, int reserve);
   
   These allow the caller to perform the memory allocation.  */

#define VEC_embedded_size(TDEF,A)	(VEC_OP(TDEF,embedded_size)(A))
#define VEC_embedded_init(TDEF,O,A)	(VEC_OP(TDEF,embedded_init)(O,A))

/* Determine if a vector has additional capacity.
   
   int VEC_T_space (VEC(T) *v,int reserve)

   If V has space for RESERVE additional entries, return nonzero.  If
   RESERVE is < 0, ensure there is at least one space slot.  You
   usually only need to use this if you are doing your own vector
   reallocation, for instance on an embedded vector.  This returns
   nonzero in exactly the same circumstances that VEC_T_reserve
   will.  */

#define VEC_space(TDEF,V,R)	(VEC_OP(TDEF,space)(V,R))

/* Reserve space.
   int VEC_T_reserve(VEC(T) *&v, int reserve);

   Ensure that V has at least RESERVE slots available, if RESERVE is
   >= 0.  If RESERVE < 0, ensure that there is at least one spare
   slot.  These differ in their reallocation behavior, the first will
   not create additional headroom, but the second mechanism will
   perform the usual exponential headroom increase.  Note this can
   cause V to be reallocated.  Returns nonzero iff reallocation
   actually occurred.  */

#define VEC_reserve(TDEF,V,R)	(VEC_OP(TDEF,reserve)(&(V),R MEM_STAT_INFO))

/* Push object with no reallocation
   T *VEC_T_quick_push (VEC(T) *v, T obj); // Pointer
   T *VEC_T_quick_push (VEC(T) *v, T *obj); // Object
   
   Push a new element onto the end, returns a pointer to the slot
   filled in. For object vectors, the new value can be NULL, in which
   case NO initialization is performed.  Aborts if there is
   insufficient space in the vector.  */

#define VEC_quick_push(TDEF,V,O)	\
	(VEC_OP(TDEF,quick_push)(V,O VEC_CHECK_INFO))

/* Push object with reallocation
   T *VEC_T_safe_push (VEC(T) *&v, T obj); // Pointer
   T *VEC_T_safe_push (VEC(T) *&v, T *obj); // Object
   
   Push a new element onto the end, returns a pointer to the slot
   filled in. For object vectors, the new value can be NULL, in which
   case NO initialization is performed.  Reallocates V, if needed.  */

#define VEC_safe_push(TDEF,V,O)		\
	(VEC_OP(TDEF,safe_push)(&(V),O VEC_CHECK_INFO MEM_STAT_INFO))

/* Pop element off end
   T VEC_T_pop (VEC(T) *v);		// Pointer
   void VEC_T_pop (VEC(T) *v);		// Object

   Pop the last element off the end. Returns the element popped, for
   pointer vectors.  */

#define VEC_pop(TDEF,V)			(VEC_OP(TDEF,pop)(V VEC_CHECK_INFO))

/* Truncate to specific length
   void VEC_T_truncate (VEC(T) *v, unsigned len);
   
   Set the length as specified.  This is an O(1) operation.  */

#define VEC_truncate(TDEF,V,I)		\
	(VEC_OP(TDEF,truncate)(V,I VEC_CHECK_INFO))

/* Replace element
   T VEC_T_replace (VEC(T) *v, unsigned ix, T val); // Pointer
   T *VEC_T_replace (VEC(T) *v, unsigned ix, T *val);  // Object
   
   Replace the IXth element of V with a new value, VAL.  For pointer
   vectors returns the original value. For object vectors returns a
   pointer to the new value.  For object vectors the new value can be
   NULL, in which case no overwriting of the slot is actually
   performed.  */

#define VEC_replace(TDEF,V,I,O)		\
	(VEC_OP(TDEF,replace)(V,I,O VEC_CHECK_INFO))

/* Insert object with no reallocation
   T *VEC_T_quick_insert (VEC(T) *v, unsigned ix, T val); // Pointer
   T *VEC_T_quick_insert (VEC(T) *v, unsigned ix, T *val); // Object
   
   Insert an element, VAL, at the IXth position of V. Return a pointer
   to the slot created.  For vectors of object, the new value can be
   NULL, in which case no initialization of the inserted slot takes
   place. Aborts if there is insufficient space.  */

#define VEC_quick_insert(TDEF,V,I,O)	\
	(VEC_OP(TDEF,quick_insert)(V,I,O VEC_CHECK_INFO))

/* Insert object with reallocation
   T *VEC_T_safe_insert (VEC(T) *&v, unsigned ix, T val); // Pointer
   T *VEC_T_safe_insert (VEC(T) *&v, unsigned ix, T *val); // Object
   
   Insert an element, VAL, at the IXth position of V. Return a pointer
   to the slot created.  For vectors of object, the new value can be
   NULL, in which case no initialization of the inserted slot takes
   place. Reallocate V, if necessary.  */

#define VEC_safe_insert(TDEF,V,I,O)	\
	(VEC_OP(TDEF,safe_insert)(&(V),I,O VEC_CHECK_INFO MEM_STAT_INFO))
     
/* Remove element retaining order
   T VEC_T_ordered_remove (VEC(T) *v, unsigned ix); // Pointer
   void VEC_T_ordered_remove (VEC(T) *v, unsigned ix); // Object
   
   Remove an element from the IXth position of V. Ordering of
   remaining elements is preserved.  For pointer vectors returns the
   removed object.  This is an O(N) operation due to a memmove.  */

#define VEC_ordered_remove(TDEF,V,I)	\
	(VEC_OP(TDEF,ordered_remove)(V,I VEC_CHECK_INFO))

/* Remove element destroying order
   T VEC_T_unordered_remove (VEC(T) *v, unsigned ix); // Pointer
   void VEC_T_unordered_remove (VEC(T) *v, unsigned ix); // Object
   
   Remove an element from the IXth position of V. Ordering of
   remaining elements is destroyed.  For pointer vectors returns the
   removed object.  This is an O(1) operation.  */

#define VEC_unordered_remove(TDEF,V,I)	\
	(VEC_OP(TDEF,unordered_remove)(V,I VEC_CHECK_INFO))

/* Get the address of the array of elements
   T *VEC_T_address (VEC(T) v)

   If you need to directly manipulate the array (for instance, you
   want to feed it to qsort), use this accessor.  */

#define VEC_address(TDEF,V)		(VEC_OP(TDEF,address)(V))

/* Find the first index in the vector not less than the object.
   unsigned VEC_T_lower_bound (VEC(T) *v, const T val, 
                               bool (*lessthan) (const T, const T)); // Pointer
   unsigned VEC_T_lower_bound (VEC(T) *v, const T *val,
                               bool (*lessthan) (const T*, const T*)); // Object
   
   Find the first position in which VAL could be inserted without
   changing the ordering of V.  LESSTHAN is a function that returns
   true if the first argument is strictly less than the second.  */
   
#define VEC_lower_bound(TDEF,V,O,LT)    \
       (VEC_OP(TDEF,lower_bound)(V,O,LT VEC_CHECK_INFO))

#if !IN_GENGTYPE
/* Reallocate an array of elements with prefix.  */
extern void *vec_gc_p_reserve (void *, int MEM_STAT_DECL);
extern void *vec_gc_o_reserve (void *, int, size_t, size_t MEM_STAT_DECL);
extern void vec_gc_free (void *);
extern void *vec_heap_p_reserve (void *, int MEM_STAT_DECL);
extern void *vec_heap_o_reserve (void *, int, size_t, size_t MEM_STAT_DECL);
extern void vec_heap_free (void *);

#if ENABLE_CHECKING
#define VEC_CHECK_INFO ,__FILE__,__LINE__,__FUNCTION__
#define VEC_CHECK_DECL ,const char *file_,unsigned line_,const char *function_
#define VEC_CHECK_PASS ,file_,line_,function_
     
#define VEC_ASSERT(EXPR,OP,TDEF) \
  (void)((EXPR) ? 0 : (VEC_ASSERT_FAIL(OP,VEC(TDEF)), 0))

extern void vec_assert_fail (const char *, const char * VEC_CHECK_DECL)
     ATTRIBUTE_NORETURN;
#define VEC_ASSERT_FAIL(OP,VEC) vec_assert_fail (OP,#VEC VEC_CHECK_PASS)
#else
#define VEC_CHECK_INFO
#define VEC_CHECK_DECL
#define VEC_CHECK_PASS
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
  unsigned num;								  \
  unsigned alloc;							  \
  TDEF GTY ((length ("%h.num"))) vec[1];				  \
} VEC (TDEF)

/* Vector of pointer to object.  */
#if IN_GENGTYPE
{"DEF_VEC_GC_P", VEC_STRINGIFY (VEC_TDEF (#)) ";", NULL},
{"DEF_VEC_MALLOC_P", "", NULL},
#else
#define DEF_VEC_GC_P(TDEF) DEF_VEC_P(TDEF,gc)
#define DEF_VEC_MALLOC_P(TDEF) DEF_VEC_P(TDEF,heap)
  
#define DEF_VEC_P(TDEF,a)						  \
VEC_TDEF (TDEF);							  \
									  \
static inline unsigned VEC_OP (TDEF,length)				  \
     (const VEC (TDEF) *vec_) 						  \
{									  \
  return vec_ ? vec_->num : 0;						  \
}									  \
									  \
static inline TDEF VEC_OP (TDEF,last)					  \
     (const VEC (TDEF) *vec_ VEC_CHECK_DECL)				  \
{									  \
  VEC_ASSERT (vec_ && vec_->num, "last", TDEF);				  \
  									  \
  return vec_->vec[vec_->num - 1];					  \
}									  \
									  \
static inline TDEF VEC_OP (TDEF,index)					  \
     (const VEC (TDEF) *vec_, unsigned ix_ VEC_CHECK_DECL)		  \
{									  \
  VEC_ASSERT (vec_ && ix_ < vec_->num, "index", TDEF);			  \
  									  \
  return vec_->vec[ix_];						  \
}									  \
									  \
static inline int VEC_OP (TDEF,iterate)			  	     	  \
     (const VEC (TDEF) *vec_, unsigned ix_, TDEF *ptr)			  \
{									  \
  if (vec_ && ix_ < vec_->num)						  \
    {									  \
      *ptr = vec_->vec[ix_];						  \
      return 1;								  \
    }									  \
  else									  \
    {									  \
      *ptr = 0;								  \
      return 0;								  \
    }									  \
}									  \
									  \
static inline VEC (TDEF) *VEC_OP (TDEF,alloc)				  \
     (int alloc_ MEM_STAT_DECL)						  \
{									  \
  return (VEC (TDEF) *) vec_##a##_p_reserve (NULL, alloc_ - !alloc_ PASS_MEM_STAT);\
}									  \
									  \
static inline void VEC_OP (TDEF,free)					  \
     (VEC (TDEF) **vec_)						  \
{									  \
  vec_##a##_free (*vec_);						  \
  *vec_ = NULL;								  \
}									  \
									  \
static inline size_t VEC_OP (TDEF,embedded_size)			  \
     (int alloc_)							  \
{									  \
  return offsetof (VEC(TDEF),vec) + alloc_ * sizeof(TDEF);		  \
}									  \
									  \
static inline void VEC_OP (TDEF,embedded_init)				  \
     (VEC (TDEF) *vec_, int alloc_)					  \
{									  \
  vec_->num = 0;							  \
  vec_->alloc = alloc_;							  \
}									  \
									  \
static inline int VEC_OP (TDEF,space)	       				  \
     (VEC (TDEF) *vec_, int alloc_)					  \
{									  \
  return vec_ ? ((vec_)->alloc - (vec_)->num				  \
		 >= (unsigned)(alloc_ < 0 ? 1 : alloc_)) : !alloc_;	  \
}									  \
									  \
static inline int VEC_OP (TDEF,reserve)	       				  \
     (VEC (TDEF) **vec_, int alloc_ MEM_STAT_DECL)			  \
{									  \
  int extend = !VEC_OP (TDEF,space) (*vec_, alloc_);			  \
		  							  \
  if (extend)	  							  \
    *vec_ = (VEC (TDEF) *) vec_##a##_p_reserve (*vec_, alloc_ PASS_MEM_STAT);   \
		  							  \
  return extend;							  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,quick_push)				  \
     (VEC (TDEF) *vec_, TDEF obj_ VEC_CHECK_DECL)			  \
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
     (VEC (TDEF) **vec_, TDEF obj_ VEC_CHECK_DECL MEM_STAT_DECL)       	  \
{									  \
  VEC_OP (TDEF,reserve) (vec_, -1 PASS_MEM_STAT);			  \
									  \
  return VEC_OP (TDEF,quick_push) (*vec_, obj_ VEC_CHECK_PASS);		  \
}									  \
									  \
static inline TDEF VEC_OP (TDEF,pop)					  \
     (VEC (TDEF) *vec_ VEC_CHECK_DECL)	    				  \
{									  \
  TDEF obj_;								  \
									  \
  VEC_ASSERT (vec_->num, "pop", TDEF);					  \
  obj_ = vec_->vec[--vec_->num];					  \
									  \
  return obj_;								  \
}									  \
									  \
static inline void VEC_OP (TDEF,truncate)				  \
     (VEC (TDEF) *vec_, unsigned size_ VEC_CHECK_DECL)			  \
{									  \
  VEC_ASSERT (vec_ ? vec_->num >= size_ : !size_, "truncate", TDEF);	  \
  if (vec_)								  \
    vec_->num = size_;							  \
}									  \
									  \
static inline TDEF VEC_OP (TDEF,replace)		  	     	  \
     (VEC (TDEF) *vec_, unsigned ix_, TDEF obj_ VEC_CHECK_DECL)		  \
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
static inline unsigned VEC_OP (TDEF,lower_bound)			\
     (VEC (TDEF) *vec_, const TDEF obj_, bool (*lessthan_)(const TDEF, const TDEF) VEC_CHECK_DECL) \
{									\
   unsigned int len_ = VEC_OP (TDEF, length) (vec_);			\
   unsigned int half_, middle_;						\
   unsigned int first_ = 0;						\
   while (len_ > 0)							\
     {									\
        TDEF middle_elem_;						\
        half_ = len_ >> 1;						\
        middle_ = first_;						\
        middle_ += half_;						\
        middle_elem_ = VEC_OP (TDEF, index) (vec_, middle_ VEC_CHECK_PASS); \
        if (lessthan_ (middle_elem_, obj_))				\
          {								\
             first_ = middle_;						\
             ++first_;							\
             len_ = len_ - half_ - 1;					\
          }								\
        else								\
          len_ = half_;							\
     }									\
   return first_;							\
}									\
									\
static inline TDEF *VEC_OP (TDEF,quick_insert)				\
     (VEC (TDEF) *vec_, unsigned ix_, TDEF obj_ VEC_CHECK_DECL)		  \
{									  \
  TDEF *slot_;								  \
									  \
  VEC_ASSERT (vec_->num < vec_->alloc, "insert", TDEF);			  \
  VEC_ASSERT (ix_ <= vec_->num, "insert", TDEF);			  \
  slot_ = &vec_->vec[ix_];						  \
  memmove (slot_ + 1, slot_, (vec_->num++ - ix_) * sizeof (TDEF));	  \
  *slot_ = obj_;							  \
  									  \
  return slot_;								  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,safe_insert)		     	  	  \
     (VEC (TDEF) **vec_, unsigned ix_, TDEF obj_ 			  \
	VEC_CHECK_DECL MEM_STAT_DECL)					  \
{									  \
  VEC_OP (TDEF,reserve) (vec_, -1 PASS_MEM_STAT);			  \
									  \
  return VEC_OP (TDEF,quick_insert) (*vec_, ix_, obj_ VEC_CHECK_PASS);	  \
}									  \
									  \
static inline TDEF VEC_OP (TDEF,ordered_remove)				  \
     (VEC (TDEF) *vec_, unsigned ix_ VEC_CHECK_DECL)			  \
{									  \
  TDEF *slot_;								  \
  TDEF obj_;								  \
									  \
  VEC_ASSERT (ix_ < vec_->num, "remove", TDEF);				  \
  slot_ = &vec_->vec[ix_];						  \
  obj_ = *slot_;							  \
  memmove (slot_, slot_ + 1, (--vec_->num - ix_) * sizeof (TDEF));     	  \
									  \
  return obj_;								  \
}									  \
									  \
static inline TDEF VEC_OP (TDEF,unordered_remove)			  \
     (VEC (TDEF) *vec_, unsigned ix_ VEC_CHECK_DECL)			  \
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
static inline TDEF *VEC_OP (TDEF,address)				  \
     (VEC (TDEF) *vec_)							  \
{									  \
  return vec_ ? vec_->vec : 0;						  \
}									  \
									  \
struct vec_swallow_trailing_semi
#endif

/* Vector of object.  */
#if IN_GENGTYPE
{"DEF_VEC_GC_O", VEC_STRINGIFY (VEC_TDEF (#)) ";", NULL},
{"DEF_VEC_MALLOC_O", "", NULL},
#else
  
#define DEF_VEC_GC_O(TDEF) DEF_VEC_O(TDEF,gc)
#define DEF_VEC_MALLOC_O(TDEF) DEF_VEC_O(TDEF,heap)

#define DEF_VEC_O(TDEF,a)						  \
VEC_TDEF (TDEF);							  \
									  \
static inline unsigned VEC_OP (TDEF,length)				  \
     (const VEC (TDEF) *vec_) 						  \
{									  \
  return vec_ ? vec_->num : 0;						  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,last)					  \
     (VEC (TDEF) *vec_ VEC_CHECK_DECL)					  \
{									  \
  VEC_ASSERT (vec_ && vec_->num, "last", TDEF);				  \
  									  \
  return &vec_->vec[vec_->num - 1];					  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,index)					  \
     (VEC (TDEF) *vec_, unsigned ix_ VEC_CHECK_DECL)			  \
{									  \
  VEC_ASSERT (vec_ && ix_ < vec_->num, "index", TDEF);			  \
  									  \
  return &vec_->vec[ix_];						  \
}									  \
									  \
static inline int VEC_OP (TDEF,iterate)			  	     	  \
     (VEC (TDEF) *vec_, unsigned ix_, TDEF **ptr)			  \
{									  \
  if (vec_ && ix_ < vec_->num)						  \
    {									  \
      *ptr = &vec_->vec[ix_];						  \
      return 1;								  \
    }									  \
  else									  \
    {									  \
      *ptr = 0;								  \
      return 0;								  \
    }									  \
}									  \
									  \
static inline VEC (TDEF) *VEC_OP (TDEF,alloc)      			  \
     (int alloc_ MEM_STAT_DECL)						  \
{									  \
  return (VEC (TDEF) *) vec_##a##_o_reserve (NULL, alloc_ - !alloc_,	  \
                                       offsetof (VEC(TDEF),vec), sizeof (TDEF)\
                                       PASS_MEM_STAT);			  \
}									  \
									  \
static inline void VEC_OP (TDEF,free)					  \
     (VEC (TDEF) **vec_)						  \
{									  \
  vec_##a##_free (*vec_);						  \
  *vec_ = NULL;								  \
}									  \
									  \
static inline size_t VEC_OP (TDEF,embedded_size)			  \
     (int alloc_)							  \
{									  \
  return offsetof (VEC(TDEF),vec) + alloc_ * sizeof(TDEF);		  \
}									  \
									  \
static inline void VEC_OP (TDEF,embedded_init)				  \
     (VEC (TDEF) *vec_, int alloc_)					  \
{									  \
  vec_->num = 0;							  \
  vec_->alloc = alloc_;							  \
}									  \
									  \
static inline int VEC_OP (TDEF,space)	       				  \
     (VEC (TDEF) *vec_, int alloc_)					  \
{									  \
  return vec_ ? ((vec_)->alloc - (vec_)->num				  \
		 >= (unsigned)(alloc_ < 0 ? 1 : alloc_)) : !alloc_;	  \
}									  \
									  \
static inline int VEC_OP (TDEF,reserve)	   	    			  \
     (VEC (TDEF) **vec_, int alloc_ MEM_STAT_DECL)			  \
{									  \
  int extend = !VEC_OP (TDEF,space) (*vec_, alloc_);			  \
									  \
  if (extend)								  \
    *vec_ = (VEC (TDEF) *) vec_##a##_o_reserve (*vec_, alloc_,		  \
			   offsetof (VEC(TDEF),vec), sizeof (TDEF)	  \
			   PASS_MEM_STAT);				  \
									  \
  return extend;							  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,quick_push)				  \
     (VEC (TDEF) *vec_, const TDEF *obj_ VEC_CHECK_DECL)		  \
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
     (VEC (TDEF) **vec_, const TDEF *obj_ VEC_CHECK_DECL MEM_STAT_DECL)   \
{									  \
  VEC_OP (TDEF,reserve) (vec_, -1 PASS_MEM_STAT);			  \
									  \
  return VEC_OP (TDEF,quick_push) (*vec_, obj_ VEC_CHECK_PASS);		  \
}									  \
									  \
static inline void VEC_OP (TDEF,pop)					  \
     (VEC (TDEF) *vec_ VEC_CHECK_DECL)					  \
{									  \
  VEC_ASSERT (vec_->num, "pop", TDEF);					  \
  --vec_->num;								  \
}									  \
									  \
static inline void VEC_OP (TDEF,truncate)				  \
     (VEC (TDEF) *vec_, unsigned size_ VEC_CHECK_DECL)			  \
{									  \
  VEC_ASSERT (vec_ ? vec_->num >= size_ : !size_, "truncate", TDEF);	  \
  if (vec_)								  \
    vec_->num = size_;							  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,replace)				  \
     (VEC (TDEF) *vec_, unsigned ix_, const TDEF *obj_ VEC_CHECK_DECL)	  \
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
static inline unsigned VEC_OP (TDEF,lower_bound)			\
     (VEC (TDEF) *vec_, const TDEF *obj_, bool (*lessthan_)(const TDEF *, const TDEF *) VEC_CHECK_DECL) \
{									\
   unsigned int len_ = VEC_OP (TDEF, length) (vec_);			\
   unsigned int half_, middle_;						\
   unsigned int first_ = 0;						\
   while (len_ > 0)							\
     {									\
        TDEF *middle_elem_;						\
        half_ = len_ >> 1;						\
        middle_ = first_;						\
        middle_ += half_;						\
        middle_elem_ = VEC_OP (TDEF, index) (vec_, middle_ VEC_CHECK_PASS); \
        if (lessthan_ (middle_elem_, obj_))				\
          {								\
             first_ = middle_;						\
             ++first_;							\
             len_ = len_ - half_ - 1;					\
          }								\
        else								\
          len_ = half_;							\
     }									\
   return first_;							\
}									\
									\
static inline TDEF *VEC_OP (TDEF,quick_insert)				\
     (VEC (TDEF) *vec_, unsigned ix_, const TDEF *obj_ VEC_CHECK_DECL)	\
{									  \
  TDEF *slot_;								  \
									  \
  VEC_ASSERT (vec_->num < vec_->alloc, "insert", TDEF);			  \
  VEC_ASSERT (ix_ <= vec_->num, "insert", TDEF);			  \
  slot_ = &vec_->vec[ix_];						  \
  memmove (slot_ + 1, slot_, (vec_->num++ - ix_) * sizeof (TDEF));	  \
  if (obj_)								  \
    *slot_ = *obj_;							  \
  									  \
  return slot_;								  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,safe_insert)		     	  	  \
     (VEC (TDEF) **vec_, unsigned ix_, const TDEF *obj_			  \
 		VEC_CHECK_DECL MEM_STAT_DECL)				  \
{									  \
  VEC_OP (TDEF,reserve) (vec_, -1 PASS_MEM_STAT);			  \
									  \
  return VEC_OP (TDEF,quick_insert) (*vec_, ix_, obj_ VEC_CHECK_PASS);	  \
}									  \
									  \
static inline void VEC_OP (TDEF,ordered_remove)				  \
     (VEC (TDEF) *vec_, unsigned ix_ VEC_CHECK_DECL)			  \
{									  \
  TDEF *slot_;								  \
									  \
  VEC_ASSERT (ix_ < vec_->num, "remove", TDEF);				  \
  slot_ = &vec_->vec[ix_];						  \
  memmove (slot_, slot_ + 1, (--vec_->num - ix_) * sizeof (TDEF));	  \
}									  \
									  \
static inline void VEC_OP (TDEF,unordered_remove)			  \
     (VEC (TDEF) *vec_, unsigned ix_ VEC_CHECK_DECL)			  \
{									  \
  VEC_ASSERT (ix_ < vec_->num, "remove", TDEF);				  \
  vec_->vec[ix_] = vec_->vec[--vec_->num];				  \
}									  \
									  \
static inline TDEF *VEC_OP (TDEF,address)				  \
     (VEC (TDEF) *vec_)							  \
{									  \
  return vec_ ? vec_->vec : 0;						  \
}									  \
									  \
struct vec_swallow_trailing_semi
#endif

#endif /* GCC_VEC_H */
