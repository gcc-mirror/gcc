/* Copyright (C) 2010-2013 Free Software Foundation, Inc.
   This file is part of the UPC runtime Library.
   Written by Gary Funck <gary@intrepid.com>
   and Nenad Vukicevic <nenad@intrepid.com>

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

#ifndef _UPC_LOCK_SUP_H_
#define _UPC_LOCK_SUP_H_

/* GUPC lock implementation support routines.  */

/* Lock reference atomic type definitions.  */
#define GUPCR_LOCK_ATOMIC_THREAD_TYPE unsigned long
#define GUPCR_LOCK_ATOMIC_VADDR_TYPE unsigned long long
#define GUPCR_LOCK_ATOMIC_TYPE unsigned long long
/* Store/Load of lock reference values must be atomic. On 32 bit
   use atomic functions.  */
#if __SIZEOF_POINTER__ == 4
#define GUPCR_ATOMIC_LOCK_REF_ACCESS 1
#else
#define GUPCR_ATOMIC_LOCK_REF_ACCESS 0
#endif
/* Lock reference value is a 64 bits value.  If bigger (struct
   implementation on 64 bit) it must be converted.  */
#if defined (GUPCR_PTS_STRUCT_REP) && __SIZEOF_POINTER__ == 8
#define GUPCR_CONVERT_LOCK_REF 1
#else
#define GUPCR_CONVERT_LOCK_REF 0
#endif

/* Lock link reference pointer.
   "shared" pointer with thread and offset only. Small enough that
   runtime can read/write it without the need to have the exclusive
   rights to the memory (at least for packed/64bits).  */
struct upc_lock_thread_struct
  {
    GUPCR_LOCK_ATOMIC_THREAD_TYPE thread:GUPCR_THREAD_SIZE;
    GUPCR_LOCK_ATOMIC_VADDR_TYPE addr:64-GUPCR_THREAD_SIZE;
  };
union upc_lock_link_ptr
  {
    struct upc_lock_thread_struct sptr;
    GUPCR_LOCK_ATOMIC_TYPE atomic;
  };
typedef union upc_lock_link_ptr upc_link_ref;
#define NULL_LOCK_REF(P) (P.atomic == 0)
#define SET_NULL_LOCK_REF(P) (P.atomic = 0)
#define LOCK_REF_THREAD(P) (P.sptr.thread)
#define SAME_LOCK_REF(P,V) (P.atomic == V.atomic)

typedef struct upc_lock_link_struct upc_lock_link_t;

/* upc_lock_t is an opaque shared type.  The 'upc_lock_struct'
   structure describes the internal representation of the
   UPC lock type.

   UPC lock implementation uses builtin atomic functions
   for swap/cswap of the UPC shared pointer.  */

struct upc_lock_struct
{
  upc_link_ref last;		/* Last thread on the waiting list.  */
  upc_link_ref owner_link;	/* Lock owner link block pointer.  */
  upc_lock_t *free_link;
} __attribute__ ((aligned(64)));

struct upc_lock_link_struct
{
  upc_link_ref next;		  /* Next thread on the waiting list.  */
  int signal;			  /* Notification of lock ownership.  */
  int free;			  /* Indication that link block is not used.  */
  upc_link_ref link_ref;	  /* Lock reference of this block.  */
  upc_lock_link_t *link;	  /* Free list link pointer.  */
} __attribute__ ((aligned(64)));

/* UPC shared point to C representation. */
typedef union pts_as_rep
  {
    shared void *pts;
    upc_shared_ptr_t rep;
  } pts_as_rep_t;

/* Convert pointer to shared into the link reference.  */
__attribute__((__always_inline__))
static inline
upc_link_ref
upc_to_link_ref (shared void *p)
{
#if GUPCR_CONVERT_LOCK_REF
  upc_link_ref ref;
  union pts_as_rep
    {
      shared void *s;
      upc_shared_ptr_t v;
    } pts = { .s = p };
  ref.sptr.thread = GUPCR_PTS_THREAD (pts.v);
  ref.sptr.addr = GUPCR_PTS_VADDR (pts.v);
  return ref;
#else
  union pts_as_rep
    {
      shared void *pts;
      upc_link_ref ref;
    } pts = { .pts = p };
  return pts.ref;
#endif
}

/* Convert link reference into a pointer to shared.  */
__attribute__((__always_inline__))
static inline
shared upc_lock_link_t *
upc_from_link_ref (upc_link_ref val)
{
#if GUPCR_CONVERT_LOCK_REF
  union pts_as_rep
    {
      shared upc_lock_link_t  *s;
      upc_shared_ptr_t v;
    } pts;
  GUPCR_PTS_SET_NULL_SHARED (pts.v);
  GUPCR_PTS_SET_VADDR (pts.v, (GUPCR_LOCK_ATOMIC_VADDR_TYPE)val.sptr.addr);
  GUPCR_PTS_SET_THREAD (pts.v, val.sptr.thread);
  return pts.s; 
#else
  union pts_as_rep
    {
      shared upc_lock_link_t  *pts;
      upc_link_ref ref;
    } pts = { .ref = val };
  return pts.pts;
#endif
}

__attribute__((__always_inline__))
static inline
void
upc_link_ref_swap (shared void *p, upc_link_ref *old, upc_link_ref val)
{
  upc_link_ref *addr = __upc_map_to_local (p);
  do
    {
      *old = *addr;
    } while (!__sync_bool_compare_and_swap ((GUPCR_LOCK_ATOMIC_TYPE *) addr,
		old->atomic, val.atomic));
}

__attribute__((__always_inline__))
static inline
int
upc_link_ref_cswap (shared void *p, upc_link_ref cmp, upc_link_ref val)
{
  upc_link_ref *addr = __upc_map_to_local (p);
  return __sync_bool_compare_and_swap ((GUPCR_LOCK_ATOMIC_TYPE *) addr,
		cmp.atomic, val.atomic);
}

__attribute__((__always_inline__))
static inline
void
upc_link_ref_put (shared upc_link_ref *p, upc_link_ref val)
{
  GUPCR_LOCK_ATOMIC_TYPE *addr;
  addr = __upc_map_to_local(p);
#if GUPCR_ATOMIC_LOCK_REF_ACCESS
  {
    GUPCR_LOCK_ATOMIC_TYPE tmp;
    do
      {
        tmp = *addr;
      } while (!__sync_bool_compare_and_swap (addr, tmp, val.atomic));
  }
#else
  *addr = val.atomic;
#endif
}

__attribute__((__always_inline__))
static inline
upc_link_ref
upc_link_ref_get (upc_link_ref *p)
{
#if GUPCR_ATOMIC_LOCK_REF_ACCESS
  GUPCR_LOCK_ATOMIC_TYPE *addr = (GUPCR_LOCK_ATOMIC_TYPE *) p;;
  GUPCR_LOCK_ATOMIC_TYPE tmp;
  do
    {
      tmp = * (GUPCR_LOCK_ATOMIC_TYPE *) addr;
    } while (!__sync_bool_compare_and_swap (
	     (GUPCR_LOCK_ATOMIC_TYPE *) addr, tmp, tmp));
  return (upc_link_ref) tmp;
#else
  return *p;
#endif
}

__attribute__((__always_inline__))
static inline
upc_link_ref
upc_link_ref_last (shared void *p)
{
  upc_link_ref *addr = __upc_map_to_local (p);
  return upc_link_ref_get (addr);
}

#endif /* !_UPC_LOCK_SUP_H_ */
