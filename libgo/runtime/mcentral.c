// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Central free lists.
//
// See malloc.h for an overview.
//
// The MCentral doesn't actually contain the list of free objects; the MSpan does.
// Each MCentral is two lists of MSpans: those with free objects (c->nonempty)
// and those that are completely allocated (c->empty).
//
// TODO(rsc): tcmalloc uses a "transfer cache" to split the list
// into sections of class_to_transfercount[sizeclass] objects
// so that it is faster to move those lists between MCaches and MCentrals.

#include "runtime.h"
#include "arch.h"
#include "malloc.h"

static bool MCentral_Grow(MCentral *c);
static void MCentral_Free(MCentral *c, void *v);

// Initialize a single central free list.
void
runtime_MCentral_Init(MCentral *c, int32 sizeclass)
{
	c->sizeclass = sizeclass;
	runtime_MSpanList_Init(&c->nonempty);
	runtime_MSpanList_Init(&c->empty);
}

// Allocate up to n objects from the central free list.
// Return the number of objects allocated.
// The objects are linked together by their first words.
// On return, *pstart points at the first object.
int32
runtime_MCentral_AllocList(MCentral *c, int32 n, MLink **pfirst)
{
	MSpan *s;
	MLink *first, *last;
	int32 cap, avail, i;

	runtime_lock(c);
	// Replenish central list if empty.
	if(runtime_MSpanList_IsEmpty(&c->nonempty)) {
		if(!MCentral_Grow(c)) {
			runtime_unlock(c);
			*pfirst = nil;
			return 0;
		}
	}
	s = c->nonempty.next;
	cap = (s->npages << PageShift) / s->elemsize;
	avail = cap - s->ref;
	if(avail < n)
		n = avail;

	// First one is guaranteed to work, because we just grew the list.
	first = s->freelist;
	last = first;
	for(i=1; i<n; i++) {
		last = last->next;
	}
	s->freelist = last->next;
	last->next = nil;
	s->ref += n;
	c->nfree -= n;

	if(n == avail) {
		if(s->freelist != nil || s->ref != (uint32)cap) {
			runtime_throw("invalid freelist");
		}
		runtime_MSpanList_Remove(s);
		runtime_MSpanList_Insert(&c->empty, s);
	}

	runtime_unlock(c);
	*pfirst = first;
	return n;
}

// Free n objects back into the central free list.
void
runtime_MCentral_FreeList(MCentral *c, int32 n, MLink *start)
{
	MLink *v, *next;

	// Assume next == nil marks end of list.
	// n and end would be useful if we implemented
	// the transfer cache optimization in the TODO above.
	USED(n);

	runtime_lock(c);
	for(v=start; v; v=next) {
		next = v->next;
		MCentral_Free(c, v);
	}
	runtime_unlock(c);
}

// Helper: free one object back into the central free list.
static void
MCentral_Free(MCentral *c, void *v)
{
	MSpan *s;
	MLink *p;
	int32 size;

	// Find span for v.
	s = runtime_MHeap_Lookup(&runtime_mheap, v);
	if(s == nil || s->ref == 0)
		runtime_throw("invalid free");

	// Move to nonempty if necessary.
	if(s->freelist == nil) {
		runtime_MSpanList_Remove(s);
		runtime_MSpanList_Insert(&c->nonempty, s);
	}

	// Add v back to s's free list.
	p = v;
	p->next = s->freelist;
	s->freelist = p;
	c->nfree++;

	// If s is completely freed, return it to the heap.
	if(--s->ref == 0) {
		size = runtime_class_to_size[c->sizeclass];
		runtime_MSpanList_Remove(s);
		runtime_unmarkspan((byte*)(s->start<<PageShift), s->npages<<PageShift);
		*(uintptr*)(s->start<<PageShift) = 1;  // needs zeroing
		s->freelist = nil;
		c->nfree -= (s->npages << PageShift) / size;
		runtime_unlock(c);
		runtime_MHeap_Free(&runtime_mheap, s, 0);
		runtime_lock(c);
	}
}

// Free n objects from a span s back into the central free list c.
// Called from GC.
void
runtime_MCentral_FreeSpan(MCentral *c, MSpan *s, int32 n, MLink *start, MLink *end)
{
	int32 size;

	runtime_lock(c);

	// Move to nonempty if necessary.
	if(s->freelist == nil) {
		runtime_MSpanList_Remove(s);
		runtime_MSpanList_Insert(&c->nonempty, s);
	}

	// Add the objects back to s's free list.
	end->next = s->freelist;
	s->freelist = start;
	s->ref -= n;
	c->nfree += n;

	// If s is completely freed, return it to the heap.
	if(s->ref == 0) {
		size = runtime_class_to_size[c->sizeclass];
		runtime_MSpanList_Remove(s);
		*(uintptr*)(s->start<<PageShift) = 1;  // needs zeroing
		s->freelist = nil;
		c->nfree -= (s->npages << PageShift) / size;
		runtime_unlock(c);
		runtime_unmarkspan((byte*)(s->start<<PageShift), s->npages<<PageShift);
		runtime_MHeap_Free(&runtime_mheap, s, 0);
	} else {
		runtime_unlock(c);
	}
}

void
runtime_MGetSizeClassInfo(int32 sizeclass, uintptr *sizep, int32 *npagesp, int32 *nobj)
{
	int32 size;
	int32 npages;

	npages = runtime_class_to_allocnpages[sizeclass];
	size = runtime_class_to_size[sizeclass];
	*npagesp = npages;
	*sizep = size;
	*nobj = (npages << PageShift) / size;
}

// Fetch a new span from the heap and
// carve into objects for the free list.
static bool
MCentral_Grow(MCentral *c)
{
	int32 i, n, npages;
	uintptr size;
	MLink **tailp, *v;
	byte *p;
	MSpan *s;

	runtime_unlock(c);
	runtime_MGetSizeClassInfo(c->sizeclass, &size, &npages, &n);
	s = runtime_MHeap_Alloc(&runtime_mheap, npages, c->sizeclass, 0, 1);
	if(s == nil) {
		// TODO(rsc): Log out of memory
		runtime_lock(c);
		return false;
	}

	// Carve span into sequence of blocks.
	tailp = &s->freelist;
	p = (byte*)(s->start << PageShift);
	s->limit = p + size*n;
	for(i=0; i<n; i++) {
		v = (MLink*)p;
		*tailp = v;
		tailp = &v->next;
		p += size;
	}
	*tailp = nil;
	runtime_markspan((byte*)(s->start<<PageShift), size, n, size*n < (s->npages<<PageShift));

	runtime_lock(c);
	c->nfree += n;
	runtime_MSpanList_Insert(&c->nonempty, s);
	return true;
}
