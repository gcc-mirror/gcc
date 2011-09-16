// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Garbage collector.

#include "runtime.h"
#include "malloc.h"

enum {
	Debug = 0,
	UseCas = 1,
	PtrSize = sizeof(void*),
	
	// Four bits per word (see #defines below).
	wordsPerBitmapWord = sizeof(void*)*8/4,
	bitShift = sizeof(void*)*8/4,
};

// Bits in per-word bitmap.
// #defines because enum might not be able to hold the values.
//
// Each word in the bitmap describes wordsPerBitmapWord words
// of heap memory.  There are 4 bitmap bits dedicated to each heap word,
// so on a 64-bit system there is one bitmap word per 16 heap words.
// The bits in the word are packed together by type first, then by
// heap location, so each 64-bit bitmap word consists of, from top to bottom,
// the 16 bitSpecial bits for the corresponding heap words, then the 16 bitMarked bits,
// then the 16 bitNoPointers/bitBlockBoundary bits, then the 16 bitAllocated bits.
// This layout makes it easier to iterate over the bits of a given type.
//
// The bitmap starts at mheap.arena_start and extends *backward* from
// there.  On a 64-bit system the off'th word in the arena is tracked by
// the off/16+1'th word before mheap.arena_start.  (On a 32-bit system,
// the only difference is that the divisor is 8.)
//
// To pull out the bits corresponding to a given pointer p, we use:
//
//	off = p - (uintptr*)mheap.arena_start;  // word offset
//	b = (uintptr*)mheap.arena_start - off/wordsPerBitmapWord - 1;
//	shift = off % wordsPerBitmapWord
//	bits = *b >> shift;
//	/* then test bits & bitAllocated, bits & bitMarked, etc. */
//
#define bitAllocated		((uintptr)1<<(bitShift*0))
#define bitNoPointers		((uintptr)1<<(bitShift*1))	/* when bitAllocated is set */
#define bitMarked		((uintptr)1<<(bitShift*2))	/* when bitAllocated is set */
#define bitSpecial		((uintptr)1<<(bitShift*3))	/* when bitAllocated is set - has finalizer or being profiled */
#define bitBlockBoundary	((uintptr)1<<(bitShift*1))	/* when bitAllocated is NOT set */

#define bitMask (bitBlockBoundary | bitAllocated | bitMarked | bitSpecial)

static uint64 nlookup;
static uint64 nsizelookup;
static uint64 naddrlookup;
static int32 gctrace;

typedef struct Workbuf Workbuf;
struct Workbuf
{
	Workbuf *next;
	uintptr nw;
	byte *w[2048-2];
};

static bool finstarted;
static pthread_mutex_t finqlock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t finqcond = PTHREAD_COND_INITIALIZER;
static Finalizer *finq;
static int32 fingwait;

static void runfinq(void*);
static Workbuf* getempty(Workbuf*);
static Workbuf* getfull(Workbuf*);

// scanblock scans a block of n bytes starting at pointer b for references
// to other objects, scanning any it finds recursively until there are no
// unscanned objects left.  Instead of using an explicit recursion, it keeps
// a work list in the Workbuf* structures and loops in the main function
// body.  Keeping an explicit work list is easier on the stack allocator and
// more efficient.
static void
scanblock(byte *b, int64 n)
{
	byte *obj, *arena_start, *p;
	void **vp;
	uintptr size, *bitp, bits, shift, i, j, x, xbits, off;
	MSpan *s;
	PageID k;
	void **bw, **w, **ew;
	Workbuf *wbuf;

	if((int64)(uintptr)n != n || n < 0) {
		// runtime_printf("scanblock %p %lld\n", b, (long long)n);
		runtime_throw("scanblock");
	}

	// Memory arena parameters.
	arena_start = runtime_mheap.arena_start;
	
	wbuf = nil;  // current work buffer
	ew = nil;  // end of work buffer
	bw = nil;  // beginning of work buffer
	w = nil;  // current pointer into work buffer

	// Align b to a word boundary.
	off = (uintptr)b & (PtrSize-1);
	if(off != 0) {
		b += PtrSize - off;
		n -= PtrSize - off;
	}

	for(;;) {
		// Each iteration scans the block b of length n, queueing pointers in
		// the work buffer.
		if(Debug > 1)
			runtime_printf("scanblock %p %lld\n", b, (long long) n);

		vp = (void**)b;
		n /= PtrSize;
		for(i=0; i<(uintptr)n; i++) {
			obj = (byte*)vp[i];
			
			// Words outside the arena cannot be pointers.
			if((byte*)obj < arena_start || (byte*)obj >= runtime_mheap.arena_used)
				continue;
			
			// obj may be a pointer to a live object.
			// Try to find the beginning of the object.
			
			// Round down to word boundary.
			obj = (void*)((uintptr)obj & ~((uintptr)PtrSize-1));

			// Find bits for this word.
			off = (uintptr*)obj - (uintptr*)arena_start;
			bitp = (uintptr*)arena_start - off/wordsPerBitmapWord - 1;
			shift = off % wordsPerBitmapWord;
			xbits = *bitp;
			bits = xbits >> shift;

			// Pointing at the beginning of a block?
			if((bits & (bitAllocated|bitBlockBoundary)) != 0)
				goto found;

			// Pointing just past the beginning?
			// Scan backward a little to find a block boundary.
			for(j=shift; j-->0; ) {
				if(((xbits>>j) & (bitAllocated|bitBlockBoundary)) != 0) {
					obj = (byte*)obj - (shift-j)*PtrSize;
					shift = j;
					bits = xbits>>shift;
					goto found;
				}
			}

			// Otherwise consult span table to find beginning.
			// (Manually inlined copy of MHeap_LookupMaybe.)
			nlookup++;
			naddrlookup++;
			k = (uintptr)obj>>PageShift;
			x = k;
			if(sizeof(void*) == 8)
				x -= (uintptr)arena_start>>PageShift;
			s = runtime_mheap.map[x];
			if(s == nil || k < s->start || k - s->start >= s->npages || s->state != MSpanInUse)
				continue;
			p =  (byte*)((uintptr)s->start<<PageShift);
			if(s->sizeclass == 0) {
				obj = p;
			} else {
				if((byte*)obj >= (byte*)s->limit)
					continue;
				size = runtime_class_to_size[s->sizeclass];
				int32 i = ((byte*)obj - p)/size;
				obj = p+i*size;
			}

			// Now that we know the object header, reload bits.
			off = (uintptr*)obj - (uintptr*)arena_start;
			bitp = (uintptr*)arena_start - off/wordsPerBitmapWord - 1;
			shift = off % wordsPerBitmapWord;
			xbits = *bitp;
			bits = xbits >> shift;

		found:
			// Now we have bits, bitp, and shift correct for
			// obj pointing at the base of the object.
			// If not allocated or already marked, done.
			if((bits & bitAllocated) == 0 || (bits & bitMarked) != 0)
				continue;
			*bitp |= bitMarked<<shift;

			// If object has no pointers, don't need to scan further.
			if((bits & bitNoPointers) != 0)
				continue;

			// If buffer is full, get a new one.
			if(w >= ew) {
				wbuf = getempty(wbuf);
				bw = (void**)wbuf->w;
				w = bw;
				ew = bw + nelem(wbuf->w);
			}
			*w++ = obj;
		}
		
		// Done scanning [b, b+n).  Prepare for the next iteration of
		// the loop by setting b and n to the parameters for the next block.

		// Fetch b from the work buffers.
		if(w <= bw) {
			// Emptied our buffer: refill.
			wbuf = getfull(wbuf);
			if(wbuf == nil)
				break;
			bw = (void**)wbuf->w;
			ew = (void**)(wbuf->w + nelem(wbuf->w));
			w = bw+wbuf->nw;
		}
		b = *--w;
	
		// Figure out n = size of b.  Start by loading bits for b.
		off = (uintptr*)b - (uintptr*)arena_start;
		bitp = (uintptr*)arena_start - off/wordsPerBitmapWord - 1;
		shift = off % wordsPerBitmapWord;
		xbits = *bitp;
		bits = xbits >> shift;
		
		// Might be small; look for nearby block boundary.
		// A block boundary is marked by either bitBlockBoundary
		// or bitAllocated being set (see notes near their definition).
		enum {
			boundary = bitBlockBoundary|bitAllocated
		};
		// Look for a block boundary both after and before b
		// in the same bitmap word.
		//
		// A block boundary j words after b is indicated by
		//	bits>>j & boundary
		// assuming shift+j < bitShift.  (If shift+j >= bitShift then
		// we'll be bleeding other bit types like bitMarked into our test.)
		// Instead of inserting the conditional shift+j < bitShift into the loop,
		// we can let j range from 1 to bitShift as long as we first
		// apply a mask to keep only the bits corresponding
		// to shift+j < bitShift aka j < bitShift-shift.
		bits &= (boundary<<(bitShift-shift)) - boundary;
		
		// A block boundary j words before b is indicated by
		//	xbits>>(shift-j) & boundary
		// (assuming shift >= j).  There is no cleverness here
		// avoid the test, because when j gets too large the shift
		// turns negative, which is undefined in C.		

		for(j=1; j<bitShift; j++) {
			if(((bits>>j)&boundary) != 0 || (shift>=j && ((xbits>>(shift-j))&boundary) != 0)) {
				n = j*PtrSize;
				goto scan;
			}
		}
		
		// Fall back to asking span about size class.
		// (Manually inlined copy of MHeap_Lookup.)
		nlookup++;
		nsizelookup++;
		x = (uintptr)b>>PageShift;
		if(sizeof(void*) == 8)
			x -= (uintptr)arena_start>>PageShift;
		s = runtime_mheap.map[x];
		if(s->sizeclass == 0)
			n = s->npages<<PageShift;
		else
			n = runtime_class_to_size[s->sizeclass];
	scan:;
	}
}

static struct {
	Workbuf	*full;
	Workbuf	*empty;
	byte	*chunk;
	uintptr	nchunk;
} work;

// Get an empty work buffer off the work.empty list,
// allocating new buffers as needed.
static Workbuf*
getempty(Workbuf *b)
{
	if(b != nil) {
		b->nw = nelem(b->w);
		b->next = work.full;
		work.full = b;
	}
	b = work.empty;
	if(b != nil) {
		work.empty = b->next;
		return b;
	}
	
	if(work.nchunk < sizeof *b) {
		work.nchunk = 1<<20;
		work.chunk = runtime_SysAlloc(work.nchunk);
	}
	b = (Workbuf*)work.chunk;
	work.chunk += sizeof *b;
	work.nchunk -= sizeof *b;
	return b;
}

// Get a full work buffer off the work.full list, or return nil.
static Workbuf*
getfull(Workbuf *b)
{
	if(b != nil) {
		b->nw = 0;
		b->next = work.empty;
		work.empty = b;
	}
	b = work.full;
	if(b != nil)
		work.full = b->next;
	return b;
}

// Scanstack calls scanblock on each of gp's stack segments.
static void
markfin(void *v)
{
	uintptr size;

	size = 0;
	if(!runtime_mlookup(v, (byte**)&v, &size, nil) || !runtime_blockspecial(v))
		runtime_throw("mark - finalizer inconsistency");

	// do not mark the finalizer block itself.  just mark the things it points at.
	scanblock(v, size);
}

struct root_list {
	struct root_list *next;
	struct root {
		void *decl;
		size_t size;
	} roots[];
};

static struct root_list* roots;

void
__go_register_gc_roots (struct root_list* r)
{
	// FIXME: This needs locking if multiple goroutines can call
	// dlopen simultaneously.
	r->next = roots;
	roots = r;
}

// Mark 
static void
mark(void)
{
	struct root_list *pl;

	for(pl = roots; pl != nil; pl = pl->next) {
		struct root* pr = &pl->roots[0];
		while(1) {
			void *decl = pr->decl;
			if(decl == nil)
				break;
			scanblock(decl, pr->size);
			pr++;
		}
	}

	scanblock((byte*)&m0, sizeof m0);
	scanblock((byte*)&finq, sizeof finq);
	runtime_MProf_Mark(scanblock);

	// mark stacks
	__go_scanstacks(scanblock);

	// mark things pointed at by objects with finalizers
	runtime_walkfintab(markfin, scanblock);
}

// Sweep frees or calls finalizers for blocks not marked in the mark phase.
// It clears the mark bits in preparation for the next GC round.
static void
sweep(void)
{
	MSpan *s;
	int32 cl, n, npages;
	uintptr size;
	byte *p;
	MCache *c;
	Finalizer *f;

	for(s = runtime_mheap.allspans; s != nil; s = s->allnext) {
		if(s->state != MSpanInUse)
			continue;

		p = (byte*)(s->start << PageShift);
		cl = s->sizeclass;
		if(cl == 0) {
			size = s->npages<<PageShift;
			n = 1;
		} else {
			// Chunk full of small blocks.
			size = runtime_class_to_size[cl];
			npages = runtime_class_to_allocnpages[cl];
			n = (npages << PageShift) / size;
		}
	
		// sweep through n objects of given size starting at p.
		for(; n > 0; n--, p += size) {
			uintptr off, *bitp, shift, bits;

			off = (uintptr*)p - (uintptr*)runtime_mheap.arena_start;
			bitp = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
			shift = off % wordsPerBitmapWord;
			bits = *bitp>>shift;

			if((bits & bitAllocated) == 0)
				continue;

			if((bits & bitMarked) != 0) {
				*bitp &= ~(bitMarked<<shift);
				continue;
			}

			if((bits & bitSpecial) != 0) {
				// Special means it has a finalizer or is being profiled.
				f = runtime_getfinalizer(p, 1);
				if(f != nil) {
					f->arg = p;
					f->next = finq;
					finq = f;
					continue;
				}
				runtime_MProf_Free(p, size);
			}

			// Mark freed; restore block boundary bit.
			*bitp = (*bitp & ~(bitMask<<shift)) | (bitBlockBoundary<<shift);

			c = m->mcache;
			if(s->sizeclass == 0) {
				// Free large span.
				runtime_unmarkspan(p, 1<<PageShift);
				*(uintptr*)p = 1;	// needs zeroing
				runtime_MHeap_Free(&runtime_mheap, s, 1);
			} else {
				// Free small object.
				if(size > sizeof(uintptr))
					((uintptr*)p)[1] = 1;	// mark as "needs to be zeroed"
				c->local_by_size[s->sizeclass].nfree++;
				runtime_MCache_Free(c, p, s->sizeclass, size);
			}
			c->local_alloc -= size;
			c->local_nfree++;
		}
	}
}

static pthread_mutex_t gcsema = PTHREAD_MUTEX_INITIALIZER;

// Initialized from $GOGC.  GOGC=off means no gc.
//
// Next gc is after we've allocated an extra amount of
// memory proportional to the amount already in use.
// If gcpercent=100 and we're using 4M, we'll gc again
// when we get to 8M.  This keeps the gc cost in linear
// proportion to the allocation cost.  Adjusting gcpercent
// just changes the linear constant (and also the amount of
// extra memory used).
static int32 gcpercent = -2;

void
runtime_gc(int32 force __attribute__ ((unused)))
{
	int64 t0, t1, t2, t3;
	uint64 heap0, heap1, obj0, obj1;
	char *p;
	Finalizer *fp;

	// The gc is turned off (via enablegc) until
	// the bootstrap has completed.
	// Also, malloc gets called in the guts
	// of a number of libraries that might be
	// holding locks.  To avoid priority inversion
	// problems, don't bother trying to run gc
	// while holding a lock.  The next mallocgc
	// without a lock will do the gc instead.
	if(!mstats.enablegc || m->locks > 0 /* || runtime_panicking */)
		return;

	if(gcpercent == -2) {	// first time through
		p = runtime_getenv("GOGC");
		if(p == nil || p[0] == '\0')
			gcpercent = 100;
		else if(runtime_strcmp(p, "off") == 0)
			gcpercent = -1;
		else
			gcpercent = runtime_atoi(p);
		
		p = runtime_getenv("GOGCTRACE");
		if(p != nil)
			gctrace = runtime_atoi(p);
	}
	if(gcpercent < 0)
		return;

	pthread_mutex_lock(&finqlock);
	pthread_mutex_lock(&gcsema);
	if(!force && mstats.heap_alloc < mstats.next_gc) {
		pthread_mutex_unlock(&gcsema);
		pthread_mutex_unlock(&finqlock);
		return;
	}

	t0 = runtime_nanotime();
	nlookup = 0;
	nsizelookup = 0;
	naddrlookup = 0;

	m->gcing = 1;
	runtime_stoptheworld();
	if(runtime_mheap.Lock.key != 0)
		runtime_throw("runtime_mheap locked during gc");

	__go_cachestats();
	heap0 = mstats.heap_alloc;
	obj0 = mstats.nmalloc - mstats.nfree;

	mark();
	t1 = runtime_nanotime();
	sweep();
	t2 = runtime_nanotime();
	__go_stealcache();
	__go_cachestats();

	mstats.next_gc = mstats.heap_alloc+mstats.heap_alloc*gcpercent/100;
	m->gcing = 0;

	m->locks++;	// disable gc during the mallocs in newproc

	heap1 = mstats.heap_alloc;
	obj1 = mstats.nmalloc - mstats.nfree;

	t3 = runtime_nanotime();
	mstats.pause_ns[mstats.numgc%nelem(mstats.pause_ns)] = t3 - t0;
	mstats.pause_total_ns += t3 - t0;
	mstats.numgc++;
	if(mstats.debuggc)
		runtime_printf("pause %llu\n", (unsigned long long)t3-t0);
	
	if(gctrace) {
		runtime_printf("gc%d: %llu+%llu+%llu ms %llu -> %llu MB %llu -> %llu (%llu-%llu) objects %llu pointer lookups (%llu size, %llu addr)\n",
			mstats.numgc, (unsigned long long)(t1-t0)/1000000, (unsigned long long)(t2-t1)/1000000, (unsigned long long)(t3-t2)/1000000,
			(unsigned long long)heap0>>20, (unsigned long long)heap1>>20, (unsigned long long)obj0, (unsigned long long)obj1,
			(unsigned long long)mstats.nmalloc, (unsigned long long)mstats.nfree,
			(unsigned long long)nlookup, (unsigned long long)nsizelookup, (unsigned long long)naddrlookup);
	}

	pthread_mutex_unlock(&gcsema);
	runtime_starttheworld();

	// finqlock is still held.
	fp = finq;
	if(fp != nil) {
		// kick off or wake up goroutine to run queued finalizers
		if(!finstarted) {
			__go_go(runfinq, nil);
			finstarted = 1;
		}
		else if(fingwait) {
			fingwait = 0;
			pthread_cond_signal(&finqcond);
		}
	}
	m->locks--;
	pthread_mutex_unlock(&finqlock);

	if(gctrace > 1 && !force)
		runtime_gc(1);
}

void runtime_UpdateMemStats(void)
  __asm__("libgo_runtime.runtime.UpdateMemStats");

void
runtime_UpdateMemStats(void)
{
	// Have to acquire gcsema to stop the world,
	// because stoptheworld can only be used by
	// one goroutine at a time, and there might be
	// a pending garbage collection already calling it.
	pthread_mutex_lock(&gcsema);
	m->gcing = 1;
	runtime_stoptheworld();
	__go_cachestats();
	m->gcing = 0;
	pthread_mutex_unlock(&gcsema);
	runtime_starttheworld();
}

static void
runfinq(void* dummy)
{
	Finalizer *f, *next;

	USED(dummy);

	for(;;) {
		pthread_mutex_lock(&finqlock);
		f = finq;
		finq = nil;
		if(f == nil) {
			fingwait = 1;
			pthread_cond_wait(&finqcond, &finqlock);
			pthread_mutex_unlock(&finqlock);
			continue;
		}
		pthread_mutex_unlock(&finqlock);
		for(; f; f=next) {
			void *params[1];

			next = f->next;
			params[0] = &f->arg;
			reflect_call(f->ft, (void*)f->fn, 0, 0, params, nil);
			f->fn = nil;
			f->arg = nil;
			f->next = nil;
			runtime_free(f);
		}
		runtime_gc(1);	// trigger another gc to clean up the finalized objects, if possible
	}
}

#define runtime_singleproc 0

// mark the block at v of size n as allocated.
// If noptr is true, mark it as having no pointers.
void
runtime_markallocated(void *v, uintptr n, bool noptr)
{
	uintptr *b, obits, bits, off, shift;

	// if(0)
		// runtime_printf("markallocated %p+%p\n", v, n);

	if((byte*)v+n > (byte*)runtime_mheap.arena_used || (byte*)v < runtime_mheap.arena_start)
		runtime_throw("markallocated: bad pointer");

	off = (uintptr*)v - (uintptr*)runtime_mheap.arena_start;  // word offset
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;

	for(;;) {
		obits = *b;
		bits = (obits & ~(bitMask<<shift)) | (bitAllocated<<shift);
		if(noptr)
			bits |= bitNoPointers<<shift;
		if(runtime_singleproc) {
			*b = bits;
			break;
		} else {
			// more than one goroutine is potentially running: use atomic op
			if(runtime_casp((void**)b, (void*)obits, (void*)bits))
				break;
		}
	}
}

// mark the block at v of size n as freed.
void
runtime_markfreed(void *v, uintptr n)
{
	uintptr *b, obits, bits, off, shift;

	// if(0)
		// runtime_printf("markallocated %p+%p\n", v, n);

	if((byte*)v+n > (byte*)runtime_mheap.arena_used || (byte*)v < runtime_mheap.arena_start)
		runtime_throw("markallocated: bad pointer");

	off = (uintptr*)v - (uintptr*)runtime_mheap.arena_start;  // word offset
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;

	for(;;) {
		obits = *b;
		bits = (obits & ~(bitMask<<shift)) | (bitBlockBoundary<<shift);
		if(runtime_singleproc) {
			*b = bits;
			break;
		} else {
			// more than one goroutine is potentially running: use atomic op
			if(runtime_casp((void**)b, (void*)obits, (void*)bits))
				break;
		}
	}
}

// check that the block at v of size n is marked freed.
void
runtime_checkfreed(void *v, uintptr n)
{
	uintptr *b, bits, off, shift;

	if(!runtime_checking)
		return;

	if((byte*)v+n > (byte*)runtime_mheap.arena_used || (byte*)v < runtime_mheap.arena_start)
		return;	// not allocated, so okay

	off = (uintptr*)v - (uintptr*)runtime_mheap.arena_start;  // word offset
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;

	bits = *b>>shift;
	if((bits & bitAllocated) != 0) {
		runtime_printf("checkfreed %p+%p: off=%p have=%p\n",
			v, (void*)n, (void*)off, (void*)(bits & bitMask));
		runtime_throw("checkfreed: not freed");
	}
}

// mark the span of memory at v as having n blocks of the given size.
// if leftover is true, there is left over space at the end of the span.
void
runtime_markspan(void *v, uintptr size, uintptr n, bool leftover)
{
	uintptr *b, off, shift;
	byte *p;

	if((byte*)v+size*n > (byte*)runtime_mheap.arena_used || (byte*)v < runtime_mheap.arena_start)
		runtime_throw("markspan: bad pointer");

	p = v;
	if(leftover)	// mark a boundary just past end of last block too
		n++;
	for(; n-- > 0; p += size) {
		// Okay to use non-atomic ops here, because we control
		// the entire span, and each bitmap word has bits for only
		// one span, so no other goroutines are changing these
		// bitmap words.
		off = (uintptr*)p - (uintptr*)runtime_mheap.arena_start;  // word offset
		b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
		shift = off % wordsPerBitmapWord;
		*b = (*b & ~(bitMask<<shift)) | (bitBlockBoundary<<shift);
	}
}

// unmark the span of memory at v of length n bytes.
void
runtime_unmarkspan(void *v, uintptr n)
{
	uintptr *p, *b, off;

	if((byte*)v+n > (byte*)runtime_mheap.arena_used || (byte*)v < runtime_mheap.arena_start)
		runtime_throw("markspan: bad pointer");

	p = v;
	off = p - (uintptr*)runtime_mheap.arena_start;  // word offset
	if(off % wordsPerBitmapWord != 0)
		runtime_throw("markspan: unaligned pointer");
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	n /= PtrSize;
	if(n%wordsPerBitmapWord != 0)
		runtime_throw("unmarkspan: unaligned length");
	// Okay to use non-atomic ops here, because we control
	// the entire span, and each bitmap word has bits for only
	// one span, so no other goroutines are changing these
	// bitmap words.
	n /= wordsPerBitmapWord;
	while(n-- > 0)
		*b-- = 0;
}

bool
runtime_blockspecial(void *v)
{
	uintptr *b, off, shift;

	off = (uintptr*)v - (uintptr*)runtime_mheap.arena_start;
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;

	return (*b & (bitSpecial<<shift)) != 0;
}

void
runtime_setblockspecial(void *v)
{
	uintptr *b, off, shift, bits, obits;

	off = (uintptr*)v - (uintptr*)runtime_mheap.arena_start;
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;

	for(;;) {
		obits = *b;
		bits = obits | (bitSpecial<<shift);
		if(runtime_singleproc) {
			*b = bits;
			break;
		} else {
			// more than one goroutine is potentially running: use atomic op
			if(runtime_casp((void**)b, (void*)obits, (void*)bits))
				break;
		}
	}
}
 
void
runtime_MHeap_MapBits(MHeap *h)
{
	// Caller has added extra mappings to the arena.
	// Add extra mappings of bitmap words as needed.
	// We allocate extra bitmap pieces in chunks of bitmapChunk.
	enum {
		bitmapChunk = 8192
	};
	uintptr n;
	
	n = (h->arena_used - h->arena_start) / wordsPerBitmapWord;
	n = (n+bitmapChunk-1) & ~(bitmapChunk-1);
	if(h->bitmap_mapped >= n)
		return;

	runtime_SysMap(h->arena_start - n, n - h->bitmap_mapped);
	h->bitmap_mapped = n;
}

void
__go_enable_gc()
{
  mstats.enablegc = 1;
}
