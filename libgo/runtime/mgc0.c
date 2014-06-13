// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Garbage collector (GC).
//
// GC is:
// - mark&sweep
// - mostly precise (with the exception of some C-allocated objects, assembly frames/arguments, etc)
// - parallel (up to MaxGcproc threads)
// - partially concurrent (mark is stop-the-world, while sweep is concurrent)
// - non-moving/non-compacting
// - full (non-partial)
//
// GC rate.
// Next GC is after we've allocated an extra amount of memory proportional to
// the amount already in use. The proportion is controlled by GOGC environment variable
// (100 by default). If GOGC=100 and we're using 4M, we'll GC again when we get to 8M
// (this mark is tracked in next_gc variable). This keeps the GC cost in linear
// proportion to the allocation cost. Adjusting GOGC just changes the linear constant
// (and also the amount of extra memory used).
//
// Concurrent sweep.
// The sweep phase proceeds concurrently with normal program execution.
// The heap is swept span-by-span both lazily (when a goroutine needs another span)
// and concurrently in a background goroutine (this helps programs that are not CPU bound).
// However, at the end of the stop-the-world GC phase we don't know the size of the live heap,
// and so next_gc calculation is tricky and happens as follows.
// At the end of the stop-the-world phase next_gc is conservatively set based on total
// heap size; all spans are marked as "needs sweeping".
// Whenever a span is swept, next_gc is decremented by GOGC*newly_freed_memory.
// The background sweeper goroutine simply sweeps spans one-by-one bringing next_gc
// closer to the target value. However, this is not enough to avoid over-allocating memory.
// Consider that a goroutine wants to allocate a new span for a large object and
// there are no free swept spans, but there are small-object unswept spans.
// If the goroutine naively allocates a new span, it can surpass the yet-unknown
// target next_gc value. In order to prevent such cases (1) when a goroutine needs
// to allocate a new small-object span, it sweeps small-object spans for the same
// object size until it frees at least one object; (2) when a goroutine needs to
// allocate large-object span from heap, it sweeps spans until it frees at least
// that many pages into heap. Together these two measures ensure that we don't surpass
// target next_gc value by a large margin. There is an exception: if a goroutine sweeps
// and frees two nonadjacent one-page spans to the heap, it will allocate a new two-page span,
// but there can still be other one-page unswept spans which could be combined into a two-page span.
// It's critical to ensure that no operations proceed on unswept spans (that would corrupt
// mark bits in GC bitmap). During GC all mcaches are flushed into the central cache,
// so they are empty. When a goroutine grabs a new span into mcache, it sweeps it.
// When a goroutine explicitly frees an object or sets a finalizer, it ensures that
// the span is swept (either by sweeping it, or by waiting for the concurrent sweep to finish).
// The finalizer goroutine is kicked off only when all spans are swept.
// When the next GC starts, it sweeps all not-yet-swept spans (if any).

#include <unistd.h>

#include "runtime.h"
#include "arch.h"
#include "malloc.h"
#include "mgc0.h"
#include "race.h"
#include "go-type.h"

// Map gccgo field names to gc field names.
// Slice aka __go_open_array.
#define array __values
#define cap __capacity
// Iface aka __go_interface
#define tab __methods
// Hmap aka __go_map
typedef struct __go_map Hmap;
// Type aka __go_type_descriptor
#define string __reflection
#define KindPtr GO_PTR
#define KindNoPointers GO_NO_POINTERS
// PtrType aka __go_ptr_type
#define elem __element_type

#ifdef USING_SPLIT_STACK

extern void * __splitstack_find (void *, void *, size_t *, void **, void **,
				 void **);

extern void * __splitstack_find_context (void *context[10], size_t *, void **,
					 void **, void **);

#endif

enum {
	Debug = 0,
	CollectStats = 0,
	ScanStackByFrames = 1,
	IgnorePreciseGC = 0,
	ConcurrentSweep = 1,

	// Four bits per word (see #defines below).
	wordsPerBitmapWord = sizeof(void*)*8/4,
	bitShift = sizeof(void*)*8/4,

	WorkbufSize	= 16*1024,
	RootBlockSize	= 4*1024,
	FinBlockSize	= 4*1024,

	handoffThreshold = 4,
	IntermediateBufferCapacity = 64,

	// Bits in type information
	PRECISE = 1,
	LOOP = 2,
	PC_BITS = PRECISE | LOOP,

	// Pointer map
	BitsPerPointer = 2,
	BitsNoPointer = 0,
	BitsPointer = 1,
	BitsIface = 2,
	BitsEface = 3,

	RootData	= 0,
	RootBss		= 1,
	RootFinalizers	= 2,
	RootSpanTypes	= 3,
	RootFlushCaches = 4,
	RootCount	= 5,
};

#define GcpercentUnknown (-2)

// Initialized from $GOGC.  GOGC=off means no gc.
static int32 gcpercent = GcpercentUnknown;

static struct
{
	Lock;  
	void* head;
} pools;

void sync_runtime_registerPool(void **)
  __asm__ (GOSYM_PREFIX "sync.runtime_registerPool");

void
sync_runtime_registerPool(void **p)
{
	runtime_lock(&pools);
	p[0] = pools.head;
	pools.head = p;
	runtime_unlock(&pools);
}

static void
clearpools(void)
{
	void **pool, **next;
	P *p, **pp;
	MCache *c;
	uintptr off;

	// clear sync.Pool's
	for(pool = pools.head; pool != nil; pool = next) {
		next = pool[0];
		pool[0] = nil; // next
		pool[1] = nil; // local
		pool[2] = nil; // localSize
		off = (uintptr)pool[3] / sizeof(void*);
		pool[off+0] = nil; // global slice
		pool[off+1] = nil;
		pool[off+2] = nil;
	}
	pools.head = nil;

	for(pp=runtime_allp; (p=*pp) != nil; pp++) {
		// clear tinyalloc pool
		c = p->mcache;
		if(c != nil) {
			c->tiny = nil;
			c->tinysize = 0;
		}
		// clear defer pools
		p->deferpool = nil;
	}
}

// Bits in per-word bitmap.
// #defines because enum might not be able to hold the values.
//
// Each word in the bitmap describes wordsPerBitmapWord words
// of heap memory.  There are 4 bitmap bits dedicated to each heap word,
// so on a 64-bit system there is one bitmap word per 16 heap words.
// The bits in the word are packed together by type first, then by
// heap location, so each 64-bit bitmap word consists of, from top to bottom,
// the 16 bitSpecial bits for the corresponding heap words, then the 16 bitMarked bits,
// then the 16 bitScan/bitBlockBoundary bits, then the 16 bitAllocated bits.
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
#define bitAllocated		((uintptr)1<<(bitShift*0))	/* block start; eligible for garbage collection */
#define bitScan			((uintptr)1<<(bitShift*1))	/* when bitAllocated is set */
#define bitMarked		((uintptr)1<<(bitShift*2))	/* when bitAllocated is set */
#define bitSpecial		((uintptr)1<<(bitShift*3))	/* when bitAllocated is set - has finalizer or being profiled */
#define bitBlockBoundary	((uintptr)1<<(bitShift*1))	/* when bitAllocated is NOT set - mark for FlagNoGC objects */

#define bitMask (bitAllocated | bitScan | bitMarked | bitSpecial)

// Holding worldsema grants an M the right to try to stop the world.
// The procedure is:
//
//	runtime_semacquire(&runtime_worldsema);
//	m->gcing = 1;
//	runtime_stoptheworld();
//
//	... do stuff ...
//
//	m->gcing = 0;
//	runtime_semrelease(&runtime_worldsema);
//	runtime_starttheworld();
//
uint32 runtime_worldsema = 1;

typedef struct Workbuf Workbuf;
struct Workbuf
{
#define SIZE (WorkbufSize-sizeof(LFNode)-sizeof(uintptr))
	LFNode  node; // must be first
	uintptr nobj;
	Obj     obj[SIZE/sizeof(Obj) - 1];
	uint8   _padding[SIZE%sizeof(Obj) + sizeof(Obj)];
#undef SIZE
};

typedef struct Finalizer Finalizer;
struct Finalizer
{
	FuncVal *fn;
	void *arg;
	const struct __go_func_type *ft;
	const struct __go_ptr_type *ot;
};

typedef struct FinBlock FinBlock;
struct FinBlock
{
	FinBlock *alllink;
	FinBlock *next;
	int32 cnt;
	int32 cap;
	Finalizer fin[1];
};

static G	*fing;
static FinBlock	*finq; // list of finalizers that are to be executed
static FinBlock	*finc; // cache of free blocks
static FinBlock	*allfin; // list of all blocks
static int32	fingwait;
static Lock	gclock;

static void	runfinq(void*);
static void	bgsweep(void*);
static Workbuf* getempty(Workbuf*);
static Workbuf* getfull(Workbuf*);
static void	putempty(Workbuf*);
static Workbuf* handoff(Workbuf*);
static void	gchelperstart(void);
static void	flushallmcaches(void);
static void	addstackroots(G *gp, Workbuf **wbufp);

static struct {
	uint64	full;  // lock-free list of full blocks
	uint64	empty; // lock-free list of empty blocks
	byte	pad0[CacheLineSize]; // prevents false-sharing between full/empty and nproc/nwait
	uint32	nproc;
	int64	tstart;
	volatile uint32	nwait;
	volatile uint32	ndone;
	Note	alldone;
	ParFor	*markfor;

	Lock;
	byte	*chunk;
	uintptr	nchunk;
} work __attribute__((aligned(8)));

enum {
	GC_DEFAULT_PTR = GC_NUM_INSTR,
	GC_CHAN,

	GC_NUM_INSTR2
};

static struct {
	struct {
		uint64 sum;
		uint64 cnt;
	} ptr;
	uint64 nbytes;
	struct {
		uint64 sum;
		uint64 cnt;
		uint64 notype;
		uint64 typelookup;
	} obj;
	uint64 rescan;
	uint64 rescanbytes;
	uint64 instr[GC_NUM_INSTR2];
	uint64 putempty;
	uint64 getfull;
	struct {
		uint64 foundbit;
		uint64 foundword;
		uint64 foundspan;
	} flushptrbuf;
	struct {
		uint64 foundbit;
		uint64 foundword;
		uint64 foundspan;
	} markonly;
	uint32 nbgsweep;
	uint32 npausesweep;
} gcstats;

// markonly marks an object. It returns true if the object
// has been marked by this function, false otherwise.
// This function doesn't append the object to any buffer.
static bool
markonly(void *obj)
{
	byte *p;
	uintptr *bitp, bits, shift, x, xbits, off, j;
	MSpan *s;
	PageID k;

	// Words outside the arena cannot be pointers.
	if((byte*)obj < runtime_mheap.arena_start || (byte*)obj >= runtime_mheap.arena_used)
		return false;

	// obj may be a pointer to a live object.
	// Try to find the beginning of the object.

	// Round down to word boundary.
	obj = (void*)((uintptr)obj & ~((uintptr)PtrSize-1));

	// Find bits for this word.
	off = (uintptr*)obj - (uintptr*)runtime_mheap.arena_start;
	bitp = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;
	xbits = *bitp;
	bits = xbits >> shift;

	// Pointing at the beginning of a block?
	if((bits & (bitAllocated|bitBlockBoundary)) != 0) {
		if(CollectStats)
			runtime_xadd64(&gcstats.markonly.foundbit, 1);
		goto found;
	}

	// Pointing just past the beginning?
	// Scan backward a little to find a block boundary.
	for(j=shift; j-->0; ) {
		if(((xbits>>j) & (bitAllocated|bitBlockBoundary)) != 0) {
			shift = j;
			bits = xbits>>shift;
			if(CollectStats)
				runtime_xadd64(&gcstats.markonly.foundword, 1);
			goto found;
		}
	}

	// Otherwise consult span table to find beginning.
	// (Manually inlined copy of MHeap_LookupMaybe.)
	k = (uintptr)obj>>PageShift;
	x = k;
	x -= (uintptr)runtime_mheap.arena_start>>PageShift;
	s = runtime_mheap.spans[x];
	if(s == nil || k < s->start || (byte*)obj >= s->limit || s->state != MSpanInUse)
		return false;
	p = (byte*)((uintptr)s->start<<PageShift);
	if(s->sizeclass == 0) {
		obj = p;
	} else {
		uintptr size = s->elemsize;
		int32 i = ((byte*)obj - p)/size;
		obj = p+i*size;
	}

	// Now that we know the object header, reload bits.
	off = (uintptr*)obj - (uintptr*)runtime_mheap.arena_start;
	bitp = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;
	xbits = *bitp;
	bits = xbits >> shift;
	if(CollectStats)
		runtime_xadd64(&gcstats.markonly.foundspan, 1);

found:
	// Now we have bits, bitp, and shift correct for
	// obj pointing at the base of the object.
	// Only care about allocated and not marked.
	if((bits & (bitAllocated|bitMarked)) != bitAllocated)
		return false;
	if(work.nproc == 1)
		*bitp |= bitMarked<<shift;
	else {
		for(;;) {
			x = *bitp;
			if(x & (bitMarked<<shift))
				return false;
			if(runtime_casp((void**)bitp, (void*)x, (void*)(x|(bitMarked<<shift))))
				break;
		}
	}

	// The object is now marked
	return true;
}

// PtrTarget is a structure used by intermediate buffers.
// The intermediate buffers hold GC data before it
// is moved/flushed to the work buffer (Workbuf).
// The size of an intermediate buffer is very small,
// such as 32 or 64 elements.
typedef struct PtrTarget PtrTarget;
struct PtrTarget
{
	void *p;
	uintptr ti;
};

typedef	struct Scanbuf Scanbuf;
struct	Scanbuf
{
	struct {
		PtrTarget *begin;
		PtrTarget *end;
		PtrTarget *pos;
	} ptr;
	struct {
		Obj *begin;
		Obj *end;
		Obj *pos;
	} obj;
	Workbuf *wbuf;
	Obj *wp;
	uintptr nobj;
};

typedef struct BufferList BufferList;
struct BufferList
{
	PtrTarget ptrtarget[IntermediateBufferCapacity];
	Obj obj[IntermediateBufferCapacity];
	uint32 busy;
	byte pad[CacheLineSize];
};
static BufferList bufferList[MaxGcproc];

static Type *itabtype;

static void enqueue(Obj obj, Workbuf **_wbuf, Obj **_wp, uintptr *_nobj);

// flushptrbuf moves data from the PtrTarget buffer to the work buffer.
// The PtrTarget buffer contains blocks irrespective of whether the blocks have been marked or scanned,
// while the work buffer contains blocks which have been marked
// and are prepared to be scanned by the garbage collector.
//
// _wp, _wbuf, _nobj are input/output parameters and are specifying the work buffer.
//
// A simplified drawing explaining how the todo-list moves from a structure to another:
//
//     scanblock
//  (find pointers)
//    Obj ------> PtrTarget (pointer targets)
//     ↑          |
//     |          |
//     `----------'
//     flushptrbuf
//  (find block start, mark and enqueue)
static void
flushptrbuf(Scanbuf *sbuf)
{
	byte *p, *arena_start, *obj;
	uintptr size, *bitp, bits, shift, j, x, xbits, off, nobj, ti, n;
	MSpan *s;
	PageID k;
	Obj *wp;
	Workbuf *wbuf;
	PtrTarget *ptrbuf;
	PtrTarget *ptrbuf_end;

	arena_start = runtime_mheap.arena_start;

	wp = sbuf->wp;
	wbuf = sbuf->wbuf;
	nobj = sbuf->nobj;

	ptrbuf = sbuf->ptr.begin;
	ptrbuf_end = sbuf->ptr.pos;
	n = ptrbuf_end - sbuf->ptr.begin;
	sbuf->ptr.pos = sbuf->ptr.begin;

	if(CollectStats) {
		runtime_xadd64(&gcstats.ptr.sum, n);
		runtime_xadd64(&gcstats.ptr.cnt, 1);
	}

	// If buffer is nearly full, get a new one.
	if(wbuf == nil || nobj+n >= nelem(wbuf->obj)) {
		if(wbuf != nil)
			wbuf->nobj = nobj;
		wbuf = getempty(wbuf);
		wp = wbuf->obj;
		nobj = 0;

		if(n >= nelem(wbuf->obj))
			runtime_throw("ptrbuf has to be smaller than WorkBuf");
	}

	while(ptrbuf < ptrbuf_end) {
		obj = ptrbuf->p;
		ti = ptrbuf->ti;
		ptrbuf++;

		// obj belongs to interval [mheap.arena_start, mheap.arena_used).
		if(Debug > 1) {
			if(obj < runtime_mheap.arena_start || obj >= runtime_mheap.arena_used)
				runtime_throw("object is outside of mheap");
		}

		// obj may be a pointer to a live object.
		// Try to find the beginning of the object.

		// Round down to word boundary.
		if(((uintptr)obj & ((uintptr)PtrSize-1)) != 0) {
			obj = (void*)((uintptr)obj & ~((uintptr)PtrSize-1));
			ti = 0;
		}

		// Find bits for this word.
		off = (uintptr*)obj - (uintptr*)arena_start;
		bitp = (uintptr*)arena_start - off/wordsPerBitmapWord - 1;
		shift = off % wordsPerBitmapWord;
		xbits = *bitp;
		bits = xbits >> shift;

		// Pointing at the beginning of a block?
		if((bits & (bitAllocated|bitBlockBoundary)) != 0) {
			if(CollectStats)
				runtime_xadd64(&gcstats.flushptrbuf.foundbit, 1);
			goto found;
		}

		ti = 0;

		// Pointing just past the beginning?
		// Scan backward a little to find a block boundary.
		for(j=shift; j-->0; ) {
			if(((xbits>>j) & (bitAllocated|bitBlockBoundary)) != 0) {
				obj = (byte*)obj - (shift-j)*PtrSize;
				shift = j;
				bits = xbits>>shift;
				if(CollectStats)
					runtime_xadd64(&gcstats.flushptrbuf.foundword, 1);
				goto found;
			}
		}

		// Otherwise consult span table to find beginning.
		// (Manually inlined copy of MHeap_LookupMaybe.)
		k = (uintptr)obj>>PageShift;
		x = k;
		x -= (uintptr)arena_start>>PageShift;
		s = runtime_mheap.spans[x];
		if(s == nil || k < s->start || obj >= s->limit || s->state != MSpanInUse)
			continue;
		p = (byte*)((uintptr)s->start<<PageShift);
		if(s->sizeclass == 0) {
			obj = p;
		} else {
			size = s->elemsize;
			int32 i = ((byte*)obj - p)/size;
			obj = p+i*size;
		}

		// Now that we know the object header, reload bits.
		off = (uintptr*)obj - (uintptr*)arena_start;
		bitp = (uintptr*)arena_start - off/wordsPerBitmapWord - 1;
		shift = off % wordsPerBitmapWord;
		xbits = *bitp;
		bits = xbits >> shift;
		if(CollectStats)
			runtime_xadd64(&gcstats.flushptrbuf.foundspan, 1);

	found:
		// Now we have bits, bitp, and shift correct for
		// obj pointing at the base of the object.
		// Only care about allocated and not marked.
		if((bits & (bitAllocated|bitMarked)) != bitAllocated)
			continue;
		if(work.nproc == 1)
			*bitp |= bitMarked<<shift;
		else {
			for(;;) {
				x = *bitp;
				if(x & (bitMarked<<shift))
					goto continue_obj;
				if(runtime_casp((void**)bitp, (void*)x, (void*)(x|(bitMarked<<shift))))
					break;
			}
		}

		// If object has no pointers, don't need to scan further.
		if((bits & bitScan) == 0)
			continue;

		// Ask span about size class.
		// (Manually inlined copy of MHeap_Lookup.)
		x = (uintptr)obj >> PageShift;
		x -= (uintptr)arena_start>>PageShift;
		s = runtime_mheap.spans[x];

		PREFETCH(obj);

		*wp = (Obj){obj, s->elemsize, ti};
		wp++;
		nobj++;
	continue_obj:;
	}

	// If another proc wants a pointer, give it some.
	if(work.nwait > 0 && nobj > handoffThreshold && work.full == 0) {
		wbuf->nobj = nobj;
		wbuf = handoff(wbuf);
		nobj = wbuf->nobj;
		wp = wbuf->obj + nobj;
	}

	sbuf->wp = wp;
	sbuf->wbuf = wbuf;
	sbuf->nobj = nobj;
}

static void
flushobjbuf(Scanbuf *sbuf)
{
	uintptr nobj, off;
	Obj *wp, obj;
	Workbuf *wbuf;
	Obj *objbuf;
	Obj *objbuf_end;

	wp = sbuf->wp;
	wbuf = sbuf->wbuf;
	nobj = sbuf->nobj;

	objbuf = sbuf->obj.begin;
	objbuf_end = sbuf->obj.pos;
	sbuf->obj.pos = sbuf->obj.begin;

	while(objbuf < objbuf_end) {
		obj = *objbuf++;

		// Align obj.b to a word boundary.
		off = (uintptr)obj.p & (PtrSize-1);
		if(off != 0) {
			obj.p += PtrSize - off;
			obj.n -= PtrSize - off;
			obj.ti = 0;
		}

		if(obj.p == nil || obj.n == 0)
			continue;

		// If buffer is full, get a new one.
		if(wbuf == nil || nobj >= nelem(wbuf->obj)) {
			if(wbuf != nil)
				wbuf->nobj = nobj;
			wbuf = getempty(wbuf);
			wp = wbuf->obj;
			nobj = 0;
		}

		*wp = obj;
		wp++;
		nobj++;
	}

	// If another proc wants a pointer, give it some.
	if(work.nwait > 0 && nobj > handoffThreshold && work.full == 0) {
		wbuf->nobj = nobj;
		wbuf = handoff(wbuf);
		nobj = wbuf->nobj;
		wp = wbuf->obj + nobj;
	}

	sbuf->wp = wp;
	sbuf->wbuf = wbuf;
	sbuf->nobj = nobj;
}

// Program that scans the whole block and treats every block element as a potential pointer
static uintptr defaultProg[2] = {PtrSize, GC_DEFAULT_PTR};

#if 0
// Hchan program
static uintptr chanProg[2] = {0, GC_CHAN};
#endif

// Local variables of a program fragment or loop
typedef struct Frame Frame;
struct Frame {
	uintptr count, elemsize, b;
	uintptr *loop_or_ret;
};

// Sanity check for the derived type info objti.
static void
checkptr(void *obj, uintptr objti)
{
	uintptr type, tisize, i, x;
	byte *objstart;
	Type *t;
	MSpan *s;

	if(!Debug)
		runtime_throw("checkptr is debug only");

	if((byte*)obj < runtime_mheap.arena_start || (byte*)obj >= runtime_mheap.arena_used)
		return;
	type = runtime_gettype(obj);
	t = (Type*)(type & ~(uintptr)(PtrSize-1));
	if(t == nil)
		return;
	x = (uintptr)obj >> PageShift;
	x -= (uintptr)(runtime_mheap.arena_start)>>PageShift;
	s = runtime_mheap.spans[x];
	objstart = (byte*)((uintptr)s->start<<PageShift);
	if(s->sizeclass != 0) {
		i = ((byte*)obj - objstart)/s->elemsize;
		objstart += i*s->elemsize;
	}
	tisize = *(uintptr*)objti;
	// Sanity check for object size: it should fit into the memory block.
	if((byte*)obj + tisize > objstart + s->elemsize) {
		runtime_printf("object of type '%S' at %p/%p does not fit in block %p/%p\n",
			       *t->string, obj, tisize, objstart, s->elemsize);
		runtime_throw("invalid gc type info");
	}
	if(obj != objstart)
		return;
	// If obj points to the beginning of the memory block,
	// check type info as well.
	if(t->string == nil ||
		// Gob allocates unsafe pointers for indirection.
		(runtime_strcmp((const char *)t->string->str, (const char*)"unsafe.Pointer") &&
		// Runtime and gc think differently about closures.
		 runtime_strstr((const char *)t->string->str, (const char*)"struct { F uintptr") != (const char *)t->string->str)) {
#if 0
		pc1 = (uintptr*)objti;
		pc2 = (uintptr*)t->gc;
		// A simple best-effort check until first GC_END.
		for(j = 1; pc1[j] != GC_END && pc2[j] != GC_END; j++) {
			if(pc1[j] != pc2[j]) {
				runtime_printf("invalid gc type info for '%s' at %p, type info %p, block info %p\n",
					       t->string ? (const int8*)t->string->str : (const int8*)"?", j, pc1[j], pc2[j]);
				runtime_throw("invalid gc type info");
			}
		}
#endif
	}
}					

// scanblock scans a block of n bytes starting at pointer b for references
// to other objects, scanning any it finds recursively until there are no
// unscanned objects left.  Instead of using an explicit recursion, it keeps
// a work list in the Workbuf* structures and loops in the main function
// body.  Keeping an explicit work list is easier on the stack allocator and
// more efficient.
static void
scanblock(Workbuf *wbuf, bool keepworking)
{
	byte *b, *arena_start, *arena_used;
	uintptr n, i, end_b, elemsize, size, ti, objti, count, /* type, */ nobj;
	uintptr *pc, precise_type, nominal_size;
#if 0
	uintptr *chan_ret, chancap;
#endif
	void *obj;
	const Type *t;
	Slice *sliceptr;
	Frame *stack_ptr, stack_top, stack[GC_STACK_CAPACITY+4];
	BufferList *scanbuffers;
	Scanbuf sbuf;
	Eface *eface;
	Iface *iface;
#if 0
	Hchan *chan;
	ChanType *chantype;
#endif
	Obj *wp;

	if(sizeof(Workbuf) % WorkbufSize != 0)
		runtime_throw("scanblock: size of Workbuf is suboptimal");

	// Memory arena parameters.
	arena_start = runtime_mheap.arena_start;
	arena_used = runtime_mheap.arena_used;

	stack_ptr = stack+nelem(stack)-1;

	precise_type = false;
	nominal_size = 0;

	if(wbuf) {
		nobj = wbuf->nobj;
		wp = &wbuf->obj[nobj];
	} else {
		nobj = 0;
		wp = nil;
	}

	// Initialize sbuf
	scanbuffers = &bufferList[runtime_m()->helpgc];

	sbuf.ptr.begin = sbuf.ptr.pos = &scanbuffers->ptrtarget[0];
	sbuf.ptr.end = sbuf.ptr.begin + nelem(scanbuffers->ptrtarget);

	sbuf.obj.begin = sbuf.obj.pos = &scanbuffers->obj[0];
	sbuf.obj.end = sbuf.obj.begin + nelem(scanbuffers->obj);

	sbuf.wbuf = wbuf;
	sbuf.wp = wp;
	sbuf.nobj = nobj;

	// (Silence the compiler)
#if 0
	chan = nil;
	chantype = nil;
	chan_ret = nil;
#endif

	goto next_block;

	for(;;) {
		// Each iteration scans the block b of length n, queueing pointers in
		// the work buffer.
		if(Debug > 1) {
			runtime_printf("scanblock %p %D\n", b, (int64)n);
		}

		if(CollectStats) {
			runtime_xadd64(&gcstats.nbytes, n);
			runtime_xadd64(&gcstats.obj.sum, sbuf.nobj);
			runtime_xadd64(&gcstats.obj.cnt, 1);
		}

		if(ti != 0 && false) {
			pc = (uintptr*)(ti & ~(uintptr)PC_BITS);
			precise_type = (ti & PRECISE);
			stack_top.elemsize = pc[0];
			if(!precise_type)
				nominal_size = pc[0];
			if(ti & LOOP) {
				stack_top.count = 0;	// 0 means an infinite number of iterations
				stack_top.loop_or_ret = pc+1;
			} else {
				stack_top.count = 1;
			}
			if(Debug) {
				// Simple sanity check for provided type info ti:
				// The declared size of the object must be not larger than the actual size
				// (it can be smaller due to inferior pointers).
				// It's difficult to make a comprehensive check due to inferior pointers,
				// reflection, gob, etc.
				if(pc[0] > n) {
					runtime_printf("invalid gc type info: type info size %p, block size %p\n", pc[0], n);
					runtime_throw("invalid gc type info");
				}
			}
		} else if(UseSpanType && false) {
			if(CollectStats)
				runtime_xadd64(&gcstats.obj.notype, 1);

#if 0
			type = runtime_gettype(b);
			if(type != 0) {
				if(CollectStats)
					runtime_xadd64(&gcstats.obj.typelookup, 1);

				t = (Type*)(type & ~(uintptr)(PtrSize-1));
				switch(type & (PtrSize-1)) {
				case TypeInfo_SingleObject:
					pc = (uintptr*)t->gc;
					precise_type = true;  // type information about 'b' is precise
					stack_top.count = 1;
					stack_top.elemsize = pc[0];
					break;
				case TypeInfo_Array:
					pc = (uintptr*)t->gc;
					if(pc[0] == 0)
						goto next_block;
					precise_type = true;  // type information about 'b' is precise
					stack_top.count = 0;  // 0 means an infinite number of iterations
					stack_top.elemsize = pc[0];
					stack_top.loop_or_ret = pc+1;
					break;
				case TypeInfo_Chan:
					chan = (Hchan*)b;
					chantype = (ChanType*)t;
					chan_ret = nil;
					pc = chanProg;
					break;
				default:
					runtime_throw("scanblock: invalid type");
					return;
				}
			} else {
				pc = defaultProg;
			}
#endif
		} else {
			pc = defaultProg;
		}

		if(IgnorePreciseGC)
			pc = defaultProg;

		pc++;
		stack_top.b = (uintptr)b;

		end_b = (uintptr)b + n - PtrSize;

	for(;;) {
		if(CollectStats)
			runtime_xadd64(&gcstats.instr[pc[0]], 1);

		obj = nil;
		objti = 0;
		switch(pc[0]) {
		case GC_PTR:
			obj = *(void**)(stack_top.b + pc[1]);
			objti = pc[2];
			pc += 3;
			if(Debug)
				checkptr(obj, objti);
			break;

		case GC_SLICE:
			sliceptr = (Slice*)(stack_top.b + pc[1]);
			if(sliceptr->cap != 0) {
				obj = sliceptr->array;
				// Can't use slice element type for scanning,
				// because if it points to an array embedded
				// in the beginning of a struct,
				// we will scan the whole struct as the slice.
				// So just obtain type info from heap.
			}
			pc += 3;
			break;

		case GC_APTR:
			obj = *(void**)(stack_top.b + pc[1]);
			pc += 2;
			break;

		case GC_STRING:
			obj = *(void**)(stack_top.b + pc[1]);
			markonly(obj);
			pc += 2;
			continue;

		case GC_EFACE:
			eface = (Eface*)(stack_top.b + pc[1]);
			pc += 2;
			if(eface->__type_descriptor == nil)
				continue;

			// eface->type
			t = eface->__type_descriptor;
			if((const byte*)t >= arena_start && (const byte*)t < arena_used) {
				union { const Type *tc; Type *tr; } u;
				u.tc = t;
				*sbuf.ptr.pos++ = (PtrTarget){u.tr, 0};
				if(sbuf.ptr.pos == sbuf.ptr.end)
					flushptrbuf(&sbuf);
			}

			// eface->__object
			if((byte*)eface->__object >= arena_start && (byte*)eface->__object < arena_used) {
				if(t->__size <= sizeof(void*)) {
					if((t->__code & KindNoPointers))
						continue;

					obj = eface->__object;
					if((t->__code & ~KindNoPointers) == KindPtr)
						// objti = (uintptr)((PtrType*)t)->elem->gc;
						objti = 0;
				} else {
					obj = eface->__object;
					// objti = (uintptr)t->gc;
					objti = 0;
				}
			}
			break;

		case GC_IFACE:
			iface = (Iface*)(stack_top.b + pc[1]);
			pc += 2;
			if(iface->tab == nil)
				continue;
			
			// iface->tab
			if((byte*)iface->tab >= arena_start && (byte*)iface->tab < arena_used) {
				*sbuf.ptr.pos++ = (PtrTarget){iface->tab, /* (uintptr)itabtype->gc */ 0};
				if(sbuf.ptr.pos == sbuf.ptr.end)
					flushptrbuf(&sbuf);
			}

			// iface->data
			if((byte*)iface->__object >= arena_start && (byte*)iface->__object < arena_used) {
				// t = iface->tab->type;
				t = nil;
				if(t->__size <= sizeof(void*)) {
					if((t->__code & KindNoPointers))
						continue;

					obj = iface->__object;
					if((t->__code & ~KindNoPointers) == KindPtr)
						// objti = (uintptr)((const PtrType*)t)->elem->gc;
						objti = 0;
				} else {
					obj = iface->__object;
					// objti = (uintptr)t->gc;
					objti = 0;
				}
			}
			break;

		case GC_DEFAULT_PTR:
			while(stack_top.b <= end_b) {
				obj = *(byte**)stack_top.b;
				stack_top.b += PtrSize;
				if((byte*)obj >= arena_start && (byte*)obj < arena_used) {
					*sbuf.ptr.pos++ = (PtrTarget){obj, 0};
					if(sbuf.ptr.pos == sbuf.ptr.end)
						flushptrbuf(&sbuf);
				}
			}
			goto next_block;

		case GC_END:
			if(--stack_top.count != 0) {
				// Next iteration of a loop if possible.
				stack_top.b += stack_top.elemsize;
				if(stack_top.b + stack_top.elemsize <= end_b+PtrSize) {
					pc = stack_top.loop_or_ret;
					continue;
				}
				i = stack_top.b;
			} else {
				// Stack pop if possible.
				if(stack_ptr+1 < stack+nelem(stack)) {
					pc = stack_top.loop_or_ret;
					stack_top = *(++stack_ptr);
					continue;
				}
				i = (uintptr)b + nominal_size;
			}
			if(!precise_type) {
				// Quickly scan [b+i,b+n) for possible pointers.
				for(; i<=end_b; i+=PtrSize) {
					if(*(byte**)i != nil) {
						// Found a value that may be a pointer.
						// Do a rescan of the entire block.
						enqueue((Obj){b, n, 0}, &sbuf.wbuf, &sbuf.wp, &sbuf.nobj);
						if(CollectStats) {
							runtime_xadd64(&gcstats.rescan, 1);
							runtime_xadd64(&gcstats.rescanbytes, n);
						}
						break;
					}
				}
			}
			goto next_block;

		case GC_ARRAY_START:
			i = stack_top.b + pc[1];
			count = pc[2];
			elemsize = pc[3];
			pc += 4;

			// Stack push.
			*stack_ptr-- = stack_top;
			stack_top = (Frame){count, elemsize, i, pc};
			continue;

		case GC_ARRAY_NEXT:
			if(--stack_top.count != 0) {
				stack_top.b += stack_top.elemsize;
				pc = stack_top.loop_or_ret;
			} else {
				// Stack pop.
				stack_top = *(++stack_ptr);
				pc += 1;
			}
			continue;

		case GC_CALL:
			// Stack push.
			*stack_ptr-- = stack_top;
			stack_top = (Frame){1, 0, stack_top.b + pc[1], pc+3 /*return address*/};
			pc = (uintptr*)((byte*)pc + *(int32*)(pc+2));  // target of the CALL instruction
			continue;

		case GC_REGION:
			obj = (void*)(stack_top.b + pc[1]);
			size = pc[2];
			objti = pc[3];
			pc += 4;

			*sbuf.obj.pos++ = (Obj){obj, size, objti};
			if(sbuf.obj.pos == sbuf.obj.end)
				flushobjbuf(&sbuf);
			continue;

#if 0
		case GC_CHAN_PTR:
			chan = *(Hchan**)(stack_top.b + pc[1]);
			if(chan == nil) {
				pc += 3;
				continue;
			}
			if(markonly(chan)) {
				chantype = (ChanType*)pc[2];
				if(!(chantype->elem->__code & KindNoPointers)) {
					// Start chanProg.
					chan_ret = pc+3;
					pc = chanProg+1;
					continue;
				}
			}
			pc += 3;
			continue;

		case GC_CHAN:
			// There are no heap pointers in struct Hchan,
			// so we can ignore the leading sizeof(Hchan) bytes.
			if(!(chantype->elem->__code & KindNoPointers)) {
				// Channel's buffer follows Hchan immediately in memory.
				// Size of buffer (cap(c)) is second int in the chan struct.
				chancap = ((uintgo*)chan)[1];
				if(chancap > 0) {
					// TODO(atom): split into two chunks so that only the
					// in-use part of the circular buffer is scanned.
					// (Channel routines zero the unused part, so the current
					// code does not lead to leaks, it's just a little inefficient.)
					*sbuf.obj.pos++ = (Obj){(byte*)chan+runtime_Hchansize, chancap*chantype->elem->size,
						(uintptr)chantype->elem->gc | PRECISE | LOOP};
					if(sbuf.obj.pos == sbuf.obj.end)
						flushobjbuf(&sbuf);
				}
			}
			if(chan_ret == nil)
				goto next_block;
			pc = chan_ret;
			continue;
#endif

		default:
			runtime_throw("scanblock: invalid GC instruction");
			return;
		}

		if((byte*)obj >= arena_start && (byte*)obj < arena_used) {
			*sbuf.ptr.pos++ = (PtrTarget){obj, objti};
			if(sbuf.ptr.pos == sbuf.ptr.end)
				flushptrbuf(&sbuf);
		}
	}

	next_block:
		// Done scanning [b, b+n).  Prepare for the next iteration of
		// the loop by setting b, n, ti to the parameters for the next block.

		if(sbuf.nobj == 0) {
			flushptrbuf(&sbuf);
			flushobjbuf(&sbuf);

			if(sbuf.nobj == 0) {
				if(!keepworking) {
					if(sbuf.wbuf)
						putempty(sbuf.wbuf);
					return;
				}
				// Emptied our buffer: refill.
				sbuf.wbuf = getfull(sbuf.wbuf);
				if(sbuf.wbuf == nil)
					return;
				sbuf.nobj = sbuf.wbuf->nobj;
				sbuf.wp = sbuf.wbuf->obj + sbuf.wbuf->nobj;
			}
		}

		// Fetch b from the work buffer.
		--sbuf.wp;
		b = sbuf.wp->p;
		n = sbuf.wp->n;
		ti = sbuf.wp->ti;
		sbuf.nobj--;
	}
}

static struct root_list* roots;

void
__go_register_gc_roots (struct root_list* r)
{
	// FIXME: This needs locking if multiple goroutines can call
	// dlopen simultaneously.
	r->next = roots;
	roots = r;
}

// Append obj to the work buffer.
// _wbuf, _wp, _nobj are input/output parameters and are specifying the work buffer.
static void
enqueue(Obj obj, Workbuf **_wbuf, Obj **_wp, uintptr *_nobj)
{
	uintptr nobj, off;
	Obj *wp;
	Workbuf *wbuf;

	if(Debug > 1)
		runtime_printf("append obj(%p %D %p)\n", obj.p, (int64)obj.n, obj.ti);

	// Align obj.b to a word boundary.
	off = (uintptr)obj.p & (PtrSize-1);
	if(off != 0) {
		obj.p += PtrSize - off;
		obj.n -= PtrSize - off;
		obj.ti = 0;
	}

	if(obj.p == nil || obj.n == 0)
		return;

	// Load work buffer state
	wp = *_wp;
	wbuf = *_wbuf;
	nobj = *_nobj;

	// If another proc wants a pointer, give it some.
	if(work.nwait > 0 && nobj > handoffThreshold && work.full == 0) {
		wbuf->nobj = nobj;
		wbuf = handoff(wbuf);
		nobj = wbuf->nobj;
		wp = wbuf->obj + nobj;
	}

	// If buffer is full, get a new one.
	if(wbuf == nil || nobj >= nelem(wbuf->obj)) {
		if(wbuf != nil)
			wbuf->nobj = nobj;
		wbuf = getempty(wbuf);
		wp = wbuf->obj;
		nobj = 0;
	}

	*wp = obj;
	wp++;
	nobj++;

	// Save work buffer state
	*_wp = wp;
	*_wbuf = wbuf;
	*_nobj = nobj;
}

static void
enqueue1(Workbuf **wbufp, Obj obj)
{
	Workbuf *wbuf;

	wbuf = *wbufp;
	if(wbuf->nobj >= nelem(wbuf->obj))
		*wbufp = wbuf = getempty(wbuf);
	wbuf->obj[wbuf->nobj++] = obj;
}

static void
markroot(ParFor *desc, uint32 i)
{
	Workbuf *wbuf;
	FinBlock *fb;
	MHeap *h;
	MSpan **allspans, *s;
	uint32 spanidx, sg;
	G *gp;
	void *p;

	USED(&desc);
	wbuf = getempty(nil);
	switch(i) {
	case RootData:
		// For gccgo this is both data and bss.
		{
			struct root_list *pl;

			for(pl = roots; pl != nil; pl = pl->next) {
				struct root *pr = &pl->roots[0];
				while(1) {
					void *decl = pr->decl;
					if(decl == nil)
						break;
					enqueue1(&wbuf, (Obj){decl, pr->size, 0});
					pr++;
				}
			}
		}
		break;

	case RootBss:
		// For gccgo we use this for all the other global roots.
		enqueue1(&wbuf, (Obj){(byte*)&runtime_m0, sizeof runtime_m0, 0});
		enqueue1(&wbuf, (Obj){(byte*)&runtime_g0, sizeof runtime_g0, 0});
		enqueue1(&wbuf, (Obj){(byte*)&runtime_allg, sizeof runtime_allg, 0});
		enqueue1(&wbuf, (Obj){(byte*)&runtime_allm, sizeof runtime_allm, 0});
		enqueue1(&wbuf, (Obj){(byte*)&runtime_allp, sizeof runtime_allp, 0});
		enqueue1(&wbuf, (Obj){(byte*)&work, sizeof work, 0});
		runtime_proc_scan(&wbuf, enqueue1);
		runtime_MProf_Mark(&wbuf, enqueue1);
		runtime_time_scan(&wbuf, enqueue1);
		runtime_netpoll_scan(&wbuf, enqueue1);
		break;

	case RootFinalizers:
		for(fb=allfin; fb; fb=fb->alllink)
			enqueue1(&wbuf, (Obj){(byte*)fb->fin, fb->cnt*sizeof(fb->fin[0]), 0});
		break;

	case RootSpanTypes:
		// mark span types and MSpan.specials (to walk spans only once)
		h = &runtime_mheap;
		sg = h->sweepgen;
		allspans = h->allspans;
		for(spanidx=0; spanidx<runtime_mheap.nspan; spanidx++) {
			Special *sp;
			SpecialFinalizer *spf;

			s = allspans[spanidx];
			if(s->sweepgen != sg) {
				runtime_printf("sweep %d %d\n", s->sweepgen, sg);
				runtime_throw("gc: unswept span");
			}
			if(s->state != MSpanInUse)
				continue;
			// The garbage collector ignores type pointers stored in MSpan.types:
			//  - Compiler-generated types are stored outside of heap.
			//  - The reflect package has runtime-generated types cached in its data structures.
			//    The garbage collector relies on finding the references via that cache.
			if(s->types.compression == MTypes_Words || s->types.compression == MTypes_Bytes)
				markonly((byte*)s->types.data);
			for(sp = s->specials; sp != nil; sp = sp->next) {
				if(sp->kind != KindSpecialFinalizer)
					continue;
				// don't mark finalized object, but scan it so we
				// retain everything it points to.
				spf = (SpecialFinalizer*)sp;
				// A finalizer can be set for an inner byte of an object, find object beginning.
				p = (void*)((s->start << PageShift) + spf->offset/s->elemsize*s->elemsize);
				enqueue1(&wbuf, (Obj){p, s->elemsize, 0});
				enqueue1(&wbuf, (Obj){(void*)&spf->fn, PtrSize, 0});
				enqueue1(&wbuf, (Obj){(void*)&spf->ft, PtrSize, 0});
				enqueue1(&wbuf, (Obj){(void*)&spf->ot, PtrSize, 0});
			}
		}
		break;

	case RootFlushCaches:
		flushallmcaches();
		break;

	default:
		// the rest is scanning goroutine stacks
		if(i - RootCount >= runtime_allglen)
			runtime_throw("markroot: bad index");
		gp = runtime_allg[i - RootCount];
		// remember when we've first observed the G blocked
		// needed only to output in traceback
		if((gp->status == Gwaiting || gp->status == Gsyscall) && gp->waitsince == 0)
			gp->waitsince = work.tstart;
		addstackroots(gp, &wbuf);
		break;
		
	}

	if(wbuf)
		scanblock(wbuf, false);
}

// Get an empty work buffer off the work.empty list,
// allocating new buffers as needed.
static Workbuf*
getempty(Workbuf *b)
{
	if(b != nil)
		runtime_lfstackpush(&work.full, &b->node);
	b = (Workbuf*)runtime_lfstackpop(&work.empty);
	if(b == nil) {
		// Need to allocate.
		runtime_lock(&work);
		if(work.nchunk < sizeof *b) {
			work.nchunk = 1<<20;
			work.chunk = runtime_SysAlloc(work.nchunk, &mstats.gc_sys);
			if(work.chunk == nil)
				runtime_throw("runtime: cannot allocate memory");
		}
		b = (Workbuf*)work.chunk;
		work.chunk += sizeof *b;
		work.nchunk -= sizeof *b;
		runtime_unlock(&work);
	}
	b->nobj = 0;
	return b;
}

static void
putempty(Workbuf *b)
{
	if(CollectStats)
		runtime_xadd64(&gcstats.putempty, 1);

	runtime_lfstackpush(&work.empty, &b->node);
}

// Get a full work buffer off the work.full list, or return nil.
static Workbuf*
getfull(Workbuf *b)
{
	M *m;
	int32 i;

	if(CollectStats)
		runtime_xadd64(&gcstats.getfull, 1);

	if(b != nil)
		runtime_lfstackpush(&work.empty, &b->node);
	b = (Workbuf*)runtime_lfstackpop(&work.full);
	if(b != nil || work.nproc == 1)
		return b;

	m = runtime_m();
	runtime_xadd(&work.nwait, +1);
	for(i=0;; i++) {
		if(work.full != 0) {
			runtime_xadd(&work.nwait, -1);
			b = (Workbuf*)runtime_lfstackpop(&work.full);
			if(b != nil)
				return b;
			runtime_xadd(&work.nwait, +1);
		}
		if(work.nwait == work.nproc)
			return nil;
		if(i < 10) {
			m->gcstats.nprocyield++;
			runtime_procyield(20);
		} else if(i < 20) {
			m->gcstats.nosyield++;
			runtime_osyield();
		} else {
			m->gcstats.nsleep++;
			runtime_usleep(100);
		}
	}
}

static Workbuf*
handoff(Workbuf *b)
{
	M *m;
	int32 n;
	Workbuf *b1;

	m = runtime_m();

	// Make new buffer with half of b's pointers.
	b1 = getempty(nil);
	n = b->nobj/2;
	b->nobj -= n;
	b1->nobj = n;
	runtime_memmove(b1->obj, b->obj+b->nobj, n*sizeof b1->obj[0]);
	m->gcstats.nhandoff++;
	m->gcstats.nhandoffcnt += n;

	// Put b on full list - let first half of b get stolen.
	runtime_lfstackpush(&work.full, &b->node);
	return b1;
}

static void
addstackroots(G *gp, Workbuf **wbufp)
{
	switch(gp->status){
	default:
		runtime_printf("unexpected G.status %d (goroutine %p %D)\n", gp->status, gp, gp->goid);
		runtime_throw("mark - bad status");
	case Gdead:
		return;
	case Grunning:
		runtime_throw("mark - world not stopped");
	case Grunnable:
	case Gsyscall:
	case Gwaiting:
		break;
	}

#ifdef USING_SPLIT_STACK
	M *mp;
	void* sp;
	size_t spsize;
	void* next_segment;
	void* next_sp;
	void* initial_sp;

	if(gp == runtime_g()) {
		// Scanning our own stack.
		sp = __splitstack_find(nil, nil, &spsize, &next_segment,
				       &next_sp, &initial_sp);
	} else if((mp = gp->m) != nil && mp->helpgc) {
		// gchelper's stack is in active use and has no interesting pointers.
		return;
	} else {
		// Scanning another goroutine's stack.
		// The goroutine is usually asleep (the world is stopped).

		// The exception is that if the goroutine is about to enter or might
		// have just exited a system call, it may be executing code such
		// as schedlock and may have needed to start a new stack segment.
		// Use the stack segment and stack pointer at the time of
		// the system call instead, since that won't change underfoot.
		if(gp->gcstack != nil) {
			sp = gp->gcstack;
			spsize = gp->gcstack_size;
			next_segment = gp->gcnext_segment;
			next_sp = gp->gcnext_sp;
			initial_sp = gp->gcinitial_sp;
		} else {
			sp = __splitstack_find_context(&gp->stack_context[0],
						       &spsize, &next_segment,
						       &next_sp, &initial_sp);
		}
	}
	if(sp != nil) {
		enqueue1(wbufp, (Obj){sp, spsize, 0});
		while((sp = __splitstack_find(next_segment, next_sp,
					      &spsize, &next_segment,
					      &next_sp, &initial_sp)) != nil)
			enqueue1(wbufp, (Obj){sp, spsize, 0});
	}
#else
	M *mp;
	byte* bottom;
	byte* top;

	if(gp == runtime_g()) {
		// Scanning our own stack.
		bottom = (byte*)&gp;
	} else if((mp = gp->m) != nil && mp->helpgc) {
		// gchelper's stack is in active use and has no interesting pointers.
		return;
	} else {
		// Scanning another goroutine's stack.
		// The goroutine is usually asleep (the world is stopped).
		bottom = (byte*)gp->gcnext_sp;
		if(bottom == nil)
			return;
	}
	top = (byte*)gp->gcinitial_sp + gp->gcstack_size;
	if(top > bottom)
		enqueue1(wbufp, (Obj){bottom, top - bottom, 0});
	else
		enqueue1(wbufp, (Obj){top, bottom - top, 0});
#endif
}

void
runtime_queuefinalizer(void *p, FuncVal *fn, const FuncType *ft, const PtrType *ot)
{
	FinBlock *block;
	Finalizer *f;

	runtime_lock(&gclock);
	if(finq == nil || finq->cnt == finq->cap) {
		if(finc == nil) {
			finc = runtime_persistentalloc(FinBlockSize, 0, &mstats.gc_sys);
			finc->cap = (FinBlockSize - sizeof(FinBlock)) / sizeof(Finalizer) + 1;
			finc->alllink = allfin;
			allfin = finc;
		}
		block = finc;
		finc = block->next;
		block->next = finq;
		finq = block;
	}
	f = &finq->fin[finq->cnt];
	finq->cnt++;
	f->fn = fn;
	f->ft = ft;
	f->ot = ot;
	f->arg = p;
	runtime_unlock(&gclock);
}

void
runtime_MSpan_EnsureSwept(MSpan *s)
{
	M *m = runtime_m();
	uint32 sg;

	sg = runtime_mheap.sweepgen;
	if(runtime_atomicload(&s->sweepgen) == sg)
		return;
	m->locks++;
	if(runtime_cas(&s->sweepgen, sg-2, sg-1)) {
		runtime_MSpan_Sweep(s);
		m->locks--;
		return;
	}
	m->locks--;
	// unfortunate condition, and we don't have efficient means to wait
	while(runtime_atomicload(&s->sweepgen) != sg)
		runtime_osyield();  
}

// Sweep frees or collects finalizers for blocks not marked in the mark phase.
// It clears the mark bits in preparation for the next GC round.
// Returns true if the span was returned to heap.
bool
runtime_MSpan_Sweep(MSpan *s)
{
	M *m;
	int32 cl, n, npages, nfree;
	uintptr size, off, *bitp, shift, bits;
	uint32 sweepgen;
	byte *p;
	MCache *c;
	byte *arena_start;
	MLink head, *end;
	byte *type_data;
	byte compression;
	uintptr type_data_inc;
	MLink *x;
	Special *special, **specialp, *y;
	bool res, sweepgenset;

	m = runtime_m();

	// It's critical that we enter this function with preemption disabled,
	// GC must not start while we are in the middle of this function.
	if(m->locks == 0 && m->mallocing == 0 && runtime_g() != m->g0)
		runtime_throw("MSpan_Sweep: m is not locked");
	sweepgen = runtime_mheap.sweepgen;
	if(s->state != MSpanInUse || s->sweepgen != sweepgen-1) {
		runtime_printf("MSpan_Sweep: state=%d sweepgen=%d mheap.sweepgen=%d\n",
			s->state, s->sweepgen, sweepgen);
		runtime_throw("MSpan_Sweep: bad span state");
	}
	arena_start = runtime_mheap.arena_start;
	cl = s->sizeclass;
	size = s->elemsize;
	if(cl == 0) {
		n = 1;
	} else {
		// Chunk full of small blocks.
		npages = runtime_class_to_allocnpages[cl];
		n = (npages << PageShift) / size;
	}
	res = false;
	nfree = 0;
	end = &head;
	c = m->mcache;
	sweepgenset = false;

	// mark any free objects in this span so we don't collect them
	for(x = s->freelist; x != nil; x = x->next) {
		// This is markonly(x) but faster because we don't need
		// atomic access and we're guaranteed to be pointing at
		// the head of a valid object.
		off = (uintptr*)x - (uintptr*)runtime_mheap.arena_start;
		bitp = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
		shift = off % wordsPerBitmapWord;
		*bitp |= bitMarked<<shift;
	}
	
	// Unlink & free special records for any objects we're about to free.
	specialp = &s->specials;
	special = *specialp;
	while(special != nil) {
		// A finalizer can be set for an inner byte of an object, find object beginning.
		p = (byte*)(s->start << PageShift) + special->offset/size*size;
		off = (uintptr*)p - (uintptr*)arena_start;
		bitp = (uintptr*)arena_start - off/wordsPerBitmapWord - 1;
		shift = off % wordsPerBitmapWord;
		bits = *bitp>>shift;
		if((bits & (bitAllocated|bitMarked)) == bitAllocated) {
			// Find the exact byte for which the special was setup
			// (as opposed to object beginning).
			p = (byte*)(s->start << PageShift) + special->offset;
			// about to free object: splice out special record
			y = special;
			special = special->next;
			*specialp = special;
			if(!runtime_freespecial(y, p, size, false)) {
				// stop freeing of object if it has a finalizer
				*bitp |= bitMarked << shift;
			}
		} else {
			// object is still live: keep special record
			specialp = &special->next;
			special = *specialp;
		}
	}

	type_data = (byte*)s->types.data;
	type_data_inc = sizeof(uintptr);
	compression = s->types.compression;
	switch(compression) {
	case MTypes_Bytes:
		type_data += 8*sizeof(uintptr);
		type_data_inc = 1;
		break;
	}

	// Sweep through n objects of given size starting at p.
	// This thread owns the span now, so it can manipulate
	// the block bitmap without atomic operations.
	p = (byte*)(s->start << PageShift);
	for(; n > 0; n--, p += size, type_data+=type_data_inc) {
		off = (uintptr*)p - (uintptr*)arena_start;
		bitp = (uintptr*)arena_start - off/wordsPerBitmapWord - 1;
		shift = off % wordsPerBitmapWord;
		bits = *bitp>>shift;

		if((bits & bitAllocated) == 0)
			continue;

		if((bits & bitMarked) != 0) {
			*bitp &= ~(bitMarked<<shift);
			continue;
		}

		// Clear mark, scan, and special bits.
		*bitp &= ~((bitScan|bitMarked|bitSpecial)<<shift);

		if(cl == 0) {
			// Free large span.
			runtime_unmarkspan(p, 1<<PageShift);
			s->needzero = 1;
			// important to set sweepgen before returning it to heap
			runtime_atomicstore(&s->sweepgen, sweepgen);
			sweepgenset = true;
			if(runtime_debug.efence)
				runtime_SysFree(p, size, &mstats.gc_sys);
			else
				runtime_MHeap_Free(&runtime_mheap, s, 1);
			c->local_nlargefree++;
			c->local_largefree += size;
			runtime_xadd64(&mstats.next_gc, -(uint64)(size * (gcpercent + 100)/100));
			res = true;
		} else {
			// Free small object.
			switch(compression) {
			case MTypes_Words:
				*(uintptr*)type_data = 0;
				break;
			case MTypes_Bytes:
				*(byte*)type_data = 0;
				break;
			}
			if(size > 2*sizeof(uintptr))
				((uintptr*)p)[1] = (uintptr)0xdeaddeaddeaddeadll;	// mark as "needs to be zeroed"
			else if(size > sizeof(uintptr))
				((uintptr*)p)[1] = 0;

			end->next = (MLink*)p;
			end = (MLink*)p;
			nfree++;
		}
	}

	if(!sweepgenset) {
		// The span must be in our exclusive ownership until we update sweepgen,
		// check for potential races.
		if(s->state != MSpanInUse || s->sweepgen != sweepgen-1) {
			runtime_printf("MSpan_Sweep: state=%d sweepgen=%d mheap.sweepgen=%d\n",
				s->state, s->sweepgen, sweepgen);
			runtime_throw("MSpan_Sweep: bad span state after sweep");
		}
		runtime_atomicstore(&s->sweepgen, sweepgen);
	}
	if(nfree) {
		c->local_nsmallfree[cl] += nfree;
		c->local_cachealloc -= nfree * size;
		runtime_xadd64(&mstats.next_gc, -(uint64)(nfree * size * (gcpercent + 100)/100));
		res = runtime_MCentral_FreeSpan(&runtime_mheap.central[cl], s, nfree, head.next, end);
	}
	return res;
}

// State of background sweep.
// Pretected by gclock.
static struct
{
	G*	g;
	bool	parked;

	MSpan**	spans;
	uint32	nspan;
	uint32	spanidx;
} sweep;

// background sweeping goroutine
static void
bgsweep(void* dummy __attribute__ ((unused)))
{
	runtime_g()->issystem = 1;
	for(;;) {
		while(runtime_sweepone() != (uintptr)-1) {
			gcstats.nbgsweep++;
			runtime_gosched();
		}
		runtime_lock(&gclock);
		if(finq != nil) {
			// kick off or wake up goroutine to run queued finalizers
			if(fing == nil)
				fing = __go_go(runfinq, nil);
			else if(fingwait) {
				fingwait = 0;
				runtime_ready(fing);
			}
		}
		sweep.parked = true;
		runtime_parkunlock(&gclock, "GC sweep wait");
	}
}

// sweeps one span
// returns number of pages returned to heap, or -1 if there is nothing to sweep
uintptr
runtime_sweepone(void)
{
	M *m = runtime_m();
	MSpan *s;
	uint32 idx, sg;
	uintptr npages;

	// increment locks to ensure that the goroutine is not preempted
	// in the middle of sweep thus leaving the span in an inconsistent state for next GC
	m->locks++;
	sg = runtime_mheap.sweepgen;
	for(;;) {
		idx = runtime_xadd(&sweep.spanidx, 1) - 1;
		if(idx >= sweep.nspan) {
			runtime_mheap.sweepdone = true;
			m->locks--;
			return (uintptr)-1;
		}
		s = sweep.spans[idx];
		if(s->state != MSpanInUse) {
			s->sweepgen = sg;
			continue;
		}
		if(s->sweepgen != sg-2 || !runtime_cas(&s->sweepgen, sg-2, sg-1))
			continue;
		npages = s->npages;
		if(!runtime_MSpan_Sweep(s))
			npages = 0;
		m->locks--;
		return npages;
	}
}

static void
dumpspan(uint32 idx)
{
	int32 sizeclass, n, npages, i, column;
	uintptr size;
	byte *p;
	byte *arena_start;
	MSpan *s;
	bool allocated, special;

	s = runtime_mheap.allspans[idx];
	if(s->state != MSpanInUse)
		return;
	arena_start = runtime_mheap.arena_start;
	p = (byte*)(s->start << PageShift);
	sizeclass = s->sizeclass;
	size = s->elemsize;
	if(sizeclass == 0) {
		n = 1;
	} else {
		npages = runtime_class_to_allocnpages[sizeclass];
		n = (npages << PageShift) / size;
	}
	
	runtime_printf("%p .. %p:\n", p, p+n*size);
	column = 0;
	for(; n>0; n--, p+=size) {
		uintptr off, *bitp, shift, bits;

		off = (uintptr*)p - (uintptr*)arena_start;
		bitp = (uintptr*)arena_start - off/wordsPerBitmapWord - 1;
		shift = off % wordsPerBitmapWord;
		bits = *bitp>>shift;

		allocated = ((bits & bitAllocated) != 0);
		special = ((bits & bitSpecial) != 0);

		for(i=0; (uint32)i<size; i+=sizeof(void*)) {
			if(column == 0) {
				runtime_printf("\t");
			}
			if(i == 0) {
				runtime_printf(allocated ? "(" : "[");
				runtime_printf(special ? "@" : "");
				runtime_printf("%p: ", p+i);
			} else {
				runtime_printf(" ");
			}

			runtime_printf("%p", *(void**)(p+i));

			if(i+sizeof(void*) >= size) {
				runtime_printf(allocated ? ") " : "] ");
			}

			column++;
			if(column == 8) {
				runtime_printf("\n");
				column = 0;
			}
		}
	}
	runtime_printf("\n");
}

// A debugging function to dump the contents of memory
void
runtime_memorydump(void)
{
	uint32 spanidx;

	for(spanidx=0; spanidx<runtime_mheap.nspan; spanidx++) {
		dumpspan(spanidx);
	}
}

void
runtime_gchelper(void)
{
	uint32 nproc;

	gchelperstart();

	// parallel mark for over gc roots
	runtime_parfordo(work.markfor);

	// help other threads scan secondary blocks
	scanblock(nil, true);

	bufferList[runtime_m()->helpgc].busy = 0;
	nproc = work.nproc;  // work.nproc can change right after we increment work.ndone
	if(runtime_xadd(&work.ndone, +1) == nproc-1)
		runtime_notewakeup(&work.alldone);
}

static void
cachestats(void)
{
	MCache *c;
	P *p, **pp;

	for(pp=runtime_allp; (p=*pp) != nil; pp++) {
		c = p->mcache;
		if(c==nil)
			continue;
		runtime_purgecachedstats(c);
	}
}

static void
flushallmcaches(void)
{
	P *p, **pp;
	MCache *c;

	// Flush MCache's to MCentral.
	for(pp=runtime_allp; (p=*pp) != nil; pp++) {
		c = p->mcache;
		if(c==nil)
			continue;
		runtime_MCache_ReleaseAll(c);
	}
}

static void
updatememstats(GCStats *stats)
{
	M *mp;
	MSpan *s;
	uint32 i;
	uint64 stacks_inuse, smallfree;
	uint64 *src, *dst;

	if(stats)
		runtime_memclr((byte*)stats, sizeof(*stats));
	stacks_inuse = 0;
	for(mp=runtime_allm; mp; mp=mp->alllink) {
		//stacks_inuse += mp->stackinuse*FixedStack;
		if(stats) {
			src = (uint64*)&mp->gcstats;
			dst = (uint64*)stats;
			for(i=0; i<sizeof(*stats)/sizeof(uint64); i++)
				dst[i] += src[i];
			runtime_memclr((byte*)&mp->gcstats, sizeof(mp->gcstats));
		}
	}
	mstats.stacks_inuse = stacks_inuse;
	mstats.mcache_inuse = runtime_mheap.cachealloc.inuse;
	mstats.mspan_inuse = runtime_mheap.spanalloc.inuse;
	mstats.sys = mstats.heap_sys + mstats.stacks_sys + mstats.mspan_sys +
		mstats.mcache_sys + mstats.buckhash_sys + mstats.gc_sys + mstats.other_sys;
	
	// Calculate memory allocator stats.
	// During program execution we only count number of frees and amount of freed memory.
	// Current number of alive object in the heap and amount of alive heap memory
	// are calculated by scanning all spans.
	// Total number of mallocs is calculated as number of frees plus number of alive objects.
	// Similarly, total amount of allocated memory is calculated as amount of freed memory
	// plus amount of alive heap memory.
	mstats.alloc = 0;
	mstats.total_alloc = 0;
	mstats.nmalloc = 0;
	mstats.nfree = 0;
	for(i = 0; i < nelem(mstats.by_size); i++) {
		mstats.by_size[i].nmalloc = 0;
		mstats.by_size[i].nfree = 0;
	}

	// Flush MCache's to MCentral.
	flushallmcaches();

	// Aggregate local stats.
	cachestats();

	// Scan all spans and count number of alive objects.
	for(i = 0; i < runtime_mheap.nspan; i++) {
		s = runtime_mheap.allspans[i];
		if(s->state != MSpanInUse)
			continue;
		if(s->sizeclass == 0) {
			mstats.nmalloc++;
			mstats.alloc += s->elemsize;
		} else {
			mstats.nmalloc += s->ref;
			mstats.by_size[s->sizeclass].nmalloc += s->ref;
			mstats.alloc += s->ref*s->elemsize;
		}
	}

	// Aggregate by size class.
	smallfree = 0;
	mstats.nfree = runtime_mheap.nlargefree;
	for(i = 0; i < nelem(mstats.by_size); i++) {
		mstats.nfree += runtime_mheap.nsmallfree[i];
		mstats.by_size[i].nfree = runtime_mheap.nsmallfree[i];
		mstats.by_size[i].nmalloc += runtime_mheap.nsmallfree[i];
		smallfree += runtime_mheap.nsmallfree[i] * runtime_class_to_size[i];
	}
	mstats.nmalloc += mstats.nfree;

	// Calculate derived stats.
	mstats.total_alloc = mstats.alloc + runtime_mheap.largefree + smallfree;
	mstats.heap_alloc = mstats.alloc;
	mstats.heap_objects = mstats.nmalloc - mstats.nfree;
}

// Structure of arguments passed to function gc().
// This allows the arguments to be passed via runtime_mcall.
struct gc_args
{
	int64 start_time; // start time of GC in ns (just before stoptheworld)
};

static void gc(struct gc_args *args);
static void mgc(G *gp);

static int32
readgogc(void)
{
	const byte *p;

	p = runtime_getenv("GOGC");
	if(p == nil || p[0] == '\0')
		return 100;
	if(runtime_strcmp((const char *)p, "off") == 0)
		return -1;
	return runtime_atoi(p);
}

void
runtime_gc(int32 force)
{
	M *m;
	G *g;
	struct gc_args a;
	int32 i;

	// The atomic operations are not atomic if the uint64s
	// are not aligned on uint64 boundaries. This has been
	// a problem in the past.
	if((((uintptr)&work.empty) & 7) != 0)
		runtime_throw("runtime: gc work buffer is misaligned");
	if((((uintptr)&work.full) & 7) != 0)
		runtime_throw("runtime: gc work buffer is misaligned");

	// Make sure all registers are saved on stack so that
	// scanstack sees them.
	__builtin_unwind_init();

	// The gc is turned off (via enablegc) until
	// the bootstrap has completed.
	// Also, malloc gets called in the guts
	// of a number of libraries that might be
	// holding locks.  To avoid priority inversion
	// problems, don't bother trying to run gc
	// while holding a lock.  The next mallocgc
	// without a lock will do the gc instead.
	m = runtime_m();
	if(!mstats.enablegc || runtime_g() == m->g0 || m->locks > 0 || runtime_panicking)
		return;

	if(gcpercent == GcpercentUnknown) {	// first time through
		runtime_lock(&runtime_mheap);
		if(gcpercent == GcpercentUnknown)
			gcpercent = readgogc();
		runtime_unlock(&runtime_mheap);
	}
	if(gcpercent < 0)
		return;

	runtime_semacquire(&runtime_worldsema, false);
	if(!force && mstats.heap_alloc < mstats.next_gc) {
		// typically threads which lost the race to grab
		// worldsema exit here when gc is done.
		runtime_semrelease(&runtime_worldsema);
		return;
	}

	// Ok, we're doing it!  Stop everybody else
	a.start_time = runtime_nanotime();
	m->gcing = 1;
	runtime_stoptheworld();
	
	if(runtime_debug.allocfreetrace)
		runtime_MProf_TraceGC();

	clearpools();

	// Run gc on the g0 stack.  We do this so that the g stack
	// we're currently running on will no longer change.  Cuts
	// the root set down a bit (g0 stacks are not scanned, and
	// we don't need to scan gc's internal state).  Also an
	// enabler for copyable stacks.
	for(i = 0; i < (runtime_debug.gctrace > 1 ? 2 : 1); i++) {
		// switch to g0, call gc(&a), then switch back
		g = runtime_g();
		g->param = &a;
		g->status = Gwaiting;
		g->waitreason = "garbage collection";
		runtime_mcall(mgc);
		// record a new start time in case we're going around again
		a.start_time = runtime_nanotime();
	}

	// all done
	m->gcing = 0;
	m->locks++;
	runtime_semrelease(&runtime_worldsema);
	runtime_starttheworld();
	m->locks--;

	// now that gc is done, kick off finalizer thread if needed
	if(!ConcurrentSweep) {
		if(finq != nil) {
			runtime_lock(&gclock);
			// kick off or wake up goroutine to run queued finalizers
			if(fing == nil)
				fing = __go_go(runfinq, nil);
			else if(fingwait) {
				fingwait = 0;
				runtime_ready(fing);
			}
			runtime_unlock(&gclock);
		}
		// give the queued finalizers, if any, a chance to run
		runtime_gosched();
	} else {
		// For gccgo, let other goroutines run.
		runtime_gosched();
	}
}

static void
mgc(G *gp)
{
	gc(gp->param);
	gp->param = nil;
	gp->status = Grunning;
	runtime_gogo(gp);
}

static void
gc(struct gc_args *args)
{
	M *m;
	int64 t0, t1, t2, t3, t4;
	uint64 heap0, heap1, obj, ninstr;
	GCStats stats;
	M *mp;
	uint32 i;
	// Eface eface;

	m = runtime_m();

	t0 = args->start_time;
	work.tstart = args->start_time; 

	if(CollectStats)
		runtime_memclr((byte*)&gcstats, sizeof(gcstats));

	for(mp=runtime_allm; mp; mp=mp->alllink)
		runtime_settype_flush(mp);

	m->locks++;	// disable gc during mallocs in parforalloc
	if(work.markfor == nil)
		work.markfor = runtime_parforalloc(MaxGcproc);
	m->locks--;

	if(itabtype == nil) {
		// get C pointer to the Go type "itab"
		// runtime_gc_itab_ptr(&eface);
		// itabtype = ((PtrType*)eface.__type_descriptor)->elem;
	}

	t1 = runtime_nanotime();

	// Sweep what is not sweeped by bgsweep.
	while(runtime_sweepone() != (uintptr)-1)
		gcstats.npausesweep++;

	work.nwait = 0;
	work.ndone = 0;
	work.nproc = runtime_gcprocs();
	runtime_parforsetup(work.markfor, work.nproc, RootCount + runtime_allglen, nil, false, markroot);
	if(work.nproc > 1) {
		runtime_noteclear(&work.alldone);
		runtime_helpgc(work.nproc);
	}

	t2 = runtime_nanotime();

	gchelperstart();
	runtime_parfordo(work.markfor);
	scanblock(nil, true);

	t3 = runtime_nanotime();

	bufferList[m->helpgc].busy = 0;
	if(work.nproc > 1)
		runtime_notesleep(&work.alldone);

	cachestats();
	// next_gc calculation is tricky with concurrent sweep since we don't know size of live heap
	// estimate what was live heap size after previous GC (for tracing only)
	heap0 = mstats.next_gc*100/(gcpercent+100);
	// conservatively set next_gc to high value assuming that everything is live
	// concurrent/lazy sweep will reduce this number while discovering new garbage
	mstats.next_gc = mstats.heap_alloc+mstats.heap_alloc*gcpercent/100;

	t4 = runtime_nanotime();
	mstats.last_gc = t4;
	mstats.pause_ns[mstats.numgc%nelem(mstats.pause_ns)] = t4 - t0;
	mstats.pause_total_ns += t4 - t0;
	mstats.numgc++;
	if(mstats.debuggc)
		runtime_printf("pause %D\n", t4-t0);

	if(runtime_debug.gctrace) {
		updatememstats(&stats);
		heap1 = mstats.heap_alloc;
		obj = mstats.nmalloc - mstats.nfree;

		stats.nprocyield += work.markfor->nprocyield;
		stats.nosyield += work.markfor->nosyield;
		stats.nsleep += work.markfor->nsleep;

		runtime_printf("gc%d(%d): %D+%D+%D ms, %D -> %D MB, %D (%D-%D) objects,"
				" %d/%d/%d sweeps,"
				" %D(%D) handoff, %D(%D) steal, %D/%D/%D yields\n",
			mstats.numgc, work.nproc, (t3-t2)/1000000, (t2-t1)/1000000, (t1-t0+t4-t3)/1000000,
			heap0>>20, heap1>>20, obj,
			mstats.nmalloc, mstats.nfree,
			sweep.nspan, gcstats.nbgsweep, gcstats.npausesweep,
			stats.nhandoff, stats.nhandoffcnt,
			work.markfor->nsteal, work.markfor->nstealcnt,
			stats.nprocyield, stats.nosyield, stats.nsleep);
		gcstats.nbgsweep = gcstats.npausesweep = 0;
		if(CollectStats) {
			runtime_printf("scan: %D bytes, %D objects, %D untyped, %D types from MSpan\n",
				gcstats.nbytes, gcstats.obj.cnt, gcstats.obj.notype, gcstats.obj.typelookup);
			if(gcstats.ptr.cnt != 0)
				runtime_printf("avg ptrbufsize: %D (%D/%D)\n",
					gcstats.ptr.sum/gcstats.ptr.cnt, gcstats.ptr.sum, gcstats.ptr.cnt);
			if(gcstats.obj.cnt != 0)
				runtime_printf("avg nobj: %D (%D/%D)\n",
					gcstats.obj.sum/gcstats.obj.cnt, gcstats.obj.sum, gcstats.obj.cnt);
			runtime_printf("rescans: %D, %D bytes\n", gcstats.rescan, gcstats.rescanbytes);

			runtime_printf("instruction counts:\n");
			ninstr = 0;
			for(i=0; i<nelem(gcstats.instr); i++) {
				runtime_printf("\t%d:\t%D\n", i, gcstats.instr[i]);
				ninstr += gcstats.instr[i];
			}
			runtime_printf("\ttotal:\t%D\n", ninstr);

			runtime_printf("putempty: %D, getfull: %D\n", gcstats.putempty, gcstats.getfull);

			runtime_printf("markonly base lookup: bit %D word %D span %D\n", gcstats.markonly.foundbit, gcstats.markonly.foundword, gcstats.markonly.foundspan);
			runtime_printf("flushptrbuf base lookup: bit %D word %D span %D\n", gcstats.flushptrbuf.foundbit, gcstats.flushptrbuf.foundword, gcstats.flushptrbuf.foundspan);
		}
	}

	// We cache current runtime_mheap.allspans array in sweep.spans,
	// because the former can be resized and freed.
	// Otherwise we would need to take heap lock every time
	// we want to convert span index to span pointer.

	// Free the old cached array if necessary.
	if(sweep.spans && sweep.spans != runtime_mheap.allspans)
		runtime_SysFree(sweep.spans, sweep.nspan*sizeof(sweep.spans[0]), &mstats.other_sys);
	// Cache the current array.
	runtime_mheap.sweepspans = runtime_mheap.allspans;
	runtime_mheap.sweepgen += 2;
	runtime_mheap.sweepdone = false;
	sweep.spans = runtime_mheap.allspans;
	sweep.nspan = runtime_mheap.nspan;
	sweep.spanidx = 0;

	// Temporary disable concurrent sweep, because we see failures on builders.
	if(ConcurrentSweep) {
		runtime_lock(&gclock);
		if(sweep.g == nil)
			sweep.g = __go_go(bgsweep, nil);
		else if(sweep.parked) {
			sweep.parked = false;
			runtime_ready(sweep.g);
		}
		runtime_unlock(&gclock);
	} else {
		// Sweep all spans eagerly.
		while(runtime_sweepone() != (uintptr)-1)
			gcstats.npausesweep++;
	}

	runtime_MProf_GC();
}

extern uintptr runtime_sizeof_C_MStats
  __asm__ (GOSYM_PREFIX "runtime.Sizeof_C_MStats");

void runtime_ReadMemStats(MStats *)
  __asm__ (GOSYM_PREFIX "runtime.ReadMemStats");

void
runtime_ReadMemStats(MStats *stats)
{
	M *m;

	// Have to acquire worldsema to stop the world,
	// because stoptheworld can only be used by
	// one goroutine at a time, and there might be
	// a pending garbage collection already calling it.
	runtime_semacquire(&runtime_worldsema, false);
	m = runtime_m();
	m->gcing = 1;
	runtime_stoptheworld();
	updatememstats(nil);
	// Size of the trailing by_size array differs between Go and C,
	// NumSizeClasses was changed, but we can not change Go struct because of backward compatibility.
	runtime_memmove(stats, &mstats, runtime_sizeof_C_MStats);
	m->gcing = 0;
	m->locks++;
	runtime_semrelease(&runtime_worldsema);
	runtime_starttheworld();
	m->locks--;
}

void runtime_debug_readGCStats(Slice*)
  __asm__("runtime_debug.readGCStats");

void
runtime_debug_readGCStats(Slice *pauses)
{
	uint64 *p;
	uint32 i, n;

	// Calling code in runtime/debug should make the slice large enough.
	if((size_t)pauses->cap < nelem(mstats.pause_ns)+3)
		runtime_throw("runtime: short slice passed to readGCStats");

	// Pass back: pauses, last gc (absolute time), number of gc, total pause ns.
	p = (uint64*)pauses->array;
	runtime_lock(&runtime_mheap);
	n = mstats.numgc;
	if(n > nelem(mstats.pause_ns))
		n = nelem(mstats.pause_ns);
	
	// The pause buffer is circular. The most recent pause is at
	// pause_ns[(numgc-1)%nelem(pause_ns)], and then backward
	// from there to go back farther in time. We deliver the times
	// most recent first (in p[0]).
	for(i=0; i<n; i++)
		p[i] = mstats.pause_ns[(mstats.numgc-1-i)%nelem(mstats.pause_ns)];

	p[n] = mstats.last_gc;
	p[n+1] = mstats.numgc;
	p[n+2] = mstats.pause_total_ns;	
	runtime_unlock(&runtime_mheap);
	pauses->__count = n+3;
}

intgo runtime_debug_setGCPercent(intgo)
  __asm__("runtime_debug.setGCPercent");

intgo
runtime_debug_setGCPercent(intgo in)
{
	intgo out;

	runtime_lock(&runtime_mheap);
	if(gcpercent == GcpercentUnknown)
		gcpercent = readgogc();
	out = gcpercent;
	if(in < 0)
		in = -1;
	gcpercent = in;
	runtime_unlock(&runtime_mheap);
	return out;
}

static void
gchelperstart(void)
{
	M *m;

	m = runtime_m();
	if(m->helpgc < 0 || m->helpgc >= MaxGcproc)
		runtime_throw("gchelperstart: bad m->helpgc");
	if(runtime_xchg(&bufferList[m->helpgc].busy, 1))
		runtime_throw("gchelperstart: already busy");
	if(runtime_g() != m->g0)
		runtime_throw("gchelper not running on g0 stack");
}

static void
runfinq(void* dummy __attribute__ ((unused)))
{
	Finalizer *f;
	FinBlock *fb, *next;
	uint32 i;
	Eface ef;
	Iface iface;

	for(;;) {
		runtime_lock(&gclock);
		fb = finq;
		finq = nil;
		if(fb == nil) {
			fingwait = 1;
			runtime_parkunlock(&gclock, "finalizer wait");
			continue;
		}
		runtime_unlock(&gclock);
		if(raceenabled)
			runtime_racefingo();
		for(; fb; fb=next) {
			next = fb->next;
			for(i=0; i<(uint32)fb->cnt; i++) {
				const Type *fint;
				void *param;

				f = &fb->fin[i];
				fint = ((const Type**)f->ft->__in.array)[0];
				if(fint->__code == KindPtr) {
					// direct use of pointer
					param = &f->arg;
				} else if(((const InterfaceType*)fint)->__methods.__count == 0) {
					// convert to empty interface
					ef.__type_descriptor = (const Type*)f->ot;
					ef.__object = f->arg;
					param = &ef;
				} else {
					// convert to interface with methods
					iface.__methods = __go_convert_interface_2((const Type*)fint,
										   (const Type*)f->ot,
										   1);
					iface.__object = f->arg;
					if(iface.__methods == nil)
						runtime_throw("invalid type conversion in runfinq");
					param = &iface;
				}
				reflect_call(f->ft, f->fn, 0, 0, &param, nil);
				f->fn = nil;
				f->arg = nil;
				f->ot = nil;
			}
			fb->cnt = 0;
			fb->next = finc;
			finc = fb;
		}
		runtime_gc(1);	// trigger another gc to clean up the finalized objects, if possible
	}
}

void
runtime_marknogc(void *v)
{
	uintptr *b, obits, bits, off, shift;

	off = (uintptr*)v - (uintptr*)runtime_mheap.arena_start;  // word offset
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;

	for(;;) {
		obits = *b;
		if((obits>>shift & bitMask) != bitAllocated)
			runtime_throw("bad initial state for marknogc");
		bits = (obits & ~(bitAllocated<<shift)) | bitBlockBoundary<<shift;
		if(runtime_gomaxprocs == 1) {
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
runtime_markscan(void *v)
{
	uintptr *b, obits, bits, off, shift;

	off = (uintptr*)v - (uintptr*)runtime_mheap.arena_start;  // word offset
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;

	for(;;) {
		obits = *b;
		if((obits>>shift & bitMask) != bitAllocated)
			runtime_throw("bad initial state for markscan");
		bits = obits | bitScan<<shift;
		if(runtime_gomaxprocs == 1) {
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

	if(0)
		runtime_printf("markfreed %p+%p\n", v, n);

	if((byte*)v+n > (byte*)runtime_mheap.arena_used || (byte*)v < runtime_mheap.arena_start)
		runtime_throw("markfreed: bad pointer");

	off = (uintptr*)v - (uintptr*)runtime_mheap.arena_start;  // word offset
	b = (uintptr*)runtime_mheap.arena_start - off/wordsPerBitmapWord - 1;
	shift = off % wordsPerBitmapWord;

	for(;;) {
		obits = *b;
		// This could be a free of a gc-eligible object (bitAllocated + others) or
		// a FlagNoGC object (bitBlockBoundary set).  In either case, we revert to
		// a simple no-scan allocated object because it is going on a free list.
		bits = (obits & ~(bitMask<<shift)) | (bitAllocated<<shift);
		if(runtime_gomaxprocs == 1) {
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
			v, n, off, bits & bitMask);
		runtime_throw("checkfreed: not freed");
	}
}

// mark the span of memory at v as having n blocks of the given size.
// if leftover is true, there is left over space at the end of the span.
void
runtime_markspan(void *v, uintptr size, uintptr n, bool leftover)
{
	uintptr *b, off, shift, i;
	byte *p;

	if((byte*)v+size*n > (byte*)runtime_mheap.arena_used || (byte*)v < runtime_mheap.arena_start)
		runtime_throw("markspan: bad pointer");

	if(runtime_checking) {
		// bits should be all zero at the start
		off = (byte*)v + size - runtime_mheap.arena_start;
		b = (uintptr*)(runtime_mheap.arena_start - off/wordsPerBitmapWord);
		for(i = 0; i < size/PtrSize/wordsPerBitmapWord; i++) {
			if(b[i] != 0)
				runtime_throw("markspan: span bits not zero");
		}
	}

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
		*b = (*b & ~(bitMask<<shift)) | (bitAllocated<<shift);
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

void
runtime_MHeap_MapBits(MHeap *h)
{
	size_t page_size;

	// Caller has added extra mappings to the arena.
	// Add extra mappings of bitmap words as needed.
	// We allocate extra bitmap pieces in chunks of bitmapChunk.
	enum {
		bitmapChunk = 8192
	};
	uintptr n;

	n = (h->arena_used - h->arena_start) / wordsPerBitmapWord;
	n = ROUND(n, bitmapChunk);
	if(h->bitmap_mapped >= n)
		return;

	page_size = getpagesize();
	n = (n+page_size-1) & ~(page_size-1);

	runtime_SysMap(h->arena_start - n, n - h->bitmap_mapped, &mstats.gc_sys);
	h->bitmap_mapped = n;
}
