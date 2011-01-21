// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Garbage collector -- step 0.
//
// Stop the world, mark and sweep garbage collector.
// NOT INTENDED FOR PRODUCTION USE.
//
// A mark and sweep collector provides a way to exercise
// and test the memory allocator and the stack walking machinery
// without also needing to get reference counting
// exactly right.

#include "runtime.h"
#include "malloc.h"

enum {
	Debug = 0
};

typedef struct BlockList BlockList;
struct BlockList
{
	byte *obj;
	uintptr size;
};

static bool finstarted;
static pthread_mutex_t finqlock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t finqcond = PTHREAD_COND_INITIALIZER;
static Finalizer *finq;
static int32 fingwait;
static BlockList *bl, *ebl;

static void runfinq(void*);

enum {
	PtrSize = sizeof(void*)
};

static void
scanblock(byte *b, int64 n)
{
	int32 off;
	void *obj;
	uintptr size;
	uint32 *refp, ref;
	void **vp;
	int64 i;
	BlockList *w;

	w = bl;
	w->obj = b;
	w->size = n;
	w++;

	while(w > bl) {
		w--;
		b = w->obj;
		n = w->size;

		if(Debug > 1)
			runtime_printf("scanblock %p %lld\n", b, (long long) n);
		off = (uint32)(uintptr)b & (PtrSize-1);
		if(off) {
			b += PtrSize - off;
			n -= PtrSize - off;
		}
	
		vp = (void**)b;
		n /= PtrSize;
		for(i=0; i<n; i++) {
			obj = vp[i];
			if(obj == nil)
				continue;
			if(runtime_mheap.min <= (byte*)obj && (byte*)obj < runtime_mheap.max) {
				if(runtime_mlookup(obj, (byte**)&obj, &size, nil, &refp)) {
					ref = *refp;
					switch(ref & ~RefFlags) {
					case RefNone:
						if(Debug > 1)
							runtime_printf("found at %p: ", &vp[i]);
						*refp = RefSome | (ref & RefFlags);
						if(!(ref & RefNoPointers)) {
							if(w >= ebl)
								runtime_throw("scanblock: garbage collection stack overflow");
							w->obj = obj;
							w->size = size;
							w++;
						}
						break;
					}
				}
			}
		}
	}
}

static void
markfin(void *v)
{
	uintptr size;
	uint32 *refp;

	size = 0;
	refp = nil;
	if(!runtime_mlookup(v, (byte**)&v, &size, nil, &refp) || !(*refp & RefHasFinalizer))
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

static void
mark(void)
{
	uintptr blsize, nobj;
	struct root_list *pl;

	// Figure out how big an object stack we need.
	// Get a new one if we need more than we have
	// or we need significantly less than we have.
	nobj = mstats.heap_objects;
	if(nobj > (uintptr)(ebl - bl) || nobj < (uintptr)(ebl-bl)/4) {
		if(bl != nil)
			runtime_SysFree(bl, (byte*)ebl - (byte*)bl);
		
		// While we're allocated a new object stack,
		// add 20% headroom and also round up to
		// the nearest page boundary, since mmap
		// will anyway.
		nobj = nobj * 12/10;
		blsize = nobj * sizeof *bl;
		blsize = (blsize + 4095) & ~4095;
		nobj = blsize / sizeof *bl;
		bl = runtime_SysAlloc(blsize);
		ebl = bl + nobj;
	}

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

// free RefNone, free & queue finalizers for RefNone|RefHasFinalizer, reset RefSome
static void
sweepspan(MSpan *s)
{
	int32 n, npages, size;
	byte *p;
	uint32 ref, *gcrefp, *gcrefep;
	MCache *c;
	Finalizer *f;

	p = (byte*)(s->start << PageShift);
	if(s->sizeclass == 0) {
		// Large block.
		ref = s->gcref0;
		switch(ref & ~(RefFlags^RefHasFinalizer)) {
		case RefNone:
			// Free large object.
			mstats.alloc -= s->npages<<PageShift;
			mstats.nfree++;
			runtime_memclr(p, s->npages<<PageShift);
			if(ref & RefProfiled)
				runtime_MProf_Free(p, s->npages<<PageShift);
			s->gcref0 = RefFree;
			runtime_MHeap_Free(&runtime_mheap, s, 1);
			break;
		case RefNone|RefHasFinalizer:
			f = runtime_getfinalizer(p, 1);
			if(f == nil)
				runtime_throw("finalizer inconsistency");
			f->arg = p;
			f->next = finq;
			finq = f;
			ref &= ~RefHasFinalizer;
			// fall through
		case RefSome:
		case RefSome|RefHasFinalizer:
			s->gcref0 = RefNone | (ref&RefFlags);
			break;
		}
		return;
	}

	// Chunk full of small blocks.
	runtime_MGetSizeClassInfo(s->sizeclass, &size, &npages, &n);
	gcrefp = s->gcref;
	gcrefep = s->gcref + n;
	for(; gcrefp < gcrefep; gcrefp++, p += size) {
		ref = *gcrefp;
		if(ref < RefNone)	// RefFree or RefStack
			continue;
		switch(ref & ~(RefFlags^RefHasFinalizer)) {
		case RefNone:
			// Free small object.
			if(ref & RefProfiled)
				runtime_MProf_Free(p, size);
			*gcrefp = RefFree;
			c = m->mcache;
			if(size > (int32)sizeof(uintptr))
				((uintptr*)p)[1] = 1;	// mark as "needs to be zeroed"
			mstats.alloc -= size;
			mstats.nfree++;
			mstats.by_size[s->sizeclass].nfree++;
			runtime_MCache_Free(c, p, s->sizeclass, size);
			break;
		case RefNone|RefHasFinalizer:
			f = runtime_getfinalizer(p, 1);
			if(f == nil)
				runtime_throw("finalizer inconsistency");
			f->arg = p;
			f->next = finq;
			finq = f;
			ref &= ~RefHasFinalizer;
			// fall through
		case RefSome:
		case RefSome|RefHasFinalizer:
			*gcrefp = RefNone | (ref&RefFlags);
			break;
		}
	}
}

static void
sweep(void)
{
	MSpan *s;

	for(s = runtime_mheap.allspans; s != nil; s = s->allnext)
		if(s->state == MSpanInUse)
			sweepspan(s);
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
	int64 t0, t1;
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
	}
	if(gcpercent < 0)
		return;

	pthread_mutex_lock(&finqlock);
	pthread_mutex_lock(&gcsema);
	m->locks++;	// disable gc during the mallocs in newproc
	t0 = runtime_nanotime();
	runtime_stoptheworld();
	if(force || mstats.heap_alloc >= mstats.next_gc) {
		__go_cachestats();
		mark();
		sweep();
		__go_stealcache();
		mstats.next_gc = mstats.heap_alloc+mstats.heap_alloc*gcpercent/100;
	}

	t1 = runtime_nanotime();
	mstats.numgc++;
	mstats.pause_ns[mstats.numgc%nelem(mstats.pause_ns)] = t1 - t0;
	mstats.pause_total_ns += t1 - t0;
	if(mstats.debuggc)
		runtime_printf("pause %llu\n", (unsigned long long)t1-t0);
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
			reflect_call(f->ft, (void*)f->fn, 0, params, nil);
			f->fn = nil;
			f->arg = nil;
			f->next = nil;
			runtime_free(f);
		}
		runtime_gc(1);	// trigger another gc to clean up the finalized objects, if possible
	}
}

void
__go_enable_gc()
{
  mstats.enablegc = 1;
}
