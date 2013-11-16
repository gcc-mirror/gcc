// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "runtime.h"
#include "arch.h"
#include "go-type.h"
#include "race.h"
#include "malloc.h"

#define	NOSELGEN	1

typedef	struct	WaitQ	WaitQ;
typedef	struct	SudoG	SudoG;
typedef	struct	Select	Select;
typedef	struct	Scase	Scase;

typedef struct	__go_type_descriptor	Type;
typedef struct	__go_channel_type	ChanType;

struct	SudoG
{
	G*	g;		// g and selgen constitute
	uint32	selgen;		// a weak pointer to g
	SudoG*	link;
	int64	releasetime;
	byte*	elem;		// data element
};

struct	WaitQ
{
	SudoG*	first;
	SudoG*	last;
};

// The garbage collector is assuming that Hchan can only contain pointers into the stack
// and cannot contain pointers into the heap.
struct	Hchan
{
	uintgo	qcount;			// total data in the q
	uintgo	dataqsiz;		// size of the circular q
	uint16	elemsize;
	uint8	elemalign;
	uint8	pad;			// ensures proper alignment of the buffer that follows Hchan in memory
	bool	closed;
	uintgo	sendx;			// send index
	uintgo	recvx;			// receive index
	WaitQ	recvq;			// list of recv waiters
	WaitQ	sendq;			// list of send waiters
	Lock;
};

uint32 runtime_Hchansize = sizeof(Hchan);

// Buffer follows Hchan immediately in memory.
// chanbuf(c, i) is pointer to the i'th slot in the buffer.
#define chanbuf(c, i) ((byte*)((c)+1)+(uintptr)(c)->elemsize*(i))

enum
{
	debug = 0,

	// Scase.kind
	CaseRecv,
	CaseSend,
	CaseDefault,
};

struct	Scase
{
	SudoG	sg;			// must be first member (cast to Scase)
	Hchan*	chan;			// chan
	uint16	kind;
	uint16	index;			// index to return
	bool*	receivedp;		// pointer to received bool (recv2)
};

struct	Select
{
	uint16	tcase;			// total count of scase[]
	uint16	ncase;			// currently filled scase[]
	uint16*	pollorder;		// case poll order
	Hchan**	lockorder;		// channel lock order
	Scase	scase[1];		// one per case (in order of appearance)
};

static	void	dequeueg(WaitQ*);
static	SudoG*	dequeue(WaitQ*);
static	void	enqueue(WaitQ*, SudoG*);
static	void	racesync(Hchan*, SudoG*);

Hchan*
runtime_makechan_c(ChanType *t, int64 hint)
{
	Hchan *c;
	uintptr n;
	const Type *elem;

	elem = t->__element_type;

	// compiler checks this but be safe.
	if(elem->__size >= (1<<16))
		runtime_throw("makechan: invalid channel element type");

	if(hint < 0 || (intgo)hint != hint || (elem->__size > 0 && (uintptr)hint > MaxMem / elem->__size))
		runtime_panicstring("makechan: size out of range");

	n = sizeof(*c);
	n = ROUND(n, elem->__align);

	// allocate memory in one call
	c = (Hchan*)runtime_mallocgc(n + hint*elem->__size, (uintptr)t | TypeInfo_Chan, 0);
	c->elemsize = elem->__size;
	c->elemalign = elem->__align;
	c->dataqsiz = hint;

	if(debug)
		runtime_printf("makechan: chan=%p; elemsize=%D; dataqsiz=%D\n",
			c, (int64)elem->__size, (int64)c->dataqsiz);

	return c;
}

// For reflect
//	func makechan(typ *ChanType, size uint64) (chan)
uintptr reflect_makechan(ChanType *, uint64)
  __asm__ (GOSYM_PREFIX "reflect.makechan");

uintptr
reflect_makechan(ChanType *t, uint64 size)
{
	void *ret;
	Hchan *c;

	c = runtime_makechan_c(t, size);
	ret = runtime_mal(sizeof(void*));
	__builtin_memcpy(ret, &c, sizeof(void*));
	return (uintptr)ret;
}

// makechan(t *ChanType, hint int64) (hchan *chan any);
Hchan*
__go_new_channel(ChanType *t, uintptr hint)
{
	return runtime_makechan_c(t, hint);
}

Hchan*
__go_new_channel_big(ChanType *t, uint64 hint)
{
	return runtime_makechan_c(t, hint);
}

/*
 * generic single channel send/recv
 * if the bool pointer is nil,
 * then the full exchange will
 * occur. if pres is not nil,
 * then the protocol will not
 * sleep but return if it could
 * not complete.
 *
 * sleep can wake up with g->param == nil
 * when a channel involved in the sleep has
 * been closed.  it is easiest to loop and re-run
 * the operation; we'll see that it's now closed.
 */
void
runtime_chansend(ChanType *t, Hchan *c, byte *ep, bool *pres, void *pc)
{
	SudoG *sg;
	SudoG mysg;
	G* gp;
	int64 t0;
	G* g;

	g = runtime_g();

	if(c == nil) {
		USED(t);
		if(pres != nil) {
			*pres = false;
			return;
		}
		runtime_park(nil, nil, "chan send (nil chan)");
		return;  // not reached
	}

	if(runtime_gcwaiting())
		runtime_gosched();

	if(debug) {
		runtime_printf("chansend: chan=%p\n", c);
	}

	t0 = 0;
	mysg.releasetime = 0;
	if(runtime_blockprofilerate > 0) {
		t0 = runtime_cputicks();
		mysg.releasetime = -1;
	}

	runtime_lock(c);
	if(raceenabled)
		runtime_racereadpc(c, pc, runtime_chansend);
	if(c->closed)
		goto closed;

	if(c->dataqsiz > 0)
		goto asynch;

	sg = dequeue(&c->recvq);
	if(sg != nil) {
		if(raceenabled)
			racesync(c, sg);
		runtime_unlock(c);

		gp = sg->g;
		gp->param = sg;
		if(sg->elem != nil)
			runtime_memmove(sg->elem, ep, c->elemsize);
		if(sg->releasetime)
			sg->releasetime = runtime_cputicks();
		runtime_ready(gp);

		if(pres != nil)
			*pres = true;
		return;
	}

	if(pres != nil) {
		runtime_unlock(c);
		*pres = false;
		return;
	}

	mysg.elem = ep;
	mysg.g = g;
	mysg.selgen = NOSELGEN;
	g->param = nil;
	enqueue(&c->sendq, &mysg);
	runtime_park(runtime_unlock, c, "chan send");

	if(g->param == nil) {
		runtime_lock(c);
		if(!c->closed)
			runtime_throw("chansend: spurious wakeup");
		goto closed;
	}

	if(mysg.releasetime > 0)
		runtime_blockevent(mysg.releasetime - t0, 2);

	return;

asynch:
	if(c->closed)
		goto closed;

	if(c->qcount >= c->dataqsiz) {
		if(pres != nil) {
			runtime_unlock(c);
			*pres = false;
			return;
		}
		mysg.g = g;
		mysg.elem = nil;
		mysg.selgen = NOSELGEN;
		enqueue(&c->sendq, &mysg);
		runtime_park(runtime_unlock, c, "chan send");

		runtime_lock(c);
		goto asynch;
	}

	if(raceenabled)
		runtime_racerelease(chanbuf(c, c->sendx));

	runtime_memmove(chanbuf(c, c->sendx), ep, c->elemsize);
	if(++c->sendx == c->dataqsiz)
		c->sendx = 0;
	c->qcount++;

	sg = dequeue(&c->recvq);
	if(sg != nil) {
		gp = sg->g;
		runtime_unlock(c);
		if(sg->releasetime)
			sg->releasetime = runtime_cputicks();
		runtime_ready(gp);
	} else
		runtime_unlock(c);
	if(pres != nil)
		*pres = true;
	if(mysg.releasetime > 0)
		runtime_blockevent(mysg.releasetime - t0, 2);
	return;

closed:
	runtime_unlock(c);
	runtime_panicstring("send on closed channel");
}


void
runtime_chanrecv(ChanType *t, Hchan* c, byte *ep, bool *selected, bool *received)
{
	SudoG *sg;
	SudoG mysg;
	G *gp;
	int64 t0;
	G *g;

	if(runtime_gcwaiting())
		runtime_gosched();

	if(debug)
		runtime_printf("chanrecv: chan=%p\n", c);

	g = runtime_g();

	if(c == nil) {
		USED(t);
		if(selected != nil) {
			*selected = false;
			return;
		}
		runtime_park(nil, nil, "chan receive (nil chan)");
		return;  // not reached
	}

	t0 = 0;
	mysg.releasetime = 0;
	if(runtime_blockprofilerate > 0) {
		t0 = runtime_cputicks();
		mysg.releasetime = -1;
	}

	runtime_lock(c);
	if(c->dataqsiz > 0)
		goto asynch;

	if(c->closed)
		goto closed;

	sg = dequeue(&c->sendq);
	if(sg != nil) {
		if(raceenabled)
			racesync(c, sg);
		runtime_unlock(c);

		if(ep != nil)
			runtime_memmove(ep, sg->elem, c->elemsize);
		gp = sg->g;
		gp->param = sg;
		if(sg->releasetime)
			sg->releasetime = runtime_cputicks();
		runtime_ready(gp);

		if(selected != nil)
			*selected = true;
		if(received != nil)
			*received = true;
		return;
	}

	if(selected != nil) {
		runtime_unlock(c);
		*selected = false;
		return;
	}

	mysg.elem = ep;
	mysg.g = g;
	mysg.selgen = NOSELGEN;
	g->param = nil;
	enqueue(&c->recvq, &mysg);
	runtime_park(runtime_unlock, c, "chan receive");

	if(g->param == nil) {
		runtime_lock(c);
		if(!c->closed)
			runtime_throw("chanrecv: spurious wakeup");
		goto closed;
	}

	if(received != nil)
		*received = true;
	if(mysg.releasetime > 0)
		runtime_blockevent(mysg.releasetime - t0, 2);
	return;

asynch:
	if(c->qcount <= 0) {
		if(c->closed)
			goto closed;

		if(selected != nil) {
			runtime_unlock(c);
			*selected = false;
			if(received != nil)
				*received = false;
			return;
		}
		mysg.g = g;
		mysg.elem = nil;
		mysg.selgen = NOSELGEN;
		enqueue(&c->recvq, &mysg);
		runtime_park(runtime_unlock, c, "chan receive");

		runtime_lock(c);
		goto asynch;
	}

	if(raceenabled)
		runtime_raceacquire(chanbuf(c, c->recvx));

	if(ep != nil)
		runtime_memmove(ep, chanbuf(c, c->recvx), c->elemsize);
	runtime_memclr(chanbuf(c, c->recvx), c->elemsize);
	if(++c->recvx == c->dataqsiz)
		c->recvx = 0;
	c->qcount--;

	sg = dequeue(&c->sendq);
	if(sg != nil) {
		gp = sg->g;
		runtime_unlock(c);
		if(sg->releasetime)
			sg->releasetime = runtime_cputicks();
		runtime_ready(gp);
	} else
		runtime_unlock(c);

	if(selected != nil)
		*selected = true;
	if(received != nil)
		*received = true;
	if(mysg.releasetime > 0)
		runtime_blockevent(mysg.releasetime - t0, 2);
	return;

closed:
	if(ep != nil)
		runtime_memclr(ep, c->elemsize);
	if(selected != nil)
		*selected = true;
	if(received != nil)
		*received = false;
	if(raceenabled)
		runtime_raceacquire(c);
	runtime_unlock(c);
	if(mysg.releasetime > 0)
		runtime_blockevent(mysg.releasetime - t0, 2);
}

// The compiler generates a call to __go_send_small to send a value 8
// bytes or smaller.
void
__go_send_small(ChanType *t, Hchan* c, uint64 val)
{
	union
	{
		byte b[sizeof(uint64)];
		uint64 v;
	} u;
	byte *p;

	u.v = val;
#ifndef WORDS_BIGENDIAN
	p = u.b;
#else
	p = u.b + sizeof(uint64) - t->__element_type->__size;
#endif
	runtime_chansend(t, c, p, nil, runtime_getcallerpc(&t));
}

// The compiler generates a call to __go_send_big to send a value
// larger than 8 bytes or smaller.
void
__go_send_big(ChanType *t, Hchan* c, byte* p)
{
	runtime_chansend(t, c, p, nil, runtime_getcallerpc(&t));
}

// The compiler generates a call to __go_receive_small to receive a
// value 8 bytes or smaller.
uint64
__go_receive_small(ChanType *t, Hchan* c)
{
	union {
		byte b[sizeof(uint64)];
		uint64 v;
	} u;
	byte *p;

	u.v = 0;
#ifndef WORDS_BIGENDIAN
	p = u.b;
#else
	p = u.b + sizeof(uint64) - t->__element_type->__size;
#endif
	runtime_chanrecv(t, c, p, nil, nil);
	return u.v;
}

// The compiler generates a call to __go_receive_big to receive a
// value larger than 8 bytes.
void
__go_receive_big(ChanType *t, Hchan* c, byte* p)
{
	runtime_chanrecv(t, c, p, nil, nil);
}

_Bool runtime_chanrecv2(ChanType *t, Hchan* c, byte* p)
  __asm__ (GOSYM_PREFIX "runtime.chanrecv2");

_Bool
runtime_chanrecv2(ChanType *t, Hchan* c, byte* p)
{
	bool received;

	runtime_chanrecv(t, c, p, nil, &received);
	return received;
}

// func selectnbsend(c chan any, elem any) bool
//
// compiler implements
//
//	select {
//	case c <- v:
//		... foo
//	default:
//		... bar
//	}
//
// as
//
//	if selectnbsend(c, v) {
//		... foo
//	} else {
//		... bar
//	}
//
_Bool
runtime_selectnbsend(ChanType *t, Hchan *c, byte *p)
{
	bool res;

	runtime_chansend(t, c, p, &res, runtime_getcallerpc(&t));
	return res;
}

// func selectnbrecv(elem *any, c chan any) bool
//
// compiler implements
//
//	select {
//	case v = <-c:
//		... foo
//	default:
//		... bar
//	}
//
// as
//
//	if selectnbrecv(&v, c) {
//		... foo
//	} else {
//		... bar
//	}
//
_Bool
runtime_selectnbrecv(ChanType *t, byte *v, Hchan *c)
{
	bool selected;

	runtime_chanrecv(t, c, v, &selected, nil);
	return selected;
}

// func selectnbrecv2(elem *any, ok *bool, c chan any) bool
//
// compiler implements
//
//	select {
//	case v, ok = <-c:
//		... foo
//	default:
//		... bar
//	}
//
// as
//
//	if c != nil && selectnbrecv2(&v, &ok, c) {
//		... foo
//	} else {
//		... bar
//	}
//
_Bool
runtime_selectnbrecv2(ChanType *t, byte *v, _Bool *received, Hchan *c)
{
	bool selected;
	bool r;

	r = false;
	runtime_chanrecv(t, c, v, &selected, received == nil ? nil : &r);
	if(received != nil)
		*received = r;
	return selected;
}

// For reflect:
//	func chansend(c chan, val iword, nb bool) (selected bool)
// where an iword is the same word an interface value would use:
// the actual data if it fits, or else a pointer to the data.

_Bool reflect_chansend(ChanType *, Hchan *, uintptr, _Bool)
  __asm__ (GOSYM_PREFIX "reflect.chansend");

_Bool
reflect_chansend(ChanType *t, Hchan *c, uintptr val, _Bool nb)
{
	bool selected;
	bool *sp;
	byte *vp;

	if(nb) {
		selected = false;
		sp = (bool*)&selected;
	} else {
		selected = true;
		sp = nil;
	}
	if(__go_is_pointer_type(t->__element_type))
		vp = (byte*)&val;
	else
		vp = (byte*)val;
	runtime_chansend(t, c, vp, sp, runtime_getcallerpc(&t));
	return selected;
}

// For reflect:
//	func chanrecv(c chan, nb bool) (val iword, selected, received bool)
// where an iword is the same word an interface value would use:
// the actual data if it fits, or else a pointer to the data.

struct chanrecv_ret
{
	uintptr val;
	_Bool selected;
	_Bool received;
};

struct chanrecv_ret reflect_chanrecv(ChanType *, Hchan *, _Bool)
  __asm__ (GOSYM_PREFIX "reflect.chanrecv");

struct chanrecv_ret
reflect_chanrecv(ChanType *t, Hchan *c, _Bool nb)
{
	struct chanrecv_ret ret;
	byte *vp;
	bool *sp;
	bool selected;
	bool received;

	if(nb) {
		selected = false;
		sp = &selected;
	} else {
		ret.selected = true;
		sp = nil;
	}
	received = false;
	if(__go_is_pointer_type(t->__element_type)) {
		vp = (byte*)&ret.val;
	} else {
		vp = runtime_mal(t->__element_type->__size);
		ret.val = (uintptr)vp;
	}
	runtime_chanrecv(t, c, vp, sp, &received);
	if(nb)
		ret.selected = selected;
	ret.received = received;
	return ret;
}

static void newselect(int32, Select**);

// newselect(size uint32) (sel *byte);

void* runtime_newselect(int32) __asm__ (GOSYM_PREFIX "runtime.newselect");

void*
runtime_newselect(int32 size)
{
	Select *sel;

	newselect(size, &sel);
	return (void*)sel;
}

static void
newselect(int32 size, Select **selp)
{
	int32 n;
	Select *sel;

	n = 0;
	if(size > 1)
		n = size-1;

	// allocate all the memory we need in a single allocation
	// start with Select with size cases
	// then lockorder with size entries
	// then pollorder with size entries
	sel = runtime_mal(sizeof(*sel) +
		n*sizeof(sel->scase[0]) +
		size*sizeof(sel->lockorder[0]) +
		size*sizeof(sel->pollorder[0]));

	sel->tcase = size;
	sel->ncase = 0;
	sel->lockorder = (void*)(sel->scase + size);
	sel->pollorder = (void*)(sel->lockorder + size);
	*selp = sel;

	if(debug)
		runtime_printf("newselect s=%p size=%d\n", sel, size);
}

// cut in half to give stack a chance to split
static void selectsend(Select *sel, Hchan *c, int index, void *elem);

// selectsend(sel *byte, hchan *chan any, elem *any) (selected bool);

void runtime_selectsend(Select *, Hchan *, void *, int32)
  __asm__ (GOSYM_PREFIX "runtime.selectsend");

void
runtime_selectsend(Select *sel, Hchan *c, void *elem, int32 index)
{
	// nil cases do not compete
	if(c == nil)
		return;

	selectsend(sel, c, index, elem);
}

static void
selectsend(Select *sel, Hchan *c, int index, void *elem)
{
	int32 i;
	Scase *cas;

	i = sel->ncase;
	if(i >= sel->tcase)
		runtime_throw("selectsend: too many cases");
	sel->ncase = i+1;
	cas = &sel->scase[i];

	cas->index = index;
	cas->chan = c;
	cas->kind = CaseSend;
	cas->sg.elem = elem;

	if(debug)
		runtime_printf("selectsend s=%p index=%d chan=%p\n",
			sel, cas->index, cas->chan);
}

// cut in half to give stack a chance to split
static void selectrecv(Select *sel, Hchan *c, int index, void *elem, bool*);

// selectrecv(sel *byte, hchan *chan any, elem *any) (selected bool);

void runtime_selectrecv(Select *, Hchan *, void *, int32)
  __asm__ (GOSYM_PREFIX "runtime.selectrecv");

void
runtime_selectrecv(Select *sel, Hchan *c, void *elem, int32 index)
{
	// nil cases do not compete
	if(c == nil)
		return;

	selectrecv(sel, c, index, elem, nil);
}

// selectrecv2(sel *byte, hchan *chan any, elem *any, received *bool) (selected bool);

void runtime_selectrecv2(Select *, Hchan *, void *, bool *, int32)
  __asm__ (GOSYM_PREFIX "runtime.selectrecv2");

void
runtime_selectrecv2(Select *sel, Hchan *c, void *elem, bool *received, int32 index)
{
	// nil cases do not compete
	if(c == nil)
		return;

	selectrecv(sel, c, index, elem, received);
}

static void
selectrecv(Select *sel, Hchan *c, int index, void *elem, bool *received)
{
	int32 i;
	Scase *cas;

	i = sel->ncase;
	if(i >= sel->tcase)
		runtime_throw("selectrecv: too many cases");
	sel->ncase = i+1;
	cas = &sel->scase[i];
	cas->index = index;
	cas->chan = c;

	cas->kind = CaseRecv;
	cas->sg.elem = elem;
	cas->receivedp = received;

	if(debug)
		runtime_printf("selectrecv s=%p index=%d chan=%p\n",
			sel, cas->index, cas->chan);
}

// cut in half to give stack a chance to split
static void selectdefault(Select*, int);

// selectdefault(sel *byte) (selected bool);

void runtime_selectdefault(Select *, int32) __asm__ (GOSYM_PREFIX "runtime.selectdefault");

void
runtime_selectdefault(Select *sel, int32 index)
{
	selectdefault(sel, index);
}

static void
selectdefault(Select *sel, int32 index)
{
	int32 i;
	Scase *cas;

	i = sel->ncase;
	if(i >= sel->tcase)
		runtime_throw("selectdefault: too many cases");
	sel->ncase = i+1;
	cas = &sel->scase[i];
	cas->index = index;
	cas->chan = nil;

	cas->kind = CaseDefault;

	if(debug)
		runtime_printf("selectdefault s=%p index=%d\n",
			sel, cas->index);
}

static void
sellock(Select *sel)
{
	uint32 i;
	Hchan *c, *c0;

	c = nil;
	for(i=0; i<sel->ncase; i++) {
		c0 = sel->lockorder[i];
		if(c0 && c0 != c) {
			c = sel->lockorder[i];
			runtime_lock(c);
		}
	}
}

static void
selunlock(Select *sel)
{
	int32 i, n, r;
	Hchan *c;

	// We must be very careful here to not touch sel after we have unlocked
	// the last lock, because sel can be freed right after the last unlock.
	// Consider the following situation.
	// First M calls runtime_park() in runtime_selectgo() passing the sel.
	// Once runtime_park() has unlocked the last lock, another M makes
	// the G that calls select runnable again and schedules it for execution.
	// When the G runs on another M, it locks all the locks and frees sel.
	// Now if the first M touches sel, it will access freed memory.
	n = (int32)sel->ncase;
	r = 0;
	// skip the default case
	if(n>0 && sel->lockorder[0] == nil)
		r = 1;
	for(i = n-1; i >= r; i--) {
		c = sel->lockorder[i];
		if(i>0 && sel->lockorder[i-1] == c)
			continue;  // will unlock it on the next iteration
		runtime_unlock(c);
	}
}

void
runtime_block(void)
{
	runtime_park(nil, nil, "select (no cases)");	// forever
}

static int selectgo(Select**);

// selectgo(sel *byte);

int runtime_selectgo(Select *) __asm__ (GOSYM_PREFIX "runtime.selectgo");

int
runtime_selectgo(Select *sel)
{
	return selectgo(&sel);
}

static int
selectgo(Select **selp)
{
	Select *sel;
	uint32 o, i, j, k;
	int64 t0;
	Scase *cas, *dfl;
	Hchan *c;
	SudoG *sg;
	G *gp;
	int index;
	G *g;

	sel = *selp;
	if(runtime_gcwaiting())
		runtime_gosched();

	if(debug)
		runtime_printf("select: sel=%p\n", sel);

	g = runtime_g();

	t0 = 0;
	if(runtime_blockprofilerate > 0) {
		t0 = runtime_cputicks();
		for(i=0; i<sel->ncase; i++)
			sel->scase[i].sg.releasetime = -1;
	}

	// The compiler rewrites selects that statically have
	// only 0 or 1 cases plus default into simpler constructs.
	// The only way we can end up with such small sel->ncase
	// values here is for a larger select in which most channels
	// have been nilled out.  The general code handles those
	// cases correctly, and they are rare enough not to bother
	// optimizing (and needing to test).

	// generate permuted order
	for(i=0; i<sel->ncase; i++)
		sel->pollorder[i] = i;
	for(i=1; i<sel->ncase; i++) {
		o = sel->pollorder[i];
		j = runtime_fastrand1()%(i+1);
		sel->pollorder[i] = sel->pollorder[j];
		sel->pollorder[j] = o;
	}

	// sort the cases by Hchan address to get the locking order.
	// simple heap sort, to guarantee n log n time and constant stack footprint.
	for(i=0; i<sel->ncase; i++) {
		j = i;
		c = sel->scase[j].chan;
		while(j > 0 && sel->lockorder[k=(j-1)/2] < c) {
			sel->lockorder[j] = sel->lockorder[k];
			j = k;
		}
		sel->lockorder[j] = c;
	}
	for(i=sel->ncase; i-->0; ) {
		c = sel->lockorder[i];
		sel->lockorder[i] = sel->lockorder[0];
		j = 0;
		for(;;) {
			k = j*2+1;
			if(k >= i)
				break;
			if(k+1 < i && sel->lockorder[k] < sel->lockorder[k+1])
				k++;
			if(c < sel->lockorder[k]) {
				sel->lockorder[j] = sel->lockorder[k];
				j = k;
				continue;
			}
			break;
		}
		sel->lockorder[j] = c;
	}
	/*
	for(i=0; i+1<sel->ncase; i++)
		if(sel->lockorder[i] > sel->lockorder[i+1]) {
			runtime_printf("i=%d %p %p\n", i, sel->lockorder[i], sel->lockorder[i+1]);
			runtime_throw("select: broken sort");
		}
	*/
	sellock(sel);

loop:
	// pass 1 - look for something already waiting
	dfl = nil;
	for(i=0; i<sel->ncase; i++) {
		o = sel->pollorder[i];
		cas = &sel->scase[o];
		c = cas->chan;

		switch(cas->kind) {
		case CaseRecv:
			if(c->dataqsiz > 0) {
				if(c->qcount > 0)
					goto asyncrecv;
			} else {
				sg = dequeue(&c->sendq);
				if(sg != nil)
					goto syncrecv;
			}
			if(c->closed)
				goto rclose;
			break;

		case CaseSend:
			if(raceenabled)
				runtime_racereadpc(c, runtime_selectgo, runtime_chansend);
			if(c->closed)
				goto sclose;
			if(c->dataqsiz > 0) {
				if(c->qcount < c->dataqsiz)
					goto asyncsend;
			} else {
				sg = dequeue(&c->recvq);
				if(sg != nil)
					goto syncsend;
			}
			break;

		case CaseDefault:
			dfl = cas;
			break;
		}
	}

	if(dfl != nil) {
		selunlock(sel);
		cas = dfl;
		goto retc;
	}


	// pass 2 - enqueue on all chans
	for(i=0; i<sel->ncase; i++) {
		o = sel->pollorder[i];
		cas = &sel->scase[o];
		c = cas->chan;
		sg = &cas->sg;
		sg->g = g;
		sg->selgen = g->selgen;

		switch(cas->kind) {
		case CaseRecv:
			enqueue(&c->recvq, sg);
			break;

		case CaseSend:
			enqueue(&c->sendq, sg);
			break;
		}
	}

	g->param = nil;
	runtime_park((void(*)(Lock*))selunlock, (Lock*)sel, "select");

	sellock(sel);
	sg = g->param;

	// pass 3 - dequeue from unsuccessful chans
	// otherwise they stack up on quiet channels
	for(i=0; i<sel->ncase; i++) {
		cas = &sel->scase[i];
		if(cas != (Scase*)sg) {
			c = cas->chan;
			if(cas->kind == CaseSend)
				dequeueg(&c->sendq);
			else
				dequeueg(&c->recvq);
		}
	}

	if(sg == nil)
		goto loop;

	cas = (Scase*)sg;
	c = cas->chan;

	if(c->dataqsiz > 0)
		runtime_throw("selectgo: shouldn't happen");

	if(debug)
		runtime_printf("wait-return: sel=%p c=%p cas=%p kind=%d\n",
			sel, c, cas, cas->kind);

	if(cas->kind == CaseRecv) {
		if(cas->receivedp != nil)
			*cas->receivedp = true;
	}

	selunlock(sel);
	goto retc;

asyncrecv:
	// can receive from buffer
	if(raceenabled)
		runtime_raceacquire(chanbuf(c, c->recvx));
	if(cas->receivedp != nil)
		*cas->receivedp = true;
	if(cas->sg.elem != nil)
		runtime_memmove(cas->sg.elem, chanbuf(c, c->recvx), c->elemsize);
	runtime_memclr(chanbuf(c, c->recvx), c->elemsize);
	if(++c->recvx == c->dataqsiz)
		c->recvx = 0;
	c->qcount--;
	sg = dequeue(&c->sendq);
	if(sg != nil) {
		gp = sg->g;
		selunlock(sel);
		if(sg->releasetime)
			sg->releasetime = runtime_cputicks();
		runtime_ready(gp);
	} else {
		selunlock(sel);
	}
	goto retc;

asyncsend:
	// can send to buffer
	if(raceenabled)
		runtime_racerelease(chanbuf(c, c->sendx));
	runtime_memmove(chanbuf(c, c->sendx), cas->sg.elem, c->elemsize);
	if(++c->sendx == c->dataqsiz)
		c->sendx = 0;
	c->qcount++;
	sg = dequeue(&c->recvq);
	if(sg != nil) {
		gp = sg->g;
		selunlock(sel);
		if(sg->releasetime)
			sg->releasetime = runtime_cputicks();
		runtime_ready(gp);
	} else {
		selunlock(sel);
	}
	goto retc;

syncrecv:
	// can receive from sleeping sender (sg)
	if(raceenabled)
		racesync(c, sg);
	selunlock(sel);
	if(debug)
		runtime_printf("syncrecv: sel=%p c=%p o=%d\n", sel, c, o);
	if(cas->receivedp != nil)
		*cas->receivedp = true;
	if(cas->sg.elem != nil)
		runtime_memmove(cas->sg.elem, sg->elem, c->elemsize);
	gp = sg->g;
	gp->param = sg;
	if(sg->releasetime)
		sg->releasetime = runtime_cputicks();
	runtime_ready(gp);
	goto retc;

rclose:
	// read at end of closed channel
	selunlock(sel);
	if(cas->receivedp != nil)
		*cas->receivedp = false;
	if(cas->sg.elem != nil)
		runtime_memclr(cas->sg.elem, c->elemsize);
	if(raceenabled)
		runtime_raceacquire(c);
	goto retc;

syncsend:
	// can send to sleeping receiver (sg)
	if(raceenabled)
		racesync(c, sg);
	selunlock(sel);
	if(debug)
		runtime_printf("syncsend: sel=%p c=%p o=%d\n", sel, c, o);
	if(sg->elem != nil)
		runtime_memmove(sg->elem, cas->sg.elem, c->elemsize);
	gp = sg->g;
	gp->param = sg;
	if(sg->releasetime)
		sg->releasetime = runtime_cputicks();
	runtime_ready(gp);

retc:
	// return index corresponding to chosen case
	index = cas->index;
	if(cas->sg.releasetime > 0)
		runtime_blockevent(cas->sg.releasetime - t0, 2);
	runtime_free(sel);
	return index;

sclose:
	// send on closed channel
	selunlock(sel);
	runtime_panicstring("send on closed channel");
	return 0;  // not reached
}

// This struct must match ../reflect/value.go:/runtimeSelect.
typedef struct runtimeSelect runtimeSelect;
struct runtimeSelect
{
	uintptr dir;
	ChanType *typ;
	Hchan *ch;
	uintptr val;
};

// This enum must match ../reflect/value.go:/SelectDir.
enum SelectDir {
	SelectSend = 1,
	SelectRecv,
	SelectDefault,
};

struct rselect_ret {
	intgo chosen;
	uintptr word;
	bool recvOK;
};

// func rselect(cases []runtimeSelect) (chosen int, word uintptr, recvOK bool)

struct rselect_ret reflect_rselect(Slice)
     __asm__ (GOSYM_PREFIX "reflect.rselect");

struct rselect_ret
reflect_rselect(Slice cases)
{
	struct rselect_ret ret;
	int32 i;
	Select *sel;
	runtimeSelect* rcase, *rc;
	void *elem;
	void *recvptr;
	uintptr maxsize;
	bool onlyptr;

	ret.chosen = -1;
	ret.word = 0;
	ret.recvOK = false;

	maxsize = 0;
	onlyptr = true;
	rcase = (runtimeSelect*)cases.__values;
	for(i=0; i<cases.__count; i++) {
		rc = &rcase[i];
		if(rc->dir == SelectRecv && rc->ch != nil) {
			if(maxsize < rc->typ->__element_type->__size)
				maxsize = rc->typ->__element_type->__size;
			if(!__go_is_pointer_type(rc->typ->__element_type))
				onlyptr = false;
		}
	}

	recvptr = nil;
	if(!onlyptr)
		recvptr = runtime_mal(maxsize);

	newselect(cases.__count, &sel);
	for(i=0; i<cases.__count; i++) {
		rc = &rcase[i];
		switch(rc->dir) {
		case SelectDefault:
			selectdefault(sel, i);
			break;
		case SelectSend:
			if(rc->ch == nil)
				break;
			if(!__go_is_pointer_type(rc->typ->__element_type))
				elem = (void*)rc->val;
			else
				elem = (void*)&rc->val;
			selectsend(sel, rc->ch, i, elem);
			break;
		case SelectRecv:
			if(rc->ch == nil)
				break;
			if(!__go_is_pointer_type(rc->typ->__element_type))
				elem = recvptr;
			else
				elem = &ret.word;
			selectrecv(sel, rc->ch, i, elem, &ret.recvOK);
			break;
		}
	}

	ret.chosen = (intgo)(uintptr)selectgo(&sel);
	if(rcase[ret.chosen].dir == SelectRecv && !__go_is_pointer_type(rcase[ret.chosen].typ->__element_type))
		ret.word = (uintptr)recvptr;

	return ret;
}

static void closechan(Hchan *c, void *pc);

// closechan(sel *byte);
void
runtime_closechan(Hchan *c)
{
	closechan(c, runtime_getcallerpc(&c));
}

// For reflect
//	func chanclose(c chan)

void reflect_chanclose(uintptr) __asm__ (GOSYM_PREFIX "reflect.chanclose");

void
reflect_chanclose(uintptr c)
{
	closechan((Hchan*)c, runtime_getcallerpc(&c));
}

static void
closechan(Hchan *c, void *pc)
{
	SudoG *sg;
	G* gp;

	if(c == nil)
		runtime_panicstring("close of nil channel");

	if(runtime_gcwaiting())
		runtime_gosched();

	runtime_lock(c);
	if(c->closed) {
		runtime_unlock(c);
		runtime_panicstring("close of closed channel");
	}

	if(raceenabled) {
		runtime_racewritepc(c, pc, runtime_closechan);
		runtime_racerelease(c);
	}

	c->closed = true;

	// release all readers
	for(;;) {
		sg = dequeue(&c->recvq);
		if(sg == nil)
			break;
		gp = sg->g;
		gp->param = nil;
		if(sg->releasetime)
			sg->releasetime = runtime_cputicks();
		runtime_ready(gp);
	}

	// release all writers
	for(;;) {
		sg = dequeue(&c->sendq);
		if(sg == nil)
			break;
		gp = sg->g;
		gp->param = nil;
		if(sg->releasetime)
			sg->releasetime = runtime_cputicks();
		runtime_ready(gp);
	}

	runtime_unlock(c);
}

void
__go_builtin_close(Hchan *c)
{
	runtime_closechan(c);
}

// For reflect
//	func chanlen(c chan) (len int)

intgo reflect_chanlen(uintptr) __asm__ (GOSYM_PREFIX "reflect.chanlen");

intgo
reflect_chanlen(uintptr ca)
{
	Hchan *c;
	intgo len;

	c = (Hchan*)ca;
	if(c == nil)
		len = 0;
	else
		len = c->qcount;
	return len;
}

intgo
__go_chan_len(Hchan *c)
{
	return reflect_chanlen((uintptr)c);
}

// For reflect
//	func chancap(c chan) (cap intgo)

intgo reflect_chancap(uintptr) __asm__ (GOSYM_PREFIX "reflect.chancap");

intgo
reflect_chancap(uintptr ca)
{
	Hchan *c;
	intgo cap;

	c = (Hchan*)ca;
	if(c == nil)
		cap = 0;
	else
		cap = c->dataqsiz;
	return cap;
}

intgo
__go_chan_cap(Hchan *c)
{
	return reflect_chancap((uintptr)c);
}

static SudoG*
dequeue(WaitQ *q)
{
	SudoG *sgp;

loop:
	sgp = q->first;
	if(sgp == nil)
		return nil;
	q->first = sgp->link;

	// if sgp is stale, ignore it
	if(sgp->selgen != NOSELGEN &&
		(sgp->selgen != sgp->g->selgen ||
		!runtime_cas(&sgp->g->selgen, sgp->selgen, sgp->selgen + 2))) {
		//prints("INVALID PSEUDOG POINTER\n");
		goto loop;
	}

	return sgp;
}

static void
dequeueg(WaitQ *q)
{
	SudoG **l, *sgp, *prevsgp;
	G *g;

	g = runtime_g();
	prevsgp = nil;
	for(l=&q->first; (sgp=*l) != nil; l=&sgp->link, prevsgp=sgp) {
		if(sgp->g == g) {
			*l = sgp->link;
			if(q->last == sgp)
				q->last = prevsgp;
			break;
		}
	}
}

static void
enqueue(WaitQ *q, SudoG *sgp)
{
	sgp->link = nil;
	if(q->first == nil) {
		q->first = sgp;
		q->last = sgp;
		return;
	}
	q->last->link = sgp;
	q->last = sgp;
}

static void
racesync(Hchan *c, SudoG *sg)
{
	runtime_racerelease(chanbuf(c, 0));
	runtime_raceacquireg(sg->g, chanbuf(c, 0));
	runtime_racereleaseg(sg->g, chanbuf(c, 0));
	runtime_raceacquire(chanbuf(c, 0));
}
