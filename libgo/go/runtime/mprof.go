// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Malloc profiling.
// Patterned after tcmalloc's algorithms; shorter code.

package runtime

import (
	"runtime/internal/atomic"
	"unsafe"
)

// NOTE(rsc): Everything here could use cas if contention became an issue.
var proflock mutex

// All memory allocations are local and do not escape outside of the profiler.
// The profiler is forbidden from referring to garbage-collected memory.

const (
	// profile types
	memProfile bucketType = 1 + iota
	blockProfile
	mutexProfile

	// a profile bucket from one of the categories above whose stack
	// trace has been fixed up / pruned.
	prunedProfile

	// size of bucket hash table
	buckHashSize = 179999

	// max depth of stack to record in bucket
	maxStack = 32
)

type bucketType int

// A bucket holds per-call-stack profiling information.
// The representation is a bit sleazy, inherited from C.
// This struct defines the bucket header. It is followed in
// memory by the stack words and then the actual record
// data, either a memRecord or a blockRecord.
//
// Per-call-stack profiling information.
// Lookup by hashing call stack into a linked-list hash table.
//
// No heap pointers.
//
//go:notinheap
type bucket struct {
	next    *bucket
	allnext *bucket
	typ     bucketType // memBucket or blockBucket (includes mutexProfile)
	hash    uintptr
	size    uintptr
	nstk    uintptr
	skip    int
}

// A memRecord is the bucket data for a bucket of type memProfile,
// part of the memory profile.
type memRecord struct {
	// The following complex 3-stage scheme of stats accumulation
	// is required to obtain a consistent picture of mallocs and frees
	// for some point in time.
	// The problem is that mallocs come in real time, while frees
	// come only after a GC during concurrent sweeping. So if we would
	// naively count them, we would get a skew toward mallocs.
	//
	// Hence, we delay information to get consistent snapshots as
	// of mark termination. Allocations count toward the next mark
	// termination's snapshot, while sweep frees count toward the
	// previous mark termination's snapshot:
	//
	//              MT          MT          MT          MT
	//             .·|         .·|         .·|         .·|
	//          .·˙  |      .·˙  |      .·˙  |      .·˙  |
	//       .·˙     |   .·˙     |   .·˙     |   .·˙     |
	//    .·˙        |.·˙        |.·˙        |.·˙        |
	//
	//       alloc → ▲ ← free
	//               ┠┅┅┅┅┅┅┅┅┅┅┅P
	//       C+2     →    C+1    →  C
	//
	//                   alloc → ▲ ← free
	//                           ┠┅┅┅┅┅┅┅┅┅┅┅P
	//                   C+2     →    C+1    →  C
	//
	// Since we can't publish a consistent snapshot until all of
	// the sweep frees are accounted for, we wait until the next
	// mark termination ("MT" above) to publish the previous mark
	// termination's snapshot ("P" above). To do this, allocation
	// and free events are accounted to *future* heap profile
	// cycles ("C+n" above) and we only publish a cycle once all
	// of the events from that cycle must be done. Specifically:
	//
	// Mallocs are accounted to cycle C+2.
	// Explicit frees are accounted to cycle C+2.
	// GC frees (done during sweeping) are accounted to cycle C+1.
	//
	// After mark termination, we increment the global heap
	// profile cycle counter and accumulate the stats from cycle C
	// into the active profile.

	// active is the currently published profile. A profiling
	// cycle can be accumulated into active once its complete.
	active memRecordCycle

	// future records the profile events we're counting for cycles
	// that have not yet been published. This is ring buffer
	// indexed by the global heap profile cycle C and stores
	// cycles C, C+1, and C+2. Unlike active, these counts are
	// only for a single cycle; they are not cumulative across
	// cycles.
	//
	// We store cycle C here because there's a window between when
	// C becomes the active cycle and when we've flushed it to
	// active.
	future [3]memRecordCycle
}

// memRecordCycle
type memRecordCycle struct {
	allocs, frees           uintptr
	alloc_bytes, free_bytes uintptr
}

// add accumulates b into a. It does not zero b.
func (a *memRecordCycle) add(b *memRecordCycle) {
	a.allocs += b.allocs
	a.frees += b.frees
	a.alloc_bytes += b.alloc_bytes
	a.free_bytes += b.free_bytes
}

// A blockRecord is the bucket data for a bucket of type blockProfile,
// which is used in blocking and mutex profiles.
type blockRecord struct {
	count  int64
	cycles int64
}

var (
	mbuckets    *bucket // memory profile buckets
	bbuckets    *bucket // blocking profile buckets
	xbuckets    *bucket // mutex profile buckets
	sbuckets    *bucket // pre-symbolization profile buckets (stacks fixed up)
	freebuckets *bucket // freelist of unused fixed up profile buckets
	buckhash    *[179999]*bucket
	bucketmem   uintptr

	mProf struct {
		// All fields in mProf are protected by proflock.

		// cycle is the global heap profile cycle. This wraps
		// at mProfCycleWrap.
		cycle uint32
		// flushed indicates that future[cycle] in all buckets
		// has been flushed to the active profile.
		flushed bool
	}
)

const mProfCycleWrap = uint32(len(memRecord{}.future)) * (2 << 24)

// payloadOffset() returns a pointer into the part of a bucket
// containing the profile payload (skips past the bucket struct itself
// and then the stack trace).
func payloadOffset(typ bucketType, nstk uintptr) uintptr {
	if typ == prunedProfile {
		// To allow reuse of prunedProfile buckets between different
		// collections, allocate them with the max stack size (the portion
		// of the stack used will vary from trace to trace).
		nstk = maxStack
	}
	return unsafe.Sizeof(bucket{}) + uintptr(nstk)*unsafe.Sizeof(uintptr)
}

func max(x, y uintptr) uintptr {
	if x > y {
		return x
	}
	return y
}

// newBucket allocates a bucket with the given type and number of stack entries.
func newBucket(typ bucketType, nstk int, skipCount int) *bucket {
	size := payloadOffset(typ, uintptr(nstk))
	switch typ {
	default:
		throw("invalid profile bucket type")
	case prunedProfile:
		// stack-fixed buckets are large enough to accommodate any payload.
		size += max(unsafe.Sizeof(memRecord{}), unsafe.Sizeof(blockRecord{}))
	case memProfile:
		size += unsafe.Sizeof(memRecord{})
	case blockProfile, mutexProfile:
		size += unsafe.Sizeof(blockRecord{})
	}

	b := (*bucket)(persistentalloc(size, 0, &memstats.buckhash_sys))
	bucketmem += size
	b.typ = typ
	b.nstk = uintptr(nstk)
	b.skip = skipCount
	return b
}

// stk returns the slice in b holding the stack.
func (b *bucket) stk() []uintptr {
	stk := (*[maxStack]uintptr)(add(unsafe.Pointer(b), unsafe.Sizeof(*b)))
	return stk[:b.nstk:b.nstk]
}

// mp returns the memRecord associated with the memProfile bucket b.
func (b *bucket) mp() *memRecord {
	if b.typ != memProfile && b.typ != prunedProfile {
		throw("bad use of bucket.mp")
	}
	return (*memRecord)(add(unsafe.Pointer(b), payloadOffset(b.typ, b.nstk)))
}

// bp returns the blockRecord associated with the blockProfile bucket b.
func (b *bucket) bp() *blockRecord {
	if b.typ != blockProfile && b.typ != mutexProfile && b.typ != prunedProfile {
		throw("bad use of bucket.bp")
	}
	return (*blockRecord)(add(unsafe.Pointer(b), payloadOffset(b.typ, b.nstk)))
}

// Return the bucket for stk[0:nstk], allocating new bucket if needed.
func stkbucket(typ bucketType, size uintptr, skip int, stk []uintptr, alloc bool) *bucket {
	if buckhash == nil {
		buckhash = (*[buckHashSize]*bucket)(sysAlloc(unsafe.Sizeof(*buckhash), &memstats.buckhash_sys))
		if buckhash == nil {
			throw("runtime: cannot allocate memory")
		}
	}

	// Hash stack.
	var h uintptr
	for _, pc := range stk {
		h += pc
		h += h << 10
		h ^= h >> 6
	}
	// hash in size
	h += size
	h += h << 10
	h ^= h >> 6
	// finalize
	h += h << 3
	h ^= h >> 11

	i := int(h % buckHashSize)
	for b := buckhash[i]; b != nil; b = b.next {
		if b.typ == typ && b.hash == h && b.size == size && eqslice(b.stk(), stk) {
			return b
		}
	}

	if !alloc {
		return nil
	}

	// Create new bucket.
	b := newBucket(typ, len(stk), skip)
	copy(b.stk(), stk)
	b.hash = h
	b.size = size
	b.next = buckhash[i]
	buckhash[i] = b
	if typ == memProfile {
		b.allnext = mbuckets
		mbuckets = b
	} else if typ == mutexProfile {
		b.allnext = xbuckets
		xbuckets = b
	} else if typ == prunedProfile {
		b.allnext = sbuckets
		sbuckets = b
	} else {
		b.allnext = bbuckets
		bbuckets = b
	}
	return b
}

func eqslice(x, y []uintptr) bool {
	if len(x) != len(y) {
		return false
	}
	for i, xi := range x {
		if xi != y[i] {
			return false
		}
	}
	return true
}

// mProf_NextCycle publishes the next heap profile cycle and creates a
// fresh heap profile cycle. This operation is fast and can be done
// during STW. The caller must call mProf_Flush before calling
// mProf_NextCycle again.
//
// This is called by mark termination during STW so allocations and
// frees after the world is started again count towards a new heap
// profiling cycle.
func mProf_NextCycle() {
	lock(&proflock)
	// We explicitly wrap mProf.cycle rather than depending on
	// uint wraparound because the memRecord.future ring does not
	// itself wrap at a power of two.
	mProf.cycle = (mProf.cycle + 1) % mProfCycleWrap
	mProf.flushed = false
	unlock(&proflock)
}

// mProf_Flush flushes the events from the current heap profiling
// cycle into the active profile. After this it is safe to start a new
// heap profiling cycle with mProf_NextCycle.
//
// This is called by GC after mark termination starts the world. In
// contrast with mProf_NextCycle, this is somewhat expensive, but safe
// to do concurrently.
func mProf_Flush() {
	lock(&proflock)
	if !mProf.flushed {
		mProf_FlushLocked()
		mProf.flushed = true
	}
	unlock(&proflock)
}

func mProf_FlushLocked() {
	c := mProf.cycle
	for b := mbuckets; b != nil; b = b.allnext {
		mp := b.mp()

		// Flush cycle C into the published profile and clear
		// it for reuse.
		mpc := &mp.future[c%uint32(len(mp.future))]
		mp.active.add(mpc)
		*mpc = memRecordCycle{}
	}
}

// mProf_PostSweep records that all sweep frees for this GC cycle have
// completed. This has the effect of publishing the heap profile
// snapshot as of the last mark termination without advancing the heap
// profile cycle.
func mProf_PostSweep() {
	lock(&proflock)
	// Flush cycle C+1 to the active profile so everything as of
	// the last mark termination becomes visible. *Don't* advance
	// the cycle, since we're still accumulating allocs in cycle
	// C+2, which have to become C+1 in the next mark termination
	// and so on.
	c := mProf.cycle
	for b := mbuckets; b != nil; b = b.allnext {
		mp := b.mp()
		mpc := &mp.future[(c+1)%uint32(len(mp.future))]
		mp.active.add(mpc)
		*mpc = memRecordCycle{}
	}
	unlock(&proflock)
}

// Called by malloc to record a profiled block.
func mProf_Malloc(p unsafe.Pointer, size uintptr) {
	var stk [maxStack]uintptr
	nstk := callersRaw(stk[:])
	lock(&proflock)
	skip := 1
	b := stkbucket(memProfile, size, skip, stk[:nstk], true)
	c := mProf.cycle
	mp := b.mp()
	mpc := &mp.future[(c+2)%uint32(len(mp.future))]
	mpc.allocs++
	mpc.alloc_bytes += size
	unlock(&proflock)

	// Setprofilebucket locks a bunch of other mutexes, so we call it outside of proflock.
	// This reduces potential contention and chances of deadlocks.
	// Since the object must be alive during call to mProf_Malloc,
	// it's fine to do this non-atomically.
	systemstack(func() {
		setprofilebucket(p, b)
	})
}

// Called when freeing a profiled block.
func mProf_Free(b *bucket, size uintptr) {
	lock(&proflock)
	c := mProf.cycle
	mp := b.mp()
	mpc := &mp.future[(c+1)%uint32(len(mp.future))]
	mpc.frees++
	mpc.free_bytes += size
	unlock(&proflock)
}

var blockprofilerate uint64 // in CPU ticks

// SetBlockProfileRate controls the fraction of goroutine blocking events
// that are reported in the blocking profile. The profiler aims to sample
// an average of one blocking event per rate nanoseconds spent blocked.
//
// To include every blocking event in the profile, pass rate = 1.
// To turn off profiling entirely, pass rate <= 0.
func SetBlockProfileRate(rate int) {
	var r int64
	if rate <= 0 {
		r = 0 // disable profiling
	} else if rate == 1 {
		r = 1 // profile everything
	} else {
		// convert ns to cycles, use float64 to prevent overflow during multiplication
		r = int64(float64(rate) * float64(tickspersecond()) / (1000 * 1000 * 1000))
		if r == 0 {
			r = 1
		}
	}

	atomic.Store64(&blockprofilerate, uint64(r))
}

func blockevent(cycles int64, skip int) {
	if cycles <= 0 {
		cycles = 1
	}
	if blocksampled(cycles) {
		saveblockevent(cycles, skip+1, blockProfile)
	}
}

func blocksampled(cycles int64) bool {
	rate := int64(atomic.Load64(&blockprofilerate))
	if rate <= 0 || (rate > cycles && int64(fastrand())%rate > cycles) {
		return false
	}
	return true
}

func saveblockevent(cycles int64, skip int, which bucketType) {
	gp := getg()
	var nstk int
	var stk [maxStack]uintptr
	if gp.m.curg == nil || gp.m.curg == gp {
		nstk = callersRaw(stk[:])
	} else {
		// FIXME: This should get a traceback of gp.m.curg.
		// nstk = gcallers(gp.m.curg, skip, stk[:])
		nstk = callersRaw(stk[:])
	}
	lock(&proflock)
	b := stkbucket(which, 0, skip, stk[:nstk], true)
	b.bp().count++
	b.bp().cycles += cycles
	unlock(&proflock)
}

var mutexprofilerate uint64 // fraction sampled

// SetMutexProfileFraction controls the fraction of mutex contention events
// that are reported in the mutex profile. On average 1/rate events are
// reported. The previous rate is returned.
//
// To turn off profiling entirely, pass rate 0.
// To just read the current rate, pass rate < 0.
// (For n>1 the details of sampling may change.)
func SetMutexProfileFraction(rate int) int {
	if rate < 0 {
		return int(mutexprofilerate)
	}
	old := mutexprofilerate
	atomic.Store64(&mutexprofilerate, uint64(rate))
	return int(old)
}

//go:linkname mutexevent sync.event
func mutexevent(cycles int64, skip int) {
	if cycles < 0 {
		cycles = 0
	}
	rate := int64(atomic.Load64(&mutexprofilerate))
	// TODO(pjw): measure impact of always calling fastrand vs using something
	// like malloc.go:nextSample()
	if rate > 0 && int64(fastrand())%rate == 0 {
		saveblockevent(cycles, skip+1, mutexProfile)
	}
}

// Go interface to profile data.

// A StackRecord describes a single execution stack.
type StackRecord struct {
	Stack0 [32]uintptr // stack trace for this record; ends at first 0 entry
}

// Stack returns the stack trace associated with the record,
// a prefix of r.Stack0.
func (r *StackRecord) Stack() []uintptr {
	for i, v := range r.Stack0 {
		if v == 0 {
			return r.Stack0[0:i]
		}
	}
	return r.Stack0[0:]
}

// MemProfileRate controls the fraction of memory allocations
// that are recorded and reported in the memory profile.
// The profiler aims to sample an average of
// one allocation per MemProfileRate bytes allocated.
//
// To include every allocated block in the profile, set MemProfileRate to 1.
// To turn off profiling entirely, set MemProfileRate to 0.
//
// The tools that process the memory profiles assume that the
// profile rate is constant across the lifetime of the program
// and equal to the current value. Programs that change the
// memory profiling rate should do so just once, as early as
// possible in the execution of the program (for example,
// at the beginning of main).
var MemProfileRate int = 512 * 1024

// A MemProfileRecord describes the live objects allocated
// by a particular call sequence (stack trace).
type MemProfileRecord struct {
	AllocBytes, FreeBytes     int64       // number of bytes allocated, freed
	AllocObjects, FreeObjects int64       // number of objects allocated, freed
	Stack0                    [32]uintptr // stack trace for this record; ends at first 0 entry
}

// InUseBytes returns the number of bytes in use (AllocBytes - FreeBytes).
func (r *MemProfileRecord) InUseBytes() int64 { return r.AllocBytes - r.FreeBytes }

// InUseObjects returns the number of objects in use (AllocObjects - FreeObjects).
func (r *MemProfileRecord) InUseObjects() int64 {
	return r.AllocObjects - r.FreeObjects
}

// Stack returns the stack trace associated with the record,
// a prefix of r.Stack0.
func (r *MemProfileRecord) Stack() []uintptr {
	for i, v := range r.Stack0 {
		if v == 0 {
			return r.Stack0[0:i]
		}
	}
	return r.Stack0[0:]
}

// reusebucket tries to pick a prunedProfile bucket off
// the freebuckets list, returning it if one is available or nil
// if the free list is empty.
func reusebucket(nstk int) *bucket {
	var b *bucket
	if freebuckets != nil {
		b = freebuckets
		freebuckets = freebuckets.allnext
		b.typ = prunedProfile
		b.nstk = uintptr(nstk)
		mp := b.mp()
		// Hack: rely on the fact that memprofile records are
		// larger than blockprofile records when clearing.
		*mp = memRecord{}
	}
	return b
}

// freebucket appends the specified prunedProfile bucket
// onto the free list, and removes references to it from the hash.
func freebucket(tofree *bucket) *bucket {
	// Thread this bucket into the free list.
	ret := tofree.allnext
	tofree.allnext = freebuckets
	freebuckets = tofree

	// Clean up the hash. The hash may point directly to this bucket...
	i := int(tofree.hash % buckHashSize)
	if buckhash[i] == tofree {
		buckhash[i] = tofree.next
	} else {
		// ... or when this bucket was inserted by stkbucket, it may have been
		// chained off some other unrelated bucket.
		for b := buckhash[i]; b != nil; b = b.next {
			if b.next == tofree {
				b.next = tofree.next
				break
			}
		}
	}
	return ret
}

// fixupStack takes a 'raw' stack trace (stack of PCs generated by
// callersRaw) and performs pre-symbolization fixup on it, returning
// the results in 'canonStack'. For each frame we look at the
// file/func/line information, then use that info to decide whether to
// include the frame in the final symbolized stack (removing frames
// corresponding to 'morestack' routines, for example). We also expand
// frames if the PC values to which they refer correponds to inlined
// functions to allow for expanded symbolic info to be filled in
// later. Note: there is code in go-callers.c's backtrace_full callback()
// function that performs very similar fixups; these two code paths
// should be kept in sync.
func fixupStack(stk []uintptr, skip int, canonStack *[maxStack]uintptr, size uintptr) int {
	var cidx int
	var termTrace bool
	// Increase the skip count to take into account the frames corresponding
	// to runtime.callersRaw and to the C routine that it invokes.
	skip += 2
	sawSigtramp := false
	for _, pc := range stk {
		// Subtract 1 from PC to undo the 1 we added in callback in
		// go-callers.c.
		function, file, _, frames := funcfileline(pc-1, -1, false)

		// Skip an unnamed function above sigtramp, as it is
		// likely the signal handler.
		if sawSigtramp {
			sawSigtramp = false
			if function == "" {
				continue
			}
		}

		// Skip split-stack functions (match by function name)
		skipFrame := false
		if hasPrefix(function, "_____morestack_") || hasPrefix(function, "__morestack_") {
			skipFrame = true
		}

		// Skip split-stack functions (match by file)
		if hasSuffix(file, "/morestack.S") {
			skipFrame = true
		}

		// Skip thunks and recover functions and other functions
		// specific to gccgo, that do not appear in the gc toolchain.
		fcn := function
		if hasSuffix(fcn, "..r") {
			skipFrame = true
		} else if function == "runtime.deferreturn" || function == "runtime.sighandler" {
			skipFrame = true
		} else if function == "runtime.sigtramp" || function == "runtime.sigtrampgo" {
			skipFrame = true
			// Also skip subsequent unnamed functions,
			// which will be the signal handler itself.
			sawSigtramp = true
		} else {
			for fcn != "" && (fcn[len(fcn)-1] >= '0' && fcn[len(fcn)-1] <= '9') {
				fcn = fcn[:len(fcn)-1]
			}
			if hasSuffix(fcn, "..stub") || hasSuffix(fcn, "..thunk") {
				skipFrame = true
			}
		}
		if skipFrame {
			continue
		}

		// Terminate the trace if we encounter a frame corresponding to
		// runtime.main, runtime.kickoff, makecontext, etc. See the
		// corresponding code in go-callers.c, callback function used
		// with backtrace_full.
		if function == "makecontext" {
			termTrace = true
		}
		if hasSuffix(file, "/proc.c") && function == "runtime_mstart" {
			termTrace = true
		}
		if hasSuffix(file, "/proc.go") &&
			(function == "runtime.main" || function == "runtime.kickoff") {
			termTrace = true
		}

		// Expand inline frames.
		for i := 0; i < frames; i++ {
			(*canonStack)[cidx] = pc
			cidx++
			if cidx >= maxStack {
				termTrace = true
				break
			}
		}
		if termTrace {
			break
		}
	}

	// Apply skip count. Needs to be done after expanding inline frames.
	if skip != 0 {
		if skip >= cidx {
			return 0
		}
		copy(canonStack[:cidx-skip], canonStack[skip:])
		return cidx - skip
	}

	return cidx
}

// fixupBucket takes a raw memprofile bucket and creates a new bucket
// in which the stack trace has been fixed up (inline frames expanded,
// unwanted frames stripped out). Original bucket is left unmodified;
// a new symbolizeProfile bucket may be generated as a side effect.
// Payload information from the original bucket is incorporated into
// the new bucket.
func fixupBucket(b *bucket) {
	var canonStack [maxStack]uintptr
	frames := fixupStack(b.stk(), b.skip, &canonStack, b.size)
	cb := stkbucket(prunedProfile, b.size, 0, canonStack[:frames], true)
	switch b.typ {
	default:
		throw("invalid profile bucket type")
	case memProfile:
		rawrecord := b.mp()
		cb.mp().active.add(&rawrecord.active)
	case blockProfile, mutexProfile:
		bpcount := b.bp().count
		cb.bp().count += bpcount
		cb.bp().cycles += bpcount
	}
}

// MemProfile returns a profile of memory allocated and freed per allocation
// site.
//
// MemProfile returns n, the number of records in the current memory profile.
// If len(p) >= n, MemProfile copies the profile into p and returns n, true.
// If len(p) < n, MemProfile does not change p and returns n, false.
//
// If inuseZero is true, the profile includes allocation records
// where r.AllocBytes > 0 but r.AllocBytes == r.FreeBytes.
// These are sites where memory was allocated, but it has all
// been released back to the runtime.
//
// The returned profile may be up to two garbage collection cycles old.
// This is to avoid skewing the profile toward allocations; because
// allocations happen in real time but frees are delayed until the garbage
// collector performs sweeping, the profile only accounts for allocations
// that have had a chance to be freed by the garbage collector.
//
// Most clients should use the runtime/pprof package or
// the testing package's -test.memprofile flag instead
// of calling MemProfile directly.
func MemProfile(p []MemProfileRecord, inuseZero bool) (n int, ok bool) {
	lock(&proflock)
	// If we're between mProf_NextCycle and mProf_Flush, take care
	// of flushing to the active profile so we only have to look
	// at the active profile below.
	mProf_FlushLocked()
	clear := true
	for b := mbuckets; b != nil; b = b.allnext {
		mp := b.mp()
		if inuseZero || mp.active.alloc_bytes != mp.active.free_bytes {
			n++
		}
		if mp.active.allocs != 0 || mp.active.frees != 0 {
			clear = false
		}
	}
	if clear {
		// Absolutely no data, suggesting that a garbage collection
		// has not yet happened. In order to allow profiling when
		// garbage collection is disabled from the beginning of execution,
		// accumulate all of the cycles, and recount buckets.
		n = 0
		for b := mbuckets; b != nil; b = b.allnext {
			mp := b.mp()
			for c := range mp.future {
				mp.active.add(&mp.future[c])
				mp.future[c] = memRecordCycle{}
			}
			if inuseZero || mp.active.alloc_bytes != mp.active.free_bytes {
				n++
			}
		}
	}
	if n <= len(p) {
		var bnext *bucket

		// Post-process raw buckets to fix up their stack traces
		for b := mbuckets; b != nil; b = bnext {
			bnext = b.allnext
			mp := b.mp()
			if inuseZero || mp.active.alloc_bytes != mp.active.free_bytes {
				fixupBucket(b)
			}
		}

		// Record pruned/fixed-up buckets
		ok = true
		idx := 0
		for b := sbuckets; b != nil; b = b.allnext {
			record(&p[idx], b)
			idx++
		}
		n = idx

		// Free up pruned buckets for use in next round
		for b := sbuckets; b != nil; b = bnext {
			bnext = freebucket(b)
		}
		sbuckets = nil
	}
	unlock(&proflock)
	return
}

// Write b's data to r.
func record(r *MemProfileRecord, b *bucket) {
	mp := b.mp()
	r.AllocBytes = int64(mp.active.alloc_bytes)
	r.FreeBytes = int64(mp.active.free_bytes)
	r.AllocObjects = int64(mp.active.allocs)
	r.FreeObjects = int64(mp.active.frees)
	for i, pc := range b.stk() {
		if i >= len(r.Stack0) {
			break
		}
		r.Stack0[i] = pc
	}
	for i := int(b.nstk); i < len(r.Stack0); i++ {
		r.Stack0[i] = 0
	}
}

func iterate_memprof(fn func(*bucket, uintptr, *uintptr, uintptr, uintptr, uintptr)) {
	lock(&proflock)
	for b := mbuckets; b != nil; b = b.allnext {
		mp := b.mp()
		fn(b, b.nstk, &b.stk()[0], b.size, mp.active.allocs, mp.active.frees)
	}
	unlock(&proflock)
}

// BlockProfileRecord describes blocking events originated
// at a particular call sequence (stack trace).
type BlockProfileRecord struct {
	Count  int64
	Cycles int64
	StackRecord
}

func harvestBlockMutexProfile(buckets *bucket, p []BlockProfileRecord) (n int, ok bool) {
	for b := buckets; b != nil; b = b.allnext {
		n++
	}
	if n <= len(p) {
		var bnext *bucket

		// Post-process raw buckets to create pruned/fixed-up buckets
		for b := buckets; b != nil; b = bnext {
			bnext = b.allnext
			fixupBucket(b)
		}

		// Record
		ok = true
		for b := sbuckets; b != nil; b = b.allnext {
			bp := b.bp()
			r := &p[0]
			r.Count = bp.count
			r.Cycles = bp.cycles
			i := 0
			var pc uintptr
			for i, pc = range b.stk() {
				if i >= len(r.Stack0) {
					break
				}
				r.Stack0[i] = pc
			}
			for ; i < len(r.Stack0); i++ {
				r.Stack0[i] = 0
			}
			p = p[1:]
		}

		// Free up pruned buckets for use in next round.
		for b := sbuckets; b != nil; b = bnext {
			bnext = freebucket(b)
		}
		sbuckets = nil
	}
	return
}

// BlockProfile returns n, the number of records in the current blocking profile.
// If len(p) >= n, BlockProfile copies the profile into p and returns n, true.
// If len(p) < n, BlockProfile does not change p and returns n, false.
//
// Most clients should use the runtime/pprof package or
// the testing package's -test.blockprofile flag instead
// of calling BlockProfile directly.
func BlockProfile(p []BlockProfileRecord) (n int, ok bool) {
	lock(&proflock)
	n, ok = harvestBlockMutexProfile(bbuckets, p)
	unlock(&proflock)
	return
}

// MutexProfile returns n, the number of records in the current mutex profile.
// If len(p) >= n, MutexProfile copies the profile into p and returns n, true.
// Otherwise, MutexProfile does not change p, and returns n, false.
//
// Most clients should use the runtime/pprof package
// instead of calling MutexProfile directly.
func MutexProfile(p []BlockProfileRecord) (n int, ok bool) {
	lock(&proflock)
	n, ok = harvestBlockMutexProfile(xbuckets, p)
	unlock(&proflock)
	return
}

// ThreadCreateProfile returns n, the number of records in the thread creation profile.
// If len(p) >= n, ThreadCreateProfile copies the profile into p and returns n, true.
// If len(p) < n, ThreadCreateProfile does not change p and returns n, false.
//
// Most clients should use the runtime/pprof package instead
// of calling ThreadCreateProfile directly.
func ThreadCreateProfile(p []StackRecord) (n int, ok bool) {
	first := (*m)(atomic.Loadp(unsafe.Pointer(&allm)))
	for mp := first; mp != nil; mp = mp.alllink {
		n++
	}
	if n <= len(p) {
		ok = true
		i := 0
		for mp := first; mp != nil; mp = mp.alllink {
			for j := range mp.createstack {
				p[i].Stack0[j] = mp.createstack[j].pc
			}
			i++
		}
	}
	return
}

// GoroutineProfile returns n, the number of records in the active goroutine stack profile.
// If len(p) >= n, GoroutineProfile copies the profile into p and returns n, true.
// If len(p) < n, GoroutineProfile does not change p and returns n, false.
//
// Most clients should use the runtime/pprof package instead
// of calling GoroutineProfile directly.
func GoroutineProfile(p []StackRecord) (n int, ok bool) {
	gp := getg()

	isOK := func(gp1 *g) bool {
		// Checking isSystemGoroutine here makes GoroutineProfile
		// consistent with both NumGoroutine and Stack.
		return gp1 != gp && readgstatus(gp1) != _Gdead && !isSystemGoroutine(gp1, false)
	}

	stopTheWorld("profile")

	n = 1
	for _, gp1 := range allgs {
		if isOK(gp1) {
			n++
		}
	}

	if n <= len(p) {
		ok = true
		r := p

		// Save current goroutine.
		saveg(gp, &r[0])
		r = r[1:]

		// Save other goroutines.
		for _, gp1 := range allgs {
			if isOK(gp1) {
				if len(r) == 0 {
					// Should be impossible, but better to return a
					// truncated profile than to crash the entire process.
					break
				}
				saveg(gp1, &r[0])
				r = r[1:]
			}
		}
	}

	startTheWorld()

	return n, ok
}

func saveg(gp *g, r *StackRecord) {
	if gp == getg() {
		var locbuf [32]location
		n := callers(1, locbuf[:])
		for i := 0; i < n; i++ {
			r.Stack0[i] = locbuf[i].pc
		}
		if n < len(r.Stack0) {
			r.Stack0[n] = 0
		}
	} else {
		// FIXME: Not implemented.
		r.Stack0[0] = 0
	}
}

// Stack formats a stack trace of the calling goroutine into buf
// and returns the number of bytes written to buf.
// If all is true, Stack formats stack traces of all other goroutines
// into buf after the trace for the current goroutine.
func Stack(buf []byte, all bool) int {
	if all {
		stopTheWorld("stack trace")
	}

	n := 0
	if len(buf) > 0 {
		gp := getg()
		// Force traceback=1 to override GOTRACEBACK setting,
		// so that Stack's results are consistent.
		// GOTRACEBACK is only about crash dumps.
		gp.m.traceback = 1
		gp.writebuf = buf[0:0:len(buf)]
		goroutineheader(gp)
		traceback(1)
		if all {
			tracebackothers(gp)
		}
		gp.m.traceback = 0
		n = len(gp.writebuf)
		gp.writebuf = nil
	}

	if all {
		startTheWorld()
	}
	return n
}

// Tracing of alloc/free/gc.

var tracelock mutex

func tracealloc(p unsafe.Pointer, size uintptr, typ *_type) {
	lock(&tracelock)
	gp := getg()
	gp.m.traceback = 2
	if typ == nil {
		print("tracealloc(", p, ", ", hex(size), ")\n")
	} else {
		print("tracealloc(", p, ", ", hex(size), ", ", typ.string(), ")\n")
	}
	if gp.m.curg == nil || gp == gp.m.curg {
		goroutineheader(gp)
		traceback(1)
	} else {
		goroutineheader(gp.m.curg)
		// FIXME: Can't do traceback of other g.
	}
	print("\n")
	gp.m.traceback = 0
	unlock(&tracelock)
}

func tracefree(p unsafe.Pointer, size uintptr) {
	lock(&tracelock)
	gp := getg()
	gp.m.traceback = 2
	print("tracefree(", p, ", ", hex(size), ")\n")
	goroutineheader(gp)
	traceback(1)
	print("\n")
	gp.m.traceback = 0
	unlock(&tracelock)
}

func tracegc() {
	lock(&tracelock)
	gp := getg()
	gp.m.traceback = 2
	print("tracegc()\n")
	// running on m->g0 stack; show all non-g0 goroutines
	tracebackothers(gp)
	print("end tracegc\n")
	print("\n")
	gp.m.traceback = 0
	unlock(&tracelock)
}
