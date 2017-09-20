// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Implementation of runtime/debug.WriteHeapDump. Writes all
// objects in the heap plus additional info (roots, threads,
// finalizers, etc.) to a file.

// The format of the dumped file is described at
// https://golang.org/s/go15heapdump.

package runtime

import (
	"runtime/internal/sys"
	"unsafe"
)

//go:linkname runtime_debug_WriteHeapDump runtime_debug.WriteHeapDump
func runtime_debug_WriteHeapDump(fd uintptr) {
	stopTheWorld("write heap dump")

	systemstack(func() {
		writeheapdump_m(fd)
	})

	startTheWorld()
}

const (
	fieldKindEol       = 0
	fieldKindPtr       = 1
	fieldKindIface     = 2
	fieldKindEface     = 3
	tagEOF             = 0
	tagObject          = 1
	tagOtherRoot       = 2
	tagType            = 3
	tagGoroutine       = 4
	tagStackFrame      = 5
	tagParams          = 6
	tagFinalizer       = 7
	tagItab            = 8
	tagOSThread        = 9
	tagMemStats        = 10
	tagQueuedFinalizer = 11
	tagData            = 12
	tagBSS             = 13
	tagDefer           = 14
	tagPanic           = 15
	tagMemProf         = 16
	tagAllocSample     = 17
)

var dumpfd uintptr // fd to write the dump to.
var tmpbuf []byte

// buffer of pending write data
const (
	bufSize = 4096
)

var buf [bufSize]byte
var nbuf uintptr

func dwrite(data unsafe.Pointer, len uintptr) {
	if len == 0 {
		return
	}
	if nbuf+len <= bufSize {
		copy(buf[nbuf:], (*[bufSize]byte)(data)[:len])
		nbuf += len
		return
	}

	write(dumpfd, unsafe.Pointer(&buf), int32(nbuf))
	if len >= bufSize {
		write(dumpfd, data, int32(len))
		nbuf = 0
	} else {
		copy(buf[:], (*[bufSize]byte)(data)[:len])
		nbuf = len
	}
}

func dwritebyte(b byte) {
	dwrite(unsafe.Pointer(&b), 1)
}

func flush() {
	write(dumpfd, unsafe.Pointer(&buf), int32(nbuf))
	nbuf = 0
}

// Cache of types that have been serialized already.
// We use a type's hash field to pick a bucket.
// Inside a bucket, we keep a list of types that
// have been serialized so far, most recently used first.
// Note: when a bucket overflows we may end up
// serializing a type more than once. That's ok.
const (
	typeCacheBuckets = 256
	typeCacheAssoc   = 4
)

type typeCacheBucket struct {
	t [typeCacheAssoc]*_type
}

var typecache [typeCacheBuckets]typeCacheBucket

// dump a uint64 in a varint format parseable by encoding/binary
func dumpint(v uint64) {
	var buf [10]byte
	var n int
	for v >= 0x80 {
		buf[n] = byte(v | 0x80)
		n++
		v >>= 7
	}
	buf[n] = byte(v)
	n++
	dwrite(unsafe.Pointer(&buf), uintptr(n))
}

func dumpbool(b bool) {
	if b {
		dumpint(1)
	} else {
		dumpint(0)
	}
}

// dump varint uint64 length followed by memory contents
func dumpmemrange(data unsafe.Pointer, len uintptr) {
	dumpint(uint64(len))
	dwrite(data, len)
}

func dumpslice(b []byte) {
	dumpint(uint64(len(b)))
	if len(b) > 0 {
		dwrite(unsafe.Pointer(&b[0]), uintptr(len(b)))
	}
}

func dumpstr(s string) {
	sp := stringStructOf(&s)
	dumpmemrange(sp.str, uintptr(sp.len))
}

// dump information for a type
func dumptype(t *_type) {
	if t == nil {
		return
	}

	// If we've definitely serialized the type before,
	// no need to do it again.
	b := &typecache[t.hash&(typeCacheBuckets-1)]
	if t == b.t[0] {
		return
	}
	for i := 1; i < typeCacheAssoc; i++ {
		if t == b.t[i] {
			// Move-to-front
			for j := i; j > 0; j-- {
				b.t[j] = b.t[j-1]
			}
			b.t[0] = t
			return
		}
	}

	// Might not have been dumped yet. Dump it and
	// remember we did so.
	for j := typeCacheAssoc - 1; j > 0; j-- {
		b.t[j] = b.t[j-1]
	}
	b.t[0] = t

	// dump the type
	dumpint(tagType)
	dumpint(uint64(uintptr(unsafe.Pointer(t))))
	dumpint(uint64(t.size))
	if x := t.uncommontype; x == nil || t.pkgPath == nil || *t.pkgPath == "" {
		dumpstr(*t.string)
	} else {
		pkgpathstr := *t.pkgPath
		pkgpath := stringStructOf(&pkgpathstr)
		namestr := *t.name
		name := stringStructOf(&namestr)
		dumpint(uint64(uintptr(pkgpath.len) + 1 + uintptr(name.len)))
		dwrite(pkgpath.str, uintptr(pkgpath.len))
		dwritebyte('.')
		dwrite(name.str, uintptr(name.len))
	}
	dumpbool(t.kind&kindDirectIface == 0 || t.kind&kindNoPointers == 0)
}

// dump an object
func dumpobj(obj unsafe.Pointer, size uintptr, bv bitvector) {
	dumpbvtypes(&bv, obj)
	dumpint(tagObject)
	dumpint(uint64(uintptr(obj)))
	dumpmemrange(obj, size)
	dumpfields(bv)
}

func dumpotherroot(description string, to unsafe.Pointer) {
	dumpint(tagOtherRoot)
	dumpstr(description)
	dumpint(uint64(uintptr(to)))
}

func dumpfinalizer(obj unsafe.Pointer, fn *funcval, ft *functype, ot *ptrtype) {
	dumpint(tagFinalizer)
	dumpint(uint64(uintptr(obj)))
	dumpint(uint64(uintptr(unsafe.Pointer(fn))))
	dumpint(uint64(uintptr(unsafe.Pointer(fn.fn))))
	dumpint(uint64(uintptr(unsafe.Pointer(ft))))
	dumpint(uint64(uintptr(unsafe.Pointer(ot))))
}

type childInfo struct {
	// Information passed up from the callee frame about
	// the layout of the outargs region.
	argoff uintptr   // where the arguments start in the frame
	arglen uintptr   // size of args region
	args   bitvector // if args.n >= 0, pointer map of args region
	sp     *uint8    // callee sp
	depth  uintptr   // depth in call stack (0 == most recent)
}

// dump kinds & offsets of interesting fields in bv
func dumpbv(cbv *bitvector, offset uintptr) {
	bv := gobv(*cbv)
	for i := uintptr(0); i < bv.n; i++ {
		if bv.bytedata[i/8]>>(i%8)&1 == 1 {
			dumpint(fieldKindPtr)
			dumpint(uint64(offset + i*sys.PtrSize))
		}
	}
}

func dumpgoroutine(gp *g) {
	sp := gp.syscallsp

	dumpint(tagGoroutine)
	dumpint(uint64(uintptr(unsafe.Pointer(gp))))
	dumpint(uint64(sp))
	dumpint(uint64(gp.goid))
	dumpint(uint64(gp.gopc))
	dumpint(uint64(readgstatus(gp)))
	dumpbool(isSystemGoroutine(gp))
	dumpbool(false) // isbackground
	dumpint(uint64(gp.waitsince))
	dumpstr(gp.waitreason)
	dumpint(0)
	dumpint(uint64(uintptr(unsafe.Pointer(gp.m))))
	dumpint(uint64(uintptr(unsafe.Pointer(gp._defer))))
	dumpint(uint64(uintptr(unsafe.Pointer(gp._panic))))

	// dump defer & panic records
	for d := gp._defer; d != nil; d = d.link {
		dumpint(tagDefer)
		dumpint(uint64(uintptr(unsafe.Pointer(d))))
		dumpint(uint64(uintptr(unsafe.Pointer(gp))))
		dumpint(0)
		dumpint(0)
		dumpint(uint64(uintptr(unsafe.Pointer(d.pfn))))
		dumpint(0)
		dumpint(uint64(uintptr(unsafe.Pointer(d.link))))
	}
	for p := gp._panic; p != nil; p = p.link {
		dumpint(tagPanic)
		dumpint(uint64(uintptr(unsafe.Pointer(p))))
		dumpint(uint64(uintptr(unsafe.Pointer(gp))))
		eface := efaceOf(&p.arg)
		dumpint(uint64(uintptr(unsafe.Pointer(eface._type))))
		dumpint(uint64(uintptr(unsafe.Pointer(eface.data))))
		dumpint(0) // was p->defer, no longer recorded
		dumpint(uint64(uintptr(unsafe.Pointer(p.link))))
	}
}

func dumpgs() {
	// goroutines & stacks
	for i := 0; uintptr(i) < allglen; i++ {
		gp := allgs[i]
		status := readgstatus(gp) // The world is stopped so gp will not be in a scan state.
		switch status {
		default:
			print("runtime: unexpected G.status ", hex(status), "\n")
			throw("dumpgs in STW - bad status")
		case _Gdead:
			// ok
		case _Grunnable,
			_Gsyscall,
			_Gwaiting:
			dumpgoroutine(gp)
		}
	}
}

func finq_callback(fn *funcval, obj unsafe.Pointer, ft *functype, ot *ptrtype) {
	dumpint(tagQueuedFinalizer)
	dumpint(uint64(uintptr(obj)))
	dumpint(uint64(uintptr(unsafe.Pointer(fn))))
	dumpint(uint64(uintptr(unsafe.Pointer(fn.fn))))
	dumpint(uint64(uintptr(unsafe.Pointer(ft))))
	dumpint(uint64(uintptr(unsafe.Pointer(ot))))
}

func dumproots() {
	// MSpan.types
	for _, s := range mheap_.allspans {
		if s.state == _MSpanInUse {
			// Finalizers
			for sp := s.specials; sp != nil; sp = sp.next {
				if sp.kind != _KindSpecialFinalizer {
					continue
				}
				spf := (*specialfinalizer)(unsafe.Pointer(sp))
				p := unsafe.Pointer(s.base() + uintptr(spf.special.offset))
				dumpfinalizer(p, spf.fn, spf.ft, spf.ot)
			}
		}
	}

	// Finalizer queue
	iterate_finq(finq_callback)
}

// Bit vector of free marks.
// Needs to be as big as the largest number of objects per span.
var freemark [_PageSize / 8]bool

func dumpobjs() {
	for _, s := range mheap_.allspans {
		if s.state != _MSpanInUse {
			continue
		}
		p := s.base()
		size := s.elemsize
		n := (s.npages << _PageShift) / size
		if n > uintptr(len(freemark)) {
			throw("freemark array doesn't have enough entries")
		}

		for freeIndex := uintptr(0); freeIndex < s.nelems; freeIndex++ {
			if s.isFree(freeIndex) {
				freemark[freeIndex] = true
			}
		}

		for j := uintptr(0); j < n; j, p = j+1, p+size {
			if freemark[j] {
				freemark[j] = false
				continue
			}
			dumpobj(unsafe.Pointer(p), size, makeheapobjbv(p, size))
		}
	}
}

func dumpparams() {
	dumpint(tagParams)
	x := uintptr(1)
	if *(*byte)(unsafe.Pointer(&x)) == 1 {
		dumpbool(false) // little-endian ptrs
	} else {
		dumpbool(true) // big-endian ptrs
	}
	dumpint(sys.PtrSize)
	dumpint(uint64(mheap_.arena_start))
	dumpint(uint64(mheap_.arena_used))
	dumpstr(sys.GOARCH)
	dumpstr(sys.Goexperiment)
	dumpint(uint64(ncpu))
}

func dumpms() {
	for mp := allm; mp != nil; mp = mp.alllink {
		dumpint(tagOSThread)
		dumpint(uint64(uintptr(unsafe.Pointer(mp))))
		dumpint(uint64(mp.id))
		dumpint(mp.procid)
	}
}

func dumpmemstats() {
	dumpint(tagMemStats)
	dumpint(memstats.alloc)
	dumpint(memstats.total_alloc)
	dumpint(memstats.sys)
	dumpint(memstats.nlookup)
	dumpint(memstats.nmalloc)
	dumpint(memstats.nfree)
	dumpint(memstats.heap_alloc)
	dumpint(memstats.heap_sys)
	dumpint(memstats.heap_idle)
	dumpint(memstats.heap_inuse)
	dumpint(memstats.heap_released)
	dumpint(memstats.heap_objects)
	dumpint(memstats.stacks_inuse)
	dumpint(memstats.stacks_sys)
	dumpint(memstats.mspan_inuse)
	dumpint(memstats.mspan_sys)
	dumpint(memstats.mcache_inuse)
	dumpint(memstats.mcache_sys)
	dumpint(memstats.buckhash_sys)
	dumpint(memstats.gc_sys)
	dumpint(memstats.other_sys)
	dumpint(memstats.next_gc)
	dumpint(memstats.last_gc_unix)
	dumpint(memstats.pause_total_ns)
	for i := 0; i < 256; i++ {
		dumpint(memstats.pause_ns[i])
	}
	dumpint(uint64(memstats.numgc))
}

func dumpmemprof_callback(b *bucket, nstk uintptr, pstk *location, size, allocs, frees uintptr) {
	stk := (*[100000]location)(unsafe.Pointer(pstk))
	dumpint(tagMemProf)
	dumpint(uint64(uintptr(unsafe.Pointer(b))))
	dumpint(uint64(size))
	dumpint(uint64(nstk))
	for i := uintptr(0); i < nstk; i++ {
		pc := stk[i].pc
		fn := stk[i].function
		file := stk[i].filename
		line := stk[i].lineno
		if fn == "" {
			var buf [64]byte
			n := len(buf)
			n--
			buf[n] = ')'
			if pc == 0 {
				n--
				buf[n] = '0'
			} else {
				for pc > 0 {
					n--
					buf[n] = "0123456789abcdef"[pc&15]
					pc >>= 4
				}
			}
			n--
			buf[n] = 'x'
			n--
			buf[n] = '0'
			n--
			buf[n] = '('
			dumpslice(buf[n:])
			dumpstr("?")
			dumpint(0)
		} else {
			dumpstr(fn)
			dumpstr(file)
			dumpint(uint64(line))
		}
	}
	dumpint(uint64(allocs))
	dumpint(uint64(frees))
}

func dumpmemprof() {
	iterate_memprof(dumpmemprof_callback)
	for _, s := range mheap_.allspans {
		if s.state != _MSpanInUse {
			continue
		}
		for sp := s.specials; sp != nil; sp = sp.next {
			if sp.kind != _KindSpecialProfile {
				continue
			}
			spp := (*specialprofile)(unsafe.Pointer(sp))
			p := s.base() + uintptr(spp.special.offset)
			dumpint(tagAllocSample)
			dumpint(uint64(p))
			dumpint(uint64(uintptr(unsafe.Pointer(spp.b))))
		}
	}
}

var dumphdr = []byte("go1.7 heap dump\n")

func mdump() {
	// make sure we're done sweeping
	for _, s := range mheap_.allspans {
		if s.state == _MSpanInUse {
			s.ensureSwept()
		}
	}
	memclrNoHeapPointers(unsafe.Pointer(&typecache), unsafe.Sizeof(typecache))
	dwrite(unsafe.Pointer(&dumphdr[0]), uintptr(len(dumphdr)))
	dumpparams()
	dumpobjs()
	dumpgs()
	dumpms()
	dumproots()
	dumpmemstats()
	dumpmemprof()
	dumpint(tagEOF)
	flush()
}

func writeheapdump_m(fd uintptr) {
	_g_ := getg()
	casgstatus(_g_.m.curg, _Grunning, _Gwaiting)
	_g_.waitreason = "dumping heap"

	// Update stats so we can dump them.
	// As a side effect, flushes all the MCaches so the MSpan.freelist
	// lists contain all the free objects.
	updatememstats()

	// Set dump file.
	dumpfd = fd

	// Call dump routine.
	mdump()

	// Reset dump file.
	dumpfd = 0
	if tmpbuf != nil {
		sysFree(unsafe.Pointer(&tmpbuf[0]), uintptr(len(tmpbuf)), &memstats.other_sys)
		tmpbuf = nil
	}

	casgstatus(_g_.m.curg, _Gwaiting, _Grunning)
}

// dumpint() the kind & offset of each field in an object.
func dumpfields(bv bitvector) {
	dumpbv(&bv, 0)
	dumpint(fieldKindEol)
}

// The heap dump reader needs to be able to disambiguate
// Eface entries. So it needs to know every type that might
// appear in such an entry. The following routine accomplishes that.
// TODO(rsc, khr): Delete - no longer possible.

// Dump all the types that appear in the type field of
// any Eface described by this bit vector.
func dumpbvtypes(bv *bitvector, base unsafe.Pointer) {
}

func makeheapobjbv(p uintptr, size uintptr) bitvector {
	// Extend the temp buffer if necessary.
	nptr := size / sys.PtrSize
	if uintptr(len(tmpbuf)) < nptr/8+1 {
		if tmpbuf != nil {
			sysFree(unsafe.Pointer(&tmpbuf[0]), uintptr(len(tmpbuf)), &memstats.other_sys)
		}
		n := nptr/8 + 1
		p := sysAlloc(n, &memstats.other_sys)
		if p == nil {
			throw("heapdump: out of memory")
		}
		tmpbuf = (*[1 << 30]byte)(p)[:n]
	}
	// Convert heap bitmap to pointer bitmap.
	for i := uintptr(0); i < nptr/8+1; i++ {
		tmpbuf[i] = 0
	}
	i := uintptr(0)
	hbits := heapBitsForAddr(p)
	for ; i < nptr; i++ {
		if i != 1 && !hbits.morePointers() {
			break // end of object
		}
		if hbits.isPointer() {
			tmpbuf[i/8] |= 1 << (i % 8)
		}
		hbits = hbits.next()
	}
	return bitvector{int32(i), &tmpbuf[0]}
}

type gobitvector struct {
	n        uintptr
	bytedata []uint8
}

func gobv(bv bitvector) gobitvector {
	return gobitvector{
		uintptr(bv.n),
		(*[1 << 30]byte)(unsafe.Pointer(bv.bytedata))[:(bv.n+7)/8],
	}
}
