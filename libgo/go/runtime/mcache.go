// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

// This is a temporary mcache.go for gccgo.
// At some point it will be replaced by the one in the gc runtime package.

import "unsafe"

const (
	// Computed constant. The definition of MaxSmallSize and the
	// algorithm in msize.go produces some number of different allocation
	// size classes. NumSizeClasses is that number. It's needed here
	// because there are static arrays of this length; when msize runs its
	// size choosing algorithm it double-checks that NumSizeClasses agrees.
	_NumSizeClasses = 67
)

type mcachelist struct {
	list  *mlink
	nlist uint32
}

// Per-thread (in Go, per-P) cache for small objects.
// No locking needed because it is per-thread (per-P).
//
// mcaches are allocated from non-GC'd memory, so any heap pointers
// must be specially handled.
type mcache struct {
	// The following members are accessed on every malloc,
	// so they are grouped here for better caching.
	next_sample      int32   // trigger heap sample after allocating this many bytes
	local_cachealloc uintptr // bytes allocated (or freed) from cache since last lock of heap

	// Allocator cache for tiny objects w/o pointers.
	// See "Tiny allocator" comment in malloc.go.

	// tiny points to the beginning of the current tiny block, or
	// nil if there is no current tiny block.
	//
	// tiny is a heap pointer. Since mcache is in non-GC'd memory,
	// we handle it by clearing it in releaseAll during mark
	// termination.
	tiny     unsafe.Pointer
	tinysize uintptr

	// The rest is not accessed on every malloc.
	alloc [_NumSizeClasses]*mspan     // spans to allocate from
	free  [_NumSizeClasses]mcachelist // lists of explicitly freed objects

	// Local allocator stats, flushed during GC.
	local_nlookup    uintptr                  // number of pointer lookups
	local_largefree  uintptr                  // bytes freed for large objects (>maxsmallsize)
	local_nlargefree uintptr                  // number of frees for large objects (>maxsmallsize)
	local_nsmallfree [_NumSizeClasses]uintptr // number of frees for small objects (<=maxsmallsize)
}

type mtypes struct {
	compression byte
	data        uintptr
}

type special struct {
	next   *special
	offset uint16
	kind   byte
}

type mspan struct {
	next     *mspan // next span in list, or nil if none
	prev     *mspan // previous span's next field, or list head's first field if none
	start    uintptr
	npages   uintptr // number of pages in span
	freelist *mlink

	// sweep generation:
	// if sweepgen == h->sweepgen - 2, the span needs sweeping
	// if sweepgen == h->sweepgen - 1, the span is currently being swept
	// if sweepgen == h->sweepgen, the span is swept and ready to use
	// h->sweepgen is incremented by 2 after every GC

	sweepgen    uint32
	ref         uint16
	sizeclass   uint8   // size class
	incache     bool    // being used by an mcache
	state       uint8   // mspaninuse etc
	needzero    uint8   // needs to be zeroed before allocation
	elemsize    uintptr // computed from sizeclass or from npages
	unusedsince int64   // first time spotted by gc in mspanfree state
	npreleased  uintptr // number of pages released to the os
	limit       uintptr // end of data in span
	types       mtypes
	speciallock mutex    // guards specials list
	specials    *special // linked list of special records sorted by offset.
	freebuf     *mlink
}

type mlink struct {
	next *mlink
}
