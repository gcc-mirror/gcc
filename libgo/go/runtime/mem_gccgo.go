// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// The gccgo version of mem_*.go.

package runtime

import (
	"unsafe"
)

// Functions called by C code.
//go:linkname sysAlloc
//go:linkname sysFree

//extern mmap
func sysMmap(addr unsafe.Pointer, n uintptr, prot, flags, fd int32, off _libgo_off_t_type) unsafe.Pointer

//extern munmap
func munmap(addr unsafe.Pointer, length uintptr) int32

//extern madvise
func madvise(addr unsafe.Pointer, n uintptr, flags int32) int32

var mmapFD = int32(-1)

var devZero = []byte("/dev/zero\x00")

func init() {
	if _MAP_ANON == 0 {
		mmapFD = open(&devZero[0], 0 /* O_RDONLY */, 0)
		if mmapFD < 0 {
			println("open /dev/zero: errno=", errno())
			exit(2)
		}
	}
}

func mmap(addr unsafe.Pointer, n uintptr, prot, flags, fd int32, off uintptr) (unsafe.Pointer, int) {
	p := sysMmap(addr, n, prot, flags, fd, _libgo_off_t_type(off))
	if uintptr(p) == _MAP_FAILED {
		return nil, errno()
	}
	return p, 0
}

// Don't split the stack as this method may be invoked without a valid G, which
// prevents us from allocating more stack.
//
//go:nosplit
func sysAlloc(n uintptr, sysStat *sysMemStat) unsafe.Pointer {
	p, err := mmap(nil, n, _PROT_READ|_PROT_WRITE, _MAP_ANON|_MAP_PRIVATE, mmapFD, 0)
	if err != 0 {
		if err == _EACCES {
			print("runtime: mmap: access denied\n")
			exit(2)
		}
		if err == _EAGAIN {
			print("runtime: mmap: too much locked memory (check 'ulimit -l').\n")
			exit(2)
		}
		return nil
	}
	sysStat.add(int64(n))
	return p
}

func sysUnused(v unsafe.Pointer, n uintptr) {
	// By default, Linux's "transparent huge page" support will
	// merge pages into a huge page if there's even a single
	// present regular page, undoing the effects of the DONTNEED
	// below. On amd64, that means khugepaged can turn a single
	// 4KB page to 2MB, bloating the process's RSS by as much as
	// 512X. (See issue #8832 and Linux kernel bug
	// https://bugzilla.kernel.org/show_bug.cgi?id=93111)
	//
	// To work around this, we explicitly disable transparent huge
	// pages when we release pages of the heap. However, we have
	// to do this carefully because changing this flag tends to
	// split the VMA (memory mapping) containing v in to three
	// VMAs in order to track the different values of the
	// MADV_NOHUGEPAGE flag in the different regions. There's a
	// default limit of 65530 VMAs per address space (sysctl
	// vm.max_map_count), so we must be careful not to create too
	// many VMAs (see issue #12233).
	//
	// Since huge pages are huge, there's little use in adjusting
	// the MADV_NOHUGEPAGE flag on a fine granularity, so we avoid
	// exploding the number of VMAs by only adjusting the
	// MADV_NOHUGEPAGE flag on a large granularity. This still
	// gets most of the benefit of huge pages while keeping the
	// number of VMAs under control. With hugePageSize = 2MB, even
	// a pessimal heap can reach 128GB before running out of VMAs.
	if physHugePageSize != 0 && _MADV_NOHUGEPAGE != 0 {
		// If it's a large allocation, we want to leave huge
		// pages enabled. Hence, we only adjust the huge page
		// flag on the huge pages containing v and v+n-1, and
		// only if those aren't aligned.
		var head, tail uintptr
		if uintptr(v)%physHugePageSize != 0 {
			// Compute huge page containing v.
			head = uintptr(v) &^ (physHugePageSize - 1)
		}
		if (uintptr(v)+n)%physHugePageSize != 0 {
			// Compute huge page containing v+n-1.
			tail = (uintptr(v) + n - 1) &^ (physHugePageSize - 1)
		}

		// Note that madvise will return EINVAL if the flag is
		// already set, which is quite likely. We ignore
		// errors.
		if head != 0 && head+physHugePageSize == tail {
			// head and tail are different but adjacent,
			// so do this in one call.
			madvise(unsafe.Pointer(head), 2*physHugePageSize, _MADV_NOHUGEPAGE)
		} else {
			// Advise the huge pages containing v and v+n-1.
			if head != 0 {
				madvise(unsafe.Pointer(head), physHugePageSize, _MADV_NOHUGEPAGE)
			}
			if tail != 0 && tail != head {
				madvise(unsafe.Pointer(tail), physHugePageSize, _MADV_NOHUGEPAGE)
			}
		}
	}

	if uintptr(v)&(physPageSize-1) != 0 || n&(physPageSize-1) != 0 {
		// madvise will round this to any physical page
		// *covered* by this range, so an unaligned madvise
		// will release more memory than intended.
		throw("unaligned sysUnused")
	}

	if _MADV_DONTNEED != 0 {
		madvise(v, n, _MADV_DONTNEED)
	} else if _MADV_FREE != 0 {
		madvise(v, n, _MADV_FREE)
	}
}

func sysUsed(v unsafe.Pointer, n uintptr) {
	// Partially undo the NOHUGEPAGE marks from sysUnused
	// for whole huge pages between v and v+n. This may
	// leave huge pages off at the end points v and v+n
	// even though allocations may cover these entire huge
	// pages. We could detect this and undo NOHUGEPAGE on
	// the end points as well, but it's probably not worth
	// the cost because when neighboring allocations are
	// freed sysUnused will just set NOHUGEPAGE again.
	sysHugePage(v, n)
}

func sysHugePage(v unsafe.Pointer, n uintptr) {
	if physHugePageSize != 0 && _MADV_HUGEPAGE != 0 {
		// Round v up to a huge page boundary.
		beg := (uintptr(v) + (physHugePageSize - 1)) &^ (physHugePageSize - 1)
		// Round v+n down to a huge page boundary.
		end := (uintptr(v) + n) &^ (physHugePageSize - 1)

		if beg < end {
			madvise(unsafe.Pointer(beg), end-beg, _MADV_HUGEPAGE)
		}
	}
}

// Don't split the stack as this function may be invoked without a valid G,
// which prevents us from allocating more stack.
//
//go:nosplit
func sysFree(v unsafe.Pointer, n uintptr, sysStat *sysMemStat) {
	sysStat.add(-int64(n))
	munmap(v, n)
}

func sysFault(v unsafe.Pointer, n uintptr) {
	mmap(v, n, _PROT_NONE, _MAP_ANON|_MAP_PRIVATE|_MAP_FIXED, mmapFD, 0)
}

func sysReserve(v unsafe.Pointer, n uintptr) unsafe.Pointer {
	p, err := mmap(v, n, _PROT_NONE, _MAP_ANON|_MAP_PRIVATE, mmapFD, 0)
	if err != 0 {
		return nil
	}
	return p
}

func sysMap(v unsafe.Pointer, n uintptr, sysStat *sysMemStat) {
	sysStat.add(int64(n))

	if GOOS == "aix" {
		// AIX does not allow mapping a range that is already mapped.
		// So always unmap first even if it is already unmapped.
		munmap(v, n)
	}
	p, err := mmap(v, n, _PROT_READ|_PROT_WRITE, _MAP_ANON|_MAP_FIXED|_MAP_PRIVATE, mmapFD, 0)
	if err == _ENOMEM {
		throw("runtime: out of memory")
	}
	if p != v || err != 0 {
		throw("runtime: cannot map pages in arena address space")
	}
}
