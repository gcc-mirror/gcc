// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// The gccgo version of mem_*.go.

package runtime

import (
	"runtime/internal/sys"
	"unsafe"
)

// Functions called by C code.
//go:linkname sysAlloc runtime.sysAlloc

//extern mmap
func mmap(addr unsafe.Pointer, n uintptr, prot, flags, fd int32, off uintptr) unsafe.Pointer

//extern munmap
func munmap(addr unsafe.Pointer, length uintptr) int32

//extern mincore
func mincore(addr unsafe.Pointer, n uintptr, dst *byte) int32

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

// NOTE: vec must be just 1 byte long here.
// Mincore returns ENOMEM if any of the pages are unmapped,
// but we want to know that all of the pages are unmapped.
// To make these the same, we can only ask about one page
// at a time. See golang.org/issue/7476.
var addrspace_vec [1]byte

func addrspace_free(v unsafe.Pointer, n uintptr) bool {
	for off := uintptr(0); off < n; off += physPageSize {
		// Use a length of 1 byte, which the kernel will round
		// up to one physical page regardless of the true
		// physical page size.
		errval := 0
		if mincore(unsafe.Pointer(uintptr(v)+off), 1, &addrspace_vec[0]) < 0 {
			errval = errno()
		}
		if errval == _ENOSYS {
			// mincore is not available on this system.
			// Assume the address is available.
			return true
		}
		if errval == _EINVAL {
			// Address is not a multiple of the physical
			// page size. Shouldn't happen, but just ignore it.
			continue
		}
		// ENOMEM means unmapped, which is what we want.
		// Anything else we assume means the pages are mapped.
		if errval != _ENOMEM {
			return false
		}
	}
	return true
}

func mmap_fixed(v unsafe.Pointer, n uintptr, prot, flags, fd int32, offset uintptr) unsafe.Pointer {
	p := mmap(v, n, prot, flags, fd, offset)
	// On some systems, mmap ignores v without
	// MAP_FIXED, so retry if the address space is free.
	if p != v && addrspace_free(v, n) {
		if uintptr(p) != _MAP_FAILED {
			munmap(p, n)
		}
		p = mmap(v, n, prot, flags|_MAP_FIXED, fd, offset)
	}
	return p
}

// Don't split the stack as this method may be invoked without a valid G, which
// prevents us from allocating more stack.
//go:nosplit
func sysAlloc(n uintptr, sysStat *uint64) unsafe.Pointer {
	p := mmap(nil, n, _PROT_READ|_PROT_WRITE, _MAP_ANON|_MAP_PRIVATE, mmapFD, 0)
	if uintptr(p) == _MAP_FAILED {
		errval := errno()
		if errval == _EACCES {
			print("runtime: mmap: access denied\n")
			exit(2)
		}
		if errval == _EAGAIN {
			print("runtime: mmap: too much locked memory (check 'ulimit -l').\n")
			exit(2)
		}
		return nil
	}
	mSysStatInc(sysStat, n)
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
	if sys.HugePageSize != 0 && _MADV_NOHUGEPAGE != 0 {
		var s uintptr = sys.HugePageSize // division by constant 0 is a compile-time error :(

		// If it's a large allocation, we want to leave huge
		// pages enabled. Hence, we only adjust the huge page
		// flag on the huge pages containing v and v+n-1, and
		// only if those aren't aligned.
		var head, tail uintptr
		if uintptr(v)%s != 0 {
			// Compute huge page containing v.
			head = uintptr(v) &^ (s - 1)
		}
		if (uintptr(v)+n)%s != 0 {
			// Compute huge page containing v+n-1.
			tail = (uintptr(v) + n - 1) &^ (s - 1)
		}

		// Note that madvise will return EINVAL if the flag is
		// already set, which is quite likely. We ignore
		// errors.
		if head != 0 && head+sys.HugePageSize == tail {
			// head and tail are different but adjacent,
			// so do this in one call.
			madvise(unsafe.Pointer(head), 2*sys.HugePageSize, _MADV_NOHUGEPAGE)
		} else {
			// Advise the huge pages containing v and v+n-1.
			if head != 0 {
				madvise(unsafe.Pointer(head), sys.HugePageSize, _MADV_NOHUGEPAGE)
			}
			if tail != 0 && tail != head {
				madvise(unsafe.Pointer(tail), sys.HugePageSize, _MADV_NOHUGEPAGE)
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
	if sys.HugePageSize != 0 && _MADV_HUGEPAGE != 0 {
		// Partially undo the NOHUGEPAGE marks from sysUnused
		// for whole huge pages between v and v+n. This may
		// leave huge pages off at the end points v and v+n
		// even though allocations may cover these entire huge
		// pages. We could detect this and undo NOHUGEPAGE on
		// the end points as well, but it's probably not worth
		// the cost because when neighboring allocations are
		// freed sysUnused will just set NOHUGEPAGE again.
		var s uintptr = sys.HugePageSize

		// Round v up to a huge page boundary.
		beg := (uintptr(v) + (s - 1)) &^ (s - 1)
		// Round v+n down to a huge page boundary.
		end := (uintptr(v) + n) &^ (s - 1)

		if beg < end {
			madvise(unsafe.Pointer(beg), end-beg, _MADV_HUGEPAGE)
		}
	}
}

// Don't split the stack as this function may be invoked without a valid G,
// which prevents us from allocating more stack.
//go:nosplit
func sysFree(v unsafe.Pointer, n uintptr, sysStat *uint64) {
	mSysStatDec(sysStat, n)
	munmap(v, n)
}

func sysFault(v unsafe.Pointer, n uintptr) {
	mmap(v, n, _PROT_NONE, _MAP_ANON|_MAP_PRIVATE|_MAP_FIXED, mmapFD, 0)
}

func sysReserve(v unsafe.Pointer, n uintptr, reserved *bool) unsafe.Pointer {
	// On 64-bit, people with ulimit -v set complain if we reserve too
	// much address space. Instead, assume that the reservation is okay
	// if we can reserve at least 64K and check the assumption in SysMap.
	// Only user-mode Linux (UML) rejects these requests.
	if sys.PtrSize == 8 && uint64(n) > 1<<32 {
		p := mmap_fixed(v, 64<<10, _PROT_NONE, _MAP_ANON|_MAP_PRIVATE, mmapFD, 0)
		if p != v {
			if uintptr(p) != _MAP_FAILED {
				munmap(p, 64<<10)
			}
			return nil
		}
		munmap(p, 64<<10)
		*reserved = false
		return v
	}

	p := mmap(v, n, _PROT_NONE, _MAP_ANON|_MAP_PRIVATE, mmapFD, 0)
	if uintptr(p) == _MAP_FAILED {
		return nil
	}
	*reserved = true
	return p
}

func sysMap(v unsafe.Pointer, n uintptr, reserved bool, sysStat *uint64) {
	mSysStatInc(sysStat, n)

	// On 64-bit, we don't actually have v reserved, so tread carefully.
	if !reserved {
		flags := int32(_MAP_ANON | _MAP_PRIVATE)
		if GOOS == "dragonfly" {
			// TODO(jsing): For some reason DragonFly seems to return
			// memory at a different address than we requested, even when
			// there should be no reason for it to do so. This can be
			// avoided by using MAP_FIXED, but I'm not sure we should need
			// to do this - we do not on other platforms.
			flags |= _MAP_FIXED
		}
		p := mmap_fixed(v, n, _PROT_READ|_PROT_WRITE, flags, mmapFD, 0)
		if uintptr(p) == _MAP_FAILED && errno() == _ENOMEM {
			throw("runtime: out of memory")
		}
		if p != v {
			print("runtime: address space conflict: map(", v, ") = ", p, "\n")
			throw("runtime: address space conflict")
		}
		return
	}

	if GOOS == "aix" {
		// AIX does not allow mapping a range that is already mapped.
		// So always unmap first even if it is already unmapped.
		munmap(v, n)
	}
	p := mmap(v, n, _PROT_READ|_PROT_WRITE, _MAP_ANON|_MAP_FIXED|_MAP_PRIVATE, mmapFD, 0)
	if uintptr(p) == _MAP_FAILED && errno() == _ENOMEM {
		throw("runtime: out of memory")
	}
	if p != v {
		throw("runtime: cannot map pages in arena address space")
	}
}
