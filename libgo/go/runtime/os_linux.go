// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"internal/goarch"
	"runtime/internal/atomic"
	"unsafe"
)

type mOS struct {
	// profileTimer holds the ID of the POSIX interval timer for profiling CPU
	// usage on this thread.
	//
	// It is valid when the profileTimerValid field is non-zero. A thread
	// creates and manages its own timer, and these fields are read and written
	// only by this thread. But because some of the reads on profileTimerValid
	// are in signal handling code, access to that field uses atomic operations.
	profileTimer      int32
	profileTimerValid uint32
}

// setSigeventTID is written in C to set the sigev_notify_thread_id
// field of a sigevent struct.
//
//go:noescape
func setSigeventTID(*_sigevent, int32)

func getProcID() uint64 {
	return uint64(gettid())
}

func futex(addr unsafe.Pointer, op int32, val uint32, ts, addr2 unsafe.Pointer, val3 uint32) int32 {
	return int32(syscall(_SYS_futex, uintptr(addr), uintptr(op), uintptr(val), uintptr(ts), uintptr(addr2), uintptr(val3)))
}

// For sched_getaffinity use the system call rather than the libc call,
// because the system call returns the number of entries set by the kernel.
func sched_getaffinity(pid _pid_t, cpusetsize uintptr, mask *byte) int32 {
	return int32(syscall(_SYS_sched_getaffinity, uintptr(pid), cpusetsize, uintptr(unsafe.Pointer(mask)), 0, 0, 0))
}

// Linux futex.
//
//	futexsleep(uint32 *addr, uint32 val)
//	futexwakeup(uint32 *addr)
//
// Futexsleep atomically checks if *addr == val and if so, sleeps on addr.
// Futexwakeup wakes up threads sleeping on addr.
// Futexsleep is allowed to wake up spuriously.

const (
	_FUTEX_PRIVATE_FLAG = 128
	_FUTEX_WAIT_PRIVATE = 0 | _FUTEX_PRIVATE_FLAG
	_FUTEX_WAKE_PRIVATE = 1 | _FUTEX_PRIVATE_FLAG
)

// Atomically,
//
//	if(*addr == val) sleep
//
// Might be woken up spuriously; that's allowed.
// Don't sleep longer than ns; ns < 0 means forever.
//
//go:nosplit
func futexsleep(addr *uint32, val uint32, ns int64) {
	// Some Linux kernels have a bug where futex of
	// FUTEX_WAIT returns an internal error code
	// as an errno. Libpthread ignores the return value
	// here, and so can we: as it says a few lines up,
	// spurious wakeups are allowed.
	if ns < 0 {
		futex(unsafe.Pointer(addr), _FUTEX_WAIT_PRIVATE, val, nil, nil, 0)
		return
	}

	var ts timespec
	ts.setNsec(ns)
	futex(unsafe.Pointer(addr), _FUTEX_WAIT_PRIVATE, val, unsafe.Pointer(&ts), nil, 0)
}

// If any procs are sleeping on addr, wake up at most cnt.
//
//go:nosplit
func futexwakeup(addr *uint32, cnt uint32) {
	ret := futex(unsafe.Pointer(addr), _FUTEX_WAKE_PRIVATE, cnt, nil, nil, 0)
	if ret >= 0 {
		return
	}

	// I don't know that futex wakeup can return
	// EAGAIN or EINTR, but if it does, it would be
	// safe to loop and call futex again.
	systemstack(func() {
		print("futexwakeup addr=", addr, " returned ", ret, "\n")
	})

	*(*int32)(unsafe.Pointer(uintptr(0x1006))) = 0x1006
}

func getproccount() int32 {
	// This buffer is huge (8 kB) but we are on the system stack
	// and there should be plenty of space (64 kB).
	// Also this is a leaf, so we're not holding up the memory for long.
	// See golang.org/issue/11823.
	// The suggested behavior here is to keep trying with ever-larger
	// buffers, but we don't have a dynamic memory allocator at the
	// moment, so that's a bit tricky and seems like overkill.
	const maxCPUs = 64 * 1024
	var buf [maxCPUs / 8]byte
	r := sched_getaffinity(0, unsafe.Sizeof(buf), &buf[0])
	if r < 0 {
		return 1
	}
	n := int32(0)
	for _, v := range buf[:r] {
		for v != 0 {
			n += int32(v & 1)
			v >>= 1
		}
	}
	if n == 0 {
		n = 1
	}
	return n
}

const (
	_AT_NULL   = 0  // End of vector
	_AT_PAGESZ = 6  // System physical page size
	_AT_HWCAP  = 16 // hardware capability bit vector
	_AT_RANDOM = 25 // introduced in 2.6.29
	_AT_HWCAP2 = 26 // hardware capability bit vector 2
)

var procAuxv = []byte("/proc/self/auxv\x00")

var addrspace_vec [1]byte

//extern-sysinfo mincore
func mincore(addr unsafe.Pointer, n uintptr, dst *byte) int32

func sysargs(argc int32, argv **byte) {
	n := argc + 1

	// skip over argv, envp to get to auxv
	for argv_index(argv, n) != nil {
		n++
	}

	// skip NULL separator
	n++

	// now argv+n is auxv
	auxv := (*[1 << 28]uintptr)(add(unsafe.Pointer(argv), uintptr(n)*goarch.PtrSize))
	if sysauxv(auxv[:]) != 0 {
		return
	}
	// In some situations we don't get a loader-provided
	// auxv, such as when loaded as a library on Android.
	// Fall back to /proc/self/auxv.
	fd := open(&procAuxv[0], 0 /* O_RDONLY */, 0)
	if fd < 0 {
		// On Android, /proc/self/auxv might be unreadable (issue 9229), so we fallback to
		// try using mincore to detect the physical page size.
		// mincore should return EINVAL when address is not a multiple of system page size.
		const size = 256 << 10 // size of memory region to allocate
		p, err := mmap(nil, size, _PROT_READ|_PROT_WRITE, _MAP_ANON|_MAP_PRIVATE, -1, 0)
		if err != 0 {
			return
		}
		var n uintptr
		for n = 4 << 10; n < size; n <<= 1 {
			err := mincore(unsafe.Pointer(uintptr(p)+n), 1, &addrspace_vec[0])
			if err == 0 {
				physPageSize = n
				break
			}
		}
		if physPageSize == 0 {
			physPageSize = size
		}
		munmap(p, size)
		return
	}
	var buf [128]uintptr
	n = read(fd, noescape(unsafe.Pointer(&buf[0])), int32(unsafe.Sizeof(buf)))
	closefd(fd)
	if n < 0 {
		return
	}
	// Make sure buf is terminated, even if we didn't read
	// the whole file.
	buf[len(buf)-2] = _AT_NULL
	sysauxv(buf[:])
}

func sysauxv(auxv []uintptr) int {
	var i int
	for ; auxv[i] != _AT_NULL; i += 2 {
		tag, val := auxv[i], auxv[i+1]
		switch tag {
		case _AT_RANDOM:
			// The kernel provides a pointer to 16-bytes
			// worth of random data.
			startupRandomData = (*[16]byte)(unsafe.Pointer(val))[:]

			setRandomNumber(uint32(startupRandomData[4]) | uint32(startupRandomData[5])<<8 |
				uint32(startupRandomData[6])<<16 | uint32(startupRandomData[7])<<24)

		case _AT_PAGESZ:
			physPageSize = val
		}

		archauxv(tag, val)

		// Commented out for gccgo for now.
		// vdsoauxv(tag, val)
	}
	return i / 2
}

var sysTHPSizePath = []byte("/sys/kernel/mm/transparent_hugepage/hpage_pmd_size\x00")

func getHugePageSize() uintptr {
	var numbuf [20]byte
	fd := open(&sysTHPSizePath[0], 0 /* O_RDONLY */, 0)
	if fd < 0 {
		return 0
	}
	ptr := noescape(unsafe.Pointer(&numbuf[0]))
	n := read(fd, ptr, int32(len(numbuf)))
	closefd(fd)
	if n <= 0 {
		return 0
	}
	n-- // remove trailing newline
	v, ok := atoi(slicebytetostringtmp((*byte)(ptr), int(n)))
	if !ok || v < 0 {
		v = 0
	}
	if v&(v-1) != 0 {
		// v is not a power of 2
		return 0
	}
	return uintptr(v)
}

func osinit() {
	ncpu = getproccount()
	physHugePageSize = getHugePageSize()
}

func timer_create(clockid int32, sevp *_sigevent, timerid *int32) int32 {
	return int32(syscall(_SYS_timer_create, uintptr(clockid), uintptr(unsafe.Pointer(sevp)), uintptr(unsafe.Pointer(timerid)), 0, 0, 0))
}

func timer_settime(timerid int32, flags int32, new, old *_itimerspec) int32 {
	return int32(syscall(_SYS_timer_settime, uintptr(timerid), uintptr(flags), uintptr(unsafe.Pointer(new)), uintptr(unsafe.Pointer(old)), 0, 0))
}

func timer_delete(timerid int32) int32 {
	return int32(syscall(_SYS_timer_delete, uintptr(timerid), 0, 0, 0, 0, 0))
}

// go118UseTimerCreateProfiler enables the per-thread CPU profiler.
const go118UseTimerCreateProfiler = true

// validSIGPROF compares this signal delivery's code against the signal sources
// that the profiler uses, returning whether the delivery should be processed.
// To be processed, a signal delivery from a known profiling mechanism should
// correspond to the best profiling mechanism available to this thread. Signals
// from other sources are always considered valid.
//
//go:nosplit
func validSIGPROF(mp *m, c *sigctxt) bool {
	code := int32(c.sigcode())
	setitimer := code == _SI_KERNEL
	timer_create := code == _SI_TIMER

	if !(setitimer || timer_create) {
		// The signal doesn't correspond to a profiling mechanism that the
		// runtime enables itself. There's no reason to process it, but there's
		// no reason to ignore it either.
		return true
	}

	if mp == nil {
		// Since we don't have an M, we can't check if there's an active
		// per-thread timer for this thread. We don't know how long this thread
		// has been around, and if it happened to interact with the Go scheduler
		// at a time when profiling was active (causing it to have a per-thread
		// timer). But it may have never interacted with the Go scheduler, or
		// never while profiling was active. To avoid double-counting, process
		// only signals from setitimer.
		//
		// When a custom cgo traceback function has been registered (on
		// platforms that support runtime.SetCgoTraceback), SIGPROF signals
		// delivered to a thread that cannot find a matching M do this check in
		// the assembly implementations of runtime.cgoSigtramp.
		return setitimer
	}

	// Having an M means the thread interacts with the Go scheduler, and we can
	// check whether there's an active per-thread timer for this thread.
	if atomic.Load(&mp.profileTimerValid) != 0 {
		// If this M has its own per-thread CPU profiling interval timer, we
		// should track the SIGPROF signals that come from that timer (for
		// accurate reporting of its CPU usage; see issue 35057) and ignore any
		// that it gets from the process-wide setitimer (to not over-count its
		// CPU consumption).
		return timer_create
	}

	// No active per-thread timer means the only valid profiler is setitimer.
	return setitimer
}

func setProcessCPUProfiler(hz int32) {
	setProcessCPUProfilerTimer(hz)
}

func setThreadCPUProfiler(hz int32) {
	mp := getg().m
	mp.profilehz = hz

	if !go118UseTimerCreateProfiler {
		return
	}

	// destroy any active timer
	if atomic.Load(&mp.profileTimerValid) != 0 {
		timerid := mp.profileTimer
		atomic.Store(&mp.profileTimerValid, 0)
		mp.profileTimer = 0

		ret := timer_delete(timerid)
		if ret != 0 {
			print("runtime: failed to disable profiling timer; timer_delete(", timerid, ") errno=", -ret, "\n")
			throw("timer_delete")
		}
	}

	if hz == 0 {
		// If the goal was to disable profiling for this thread, then the job's done.
		return
	}

	// The period of the timer should be 1/Hz. For every "1/Hz" of additional
	// work, the user should expect one additional sample in the profile.
	//
	// But to scale down to very small amounts of application work, to observe
	// even CPU usage of "one tenth" of the requested period, set the initial
	// timing delay in a different way: So that "one tenth" of a period of CPU
	// spend shows up as a 10% chance of one sample (for an expected value of
	// 0.1 samples), and so that "two and six tenths" periods of CPU spend show
	// up as a 60% chance of 3 samples and a 40% chance of 2 samples (for an
	// expected value of 2.6). Set the initial delay to a value in the unifom
	// random distribution between 0 and the desired period. And because "0"
	// means "disable timer", add 1 so the half-open interval [0,period) turns
	// into (0,period].
	//
	// Otherwise, this would show up as a bias away from short-lived threads and
	// from threads that are only occasionally active: for example, when the
	// garbage collector runs on a mostly-idle system, the additional threads it
	// activates may do a couple milliseconds of GC-related work and nothing
	// else in the few seconds that the profiler observes.
	spec := new(_itimerspec)
	spec.it_value.setNsec(1 + int64(fastrandn(uint32(1e9/hz))))
	spec.it_interval.setNsec(1e9 / int64(hz))

	var timerid int32
	var sevp _sigevent
	sevp.sigev_notify = _SIGEV_THREAD_ID
	sevp.sigev_signo = _SIGPROF
	setSigeventTID(&sevp, int32(mp.procid))
	ret := timer_create(_CLOCK_THREAD_CPUTIME_ID, &sevp, &timerid)
	if ret != 0 {
		// If we cannot create a timer for this M, leave profileTimerValid false
		// to fall back to the process-wide setitimer profiler.
		return
	}

	ret = timer_settime(timerid, 0, spec, nil)
	if ret != 0 {
		print("runtime: failed to configure profiling timer; timer_settime(", timerid,
			", 0, {interval: {",
			spec.it_interval.tv_sec, "s + ", spec.it_interval.tv_nsec, "ns} value: {",
			spec.it_value.tv_sec, "s + ", spec.it_value.tv_nsec, "ns}}, nil) errno=", -ret, "\n")
		throw("timer_settime")
	}

	mp.profileTimer = timerid
	atomic.Store(&mp.profileTimerValid, 1)
}
