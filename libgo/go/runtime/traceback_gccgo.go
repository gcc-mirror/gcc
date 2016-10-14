// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Traceback support for gccgo.
// The actual traceback code is written in C.

package runtime

import (
	"runtime/internal/sys"
	_ "unsafe" // for go:linkname
)

// For gccgo, use go:linkname to rename compiler-called functions to
// themselves, so that the compiler will export them.
// These are temporary for C runtime code to call.
//go:linkname traceback runtime.traceback
//go:linkname printtrace runtime.printtrace
//go:linkname goroutineheader runtime.goroutineheader
//go:linkname printcreatedby runtime.printcreatedby

func printcreatedby(gp *g) {
	// Show what created goroutine, except main goroutine (goid 1).
	pc := gp.gopc
	tracepc := pc // back up to CALL instruction for funcfileline.
	entry := funcentry(tracepc)
	if entry != 0 && tracepc > entry {
		tracepc -= sys.PCQuantum
	}
	function, file, line := funcfileline(tracepc, -1)
	if function != "" && showframe(function, gp) && gp.goid != 1 {
		print("created by ", function, "\n")
		print("\t", file, ":", line)
		if entry != 0 && pc > entry {
			print(" +", hex(pc-entry))
		}
		print("\n")
	}
}

// tracebackg is used to collect stack traces from other goroutines.
type tracebackg struct {
	gp     *g
	locbuf [_TracebackMaxFrames]location
	c      int
}

// location is a location in the program, used for backtraces.
type location struct {
	pc       uintptr
	filename string
	function string
	lineno   int
}

//extern runtime_callers
func c_callers(skip int32, locbuf *location, max int32, keepThunks bool) int32

// callers returns a stack trace of the current goroutine.
// The gc version of callers takes []uintptr, but we take []location.
func callers(skip int, locbuf []location) int {
	n := c_callers(int32(skip), &locbuf[0], int32(len(locbuf)), false)
	return int(n)
}

// traceback prints a traceback of the current goroutine.
// This differs from the gc version, which is given pc, sp, lr and g and
// can print a traceback of any goroutine.
func traceback() {
	var locbuf [100]location
	c := c_callers(1, &locbuf[0], int32(len(locbuf)), false)
	printtrace(locbuf[:c], getg())
}

// printtrace prints a traceback from locbuf.
func printtrace(locbuf []location, gp *g) {
	for i := range locbuf {
		if showframe(locbuf[i].function, gp) {
			print(locbuf[i].function, "\n\t", locbuf[i].filename, ":", locbuf[i].lineno)
		}
	}
}

// showframe returns whether to print a frame in a traceback.
// name is the function name.
func showframe(name string, gp *g) bool {
	g := getg()
	if g.m.throwing > 0 && gp != nil && (gp == g.m.curg || gp == g.m.caughtsig.ptr()) {
		return true
	}
	level, _, _ := gotraceback()

	// Special case: always show runtime.gopanic frame, so that we can
	// see where a panic started in the middle of a stack trace.
	// See golang.org/issue/5832.
	// __go_panic is the current gccgo name.
	if name == "runtime.gopanic" || name == "__go_panic" {
		return true
	}

	return level > 1 || contains(name, ".") && (!hasprefix(name, "runtime.") || isExportedRuntime(name))
}

// isExportedRuntime reports whether name is an exported runtime function.
// It is only for runtime functions, so ASCII A-Z is fine.
func isExportedRuntime(name string) bool {
	const n = len("runtime.")
	return len(name) > n && name[:n] == "runtime." && 'A' <= name[n] && name[n] <= 'Z'
}

var gStatusStrings = [...]string{
	_Gidle:      "idle",
	_Grunnable:  "runnable",
	_Grunning:   "running",
	_Gsyscall:   "syscall",
	_Gwaiting:   "waiting",
	_Gdead:      "dead",
	_Gcopystack: "copystack",
}

func goroutineheader(gp *g) {
	gpstatus := readgstatus(gp)

	isScan := gpstatus&_Gscan != 0
	gpstatus &^= _Gscan // drop the scan bit

	// Basic string status
	var status string
	if 0 <= gpstatus && gpstatus < uint32(len(gStatusStrings)) {
		status = gStatusStrings[gpstatus]
	} else {
		status = "???"
	}

	// Override.
	if gpstatus == _Gwaiting && gp.waitreason != "" {
		status = gp.waitreason
	}

	// approx time the G is blocked, in minutes
	var waitfor int64
	if (gpstatus == _Gwaiting || gpstatus == _Gsyscall) && gp.waitsince != 0 {
		waitfor = (nanotime() - gp.waitsince) / 60e9
	}
	print("goroutine ", gp.goid, " [", status)
	if isScan {
		print(" (scan)")
	}
	if waitfor >= 1 {
		print(", ", waitfor, " minutes")
	}
	if gp.lockedm != nil {
		print(", locked to thread")
	}
	print("]:\n")
}

// isSystemGoroutine reports whether the goroutine g must be omitted in
// stack dumps and deadlock detector.
func isSystemGoroutine(gp *g) bool {
	// FIXME.
	return false
}
