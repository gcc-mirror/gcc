// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Traceback support for gccgo.
// The actual traceback code is written in C.

package runtime

import (
	"runtime/internal/sys"
	"unsafe"
)

func printcreatedby(gp *g) {
	// Show what created goroutine, except main goroutine (goid 1).
	pc := gp.gopc
	tracepc := pc // back up to CALL instruction for funcfileline.
	entry := funcentry(tracepc)
	if entry != 0 && tracepc > entry {
		tracepc -= sys.PCQuantum
	}
	function, file, line, _ := funcfileline(tracepc, -1)
	if function != "" && showframe(function, gp, false) && gp.goid != 1 {
		printcreatedby1(function, file, line, entry, pc)
	}
}

func printcreatedby1(function, file string, line int, entry, pc uintptr) {
	print("created by ", function, "\n")
	print("\t", file, ":", line)
	if entry != 0 && pc > entry {
		print(" +", hex(pc-entry))
	}
	print("\n")
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

//go:noescape
//extern runtime_callers
func c_callers(skip int32, locbuf *location, max int32, keepThunks bool) int32

// callers returns a stack trace of the current goroutine.
// The gc version of callers takes []uintptr, but we take []location.
func callers(skip int, locbuf []location) int {
	n := c_callers(int32(skip)+1, &locbuf[0], int32(len(locbuf)), false)
	return int(n)
}

//go:noescape
//extern runtime_callersRaw
func c_callersRaw(pcs *uintptr, max int32) int32

// callersRaw returns a raw (PCs only) stack trace of the current goroutine.
func callersRaw(pcbuf []uintptr) int {
	n := c_callersRaw(&pcbuf[0], int32(len(pcbuf)))
	return int(n)
}

// traceback prints a traceback of the current goroutine.
// This differs from the gc version, which is given pc, sp, lr and g and
// can print a traceback of any goroutine.
func traceback(skip int32) {
	var locbuf [100]location
	c := c_callers(skip+1, &locbuf[0], int32(len(locbuf)), false)
	gp := getg()
	printtrace(locbuf[:c], gp)
	printcreatedby(gp)

	if gp.ancestors == nil {
		return
	}
	for _, ancestor := range *gp.ancestors {
		printAncestorTraceback(ancestor)
	}
}

// printAncestorTraceback prints the traceback of the given ancestor.
func printAncestorTraceback(ancestor ancestorInfo) {
	print("[originating from goroutine ", ancestor.goid, "]:\n")
	for fidx, pc := range ancestor.pcs {
		function, file, line, _ := funcfileline(pc, -1)
		if showfuncinfo(function, fidx == 0) {
			printAncestorTracebackFuncInfo(function, file, line, pc)
		}
	}
	if len(ancestor.pcs) == _TracebackMaxFrames {
		print("...additional frames elided...\n")
	}
	// Show what created goroutine, except main goroutine (goid 1).
	function, file, line, _ := funcfileline(ancestor.gopc, -1)
	if function != "" && showfuncinfo(function, false) && ancestor.goid != 1 {
		printcreatedby1(function, file, line, funcentry(ancestor.gopc), ancestor.gopc)
	}
}

// printAncestorTraceback prints the given function info at a given pc
// within an ancestor traceback. The precision of this info is reduced
// due to only have access to the pcs at the time of the caller
// goroutine being created.
func printAncestorTracebackFuncInfo(name, file string, line int, pc uintptr) {
	if name == "runtime.gopanic" {
		name = "panic"
	}
	print(name, "(...)\n")
	print("\t", file, ":", line)
	entry := funcentry(pc)
	if pc > entry {
		print(" +", hex(pc-entry))
	}
	print("\n")
}

// printtrace prints a traceback from locbuf.
func printtrace(locbuf []location, gp *g) {
	nprint := 0
	for i := range locbuf {
		if showframe(locbuf[i].function, gp, nprint == 0) {
			name := locbuf[i].function
			if name == "runtime.gopanic" {
				name = "panic"
			}
			print(name, "\n\t", locbuf[i].filename, ":", locbuf[i].lineno, "\n")
			nprint++
		}
	}
}

// showframe returns whether to print a frame in a traceback.
// name is the function name.
func showframe(name string, gp *g, firstFrame bool) bool {
	g := getg()
	if g.m.throwing > 0 && gp != nil && (gp == g.m.curg || gp == g.m.caughtsig.ptr()) {
		return true
	}
	return showfuncinfo(name, firstFrame)
}

func showfuncinfo(name string, firstFrame bool) bool {
	// Gccgo can trace back through C functions called via cgo.
	// We want to print those in the traceback.
	// But unless GOTRACEBACK > 1 (checked below), still skip
	// internal C functions and cgo-generated functions.
	if name != "" && !contains(name, ".") && !hasPrefix(name, "__go_") && !hasPrefix(name, "_cgo_") {
		return true
	}

	level, _, _ := gotraceback()
	if level > 1 {
		// Show all frames.
		return true
	}

	if name == "" {
		return false
	}

	// Special case: always show runtime.gopanic frame
	// in the middle of a stack trace, so that we can
	// see the boundary between ordinary code and
	// panic-induced deferred code.
	// See golang.org/issue/5832.
	if name == "runtime.gopanic" && !firstFrame {
		return true
	}

	return contains(name, ".") && (!hasPrefix(name, "runtime.") || isExportedRuntime(name))
}

// isExportedRuntime reports whether name is an exported runtime function.
// It is only for runtime functions, so ASCII A-Z is fine. Here also check
// for mangled functions from runtime/<...>, which will be prefixed with
// "runtime..z2f".
func isExportedRuntime(name string) bool {
	const n = len("runtime.")
	if hasPrefix(name, "runtime..z2f") {
		return true
	}
	return len(name) > n && name[:n] == "runtime." && 'A' <= name[n] && name[n] <= 'Z'
}

var gStatusStrings = [...]string{
	_Gidle:           "idle",
	_Grunnable:       "runnable",
	_Grunning:        "running",
	_Gsyscall:        "syscall",
	_Gwaiting:        "waiting",
	_Gdead:           "dead",
	_Gcopystack:      "copystack",
	_Gexitingsyscall: "exiting syscall",
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
	if gpstatus == _Gwaiting && gp.waitreason != waitReasonZero {
		status = gp.waitreason.String()
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
	if gp.lockedm != 0 {
		print(", locked to thread")
	}
	print("]:\n")
}

// isSystemGoroutine reports whether the goroutine g must be omitted
// in stack dumps and deadlock detector. This is any goroutine that
// starts at a runtime.* entry point, except for runtime.main and
// sometimes runtime.runfinq.
//
// If fixed is true, any goroutine that can vary between user and
// system (that is, the finalizer goroutine) is considered a user
// goroutine.
func isSystemGoroutine(gp *g, fixed bool) bool {
	if !gp.isSystemGoroutine {
		return false
	}
	if fixed && gp.isFinalizerGoroutine {
		// This goroutine can vary. In fixed mode,
		// always consider it a user goroutine.
		return false
	}
	return true
}

func tracebackothers(me *g) {
	var tb tracebackg
	tb.gp = me

	// The getTraceback function will modify me's stack context.
	// Preserve it in case we have been called via systemstack.
	context := me.context
	stackcontext := me.stackcontext

	level, _, _ := gotraceback()

	// Show the current goroutine first, if we haven't already.
	g := getg()
	gp := g.m.curg
	if gp != nil && gp != me {
		print("\n")
		goroutineheader(gp)
		gp.traceback = (uintptr)(noescape(unsafe.Pointer(&tb)))
		getTraceback(me, gp)
		printtrace(tb.locbuf[:tb.c], nil)
		printcreatedby(gp)
	}

	lock(&allglock)
	for _, gp := range allgs {
		if gp == me || gp == g.m.curg || readgstatus(gp) == _Gdead || isSystemGoroutine(gp, false) && level < 2 {
			continue
		}
		print("\n")
		goroutineheader(gp)

		// gccgo's only mechanism for doing a stack trace is
		// _Unwind_Backtrace.  And that only works for the
		// current thread, not for other random goroutines.
		// So we need to switch context to the goroutine, get
		// the backtrace, and then switch back.
		//
		// This means that if g is running or in a syscall, we
		// can't reliably print a stack trace.  FIXME.

		// Note: gp.m == g.m occurs when tracebackothers is
		// called from a signal handler initiated during a
		// systemstack call. The original G is still in the
		// running state, and we want to print its stack.
		if gp.m != g.m && readgstatus(gp)&^_Gscan == _Grunning {
			print("\tgoroutine running on other thread; stack unavailable\n")
			printcreatedby(gp)
		} else if readgstatus(gp)&^_Gscan == _Gsyscall {
			print("\tgoroutine in C code; stack unavailable\n")
			printcreatedby(gp)
		} else {
			gp.traceback = (uintptr)(noescape(unsafe.Pointer(&tb)))
			getTraceback(me, gp)
			printtrace(tb.locbuf[:tb.c], nil)
			printcreatedby(gp)
		}
	}
	unlock(&allglock)

	me.context = context
	me.stackcontext = stackcontext
}
