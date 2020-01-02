// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"unsafe"
)

// Return the minimum value seen for the zone CPU cap, or 0 if no cap is
// detected.
func getcpucap() uint64 {
	// The resource control block is an opaque object whose size is only
	// known to libc.  In practice, given the contents, it is unlikely to
	// grow beyond 8KB so we'll use a static buffer of that size here.
	const rblkmaxsize = 8 * 1024
	if rctlblk_size() > rblkmaxsize {
		return 0
	}

	// The "zone.cpu-cap" resource control, as described in
	// resource_controls(5), "sets a limit on the amount of CPU time that
	// can be used by a zone.  The unit used is the percentage of a single
	// CPU that can be used by all user threads in a zone, expressed as an
	// integer."  A C string of the name must be passed to getrctl(2).
	name := []byte("zone.cpu-cap\x00")

	// To iterate over the list of values for a particular resource
	// control, we need two blocks: one for the previously read value and
	// one for the next value.
	var rblk0 [rblkmaxsize]byte
	var rblk1 [rblkmaxsize]byte
	rblk := &rblk0[0]
	rblkprev := &rblk1[0]

	var flag uint32 = _RCTL_FIRST
	var capval uint64 = 0

	for {
		if getrctl(unsafe.Pointer(&name[0]), unsafe.Pointer(rblkprev), unsafe.Pointer(rblk), flag) != 0 {
			// The end of the sequence is reported as an ENOENT
			// failure, but determining the CPU cap is not critical
			// here.  We'll treat any failure as if it were the end
			// of sequence.
			break
		}

		lflags := rctlblk_get_local_flags(unsafe.Pointer(rblk))
		action := rctlblk_get_local_action(unsafe.Pointer(rblk), 0)
		if (lflags&_RCTL_LOCAL_MAXIMAL) == 0 && action == _RCTL_LOCAL_DENY {
			// This is a finite (not maximal) value representing a
			// cap (deny) action.
			v := rctlblk_get_value(unsafe.Pointer(rblk))
			if capval == 0 || capval > v {
				capval = v
			}
		}

		// Swap the blocks around so that we can fetch the next value
		t := rblk
		rblk = rblkprev
		rblkprev = t
		flag = _RCTL_NEXT
	}

	return capval
}

func getncpu() int32 {
	n := int32(sysconf(__SC_NPROCESSORS_ONLN))
	if n < 1 {
		return 1
	}

	if cents := int32(getcpucap()); cents > 0 {
		// Convert from a percentage of CPUs to a number of CPUs,
		// rounding up to make use of a fractional CPU
		// e.g., 336% becomes 4 CPUs
		ncap := (cents + 99) / 100
		if ncap < n {
			return ncap
		}
	}

	return n
}

//extern getrctl
func getrctl(controlname, oldbuf, newbuf unsafe.Pointer, flags uint32) int32

//extern rctlblk_get_local_action
func rctlblk_get_local_action(buf, signalp unsafe.Pointer) uint32

//extern rctlblk_get_local_flags
func rctlblk_get_local_flags(buf unsafe.Pointer) uint32

//extern rctlblk_get_value
func rctlblk_get_value(buf unsafe.Pointer) uint64

//extern rctlblk_size
func rclblk_size() uintptr
