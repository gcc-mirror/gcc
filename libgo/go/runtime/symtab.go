// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

// Frames may be used to get function/file/line information for a
// slice of PC values returned by Callers.
type Frames struct {
	callers []uintptr

	// The last PC we saw.
	last uintptr

	// The number of times we've seen last.
	lastCount int
}

// Frame is the information returned by Frames for each call frame.
type Frame struct {
	// Program counter for this frame; multiple frames may have
	// the same PC value.
	PC uintptr

	// Func for this frame; may be nil for non-Go code or fully
	// inlined functions.
	Func *Func

	// Function name, file name, and line number for this call frame.
	// May be the empty string or zero if not known.
	// If Func is not nil then Function == Func.Name().
	Function string
	File     string
	Line     int

	// Entry point for the function; may be zero if not known.
	// If Func is not nil then Entry == Func.Entry().
	Entry uintptr
}

// CallersFrames takes a slice of PC values returned by Callers and
// prepares to return function/file/line information.
// Do not change the slice until you are done with the Frames.
func CallersFrames(callers []uintptr) *Frames {
	return &Frames{callers: callers}
}

// Next returns frame information for the next caller.
// If more is false, there are no more callers (the Frame value is valid).
func (ci *Frames) Next() (frame Frame, more bool) {
	if len(ci.callers) == 0 {
		return Frame{}, false
	}

	pc := ci.callers[0]
	ci.callers = ci.callers[1:]

	i := 0
	if pc == ci.last {
		ci.lastCount++
		i = ci.lastCount
	} else {
		ci.last = pc
		ci.lastCount = 0
	}
	more = len(ci.callers) > 0

	// Subtract 1 from PC to undo the 1 we added in callback in
	// go-callers.c.
	function, file, line := funcfileline(pc-1, int32(i))
	if function == "" && file == "" {
		return Frame{}, more
	}
	entry := funcentry(pc - 1)
	f := &Func{name: function, entry: entry}

	xpc := pc
	if xpc > entry {
		xpc--
	}

	frame = Frame{
		PC:       xpc,
		Func:     f,
		Function: function,
		File:     file,
		Line:     line,
		Entry:    entry,
	}

	return frame, more
}

// NOTE: Func does not expose the actual unexported fields, because we return *Func
// values to users, and we want to keep them from being able to overwrite the data
// with (say) *f = Func{}.
// All code operating on a *Func must call raw to get the *_func instead.

// A Func represents a Go function in the running binary.
type Func struct {
	name  string
	entry uintptr
}

// FuncForPC returns a *Func describing the function that contains the
// given program counter address, or else nil.
func FuncForPC(pc uintptr) *Func {
	name, _, _ := funcfileline(pc, -1)
	if name == "" {
		return nil
	}
	entry := funcentry(pc)
	return &Func{name: name, entry: entry}
}

// Name returns the name of the function.
func (f *Func) Name() string {
	return f.name
}

// Entry returns the entry address of the function.
func (f *Func) Entry() uintptr {
	return f.entry
}

// FileLine returns the file name and line number of the
// source code corresponding to the program counter pc.
// The result will not be accurate if pc is not a program
// counter within f.
func (f *Func) FileLine(pc uintptr) (file string, line int) {
	_, file, line = funcfileline(pc, -1)
	return file, line
}

// implemented in go-caller.c
func funcfileline(uintptr, int32) (string, string, int)
func funcentry(uintptr) uintptr
