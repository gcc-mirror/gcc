// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import (
	"internal/bytealg"
	_ "unsafe" // for go:linkname
)

// Frames may be used to get function/file/line information for a
// slice of PC values returned by Callers.
type Frames struct {
	// callers is a slice of PCs that have not yet been expanded to frames.
	callers []uintptr

	// The last PC we saw.
	last uintptr

	// The number of times we've seen last.
	lastCount int
}

// Frame is the information returned by Frames for each call frame.
type Frame struct {
	// PC is the program counter for the location in this frame.
	// For a frame that calls another frame, this will be the
	// program counter of a call instruction. Because of inlining,
	// multiple frames may have the same PC value, but different
	// symbolic information.
	PC uintptr

	// Func is the Func value of this call frame. This may be nil
	// for non-Go code or fully inlined functions.
	Func *Func

	// Function is the package path-qualified function name of
	// this call frame. If non-empty, this string uniquely
	// identifies a single function in the program.
	// This may be the empty string if not known.
	// If Func is not nil then Function == Func.Name().
	Function string

	// File and Line are the file name and line number of the
	// location in this frame. For non-leaf frames, this will be
	// the location of a call. These may be the empty string and
	// zero, respectively, if not known.
	File string
	Line int

	// Entry point program counter for the function; may be zero
	// if not known. If Func is not nil then Entry ==
	// Func.Entry().
	Entry uintptr
}

// CallersFrames takes a slice of PC values returned by Callers and
// prepares to return function/file/line information.
// Do not change the slice until you are done with the Frames.
func CallersFrames(callers []uintptr) *Frames {
	return &Frames{callers: callers}
}

// Next returns a Frame representing the next call frame in the slice
// of PC values. If it has already returned all call frames, Next
// returns a zero Frame.
//
// The more result indicates whether the next call to Next will return
// a valid Frame. It does not necessarily indicate whether this call
// returned one.
//
// See the Frames example for idiomatic usage.
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
	function, file, line, _ := funcfileline(pc-1, int32(i), more)
	if function == "" && file == "" {
		return Frame{}, more
	}

	// Demangle function name if needed.
	function = demangleSymbol(function)

	// Create entry.
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

//go:noescape
// pcInlineCallers is written in C.
func pcInlineCallers(pc uintptr, locbuf *location, max int32) int32

// runtime_expandFinalInlineFrame expands the final pc in stk to include all
// "callers" if pc is inline.
//
//go:linkname runtime_expandFinalInlineFrame runtime_1pprof.runtime__expandFinalInlineFrame
func runtime_expandFinalInlineFrame(stk []uintptr) []uintptr {
	if len(stk) == 0 {
		return stk
	}
	pc := stk[len(stk)-1]
	tracepc := pc - 1

	var locbuf [_TracebackMaxFrames]location
	n := pcInlineCallers(tracepc, &locbuf[0], int32(len(locbuf)))

	// Returning the same PC several times causes Frame.Next to do
	// the right thing.
	for i := int32(1); i < n; i++ {
		stk = append(stk, pc)
	}

	return stk
}

// NOTE: Func does not expose the actual unexported fields, because we return *Func
// values to users, and we want to keep them from being able to overwrite the data
// with (say) *f = Func{}.
// All code operating on a *Func must call raw() to get the *_func
// or funcInfo() to get the funcInfo instead.

// A Func represents a Go function in the running binary.
type Func struct {
	name  string
	entry uintptr
}

// FuncForPC returns a *Func describing the function that contains the
// given program counter address, or else nil.
//
// If pc represents multiple functions because of inlining, it returns
// the *Func describing the innermost function, but with an entry of
// the outermost function.
func FuncForPC(pc uintptr) *Func {
	name, _, _, _ := funcfileline(pc, -1, false)
	if name == "" {
		return nil
	}
	entry := funcentry(pc)
	return &Func{name: name, entry: entry}
}

// Name returns the name of the function.
func (f *Func) Name() string {
	if f == nil {
		return ""
	}
	return f.name
}

// Entry returns the entry address of the function.
func (f *Func) Entry() uintptr {
	if f == nil {
		return 0
	}
	return f.entry
}

// FileLine returns the file name and line number of the
// source code corresponding to the program counter pc.
// The result will not be accurate if pc is not a program
// counter within f.
func (f *Func) FileLine(pc uintptr) (file string, line int) {
	_, file, line, _ = funcfileline(pc, -1, false)
	return file, line
}

func hexval(b byte) uint {
	if b >= '0' && b <= '9' {
		return uint(b - '0')
	}
	if b >= 'a' && b <= 'f' {
		return uint(b-'a') + 10
	}
	return 0
}

func hexDigitsToRune(digits []byte, ndig int) rune {
	result := uint(0)
	for i := 0; i < ndig; i++ {
		result <<= uint(4)
		result |= hexval(digits[i])
	}
	return rune(result)
}

// decodeIdentifier performs an in-place decoding on the input byte slice.
// This undoes the compiler underscore mangling.
// Returns the number of bytes used by the result.
func decodeIdentifier(bsl []byte) int {
	underscoreCodes := map[byte]byte{
		'_': '_',
		'0': '.',
		'1': '/',
		'2': '*',
		'3': ',',
		'4': '{',
		'5': '}',
		'6': '[',
		'7': ']',
		'8': '(',
		'9': ')',
		'a': '"',
		'b': ' ',
		'c': ';',
	}

	j := 0
	for i := 0; i < len(bsl); i++ {
		b := bsl[i]
		if b != '_' || i+1 >= len(bsl) {
			bsl[j] = b
			j++
			continue
		}

		if d, ok := underscoreCodes[bsl[i+1]]; ok {
			i++
			bsl[j] = d
			j++
			continue
		}

		rlen := 0
		switch bsl[i+1] {
		case 'x':
			rlen = 2
		case 'u':
			rlen = 4
		case 'U':
			rlen = 8
		}

		if rlen > 0 && i+1+rlen < len(bsl) {
			r := hexDigitsToRune(bsl[i+2:], rlen)
			nc := encoderune(bsl[j:], r)
			j += nc
			i += rlen + 1
		} else {
			bsl[j] = b
			j++
		}
	}
	return j
}

// Demangle a function symbol. Applies the reverse of go_encode_id()
// as used in the compiler.

func demangleSymbol(s string) string {
	if bytealg.IndexByteString(s, '.') < 0 {
		// A symbol with no '.' is not a Go symbol.
		return s
	}

	bsl := []byte(s)
	nchars := decodeIdentifier(bsl)
	bsl = bsl[:nchars]
	return string(bsl)
}

// implemented in go-caller.c
func funcfileline(uintptr, int32, bool) (string, string, int, int)
func funcentry(uintptr) uintptr
