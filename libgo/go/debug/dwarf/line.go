// Copyright 2012 The Go Authors.  All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// DWARF line number information.

package dwarf

import (
	"errors"
	"path/filepath"
	"sort"
	"strconv"
)

// A Line holds all the available information about the source code
// corresponding to a specific program counter address.
type Line struct {
	Filename      string // source file name
	OpIndex       int    // index of operation in VLIW instruction
	Line          int    // line number
	Column        int    // column number
	ISA           int    // instruction set code
	Discriminator int    // block discriminator
	Stmt          bool   // instruction starts statement
	Block         bool   // instruction starts basic block
	EndPrologue   bool   // instruction ends function prologue
	BeginEpilogue bool   // instruction begins function epilogue
}

// LineForPc returns the line number information for a program counter
// address, if any.  When this returns multiple Line structures in a
// context where only one can be used, the last one is the best.
func (d *Data) LineForPC(pc uint64) ([]*Line, error) {
	for i := range d.unit {
		u := &d.unit[i]
		if u.pc == nil {
			if err := d.readUnitLine(i, u); err != nil {
				return nil, err
			}
		}
		for _, ar := range u.pc {
			if pc >= ar.low && pc < ar.high {
				return d.findLine(u, pc)
			}
		}
	}
	return nil, nil
}

// readUnitLine reads in the line number information for a compilation
// unit.
func (d *Data) readUnitLine(i int, u *unit) error {
	r := d.unitReader(i)
	setLineOff := false
	for {
		e, err := r.Next()
		if err != nil {
			return err
		}
		if e == nil {
			break
		}
		if r.unit != i {
			break
		}
		switch e.Tag {
		case TagCompileUnit, TagSubprogram, TagEntryPoint, TagInlinedSubroutine:
			low, lowok := e.Val(AttrLowpc).(uint64)
			high, highok := e.Val(AttrHighpc).(uint64)
			if lowok && highok {
				u.pc = append(u.pc, addrRange{low, high})
			} else if f, ok := e.Val(AttrRanges).(Offset); ok {
				// TODO: Handle AttrRanges and .debug_ranges.
				_ = f
			}
			if off, ok := e.Val(AttrStmtList).(int64); ok {
				u.lineoff = Offset(off)
				setLineOff = true
			}
			if dir, ok := e.Val(AttrCompDir).(string); ok {
				u.dir = dir
			}
		}
	}
	if !setLineOff {
		u.lineoff = Offset(0)
		u.lineoff--
	}
	return nil
}

// findLine finds the line information for a PC value, given the unit
// containing the information.
func (d *Data) findLine(u *unit, pc uint64) ([]*Line, error) {
	if u.lines == nil {
		if err := d.parseLine(u); err != nil {
			return nil, err
		}
	}

	for _, ln := range u.lines {
		if pc < ln.addrs[0].pc || pc > ln.addrs[len(ln.addrs)-1].pc {
			continue
		}
		i := sort.Search(len(ln.addrs),
			func(i int) bool { return ln.addrs[i].pc > pc })
		i--
		p := new(Line)
		*p = ln.line
		p.Line = ln.addrs[i].line
		ret := []*Line{p}
		for i++; i < len(ln.addrs) && ln.addrs[i].pc == pc; i++ {
			p = new(Line)
			*p = ln.line
			p.Line = ln.addrs[i].line
			ret = append(ret, p)
		}
		return ret, nil
	}

	return nil, nil
}

// FileLine returns the file name and line number for a program
// counter address, or "", 0 if unknown.
func (d *Data) FileLine(pc uint64) (string, int, error) {
	r, err := d.LineForPC(pc)
	if err != nil {
		return "", 0, err
	}
	if r == nil {
		return "", 0, nil
	}
	ln := r[len(r)-1]
	return ln.Filename, ln.Line, nil
}

// A mapLineInfo holds the PC values and line numbers associated with
// a single Line structure.  This representation is chosen to reduce
// memory usage based on typical debug info.
type mapLineInfo struct {
	line  Line      // line.Line will be zero
	addrs lineAddrs // sorted by PC
}

// A list of lines.  This will be sorted by PC.
type lineAddrs []oneLineInfo

func (p lineAddrs) Len() int           { return len(p) }
func (p lineAddrs) Less(i, j int) bool { return p[i].pc < p[j].pc }
func (p lineAddrs) Swap(i, j int)      { p[i], p[j] = p[j], p[i] }

// A oneLineInfo is a single PC and line number.
type oneLineInfo struct {
	pc   uint64
	line int
}

// A lineHdr holds the relevant information from a line number
// program header.
type lineHdr struct {
	version       uint16   // version of line number encoding
	minInsnLen    uint8    // minimum instruction length
	maxOpsPerInsn uint8    // maximum number of ops per instruction
	defStmt       bool     // initial value of stmt register
	lineBase      int8     // line adjustment base
	lineRange     uint8    // line adjustment step
	opBase        uint8    // base of special opcode values
	opLen         []uint8  // lengths of standard opcodes
	dirs          []string // directories
	files         []string // file names
}

// parseLine parses the line number information for a compilation unit
func (d *Data) parseLine(u *unit) error {
	if u.lineoff+1 == 0 {
		return errors.New("unknown line offset")
	}
	b := makeBuf(d, "line", u.lineoff, d.line, u.addrsize)
	len := uint64(b.uint32())
	offSize := 4
	if len == 0xffffffff {
		len = b.uint64()
		offSize = 8
	}
	end := b.off + Offset(len)
	hdr := d.parseLineHdr(u, &b, offSize)
	if b.err == nil {
		d.parseLineProgram(u, &b, hdr, end)
	}
	return b.err
}

// parseLineHdr parses a line number program header.
func (d *Data) parseLineHdr(u *unit, b *buf, offSize int) (hdr lineHdr) {
	hdr.version = b.uint16()
	if hdr.version < 2 || hdr.version > 4 {
		b.error("unsupported DWARF version " + strconv.Itoa(int(hdr.version)))
		return
	}

	b.bytes(offSize) // header length

	hdr.minInsnLen = b.uint8()
	if hdr.version < 4 {
		hdr.maxOpsPerInsn = 1
	} else {
		hdr.maxOpsPerInsn = b.uint8()
	}

	if b.uint8() == 0 {
		hdr.defStmt = false
	} else {
		hdr.defStmt = true
	}
	hdr.lineBase = int8(b.uint8())
	hdr.lineRange = b.uint8()
	hdr.opBase = b.uint8()
	hdr.opLen = b.bytes(int(hdr.opBase - 1))

	for d := b.string(); len(d) > 0; d = b.string() {
		hdr.dirs = append(hdr.dirs, d)
	}

	for f := b.string(); len(f) > 0; f = b.string() {
		d := b.uint()
		if !filepath.IsAbs(f) {
			if d > 0 {
				if d > uint64(len(hdr.dirs)) {
					b.error("DWARF directory index out of range")
					return
				}
				f = filepath.Join(hdr.dirs[d-1], f)
			} else if u.dir != "" {
				f = filepath.Join(u.dir, f)
			}
		}
		b.uint() // file's last mtime
		b.uint() // file length
		hdr.files = append(hdr.files, f)
	}

	return
}

// parseLineProgram parses a line program, adding information to
// d.lineInfo as it goes.
func (d *Data) parseLineProgram(u *unit, b *buf, hdr lineHdr, end Offset) {
	address := uint64(0)
	line := 1
	resetLineInfo := Line{
		Filename:      "",
		OpIndex:       0,
		Line:          0,
		Column:        0,
		ISA:           0,
		Discriminator: 0,
		Stmt:          hdr.defStmt,
		Block:         false,
		EndPrologue:   false,
		BeginEpilogue: false,
	}
	if len(hdr.files) > 0 {
		resetLineInfo.Filename = hdr.files[0]
	}
	lineInfo := resetLineInfo

	var lines []mapLineInfo

	minInsnLen := uint64(hdr.minInsnLen)
	maxOpsPerInsn := uint64(hdr.maxOpsPerInsn)
	lineBase := int(hdr.lineBase)
	lineRange := hdr.lineRange
	newLineInfo := true
	for b.off < end && b.err == nil {
		op := b.uint8()
		if op >= hdr.opBase {
			// This is a special opcode.
			op -= hdr.opBase
			advance := uint64(op / hdr.lineRange)
			opIndex := uint64(lineInfo.OpIndex)
			address += minInsnLen * ((opIndex + advance) / maxOpsPerInsn)
			newOpIndex := int((opIndex + advance) % maxOpsPerInsn)
			line += lineBase + int(op%lineRange)
			if newOpIndex != lineInfo.OpIndex {
				lineInfo.OpIndex = newOpIndex
				newLineInfo = true
			}
			lines, lineInfo, newLineInfo = d.addLine(lines, lineInfo, address, line, newLineInfo)
		} else if op == LineExtendedOp {
			c := b.uint()
			op = b.uint8()
			switch op {
			case LineExtEndSequence:
				u.lines = append(u.lines, lines...)
				lineInfo = resetLineInfo
				lines = nil
			case LineExtSetAddress:
				address = b.addr()
			case LineExtDefineFile:
				f := b.string()
				d := b.uint()
				b.uint() // mtime
				b.uint() // length
				if d > 0 && !filepath.IsAbs(f) {
					if d >= uint64(len(hdr.dirs)) {
						b.error("DWARF directory index out of range")
						return
					}
					f = filepath.Join(hdr.dirs[d-1], f)
				}
				hdr.files = append(hdr.files, f)
			case LineExtSetDiscriminator:
				lineInfo.Discriminator = int(b.uint())
				newLineInfo = true
			default:
				if c > 0 {
					b.bytes(int(c) - 1)
				}
			}
		} else {
			switch op {
			case LineCopy:
				lines, lineInfo, newLineInfo = d.addLine(lines, lineInfo, address, line, newLineInfo)
			case LineAdvancePC:
				advance := b.uint()
				opIndex := uint64(lineInfo.OpIndex)
				address += minInsnLen * ((opIndex + advance) / maxOpsPerInsn)
				newOpIndex := int((opIndex + advance) % maxOpsPerInsn)
				if newOpIndex != lineInfo.OpIndex {
					lineInfo.OpIndex = newOpIndex
					newLineInfo = true
				}
			case LineAdvanceLine:
				line += int(b.int())
			case LineSetFile:
				i := b.uint()
				if i > uint64(len(hdr.files)) {
					b.error("DWARF file number out of range")
					return
				}
				lineInfo.Filename = hdr.files[i]
				newLineInfo = true
			case LineSetColumn:
				lineInfo.Column = int(b.uint())
				newLineInfo = true
			case LineNegateStmt:
				lineInfo.Stmt = !lineInfo.Stmt
				newLineInfo = true
			case LineSetBasicBlock:
				lineInfo.Block = true
				newLineInfo = true
			case LineConstAddPC:
				op = 255 - hdr.opBase
				advance := uint64(op / hdr.lineRange)
				opIndex := uint64(lineInfo.OpIndex)
				address += minInsnLen * ((opIndex + advance) / maxOpsPerInsn)
				newOpIndex := int((opIndex + advance) % maxOpsPerInsn)
				if newOpIndex != lineInfo.OpIndex {
					lineInfo.OpIndex = newOpIndex
					newLineInfo = true
				}
			case LineFixedAdvancePC:
				address += uint64(b.uint16())
				if lineInfo.OpIndex != 0 {
					lineInfo.OpIndex = 0
					newLineInfo = true
				}
			case LineSetPrologueEnd:
				lineInfo.EndPrologue = true
				newLineInfo = true
			case LineSetEpilogueBegin:
				lineInfo.BeginEpilogue = true
				newLineInfo = true
			case LineSetISA:
				lineInfo.ISA = int(b.uint())
				newLineInfo = true
			default:
				if int(op) >= len(hdr.opLen) {
					b.error("DWARF line opcode has unknown length")
					return
				}
				for i := hdr.opLen[op-1]; i > 0; i-- {
					b.int()
				}
			}
		}
	}
}

// addLine adds the current address and line to lines using lineInfo.
// If newLineInfo is true this is a new lineInfo.  This returns the
// updated lines, lineInfo, and newLineInfo.
func (d *Data) addLine(lines []mapLineInfo, lineInfo Line, address uint64, line int, newLineInfo bool) ([]mapLineInfo, Line, bool) {
	if newLineInfo {
		if len(lines) > 0 {
			sort.Sort(lines[len(lines)-1].addrs)
		}
		lines = append(lines, mapLineInfo{line: lineInfo})
	}
	p := &lines[len(lines)-1]
	p.addrs = append(p.addrs, oneLineInfo{address, line})

	if lineInfo.Block || lineInfo.EndPrologue || lineInfo.BeginEpilogue || lineInfo.Discriminator != 0 {
		lineInfo.Block = false
		lineInfo.EndPrologue = false
		lineInfo.BeginEpilogue = false
		lineInfo.Discriminator = 0
		newLineInfo = true
	} else {
		newLineInfo = false
	}

	return lines, lineInfo, newLineInfo
}
