// Copyright 2012 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This is gccgo-specific code that uses DWARF information to fetch
// file/line information for PC values.  This package registers itself
// with the runtime package.

package elf

import (
	"debug/dwarf"
	"debug/macho"
	"os"
	"runtime"
	"sort"
	"sync"
)

func init() {
	// Register our lookup functions with the runtime package.
	runtime.RegisterDebugLookup(funcFileLine, symbolValue)
}

// The file struct holds information for a specific file that is part
// of the execution.
type file struct {
	elf   *File       // If ELF
	macho *macho.File // If Mach-O
	dwarf *dwarf.Data // DWARF information

	symsByName []sym // Sorted by name
	symsByAddr []sym // Sorted by address
}

// Sort symbols by name.
type symsByName []sym

func (s symsByName) Len() int           { return len(s) }
func (s symsByName) Less(i, j int) bool { return s[i].name < s[j].name }
func (s symsByName) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }

// Sort symbols by address.
type symsByAddr []sym

func (s symsByAddr) Len() int           { return len(s) }
func (s symsByAddr) Less(i, j int) bool { return s[i].addr < s[j].addr }
func (s symsByAddr) Swap(i, j int)      { s[i], s[j] = s[j], s[i] }

// The sym structure holds the information we care about for a symbol,
// namely name and address.
type sym struct {
	name string
	addr uintptr
}

// Open an input file.
func open(name string) (*file, error) {
	efile, err := Open(name)
	var mfile *macho.File
	if err != nil {
		var merr error
		mfile, merr = macho.Open(name)
		if merr != nil {
			return nil, err
		}
	}

	r := &file{elf: efile, macho: mfile}

	if efile != nil {
		r.dwarf, err = efile.DWARF()
	} else {
		r.dwarf, err = mfile.DWARF()
	}
	if err != nil {
		return nil, err
	}

	var syms []sym
	if efile != nil {
		esyms, err := efile.Symbols()
		if err != nil {
			return nil, err
		}
		syms = make([]sym, 0, len(esyms))
		for _, s := range esyms {
			if ST_TYPE(s.Info) == STT_FUNC {
				syms = append(syms, sym{s.Name, uintptr(s.Value)})
			}
		}
	} else {
		syms = make([]sym, 0, len(mfile.Symtab.Syms))
		for _, s := range mfile.Symtab.Syms {
			syms = append(syms, sym{s.Name, uintptr(s.Value)})
		}
	}

	r.symsByName = make([]sym, len(syms))
	copy(r.symsByName, syms)
	sort.Sort(symsByName(r.symsByName))

	r.symsByAddr = syms
	sort.Sort(symsByAddr(r.symsByAddr))

	return r, nil
}

// The main executable
var executable *file

// Only open the executable once.
var executableOnce sync.Once

func openExecutable() {
	executableOnce.Do(func() {
		f, err := open("/proc/self/exe")
		if err != nil {
			f, err = open(os.Args[0])
			if err != nil {
				return
			}
		}
		executable = f
	})
}

// The funcFileLine function looks up the function name, file name,
// and line number for a PC value.
func funcFileLine(pc uintptr, function *string, file *string, line *int) bool {
	openExecutable()
	if executable.dwarf == nil {
		return false
	}
	f, ln, err := executable.dwarf.FileLine(uint64(pc))
	if err != nil {
		return false
	}
	*file = f
	*line = ln

	*function = ""
	if len(executable.symsByAddr) > 0 && pc >= executable.symsByAddr[0].addr {
		i := sort.Search(len(executable.symsByAddr),
			func(i int) bool { return executable.symsByAddr[i].addr > pc })
		*function = executable.symsByAddr[i-1].name
	}

	return true
}

// The symbolValue function fetches the value of a symbol.
func symbolValue(name string, val *uintptr) bool {
	i := sort.Search(len(executable.symsByName),
		func(i int) bool { return executable.symsByName[i].name >= name })
	if i >= len(executable.symsByName) || executable.symsByName[i].name != name {
		return false
	}
	*val = executable.symsByName[i].addr
	return true
}
