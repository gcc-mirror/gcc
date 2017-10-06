// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package xcoff

import (
	"reflect"
	"testing"
)

type fileTest struct {
	file     string
	hdr      FileHeader
	sections []*SectionHeader
	needed   []string
}

var fileTests = []fileTest{
	{
		"testdata/gcc-ppc32-aix-exec",
		FileHeader{U802TOCMAGIC},
		[]*SectionHeader{
			{".text", 0x10000150, 0x00000bbd, STYP_TEXT},
			{".data", 0x20000d0d, 0x0000042b, STYP_DATA},
			{".bss", 0x20001138, 0x00000218, STYP_BSS},
			{".loader", 0x00000000, 0x000004b3, STYP_LOADER},
			{".debug", 0x00000000, 0x0000751e, STYP_DEBUG},
		},
		[]string{"libc.a"},
	},
	{
		"testdata/gcc-ppc64-aix-exec",
		FileHeader{U64_TOCMAGIC},
		[]*SectionHeader{
			{".text", 0x10000240, 0x00000afd, STYP_TEXT},
			{".data", 0x20000d3d, 0x000002e3, STYP_DATA},
			{".bss", 0x20001020, 0x00000428, STYP_BSS},
			{".loader", 0x00000000, 0x00000535, STYP_LOADER},
			{".debug", 0x00000000, 0x00008238, STYP_DEBUG},
		},
		[]string{"libc.a"},
	},
	{
		"testdata/xlc-ppc32-aix-exec",
		FileHeader{U802TOCMAGIC},
		[]*SectionHeader{
			{".text", 0x10000150, 0x00000372, STYP_TEXT},
			{".data", 0x200004c2, 0x0000032e, STYP_DATA},
			{".bss", 0x200007f0, 0x00000004, STYP_BSS},
			{".loader", 0x00000000, 0x0000029d, STYP_LOADER},
			{".debug", 0x00000000, 0x0000008f, STYP_DEBUG},
		},
		[]string{"libc.a"},
	},
	{
		"testdata/xlc-ppc64-aix-exec",
		FileHeader{U64_TOCMAGIC},
		[]*SectionHeader{
			{".text", 0x100000240, 0x00000326, STYP_TEXT},
			{".data", 0x110000566, 0x00000182, STYP_DATA},
			{".bss", 0x1100006e8, 0x00000008, STYP_BSS},
			{".loader", 0x00000000, 0x0000029b, STYP_LOADER},
			{".debug", 0x00000000, 0x000000ea, STYP_DEBUG},
		},
		[]string{"libc.a"},
	},
	{
		"testdata/gcc-ppc32-aix-dwarf2-exec",
		FileHeader{U802TOCMAGIC},
		[]*SectionHeader{
			{".text", 0x10000290, 0x00000bbd, STYP_TEXT},
			{".data", 0x20000e4d, 0x00000437, STYP_DATA},
			{".bss", 0x20001284, 0x0000021c, STYP_BSS},
			{".loader", 0x00000000, 0x000004b3, STYP_LOADER},
			{".dwline", 0x00000000, 0x000000df, STYP_DWARF | SSUBTYP_DWLINE},
			{".dwinfo", 0x00000000, 0x00000314, STYP_DWARF | SSUBTYP_DWINFO},
			{".dwabrev", 0x00000000, 0x000000d6, STYP_DWARF | SSUBTYP_DWABREV},
			{".dwarnge", 0x00000000, 0x00000020, STYP_DWARF | SSUBTYP_DWARNGE},
			{".dwloc", 0x00000000, 0x00000074, STYP_DWARF | SSUBTYP_DWLOC},
			{".debug", 0x00000000, 0x00005e4f, STYP_DEBUG},
		},
		[]string{"libc.a"},
	},
	{
		"testdata/gcc-ppc64-aix-dwarf2-exec",
		FileHeader{U64_TOCMAGIC},
		[]*SectionHeader{
			{".text", 0x10000480, 0x00000afd, STYP_TEXT},
			{".data", 0x20000f7d, 0x000002f3, STYP_DATA},
			{".bss", 0x20001270, 0x00000428, STYP_BSS},
			{".loader", 0x00000000, 0x00000535, STYP_LOADER},
			{".dwline", 0x00000000, 0x000000b4, STYP_DWARF | SSUBTYP_DWLINE},
			{".dwinfo", 0x00000000, 0x0000036a, STYP_DWARF | SSUBTYP_DWINFO},
			{".dwabrev", 0x00000000, 0x000000b5, STYP_DWARF | SSUBTYP_DWABREV},
			{".dwarnge", 0x00000000, 0x00000040, STYP_DWARF | SSUBTYP_DWARNGE},
			{".dwloc", 0x00000000, 0x00000062, STYP_DWARF | SSUBTYP_DWLOC},
			{".debug", 0x00000000, 0x00006605, STYP_DEBUG},
		},
		[]string{"libc.a"},
	},
}

func TestOpen(t *testing.T) {
	for i := range fileTests {
		tt := &fileTests[i]

		f, err := Open(tt.file)
		if err != nil {
			t.Error(err)
			continue
		}
		if !reflect.DeepEqual(f.FileHeader, tt.hdr) {
			t.Errorf("open %s:\n\thave %#v\n\twant %#v\n", tt.file, f.FileHeader, tt.hdr)
			continue
		}

		for i, sh := range f.Sections {
			if i >= len(tt.sections) {
				break
			}
			have := &sh.SectionHeader
			want := tt.sections[i]
			if !reflect.DeepEqual(have, want) {
				t.Errorf("open %s, section %d:\n\thave %#v\n\twant %#v\n", tt.file, i, have, want)
			}
		}
		tn := len(tt.sections)
		fn := len(f.Sections)
		if tn != fn {
			t.Errorf("open %s: len(Sections) = %d, want %d", tt.file, fn, tn)
		}
		tl := tt.needed
		fl, err := f.ImportedLibraries()
		if err != nil {
			t.Error(err)
		}
		if !reflect.DeepEqual(tl, fl) {
			t.Errorf("open %s: loader import = %v, want %v", tt.file, tl, fl)
		}
	}
}

func TestOpenFailure(t *testing.T) {
	filename := "file.go"    // not an XCOFF object file
	_, err := Open(filename) // don't crash
	if err == nil {
		t.Errorf("open %s: succeeded unexpectedly", filename)
	}
}
