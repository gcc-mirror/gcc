// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import (
	"sync"
	"unsafe"
)

// See version list in https://github.com/DragonFlyBSD/DragonFlyBSD/blob/master/sys/sys/param.h
var (
	osreldateOnce sync.Once
	osreldate     uint32
)

// First __DragonFly_version after September 2019 ABI changes
// http://lists.dragonflybsd.org/pipermail/users/2019-September/358280.html
const _dragonflyABIChangeVersion = 500705

func supportsABI(ver uint32) bool {
	osreldateOnce.Do(func() { osreldate, _ = SysctlUint32("kern.osreldate") })
	return osreldate >= ver
}

func direntIno(buf []byte) (uint64, bool) {
	return readInt(buf, unsafe.Offsetof(Dirent{}.Fileno), unsafe.Sizeof(Dirent{}.Fileno))
}

func direntReclen(buf []byte) (uint64, bool) {
	namlen, ok := direntNamlen(buf)
	if !ok {
		return 0, false
	}
	return (16 + namlen + 1 + 7) &^ 7, true
}

func direntNamlen(buf []byte) (uint64, bool) {
	return readInt(buf, unsafe.Offsetof(Dirent{}.Namlen), unsafe.Sizeof(Dirent{}.Namlen))
}
