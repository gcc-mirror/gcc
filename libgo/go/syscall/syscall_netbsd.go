// Copyright 2009,2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import "unsafe"

func direntIno(buf []byte) (uint64, bool) {
	return readInt(buf, unsafe.Offsetof(Dirent{}.Fileno), unsafe.Sizeof(Dirent{}.Fileno))
}

func direntReclen(buf []byte) (uint64, bool) {
	return readInt(buf, unsafe.Offsetof(Dirent{}.Reclen), unsafe.Sizeof(Dirent{}.Reclen))
}

func direntNamlen(buf []byte) (uint64, bool) {
	return readInt(buf, unsafe.Offsetof(Dirent{}.Namlen), unsafe.Sizeof(Dirent{}.Namlen))
}

//sys	Getdents(fd int, buf []byte) (n int, err error)
//getdents(fd _C_int, buf *byte, nbytes uintptr) _C_int

func Getdirentries(fd int, buf []byte, basep *uintptr) (n int, err error) {
	n, err = Getdents(fd, buf)
	if err != nil || basep == nil {
		return
	}

	var off int64
	off, err = Seek(fd, 0, 1 /* SEEK_CUR */)
	if err != nil {
		*basep = ^uintptr(0)
		return
	}
	*basep = uintptr(off)
	if unsafe.Sizeof(*basep) == 8 {
		return
	}
	if off>>32 != 0 {
		// We can't stuff the offset back into a uintptr, so any
		// future calls would be suspect. Generate an error.
		// EIO is allowed by getdirentries.
		err = EIO
	}
	return
}

func sysctlNodes(mib []_C_int) (nodes []Sysctlnode, err error) {
	var olen uintptr

	// Get a list of all sysctl nodes below the given MIB by performing
	// a sysctl for the given MIB with CTL_QUERY appended.
	mib = append(mib, CTL_QUERY)
	qnode := Sysctlnode{Flags: SYSCTL_VERS_1}
	qp := (*byte)(unsafe.Pointer(&qnode))
	sz := unsafe.Sizeof(qnode)
	if err = sysctl(mib, nil, &olen, qp, sz); err != nil {
		return nil, err
	}

	// Now that we know the size, get the actual nodes.
	nodes = make([]Sysctlnode, olen/sz)
	np := (*byte)(unsafe.Pointer(&nodes[0]))
	if err = sysctl(mib, np, &olen, qp, sz); err != nil {
		return nil, err
	}

	return nodes, nil
}

func nametomib(name string) (mib []_C_int, err error) {
	// Split name into components.
	var parts []string
	last := 0
	for i := 0; i < len(name); i++ {
		if name[i] == '.' {
			parts = append(parts, name[last:i])
			last = i + 1
		}
	}
	parts = append(parts, name[last:])

	// Discover the nodes and construct the MIB OID.
	for partno, part := range parts {
		nodes, err := sysctlNodes(mib)
		if err != nil {
			return nil, err
		}
		for _, node := range nodes {
			n := make([]byte, 0)
			for i := range node.Name {
				if node.Name[i] != 0 {
					n = append(n, byte(node.Name[i]))
				}
			}
			if string(n) == part {
				mib = append(mib, _C_int(node.Num))
				break
			}
		}
		if len(mib) != partno+1 {
			return nil, EINVAL
		}
	}

	return mib, nil
}
