// syscall_linux.go -- GNU/Linux specific syscall interface.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package syscall

import "unsafe"

func libc_ptrace(request int, pid Pid_t, addr uintptr, data *byte) _C_long __asm__ ("ptrace")

var dummy *byte
const sizeofPtr uintptr = uintptr(unsafe.Sizeof(dummy))

func ptracePeek(req int, pid int, addr uintptr, out []byte) (count int, errno int) {
	// The peek requests are machine-size oriented, so we wrap it
	// to retrieve arbitrary-length data.

	var buf [sizeofPtr]byte;

	// Leading edge.  PEEKTEXT/PEEKDATA don't require aligned
	// access (PEEKUSER warns that it might), but if we don't
	// align our reads, we might straddle an unmapped page
	// boundary and not get the bytes leading up to the page
	// boundary.
	n := 0;
	if addr % sizeofPtr != 0 {
		SetErrno(0);
		val := libc_ptrace(req, Pid_t(pid), addr - addr%sizeofPtr, nil);
		if errno := GetErrno(); errno != 0 {
			return 0, errno;
		}
		*(*_C_long)(unsafe.Pointer(&buf[0])) = val;
		n += copy(out, buf[addr%sizeofPtr:]);
		out = out[n:];
	}

	// Remainder.
	for len(out) > 0 {
		// We use an internal buffer to gaurantee alignment.
		// It's not documented if this is necessary, but we're paranoid.
		SetErrno(0);
		val := libc_ptrace(req, Pid_t(pid), addr+uintptr(n), nil);
		if errno = GetErrno(); errno != 0 {
			return n, errno;
		}
		*(*_C_long)(unsafe.Pointer(&buf[0])) = val;
		copied := copy(out, buf[0:]);
		n += copied;
		out = out[copied:];
	}

	return n, 0;
}

func PtracePeekText(pid int, addr uintptr, out []byte) (count int, errno int) {
	return ptracePeek(_PTRACE_PEEKTEXT, pid, addr, out);
}

func PtracePeekData(pid int, addr uintptr, out []byte) (count int, errno int) {
	return ptracePeek(_PTRACE_PEEKDATA, pid, addr, out);
}

func ptracePoke(pokeReq int, peekReq int, pid int, addr uintptr, data []byte) (count int, errno int) {
	// As for ptracePeek, we need to align our accesses to deal
	// with the possibility of straddling an invalid page.

	// Leading edge.
	n := 0;
	if addr % sizeofPtr != 0 {
		var buf [sizeofPtr]byte;
		if libc_ptrace(peekReq, Pid_t(pid), addr - addr%sizeofPtr, &buf[0]) < 0 {
			return 0, GetErrno();
		}
		n += copy(buf[addr%sizeofPtr:], data);
		word := (*byte)(unsafe.Pointer(*((*uintptr)(unsafe.Pointer(&buf[0])))));
		if libc_ptrace(pokeReq, Pid_t(pid), addr - addr%sizeofPtr, word) < 0 {
			return 0, GetErrno();
		}
		data = data[n:len(data)];
	}

	// Interior.
	for uintptr(len(data)) > sizeofPtr {
		word := (*byte)(unsafe.Pointer(*((*uintptr)(unsafe.Pointer(&data[0])))));
		if libc_ptrace(pokeReq, Pid_t(pid), addr+uintptr(n), word) < 0 {
			return n, GetErrno();
		}
		n += int(sizeofPtr);
		data = data[sizeofPtr:len(data)];
	}

	// Trailing edge.
	if len(data) > 0 {
		var buf [sizeofPtr]byte;
		if libc_ptrace(peekReq, Pid_t(pid), addr+uintptr(n), &buf[0]) < 0 {
			return n, GetErrno();
		}
		copy(buf[0:], data);
		word := (*byte)(unsafe.Pointer(*((*uintptr)(unsafe.Pointer(&buf[0])))));
		if libc_ptrace(pokeReq, Pid_t(pid), addr+uintptr(n), word) < 0 {
			return n, GetErrno();
		}
		n += len(data);
	}

	return n, 0;
}

func PtracePokeText(pid int, addr uintptr, data []byte) (count int, errno int) {
	return ptracePoke(_PTRACE_POKETEXT, _PTRACE_PEEKTEXT, pid, addr, data);
}

func PtracePokeData(pid int, addr uintptr, data []byte) (count int, errno int) {
	return ptracePoke(_PTRACE_POKEDATA, _PTRACE_PEEKDATA, pid, addr, data);
}

func PtraceGetRegs(pid int, regsout *PtraceRegs) (errno int) {
	if libc_ptrace(_PTRACE_GETREGS, Pid_t(pid), 0, (*byte)(unsafe.Pointer(regsout))) < 0 {
		return GetErrno();
	} else {
		return 0;
	}
}

func PtraceSetRegs(pid int, regs *PtraceRegs) (errno int) {
	if libc_ptrace(_PTRACE_SETREGS, Pid_t(pid), 0, (*byte)(unsafe.Pointer(regs))) < 0 {
		return GetErrno();
	} else {
		return 0;
	}
}

func PtraceSetOptions(pid int, options int) (errno int) {
	if libc_ptrace(_PTRACE_SETOPTIONS, Pid_t(pid), 0, (*byte)(unsafe.Pointer(uintptr(options)))) < 0 {
		return GetErrno();
	} else {
		return 0;
	}
}

func PtraceGetEventMsg(pid int) (msg uint, errno int) {
	var data _C_long;
	if libc_ptrace(_PTRACE_GETEVENTMSG, Pid_t(pid), 0, (*byte)(unsafe.Pointer(&data))) < 0 {
		errno = GetErrno();
	}
	msg = uint(data);
	return;
}

func PtraceCont(pid int, signal int) (errno int) {
	if libc_ptrace(_PTRACE_CONT, Pid_t(pid), 0, (*byte)(unsafe.Pointer(uintptr(signal)))) < 0 {
		return GetErrno();
	} else {
		return 0;
	}
}

func PtraceSingleStep(pid int) (errno int) {
	if libc_ptrace(_PTRACE_SINGLESTEP, Pid_t(pid), 0, nil) < 0 {
		return GetErrno();
	} else {
		return 0;
	}
}

func PtraceAttach(pid int) (errno int) {
	if libc_ptrace(_PTRACE_ATTACH, Pid_t(pid), 0, nil) < 0 {
		return GetErrno();
	} else {
		return 0;
	}
}

func PtraceDetach(pid int) (errno int) {
	if libc_ptrace(_PTRACE_DETACH, Pid_t(pid), 0, nil) < 0 {
		return GetErrno();
	} else {
		return 0;
	}
}

func Tgkill(tgid int, tid int, sig int) (errno int) {
	r1, r2, err := Syscall(SYS_TGKILL, uintptr(tgid), uintptr(tid),
				 uintptr(sig));
	return int(err);
}
