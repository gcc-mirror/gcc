// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime

import "unsafe"

type mOS struct {
	machport uint32 // return address for mach ipc
	waitsema uint32 // semaphore for parking on locks
}

//go:noescape
//extern mach_msg_trap
func mach_msg_trap(h unsafe.Pointer, op int32, send_size, rcv_size, rcv_name, timeout, notify uint32) int32

//extern mach_reply_port
func mach_reply_port() uint32

//extern mach_task_self
func mach_task_self() uint32

func unimplemented(name string) {
	println(name, "not implemented")
	*(*int)(unsafe.Pointer(uintptr(1231))) = 1231
}

//go:nosplit
func semawakeup(mp *m) {
	mach_semrelease(mp.mos.waitsema)
}

//go:nosplit
func semacreate(mp *m) {
	if mp.mos.waitsema != 0 {
		return
	}
	systemstack(func() {
		mp.mos.waitsema = mach_semcreate()
	})
}

// Mach IPC, to get at semaphores
// Definitions are in /usr/include/mach on a Mac.

func macherror(r int32, fn string) {
	print("mach error ", fn, ": ", r, "\n")
	throw("mach error")
}

const _DebugMach = false

var zerondr machndr

func mach_msgh_bits(a, b uint32) uint32 {
	return a | b<<8
}

func mach_msg(h *machheader, op int32, send_size, rcv_size, rcv_name, timeout, notify uint32) int32 {
	// TODO: Loop on interrupt.
	return mach_msg_trap(unsafe.Pointer(h), op, send_size, rcv_size, rcv_name, timeout, notify)
}

// Mach RPC (MIG)
const (
	_MinMachMsg = 48
	_MachReply  = 100
)

type codemsg struct {
	h    machheader
	ndr  machndr
	code int32
}

func machcall(h *machheader, maxsize int32, rxsize int32) int32 {
	_g_ := getg()
	port := _g_.m.mos.machport
	if port == 0 {
		port = mach_reply_port()
		_g_.m.mos.machport = port
	}

	h.msgh_bits |= mach_msgh_bits(_MACH_MSG_TYPE_COPY_SEND, _MACH_MSG_TYPE_MAKE_SEND_ONCE)
	h.msgh_local_port = port
	h.msgh_reserved = 0
	id := h.msgh_id

	if _DebugMach {
		p := (*[10000]unsafe.Pointer)(unsafe.Pointer(h))
		print("send:\t")
		var i uint32
		for i = 0; i < h.msgh_size/uint32(unsafe.Sizeof(p[0])); i++ {
			print(" ", p[i])
			if i%8 == 7 {
				print("\n\t")
			}
		}
		if i%8 != 0 {
			print("\n")
		}
	}
	ret := mach_msg(h, _MACH_SEND_MSG|_MACH_RCV_MSG, h.msgh_size, uint32(maxsize), port, 0, 0)
	if ret != 0 {
		if _DebugMach {
			print("mach_msg error ", ret, "\n")
		}
		return ret
	}
	if _DebugMach {
		p := (*[10000]unsafe.Pointer)(unsafe.Pointer(h))
		var i uint32
		for i = 0; i < h.msgh_size/uint32(unsafe.Sizeof(p[0])); i++ {
			print(" ", p[i])
			if i%8 == 7 {
				print("\n\t")
			}
		}
		if i%8 != 0 {
			print("\n")
		}
	}
	if h.msgh_id != id+_MachReply {
		if _DebugMach {
			print("mach_msg _MachReply id mismatch ", h.msgh_id, " != ", id+_MachReply, "\n")
		}
		return -303 // MIG_REPLY_MISMATCH
	}
	// Look for a response giving the return value.
	// Any call can send this back with an error,
	// and some calls only have return values so they
	// send it back on success too. I don't quite see how
	// you know it's one of these and not the full response
	// format, so just look if the message is right.
	c := (*codemsg)(unsafe.Pointer(h))
	if uintptr(h.msgh_size) == unsafe.Sizeof(*c) && h.msgh_bits&_MACH_MSGH_BITS_COMPLEX == 0 {
		if _DebugMach {
			print("mig result ", c.code, "\n")
		}
		return c.code
	}
	if h.msgh_size != uint32(rxsize) {
		if _DebugMach {
			print("mach_msg _MachReply size mismatch ", h.msgh_size, " != ", rxsize, "\n")
		}
		return -307 // MIG_ARRAY_TOO_LARGE
	}
	return 0
}

// Semaphores!

const (
	tmach_semcreate = 3418
	rmach_semcreate = tmach_semcreate + _MachReply

	tmach_semdestroy = 3419
	rmach_semdestroy = tmach_semdestroy + _MachReply

	_KERN_ABORTED             = 14
	_KERN_OPERATION_TIMED_OUT = 49
)

type tmach_semcreatemsg struct {
	h      machheader
	ndr    machndr
	policy int32
	value  int32
}

type rmach_semcreatemsg struct {
	h         machheader
	body      machbody
	semaphore machport
}

type tmach_semdestroymsg struct {
	h         machheader
	body      machbody
	semaphore machport
}

func mach_semcreate() uint32 {
	var m [256]uint8
	tx := (*tmach_semcreatemsg)(unsafe.Pointer(&m))
	rx := (*rmach_semcreatemsg)(unsafe.Pointer(&m))

	tx.h.msgh_bits = 0
	tx.h.msgh_size = uint32(unsafe.Sizeof(*tx))
	tx.h.msgh_remote_port = mach_task_self()
	tx.h.msgh_id = tmach_semcreate
	tx.ndr = zerondr

	tx.policy = 0 // 0 = SYNC_POLICY_FIFO
	tx.value = 0

	for {
		r := machcall(&tx.h, int32(unsafe.Sizeof(m)), int32(unsafe.Sizeof(*rx)))
		if r == 0 {
			break
		}
		if r == _KERN_ABORTED { // interrupted
			continue
		}
		macherror(r, "semaphore_create")
	}
	if rx.body.msgh_descriptor_count != 1 {
		unimplemented("mach_semcreate desc count")
	}
	return rx.semaphore.name
}

func mach_semdestroy(sem uint32) {
	var m [256]uint8
	tx := (*tmach_semdestroymsg)(unsafe.Pointer(&m))

	tx.h.msgh_bits = _MACH_MSGH_BITS_COMPLEX
	tx.h.msgh_size = uint32(unsafe.Sizeof(*tx))
	tx.h.msgh_remote_port = mach_task_self()
	tx.h.msgh_id = tmach_semdestroy
	tx.body.msgh_descriptor_count = 1
	tx.semaphore.name = sem
	tx.semaphore.disposition = _MACH_MSG_TYPE_MOVE_SEND
	tx.semaphore._type = 0

	for {
		r := machcall(&tx.h, int32(unsafe.Sizeof(m)), 0)
		if r == 0 {
			break
		}
		if r == _KERN_ABORTED { // interrupted
			continue
		}
		macherror(r, "semaphore_destroy")
	}
}

//extern semaphore_wait
func mach_semaphore_wait(sema uint32) int32

//extern semaphore_timedwait
func mach_semaphore_timedwait(sema, sec, nsec uint32) int32

//extern semaphore_signal
func mach_semaphore_signal(sema uint32) int32

//extern semaphore_signal_all
func mach_semaphore_signal_all(sema uint32) int32

func semasleep1(ns int64) int32 {
	_g_ := getg()

	if ns >= 0 {
		var nsecs int32
		secs := timediv(ns, 1000000000, &nsecs)
		r := mach_semaphore_timedwait(_g_.m.mos.waitsema, uint32(secs), uint32(nsecs))
		if r == _KERN_ABORTED || r == _KERN_OPERATION_TIMED_OUT {
			return -1
		}
		if r != 0 {
			macherror(r, "semaphore_wait")
		}
		return 0
	}

	for {
		r := mach_semaphore_wait(_g_.m.mos.waitsema)
		if r == 0 {
			break
		}
		if r == _KERN_ABORTED { // interrupted
			continue
		}
		macherror(r, "semaphore_wait")
	}
	return 0
}

//go:nosplit
func semasleep(ns int64) int32 {
	var r int32
	systemstack(func() {
		r = semasleep1(ns)
	})
	return r
}

//go:nosplit
func mach_semrelease(sem uint32) {
	for {
		r := mach_semaphore_signal(sem)
		if r == 0 {
			break
		}
		if r == _KERN_ABORTED { // interrupted
			continue
		}

		// mach_semrelease must be completely nosplit,
		// because it is called from Go code.
		// If we're going to die, start that process on the system stack
		// to avoid a Go stack split.
		systemstack(func() { macherror(r, "semaphore_signal") })
	}
}

type machheader struct {
	msgh_bits        uint32
	msgh_size        uint32
	msgh_remote_port uint32
	msgh_local_port  uint32
	msgh_reserved    uint32
	msgh_id          int32
}

type machndr struct {
	mig_vers     uint8
	if_vers      uint8
	reserved1    uint8
	mig_encoding uint8
	int_rep      uint8
	char_rep     uint8
	float_rep    uint8
	reserved2    uint8
}
