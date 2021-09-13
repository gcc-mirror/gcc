// Copyright 2016 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
//go:build 386 || alpha || amd64 || amd64p32 || arm || arm64 || ia64 || ppc64le || mips64le || mipsle || mips64p32le || nios2 || riscv || riscv64 || sh || wasm
// +build 386 alpha amd64 amd64p32 arm arm64 ia64 ppc64le mips64le mipsle mips64p32le nios2 riscv riscv64 sh wasm

package syscall

const isBigEndian = false
