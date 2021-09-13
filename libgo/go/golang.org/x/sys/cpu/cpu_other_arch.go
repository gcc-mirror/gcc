// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build !aix
// +build !linux
// +build !zos

// +build !386
// +build !amd64
// +build !amd64p32
// +build !arm
// +build !arm64
// +build !mips64
// +build !mips64le
// +build !wasm

package cpu

func archInit() {}
