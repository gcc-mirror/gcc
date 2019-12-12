// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build !compiler_bootstrap

package bits

import _ "unsafe"

//go:linkname getOverflowError runtime.getOverflowError
func getOverflowError() error

//go:linkname getDivideError runtime.getDivideError
func getDivideError() error
