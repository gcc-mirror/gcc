// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build gccgo

package bytealg

// MaxLen is the maximum length of the string to be searched for (argument b) in Index.
var MaxLen int = 32

const MaxBruteForce = 64
