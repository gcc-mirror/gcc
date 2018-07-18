// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main

// This program will crash.
// We want the stack trace to include the C++ functions,
// even though we compile with -g0.

/*
#cgo CXXFLAGS: -g0 -O0

extern int cxxFunction1(void);
*/
import "C"

func init() {
	register("CrashTracebackNodebug", CrashTracebackNodebug)
}

func CrashTracebackNodebug() {
	C.cxxFunction1()
}
