// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//go:build gccgo
// +build gccgo

package main

// This program will crash.
// We want the stack trace to include the C functions.

/*
#cgo CFLAGS: -g -O0

#include <stdint.h>

char *p;

static int CFunction3(void) {
	*p = 0;
	return 0;
}

static int CFunction2(void) {
	return CFunction3();
}

static int CFunction1(void) {
	return CFunction2();
}
*/
import "C"

func init() {
	register("CrashTracebackGccgo", CrashTracebackGccgo)
}

func CrashTracebackGccgo() {
	C.CFunction1()
}
