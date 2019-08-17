// Copyright 2019 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package runtime_test

import "testing"

func TestDeferHeapAndStack(t *testing.T) {
	P := 4     // processors
	N := 10000 // iterations
	D := 200   // stack depth

	if testing.Short() {
		P /= 2
		N /= 10
		D /= 10
	}
	c := make(chan bool)
	for p := 0; p < P; p++ {
		go func() {
			for i := 0; i < N; i++ {
				if deferHeapAndStack(D) != 2*D {
					panic("bad result")
				}
			}
			c <- true
		}()
	}
	for p := 0; p < P; p++ {
		<-c
	}
}

// deferHeapAndStack(n) computes 2*n
func deferHeapAndStack(n int) (r int) {
	if n == 0 {
		return 0
	}
	if n%2 == 0 {
		// heap-allocated defers
		for i := 0; i < 2; i++ {
			defer func() {
				r++
			}()
		}
	} else {
		// stack-allocated defers
		defer func() {
			r++
		}()
		defer func() {
			r++
		}()
	}
	r = deferHeapAndStack(n - 1)
	escapeMe(new([1024]byte)) // force some GCs
	return
}

// Pass a value to escapeMe to force it to escape.
var escapeMe = func(x interface{}) {}
