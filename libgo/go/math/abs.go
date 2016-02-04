// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package math

// Abs returns the absolute value of x.
//
// Special cases are:
//	Abs(±Inf) = +Inf
//	Abs(NaN) = NaN

//extern fabs
func libc_fabs(float64) float64

func Abs(x float64) float64 {
	return libc_fabs(x)
}

func abs(x float64) float64 {
	// TODO: once golang.org/issue/13095 is fixed, change this to:
	// return Float64frombits(Float64bits(x) &^ (1 << 63))
	// But for now, this generates better code and can also be inlined:
	if x < 0 {
		return -x
	}
	if x == 0 {
		return 0 // return correctly abs(-0)
	}
	return x
}
