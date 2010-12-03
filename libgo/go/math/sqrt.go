// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package math

func libc_sqrt(float64) float64 __asm__("sqrt")

// Sqrt returns the square root of x.
//
// Special cases are:
//	Sqrt(+Inf) = +Inf
//	Sqrt(±0) = ±0
//	Sqrt(x < 0) = NaN
//	Sqrt(NaN) = NaN
func Sqrt(x float64) float64 {
	// special cases
	// TODO(rsc): Remove manual inlining of IsNaN, IsInf
	// when compiler does it for us
	switch {
	case x == 0 || x != x || x > MaxFloat64: // x == 0 || IsNaN(x) || IsInf(x, 1):
		return x
	case x < 0:
		return NaN()
	}

	return libc_sqrt(x)
}
