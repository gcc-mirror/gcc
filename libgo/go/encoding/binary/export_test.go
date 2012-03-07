// Copyright 2012 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package binary

import "reflect"

// Export for testing.

func DataSize(v reflect.Value) int {
	return dataSize(v)
}

var Overflow = overflow
