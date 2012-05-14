// Copyright 2012 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package regexp

import "regexp/syntax"

func (re *Regexp) SetLongest(b bool) {
	re.longest = b
}

func CompileInternal(expr string, mode syntax.Flags, longest bool) (*Regexp, error) {
	return compile(expr, mode, longest)
}
