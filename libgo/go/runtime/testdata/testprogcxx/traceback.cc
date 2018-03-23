// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

char *p;

static int cxxFunction3() {
	*p = 0;
	return 0;
}

static int cxxFunction2() {
	return cxxFunction3();
}

extern "C"
int cxxFunction1() {
	return cxxFunction2();
}
