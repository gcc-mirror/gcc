// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// +build cgo,gccgo

package runtime_test

import (
	"bytes"
	"fmt"
	"internal/testenv"
	"os"
	"os/exec"
	"strings"
	"testing"
)

func TestGccgoCrashTraceback(t *testing.T) {
	t.Parallel()
	got := runTestProg(t, "testprogcgo", "CrashTracebackGccgo")
	ok := true
	for i := 1; i <= 3; i++ {
		if !strings.Contains(got, fmt.Sprintf("CFunction%d", i)) {
			t.Errorf("missing C function CFunction%d", i)
			ok = false
		}
	}
	if !ok {
		t.Log(got)
	}
}

func TestGccgoCrashTracebackNodebug(t *testing.T) {
	testenv.MustHaveGoBuild(t)
	if os.Getenv("CC") == "" {
		t.Skip("no compiler in environment")
	}

	cc := strings.Fields(os.Getenv("CC"))
	cc = append(cc, "-o", os.DevNull, "-x", "c++", "-")
	out, _ := testenv.CleanCmdEnv(exec.Command(cc[0], cc[1:]...)).CombinedOutput()
	if bytes.Contains(out, []byte("error trying to exec 'cc1plus'")) {
		t.Skip("no C++ compiler")
	}
	os.Setenv("CXX", os.Getenv("CC"))

	got := runTestProg(t, "testprogcxx", "CrashTracebackNodebug")
	ok := true
	for i := 1; i <= 3; i++ {
		if !strings.Contains(got, fmt.Sprintf("cxxFunction%d", i)) {
			t.Errorf("missing C++ function cxxFunction%d", i)
			ok = false
		}
	}
	if !ok {
		t.Log(got)
	}
}
