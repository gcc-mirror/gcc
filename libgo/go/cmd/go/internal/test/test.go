// Copyright 2011 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package test

import (
	"bytes"
	"crypto/sha256"
	"errors"
	"fmt"
	"go/ast"
	"go/build"
	"go/doc"
	"go/parser"
	"go/token"
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"sync"
	"text/template"
	"time"
	"unicode"
	"unicode/utf8"

	"cmd/go/internal/base"
	"cmd/go/internal/cache"
	"cmd/go/internal/cfg"
	"cmd/go/internal/load"
	"cmd/go/internal/str"
	"cmd/go/internal/work"
	"cmd/internal/test2json"
)

// Break init loop.
func init() {
	CmdTest.Run = runTest
}

const testUsage = "test [build/test flags] [packages] [build/test flags & test binary flags]"

var CmdTest = &base.Command{
	CustomFlags: true,
	UsageLine:   testUsage,
	Short:       "test packages",
	Long: `
'Go test' automates testing the packages named by the import paths.
It prints a summary of the test results in the format:

	ok   archive/tar   0.011s
	FAIL archive/zip   0.022s
	ok   compress/gzip 0.033s
	...

followed by detailed output for each failed package.

'Go test' recompiles each package along with any files with names matching
the file pattern "*_test.go".
These additional files can contain test functions, benchmark functions, and
example functions. See 'go help testfunc' for more.
Each listed package causes the execution of a separate test binary.
Files whose names begin with "_" (including "_test.go") or "." are ignored.

Test files that declare a package with the suffix "_test" will be compiled as a
separate package, and then linked and run with the main test binary.

The go tool will ignore a directory named "testdata", making it available
to hold ancillary data needed by the tests.

As part of building a test binary, go test runs go vet on the package
and its test source files to identify significant problems. If go vet
finds any problems, go test reports those and does not run the test binary.
Only a high-confidence subset of the default go vet checks are used.
To disable the running of go vet, use the -vet=off flag.

All test output and summary lines are printed to the go command's
standard output, even if the test printed them to its own standard
error. (The go command's standard error is reserved for printing
errors building the tests.)

Go test runs in two different modes:

The first, called local directory mode, occurs when go test is
invoked with no package arguments (for example, 'go test' or 'go
test -v'). In this mode, go test compiles the package sources and
tests found in the current directory and then runs the resulting
test binary. In this mode, caching (discussed below) is disabled.
After the package test finishes, go test prints a summary line
showing the test status ('ok' or 'FAIL'), package name, and elapsed
time.

The second, called package list mode, occurs when go test is invoked
with explicit package arguments (for example 'go test math', 'go
test ./...', and even 'go test .'). In this mode, go test compiles
and tests each of the packages listed on the command line. If a
package test passes, go test prints only the final 'ok' summary
line. If a package test fails, go test prints the full test output.
If invoked with the -bench or -v flag, go test prints the full
output even for passing package tests, in order to display the
requested benchmark results or verbose logging.

In package list mode only, go test caches successful package test
results to avoid unnecessary repeated running of tests. When the
result of a test can be recovered from the cache, go test will
redisplay the previous output instead of running the test binary
again. When this happens, go test prints '(cached)' in place of the
elapsed time in the summary line.

The rule for a match in the cache is that the run involves the same
test binary and the flags on the command line come entirely from a
restricted set of 'cacheable' test flags, defined as -cpu, -list,
-parallel, -run, -short, and -v. If a run of go test has any test
or non-test flags outside this set, the result is not cached. To
disable test caching, use any test flag or argument other than the
cacheable flags. The idiomatic way to disable test caching explicitly
is to use -count=1. Tests that open files within the package's source
root (usually $GOPATH) or that consult environment variables only
match future runs in which the files and environment variables are unchanged.
A cached test result is treated as executing in no time at all,
so a successful package test result will be cached and reused
regardless of -timeout setting.

` + strings.TrimSpace(testFlag1) + ` See 'go help testflag' for details.

For more about build flags, see 'go help build'.
For more about specifying packages, see 'go help packages'.

See also: go build, go vet.
`,
}

const testFlag1 = `
In addition to the build flags, the flags handled by 'go test' itself are:

	-args
	    Pass the remainder of the command line (everything after -args)
	    to the test binary, uninterpreted and unchanged.
	    Because this flag consumes the remainder of the command line,
	    the package list (if present) must appear before this flag.

	-c
	    Compile the test binary to pkg.test but do not run it
	    (where pkg is the last element of the package's import path).
	    The file name can be changed with the -o flag.

	-exec xprog
	    Run the test binary using xprog. The behavior is the same as
	    in 'go run'. See 'go help run' for details.

	-i
	    Install packages that are dependencies of the test.
	    Do not run the test.

	-json
	    Convert test output to JSON suitable for automated processing.
	    See 'go doc test2json' for the encoding details.

	-o file
	    Compile the test binary to the named file.
	    The test still runs (unless -c or -i is specified).

The test binary also accepts flags that control execution of the test; these
flags are also accessible by 'go test'.
`

// Usage prints the usage message for 'go test -h' and exits.
func Usage() {
	os.Stderr.WriteString(testUsage + "\n\n" +
		strings.TrimSpace(testFlag1) + "\n\n\t" +
		strings.TrimSpace(testFlag2) + "\n")
	os.Exit(2)
}

var HelpTestflag = &base.Command{
	UsageLine: "testflag",
	Short:     "testing flags",
	Long: `
The 'go test' command takes both flags that apply to 'go test' itself
and flags that apply to the resulting test binary.

Several of the flags control profiling and write an execution profile
suitable for "go tool pprof"; run "go tool pprof -h" for more
information. The --alloc_space, --alloc_objects, and --show_bytes
options of pprof control how the information is presented.

The following flags are recognized by the 'go test' command and
control the execution of any test:

	` + strings.TrimSpace(testFlag2) + `
`,
}

const testFlag2 = `
	-bench regexp
	    Run only those benchmarks matching a regular expression.
	    By default, no benchmarks are run.
	    To run all benchmarks, use '-bench .' or '-bench=.'.
	    The regular expression is split by unbracketed slash (/)
	    characters into a sequence of regular expressions, and each
	    part of a benchmark's identifier must match the corresponding
	    element in the sequence, if any. Possible parents of matches
	    are run with b.N=1 to identify sub-benchmarks. For example,
	    given -bench=X/Y, top-level benchmarks matching X are run
	    with b.N=1 to find any sub-benchmarks matching Y, which are
	    then run in full.

	-benchtime t
	    Run enough iterations of each benchmark to take t, specified
	    as a time.Duration (for example, -benchtime 1h30s).
	    The default is 1 second (1s).

	-count n
	    Run each test and benchmark n times (default 1).
	    If -cpu is set, run n times for each GOMAXPROCS value.
	    Examples are always run once.

	-cover
	    Enable coverage analysis.
	    Note that because coverage works by annotating the source
	    code before compilation, compilation and test failures with
	    coverage enabled may report line numbers that don't correspond
	    to the original sources.

	-covermode set,count,atomic
	    Set the mode for coverage analysis for the package[s]
	    being tested. The default is "set" unless -race is enabled,
	    in which case it is "atomic".
	    The values:
		set: bool: does this statement run?
		count: int: how many times does this statement run?
		atomic: int: count, but correct in multithreaded tests;
			significantly more expensive.
	    Sets -cover.

	-coverpkg pattern1,pattern2,pattern3
	    Apply coverage analysis in each test to packages matching the patterns.
	    The default is for each test to analyze only the package being tested.
	    See 'go help packages' for a description of package patterns.
	    Sets -cover.

	-cpu 1,2,4
	    Specify a list of GOMAXPROCS values for which the tests or
	    benchmarks should be executed. The default is the current value
	    of GOMAXPROCS.

	-failfast
	    Do not start new tests after the first test failure.

	-list regexp
	    List tests, benchmarks, or examples matching the regular expression.
	    No tests, benchmarks or examples will be run. This will only
	    list top-level tests. No subtest or subbenchmarks will be shown.

	-parallel n
	    Allow parallel execution of test functions that call t.Parallel.
	    The value of this flag is the maximum number of tests to run
	    simultaneously; by default, it is set to the value of GOMAXPROCS.
	    Note that -parallel only applies within a single test binary.
	    The 'go test' command may run tests for different packages
	    in parallel as well, according to the setting of the -p flag
	    (see 'go help build').

	-run regexp
	    Run only those tests and examples matching the regular expression.
	    For tests, the regular expression is split by unbracketed slash (/)
	    characters into a sequence of regular expressions, and each part
	    of a test's identifier must match the corresponding element in
	    the sequence, if any. Note that possible parents of matches are
	    run too, so that -run=X/Y matches and runs and reports the result
	    of all tests matching X, even those without sub-tests matching Y,
	    because it must run them to look for those sub-tests.

	-short
	    Tell long-running tests to shorten their run time.
	    It is off by default but set during all.bash so that installing
	    the Go tree can run a sanity check but not spend time running
	    exhaustive tests.

	-timeout d
	    If a test binary runs longer than duration d, panic.
	    If d is 0, the timeout is disabled.
	    The default is 10 minutes (10m).

	-v
	    Verbose output: log all tests as they are run. Also print all
	    text from Log and Logf calls even if the test succeeds.

	-vet list
	    Configure the invocation of "go vet" during "go test"
	    to use the comma-separated list of vet checks.
	    If list is empty, "go test" runs "go vet" with a curated list of
	    checks believed to be always worth addressing.
	    If list is "off", "go test" does not run "go vet" at all.

The following flags are also recognized by 'go test' and can be used to
profile the tests during execution:

	-benchmem
	    Print memory allocation statistics for benchmarks.

	-blockprofile block.out
	    Write a goroutine blocking profile to the specified file
	    when all tests are complete.
	    Writes test binary as -c would.

	-blockprofilerate n
	    Control the detail provided in goroutine blocking profiles by
	    calling runtime.SetBlockProfileRate with n.
	    See 'go doc runtime.SetBlockProfileRate'.
	    The profiler aims to sample, on average, one blocking event every
	    n nanoseconds the program spends blocked. By default,
	    if -test.blockprofile is set without this flag, all blocking events
	    are recorded, equivalent to -test.blockprofilerate=1.

	-coverprofile cover.out
	    Write a coverage profile to the file after all tests have passed.
	    Sets -cover.

	-cpuprofile cpu.out
	    Write a CPU profile to the specified file before exiting.
	    Writes test binary as -c would.

	-memprofile mem.out
	    Write a memory profile to the file after all tests have passed.
	    Writes test binary as -c would.

	-memprofilerate n
	    Enable more precise (and expensive) memory profiles by setting
	    runtime.MemProfileRate. See 'go doc runtime.MemProfileRate'.
	    To profile all memory allocations, use -test.memprofilerate=1
	    and pass --alloc_space flag to the pprof tool.

	-mutexprofile mutex.out
	    Write a mutex contention profile to the specified file
	    when all tests are complete.
	    Writes test binary as -c would.

	-mutexprofilefraction n
	    Sample 1 in n stack traces of goroutines holding a
	    contended mutex.

	-outputdir directory
	    Place output files from profiling in the specified directory,
	    by default the directory in which "go test" is running.

	-trace trace.out
	    Write an execution trace to the specified file before exiting.

Each of these flags is also recognized with an optional 'test.' prefix,
as in -test.v. When invoking the generated test binary (the result of
'go test -c') directly, however, the prefix is mandatory.

The 'go test' command rewrites or removes recognized flags,
as appropriate, both before and after the optional package list,
before invoking the test binary.

For instance, the command

	go test -v -myflag testdata -cpuprofile=prof.out -x

will compile the test binary and then run it as

	pkg.test -test.v -myflag testdata -test.cpuprofile=prof.out

(The -x flag is removed because it applies only to the go command's
execution, not to the test itself.)

The test flags that generate profiles (other than for coverage) also
leave the test binary in pkg.test for use when analyzing the profiles.

When 'go test' runs a test binary, it does so from within the
corresponding package's source code directory. Depending on the test,
it may be necessary to do the same when invoking a generated test
binary directly.

The command-line package list, if present, must appear before any
flag not known to the go test command. Continuing the example above,
the package list would have to appear before -myflag, but could appear
on either side of -v.

To keep an argument for a test binary from being interpreted as a
known flag or a package name, use -args (see 'go help test') which
passes the remainder of the command line through to the test binary
uninterpreted and unaltered.

For instance, the command

	go test -v -args -x -v

will compile the test binary and then run it as

	pkg.test -test.v -x -v

Similarly,

	go test -args math

will compile the test binary and then run it as

	pkg.test math

In the first example, the -x and the second -v are passed through to the
test binary unchanged and with no effect on the go command itself.
In the second example, the argument math is passed through to the test
binary, instead of being interpreted as the package list.
`

var HelpTestfunc = &base.Command{
	UsageLine: "testfunc",
	Short:     "testing functions",
	Long: `
The 'go test' command expects to find test, benchmark, and example functions
in the "*_test.go" files corresponding to the package under test.

A test function is one named TestXxx (where Xxx does not start with a
lower case letter) and should have the signature,

	func TestXxx(t *testing.T) { ... }

A benchmark function is one named BenchmarkXxx and should have the signature,

	func BenchmarkXxx(b *testing.B) { ... }

An example function is similar to a test function but, instead of using
*testing.T to report success or failure, prints output to os.Stdout.
If the last comment in the function starts with "Output:" then the output
is compared exactly against the comment (see examples below). If the last
comment begins with "Unordered output:" then the output is compared to the
comment, however the order of the lines is ignored. An example with no such
comment is compiled but not executed. An example with no text after
"Output:" is compiled, executed, and expected to produce no output.

Godoc displays the body of ExampleXxx to demonstrate the use
of the function, constant, or variable Xxx. An example of a method M with
receiver type T or *T is named ExampleT_M. There may be multiple examples
for a given function, constant, or variable, distinguished by a trailing _xxx,
where xxx is a suffix not beginning with an upper case letter.

Here is an example of an example:

	func ExamplePrintln() {
		Println("The output of\nthis example.")
		// Output: The output of
		// this example.
	}

Here is another example where the ordering of the output is ignored:

	func ExamplePerm() {
		for _, value := range Perm(4) {
			fmt.Println(value)
		}

		// Unordered output: 4
		// 2
		// 1
		// 3
		// 0
	}

The entire test file is presented as the example when it contains a single
example function, at least one other function, type, variable, or constant
declaration, and no test or benchmark functions.

See the documentation of the testing package for more information.
`,
}

var (
	testC            bool            // -c flag
	testCover        bool            // -cover flag
	testCoverMode    string          // -covermode flag
	testCoverPaths   []string        // -coverpkg flag
	testCoverPkgs    []*load.Package // -coverpkg flag
	testCoverProfile string          // -coverprofile flag
	testOutputDir    string          // -outputdir flag
	testO            string          // -o flag
	testProfile      string          // profiling flag that limits test to one package
	testNeedBinary   bool            // profile needs to keep binary around
	testJSON         bool            // -json flag
	testV            bool            // -v flag
	testTimeout      string          // -timeout flag
	testArgs         []string
	testBench        bool
	testList         bool
	testShowPass     bool   // show passing output
	testVetList      string // -vet flag
	pkgArgs          []string
	pkgs             []*load.Package

	testKillTimeout = 10 * time.Minute
	testCacheExpire time.Time // ignore cached test results before this time
)

var testMainDeps = []string{
	// Dependencies for testmain.
	"os",
	"testing",
	"testing/internal/testdeps",
}

// testVetFlags is the list of flags to pass to vet when invoked automatically during go test.
var testVetFlags = []string{
	// TODO(rsc): Decide which tests are enabled by default.
	// See golang.org/issue/18085.
	// "-asmdecl",
	// "-assign",
	"-atomic",
	"-bool",
	"-buildtags",
	// "-cgocall",
	// "-composites",
	// "-copylocks",
	// "-httpresponse",
	// "-lostcancel",
	// "-methods",
	"-nilfunc",
	"-printf",
	// "-rangeloops",
	// "-shift",
	// "-structtags",
	// "-tests",
	// "-unreachable",
	// "-unsafeptr",
	// "-unusedresult",
}

func runTest(cmd *base.Command, args []string) {
	pkgArgs, testArgs = testFlags(args)

	work.FindExecCmd() // initialize cached result

	work.BuildInit()
	work.VetFlags = testVetFlags

	pkgs = load.PackagesForBuild(pkgArgs)
	if len(pkgs) == 0 {
		base.Fatalf("no packages to test")
	}

	if testC && len(pkgs) != 1 {
		base.Fatalf("cannot use -c flag with multiple packages")
	}
	if testO != "" && len(pkgs) != 1 {
		base.Fatalf("cannot use -o flag with multiple packages")
	}
	if testProfile != "" && len(pkgs) != 1 {
		base.Fatalf("cannot use %s flag with multiple packages", testProfile)
	}
	initCoverProfile()
	defer closeCoverProfile()

	// If a test timeout was given and is parseable, set our kill timeout
	// to that timeout plus one minute. This is a backup alarm in case
	// the test wedges with a goroutine spinning and its background
	// timer does not get a chance to fire.
	if dt, err := time.ParseDuration(testTimeout); err == nil && dt > 0 {
		testKillTimeout = dt + 1*time.Minute
	} else if err == nil && dt == 0 {
		// An explicit zero disables the test timeout.
		// Let it have one century (almost) before we kill it.
		testKillTimeout = 100 * 365 * 24 * time.Hour
	}

	// show passing test output (after buffering) with -v flag.
	// must buffer because tests are running in parallel, and
	// otherwise the output will get mixed.
	testShowPass = testV || testList

	// For 'go test -i -o x.test', we want to build x.test. Imply -c to make the logic easier.
	if cfg.BuildI && testO != "" {
		testC = true
	}

	// Read testcache expiration time, if present.
	// (We implement go clean -testcache by writing an expiration date
	// instead of searching out and deleting test result cache entries.)
	if dir := cache.DefaultDir(); dir != "off" {
		if data, _ := ioutil.ReadFile(filepath.Join(dir, "testexpire.txt")); len(data) > 0 && data[len(data)-1] == '\n' {
			if t, err := strconv.ParseInt(string(data[:len(data)-1]), 10, 64); err == nil {
				testCacheExpire = time.Unix(0, t)
			}
		}
	}

	var b work.Builder
	b.Init()

	if cfg.BuildI {
		cfg.BuildV = testV

		deps := make(map[string]bool)
		for _, dep := range testMainDeps {
			deps[dep] = true
		}

		for _, p := range pkgs {
			// Dependencies for each test.
			for _, path := range p.Imports {
				deps[path] = true
			}
			for _, path := range p.Resolve(p.TestImports) {
				deps[path] = true
			}
			for _, path := range p.Resolve(p.XTestImports) {
				deps[path] = true
			}
		}

		// translate C to runtime/cgo
		if deps["C"] {
			delete(deps, "C")
			deps["runtime/cgo"] = true
		}
		// Ignore pseudo-packages.
		delete(deps, "unsafe")

		all := []string{}
		for path := range deps {
			if !build.IsLocalImport(path) {
				all = append(all, path)
			}
		}
		sort.Strings(all)

		a := &work.Action{Mode: "go test -i"}
		for _, p := range load.PackagesForBuild(all) {
			if cfg.BuildToolchainName == "gccgo" && p.Standard {
				// gccgo's standard library packages
				// can not be reinstalled.
				continue
			}
			a.Deps = append(a.Deps, b.CompileAction(work.ModeInstall, work.ModeInstall, p))
		}
		b.Do(a)
		if !testC || a.Failed {
			return
		}
		b.Init()
	}

	var builds, runs, prints []*work.Action

	if testCoverPaths != nil {
		match := make([]func(*load.Package) bool, len(testCoverPaths))
		matched := make([]bool, len(testCoverPaths))
		for i := range testCoverPaths {
			match[i] = load.MatchPackage(testCoverPaths[i], base.Cwd)
		}

		// Select for coverage all dependencies matching the testCoverPaths patterns.
		for _, p := range load.PackageList(pkgs) {
			haveMatch := false
			for i := range testCoverPaths {
				if match[i](p) {
					matched[i] = true
					haveMatch = true
				}
			}

			// Silently ignore attempts to run coverage on
			// sync/atomic when using atomic coverage mode.
			// Atomic coverage mode uses sync/atomic, so
			// we can't also do coverage on it.
			if testCoverMode == "atomic" && p.Standard && p.ImportPath == "sync/atomic" {
				continue
			}

			// If using the race detector, silently ignore
			// attempts to run coverage on the runtime
			// packages. It will cause the race detector
			// to be invoked before it has been initialized.
			if cfg.BuildRace && p.Standard && (p.ImportPath == "runtime" || strings.HasPrefix(p.ImportPath, "runtime/internal")) {
				continue
			}

			if haveMatch {
				testCoverPkgs = append(testCoverPkgs, p)
			}
		}

		// Warn about -coverpkg arguments that are not actually used.
		for i := range testCoverPaths {
			if !matched[i] {
				fmt.Fprintf(os.Stderr, "warning: no packages being tested depend on matches for pattern %s\n", testCoverPaths[i])
			}
		}

		// Mark all the coverage packages for rebuilding with coverage.
		for _, p := range testCoverPkgs {
			// There is nothing to cover in package unsafe; it comes from the compiler.
			if p.ImportPath == "unsafe" {
				continue
			}
			p.Internal.CoverMode = testCoverMode
			var coverFiles []string
			coverFiles = append(coverFiles, p.GoFiles...)
			coverFiles = append(coverFiles, p.CgoFiles...)
			coverFiles = append(coverFiles, p.TestGoFiles...)
			p.Internal.CoverVars = declareCoverVars(p.ImportPath, coverFiles...)
			if testCover && testCoverMode == "atomic" {
				ensureImport(p, "sync/atomic")
			}
		}
	}

	// Prepare build + run + print actions for all packages being tested.
	for _, p := range pkgs {
		// sync/atomic import is inserted by the cover tool. See #18486
		if testCover && testCoverMode == "atomic" {
			ensureImport(p, "sync/atomic")
		}

		buildTest, runTest, printTest, err := builderTest(&b, p)
		if err != nil {
			str := err.Error()
			if strings.HasPrefix(str, "\n") {
				str = str[1:]
			}
			failed := fmt.Sprintf("FAIL\t%s [setup failed]\n", p.ImportPath)

			if p.ImportPath != "" {
				base.Errorf("# %s\n%s\n%s", p.ImportPath, str, failed)
			} else {
				base.Errorf("%s\n%s", str, failed)
			}
			continue
		}
		builds = append(builds, buildTest)
		runs = append(runs, runTest)
		prints = append(prints, printTest)
	}

	// Ultimately the goal is to print the output.
	root := &work.Action{Mode: "go test", Deps: prints}

	// Force the printing of results to happen in order,
	// one at a time.
	for i, a := range prints {
		if i > 0 {
			a.Deps = append(a.Deps, prints[i-1])
		}
	}

	// Force benchmarks to run in serial.
	if !testC && testBench {
		// The first run must wait for all builds.
		// Later runs must wait for the previous run's print.
		for i, run := range runs {
			if i == 0 {
				run.Deps = append(run.Deps, builds...)
			} else {
				run.Deps = append(run.Deps, prints[i-1])
			}
		}
	}

	b.Do(root)
}

// ensures that package p imports the named package
func ensureImport(p *load.Package, pkg string) {
	for _, d := range p.Internal.Imports {
		if d.Name == pkg {
			return
		}
	}

	p1 := load.LoadPackage(pkg, &load.ImportStack{})
	if p1.Error != nil {
		base.Fatalf("load %s: %v", pkg, p1.Error)
	}

	p.Internal.Imports = append(p.Internal.Imports, p1)
}

var windowsBadWords = []string{
	"install",
	"patch",
	"setup",
	"update",
}

func builderTest(b *work.Builder, p *load.Package) (buildAction, runAction, printAction *work.Action, err error) {
	if len(p.TestGoFiles)+len(p.XTestGoFiles) == 0 {
		build := b.CompileAction(work.ModeBuild, work.ModeBuild, p)
		run := &work.Action{Mode: "test run", Package: p, Deps: []*work.Action{build}}
		addTestVet(b, p, run, nil)
		print := &work.Action{Mode: "test print", Func: builderNoTest, Package: p, Deps: []*work.Action{run}}
		return build, run, print, nil
	}

	// Build Package structs describing:
	//	ptest - package + test files
	//	pxtest - package of external test files
	//	pmain - pkg.test binary
	var ptest, pxtest, pmain *load.Package

	localCover := testCover && testCoverPaths == nil

	ptest, pxtest, err = load.GetTestPackagesFor(p, localCover || p.Name == "main")
	if err != nil {
		return nil, nil, nil, err
	}

	// Use last element of import path, not package name.
	// They differ when package name is "main".
	// But if the import path is "command-line-arguments",
	// like it is during 'go run', use the package name.
	var elem string
	if p.ImportPath == "command-line-arguments" {
		elem = p.Name
	} else {
		_, elem = path.Split(p.ImportPath)
	}
	testBinary := elem + ".test"

	// Should we apply coverage analysis locally,
	// only for this package and only for this test?
	// Yes, if -cover is on but -coverpkg has not specified
	// a list of packages for global coverage.
	if localCover {
		ptest.Internal.CoverMode = testCoverMode
		var coverFiles []string
		coverFiles = append(coverFiles, ptest.GoFiles...)
		coverFiles = append(coverFiles, ptest.CgoFiles...)
		ptest.Internal.CoverVars = declareCoverVars(ptest.ImportPath, coverFiles...)
	}

	testDir := b.NewObjdir()
	if err := b.Mkdir(testDir); err != nil {
		return nil, nil, nil, err
	}

	// Action for building pkg.test.
	pmain = &load.Package{
		PackagePublic: load.PackagePublic{
			Name:       "main",
			Dir:        testDir,
			GoFiles:    []string{"_testmain.go"},
			ImportPath: p.ImportPath + " (testmain)",
			Root:       p.Root,
		},
		Internal: load.PackageInternal{
			Build:     &build.Package{Name: "main"},
			OmitDebug: !testC && !testNeedBinary,

			Asmflags:   p.Internal.Asmflags,
			Gcflags:    p.Internal.Gcflags,
			Ldflags:    p.Internal.Ldflags,
			Gccgoflags: p.Internal.Gccgoflags,
		},
	}

	// The generated main also imports testing, regexp, and os.
	// Also the linker introduces implicit dependencies reported by LinkerDeps.
	var stk load.ImportStack
	stk.Push("testmain")
	deps := testMainDeps // cap==len, so safe for append
	for _, d := range load.LinkerDeps(p) {
		deps = append(deps, d)
	}
	for _, dep := range deps {
		if dep == ptest.ImportPath {
			pmain.Internal.Imports = append(pmain.Internal.Imports, ptest)
		} else {
			p1 := load.LoadImport(dep, "", nil, &stk, nil, 0)
			if p1.Error != nil {
				return nil, nil, nil, p1.Error
			}
			pmain.Internal.Imports = append(pmain.Internal.Imports, p1)
		}
	}

	if testCoverPkgs != nil {
		// Add imports, but avoid duplicates.
		seen := map[*load.Package]bool{p: true, ptest: true}
		for _, p1 := range pmain.Internal.Imports {
			seen[p1] = true
		}
		for _, p1 := range testCoverPkgs {
			if !seen[p1] {
				seen[p1] = true
				pmain.Internal.Imports = append(pmain.Internal.Imports, p1)
			}
		}
	}

	// Do initial scan for metadata needed for writing _testmain.go
	// Use that metadata to update the list of imports for package main.
	// The list of imports is used by recompileForTest and by the loop
	// afterward that gathers t.Cover information.
	t, err := loadTestFuncs(ptest)
	if err != nil {
		return nil, nil, nil, err
	}
	if len(ptest.GoFiles)+len(ptest.CgoFiles) > 0 {
		pmain.Internal.Imports = append(pmain.Internal.Imports, ptest)
		t.ImportTest = true
	}
	if pxtest != nil {
		pmain.Internal.Imports = append(pmain.Internal.Imports, pxtest)
		t.ImportXtest = true
	}

	if ptest != p {
		// We have made modifications to the package p being tested
		// and are rebuilding p (as ptest).
		// Arrange to rebuild all packages q such that
		// the test depends on q and q depends on p.
		// This makes sure that q sees the modifications to p.
		// Strictly speaking, the rebuild is only necessary if the
		// modifications to p change its export metadata, but
		// determining that is a bit tricky, so we rebuild always.
		recompileForTest(pmain, p, ptest, pxtest)
	}

	for _, cp := range pmain.Internal.Imports {
		if len(cp.Internal.CoverVars) > 0 {
			t.Cover = append(t.Cover, coverInfo{cp, cp.Internal.CoverVars})
		}
	}

	if !cfg.BuildN {
		// writeTestmain writes _testmain.go,
		// using the test description gathered in t.
		if err := writeTestmain(testDir+"_testmain.go", t); err != nil {
			return nil, nil, nil, err
		}
	}

	// Set compile objdir to testDir we've already created,
	// so that the default file path stripping applies to _testmain.go.
	b.CompileAction(work.ModeBuild, work.ModeBuild, pmain).Objdir = testDir

	a := b.LinkAction(work.ModeBuild, work.ModeBuild, pmain)
	a.Target = testDir + testBinary + cfg.ExeSuffix
	if cfg.Goos == "windows" {
		// There are many reserved words on Windows that,
		// if used in the name of an executable, cause Windows
		// to try to ask for extra permissions.
		// The word list includes setup, install, update, and patch,
		// but it does not appear to be defined anywhere.
		// We have run into this trying to run the
		// go.codereview/patch tests.
		// For package names containing those words, use test.test.exe
		// instead of pkgname.test.exe.
		// Note that this file name is only used in the Go command's
		// temporary directory. If the -c or other flags are
		// given, the code below will still use pkgname.test.exe.
		// There are two user-visible effects of this change.
		// First, you can actually run 'go test' in directories that
		// have names that Windows thinks are installer-like,
		// without getting a dialog box asking for more permissions.
		// Second, in the Windows process listing during go test,
		// the test shows up as test.test.exe, not pkgname.test.exe.
		// That second one is a drawback, but it seems a small
		// price to pay for the test running at all.
		// If maintaining the list of bad words is too onerous,
		// we could just do this always on Windows.
		for _, bad := range windowsBadWords {
			if strings.Contains(testBinary, bad) {
				a.Target = testDir + "test.test" + cfg.ExeSuffix
				break
			}
		}
	}
	buildAction = a
	var installAction, cleanAction *work.Action
	if testC || testNeedBinary {
		// -c or profiling flag: create action to copy binary to ./test.out.
		target := filepath.Join(base.Cwd, testBinary+cfg.ExeSuffix)
		if testO != "" {
			target = testO
			if !filepath.IsAbs(target) {
				target = filepath.Join(base.Cwd, target)
			}
		}
		pmain.Target = target
		installAction = &work.Action{
			Mode:    "test build",
			Func:    work.BuildInstallFunc,
			Deps:    []*work.Action{buildAction},
			Package: pmain,
			Target:  target,
		}
		runAction = installAction // make sure runAction != nil even if not running test
	}
	if testC {
		printAction = &work.Action{Mode: "test print (nop)", Package: p, Deps: []*work.Action{runAction}} // nop
	} else {
		// run test
		c := new(runCache)
		runAction = &work.Action{
			Mode:       "test run",
			Func:       c.builderRunTest,
			Deps:       []*work.Action{buildAction},
			Package:    p,
			IgnoreFail: true, // run (prepare output) even if build failed
			TryCache:   c.tryCache,
			Objdir:     testDir,
		}
		if len(ptest.GoFiles)+len(ptest.CgoFiles) > 0 {
			addTestVet(b, ptest, runAction, installAction)
		}
		if pxtest != nil {
			addTestVet(b, pxtest, runAction, installAction)
		}
		cleanAction = &work.Action{
			Mode:       "test clean",
			Func:       builderCleanTest,
			Deps:       []*work.Action{runAction},
			Package:    p,
			IgnoreFail: true, // clean even if test failed
			Objdir:     testDir,
		}
		printAction = &work.Action{
			Mode:       "test print",
			Func:       builderPrintTest,
			Deps:       []*work.Action{cleanAction},
			Package:    p,
			IgnoreFail: true, // print even if test failed
		}
	}
	if installAction != nil {
		if runAction != installAction {
			installAction.Deps = append(installAction.Deps, runAction)
		}
		if cleanAction != nil {
			cleanAction.Deps = append(cleanAction.Deps, installAction)
		}
	}

	return buildAction, runAction, printAction, nil
}

func addTestVet(b *work.Builder, p *load.Package, runAction, installAction *work.Action) {
	if testVetList == "off" {
		return
	}

	vet := b.VetAction(work.ModeBuild, work.ModeBuild, p)
	runAction.Deps = append(runAction.Deps, vet)
	// Install will clean the build directory.
	// Make sure vet runs first.
	// The install ordering in b.VetAction does not apply here
	// because we are using a custom installAction (created above).
	if installAction != nil {
		installAction.Deps = append(installAction.Deps, vet)
	}
}

func recompileForTest(pmain, preal, ptest, pxtest *load.Package) {
	// The "test copy" of preal is ptest.
	// For each package that depends on preal, make a "test copy"
	// that depends on ptest. And so on, up the dependency tree.
	testCopy := map[*load.Package]*load.Package{preal: ptest}
	for _, p := range load.PackageList([]*load.Package{pmain}) {
		if p == preal {
			continue
		}
		// Copy on write.
		didSplit := p == pmain || p == pxtest
		split := func() {
			if didSplit {
				return
			}
			didSplit = true
			if testCopy[p] != nil {
				panic("recompileForTest loop")
			}
			p1 := new(load.Package)
			testCopy[p] = p1
			*p1 = *p
			p1.Internal.Imports = make([]*load.Package, len(p.Internal.Imports))
			copy(p1.Internal.Imports, p.Internal.Imports)
			p = p1
			p.Target = ""
		}

		// Update p.Internal.Imports to use test copies.
		for i, imp := range p.Internal.Imports {
			if p1 := testCopy[imp]; p1 != nil && p1 != imp {
				split()
				p.Internal.Imports[i] = p1
			}
		}
	}
}

// isTestFile reports whether the source file is a set of tests and should therefore
// be excluded from coverage analysis.
func isTestFile(file string) bool {
	// We don't cover tests, only the code they test.
	return strings.HasSuffix(file, "_test.go")
}

// declareCoverVars attaches the required cover variables names
// to the files, to be used when annotating the files.
func declareCoverVars(importPath string, files ...string) map[string]*load.CoverVar {
	coverVars := make(map[string]*load.CoverVar)
	coverIndex := 0
	// We create the cover counters as new top-level variables in the package.
	// We need to avoid collisions with user variables (GoCover_0 is unlikely but still)
	// and more importantly with dot imports of other covered packages,
	// so we append 12 hex digits from the SHA-256 of the import path.
	// The point is only to avoid accidents, not to defeat users determined to
	// break things.
	sum := sha256.Sum256([]byte(importPath))
	h := fmt.Sprintf("%x", sum[:6])
	for _, file := range files {
		if isTestFile(file) {
			continue
		}
		coverVars[file] = &load.CoverVar{
			File: filepath.Join(importPath, file),
			Var:  fmt.Sprintf("GoCover_%d_%x", coverIndex, h),
		}
		coverIndex++
	}
	return coverVars
}

var noTestsToRun = []byte("\ntesting: warning: no tests to run\n")

type runCache struct {
	disableCache bool // cache should be disabled for this run

	buf *bytes.Buffer
	id1 cache.ActionID
	id2 cache.ActionID
}

// stdoutMu and lockedStdout provide a locked standard output
// that guarantees never to interlace writes from multiple
// goroutines, so that we can have multiple JSON streams writing
// to a lockedStdout simultaneously and know that events will
// still be intelligible.
var stdoutMu sync.Mutex

type lockedStdout struct{}

func (lockedStdout) Write(b []byte) (int, error) {
	stdoutMu.Lock()
	defer stdoutMu.Unlock()
	return os.Stdout.Write(b)
}

// builderRunTest is the action for running a test binary.
func (c *runCache) builderRunTest(b *work.Builder, a *work.Action) error {
	if a.Failed {
		// We were unable to build the binary.
		a.Failed = false
		a.TestOutput = new(bytes.Buffer)
		fmt.Fprintf(a.TestOutput, "FAIL\t%s [build failed]\n", a.Package.ImportPath)
		base.SetExitStatus(1)
		return nil
	}

	var stdout io.Writer = os.Stdout
	if testJSON {
		json := test2json.NewConverter(lockedStdout{}, a.Package.ImportPath, test2json.Timestamp)
		defer json.Close()
		stdout = json
	}

	var buf bytes.Buffer
	if len(pkgArgs) == 0 || testBench {
		// Stream test output (no buffering) when no package has
		// been given on the command line (implicit current directory)
		// or when benchmarking.
		// No change to stdout.
	} else {
		// If we're only running a single package under test or if parallelism is
		// set to 1, and if we're displaying all output (testShowPass), we can
		// hurry the output along, echoing it as soon as it comes in.
		// We still have to copy to &buf for caching the result. This special
		// case was introduced in Go 1.5 and is intentionally undocumented:
		// the exact details of output buffering are up to the go command and
		// subject to change. It would be nice to remove this special case
		// entirely, but it is surely very helpful to see progress being made
		// when tests are run on slow single-CPU ARM systems.
		//
		// If we're showing JSON output, then display output as soon as
		// possible even when multiple tests are being run: the JSON output
		// events are attributed to specific package tests, so interlacing them
		// is OK.
		if testShowPass && (len(pkgs) == 1 || cfg.BuildP == 1) || testJSON {
			// Write both to stdout and buf, for possible saving
			// to cache, and for looking for the "no tests to run" message.
			stdout = io.MultiWriter(stdout, &buf)
		} else {
			stdout = &buf
		}
	}

	if c.buf == nil {
		// We did not find a cached result using the link step action ID,
		// so we ran the link step. Try again now with the link output
		// content ID. The attempt using the action ID makes sure that
		// if the link inputs don't change, we reuse the cached test
		// result without even rerunning the linker. The attempt using
		// the link output (test binary) content ID makes sure that if
		// we have different link inputs but the same final binary,
		// we still reuse the cached test result.
		// c.saveOutput will store the result under both IDs.
		c.tryCacheWithID(b, a, a.Deps[0].BuildContentID())
	}
	if c.buf != nil {
		if stdout != &buf {
			stdout.Write(c.buf.Bytes())
			c.buf.Reset()
		}
		a.TestOutput = c.buf
		return nil
	}

	execCmd := work.FindExecCmd()
	testlogArg := []string{}
	if !c.disableCache && len(execCmd) == 0 {
		testlogArg = []string{"-test.testlogfile=" + a.Objdir + "testlog.txt"}
	}
	args := str.StringList(execCmd, a.Deps[0].BuiltTarget(), testlogArg, testArgs)

	if testCoverProfile != "" {
		// Write coverage to temporary profile, for merging later.
		for i, arg := range args {
			if strings.HasPrefix(arg, "-test.coverprofile=") {
				args[i] = "-test.coverprofile=" + a.Objdir + "_cover_.out"
			}
		}
	}

	if cfg.BuildN || cfg.BuildX {
		b.Showcmd("", "%s", strings.Join(args, " "))
		if cfg.BuildN {
			return nil
		}
	}

	cmd := exec.Command(args[0], args[1:]...)
	cmd.Dir = a.Package.Dir
	cmd.Env = base.EnvForDir(cmd.Dir, cfg.OrigEnv)
	cmd.Stdout = stdout
	cmd.Stderr = stdout

	// If there are any local SWIG dependencies, we want to load
	// the shared library from the build directory.
	if a.Package.UsesSwig() {
		env := cmd.Env
		found := false
		prefix := "LD_LIBRARY_PATH="
		for i, v := range env {
			if strings.HasPrefix(v, prefix) {
				env[i] = v + ":."
				found = true
				break
			}
		}
		if !found {
			env = append(env, "LD_LIBRARY_PATH=.")
		}
		cmd.Env = env
	}

	t0 := time.Now()
	err := cmd.Start()

	// This is a last-ditch deadline to detect and
	// stop wedged test binaries, to keep the builders
	// running.
	if err == nil {
		tick := time.NewTimer(testKillTimeout)
		base.StartSigHandlers()
		done := make(chan error)
		go func() {
			done <- cmd.Wait()
		}()
	Outer:
		select {
		case err = <-done:
			// ok
		case <-tick.C:
			if base.SignalTrace != nil {
				// Send a quit signal in the hope that the program will print
				// a stack trace and exit. Give it five seconds before resorting
				// to Kill.
				cmd.Process.Signal(base.SignalTrace)
				select {
				case err = <-done:
					fmt.Fprintf(cmd.Stdout, "*** Test killed with %v: ran too long (%v).\n", base.SignalTrace, testKillTimeout)
					break Outer
				case <-time.After(5 * time.Second):
				}
			}
			cmd.Process.Kill()
			err = <-done
			fmt.Fprintf(cmd.Stdout, "*** Test killed: ran too long (%v).\n", testKillTimeout)
		}
		tick.Stop()
	}
	out := buf.Bytes()
	a.TestOutput = &buf
	t := fmt.Sprintf("%.3fs", time.Since(t0).Seconds())

	mergeCoverProfile(cmd.Stdout, a.Objdir+"_cover_.out")

	if err == nil {
		norun := ""
		if !testShowPass && !testJSON {
			buf.Reset()
		}
		if bytes.HasPrefix(out, noTestsToRun[1:]) || bytes.Contains(out, noTestsToRun) {
			norun = " [no tests to run]"
		}
		fmt.Fprintf(cmd.Stdout, "ok  \t%s\t%s%s%s\n", a.Package.ImportPath, t, coveragePercentage(out), norun)
		c.saveOutput(a)
	} else {
		base.SetExitStatus(1)
		// If there was test output, assume we don't need to print the exit status.
		// Buf there's no test output, do print the exit status.
		if len(out) == 0 {
			fmt.Fprintf(cmd.Stdout, "%s\n", err)
		}
		fmt.Fprintf(cmd.Stdout, "FAIL\t%s\t%s\n", a.Package.ImportPath, t)
	}

	if cmd.Stdout != &buf {
		buf.Reset() // cmd.Stdout was going to os.Stdout already
	}
	return nil
}

// tryCache is called just before the link attempt,
// to see if the test result is cached and therefore the link is unneeded.
// It reports whether the result can be satisfied from cache.
func (c *runCache) tryCache(b *work.Builder, a *work.Action) bool {
	return c.tryCacheWithID(b, a, a.Deps[0].BuildActionID())
}

func (c *runCache) tryCacheWithID(b *work.Builder, a *work.Action, id string) bool {
	if len(pkgArgs) == 0 {
		// Caching does not apply to "go test",
		// only to "go test foo" (including "go test .").
		if cache.DebugTest {
			fmt.Fprintf(os.Stderr, "testcache: caching disabled in local directory mode\n")
		}
		c.disableCache = true
		return false
	}

	var cacheArgs []string
	for _, arg := range testArgs {
		i := strings.Index(arg, "=")
		if i < 0 || !strings.HasPrefix(arg, "-test.") {
			if cache.DebugTest {
				fmt.Fprintf(os.Stderr, "testcache: caching disabled for test argument: %s\n", arg)
			}
			c.disableCache = true
			return false
		}
		switch arg[:i] {
		case "-test.cpu",
			"-test.list",
			"-test.parallel",
			"-test.run",
			"-test.short",
			"-test.v":
			// These are cacheable.
			// Note that this list is documented above,
			// so if you add to this list, update the docs too.
			cacheArgs = append(cacheArgs, arg)

		case "-test.timeout":
			// Special case: this is cacheable but ignored during the hash.
			// Do not add to cacheArgs.

		default:
			// nothing else is cacheable
			if cache.DebugTest {
				fmt.Fprintf(os.Stderr, "testcache: caching disabled for test argument: %s\n", arg)
			}
			c.disableCache = true
			return false
		}
	}

	if cache.Default() == nil {
		if cache.DebugTest {
			fmt.Fprintf(os.Stderr, "testcache: GOCACHE=off\n")
		}
		c.disableCache = true
		return false
	}

	// The test cache result fetch is a two-level lookup.
	//
	// First, we use the content hash of the test binary
	// and its command-line arguments to find the
	// list of environment variables and files consulted
	// the last time the test was run with those arguments.
	// (To avoid unnecessary links, we store this entry
	// under two hashes: id1 uses the linker inputs as a
	// proxy for the test binary, and id2 uses the actual
	// test binary. If the linker inputs are unchanged,
	// this way we avoid the link step, even though we
	// do not cache link outputs.)
	//
	// Second, we compute a hash of the values of the
	// environment variables and the content of the files
	// listed in the log from the previous run.
	// Then we look up test output using a combination of
	// the hash from the first part (testID) and the hash of the
	// test inputs (testInputsID).
	//
	// In order to store a new test result, we must redo the
	// testInputsID computation using the log from the run
	// we want to cache, and then we store that new log and
	// the new outputs.

	h := cache.NewHash("testResult")
	fmt.Fprintf(h, "test binary %s args %q execcmd %q", id, cacheArgs, work.ExecCmd)
	testID := h.Sum()
	if c.id1 == (cache.ActionID{}) {
		c.id1 = testID
	} else {
		c.id2 = testID
	}
	if cache.DebugTest {
		fmt.Fprintf(os.Stderr, "testcache: %s: test ID %x => %x\n", a.Package.ImportPath, id, testID)
	}

	// Load list of referenced environment variables and files
	// from last run of testID, and compute hash of that content.
	data, entry, err := cache.Default().GetBytes(testID)
	if !bytes.HasPrefix(data, testlogMagic) || data[len(data)-1] != '\n' {
		if cache.DebugTest {
			if err != nil {
				fmt.Fprintf(os.Stderr, "testcache: %s: input list not found: %v\n", a.Package.ImportPath, err)
			} else {
				fmt.Fprintf(os.Stderr, "testcache: %s: input list malformed\n", a.Package.ImportPath)
			}
		}
		return false
	}
	testInputsID, err := computeTestInputsID(a, data)
	if err != nil {
		return false
	}
	if cache.DebugTest {
		fmt.Fprintf(os.Stderr, "testcache: %s: test ID %x => input ID %x => %x\n", a.Package.ImportPath, testID, testInputsID, testAndInputKey(testID, testInputsID))
	}

	// Parse cached result in preparation for changing run time to "(cached)".
	// If we can't parse the cached result, don't use it.
	data, entry, err = cache.Default().GetBytes(testAndInputKey(testID, testInputsID))
	if len(data) == 0 || data[len(data)-1] != '\n' {
		if cache.DebugTest {
			if err != nil {
				fmt.Fprintf(os.Stderr, "testcache: %s: test output not found: %v\n", a.Package.ImportPath, err)
			} else {
				fmt.Fprintf(os.Stderr, "testcache: %s: test output malformed\n", a.Package.ImportPath)
			}
		}
		return false
	}
	if entry.Time.Before(testCacheExpire) {
		if cache.DebugTest {
			fmt.Fprintf(os.Stderr, "testcache: %s: test output expired due to go clean -testcache\n", a.Package.ImportPath)
		}
		return false
	}
	i := bytes.LastIndexByte(data[:len(data)-1], '\n') + 1
	if !bytes.HasPrefix(data[i:], []byte("ok  \t")) {
		if cache.DebugTest {
			fmt.Fprintf(os.Stderr, "testcache: %s: test output malformed\n", a.Package.ImportPath)
		}
		return false
	}
	j := bytes.IndexByte(data[i+len("ok  \t"):], '\t')
	if j < 0 {
		if cache.DebugTest {
			fmt.Fprintf(os.Stderr, "testcache: %s: test output malformed\n", a.Package.ImportPath)
		}
		return false
	}
	j += i + len("ok  \t") + 1

	// Committed to printing.
	c.buf = new(bytes.Buffer)
	c.buf.Write(data[:j])
	c.buf.WriteString("(cached)")
	for j < len(data) && ('0' <= data[j] && data[j] <= '9' || data[j] == '.' || data[j] == 's') {
		j++
	}
	c.buf.Write(data[j:])
	return true
}

var errBadTestInputs = errors.New("error parsing test inputs")
var testlogMagic = []byte("# test log\n") // known to testing/internal/testdeps/deps.go

// computeTestInputsID computes the "test inputs ID"
// (see comment in tryCacheWithID above) for the
// test log.
func computeTestInputsID(a *work.Action, testlog []byte) (cache.ActionID, error) {
	testlog = bytes.TrimPrefix(testlog, testlogMagic)
	h := cache.NewHash("testInputs")
	pwd := a.Package.Dir
	for _, line := range bytes.Split(testlog, []byte("\n")) {
		if len(line) == 0 {
			continue
		}
		s := string(line)
		i := strings.Index(s, " ")
		if i < 0 {
			if cache.DebugTest {
				fmt.Fprintf(os.Stderr, "testcache: %s: input list malformed (%q)\n", a.Package.ImportPath, line)
			}
			return cache.ActionID{}, errBadTestInputs
		}
		op := s[:i]
		name := s[i+1:]
		switch op {
		default:
			if cache.DebugTest {
				fmt.Fprintf(os.Stderr, "testcache: %s: input list malformed (%q)\n", a.Package.ImportPath, line)
			}
			return cache.ActionID{}, errBadTestInputs
		case "getenv":
			fmt.Fprintf(h, "env %s %x\n", name, hashGetenv(name))
		case "chdir":
			pwd = name // always absolute
			fmt.Fprintf(h, "cbdir %s %x\n", name, hashStat(name))
		case "stat":
			if !filepath.IsAbs(name) {
				name = filepath.Join(pwd, name)
			}
			if !inDir(name, a.Package.Root) {
				// Do not recheck files outside the GOPATH or GOROOT root.
				break
			}
			fmt.Fprintf(h, "stat %s %x\n", name, hashStat(name))
		case "open":
			if !filepath.IsAbs(name) {
				name = filepath.Join(pwd, name)
			}
			if !inDir(name, a.Package.Root) {
				// Do not recheck files outside the GOPATH or GOROOT root.
				break
			}
			fh, err := hashOpen(name)
			if err != nil {
				if cache.DebugTest {
					fmt.Fprintf(os.Stderr, "testcache: %s: input file %s: %s\n", a.Package.ImportPath, name, err)
				}
				return cache.ActionID{}, err
			}
			fmt.Fprintf(h, "open %s %x\n", name, fh)
		}
	}
	sum := h.Sum()
	return sum, nil
}

func inDir(path, dir string) bool {
	if str.HasFilePathPrefix(path, dir) {
		return true
	}
	xpath, err1 := filepath.EvalSymlinks(path)
	xdir, err2 := filepath.EvalSymlinks(dir)
	if err1 == nil && err2 == nil && str.HasFilePathPrefix(xpath, xdir) {
		return true
	}
	return false
}

func hashGetenv(name string) cache.ActionID {
	h := cache.NewHash("getenv")
	v, ok := os.LookupEnv(name)
	if !ok {
		h.Write([]byte{0})
	} else {
		h.Write([]byte{1})
		h.Write([]byte(v))
	}
	return h.Sum()
}

const modTimeCutoff = 2 * time.Second

var errFileTooNew = errors.New("file used as input is too new")

func hashOpen(name string) (cache.ActionID, error) {
	h := cache.NewHash("open")
	info, err := os.Stat(name)
	if err != nil {
		fmt.Fprintf(h, "err %v\n", err)
		return h.Sum(), nil
	}
	hashWriteStat(h, info)
	if info.IsDir() {
		names, err := ioutil.ReadDir(name)
		if err != nil {
			fmt.Fprintf(h, "err %v\n", err)
		}
		for _, f := range names {
			fmt.Fprintf(h, "file %s ", f.Name())
			hashWriteStat(h, f)
		}
	} else if info.Mode().IsRegular() {
		// Because files might be very large, do not attempt
		// to hash the entirety of their content. Instead assume
		// the mtime and size recorded in hashWriteStat above
		// are good enough.
		//
		// To avoid problems for very recent files where a new
		// write might not change the mtime due to file system
		// mtime precision, reject caching if a file was read that
		// is less than modTimeCutoff old.
		if time.Since(info.ModTime()) < modTimeCutoff {
			return cache.ActionID{}, errFileTooNew
		}
	}
	return h.Sum(), nil
}

func hashStat(name string) cache.ActionID {
	h := cache.NewHash("stat")
	if info, err := os.Stat(name); err != nil {
		fmt.Fprintf(h, "err %v\n", err)
	} else {
		hashWriteStat(h, info)
	}
	if info, err := os.Lstat(name); err != nil {
		fmt.Fprintf(h, "err %v\n", err)
	} else {
		hashWriteStat(h, info)
	}
	return h.Sum()
}

func hashWriteStat(h io.Writer, info os.FileInfo) {
	fmt.Fprintf(h, "stat %d %x %v %v\n", info.Size(), uint64(info.Mode()), info.ModTime(), info.IsDir())
}

// testAndInputKey returns the actual cache key for the pair (testID, testInputsID).
func testAndInputKey(testID, testInputsID cache.ActionID) cache.ActionID {
	return cache.Subkey(testID, fmt.Sprintf("inputs:%x", testInputsID))
}

func (c *runCache) saveOutput(a *work.Action) {
	if c.id1 == (cache.ActionID{}) && c.id2 == (cache.ActionID{}) {
		return
	}

	// See comment about two-level lookup in tryCacheWithID above.
	testlog, err := ioutil.ReadFile(a.Objdir + "testlog.txt")
	if err != nil || !bytes.HasPrefix(testlog, testlogMagic) || testlog[len(testlog)-1] != '\n' {
		if cache.DebugTest {
			if err != nil {
				fmt.Fprintf(os.Stderr, "testcache: %s: reading testlog: %v\n", a.Package.ImportPath, err)
			} else {
				fmt.Fprintf(os.Stderr, "testcache: %s: reading testlog: malformed\n", a.Package.ImportPath)
			}
		}
		return
	}
	testInputsID, err := computeTestInputsID(a, testlog)
	if err != nil {
		return
	}
	if c.id1 != (cache.ActionID{}) {
		if cache.DebugTest {
			fmt.Fprintf(os.Stderr, "testcache: %s: save test ID %x => input ID %x => %x\n", a.Package.ImportPath, c.id1, testInputsID, testAndInputKey(c.id1, testInputsID))
		}
		cache.Default().PutNoVerify(c.id1, bytes.NewReader(testlog))
		cache.Default().PutNoVerify(testAndInputKey(c.id1, testInputsID), bytes.NewReader(a.TestOutput.Bytes()))
	}
	if c.id2 != (cache.ActionID{}) {
		if cache.DebugTest {
			fmt.Fprintf(os.Stderr, "testcache: %s: save test ID %x => input ID %x => %x\n", a.Package.ImportPath, c.id2, testInputsID, testAndInputKey(c.id2, testInputsID))
		}
		cache.Default().PutNoVerify(c.id2, bytes.NewReader(testlog))
		cache.Default().PutNoVerify(testAndInputKey(c.id2, testInputsID), bytes.NewReader(a.TestOutput.Bytes()))
	}
}

// coveragePercentage returns the coverage results (if enabled) for the
// test. It uncovers the data by scanning the output from the test run.
func coveragePercentage(out []byte) string {
	if !testCover {
		return ""
	}
	// The string looks like
	//	test coverage for encoding/binary: 79.9% of statements
	// Extract the piece from the percentage to the end of the line.
	re := regexp.MustCompile(`coverage: (.*)\n`)
	matches := re.FindSubmatch(out)
	if matches == nil {
		// Probably running "go test -cover" not "go test -cover fmt".
		// The coverage output will appear in the output directly.
		return ""
	}
	return fmt.Sprintf("\tcoverage: %s", matches[1])
}

// builderCleanTest is the action for cleaning up after a test.
func builderCleanTest(b *work.Builder, a *work.Action) error {
	if cfg.BuildWork {
		return nil
	}
	if cfg.BuildX {
		b.Showcmd("", "rm -r %s", a.Objdir)
	}
	os.RemoveAll(a.Objdir)
	return nil
}

// builderPrintTest is the action for printing a test result.
func builderPrintTest(b *work.Builder, a *work.Action) error {
	clean := a.Deps[0]
	run := clean.Deps[0]
	if run.TestOutput != nil {
		os.Stdout.Write(run.TestOutput.Bytes())
		run.TestOutput = nil
	}
	return nil
}

// builderNoTest is the action for testing a package with no test files.
func builderNoTest(b *work.Builder, a *work.Action) error {
	var stdout io.Writer = os.Stdout
	if testJSON {
		json := test2json.NewConverter(lockedStdout{}, a.Package.ImportPath, test2json.Timestamp)
		defer json.Close()
		stdout = json
	}
	fmt.Fprintf(stdout, "?   \t%s\t[no test files]\n", a.Package.ImportPath)
	return nil
}

// isTestFunc tells whether fn has the type of a testing function. arg
// specifies the parameter type we look for: B, M or T.
func isTestFunc(fn *ast.FuncDecl, arg string) bool {
	if fn.Type.Results != nil && len(fn.Type.Results.List) > 0 ||
		fn.Type.Params.List == nil ||
		len(fn.Type.Params.List) != 1 ||
		len(fn.Type.Params.List[0].Names) > 1 {
		return false
	}
	ptr, ok := fn.Type.Params.List[0].Type.(*ast.StarExpr)
	if !ok {
		return false
	}
	// We can't easily check that the type is *testing.M
	// because we don't know how testing has been imported,
	// but at least check that it's *M or *something.M.
	// Same applies for B and T.
	if name, ok := ptr.X.(*ast.Ident); ok && name.Name == arg {
		return true
	}
	if sel, ok := ptr.X.(*ast.SelectorExpr); ok && sel.Sel.Name == arg {
		return true
	}
	return false
}

// isTest tells whether name looks like a test (or benchmark, according to prefix).
// It is a Test (say) if there is a character after Test that is not a lower-case letter.
// We don't want TesticularCancer.
func isTest(name, prefix string) bool {
	if !strings.HasPrefix(name, prefix) {
		return false
	}
	if len(name) == len(prefix) { // "Test" is ok
		return true
	}
	rune, _ := utf8.DecodeRuneInString(name[len(prefix):])
	return !unicode.IsLower(rune)
}

type coverInfo struct {
	Package *load.Package
	Vars    map[string]*load.CoverVar
}

// loadTestFuncs returns the testFuncs describing the tests that will be run.
func loadTestFuncs(ptest *load.Package) (*testFuncs, error) {
	t := &testFuncs{
		Package: ptest,
	}
	for _, file := range ptest.TestGoFiles {
		if err := t.load(filepath.Join(ptest.Dir, file), "_test", &t.ImportTest, &t.NeedTest); err != nil {
			return nil, err
		}
	}
	for _, file := range ptest.XTestGoFiles {
		if err := t.load(filepath.Join(ptest.Dir, file), "_xtest", &t.ImportXtest, &t.NeedXtest); err != nil {
			return nil, err
		}
	}
	return t, nil
}

// writeTestmain writes the _testmain.go file for t to the file named out.
func writeTestmain(out string, t *testFuncs) error {
	f, err := os.Create(out)
	if err != nil {
		return err
	}
	defer f.Close()

	if err := testmainTmpl.Execute(f, t); err != nil {
		return err
	}

	return nil
}

type testFuncs struct {
	Tests       []testFunc
	Benchmarks  []testFunc
	Examples    []testFunc
	TestMain    *testFunc
	Package     *load.Package
	ImportTest  bool
	NeedTest    bool
	ImportXtest bool
	NeedXtest   bool
	Cover       []coverInfo
}

func (t *testFuncs) CoverMode() string {
	return testCoverMode
}

func (t *testFuncs) CoverEnabled() bool {
	return testCover
}

// ImportPath returns the import path of the package being tested, if it is within GOPATH.
// This is printed by the testing package when running benchmarks.
func (t *testFuncs) ImportPath() string {
	pkg := t.Package.ImportPath
	if strings.HasPrefix(pkg, "_/") {
		return ""
	}
	if pkg == "command-line-arguments" {
		return ""
	}
	return pkg
}

// Covered returns a string describing which packages are being tested for coverage.
// If the covered package is the same as the tested package, it returns the empty string.
// Otherwise it is a comma-separated human-readable list of packages beginning with
// " in", ready for use in the coverage message.
func (t *testFuncs) Covered() string {
	if testCoverPaths == nil {
		return ""
	}
	return " in " + strings.Join(testCoverPaths, ", ")
}

// Tested returns the name of the package being tested.
func (t *testFuncs) Tested() string {
	return t.Package.Name
}

type testFunc struct {
	Package   string // imported package name (_test or _xtest)
	Name      string // function name
	Output    string // output, for examples
	Unordered bool   // output is allowed to be unordered.
}

var testFileSet = token.NewFileSet()

func (t *testFuncs) load(filename, pkg string, doImport, seen *bool) error {
	f, err := parser.ParseFile(testFileSet, filename, nil, parser.ParseComments)
	if err != nil {
		return base.ExpandScanner(err)
	}
	for _, d := range f.Decls {
		n, ok := d.(*ast.FuncDecl)
		if !ok {
			continue
		}
		if n.Recv != nil {
			continue
		}
		name := n.Name.String()
		switch {
		case name == "TestMain":
			if isTestFunc(n, "T") {
				t.Tests = append(t.Tests, testFunc{pkg, name, "", false})
				*doImport, *seen = true, true
				continue
			}
			err := checkTestFunc(n, "M")
			if err != nil {
				return err
			}
			if t.TestMain != nil {
				return errors.New("multiple definitions of TestMain")
			}
			t.TestMain = &testFunc{pkg, name, "", false}
			*doImport, *seen = true, true
		case isTest(name, "Test"):
			err := checkTestFunc(n, "T")
			if err != nil {
				return err
			}
			t.Tests = append(t.Tests, testFunc{pkg, name, "", false})
			*doImport, *seen = true, true
		case isTest(name, "Benchmark"):
			err := checkTestFunc(n, "B")
			if err != nil {
				return err
			}
			t.Benchmarks = append(t.Benchmarks, testFunc{pkg, name, "", false})
			*doImport, *seen = true, true
		}
	}
	ex := doc.Examples(f)
	sort.Slice(ex, func(i, j int) bool { return ex[i].Order < ex[j].Order })
	for _, e := range ex {
		*doImport = true // import test file whether executed or not
		if e.Output == "" && !e.EmptyOutput {
			// Don't run examples with no output.
			continue
		}
		t.Examples = append(t.Examples, testFunc{pkg, "Example" + e.Name, e.Output, e.Unordered})
		*seen = true
	}
	return nil
}

func checkTestFunc(fn *ast.FuncDecl, arg string) error {
	if !isTestFunc(fn, arg) {
		name := fn.Name.String()
		pos := testFileSet.Position(fn.Pos())
		return fmt.Errorf("%s: wrong signature for %s, must be: func %s(%s *testing.%s)", pos, name, name, strings.ToLower(arg), arg)
	}
	return nil
}

var testmainTmpl = template.Must(template.New("main").Parse(`
package main

import (
{{if not .TestMain}}
	"os"
{{end}}
	"testing"
	"testing/internal/testdeps"

{{if .ImportTest}}
	{{if .NeedTest}}_test{{else}}_{{end}} {{.Package.ImportPath | printf "%q"}}
{{end}}
{{if .ImportXtest}}
	{{if .NeedXtest}}_xtest{{else}}_{{end}} {{.Package.ImportPath | printf "%s_test" | printf "%q"}}
{{end}}
{{range $i, $p := .Cover}}
	_cover{{$i}} {{$p.Package.ImportPath | printf "%q"}}
{{end}}
)

var tests = []testing.InternalTest{
{{range .Tests}}
	{"{{.Name}}", {{.Package}}.{{.Name}}},
{{end}}
}

var benchmarks = []testing.InternalBenchmark{
{{range .Benchmarks}}
	{"{{.Name}}", {{.Package}}.{{.Name}}},
{{end}}
}

var examples = []testing.InternalExample{
{{range .Examples}}
	{"{{.Name}}", {{.Package}}.{{.Name}}, {{.Output | printf "%q"}}, {{.Unordered}}},
{{end}}
}

func init() {
	testdeps.ImportPath = {{.ImportPath | printf "%q"}}
}

{{if .CoverEnabled}}

// Only updated by init functions, so no need for atomicity.
var (
	coverCounters = make(map[string][]uint32)
	coverBlocks = make(map[string][]testing.CoverBlock)
)

func init() {
	{{range $i, $p := .Cover}}
	{{range $file, $cover := $p.Vars}}
	coverRegisterFile({{printf "%q" $cover.File}}, _cover{{$i}}.{{$cover.Var}}.Count[:], _cover{{$i}}.{{$cover.Var}}.Pos[:], _cover{{$i}}.{{$cover.Var}}.NumStmt[:])
	{{end}}
	{{end}}
}

func coverRegisterFile(fileName string, counter []uint32, pos []uint32, numStmts []uint16) {
	if 3*len(counter) != len(pos) || len(counter) != len(numStmts) {
		panic("coverage: mismatched sizes")
	}
	if coverCounters[fileName] != nil {
		// Already registered.
		return
	}
	coverCounters[fileName] = counter
	block := make([]testing.CoverBlock, len(counter))
	for i := range counter {
		block[i] = testing.CoverBlock{
			Line0: pos[3*i+0],
			Col0: uint16(pos[3*i+2]),
			Line1: pos[3*i+1],
			Col1: uint16(pos[3*i+2]>>16),
			Stmts: numStmts[i],
		}
	}
	coverBlocks[fileName] = block
}
{{end}}

func main() {
{{if .CoverEnabled}}
	testing.RegisterCover(testing.Cover{
		Mode: {{printf "%q" .CoverMode}},
		Counters: coverCounters,
		Blocks: coverBlocks,
		CoveredPackages: {{printf "%q" .Covered}},
	})
{{end}}
	m := testing.MainStart(testdeps.TestDeps{}, tests, benchmarks, examples)
{{with .TestMain}}
	{{.Package}}.{{.Name}}(m)
{{else}}
	os.Exit(m.Run())
{{end}}
}

`))
