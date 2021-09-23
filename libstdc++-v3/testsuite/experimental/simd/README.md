# SIMD Tests

To execute the simd testsuite, call `make check-simd`, typically with `-j N` 
argument.

For more control over verbosity, compiler flags, and use of a simulator, use 
the environment variables documented below.

## Environment variables

### `target_list`

Similar to dejagnu target lists: E.g. 
`target_list="unix{-march=sandybridge,-march=native/-ffast-math,-march=native/-ffinite-math-only}"` 
would create three subdirs in `testsuite/simd/` to run the complete simd 
testsuite first with `-march=sandybridge`, then with `-march=native 
-ffast-math`, and finally with `-march=native -ffinite-math-only`.


### `CHECK_SIMD_CONFIG`

This variable can be set to a path to a file which is equivalent to a dejagnu 
board. The file needs to be a valid `sh` script since it is sourced from the 
`scripts/check_simd` script. Its purpose is to set the `target_list` variable 
depending on `$target_triplet` (or whatever else makes sense for you). Example:

```sh
case "$target_triplet" in
x86_64-*)
  target_list="unix{-march=sandybridge,-march=skylake-avx512,-march=native/-ffast-math,-march=athlon64,-march=core2,-march=nehalem,-march=skylake,-march=native/-ffinite-math-only,-march=knl}"
  ;;

powerpc64le-*)
  define_target power7 "-mcpu=power7 -static" "$HOME/bin/run_on_gccfarm gcc112"
  define_target power8 "-mcpu=power8 -static" "$HOME/bin/run_on_gccfarm gcc112"
  define_target power9 "-mcpu=power9 -static" "$HOME/bin/run_on_gccfarm gcc135"
  target_list="power7 power8 power9{,-ffast-math}"
  ;;

powerpc64-*)
  define_target power7 "-mcpu=power7 -static" "$HOME/bin/run_on_gccfarm gcc110"
  define_target power8 "-mcpu=power8 -static" "$HOME/bin/run_on_gccfarm gcc110"
  target_list="power7 power8{,-ffast-math}"
  ;;
esac
```

The `unix` target is pre-defined to have no initial flags and no simulator. Use 
the `define_target(name, flags, sim)` function to define your own targets for 
the `target_list` variable. In the example above `define_target power7 
"-mcpu=power7 -static" "$HOME/bin/run_on_gccfarm gcc112"` defines the target 
`power7` which always uses the flags `-mcpu=power7` and `-static` when 
compiling tests and prepends `$HOME/bin/run_on_gccfarm gcc112` to test 
executables. In `target_list` you can now use the name `power7`. E.g. 
`target_list="power7 power7/-ffast-math"` or its shorthand 
`target_list="power7{,-ffast-math}"`.


### `DRIVEROPTS`

This variable affects the `Makefile`s generated per target (as defined above). 
It's a string of flags that are prepended to the `driver.sh` invocation which 
builds and runs the tests. You `cd` into a simd test subdir and use `make help` 
to see possible options and a list of all valid targets.

```
use DRIVEROPTS=<options> to pass the following options:
-q, --quiet         Disable same-line progress output (default if stdout is
                    not a tty).
-p, --percentage    Add percentage to default same-line progress output.
-v, --verbose       Print one line per test and minimal extra information on
                    failure.
-vv                 Print all compiler and test output.
-k, --keep-failed   Keep executables of failed tests.
--sim <executable>  Path to an executable that is prepended to the test
                    execution binary (default: the value of
                    GCC_TEST_SIMULATOR).
--timeout-factor <x>
                    Multiply the default timeout with x.
-x, --run-expensive Compile and run tests marked as expensive (default:
                    true if GCC_TEST_RUN_EXPENSIVE is set, false otherwise).
-o <pattern>, --only <pattern>
                    Compile and run only tests matching the given pattern.
```


### `TESTFLAGS`

This variable also affects the `Makefile`s generated per target. It's a list of 
compiler flags that are appended to `CXXFLAGS`.


### `GCC_TEST_SIMULATOR`

If `--sim` is not passed via `DRIVEROPTS`, then this variable is prepended to 
test invocations. If a simulator was defined via the `CHECK_SIMD_CONFIG` 
script, then then generated `Makefile` sets the `GCC_TEST_SIMULATOR` variable.


### `GCC_TEST_RUN_EXPENSIVE`

If set to any non-empty string, run tests marked as expensive, otherwise treat 
these tests as `UNSUPPORTED`.


## Writing new tests

A test starts with the copyright header, directly followed by directives 
influencing the set of tests to generate and whether the test driver should 
expect a failure.

Then the test must at least `#include "bits/verify.h"`, which provides `main` 
and declares a `template <typename V> void test()` function, which the test has 
to define. The template parameter is set to `simd<T, Abi>` type where `T` and 
`Abi` are determined by the type and ABI subset dimensions.

The `test()` functions are typically implemented using the `COMPARE(x, 
reference)`, `VERIFY(boolean)`, and `ULP_COMPARE(x, reference, 
allowed_distance)` macros.

### Directives

* `// skip: <type pattern> <ABI subset pattern> <target triplet pattern> 
  <CXXFLAGS pattern>`
  If all patterns match, the test is silently skipped.

* `// only: <type pattern> <ABI subset pattern> <target triplet pattern> 
  <CXXFLAGS pattern>`
  If any pattern doesn't match, the test is silently skipped.

* `// expensive: <type pattern> <ABI subset pattern> <target triplet pattern>
  <CXXFLAGS pattern>`
  If all patterns match, the test is `UNSUPPORTED` unless expensive tests are 
  enabled.

* `// xfail: run|compile <type pattern> <ABI subset pattern> <target triplet 
  pattern> <CXXFLAGS pattern>`
  If all patterns match, test compilation or execution is expected to fail. The 
  test then shows as "XFAIL: ...". If the test passes, the test shows "XPASS: 
  ...".

All patterns are matched via
```sh
case '<test context>' in
  <pattern>)
  # treat as match
  ;;
esac
```
The `<CXXFLAGS pattern>` is implicitly adds a `*` wildcard before and after the 
pattern. Thus, the `CXXFLAGS` pattern matches a substring and all other 
patterns require a full match.

Examples:
```cpp
// The test is only valid for floating-point types:
// only: float|double|ldouble * * *

// Skip the test for long double for all powerpc64* targets:
// skip: ldouble * powerpc64* *

// The test is expected to unconditionally fail on execution:
// xfail: run * * * *

// ABI subsets 1-9 are considered expensive:
// expensive: * [1-9] * *
```


## Implementation sketch

* `scripts/create_testsuite_files` collects all `*.c` and `*.cc` files with 
  `simd/tests/` in their path into the file `testsuite_file_simd` (and at the 
  same time removes them from `testsuite_files`.

* The `check-simd` target in `testsuite/Makefile.am` calls 
  `scripts/check_simd`. This script calls 
  `testsuite/experimental/simd/generate_makefile.sh` to generate `Makefile`s in 
  all requested subdirectories. The subdirectories are communicated back to the 
  make target via a `stdout` pipe. The `check-simd` rule then spawns sub-make 
  in these subdirectories. Finally it collects all summaries 
  (`simd_testsuite.sum`) to present them at the end of the rule.

* The generated Makefiles define targets for each file in `testsuite_file_simd` 
  (you can edit this file after it was generated, though that's not 
  recommended) while adding two test dimensions: type and ABI subset. The type 
  is a list of all arithmetic types, potentially reduced via `only` and/or 
  `skip` directives in the test's source file. The ABI subset is a number 
  between 0 and 9 (inclusive) mapping to a set of `simd_abi`s in
  `testsuite/experimental/simd/tests/bits/verify.h` (`iterate_abis()`). The 
  tests are thus potentially compiled 170 (17 arithmetic types * 10 ABI 
  subsets) times. This is necessary to limit the memory usage of GCC to 
  reasonable numbers and keep the compile time below 1 minute (per compiler 
  invocation).

* When `make` executes in the generated subdir, the `all` target depends on 
  building and running all tests via `testsuite/experimental/simd/driver.sh` 
  and collecting their logs into a `simd_testsuite.log` and then extracting 
  `simd_testsuite.sum` from it.

* The `driver.sh` script builds and runs the test, parses the compiler and test 
  output, and prints progress information to the terminal.

## Appendix

### `run_on_gccfarm` script

```sh
#!/bin/sh
usage() {
  cat <<EOF
Usage $0 <hostname> <executable> [arguments]

Copies <executable> to $host, executes it and cleans up again.
EOF
}

[ $# -lt 2 ] && usage && exit 1
case "$1" in
  -h|--help)
    usage
    exit
    ;;
esac

host="$1"
exe="$2"
shift 2

# Copy executable locally to strip it before scp to remote host
local_tmpdir=$(mktemp -d)
cp "$exe" $local_tmpdir
cd $local_tmpdir
exe="${exe##*/}"
powerpc64le-linux-gnu-strip "$exe"

ssh_controlpath=~/.local/run_on_gccfarm/$host
if [ ! -S $ssh_controlpath ]; then
  mkdir -p ~/.local/run_on_gccfarm
  (
    flock -n 9
    if [ ! -S $ssh_controlpath ]; then
      ssh -o ControlMaster=yes -o ControlPath=$ssh_controlpath -o ControlPersist=10m $host.fsffrance.org true
    fi
  ) 9> ~/.local/run_on_gccfarm/lockfile
fi
opts="-o ControlPath=$ssh_controlpath"

remote_tmpdir=$(ssh $opts $host.fsffrance.org mktemp -d -p .)
scp $opts -C -q "$exe" $host.fsffrance.org:$remote_tmpdir/
cd
rm -r "$local_tmpdir" &
ssh $opts $host.fsffrance.org $remote_tmpdir/$exe "$@"
ret=$?
ssh $opts $host.fsffrance.org rm -r $remote_tmpdir &
exit $ret
```
