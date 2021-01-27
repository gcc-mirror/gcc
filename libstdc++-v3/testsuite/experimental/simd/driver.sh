#!/bin/sh

type=float
abi=0
name=
srcdir="$(cd "${0%/*}" && pwd)/tests"
sim="$GCC_TEST_SIMULATOR"
quiet=false
verbose=false
timeout=180
run_expensive=false
if [ -n "$GCC_TEST_RUN_EXPENSIVE" ]; then
  run_expensive=true
fi
keep_failed=false
only=

usage() {
  cat <<EOF
Usage: $0 [Options] <g++ invocation>

Options:
  -h, --help          Print this message and exit.
  -q, --quiet         Only print failures.
  -v, --verbose       Print compiler and test output on failure.
  -t <type>, --type <type>
                      The value_type to test (default: $type).
  -a [0-9], --abi [0-9]
                      The ABI tag subset to test (default: $abi).
  -n <name>, --name <name>
                      The name of the test (required).
  -k, --keep-failed   Keep executables of failed tests.
  --srcdir <path>     The source directory of the tests (default: $srcdir).
  --sim <executable>  Path to an executable that is prepended to the test
                      execution binary (default: the value of
                      GCC_TEST_SIMULATOR).
  --timeout-factor <x>
                      Multiply the default timeout with x.
  --run-expensive     Compile and run tests marked as expensive (default:
                      true if GCC_TEST_RUN_EXPENSIVE is set, false otherwise).
  --only <pattern>    Compile and run only tests matching the given pattern.
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
  -h|--help)
    usage
    exit
    ;;
  -q|--quiet)
    quiet=true
    ;;
  -v|--verbose)
    verbose=true
    ;;
  --run-expensive)
    run_expensive=true
    ;;
  -k|--keep-failed)
    keep_failed=true
    ;;
  --only)
    only="$2"
    shift
    ;;
  --only=*)
    only="${1#--only=}"
    ;;
  -t|--type)
    type="$2"
    shift
    ;;
  --type=*)
    type="${1#--type=}"
    ;;
  -a|--abi)
    abi="$2"
    shift
    ;;
  --abi=*)
    abi="${1#--abi=}"
    ;;
  -n|--name)
    name="$2"
    shift
    ;;
  --name=*)
    name="${1#--name=}"
    ;;
  --srcdir)
    srcdir="$2"
    shift
    ;;
  --srcdir=*)
    srcdir="${1#--srcdir=}"
    ;;
  --sim)
    sim="$2"
    shift
    ;;
  --sim=*)
    sim="${1#--sim=}"
    ;;
  --timeout-factor)
    timeout=$(awk "BEGIN { print int($timeout * $2) }")
    shift
    ;;
  --timeout-factor=*)
    x=${1#--timeout-factor=}
    timeout=$(awk "BEGIN { print int($timeout * $x) }")
    ;;
  --)
    shift
    break
    ;;
  *)
    break
    ;;
  esac
  shift
done

CXX="$1"
shift
CXXFLAGS="$@"
src="${srcdir}/${name}.cc"
shorttype=$(echo $type|sed -e 's/long /l/' -e 's/unsigned /u/' -e 's/signed /s/')
testname="${name}-${shorttype}-${abi}"
exe="${testname}.exe"
log="${testname}.log"
sum="${testname}.sum"
if [ -n "$only" ]; then
  if echo "$testname"|awk "{ exit /$only/ }"; then
    touch "$log" "$sum"
    exit 0
  fi
fi

if [ $abi -eq 0 ]; then
  abi=""
elif [ $abi -gt 0 -a $abi -lt 10 ]; then
  abi="-DEXTENDEDTESTS=$((abi-1))"
else
  echo "Error: The -a argument must be a value between 0 and 9 (inclusive)." >&2
  exit 1
fi

fail() {
  echo "FAIL: $src $type $abi ($*)" | tee -a "$sum" "$log"
}

pass() {
  $quiet || echo "PASS: $src $type $abi ($*)"
  echo "PASS: $src $type $abi ($*)" >> "$sum"
  echo "PASS: $src $type $abi ($*)" >> "$log"
}

unsupported() {
  $quiet || echo "UNSUPPORTED: $src $type $abi ($*)"
  echo "UNSUPPORTED: $src $type $abi ($*)" >> "$sum"
  echo "UNSUPPORTED: $src $type $abi ($*)" >> "$log"
}

verify_compilation() {
  failed=$1
  if [ $failed -eq 0 ]; then
    warnings=$(grep -ic 'warning:' "$log")
    if [ $warnings -gt 0 ]; then
      fail "excess warnings:" $warnings
      if $verbose; then
        cat "$log"
      elif ! $quiet; then
        grep -i 'warning:' "$log" | head -n5
      fi
    else
      pass "test for excess errors"
    fi
  else
    if [ $failed -eq 124 ]; then
      fail "timeout: test for excess errors"
    else
      errors=$(grep -ic 'error:' "$log")
      fail "excess errors:" $errors
    fi
    if $verbose; then
      cat "$log"
    elif ! $quiet; then
      grep -i 'error:' "$log" | head -n5
    fi
    exit 0
  fi
}

verify_test() {
  failed=$1
  if [ $failed -eq 0 ]; then
    rm "$exe"
    pass "execution test"
  else
    $keep_failed || rm "$exe"
    if [ $failed -eq 124 ]; then
      fail "timeout: execution test"
    else
      fail "execution test"
    fi
    if $verbose; then
      if [ $(cat "$log"|wc -l) -gt 1000 ]; then
        echo "[...]"
        tail -n1000 "$log"
      else
        cat "$log"
      fi
    elif ! $quiet; then
      grep -i fail "$log" | head -n5
    fi
    exit 0
  fi
}

write_log_and_verbose() {
  echo "$*" >> "$log"
  if $verbose; then
    echo "$*"
  fi
}

rm -f "$log" "$sum"
touch "$log" "$sum"

if ! $run_expensive && [ -n "$abi" ]; then
  unsupported "skip expensive tests"
  exit 0
fi

write_log_and_verbose "$CXX $src $@ -D_GLIBCXX_SIMD_TESTTYPE=$type $abi -o $exe"
timeout $timeout "$CXX" "$src" "$@" "-D_GLIBCXX_SIMD_TESTTYPE=$type" $abi -o "$exe" >> "$log" 2>&1
verify_compilation $?
if [ -n "$sim" ]; then
  write_log_and_verbose "$sim ./$exe"
  timeout $timeout $sim "./$exe" >> "$log" 2>&1 <&-
else
  write_log_and_verbose "./$exe"
  timeout=$(awk "BEGIN { print int($timeout / 2) }")
  timeout $timeout "./$exe" >> "$log" 2>&1 <&-
fi
verify_test $?

# vim: sw=2 et cc=81 si
