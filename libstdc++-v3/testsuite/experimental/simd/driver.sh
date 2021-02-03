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
  abiflag=""
elif [ $abi -gt 0 -a $abi -lt 10 ]; then
  abiflag="-DEXTENDEDTESTS=$((abi-1))"
else
  echo "Error: The -a argument must be a value between 0 and 9 (inclusive)." >&2
  exit 1
fi

fail() {
  echo "FAIL: $src $type $abiflag ($*)" | tee -a "$sum" "$log"
}

xpass() {
  echo "XPASS: $src $type $abiflag ($*)" | tee -a "$sum" "$log"
}

xfail() {
  $quiet || echo "XFAIL: $src $type $abiflag ($*)"
  echo "XFAIL: $src $type $abiflag ($*)" >> "$sum"
  echo "XFAIL: $src $type $abiflag ($*)" >> "$log"
}

pass() {
  $quiet || echo "PASS: $src $type $abiflag ($*)"
  echo "PASS: $src $type $abiflag ($*)" >> "$sum"
  echo "PASS: $src $type $abiflag ($*)" >> "$log"
}

unsupported() {
  $quiet || echo "UNSUPPORTED: $src $type $abiflag ($*)"
  echo "UNSUPPORTED: $src $type $abiflag ($*)" >> "$sum"
  echo "UNSUPPORTED: $src $type $abiflag ($*)" >> "$log"
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
    elif [ "$xfail" = "compile" ]; then
      xpass "test for excess errors"
    else
      pass "test for excess errors"
    fi
  else
    if [ $failed -eq 124 ]; then
      fail "timeout: test for excess errors"
    else
      errors=$(grep -ic 'error:' "$log")
      if [ "$xfail" = "compile" ]; then
        xfail "excess errors:" $errors
        exit 0
      else
        fail "excess errors:" $errors
      fi
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
    if [ "$xfail" = "run" ]; then
      xpass "execution test"
    else
      pass "execution test"
    fi
  else
    $keep_failed || rm "$exe"
    if [ $failed -eq 124 ]; then
      fail "timeout: execution test"
    elif [ "$xfail" = "run" ]; then
      xfail "execution test"
      exit 0
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

matches() {
  eval "case '$1' in
    $2) return 0;; esac"
  return 1
}

test_selector() {
  string="$1"
  pat_type="${string%% *}"
  if matches "$shorttype" "$pat_type"; then
    string="${string#* }"
    pat_abi="${string%% *}"
    if matches "$abi" "$pat_abi"; then
      string="${string#* }"
      pat_triplet="${string%% *}"
      [ -z "$target_triplet" ] && target_triplet=$($CXX -dumpmachine)
      if matches "$target_triplet" "$pat_triplet"; then
        pat_flags="${string#* }"
        if matches "$CXXFLAGS" "$pat_flags"; then
          return 0
        fi
      fi
    fi
  fi
  return 1
}

rm -f "$log" "$sum"
touch "$log" "$sum"

skip="$(head -n25 "$src" | grep '^//\s*skip: ')"
if [ -n "$skip" ]; then
  skip="$(echo "$skip" | sed -e 's/^.*:\s*//' -e 's/ \+/ /g')"
  if test_selector "$skip"; then
    # silently skip this test
    exit 0
  fi
fi
only="$(head -n25 "$src" | grep '^//\s*only: ')"
if [ -n "$only" ]; then
  only="$(echo "$only" | sed -e 's/^.*:\s*//' -e 's/ \+/ /g')"
  if ! test_selector "$only"; then
    # silently skip this test
    exit 0
  fi
fi
if ! $run_expensive; then
  expensive="$(head -n25 "$src" | grep '^//\s*expensive: ')"
  if [ -n "$expensive" ]; then
    expensive="$(echo "$expensive" | sed -e 's/^.*:\s*//' -e 's/ \+/ /g')"
    if test_selector "$expensive"; then
      unsupported "skip expensive tests"
      exit 0
    fi
  fi
fi
xfail="$(head -n25 "$src" | grep '^//\s*xfail: ')"
if [ -n "$xfail" ]; then
  xfail="$(echo "$xfail" | sed -e 's/^.*:\s*//' -e 's/ \+/ /g')"
  if test_selector "${xfail#* }"; then
    xfail="${xfail%% *}"
  else
    unset xfail
  fi
fi

write_log_and_verbose "$CXX $src $@ -D_GLIBCXX_SIMD_TESTTYPE=$type $abiflag -o $exe"
timeout $timeout "$CXX" "$src" "$@" "-D_GLIBCXX_SIMD_TESTTYPE=$type" $abiflag -o "$exe" >> "$log" 2>&1
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
