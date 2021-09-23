#!/bin/sh

type=float
abi=0
name=
srcdir="$(cd "${0%/*}" && pwd)/tests"
sim="$GCC_TEST_SIMULATOR"

# output_mode values:
# print only failures with minimal context
readonly really_quiet=0
# as above plus same-line output of last successful test
readonly same_line=1
# as above plus percentage
readonly percentage=2
# print one line per finished test with minimal context on failure
readonly verbose=3
# print one line per finished test with full output of the compiler and test
readonly really_verbose=4

output_mode=$really_quiet
[ -t 1 ] && output_mode=$same_line

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
  -q, --quiet         Disable same-line progress output (default if stdout is
                      not a tty).
  -p, --percentage    Add percentage to default same-line progress output.
  -v, --verbose       Print one line per test and minimal extra information on
                      failure.
  -vv                 Print all compiler and test output.
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
  -x, --run-expensive Compile and run tests marked as expensive (default:
                      true if GCC_TEST_RUN_EXPENSIVE is set, false otherwise).
  -o <pattern>, --only <pattern>
                      Compile and run only tests matching the given pattern.
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
  -h|--help)
    usage
    exit
    ;;
  -q|--quiet)
    output_mode=$really_quiet
    ;;
  -p|--percentage)
    output_mode=$percentage
    ;;
  -v|--verbose)
    if [ $output_mode -lt $verbose ]; then
      output_mode=$verbose
    else
      output_mode=$really_verbose
    fi
    ;;
  -x|--run-expensive)
    run_expensive=true
    ;;
  -k|--keep-failed)
    keep_failed=true
    ;;
  -o|--only)
    only="$2"
    shift
    ;;
  -t|--type)
    type="$2"
    shift
    ;;
  -a|--abi)
    abi="$2"
    shift
    ;;
  -n|--name)
    name="$2"
    shift
    ;;
  --srcdir)
    srcdir="$2"
    shift
    ;;
  --sim)
    sim="$2"
    shift
    ;;
  --timeout-factor)
    timeout=$(awk "BEGIN { print int($timeout * $2) }")
    shift
    ;;
  --)
    shift
    break
    ;;
  --*=*)
    opt="$1"
    shift
    value=${opt#*=}
    set -- ${opt%=$value} "$value" ${1+"$@"}
    continue
    ;;
  -[ahknopqtvx][ahknopqtvx]*)
    opt="$1"
    shift
    next=${opt#??}
    set -- ${opt%$next} "-$next" ${1+"$@"}
    continue
    ;;
  -*)
    echo "Error: Unrecognized option '$1'" >&2
    exit 1
    ;;
  *)
    break
    ;;
  esac
  shift
done

if [ $output_mode = $percentage ]; then
  inc_progress() {
    {
      flock -n 9
      n=$(($(cat .progress) + 1))
      echo $n >&9
      echo $n
    } 9<>.progress
  }
fi

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
    [ $output_mode = $percentage ] && inc_progress >/dev/null
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

if [ $output_mode = $percentage ]; then
  show_progress() {
    n=$(inc_progress)
    read total < .progress_total
    total=${total}0
    printf "\e[1G\e[K[%3d %%] ${src##*/} $type $abiflag" \
      $((n * 1005 / total))
  }
  trap 'show_progress' EXIT
  prefix="\e[1G\e[K"
elif [ $output_mode = $same_line ]; then
  show_progress() {
    printf "\e[1G\e[K${src##*/} $type $abiflag"
  }
  trap 'show_progress' EXIT
  prefix="\e[1G\e[K"
else
  prefix=""
fi

fail() {
  printf "$prefix"
  echo "FAIL: $src $type $abiflag ($*)" | tee -a "$sum" "$log"
}

xpass() {
  printf "$prefix"
  echo "XPASS: $src $type $abiflag ($*)" | tee -a "$sum" "$log"
}

xfail() {
  [ $output_mode -ge $verbose ] && echo "XFAIL: $src $type $abiflag ($*)"
  echo "XFAIL: $src $type $abiflag ($*)" >> "$sum"
  echo "XFAIL: $src $type $abiflag ($*)" >> "$log"
}

pass() {
  [ $output_mode -ge $verbose ] && echo "PASS: $src $type $abiflag ($*)"
  echo "PASS: $src $type $abiflag ($*)" >> "$sum"
  echo "PASS: $src $type $abiflag ($*)" >> "$log"
}

unsupported() {
  test
  [ $output_mode -ge $verbose ] && echo "UNSUPPORTED: $src $type $abiflag ($*)"
  echo "UNSUPPORTED: $src $type $abiflag ($*)" >> "$sum"
  echo "UNSUPPORTED: $src $type $abiflag ($*)" >> "$log"
}

write_log_and_verbose() {
  echo "$*" >> "$log"
  if [ $output_mode = $really_verbose ]; then
    if [ -z "$COLUMNS" ] || ! type fmt>/dev/null; then
      echo "$*"
    else
      echo "$*" | fmt -w $COLUMNS -s - || cat
    fi
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
        if matches "$CXXFLAGS" "*$pat_flags*"; then
          return 0
        fi
      fi
    fi
  fi
  return 1
}

trap "rm -f '$log' '$sum' $exe; exit" INT
rm -f "$log" "$sum"
touch "$log" "$sum"

read_src_option() {
  local key tmp var
  key="$1"
  var="$2"
  [ -z "$var" ] && var="$1"
  local tmp="$(head -n25 "$src" | grep "^//\\s*${key}: ")"
  if [ -n "$tmp" ]; then
    tmp="$(echo "${tmp#//*${key}: }" | sed -e 's/ \+/ /g' -e 's/^ //' -e 's/$//')"
    eval "$var=\"$tmp\""
  else
    return 1
  fi
}

if read_src_option skip; then
  if test_selector "$skip"; then
    # silently skip this test
    exit 0
  fi
fi
if read_src_option only; then
  if ! test_selector "$only"; then
    # silently skip this test
    exit 0
  fi
fi

if ! $run_expensive; then
  if read_src_option expensive; then
    if test_selector "$expensive"; then
      unsupported "skip expensive tests"
      exit 0
    fi
  fi
fi

if read_src_option xfail; then
  if test_selector "${xfail#* }"; then
    xfail="${xfail%% *}"
  else
    unset xfail
  fi
fi

read_src_option timeout

if read_src_option timeout-factor factor; then
  timeout=$(awk "BEGIN { print int($timeout * $factor) }")
fi

log_output() {
  if [ $output_mode = $really_verbose ]; then
    maxcol=${1:-1024}
    awk "
BEGIN { count = 0 }
/^###exitstatus### [0-9]+$/ { exit \$2 }
{
  print >> \"$log\"
  if (count >= 1000) {
    print \"Aborting: too much output\" >> \"$log\"
    print \"Aborting: too much output\"
    exit 125
  }
  ++count
  if (length(\$0) > $maxcol) {
    i = 1
    while (i + $maxcol <= length(\$0)) {
      len = $maxcol
      line = substr(\$0, i, len)
      len = match(line, / [^ ]*$/)
      if (len <= 0) {
        len = match(substr(\$0, i), / [^ ]/)
        if (len <= 0) len = $maxcol
      }
      print substr(\$0, i, len)
      i += len
    }
    print substr(\$0, i)
  } else {
    print
  }
}
END { close(\"$log\") }
"
  else
    awk "
BEGIN { count = 0 }
/^###exitstatus### [0-9]+$/ { exit \$2 }
{
  print >> \"$log\"
  if (count >= 1000) {
    print \"Aborting: too much output\" >> \"$log\"
    print \"Aborting: too much output\"
    exit 125
  }
  ++count
}
END { close(\"$log\") }
"
  fi
}

verify_compilation() {
  log_output $COLUMNS
  exitstatus=$?
  if [ $exitstatus -eq 0 ]; then
    warnings=$(grep -ic 'warning:' "$log")
    if [ $warnings -gt 0 ]; then
      fail "excess warnings:" $warnings
      if [ $output_mode = $verbose ]; then
        grep -i 'warning:' "$log" | head -n5
      fi
    elif [ "$xfail" = "compile" ]; then
      xpass "test for excess errors"
    else
      pass "test for excess errors"
    fi
    return 0
  else
    if [ $exitstatus -eq 124 ]; then
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
    if [ $output_mode = $verbose ]; then
      grep -i 'error:' "$log" | head -n5
    fi
    return 1
  fi
}

verify_test() {
  log_output $COLUMNS
  exitstatus=$?
  if [ $exitstatus -eq 0 ]; then
    if [ "$xfail" = "run" ]; then
      $keep_failed || rm "$exe"
      xpass "execution test"
    else
      rm "$exe"
      pass "execution test"
    fi
    return 0
  else
    $keep_failed || rm "$exe"
    if [ $output_mode = $verbose ]; then
      grep -i fail "$log" | head -n5
    fi
    if [ $exitstatus -eq 124 ]; then
      fail "timeout: execution test"
    elif [ "$xfail" = "run" ]; then
      xfail "execution test"
    else
      fail "execution test"
    fi
    return 1
  fi
}

write_log_and_verbose "$CXX $src $@ -D_GLIBCXX_SIMD_TESTTYPE=$type $abiflag -o $exe"
{
  timeout --foreground $timeout "$CXX" "$src" "$@" "-D_GLIBCXX_SIMD_TESTTYPE=$type" $abiflag -o "$exe" 2>&1 <&-
  printf "###exitstatus### %d\n" $?
} | verify_compilation || exit 0
if [ -n "$sim" ]; then
  write_log_and_verbose "$sim ./$exe"
else
  write_log_and_verbose "./$exe"
  timeout=$(awk "BEGIN { print int($timeout / 2) }")
fi
{
  timeout --foreground $timeout $sim "./$exe" 2>&1 <&-
  printf "###exitstatus### %d\n" $?
} | verify_test || exit 0

# vim: sw=2 et cc=81 si
