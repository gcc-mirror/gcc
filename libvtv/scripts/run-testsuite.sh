#!/usr/bin/env bash

# Script to do testing.

# Invocation
# run-testsuite SRC_DIR BUILD_DIR

# Pass in build/src directory as parameters.
SRC_DIR=$1
BUILD_DIR=$2

# Now that we've successfully translated the numerical option into
# a symbolic one, we can safely ignore it.
shift

# Use build compiler/library flags from libstdc++
flags_script=$BUILD_DIR/../libstdc++-v3/scripts/testsuite_flags
INCLUDES=`$flags_script --build-includes`
COMPILER=`$flags_script --build-cxx`
CXX="$COMPILER $INCLUDES -L$BUILD_DIR/.libs -Wl,--rpath -Wl,$BUILD_DIR/.libs"

echo "compiler config is:"
echo $CXX
echo ""

# Other constants.
LOPT_LEVELS=${OPT_LEVELS:-"-O0 -O2"}
#LDATA_MODELS=${DATA_MODELS:-"32 64"}
LDATA_MODELS=${DATA_MODELS:-"64"}

# Check if value of LGCC_LIB_PATH/$1 exists. If it does, save this path.
# If it doesn't, use LGCC_LIB_PATH as the library path.
# This allows us to check for paths that are of the form <x>/lib32 or <x>/lib64.
get_lib_path()
{
  if [[ -e $LGCC_LIB_PATH$1 ]]; then
    LLGCC_LIB_PATH=$LGCC_LIB_PATH$1
  else
    LLGCC_LIB_PATH=$LGCC_LIB_PATH
  fi
  return
}

LGCC_SRC=$SRC_DIR/testsuite

TESTS="const_vtable.cc dataentry.cc dup_name.cc environment.cc template-list.cc template-list2.cc template-list-iostream.cc povray-derived.cc thunk.cc thunk_vtable_map_attack.cc virtual_inheritance.cc "

for M in $LDATA_MODELS; do
  get_lib_path lib${M}
  for T in $TESTS; do
    TSRC=${LGCC_SRC}/${T}
    for OL in $LOPT_LEVELS; do
      CMD="${CXX} -m${M} -fvtable-verify=std -fpic -rdynamic -Wl,-z,relro ${TSRC} ${OL}"
      echo $CMD
      ($CMD  && ( ./a.out > $T.$OL.out 2>&1 ) && echo "PASS $T $OL") || echo "FAIL $T $OL"
    done
  done
done

TESTS_COMPOUND_parts="${LGCC_SRC}/parts-test-main.cc ${LGCC_SRC}/parts-test-extra-parts.cc ${LGCC_SRC}/parts-test-extra-parts-views.cc"
TESTS_COMPOUND_events="${LGCC_SRC}/event-main.cc ${LGCC_SRC}/event-definitions.cc ${LGCC_SRC}/event-private.cc"

for M in $LDATA_MODELS; do
  get_lib_path lib${M}
  for OL in $LOPT_LEVELS; do
      CMD="${CXX} -m${M} -fvtable-verify=std -fpic -rdynamic -Wl,-z,relro ${TESTS_COMPOUND_parts} ${OL}"
      echo $CMD
      ($CMD  && ( ./a.out > $T.$OL.out 2>&1 ) && echo "PASS $T $OL") || echo "FAIL $T $OL"
   done
done

for M in $LDATA_MODELS; do
  for (( TN = 0 ; TN < 100 ; TN++ )); do
    SO_NAME=so$TN.so
    if [ -f ./lib${M}/$SO_NAME ]; then
      /bin/rm ./lib${M}/$SO_NAME
    fi
    CMD="${CXX} -m${M} -fvtable-verify=std -O0 -g -shared -fpic -rdynamic -Wl,-z,relro -DTPID=$TN -I${SRC_DIR} ${LGCC_SRC}/so.cc -o ./lib${M}/$SO_NAME"
    echo ${CMD}
    ${CMD} || exit 8
  done
done

DLOPEN_TESTS="dlopen.cc dlopen_mt.cc"

for M in $LDATA_MODELS; do
  get_lib_path lib${M}
  for T in $DLOPEN_TESTS; do
    TSRC=${LGCC_SRC}/${T}
    for OL in $LOPT_LEVELS; do
      CMD="${CXX} -m${M} -fvtable-verify=std -fpic -rdynamic -Wl,-z,relro -Wl,-R,./lib${M} -I${SRC_DIR} ${TSRC} ${OL} -ldl -lpthread"
      echo $CMD
      ($CMD  && ( ./a.out > $T.$OL.out 2>&1 ) && echo "PASS $T $OL") || echo "FAIL $T $OL"
    done
  done
done

for M in $LDATA_MODELS; do
  get_lib_path lib${M}
  for T in $TESTS; do
    TSRC=${LGCC_SRC}/${T}
    for OL in $LOPT_LEVELS; do
      CMD="${CXX} -m${M} -fvtable-verify=preinit -fpic -rdynamic -Wl,-z,relro ${TSRC} ${OL}"
      echo $CMD
      ($CMD  && ( ./a.out > $T.$OL.out 2>&1 ) && echo "PASS $T $OL") || echo "FAIL $T $OL"
    done
  done
done

for M in $LDATA_MODELS; do
  if [ -f ./lib${M}/vtv_malloc.o ]; then
    /bin/rm ./lib${M}/vtv_malloc.o;
  fi
  CMD="${CXX} -m${M} -O2 -g -c -fpic ${SRC_DIR}/vtv_malloc.cc -o ./lib${M}/vtv_malloc.o"
  echo ${CMD}
  ${CMD} || exit 3

  if [ -f ./lib${M}/vtv_utils.o ]; then
    /bin/rm ./lib${M}/vtv_utils.o;
  fi
  CMD="${CXX} -m${M} -O2 -g -c -fpic ${SRC_DIR}/vtv_utils.cc -o ./lib${M}/vtv_utils.o"
  echo ${CMD}
  ${CMD} || exit 4
done

MEMPOOL_TESTS="mempool_positive.c mempool_negative.c"

for M in $LDATA_MODELS; do
  get_lib_path lib${M}
  for T in $MEMPOOL_TESTS; do
    TSRC=${LGCC_SRC}/${T}
    for OL in $LOPT_LEVELS; do
      CMD="${CXX} -m${M} -fpic -rdynamic -I${SRC_DIR} ${TSRC} ${OL} ./lib${M}/vtv_malloc.o ./lib${M}/vtv_utils.o"
      echo $CMD
      ($CMD  && ( ./a.out > $T.$OL.out 2>&1 ) && echo "PASS $T $OL") || echo "FAIL $T $OL"
    done
  done
done

# bkoz not working ATM signature mismatch
#MT_TESTS="register_pair_mt.cc register_pair_inserts_mt.cc"
MT_TESTS=
for M in $LDATA_MODELS; do
  get_lib_path lib${M}
  for T in $MT_TESTS; do
    TSRC=${LGCC_SRC}/${T}
    for OL in $LOPT_LEVELS; do
      CMD="${CXX} -m${M} -fpic -rdynamic -I${SRC_DIR} ${TSRC} ${OL} -lpthread"
      echo $CMD
      ($CMD  && ( ./a.out > $T.$OL.out 2>&1 ) && echo "PASS $T $OL") || echo "FAIL $T $OL"
    done
  done
done

# These test cases were written for performance measurements, not for
# correctness but lets run them here so that we dont loose track of
# them
# bkoz not working ATM signature mismatch
#PERF_TESTS="register_pair.cc register_pair_inserts.cc"
PERF_TESTS=
for M in $LDATA_MODELS; do
  get_lib_path lib${M}
  for T in $PERF_TESTS; do
    TSRC=${LGCC_SRC}/${T}
    for OL in $LOPT_LEVELS; do
      CMD="${CXX} -m${M} -fpic -rdynamic -I${SRC_DIR} ${TSRC} ${OL} "
      echo $CMD
      ($CMD  && ( ./a.out > $T.$OL.out 2>&1 ) && echo "PASS $T $OL") || echo "FAIL $T $OL"
    done
  done
done


PASS_FAIL_TESTS="field-test.cc temp_deriv.cc temp_deriv2.cc temp_deriv3.cc"

for M in $LDATA_MODELS; do
  get_lib_path lib${M}
  for T in $PASS_FAIL_TESTS; do
    TSRC=${LGCC_SRC}/${T}
    for OL in $LOPT_LEVELS; do
      CMD="${CXX} -m${M} -fvtable-verify=std -fpic -rdynamic ${TSRC} ${OL}  -Wl,-z,relro -DTPID"
      echo $CMD
      ($CMD && ( ./a.out > $T.$OL.out 2>&1 )) || (( grep "Pass first attack" $T.$OL.out ) && echo "PASS $T $OL - correctly passed then failed.") || echo "FAIL $T $OL"
    done
  done
done

for M in $LDATA_MODELS; do
  get_lib_path lib${M}
  for T in $PERF_TESTS; do
    TSRC=${LGCC_SRC}/${T}
    for OL in $LOPT_LEVELS; do
      CMD="${CXX} -m${M} -fpic -rdynamic -I${SRC_DIR} ${TSRC} ${OL} "
      echo $CMD
      ($CMD  && ( ./a.out > $T.$OL.out 2>&1 ) && echo "PASS $T $OL") || echo "FAIL $T $OL"
    done
  done
done

for M in $LDATA_MODELS; do
  get_lib_path lib${M}
  for OL in $LOPT_LEVELS; do

    CMD="as --${M} -o environment-fail-${M}.o ${LGCC_SRC}/environment-fail-${M}.s"
    echo $CMD
    ${CMD} || exit 5

    CMD="${CXX} -m${M} environment-fail-${M}.o -O0 -Wl,-z,relro -o environment-fail-${M}"
    echo ${CMD}
 #   ${CMD} || exit 6
 #   (./environment-fail-${M}) || echo "PASS environment-fail-${M} (correctly failed), ${OL}"

    CMD="${CXX} -fvtable-verify=std -m${M} environment-fail-${M}.o -O0 -Wl,-z,relro -o environment-fail-${M}"
    echo ${CMD}
    ($CMD && ( ./environment-fail-${M} > environment-fail-${M}-stubs.out 2>&1 ) && echo "PASS environment-fail-${M} with libvtv_stubs ${OL}" ) || echo "FAIL environment-fail-${M} with libvtv_stubs ${OL}"

    CMD="${CXX} -m${M} ${LGCC_SRC}/replace-fail.cc -O0 -c -o replace-fail-${M}.o"
    echo ${CMD}
    ${CMD} || exit 7

    CMD="${CXX} -fvtable-verify=std -m${M} environment-fail-${M}.o replace-fail-${M}.o -O0 -Wl,-z,relro -o environment-fail-${M}"
    echo ${CMD}
    ($CMD && ( ./environment-fail-${M} > environment-fail-${M}-stubs.out 2>&1 ) && echo "PASS environment-fail-${M} with replace-fail ${OL}" ) || echo "FAIL environment-fail-${M} with replace-fail ${OL}"
  done
done

