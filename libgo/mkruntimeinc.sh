#!/bin/sh

# Copyright 2018 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

# Create tmp-runtime.inc from runtime.inc.raw.

IN=runtime.inc.raw
OUT=tmp-runtime.inc

rm -f runtime.inc.tmp2 runtime.inc.tmp3

# _Complex_lock and _Reader_lock are Go translations of some AIX system
# types and should not be exported back to C
# semt is a Go translation of the C type sem_t; it fails to convert on
# some systems and need not be exported back to C.
# sigset conflicts with system type sigset on AIX, so we need to rename it

grep -v "#define _" ${IN} | grep -v "#define [cm][01234] " | grep -v "#define empty " | grep -v "#define \\$" > runtime.inc.tmp2
for pattern in '_[GP][a-z]' _Max _Lock _Sig _Trace _MHeap _Num
do
  grep "#define $pattern" ${IN} >> runtime.inc.tmp2
done
TYPES="_Complex_lock _Reader_lock semt"
for TYPE in $TYPES
do
  sed -e '/struct '${TYPE}' {/,/^}/s/^.*$//' runtime.inc.tmp2 > runtime.inc.tmp3;
  mv runtime.inc.tmp3 runtime.inc.tmp2
done
sed -e 's/sigset/sigset_go/' runtime.inc.tmp2 > ${OUT}
rm -f runtime.inc.tmp2 runtime.inc.tmp3
exit 0
