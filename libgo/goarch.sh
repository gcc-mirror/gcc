#!/bin/sh

# Copyright 2018 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

# Code in Makefile.am will invoke this script with two arguments.
# The first is a GOARCH value.  The second is a keyword.
# The script will print the value of that keyword for that GOARCH.
# Keywords:
# - bigendian: true or false
# - cachelinesize: the cache line size in bytes
#	(for performance only; it's not essential to get this right)
# - defaultphyspagesize: the default physical page size in bytes
#	(not currently used, but maybe some day)
# - family: the processor family, from ALLGOARCHFAMILY in configure.ac
# - int64align: alignment of int64 type in bytes
# - maxalign: maximum alignment of values of Go types in bytes
# - minframesize: size of smallest possible function frame in bytes
#	(not currently used, may never be used)
# - pcquantum: minimum size of a single instruction in bytes
# - ptrsize: size of a pointer in bytes

if test $# -ne 2; then
    echo 1>&2 "usage: goarch <goarch> <keyword>"
    exit 1
fi

goarch=$1
keyword=$2

# Default values
bigendian=false
cachelinesize=64
defaultphyspagesize=4096
family=unknown
int64align=8
maxalign=8
minframesize=0
pcquantum=1
ptrsize=8

case $goarch in
    386)
	family=I386
	int64align=4
	maxalign=4
	ptrsize=4
	;;
    alpha)
	family=ALPHA
	defaultphyspagesize=8192
	pcquantum=4
	;;
    amd64)
	family=AMD64
	;;
    amd64p32)
	family=AMD64
	ptrsize=4
	;;
    arm | armbe)
	family=ARM
	cachelinesize=32
	minframesize=4
	pcquantum=4
	ptrsize=4
	case $goarch in
	    *be)
		bigendian=true
		;;
	esac
	;;
    arm64 | arm64be)
	family=ARM64
	cachelinesize=32
	defaultphyspagesize=65536
	minframesize=8
	pcquantum=4
	case $goarch in
	    *be)
		bigendian=true
		;;
	esac
	;;
    ia64)
	family=IA64
	cachelinesize=128
	defaultphyspagesize=65536
	;;
    m68k)
	family=M68K
	bigendian=true
	cachelinesize=16
	int64align=2
	maxalign=2
	pcquantum=4
	ptrsize=4
	;;
    mips | mipsle | mips64p32 | mips64p32le)
	family=MIPS
	bigendian=true
	cachelinesize=32
	defaultphyspagesize=16384
	minframesize=4
	pcquantum=4
	ptrsize=4
	case $goarch in
	    *le)
		bigendian=false
	        ;;
	esac
	;;
    mips64 | mips64le)
	family=MIPS64
	bigendian=true
	cachelinesize=32
	defaultphyspagesize=16384
	minframesize=8
	pcquantum=4
	case $goarch in
	    *le)
		bigendian=false
		;;
	esac
	;;
    nios2)
        family=NIOS2
        cachelinesize=32
        minframesize=16
        pcquantum=4
        ptrsize=4
        ;;
    ppc)
	family=PPC
	bigendian=true
	defaultphyspagesize=65536
	minframesize=32
	pcquantum=4
	ptrsize=4
	;;
    ppc64 | ppc64le)
	family=PPC64
	bigendian=true
	defaultphyspagesize=65536
	minframesize=32
	pcquantum=4
	case $goarch in
	    *le)
		bigendian=false
		;;
	esac
	;;
    riscv)
	family=RISCV
	pcquantum=2
	ptrsize=4
	;;
    riscv64)
	family=RISCV64
	pcquantum=2
	;;
    s390)
	family=S390
	bigendian=true
	cachelinesize=256
	minframesize=4
	pcquantum=2
	ptrsize=4
	;;
    s390x)
	family=S390X
	bigendian=true
	cachelinesize=256
	minframesize=8
	pcquantum=2
	;;
    sh | shbe)
	family=SH
	cachelinesize=16
	int64align=4
	minframesize=4
	pcquantum=2
	ptrsize=4
	case $goarch in
	    *be)
		bigendian=true
		;;
	esac
	;;
    sparc)
	family=SPARC
	bigendian=true
	defaultphyspagesize=8192
	pcquantum=4
	ptrsize=4
	;;
    sparc64)
	family=SPARC64
	bigendian=true
	defaultphyspagesize=8192
	pcquantum=4
	;;
    wasm)
	family=WASM
	defaultphyspagesize=65536
	;;
    *)
	echo 1>&2 "unrecognized goarch value \"$goarch\""
	exit 1
	;;
esac

if test "$family" = "unknown"; then
    echo 1>&2 "internal error: no family for goarch value \"$goarch\""
    exit 1
fi

case $keyword in
    bigendian)
	echo $bigendian
	;;
    cachelinesize)
	echo $cachelinesize
	;;
    defaultphyspagesize)
	echo $defaultphyspagesize
	;;
    family)
	echo $family
	;;
    int64align)
	echo $int64align
	;;
    maxalign)
	echo $maxalign
	;;
    minframesize)
	echo $minframesize
	;;
    pcquantum)
	echo $pcquantum
	;;
    ptrsize)
	echo $ptrsize
	;;
    *)
	echo 1>&2 "unrecognized keyword \"$keyword\""
	exit 1
	;;
esac

exit 0
