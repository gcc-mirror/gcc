#!/bin/sh

# Copyright 2016 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

# Given a source directory, returns the non-test Go files that should
# be built for this target.  This implements Go's build constraints in
# a shell script.  There is similar code in testsuite/gotest.

set -e

LANG=C
LC_ALL=C
LC_CTYPE=C
export LANG LC_ALL LC_CTYPE

srcdir=""
goarch=""
goos=""
extrafiles=""
cmdlinetag="nosuchtag"
cgotag="cgo"

for arg; do
    case "x$arg" in
	x--srcdir)
	    srcdir=$2
	    shift
	    shift
	    ;;
	x--srcdir=*)
	    srcdir=`echo $1 | sed -e 's/^--srcdir=//'`
	    shift
	    ;;
	x--goarch)
	    goarch=$2
	    shift
	    shift
	    ;;
	x--goarch=*)
	    goarch=`echo $1 | sed -e 's/^--goarch=//'`
	    shift
	    ;;
	x--goos)
	    goos=$2
	    shift
	    shift
	    ;;
	x--goos=*)
	    goos=`echo $1 | sed -e 's/^--goos=//'`
	    shift
	    ;;
	x--extrafiles)
	    extrafiles=$2
	    shift
	    shift
	    ;;
	x--extrafiles=*)
	    extrafiles=`echo $1 | sed -e 's/^--extrafiles=//'`
	    shift
	    ;;
	x--tag)
	    cmdlinetag=$2
	    shift
	    shift
	    ;;
	x--tag=*)
	    cmdlinetag=`echo $1 | sed -e 's/^--tag=//'`
	    shift
	    ;;
	x--nocgo)
	    cgotag="nosuchtag"
	    shift
	    ;;
	*)
	    echo 1>&2 "unknown argument $arg"
	    exit 1
	    ;;
    esac
done

cd $srcdir

gofiles=
for f in *.go; do
    case $f in
	*_test.go)
	    ;;
	*.go)
	    gofiles="$gofiles $f"
	    ;;
    esac
done

if test "$gofiles" = ""; then
    echo 1>&2 "no non-test .go files in $srcdir"
    exit 1
fi

matched=
for f in $gofiles; do
    tag1=`echo $f | sed -e 's/^.*_\([^_]*\).go$/\1/'`
    tag2=`echo $f | sed -e 's/^.*_\([^_]*\)_[^_]*.go$/\1/'`
    if test x$tag1 = x$f; then
	tag1=
    fi
    if test x$tag2 = x$f; then
	tag2=
    fi

    case "$tag1" in
	"") ;;
	$goarch) ;;
	$goos) ;;
	aix | android | darwin | dragonfly | freebsd | linux | nacl | netbsd | openbsd | plan9 | solaris | windows)
	    tag1=nonmatchingtag
	    ;;
	386 | amd64 | amd64p32 | arm | armbe | arm64 | arm64be | alpha | ia64 | m68k | mips | mipsle | mips64 | mips64le | mips64p32 | mips64p32le | nios2 | ppc | ppc64 | ppc64le | riscv64 | s390 | s390x | sh | shbe | sparc | sparc64)
	    tag1=nonmatchingtag
	    ;;
    esac

    case "$tag2" in
	"") ;;
	$goarch) ;;
	$goos) ;;
	aix | android | darwin | dragonfly | freebsd | linux | nacl | netbsd | openbsd | plan9 | solaris | windows)
	    tag2=nonmatchingtag
	    ;;
	386 | amd64 | amd64p32 | arm | armbe | arm64 | arm64be | alpha | ia64 | m68k | mips | mipsle | mips64 | mips64le | mips64p32 | mips64p32le | nios2 | ppc | ppc64 | ppc64le | riscv64 | s390 | s390x | sh | shbe | sparc | sparc64)
	    tag2=nonmatchingtag
	    ;;
    esac

    if test x$tag1 != xnonmatchingtag -a x$tag2 != xnonmatchingtag; then
	# Pipe through cat so that `set -e` doesn't affect fgrep.
	tags=`sed '/^package /q' < $f | grep '^// +build ' | cat`
	omatch=true
	first=true
	match=false
	for tag in $tags; do
	    case $tag in
		"//")
		    ;;
		"+build")
		    if test "$first" = "true"; then
			first=false
		    elif test "$match" = "false"; then
			omatch=false
		    fi
		    match=false
		    ;;
		$goos | $goarch | $cgotag | $cmdlinetag | "gccgo" | go1.[0-9])
		    match=true
		    ;;
		"!"$goos | "!"$goarch | "!"$cgotag | "!"$cmdlinetag | "!gccgo" | "!"go1.[0-9])
		    ;;
		*,*)
		    cmatch=true
		    for ctag in `echo $tag | sed -e 's/,/ /g'`; do
			case $ctag in
			    $goos | $goarch | $cgotag | $cmdlinetag | "gccgo" | go1.[0-9])
				;;
			    "!"$goos | "!"$goarch | "!"$cgotag | "!"$cmdlinetag | "!gccgo" | "!"go1.[0-9])
				cmatch=false
				;;
			    "!"*)
				;;
			    *)
				cmatch=false
				;;
			esac
		    done
		    if test "$cmatch" = "true"; then
			match=true
		    fi
		    ;;
		"!"*)
		    match=true
		    ;;
	    esac
	done

	if test "$match" = "false" -a "$first" = "false"; then
	    omatch=false
	fi

	if test "$omatch" = "true"; then
	    matched="$matched $srcdir/$f"
	fi
    fi
done

echo $matched $extrafiles

exit 0
