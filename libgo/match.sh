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

gobuild() {
    line=$(echo "$1" | sed -e 's|//go:build ||')
    line=$(echo "$line" | sed -e 's/go1\.[0-9][0-9]*/1/g' -e 's/goexperiment\./goexperiment/')
    line=" $line "
    wrap='[ ()!&|]'
    for ones in $goarch $goos $cgotag $cmdlinetag gccgo goexperimentfieldtrack; do
	line=$(echo "$line" | sed -e "s/\\(${wrap}\\)${ones}\\(${wrap}\\)/"'\11\2/g')
    done
    # 386 is a special case since it looks like a number to the shell.
    # We need it to be 0 if it's not $goarch.
    if test "$goarch" != "386"; then
	line=$(echo "$line" | sed -e "s/\\(${wrap}\\)386\\(${wrap}\\)/\10\2/g")
    fi
    return $((!($line)))
}

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
	aix | android | darwin | dragonfly | freebsd | illumos | hurd | ios | js | linux | nacl | netbsd | openbsd | plan9 | solaris | windows | zos)
	    tag1=nonmatchingtag
	    ;;
	386 | amd64 | amd64p32 | arm | armbe | arm64 | arm64be | alpha | ia64 | m68k | mips | mipsle | mips64 | mips64le | mips64p32 | mips64p32le | nios2 | ppc | ppc64 | ppc64le | riscv | riscv64 | s390 | s390x | sh | shbe | sparc | sparc64 | wasm)
	    tag1=nonmatchingtag
	    ;;
	*)
	    # File name like x_amd64_random.go, where tag1=random.
	    # Don't match based on tag2.
	    tag2=
	    ;;
    esac

    case "$tag2" in
	"") ;;
	$goarch) ;;
	$goos) ;;
	aix | android | darwin | dragonfly | freebsd | hurd | ios | illumos | js | linux | nacl | netbsd | openbsd | plan9 | solaris | windows | zos)
	    tag2=nonmatchingtag
	    ;;
	386 | amd64 | amd64p32 | arm | armbe | arm64 | arm64be | alpha | ia64 | m68k | mips | mipsle | mips64 | mips64le | mips64p32 | mips64p32le | nios2 | ppc | ppc64 | ppc64le | riscv | riscv64 | s390 | s390x | sh | shbe | sparc | sparc64 | wasm)
	    tag2=nonmatchingtag
	    ;;
    esac

    if test x$tag1 = xnonmatchingtag -o x$tag2 = xnonmatchingtag; then
	continue
    fi

    # Check for go:build line
    build=$(sed '/^package /q' < $f | grep '^//go:build ' | cat)
    if test -n "$build"; then
	if $(gobuild "$build"); then
	    matched="$matched $srcdir/$f"
	fi
	continue
    fi

    # No go:build line, check for +build lines.
    # Pipe through cat so that `set -e` doesn't affect fgrep.
    tags=`sed '/^package /q' < $f | grep '^// *+build ' | cat`
    omatch=true
    first=true
    match=false
    for tag in $tags; do
	case $tag in
	    "//")
	    ;;
	    "+build" | "//+build")
		if test "$first" = "true"; then
		    first=false
		elif test "$match" = "false"; then
		    omatch=false
		fi
		match=false
		;;
	    $goos | $goarch | $cgotag | $cmdlinetag | "gccgo" | "goexperiment.fieldtrack" | go1.[0-9] | go1.[0-9][0-9])
		match=true
		;;
	    "!"$goos | "!"$goarch | "!"$cgotag | "!"$cmdlinetag | "!gccgo" | "!goexperiment.fieldtrack" | "!"go1.[0-9] | "!"go1.1[0-7])
		;;
	    *,*)
		cmatch=true
		for ctag in `echo $tag | sed -e 's/,/ /g'`; do
		    case $ctag in
			$goos | $goarch | $cgotag | $cmdlinetag | "gccgo" | "goexperiment.fieldtrack" | go1.[0-9] | go1.[0-9][0-9])
			;;
			"!"$goos | "!"$goarch | "!"$cgotag | "!"$cmdlinetag | "!gccgo" | "!goexperiment.fieldtrack" | "!"go1.[0-9] | "!"go1.1[0-7])
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
done

echo $matched $extrafiles

exit 0
