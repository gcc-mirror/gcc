#!/bin/sh

# Copyright 2009 The Go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

# This script merges changes from the master copy of the Go library
# into the libgo library.  This does the easy stuff; the hard stuff is
# left to the user.

# The file MERGE should hold the Mercurial revision number of the last
# revision which was merged into these sources.  Given that, and given
# the current sources, we can run the usual diff3 algorithm to merge
# all changes into our sources.

set -e

TMPDIR=${TMPDIR:-/tmp}

OLDDIR=${TMPDIR}/libgo-merge-old
NEWDIR=${TMPDIR}/libgo-merge-new

if ! test -f MERGE; then
  echo 1>&2 "merge.sh: must be run in libgo source directory"
  exit 1
fi

if test $# -ne 1; then
  echo 1>&2 "merge.sh: Usage: merge.sh mercurial-repository"
  exit 1
fi

repository=$1

merge_rev=`sed 1q MERGE`

rm -rf ${OLDDIR}
hg clone -r ${merge_rev} ${repository} ${OLDDIR}

rm -rf ${NEWDIR}
hg clone ${repository} ${NEWDIR}

new_rev=`cd ${NEWDIR} && hg log | sed 1q | sed -e 's/.*://'`

merge() {
  name=$1
  old=$2
  new=$3
  libgo=$4
  if ! test -f ${new}; then
    # The file does not exist in the new version.
    if ! test -f ${old}; then
      echo 1>&2 "merge.sh internal error no files $old $new"
      exit 1
    fi
    if ! test -f ${libgo}; then
      # File removed in new version and libgo.
      :;
    else
      echo "merge.sh: ${name}: REMOVED"
      rm -f ${libgo}
      hg rm ${libgo}
    fi
  elif test -f ${old}; then
    # The file exists in the old version.
    if ! test -f ${libgo}; then
      echo "merge.sh: $name: skipping: exists in old and new hg, but not in libgo"
      continue
    fi
    if cmp -s ${old} ${libgo}; then
      # The libgo file is unchanged from the old version.
      if cmp -s ${new} ${libgo}; then
        # File is unchanged from old to new version.
        continue
      fi
      # Update file in libgo.
      echo "merge.sh: $name: updating"
      cp ${new} ${libgo}
    else
      # The libgo file has local changes.
      set +e
      diff3 -m -E ${libgo} ${old} ${new} > ${libgo}.tmp
      status=$?
      set -e
      case $status in
      0)
        echo "merge.sh: $name: updating"
        mv ${libgo}.tmp ${libgo}
        ;;
      1)
        echo "merge.sh: $name: CONFLICTS"
        mv ${libgo}.tmp ${libgo}
        hg resolve -u ${libgo}
        ;;
      *)
        echo 1>&2 "merge.sh: $name: diff3 failure"
        exit 1
        ;;
      esac
    fi
  else
    # The file does not exist in the old version.
    if test -f ${libgo}; then
      if ! cmp -s ${new} ${libgo}; then
        echo 1>&2 "merge.sh: $name: IN NEW AND LIBGO BUT NOT OLD"
      fi
    else
      echo "merge.sh: $name: NEW"
      dir=`dirname ${libgo}`
      if ! test -d ${dir}; then
        mkdir -p ${dir}
      fi
      cp ${new} ${libgo}
      hg add ${libgo}
    fi
  fi
}

(cd ${NEWDIR}/src/pkg && find . -name '*.go' -print) | while read f; do
  if test `dirname $f` = "./syscall"; then
    continue
  fi
  oldfile=${OLDDIR}/src/pkg/$f
  newfile=${NEWDIR}/src/pkg/$f
  libgofile=go/$f
  merge $f ${oldfile} ${newfile} ${libgofile}
done

(cd ${NEWDIR}/src/pkg && find . -name testdata -print) | while read d; do
  oldtd=${OLDDIR}/src/pkg/$d
  newtd=${NEWDIR}/src/pkg/$d
  libgotd=go/$d
  if ! test -d ${oldtd}; then
    continue
  fi
  (cd ${oldtd} && hg status -A .) | while read f; do
    if test "`basename $f`" = ".hgignore"; then
      continue
    fi
    f=`echo $f | sed -e 's/^..//'`
    name=$d/$f
    oldfile=${oldtd}/$f
    newfile=${newtd}/$f
    libgofile=${libgotd}/$f
    merge ${name} ${oldfile} ${newfile} ${libgofile}
  done
done

runtime="goc2c.c mcache.c mcentral.c mfinal.c mfixalloc.c mgc0.c mheap.c mheapmap32.c mheapmap64.c msize.c malloc.h mheapmap32.h mheapmap64.h malloc.goc mprof.goc"
for f in $runtime; do
  oldfile=${OLDDIR}/src/pkg/runtime/$f
  newfile=${NEWDIR}/src/pkg/runtime/$f
  libgofile=runtime/$f
  merge $f ${oldfile} ${newfile} ${libgofile}
done

(cd ${OLDDIR}/src/pkg && find . -name '*.go' -print) | while read f; do
  oldfile=${OLDDIR}/src/pkg/$f
  newfile=${NEWDIR}/src/pkg/$f
  libgofile=go/$f
  if test -f ${newfile}; then
    continue
  fi
  if ! test -f ${libgofile}; then
    continue
  fi
  echo "merge.sh: ${libgofile}: REMOVED"
  rm -f ${libgofile}
  hg rm ${libgofile}
done

(echo ${new_rev}; sed -ne '2,$p' MERGE) > MERGE.tmp
mv MERGE.tmp MERGE
