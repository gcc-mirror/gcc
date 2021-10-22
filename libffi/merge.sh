#!/bin/bash

# FIXME: do we need a license (or whatever else) header here?

# This script merges libffi sources from upstream.

# Default to the tip of master branch.
commit=${1-master}

fatal() {
  echo "$1"
  exit 1;
}

get_upstream() {
  rm -rf upstream
  git clone https://github.com/libffi/libffi.git upstream
  pushd upstream
  git checkout $commit || fatal "Failed to checkout $commit"
  popd
}

get_current_rev() {
  cd upstream
  git rev-parse HEAD
}

pwd | grep 'libffi$' || \
  fatal "Run this script from the libffi directory"
get_upstream
CUR_REV=$(get_current_rev)
echo Current upstream revision: $CUR_REV

# Remove the unused files.
pushd upstream
rm -rf ChangeLog.old .appveyor* .ci .github .gitignore .travis* \
	config.guess config.sub libtool-ldflags m4 make_sunver.pl \
	msvc_build
rm -rf .git autogen.sh
cp -a . ..
popd

rm -rf upstream

# Update the MERGE file.
cat << EOF > MERGE
$CUR_REV

The first line of this file holds the git revision number of the
last merge done from the master library sources.
EOF
