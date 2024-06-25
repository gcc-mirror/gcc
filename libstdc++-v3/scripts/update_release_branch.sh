#!/bin/bash

# This should be run on a release branch after branching from trunk.
# Various links and references to trunk in the docs will be updated to
# refer to the new release branch.

# The major version of the new release branch.
major=$1
(($major)) || { echo "$0: Integer argument expected" >& 2 ; exit 1; }

# This assumes GNU sed
sed -i "s@^mainline GCC, not in any particular major.\$@the GCC ${major} series.@" doc/xml/manual/status_cxx*.xml
sed -i 's@https://gcc.gnu.org/cgit/gcc/tree/libstdc++-v3/testsuite/[^"]\+@&?h=releases%2Fgcc-'${major}@ doc/xml/manual/allocator.xml doc/xml/manual/mt_allocator.xml
sed -i "s@https://gcc.gnu.org/onlinedocs/gcc/Invoking-GCC.html@https://gcc.gnu.org/onlinedocs/gcc-${major}.1.0/gcc/Invoking-GCC.html@" doc/xml/manual/using.xml
