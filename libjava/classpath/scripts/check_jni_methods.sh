#!/bin/sh

# Fail if any command fails
set -e
# Don't override existing files
set -C

TMPFILE=/tmp/check-jni-methods.$$.1
TMPFILE2=/tmp/check-jni-methods.$$.2
TMPFILE3=/tmp/check-jni-methods.$$.3

# Find all methods defined in the header files generated
# from the java source files.
grep -h '^JNIEXPORT .* Java_' include/*.h | \
        LC_ALL=C sed -e 's,.*JNICALL \(Java_[a-z_A-Z0-9]*\).*$,\1,' | \
	sort > $TMPFILE

# Find all methods in the JNI C source files.
find native/jni -name \*.c | \
	xargs grep -h '^Java_' | \
        LC_ALL=C sed -e 's,^\(Java_[a-z_A-Z0-9]*\) *(.*$,\1,' > $TMPFILE2
# Or in the the C++ files. (Note that cpp doesn't follow gnu conventions atm)
# So we try to match both GNU style and some other style.
find native/jni -name \*.cpp | \
	xargs grep -h '^Java_' | \
        LC_ALL=C sed -e 's,^\(Java_[a-z_A-Z0-9]*\) *(.*$,\1,' >> $TMPFILE2
find native/jni -name \*.cpp | \
	xargs egrep -h '^(JNIEXPORT .* JNICALL )?Java_' | \
	cut -f4 -d\  | \
        LC_ALL=C sed -e 's,^\JNIEXPORT .* JNICALL \(Java_[a-z_A-Z0-9]*\) *(.*$,\1,' >> $TMPFILE2
mv $TMPFILE2 $TMPFILE3
sort $TMPFILE3 > $TMPFILE2
rm $TMPFILE3

# Write temporary ignore file.
cat > $TMPFILE3 << EOF
-Java_gnu_java_awt_peer_gtk_GtkMenuComponentPeer_dispose
-Java_java_lang_VMSystem_arraycopy
-Java_java_lang_VMSystem_identityHashCode
EOF

# Compare again silently.
# Use fgrep and direct the output to /dev/null for compatibility with older
# grep instead of using the non portable -q.
if diff -b -U 0 $TMPFILE $TMPFILE2 | grep '^[+-]Java' | \
    fgrep -v -f $TMPFILE3 > /dev/null;
then
  PROBLEM=1
  echo "Found a problem with the JNI methods declared and implemented."
  echo "(-) missing in implementation, (+) missing in header files"

  # Compare the found method lists.
  diff -b -U 0 $TMPFILE $TMPFILE2  | grep '^[+-]Java' | fgrep -v -f $TMPFILE3
fi

# Cleanup.
rm -f $TMPFILE $TMPFILE2 $TMPFILE3

if test "$PROBLEM" = "1" ; then
  exit 1
fi

exit 0
