#!/bin/sh

TMPFILE=check-jni-methods.$$.1
TMPFILE2=check-jni-methods.$$.2
TMPFILE3=check-jni-methods.$$.3

# Find all methods defined in the header files generated
# from the java source files.
grep -h '^JNIEXPORT .* Java_' include/*.h | \
        LC_ALL=C sed -e 's,.*JNICALL \(Java_[a-z_A-Z0-9]*\).*$,\1,' | \
	sort > $TMPFILE

# Find all methods in the JNI C source files.
find native/jni -name \*.c | \
	xargs grep -h '^Java_' | \
        LC_ALL=C sed -e 's,^\(Java_[a-z_A-Z0-9]*\) *(.*$,\1,' | \
	sort > $TMPFILE2

# Write temporary ignore file.
cat > $TMPFILE3 << EOF
-Java_gnu_java_awt_peer_gtk_GtkMenuComponentPeer_dispose
-Java_java_lang_VMSystem_arraycopy
-Java_java_lang_VMSystem_identityHashCode
EOF

# Compare again silently.
diff -ub -0 $TMPFILE $TMPFILE2  | grep '^[+-]Java' | grep -q -v -f $TMPFILE3
RESULT=$?

if test "$RESULT" = "0" ; then
  echo "Found a problem with the JNI methods declared and implemented."
  echo "(-) missing in implementation, (+) missing in header files"

  # Compare the found method lists.
  diff -ub -0 $TMPFILE $TMPFILE2  | grep '^[+-]Java' | grep -v -f $TMPFILE3
fi

# Cleanup.
rm -f $TMPFILE $TMPFILE2 $TMPFILE3

if test "$RESULT" = "0" ; then
  exit 1
fi

exit 0

