#! /bin/sh

# This script is used when compiling Classpath with gcj.  The idea is
# to compile one package at a time, and only recompile packages when
# actually required.

# We build java->class by package so we need to know what .java files
# correspond to what package.

# We have a .stamp file for each package; this is the makefile target.
# We also have a .list file for each package, which lists all the
# input files in that package.

# gen-classlist.sh makes a list of all the .java files we are going to compile.

# This script generates Makefile.deps, which looks like this:
# 
# java/awt/AWTUtilities.class: lists/java-awt.stamp
# lists/java-awt.list: /home/aph/gcc/gcc/libjava/classpath/gnu/java/awt/AWTUtilities.java
# java/awt/BitMaskExtent.class: lists/java-awt.stamp
# lists/java-awt.list: /home/aph/gcc/gcc/libjava/classpath/gnu/java/awt/BitMaskExtent.java
# java/awt/BitwiseXORComposite.class: lists/java-awt.stamp
# lists/java-awt.list: /home/aph/gcc/gcc/libjava/classpath/gnu/java/awt/BitwiseXORComposite.java

echo "Splitting for gcj"
rm -f Makefile.dtmp > /dev/null 2>&1
test -d lists || mkdir lists
# Much more efficient to do processing outside the loop...
# The first expression computes the .class file name.
# We only want the first three package components, and
# we want them separated by '-'; this is the remaining expressions.
sed -e 's, \(.*\)[.]java$, \1.java \1.class,' \
   -e 's,^\([^/ ]*\)/\([^/ ]*\) ,\1-\2 ,' \
   -e 's,^\([^/ ]*\)/\([^/ ]*\)/\([^/ ]*\) ,\1-\2-\3 ,' \
   -e 's,^\([^/ ]*\)/\([^/ ]*\)/\([^/ ]*\)/[^ ]* ,\1-\2-\3 ,' \
   classes.2 |
while read pkg dir file f2; do
   list=lists/$pkg
   echo "$dir/$file" >> ${list}.list.1

   echo "$f2: ${list}.stamp" >> Makefile.dtmp
   echo "${list}.list: $dir/$file" >> Makefile.dtmp
done

# Only update a .list file if it changed.
for file in lists/*.list.1; do
   real=`echo "$file" | sed -e 's/.1$//'`
   if cmp -s $real $file; then
      rm $file
   else
      mv $file $real
   fi
done

# If we were run we must update Makefile.deps.
mv Makefile.dtmp Makefile.deps
